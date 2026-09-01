package dotty.tools
package repl

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths, StandardOpenOption}

import scala.util.control.NonFatal

/** Per-line transcript writer for the REPL.
 *
 *  When the user passes `-Xrepl-history-file:<path>`, the driver wraps
 *  each line's interpretation in [[captureLine]], which tees the REPL's
 *  output stream into a per-line buffer for the duration of that line
 *  and then appends an entry of the form:
 *
 *      scala> <input>
 *           | <input continuation lines>
 *      <output>
 *
 *      scala> <next input>
 *      ...
 *
 *  The driver already redirects `System.out`/`System.err` through the
 *  same tee in `withRedirectedOutput`, so user `println`s land in the
 *  buffer without a second redirection layer. (If `redirectOutput` is
 *  off, the file records only what the driver itself prints.)
 *
 *  The file is the history; a reader (typically an `agent[T]` body that
 *  wants to surface recent REPL context to the LLM) reads it like any
 *  other file:
 *
 *  {{{
 *    val transcript = scala.io.Source.fromFile("history.repl").mkString
 *    agent[String](s"continuation of: $transcript")
 *  }}}
 *
 *  The file is opened append-only so multiple runs against the same
 *  path stack chronologically, and each entry is appended with a
 *  single write so concurrent sessions do not interleave partial
 *  entries. Entries are UTF-8; ANSI color codes are stripped. A line's
 *  capture is capped at [[captureLimit]] bytes so a line that streams
 *  large output cannot exhaust the heap; the entry then ends with a
 *  truncation marker. The writer is best-effort: the first failed
 *  append warns on stderr and the REPL continues, later failures are
 *  dropped silently.
 */
object ReplHistory:

  /** Package-local charset metadata for explicit streams on JDK 17, where
   *  `PrintStream.charset()` is not yet public.
   */
  private[repl] trait CharsetCarrier:
    def replCharset: Charset

  /** Per-line capture cap. Output beyond this many bytes is dropped
   *  from the history entry (never from the live stream) and recorded
   *  as a truncation marker.
   */
  private[repl] val captureLimit: Int = 1 << 20

  /** Run `work`, capturing everything written to `tee` (the REPL
   *  driver's `out` stream) for the duration, then append a transcript
   *  entry to `historyFile` if the path is non-empty.
   *
   *  We rely on the driver's own `withRedirectedOutput` having already
   *  pointed `System.out` / `System.err` at `tee` for user prints to
   *  reach the capture too. When the driver's `redirectOutput` is
   *  disabled, this records only diagnostics the driver itself emits
   *  (definitions, error messages); user `println`s would go elsewhere.
   *
   *  When `historyFile` is empty, work runs untouched (no capture
   *  installation, `input` never forced): the no-flag path has zero
   *  overhead.
   */
  private[repl] def captureLine[A](
      tee: TeePrintStream,
      historyFile: String,
      input: => String
  )(work: => A): A =
    // Nested interpretation (`:load`, `:replay`, init scripts) belongs to the
    // initiating submission. Keep writing to its installed capture and let
    // only that outermost call append a transcript entry.
    if historyFile.isEmpty || tee.isCapturing then return work

    val buf = new BoundedCapture(captureLimit)
    try tee.withCapture(buf)(work)
    finally append(historyFile, input, buf.text(tee.charset), buf.dropped)

  private var appendFailureWarned = false

  /** Append one transcript entry to `path` in a single write. Skipped
   *  when both `input` and `output` are empty (e.g. blank line).
   *  Creates the parent directory on demand.
   */
  private def append(path: String, input: String, output: String, dropped: Long): Unit =
    val cleanInput = input.stripTrailing
    val cleanOutput = stripAnsi(output).stripTrailing
    if cleanInput.isEmpty && cleanOutput.isEmpty then return

    val entry = new StringBuilder
    if cleanInput.nonEmpty then
      val lines = cleanInput.linesIterator
      entry ++= "scala> " ++= lines.next() += '\n'
      // Mark continuation lines like the live prompt does, so a reader
      // can split input from output in multi-line entries.
      lines.foreach(line => entry ++= "     | " ++= line += '\n')
    if cleanOutput.nonEmpty then entry ++= cleanOutput += '\n'
    if dropped > 0 then entry ++= s"[... output truncated: $dropped bytes dropped]" += '\n'
    entry += '\n'

    try
      val file = Paths.get(path)
      val parent = file.getParent
      if parent != null then Files.createDirectories(parent)
      Files.write(file, entry.toString.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    catch case NonFatal(e) =>
      // Warn once (typically a permissions / disk-full problem), then
      // go quiet: a broken history path must not spam or crash the
      // REPL, but should not fail silently either.
      if !appendFailureWarned then
        appendFailureWarned = true
        System.err.println(
          s"[repl-history] WARNING: failed to append to '$path': " +
          s"${e.getClass.getSimpleName}: ${e.getMessage}")

  // ANSI escape: ESC[<digits/semicolons>m. The driver might emit
  // colored output even with `-color:never` (some message-rendering
  // paths re-apply highlighting), so we always strip on the way
  // to the file. The `\u001b` in the pattern is a literal ESC.
  private val ansi = java.util.regex.Pattern.compile("\u001b\\[[0-9;]*m")
  private def stripAnsi(s: String): String =
    if s.indexOf('\u001b') < 0 then s
    else ansi.matcher(s).replaceAll("")

  /** A byte sink that keeps the first `limit` bytes and counts the
   *  rest, so a single line's capture cannot grow without bound.
   */
  private final class BoundedCapture(limit: Int) extends java.io.OutputStream:
    private val buf = new java.io.ByteArrayOutputStream
    private var droppedCount: Long = 0

    override def write(b: Int): Unit =
      if buf.size() < limit then buf.write(b)
      else droppedCount += 1

    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      val room = limit - buf.size()
      if room >= len then buf.write(b, off, len)
      else
        if room > 0 then buf.write(b, off, room)
        droppedCount += len - math.max(room, 0)

    def text(charset: Charset): String = buf.toString(charset)
    def dropped: Long = droppedCount
  end BoundedCapture

  // `PrintStream.charset()` exists since JDK 18; probe reflectively so
  // we still run on JDK 17. A caller that explicitly chose a non-default
  // charset on JDK 17 can be wrapped internally with CharsetCarrier;
  // otherwise the fallback matches PrintStream's default constructors.
  private def charsetOf(ps: java.io.PrintStream): Charset =
    ps match
      case carrier: CharsetCarrier => carrier.replCharset
      case _ =>
        try
          ps.getClass.getMethod("charset").invoke(ps) match
            case cs: Charset => cs
            case _ => Charset.defaultCharset()
        catch case _: ReflectiveOperationException => Charset.defaultCharset()

  /** A `PrintStream` that forwards to a primary destination and, when
   *  a capture is installed, also writes everything to the capture.
   *  Used by the REPL driver as its `out` field so [[captureLine]] can
   *  intercept the rendering without disturbing the caller-supplied
   *  output stream. Encodes with `primary`'s own charset so the bytes
   *  passed through are the ones `primary` would have produced.
   */
  final class TeePrintStream(primary: java.io.PrintStream)
      extends java.io.PrintStream(primary, false, charsetOf(primary)):
    private[repl] val charset: Charset = charsetOf(primary)

    @volatile private var capture: java.io.OutputStream | Null = null

    private[repl] def isCapturing: Boolean = capture != null

    /** Install `buf` as the capture target for the duration of `work`,
     *  then restore whatever capture was installed before. Reentrant.
     */
    private[repl] def withCapture[A](buf: java.io.OutputStream)(work: => A): A =
      val prev = capture
      capture = buf
      try work
      finally capture = prev

    override def write(b: Int): Unit =
      super.write(b)
      val c = capture
      if c != null then
        try c.write(b)
        catch case NonFatal(e) => reportCaptureFailure(e)

    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      super.write(b, off, len)
      val c = capture
      if c != null then
        try c.write(b, off, len)
        catch case NonFatal(e) => reportCaptureFailure(e)

    /** Report through `primary`, not `System.err`: `System.err` may be
     *  redirected to this tee while the capture is installed, and
     *  re-entering `write` from the handler would recurse on the same
     *  failing capture. Uninstalling the capture also stops repeat
     *  failures for the rest of the line.
     */
    private def reportCaptureFailure(e: Throwable): Unit =
      capture = null
      primary.println(
        "[repl-history] WARNING: output capture failed, history entry " +
        s"will be incomplete: ${e.getClass.getSimpleName}: ${e.getMessage}")
  end TeePrintStream

end ReplHistory
