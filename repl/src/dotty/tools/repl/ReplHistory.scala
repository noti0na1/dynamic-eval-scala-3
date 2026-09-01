package dotty.tools
package repl

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.util.concurrent.ConcurrentHashMap
import java.util.regex.Pattern

import scala.util.control.NonFatal

/** Writes REPL submissions and their output to the transcript selected by
 *  `-Xrepl-history-file`.
 *
 *  Each entry uses the live prompt format:
 *
 *      scala> <input>
 *           | <input continuation lines>
 *      <output>
 *
 *  Entries are appended as UTF-8 in a single write, with ANSI color codes
 *  removed. Captured output is bounded to prevent an individual submission
 *  from consuming unbounded memory. History is best-effort: an append failure
 *  warns once for each path and never interrupts the REPL session.
 */
object ReplHistory:

  /** Exposes the charset of a caller-supplied stream on runtimes where
   *  `PrintStream.charset()` is unavailable.
   */
  trait CharsetCarrier:
    def replCharset: Charset

  /** Maximum captured output per submission. Additional bytes still reach the
   *  live stream and are summarized by a truncation marker in the transcript.
   */
  private[repl] val captureLimit: Int = 1 << 20

  /** Run `work`, capture writes to `tee`, and append one transcript entry.
   *  An empty path disables capture without evaluating `input`. Nested
   *  interpretation contributes to the outer submission instead of creating
   *  a separate entry. Writes to `System.out` and `System.err` are included
   *  when the driver redirects those streams through `tee`.
   */
  private[repl] def captureLine[A](
      tee: TeePrintStream,
      historyFile: String,
      input: => String
  )(work: => A): A =
    if historyFile.isEmpty || tee.isCapturing then work
    else
      val capture = new BoundedCapture(captureLimit)
      try tee.withCapture(capture)(work)
      finally append(historyFile, input, capture.text(tee.charset), capture.dropped)

  private val warnedAppendFailures =
    ConcurrentHashMap.newKeySet[String]()

  /** Append one entry in a single write, creating parent directories as needed.
   *  Blank entries are omitted.
   */
  private def append(path: String, input: String, output: String, dropped: Long): Unit =
    val cleanInput = input.stripTrailing
    val cleanOutput = stripAnsi(output).stripTrailing
    if cleanInput.isEmpty && cleanOutput.isEmpty then return

    val entry = new StringBuilder
    if cleanInput.nonEmpty then
      val lines = cleanInput.linesIterator
      entry ++= "scala> " ++= lines.next() += '\n'
      // Match the live continuation prompt to distinguish input from output.
      lines.foreach(line => entry ++= "     | " ++= line += '\n')
    if cleanOutput.nonEmpty then entry ++= cleanOutput += '\n'
    if dropped > 0 then entry ++= s"[... output truncated: $dropped bytes dropped]" += '\n'
    entry += '\n'

    try
      val file = Paths.get(path)
      val parent = file.getParent
      if parent != null then Files.createDirectories(parent)
      Files.write(
        file,
        entry.toString.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    catch case NonFatal(e) =>
      // Report the first failure without repeatedly disrupting the session.
      if warnedAppendFailures.add(path) then
        System.err.println(
          s"[repl-history] WARNING: failed to append to '$path': " +
          s"${e.getClass.getSimpleName}: ${e.getMessage}")

  // Strip ANSI color sequences because individual renderers can introduce them
  // independently of the session's color setting.
  private val ansi = Pattern.compile("\u001b\\[[0-9;]*m")
  private def stripAnsi(s: String): String =
    if s.indexOf('\u001b') < 0 then s
    else ansi.matcher(s).replaceAll("")

  /** Keeps the first `limit` bytes and counts the remainder. */
  private final class BoundedCapture(limit: Int) extends OutputStream:
    private val buf = new ByteArrayOutputStream
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

  // Use reflection to remain compatible with runtimes where
  // `PrintStream.charset()` is not public. `CharsetCarrier` handles custom
  // streams. For system streams, consult their encoding properties before
  // falling back to the default charset.
  private def charsetOf(ps: PrintStream): Charset =
    def propertyCharset(name: String): Option[Charset] =
      try
        Option(System.getProperty(name)).flatMap: value =>
          try Some(Charset.forName(value))
          catch case NonFatal(_) => None
      catch case NonFatal(_) => None
    def fallback: Charset =
      if ps eq System.out then propertyCharset("sun.stdout.encoding").getOrElse(Charset.defaultCharset())
      else if ps eq System.err then propertyCharset("sun.stderr.encoding").getOrElse(Charset.defaultCharset())
      else Charset.defaultCharset()
    ps match
      case carrier: CharsetCarrier => carrier.replCharset
      case _ =>
        try
          ps.getClass.getMethod("charset").invoke(ps) match
            case cs: Charset => cs
            case _ => fallback
        catch case NonFatal(_) => fallback

  /** Forwards to `primary` and duplicates writes to the active capture. The
   *  stream uses `primary`'s charset so captured bytes are decoded consistently.
   */
  final class TeePrintStream(primary: PrintStream)
      extends PrintStream(primary, false, charsetOf(primary)):
    private[repl] val charset: Charset = charsetOf(primary)

    @volatile private var capture: OutputStream | Null = null

    private[repl] def isCapturing: Boolean = capture != null

    /** Install `target` for `work`, then restore the previous capture. */
    private[repl] def withCapture[A](target: OutputStream)(work: => A): A =
      val previous = capture
      capture = target
      try work
      finally capture = previous

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

    /** Disable the failed capture and report directly to `primary`. Reporting
     *  through a redirected system stream could re-enter this tee recursively.
     */
    private def reportCaptureFailure(e: Throwable): Unit =
      capture = null
      primary.println(
        "[repl-history] WARNING: output capture failed, history entry " +
        s"will be incomplete: ${e.getClass.getSimpleName}: ${e.getMessage}")
  end TeePrintStream

end ReplHistory
