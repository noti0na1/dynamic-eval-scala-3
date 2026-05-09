package dotty.tools
package repl

import scala.util.control.NonFatal

/** Per-line transcript writer for the REPL.
 *
 *  When the user passes `-Xrepl-history-file:<path>`, the driver wraps
 *  each line's interpretation in [[captureLine]], which tees the REPL's
 *  output stream into a per-line buffer for the duration of that line
 *  and then appends an entry of the form:
 *
 *      scala> <input>
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
 *  No global mutable state lives in this module; the file IS the
 *  history. A reader (typically an `agent[T]` body that wants to
 *  surface recent REPL context to the LLM) reads it like any other
 *  file:
 *
 *  {{{
 *    val transcript = scala.io.Source.fromFile("history.repl").mkString
 *    agent[String](s"continuation of: $transcript")
 *  }}}
 *
 *  The file is opened append-only so multiple runs against the same
 *  path stack chronologically. ANSI color codes are stripped before
 *  writing. The writer is best-effort: I/O failures during a write
 *  are swallowed silently rather than disturbing the REPL.
 */
object ReplHistory:

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
   *  installation): the no-flag path has zero overhead.
   */
  private[repl] def captureLine[A](
      tee: TeePrintStream,
      historyFile: String,
      input: String
  )(work: => A): A =
    if historyFile.isEmpty then return work

    val buf = new java.io.ByteArrayOutputStream
    try tee.withCapture(buf)(work)
    finally append(historyFile, input, buf.toString)

  /** Append one transcript entry to `path`. Skipped silently when both
   *  `input` and `output` are empty (e.g. blank line) or when I/O
   *  fails. Creates the parent directory on demand.
   */
  private def append(path: String, input: String, output: String): Unit =
    val cleanInput = input.stripTrailing
    val cleanOutput = stripAnsi(output).stripTrailing
    if cleanInput.isEmpty && cleanOutput.isEmpty then return
    try
      val file = new java.io.File(path)
      val parent = file.getParentFile
      if parent != null && !parent.exists() then parent.mkdirs()
      val w = new java.io.FileWriter(file, /* append = */ true)
      try
        if cleanInput.nonEmpty then
          w.write("scala> ")
          w.write(cleanInput)
          w.write("\n")
        if cleanOutput.nonEmpty then
          w.write(cleanOutput)
          w.write("\n")
        w.write("\n")
      finally w.close()
    catch case NonFatal(e) =>
      // Surface the cause (typically a permissions / disk-full
      // problem) once instead of silently dropping every entry.
      // We don't want a broken history file to crash the REPL.
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

  /** A `PrintStream` that forwards to a primary destination and, when
   *  a capture is installed, also writes everything to the capture.
   *  Used by the REPL driver as its `out` field so [[captureLine]] can
   *  intercept the rendering without disturbing the caller-supplied
   *  output stream.
   */
  final class TeePrintStream(primary: java.io.PrintStream)
      extends java.io.PrintStream(primary):
    @volatile private var capture: java.io.OutputStream | Null = null

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
        try c.write(b) catch case NonFatal(e) =>
          System.err.println(
            s"[repl-history] WARNING: failed to write to capture buffer: " +
            s"${e.getClass.getSimpleName}: ${e.getMessage}")

    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      super.write(b, off, len)
      val c = capture
      if c != null then
        try c.write(b, off, len) catch case NonFatal(e) =>
          System.err.println(
            s"[repl-history] WARNING: failed to write to capture buffer: " +
            s"${e.getClass.getSimpleName}: ${e.getMessage}")
  end TeePrintStream

end ReplHistory
