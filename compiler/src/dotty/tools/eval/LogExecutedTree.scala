package dotty.tools
package eval

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.Phase

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.util.control.NonFatal

/** Writes the tree after [[ResolveEvalAccess]] to the configured wrapper log.
 *  [[EvalCompiler]] inserts this phase only when logging is enabled.
 */
private[eval] class LogExecutedTree(config: EvalCompilerConfig) extends Phase:

  override def phaseName: String = LogExecutedTree.name
  override def isCheckable: Boolean = false

  protected def run(using Context): Unit =
    val rendered =
      try ctx.compilationUnit.tpdTree.show(using ctx.withoutColors)
      catch case NonFatal(e) => s"// LogExecutedTree: tree.show failed: ${e.getMessage}"
    val target = new java.io.File(config.evalLogDir, s"eval_${config.evalLogTimestamp}_wrapper.scala")
    try
      val parent = target.getParentFile
      if parent != null then parent.mkdirs()
      Files.write(target.toPath, rendered.getBytes(StandardCharsets.UTF_8))
    catch case NonFatal(e) =>
      // Logging is optional, so report the failure without failing evaluation.
      System.err.println(
        s"[eval] WARNING: failed to write $target: " +
        s"${e.getClass.getSimpleName}: ${e.getMessage}")

private[eval] object LogExecutedTree:
  val name: String = "logExecutedTree"
