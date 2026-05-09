package dotty.tools
package repl
package eval

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.Phase

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.util.control.NonFatal

/** Pretty-prints the post-[[ResolveEvalAccess]] tree to
 *  `eval_<timestamp>_wrapper.scala` under `-Xrepl-eval-log-dir`. The
 *  snapshot captures the executed shape — `__Expression.evaluate`
 *  with every `reflectEval` placeholder lowered to `getValue` /
 *  `callMethod` / `getField` / etc.
 *
 *  [[EvalCompiler]] only inserts this phase when the log dir and
 *  timestamp are both set.
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
      Files.write(target.toPath, rendered.getBytes(StandardCharsets.UTF_8))
    catch case NonFatal(_) => ()

private[eval] object LogExecutedTree:
  val name: String = "logExecutedTree"
