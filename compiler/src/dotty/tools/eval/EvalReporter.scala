package dotty.tools
package eval

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.reporting.AbstractReporter
import dotty.tools.dotc.reporting.Diagnostic

/** Reporter that forwards each compile error as a string to the user-
 *  supplied callback in [[EvalCompilerConfig.errorReporter]].
 *
 *  Mirrors `dotty.tools.debug.ExpressionReporter` from the debug
 *  pipeline. Warnings are intentionally dropped for now — the
 *  interesting feedback for an eval call is whether it compiles, not
 *  best-practice lints in code the user didn't author directly.
 */
private[eval] class EvalReporter(reportError: String => Unit) extends AbstractReporter:
  override def doReport(dia: Diagnostic)(using Context): Unit =
    dia match
      case error: Diagnostic.Error =>
        reportError(stripColor(messageAndPos(error)))
      case _ => ()
