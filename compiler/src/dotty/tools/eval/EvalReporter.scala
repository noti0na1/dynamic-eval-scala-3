package dotty.tools
package eval

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.reporting.AbstractReporter
import dotty.tools.dotc.reporting.Diagnostic

/** Reporter that forwards compilation errors to the configured callback.
 *  Warnings are not part of the dynamic-eval result.
 */
private[eval] class EvalReporter(reportError: String => Unit) extends AbstractReporter:
  override def doReport(dia: Diagnostic)(using Context): Unit =
    dia match
      case error: Diagnostic.Error =>
        reportError(stripColor(messageAndPos(error)))
      case _ => ()
