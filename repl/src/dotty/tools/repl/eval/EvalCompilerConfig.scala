package dotty.tools
package repl
package eval

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Names.*

import java.util.function.Consumer

/** Configuration for one compile-and-run of an eval body through the
 *  eval pipeline. Construct with defaults via `EvalCompilerConfig()`
 *  and override fields with the case class's auto-generated `copy`:
 *
 *  ```
 *  EvalCompilerConfig().copy(
 *    outputClassName = name,
 *    body = code,
 *    expectedType = "Int"
 *  )
 *  ```
 *
 *  `outputClassName` must be unique per compile (a UUID-flavoured
 *  suffix is the typical choice) so different calls don't collide on
 *  the classloader.
 */
private[eval] case class EvalCompilerConfig(
    packageName: String = "",
    outputClassName: String = "",
    body: String = "",
    marker: String = EvalContext.placeholder,
    errorReporter: Consumer[String] = (_: String) => (),
    testMode: Boolean = false,
    expectedType: String = "",
    /** Names of bindings already captured by the outer eval call.
     *  Seeded into the runtime nested-eval rewriter inside
     *  [[SpliceEvalBody]] so an inner eval call's bindings list
     *  includes both these and any new in-scope names introduced
     *  inside the body itself.
     */
    initialScope: Array[(String, Boolean)] = Array.empty,
    /** The outer eval's `enclosingSource` slice (with its own
     *  marker). When non-empty, [[SpliceEvalBody]] activates the
     *  rewriter's nested mode so each inner eval call gets a
     *  composed `enclosingSource` chained off this one.
     */
    outerEnclosingSource: String = "",
    evalLogDir: String = "",
    evalLogTimestamp: String = ""
):
  val expressionClassName: TypeName = typeName(outputClassName)

  def expressionClass(using Context): ClassSymbol =
    if packageName.isEmpty then requiredClass(outputClassName)
    else requiredClass(s"$packageName.$outputClassName")

  def evaluateMethod(using Context): Symbol =
    expressionClass.info.decl(termName("evaluate")).symbol
