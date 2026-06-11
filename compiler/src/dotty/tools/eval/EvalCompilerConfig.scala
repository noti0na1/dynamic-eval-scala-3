package dotty.tools
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
    evalLogTimestamp: String = "",
    /** True when the call comes from the standalone runtime path (an
     *  ordinary program compiled with `-Xdynamic-eval`, no REPL
     *  session). Turns on [[SpliceEvalBody]]'s class-method lift:
     *  the program's classes are on the runtime classpath, exactly
     *  like REPL line wrappers are during a session, so the wrapper
     *  must not re-mint them.
     */
    standalone: Boolean = false
):
  val expressionClassName: TypeName = typeName(outputClassName)

  /** Names of all bindings the call site captured (visible and
   *  synthetic alike). The inner compile consults this to decide
   *  whether a term-owned class/module in the wrapper is *linked*
   *  (i.e. has a `__evalClass_…__` / `__evalModule_…__` /
   *  `__evalNew_…__$i` synthetic binding carrying the original
   *  runtime entity) and whether a non-local `return` can be
   *  honoured (`__evalReturnKey__` present).
   */
  val bindingNames: Set[String] = initialScope.map(_._1).toSet

  /** True when the call site wrapped the eval call in a
   *  non-local-return catch and passed the key binding, so
   *  [[ExtractEvalBody]] may lower a body `return` to an
   *  [[EvalNonLocalReturn]] throw.
   */
  def hasReturnKey: Boolean = bindingNames.contains(EvalNames.ReturnKeyBinding)

  def expressionClass(using Context): ClassSymbol =
    if packageName.isEmpty then requiredClass(outputClassName)
    else requiredClass(s"$packageName.$outputClassName")

  def evaluateMethod(using Context): Symbol =
    expressionClass.info.decl(termName("evaluate")).symbol
