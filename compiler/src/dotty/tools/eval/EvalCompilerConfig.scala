package dotty.tools
package eval

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Names.*

import java.util.function.Consumer

/** Configuration for one eval compilation. `outputClassName` must be unique
 *  within its classloader; callers normally include a UUID.
 */
private[eval] case class EvalCompilerConfig(
    outputClassName: String = "",
    body: String = "",
    errorReporter: Consumer[String] = (_: String) => (),
    testMode: Boolean = false,
    expectedType: String = "",
    /** Names captured by the outer eval call. The inner compile uses them to
     *  recognize linked runtime entities and the non-local-return key.
     */
    initialBindingNames: Array[String] = Array.empty,
    /** The enclosing-source slice of the outer eval. Nested eval calls compose
     *  their slices with it to retain the complete lexical context.
     */
    outerEnclosingSource: String = "",
    evalLogDir: String = "",
    evalLogTimestamp: String = "",
    /** Enables class-member lifting for standalone programs, whose original
     *  classes are already available on the runtime classpath.
     */
    standalone: Boolean = false
):
  /** Names of all captured bindings, including compiler-generated links. */
  val bindingNames: Set[String] = initialBindingNames.toSet

  /** Whether non-local returns can be lowered through [[EvalNonLocalReturn]]. */
  def hasReturnKey: Boolean = bindingNames.contains(EvalNames.ReturnKeyBinding)

  /** The generated expression class, always placed in the empty package. */
  def expressionClass(using Context): ClassSymbol =
    requiredClass(outputClassName)

  def evaluateMethod(using Context): Symbol =
    expressionClass.info.decl(termName("evaluate")).symbol
