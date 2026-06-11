package dotty.tools
package eval

/** Information about the call site passed to the closure form of
 *  [[Eval.eval]].
 *
 *  The agent / LLM use case: a generator function inspects
 *  `enclosingSource` (the source of the enclosing top-level statement
 *  with this eval call's location replaced by `placeholder`), decides
 *  what code to fill in, and returns it as a String. The runtime then
 *  compiles and runs that String the same way the literal-string form
 *  does, including the capture-checking verification pass.
 *
 *  ```
 *  val r: Int = eval { ctx =>
 *    llm.complete(
 *      prompt = s"Fill the placeholder ${ctx.placeholder} in:\n${ctx.enclosingSource}"
 *    )
 *  }
 *  ```
 *
 *  Lives in `dotty.tools.eval` so the eval-output classloader routes
 *  it through the parent loader and there's a single shared `Class`
 *  on both sides of the eval / REPL boundary (see README.md
 *  "Classloader bridging").
 *
 *  The constructor parameter order mirrors the synthetic argument
 *  order of `eval[T](code, bindings, expectedType, enclosingSource)`.
 *
 *  @param bindings0       Every binding the rewriter captured at the
 *                         call site; the compiler-only synthetic ones
 *                         ([[Eval.Binding.isSynthetic]]) are filtered
 *                         out at construction, so [[bindings]] is the
 *                         user-visible list only.
 *  @param expectedType    Source-level rendering of the call's `[T]`
 *                         type argument as recorded in the typed
 *                         tree. In practice this means the explicit
 *                         type argument: a plain `eval` without one
 *                         gives the typer no lower constraint on
 *                         `T`, so `T` minimises to `Nothing` and
 *                         renders empty (typing then comes from the
 *                         splice position inside the enclosing
 *                         source). Also empty for a locally-scoped
 *                         type at a call site with no
 *                         enclosing-source slice.
 *  @param enclosingSource Source text of the enclosing top-level
 *                         statement at the eval call site, with the
 *                         eval call's span replaced by `placeholder`.
 *                         Empty when the rewriter couldn't compute a
 *                         slice (e.g. a programmatic call to
 *                         `Eval.eval` from outside the REPL).
 */
final class EvalContext(
    bindings0: Array[Eval.Binding],
    val expectedType: String,
    val enclosingSource: String
):
  /** The string the rewriter substituted into `enclosingSource` at the
   *  eval call's location. An agent that wants to splice generated
   *  code into the enclosing source can do
   *  `enclosingSource.replace(placeholder, generated)`.
   */
  def placeholder: String = EvalContext.placeholder

  /** The user-nameable bindings in scope at the call site (lambda
   *  parameters, block-local vals, etc.): the list an agent should
   *  show to an LLM as "names in scope". Each binding carries the
   *  source rendering of its static type in [[Eval.Binding.tpe]]
   *  (empty when not renderable), so an agent can present
   *  `name: Type` pairs. Compiler-only synthetic bindings
   *  (`__this__*`, local-class links, constructor factories, the
   *  return key) are not part of the user interface and are filtered
   *  out at construction.
   */
  val bindings: Array[Eval.Binding] =
    bindings0.filter(b => !b.isSynthetic)

  override def toString: String =
    s"EvalContext(bindings=${bindings.length}, expectedType=$expectedType, enclosingSource=${enclosingSource.length} chars)"

object EvalContext:
  /** The marker the [[EvalRewriteTyped]] rewriter substitutes into
   *  the `enclosingSource` text at each eval call site.
   */
  val placeholder: String = "__evalBodyPlaceholder__"
