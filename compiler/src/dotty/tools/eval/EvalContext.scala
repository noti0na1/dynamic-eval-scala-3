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
 *  @param enclosingSource Source text of the enclosing top-level
 *                         statement at the eval call site, with the
 *                         eval call's span replaced by `placeholder`.
 *                         Empty when the rewriter couldn't compute a
 *                         slice (e.g. a programmatic call to
 *                         `Eval.eval` from outside the REPL).
 *  @param allBindings     Every binding the rewriter captured at the
 *                         call site, including the compiler-only
 *                         synthetic ones ([[Eval.Binding.isSynthetic]]).
 */
final class EvalContext(
    val enclosingSource: String,
    val allBindings: Array[Eval.Binding]
):
  /** The string the rewriter substituted into `enclosingSource` at the
   *  eval call's location. An agent that wants to splice generated
   *  code into the enclosing source can do
   *  `enclosingSource.replace(placeholder, generated)`.
   */
  def placeholder: String = EvalContext.placeholder

  /** The user-nameable bindings in scope at the call site (lambda
   *  parameters, block-local vals, etc.): the list an agent should
   *  show to an LLM as "names in scope". Compiler-only synthetic
   *  bindings (`__this__*`, local-class links, constructor factories,
   *  the return key) are filtered out; see [[allBindings]] for the
   *  raw array.
   */
  def bindings: Array[Eval.Binding] =
    allBindings.filter(b => !b.isSynthetic)

  override def toString: String =
    s"EvalContext(enclosingSource=${enclosingSource.length} chars, bindings=${allBindings.length})"

object EvalContext:
  /** The marker the [[EvalRewriteTyped]] rewriter substitutes into
   *  the `enclosingSource` text at each eval call site.
   */
  val placeholder: String = "__evalBodyPlaceholder__"
