package dotty.tools
package repl
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
 *  Lives in `dotty.tools.repl` so the eval-output classloader routes
 *  it through the parent loader and there's a single shared `Class`
 *  on both sides of the eval / REPL boundary (see BetterEval.md
 *  "Classloader bridging").
 *
 *  @param enclosingSource Source text of the enclosing top-level
 *                         statement at the eval call site, with the
 *                         eval call's span replaced by `placeholder`.
 *                         Empty when the rewriter couldn't compute a
 *                         slice (e.g. a programmatic call to
 *                         `Eval.eval` from outside the REPL).
 *  @param bindings        The bindings the rewriter captured at the
 *                         call site (lambda parameters, block-local
 *                         vals, etc.). Useful for an agent that wants
 *                         to mention the in-scope names by name.
 */
final class EvalContext(
    val enclosingSource: String,
    val bindings: Array[Eval.Binding]
):
  /** The string the rewriter substituted into `enclosingSource` at the
   *  eval call's location. An agent that wants to splice generated
   *  code into the enclosing source can do
   *  `enclosingSource.replace(placeholder, generated)`.
   */
  def placeholder: String = EvalContext.placeholder

  override def toString: String =
    s"EvalContext(enclosingSource=${enclosingSource.length} chars, bindings=${bindings.length})"

object EvalContext:
  /** The marker the parser-stage rewriter substitutes into the
   *  `enclosingSource` text at each eval call site.
   */
  val placeholder: String = "__evalBodyPlaceholder__"
