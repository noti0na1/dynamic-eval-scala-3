package dotty.tools
package eval

/** Call-site information passed to the generator overload of [[Eval.eval]].
 *
 *  A generator can inspect the enclosing source and visible bindings before
 *  returning the code to compile. The generated code is checked under the
 *  current compiler settings, including safe mode when enabled. The generator
 *  itself is ordinary host code and may inspect each binding's runtime value.
 *
 *  ```
 *  val r: Int = eval { ctx =>
 *    llm.complete(
 *      prompt = s"Fill the placeholder ${ctx.placeholder} in:\n${ctx.enclosingSource}"
 *    )
 *  }
 *  ```
 *
 *  This class is part of the classloader-neutral eval API, so its public
 *  surface uses only JVM types.
 *
 *  @param bindings0 captured bindings; compiler-generated bindings are removed
 *                    from [[bindings]].
 *  @param expectedType source representation of the requested result type, or
 *                      an empty string when no usable type was recorded.
 *  @param enclosingSource enclosing statement with the eval call replaced by
 *                         [[placeholder]], or an empty string when unavailable.
 */
final class EvalContext(
    bindings0: Array[Eval.Binding],
    val expectedType: String,
    val enclosingSource: String
):
  /** Marker that replaces the eval call in [[enclosingSource]].
   */
  def placeholder: String = EvalContext.placeholder

  /** User-visible bindings in scope at the call site. Each binding includes
   *  its static source type when renderable and its runtime value. Internal
   *  links such as enclosing instances and local-class factories are omitted.
   */
  val bindings: Array[Eval.Binding] =
    bindings0.filter(b => !b.isSynthetic)

  override def toString: String =
    s"EvalContext(bindings=${bindings.length}, expectedType=$expectedType, enclosingSource=${enclosingSource.length} chars)"

object EvalContext:
  /** Marker inserted into the recorded enclosing source. */
  val placeholder: String = "__evalBodyPlaceholder__"
