package dotty.tools
package eval

import scala.annotation.StaticAnnotation

/** Marks a user-defined function whose calls should capture the dynamic-eval
 *  context. [[EvalRewriteTyped]] fills parameters named `bindings`,
 *  `expectedType`, and `enclosingSource`, and leaves all other parameters
 *  unchanged.
 *
 *  The three parameters must have exactly those names and the types
 *  `Array[Eval.Binding]`, `String`, and `String`. Each must have a default
 *  value. A call must either omit all three parameters, allowing the compiler
 *  to fill them, or supply all three explicitly. Partially supplied context
 *  is rejected to avoid combining caller-provided and generated values.
 *
 *  Use [[evalSafeLike]] when the function returns `EvalResult[T]`.
 */
final class evalLike extends StaticAnnotation

/** Like [[evalLike]], but for a function returning `EvalResult[T]`.
 *  Verification wraps the spliced body with `Eval.handleCompileError` so the
 *  enclosing source is checked against `EvalResult[T]` rather than `T`.
 */
final class evalSafeLike extends StaticAnnotation
