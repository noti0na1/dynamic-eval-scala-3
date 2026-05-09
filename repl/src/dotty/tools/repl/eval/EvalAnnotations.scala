package dotty.tools
package repl
package eval

import scala.annotation.StaticAnnotation

/** Marks a user-defined function as an eval-like generator. The
 *  post-PostTyper [[EvalRewriteTyped]] phase rewrites calls to the
 *  annotated function the same way it rewrites `Eval.eval`: it
 *  matches three synthetic parameter slots — `bindings`,
 *  `expectedType`, `enclosingSource` — *by name* and fills them
 *  with the typed scope's bindings array, the rendered `[T]`, and
 *  the encl-source slice respectively. Other parameters (the user's
 *  task / body / additional config like `maxAttempts`) are left
 *  untouched.
 *
 *  The annotated function must declare parameters named exactly
 *  `bindings: Array[Eval.Binding]`, `expectedType: String`, and
 *  `enclosingSource: String`, each with a default so call sites can
 *  omit them. The result type is whatever the user's body produces.
 *
 *  Call-site rule: a call to an annotated function must have either
 *  *all three* synthetic slots default-filled (the rewriter will
 *  populate them) or *all three* explicitly supplied by the caller
 *  (the rewriter leaves them alone). Mixed states are rejected as
 *  ill-formed because the populated subset would silently override
 *  whatever the partial caller-supplied subset was trying to express.
 *
 *  See [[evalSafeLike]] for the non-throwing variant whose result
 *  type is `EvalResult[T]`.
 */
final class evalLike extends StaticAnnotation

/** Like [[evalLike]] but for the non-throwing variant: the
 *  annotated function's result type is `EvalResult[T]`. The
 *  rewriter wraps the verify-marker in `Eval.handleCompileError(...)`
 *  so the inner verification's spliced body type-checks against
 *  `EvalResult[T]` rather than `T`.
 */
final class evalSafeLike extends StaticAnnotation
