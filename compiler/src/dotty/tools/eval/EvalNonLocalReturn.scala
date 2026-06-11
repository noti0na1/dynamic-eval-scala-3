package dotty.tools
package eval

/** Control-flow throwable carrying a `return` out of an eval body.
 *
 *  The eval body executes inside `__Expression.evaluate()`, so a
 *  `return` targeting the method enclosing the eval *call* cannot be
 *  an ordinary JVM return; the target frame is several (reflective)
 *  frames up. Instead:
 *
 *    - [[ExtractEvalBody]] lowers the body's `return expr` into
 *      `throw new EvalNonLocalReturn(<key>, expr)`, where `<key>` is
 *      the call's `__evalReturnKey__` synthetic binding;
 *    - [[EvalRewriteTyped]] wraps every eval call that sits directly
 *      inside a method with
 *      `try ... catch case ex: EvalNonLocalReturn if ex.key eq <key> =>
 *      return ex.value.asInstanceOf[R]`, allocating a fresh key
 *      object per execution so recursive frames stay distinct.
 *
 *  The throwable unwinds through `evaluate()`, the adapter's
 *  reflective `invoke` (which re-throws the
 *  `InvocationTargetException` cause), and the `Eval.eval` frame,
 *  until the matching call-site catch turns it back into an ordinary
 *  `return`.
 *
 *  Extends `ControlThrowable` so it carries no stack trace and is
 *  ignored by `NonFatal`. Lives in `dotty.tools.eval` so the
 *  eval-output classloader routes the `Class` through the shared
 *  parent loader (one `Class` on both sides of the eval boundary;
 *  see README.md "Classloader bridging").
 */
final class EvalNonLocalReturn(val key: AnyRef, val value: Any)
    extends scala.util.control.ControlThrowable
