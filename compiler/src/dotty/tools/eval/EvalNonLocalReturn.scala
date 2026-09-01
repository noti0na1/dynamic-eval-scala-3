package dotty.tools
package eval

/** Control-flow exception used for a `return` from an eval body to its
 *  enclosing method.
 *
 *  [[ExtractEvalBody]] replaces the return with a throw. A catch inserted by
 *  [[EvalRewriteTyped]] recognizes the per-invocation key and performs the
 *  actual return. Distinct keys keep recursive invocations separate.
 *
 *  Extending `ControlThrowable` avoids a stack trace and excludes the exception
 *  from `NonFatal`. The type is shared across the eval classloader boundary.
 */
final class EvalNonLocalReturn(val key: AnyRef, val value: Any)
    extends scala.util.control.ControlThrowable
