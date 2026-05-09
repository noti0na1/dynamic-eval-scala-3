package dotty.tools
package repl
package eval

/** Marker text the eval pipeline splices into `enclosingSource` and
 *  later replaces with the (now known) eval body. Picked so it stays
 *  a single Scala identifier, valid in expression position, with no
 *  collision risk against ordinary user names.
 *
 *  `Marker` is just an alias for [[EvalContext.placeholder]] so the
 *  enclosing-source slicing in [[EvalRewriteTyped]] and the wrapper-
 *  side helpers stay in sync. `emit` wraps the body in parens so the
 *  splice is syntactically valid in any expression position.
 */
private[repl] object EvalBodyPlaceholder:
  inline def Marker: String = EvalContext.placeholder
  def emit(body: String): String = s"({ $body })"

/** Method names the eval pipeline recognises as call sites it fills
 *  in. The post-PostTyper [[EvalRewriteTyped]] phase additionally
 *  restricts `eval` / `evalSafe` to symbols owned by the `Eval`
 *  module; `agent` / `agentSafe` are user-defined generators that
 *  share the same synthetic-argument shape, so they're matched by
 *  name only.
 */
private[repl] object EvalNames:
  val EvalLike: Set[String] = Set("eval", "evalSafe", "agent", "agentSafe")
  val EvalOwned: Set[String] = Set("eval", "evalSafe")
  /** The non-throwing variants — `evalSafe` / `agentSafe`. The call's
   *  result is `EvalResult[T]` rather than `T`. [[EvalRewriteTyped]]
   *  uses this to decide whether to wrap the verify-marker in
   *  `Eval.handleCompileError(...)`.
   */
  val EvalSafeLike: Set[String] = Set("evalSafe", "agentSafe")
