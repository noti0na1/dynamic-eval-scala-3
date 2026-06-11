package dotty.tools
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
private[eval] object EvalBodyPlaceholder:
  inline def Marker: String = EvalContext.placeholder
  def emit(body: String): String = s"({ $body })"

/** Method names the eval pipeline recognises as call sites it fills
 *  in. The post-PostTyper [[EvalRewriteTyped]] phase additionally
 *  restricts `eval` / `evalSafe` to symbols owned by the `Eval`
 *  module; `agent` / `agentSafe` are user-defined generators that
 *  share the same synthetic-argument shape, so they're matched by
 *  name only.
 */
private[eval] object EvalNames:
  val EvalLike: Set[String] = Set("eval", "evalSafe", "agent", "agentSafe")
  val EvalOwned: Set[String] = Set("eval", "evalSafe")
  /** The non-throwing variants — `evalSafe` / `agentSafe`. The call's
   *  result is `EvalResult[T]` rather than `T`. [[EvalRewriteTyped]]
   *  uses this to decide whether to wrap the verify-marker in
   *  `Eval.handleCompileError(...)`.
   */
  val EvalSafeLike: Set[String] = Set("evalSafe", "agentSafe")

  // --------------------------------------------------------------
  // Synthetic binding names. Produced by [[EvalRewriteTyped]] at the
  // call site and consumed by [[ExtractEvalBody]] /
  // [[ResolveEvalAccess]] in the wrapper compile. Both sides derive
  // the name from the local definition's *source* name, which is
  // identical in the original compile and in the wrapper's
  // re-elaboration of the enclosing source.
  // --------------------------------------------------------------

  /** `classOf[C]` of a local (term-owned) class `C`. */
  def classBinding(sourceName: String): String =
    s"__evalClass_${sourceName}__"

  /** Constructor factory closure for constructor `idx` of local class
   *  `C` (primary = 0, secondaries in source order).
   */
  def ctorBinding(sourceName: String, idx: Int): String =
    s"__evalNew_${sourceName}__$$$idx"

  /** Live module instance of a local `object M` (or the synthesized
   *  companion of a local class).
   */
  def moduleBinding(sourceName: String): String =
    s"__evalModule_${sourceName}__"

  /** Key object for non-local `return` out of the eval body. */
  val ReturnKeyBinding: String = "__evalReturnKey__"
