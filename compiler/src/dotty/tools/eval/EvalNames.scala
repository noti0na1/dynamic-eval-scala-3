package dotty.tools
package eval

import dotty.tools.dotc.core.Names.Name

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

/** Reserved binding names shared between the call-site rewriter and
 *  the wrapper compile. Call-site classification itself is not name
 *  based: [[EvalRewriteTyped]]'s `classifyCall` matches `eval` /
 *  `evalSafe` by symbol (owner must be the `Eval` module) and
 *  `@evalLike` / `@evalSafeLike` wrappers by annotation.
 */
private[eval] object EvalNames:

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

  /** Reserved prefix of the synthetic object a `topLevel` defs
   *  compile wraps the definitions in. The rewriter recognises a
   *  call site compiled inside such an object by this prefix, so the
   *  capture re-attaches the object's own handle at runtime through
   *  the registry — the handle itself does not exist yet while the
   *  defs compile.
   */
  val TopLevelObjectPrefix: String = "__EvalTopLevel_"

  /** Reserved prefix for [[Eval.TopLevel]] handle bindings. A
   *  `TopLevel.eval` call appends one synthetic binding per handle
   *  (name built by [[topLevelBinding]], value the handle itself);
   *  [[EvalAdapter.evalIsolated]] extracts them to put the handle's
   *  compiled definitions on the wrapper compile's classpath, import
   *  them into the wrapper, and chain the handle's classloader for
   *  the wrapper load. The generated code never reads the binding.
   */
  val TopLevelBindingPrefix: String = "__evalTopLevel_"

  /** Handle binding name for the top-level defs object `objectName`.
   *  The object name is unique per handle (UUID-flavoured), so the
   *  binding name discriminates handles in the wrapper cache key via
   *  the bindings fingerprint.
   */
  def topLevelBinding(objectName: String): String =
    s"$TopLevelBindingPrefix${objectName}__"

  /** Reserved prefix for captured enclosing instances. The bare
   *  `__this__` binding holds the call site's innermost `this`; the
   *  qualified `__this__<C>` form (built by [[thisBinding]]) holds
   *  the enclosing instance of class `C` further out, captured once
   *  per enclosing class.
   */
  val ThisBinding: String = "__this__"

  /** `__this__<C>` binding name for the enclosing instance of the
   *  class named `clsName`.
   */
  def thisBinding(clsName: Name | String): String = s"$ThisBinding$clsName"

  /** Binding name for a value introduced by *inline expansion* around
   *  the eval call site: an `Inlined` node's binding (a parameter
   *  proxy or the inline def's own local, e.g.
   *  `scala.util.boundary.apply`'s `val local` label). Inline calls
   *  expand at the typer, so the outer and the wrapper compile see
   *  the same expansion and agree on `name`; the binding is reserved
   *  (hygiene makes the value un-nameable from source) and the
   *  mangling keeps it from colliding with a user local that happens
   *  to share the inline def's internal name.
   */
  def inlinedBinding(name: Name | String): String = s"__evalInlined_${name}__"
