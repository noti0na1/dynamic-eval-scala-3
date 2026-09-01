package dotty.tools
package eval

import dotty.tools.dotc.core.Names.Name

/** Wraps a body replacing [[EvalContext.placeholder]] so it remains valid in
 *  any expression position.
 */
private[eval] object EvalBodyPlaceholder:
  def emit(body: String): String = s"({ $body })"

/** Reserved binding names shared by the call-site and wrapper compiles. */
private[eval] object EvalNames:

  // Both compiles derive link names from the original source name so they
  // remain stable when compiler phases rename symbols.

  /** `classOf[C]` of a local (term-owned) class `C`. */
  def classBinding(sourceName: String): String =
    s"__evalClass_${sourceName}__"

  /** Constructor factory for constructor `idx` of local class `C`. The primary
   *  constructor is index 0, followed by secondary constructors in source order.
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

  /** Prefix of the object containing compiled [[Eval.topLevel]] definitions.
   *  Calls compiled inside it use the prefix to recover the handle at runtime.
   */
  val TopLevelObjectPrefix: String = "__EvalTopLevel_"

  /** Prefix for [[Eval.TopLevel]] handle bindings. The adapter uses these
   *  bindings to extend the wrapper classpath, imports, and loader chain.
   */
  val TopLevelBindingPrefix: String = "__evalTopLevel_"

  /** Handle binding name for the top-level definitions object. */
  def topLevelBinding(objectName: String): String =
    s"$TopLevelBindingPrefix${objectName}__"

  /** Prefix for captured enclosing instances. `__this__` denotes the nearest
   *  instance; [[thisBinding]] qualifies instances farther out.
   */
  val ThisBinding: String = "__this__"

  /** Binding name for the enclosing instance of `clsName`. */
  def thisBinding(clsName: Name | String): String = s"$ThisBinding$clsName"

  /** Binding name for a value introduced by inline expansion around the eval
   *  call. The reserved form avoids collisions with source-level locals.
   */
  def inlinedBinding(name: Name | String): String = s"__evalInlined_${name}__"
