package dotty.tools
package eval

import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.Property.StickyKey

/** Strategy attached to each `reflectEval(...)` placeholder by
 *  [[ExtractEvalBody]] and consumed by [[ResolveEvalAccess]], which
 *  lowers the placeholder into the matching reflective accessor call
 *  on `__Expression`. Mirrors `dotty.tools.debug.ReflectEvalStrategy`,
 *  minus that enum's by-name flags: extract runs after `ElimByName`,
 *  so by-name values are already ordinary function values there and
 *  an `ExprType` can no longer occur.
 */
private[eval] enum ReflectEvalStrategy:
  case This(cls: ClassSymbol)
  case LocalValue(variable: TermSymbol)
  case LocalValueAssign(variable: TermSymbol)
  case MethodCapture(method: TermSymbol)
  /** `useReceiverClass`: lower className to "" so the runtime helper
   *  walks `obj.getClass` instead of the wrapper's encoded enclosing
   *  class. Set when the field's class was term-owned at extract time;
   *  by the time ResolveEvalAccess runs, LambdaLift has flattened the
   *  owner to the package, so we can no longer detect this from the
   *  symbol; the flag must be precomputed here.
   */
  case Field(field: TermSymbol, useReceiverClass: Boolean = false)
  case FieldAssign(field: TermSymbol, useReceiverClass: Boolean = false)
  case MethodCall(method: TermSymbol, useReceiverClass: Boolean = false)
  /** Construct an instance of a *linked* local class by applying the
   *  call-site factory closure stored under `bindingName`
   *  (`__evalNew_<C>__$<i>`). The factory closes over the class's
   *  captured environment, so the instance belongs to the *original*
   *  lifted class rather than the wrapper's re-elaborated copy.
   */
  case ConstructLocal(bindingName: String)
  /** Read a synthetic binding by its exact name: a linked module
   *  instance (`__evalModule_<M>__`), an enclosing instance
   *  (`__this__<C>`), or the non-local-return key
   *  (`__evalReturnKey__`).
   */
  case BindingValue(bindingName: String)

private[eval] object ReflectEvalStrategy extends StickyKey[ReflectEvalStrategy]
