package dotty.tools
package repl
package eval

import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.Property.StickyKey

/** Strategy attached to each `reflectEval(...)` placeholder by
 *  [[ExtractEvalBody]] and consumed by [[ResolveEvalAccess]], which
 *  lowers the placeholder into the matching reflective accessor call
 *  on `__Expression`. Mirrors `dotty.tools.debug.ReflectEvalStrategy`.
 */
private[eval] enum ReflectEvalStrategy:
  case This(cls: ClassSymbol)
  case Outer(outerCls: ClassSymbol)
  case LocalValue(variable: TermSymbol, isByName: Boolean)
  case LocalValueAssign(variable: TermSymbol)
  case MethodCapture(variable: TermSymbol, method: TermSymbol, isByName: Boolean)
  /** `useReceiverClass`: lower className to "" so the runtime helper
   *  walks `obj.getClass` instead of the wrapper's encoded enclosing
   *  class. Set when the field's class was term-owned at extract time;
   *  by the time ResolveEvalAccess runs, LambdaLift has flattened the
   *  owner to the package, so we can no longer detect this from the
   *  symbol — the flag must be precomputed here.
   */
  case Field(field: TermSymbol, isByName: Boolean, useReceiverClass: Boolean = false)
  case FieldAssign(field: TermSymbol, useReceiverClass: Boolean = false)
  case MethodCall(method: TermSymbol, useReceiverClass: Boolean = false)

private[eval] object ReflectEvalStrategy extends StickyKey[ReflectEvalStrategy]
