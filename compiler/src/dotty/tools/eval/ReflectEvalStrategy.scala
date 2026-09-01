package dotty.tools
package eval

import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.Property.StickyKey

/** Describes how [[ResolveEvalAccess]] should lower a `reflectEval` placeholder
 *  inserted by [[ExtractEvalBody]]. By-name flags from the debugger equivalent
 *  are unnecessary because extraction runs after `ElimByName`.
 */
private[eval] enum ReflectEvalStrategy:
  case This(cls: ClassSymbol)
  case LocalValue(variable: TermSymbol)
  case LocalValueAssign(variable: TermSymbol)
  case MethodCapture(method: TermSymbol)
  /** `useReceiverClass` selects the receiver's runtime class instead of the
   *  encoded owner. Extraction records it before LambdaLift flattens owners.
   */
  case Field(field: TermSymbol, useReceiverClass: Boolean = false)
  case FieldAssign(field: TermSymbol, useReceiverClass: Boolean = false)
  case MethodCall(method: TermSymbol, useReceiverClass: Boolean = false)
  /** Constructs a linked local class through its captured factory, producing
   *  an instance of the original class rather than the wrapper's copy.
   */
  case ConstructLocal(bindingName: String)
  /** Allocates an array whose ultimate component is the original linked class.
   */
  case NewLinkedArray(sourceName: String, dims: Int)
  /** Reads a compiler-generated binding by its exact name. */
  case BindingValue(bindingName: String)

private[eval] object ReflectEvalStrategy extends StickyKey[ReflectEvalStrategy]
