package dotty.tools
package eval

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Types.*

/** Per-compilation state shared by [[ExtractEvalBody]] and
 *  [[ResolveEvalAccess]]. `symbol` anchors body-local ownership and
 *  `classOwners` records enclosing classes from innermost to outermost.
 */
private[eval] class EvalStore:
  var symbol: TermSymbol | Null = null
  var classOwners: Seq[ClassSymbol] = Seq.empty

  /** Wrapper-side symbols of linked local classes, mapped to their source
   *  names. Symbol identity remains stable when LambdaLift changes names.
   */
  var linkedClasses: Map[Symbol, String] = Map.empty

  /** Linked local modules. Both the module value and module class map to the
   *  same source name because later phases may refer to either symbol.
   */
  var linkedModules: Map[Symbol, String] = Map.empty

  /** Classes declared in the body. Extraction records them by symbol so Resolve
   *  can find them after LambdaLift and Flatten move them out of the expression
   *  class.
   */
  var bodyLocalClasses: Set[Symbol] = Set.empty

  /** Symbols introduced by inline expansion, mapped to the binding names
   *  captured by [[EvalCaptureInlined]].
   */
  var inlinedBindingNames: Map[Symbol, String] = Map.empty

  /** Source name of the linked class `tpe` refers to, if any. */
  def linkedClassName(tpe: Type)(using Context): Option[String] =
    if linkedClasses.isEmpty then None
    else linkedClasses.get(tpe.typeSymbol)

  /** Returns the linked element's source name and array depth. Handles both
   *  `AppliedType(Array, ...)` and post-erasure `JavaArrayType` shapes.
   */
  def linkedArrayElemInfo(tpe: Type)(using Context): Option[(String, Int)] =
    if linkedClasses.isEmpty then None
    else
      def elem(p: Type): Type = p match
        case AppliedType(tycon, arg :: Nil) if tycon.typeSymbol == defn.ArrayClass => arg
        case JavaArrayType(arg) => arg
        case _ => NoType
      var dims = 0
      var cur = tpe
      var e = elem(cur)
      while e.exists do
        dims += 1
        cur = e.widenDealias
        e = elem(cur)
      if dims == 0 then None
      else linkedClasses.get(cur.typeSymbol).map((_, dims))

  def hasLinked: Boolean =
    linkedClasses.nonEmpty || linkedModules.nonEmpty

  /** Whether a type part refers to a linked class or module. */
  def isLinkedPart(p: Type)(using Context): Boolean =
    p match
      case p: TermRef =>
        linkedModules.contains(p.symbol)
      case _ =>
        val sym = p.typeSymbol
        linkedClasses.contains(sym) || linkedModules.contains(sym)

  /** Replaces linked entities in `info` with `Object`. Runtime values belong to
   *  the original classes, so generated descriptors must not name the wrapper's
   *  re-elaborated copies.
   */
  def eraseLinkedRefs(info: Type)(using Context): Type =
    val mapper = new TypeMap:
      def apply(tp: Type): Type = tp match
        case tp: TermRef if linkedModules.contains(tp.symbol) =>
          defn.ObjectType
        case tp: TypeRef
            if linkedClasses.contains(tp.symbol) || linkedModules.contains(tp.symbol) =>
          defn.ObjectType
        case tp: AppliedType if linkedClasses.contains(tp.tycon.typeSymbol) =>
          defn.ObjectType
        case _ => mapOver(tp)
    mapper(info)

  def mentionsLinkedRef(info: Type)(using Context): Boolean =
    hasLinked && info.existsPart(isLinkedPart)

  def store(exprSym: Symbol)(using Context): Unit =
    symbol = exprSym.asTerm
    classOwners = exprSym.ownersIterator.collect { case cls: ClassSymbol => cls }.toSeq
