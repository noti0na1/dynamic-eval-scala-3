package dotty.tools
package eval

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Types.*

/** Per-compile state shared between [[ExtractEvalBody]] and
 *  [[ResolveEvalAccess]]. Mirrors `dotty.tools.debug.ExpressionStore`.
 *
 *  Populated by Extract once it finds the spliced `val __evalResult`:
 *
 *    - `symbol`: the val symbol; used as the owner-chain anchor when
 *      classifying body-local vs outer references.
 *    - `classOwners`: enclosing classes from innermost out; Extract's
 *      `thisOrOuterValue` consults this chain to decide between a
 *      `This` placeholder (innermost class) and a `__this__<C>`
 *      binding read (outer classes).
 */
private[eval] class EvalStore:
  var symbol: TermSymbol | Null = null
  var classOwners: Seq[ClassSymbol] = Seq.empty

  /** *Linked* local classes: term-owned classes re-elaborated by the
   *  wrapper compile whose original runtime entities the call site
   *  captured as synthetic bindings (`__evalClass_<C>__` /
   *  `__evalNew_<C>__$i`). Keyed by the wrapper-side class symbol
   *  (symbol identity is stable across phases, while names are not;
   *  LambdaLift freshens lifted-class names), with the *source*
   *  name as value, from which both sides derive the binding names.
   *  Populated by [[ExtractEvalBody]]; consumed there and by
   *  [[ResolveEvalAccess]]'s post-erasure sweep.
   */
  var linkedClasses: Map[Symbol, String] = Map.empty

  /** Same for linked local modules (`__evalModule_<M>__`): both the
   *  module *val* symbol (term references) and the module *class*
   *  symbol (member-call owners) map to the source name.
   */
  var linkedModules: Map[Symbol, String] = Map.empty

  /** Classes declared *inside* the eval body. Recorded at extract
   *  time (symbol identity is stable across phases): by the time
   *  [[ResolveEvalAccess]] runs, LambdaLift/Flatten have moved them
   *  out of `__Expression`, so the placeholder sweep can no longer
   *  find them by position and consults this set instead.
   */
  var bodyLocalClasses: Set[Symbol] = Set.empty

  /** Source name of the linked class `tpe` refers to, if any. */
  def linkedClassName(tpe: Type)(using Context): Option[String] =
    if linkedClasses.isEmpty then None
    else linkedClasses.get(tpe.typeSymbol)

  def hasLinked: Boolean =
    linkedClasses.nonEmpty || linkedModules.nonEmpty

  /** True when the type *part* `p` refers to a linked entity: a
   *  linked class, a linked module's class, or (as a TermRef) the
   *  linked module val itself.
   */
  def isLinkedPart(p: Type)(using Context): Boolean =
    p match
      case p: TermRef =>
        linkedModules.contains(p.symbol)
      case _ =>
        val sym = p.typeSymbol
        linkedClasses.contains(sym) || linkedModules.contains(sym)

  /** Substitute every linked-entity occurrence in `info` with
   *  `Object`. Shared by [[ExtractEvalBody]] (body-local symbols
   *  that exist when it runs, including PatternMatcher binders) and
   *  [[ResolveEvalAccess]] (symbols minted by phases that run after
   *  extract, e.g. LetOverApply receiver temps, Memoize fields, and
   *  erasure temps): the runtime values flowing through these
   *  positions belong to the *original* lifted classes, so a
   *  descriptor or checkcast naming the wrapper's re-elaborated
   *  copy would be wrong.
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
