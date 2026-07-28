package dotty.tools
package eval

import scala.collection.mutable

import dotc.ast.tpd
import dotc.ast.tpd.*
import dotc.cc.CheckCaptures
import dotc.core.Constants.Constant
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Flags
import dotc.core.Names.termName
import dotc.core.Symbols.*
import dotc.core.Types.*
import dotc.transform.MacroTransform

/** Post-`Inlining` companion to [[EvalRewriteTyped]]: appends
 *  synthetic `__evalInlined_<name>__` bindings for values the inliner
 *  introduced around an already-filled eval call.
 *
 *  [[EvalRewriteTyped]] runs before the `Inlining` phase, so its
 *  captures carry the names the user's source elaborates to (a
 *  context-function parameter for `boundary { eval(...) }`). The
 *  inliner then beta-reduces the inline call's lambda arguments,
 *  substituting those references with the expansion's internal
 *  bindings (e.g. `scala.util.boundary.apply`'s `val local` label).
 *  The wrapper compile runs the same expansion over the same source,
 *  so an eval body's reference to such a value (a body `break(42)`
 *  resolves the call site's label) lowers to a read of the
 *  *expansion's* name, which the pre-inlining capture cannot supply.
 *
 *  This phase closes the gap: for every eval call whose bindings
 *  array the rewriter filled (recognised as a `JavaSeqLiteral`
 *  argument of type `Array[Eval.Binding]`), it walks the enclosing
 *  `Inlined` nodes' val bindings and appends
 *  `Eval.bindSynthetic("__evalInlined_<name>__", <ref>)` entries,
 *  innermost layer winning on name collisions (matching the
 *  wrapper's own innermost-wins elaboration). The wrapper side
 *  ([[ExtractEvalBody]] / [[ResolveEvalAccess]]) emits the same
 *  reserved names for symbols it finds on its own `Inlined` nodes;
 *  both compiles expand the same source, so the names agree. The
 *  mangling keeps the entries clear of user names (hygiene already
 *  makes the values un-nameable from source).
 *
 *  Explicitly-supplied bindings arguments are left alone (they are
 *  not rewriter-built literals), mirroring the rewriter's
 *  all-or-nothing contract for synthetic arguments.
 */
class EvalCaptureInlined(
    maybeConfig: Option[EvalCompilerConfig] = None,
    alwaysEnabled: Boolean = false
) extends MacroTransform:

  override def phaseName: String = EvalCaptureInlined.name

  override def runsAfter: Set[String] = Set(dotc.transform.Inlining.name)

  /** Same three installers as [[EvalRewriteTyped]]: the main pipeline
   *  (gated on `-Xdynamic-eval`), the REPL (always), and the wrapper
   *  compile (config present; nested eval calls in the body need the
   *  same treatment).
   */
  override def isEnabled(using Context): Boolean =
    alwaysEnabled || maybeConfig.isDefined || ctx.settings.XdynamicEval.value

  override protected def newTransformer(using Context): Transformer = new Capture

  private class Capture extends Transformer:

    /** Expansion-introduced val bindings in scope, innermost frame
     *  first. Two producers: the `Inlined` node's own bindings
     *  (parameter proxies) and the stats of blocks *inside* an
     *  expansion region (the inline def's body locals, e.g.
     *  `boundary.apply`'s `val local`; the inliner copies them as
     *  ordinary block statements, not node bindings).
     */
    private var frames: List[List[ValDef]] = Nil

    /** Inside an inline expansion's own code. A nested
     *  `Inlined(EmptyTree, ...)` region marks beta-reduced call-site
     *  code (the user's lambda argument): its blocks are user code,
     *  already captured under their source names by the rewriter,
     *  and must not be re-captured here.
     */
    private var inExpansion: Boolean = false

    private def collectVals(stats: List[Tree])(using Context): List[ValDef] =
      stats.collect {
        // Vars would need the `VarRef` facade plumbing; not built
        // here (an inline-internal var written from a body is not a
        // known use case), and `ExtractEvalBody` diagnoses an
        // unmatched reference cleanly. Same for `def`-shaped by-name
        // argument proxies.
        case vd: ValDef
            if !vd.name.isEmpty
            && !vd.symbol.isOneOf(Flags.Erased | Flags.Module | Flags.Mutable) =>
          vd
      }

    // Resolved once per unit; `getModuleIfDefined` so a program
    // compiled with `-Xdynamic-eval` but without the eval API on the
    // classpath degrades to a no-op instead of crashing.
    private var apiResolved = false
    private var evalModuleCls: Symbol = NoSymbol
    private var bindSyntheticFn: Symbol = NoSymbol
    private var withInheritedFn: Symbol = NoSymbol
    private var evalLikeAnnot: Symbol = NoSymbol
    private var evalSafeLikeAnnot: Symbol = NoSymbol

    private def resolveApi()(using Context): Unit =
      if !apiResolved then
        apiResolved = true
        val mod = getModuleIfDefined("dotty.tools.eval.Eval")
        if mod.exists then
          evalModuleCls = mod.moduleClass
          bindSyntheticFn = mod.requiredMethod("bindSynthetic")
          withInheritedFn = mod.requiredMethod("withInheritedHandles")
        evalLikeAnnot = getClassIfDefined("dotty.tools.eval.evalLike")
        evalSafeLikeAnnot = getClassIfDefined("dotty.tools.eval.evalSafeLike")

    private def isEvalLike(sym: Symbol)(using Context): Boolean =
      sym.exists && bindSyntheticFn.exists && {
        (sym.maybeOwner == evalModuleCls
          && (sym.name == termName("eval") || sym.name == termName("evalSafe")))
        || (evalLikeAnnot.exists && sym.hasAnnotation(evalLikeAnnot))
        || (evalSafeLikeAnnot.exists && sym.hasAnnotation(evalSafeLikeAnnot))
      }

    /** A rewriter-built bindings argument: a `JavaSeqLiteral` whose
     *  element type is `Eval.Binding`, possibly inside the
     *  `Eval.withInheritedHandles(<defsObject>, <literal>)` wrap the
     *  rewriter routes every array through. Returns the literal and
     *  a rebuild that re-establishes the original shape around a
     *  replacement literal.
     */
    private def filledBindingsArg(arg: Tree)(using Context): Option[(JavaSeqLiteral, JavaSeqLiteral => Tree)] =
      def isBindingsLiteral(lit: JavaSeqLiteral): Boolean =
        val elem = lit.elemtpt.tpe.typeSymbol
        elem.name.toString == "Binding" && elem.maybeOwner == evalModuleCls
      arg match
        case lit: JavaSeqLiteral if isBindingsLiteral(lit) =>
          Some((lit, identity))
        case app @ Apply(fun, List(defsObj, lit: JavaSeqLiteral))
            if withInheritedFn.exists && fun.symbol == withInheritedFn && isBindingsLiteral(lit) =>
          Some((lit, newLit => cpy.Apply(app)(fun, List(defsObj, newLit))))
        case _ => None

    override def transform(tree: Tree)(using Context): Tree = tree match
      case tree: Inlined =>
        val saved = inExpansion
        inExpansion = !tree.call.isEmpty
        try
          if !inExpansion then super.transform(tree)
          else
            // The node bindings' own right-hand sides are transformed
            // *without* their frame: a non-inline function argument
            // becomes a proxy val whose rhs is the (not yet
            // beta-reduced) argument closure, and an eval call inside
            // it must not capture the proxy being defined (a self
            // read of an uninitialized local; the closure's own
            // parameters already carry the values it needs). Only the
            // expansion, where the bindings are initialized, sees the
            // frame.
            val bindings1 = tree.bindings.map(b => transform(b).asInstanceOf[tpd.MemberDef])
            val frame = collectVals(tree.bindings)
            val expansion1 =
              if frame.isEmpty then transform(tree.expansion)
              else
                frames = frame :: frames
                try transform(tree.expansion)
                finally frames = frames.tail
            cpy.Inlined(tree)(tree.call, bindings1, expansion1)
        finally inExpansion = saved

      case tree: Block if inExpansion =>
        val frame = collectVals(tree.stats)
        if frame.isEmpty then super.transform(tree)
        else
          frames = frame :: frames
          try super.transform(tree)
          finally frames = frames.tail

      case app: Apply =>
        resolveApi()
        val app1 = super.transform(app)
        app1 match
          case app1: Apply
              if frames.nonEmpty && isEvalLike(methPart(app1).symbol) =>
            appendInlinedBindings(app1)
          case _ => app1

      case _ => super.transform(tree)

    private def appendInlinedBindings(app: Apply)(using Context): Tree =
      val argIdx = app.args.indexWhere(a => filledBindingsArg(a).isDefined)
      if argIdx < 0 then return app
      val (lit, rebuild) = filledBindingsArg(app.args(argIdx)).get
      val span = app.span
      val seen = mutable.Set.empty[String]
      val appended = frames.flatMap { frame =>
        frame.flatMap { vd =>
          val name = EvalNames.inlinedBinding(vd.name)
          if seen.add(name) then
            val nameLit = Literal(Constant(name)).withSpan(span)
            val tpeLit = Literal(Constant("")).withSpan(span)
            val bind = ref(bindSyntheticFn)
              .appliedTo(nameLit, ref(vd.symbol).withSpan(span), tpeLit)
              .withSpan(span)
              // Same capture-checking exemption as the rewriter's
              // bind calls: the reference only travels to the body,
              // which is rechecked in its own context.
              .withAttachment(CheckCaptures.DiscardUses, ())
            bind :: Nil
          else Nil
        }
      }
      if appended.isEmpty then app
      else
        val newLit = cpy.SeqLiteral(lit)(lit.elems ++ appended, lit.elemtpt)
        cpy.Apply(app)(app.fun, app.args.updated(argIdx, rebuild(newLit.asInstanceOf[JavaSeqLiteral])))

  end Capture

object EvalCaptureInlined:
  val name: String = "evalCaptureInlined"
