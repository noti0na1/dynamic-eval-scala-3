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

/** Captures values introduced by inlining around an eval call already handled
 *  by [[EvalRewriteTyped]].
 *
 *  The initial rewrite runs before inlining, but the generated wrapper repeats
 *  the same expansion and may refer to expansion-local symbols. For example, a
 *  `boundary` label in an eval body resolves to the inliner's internal binding
 *  rather than the context-function parameter captured earlier.
 *
 *  This phase appends those symbols under reserved names to compiler-generated
 *  bindings arrays. The wrapper compile derives the same names, and the nearest
 *  expansion wins on collisions to match lexical lookup.
 *
 *  Explicitly supplied bindings are left unchanged.
 */
class EvalCaptureInlined(
    maybeConfig: Option[EvalCompilerConfig] = None,
    alwaysEnabled: Boolean = false
) extends MacroTransform:

  override def phaseName: String = EvalCaptureInlined.name

  override def runsAfter: Set[String] = Set(dotc.transform.Inlining.name)

  /** Enabled for regular dynamic eval, the REPL, and nested wrapper compiles. */
  override def isEnabled(using Context): Boolean =
    alwaysEnabled || maybeConfig.isDefined || ctx.settings.XdynamicEval.value

  override protected def newTransformer(using Context): Transformer = new Capture

  private class Capture extends Transformer:

    /** Expansion-local values in scope, innermost frame first. These include
     *  `Inlined` parameter proxies and block-local values copied from the inline
     *  method body.
     */
    private var frames: List[List[ValDef]] = Nil

    /** Whether the traversal is in expansion code. `Inlined(EmptyTree, ...)`
     *  represents call-site code, whose source bindings were captured earlier.
     */
    private var inExpansion: Boolean = false

    private def withFrame[T](frame: List[ValDef])(op: => T): T =
      if frame.isEmpty then op
      else
        frames = frame :: frames
        try op
        finally frames = frames.tail

    private def collectVals(stats: List[Tree])(using Context): List[ValDef] =
      stats.collect {
        // Mutable values require VarRef and by-name proxies require def capture;
        // unsupported references receive the normal missing-binding diagnostic.
        case vd: ValDef
            if !vd.name.isEmpty
            && !vd.symbol.isOneOf(Flags.Erased | Flags.Module | Flags.Mutable) =>
          vd
      }

    // Resolve once per unit. Optional lookups make the phase a no-op when a
    // compilation enables the flag without the eval API on its classpath.
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

    /** Finds a compiler-generated bindings literal, optionally wrapped by
     *  `withInheritedHandles`, and returns a function that preserves the wrapper.
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
            // A node binding is not in scope in its own right-hand side. Only the
            // initialized expansion sees the frame, avoiding self-captures of
            // parameter proxies.
            val bindings1 = tree.bindings.map(b => transform(b).asInstanceOf[tpd.MemberDef])
            val frame = collectVals(tree.bindings)
            val expansion1 = withFrame(frame)(transform(tree.expansion))
            cpy.Inlined(tree)(tree.call, bindings1, expansion1)
        finally inExpansion = saved

      case tree: Block if inExpansion =>
        val blockVals = collectVals(tree.stats)
        if blockVals.isEmpty then super.transform(tree)
        else
          // Extend the frame after each statement: later values are not visible
          // in earlier initializers.
          var preceding: List[ValDef] = Nil
          val stats1 = tree.stats.map { stat =>
            val stat1 = withFrame(preceding)(transform(stat))
            preceding = preceding ++ collectVals(stat :: Nil)
            stat1
          }
          val expr1 = withFrame(preceding)(transform(tree.expr))
          cpy.Block(tree)(stats1, expr1)

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
              // The captured reference is rechecked when the body is compiled.
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
