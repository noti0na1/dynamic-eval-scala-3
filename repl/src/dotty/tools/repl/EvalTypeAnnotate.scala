package dotty.tools
package repl

import dotc.ast.tpd
import dotc.core.Constants.Constant
import dotc.core.Contexts.*
import dotc.core.Phases.Phase
import dotc.core.Symbols.{NoSymbol, Symbol, requiredModule}
import dotc.core.Types.Type

/** Post-typer phase that fills in the source-level type of every
 *  `Eval.bind` / `Eval.bindVar` call the parser-stage rewriter produced.
 *
 *  The parser stage emits each call with an empty string as the third
 *  argument: `Eval.bind("name", value, "")`. That sentinel says "the
 *  rewriter doesn't know the type yet". Once the typer has elaborated
 *  the surrounding expression we can read `value.tpe`, render it as a
 *  Scala source string, and substitute the literal so the runtime can
 *  use it directly instead of inferring a type from `value.getClass`.
 *
 *  Concretely this turns:
 *
 *  ```
 *  Eval.bind("g", g, "")
 *  ```
 *
 *  into:
 *
 *  ```
 *  Eval.bind("g", g, "Int => Int")
 *  ```
 *
 *  when `g`'s typer-side type is `Int => Int`. The wrapper module the
 *  runtime synthesises then declares `__run__(`g`: Int => Int, ...)`,
 *  which means `g(2)` inside the eval body type-checks precisely
 *  rather than returning `Any`.
 *
 *  Bindings that don't reach this phase (notably the runtime
 *  nested-eval rewrite, which acts on an untyped tree) keep the empty
 *  sentinel and the runtime falls back to `Class`-walking.
 */
class EvalTypeAnnotate extends Phase:
  import tpd.*

  def phaseName: String = "evalTypeAnnotate"

  protected def run(using Context): Unit =
    val tree = ctx.compilationUnit.tpdTree
    val transformer = new BindAnnotator
    val annotated = transformer.transform(tree)
    if annotated ne tree then ctx.compilationUnit.tpdTree = annotated

  private class BindAnnotator extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree = tree match
      case app @ Apply(fun, name :: value :: (sentinel @ Literal(Constant(""))) :: Nil)
          if isEvalBindCall(fun) =>
        val isVar = fun.symbol.name.toString == "bindVar"
        // For `bindVar`, the captured value is a `VarCell` (an
        // `AtomicReference[T]`). The runtime evaluator already wraps
        // the recorded source type back into `AtomicReference[...]`
        // when synthesising the wrapper signature, so we want the
        // inner `T`, not the cell type itself.
        val tpe = if isVar then EvalTypeAnnotate.unwrapCellType(value.tpe) else value.tpe
        val tpeStr = EvalTypeAnnotate.renderType(tpe)
        if tpeStr.isEmpty then app
        else
          val tpeLit = Literal(Constant(tpeStr)).withSpan(sentinel.span)
          cpy.Apply(app)(fun, name :: value :: tpeLit :: Nil)
      case _ =>
        super.transform(tree)

    private def isEvalBindCall(fun: Tree)(using Context): Boolean =
      val sym = fun.symbol
      sym != NoSymbol
        && (sym.name.toString == "bind" || sym.name.toString == "bindVar")
        && sym.owner == EvalTypeAnnotate.evalModuleClass
  end BindAnnotator

end EvalTypeAnnotate

object EvalTypeAnnotate:

  /** Cached lookup of `Eval`'s module class. The phase fires every line
   *  of the REPL session, so we resolve once per Run via the inContext
   *  lookup rather than per-tree.
   */
  private def evalModuleClass(using Context): Symbol =
    requiredModule("dotty.tools.repl.Eval").moduleClass

  /** If `tpe` is `java.util.concurrent.atomic.AtomicReference[X]` (the
   *  underlying type of `Eval.VarCell`), return `X`; otherwise return
   *  `tpe` unchanged. The parser-stage rewriter wraps captured vars
   *  in cells, so the typed `value` argument of every `bindVar` call
   *  reaches us as `AtomicReference[T]`. We peel that wrapper so the
   *  recorded source type is `T`, matching how the runtime synthesises
   *  the wrapper signature.
   */
  private[repl] def unwrapCellType(tpe: Type)(using Context): Type =
    import dotc.core.Types.AppliedType
    if tpe == null || !tpe.exists then return tpe
    tpe.widen match
      case at @ AppliedType(tycon, arg :: Nil)
          if tycon.typeSymbol.fullName.toString == "java.util.concurrent.atomic.AtomicReference" =>
        arg
      case _ => tpe

  /** Render `tpe` as a Scala source string suitable for splicing into
   *  the synthesised eval wrapper. Returns the empty string when the
   *  type is degenerate (NoType, ErrorType, missing) or references a
   *  symbol the eval body wouldn't be able to resolve (an enclosing
   *  method's type parameter, a locally-scoped class, etc.). The
   *  empty result means "leave the binding alone, use the runtime
   *  fallback".
   *
   *  We `widen` to collapse singleton types (a `1` literal has type
   *  `Int(1)`; we want `Int`). We deliberately do *not* dealias: type
   *  aliases the user has in scope (and that the imports the eval
   *  driver injects also bring into scope) are usually preferable to
   *  their expansions, especially for REPL-defined opaque types.
   */
  private[repl] def renderType(tpe: Type)(using Context): String =
    if tpe == null || !tpe.exists || tpe.isError then return ""
    val widened = tpe.widen
    if !widened.exists || widened.isError then return ""
    if mentionsLocallyScopedSymbol(widened) then return ""
    // Disable colours so the rendered string never contains ANSI
    // escapes that would later confuse the eval driver's parser.
    val printCtx = ctx.fresh.setSetting(ctx.settings.color, "never")
    try widened.show(using printCtx)
    catch case _: Throwable => ""

  /** True iff `tpe` mentions a symbol that wouldn't resolve in the
   *  fresh eval wrapper module. The two cases that matter in practice:
   *
   *    - **Type parameters** of an enclosing method (`def f[T](x: T)`).
   *      `x.tpe` is just `T`, which doesn't exist in the wrapper.
   *    - **Locally-defined classes** (`def f() = { class L; new L }`).
   *      `L`'s symbol is owned by `f`'s term-level scope; the wrapper
   *      can't see into that scope.
   *
   *  Anything REPL-session-level is fine: those symbols live in
   *  `rs$line$N` modules whose contents we already import. So the
   *  check is "does the type mention a symbol whose enclosing module
   *  is a *term*". REPL session symbols are owned by the wrapper
   *  module class, which is not a term, so they pass.
   */
  private def mentionsLocallyScopedSymbol(tpe: Type)(using Context): Boolean =
    import dotc.core.Flags
    tpe.existsPart { part =>
      val sym = part.typeSymbol
      sym.exists
        && (sym.is(Flags.TypeParam)
            || (sym.maybeOwner.exists && sym.maybeOwner.isTerm))
    }

end EvalTypeAnnotate
