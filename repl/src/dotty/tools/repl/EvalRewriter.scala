package dotty.tools
package repl

import scala.collection.mutable

import dotc.ast.untpd
import dotc.core.Constants.Constant
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Flags
import dotc.util.Spans.Span

/** Parse-stage rewriter that augments each `eval(...)` call with
 *  `Eval.bind("name", name)` (or `Eval.bindVar("name", cell)` for
 *  mutable bindings) for every name introduced by an enclosing lambda,
 *  block, or method scope. The body string itself is *not* parsed: it
 *  stays dynamic and is compiled at runtime by `Eval.evalIsolated`.
 *
 *  For `var` captures the rewriter further wraps the eval call in a
 *  `Block` that creates a `VarCell` per captured var, runs eval against
 *  those cells, then writes each cell's value back to the outer var so
 *  mutation inside the eval body propagates to the caller.
 */
object EvalRewriter:

  /** Rewrite all `eval(...)` calls in `trees`. */
  def rewrite(trees: List[untpd.Tree])(using Context): List[untpd.Tree] =
    val tx = new Transformer
    trees.mapConserve(tx.transform(_))

  /** Rewrite a code string by parsing it, applying the rewriter (with
   *  `initialScope` seeded so nested eval calls inside the body capture
   *  the outer bindings plus any local val/var the body declares), and
   *  pretty-printing the result. Used by `Eval.evalIsolated` so a body
   *  like `val j = 2; eval("i + j")` has its inner eval rewritten to
   *  receive `j` as a binding.
   */
  def rewriteCode(code: String, initialScope: Array[(String, Boolean)])(using Context): String =
    import dotty.tools.dotc.parsing.Parsers.Parser
    import dotty.tools.dotc.util.SourceFile
    val source = SourceFile.virtual("<eval-body>", code)
    val parser = new Parser(source)
    val tree = parser.block()
    val tx = new Transformer
    val seed = initialScope.iterator.map((n, isVar) => CapturedName(n, isVar)).toList
    tx.pushInitialScope(seed)
    tx.transform(tree).show

  /** A captured name with its mutability. Vars are wrapped in a cell so
   *  the eval body can mutate them and have writes propagate.
   */
  private final case class CapturedName(name: String, isVar: Boolean)

  private object Names:
    val EvalResult: String = "__eval_result__"
    def cell(name: String): String = s"${name}__cell"

  private class Transformer extends untpd.UntypedTreeMap:
    import untpd.*

    /** Stack of in-scope local bindings, innermost on top. Each frame
     *  records the names a single lambda, block, or method introduces.
     */
    private val scopeStack = mutable.Stack.empty[List[CapturedName]]

    /** Seed the scope from outside. Used by `rewriteCode` so nested
     *  eval calls inside a body see the outer bindings.
     */
    def pushInitialScope(names: List[CapturedName]): Unit =
      if names.nonEmpty then scopeStack.push(names)

    /** Names visible at the current point, deduplicated with innermost
     *  shadowing outer.
     */
    private def currentBindings: List[CapturedName] =
      val seen = mutable.LinkedHashMap.empty[String, CapturedName]
      for level <- scopeStack.iterator; c <- level if !seen.contains(c.name) do
        seen(c.name) = c
      seen.values.toList

    private def withScope[T](names: List[CapturedName])(action: => T): T =
      scopeStack.push(names)
      try action
      finally scopeStack.pop()

    override def transform(tree: Tree)(using Context): Tree = tree match
      // Lambda: its parameters become locals visible inside the body.
      // Lambda parameters are always immutable.
      case fn @ Function(args, body) =>
        val names = args.flatMap {
          case vd: ValDef => Some(CapturedName(vd.name.toString, isVar = false))
          case Ident(n)   => Some(CapturedName(n.toString, isVar = false))
          case _ => None
        }
        val newArgs = args.mapConserve(transform)
        val newBody = withScope(names)(transform(body))
        // `Function` is an untpd-only node; use the `untpd.cpy` singleton
        // directly because the base `TreeCopier` we inherit doesn't expose it.
        untpd.cpy.Function(fn)(newArgs, newBody)

      // Block: process stats in order, accumulating names from each val
      // (or var) so subsequent stats and the trailing expression see
      // them. Block-local `def`s are intentionally not captured: at
      // runtime they are synthetic Lambda classes whose names
      // (e.g. `rs$line$1$$Lambda/0x...`) aren't valid Scala source.
      case bk @ Block(stats, expr) =>
        val processed = mutable.ListBuffer.empty[Tree]
        var blockNames = List.empty[CapturedName]
        for stat <- stats do
          val newStat = withScope(blockNames)(transform(stat))
          processed += newStat
          stat match
            case vd: ValDef =>
              val isVar = vd.mods.is(Flags.Mutable)
              blockNames = CapturedName(vd.name.toString, isVar) :: blockNames
            case _ =>
        val newExpr = withScope(blockNames)(transform(expr))
        cpy.Block(bk)(processed.toList, newExpr)

      // Method definition: its term parameters are visible in the body.
      // Method parameters are always immutable in Scala.
      case dd: DefDef =>
        val paramNames = dd.paramss.flatMap { clause =>
          clause.collect { case vd: ValDef => CapturedName(vd.name.toString, isVar = false) }
        }
        val newRhs = withScope(paramNames)(transform(dd.rhs))
        cpy.DefDef(dd)(dd.name, dd.paramss, dd.tpt, newRhs)

      // The eval call itself: splice in an
      // `Array(Eval.bind/bindVar(...), ...)` argument for every captured
      // local. When any captures are vars, also wrap the call in a Block
      // that creates `VarCell`s per var and syncs them back after eval.
      case app @ Apply(fn, args) if isEvalCall(fn) =>
        val captured = currentBindings
        if captured.isEmpty then super.transform(tree)
        else
          val newArgs = args.mapConserve(transform)
          if !captured.exists(_.isVar) then
            val bindArgs = captured.map(c => buildBind(c.name, app.span))
            cpy.Apply(app)(fn, newArgs :+ buildArray(bindArgs, app.span))
          else
            buildVarAwareCall(app, fn, newArgs, captured)

      case _ => super.transform(tree)
    end transform

    /** Build a Block that:
     *    1. creates a `VarCell` per captured var,
     *    2. invokes eval, passing the cells via `Eval.bindVar`,
     *    3. writes each cell's value back to the corresponding outer var,
     *    4. yields the eval result.
     */
    private def buildVarAwareCall(
        app: Apply,
        fn: Tree,
        newArgs: List[Tree],
        captured: List[CapturedName]
    )(using Context): Tree =
      val span = app.span

      // 1. cell vals: `val name__cell = Eval.VarCell(name)`.
      val cellDefs: List[Tree] = captured.collect { case c if c.isVar =>
        val cellApply = makeFqn("dotty.tools.repl.Eval.VarCell.apply", span)
        val arg = Ident(c.name.toTermName).withSpan(span)
        ValDef(
          Names.cell(c.name).toTermName,
          TypeTree(),
          Apply(cellApply, arg :: Nil).withSpan(span)
        ).withSpan(span)
      }

      // 2. the eval call (passing bind / bindVar args).
      val bindArgs: List[Tree] = captured.map { c =>
        if c.isVar then buildBindVar(c.name, span) else buildBind(c.name, span)
      }
      val rebuiltCall = cpy.Apply(app)(fn, newArgs :+ buildArray(bindArgs, span))
      val resultDef = ValDef(
        Names.EvalResult.toTermName,
        TypeTree(),
        rebuiltCall
      ).withSpan(span)

      // 3. sync-back assignments: `name = name__cell.get()`.
      val syncs: List[Tree] = captured.collect { case c if c.isVar =>
        val cellRef = Ident(Names.cell(c.name).toTermName).withSpan(span)
        val getCall = Apply(Select(cellRef, "get".toTermName).withSpan(span), Nil).withSpan(span)
        Assign(Ident(c.name.toTermName).withSpan(span), getCall).withSpan(span)
      }

      // 4. yield the eval result.
      val finalExpr = Ident(Names.EvalResult.toTermName).withSpan(span)
      Block(cellDefs ++ (resultDef :: syncs), finalExpr).withSpan(span)
    end buildVarAwareCall

    private def isEvalCall(fn: Tree): Boolean = fn match
      case Ident(n) => n.toString == "eval"
      case Select(qual, n) => n.toString == "eval" && isEvalQualifier(qual)
      // `eval[T](...)` desugars to `Apply(TypeApply(Ident("eval"), ...), ...)`.
      case TypeApply(inner, _) => isEvalCall(inner)
      case _ => false

    private def isEvalQualifier(t: Tree): Boolean = t match
      case Ident(n) => n.toString == "Eval"
      case Select(_, n) => n.toString == "Eval"
      case _ => false

    private def buildBind(name: String, span: Span)(using Context): Tree =
      val bindFn = makeFqn("dotty.tools.repl.Eval.bind", span)
      val nameLit = Literal(Constant(name)).withSpan(span)
      val valueRef = Ident(name.toTermName).withSpan(span)
      Apply(bindFn, nameLit :: valueRef :: Nil).withSpan(span)

    private def buildBindVar(name: String, span: Span)(using Context): Tree =
      val bindFn = makeFqn("dotty.tools.repl.Eval.bindVar", span)
      val nameLit = Literal(Constant(name)).withSpan(span)
      val cellRef = Ident(Names.cell(name).toTermName).withSpan(span)
      Apply(bindFn, nameLit :: cellRef :: Nil).withSpan(span)

    /** Build `scala.Array(elems...)`. */
    private def buildArray(elems: List[Tree], span: Span)(using Context): Tree =
      val arrayApply = ReplCompiler.selectFqn("scala.Array.apply", span)
      Apply(arrayApply, elems).withSpan(span)

    private def makeFqn(fqn: String, span: Span)(using Context): Tree =
      ReplCompiler.selectFqn(fqn, span)
  end Transformer

end EvalRewriter
