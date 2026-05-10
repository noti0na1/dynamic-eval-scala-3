package dotty.tools
package repl
package eval

import scala.collection.mutable

import dotc.ast.tpd
import dotc.ast.tpd.*
import dotc.cc.CaptureAnnotation
import dotc.core.Annotations.Annotation
import dotc.core.Constants.Constant
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Flags
import dotc.core.NameKinds.DefaultGetterName
import dotc.core.Names.{Name, TermName, termName}
import dotc.core.Phases.Phase
import dotc.core.StdNames.nme
import dotc.core.Symbols.*
import dotc.core.Types.*
import dotc.report
import dotc.transform.MacroTransform
import dotc.util.SourceFile
import dotc.util.Spans.{NoSpan, Span}

/** Post-PostTyper phase that fills the `bindings`, `expectedType`,
 *  and `enclosingSource` arguments of every `eval[T]` /
 *  `evalSafe[T]` / `agent[T]` / `agentSafe[T]` call. This phase is
 *  *the* eval rewriter — there is no parser-stage counterpart.
 *
 *  Runs after PostTyper because the typed tree carries:
 *    - resolved symbols, so `eval` / `evalSafe` are matched by
 *      `sym.owner == Eval.moduleClass` rather than by name. A
 *      user-defined `eval` shadowing the import is *not* rewritten.
 *    - inline / macro expansion still pending (those run later in
 *      `transformPhases`), so any captured local we collect here is
 *      something the user wrote and that survives to runtime.
 *    - class-member references shaped as `This(cls).select(name)`
 *      (PostTyper rewrites bare `Ident` whose `tpe` is `ThisType`
 *      into `This(cls)`).
 *
 *  Responsibilities:
 *    - 1 → 4 argument shape conversion: `eval[T]({ ctx => "..." })`
 *      → `eval[T]({ ctx => "..." }, Array.empty, "", "")` by
 *      switching to the 4-arg overload symbol of the same method.
 *    - bindings: build an `Array[Eval.Binding]` from the typed scope
 *      (block-local val/var, method val params, by-name params,
 *      block-local mono / poly defs, `__this__` / `__this__<Cls>`
 *      synthetics for the enclosing class chain, and class-member
 *      DCE-keepers for the body's reflective lookups).
 *    - expectedType: render the typed `[T]` argument back to source
 *      with cc-aware annotations.
 *    - enclosingSource: slice the current top-level statement's
 *      source with the eval call's span replaced by the marker.
 *      For safe-flavor (`evalSafe` / `agentSafe`) calls, also wrap
 *      the marker in `Eval.handleCompileError(...)` so the inner
 *      verify compile lifts the body's `T` to `EvalResult[T]`.
 *
 *  Chained-mode `enclosingSource` (an inner eval inside an outer
 *  eval's body) is set up earlier by [[SpliceEvalBody.parseBody]],
 *  before this phase runs on the wrapper compile.
 */
class EvalRewriteTyped(maybeConfig: Option[EvalCompilerConfig] = None) extends MacroTransform:

  override def phaseName: String = EvalRewriteTyped.name

  override def runsAfter: Set[String] =
    Set(dotc.transform.PostTyper.name)

  override protected def newTransformer(using Context): Transformer =
    new EvalRewriteTransformer

  /** Captured local at a typed call site. We carry the Symbol and
   *  the source-level name; the latter is what the eval body refers
   *  to (and may differ for `__this__` synthetics, `__field` aliases,
   *  etc.).
   *
   *  - `selfThisCls` set: the binding is a `__this__` / `__this__<Cls>`
   *    synthetic; its value is `This(cls)` constructed *at the bind
   *    call site* (not eagerly), so the typer's view of `this` is
   *    correct for the call's context (matters for nested classes
   *    where the outer `this` reaches via the outer chain).
   *  - `classMemberOf` set: a class-member capture whose value is
   *    `This(cls).select(sym)`. We emit these bindings to keep the
   *    member alive across the method-lift / body-rewrite pipeline:
   *    without a typed reference at the bind site, dead-code
   *    elimination drops private members (the body-rewrite step
   *    replaces the only reference with a reflective lookup that
   *    looks up the field at runtime, missing it after DCE).
   */
  private case class CapturedSym(
      sym: Symbol,
      sourceName: String,
      isVar: Boolean,
      isGiven: Boolean = false,
      isByName: Boolean = false,
      isDef: Boolean = false,
      selfThisCls: Option[ClassSymbol] = None,
      classMemberOf: Option[ClassSymbol] = None
  )

  /** What kind of top-level shape encloses an eval call. The verify
   *  compile wraps `Expression` shapes in a synthetic
   *  `val __unused__: Any = { ... }` so the result type-checks; for
   *  `Definition` shapes (def/val/object/class/import) the source is
   *  already a valid module member and gets dropped in as-is.
   */
  private enum TopKind:
    case Unknown, Definition, Expression

  /** Pipeline-level category an Apply call belongs to. Determined
   *  purely by symbol identity (for `Eval.eval` / `Eval.evalSafe`)
   *  or by `@evalLike` / `@evalSafeLike` annotation (for
   *  user-defined generators such as `agent`). Never by name match.
   */
  private enum EvalKind:
    case NotEval, PlainEval, PlainEvalSafe, EvalLike, EvalSafeLike

    /** Plain `Eval.eval` / `Eval.evalSafe` — known signature, fixed
     *  positional layout. The 1-arg closure form must be expanded
     *  to the 4-arg overload before filling.
     */
    def isPlain: Boolean = this match
      case PlainEval | PlainEvalSafe => true
      case _ => false

    /** Non-throwing variant whose result type is `EvalResult[T]`.
     *  The encl-source's marker is wrapped in
     *  `Eval.handleCompileError(...)` so the inner verify compile
     *  lifts the body's `T` to `EvalResult[T]`.
     */
    def isSafe: Boolean = this match
      case PlainEvalSafe | EvalSafeLike => true
      case _ => false

  private class EvalRewriteTransformer extends Transformer:

    /** Stack of in-scope local frames (innermost on top). */
    private val frameStack = mutable.Stack.empty[List[CapturedSym]]

    /** Span / source / kind of the *top-level* statement currently
     *  being processed — i.e. the outermost user-written declaration
     *  that contains this eval call. The encl-source slice is taken
     *  from this range with the eval call's span replaced by
     *  `EvalBodyPlaceholder.Marker`. Updated by the wrapper-Template
     *  case as we descend into each member of the REPL wrapper's body.
     */
    private var topLevelStart: Int = -1
    private var topLevelEnd: Int = -1
    private var topLevelSource: SourceFile | Null = null
    private var topLevelKind: TopKind = TopKind.Unknown

    private def classifyTopLevel(tree: Tree): TopKind = tree match
      case _: DefDef | _: ValDef | _: TypeDef | _: Import | _: PackageDef =>
        TopKind.Definition
      case _ => TopKind.Expression

    /** Run `action` with `stat` recorded as the current top-level. */
    private def withTopLevel[T](stat: Tree)(action: => T)(using Context): T =
      val savedStart = topLevelStart
      val savedEnd = topLevelEnd
      val savedSrc = topLevelSource
      val savedKind = topLevelKind
      val span = stat.span
      if span.exists then
        topLevelStart = span.start
        topLevelEnd = span.end
        topLevelSource = stat.source
        topLevelKind = classifyTopLevel(stat)
      try action
      finally
        topLevelStart = savedStart
        topLevelEnd = savedEnd
        topLevelSource = savedSrc
        topLevelKind = savedKind

    private def withScope[T](caps: List[CapturedSym])(action: => T): T =
      val pushed = caps.nonEmpty
      if pushed then frameStack.push(caps)
      try action
      finally if pushed then frameStack.pop()

    /** All captures visible at the current point, deduplicated with
     *  innermost shadowing outer.
     */
    private def currentBindings: List[CapturedSym] =
      val seen = mutable.LinkedHashMap.empty[String, CapturedSym]
      for frame <- frameStack.toList.reverse; c <- frame do
        seen(c.sourceName) = c
      seen.values.toList

    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case Block(stats, expr) =>
          // Pre-scan defs: block-local defs are visible from any
          // sibling (Scala's forward-reference semantics for `def`s
          // in the same block). We seed them into the scope before
          // processing any stat so mutual recursion captures the
          // sibling correctly.
          // Every block-local non-synthetic named def gets captured
          // as an eta-expanded function value. Given-flagged defs
          // (`given foo(using Bar): Foo = ...`) flow through the same
          // path — the body's implicit search may resolve `summon[Foo]`
          // through the def, and the typer's `foo(<given-Bar>)` call
          // lowers via the captured eta-expansion.
          val forwardDefs: List[CapturedSym] = stats.collect {
            case dd: DefDef
                if !dd.symbol.is(Flags.Synthetic)
                && !dd.name.isEmpty =>
              CapturedSym(dd.symbol, dd.name.toString, isVar = false, isDef = true)
          }
          val processed = mutable.ListBuffer.empty[Tree]
          var blockCaps: List[CapturedSym] = forwardDefs
          for stat <- stats do
            val newStat = withScope(blockCaps)(transform(stat))
            processed += newStat
            stat match
              case vd: ValDef
                  if !vd.symbol.is(Flags.Synthetic)
                  && !vd.name.isEmpty =>
                val isVar = vd.symbol.is(Flags.Mutable)
                val isGiven = vd.symbol.is(Flags.Given)
                // Block-local `given x: T = ...` reaches the body two
                // ways. When the marker is the block's trailing expr,
                // SpliceEvalBody hoists the given into the body and
                // the binding is unused (a dead capture). Otherwise
                // (e.g. `val r = eval[T](...)` after the given), the
                // typer resolves bare-name and `summon[T]` references
                // through the outer-scope given val, and ExtractEvalBody
                // lowers them to `getValue(name)` — which only works
                // when the rewriter actually emitted a binding.
                blockCaps = blockCaps ++ List(
                  CapturedSym(vd.symbol, vd.name.toString, isVar, isGiven = isGiven)
                )
              case _ => // defs already in `forwardDefs`
          val newExpr = withScope(blockCaps)(transform(expr))
          cpy.Block(tree)(processed.toList, newExpr)

        case dd: DefDef =>
          // Using params (incl. context-function lambda params named
          // `contextual$<n>`) are captured the same as regular params:
          // the typer binds the body's implicit lookup directly to the
          // param symbol, so an `Eval.bind(name, value)` under that
          // exact name lets `getValue` resolve it at runtime. We tag
          // them `isGiven` for cache-key clarity; the lookup mechanism
          // is otherwise the same as a regular bind.
          val paramCaps: List[CapturedSym] = dd.paramss.flatMap { clause =>
            clause.collect {
              case vd: ValDef if !vd.name.isEmpty =>
                val byName = vd.symbol.info.isInstanceOf[ExprType]
                val isGiven = vd.symbol.is(Flags.Given)
                CapturedSym(vd.symbol, vd.name.toString, isVar = false, isGiven = isGiven, isByName = byName)
            }
          }
          // Switch ctx.owner to the def's symbol so any new symbols
          // we synthesise inside its body (anon-fun closures for
          // bindVar's get/set, etc.) get the right enclosure for
          // LambdaLift's free-var analysis.
          val newRhs =
            inContext(ctx.withOwner(dd.symbol)) {
              withScope(paramCaps)(transform(dd.rhs))
            }
          cpy.DefDef(tree)(dd.name, dd.paramss, dd.tpt, newRhs)

        case impl: Template
            if ctx.owner.isClass
            && ctx.owner.is(Flags.Module)
            && ctx.owner.maybeOwner.is(Flags.Package) =>
          // The REPL session wrapper (`object rs$line$N`). Each member
          // of its Template body is a "top-level statement" w.r.t. the
          // user's REPL line — that's the boundary the encl-source
          // slice uses to splice the eval call's marker. Walk the
          // body stats one at a time, recording each as the current
          // top-level for the duration of its transform.
          val newStats = impl.body.mapConserve { stat =>
            withTopLevel(stat)(transform(stat))
          }
          cpy.Template(impl)(
            transformSub(impl.constr),
            transform(impl.parents)(using ctx.superCallContext),
            Nil,
            transformSelf(impl.self),
            newStats
          )

        case impl: Template
            if ctx.owner.isClass
            && !ctx.owner.is(Flags.Module)
            && !ctx.owner.is(Flags.Package) =>
          val classSym = ctx.owner.asClass
          // `__this__` lets the runtime body-rewrite turn `this.x`
          // references into `__this__.x` reflective accesses; the
          // qualified form (`__this__<ClassName>`) lets a *nested*
          // class body reach the outer class's `this` after that
          // rewrite. Both bind the class's `This(cls)` value.
          val syntheticThises = List(
            CapturedSym(NoSymbol, "__this__", isVar = false, selfThisCls = Some(classSym)),
            CapturedSym(NoSymbol, s"__this__${classSym.name}", isVar = false, selfThisCls = Some(classSym))
          )
          // Class val/var members are emitted as bindings even though
          // the body's bare `x` resolves lexically through the encl
          // source. The bindings exist to KEEP the member alive across
          // DCE: SpliceEvalBody lifts the eval-bearing method out of
          // its enclosing class, after which a private `val x` has no
          // remaining typed-tree reference (the body rewrite replaces
          // `this.x` with a reflective `__refl_get__` call by name).
          // A `Eval.bind("x", this.x)` at the call site forces the
          // typed reference to stay live, so the field survives to
          // runtime where reflection can find it.
          val members = collectClassMembers(impl, classSym)
          withScope(syntheticThises ++ members)(super.transform(impl))

        case app: Apply =>
          // Walk children first so any nested eval calls inside this
          // call's args (e.g. inside a closure-form's body) are
          // processed bottom-up, then fill at the outer call site.
          val withChildren = super.transform(app).asInstanceOf[Apply]
          val kind = classifyCall(withChildren)
          if kind == EvalKind.NotEval then
            warnIfShadowingEvalName(withChildren)
            withChildren
          else if withChildren.args.length == 1 && kind.isPlain then
            // Plain `eval` / `evalSafe`'s closure form has a 1-arg
            // overload that delegates to the 4-arg shape internally;
            // expand to the 4-arg overload AND fill the synthetic
            // args inline. The 4-arg closure overload has no defaults
            // the rewriter could reach via accessor, so the values
            // are baked in here.
            expandOneArgToFourArg(withChildren, kind).getOrElse(withChildren)
          else
            // User eval-likes (and `eval`/`evalSafe` string-form)
            // already have all default-bearing slots filled by typer.
            // [[fillEvalArgs]] enforces the all-or-nothing rule.
            fillEvalArgs(withChildren, kind)

        case _ =>
          super.transform(tree)

/** Class members (val/var) emitted as `Eval.bind("x", this.x)`
     *  bindings. The body-rewrite step turns `this.x` references in
     *  the body into reflective lookups by name, so the wrapper
     *  doesn't actually consume these by value — they exist solely
     *  to keep the typed reference alive at the bind-call site, so
     *  DCE doesn't drop a `private val x` that the lifted method's
     *  body no longer mentions directly.
     *
     *  Synthetic / unnamed members are skipped. Vars are emitted as
     *  immutable reads here (writes through `this.x = v` are rerouted
     *  to `__refl_set__` by the body rewrite, which uses the class
     *  symbol from `__this__` rather than this binding).
     */
    private def collectClassMembers(impl: Template, classSym: ClassSymbol)(using Context): List[CapturedSym] =
      val out = mutable.ListBuffer.empty[CapturedSym]
      def addMember(vd: ValDef): Unit =
        if vd.symbol.exists && !vd.name.isEmpty && !vd.symbol.is(Flags.Synthetic) then
          out += CapturedSym(
            vd.symbol, vd.name.toString, isVar = false,
            classMemberOf = Some(classSym)
          )
      impl.constr.paramss.foreach { clause =>
        clause.foreach {
          case vd: ValDef
              if vd.symbol.is(Flags.ParamAccessor) || vd.symbol.is(Flags.Mutable) =>
            addMember(vd)
          case _ =>
        }
      }
      impl.body.foreach {
        case vd: ValDef => addMember(vd)
        case _ =>
      }
      out.toList

    /** Classify an Apply into the eval-pipeline category whose call
     *  contract it follows. Pure symbol + annotation based — never
     *  by name or arity, so a user-defined `eval` shadowing the
     *  imported one is *not* mis-rewritten and a user eval-like
     *  with arbitrary parameter ordering still gets its synthetic
     *  args supplied by name.
     *
     *  - [[EvalKind.PlainEval]]: `Eval.eval` (any overload).
     *  - [[EvalKind.PlainEvalSafe]]: `Eval.evalSafe` (any overload).
     *  - [[EvalKind.EvalLike]]: a user-defined function annotated with
     *    [[evalLike]].
     *  - [[EvalKind.EvalSafeLike]]: a user-defined function annotated
     *    with [[evalSafeLike]].
     *  - [[EvalKind.NotEval]]: anything else (including arbitrary
     *    Apply nodes the rewriter must leave alone).
     */
    private def classifyCall(app: Apply)(using Context): EvalKind =
      val sym = app.fun.symbol
      if !sym.exists then EvalKind.NotEval
      else
        val owner = sym.maybeOwner
        if owner.exists && owner == EvalRewriteTyped.evalModuleClass then
          sym.name.toString match
            case "eval" => EvalKind.PlainEval
            case "evalSafe" => EvalKind.PlainEvalSafe
            case _ => EvalKind.NotEval
        else if sym.hasAnnotation(EvalRewriteTyped.evalLikeAnnotClass) then
          EvalKind.EvalLike
        else if sym.hasAnnotation(EvalRewriteTyped.evalSafeLikeAnnotClass) then
          EvalKind.EvalSafeLike
        else EvalKind.NotEval

    /** Expand a 1-arg closure-form call (`eval[T]({ ctx => "..." })`)
     *  to a fully-filled 4-arg call by switching to the 4-arg
     *  overload and supplying the synthetic args inline (the 4-arg
     *  closure overload has no defaults the rewriter could otherwise
     *  reach via accessor, so the values are baked in here).
     */
    private def expandOneArgToFourArg(app: Apply, kind: EvalKind)(using Context): Option[Apply] =
      val Apply(fun, List(closureArg)) = app: @unchecked
      val oneArgSym = fun.symbol
      val typeArgs = fun match
        case TypeApply(_, ts) => ts
        case _ => Nil
      paramInfosOf(oneArgSym).headOption.flatMap { firstParamTpe =>
        findFourArgOverload(oneArgSym, firstParamTpe).map { fourArgSym =>
          val newFun =
            if typeArgs.nonEmpty then
              TypeApply(ref(fourArgSym), typeArgs).withSpan(fun.span)
            else
              ref(fourArgSym).withSpan(fun.span)
          val span = app.span
          val tArg = extractTypeArg(fun)
          val rendered = if tArg eq null then "" else EvalRewriteTyped.renderType(tArg)
          val encl = computeEnclosingSource(span)
          val wrappedEncl =
            if kind.isSafe && encl.contains(EvalContext.placeholder) then
              encl.replace(
                EvalContext.placeholder,
                s"_root_.dotty.tools.repl.eval.Eval.handleCompileError(${EvalContext.placeholder})"
              )
            else encl
          val bindingsArg = buildBindingsArray(currentBindings, span)
          val expTpeArg = Literal(Constant(rendered)).withSpan(span)
          val enclArg = Literal(Constant(wrappedEncl)).withSpan(span)
          Apply(newFun, List(closureArg, bindingsArg, expTpeArg, enclArg)).withSpan(span)
        }
      }

    private def paramInfosOf(sym: Symbol)(using Context): List[Type] =
      sym.info match
        case pt: PolyType =>
          pt.resType match
            case mt: MethodType => mt.paramInfos
            case _ => Nil
        case mt: MethodType => mt.paramInfos
        case _ => Nil

    /** Find the 4-value-param overload of `sym`'s name in `sym`'s
     *  owner whose first param's type matches `firstParamTpe`. Used
     *  to switch from the 1-arg closure form (`eval[T](gen)`) to
     *  the 4-arg form (`eval[T](gen, bindings, expTpe, encl)`).
     */
    private def findFourArgOverload(sym: Symbol, firstParamTpe: Type)(using Context): Option[Symbol] =
      val owner = sym.maybeOwner
      if !owner.exists then None
      else
        owner.info.member(sym.name).alternatives.iterator.map(_.symbol).find { alt =>
          paramInfosOf(alt) match
            case head :: rest =>
              rest.length == 3 && (head =:= firstParamTpe)
            case _ => false
        }

    /** Fill the synthetic args at the call site. Three by-name slots
     *  (`bindings`, `expectedType`, `enclosingSource`) are located by
     *  matching the method's parameter names against the recognised
     *  alias sets ([[BindingsParamNames]] etc.). The user's other
     *  positional parameters — including a trailing `maxAttempts` on
     *  agent-style generators — are left untouched.
     *
     *  Plain `Eval.eval` / `Eval.evalSafe`: the synthetic slots sit
     *  at the canonical positions 1/2/3.
     *  User eval-likes: the slots are wherever the user declared
     *  them. The replacement is positional in the typed tree, but
     *  the *choice of position* is name-driven, mirroring a named
     *  argument.
     *
     *  - `bindings`: rebuilt from the typed scope (block val/var,
     *    method params, by-name params, block-local defs,
     *    `__this__` synthetics, class-member DCE-keepers).
     *  - `expectedType`: the typed `[T]` argument rendered back to
     *    source with cc-aware annotations.
     *  - `enclosingSource`: a non-empty literal at the call site
     *    (chained-mode encl set up earlier) is preserved; otherwise
     *    we slice from the current top-level statement's source.
     *    Safe-flavor (`evalSafe` / `evalSafeLike`) calls additionally
     *    wrap the placeholder in `Eval.handleCompileError(...)` so
     *    the inner verify compile lifts the body's `T` to
     *    `EvalResult[T]`.
     */
    private def fillEvalArgs(app: Apply, kind: EvalKind)(using Context): Tree =
      val sym = app.fun.symbol
      val span = app.span

      // Safe-flavor result-type validation: `evalSafe` / `evalSafeLike`
      // calls must have a result type rooted in `EvalResult[?]`. The
      // `handleCompileError` wrap injected into the encl source only
      // makes sense if the body's `T` is being lifted to
      // `EvalResult[T]`; surface a misuse early instead of letting the
      // wrapper compile produce a confusing type-error downstream.
      if kind.isSafe then
        val resultTpe = sym.info.finalResultType
        if !isEvalResultType(resultTpe) then
          report.error(
            i"""${kind} call's method must return an `EvalResult[?]` — got `$resultTpe`.
               |For safe-flavor eval generators (annotated with `@evalSafeLike`), declare
               |the result type as `EvalResult[T]` so the rewriter can wrap the encl
               |source's marker in `Eval.handleCompileError(...)` correctly.""",
            app.srcPos
          )
          return app

      // Locate the Apply that corresponds to the parameter clause
      // declaring the synthetic slots. For a multi-clause method
      // (e.g. `(body, bindings, expTpe, encl)(using ctx)`) the outer
      // Apply we matched holds the *last* clause's args; the synthetic
      // slots typically live in an earlier clause that we reach by
      // walking down `app.fun`.
      // Couldn't find the synthetic slots → treat as "not eval-like"
      // and leave alone. (Likely a misclassification: an annotated
      // function whose signature lacks the expected param names.)
      val clauseInfo = locateSyntheticClause(app, sym)
      if clauseInfo.isEmpty then return app
      val (clauseApp, bindIdx, expIdx, enclIdx) = clauseInfo.get
      val args = clauseApp.args

      // A slot is "fillable" iff the caller didn't supply a value —
      // the *only* unambiguous signal is a typer default-arg accessor
      // (`<method>$default$<n>`). An explicit `""` or
      // `Array.empty[Binding]` from the user is treated as supplied,
      // so a caller can pass `eval(body, Array.empty, "Int", "")`
      // explicitly and the rewriter will leave it alone (matching the
      // all-or-nothing rule).
      def isFillable(t: Tree): Boolean = isDefaultArgFill(t)

      val bindFillable = isFillable(args(bindIdx))
      val expFillable = isFillable(args(expIdx))
      val enclFillable = isFillable(args(enclIdx))
      val allFillable = bindFillable && expFillable && enclFillable
      val noneFillable = !bindFillable && !expFillable && !enclFillable

      if !allFillable && !noneFillable then
        // Mixed state — caller supplied some but not all of the
        // three synthetic args. The rewriter would silently
        // overwrite the partial subset, so reject as ill-formed.
        report.error(
          i"""eval-like call has a partial set of synthetic arguments — the rewriter
             |requires that `bindings`, `expectedType`, and `enclosingSource` are
             |either *all* default (filled in by the rewriter) or *all* explicitly
             |supplied (forwarded by a wrapping `@evalLike` function). Mixed states
             |would have silently overwritten one of your values.
             |
             |Slot states: bindings=${stateLabel(bindFillable)}, expectedType=${stateLabel(expFillable)}, enclosingSource=${stateLabel(enclFillable)}""",
          app.srcPos
        )
        return app

      if noneFillable then
        // Caller (or an outer `@evalLike` wrapper) supplied all
        // three explicitly — leave them alone so the supplied
        // values flow through unchanged.
        return app

      // All three default — fill them in from the typed scope.
      val argsBuf = args.toBuffer

      // expectedType: rendered from the typed `[T]`.
      val tArg = extractTypeArg(app.fun)
      val renderedTpe = if tArg eq null then "" else EvalRewriteTyped.renderType(tArg)
      argsBuf(expIdx) = Literal(Constant(renderedTpe)).withSpan(argsBuf(expIdx).span)

      // enclosingSource: slice from the current top-level statement.
      // Safe-flavor wraps the placeholder in `handleCompileError`.
      val encl = computeEnclosingSource(span)
      val wrappedEncl =
        if kind.isSafe && encl.contains(EvalContext.placeholder) then
          encl.replace(
            EvalContext.placeholder,
            s"_root_.dotty.tools.repl.eval.Eval.handleCompileError(${EvalContext.placeholder})"
          )
        else encl
      argsBuf(enclIdx) = Literal(Constant(wrappedEncl)).withSpan(argsBuf(enclIdx).span)

      // bindings: rebuilt from the typed scope.
      argsBuf(bindIdx) = buildBindingsArray(currentBindings, argsBuf(bindIdx).span)

      val newClauseApp = cpy.Apply(clauseApp)(clauseApp.fun, argsBuf.toList)
      // If the synthetic clause is the outer Apply (single-clause
      // method) we're done; otherwise re-thread the rewritten clause
      // through the chain of intervening Applies.
      if (newClauseApp eq clauseApp) || (clauseApp eq app) then
        if clauseApp eq app then newClauseApp else app
      else rethreadApply(app, clauseApp, newClauseApp)

    /** Walk `app.fun` down to find the `Apply` whose direct args are
     *  the parameter clause declaring `bindings` / `expectedType` /
     *  `enclosingSource`. Returns `(applyForThatClause, indices...)`
     *  or `None` if no such clause exists on the method.
     */
    private def locateSyntheticClause(app: Apply, sym: Symbol)(using Context): Option[(Apply, Int, Int, Int)] =
      val clauses = paramClauseNames(sym.info)
      val totalTermClauses = clauses.length
      val clauseIdx = clauses.indexWhere(names =>
        names.contains(EvalRewriteTyped.BindingsParamName) &&
          names.contains(EvalRewriteTyped.ExpectedTypeParamName) &&
          names.contains(EvalRewriteTyped.EnclosingSourceParamName)
      )
      if clauseIdx < 0 then return None
      // Outermost Apply holds the last clause (index totalTermClauses-1);
      // walk down via .fun to reach `clauseIdx`.
      val descend = totalTermClauses - 1 - clauseIdx
      var cur: Tree = app
      var stepsLeft = descend
      while stepsLeft > 0 do
        cur match
          case a: Apply => cur = a.fun; stepsLeft -= 1
          case _ => return None
      cur match
        case a: Apply =>
          val names = clauses(clauseIdx)
          Some((
            a,
            names.indexOf(EvalRewriteTyped.BindingsParamName),
            names.indexOf(EvalRewriteTyped.ExpectedTypeParamName),
            names.indexOf(EvalRewriteTyped.EnclosingSourceParamName)
          ))
        case _ => None

    /** All term-parameter clause name lists, outermost (first
     *  clause in source order) → innermost (last clause).
     */
    private def paramClauseNames(info: Type)(using Context): List[List[String]] = info match
      case pt: PolyType => paramClauseNames(pt.resType)
      case mt: MethodType => mt.paramNames.map(_.toString) :: paramClauseNames(mt.resType)
      case _ => Nil

    /** Substitute `replacement` for `original` somewhere along the
     *  `app.fun` chain. Returns a copy of `app` whose chain reflects
     *  the replacement.
     */
    private def rethreadApply(app: Apply, original: Apply, replacement: Apply)(using Context): Tree =
      def loop(t: Tree): Tree =
        if t eq original then replacement
        else t match
          case a: Apply => cpy.Apply(a)(loop(a.fun), a.args)
          case _ => t
      loop(app)

    private def stateLabel(fillable: Boolean): String =
      if fillable then "default" else "supplied"

    /** True when `tpe`'s class symbol is `EvalResult` (or a subclass).
     *  Used to validate that safe-flavor eval calls
     *  (`evalSafe` / `@evalSafeLike`) actually return an
     *  `EvalResult[T]` — the `handleCompileError` wrap relies on it.
     */
    private def isEvalResultType(tpe: Type)(using Context): Boolean =
      val cls = tpe.classSymbol
      cls.exists && cls.derivesFrom(EvalRewriteTyped.evalResultClass)

    /** True for a typer-supplied default-arg accessor call (i.e.
     *  `myEval$default$3`). Using
     *  [[NameKinds.DefaultGetterName.matches]] instead of a string-
     *  ends-with check so the recogniser stays robust to dotty's
     *  internal naming convention.
     */
    private def isDefaultArgFill(t: Tree)(using Context): Boolean =
      def loop(t: Tree): Boolean = t match
        case Apply(fn, _) => loop(fn)
        case TypeApply(fn, _) => loop(fn)
        case _ =>
          val s = t.symbol
          s.exists && s.name.is(DefaultGetterName)
      loop(t)

    /** Param-name list of `sym` flattened across type and value
     *  parameter clauses; type-param names are left out (callers
     *  match against value-param names only).
     */
    private def paramNames(sym: Symbol)(using Context): List[String] =
      sym.info match
        case pt: PolyType =>
          pt.resType match
            case mt: MethodType => mt.paramNames.map(_.toString)
            case _ => Nil
        case mt: MethodType => mt.paramNames.map(_.toString)
        case _ => Nil

    /** Warn when an Apply's call target is named `eval` or `evalSafe`
     *  but isn't actually one of [[EvalKind.PlainEval]] /
     *  [[EvalKind.PlainEvalSafe]] — i.e. a user-defined method
     *  shadowing the imported one with no `@evalLike` /
     *  `@evalSafeLike` annotation. The rewriter leaves it alone, but
     *  the user probably expected eval semantics, so flag it.
     */
    private def warnIfShadowingEvalName(app: Apply)(using Context): Unit =
      val sym = app.fun.symbol
      if sym == NoSymbol then return
      val name = sym.name.toString
      if name == "eval" || name == "evalSafe" then
        report.warning(
          i"`$name` here resolves to ${sym.owner}.${sym.name}, not `dotty.tools.repl.eval.Eval.$name`; the eval rewriter is leaving this call alone. If you intended a custom eval generator, annotate the function with `@evalLike` (or `@evalSafeLike`).",
          app.srcPos
        )

    // Walk the Apply chain so multi-clause `@evalLike` shapes (where
    // the type clause sits below additional value clauses) still find
    // their `[T]`.
    private def extractTypeArg(fun: Tree)(using Context): Type | Null = fun match
      case TypeApply(_, tArg :: _) => tArg.tpe
      case Apply(inner, _) => extractTypeArg(inner)
      case _ => null

    /** Slice the current top-level statement's source text, replacing
     *  the eval-call's span with `EvalBodyPlaceholder.Marker`. The
     *  result is the `enclosingSource` the wrapper compile uses to
     *  re-typecheck the body in its original lexical context.
     *
     *  Top-level state is set by [[withTopLevel]] when the wrapper
     *  Template's body stats are walked. Without a top-level set
     *  (e.g. the call is in an unusual unit shape) we return the
     *  empty string and the wrapper compile picks an isolated
     *  default context.
     */
    private def computeEnclosingSource(evalSpan: Span)(using Context): String =
      // Chained mode: this phase is running inside the wrapper compile
      // of an outer eval call. The inner eval's span is in the outer
      // body's coordinate system (because the body was parsed from a
      // separate virtual source by SpliceEvalBody). Compose
      // `outerEncl[Marker := emit(outerBody[innerSpan := Marker])]`
      // so the inner verify compile sees the FULL lexical context:
      // outer-method signature → outer body → inner-call marker.
      maybeConfig match
        case Some(cfg) if cfg.outerEnclosingSource.nonEmpty =>
          return composeChainedEncl(evalSpan, cfg.body, cfg.outerEnclosingSource)
        case _ =>

      val markerText = EvalBodyPlaceholder.Marker
      val sourceFile = topLevelSource
      if topLevelStart < 0 || !evalSpan.exists || sourceFile == null then return ""
      val src = sourceFile.nn.content
      if topLevelEnd > src.length || topLevelStart >= topLevelEnd then return ""
      val relStart = evalSpan.start - topLevelStart
      val relEnd = evalSpan.end - topLevelStart
      val topLen = topLevelEnd - topLevelStart
      if relStart < 0 || relEnd > topLen || relStart > relEnd then return ""
      val topSrc = String.valueOf(src, topLevelStart, topLen)
      val withMarker =
        topSrc.substring(0, relStart) + markerText + topSrc.substring(relEnd)
      topLevelKind match
        case TopKind.Definition => withMarker
        case TopKind.Expression => s"val __unused__ : Any = { $withMarker }"
        case TopKind.Unknown => ""

    private def composeChainedEncl(innerSpan: Span, outerBody: String, outerEncl: String): String =
      if !innerSpan.exists then return ""
      val s = innerSpan.start
      val e = innerSpan.end
      if s < 0 || e > outerBody.length || s > e then return ""
      val outerBodyWithInnerMarker =
        outerBody.substring(0, s) + EvalBodyPlaceholder.Marker + outerBody.substring(e)
      outerEncl.replace(
        EvalBodyPlaceholder.Marker,
        EvalBodyPlaceholder.emit(outerBodyWithInnerMarker)
      )

    /** `scala.Array(bindings...)` typed as `Array[Eval.Binding]`.
     *  Empty case still emits a typed `Array[Eval.Binding]` so the
     *  surrounding 4-arg overload solves correctly.
     */
    private def buildBindingsArray(caps: List[CapturedSym], span: Span)(using Context): Tree =
      val elemTpe: Type = EvalRewriteTyped.bindingClass.typeRef
      val elems: List[Tree] = caps.map(c => buildBind(c, span))
      JavaSeqLiteral(elems, TypeTree(elemTpe)).withSpan(span)

    /** Dispatch a `CapturedSym` to its appropriate binding builder:
     *
     *    - `isVar`     → `Eval.bindVar(name, Eval.varRef(get, set))`
     *    - `isGiven`   → `Eval.bindGiven(name, value)`
     *    - `isByName`  → `Eval.bind(name, () => name)` (Function0 thunk
     *                    so the body's post-ElimByName `apply()` lines
     *                    up; see parser-stage doc).
     *    - `isDef`     → `Eval.bind(name, eta-expansion)`
     *    - default     → `Eval.bind(name, value)`
     */
    private def buildBind(c: CapturedSym, span: Span)(using Context): Tree =
      if c.isVar then buildBindVar(c, span)
      else if c.isGiven then buildBindGiven(c, span)
      else if c.isByName then buildBindByName(c, span)
      else if c.isDef then buildBindDef(c, span)
      else
        val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
        val valueRef = readRef(c, span)
        ref(EvalRewriteTyped.bindSym)
          .appliedTo(nameLit, valueRef)
          .withSpan(span)

    /** Read site for a captured symbol.
     *  - `selfThisCls`: a `__this__` / `__this__<Cls>` synthetic
     *    binding — emit `This(cls)` constructed *at the call-site
     *    context*, so subsequent phases (ExplicitOuter, etc.) see
     *    the call's owner chain when resolving the outer-this access.
     *  - `classMemberOf`: emit `This(cls).select(sym)` so the
     *    referenced member is kept alive across DCE.
     *  - default: `ref(sym)`.
     */
    private def readRef(c: CapturedSym, span: Span)(using Context): Tree =
      c.selfThisCls match
        case Some(cls) => This(cls).withSpan(span)
        case None =>
          c.classMemberOf match
            case Some(cls) => This(cls).select(c.sym).withSpan(span)
            case None => ref(c.sym).withSpan(span)

    /** `Eval.bindGiven(name, value)` for given-val captures. */
    private def buildBindGiven(c: CapturedSym, span: Span)(using Context): Tree =
      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      val valueRef = readRef(c, span)
      ref(EvalRewriteTyped.bindGivenSym)
        .appliedTo(nameLit, valueRef)
        .withSpan(span)

    /** `Eval.bind(name, () => name)` for a by-name parameter capture.
     *  ElimByName has already lowered the body's `name` references
     *  into `name.apply()` on a `Function0[T]`; the binding must
     *  store something `apply()` can be called on.
     *
     *  Constructed via [[tpd.Lambda]] which produces a `Function0[T]`
     *  closure post-PostTyper.
     */
    private def buildBindByName(c: CapturedSym, span: Span)(using Context): Tree =
      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      val resultTpe = c.sym.info match
        case ExprType(rt) => rt
        case other => other
      val methTpe = MethodType(Nil, resultTpe)
      val fn = Lambda(methTpe, _ => readRef(c, span))
      ref(EvalRewriteTyped.bindSym)
        .appliedTo(nameLit, fn.withSpan(span))
        .withSpan(span)

    /** `Eval.bind(name, eta-expansion)` for a captured def.
     *
     *  Strategy: flatten everything into a single uncurried lambda with
     *  every param typed `Any` (and the result typed `Any`). This is
     *  enough because:
     *    - The JVM erases generics + value-param types to `Object`
     *      after erasure, so the captured `(Any, …) => Any` matches
     *      the def's bytecode signature.
     *    - Body call sites of any shape — `g(a)`, `g(a)(b)`, `g[T](a)`,
     *      `g[T](a)(using ev)` — are flattened by
     *      [[ExtractEvalBody.transformedMethodArgs]] into a single
     *      arg list, which the runtime delivers as a single
     *      `binding.apply(args …)` call.
     *
     *  Inside the lambda body each param is cast back to its expected
     *  type so the typed Apply tree we emit type-checks under the
     *  def's declared signature; the casts erase to `Object` checkcasts
     *  at runtime, which always succeed for the values the body's
     *  call site can supply.
     *
     *  Special-cased:
     *    - `def g: R` (`ExprType`) — `ref(defSym)` auto-applies, so
     *      the lambda body is just the Ident.
     *    - `def g[T]: R` — same after instantiating `T → Any`.
     *
     *  Type params are uniformly substituted to `Any`; context bounds
     *  desugar to a `using Numeric[T]` clause whose param info
     *  becomes `Numeric[Any]` after the substitution. Implicit search
     *  for the using value happens at the body's typer time (inside
     *  the wrapper compile, where the body's lexical scope is in
     *  view), so the resolved evidence flows through the binding's
     *  arg list as a regular value.
     */
    private def buildBindDef(c: CapturedSym, span: Span)(using Context): Tree =
      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      val defSym = c.sym

      // Strip a leading PolyType by instantiating its type params to
      // `Any`. Subsequent `MethodType` clauses then have any
      // `T`-mentioning param/result types substituted accordingly.
      val (typeArgs: List[Type], methodLikeTpe: Type) = defSym.info match
        case poly: PolyType =>
          val anys: List[Type] = poly.paramRefs.map(_ => defn.AnyType)
          (anys, poly.instantiate(anys))
        case other =>
          (Nil, other)

      // Walk the (possibly nested) MethodType chain and collect
      // per-clause param infos and names. Each clause becomes one
      // sub-list; the result type sits at the bottom.
      def flatten(t: Type): (List[List[Type]], List[List[TermName]], Type) = t match
        case mt: MethodType =>
          val (rest, namesRest, result) = flatten(mt.resType)
          (mt.paramInfos :: rest, mt.paramNames :: namesRest, result)
        case other =>
          (Nil, Nil, other)
      val (clauseInfos, clauseNames, _) = flatten(methodLikeTpe)

      def applyTypeArgs(t: Tree): Tree =
        if typeArgs.isEmpty then t else t.appliedToTypes(typeArgs)

      val etaTree: Tree =
        if clauseInfos.isEmpty then
          // No `MethodType` in the chain — `ref(defSym)` auto-applies
          // (no-paren `def g: R`, possibly poly).
          val methTpe = MethodType(Nil, defn.AnyType)
          Lambda(methTpe, _ => applyTypeArgs(ref(defSym))).withSpan(span)
        else
          val flatNames = clauseNames.flatten
          val flatInfos = clauseInfos.flatten
          // By-name params can't be `cast` to: `asInstanceOf[=> T]` is
          // not a legal cast target. Keep the original `ExprType` as
          // the lambda's param type for those positions so the call
          // site type-checks without a cast. Other params get `Any` and
          // a cast inside the body.
          val lambdaParamTpes: List[Type] = flatInfos.map {
            case et: ExprType => et
            case _ => defn.AnyType
          }
          val methTpe = MethodType(flatNames)(_ => lambdaParamTpes, _ => defn.AnyType)
          Lambda(methTpe, params =>
            // Cast each lambda param back to the def's expected type at
            // that position. Required so the typed Apply tree
            // type-checks; at runtime these are `checkcast`s against the
            // erased Object form and always succeed for body-supplied
            // values. By-name params skip the cast — their lambda
            // param already carries the matching `ExprType`.
            val callArgs = params.lazyZip(flatInfos).map { (p, t) =>
              t match
                case _: ExprType => p
                case _ => p.cast(t)
            }
            // Re-group the flat params into the def's original clause
            // arities so `appliedToArgss` builds the right Apply chain.
            def regroup(xs: List[Tree], sizes: List[Int]): List[List[Tree]] = sizes match
              case Nil => Nil
              case n :: rest =>
                val (head, tail) = xs.splitAt(n)
                head :: regroup(tail, rest)
            val grouped = regroup(callArgs, clauseInfos.map(_.length))
            applyTypeArgs(ref(defSym)).appliedToArgss(grouped)
          ).withSpan(span)
      ref(EvalRewriteTyped.bindSym)
        .appliedTo(nameLit, etaTree)
        .withSpan(span)

    /** `Eval.bindVar(name, Eval.varRef[T](getter, setter))` where
     *  `getter` / `setter` are SAM-typed `Supplier[T]` / `Consumer[T]`
     *  closures over the captured var. Constructed via
     *  [[tpd.Closure]] with explicit `targetType` so the SAM
     *  conversion is locked in (PostTyper has already run, so we can't
     *  rely on the typer to infer it).
     */
    private def buildBindVar(c: CapturedSym, span: Span)(using Context): Tree =
      val varSym = c.sym
      val elemTpe = varSym.info.widen
      val supplierTpe = EvalRewriteTyped.supplierClass.typeRef.appliedTo(elemTpe)
      val consumerTpe = EvalRewriteTyped.consumerClass.typeRef.appliedTo(elemTpe)

      val getMethTpe = MethodType(Nil, elemTpe)
      val getMeth = newAnonFun(ctx.owner, getMethTpe, coord = span)
      val getter =
        Closure(
          getMeth,
          _ => ref(varSym).withSpan(span).changeOwner(ctx.owner, getMeth),
          targetType = supplierTpe
        ).withSpan(span)

      val setMethTpe = MethodType(List(termName("v")))(_ => List(elemTpe), _ => defn.UnitType)
      val setMeth = newAnonFun(ctx.owner, setMethTpe, coord = span)
      val setter =
        Closure(
          setMeth,
          paramss =>
            Assign(ref(varSym), ref(paramss.head.head.symbol))
              .withSpan(span)
              .changeOwner(ctx.owner, setMeth),
          targetType = consumerTpe
        ).withSpan(span)

      val varRef = ref(EvalRewriteTyped.varRefSym)
        .appliedToType(elemTpe)
        .appliedTo(getter, setter)
        .withSpan(span)

      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      ref(EvalRewriteTyped.bindVarSym)
        .appliedTo(nameLit, varRef)
        .withSpan(span)

  end EvalRewriteTransformer

end EvalRewriteTyped

object EvalRewriteTyped:

  val name: String = "evalRewriteTyped"

  /** Canonical parameter name for the "bindings" synthetic slot.
   *  Both `Eval.eval` and user eval-like signatures must declare
   *  this exact name. (Underscore-prefixed alternatives are *not*
   *  accepted — there is one naming convention.)
   */
  private val BindingsParamName: String = "bindings"

  /** Canonical parameter name for the "expectedType" synthetic slot. */
  private val ExpectedTypeParamName: String = "expectedType"

  /** Canonical parameter name for the "enclosingSource" synthetic slot. */
  private val EnclosingSourceParamName: String = "enclosingSource"

  private def evalModuleClass(using Context): Symbol =
    requiredModule("dotty.tools.repl.eval.Eval").moduleClass

  private def evalLikeAnnotClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.repl.eval.evalLike")

  private def evalSafeLikeAnnotClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.repl.eval.evalSafeLike")

  private def evalResultClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.repl.eval.EvalResult")

  private def bindingClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.repl.eval.Eval.Binding")

  private def bindSym(using Context): Symbol =
    requiredModule("dotty.tools.repl.eval.Eval").requiredMethod("bind")

  private def bindVarSym(using Context): Symbol =
    requiredModule("dotty.tools.repl.eval.Eval").requiredMethod("bindVar")

  private def bindGivenSym(using Context): Symbol =
    requiredModule("dotty.tools.repl.eval.Eval").requiredMethod("bindGiven")

  private def varRefSym(using Context): Symbol =
    requiredModule("dotty.tools.repl.eval.Eval").requiredMethod("varRef")

  private def supplierClass(using Context): ClassSymbol =
    requiredClass("java.util.function.Supplier")

  private def consumerClass(using Context): ClassSymbol =
    requiredClass("java.util.function.Consumer")

  /** Render `tpe` as a Scala source string suitable for splicing
   *  back into the wrapper's `val __evalResult: <tpe> = ...`
   *  annotation. Returns the empty string when the type is
   *  degenerate (`Nothing`, `Null`, error type) or mentions a symbol
   *  the wrapper wouldn't be able to resolve — an enclosing
   *  method's type parameter, or a locally-scoped class.
   *
   *  Capture annotations (`^`, `^{...}`) are kept when capture
   *  checking is enabled in the live session, so the inner verify
   *  compile sees the exact capability set the user declared.
   *  Otherwise they're stripped — the wrapper's val type only needs
   *  the underlying erased shape.
   */
  private[repl] def renderType(tpe: Type)(using Context): String =
    if tpe == null || !tpe.exists || tpe.isError then return ""
    val widened = tpe.widen
    if !widened.exists || widened.isError then return ""
    if isUselessType(widened) then return ""
    val resolved = dealiasLocalAliases(widened)
    if mentionsLocallyScopedSymbol(resolved) then return ""
    val cleaned =
      if ctx.settings.YccNew.value || ctx.settings.language.value.contains("experimental.captureChecking")
      then resolved
      else stripCaptureAnnotations(resolved)
    val printCtx = ctx.fresh.setSetting(ctx.settings.color, "never")
    try
      cleaned.show(using printCtx).replace(".this.", "#")
    catch case _: Throwable => ""

  private def stripCaptureAnnotations(tpe: Type)(using Context): Type =
    val mapper = new TypeMap:
      def apply(tp: Type): Type = tp match
        case AnnotatedType(parent, ann) if isCaptureAnnotation(ann) => this(parent)
        case _ => mapOver(tp)
    mapper(tpe)

  private def isCaptureAnnotation(ann: Annotation)(using Context): Boolean =
    ann match
      case _: CaptureAnnotation => true
      case _ =>
        val sym = ann.symbol
        sym.exists && (sym == defn.RetainsAnnot || sym == defn.RetainsCapAnnot)

  private def isUselessType(tpe: Type)(using Context): Boolean =
    val sym = tpe.typeSymbol
    sym.exists && (sym == defn.NothingClass || sym == defn.NullClass)

  private def mentionsLocallyScopedSymbol(tpe: Type)(using Context): Boolean =
    tpe.existsPart { part =>
      val sym = part.typeSymbol
      sym.exists && {
        val isTypeParam = sym.is(Flags.TypeParam)
        val isTermOwned = sym.maybeOwner.exists && sym.maybeOwner.isTerm
        isTypeParam || isTermOwned
      }
    }

  private def dealiasLocalAliases(tpe: Type)(using Context): Type =
    val mapper = new TypeMap:
      def apply(tp: Type): Type = tp match
        case ref: TypeRef =>
          val sym = ref.symbol
          if sym.exists && sym.isAliasType && sym.maybeOwner.exists && sym.maybeOwner.isTerm then
            this(ref.dealias)
          else mapOver(tp)
        case _ => mapOver(tp)
    mapper(tpe)
