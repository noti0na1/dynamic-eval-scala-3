package dotty.tools
package eval

import scala.collection.mutable

import dotc.ast.tpd
import dotc.ast.tpd.*
import dotc.cc.{CaptureAnnotation, CheckCaptures}
import dotc.core.Annotations.Annotation
import dotc.core.Constants.Constant
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Flags
import dotc.core.NameKinds.{DefaultGetterName, UniqueName}
import dotc.core.Names.{Name, TermName, termName}
import dotc.core.Phases.Phase
import dotc.core.StdNames.nme
import dotc.core.Symbols.*
import dotc.core.Types.*
import dotc.report
import dotc.transform.MacroTransform
import dotc.util.{Property, SourceFile}
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
 *      with cc-aware annotations. When `[T]` was inferred and
 *      minimized to `Nothing` (e.g. `val a: A = eval("...")`, whose
 *      only constraint is `T <: A`), render the call's prototype
 *      instead, recorded during typing by the
 *      [[EvalRewriteTyped.recordEvalProto]] hook in
 *      `Applications.typedApply` and resolved here against the
 *      final instantiations.
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
class EvalRewriteTyped(maybeConfig: Option[EvalCompilerConfig] = None, alwaysEnabled: Boolean = false) extends MacroTransform:

  override def phaseName: String = EvalRewriteTyped.name

  override def runsAfter: Set[String] =
    Set(dotc.transform.PostTyper.name)

  /** Three callers install this phase:
   *    - the main `dotc.Compiler` pipeline, gated on `-Xdynamic-eval`;
   *    - the REPL's `ReplCompiler`, with `alwaysEnabled = true` (eval is
   *      a built-in REPL feature, no flag required);
   *    - [[EvalCompiler]] (the runtime wrapper compile), with a config;
   *      the wrapper must always be rewritten so nested eval calls get
   *      their synthetic args filled.
   */
  override def isEnabled(using Context): Boolean =
    alwaysEnabled || maybeConfig.isDefined || ctx.settings.XdynamicEval.value

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
   *  - `localClassOf` set: a `__evalClass_<C>__` synthetic carrying
   *    `classOf[C]` of a local (term-owned) class. The wrapper
   *    compile uses it to run `isInstanceOf` / `asInstanceOf` /
   *    type-pattern tests against the *original* lifted class
   *    instead of the wrapper's re-elaborated copy.
   *  - `ctorFactory` set: a `__evalNew_<C>__$<i>` synthetic carrying
   *    a factory closure `(args…) => new C(args…)` for constructor
   *    `i` of the local class. The closure closes over `C`'s captured
   *    environment, so LambdaLift does the env plumbing and the
   *    wrapper can construct instances of the *original* class.
   *  - `isModuleRef` set: a `__evalModule_<M>__` synthetic carrying
   *    the live module instance of a local `object M` (including the
   *    synthesized companion of a local class). The wrapper routes
   *    `M.member` / `C.apply` / `C.unapply` through it reflectively,
   *    so module state is shared rather than re-elaborated.
   */
  private case class CapturedSym(
      sym: Symbol,
      sourceName: String,
      isVar: Boolean,
      isGiven: Boolean = false,
      isByName: Boolean = false,
      isDef: Boolean = false,
      selfThisCls: Option[ClassSymbol] = None,
      classMemberOf: Option[ClassSymbol] = None,
      localClassOf: Option[ClassSymbol] = None,
      ctorFactory: Option[(ClassSymbol, Symbol)] = None,
      isModuleRef: Boolean = false
  ):
    /** Compiler-only binding (reserved name, emitted via
     *  `Eval.bindSynthetic`); never resolved by a body identifier.
     */
    def isSyntheticBinding: Boolean =
      selfThisCls.isDefined || classMemberOf.isDefined ||
        localClassOf.isDefined || ctorFactory.isDefined || isModuleRef

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

    /** Enclosing `DefDef`s, innermost first, as (symbol, declared
     *  result type). The result type comes from the tree's `tpt` so
     *  method type/term parameters are in their in-scope form (the
     *  symbol's `info.finalResultType` would carry unbound param
     *  refs). Drives the non-local-return wrap.
     */
    private var enclosingMethods: List[(Symbol, Type)] = Nil

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

    /** Lexical context that must be re-established when the slice is
     *  recompiled outside this unit, rendered as import lines:
     *
     *    - `fileImports`: the unit's package-level `import` clauses,
     *      sliced verbatim from the source.
     *    - `packageImport`: `import <pkg>.{given, *}` when the call
     *      site sits in a named package. The wrapper compiles in the
     *      empty package, so siblings of the file (other top-level
     *      classes, `<file>$package` members) must come in by import.
     *    - `moduleImport`: `import <path>.<Obj>.{given, *}` when the
     *      current top-level statement is a member of a user-written
     *      top-level `object`, mirroring the REPL's
     *      `import rs$line$N.{given, *}` so sibling members resolve
     *      against the *runtime* module (live state) rather than a
     *      re-minted copy.
     *
     *  All three are empty in a REPL compile (the REPL passes its
     *  session imports to the adapter at runtime instead), so REPL
     *  slices are byte-identical to the pre-standalone behaviour.
     */
    private var fileImports: List[String] = Nil
    private var packageImport: String = ""
    private var moduleImport: String = ""

    private def withModuleImport[T](imp: String)(action: => T): T =
      val saved = moduleImport
      moduleImport = imp
      try action
      finally moduleImport = saved

    /** Import lines prepended to every enclosing-source slice. */
    private def contextImports: List[String] =
      fileImports ++ List(packageImport, moduleImport).filter(_.nonEmpty)

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

    /** Source text of `stat`, sliced verbatim from its source file.
     *  Empty when the span is missing or out of bounds.
     */
    private def sliceSource(stat: Tree)(using Context): String =
      val span = stat.span
      if !span.exists then ""
      else
        val content = stat.source.content
        if span.start >= 0 && span.start < span.end && span.end <= content.length then
          String.valueOf(content, span.start, span.end - span.start)
        else ""

    /** True for the modules the eval machinery synthesises itself:
     *  REPL line wrappers (`rs$line$N`) and the inner compile's
     *  `__EvalWrapper…` modules. Their members get lexical context
     *  from the runtime adapter, not from compile-time imports.
     */
    private def isEvalInfraModule(sym: Symbol)(using Context): Boolean =
      val n = sym.name.toString
      n.startsWith(dotc.core.StdNames.str.REPL_SESSION_LINE) || n.contains(ExtractEvalBody.WrapperMarker)

    /** True iff every owner from `cls` up to the enclosing package is
     *  a module class: the object is reachable at runtime by a static
     *  path, which is what the wrapper compile's module lift links
     *  against.
     */
    private def isStaticModuleChain(cls: Symbol)(using Context): Boolean =
      val owner = cls.maybeOwner
      owner.is(Flags.Package)
        || (owner.is(Flags.ModuleClass) && isStaticModuleChain(owner))

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
        case pkg: PackageDef =>
          // Ordinary source file (or the eval wrapper unit): each
          // package-level stat is a top-level statement; for an eval
          // call inside a top-level class the slice is the whole
          // class. Module shapes whose members behave like REPL lines
          // (REPL wrappers, eval wrappers, `<file>$package` objects)
          // override this per member in the module-Template case
          // below. Package-level imports and the package itself are
          // recorded so slices can re-establish the file's lexical
          // context when recompiled in the wrapper's empty package.
          val savedFileImports = fileImports
          val savedPackageImport = packageImport
          fileImports = fileImports ++ pkg.stats.collect {
            case imp: Import => sliceSource(imp)
          }.filter(_.nonEmpty)
          val pkgSym = pkg.pid.symbol
          if pkgSym.exists && pkgSym != defn.EmptyPackageVal && pkgSym != defn.RootPackage then
            packageImport = s"import ${pkgSym.fullName.toString}.{given, *}"
          try
            inContext(localCtx(pkg)) {
              val newStats = pkg.stats.mapConserve { stat =>
                withTopLevel(stat)(transform(stat))
              }
              cpy.PackageDef(pkg)(pkg.pid, newStats)
            }
          finally
            fileImports = savedFileImports
            packageImport = savedPackageImport

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
          //
          // The same pre-scan collects the *synthetic* link bindings
          // for block-local classes and modules (forward-visible like
          // defs, since types resolve block-wide):
          //   - a class contributes `classOf[C]` plus one constructor
          //     factory per (non-private) constructor;
          //   - a `Module`-flagged val (a local `object`, or the
          //     synthesized companion of a local class) contributes
          //     its live module instance. Reading the lazy module val
          //     at the bind site forces initialization, which matches
          //     a body that touches the module at all.
          val forwardDefs: List[CapturedSym] = stats.flatMap {
            case dd: DefDef
                if !dd.symbol.is(Flags.Synthetic)
                && !dd.name.isEmpty =>
              CapturedSym(dd.symbol, dd.name.toString, isVar = false, isDef = true) :: Nil
            case td: TypeDef
                if td.isClassDef && td.symbol.isClass
                && !td.symbol.is(Flags.ModuleClass)
                && !td.symbol.is(Flags.Synthetic) =>
              localClassBundle(td.symbol.asClass)
            case vd: ValDef
                if vd.symbol.is(Flags.Module) && !vd.name.isEmpty =>
              CapturedSym(
                vd.symbol, EvalNames.moduleBinding(vd.name.toString),
                isVar = false, isModuleRef = true) :: Nil
            case _ => Nil
          }
          val processed = mutable.ListBuffer.empty[Tree]
          var blockCaps: List[CapturedSym] = forwardDefs
          for stat <- stats do
            val newStat = withScope(blockCaps)(transform(stat))
            processed += newStat
            stat match
              case vd: ValDef
                  if !vd.symbol.is(Flags.Synthetic)
                  && !vd.symbol.is(Flags.Module)
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
          // LambdaLift's free-var analysis. The (symbol, declared
          // result type) pair is pushed on the method stack so eval
          // calls inside the body know which frame a body `return`
          // would target (the non-local-return wrap).
          val newRhs =
            inContext(ctx.withOwner(dd.symbol)) {
              val savedMethods = enclosingMethods
              enclosingMethods = (dd.symbol, dd.tpt.tpe) :: enclosingMethods
              try withScope(paramCaps)(transform(dd.rhs))
              finally enclosingMethods = savedMethods
            }
          cpy.DefDef(tree)(dd.name, dd.paramss, dd.tpt, newRhs)

        case impl: Template
            if ctx.owner.isClass
            && ctx.owner.is(Flags.Module)
            && ctx.owner.maybeOwner.is(Flags.Package) =>
          // A top-level module whose members are "top-level statements":
          // the REPL session wrapper (`object rs$line$N`), the eval
          // wrapper of the inner compile, a `<file>$package` object
          // holding top-level definitions, or a user-written top-level
          // `object`. That member boundary is what the encl-source
          // slice uses to splice the eval call's marker. Walk the body
          // stats one at a time, recording each as the current
          // top-level for the duration of its transform.
          //
          // For a user-written `object` (standalone compilation) the
          // slice loses the enclosing object, so record an
          // `import <Obj>.{given, *}` to be prepended to the slice;
          // sibling members then resolve against the *runtime* module
          // on the classpath, mirroring the REPL's session imports.
          // REPL / eval wrappers get their imports from the adapter at
          // runtime, and package-object members are visible via the
          // package itself, so both skip the module import.
          val modImport =
            if isEvalInfraModule(ctx.owner) || ctx.owner.isPackageObject then ""
            else s"import ${ctx.owner.sourceModule.fullName.toString}.{given, *}"
          val newStats = impl.body.mapConserve { stat =>
            withTopLevel(stat)(withModuleImport(modImport)(transform(stat)))
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
            && ctx.owner.is(Flags.Module)
            && !ctx.owner.maybeOwner.is(Flags.Package)
            && isStaticModuleChain(ctx.owner) =>
          // A *nested* static `object` (every enclosing layer up to
          // the package is also a module; the top-level case is
          // handled above). Eval calls inside it go through the
          // wrapper compile's module lift, which reroutes the
          // object's private members through runtime reflection.
          // Like class members, those members need a typed reference
          // at the call site to survive DCE: an unreferenced
          // `private val` of an object is otherwise elided from the
          // emitted module class, and the runtime lookup misses it.
          // No `__this__` synthetics: the lift reaches the live
          // module by its static path, not through a captured
          // instance.
          val members = collectClassMembers(impl, ctx.owner.asClass)
          withScope(members)(super.transform(impl))

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
        // Lazy vals are skipped twice over: their self-referencing
        // accessor keeps the slot alive without our help, and the
        // keep-alive *read* would force them at the call site (a
        // self-deadlock when the eval call sits inside the lazy
        // val's own initializer).
        if vd.symbol.exists && !vd.name.isEmpty
          && !vd.symbol.is(Flags.Synthetic) && !vd.symbol.is(Flags.Lazy)
        then
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

    /** Synthetic link bindings for a block-local class `C`:
     *
     *    - `__evalClass_C__` → `classOf[C]`, resolved by the backend
     *      to the *lifted* runtime class, so the wrapper compile can
     *      run instance tests against the original class;
     *    - `__evalNew_C__$<i>` → a factory closure per reachable
     *      constructor (primary first, secondaries in source order).
     *      The closure body is an ordinary `new C(…)`, so LambdaLift
     *      threads `C`'s captured environment through the closure:
     *      exactly the synthetic plumbing the wrapper can't
     *      reconstruct on its own.
     *
     *  Traits and abstract classes contribute only the `classOf`
     *  binding. Private constructors are skipped (the factory
     *  closure would trip the JVM access check after lifting).
     */
    private def localClassBundle(cls: ClassSymbol)(using Context): List[CapturedSym] =
      val src = cls.name.toString
      val classOfCap = CapturedSym(
        cls, EvalNames.classBinding(src), isVar = false, localClassOf = Some(cls))
      val ctorCaps =
        if cls.is(Flags.Trait) || cls.is(Flags.Abstract) then Nil
        else
          constructorsOf(cls).zipWithIndex.collect {
            case (ctor, i) if !ctor.isPrivate =>
              CapturedSym(
                ctor, EvalNames.ctorBinding(src, i), isVar = false,
                ctorFactory = Some((cls, ctor)))
          }
      classOfCap :: ctorCaps

    /** Constructors of `cls` in the order both compiles agree on:
     *  primary first, then secondaries by source position. The
     *  wrapper compile re-elaborates the same class source, so the
     *  index identifies the same constructor on both sides.
     */
    private def constructorsOf(cls: ClassSymbol)(using Context): List[Symbol] =
      val primary = cls.primaryConstructor
      val secondaries = cls.info.decls.toList
        .filter(s => s.isConstructor && s != primary)
        .sortBy(_.span.start)
      if primary.exists then primary :: secondaries else secondaries

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
    private def expandOneArgToFourArg(app: Apply, kind: EvalKind)(using Context): Option[Tree] =
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
          val encl = computeEnclosingSource(span)
          // Same renderer split as [[fillEvalArgs]]: informational
          // with a slice, strict without one.
          val tArg = effectiveTypeArg(app, kind)
          val rendered =
            if tArg eq null then ""
            else if encl.nonEmpty then EvalRewriteTyped.renderTypeInfo(tArg)
            else EvalRewriteTyped.renderType(tArg)
          val wrappedEncl =
            if kind.isSafe && encl.contains(EvalContext.placeholder) then
              encl.replace(
                EvalContext.placeholder,
                s"_root_.dotty.tools.eval.Eval.handleCompileError(${EvalContext.placeholder})"
              )
            else encl
          val wrapTarget = returnWrapTarget
          val keySymOpt = wrapTarget.map { _ =>
            newSymbol(
              ctx.owner, UniqueName.fresh(termName("__evalReturnKey")),
              Flags.Synthetic, defn.ObjectType, coord = span)
          }
          val extraBinds = keySymOpt.map(k => buildBindReturnKey(k, span)).toList
          val bindingsArg = buildBindingsArray(currentBindings, span, extraBinds)
          val expTpeArg = Literal(Constant(rendered)).withSpan(span)
          val enclArg = Literal(Constant(wrappedEncl)).withSpan(span)
          val filled = Apply(newFun, List(closureArg, bindingsArg, expTpeArg, enclArg)).withSpan(span)
          (wrapTarget, keySymOpt) match
            case (Some((meth, resTpe)), Some(keySym)) =>
              wrapWithReturnHandler(filled, keySym, meth, resTpe, span)
            case _ => filled
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

      // enclosingSource: slice from the current top-level statement.
      // Safe-flavor wraps the placeholder in `handleCompileError`.
      val encl = computeEnclosingSource(span)
      val wrappedEncl =
        if kind.isSafe && encl.contains(EvalContext.placeholder) then
          encl.replace(
            EvalContext.placeholder,
            s"_root_.dotty.tools.eval.Eval.handleCompileError(${EvalContext.placeholder})"
          )
        else encl
      argsBuf(enclIdx) = Literal(Constant(wrappedEncl)).withSpan(argsBuf(enclIdx).span)

      // expectedType: rendered from the typed `[T]`, falling back to
      // the typer-recorded prototype when `[T]` was minimized to
      // `Nothing` (see [[effectiveTypeArg]]). The string is
      // re-typechecked at the marker position inside the spliced
      // slice, where locally-scoped names resolve, so a slice gets
      // the informational rendering. Without a slice the wrapper's
      // isolated fallback context only sees global names, so the
      // strict resolvable-only rendering applies.
      val tArg = effectiveTypeArg(app, kind)
      val renderedTpe =
        if tArg eq null then ""
        else if encl.nonEmpty then EvalRewriteTyped.renderTypeInfo(tArg)
        else EvalRewriteTyped.renderType(tArg)
      argsBuf(expIdx) = Literal(Constant(renderedTpe)).withSpan(argsBuf(expIdx).span)

      // bindings: rebuilt from the typed scope, plus the non-local-
      // return key when the call sits directly inside a real method.
      val wrapTarget = returnWrapTarget
      val keySymOpt = wrapTarget.map { _ =>
        newSymbol(
          ctx.owner, UniqueName.fresh(termName("__evalReturnKey")),
          Flags.Synthetic, defn.ObjectType, coord = span)
      }
      val extraBinds = keySymOpt.map(k => buildBindReturnKey(k, span)).toList
      argsBuf(bindIdx) = buildBindingsArray(currentBindings, argsBuf(bindIdx).span, extraBinds)

      val newClauseApp = cpy.Apply(clauseApp)(clauseApp.fun, argsBuf.toList)
      // If the synthetic clause is the outer Apply (single-clause
      // method) we're done; otherwise re-thread the rewritten clause
      // through the chain of intervening Applies.
      val filled =
        if (newClauseApp eq clauseApp) || (clauseApp eq app) then
          if clauseApp eq app then newClauseApp else app
        else rethreadApply(app, clauseApp, newClauseApp)
      (wrapTarget, keySymOpt) match
        case (Some((meth, resTpe)), Some(keySym)) =>
          wrapWithReturnHandler(filled, keySym, meth, resTpe, span)
        case _ => filled

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
          i"`$name` here resolves to ${sym.owner}.${sym.name}, not `dotty.tools.eval.Eval.$name`; the eval rewriter is leaving this call alone. If you intended a custom eval generator, annotate the function with `@evalLike` (or `@evalSafeLike`).",
          app.srcPos
        )

    // Walk the Apply chain so multi-clause `@evalLike` shapes (where
    // the type clause sits below additional value clauses) still find
    // their `[T]`.
    private def extractTypeArg(fun: Tree)(using Context): Type | Null = fun match
      case TypeApply(_, tArg :: _) => tArg.tpe
      case Apply(inner, _) => extractTypeArg(inner)
      case _ => null

    /** The type to render into the `expectedType` slot: the call's
     *  typed `[T]` argument when it is informative, otherwise the
     *  call's prototype recorded by the typer hook
     *  ([[EvalRewriteTyped.recordEvalProto]]).
     *
     *  The fallback recovers the inference case: in
     *  `val a: A = eval("...")` the typer's only constraint on `T`
     *  is `T <: A`, so interpolation minimizes `T := Nothing` and
     *  the constraint's upper bound survives only as the recorded
     *  pt. Safe-flavor calls return `EvalResult[T]`, so there the
     *  body's type is the pt's `EvalResult` argument, not the pt
     *  itself.
     */
    private def effectiveTypeArg(app: Apply, kind: EvalKind)(using Context): Type | Null =
      val tArg = extractTypeArg(app.fun)
      if isInformativeType(tArg) then tArg
      else
        val recorded = recordedProto(app)
        val fallback =
          if !recorded.exists then NoType
          else if !kind.isSafe then recorded
          else recorded.baseType(EvalRewriteTyped.evalResultClass) match
            case AppliedType(_, arg :: Nil) if !arg.isInstanceOf[TypeBounds] => arg
            case _ => NoType
        if isInformativeType(fallback) then fallback else tArg

    /** The prototype attached to this call by the typer hook,
     *  resolved against the final state of inference; `NoType` when
     *  absent or not fully resolvable.
     *
     *  The attachment is looked up along the Apply chain, not just
     *  on `app` itself: a using-clause application is inserted by
     *  the typer's `adapt` *around* the node `typedApply` attached
     *  to, so for `myEval(body)(using ctx)` the pt sits on the inner
     *  `myEval(body)`.
     */
    private def recordedProto(app: Apply)(using Context): Type =
      def find(t: Tree): Type = t match
        case t: Apply =>
          t.getAttachment(EvalRewriteTyped.EvalProto) match
            case Some(pt) => pt
            case None => find(t.fun)
        case TypeApply(fn, _) => find(fn)
        case _ => NoType
      resolveRecordedProto(find(app))

    /** Resolve a recorded prototype to a renderable type.
     *
     *  The pt was captured during typing, so it can mention type
     *  variables that were uninstantiated at that point: an eval
     *  call in argument position of a generic method is typed
     *  against that method's type parameter (`pick(eval("..."), x)`
     *  records `U`). By this phase every variable the run solved
     *  carries its permanent instance, so variables are replaced by
     *  their instances, transitively. Anything that did not resolve
     *  to a concrete type (an uninstantiated variable, a raw
     *  `TypeParamRef`, a skolem from dependent-method typing) makes
     *  the whole pt unusable: bail to `NoType` rather than render a
     *  type the splice cannot re-typecheck.
     */
    private def resolveRecordedProto(pt: Type)(using Context): Type =
      if !pt.exists then NoType
      else
        var ok = true
        val resolver = new TypeMap:
          def apply(t: Type): Type =
            if !ok then t
            else t match
              case tv: TypeVar =>
                if tv.isPermanentlyInstantiated then apply(tv.stripTypeVar)
                else { ok = false; t }
              case _: TypeParamRef | _: SkolemType =>
                ok = false
                t
              case _ => mapOver(t)
        val resolved = resolver(pt)
        if ok then resolved else NoType

    /** False for types the renderer suppresses anyway: missing,
     *  erroneous, `Nothing`, or `Null`.
     */
    private def isInformativeType(tpe: Type | Null)(using Context): Boolean =
      (tpe ne null) && tpe.exists && !tpe.isError && {
        val widened = tpe.widen
        widened.exists && !widened.isError && !EvalRewriteTyped.isUselessType(widened)
      }

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
      val src = sourceFile.content
      if topLevelEnd > src.length || topLevelStart >= topLevelEnd then return ""
      val relStart = evalSpan.start - topLevelStart
      val relEnd = evalSpan.end - topLevelStart
      val topLen = topLevelEnd - topLevelStart
      if relStart < 0 || relEnd > topLen || relStart > relEnd then return ""
      val topSrc = String.valueOf(src, topLevelStart, topLen)
      // The marker ends in `_`, and the lexer absorbs operator
      // characters after a trailing underscore (`foo_:` is one
      // identifier). A call directly followed by an operator char,
      // e.g. the ascription `(eval("..."): Long)`, would glue into
      // `__evalBodyPlaceholder__:`, so pad with a trailing space.
      // (A space never affects parsing here, and never a leading
      // one: indentation is leading whitespace only, and nothing
      // glues into the marker's front.)
      val withMarker =
        topSrc.substring(0, relStart) + markerText + " " + topSrc.substring(relEnd)
      // Prepend the recorded lexical-context imports (file imports,
      // package import, enclosing-object import) so the wrapper
      // compile resolves the same names the original file saw. All
      // empty in a REPL compile, where session imports are passed to
      // the adapter at runtime instead.
      def withContextImports(slice: String): String =
        val imps = contextImports
        if imps.isEmpty then slice
        else imps.mkString("", "\n", "\n") + slice
      topLevelKind match
        case TopKind.Definition => withContextImports(withMarker)
        case TopKind.Expression => withContextImports(s"val __unused__ : Any = { $withMarker }")
        case TopKind.Unknown => ""

    private def composeChainedEncl(innerSpan: Span, outerBody: String, outerEncl: String): String =
      if !innerSpan.exists then return ""
      val s = innerSpan.start
      val e = innerSpan.end
      if s < 0 || e > outerBody.length || s > e then return ""
      // Trailing space for the same lexer-gluing reason as the
      // direct slice above.
      val outerBodyWithInnerMarker =
        outerBody.substring(0, s) + EvalBodyPlaceholder.Marker + " " + outerBody.substring(e)
      outerEncl.replace(
        EvalBodyPlaceholder.Marker,
        EvalBodyPlaceholder.emit(outerBodyWithInnerMarker)
      )

    /** `scala.Array(bindings...)` typed as `Array[Eval.Binding]`.
     *  Empty case still emits a typed `Array[Eval.Binding]` so the
     *  surrounding 4-arg overload solves correctly. `extra` carries
     *  pre-built bind trees appended after the scope captures
     *  (currently only the non-local-return key).
     */
    private def buildBindingsArray(caps: List[CapturedSym], span: Span, extra: List[Tree] = Nil)(using Context): Tree =
      val elemTpe: Type = EvalRewriteTyped.bindingClass.typeRef
      val elems: List[Tree] = caps.map(c => buildBind(c, span)) ++ extra
      JavaSeqLiteral(elems, TypeTree(elemTpe)).withSpan(span)

    /** The method a body `return` would target, when the eval call
     *  sits directly inside a real (named, non-constructor) method.
     *  Inside a lambda there is no target: Scala 3 has no non-local
     *  returns, and the inner compile rejects the body's `return`
     *  with the standard diagnostic, matching source semantics at
     *  that position.
     */
    private def returnWrapTarget(using Context): Option[(Symbol, Type)] =
      enclosingMethods.headOption.filter { (meth, resTpe) =>
        meth.exists
          && meth.is(Flags.Method)
          && !meth.isAnonymousFunction
          && !meth.isConstructor
          && resTpe.exists && !resTpe.isError
          && !resTpe.isRef(defn.NothingClass)
      }

    /** `Eval.bindSynthetic("__evalReturnKey__", <key>)`. */
    private def buildBindReturnKey(keySym: Symbol, span: Span)(using Context): Tree =
      val nameLit = Literal(Constant(EvalNames.ReturnKeyBinding)).withSpan(span)
      discardUses:
        ref(EvalRewriteTyped.bindSyntheticSym)
          .appliedTo(nameLit, ref(keySym).withSpan(span), bindingTpeLit(null, span))
          .withSpan(span)

    /** Wrap a filled eval call so a `return` inside the body can
     *  reach the enclosing method's frame:
     *
     *  ```
     *  {
     *    val __evalReturnKey: Object = new Object()
     *    try <call with __evalReturnKey__ binding appended>
     *    catch case ex: EvalNonLocalReturn if ex.key eq __evalReturnKey =>
     *      return ex.value.asInstanceOf[R]   // ordinary return from m
     *  }
     *  ```
     *
     *  [[ExtractEvalBody]] lowers the body's `return expr` to
     *  `throw new EvalNonLocalReturn(<key binding>, expr)`; the
     *  throwable unwinds through `evaluate()` and the adapter to
     *  this catch. The key is a fresh object per *execution*, so
     *  recursive frames stay distinct, and the identity guard
     *  re-throws returns belonging to other frames (including outer
     *  eval calls in a nested chain).
     */
    private def wrapWithReturnHandler(
        call: Tree, keySym: Symbol, meth: Symbol, resTpe: Type, span: Span
    )(using Context): Tree =
      val nlrCls = EvalRewriteTyped.evalNonLocalReturnClass
      val keyVal = ValDef(keySym.asTerm, New(defn.ObjectType, Nil)).withSpan(span)
      val exSym = newSymbol(
        ctx.owner, UniqueName.fresh(termName("__evalNLR")),
        Flags.Case | Flags.Synthetic, nlrCls.typeRef, coord = span)
      val pat = Bind(exSym, Typed(Underscore(nlrCls.typeRef), TypeTree(nlrCls.typeRef))).withSpan(span)
      val guard = ref(exSym).select(termName("key"))
        .select(defn.Object_eq).appliedTo(ref(keySym)).withSpan(span)
      val retVal = ref(exSym).select(termName("value")).cast(resTpe)
      val ret = Return(retVal, ref(meth)).withSpan(span)
      val tryTree = Try(call, List(CaseDef(pat, guard, ret)), EmptyTree).withSpan(span)
      Block(keyVal :: Nil, tryTree).withSpan(span)

    /** Dispatch a `CapturedSym` to its appropriate binding builder:
     *
     *    - `isVar`         → `Eval.bindVar(name, Eval.varRef(get, set))`
     *    - `isGiven`       → `Eval.bindGiven(name, value)`
     *    - `isByName`      → `Eval.bind(name, () => name)` (Function0 thunk
     *                        so the body's post-ElimByName `apply()` lines
     *                        up; see [[buildBindByName]]).
     *    - `isDef`         → `Eval.bind(name, eta-expansion)`
     *    - `localClassOf`  → `Eval.bindSynthetic(name, classOf[C])`
     *    - `ctorFactory`   → `Eval.bindSynthetic(name, (args…) => new C(args…))`
     *    - `isModuleRef`   → `Eval.bindSynthetic(name, M)`
     *    - `selfThisCls` / `classMemberOf`
     *                      → `Eval.bindSynthetic(name, value)` (compiler-only
     *                        captures: the enclosing-`this` chain and the
     *                        DCE-keeper class-member reads)
     *    - default         → `Eval.bind(name, value)`
     *
     *  Every form also passes a trailing type-string argument (see
     *  [[Eval.Binding.tpe]]): the rendered static type of the
     *  captured name, or the empty string for the compiler-link
     *  synthetics (classOf, constructor factories, the return key)
     *  whose value type is not a user-facing concept.
     */
    private def buildBind(c: CapturedSym, span: Span)(using Context): Tree =
      if c.isVar then buildBindVar(c, span)
      else if c.isGiven then buildBindGiven(c, span)
      else if c.isByName then buildBindByName(c, span)
      else if c.isDef then buildBindDef(c, span)
      else if c.localClassOf.isDefined then buildBindClassOf(c, span)
      else if c.ctorFactory.isDefined then buildBindCtorFactory(c, span)
      else
        val bindFn =
          if c.isSyntheticBinding then EvalRewriteTyped.bindSyntheticSym
          else EvalRewriteTyped.bindSym
        // `__this__` synthetics carry no symbol; their static type is
        // the class itself. Everything else reads the captured
        // symbol's declared type.
        val capturedTpe: Type | Null = c.selfThisCls match
          case Some(cls) => cls.typeRef
          case None => if c.sym.exists then c.sym.info else null
        val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
        discardUses:
          ref(bindFn)
            .appliedTo(nameLit, readRef(c, span), bindingTpeLit(capturedTpe, span))
            .withSpan(span)

    /** `Eval.bindSynthetic("__evalClass_C__", classOf[C])`. Type
     *  params are instantiated to `Any` purely so the class literal's
     *  carried type is well-formed; erasure reduces it to the runtime
     *  class either way.
     */
    private def buildBindClassOf(c: CapturedSym, span: Span)(using Context): Tree =
      val cls = c.localClassOf.get
      val targs = cls.typeParams.map(_ => defn.AnyType)
      val clsTpe = if targs.isEmpty then cls.typeRef else cls.typeRef.appliedTo(targs)
      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      discardUses:
        ref(EvalRewriteTyped.bindSyntheticSym)
          .appliedTo(nameLit, clsOf(clsTpe).withSpan(span), bindingTpeLit(null, span))
          .withSpan(span)

    /** `Eval.bindSynthetic("__evalNew_C__$i", (p1…pn) => new C(p1…pn))`.
     *
     *  Same flattened, `Any`-typed lambda technique as
     *  [[buildBindDef]]: every value param is typed `Any` (cast back
     *  to the constructor's expected type inside the body), type
     *  params are instantiated to `Any`, and multiple clauses are
     *  uncurried into one. The wrapper compile flattens the body's
     *  `new C(…)` arg lists the same way, so a single
     *  `FunctionN.apply(args…)` call lines up.
     *
     *  The `new C(…)` inside the closure is ordinary code in `C`'s
     *  defining scope: LambdaLift extends it with `C`'s captured
     *  free variables and closes the factory over them, which is
     *  exactly the environment the wrapper-side construction needs
     *  but cannot reconstruct.
     */
    private def buildBindCtorFactory(c: CapturedSym, span: Span)(using Context): Tree =
      val (cls, ctor) = c.ctorFactory.get
      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      val (typeArgs: List[Type], methodLikeTpe: Type) = ctor.info match
        case poly: PolyType =>
          val anys: List[Type] = poly.paramRefs.map(_ => defn.AnyType)
          (anys, poly.instantiate(anys))
        case other =>
          (Nil, other)
      def flatten(t: Type): (List[List[Type]], List[List[TermName]]) = t match
        case mt: MethodType =>
          val (rest, namesRest) = flatten(mt.resType)
          (mt.paramInfos :: rest, mt.paramNames :: namesRest)
        case _ =>
          (Nil, Nil)
      val (clauseInfos, clauseNames) = flatten(methodLikeTpe)
      val clsTpe = if typeArgs.isEmpty then cls.typeRef else cls.typeRef.appliedTo(typeArgs)
      val flatNames = clauseNames.flatten
      val flatInfos = clauseInfos.flatten
      val lambdaParamTpes: List[Type] = flatInfos.map {
        case et: ExprType => et
        case _ => defn.AnyType
      }
      val methTpe = MethodType(flatNames)(_ => lambdaParamTpes, _ => defn.AnyType)
      val factory = Lambda(methTpe, params =>
        val callArgs = params.lazyZip(flatInfos).map { (p, t) =>
          t match
            case _: ExprType => p
            case _ => p.cast(t)
        }
        def regroup(xs: List[Tree], sizes: List[Int]): List[List[Tree]] = sizes match
          case Nil => Nil
          case n :: rest =>
            val (head, tail) = xs.splitAt(n)
            head :: regroup(tail, rest)
        val ctorSel = New(clsTpe).select(ctor)
        val ctorTyped = if typeArgs.isEmpty then ctorSel else ctorSel.appliedToTypes(typeArgs)
        ctorTyped.appliedToArgss(regroup(callArgs, clauseInfos.map(_.length)))
      ).withSpan(span)
      discardUses:
        ref(EvalRewriteTyped.bindSyntheticSym)
          .appliedTo(nameLit, factory, bindingTpeLit(null, span))
          .withSpan(span)

    /** Stamp the `Eval.bind*(...)` call with [[CheckCaptures.DiscardUses]]
     *  so the capture checker rechecks the whole application — including
     *  the value argument and any closures it nests — with
     *  `withDiscardedUses`, i.e. without recording captured-reference
     *  uses into enclosing environments.
     *
     *  Why this is needed: at every `eval` / `agent` call site the
     *  rewriter emits `Eval.bind("x", x)` for each captured local. The
     *  reference to `x` carries `x`'s capture set; without suppression
     *  the use is recorded into every enclosing function literal —
     *  including a surrounding lambda whose expected capture set forbids
     *  it (e.g. `String ->{any.rd} Int` in `Classified.map`). The binding
     *  only travels to the spliced eval body, which the driver
     *  re-typechecks in its own enclosing source where `x`'s capability
     *  is already legal; the surrounding lambda never consumes the
     *  capability through this binding.
     *
     *  We use the attachment rather than emitting
     *  `caps.unsafe.unsafeDiscardUses(...)` directly because the latter
     *  is `@rejectSafe` and would fail `SafeRefs.checkSafe` when the
     *  live REPL session runs in safe mode. No extra runtime call is
     *  introduced — the attachment rides on the `Eval.bind*` Apply we
     *  already emit.
     */
    private def discardUses(bindApp: Tree)(using Context): Tree =
      bindApp.withAttachment(CheckCaptures.DiscardUses, ())

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

    /** String literal carrying the source rendering of a binding's
     *  static type, fed to the trailing `tpe` parameter of the
     *  `Eval.bind*` methods. Uses the informational
     *  [[EvalRewriteTyped.renderTypeInfo]] (not `expectedType`'s
     *  strict `renderType`): the string is never spliced into a
     *  wrapper compile, so locally-scoped classes, local aliases,
     *  and enclosing type parameters render as the names code at
     *  the call site would use.
     */
    private def bindingTpeLit(tpe: Type | Null, span: Span)(using Context): Tree =
      val rendered = if tpe == null then "" else EvalRewriteTyped.renderTypeInfo(tpe)
      Literal(Constant(rendered)).withSpan(span)

    /** `Eval.bindGiven(name, value)` for given-val captures. */
    private def buildBindGiven(c: CapturedSym, span: Span)(using Context): Tree =
      val nameLit = Literal(Constant(c.sourceName)).withSpan(span)
      discardUses:
        ref(EvalRewriteTyped.bindGivenSym)
          .appliedTo(nameLit, readRef(c, span), bindingTpeLit(c.sym.info, span))
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
      val fn = Lambda(methTpe, _ => readRef(c, span)).withSpan(span)
      // The recorded type is the by-name result type: that's what
      // the body's bare `name` reference has.
      discardUses:
        ref(EvalRewriteTyped.bindSym)
          .appliedTo(nameLit, fn, bindingTpeLit(resultTpe, span))
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
      // The recorded type is the def's declared signature (e.g.
      // `(x: Int): String`), not the `Any`-typed eta-expansion the
      // value actually carries.
      discardUses:
        ref(EvalRewriteTyped.bindSym)
          .appliedTo(nameLit, etaTree, bindingTpeLit(defSym.info, span))
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
      // The recorded type is the var's element type (the `T` of
      // `VarRef[T]`), matching what the body's reads/writes see.
      discardUses:
        ref(EvalRewriteTyped.bindVarSym)
          .appliedTo(nameLit, varRef, bindingTpeLit(elemTpe, span))
          .withSpan(span)

  end EvalRewriteTransformer

end EvalRewriteTyped

object EvalRewriteTyped:

  val name: String = "evalRewriteTyped"

  /** Attachment key carrying the prototype (the typer's expected
   *  type) of an eval-like call, recorded by [[recordEvalProto]] at
   *  the end of `Applications.typedApply`. Sticky: only sticky
   *  attachments survive the tree copies phases make between Typer
   *  and this rewriter (`withAttachmentsFrom` drops plain keys).
   *
   *  This is how the rewriter learns the constraint upper bound of
   *  an inferred `[T]`: `val a: A = eval("...")` constrains `T` only
   *  by `T <: A`, so interpolation minimizes `T := Nothing` and the
   *  bound is gone from the typed tree. The pt at the call *is* that
   *  bound (the call's `T` occurs only in result position, so its
   *  entire constraint comes from `constrainResult` against pt).
   *  Recording pt instead of touching instantiation keeps inference
   *  byte-identical; the recorded type is resolved against the final
   *  instantiations when the rewriter consumes it.
   */
  private[eval] val EvalProto: Property.StickyKey[Type] = Property.StickyKey()

  private val EvalMethodName: TermName = termName("eval")
  private val EvalSafeMethodName: TermName = termName("evalSafe")

  /** Typer-side identification of an eval-like callee, mirroring the
   *  rewriter's `classifyCall` (symbol identity for `Eval.eval` /
   *  `Eval.evalSafe`, annotation for user generators). Resilient to
   *  the eval API being absent from the compile classpath (plain
   *  compiles must not crash): lookups go through `get*IfDefined`.
   *
   *  The name / has-annotations pre-filters keep the per-Apply cost
   *  of the hook to a couple of reference comparisons; the symbol
   *  lookups behind them are deliberately *not* cached here. Any
   *  static cache would need to key on the context base AND the run
   *  (REPL sessions create fresh context bases whose run ids
   *  restart, and test suites run many sessions per JVM, possibly
   *  concurrently), so stale or torn entries would silently disable
   *  the hook. The lookups are a few cached-denotation accesses and
   *  only happen for callees that pass the pre-filters.
   */
  private def isEvalLikeRef(sym: Symbol)(using Context): Boolean =
    sym.exists && {
      val n = sym.name
      if (n eq EvalMethodName) || (n eq EvalSafeMethodName) then
        val mod = getModuleIfDefined("dotty.tools.eval.Eval")
        mod.exists && sym.maybeOwner == mod.moduleClass
      else if sym.annotations.nonEmpty then
        val like = getClassIfDefined("dotty.tools.eval.evalLike")
        like.exists && sym.hasAnnotation(like) || {
          val safeLike = getClassIfDefined("dotty.tools.eval.evalSafeLike")
          safeLike.exists && sym.hasAnnotation(safeLike)
        }
      else false
    }

  /** Only record prototypes the typer actually constrained the call's
   *  result against, in a shape the rewriter can later render:
   *
   *    - proto types (`FunProto` when the result is applied further,
   *      `SelectionProto` for `eval(...).member`, `IgnoredProto`,
   *      view/poly protos) carry structure, not a result type, and
   *      `constrainResult` does not establish a plain `T <: pt` for
   *      them;
   *    - `WildcardType` is the no-expectation case (statements);
   *    - a repeated-param type leaks the vararg formal, which is not
   *      legal ascription source.
   *
   *  The pt may freely contain *type variables* (an eval argument to
   *  a generic method is typed against that method's uninstantiated
   *  type parameter): those resolve through their permanent
   *  instantiations when the rewriter reads the attachment.
   */
  private def isRecordablePt(pt: Type)(using Context): Boolean = pt match
    case _: ProtoType | _: WildcardType => false
    case _ => pt.exists && !pt.isError && pt.isValueType && !pt.isRepeatedParam

  /** Hook called at the end of `Applications.typedApply` with the
   *  final typed application and its prototype. Attaches the pt to
   *  the Apply node of eval-like calls.
   *
   *  Placement at the *end* of typedApply matters for overloading
   *  and implicit search: overload resolution, `tryEither` retries,
   *  and implicit-on-qualifier insertion have all settled by then,
   *  so the attachment lands on the surviving alternative only
   *  (discarded speculative attempts die with their trees). The
   *  using-clause application, if any, is inserted later by `adapt`
   *  *around* this node, which is why the rewriter looks the
   *  attachment up along the Apply chain rather than on the
   *  outermost node. Named/default-arg lifting can wrap the call in
   *  a Block, hence the strip. Re-typing after Typer (ReTyper,
   *  TreeChecker) must not overwrite a recorded pt with a synthetic
   *  one, hence the `isAfterTyper` guard.
   */
  def recordEvalProto(tree: Tree, pt: Type)(using Context): Unit =
    if ctx.isAfterTyper then return
    def strip(t: Tree): Tree = t match
      case Block(_, expr) => strip(expr)
      case t => t
    strip(tree) match
      case app: Apply if isEvalLikeRef(methPart(app).symbol) && isRecordablePt(pt) =>
        app.putAttachment(EvalProto, pt)
      case _ =>

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
    requiredModule("dotty.tools.eval.Eval").moduleClass

  private def evalLikeAnnotClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.eval.evalLike")

  private def evalSafeLikeAnnotClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.eval.evalSafeLike")

  private def evalResultClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.eval.EvalResult")

  private def bindingClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.eval.Eval.Binding")

  private def bindSym(using Context): Symbol =
    requiredModule("dotty.tools.eval.Eval").requiredMethod("bind")

  private def bindVarSym(using Context): Symbol =
    requiredModule("dotty.tools.eval.Eval").requiredMethod("bindVar")

  private def bindGivenSym(using Context): Symbol =
    requiredModule("dotty.tools.eval.Eval").requiredMethod("bindGiven")

  private def bindSyntheticSym(using Context): Symbol =
    requiredModule("dotty.tools.eval.Eval").requiredMethod("bindSynthetic")

  private def evalNonLocalReturnClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.eval.EvalNonLocalReturn")

  private def varRefSym(using Context): Symbol =
    requiredModule("dotty.tools.eval.Eval").requiredMethod("varRef")

  private def supplierClass(using Context): ClassSymbol =
    requiredClass("java.util.function.Supplier")

  private def consumerClass(using Context): ClassSymbol =
    requiredClass("java.util.function.Consumer")

  /** Strict rendering of `tpe` as a Scala source string. Returns
   *  the empty string when the type is degenerate (`Nothing`,
   *  `Null`, error type) or mentions a symbol that only resolves in
   *  the call site's local scope — an enclosing method's type
   *  parameter, or a locally-scoped class.
   *
   *  Used for `expectedType` only when the rewriter could not
   *  compute an enclosing-source slice: the wrapper compile then
   *  types the body in an isolated fallback context where only
   *  globally reachable names resolve, so a local name in the
   *  ascription would fail the compile.
   *
   *  Capture annotations (`^`, `^{...}`) are kept when capture
   *  checking is enabled in the live session, so the inner verify
   *  compile sees the exact capability set the user declared.
   *  Otherwise they're stripped — the wrapper's val type only needs
   *  the underlying erased shape.
   */
  private[eval] def renderType(tpe: Type)(using Context): String =
    renderTypeImpl(tpe, strict = true)

  /** Informational rendering: the type exactly as code written at
   *  the call site could name it. Locally-scoped classes, local
   *  type aliases, and enclosing method type parameters are all in
   *  scope there, so they are kept rather than bailed out on; empty
   *  only for degenerate types (`Nothing`, `Null`, errors) or a
   *  failed printer.
   *
   *  Used for [[Eval.Binding.tpe]] (display only) and for
   *  `expectedType` whenever an enclosing-source slice exists: the
   *  runtime re-typechecks the string as the spliced
   *  `val __evalResult: <tpe>` ascription at the marker position
   *  inside the slice, where exactly these names resolve.
   */
  private[eval] def renderTypeInfo(tpe: Type)(using Context): String =
    renderTypeImpl(tpe, strict = false)

  private def renderTypeImpl(tpe: Type, strict: Boolean)(using Context): String =
    if tpe == null || !tpe.exists || tpe.isError then return ""
    val widened = tpe.widen
    if !widened.exists || widened.isError then return ""
    // `Nothing` / `Null` are suppressed in both modes: an eval call
    // in an unconstrained position infers `T := Nothing`, and an
    // expectedType of "Nothing" would force the wrapper's
    // `val __evalResult: Nothing = <body>` ascription onto bodies
    // that compile fine without it.
    if isUselessType(widened) then return ""
    val resolved = if strict then dealiasLocalAliases(widened) else widened
    if strict && mentionsLocallyScopedSymbol(resolved) then return ""
    val cleaned =
      if ctx.settings.YccNew.value || ctx.settings.language.value.contains("experimental.captureChecking")
      then resolved
      else stripCaptureAnnotations(resolved)
    val printCtx = ctx.fresh.setSetting(ctx.settings.color, "never")
    try
      val shown = cleaned.show(using printCtx)
      // The `A#B` rewrite is a splice-resolvability hack; `A.this.B`
      // is already legal source at the call site, so the
      // informational form keeps it.
      if strict then shown.replace(".this.", "#") else shown
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
