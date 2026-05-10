package dotty.tools
package repl
package eval

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.MacroTransform
import dotty.tools.dotc.util.SrcPos

/** Post-typer phase that pulls the typed eval body out of its splice
 *  point and into the synthesised `__Expression.evaluate` method.
 *
 *  Three steps:
 *
 *    1. Walk the unit. At the spliced `val __evalResult`, capture
 *       its rhs into [[EvalStore]] and replace it with
 *       `null.asInstanceOf[T]`. The val must keep its declared type
 *       so `Ident(__evalResult)` at the splice's tail still resolves
 *       and the enclosing method bytecode is well-formed (the method
 *       is never executed, but bytecode generation still runs).
 *    2. At `__Expression.evaluate`, run the captured body through
 *       [[ExtractTransformer]] to rewrite outer-scope references into
 *       `reflectEval` placeholders, and install the result as the rhs.
 *    3. The [[DenotTransformer]] hook re-parents every body-local
 *       symbol from the val to `evaluate` so they survive the move.
 *       Per `transformPhase = this.next`, the re-owning takes effect
 *       at the following phase.
 *
 *  Outer-scope references that don't fit any known strategy produce
 *  an internal-error diagnostic; [[ResolveEvalAccess]] consumes the
 *  emitted strategies and lowers them to reflective accessor calls.
 */
private[eval] class ExtractEvalBody(config: EvalCompilerConfig, store: EvalStore)
  extends MacroTransform with DenotTransformer:

  override def phaseName: String = ExtractEvalBody.name

  /** DenotTransformer convention: re-owning takes effect at the next
   *  phase, so this phase still walks the original val-owned tree.
   */
  override def transformPhase(using Context): Phase = this.next

  /** Re-own symbols whose owner was the spliced `__evalResult` val to
   *  `__Expression.evaluate`. Identified by name so this is a no-op
   *  on units that don't carry the marker.
   */
  override def transform(ref: SingleDenotation)(using Context): SingleDenotation =
    ref match
      case ref: SymDenotation if isExpressionVal(ref.symbol.maybeOwner) =>
        ref.copySymDenotation(owner = config.evaluateMethod)
      case _ => ref

  override protected def newTransformer(using Context): Transformer =
    new ExtractEvalBodyTransformer

  private def isExpressionVal(sym: Symbol)(using Context): Boolean =
    sym.exists && sym.name == SpliceEvalBody.EvalResultName

  /** True iff `sym` is the wrapper module class or its companion val.
   *  The wrapper is synthesised in `EvalAdapter` and always carries
   *  `__EvalWrapper` as part of its name; a `contains` check covers
   *  both shapes the adapter emits — `__EvalWrapper_<uuid>` (direct
   *  bridge use) and `rs$line$<uuid>$__EvalWrapper` (REPL session use,
   *  where the `rs$line$` prefix makes `NameOps.isReplWrapperName`
   *  recognise it as a REPL wrapper).
   */
  private def isWrapperSymbol(sym: Symbol)(using Context): Boolean =
    sym.exists && sym.name.toString.contains(ExtractEvalBody.WrapperMarker)

  /** True iff the wrapper module's body has no nested class declarations.
   *  Used to gate the wrapper drop in the `PackageDef` case.
   */
  private def holdsNoNestedClasses(wrapperClass: TypeDef)(using Context): Boolean =
    wrapperClass.rhs match
      case impl: Template =>
        !impl.body.exists {
          case td: TypeDef => td.isClassDef
          case _ => false
        }
      case _ => true

  private class ExtractEvalBodyTransformer extends Transformer:
    private var bodyTree: Tree | Null = null

    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case PackageDef(pid, stats) =>
          // Walk `__Expression` last so its `evaluate` method sees a
          // populated `bodyTree`.
          val (exprClassDef, others) = stats.partition { stat =>
            stat.symbol.exists && stat.symbol == config.expressionClass
          }
          val transformedStats = (others ++ exprClassDef).map(transform)
          // Drop the wrapper module from the package when it holds only
          // dead members (helper stubs retargeted away by
          // `ExtractTransformer`, plus renamed `__eval_<name>__` defs
          // whose body became `null.asInstanceOf[T]` when their
          // `__evalResult` val rhs was drained). Skip the drop when the
          // wrapper holds nested classes: user code without a live
          // `import rs$line$N.*` (e.g. an inline `object A` defined in
          // the same enclosing source) sees the wrapper's nested copy
          // as its CallMethod-strategy className, so dropping the
          // wrapper would break the runtime `loadClass` lookup.
          // A typed tpd ModuleDef shows up as both a TypeDef for the
          // module class AND a ValDef for the module instance — drop
          // both so codegen doesn't synthesise an empty companion.
          val wrapperClass = transformedStats.collectFirst {
            case stat: TypeDef if stat.isClassDef && isWrapperSymbol(stat.symbol) => stat
          }
          val canDropWrapper = wrapperClass.exists(holdsNoNestedClasses)
          val pruned =
            if !canDropWrapper then transformedStats
            else transformedStats.filter {
              case stat: TypeDef if isWrapperSymbol(stat.symbol) => false
              case stat: ValDef if isWrapperSymbol(stat.symbol) => false
              case _ => true
            }
          cpy.PackageDef(tree)(pid, pruned)

        case tree: ValDef if isExpressionVal(tree.symbol) =>
          bodyTree = tree.rhs
          store.store(tree.symbol)
          val defaultRhs = Literal(Constant(null)).cast(tree.tpt.tpe).withSpan(tree.rhs.span)
          cpy.ValDef(tree)(rhs = defaultRhs)

        case tree: DefDef if tree.symbol == config.evaluateMethod =>
          val captured = bodyTree
          if captured == null then tree
          else cpy.DefDef(tree)(rhs = ExtractTransformer.transform(captured))

        case _ => super.transform(tree)
  end ExtractEvalBodyTransformer

  /** Walks the captured body. References local to the body or
   *  globally accessible pass through; references to outer locals,
   *  outer `this`, and inaccessible class members are rewritten to
   *  `reflectEval` placeholders carrying a [[ReflectEvalStrategy]]
   *  attachment that [[ResolveEvalAccess]] later lowers to the
   *  matching reflective accessor.
   */
  private object ExtractTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree = tree match
      case _: ImportOrExport => tree

      // Retarget the body's references to the wrapper's typer-only
      // helper stubs (`__refl_get__/_set__/_call__`) onto the inherited
      // copies on `EvalExpressionBase`. After this rewrite the body
      // calls `this.__refl_get__(...)` instead of
      // `__EvalWrapper$.MODULE$.__refl_get__(...)`, so the wrapper class
      // is no longer referenced from `evaluate()` at runtime.
      case tree: Ident if isWrapperReflHelper(tree.symbol) =>
        retargetReflHelper(tree)

      case tree: Select if isWrapperReflHelper(tree.symbol) =>
        retargetReflHelper(tree)

      case tree: This =>
        val cls = tree.symbol
        if cls == config.expressionClass then super.transform(tree)
        else if cls.is(ModuleClass) && isGloballyAccessible(cls) then
          super.transform(tree)
        else if cls.isClass && store.classOwners.contains(cls) then
          thisOrOuterValue(tree, cls.asClass)
        else
          report.error(
            s"eval: cannot reach outer `this` of class `${cls.name}` " +
              "from the eval call site (not in the captured owner chain).",
            tree.srcPos
          )
          super.transform(tree)

      // Captured-var write: outer method-local `var x` gets `x = v`
      // routed through the bind site's `VarRef.set(v)`. Body-local
      // var writes (a `var p` declared inside the eval body itself)
      // must fall through to `super.transform`; their symbol isn't in
      // the bindings array.
      case Assign(lhs: Ident, rhs)
          if isLocalVariable(lhs.symbol) && !isLocalToBody(lhs.symbol) =>
        setLocalValue(tree, lhs.symbol.asTerm, transform(rhs))

      case tree @ Assign(lhs, rhs) if isInaccessibleField(lhs) =>
        setField(tree, transformedQualifier(lhs), lhs.symbol.asTerm, transform(rhs))

      case tree: Ident =>
        val sym = tree.symbol
        if !sym.exists || isLocalToBody(sym) || isGloballyAccessible(sym) then
          super.transform(tree)
        else if isAccessibleViaStaticPrefix(tree) then
          // Typed prefix routes to the symbol via a static module
          // path (e.g. `Predef.charWrapper`, declared on
          // `LowPriorityImplicits` but reached through `Predef`).
          // Codegen lowers this to a static path reachable from
          // any caller, so no rewrite is needed.
          super.transform(tree)
        else if isTermOwnedModule(sym) then
          // Companion module of a method-local class (e.g. the
          // implicit companion of `case class C` inside `def f`).
          // LambdaLift hoists both the class and its module to
          // top-level so they're directly accessible from
          // `__Expression`; no reflective indirection needed.
          super.transform(tree)
        else if isLocalVariable(sym) then
          getLocalValue(tree, sym.asTerm)
        else if isInaccessibleField(tree) then
          getField(tree, transformedQualifier(tree), sym.asTerm)
        else if isInaccessibleMethod(tree) then
          callMethod(tree, transformedQualifier(tree), sym.asTerm, Nil)
        else if isOuterMethodLocalDef(sym) then
          captureLocalMethod(tree, sym.asTerm, Nil)
        else
          report.error(
            s"eval: cannot reference outer symbol `${sym.name}` (owner: ${sym.owner}).",
            tree.srcPos
          )
          super.transform(tree)

      case tree: Select if isInaccessibleField(tree) =>
        getField(tree, transform(tree.qualifier), tree.symbol.asTerm)

      // Member access on a captured local-class instance. The
      // wrapper compile re-elaborates the class declaration as a
      // fresh JVM class, so direct `getfield` / `invokevirtual`
      // on the wrapper's symbol would target a different class
      // than the runtime instance. Route through the reflective
      // helpers, which use `obj.getClass` (see runtime fallback in
      // SpliceEvalBody when className is empty).
      case tree: Select if isTermOwnedClassFieldAccess(tree) =>
        getField(tree, transform(tree.qualifier), tree.symbol.asTerm)

      case tree: Apply if isInaccessibleMethod(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: Apply if isTermOwnedClassMethodCall(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: TypeApply if isInaccessibleMethod(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: TypeApply if isTermOwnedClassMethodCall(tree) =>
        val args = transformedMethodArgs(tree)
        callMethod(tree, transformedQualifier(tree), tree.symbol.asTerm, args)

      case tree: Select if isInaccessibleMethod(tree) =>
        // Bare Select to a private method (no Apply yet — i.e. taken
        // as a function value). Treat as a 0-arg call; eta-expansion
        // of private methods is rare enough to revisit if needed.
        callMethod(tree, transform(tree.qualifier), tree.symbol.asTerm, Nil)

      case tree: Select if isTermOwnedClassMethodCall(tree) =>
        // Bare Select to a 0-arg method on a term-owned class
        // (e.g. `c.toString`). Same reasoning as the inaccessible-
        // method bare-Select above.
        callMethod(tree, transform(tree.qualifier), tree.symbol.asTerm, Nil)

      // Outer block-local def: `g(args)` where `g` is a `def`
      // declared in an enclosing method. The rewriter captured it as
      // an eta-expanded `FunctionN` binding under the def's bare
      // name; lower the whole call to MethodCapture.
      case tree: Apply if isOuterMethodLocalDef(tree.fun.symbol) =>
        captureLocalMethod(tree, tree.fun.symbol.asTerm, transformedMethodArgs(tree))

      case tree: TypeApply if isOuterMethodLocalDef(tree.symbol) =>
        captureLocalMethod(tree, tree.symbol.asTerm, Nil)

      // For other Selects we descend; the interesting cases (outer
      // This, outer Ident) are handled above on the qualifier.
      case _ => super.transform(tree)
    end transform

    private def getLocalValue(tree: Tree, sym: TermSymbol)(using Context): Tree =
      // For a binding whose declared type is a term-owned class, the
      // wrapper's symbol is a re-elaborated JVM class distinct from the
      // runtime instance's class. A `checkcast` to the wrapper symbol
      // would fail (different JVM names). Cast to `Object` and let
      // member accesses dispatch through the reflective helpers, which
      // use `obj.getClass` to find fields / methods on the actual
      // runtime class. (The body's typecheck still succeeds because the
      // body is spliced in `f`'s lexical scope where the wrapper's
      // `class C` is in scope; only the `__Expression.evaluate` boundary
      // uses Object.)
      val castTo =
        if isTermOwnedClass(sym.info) then defn.ObjectType
        else tree.tpe.widen
      buildReflectEvalCast(tree, nullLiteral,
        ReflectEvalStrategy.LocalValue(sym, isByName(sym.info)), Nil, castTo)

    private def setLocalValue(tree: Tree, sym: TermSymbol, rhs: Tree)(using Context): Tree =
      reflectEvalPlaceholder(tree, nullLiteral,
        ReflectEvalStrategy.LocalValueAssign(sym), rhs :: Nil)

    /** Cast target is the class type itself, not `tree.tpe.widen` —
     *  `ThisType.widen` would lose precision needed by surrounding
     *  member selects. Term-owned classes (e.g. `eval` inside a
     *  method-local class) cast to `Object` for the same reason
     *  `getLocalValue` does: the wrapper's `Inner` symbol is a
     *  different JVM class from the runtime `this`.
     */
    private def getThisObject(tree: Tree, cls: ClassSymbol)(using Context): Tree =
      val castTo = if isTermOwnedSymbol(cls) then defn.ObjectType else cls.typeRef
      buildReflectEvalCast(tree, nullLiteral, ReflectEvalStrategy.This(cls), Nil, castTo)

    private def getOuter(tree: Tree, qualifier: Tree, outerCls: ClassSymbol)(using Context): Tree =
      buildReflectEvalCast(tree, qualifier, ReflectEvalStrategy.Outer(outerCls), Nil, outerCls.typeRef)

    private def buildReflectEvalCast(
        tree: Tree,
        qualifier: Tree,
        strategy: ReflectEvalStrategy,
        args: List[Tree],
        castTo: Type
    )(using Context): Tree =
      val evalArgs = List(
        qualifier,
        Literal(Constant(strategy.toString)),
        JavaSeqLiteral(args, TypeTree(defn.ObjectType))
      )
      cpy.Apply(tree)(
        Select(This(config.expressionClass), termName("reflectEval")),
        evalArgs
      ).withAttachment(ReflectEvalStrategy, strategy).cast(castTo)

    /** True when `tree`'s typed prefix routes to the symbol via a
     *  static module path (`TermRef(ThisType(ModuleClass), _)` or a
     *  chain of module-typed `TermRef`s). Codegen lowers such
     *  accesses to static paths reachable from any caller, so the
     *  Ident is fine as-is.
     *
     *  Private/protected members fail this check: the path may be
     *  static, but the JVM-level access check still rejects calls
     *  from `__Expression`. Those go through the inaccessible-member
     *  path, which uses `setAccessible(true)`.
     */
    private def isAccessibleViaStaticPrefix(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      if sym.exists && (sym.isPrivate || sym.is(Protected)) then false
      else tree.tpe match
        case tref: TermRef => isStaticPrefix(tref.prefix)
        case _ => false

    private def isStaticPrefix(prefix: Type)(using Context): Boolean = prefix match
      case tt: ThisType =>
        val cls = tt.cls
        cls.exists && cls.is(ModuleClass) && isGloballyAccessible(cls)
      case tref: TermRef =>
        val sym = tref.symbol
        sym.exists && sym.is(Module) && isStaticPrefix(tref.prefix)
      case _ => false

    /** A real class-member field, private or protected, declared
     *  outside `evaluate`'s body. Direct `getfield` would fail the
     *  JVM access check, so route through reflection.
     */
    private def isInaccessibleField(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isField && sym.enclosingClass.isClass &&
        (sym.isPrivate || sym.is(Protected)) && !isLocalToBody(sym)

    private def isInaccessibleMethod(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isRealMethod && sym.enclosingClass.isClass &&
        (sym.isPrivate || sym.is(Protected)) && !isLocalToBody(sym)

    /** Field or val-getter of a class declared inside a method body.
     *  The wrapper compile re-elaborates the class as a fresh JVM
     *  class, so a captured runtime instance has a different JVM
     *  class than the wrapper's symbol. Direct `getfield` /
     *  `invokevirtual` on the wrapper-symbol's name would target
     *  the wrong class, so we route through the reflective helpers.
     *  Excludes the inaccessible cases (private/protected) so they
     *  go through their existing branch — both end up emitting
     *  className=empty in `ResolveEvalAccess`, but routing them
     *  separately keeps the existing logic for accessor lookup
     *  (private val getters, etc.) intact.
     */
    private def isTermOwnedClassFieldAccess(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isField && sym.enclosingClass.isClass &&
        isTermOwnedSymbol(sym.enclosingClass) &&
        !isLocalToBody(sym) &&
        !sym.isPrivate && !sym.is(Protected)

    private def isTermOwnedClassMethodCall(tree: Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.isRealMethod && !sym.isClassConstructor &&
        sym.enclosingClass.isClass &&
        isTermOwnedSymbol(sym.enclosingClass) &&
        !isLocalToBody(sym) &&
        !sym.isPrivate && !sym.is(Protected)

    /** True when `cls` is owned by a term-level scope (a method or
     *  block) — i.e. a class declared inside a `def` or other term.
     *  REPL session classes are owned by a module class (not a term)
     *  so they pass through.
     */
    private def isTermOwnedSymbol(cls: Symbol)(using Context): Boolean =
      cls.exists && cls.maybeOwner.exists && cls.maybeOwner.isTerm

    /** True when `tpe`'s widened typeSymbol is a term-owned class. */
    private def isTermOwnedClass(tpe: Type)(using Context): Boolean =
      val sym = tpe.widen.typeSymbol
      sym.exists && sym.isClass && isTermOwnedSymbol(sym)

    /** Companion module of a method-local class. After `LambdaLift`
     *  hoists the class and module to top level, references resolve
     *  via the same static path the wrapper compile uses, so we don't
     *  need a binding-array indirection.
     */
    private def isTermOwnedModule(sym: Symbol)(using Context): Boolean =
      sym.exists && sym.is(Module) && isTermOwnedSymbol(sym)

    /** True when `sym` is a `def` declared inside another method's
     *  body, with that block sitting *outside* the captured eval
     *  body. The rewriter captures such defs as eta-expanded
     *  `FunctionN` bindings; references lower to MethodCapture so
     *  Resolve retrieves the function value and applies it.
     */
    private def isOuterMethodLocalDef(sym: Symbol)(using Context): Boolean =
      sym.exists && sym.is(Method) && !sym.isClassConstructor &&
        sym.owner.exists && sym.owner.is(Method) && !isLocalToBody(sym)

    private def getField(tree: Tree, qual: Tree, field: TermSymbol)(using Context): Tree =
      // Result type stays at the field's type (e.g. `Int`) so the
      // surrounding expression (`c.i + 1`) typechecks. Only widen to
      // Object when the field's *type* itself names a term-owned
      // class — that's where a checkcast to the wrapper symbol's JVM
      // name would land on the wrong class at runtime.
      val resultTpe =
        if isTermOwnedClass(field.info) then defn.ObjectType else field.info.widen
      buildReflectEvalCast(tree, qual,
        ReflectEvalStrategy.Field(field, isByName = false, useReceiverClass(field)),
        Nil, resultTpe)

    private def setField(tree: Tree, qual: Tree, field: TermSymbol, rhs: Tree)(using Context): Tree =
      buildReflectEvalCast(tree, qual,
        ReflectEvalStrategy.FieldAssign(field, useReceiverClass(field)),
        rhs :: Nil, defn.UnitType)

    /** True for members of a term-owned class. The runtime helper must
     *  start from `obj.getClass` rather than the wrapper-symbol's
     *  encoded class name, since LambdaLift will have flattened the
     *  owner by the time `ResolveEvalAccess` runs and we can no longer
     *  detect this from the symbol downstream — so the strategy
     *  carries a precomputed flag.
     */
    private def useReceiverClass(member: Symbol)(using Context): Boolean =
      val cls = member.enclosingClass
      cls.exists && cls.isClass && isTermOwnedSymbol(cls)

    private def callMethod(tree: Tree, qual: Tree, method: TermSymbol, args: List[Tree])(using Context): Tree =
      val rawResult = method.info.finalResultType.widen
      // Same reasoning as `getField`: the result-type cast only widens
      // when the method's return type *names* a term-owned class.
      val resultTpe =
        if isTermOwnedClass(rawResult) then defn.ObjectType else rawResult
      buildReflectEvalCast(tree, qual,
        ReflectEvalStrategy.MethodCall(method, useReceiverClass(method)),
        args, resultTpe)

    private def captureLocalMethod(tree: Tree, method: TermSymbol, args: List[Tree])(using Context): Tree =
      buildReflectEvalCast(tree, nullLiteral,
        ReflectEvalStrategy.MethodCapture(method, method, isByName = false),
        args, tree.tpe.widen)

    /** Walk a fully-applied call, accumulating value args in source
     *  order. `f(x)(y)` → `[x, y]`.
     */
    private def transformedMethodArgs(tree: Tree)(using Context): List[Tree] = tree match
      case _: (Ident | Select) => Nil
      case Apply(fun, args) => transformedMethodArgs(fun) ++ args.map(transform)
      case TypeApply(fun, _) => transformedMethodArgs(fun)
      case _ => Nil

    /** Compute the qualifier tree for a member access. Selects use
     *  their `qual`; Idents synthesise the implicit-`this` prefix
     *  through [[thisOrOuterValue]]; Apply/TypeApply walk down to
     *  the underlying Select/Ident.
     */
    private def transformedQualifier(tree: Tree)(using Context): Tree = tree match
      case Select(qual, _) => transform(qual)
      case Apply(fun, _) => transformedQualifier(fun)
      case TypeApply(fun, _) => transformedQualifier(fun)
      case Assign(lhs, _) => transformedQualifier(lhs)
      case Ident(_) =>
        val cls = tree.symbol.enclosingClass
        if cls.isClass && store.classOwners.contains(cls) then
          thisOrOuterValue(tree, cls.asClass)
        else
          report.error(
            s"eval: cannot synthesise qualifier for `${tree.symbol.name}` — " +
              s"its enclosing class `${cls.name}` is not in the captured owner chain.",
            tree.srcPos
          )
          nullLiteral
      case _ => nullLiteral

    /** Build a chain of `getThisObject` + zero or more `getOuter`
     *  steps that yields a `cls` instance at runtime.
     *
     *  Example: with `cls = Outer` and
     *  `classOwners = [Inner, Middle, Outer, ...]`:
     *  {{{
     *  getThisObject()
     *    .getOuter(_, Middle)   // Inner -> Middle
     *    .getOuter(_, Outer)    // Middle -> Outer
     *  }}}
     */
    private def thisOrOuterValue(tree: Tree, cls: ClassSymbol)(using Context): Tree =
      // Globally-accessible module: the singleton is reachable via
      // `<module>$.MODULE$`, so we shouldn't thread it through
      // `__this__` — the rewriter doesn't capture `this` for object
      // methods, which would null-deref inside `Method.invoke`.
      if cls.is(ModuleClass) && isGloballyAccessible(cls) then
        return ref(cls.sourceModule).withSpan(tree.span)
      val owners = store.classOwners
      val target = owners.indexOf(cls)
      if target < 0 then
        report.error(s"internal error: class `${cls.name}` not in classOwners", tree.srcPos)
        return getThisObject(tree, cls)
      val ths = getThisObject(tree, owners.head.asClass)
      owners.iterator.drop(1).take(target).foldLeft(ths) { (inner, outerSym) =>
        getOuter(tree, inner, outerSym)
      }

    private def reflectEvalPlaceholder(
        tree: Tree,
        qualifier: Tree,
        strategy: ReflectEvalStrategy,
        args: List[Tree]
    )(using Context): Tree =
      buildReflectEvalCast(tree, qualifier, strategy, args, tree.tpe.widen)

    private def isLocalVariable(sym: Symbol)(using Context): Boolean =
      sym.exists && !sym.is(Method) && sym.isLocalToBlock

    private def isByName(tpe: Type)(using Context): Boolean = tpe match
      case _: ExprType => true
      case ref: TermRef => isByName(ref.symbol.info)
      case _ => false

    private def isLocalToBody(sym: Symbol)(using Context): Boolean =
      val valSym = store.symbol
      if !sym.exists || valSym == null then false
      else sym.ownersIterator.exists(o =>
        o == valSym || o == config.evaluateMethod || o == config.expressionClass
      )

    /** True when `sym` is reachable from arbitrary code by spelling
     *  out a path — i.e. without going through a captured `this` or
     *  caller stack frame. Each link in the owner chain must itself
     *  be globally accessible. Private/protected members of a
     *  globally-named owner fail this check: the path resolves but
     *  the JVM-level access check rejects it from `__Expression`,
     *  so they go through reflective `setAccessible(true)` access.
     */
    private def isGloballyAccessible(sym: Symbol)(using Context): Boolean =
      if !sym.exists then false
      else if sym.isPrivate || sym.is(Protected) then false
      else if sym.is(Package) || sym.is(PackageClass) then true
      else
        val owner = sym.owner
        if !owner.exists then false
        else if owner.is(PackageClass) then true
        else if owner.is(ModuleClass) then isGloballyAccessible(owner)
        else false

    /** True when `sym` is one of the wrapper-injected helper stubs
     *  (`__refl_get__/_set__/_call__`). Distinguished from the
     *  base-class copies by owner: the wrapper's stubs live on a
     *  module class, the base copies live on `EvalExpressionBase`.
     */
    private def isWrapperReflHelper(sym: Symbol)(using Context): Boolean =
      sym.exists && sym.is(Method) && reflHelperNames.contains(sym.name) &&
        sym.owner != evalExpressionBaseClass

    /** Rebuild a wrapper-helper reference as `this.<helper>` against
     *  `EvalExpressionBase`'s inherited copy. The helper signatures
     *  match the wrapper's stubs (same parameter and return shape),
     *  so any enclosing `Apply`/`TypeApply` keeps its declared type.
     */
    private def retargetReflHelper(tree: Tree)(using Context): Tree =
      val baseSym = evalExpressionBaseClass.info.decl(tree.symbol.name).symbol.asTerm
      This(config.expressionClass).select(baseSym).withSpan(tree.span)

  end ExtractTransformer

  private val reflHelperNames: Set[Name] =
    Set(termName("__refl_get__"), termName("__refl_set__"), termName("__refl_call__"))

  private def evalExpressionBaseClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.repl.eval.EvalExpressionBase")

private[eval] object ExtractEvalBody:
  val name: String = "extractEvalBody"
  /** Substring shared by every wrapper module synthesised in
   *  [[EvalAdapter]]. Used by `ExtractEvalBody` to identify the
   *  wrapper symbol via `name.contains(...)` when pruning the
   *  package after the body has moved to `__Expression.evaluate`.
   *  Both wrapper-name shapes — `__EvalWrapper_<uuid>` (direct
   *  bridge use) and `rs$line$<uuid>$__EvalWrapper` (REPL use) —
   *  carry this substring.
   */
  val WrapperMarker: String = "__EvalWrapper"
