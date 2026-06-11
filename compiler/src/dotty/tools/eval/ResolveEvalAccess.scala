package dotty.tools
package eval

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.{ClazzTag, Constant}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/** Lowers each `reflectEval(...)` placeholder in `__Expression.evaluate`
 *  to a concrete reflective accessor call. The placeholder carries a
 *  [[ReflectEvalStrategy]] sticky-key attachment placed by
 *  [[ExtractEvalBody]] that picks the accessor (`getValue`,
 *  `getThisObject`, `getOuter`, `getField`/`setField`, `callMethod`,
 *  ...). Runs after erasure so cast types match the JVM-level shapes
 *  the reflective helpers operate on.
 */
private[eval] class ResolveEvalAccess(config: EvalCompilerConfig, store: EvalStore)
  extends MiniPhase with InfoTransformer:

  override def phaseName: String = ResolveEvalAccess.name

  private val reflectEvalName: TermName = termName("reflectEval")

  /** Map linked-class occurrences in the infos of `__Expression`-owned
   *  symbols to `Object`. [[ExtractEvalBody]] does the same for
   *  body-local symbols that existed at its phase; this covers symbols
   *  created *between* the two phases, primarily PatternMatcher's
   *  binder vals, whose info names the re-elaborated class while the
   *  swept rhs is `Object`-typed (the backend asserts on that
   *  mismatch when emitting the local store).
   */
  def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
    if !infoMayChange(sym) then tp
    else if !sym.ownersIterator.contains(config.expressionClass) then tp
    else if !store.mentionsLinkedRef(tp) then tp
    else store.eraseLinkedRefs(tp)

  override protected def infoMayChange(sym: Symbol)(using Context): Boolean =
    store.hasLinked && !sym.isClass && sym.maybeOwner.exists

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    if tree.symbol == config.expressionClass then
      ExpressionTransformer.transform(tree)
    else tree

  private object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case tree: DefDef if tree.symbol == config.evaluateMethod =>
          cpy.DefDef(tree)(rhs = transform(tree.rhs))

        case reflectEval: Apply if isReflectEval(reflectEval.fun.symbol) =>
          // Recurse into the qualifier and args first: a strategy
          // chained on top of another reflectEval (e.g. `Outer` over
          // `This`, or `LocalValueAssign` whose rhs reads another
          // capture) would otherwise carry an un-lowered placeholder
          // through to bytecode and ??? at runtime.
          val qualifier = transform(reflectEval.args(0))
          val args = reflectEval.args(2).asInstanceOf[JavaSeqLiteral].elems.map(transform)
          val gen = new Gen(reflectEval.fun.asInstanceOf[Select].qualifier)
          reflectEval.attachment(ReflectEvalStrategy) match
            case ReflectEvalStrategy.LocalValue(variable, _) =>
              // `getValue` auto-unwraps `Eval.VarRef`, so this single
              // path covers both `var` captures (whose binding is a
              // `VarRef[T]`) and `val` captures whose `Mutable` flag
              // was cleared by an earlier phase.
              //
              // `originalName` peels the `$N` suffix LambdaLift
              // appends to lifted local symbols; the runtime bindings
              // array uses the source name.
              gen.getValue(variable.originalName.toString)

            case ReflectEvalStrategy.LocalValueAssign(variable) =>
              // Writes need the raw `VarRef` so we can call `.set(v)`;
              // `getValue` would auto-deref to `T`.
              val ref = gen.getRaw(variable.originalName.toString)
              gen.varRefSet(ref, args.head)

            case ReflectEvalStrategy.This(_) =>
              gen.getThisObject

            case ReflectEvalStrategy.Outer(outerCls) =>
              gen.getOuter(qualifier, outerCls)

            case ReflectEvalStrategy.Field(field, _, useReceiverClass) =>
              // Prefer the synthesised getter: its name is stable,
              // unlike the backing field's JVM name (which Scala 3
              // sometimes mangles with `$` suffixes that defeat
              // `getDeclaredField` lookups).
              val getter = field.getter
              if getter.exists then gen.callMethod(qualifier, getter.asTerm, Nil, useReceiverClass)
              else gen.getField(qualifier, field, useReceiverClass)

            case ReflectEvalStrategy.FieldAssign(field, useReceiverClass) =>
              val setter = field.setter
              if setter.exists then gen.callMethod(qualifier, setter.asTerm, args, useReceiverClass)
              else gen.setField(qualifier, field, args.head, useReceiverClass)

            case ReflectEvalStrategy.MethodCall(method, useReceiverClass) =>
              gen.callMethod(qualifier, method, args, useReceiverClass)

            case ReflectEvalStrategy.MethodCapture(_, method, _) =>
              // The rewriter captured this block-local def as an
              // eta-expanded `FunctionN` binding under the def's bare
              // name. `originalName` strips LambdaLift's `$N` suffix.
              gen.applyCapturedFunction(method.originalName.toString, args)

            case ReflectEvalStrategy.ConstructLocal(bindingName) =>
              // Apply the call-site constructor-factory closure so
              // the instance belongs to the *original* lifted class.
              gen.applyCapturedFunction(bindingName, args)

            case ReflectEvalStrategy.BindingValue(name) =>
              // Raw read of a synthetic binding (linked module
              // instance, non-local-return key).
              gen.getRaw(name)

        // ------------------------------------------------------------
        // Post-erasure sweep for linked local classes. PatternMatcher
        // and erasure generate code against the wrapper's
        // re-elaborated class *after* ExtractEvalBody ran (type tests
        // and casts in lowered matches, accessor calls on binders).
        // Rewrite anything that would emit a bytecode reference to
        // the re-minted class, so the runtime only ever touches the
        // original.
        // ------------------------------------------------------------

        // `x.isInstanceOf[C']` → `isLinkedInstance(x, "C")`
        case tree @ TypeApply(Select(qual, _), targ :: Nil)
            if tree.symbol == defn.Any_isInstanceOf
              && store.linkedClassName(targ.tpe).isDefined =>
          val gen = new Gen(This(config.expressionClass))
          gen.isLinkedInstance(transform(qual), store.linkedClassName(targ.tpe).get)

        // `x.asInstanceOf[C']` → `castLinked(x, "C")` (typed Object;
        // downstream member accesses are swept reflective, so no
        // emitted descriptor needs `C'`)
        case tree @ TypeApply(Select(qual, _), targ :: Nil)
            if tree.symbol == defn.Any_asInstanceOf
              && store.linkedClassName(targ.tpe).isDefined =>
          val gen = new Gen(This(config.expressionClass))
          gen.castLinked(transform(qual), store.linkedClassName(targ.tpe).get)

        // Cast to a linked *module* class (typically inserted by
        // erasure when adapting an `Object`-typed module read to a
        // position still typed with the re-elaborated module class).
        // The value is the *original* module instance, so the right
        // lowering is to drop the checkcast and keep `Object`.
        case tree @ TypeApply(Select(qual, _), targ :: Nil)
            if tree.symbol == defn.Any_asInstanceOf
              && store.linkedModules.contains(targ.tpe.typeSymbol) =>
          transform(qual)

        // Member call whose owner is a linked class/module
        // (`binder._1` from a lowered case-class pattern, a
        // user-defined `unapply` on a linked companion, …) →
        // receiver-class reflection. Post-erasure, so argument
        // boxing / result unboxing is done here rather than left to
        // the (already-run) erasure phase.
        case tree @ Apply(sel @ Select(qual, _), args)
            if isLinkedMember(sel.symbol) && !sel.symbol.isClassConstructor =>
          val gen = new Gen(This(config.expressionClass))
          val call = gen.callMethod(
            transform(qual), sel.symbol.asTerm,
            args.map(a => gen.boxed(transform(a))), useReceiverClass = true)
          gen.adaptResult(call, tree.tpe)

        // Bare select of a linked member (rare post-erasure; e.g. a
        // field read that survived as a Select).
        case tree @ Select(qual, _)
            if isLinkedMember(tree.symbol) && !tree.symbol.isClassConstructor
              && !tree.symbol.is(Method) =>
          val gen = new Gen(This(config.expressionClass))
          gen.adaptResult(gen.getField(transform(qual), tree.symbol.asTerm, useReceiverClass = true), tree.tpe)

        // Term reference to a linked module val generated after
        // extract (the body-visible ones were already lowered there).
        case tree: Ident if store.linkedModules.contains(tree.symbol) && !tree.symbol.isClass =>
          val gen = new Gen(This(config.expressionClass))
          gen.getRaw(EvalNames.moduleBinding(store.linkedModules(tree.symbol)))

        // `classOf[C']` constant of a linked class. User code can
        // write it directly, but the main producer is chained mode:
        // a *nested* eval call inside this body gets its own
        // `__evalClass_C__` bind value injected as `classOf[C']` of
        // the re-elaborated class, which would otherwise survive as
        // an `ldc` of a class that never reaches the runtime
        // classpath. Forward this call's own linked class object
        // instead, so the next eval level links to the same original
        // class.
        case tree @ Literal(c)
            if c.tag == ClazzTag && store.linkedClassName(c.typeValue).isDefined =>
          val gen = new Gen(This(config.expressionClass))
          gen.linkedClassOf(store.linkedClassName(c.typeValue).get)

        case _ => super.transform(tree)

    /** Member of a linked local class or linked local module's class.
     *  Owners survive to this phase unchanged (Flatten moves the
     *  classes, not their members).
     */
    private def isLinkedMember(sym: Symbol)(using Context): Boolean =
      sym.exists && {
        val owner = sym.maybeOwner
        store.linkedClasses.contains(owner) ||
          (owner.isClass && store.linkedModules.contains(owner))
      }
  end ExpressionTransformer

  private def isReflectEval(sym: Symbol)(using Context): Boolean =
    // `reflectEval` lives on `EvalExpressionBase` (the synthesised
    // subclass inherits it), so the owner check against
    // `config.expressionClass` won't match. Match by name + owner-
    // is-EvalExpressionBase instead.
    sym.exists && sym.name == reflectEvalName &&
      sym.owner.exists && sym.owner.name.toString == "EvalExpressionBase"

  /** Tree builders for the lowered accessor calls. For a
   *  `reflectEval` placeholder the `expressionThis` qualifier is the
   *  placeholder's own typed `This(__Expression)`; for the
   *  post-erasure linked-class sweep a fresh `This` is constructed.
   */
  private class Gen(expressionThis: Tree)(using Context):

    private def callOnThis(name: String, args: List[Tree]): Tree =
      Apply(Select(expressionThis, termName(name)), args)

    /** `isLinkedInstance(obj, "C")`: `Class.isInstance` against the
     *  `__evalClass_C__` binding. Typed Boolean, matching the type
     *  test it replaces.
     */
    def isLinkedInstance(qualifier: Tree, sourceName: String): Tree =
      callOnThis("isLinkedInstance",
        boxed(qualifier) :: Literal(Constant(sourceName)) :: Nil)

    /** `castLinked(obj, "C")`: checked cast against the original
     *  runtime class, typed Object.
     */
    def castLinked(qualifier: Tree, sourceName: String): Tree =
      callOnThis("castLinked",
        boxed(qualifier) :: Literal(Constant(sourceName)) :: Nil)

    /** `linkedClass("C")`: the original runtime `Class` of the
     *  linked local class `C`, replacing a `classOf[C']` constant of
     *  the re-elaborated class.
     */
    def linkedClassOf(sourceName: String): Tree =
      callOnThis("linkedClass", Literal(Constant(sourceName)) :: Nil)

    /** Box a primitive-typed tree. The sweep runs after erasure, so
     *  adaptation that the erasure phase would normally insert has
     *  to be done by hand.
     */
    def boxed(tree: Tree): Tree =
      if tree.tpe.widen.isPrimitiveValueType
      then dotc.transform.Erasure.Boxing.box(tree)
      else tree

    /** Adapt a reflective `Any`-returning call back to the type the
     *  replaced tree had: unbox primitives, re-issue Unit, checkcast
     *  reference types. Linked classes are the exception: they
     *  deliberately stay `Object` (their member accesses are swept
     *  reflective).
     */
    def adaptResult(call: Tree, expected: Type): Tree =
      val exp = expected.widen
      if exp.isRef(defn.UnitClass) then Block(call :: Nil, unitLiteral)
      else if exp.isPrimitiveValueType then dotc.transform.Erasure.Boxing.unbox(call, exp)
      else if store.isLinkedPart(exp) then call
      else call.cast(exp)

    def getValue(name: String): Tree =
      callOnThis("getValue", Literal(Constant(name)) :: Nil)

    /** Variant of `getValue` that returns the raw binding (no
     *  `VarRef` unwrap). Used by the assign codegen.
     */
    def getRaw(name: String): Tree =
      callOnThis("getRaw", Literal(Constant(name)) :: Nil)

    def getThisObject: Tree =
      callOnThis("getThisObject", Nil)

    def getOuter(qualifier: Tree, outerCls: ClassSymbol): Tree =
      callOnThis("getOuter", qualifier :: Literal(Constant(outerCls.javaClassName)) :: Nil)

    def varRefGet(ref: Tree): Tree =
      val varRefCls = requiredClass("dotty.tools.eval.Eval.VarRef")
      Apply(Select(ref.cast(varRefCls.typeRef), termName("get")), Nil)

    def varRefSet(ref: Tree, rhs: Tree): Tree =
      val varRefCls = requiredClass("dotty.tools.eval.Eval.VarRef")
      Apply(Select(ref.cast(varRefCls.typeRef), termName("set")), rhs :: Nil)

    /** `enclosingClass` rather than `owner`: Scala 3 may re-own class
     *  fields to the constructor by post-erasure phases.
     *
     *  When `useReceiverClass` is set (the strategy's flag, precomputed
     *  by ExtractEvalBody before LambdaLift flattens owners), encode
     *  an empty className — the runtime helper walks `obj.getClass`
     *  instead of loading the wrapper-symbol's encoded name. Used for
     *  members of term-owned classes whose JVM class differs from the
     *  captured runtime instance.
     */
    def getField(qualifier: Tree, field: TermSymbol, useReceiverClass: Boolean = false): Tree =
      callOnThis("getField", List(
        qualifier,
        Literal(Constant(if useReceiverClass then "" else JavaEncoding.encode(field.enclosingClass.asType))),
        Literal(Constant(JavaEncoding.encode(field.name.asTermName)))
      ))

    def setField(qualifier: Tree, field: TermSymbol, value: Tree, useReceiverClass: Boolean = false): Tree =
      callOnThis("setField", List(
        qualifier,
        Literal(Constant(if useReceiverClass then "" else JavaEncoding.encode(field.enclosingClass.asType))),
        Literal(Constant(JavaEncoding.encode(field.name.asTermName))),
        value
      ))

    /** Encodes the method's signature (param types + return type) so
     *  the reflective lookup picks the right overload. Types that
     *  name a *linked* local class are encoded as the `"*"` wildcard:
     *  the wrapper-side symbol's JVM name would never match the
     *  original class's runtime name, and the runtime matcher
     *  ([[EvalExpressionBase.callMethod]]) accepts any class at a
     *  wildcard position.
     */
    def callMethod(qualifier: Tree, method: TermSymbol, args: List[Tree], useReceiverClass: Boolean = false): Tree =
      def valueParamInfos(t: Type): List[Type] = t match
        case mt: MethodType => mt.paramInfos
        case pt: PolyType => valueParamInfos(pt.resultType)
        case _ => Nil
      def resultType(t: Type): Type = t match
        case mt: MethodType => resultType(mt.resultType)
        case pt: PolyType => resultType(pt.resultType)
        case t => t
      def encode(t: Type): String =
        if store.isLinkedPart(t.widen) then "*"
        else JavaEncoding.encode(t)
      val paramTypeNames = valueParamInfos(method.info).map(encode)
      val paramTypesArray = JavaSeqLiteral(
        paramTypeNames.map(t => Literal(Constant(t))),
        TypeTree(defn.StringType)
      )
      callOnThis("callMethod", List(
        qualifier,
        Literal(Constant(if useReceiverClass then "" else JavaEncoding.encode(method.enclosingClass.asType))),
        Literal(Constant(JavaEncoding.encode(method.name.asTermName))),
        paramTypesArray,
        Literal(Constant(encode(resultType(method.info)))),
        JavaSeqLiteral(args, TypeTree(defn.ObjectType))
      ))

    /** `getValue(name).asInstanceOf[FunctionN].apply(args*)` for a
     *  captured block-local def. We use `args.length` for the arity
     *  rather than `method.info`: LambdaLift has already added
     *  captured outer locals as extra params on `method`, inflating
     *  the signature.
     */
    def applyCapturedFunction(name: String, args: List[Tree]): Tree =
      val fnTypeRef = defn.FunctionType(args.length)
      val cast = getValue(name).cast(fnTypeRef)
      Apply(Select(cast, nme.apply), args)
  end Gen

private[eval] object ResolveEvalAccess:
  val name: String = "resolveEvalAccess"
