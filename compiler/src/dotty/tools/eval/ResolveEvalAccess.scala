package dotty.tools
package eval

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.{ClazzTag, Constant}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Phases.elimErasedValueTypePhase
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.TypeErasure.ErasedValueType
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.ValueClasses

/** Lowers each `reflectEval(...)` placeholder in `__Expression.evaluate`
 *  to a concrete reflective accessor call. The placeholder carries a
 *  [[ReflectEvalStrategy]] sticky-key attachment placed by
 *  [[ExtractEvalBody]] that picks the accessor (`getValue`,
 *  `getThisObject`, `getField`/`setField`, `callMethod`, ...).
 *  Runs after erasure so cast types match the JVM-level shapes
 *  the reflective helpers operate on.
 */
private[eval] class ResolveEvalAccess(config: EvalCompilerConfig, store: EvalStore)
  extends MiniPhase with InfoTransformer:

  override def phaseName: String = ResolveEvalAccess.name

  private val reflectEvalName: TermName = termName("reflectEval")

  /** Map linked-class occurrences in the infos of `__Expression`-owned
   *  symbols to `Object`. [[ExtractEvalBody]] does the same for
   *  body-local symbols that existed at its phase (PatternMatcher
   *  runs before extract, so its binder vals are covered there);
   *  this transformer covers symbols minted by the phases that run
   *  *after* extract, e.g. LetOverApply's receiver temps, Memoize's
   *  backing fields, and erasure's own temps, whose info names the
   *  re-elaborated class while the swept rhs is `Object`-typed (the
   *  backend asserts on that mismatch when emitting the local store).
   */
  def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
    if !infoMayChange(sym) then tp
    else if !sym.ownersIterator.exists(o =>
      o == config.expressionClass || store.bodyLocalClasses.contains(o)) then tp
    else if !store.mentionsLinkedRef(tp) then tp
    else store.eraseLinkedRefs(tp)

  override protected def infoMayChange(sym: Symbol)(using Context): Boolean =
    store.hasLinked && !sym.isClass && sym.maybeOwner.exists

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    // Body-local classes carry placeholders of their own (a method of
    // a body-declared class can reference outer captures); by this
    // phase LambdaLift/Flatten have moved them out of `__Expression`,
    // so they are matched by the symbols recorded at extract time.
    if tree.symbol == config.expressionClass
      || store.bodyLocalClasses.contains(tree.symbol)
    then ExpressionTransformer.transform(tree)
    else tree

  private object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case reflectEval: Apply if isReflectEval(reflectEval.fun.symbol) =>
          // Recurse into the qualifier and args first: a strategy
          // chained on top of another reflectEval (e.g. a `MethodCall`
          // whose qualifier is a `This` read, or a `LocalValueAssign`
          // whose rhs reads another capture) would otherwise carry an
          // un-lowered placeholder through to bytecode and ??? at
          // runtime.
          val qualifier = transform(reflectEval.args(0))
          val args = reflectEval.args(2).asInstanceOf[JavaSeqLiteral].elems.map(transform)
          val gen = new Gen(reflectEval.fun.asInstanceOf[Select].qualifier)
          reflectEval.attachment(ReflectEvalStrategy) match
            case ReflectEvalStrategy.LocalValue(variable) =>
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

            case ReflectEvalStrategy.Field(field, useReceiverClass) =>
              // Prefer the synthesised getter: its name is stable,
              // unlike the backing field's JVM name (which Scala 3
              // sometimes mangles with `$` suffixes that defeat
              // `getDeclaredField` lookups). The getter path re-boxes
              // a value-class result inside `callMethod`; the raw
              // field read needs the same adaptation by hand.
              val getter = field.getter
              if getter.exists then gen.callMethod(qualifier, getter.asTerm, Nil, useReceiverClass)
              else gen.boxIfValueClass(field, gen.getField(qualifier, field, useReceiverClass))

            case ReflectEvalStrategy.FieldAssign(field, useReceiverClass) =>
              // Same split as `Field`: the setter path unboxes a
              // value-class argument inside `callMethod`; the raw
              // field write does it by hand.
              val setter = field.setter
              if setter.exists then gen.callMethod(qualifier, setter.asTerm, args, useReceiverClass)
              else gen.setField(qualifier, field, gen.unboxIfValueClass(field, args.head), useReceiverClass)

            case ReflectEvalStrategy.MethodCall(method, useReceiverClass) =>
              gen.callMethod(qualifier, method, args, useReceiverClass)

            case ReflectEvalStrategy.MethodCapture(method) =>
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
        // Post-erasure sweep for linked local classes. Two producers
        // feed it: casts and adaptations that erasure (and the other
        // post-extract phases) inserted against the re-elaborated
        // class, and PatternMatcher output that extract has no case
        // for (PatternMatcher runs *before* extract, but extract only
        // rewrites member access and constructor calls; the lowered
        // matches' `isInstanceOf` / `asInstanceOf` trees pass through
        // it untouched). Rewrite anything that would emit a bytecode
        // reference to the re-minted class, so the runtime only ever
        // touches the original.
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
        // receiver-class reflection. Post-erasure, so `callMethod`
        // boxes primitive arguments for the `Object[]` and
        // `adaptResult` adapts the `Any` result back; value-class
        // adaptation is off, since the swept trees already carry the
        // erased underlying values on both sides of the call.
        case tree @ Apply(sel @ Select(qual, _), args)
            if isLinkedMember(sel.symbol) && !sel.symbol.isClassConstructor =>
          val gen = new Gen(This(config.expressionClass))
          val call = gen.callMethod(
            transform(qual), sel.symbol.asTerm, args.map(transform),
            useReceiverClass = true, adaptValueClasses = false)
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

  private def evalExpressionBaseClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.eval.EvalExpressionBase")

  private def isReflectEval(sym: Symbol)(using Context): Boolean =
    // `reflectEval` lives on `EvalExpressionBase` (the synthesised
    // subclass inherits it), so the owner check against
    // `config.expressionClass` won't match. Match by name + owner
    // against the resolved base-class symbol instead.
    sym.exists && sym.name == reflectEvalName &&
      sym.owner == evalExpressionBaseClass

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
     *  reflective). No value-class handling is needed here: the
     *  sweep calls `callMethod` with `adaptValueClasses = false`, so
     *  a value-class position arrives as the erased underlying
     *  value, which the primitive and checkcast arms already adapt.
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

    // --------------------------------------------------------------
    // Value-class adaptation at the reflective boundary. JVM
    // reflection works on the fully erased signature: a value-class
    // parameter takes the underlying value and a value-class result
    // comes back as the underlying value. The surrounding placeholder
    // code, however, was adapted by erasure against the *boxed*
    // class: the placeholder's arguments are value-class instances,
    // and its consumers checkcast the result and call the underlying
    // accessor on it. The helpers below convert between the two
    // representations, driven by the method's signature as it stood
    // before `elimErasedValueType` (where value-class positions are
    // still `ErasedValueType`s). Ported from
    // `debug.ResolveReflectEval`.
    // --------------------------------------------------------------

    /** The `ErasedValueType` of `tpe`'s (possibly curried) result,
     *  if any. Meaningful only on a pre-`elimErasedValueType` view
     *  of the type (see `atPhase` at the call sites).
     */
    private def erasedValueTypeOf(tpe: Type): Option[ErasedValueType] = tpe match
      case tpe: ErasedValueType => Some(tpe)
      case tpe: MethodOrPoly => erasedValueTypeOf(tpe.resultType)
      case _ => None

    /** Re-box a reflective result into a value-class instance when
     *  `member`'s erased signature says its result is one.
     */
    def boxIfValueClass(member: TermSymbol, tree: Tree): Tree =
      erasedValueTypeOf(atPhase(elimErasedValueTypePhase)(member.info)) match
        case Some(evt) => boxValueClass(evt.tycon.typeSymbol.asClass, tree)
        case None => tree

    /** `new VC(<underlying>)`. Constructed directly rather than
     *  reflectively (the debug original goes through a
     *  `callConstructor` runtime helper our base class does not
     *  carry); value classes are never method-local, so the class
     *  is statically reachable from `__Expression`.
     */
    private def boxValueClass(valueClass: ClassSymbol, tree: Tree): Tree =
      val underlying = valueClass.primaryConstructor.info.firstParamTypes.head
      New(valueClass.typeRef, List(tree.ensureConforms(underlying)))

    /** Unbox a value-class argument to its underlying value when
     *  `member`'s erased signature expects one.
     */
    def unboxIfValueClass(member: TermSymbol, tree: Tree): Tree =
      erasedValueTypeOf(atPhase(elimErasedValueTypePhase)(member.info)) match
        case Some(evt) => unboxValueClass(tree, evt)
        case None => tree

    /** Call the value class's underlying accessor on a boxed
     *  instance. Goes through the reflective `callMethod`, so a
     *  private value class works too (mirroring the original).
     */
    private def unboxValueClass(tree: Tree, evt: ErasedValueType): Tree =
      callMethod(tree, ValueClasses.valueClassUnbox(evt.tycon.typeSymbol.asClass).asTerm, Nil)

    /** Encodes the method's signature (param types + return type) so
     *  the reflective lookup picks the right overload. Types that
     *  name a *linked* local class are encoded as the `"*"` wildcard:
     *  the wrapper-side symbol's JVM name would never match the
     *  original class's runtime name, and the runtime matcher
     *  ([[EvalExpressionBase.callMethod]]) accepts any class at a
     *  wildcard position.
     *
     *  Primitive arguments are boxed for the `Object[]` literal.
     *  With `adaptValueClasses` set (the placeholder-lowering paths),
     *  value-class arguments are additionally unboxed to the
     *  underlying value and a value-class result is re-boxed into an
     *  instance (see the boundary comment above). The post-erasure
     *  sweep passes `adaptValueClasses = false`: its trees already
     *  carry the erased underlying values on both sides.
     */
    def callMethod(
        qualifier: Tree,
        method: TermSymbol,
        args: List[Tree],
        useReceiverClass: Boolean = false,
        adaptValueClasses: Boolean = true
    ): Tree =
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
      val erasedInfo =
        if adaptValueClasses then atPhase(elimErasedValueTypePhase)(method.info)
        else method.info
      val erasedParams = valueParamInfos(erasedInfo)
      val adaptedArgs =
        if adaptValueClasses && erasedParams.length == args.length then
          erasedParams.zip(args).map {
            case (evt: ErasedValueType, arg) => unboxValueClass(arg, evt)
            case (_, arg) => boxed(arg)
          }
        else args.map(boxed)
      val call = callOnThis("callMethod", List(
        qualifier,
        Literal(Constant(if useReceiverClass then "" else JavaEncoding.encode(method.enclosingClass.asType))),
        Literal(Constant(JavaEncoding.encode(method.name.asTermName))),
        paramTypesArray,
        Literal(Constant(encode(resultType(method.info)))),
        JavaSeqLiteral(adaptedArgs, TypeTree(defn.ObjectType))
      ))
      if adaptValueClasses then
        erasedValueTypeOf(erasedInfo) match
          case Some(evt) => boxValueClass(evt.tycon.typeSymbol.asClass, call)
          case None => call
      else call

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
