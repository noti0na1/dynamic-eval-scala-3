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

/** Lowers `reflectEval` placeholders inserted by [[ExtractEvalBody]] to their
 *  runtime accessors. This phase runs after erasure so argument and result
 *  adaptation uses JVM-level types.
 */
private[eval] class ResolveEvalAccess(config: EvalCompilerConfig, store: EvalStore)
  extends MiniPhase with InfoTransformer:

  override def phaseName: String = ResolveEvalAccess.name

  private val reflectEvalName: TermName = termName("reflectEval")

  /** Erases linked-class references from symbols created after extraction.
   *  Their values belong to the original runtime class, not the wrapper's copy.
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
    // LambdaLift and Flatten move body-local classes out of the expression, so
    // use the symbols recorded during extraction rather than tree position.
    if tree.symbol == config.expressionClass
      || store.bodyLocalClasses.contains(tree.symbol)
    then ExpressionTransformer.transform(tree)
    else tree

  private object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case reflectEval: Apply if isReflectEval(reflectEval.fun.symbol) =>
          // Lower nested placeholders in qualifiers and arguments first.
          val qualifier = transform(reflectEval.args(0))
          val args = reflectEval.args(2).asInstanceOf[JavaSeqLiteral].elems.map(transform)
          val gen = new Gen(reflectEval.fun.asInstanceOf[Select].qualifier)
          reflectEval.attachment(ReflectEvalStrategy) match
            case ReflectEvalStrategy.LocalValue(variable) =>
              // getValue dereferences VarRef only when the binding is mutable.
              gen.getValue(localBindingName(variable))

            case ReflectEvalStrategy.LocalValueAssign(variable) =>
              // Writes need the raw VarRef rather than its current value.
              val ref = gen.getRaw(localBindingName(variable))
              gen.varRefSet(ref, args.head)

            case ReflectEvalStrategy.This(_) =>
              gen.getThisObject

            case ReflectEvalStrategy.Field(field, useReceiverClass) =>
              // Prefer the getter because backing-field names may be mangled.
              // Raw reads need the same value-class adaptation as method calls.
              val getter = field.getter
              if getter.exists then gen.callMethod(qualifier, getter.asTerm, Nil, useReceiverClass)
              else gen.boxIfValueClass(field, gen.getField(qualifier, field, useReceiverClass))

            case ReflectEvalStrategy.FieldAssign(field, useReceiverClass) =>
              // Setter calls adapt value classes; raw writes do so explicitly.
              val setter = field.setter
              if setter.exists then gen.callMethod(qualifier, setter.asTerm, args, useReceiverClass)
              else gen.setField(qualifier, field, gen.unboxIfValueClass(field, args.head), useReceiverClass)

            case ReflectEvalStrategy.MethodCall(method, useReceiverClass) =>
              gen.callMethod(qualifier, method, args, useReceiverClass)

            case ReflectEvalStrategy.MethodCapture(method) =>
              // Block-local defs are captured as FunctionN values. Remove the
              // suffix introduced by LambdaLift when selecting the binding.
              gen.applyCapturedFunction(method.originalName.toString, args)

            case ReflectEvalStrategy.ConstructLocal(bindingName) =>
              // Use the captured factory to construct the original local class.
              gen.applyCapturedFunction(bindingName, args)

            case ReflectEvalStrategy.NewLinkedArray(sourceName, dims) =>
              // Allocate against the original linked component class.
              gen.newLinkedArray(sourceName, dims, args.head)

            case ReflectEvalStrategy.BindingValue(name) =>
              // Synthetic bindings store linked modules and return keys directly.
              gen.getRaw(name)

        // Sweep references to linked local classes that survive extraction or
        // are introduced later. No emitted bytecode may name the wrapper's copy.

        // `x.isInstanceOf[C']` → `isLinkedInstance(x, "C")`
        case tree @ TypeApply(Select(qual, _), targ :: Nil)
            if tree.symbol == defn.Any_isInstanceOf
              && store.linkedClassName(targ.tpe).isDefined =>
          val gen = new Gen(This(config.expressionClass))
          gen.isLinkedInstance(transform(qual), store.linkedClassName(targ.tpe).get)

        // Keep the result as Object; downstream linked members are reflective.
        case tree @ TypeApply(Select(qual, _), targ :: Nil)
            if tree.symbol == defn.Any_asInstanceOf
              && store.linkedClassName(targ.tpe).isDefined =>
          val gen = new Gen(This(config.expressionClass))
          gen.castLinked(transform(qual), store.linkedClassName(targ.tpe).get)

        // `x.isInstanceOf[Array[…[C']…]]` (any dimension count) →
        // `isLinkedArrayInstance(x, "C", dims)`
        case tree @ TypeApply(Select(qual, _), targ :: Nil)
            if tree.symbol == defn.Any_isInstanceOf
              && store.linkedArrayElemInfo(targ.tpe).isDefined =>
          val (src, dims) = store.linkedArrayElemInfo(targ.tpe).get
          val gen = new Gen(This(config.expressionClass))
          gen.isLinkedArrayInstance(transform(qual), src, dims)

        // Preserve array depth with Object components so direct loads and stores
        // remain valid without naming the wrapper class.
        case tree @ TypeApply(Select(qual, _), targ :: Nil)
            if tree.symbol == defn.Any_asInstanceOf
              && store.linkedArrayElemInfo(targ.tpe).isDefined =>
          val (src, dims) = store.linkedArrayElemInfo(targ.tpe).get
          val gen = new Gen(This(config.expressionClass))
          gen.castLinkedArray(transform(qual), src, dims).cast(objectArrayOfDepth(dims))

        // Lower the complete newArray intrinsic before rewriting its class
        // literal, whose constant shape is required by the backend.
        case tree @ Apply(_, List(Literal(ec), Literal(_), dimsLit: JavaSeqLiteral))
            if tree.symbol == defn.newArrayMethod && ec.tag == ClazzTag
              && linkedUltimateElemInfo(ec.typeValue).isDefined =>
          val (src, elemDims) = linkedUltimateElemInfo(ec.typeValue).get
          val gen = new Gen(This(config.expressionClass))
          gen.newLinkedArrayDims(src, elemDims,
              JavaSeqLiteral(dimsLit.elems.map(transform), TypeTree(defn.IntType)))
            .cast(objectArrayOfDepth(elemDims + dimsLit.elems.length))

        // Rebuild linked array literals with the original runtime component.
        // Outer literals receive the already rebuilt inner arrays.
        case lit: JavaSeqLiteral if linkedSeqLiteralInfo(lit).isDefined =>
          val (src, dims) = linkedSeqLiteralInfo(lit).get
          val gen = new Gen(This(config.expressionClass))
          val objElems = JavaSeqLiteral(lit.elems.map(transform), TypeTree(defn.ObjectType))
          gen.arrayOfLinked(src, dims, objElems).cast(objectArrayOfDepth(dims))

        // A linked module value is the original instance, so discard casts to
        // the wrapper's module class.
        case tree @ TypeApply(Select(qual, _), targ :: Nil)
            if tree.symbol == defn.Any_asInstanceOf
              && store.linkedModules.contains(targ.tpe.typeSymbol) =>
          transform(qual)

        // Reflect linked member calls against the receiver's runtime class. At
        // this phase value-class positions already contain erased values.
        case tree @ Apply(sel @ Select(qual, _), args)
            if isLinkedMember(sel.symbol) && !sel.symbol.isClassConstructor =>
          val gen = new Gen(This(config.expressionClass))
          val call = gen.callMethod(
            transform(qual), sel.symbol.asTerm, args.map(transform),
            useReceiverClass = true, adaptValueClasses = false)
          gen.adaptResult(call, tree.tpe)

        // Handle a linked field read that survives as a bare Select.
        case tree @ Select(qual, _)
            if isLinkedMember(tree.symbol) && !tree.symbol.isClassConstructor
              && !tree.symbol.is(Method) =>
          val gen = new Gen(This(config.expressionClass))
          gen.adaptResult(gen.getField(transform(qual), tree.symbol.asTerm, useReceiverClass = true), tree.tpe)

        // Read linked module values introduced after extraction.
        case tree: Ident if store.linkedModules.contains(tree.symbol) && !tree.symbol.isClass =>
          val gen = new Gen(This(config.expressionClass))
          gen.getRaw(EvalNames.moduleBinding(store.linkedModules(tree.symbol)))

        // Replace class literals for wrapper copies. This also keeps nested eval
        // links pointing to the original runtime class.
        case tree @ Literal(c)
            if c.tag == ClazzTag && store.linkedClassName(c.typeValue).isDefined =>
          val gen = new Gen(This(config.expressionClass))
          gen.linkedClassOf(store.linkedClassName(c.typeValue).get)

        // Replace array class literals built from wrapper copies.
        case tree @ Literal(c)
            if c.tag == ClazzTag && store.linkedArrayElemInfo(c.typeValue).isDefined =>
          val (src, dims) = store.linkedArrayElemInfo(c.typeValue).get
          val gen = new Gen(This(config.expressionClass))
          gen.linkedArrayClassOf(src, dims)

        case _ => super.transform(tree)

    /** Whether a member belongs to a linked local class or module class. */
    private def isLinkedMember(sym: Symbol)(using Context): Boolean =
      sym.exists && {
        val owner = sym.maybeOwner
        store.linkedClasses.contains(owner) ||
          (owner.isClass && store.linkedModules.contains(owner))
      }

    /** Linked element and depth for an array literal. */
    private def linkedSeqLiteralInfo(lit: JavaSeqLiteral)(using Context): Option[(String, Int)] =
      store.linkedClassName(lit.elemtpt.tpe).map((_, 1))
        .orElse(store.linkedArrayElemInfo(lit.elemtpt.tpe).map((src, d) => (src, d + 1)))

    /** Linked element and depth, with depth zero for the class itself. */
    private def linkedUltimateElemInfo(tpe: Type)(using Context): Option[(String, Int)] =
      store.linkedClassName(tpe).map((_, 0))
        .orElse(store.linkedArrayElemInfo(tpe))

    /** Returns the runtime binding name, accounting for inline expansion and
     *  LambdaLift suffixes.
     */
    private def localBindingName(variable: TermSymbol)(using Context): String =
      store.inlinedBindingNames.getOrElse(variable, variable.originalName.toString)
  end ExpressionTransformer

  /** Post-erasure Object-array type with `dims` layers. */
  private def objectArrayOfDepth(dims: Int)(using Context): Type =
    (1 to dims).foldLeft(defn.ObjectType: Type)((t, _) => JavaArrayType(t))

  private def evalExpressionBaseClass(using Context): ClassSymbol =
    requiredClass("dotty.tools.eval.EvalExpressionBase")

  private def isReflectEval(sym: Symbol)(using Context): Boolean =
    // The generated expression inherits reflectEval from its precompiled base.
    sym.exists && sym.name == reflectEvalName &&
      sym.owner == evalExpressionBaseClass

  /** Tree builders for runtime accessor calls on the generated expression. */
  private class Gen(expressionThis: Tree)(using Context):

    private def callOnThis(name: String, args: List[Tree]): Tree =
      Apply(Select(expressionThis, termName(name)), args)

    def isLinkedInstance(qualifier: Tree, sourceName: String): Tree =
      callOnThis("isLinkedInstance",
        boxed(qualifier) :: Literal(Constant(sourceName)) :: Nil)

    def castLinked(qualifier: Tree, sourceName: String): Tree =
      callOnThis("castLinked",
        boxed(qualifier) :: Literal(Constant(sourceName)) :: Nil)

    def linkedClassOf(sourceName: String): Tree =
      callOnThis("linkedClass", Literal(Constant(sourceName)) :: Nil)

    def isLinkedArrayInstance(qualifier: Tree, sourceName: String, dims: Int): Tree =
      callOnThis("isLinkedArrayInstance",
        qualifier :: Literal(Constant(sourceName)) :: Literal(Constant(dims)) :: Nil)

    def castLinkedArray(qualifier: Tree, sourceName: String, dims: Int): Tree =
      callOnThis("castLinkedArray",
        qualifier :: Literal(Constant(sourceName)) :: Literal(Constant(dims)) :: Nil)

    def linkedArrayClassOf(sourceName: String, dims: Int): Tree =
      callOnThis("linkedArrayClass",
        Literal(Constant(sourceName)) :: Literal(Constant(dims)) :: Nil)

    def newLinkedArrayDims(sourceName: String, elemDims: Int, dims: Tree): Tree =
      callOnThis("newLinkedArrayDims",
        Literal(Constant(sourceName)) :: Literal(Constant(elemDims)) :: dims :: Nil)

    /** Builds a reflective linked-array allocation, unboxing its length. */
    def newLinkedArray(sourceName: String, dims: Int, length: Tree): Tree =
      val len =
        if length.tpe.widen.isPrimitiveValueType then length
        else dotc.transform.Erasure.Boxing.unbox(length, defn.IntType)
      callOnThis("newLinkedArray",
        Literal(Constant(sourceName)) :: Literal(Constant(dims)) :: len :: Nil)

    /** Copies an Object array into an array with the linked runtime component. */
    def arrayOfLinked(sourceName: String, dims: Int, elems: Tree): Tree =
      callOnThis("arrayOfLinked",
        Literal(Constant(sourceName)) :: Literal(Constant(dims)) :: elems :: Nil)

    /** Boxes a primitive after erasure has already run. */
    def boxed(tree: Tree): Tree =
      if tree.tpe.widen.isPrimitiveValueType
      then dotc.transform.Erasure.Boxing.box(tree)
      else tree

    /** Adapts a reflective result to the replaced tree's erased type. Linked
     *  classes remain `Object` because their subsequent accesses are reflective.
     */
    def adaptResult(call: Tree, expected: Type): Tree =
      val exp = expected.widen
      if exp.isRef(defn.UnitClass) then Block(call :: Nil, unitLiteral)
      else if exp.isPrimitiveValueType then dotc.transform.Erasure.Boxing.unbox(call, exp)
      else if store.isLinkedPart(exp) then call
      else if store.linkedArrayElemInfo(exp).isDefined then
        // Preserve depth without naming the wrapper's component class.
        call.cast(objectArrayOfDepth(store.linkedArrayElemInfo(exp).get._2))
      else call.cast(exp)

    def getValue(name: String): Tree =
      callOnThis("getValue", Literal(Constant(name)) :: Nil)

    /** Reads a binding without dereferencing a `VarRef`. */
    def getRaw(name: String): Tree =
      callOnThis("getRaw", Literal(Constant(name)) :: Nil)

    def getThisObject: Tree =
      callOnThis("getThisObject", Nil)

    def varRefSet(ref: Tree, rhs: Tree): Tree =
      val varRefCls = requiredClass("dotty.tools.eval.Eval.VarRef")
      Apply(Select(ref.cast(varRefCls.typeRef), termName("set")), rhs :: Nil)

    /** Generates a field read. `enclosingClass` is used because later phases may
     *  re-own fields to constructors. An empty encoded class selects the runtime
     *  receiver for linked local classes.
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

    // Reflection uses erased value-class signatures, while the placeholder was
    // adapted against boxed values. Inspect the pre-elimErasedValueType method
    // information to convert at this boundary. This follows ResolveReflectEval.

    /** Finds a value-class result in pre-`elimErasedValueType` information. */
    private def erasedValueTypeOf(tpe: Type): Option[ErasedValueType] = tpe match
      case tpe: ErasedValueType => Some(tpe)
      case tpe: MethodOrPoly => erasedValueTypeOf(tpe.resultType)
      case _ => None

    /** Re-boxes a reflective value-class result. */
    def boxIfValueClass(member: TermSymbol, tree: Tree): Tree =
      erasedValueTypeOf(atPhase(elimErasedValueTypePhase)(member.info)) match
        case Some(evt) => boxValueClass(evt.tycon.typeSymbol.asClass, tree)
        case None => tree

    /** Constructs a boxed value class directly; value classes are not local. */
    private def boxValueClass(valueClass: ClassSymbol, tree: Tree): Tree =
      val underlying = valueClass.primaryConstructor.info.firstParamTypes.head
      New(valueClass.typeRef, List(tree.ensureConforms(underlying)))

    /** Unboxes an argument when the erased signature expects a value class. */
    def unboxIfValueClass(member: TermSymbol, tree: Tree): Tree =
      erasedValueTypeOf(atPhase(elimErasedValueTypePhase)(member.info)) match
        case Some(evt) => unboxValueClass(tree, evt)
        case None => tree

    /** Calls the underlying accessor reflectively, including for private classes. */
    private def unboxValueClass(tree: Tree, evt: ErasedValueType): Tree =
      callMethod(tree, ValueClasses.valueClassUnbox(evt.tycon.typeSymbol.asClass).asTerm, Nil)

    /** Encodes a method signature for reflective overload selection. Linked
     *  local types use `"*"` because wrapper and runtime class names differ.
     *
     *  Primitive arguments are boxed for the argument array. Placeholder
     *  lowering also adapts value classes; the post-erasure sweep already has
     *  their underlying representation and disables that step.
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
        if store.isLinkedPart(t.widen) || store.linkedArrayElemInfo(t.widen).isDefined
        then "*"
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

    /** Invokes a captured local method through its FunctionN value. Arity comes
     *  from the call because LambdaLift adds parameters to the method symbol.
     */
    def applyCapturedFunction(name: String, args: List[Tree]): Tree =
      val fnTypeRef = defn.FunctionType(args.length)
      val cast = getValue(name).cast(fnTypeRef)
      Apply(Select(cast, nme.apply), args)
  end Gen

private[eval] object ResolveEvalAccess:
  val name: String = "resolveEvalAccess"
