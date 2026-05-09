package dotty.tools
package repl
package eval

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
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
  extends MiniPhase:

  override def phaseName: String = ResolveEvalAccess.name

  private val reflectEvalName: TermName = termName("reflectEval")

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
          val gen = new Gen(reflectEval)
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

        case _ => super.transform(tree)
  end ExpressionTransformer

  private def isReflectEval(sym: Symbol)(using Context): Boolean =
    // `reflectEval` lives on `EvalExpressionBase` (the synthesised
    // subclass inherits it), so the owner check against
    // `config.expressionClass` won't match. Match by name + owner-
    // is-EvalExpressionBase instead.
    sym.exists && sym.name == reflectEvalName &&
      sym.owner.exists && sym.owner.name.toString == "EvalExpressionBase"

  /** Tree builders for the lowered accessor calls. The `reflectEval`
   *  Apply we replace always has the shape
   *  `Select(This(__Expression), reflectEval).apply(...)`, so we reuse
   *  the typed `This(__Expression)` qualifier for every replacement.
   */
  private class Gen(reflectEval: Apply)(using Context):
    private val expressionThis: Tree = reflectEval.fun.asInstanceOf[Select].qualifier

    private def callOnThis(name: String, args: List[Tree]): Tree =
      Apply(Select(expressionThis, termName(name)), args)

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
      val varRefCls = requiredClass("dotty.tools.repl.eval.Eval.VarRef")
      Apply(Select(ref.cast(varRefCls.typeRef), termName("get")), Nil)

    def varRefSet(ref: Tree, rhs: Tree): Tree =
      val varRefCls = requiredClass("dotty.tools.repl.eval.Eval.VarRef")
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
     *  the reflective lookup picks the right overload.
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
      val paramTypeNames = valueParamInfos(method.info).map(JavaEncoding.encode)
      val paramTypesArray = JavaSeqLiteral(
        paramTypeNames.map(t => Literal(Constant(t))),
        TypeTree(defn.StringType)
      )
      callOnThis("callMethod", List(
        qualifier,
        Literal(Constant(if useReceiverClass then "" else JavaEncoding.encode(method.enclosingClass.asType))),
        Literal(Constant(JavaEncoding.encode(method.name.asTermName))),
        paramTypesArray,
        Literal(Constant(JavaEncoding.encode(resultType(method.info)))),
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
