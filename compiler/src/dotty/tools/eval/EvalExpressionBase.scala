package dotty.tools
package eval

// Required for the `@caps.assumeSafe` annotation below.
import scala.language.experimental.captureChecking

/** Precompiled base for generated eval expression classes.
 *
 *  [[ExtractEvalBody]] fills `evaluate` and replaces outer references with calls
 *  to the helpers defined here.
 *
 *  Reflection is rejected by safe mode, so these helpers cannot be emitted into
 *  each generated source. The precompiled `@caps.assumeSafe` base exposes them
 *  to subclasses while the submitted body is still checked under safe mode.
 */
@caps.assumeSafe
abstract class EvalExpressionBase(
    thisObject: Object | Null,
    bindings: Array[Eval.Binding]
):

  protected final val classLoader: ClassLoader = getClass.getClassLoader

  /** Implemented by each generated subclass. It is public because the adapter
   *  invokes it reflectively without changing accessibility.
   */
  def evaluate(): Any

  /** Returns the enclosing instance captured at the call site. */
  protected final def getThisObject(): Object | Null = thisObject

  private final def __findBinding__(name: String): Eval.Binding =
    var i = 0
    while i < bindings.length do
      if bindings(i).name == name then return bindings(i)
      i = i + 1
    throw new java.util.NoSuchElementException(name)

  /** Forces lazily captured modules and `lazy val`s on their first read.
   */
  private final def __unwrapLazy__(v: Any): Any = v match
    case lzy: Eval.LazyBindingValue => lzy.value
    case _ => v

  /** Reads a binding, dereferencing [[Eval.VarRef]] only for bindings marked as
   *  mutable. An immutable value that happens to be a `VarRef` is returned as is.
   */
  protected final def getValue(name: String): Any =
    val b = __findBinding__(name)
    if b.isVar then b.value.asInstanceOf[Eval.VarRef[Any]].get()
    else __unwrapLazy__(b.value)

  /** Reads a binding without dereferencing `VarRef`; lazy wrappers are forced. */
  protected final def getRaw(name: String): Any =
    __unwrapLazy__(__findBinding__(name).value)

  /** Finds an exact or Scala-mangled field name. Exact names take precedence
   *  over suffix matches, independent of reflection declaration order.
   */
  private final def __findField__(c: Class[?], name: String): java.lang.reflect.Field | Null =
    val fs = c.getDeclaredFields
    val suffix = "$" + name
    var mangled: java.lang.reflect.Field | Null = null
    var i = 0
    while i < fs.length do
      val f = fs(i)
      val n = f.getName
      if n == name then return f
      if mangled == null && n.endsWith(suffix) then mangled = f
      i = i + 1
    mangled

  /** Reads a field, walking superclasses and then trying a parameterless getter.
   *  An empty `className` selects the receiver's runtime class, which is needed
   *  when a local class was re-elaborated in the wrapper.
   */
  protected final def getField(obj: Object | Null, className: String, fieldName: String): Any =
    val startClass: Class[?] =
      if className.isEmpty then
        if obj == null then
          throw new NullPointerException("getField on null receiver with empty className")
        else obj.getClass
      else classLoader.loadClass(className)
    var clazz: Class[?] | Null = startClass
    while clazz != null do
      val field = __findField__(clazz, fieldName)
      if field != null then
        field.setAccessible(true)
        return field.get(obj)
      val ms = clazz.getDeclaredMethods
      var i = 0
      while i < ms.length do
        val m = ms(i)
        if m.getName == fieldName && m.getParameterCount == 0 then
          m.setAccessible(true)
          try return m.invoke(obj)
          catch
            case e: java.lang.reflect.InvocationTargetException => throw e.getCause
        i = i + 1
      clazz = clazz.getSuperclass
    // A constructor parameter that is used only inside the constructor (for
    // example by a closure in a field initializer) is not stored as a field.
    throw new NoSuchFieldException(
      s"$fieldName: ${startClass.getName} declares no such field or parameterless method. " +
        "A constructor parameter that is only used inside the constructor is not " +
        "stored as a field and cannot be read from an eval body.")

  /** Writes a field, walking superclasses and then trying `<name>_$eq`. An empty
   *  `className` selects the receiver's runtime class.
   */
  protected final def setField(obj: Object | Null, className: String, fieldName: String, value: Object | Null): Unit =
    var clazz: Class[?] | Null =
      if className.isEmpty then
        if obj == null then
          throw new NullPointerException("setField on null receiver with empty className")
        else obj.getClass
      else classLoader.loadClass(className)
    val setterName = fieldName + "_$eq"
    while clazz != null do
      val field = __findField__(clazz, fieldName)
      if field != null then
        field.setAccessible(true)
        field.set(obj, value)
        return ()
      val ms = clazz.getDeclaredMethods
      var i = 0
      while i < ms.length do
        val m = ms(i)
        if m.getName == setterName && m.getParameterCount == 1 then
          m.setAccessible(true)
          try { m.invoke(obj, value); return () }
          catch
            case e: java.lang.reflect.InvocationTargetException => throw e.getCause
        i = i + 1
      clazz = clazz.getSuperclass
    throw new NoSuchFieldException(fieldName)

  /** Invokes a method selected by name, parameter types, and return type. An
   *  empty `className` selects the receiver's runtime class and skips return-type
   *  matching. `"*"` is a wildcard for linked local types whose wrapper and
   *  runtime names differ.
   */
  protected final def callMethod(
      obj: Object | Null,
      className: String,
      methodName: String,
      paramTypesNames: Array[String],
      returnTypeName: String,
      args: Array[Object | Null]
  ): Any =
    val useReceiverClass = className.isEmpty
    var clazz: Class[?] | Null =
      if useReceiverClass then
        if obj == null then
          throw new NullPointerException("callMethod on null receiver with empty className")
        else obj.getClass
      else classLoader.loadClass(className)
    def paramsMatch(actual: Array[Class[?]]): Boolean =
      actual.length == paramTypesNames.length && {
        var i = 0
        var ok = true
        while ok && i < actual.length do
          val expected = paramTypesNames(i)
          if expected != "*" && actual(i).getName != expected then ok = false
          i += 1
        ok
      }
    var method: java.lang.reflect.Method | Null = null
    while clazz != null && method == null do
      method = clazz.getDeclaredMethods
        .find { m =>
          m.getName == methodName &&
            (useReceiverClass || returnTypeName == "*" || m.getReturnType.getName == returnTypeName) &&
            paramsMatch(m.getParameterTypes)
        }
        .getOrElse(null)
      if method == null then clazz = clazz.getSuperclass
    if method == null then throw new NoSuchMethodException(methodName)
    method.setAccessible(true)
    val res: Any =
      try method.invoke(obj, args*)
      catch
        case e: java.lang.reflect.InvocationTargetException => throw e.getCause
    if returnTypeName == "void" then (() : Any) else res

  /** Returns the original runtime class for a local class re-elaborated by the
   *  wrapper. [[ResolveEvalAccess]] also uses this for `classOf[C]`.
   */
  protected final def linkedClass(sourceName: String): Class[?] =
    getRaw(EvalNames.classBinding(sourceName)) match
      case c: Class[?] => c
      case other => throw new IllegalStateException(
        s"linked-class binding for `$sourceName` is not a Class: $other")

  /** Tests against the original runtime class of a linked local class. */
  protected final def isLinkedInstance(obj: Object | Null, sourceName: String): Boolean =
    obj != null && linkedClass(sourceName).isInstance(obj)

  /** Casts against the original runtime class. `null` passes through, matching
   *  JVM `checkcast` semantics.
   */
  protected final def castLinked(obj: Object | Null, sourceName: String): Object | Null =
    if obj == null then null
    else
      val cls = linkedClass(sourceName)
      if cls.isInstance(obj) then obj
      else throw new ClassCastException(
        s"${obj.getClass.getName} cannot be cast to linked local class " +
          s"$sourceName (${cls.getName})")

  /** Returns the `dims`-dimensional array class for a linked local class.
   */
  protected final def linkedArrayClass(sourceName: String, dims: Int): Class[?] =
    var cls = linkedClass(sourceName)
    var i = 0
    while i < dims do
      cls = java.lang.reflect.Array.newInstance(cls, 0).getClass
      i += 1
    cls

  /** Allocates an array whose ultimate component is the original linked class.
   *  Only the outer dimension is initialized, matching `new Array` semantics.
   */
  protected final def newLinkedArray(sourceName: String, dims: Int, length: Int): Object =
    java.lang.reflect.Array.newInstance(linkedArrayClass(sourceName, dims - 1), length)

  /** Implements `Array.ofDim` and post-extraction generic allocation for linked
   *  elements. `elemDims` counts array layers already present in the element.
   */
  protected final def newLinkedArrayDims(sourceName: String, elemDims: Int, dims: Array[Int]): Object =
    java.lang.reflect.Array.newInstance(linkedArrayClass(sourceName, elemDims), dims*)

  /** Copies literal elements into an array whose runtime component is the
   *  original linked class. `arraycopy` preserves normal array store checks.
   */
  protected final def arrayOfLinked(sourceName: String, dims: Int, elems: Array[Object | Null]): Object =
    val out = java.lang.reflect.Array.newInstance(linkedArrayClass(sourceName, dims - 1), elems.length)
    java.lang.System.arraycopy(elems, 0, out, 0, elems.length)
    out

  /** Tests an array against the original linked component class. JVM array
   *  covariance applies at every dimension.
   */
  protected final def isLinkedArrayInstance(obj: Object | Null, sourceName: String, dims: Int): Boolean =
    obj != null && linkedArrayClass(sourceName, dims).isInstance(obj)

  /** Casts an array against the original linked component class. `null` passes
   *  through as it does for `checkcast`.
   */
  protected final def castLinkedArray(obj: Object | Null, sourceName: String, dims: Int): Object | Null =
    if obj == null then null
    else if isLinkedArrayInstance(obj, sourceName, dims) then obj
    else throw new ClassCastException(
      s"${obj.getClass.getName} cannot be cast to " +
        s"${linkedArrayClass(sourceName, dims).getName} of linked local class $sourceName")

  /** Placeholder replaced by [[ResolveEvalAccess]]. Reaching it at runtime means
   *  lowering failed; `final` prevents a generated subclass from hiding that.
   */
  protected final def reflectEval(qualifier: Object | Null, strategyDesc: String, args: Array[Object | Null]): Any =
    throw new UnsupportedOperationException("reflectEval placeholder was not lowered")

  /** Helpers targeted by the private-member rewrite in [[SpliceEvalBody]]. The
   *  get and set forms use the receiver's runtime class. The call form selects
   *  by name, arity, and runtime argument compatibility because the parser-stage
   *  rewrite has no static parameter-type names.
   */
  protected final def __refl_get__(obj: Object, name: String): Any =
    getField(obj, "", name)

  /** Typed read of a member whose declaration relies on type inference:
   *  `sample` wraps a re-parsed copy of the member's initializer, never
   *  invoked, that only pins `T` for the wrapper's typer. A JDK functional
   *  interface keeps the signature stable across the eval classloader
   *  boundary, where `scala.Function0` may have another `Class` identity.
   */
  protected final def __refl_get_as__[T](obj: Object, name: String, sample: java.util.function.Supplier[T]): T =
    getField(obj, "", name).asInstanceOf[T]

  protected final def __refl_set__(obj: Object, name: String, value: Any): Unit =
    setField(obj, "", name, value.asInstanceOf[Object | Null])

  protected final def __refl_call__(obj: Object, name: String, args: Array[Object]): Any =
    val arity = args.length
    var clazz: Class[?] | Null = obj.getClass
    val compatible = Array.newBuilder[java.lang.reflect.Method]
    while clazz != null do
      val ms = clazz.getDeclaredMethods
      var i = 0
      while i < ms.length do
        val m = ms(i)
        if m.getName == name && m.getParameterCount == arity
            && __runtimeArgsCompatible__(m.getParameterTypes, args) then
          compatible += m
        i = i + 1
      clazz = clazz.getSuperclass
    val candidates = compatible.result()
    if candidates.length == 1 then
      val method = candidates(0)
      method.setAccessible(true)
      try method.invoke(obj, args*)
      catch
        case e: java.lang.reflect.InvocationTargetException => throw e.getCause
    else if candidates.length > 1 then
      val signatures = candidates.map(__methodSignature__).sorted.mkString(", ")
      throw new IllegalArgumentException(
        s"ambiguous reflective call `$name` for runtime arguments " +
          s"${__runtimeArgTypes__(args)}; compatible declarations: $signatures")
    else throw new NoSuchMethodException(name)

  /** Whether reflection can pass the boxed runtime arguments to these parameter
   *  types. Primitive parameters require explicit unboxing compatibility.
   */
  private final def __runtimeArgsCompatible__(
      parameterTypes: Array[Class[?]],
      args: Array[Object]
  ): Boolean =
    var i = 0
    var compatible = true
    while compatible && i < parameterTypes.length do
      val parameterType = parameterTypes(i)
      val arg = args(i)
      compatible =
        if arg == null then !parameterType.isPrimitive
        else if parameterType.isPrimitive then
          __primitiveArgCompatible__(parameterType, arg.getClass)
        else parameterType.isInstance(arg)
      i += 1
    compatible

  /** Whether reflection can unbox and widen `argumentClass` to the primitive
   *  parameter type, for example `Integer` to `long`.
   */
  private final def __primitiveArgCompatible__(
      parameterType: Class[?],
      argumentClass: Class[?]
  ): Boolean =
    val isByte = argumentClass == classOf[java.lang.Byte]
    val isShort = argumentClass == classOf[java.lang.Short]
    val isChar = argumentClass == classOf[java.lang.Character]
    val isInt = argumentClass == classOf[java.lang.Integer]
    val isLong = argumentClass == classOf[java.lang.Long]
    val isFloat = argumentClass == classOf[java.lang.Float]
    val isDouble = argumentClass == classOf[java.lang.Double]
    if parameterType == java.lang.Boolean.TYPE then
      argumentClass == classOf[java.lang.Boolean]
    else if parameterType == java.lang.Byte.TYPE then isByte
    else if parameterType == java.lang.Short.TYPE then isByte || isShort
    else if parameterType == java.lang.Character.TYPE then isChar
    else if parameterType == java.lang.Integer.TYPE then isByte || isShort || isChar || isInt
    else if parameterType == java.lang.Long.TYPE then isByte || isShort || isChar || isInt || isLong
    else if parameterType == java.lang.Float.TYPE then
      isByte || isShort || isChar || isInt || isLong || isFloat
    else if parameterType == java.lang.Double.TYPE then
      isByte || isShort || isChar || isInt || isLong || isFloat || isDouble
    else false // `void` cannot occur in a method parameter list.

  private final def __methodSignature__(method: java.lang.reflect.Method): String =
    method.getDeclaringClass.getName + "." + method.getName +
      method.getParameterTypes.map(_.getTypeName).mkString("(", ", ", ")")

  private final def __runtimeArgTypes__(args: Array[Object]): String =
    args.map(arg => if arg == null then "null" else arg.getClass.getTypeName).mkString("(", ", ", ")")

end EvalExpressionBase
