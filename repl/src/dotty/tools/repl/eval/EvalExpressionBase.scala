package dotty.tools
package repl
package eval

// `experimental.captureChecking` is required for `@caps.assumeSafe`
// to be admitted on this class. When safe-mode user sessions extend
// this base, they don't re-check it — they reference the
// pre-compiled, `@assumeSafe`-tagged form and inherit its members
// without `@rejectSafe` propagation.
import scala.language.experimental.captureChecking

/** Pre-compiled base class for the synthesised `__EvalExpression_…`
 *  classes.
 *
 *  The eval driver compiles a synthesised `__EvalExpression_<uuid>`
 *  for each call site. That class's `evaluate()` method is filled
 *  in by `ExtractEvalBody` with the user's body, and references
 *  outside the body get rewritten to calls against the helpers
 *  defined here (`getValue`, `getField`, `setField`, `callMethod`,
 *  `getOuter`, `reflectEval`).
 *
 *  Why a pre-compiled base instead of inlining the helpers in the
 *  synthesised source: the helpers call into JVM reflection
 *  (`Class.getDeclaredFields`, `Method.invoke`, `getClassLoader`),
 *  which is `@rejectSafe` under safe mode. If the helpers were
 *  inlined into the synthesised class, a safe-mode REPL session
 *  would compile the helpers under safe mode and fail to compile
 *  even a trivial body. Having the helpers live on a `@assumeSafe`
 *  base, compiled at dotty-build time outside safe mode, lets a
 *  safe-mode synthesised subclass extend it and inherit the
 *  helpers without retripping the checks.
 */
@caps.assumeSafe
abstract class EvalExpressionBase(
    thisObject: Object | Null,
    bindings: Array[Eval.Binding]
):

  protected final val classLoader: ClassLoader = getClass.getClassLoader

  /** The synthesised `__EvalExpression_<uuid>` subclass implements
   *  this with the user's body — left abstract here so that a
   *  subclass missing `evaluate()` is a compile error rather than
   *  silently inheriting a no-op.
   *
   *  Public because the runtime invokes it reflectively from
   *  `EvalAdapter.invokeCached` — `protected` would force a
   *  `setAccessible(true)` call there with no benefit.
   */
  def evaluate(): Any

  /** All helpers below are `protected final`: they're only ever
   *  called from inside the synthesised `evaluate()` body (or from
   *  one another), and a subclass overriding any of them would
   *  silently break the lowered eval contract.
   */
  protected final def getThisObject(): Object | Null = thisObject

  /** Look up a binding by name, auto-unwrapping `Eval.VarRef` so the
   *  body sees a `T` rather than a live var facade. Var-assignment
   *  codegen uses `getRaw` to preserve the facade.
   */
  protected final def getValue(name: String): Any =
    val raw = getRaw(name)
    if raw.isInstanceOf[Eval.VarRef[?]] then
      raw.asInstanceOf[Eval.VarRef[Any]].get()
    else raw

  /** Return the binding's stored value as-is (no `VarRef` unwrap). */
  protected final def getRaw(name: String): Any =
    var i = 0
    while i < bindings.length do
      if bindings(i).name == name then return bindings(i).value
      i = i + 1
    throw new java.util.NoSuchElementException(name)

  /** Walk the `$outer` chain on `obj`. Used to lower the `Outer`
   *  strategy so a body inside a nested class can reach an
   *  outer-class instance.
   */
  protected final def getOuter(obj: Object | Null, outerTypeName: String): Object | Null =
    if obj == null then return null
    var clazz: Class[?] | Null = obj.getClass
    while clazz != null do
      val fields = clazz.getDeclaredFields
      var i = 0
      while i < fields.length do
        val f = fields(i)
        if f.getName == "$outer" && f.getType.getName == outerTypeName then
          f.setAccessible(true)
          return f.get(obj)
        i = i + 1
      clazz = clazz.getSuperclass
    throw new NoSuchFieldException("$outer (" + outerTypeName + ") on " + obj.getClass.getName)

  /** Match a field whose name is either the literal `name` or any
   *  Scala 3-mangled `<owner-chain>$<name>` form. Returns null on
   *  miss so the caller can decide whether to fall back to a
   *  getter / superclass walk.
   */
  private final def __findField__(c: Class[?], name: String): java.lang.reflect.Field | Null =
    val fs = c.getDeclaredFields
    var i = 0
    val suffix = "$" + name
    while i < fs.length do
      val f = fs(i)
      val n = f.getName
      if n == name || n.endsWith(suffix) then return f
      i = i + 1
    null

  /** Reflective field read on an instance of `className`. Walks
   *  superclasses; falls back to a 0-arg getter method when no
   *  direct field matches. `obj` may be null for static fields.
   *
   *  An empty `className` signals "use obj.getClass" — used for
   *  members of a term-owned class, where the wrapper's symbol and
   *  the runtime instance live in different JVM classes with the
   *  same shape.
   */
  protected final def getField(obj: Object | Null, className: String, fieldName: String): Any =
    var clazz: Class[?] | Null =
      if className.isEmpty then
        if obj == null then
          throw new NullPointerException("getField on null receiver with empty className")
        else obj.getClass
      else classLoader.loadClass(className)
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
    throw new NoSuchFieldException(fieldName)

  /** Reflective field write. Walks superclasses; falls back to a
   *  setter (`<name>_$eq`) when no direct field matches. An empty
   *  `className` means "use obj.getClass" (see `getField`).
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

  /** Reflective method call matching by name + parameter type names
   *  + return type name. `obj` may be null for static (module / Java)
   *  methods. An empty `className` means "use obj.getClass" (see
   *  `getField`); in that mode the return-type filter is skipped
   *  too — the wrapper's symbol-derived return type won't match the
   *  runtime instance's signature for a term-owned class.
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
    var method: java.lang.reflect.Method | Null = null
    while clazz != null && method == null do
      method = clazz.getDeclaredMethods
        .find { m =>
          m.getName == methodName &&
            (useReceiverClass || m.getReturnType.getName == returnTypeName) &&
            m.getParameterTypes.map(_.getName).toSeq == paramTypesNames.toSeq
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

  /** Placeholder consumed by `ResolveEvalAccess`, which rewrites
   *  every `reflectEval(...)` Apply in `evaluate`'s body into the
   *  matching concrete accessor (`getValue`, `getField`,
   *  `callMethod`, …). `final` so a synthesised subclass can't
   *  shadow it: any remaining call site (post-extract, post-resolve)
   *  hitting this body means the lowering pipeline broke and the
   *  exception surfaces the bug.
   */
  protected final def reflectEval(qualifier: Object | Null, strategyDesc: String, args: Array[Object | Null]): Any =
    throw new UnsupportedOperationException("reflectEval placeholder was not lowered")

  /** Bare-name aliases used by the body's private-member rewrite in
   *  `SpliceEvalBody.rewritePrivateAccessInBody`. The body emits
   *  `__refl_get__/_set__/_call__(qual, name, …)` references that the
   *  typer initially resolves against wrapper-local stubs; after
   *  `ExtractEvalBody` retargets the symbols, they resolve here via
   *  inheritance. `get`/`set` delegate to the corresponding accessors
   *  with an empty `className` (which falls back to `obj.getClass`).
   *  `__refl_call__` matches by name + arity (the body-rewrite path
   *  doesn't carry param-type names), so it can't reuse `callMethod`'s
   *  signature-based lookup directly.
   */
  protected final def __refl_get__(obj: Object, name: String): Any =
    getField(obj, "", name)

  protected final def __refl_set__(obj: Object, name: String, value: Any): Unit =
    setField(obj, "", name, value.asInstanceOf[Object | Null])

  protected final def __refl_call__(obj: Object, name: String, args: Array[Object]): Any =
    val arity = args.length
    var clazz: Class[?] | Null = obj.getClass
    while clazz != null do
      val ms = clazz.getDeclaredMethods
      var i = 0
      while i < ms.length do
        val m = ms(i)
        if m.getName == name && m.getParameterCount == arity then
          m.setAccessible(true)
          try return m.invoke(obj, args*)
          catch
            case e: java.lang.reflect.InvocationTargetException => throw e.getCause
        i = i + 1
      clazz = clazz.getSuperclass
    throw new NoSuchMethodException(name)

end EvalExpressionBase
