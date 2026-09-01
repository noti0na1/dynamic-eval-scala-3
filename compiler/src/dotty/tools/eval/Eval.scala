package dotty.tools
package eval

// Required for the `@caps.assumeSafe` annotation below.
import scala.language.experimental.captureChecking

/** Runtime compilation for the Scala 3 REPL and programs built with
 *  `-Xdynamic-eval`.
 *
 *  `eval[T](code)` compiles Scala source in the call site's lexical context
 *  and returns its value as `T`. REPL commands such as `:reset` are rejected;
 *  this API accepts Scala source only.
 *
 *  [[EvalRewriteTyped]] captures local terms and the enclosing source. It is
 *  always enabled by the REPL and enabled for regular compilation by
 *  `-Xdynamic-eval`.
 *
 *  Values that cross the user/eval classloader boundary use arrays, JDK
 *  functional interfaces, and eval-owned result types rather than Scala
 *  collection and function classes, whose identities may differ by loader.
 *
 *  The API is `@caps.assumeSafe` so it remains callable in safe mode. Generated
 *  bodies are compiled under the session's safe-mode settings. A generator
 *  callback, however, is ordinary host code and can inspect runtime values in
 *  its [[EvalContext]].
 */
@caps.assumeSafe
object Eval:

  /** A value captured from the call site.
   *
   *  @param name source name, or a reserved name for an internal binding.
   *  @param value runtime value; mutable captures contain a [[VarRef]].
   *  @param tpe source representation of the static type. It may be empty when
   *             no suitable representation is available.
   *  @param isVar whether this binding represents a mutable variable.
   *  @param isGiven whether this binding participates in implicit search.
   *  @param isSynthetic whether the binding is an internal runtime link rather
   *                     than a user-visible name.
   */
  final class Binding(
      val name: String,
      val value: Any,
      val tpe: String = "",
      val isVar: Boolean = false,
      val isGiven: Boolean = false,
      val isSynthetic: Boolean = false
  ):
    override def toString =
      s"Binding($name, $value, tpe=$tpe, isVar=$isVar, isGiven=$isGiven, isSynthetic=$isSynthetic)"

  /** Defers a captured value until its first read. This prevents construction
   *  of the bindings array from forcing a `lazy val` or local module.
   */
  final class LazyBindingValue(supplier: java.util.function.Supplier[Any]):
    lazy val value: Any = supplier.get()

  /** Live access to a captured `var`. The getter and setter close over the
   *  original variable, so changes remain visible on both sides.
   */
  trait VarRef[T]:
    def get(): T
    def set(v: T): Unit

  /** Creates a [[VarRef]] from JDK functional interfaces, whose class identity
   *  is stable across the eval classloader boundary.
   */
  def varRef[T](
      get: java.util.function.Supplier[T],
      set: java.util.function.Consumer[T]
  ): VarRef[T] =
    val getFn = get
    val setFn = set
    new VarRef[T]:
      def get(): T = getFn.get()
      def set(v: T): Unit = setFn.accept(v)

  /** Captures an immutable binding. */
  def bind(name: String, value: Any, tpe: String = ""): Binding =
    new Binding(name, value, tpe)

  /** Captures a mutable binding through a live [[VarRef]]. */
  def bindVar(name: String, ref: VarRef[?], tpe: String = ""): Binding =
    new Binding(name, ref, tpe, isVar = true)

  /** Captures a `given` that the wrapper exposes to implicit search. Named
   *  givens also remain accessible by name.
   */
  def bindGiven(name: String, value: Any, tpe: String = ""): Binding =
    new Binding(name, value, tpe, isGiven = true)

  /** Captures an internal value used to link generated code to the program. */
  def bindSynthetic(name: String, value: Any, tpe: String = ""): Binding =
    new Binding(name, value, tpe, isSynthetic = true)

  /** Type-checking shim inserted around a spliced `evalSafe` body. It gives the
   *  verification expression type `EvalResult[T]`; the runtime path does not
   *  invoke this method.
   */
  def handleCompileError[T](v: T): EvalResult[T] =
    EvalResult.success(v)

  /** Opaque no-op that prevents constant folding from removing the generated
   *  result value before [[ExtractEvalBody]] moves its right-hand side.
   */
  def __noFold__(): Unit = ()

  /** Compilation failure returned through the adapter boundary. Keeping it as
   *  a value lets `evalSafe` distinguish this call's compilation failure from
   *  an exception raised by a nested eval while the body runs.
   */
  final class CompileFailure(val errors: Array[String], val source: String):
    override def toString: String =
      s"CompileFailure(${errors.length} error(s))"

  /** Runtime adapter installed by a REPL driver or supplied by standalone
   *  evaluation. `expectedType` is a source representation of the requested
   *  result type, or an empty string when unavailable.
   *
   *  `enclosingSource` contains the surrounding statement with the call
   *  replaced by [[EvalContext.placeholder]]. If empty, the adapter compiles in
   *  an isolated context where only globally accessible names are available.
   *
   *  The result contains either the body value or this call's compilation
   *  failure. Exceptions thrown while running the body propagate unchanged.
   */
  trait Adapter:
    def evalCode(
        code: String,
        bindings: Array[Binding],
        expectedType: String,
        enclosingSource: String
    ): Either[CompileFailure, Any]

    /** Compiles [[topLevel]] definitions into a persistent handle.
     *  `contextHeader` contains file-level imports when available. The default
     *  preserves compatibility with adapters that support expression eval only.
     */
    def compileTopLevel(defs: String, contextHeader: String): Either[CompileFailure, TopLevel] =
      Left(new CompileFailure(
        Array("Eval.topLevel is not supported by the installed eval adapter"), defs))

  /** Handle to definitions compiled once for use by later evaluations.
   *
   *  The handle owns its output directory and classloader. Evaluations through
   *  the same handle therefore share class identity and module state.
   *
   *  Call-site locals take precedence over handle definitions, which in turn
   *  take precedence over session definitions.
   */
  @caps.assumeSafe
  final class TopLevel private[eval] (
      private[eval] val objectName: String,
      private[eval] val outputDir: Object,
      private[eval] val loader: ClassLoader,
      val defs: String
  ):
    private def withHandle(bindings: Array[Binding]): Array[Binding] =
      bindings :+ binding

    /** Binding that links this handle to an eval call. It is public so an
     *  embedder can retain and later restore the link. */
    def binding: Binding = bindSynthetic(EvalNames.topLevelBinding(objectName), this)

    /** Evaluates `code` with this handle's definitions in scope. Throws
     *  [[EvalCompileException]] when compilation fails.
     */
    @evalLike
    def eval[T](
        code: String,
        bindings: Array[Binding] = Array.empty[Binding],
        expectedType: String = "",
        enclosingSource: String = ""
    ): T =
      Eval.eval[T](code, withHandle(bindings), expectedType, enclosingSource)

    /** Non-throwing compilation variant of [[eval]]. Runtime exceptions still
     *  propagate from the body.
     */
    @evalSafeLike
    def evalSafe[T](
        code: String,
        bindings: Array[Binding] = Array.empty[Binding],
        expectedType: String = "",
        enclosingSource: String = ""
    ): EvalResult[T] =
      Eval.evalSafe[T](code, withHandle(bindings), expectedType, enclosingSource)

    override def toString: String = s"Eval.TopLevel($objectName)"
  end TopLevel

  // Inheritable so a thread created by an eval body can use the current
  // adapter. The value is copied when the thread is created:
  //
  //   - threads created while an adapter is installed (the common
  //     case) inherit it for their lifetime;
  //   - threads created earlier fall back to standalone evaluation and do not
  //     see session-local scope.
  // A process-wide fallback would incorrectly share adapters between sessions.
  private val active = new InheritableThreadLocal[Adapter]

  /** Installs an adapter for `thunk`. Passing `null` temporarily removes the
   *  current adapter and remains usable with `-Yexplicit-nulls`. */
  def withAdapter[T](adapter: Adapter | Null)(thunk: => T): T =
    val prev = active.get
    active.set(adapter)
    try thunk
    finally if prev == null then active.remove() else active.set(prev)

  /** Active [[TopLevel]] handles inherited by nested evaluations and child
   *  threads. This keeps definitions linked when a capture outlives a call. */
  private val activeHandles = new InheritableThreadLocal[List[TopLevel]]:
    override def initialValue(): List[TopLevel] = Nil

  private[eval] def currentActiveHandles: List[TopLevel] = activeHandles.get

  private[eval] def setActiveHandles(hs: List[TopLevel]): Unit =
    if hs.isEmpty then activeHandles.remove() else activeHandles.set(hs)

  /** Weak registry used by code compiled inside a definitions object to recover
   *  its handle without keeping the handle or classloader alive. */
  private object TopLevelRegistry:
    private val entries =
      java.util.concurrent.ConcurrentHashMap[String, java.lang.ref.WeakReference[TopLevel]]()
    def register(h: TopLevel): Unit =
      entries.put(h.objectName, java.lang.ref.WeakReference(h))
      ()
    def lookup(objectName: String): TopLevel | Null =
      val ref = entries.get(objectName)
      if ref == null then null
      else
        val h = ref.get()
        if h == null then entries.remove(objectName)
        h

  private[eval] def registerTopLevel(h: TopLevel): Unit = TopLevelRegistry.register(h)

  /** Adds active and enclosing-definition handles to a generated bindings
   *  array. Existing bindings take precedence and duplicate handles are removed.
   */
  def withInheritedHandles(defsObject: String, bindings: Array[Binding]): Array[Binding] =
    val own =
      if defsObject.isEmpty then Nil
      else TopLevelRegistry.lookup(defsObject) match
        case null         => Nil
        case h: TopLevel  => h :: Nil
    val extra = (activeHandles.get ++ own)
      .distinctBy(_.objectName)
      .map(_.binding)
      .filterNot(b => bindings.exists(_.name == b.name))
    if extra.isEmpty then bindings else bindings ++ extra

  /** Compiles and runs `code` using the active adapter.
   *
   *  The overloads accept either a source string or a generator. A generator
   *  receives [[EvalContext]] and returns the source to compile. JDK functional
   *  interfaces keep the public signature stable across classloaders.
   *
   *  [[EvalRewriteTyped]] normally fills the context parameters:
   *
   *    - `bindings`: every term-level name (lambda parameter,
   *      block-local val/var/def/given, method parameter) syntactically
   *      in scope at the call site;
   *    - `expectedType`: the requested result type, when renderable;
   *    - `enclosingSource`: the source of the enclosing top-level
   *      statement with this call's location replaced by
   *      [[EvalContext.placeholder]].
   *
   *  Direct callers (no rewriter) can leave them at their defaults.
   */
  def eval[T](
      code: String,
      bindings: Array[Binding] = Array.empty[Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): T =
    evalImpl[T](code, bindings, expectedType, enclosingSource)

  // Defaults belong to the string overload because Scala allows defaults on
  // only one overload. The generator gets a one-argument convenience method.

  def eval[T](gen: java.util.function.Function[EvalContext, String]): T =
    eval[T](gen, Array.empty[Binding], "", "")

  def eval[T](
      gen: java.util.function.Function[EvalContext, String],
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String
  ): T =
    val ctx = new EvalContext(bindings, expectedType, enclosingSource)
    evalImpl[T](gen.apply(ctx), bindings, expectedType, enclosingSource)

  /** Non-throwing compilation variant of [[eval]]. Compilation failures are returned
   *  in [[EvalResult]]; exceptions thrown by the body still propagate.
   */
  def evalSafe[T](
      code: String,
      bindings: Array[Binding] = Array.empty[Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): EvalResult[T] =
    evalSafeImpl[T](code, bindings, expectedType, enclosingSource)

  def evalSafe[T](gen: java.util.function.Function[EvalContext, String]): EvalResult[T] =
    evalSafe[T](gen, Array.empty[Binding], "", "")

  def evalSafe[T](
      gen: java.util.function.Function[EvalContext, String],
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String
  ): EvalResult[T] =
    val ctx = new EvalContext(bindings, expectedType, enclosingSource)
    evalSafeImpl[T](gen.apply(ctx), bindings, expectedType, enclosingSource)

  /** Compiles `defs` once and returns a [[TopLevel]] handle. Definitions see the
   *  recorded global imports and prior REPL lines, but not method-local values or
   *  types. Compilation is eager and failures are thrown here.
   *
   *  The synthetic parameters are filled by the rewriter like
   *  [[eval]]'s. Standalone evaluation uses leading imports from
   *  `enclosingSource` as the compilation context.
   */
  @evalLike
  def topLevel(
      defs: String,
      bindings: Array[Binding] = Array.empty[Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): TopLevel =
    topLevelSafe(defs, bindings, expectedType, enclosingSource).get

  /** Non-throwing compilation variant of [[topLevel]]. */
  @evalSafeLike
  def topLevelSafe(
      defs: String,
      bindings: Array[Binding] = Array.empty[Binding],
      expectedType: String = "",
      enclosingSource: String = ""
  ): EvalResult[TopLevel] =
    forbiddenReplCommand(defs) match
      case failure: CompileFailure => EvalResult.failure(failure)
      case null =>
        activeAdapter().compileTopLevel(defs, leadingImports(enclosingSource)) match
          case Right(handle) => EvalResult.success(handle)
          case Left(f) => EvalResult.failure(f)

  /** Extracts the leading imports recorded for standalone evaluation. REPL
   *  session imports reach the compiler through the adapter instead.
   */
  private def leadingImports(enclosingSource: String): String =
    enclosingSource.linesIterator
      .takeWhile(l => l.trim.isEmpty || l.trim.startsWith("import "))
      .mkString("\n")

  private def evalImpl[T](
      code: String,
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String
  ): T =
    evalSafeImpl[T](code, bindings, expectedType, enclosingSource).get

  private def evalSafeImpl[T](
      code: String,
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String
  ): EvalResult[T] =
    // Do not catch EvalCompileException: one thrown here came from code running
    // in the body, such as a nested eval. Only the adapter's Left describes this
    // call's own compilation failure.
    forbiddenReplCommand(code) match
      case failure: CompileFailure => EvalResult.failure(failure)
      case null =>
        activeAdapter().evalCode(code, bindings, expectedType, enclosingSource) match
          case Right(v) => EvalResult.success(v.asInstanceOf[T])
          case Left(f) => EvalResult.failure(f)

  /** Rejects input whose first significant token has REPL command form
   *  `:[A-Za-z]`. Dynamic evaluation accepts Scala source, not commands.
   *
   *  Leading whitespace and comments are ignored. Operators such as `::` and
   *  `:+`, and colons appearing later in Scala source, remain valid.
   */
  private def forbiddenReplCommand(code: String): CompileFailure | Null =
    var offset = 0
    // The REPL accepts a shebang before its command preamble, so skip one here
    // before checking the first significant token.
    var shebangOffset = 0
    while shebangOffset < code.length
        && code.charAt(shebangOffset) != '\n'
        && code.charAt(shebangOffset) != '\r'
        && java.lang.Character.isWhitespace(code.charAt(shebangOffset))
    do shebangOffset += 1
    if shebangOffset + 1 < code.length
        && code.charAt(shebangOffset) == '#'
        && code.charAt(shebangOffset + 1) == '!'
    then
      offset = shebangOffset + 2
      while offset < code.length && code.charAt(offset) != '\n' && code.charAt(offset) != '\r' do
        offset += 1

    var scanningPrefix = true
    while scanningPrefix do
      while offset < code.length && java.lang.Character.isWhitespace(code.charAt(offset)) do
        offset += 1
      if offset + 1 < code.length && code.charAt(offset) == '/' && code.charAt(offset + 1) == '/' then
        offset += 2
        while offset < code.length && code.charAt(offset) != '\n' && code.charAt(offset) != '\r' do
          offset += 1
      else if offset + 1 < code.length && code.charAt(offset) == '/' && code.charAt(offset + 1) == '*' then
        var depth = 1
        offset += 2
        while offset + 1 < code.length && depth > 0 do
          if code.charAt(offset) == '/' && code.charAt(offset + 1) == '*' then
            depth += 1
            offset += 2
          else if code.charAt(offset) == '*' && code.charAt(offset + 1) == '/' then
            depth -= 1
            offset += 2
          else offset += 1
        // An unterminated comment is a Scala compile error, not command input.
        if depth > 0 then scanningPrefix = false
      else scanningPrefix = false

    def isAsciiLetter(ch: Char): Boolean =
      (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')

    if offset + 1 < code.length && code.charAt(offset) == ':' && isAsciiLetter(code.charAt(offset + 1)) then
      new CompileFailure(
        Array("REPL commands are not allowed in dynamic evaluation; provide Scala code instead."),
        code
      )
    else null

  private def activeAdapter(): Adapter =
    val a = active.get
    // Programs compiled with -Xdynamic-eval use the standalone adapter when no
    // REPL session has installed one.
    if a == null then StandaloneAdapter.instance else a

end Eval
