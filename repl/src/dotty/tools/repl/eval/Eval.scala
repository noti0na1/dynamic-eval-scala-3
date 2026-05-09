package dotty.tools
package repl
package eval

// Enables the `@caps.assumeSafe` annotation on `object Eval` below: that
// annotation is `@experimental`, but the cc-experimental exception lets
// it be used in any unit that imports `experimental.captureChecking`.
import scala.language.experimental.captureChecking

/** Runtime `eval` for the dotty REPL.
 *
 *  `Eval.eval(code)` compiles and runs `code` at runtime, returning the
 *  result as the polymorphic type `T`. The argument can be any `String`.
 *
 *  The REPL parser injects bindings automatically for every lambda
 *  parameter (and block-local val/def) syntactically in scope at the call
 *  site, so `xs.map(z => eval[Int]("z + 1"))` works without the user
 *  writing the bindings explicitly.
 *
 *  The public surface intentionally avoids any Scala-library types
 *  (`Seq`, `ClassTag`, etc.). User wrappers and `Eval` are loaded by
 *  different classloaders, and Scala types resolve to different `Class`
 *  objects across the boundary, causing `LinkageError`. We use only
 *  JVM-intrinsic types (`String`, `Object`, `Array`) on the API surface.
 *
 *  Outside an active REPL session this throws.
 *
 *  Tagged `@caps.assumeSafe` so safe-mode user code can call `eval`,
 *  `evalSafe`, `bind`, etc. The eval driver re-compiles the user's body
 *  string under the live REPL's flags, so a safe-mode session still
 *  applies safe-mode checks to the body itself: this annotation only
 *  exempts the eval driver's own surface API from safe-mode rejection,
 *  not the bodies it compiles.
 */
@caps.assumeSafe
object Eval:

  /** A captured binding.
   *
   *  @param name     source-level name at the call site.
   *  @param value    runtime value, or for var captures a [[VarRef]]
   *                  facade closing over the outer var.
   *  @param isVar    `var` capture.
   *  @param isGiven  `given` capture. The runtime emits given bindings
   *                  as members of a `(using ...)` clause on the
   *                  synthesised wrapper so `summon[T]` resolves.
   */
  final class Binding(
      val name: String,
      val value: Any,
      val isVar: Boolean,
      val isGiven: Boolean = false
  ):
    override def toString =
      s"Binding($name, $value, isVar=$isVar, isGiven=$isGiven)"

  /** Live getter/setter facade over a captured `var`. The rewriter
   *  emits a `varRef(() => x, v => x = v)` at every `bindVar` site:
   *  the lambdas close over the outer var, so Scala's standard local-
   *  var capture (boxing into `runtime.IntRef`/`ObjectRef`) keeps the
   *  facade and the original variable in sync. Reads through `get()`
   *  always return the current value (not a snapshot taken when the
   *  eval call started) and writes through `set(v)` are immediately
   *  visible to every other piece of code that captured the same
   *  outer var.
   *
   *  Lives in `dotty.tools.repl` so the eval-output classloader routes
   *  the `Class` through the REPL infra loader (one shared `Class` on
   *  both sides of the eval boundary; see BetterEval.md "Classloader bridging").
   */
  trait VarRef[T]:
    def get(): T
    def set(v: T): Unit

  /** Helper used by the parser-stage rewriter so the bind-site call
   *  `Eval.varRef(() => x, v => x = v)` stays terse. The parameter
   *  types are the JDK functional interfaces (`Supplier`/`Consumer`)
   *  rather than `scala.Function0`/`Function1`: Scala-library types
   *  on the eval API surface trigger `LinkageError` when called from
   *  user-classloader-loaded code (each loader has its own `Class`
   *  object for `Function0` etc.; see BetterEval.md "Classloader bridging").
   *  Scala 3's SAM conversion accepts a `() => x` literal where a
   *  `Supplier[T]` is expected and a `v => x = v` literal where a
   *  `Consumer[T]` is expected, with `T` solved from the getter and
   *  propagated to the setter's parameter.
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

  /** Capture an immutable binding. */
  def bind(name: String, value: Any): Binding =
    new Binding(name, value, isVar = false)

  /** Capture a mutable (`var`) binding via a [[VarRef]] facade. The
   *  body's reads/writes are routed through `ref.get()` / `ref.set(v)`
   *  so they stay live with respect to the outer scope.
   */
  def bindVar(name: String, ref: VarRef[?]): Binding =
    new Binding(name, ref, isVar = true)

  /** Capture a `given` binding. The runtime emits it as a member of a
   *  `(using ...)` clause on the synthesised wrapper so `summon[T]`
   *  resolves. Named givens are also reachable by name; anonymous
   *  givens get a synthetic name and are only summonable.
   */
  def bindGiven(name: String, value: Any): Binding =
    new Binding(name, value, isVar = false, isGiven = true)

  /** Synthetic verification-compile shim for `evalSafe[T]` /
   *  `agentSafe[T]` calls. The eval driver's verification compile
   *  re-elaborates the user's `enclosingSource` with the body
   *  spliced into the marker position. For an `eval[T]` call the
   *  body's `T` matches the surrounding context; for `evalSafe[T]`
   *  the surrounding context expects `EvalResult[T]`, so
   *  [[EvalTypeAnnotate]] rewrites the marker as
   *  `Eval.handleCompileError(__evalBodyPlaceholder__)`. The
   *  verification compile then sees a tree of type
   *  `EvalResult[T]`, so the surrounding `val r =
   *  evalSafe[Int](...)` (or chained `.isFailure` / `.get` etc.)
   *  types as expected.
   *
   *  The actual evalSafe runtime path goes through the adapter and
   *  `Either`-to-`EvalResult` mapping in [[evalSafeImpl]]; this
   *  shim is never invoked at runtime — it exists purely so the
   *  verification compile's typechecker has something to call. The
   *  parameter is by-value (`v: T`) rather than by-name because we
   *  never run the body here; the verify pass only typechecks the
   *  spliced expression.
   */
  def handleCompileError[T](v: T): EvalResult[T] =
    EvalResult.success(v)

  /** Synthetic no-op call inserted by [[SpliceEvalBody.mkExprBlock]]
   *  between the spliced `val __evalResult` and the block's tail.
   *  Without an opaque side-effecting call there, dotty's optimiser
   *  is free to constant-fold the surrounding expression and erase
   *  the `val` site `ExtractEvalBody` later drains.
   */
  def __noFold__(): Unit = ()

  /** Compile-failure descriptor produced by the wrapper compile and
   *  carried back through the `Adapter` boundary. Surfaced to users
   *  as the failure side of [[EvalResult]] (and as the data behind
   *  [[EvalCompileException]] for the throwing `eval[T]` form).
   *
   *  Why a value type instead of just throwing the exception: making
   *  the failure a value lets `evalSafe` distinguish *its own* compile
   *  error (a `Left(CompileFailure)` from the adapter) from *a
   *  nested eval inside the body throwing at runtime* (a Java
   *  exception that propagates through). Without that split,
   *  `try { ... } catch case e: EvalCompileException => failure(e)`
   *  inside `evalSafe` would silently swallow nested-eval failures.
   *
   *  Lives in `dotty.tools.repl` so the eval-output classloader
   *  shares the `Class` with the REPL infra (see BetterEval.md
   *  "Classloader bridging").
   */
  final class CompileFailure(val errors: Array[String], val source: String):
    override def toString: String =
      s"CompileFailure(${errors.length} error(s))"

  /** Adapter installed by the running REPL driver. The `expectedType`
   *  argument is the source-level rendering of the type argument the
   *  caller wrote at the `eval[T](...)` call site. The empty string
   *  means the caller didn't pin a type, in which case the runtime
   *  uses `Any` as the wrapper's return type and relies on the call
   *  site's `asInstanceOf[T]` cast.
   *
   *  `enclosingSource` is the source text of the enclosing top-level
   *  statement at the eval call site, with this eval call's span
   *  replaced by `EvalContext.placeholder`. The runtime splices the
   *  body string back at that marker and type-checks the result
   *  inside the original lexical context, so identifiers resolve
   *  exactly as they would have at the call site. Empty when the
   *  rewriter couldn't compute a slice; the runtime falls back to a
   *  binding-only wrapper.
   *
   *  Returns either the body's value (`Right`) or a [[CompileFailure]]
   *  (`Left`) describing this call's own compile error. Body runtime
   *  exceptions (including a nested eval throwing
   *  [[EvalCompileException]]) propagate through as Java exceptions
   *  rather than being captured here, so callers can distinguish
   *  them from this call's compile state.
   */
  trait Adapter:
    def evalCode(
        code: String,
        bindings: Array[Binding],
        expectedType: String,
        enclosingSource: String
    ): Either[CompileFailure, Any]

  // Inheritable so a body that spawns a Future / Thread can still
  // reach the live adapter from the new thread (the snapshot at spawn
  // time is fine — there's exactly one adapter per running REPL).
  private val active = new InheritableThreadLocal[Adapter]

  def withAdapter[T](adapter: Adapter)(thunk: => T): T =
    val prev = active.get
    active.set(adapter)
    try thunk
    finally if prev == null then active.remove() else active.set(prev)

  /** Compile and run `code` against the current REPL session.
   *
   *  Two forms differ by the first argument:
   *
   *    - `eval(code: String, ...)`: the body is the literal/computed
   *      `code` string.
   *    - `eval(gen: EvalContext => String, ...)`: an agent/LLM-style
   *      generator that receives the call-site context (enclosing
   *      source, placeholder marker, captured bindings) and returns
   *      the body string. Scala 3 SAM conversion accepts a function
   *      literal here even though the parameter is
   *      `java.util.function.Function` (JDK type so the API surface
   *      crosses the eval / REPL classloader boundary cleanly — see
   *      BetterEval.md "Classloader bridging"; `scala.Function1` would trip
   *      the JVM's loader-constraint check with `LinkageError`).
   *
   *  Defaulted parameters are normally filled in by the parser-stage
   *  rewriter from the call site:
   *
   *    - `bindings`: every term-level name (lambda parameter,
   *      block-local val/var/def/given, method parameter) syntactically
   *      in scope at the call site.
   *    - `expectedType`: the source-level rendering of `T` from the
   *      explicit `eval[T](...)` type argument; empty when not
   *      writable.
   *    - `enclosingSource`: the source of the enclosing top-level
   *      statement with this call's location replaced by
   *      `EvalContext.placeholder`. The runtime splices the body in
   *      and type-checks under the original lexical context.
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

  // Closure form: convenience 1-arg overload + the full N-arg the
  // rewriter emits. We can't put defaults on these because Scala
  // forbids defaults on more than one overload of the same name —
  // the string form already owns them.

  def eval[T](gen: java.util.function.Function[EvalContext, String]): T =
    eval[T](gen, Array.empty[Binding], "", "")

  def eval[T](
      gen: java.util.function.Function[EvalContext, String],
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String
  ): T =
    val ctx = new EvalContext(enclosingSource, bindings)
    evalImpl[T](gen.apply(ctx), bindings, expectedType, enclosingSource)

  /** Non-throwing variant of [[eval]]. Returns [[EvalResult]] with
   *  the body's value on success or the [[EvalCompileException]] on a
   *  compile-time failure. Runtime exceptions thrown by the body
   *  itself still propagate (they aren't compile failures). Designed
   *  so an agent can feed `result.error.errors` back into a generator
   *  and retry without wrapping every call in `try`/`catch`.
   *
   *  Same two forms (string body, closure body) and same defaulted
   *  parameters as [[eval]].
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
    val ctx = new EvalContext(enclosingSource, bindings)
    evalSafeImpl[T](gen.apply(ctx), bindings, expectedType, enclosingSource)

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
    // Note: we do NOT catch `EvalCompileException` here. Body
    // runtime exceptions — which include a *nested* eval's
    // compile-time failure surfacing as `EvalCompileException` —
    // propagate to the caller. Only this call's own compile error
    // (delivered as `Left(CompileFailure)` from the adapter) becomes
    // an `EvalResult.failure`.
    activeAdapter().evalCode(code, bindings, expectedType, enclosingSource) match
      case Right(v) => EvalResult.success(v.asInstanceOf[T])
      case Left(f) => EvalResult.failure(f)

  private def activeAdapter(): Adapter =
    val a = active.get
    if a == null then
      throw new IllegalStateException(
        "eval(...) requires an active dotty REPL session"
      )
    a

end Eval
