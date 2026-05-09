package dotty.tools
package repl
package eval

/** The result of a non-throwing `evalSafe` call: either a successful
 *  value of type `T`, or an [[Eval.CompileFailure]] describing the
 *  compile-time failure of *this call* (not of any nested eval inside
 *  the body — those propagate as exceptions from `get` like any other
 *  body exception). Designed for agent / LLM workflows that want to
 *  feed the error text back into a generator and retry rather than
 *  handle a thrown exception.
 *
 *  Lives in `dotty.tools.repl` so the eval-output classloader routes
 *  it through the parent loader and there's a single shared `Class`
 *  on both sides of the eval / REPL boundary (see BetterEval.md
 *  "Classloader bridging"). We deliberately avoid `scala.Either` on
 *  the API surface because Scala-library types resolve to two
 *  distinct `Class` objects across that boundary, which trips the
 *  JVM's loader-constraint check with `LinkageError`.
 *
 *  ```
 *  Eval.evalSafe[Int](code) match
 *    case EvalResult.Success(v)  => use(v)
 *    case EvalResult.Failure(f)  => regenerate(f.errors)
 *  ```
 *
 *  The legacy method-style accessors (`isSuccess`, `get`, `error`,
 *  `getOrElse`) are retained so existing call sites continue to work.
 */
enum EvalResult[+T]:
  case Success(value: T)
  case Failure(failure: Eval.CompileFailure) extends EvalResult[Nothing]

  def isSuccess: Boolean = this match
    case _: Success[?] => true
    case _: Failure   => false

  def isFailure: Boolean = !isSuccess

  /** The body's return value on success, or throws an
   *  [[EvalCompileException]] constructed from the stored
   *  [[Eval.CompileFailure]] on failure (matches the throwing
   *  `eval[T]` form's behaviour).
   */
  def get: T = this match
    case Success(v) => v
    case Failure(f) => throw new EvalCompileException(f.errors, f.source)

  /** The compile-time failure on a failed result, or `null` on
   *  success. The returned [[Eval.CompileFailure]] carries the
   *  diagnostic strings (`errors`) and the synthesised source the
   *  eval driver was compiling (`source`).
   */
  def error: Eval.CompileFailure | Null = this match
    case _: Success[?] => null
    case Failure(f)    => f

  /** The body's return value on success, or `default` on failure. */
  def getOrElse[U >: T](default: U): U = this match
    case Success(v) => v
    case _: Failure => default

  override def toString: String = this match
    case Success(v) => s"EvalResult.Success($v)"
    case Failure(f) => s"EvalResult.Failure(${f.errors.length} error(s))"

object EvalResult:
  def success[T](value: T): EvalResult[T] = Success(value)
  def failure[T](error: Eval.CompileFailure): EvalResult[T] = Failure(error)
