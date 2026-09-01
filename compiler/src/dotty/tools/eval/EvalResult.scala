package dotty.tools
package eval

/** Result of a non-throwing `evalSafe` call. A failure describes a compilation
 *  error in this call. Runtime exceptions, including failures from nested eval
 *  calls, continue to propagate from the body.
 *
 *  This enum avoids `scala.Either` because Scala-library types cannot safely
 *  cross the REPL/eval classloader boundary.
 *
 *  ```
 *  Eval.evalSafe[Int](code) match
 *    case EvalResult.Success(v)  => use(v)
 *    case EvalResult.Failure(f)  => regenerate(f.errors)
 *  ```
 */
enum EvalResult[+T]:
  case Success(value: T)
  case Failure(failure: Eval.CompileFailure) extends EvalResult[Nothing]

  def isSuccess: Boolean = this match
    case _: Success[?] => true
    case _: Failure   => false

  def isFailure: Boolean = !isSuccess

  /** Returns the value or throws [[EvalCompileException]] for the stored
   *  compilation failure, matching `eval[T]`.
   */
  def get: T = this match
    case Success(v) => v
    case Failure(f) => throw new EvalCompileException(f.errors, f.source)

  /** Returns the compilation failure, or `null` on success. */
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
