package dotty.tools
package eval

/** Thrown by `eval[T]` when the generated wrapper does not compile. Typical
 *  causes include syntax errors, unresolved names, and a result that does not
 *  conform to the requested type.
 *
 *  The diagnostics and generated source remain available for programmatic
 *  inspection. `errors` uses an array because Scala collection classes cannot
 *  safely cross the REPL/eval classloader boundary. Exceptions raised while
 *  running a successfully compiled body propagate unchanged.
 */
final class EvalCompileException(
    val errors: Array[String],
    val generatedSource: String
) extends RuntimeException(EvalCompileException.formatMessage(errors, generatedSource))

object EvalCompileException:
  private def formatMessage(errors: Array[String], generatedSource: String): String =
    s"eval failed to compile:\n${errors.mkString("\n")}\n\nGenerated source:\n$generatedSource"
