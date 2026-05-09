package dotty.tools
package repl
package eval

/** Thrown by the throwing-form `eval[T]` when the inner compile of
 *  the synthesised wrapper module fails. Common causes: an unknown
 *  identifier in the body, a type mismatch when `eval[T]` pins the
 *  return type, a parse error.
 *
 *  Carries diagnostics and generated source as structured fields so
 *  callers can inspect them programmatically.
 *
 *  `errors` is `Array[String]` rather than `Seq[String]` to keep the
 *  API surface on JVM-intrinsic types — Scala-collection types
 *  resolve to two distinct `Class` objects across the eval / REPL
 *  classloader boundary and produce `LinkageError` when a user
 *  `catch` touches the field.
 *
 *  Body runtime exceptions (e.g. `eval("1 / 0")` raising
 *  `ArithmeticException`) propagate as the body's own exception,
 *  not as `EvalCompileException`.
 */
final class EvalCompileException(
    val errors: Array[String],
    val generatedSource: String
) extends RuntimeException(EvalCompileException.formatMessage(errors, generatedSource))

object EvalCompileException:
  private def formatMessage(errors: Array[String], generatedSource: String): String =
    val joined =
      val sb = new StringBuilder
      var i = 0
      while i < errors.length do
        if i > 0 then sb.append('\n')
        sb.append(errors(i))
        i += 1
      sb.toString
    s"eval failed to compile:\n$joined\n\nGenerated source:\n$generatedSource"
