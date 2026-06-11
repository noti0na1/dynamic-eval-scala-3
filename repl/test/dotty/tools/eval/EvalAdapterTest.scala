package dotty.tools
package eval

import org.junit.Test
import org.junit.Assert.*

/** Integration smoke tests for [[EvalAdapter.evalIsolated]] — the
 *  entry point `ReplDriver.evalDynamic` drives at runtime — exercised
 *  directly, without a live REPL session.
 *
 *  Covers the basic shapes (no captures, simple captures, compile
 *  failures) plus the wrapper cache's hit/eviction behaviour. The
 *  REPL-session shapes (line wrappers, nested eval, captures of every
 *  kind) are covered end-to-end in [[DynamicEvalTests]].
 */
class EvalAdapterTest:

  private val testClassLoader: ClassLoader = classOf[EvalAdapterTest].getClassLoader

  private def call(
      code: String,
      enclosingSource: String,
      bindings: Array[Eval.Binding] = Array.empty,
      expectedType: String = ""
  ): Either[Eval.CompileFailure, Any] =
    new EvalAdapter().evalIsolated(
      code = code,
      classLoader = testClassLoader,
      bindings = bindings,
      replOutDir = null,
      replWrapperImports = Array.empty,
      compilerSettings = Array.empty,
      expectedType = expectedType,
      enclosingSource = enclosingSource
    )

  @Test def evaluatesSimpleConstantBody(): Unit =
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    val r = call("1 + 2", enclosing)
    assertEquals(Right(java.lang.Integer.valueOf(3)), r)

  @Test def evaluatesBodyWithCapturedLocal(): Unit =
    val enclosing = s"def f(arg: Int): Int = ({ ${EvalContext.placeholder} })"
    val binding = new Eval.Binding("arg", java.lang.Integer.valueOf(40))
    val r = call("arg + 2", enclosing, Array(binding))
    assertEquals(Right(java.lang.Integer.valueOf(42)), r)

  @Test def evaluatesBlockWithBodyLocalDef(): Unit =
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    val body =
      """def double(x: Int) = x * 2
        |double(7)
        |""".stripMargin
    val r = call(body, enclosing)
    assertEquals(Right(java.lang.Integer.valueOf(14)), r)

  @Test def returnsCompileFailureOnSyntaxError(): Unit =
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    val r = call("this is not scala", enclosing)
    r match
      case Left(_) => () // expected
      case Right(v) => fail(s"expected compile failure, got Right($v)")

  @Test def returnsCompileFailureOnTypeError(): Unit =
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    val r = call("\"hello\"", enclosing)
    r match
      case Left(_) => () // String → Int mismatch
      case Right(v) => fail(s"expected compile failure, got Right($v)")

  @Test def cachesCompiledClassAcrossIdenticalCalls(): Unit =
    EvalAdapter.clearCache()
    val enclosing = s"def f(arg: Int): Int = ({ ${EvalContext.placeholder} })"
    val b1 = new Eval.Binding("arg", java.lang.Integer.valueOf(10))
    val b2 = new Eval.Binding("arg", java.lang.Integer.valueOf(20))
    assertEquals(Right(java.lang.Integer.valueOf(11)), call("arg + 1", enclosing, Array(b1)))
    val sizeAfterFirst = EvalAdapter.cache.size
    assertEquals("first call should add exactly one cache entry", 1, sizeAfterFirst)
    assertEquals(Right(java.lang.Integer.valueOf(21)), call("arg + 1", enclosing, Array(b2)))
    val sizeAfterSecond = EvalAdapter.cache.size
    assertEquals("second call should hit the cache, not add a new entry", sizeAfterFirst, sizeAfterSecond)
    EvalAdapter.clearCache()
    assertEquals(0, EvalAdapter.cache.size)

  @Test def cachesCompileFailureSoRetriesDontRecompile(): Unit =
    EvalAdapter.clearCache()
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    val first = call("\"hello\"", enclosing)
    assertTrue(first.isLeft)
    val sizeAfterFirst = EvalAdapter.cache.size
    assertEquals("the failure itself should be cached", 1, sizeAfterFirst)
    val second = call("\"hello\"", enclosing)
    assertTrue(second.isLeft)
    val sizeAfterSecond = EvalAdapter.cache.size
    assertEquals("repeated failures shouldn't grow the cache", sizeAfterFirst, sizeAfterSecond)
    EvalAdapter.clearCache()

  @Test def compileFailureErrorsExcludeWarnings(): Unit =
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    // The non-exhaustive match raises a (default-on) warning; the
    // unknown identifier raises the actual error. `CompileFailure.errors`
    // must carry only the error — agent retry loops feed it back into
    // generators, and lint noise about unrelated code derails them.
    val body =
      """val m = (None: Option[Int]) match { case Some(x) => x }
        |undefinedName
        |""".stripMargin
    call(body, enclosing) match
      case Left(failure) =>
        val rendered = failure.errors.mkString("\n")
        assertTrue("expected at least one error", failure.errors.nonEmpty)
        assertTrue(s"expected the unknown-identifier error, got:\n$rendered",
          rendered.contains("undefinedName"))
        assertFalse(s"expected warnings to be filtered out, got:\n$rendered",
          rendered.toLowerCase.contains("exhaustive"))
      case Right(v) => fail(s"expected compile failure, got Right($v)")
