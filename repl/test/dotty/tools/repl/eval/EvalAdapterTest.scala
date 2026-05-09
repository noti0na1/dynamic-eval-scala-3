package dotty.tools
package repl
package eval

import org.junit.Test
import org.junit.Assert.*

/** Integration smoke tests for [[EvalAdapter]] — the production-
 *  shaped entry point that takes the same inputs `Eval.evalIsolated`
 *  takes and routes them through the new eval pipeline.
 *
 *  These tests stand in for the future Phase 7 switchover: they verify
 *  the adapter handles the basic shapes the production REPL would
 *  exercise (no captures, simple captures, expectedType cast). Tests
 *  involving REPL-line wrappers and nested eval are deferred until
 *  the parser-stage rewriter's body pre-processing is wired into
 *  eval.
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
    val binding = new Eval.Binding("arg", java.lang.Integer.valueOf(40), false, false)
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
    val b1 = new Eval.Binding("arg", java.lang.Integer.valueOf(10), false, false)
    val b2 = new Eval.Binding("arg", java.lang.Integer.valueOf(20), false, false)
    assertEquals(Right(java.lang.Integer.valueOf(11)), call("arg + 1", enclosing, Array(b1)))
    val sizeAfterFirst = EvalAdapter.cache.size
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
    val second = call("\"hello\"", enclosing)
    assertTrue(second.isLeft)
    val sizeAfterSecond = EvalAdapter.cache.size
    assertEquals("repeated failures shouldn't grow the cache", sizeAfterFirst, sizeAfterSecond)
    EvalAdapter.clearCache()
