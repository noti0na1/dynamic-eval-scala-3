package dotty.tools
package eval

import org.junit.Test
import org.junit.Assert.*

/** Direct integration tests for [[EvalAdapter.evalIsolated]], the entry point
 *  used by `ReplDriver.evalDynamic`.
 *
 *  Covers the basic shapes (no captures, simple captures, compile
 *  failures) plus the wrapper cache's hit/eviction behavior. The
 *  REPL-session shapes (line wrappers, nested eval, captures of every
 *  kind) are covered end-to-end in [[DynamicEvalTests]].
 */
class EvalAdapterTest:

  private val testClassLoader: ClassLoader = classOf[EvalAdapterTest].getClassLoader

  private final class ReflectionProbe extends EvalExpressionBase(null, Array.empty):
    def evaluate(): Any = ()
    def invoke(obj: Object, name: String, args: Array[Object]): Any =
      __refl_call__(obj, name, args)

  private final class PrivateOverloads:
    private def choose(x: Int): String = s"int:$x"
    private def choose(x: String): String = s"string:$x"
    private def choose(x: java.lang.StringBuilder): String = s"builder:$x"

    // Keep every private overload reachable in ordinary bytecode as well as
    // through the reflection probe, so compiler optimization cannot elide it.
    def keepMethods(): Unit =
      choose(0)
      choose("")
      choose(java.lang.StringBuilder())
      ()

  private class ParentPrivateShadow:
    private def choose(x: Int): String = s"parent:$x"
    def keepParent(): Unit =
      choose(0)
      ()

  private final class ChildPrivateShadow extends ParentPrivateShadow:
    private def choose(x: Int): String = s"child:$x"
    def keepChild(): Unit =
      choose(0)
      ()

  private def call(
      code: String,
      enclosingSource: String,
      bindings: Array[Eval.Binding] = Array.empty,
      expectedType: String = "",
      compilerSettings: Array[String] = Array.empty
  ): Either[Eval.CompileFailure, Any] =
    new EvalAdapter().evalIsolated(
      code = code,
      classLoader = testClassLoader,
      bindings = bindings,
      replOutDir = null,
      replWrapperImports = Array.empty,
      compilerSettings = compilerSettings,
      expectedType = expectedType,
      enclosingSource = enclosingSource
    )

  @Test def evaluatesSimpleConstantBody(): Unit =
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    val r = call("1 + 2", enclosing)
    assertEquals(Right(java.lang.Integer.valueOf(3)), r)

  @Test def ambiguousDisplayMarkerIsNotReplaced(): Unit =
    val marker = EvalContext.placeholder
    val source = s"val text = \"$marker\"; val result = $marker"
    assertEquals(source, EvalAdapter.spliceBodyForDisplay(source, "40 + 2"))

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

  @Test def cachedCompileFailureDiagnosticsAreDefensiveCopies(): Unit =
    EvalAdapter.clearCache()
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    val first = call("\"hello\"", enclosing).left.toOption.get
    val expected = first.errors.clone()
    first.errors(0) = "mutated first result"

    val second = call("\"hello\"", enclosing).left.toOption.get
    assertEquals(expected.toSeq, second.errors.toSeq)
    second.errors(0) = "mutated cache hit"

    val third = call("\"hello\"", enclosing).left.toOption.get
    assertEquals(expected.toSeq, third.errors.toSeq)
    assertNotSame("each cache hit should own its diagnostics array", second.errors, third.errors)
    EvalAdapter.clearCache()

  @Test def invalidForwardedOptionReturnsSetupDiagnostic(): Unit =
    // Unknown options are configuration warnings in dotc. A known option
    // missing its required argument is a setup Diagnostic.Error.
    val option = "-source"
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    call("42", enclosing, compilerSettings = Array(option)) match
      case Left(failure) =>
        val rendered = failure.errors.mkString("\n")
        assertTrue(s"expected setup diagnostic to mention $option, got:\n$rendered",
          rendered.contains(option))
        assertTrue(s"expected the useful setup cause, got:\n$rendered",
          rendered.toLowerCase.contains("missing argument"))
      case Right(value) => fail(s"invalid compiler option unexpectedly evaluated to $value")

  @Test def privateReflectiveOverloadsDispatchByRuntimeArgumentType(): Unit =
    val target = PrivateOverloads()
    target.keepMethods()
    val probe = ReflectionProbe()
    assertEquals("int:7",
      probe.invoke(target, "choose", Array[Object](java.lang.Integer.valueOf(7))))
    assertEquals("string:ok", probe.invoke(target, "choose", Array[Object]("ok")))

  @Test def ambiguousPrivateReflectiveOverloadFailsClearly(): Unit =
    val target = PrivateOverloads()
    target.keepMethods()
    val probe = ReflectionProbe()
    val error = assertThrows(classOf[IllegalArgumentException], () =>
      probe.invoke(target, "choose", Array[Object](null.asInstanceOf[Object])))
    val message = error.getMessage
    assertTrue(s"expected an ambiguity diagnostic, got: $message",
      message != null && message.toLowerCase.contains("ambiguous"))
    assertTrue(s"expected overload signatures, got: $message",
      message.contains("java.lang.String") && message.contains("java.lang.StringBuilder"))

  @Test def subclassPrivateMethodCannotStealParentReflectiveCall(): Unit =
    val target = ChildPrivateShadow()
    target.keepParent()
    target.keepChild()
    val error = assertThrows(classOf[IllegalArgumentException], () =>
      ReflectionProbe().invoke(target, "choose", Array[Object](java.lang.Integer.valueOf(7))))
    val message = error.getMessage
    assertTrue(s"expected a cross-hierarchy ambiguity diagnostic, got: $message",
      message != null && message.contains("compatible declarations"))
    assertTrue(s"expected both declaring classes, got: $message",
      message.contains("ParentPrivateShadow") && message.contains("ChildPrivateShadow"))

  @Test def wrapperLoaderIsTheContextClassLoaderOnlyDuringEvaluation(): Unit =
    val enclosing = s"def f(): Boolean = ({ ${EvalContext.placeholder} })"
    val body =
      """class BodyLocal
        |val instance = new BodyLocal
        |val loaded = Thread.currentThread().getContextClassLoader.loadClass(instance.getClass.getName)
        |loaded eq instance.getClass
        |""".stripMargin
    val thread = Thread.currentThread()
    val saved = thread.getContextClassLoader
    assertEquals(Right(java.lang.Boolean.TRUE), call(body, enclosing))
    assertSame("the caller's context classloader should be restored", saved, thread.getContextClassLoader)

  @Test def wrapperContextClassLoaderIsRestoredWhenEvaluationThrows(): Unit =
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    val thread = Thread.currentThread()
    val saved = thread.getContextClassLoader
    val error = assertThrows(classOf[IllegalStateException], () =>
      call("throw new IllegalStateException(\"boom\")", enclosing))
    assertEquals("boom", error.getMessage)
    assertSame("the caller's context classloader should be restored", saved, thread.getContextClassLoader)

  @Test def evalContextAndAdapterReceiveOriginalBindingsOutsideSafeMode(): Unit =
    val secret = new Object:
      override def toString: String = "sensitive-value"
    val binding = new Eval.Binding("visible", secret, "Secret")
    val synthetic = new Eval.Binding("__hidden__", secret, isSynthetic = true)
    var received = Array.empty[Eval.Binding]
    val adapter = new Eval.Adapter:
      def evalCode(
          code: String,
          bindings: Array[Eval.Binding],
          expectedType: String,
          enclosingSource: String
      ): Either[Eval.CompileFailure, Any] =
        received = bindings
        Right(java.lang.Integer.valueOf(42))

    val result = Eval.withAdapter(adapter) {
      Eval.eval[Int](
        (ctx: EvalContext) =>
          assertEquals(1, ctx.bindings.length)
          val info = ctx.bindings(0)
          assertEquals("visible", info.name)
          assertEquals("Secret", info.tpe)
          assertSame("ordinary-mode generators should see the runtime value", secret, info.value)
          "ignored by the test adapter",
        Array(binding, synthetic),
        "Int",
        s"def f(): Int = ${EvalContext.placeholder}"
      )
    }
    assertEquals(42, result)
    assertSame(binding, received(0))
    assertSame(synthetic, received(1))
    assertTrue(binding.toString.contains("sensitive-value"))

  @Test def compileFailureErrorsExcludeWarnings(): Unit =
    val enclosing = s"def f(): Int = ({ ${EvalContext.placeholder} })"
    // The non-exhaustive match raises a (default-on) warning; the
    // unknown identifier raises the actual error. `CompileFailure.errors`
    // must carry only the error so callers can feed diagnostics back into
    // generators without unrelated lint noise.
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

class EvalInputPolicyTest:

  private final class RecordingAdapter extends Eval.Adapter:
    var evalCalls = 0
    var topLevelCalls = 0

    def evalCode(
        code: String,
        bindings: Array[Eval.Binding],
        expectedType: String,
        enclosingSource: String
    ): Either[Eval.CompileFailure, Any] =
      evalCalls += 1
      Right(code)

    override def compileTopLevel(
        defs: String,
        contextHeader: String
    ): Either[Eval.CompileFailure, Eval.TopLevel] =
      topLevelCalls += 1
      super.compileTopLevel(defs, contextHeader)

  private val policyMessage =
    "REPL commands are not allowed in dynamic evaluation; provide Scala code instead."

  private def assertPolicyFailure(result: EvalResult[?], source: String): Unit = result match
    case EvalResult.Failure(failure) =>
      assertEquals(List(policyMessage), failure.errors.toList)
      assertEquals(source, failure.source)
    case EvalResult.Success(value) => fail(s"expected command rejection, got $value")

  @Test def commandShapedEvalInputNeverReachesAdapter(): Unit =
    val adapter = RecordingAdapter()
    val commands = List(
      ":reset",
      ":reset\n42",
      ":he",
      "  :jar dependency.jar",
      "// leading context\n:dep org:name:version",
      "/* outer /* nested */ comment */\n:load session.scala",
      "#!/usr/bin/env scala\n:settings -Yexplicit-nulls",
      ":bogus"
    )
    Eval.withAdapter(adapter):
      commands.foreach(command => assertPolicyFailure(Eval.evalSafe[Any](command), command))
      val generated = Eval.evalSafe[Any]((_: EvalContext) => ":sh echo forbidden")
      assertPolicyFailure(generated, ":sh echo forbidden")
    assertEquals(0, adapter.evalCalls)

  @Test def commandShapedTopLevelInputNeverReachesAdapter(): Unit =
    val adapter = RecordingAdapter()
    Eval.withAdapter(adapter):
      assertPolicyFailure(Eval.topLevelSafe(":quit"), ":quit")
      val thrown = assertThrows(classOf[EvalCompileException], () => Eval.topLevel(":reset"))
      assertTrue(thrown.errors.contains(policyMessage))
    assertEquals(0, adapter.topLevelCalls)

  @Test def throwingEvalUsesTheSameCommandPolicy(): Unit =
    val adapter = RecordingAdapter()
    val thrown = Eval.withAdapter(adapter):
      assertThrows(classOf[EvalCompileException], () => Eval.eval[Any](":settings -Yexplicit-nulls"))
    assertTrue(thrown.errors.contains(policyMessage))
    assertEquals(0, adapter.evalCalls)

  @Test def scalaColonsAndCommentTextAreNotCommands(): Unit =
    val adapter = RecordingAdapter()
    val scalaInputs = List(
      "\":reset\"",
      "// :reset\n42",
      "//> using dep org:name:version\n42",
      "/* :quit */ 42",
      "List(1) :+ 2",
      "::",
      "val `:reset` = 1; `:reset`"
    )
    Eval.withAdapter(adapter):
      scalaInputs.foreach: input =>
        assertEquals(input, Eval.evalSafe[String](input).get)
    assertEquals(scalaInputs.length, adapter.evalCalls)
