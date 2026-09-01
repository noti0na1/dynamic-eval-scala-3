package dotty.tools
package eval

import dotty.tools.io.virtualDirectory
import dotty.tools.repl.{AbstractFileClassLoader, ReplTest}

import java.net.URLClassLoader

import org.junit.Assert.{assertNotSame, assertSame, assertTrue}
import org.junit.Test

class DynamicEvalLiveSettingsTests extends ReplTest:

  @Test def dynamicEvalRejectsReplCommandsWithoutChangingSession(): Unit = initially {
    run("val commandPolicySentinel = 42")
  }.andThen {
    storedOutput()
    run(
      """|val commandErrors = List(
         |  ":reset", ":jar missing.jar", ":dep org:name:version", ":load missing.scala"
         |).map(command => evalSafe[Any](command).error.errors.head)
         |val topLevelCommandError = topLevelSafe(":quit").error.errors.head
         |val commandPolicyResult = commandPolicySentinel
         |""".stripMargin)
    val out = storedOutput()
    assertTrue(s"expected stable command-policy failures, got:\n$out",
      out.contains("REPL commands are not allowed in dynamic evaluation"))
    assertTrue(s"expected the live session to remain intact, got:\n$out",
      out.contains("val commandPolicyResult: Int = 42"))
  }

  @Test def settingsChangeRecompilesCachedCallSite(): Unit = initially {
    run("""def dynamicNull = evalSafe[String]("null")""")
  }.andThen {
    val before = run("dynamicNull.isSuccess")
    val out = storedOutput()
    assertTrue(out, out.contains("true"))
    run(":settings -Yexplicit-nulls")(using before)
  }.andThen {
    storedOutput()
    run("dynamicNull.isFailure")
    val out = storedOutput()
    assertTrue(s"expected the cached call site to be recompiled with explicit nulls, got:\n$out",
      out.contains("true"))
  }

  @Test def resetSettingsReachEvalCompiler(): Unit = initially {
    run(":reset -Yexplicit-nulls")
  }.andThen {
    storedOutput()
    run("""val r = evalSafe[String]("null")""")
    val out = storedOutput()
    assertTrue(s"expected explicit-nulls compile failure after reset, got:\n$out",
      out.contains("EvalResult.Failure"))
  }

  @Test def resetSettingsReachTopLevelCompiler(): Unit = initially {
    run(":reset -Yexplicit-nulls")
  }.andThen {
    storedOutput()
    run("""val h = topLevelSafe("val s: String = null")""")
    val out = storedOutput()
    assertTrue(s"expected explicit-nulls failure in topLevel defs, got:\n$out",
      out.contains("EvalResult.Failure"))
  }

  @Test def rejectedSettingsAreNotForwardedToEval(): Unit = initially {
    run(":settings -Yexplicit-nulls")
  }.andThen {
    storedOutput()
    val state = run(":settings -source")
    storedOutput()
    state
  }.andThen {
    storedOutput()
    run(
      """|val rejectedSettingsResult = evalSafe[String]("null")
         |val rejectedSettingsErrors = rejectedSettingsResult.error.nn.errors.mkString("\n")
         |val retainedExplicitNulls =
         |  rejectedSettingsErrors.contains("Null") &&
         |    !rejectedSettingsErrors.toLowerCase.contains("missing argument")
         |""".stripMargin)
    val out = storedOutput()
    assertTrue(s"expected eval to retain the previous valid setting, got:\n$out",
      out.contains("val retainedExplicitNulls: Boolean = true"))
  }

  @Test def rejectedResetSettingsAreNotForwardedToEval(): Unit = initially {
    run(":reset -source")
  }.andThen {
    storedOutput()
    run("""val r: Int = eval[Int]("40 + 2")""")
    val out = storedOutput()
    assertTrue(s"expected eval not to inherit the rejected reset option, got:\n$out",
      out.contains("val r: Int = 42"))
  }

class DynamicEvalDisabledInstrumentationTests extends ReplTest(
  ReplTest.defaultOptions :+ "-Xrepl-interrupt-instrumentation:false"
):
  @Test def evalSharesSessionClasses(): Unit = initially {
    run("val n = 41")
  }.andThen {
    storedOutput()
    run("""val r: Int = eval[Int]("n + 1")""")
    val out = storedOutput()
    assertTrue(s"expected eval to use the live session adapter, got:\n$out",
      out.contains("val r: Int = 42"))
  }

class DynamicEvalLocalInstrumentationTests extends ReplTest(
  ReplTest.defaultOptions :+ "-Xrepl-interrupt-instrumentation:local"
):
  @Test def evalSharesSessionClasses(): Unit = initially {
    run("val n = 41")
  }.andThen {
    storedOutput()
    run("""val r: Int = eval[Int]("n + 1")""")
    val out = storedOutput()
    assertTrue(s"expected eval to use the live session adapter, got:\n$out",
      out.contains("val r: Int = 42"))
  }

class DynamicEvalClassLoaderIdentityTests:
  import AbstractFileClassLoader.InterruptInstrumentation

  private val parent = classOf[AbstractFileClassLoader].getClassLoader

  private def loader(mode: InterruptInstrumentation): AbstractFileClassLoader =
    new AbstractFileClassLoader(virtualDirectory(s"<classloader-$mode>"), parent, mode)

  @Test def enabledModeKeepsStopFlagsPerSession(): Unit =
    val first = loader(InterruptInstrumentation.Enabled).loadClass("dotty.tools.repl.StopRepl")
    val second = loader(InterruptInstrumentation.Enabled).loadClass("dotty.tools.repl.StopRepl")
    assertNotSame(first, second)

  @Test def enabledLoaderChainSharesOneSessionStopFlag(): Unit =
    val firstLoader = loader(InterruptInstrumentation.Enabled)
    val classpathLayer = new URLClassLoader(Array.empty, firstLoader)
    val chained = new AbstractFileClassLoader(
      virtualDirectory("<classloader-chain>"), classpathLayer,
      InterruptInstrumentation.Enabled, firstLoader)
    try
      assertSame(
        firstLoader.loadClass("dotty.tools.repl.StopRepl"),
        chained.loadClass("dotty.tools.repl.StopRepl")
      )
    finally classpathLayer.close()

  @Test def disabledAndLocalModesShareInfrastructureClasses(): Unit =
    val expectedStop = parent.loadClass("dotty.tools.repl.StopRepl")
    val expectedEval = parent.loadClass("dotty.tools.eval.Eval$")
    for mode <- List(InterruptInstrumentation.Disabled, InterruptInstrumentation.Local) do
      val current = loader(mode)
      assertSame(expectedStop, current.loadClass("dotty.tools.repl.StopRepl"))
      assertSame(expectedEval, current.loadClass("dotty.tools.eval.Eval$"))
