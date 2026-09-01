package dotty.tools
package eval

import org.junit.Assert.*
import org.junit.Test

import dotty.tools.repl.ReplTest

/** Focused regressions for compiler-side eval rewriting and extraction. */
class EvalCompilerHardeningTests extends ReplTest:

  private def assertContains(needle: String, haystack: String): Unit =
    assertTrue(s"expected to contain `$needle`, got:\n$haystack", haystack.contains(needle))

  @Test def bridgeIgnoresUserEvalResultValAfterSplice(): Unit =
    val enclosing =
      s"""def f(): Int =
         |  val actual = ({ ${EvalContext.placeholder} })
         |  val __evalResult = 99
         |  actual
         |""".stripMargin
    val result = new EvalAdapter().evalIsolated(
      code = "40 + 2",
      classLoader = classOf[EvalCompilerHardeningTests].getClassLoader,
      bindings = Array.empty,
      replOutDir = null,
      replWrapperImports = Array.empty,
      compilerSettings = Array.empty,
      expectedType = "Int",
      enclosingSource = enclosing
    )
    result match
      case Right(value) => assertEquals(42, value)
      case Left(failure) => fail(failure.errors.mkString("\n"))

  @Test def replIgnoresUserEvalResultValsOnBothSides = initially {
    run(
      """|def before(): Int =
         |  val __evalResult = 40
         |  eval[Int]("__evalResult + 2")
         |def after(): Int =
         |  val actual = eval[Int]("40 + 2")
         |  val __evalResult = 99
         |  actual
         |val results = (before(), after())
         |""".stripMargin
    )
    assertContains("val results: (Int, Int) = (42, 42)", storedOutput())
  }

  @Test def inlineBlockDoesNotCaptureLaterVals = initially {
    run(
      """|inline def around(inline op: Int): Int =
         |  val before = 1
         |  val middle = op
         |  val after = 2
         |  before + middle + after
         |""".stripMargin
    )
  } andThen {
    run("""val inlineResult = around(eval[Int]("40 + 2"))""")
    assertContains("val inlineResult: Int = 45", storedOutput())
  }

  @Test def privateOverloadsAreRejectedInsteadOfMisdispatched = initially {
    run(
      """|class PrivateOverloadOwner:
         |  private def choose(x: Int): String = s"int:$x"
         |  private def choose(x: String): String = s"string:$x"
         |  def result: String = eval[String]("choose(7) + \"/\" + choose(\"ok\")")
         |PrivateOverloadOwner().result
         |""".stripMargin
    )
    val out = storedOutput()
    assertContains("does not support overloaded private/protected method", out)
    assertContains("choose", out)
  }

  @Test def unrelatedPrivateOverloadsDoNotRejectEval = initially {
    run(
      """|class UnrelatedPrivateOverload:
         |  private def choose(x: Int): String = s"int:$x"
         |  private def choose(x: String): String = s"string:$x"
         |  def result: Int = eval[Int]("40 + 2")
         |val unrelatedOverloadResult = UnrelatedPrivateOverload().result
         |""".stripMargin
    )
    assertContains("val unrelatedOverloadResult: Int = 42", storedOutput())
  }

  @Test def privateMethodArgumentsUsePrimitiveWidening = initially {
    run(
      """|class PrivatePrimitiveWidening:
         |  private def addOne(x: Long): Long = x + 1L
         |  def result: Long = eval[Long]("addOne(41)")
         |val primitiveWideningResult = PrivatePrimitiveWidening().result
         |""".stripMargin
    )
    assertContains("val primitiveWideningResult: Long = 42", storedOutput())
  }

  @Test def byNameGivenKeepsThunkAndGivenMetadata = initially {
    run(
      """|var forces = 0
         |def next(): Int = { forces += 1; forces }
         |def use(using n: => Int): (Int, Int, Int) =
         |  eval[(Int, Int, Int)] { ctx =>
         |    if !ctx.bindings.exists(b => b.name == "n" && b.isGiven) then
         |      throw new AssertionError("missing given binding metadata")
         |    "(n, summon[Int], n)"
         |  }
         |val byNameResult = use(using next())
         |val byNameObserved = (byNameResult, forces)
         |""".stripMargin
    )
    assertContains("val byNameObserved: ((Int, Int, Int), Int) = ((1, 2, 3), 3)", storedOutput())
  }

  @Test def ordinaryGeneratorCanReadRuntimeBindingValue = initially {
    run(
      """|def inspectBinding(secret: Int): Int =
         |  eval[Int] { ctx =>
         |    val runtimeValue = ctx.bindings.find(_.name == "secret").get.value
         |    runtimeValue.asInstanceOf[Int].toString
         |  }
         |val observedBindingValue = inspectBinding(42)
         |""".stripMargin
    )
    assertContains("val observedBindingValue: Int = 42", storedOutput())
  }

  @Test def reportsPreExistingPlaceholderIdentifier = initially {
    run(
      """|def conflict(): Int =
         |  val __evalBodyPlaceholder__ = 1
         |  val kept = __evalBodyPlaceholder__
         |  eval[Int]("kept + 41")
         |""".stripMargin
    )
    val out = storedOutput()
    assertContains("reserved identifier", out)
    assertContains("__evalBodyPlaceholder__", out)
    assertContains("outside this eval call", out)
  }

  @Test def placeholderTextAndIdentifierInsideCallAreAllowed = initially {
    run(
      """|def noConflict(): Int =
         |  val markerText = "__evalBodyPlaceholder__"
         |  // __evalBodyPlaceholder__ in a comment is inert.
         |  val __evalBodyPlaceholder__ = "40 + 2"
         |  eval[Int](__evalBodyPlaceholder__)
         |val noConflictResult = noConflict()
         |""".stripMargin
    )
    assertContains("val noConflictResult: Int = 42", storedOutput())
  }

  @Test def nestedEvalRejectsAmbiguousMarkerText = initially {
    run(
      """|val nestedMarkerResult = eval[Int](
         |  "val markerText = \"__evalBodyPlaceholder__\"; eval[Int](\"40 + 2\")"
         |)
         |""".stripMargin
    )
    val out = storedOutput()
    assertContains("nested eval cannot compose its enclosing source", out)
    assertContains("reserved marker text", out)
  }

class EvalContextSafeModeTests extends ReplTest(
  ReplTest.defaultOptions ++ Array("-language:experimental.safe")
):
  @Test def safeGeneratorCanReadAndRenderBindingValueWithoutCast = initially {
    run(
      """|def inspectBindingSafe(secret: Int): Int =
         |  eval[Int] { ctx =>
         |    val binding = ctx.bindings.find(_.name == "secret").get
         |    assert(binding.toString.contains("42"))
         |    binding.value.toString
         |  }
         |val safeObservedBindingValue = inspectBindingSafe(42)
         |""".stripMargin
    )
    val out = storedOutput()
    assertTrue(s"expected safe mode to expose the binding without an unchecked cast, got:\n$out",
      out.contains("val safeObservedBindingValue: Int = 42"))
  }

  @Test def safeGeneratorStillRejectsUncheckedCastOfBindingValue = initially {
    run(
      """|def castBindingSafe(secret: Object): Int =
         |  eval[Int] { ctx =>
         |    ctx.bindings.find(_.name == "secret").get.value.asInstanceOf[Int].toString
         |  }
         |""".stripMargin
    )
    val out = storedOutput()
    assertTrue(s"expected safe mode to reject the unchecked cast, got:\n$out",
      out.contains("Cannot use asInstanceOf in safe mode"))
  }
