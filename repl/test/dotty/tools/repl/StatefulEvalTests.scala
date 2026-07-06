package dotty.tools
package repl

import org.junit.Assert.*
import org.junit.Test

/** End-to-end tests for the STATEFUL-EVAL pattern built on the dynamic-eval
 *  machinery (see StatefulEval.scala at the repo root): a `State` value
 *  captures a scope through the `@evalLike` call-site rewrite, `state.eval`
 *  extends that scope step by step, and `state.end[T]` computes a typed
 *  result inside it.
 *
 *  The `State` layer itself is ordinary user code; what these tests pin down
 *  is the eval behaviour it relies on:
 *    - `val`/`var`/`given` bindings carried across steps
 *    - `def`s carried across steps as eta-expanded captures — including
 *      recursive defs, generic defs, and defs calling earlier steps' defs
 *      (`ExtractEvalBody.isOuterMethodLocalDef` accepts any *term*-owned
 *      outer def: an eval-spliced step's defs are owned by a `val`, not a
 *      method)
 *    - redefinition shadowing between steps
 *    - compile failures surfacing as `EvalResult.Failure` values
 *
 *  The layer is defined as REPL input (not test source) because the
 *  `@evalLike` rewrite applies to REPL/eval-compiled code.
 */
class StatefulEvalTests extends ReplTest:

  private val stateLayer =
    """import dotty.tools.eval.{Eval, EvalContext, EvalResult, evalLike, evalSafeLike}
      |case class State(bindings: Array[Eval.Binding], expectedType: String, enclosingSource: String):
      |  private def assignToResult(code: String): String =
      |    code.replace(EvalContext.placeholder, s"val __evalEnd = ${EvalContext.placeholder}\n(??? : State)")
      |  def eval(code: String): State =
      |    Eval.eval[State](s"$code\ngetState()", bindings, expectedType, enclosingSource)
      |  @evalLike
      |  def end[T](code: String, bindings: Array[Eval.Binding] = Array.empty[Eval.Binding], expectedType: String = "", enclosingSource: String = ""): T =
      |    Eval.eval[T](code, this.bindings, expectedType, assignToResult(this.enclosingSource))
      |  @evalSafeLike
      |  def endSafe[T](code: String, bindings: Array[Eval.Binding] = Array.empty[Eval.Binding], expectedType: String = "", enclosingSource: String = ""): EvalResult[T] =
      |    Eval.evalSafe[T](code, this.bindings, expectedType, assignToResult(this.enclosingSource))
      |@evalLike
      |def getState(bindings: Array[Eval.Binding] = Array.empty[Eval.Binding], expectedType: String = "", enclosingSource: String = ""): State =
      |  State(bindings, expectedType, enclosingSource)
      |def freshState: State = State(Array.empty[Eval.Binding], "Any", EvalContext.placeholder)
      |""".stripMargin

  /** Run `op` in a session that has the State layer defined. */
  private def withStateLayer[A](op: State ?=> A): A = initially {
    val st = run(stateLayer)
    storedOutput() // discard the definition echoes
    op(using st)
  }

  private def expect(expr: String, expected: String)(using State): Unit =
    run(expr)
    val out = storedOutput()
    assertTrue(s"expected `$expected` in:\n$out", out.contains(expected))

  // ---- values across steps --------------------------------------------------

  @Test def valsAcrossSteps = withStateLayer {
    expect("""freshState.eval("val x = 10").eval("val y = x + 5").end[Int]("x + y")""",
      "Int = 25")
  }

  @Test def varMutatedAcrossSteps = withStateLayer {
    expect("""freshState.eval("var c = 1").eval("c += 41").end[Int]("c")""",
      "Int = 42")
  }

  @Test def givenAcrossSteps = withStateLayer {
    expect("""freshState.eval("given Int = 42").end[Int]("summon[Int]")""",
      "Int = 42")
  }

  @Test def redefinitionShadows = withStateLayer {
    expect("""freshState.eval("val v = 1").eval("val v = 2").end[Int]("v")""",
      "Int = 2")
  }

  // ---- defs across steps ----------------------------------------------------

  @Test def defAcrossSteps = withStateLayer {
    expect("""freshState.eval("def g(n: Int) = n * 2").end[Int]("g(21)")""",
      "Int = 42")
  }

  @Test def defCallsEarlierStepsDef = withStateLayer {
    expect("""freshState.eval("def base = 40").eval("def total = base + 2").end[Int]("total")""",
      "Int = 42")
  }

  @Test def recursiveDefAcrossSteps = withStateLayer {
    expect("""freshState.eval("def fact(n: Int): Int = if n <= 1 then 1 else n * fact(n - 1)").end[Int]("fact(5)")""",
      "Int = 120")
  }

  @Test def genericDefAcrossSteps = withStateLayer {
    expect("""freshState.eval("def pick[T](a: T, b: T): T = b").end[String]("pick(\"x\", \"y\")")""",
      """String = "y"""")
  }

  @Test def defOverEarlierValAcrossSteps = withStateLayer {
    expect("""freshState.eval("val k = 10\ndef mul(n: Int) = n * k").end[Int]("mul(4)")""",
      "Int = 40")
  }

  // ---- typed results and failure shape --------------------------------------

  @Test def typedEndResult = withStateLayer {
    expect("""freshState.end[String]("\"ab\" * 2")""",
      """String = "abab"""")
  }

  @Test def compileFailureIsAValue = withStateLayer {
    expect("""freshState.endSafe[Int]("undefinedName").isSuccess""",
      "Boolean = false")
  }
