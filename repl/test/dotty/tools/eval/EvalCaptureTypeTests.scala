package dotty.tools
package eval

import org.junit.Assert.*
import org.junit.Test

import dotty.tools.repl.{ReplTest, State}

/** Eval under capture checking: the rewriter's `expectedType`
 *  rendering must keep capture annotations (`^`, `^{...}`) when the
 *  session compiles with capture checking on, or a body whose type
 *  legitimately captures — `eval[F^{f}]("f")` — is refused by its own
 *  wrapper ascription, which would demand the pure base type.
 *
 *  The session runs with `-language:experimental.captureChecking` so
 *  `Feature.ccEnabled` holds by setting in every compile, including
 *  the runtime wrapper compiles the adapter spawns.
 */
class EvalCaptureTypeTests extends ReplTest(ReplTest.defaultOptions :+ "-language:experimental.captureChecking"):

  private def expectSteps(lines: String*)(expected: String*)(using st0: State): Unit =
    var st = st0
    val sb = new StringBuilder
    for line <- lines do
      st = run(line)(using st)
      sb ++= storedOutput()
    val out = sb.toString
    for e <- expected do
      assertTrue(s"expected `$e` in:\n$out", out.contains(e))

  @Test def capturingExpectedTypeSurvivesRendering = initially {
    // `use`'s eval carries `[F^{f}]`: the wrapper must ascribe the
    // body to exactly that capturing type — rendered `F` (stripped),
    // `f: F^{f}` fails to conform and the eval throws instead.
    expectSteps(
      """trait F extends caps.SharedCapability""",
      """def use(f: F^): F^{f} = eval[F^{f}]("f")""",
      """val ok = { val ff = new F {}; use(ff) eq ff }""")(
      "val ok: Boolean = true")
  }

  @Test def pureExpectedTypeStillRenders = initially {
    // The other direction: a plain type under a cc session renders
    // and ascribes as before.
    expectSteps(
      """def twice(n: Int): Int = eval[Int]("n * 2")""",
      """val r = twice(21)""")(
      "val r: Int = 42")
  }
