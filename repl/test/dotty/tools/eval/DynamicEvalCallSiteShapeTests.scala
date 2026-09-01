package dotty.tools
package eval

import org.junit.Test
import org.junit.Assert.*

import dotty.tools.repl.ReplTest

import DynamicEvalAssertions.*

/** Call-site shapes around the eval call itself: enclosing code that refers
 *  to the instance or its private members, constructor-argument positions,
 *  enum bodies, anonymous given instances, and user wrappers that take the
 *  built-in name.
 *
 *  The class and module lifts in `SpliceEvalBody` move the marker-bearing
 *  member out of its declaration, so the code *around* the eval call must
 *  survive the same `this` and private-member rewrite as the body does.
 */
class DynamicEvalCallSiteShapeTests extends ReplTest:

  // ===========================================================================
  // 1. Enclosing code around the call refers to the instance.
  // ===========================================================================

  @Test def enclosingCodeReadsPrivateConstructorParam = initially {
    run(
      """|class Scaled(factor: Int):
         |  def apply(xs: List[Int]): List[Int] =
         |    xs.map(x => x * factor).map(y => eval[Int]("y + factor"))
         |val scaled = Scaled(10)(List(1, 2))
         |""".stripMargin)
    assertContains("val scaled: List[Int] = List(20, 30)", storedOutput())
  }

  @Test def enclosingCodeReadsPrivateMemberBeforeEval = initially {
    run(
      """|class Counter:
         |  private var count = 40
         |  def next(): Int =
         |    count = count + 1
         |    val snapshot = count
         |    eval[Int]("snapshot + 1")
         |val counted = Counter().next()
         |""".stripMargin)
    assertContains("val counted: Int = 42", storedOutput())
  }

  @Test def enclosingCodeSelectsPrivateMemberThroughThis = initially {
    run(
      """|class Holder:
         |  private val base = 40
         |  def compute: Int =
         |    val doubled = this.base + this.base
         |    eval[Int]("doubled - base + 2")
         |val held = Holder().compute
         |""".stripMargin)
    assertContains("val held: Int = 42", storedOutput())
  }

  @Test def enclosingCodeSelectsPublicMemberThroughThis = initially {
    run(
      """|class Pair(val a: Int, val b: Int):
         |  def sum: Int =
         |    val total = this.a + this.b
         |    eval[Int]("total")
         |val summed = Pair(40, 2).sum
         |""".stripMargin)
    assertContains("val summed: Int = 42", storedOutput())
  }

  @Test def enclosingCodeInObjectReadsPrivateMember = initially {
    run(
      """|object Registry:
         |  private val entries = List(1, 2, 3)
         |  def total: Int =
         |    val n = entries.size
         |    eval[Int]("n * 14")
         |val registered = Registry.total
         |""".stripMargin)
    assertContains("val registered: Int = 42", storedOutput())
  }

  @Test def bodyReadsPrivateMemberWithInferredType = initially {
    // The member's declared type is inferred from a non-literal initializer,
    // so the reflective read is typed through a copy of that initializer.
    run(
      """|class Inventory:
         |  private val entries = List(1, 2, 3)
         |  private val doubled = entries.map(_ * 2)
         |  def total: Int = eval[Int]("entries.sum + doubled.sum + 24")
         |val inventoried = Inventory().total
         |""".stripMargin)
    assertContains("val inventoried: Int = 42", storedOutput())
  }

  @Test def enclosingCodeCallsPrivateMethod = initially {
    run(
      """|class Machine:
         |  private def step(i: Int): Int = i + 1
         |  def run(start: Int): Int =
         |    val once = step(start)
         |    eval[Int]("step(once)")
         |val ran = Machine().run(40)
         |""".stripMargin)
    assertContains("val ran: Int = 42", storedOutput())
  }

  @Test def enclosingForComprehensionOverPrivateParam = initially {
    run(
      """|class Table(rows: Map[String, Int]):
         |  def widths: List[Int] =
         |    for (k, v) <- rows.toList yield eval[Int]("v + k.length")
         |val widths = Table(Map("ab" -> 40)).widths
         |""".stripMargin)
    assertContains("val widths: List[Int] = List(42)", storedOutput())
  }

  @Test def enclosingCodeLocalShadowsPrivateMember = initially {
    // A local `base` declared around the call shadows the private member of
    // the same name, so the enclosing read must not be rerouted to the field.
    run(
      """|class Shadow:
         |  private val base = 1
         |  def compute: Int =
         |    val base = 40
         |    val next = base + 1
         |    eval[Int]("next + 1")
         |val shadowed = Shadow().compute
         |""".stripMargin)
    assertContains("val shadowed: Int = 42", storedOutput())
  }

  @Test def compoundAssignmentToPrivateVarInBody = initially {
    run(
      """|class Tally:
         |  private var n = 0
         |  def bump(): Int =
         |    eval[Unit]("n += 21")
         |    n
         |val tallied = { val t = Tally(); t.bump(); t.bump() }
         |""".stripMargin)
    assertContains("val tallied: Int = 42", storedOutput())
  }

  @Test def compoundAssignmentToPrivateVarInEnclosingCode = initially {
    run(
      """|class Ticker:
         |  private var ticks = 40
         |  def tick(): Int =
         |    ticks += 1
         |    eval[Int]("ticks + 1")
         |val ticked = Ticker().tick()
         |""".stripMargin)
    assertContains("val ticked: Int = 42", storedOutput())
  }

  // ===========================================================================
  // 2. Constructor-argument positions.
  // ===========================================================================

  @Test def evalInParentConstructorArgument = initially {
    run(
      """|class Base(val v: Int)
         |class Derived extends Base(eval[Int]("41 + 1"))
         |val derived = Derived().v
         |""".stripMargin)
    assertContains("val derived: Int = 42", storedOutput())
  }

  @Test def evalInSecondaryConstructorArgument = initially {
    run(
      """|class Sec(val a: Int):
         |  def this() = this(eval[Int]("41") + 1)
         |val sec = new Sec().a
         |""".stripMargin)
    assertContains("val sec: Int = 42", storedOutput())
  }

  @Test def bodyInParentArgumentCannotNameTheInstance = initially {
    // The instance under construction is not capturable in a parent argument,
    // so a body naming a member is rejected with a diagnostic rather than
    // running against a null receiver.
    run(
      """|class Base(val v: Int)
         |class Bad(k: Int) extends Base(eval[Int]("k"))
         |val bad = scala.util.Try(Bad(1).v).failed.get.getMessage
         |""".stripMargin)
    assertContains("instance under construction", storedOutput())
  }

  // ===========================================================================
  // 3. Declarations the lift must name correctly.
  // ===========================================================================

  @Test def enumMethodSeesCasesAndOrdinal = initially {
    run(
      """|enum Op:
         |  case Add, Mul
         |  def run(a: Int, b: Int): Int = this match
         |    case Add => eval[Int]("a + b + ordinal")
         |    case Mul => eval[Int]("a * b + ordinal")
         |val ops = (Op.Add.run(20, 22), Op.Mul.run(6, 7))
         |""".stripMargin)
    assertContains("val ops: (Int, Int) = (42, 43)", storedOutput())
  }

  @Test def evalInsideAnonymousGivenInstance = initially {
    run(
      """|trait Show[T] { def show(t: T): String }
         |given Show[Int] with
         |  def show(t: Int): String = eval[String]("\"n=\" + t")
         |val shown = summon[Show[Int]].show(42)
         |""".stripMargin)
    assertContains("""val shown: String = "n=42"""", storedOutput())
  }

  @Test def evalLikeWrapperNamedEvalIsRewritten = initially {
    // A user wrapper that shadows the built-in name is classified by its
    // annotation, not by the name it takes.
    run(
      """|import dotty.tools.eval.{Eval, evalLike}
         |object MyEval:
         |  @evalLike def eval[T](code: String, bindings: Array[Eval.Binding] = Array.empty,
         |      expectedType: String = "", enclosingSource: String = ""): T =
         |    Eval.eval[T](code, bindings, expectedType, enclosingSource)
         |import MyEval.eval
         |def twice(k: Int): Int = eval[Int]("k * 2")
         |val twiced = twice(21)
         |""".stripMargin)
    assertContains("val twiced: Int = 42", storedOutput())
  }

  // ===========================================================================
  // 4. Documented limitation: a constructor parameter that never becomes a field.
  // ===========================================================================

  @Test def constructorParamWithoutFieldReportsLimitation = initially {
    run(
      """|class Scaler(factor: Int):
         |  val scale: Int => Int = i => eval[Int]("i * factor")
         |val scaleError = scala.util.Try(Scaler(2).scale(21)).failed.get.getMessage
         |""".stripMargin)
    assertContains("not stored as a field", storedOutput())
  }

  // ===========================================================================
  // 5. Session inputs are reused across calls and refreshed per line.
  // ===========================================================================

  @Test def repeatedCallsThenNewSessionLine = initially {
    val state = run("""val firsts = (1 to 3).toList.map(i => eval[Int]("i * 2"))""")
    assertContains("val firsts: List[Int] = List(2, 4, 6)", storedOutput())
    state
  } andThen {
    val state = run("val bonus = 40")
    storedOutput()
    run("""val seconds = (1 to 2).toList.map(i => eval[Int]("i + bonus"))""")(using state)
    assertContains("val seconds: List[Int] = List(41, 42)", storedOutput())
  }
