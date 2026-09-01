package dotty.tools
package eval

import org.junit.Test
import org.junit.Assert.*

import java.net.URLClassLoader
import java.nio.file.{Files, Path}

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.{Diagnostic, StoreReporter}

/** End-to-end tests for dynamic `eval` in *regular Scala programs*
 *  (no REPL): a complete source file is compiled with the main
 *  `dotc` pipeline under `-Xdynamic-eval`, the resulting classes are
 *  loaded, and a `run()` entry point is invoked. The `eval(...)`
 *  calls inside execute through [[StandaloneAdapter]] (no driver
 *  installs an [[Eval.Adapter]] in this path).
 *
 *  Mirrors the behavior axes of [[DynamicEvalTests]] on the
 *  standalone envelope:
 *    - basic bodies and expected types
 *    - captures: locals, lambda params, method params, vars, givens
 *    - live module state through the compile-time
 *      `import <Obj>.{given, *}` (the standalone analogue of the
 *      REPL's session imports)
 *    - top-level definitions (`<file>$package`), named packages
 *    - class members via the standalone class-method lift
 *    - nested eval, compile-error reporting, flag gating
 */
class StandaloneEvalTests:

  /** sbt runs tests with the full module classpath in
   *  `java.class.path`, so the compiled program sees the compiler,
   *  the eval runtime, and the standard library.
   */
  private val testClassPath: String = System.getProperty("java.class.path")

  /** Compile `source` as one file with `-Xdynamic-eval`, load the
   *  resulting classes (parented by the test classloader so
   *  `dotty.tools.eval.*` resolves to the same `Class` objects), and
   *  invoke `run()` on the module named `objectName`.
   */
  private def compileAndRun(source: String, objectName: String = "Main", extraFlags: Array[String] = Array.empty): Any =
    val outDir = compileOrFail(source, extraFlags)
    invokeRun(outDir, objectName)

  private def compileOrFail(source: String, extraFlags: Array[String] = Array.empty): Path =
    val (outDir, errors) = compile(source, extraFlags)
    assertTrue(s"program failed to compile:\n${errors.mkString("\n")}", errors.isEmpty)
    outDir

  /** Compile and return the output dir plus any error diagnostics. */
  private def compile(source: String, extraFlags: Array[String] = Array.empty): (Path, List[String]) =
    val outDir = Files.createTempDirectory("standalone-eval-test-")
    val srcFile = Files.createTempFile("standalone-eval-", ".scala")
    Files.writeString(srcFile, source)
    val args = Array(
      "-d", outDir.toString,
      "-classpath", testClassPath,
      "-Xdynamic-eval"
    ) ++ extraFlags :+ srcFile.toString
    val reporter = new StoreReporter(null)
    val driver = new Driver {}
    driver.process(args, reporter)
    val errors = reporter.removeBufferedMessages(using dotc.core.Contexts.NoContext)
      .collect { case e: Diagnostic.Error => e.message }
    (outDir, errors)

  private def invokeRun(outDir: Path, objectName: String): Any =
    val loader = new URLClassLoader(Array(outDir.toUri.toURL), classOf[StandaloneEvalTests].getClassLoader)
    val moduleClass = loader.loadClass(objectName + "$")
    val instance = moduleClass.getField("MODULE$").get(null)
    val run = moduleClass.getMethod("run")
    try run.invoke(instance)
    catch case e: java.lang.reflect.InvocationTargetException =>
      val cause = e.getCause
      if cause != null then throw cause else throw e

  // ===========================================================================
  // Basics
  // ===========================================================================

  @Test def literalBody(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def run(): Any = eval[Int]("1 + 2")
        |""".stripMargin)
    assertEquals(3, r)

  @Test def capturesMethodLocalVal(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def run(): Any =
        |    val n = 10
        |    eval[Int]("n * 4")
        |""".stripMargin)
    assertEquals(40, r)

  @Test def capturesLambdaParam(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def run(): Any = List(1, 2, 3).map(z => eval[Int]("z * z"))
        |""".stripMargin)
    assertEquals(List(1, 4, 9), r)

  @Test def capturesMethodParam(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  private def f(j: Int): Int = eval[Int]("j + 1")
        |  def run(): Any = f(41)
        |""".stripMargin)
    assertEquals(42, r)

  @Test def writesThroughCapturedVar(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def run(): Any =
        |    var n = 10
        |    eval[Unit]("n = n + 5")
        |    n
        |""".stripMargin)
    assertEquals(15, r)

  @Test def capturesLocalGiven(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def run(): Any =
        |    given Int = 7
        |    eval[Int]("summon[Int] * 6")
        |""".stripMargin)
    assertEquals(42, r)

  @Test def runtimeComposedBody(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def f(j: Int): Int = j + 1
        |  def g(j: Int): Int = j * j
        |  def run(): Any =
        |    List("f", "g").map(fun => eval[Int](s"$fun(3)"))
        |""".stripMargin)
    assertEquals(List(4, 9), r)

  // ===========================================================================
  // Live module state (standalone analogue of REPL session imports)
  // ===========================================================================

  @Test def readsModuleMember(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  val base = 7
        |  def run(): Any = eval[Int]("base + 1")
        |""".stripMargin)
    assertEquals(8, r)

  @Test def writesLiveModuleVar(): Unit =
    // The eval body must mutate the *runtime* module instance, not a
    // re-minted copy compiled into the wrapper.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  var count = 0
        |  def run(): Any =
        |    eval[Unit]("count += 3")
        |    eval[Unit]("count += 4")
        |    count
        |""".stripMargin)
    assertEquals(7, r)

  @Test def callsModuleMethodByRuntimeName(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |import scala.util.Random
        |object Main:
        |  def double(j: Int): Int = j * 2
        |  def run(): Any =
        |    val name = if Random.nextInt(1) == 0 then "double" else "missing"
        |    eval[Int](s"$name(21)")
        |""".stripMargin)
    assertEquals(42, r)

  // ===========================================================================
  // Top-level definitions and packages
  // ===========================================================================

  @Test def callsTopLevelDef(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |
        |def f(j: Int): Int = j + 1
        |
        |object Main:
        |  def run(): Any = eval[Int]("f(41)")
        |""".stripMargin)
    assertEquals(42, r)

  @Test def evalInsideTopLevelDef(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |
        |def square(x: Int): Int = eval[Int]("x * x")
        |
        |object Main:
        |  def run(): Any = square(6)
        |""".stripMargin)
    assertEquals(36, r)

  @Test def namedPackage(): Unit =
    val r = compileAndRun(
      """package demo
        |
        |import dotty.tools.eval.Eval.eval
        |
        |def f(j: Int): Int = j + 1
        |
        |object Main:
        |  val base = 20
        |  def run(): Any = eval[Int]("f(base) + 21")
        |""".stripMargin,
      objectName = "demo.Main")
    assertEquals(42, r)

  @Test def usesFileImports(): Unit =
    // `Random` comes from the file's top-level import, which the
    // rewriter embeds into the enclosing source.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |import scala.util.Random
        |object Main:
        |  def run(): Any = eval[Int]("Random.nextInt(1) + 41")
        |""".stripMargin)
    assertEquals(41, r)

  // ===========================================================================
  // Classes
  // ===========================================================================

  @Test def evalInsideClassMethodSeesPrivateMember(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |
        |class Box(private var v: Int):
        |  def show(): Int = eval[Int]("v + 1")
        |
        |object Main:
        |  def run(): Any = new Box(41).show()
        |""".stripMargin)
    assertEquals(42, r)

  @Test def evalInsideClassMethodSeesParams(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |
        |class Adder(val base: Int):
        |  def add(j: Int): Int = eval[Int]("base + j")
        |
        |object Main:
        |  def run(): Any = new Adder(40).add(2)
        |""".stripMargin)
    assertEquals(42, r)

  // ===========================================================================
  // Nesting
  // ===========================================================================

  @Test def nestedEval(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def run(): Any =
        |    List(1, 2, 3).map(x => eval[Int]("List(x, x * 2).map(y => eval[Int](\"y + 1\")).sum"))
        |""".stripMargin)
    assertEquals(List(5, 8, 11), r)

  // ===========================================================================
  // Failure modes
  // ===========================================================================

  @Test def evalSafeReportsCompileErrors(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval
        |object Main:
        |  def run(): Any =
        |    Eval.evalSafe[Int]("\"abc\"") match
        |      case r if r.isSuccess => "unexpected success"
        |      case r => r.error.errors.mkString("\n")
        |""".stripMargin)
    val message = r.asInstanceOf[String]
    assertTrue(s"expected a type mismatch diagnostic, got:\n$message",
      message.contains("Required: Int"))

  @Test def throwingFormThrowsEvalCompileException(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |import dotty.tools.eval.EvalCompileException
        |object Main:
        |  def run(): Any =
        |    try eval[Int]("not valid scala !!!")
        |    catch case e: EvalCompileException => s"caught: ${e.errors.length > 0}"
        |""".stripMargin)
    assertEquals("caught: true", r)

  @Test def bodyRuntimeExceptionPropagates(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def run(): Any =
        |    try { eval[Int]("1 / 0"); "no throw" }
        |    catch case _: ArithmeticException => "arithmetic"
        |""".stripMargin)
    assertEquals("arithmetic", r)

  // ===========================================================================
  // Non-local `return` from the body
  // ===========================================================================

  @Test def returnFromEnclosingMethod(): Unit =
    // The body's `return` targets the method enclosing the eval call.
    // The rewriter wrapped the call in a `try/catch` keyed on the
    // per-execution `__evalReturnKey__` binding; the body's return
    // lowers to an `EvalNonLocalReturn` throw that the call-site catch
    // turns back into an ordinary `return`.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def f(): Int = eval[Int]("return 42")
        |  def run(): Any = f()
        |""".stripMargin)
    assertEquals(42, r)

  @Test def conditionalReturnFromEnclosingMethod(): Unit =
    // The non-return path falls through to the rest of the method.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def f(x: Int): Int =
        |    eval[Unit]("if x > 0 then return x * 2")
        |    -1
        |  def run(): Any = (f(21), f(0))
        |""".stripMargin)
    assertEquals((42, -1), r)

  @Test def localObjectLiveStateStandalone(): Unit =
    // A local `object`'s state is shared with the body through the
    // `__evalModule_Counter__` binding. The `+=` exercises the typer's
    // prefix-lift temp (`val $1$ = Counter`), whose info is erased to
    // Object so no checkcast against the wrapper's re-minted module
    // class is emitted.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def f(): Int =
        |    object Counter:
        |      var n = 0
        |    Counter.n = 1
        |    eval[Unit]("Counter.n += 10")
        |    Counter.n
        |  def run(): Any = f()
        |""".stripMargin)
    assertEquals(11, r)

  // ===========================================================================
  // Nested (non-top-level) objects are lifted so the body resolves against
  // the live module. This preserves module state and class identity; private
  // members are reached reflectively on the same instance.
  // ===========================================================================

  @Test def nestedObjectLiveState(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    var n = 0
        |    def bump(): Unit = eval[Unit]("n += 10")
        |  def run(): Any =
        |    Outer.n = 1
        |    Outer.bump()
        |    Outer.n
        |""".stripMargin)
    assertEquals(11, r)

  @Test def nestedObjectPeerState(): Unit =
    // `Counter` resolves through the injected `import Outer.{given, *}`
    // to the live `Main.Outer.Counter`.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    object Counter:
        |      var n = 0
        |    def bump(): Unit = eval[Unit]("Counter.n += 10")
        |  def run(): Any =
        |    Outer.Counter.n = 1
        |    Outer.bump()
        |    Outer.Counter.n
        |""".stripMargin)
    assertEquals(11, r)

  @Test def doublyNestedObjectLiveState(): Unit =
    // The marker sits two object layers deep; the lift recurses,
    // emitting one import per dropped layer (outermost first) so the
    // inner object's name resolves through the outer one's import.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    object Inner:
        |      var n = 0
        |      def bump(): Unit = eval[Unit]("n += 10")
        |  def run(): Any =
        |    Outer.Inner.n = 1
        |    Outer.Inner.bump()
        |    Outer.Inner.n
        |""".stripMargin)
    assertEquals(11, r)

  @Test def caseClassInNestedObject(): Unit =
    // The body constructs an instance of a case class declared next
    // to the lifted def. `Pt` resolves to the live classpath class,
    // so the instance pattern-matches outside against `Main.Outer.Pt`.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    case class Pt(x: Int, y: Int)
        |    def mk(): Pt = eval[Pt]("Pt(3, 4)")
        |  def run(): Any =
        |    Outer.mk() match
        |      case Main.Outer.Pt(a, b) => a + b
        |""".stripMargin)
    assertEquals(7, r)

  @Test def caseClassInNestedObjectPatternInBody(): Unit =
    // The instance is built outside, captured as a method parameter,
    // and destructured inside the body through the
    // live companion's unapply.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    case class Pt(x: Int, y: Int)
        |    def sum(p: Pt): Int = eval[Int]("p match { case Pt(a, b) => a + b }")
        |  def run(): Any = Outer.sum(Outer.Pt(20, 22))
        |""".stripMargin)
    assertEquals(42, r)

  @Test def privateValOfNestedObject(): Unit =
    // Unlike a top-level object (whose privates the embedded wildcard
    // import cannot expose), a *nested* object's privates are
    // collected from the dropped declaration and rerouted through the
    // reflective helpers with the live module as receiver.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    private val secret = 41
        |    def reveal(): Int = eval[Int]("secret + 1")
        |  def run(): Any = Outer.reveal()
        |""".stripMargin)
    assertEquals(42, r)

  @Test def privateVarOfNestedObjectWrite(): Unit =
    // A bare write to a private var of the lifted object lands on the
    // live instance through `__refl_set__`.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    private var count = 1
        |    def bump(): Unit = eval[Unit]("count = count + 10")
        |    def current: Int = count
        |  def run(): Any =
        |    Outer.bump()
        |    Outer.current
        |""".stripMargin)
    assertEquals(11, r)

  @Test def nestedObjectMethodRecursionThroughEval(): Unit =
    // The hoisted def is renamed (`__eval_fact__`), so the body's
    // `fact(n - 1)` resolves through the import to the live
    // `Outer.fact`: each recursive step re-enters the real method
    // (and its eval call) rather than the wrapper's drained stub.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    def fact(n: Int): Int =
        |      if n <= 1 then 1 else eval[Int]("n * fact(n - 1)")
        |  def run(): Any = Outer.fact(5)
        |""".stripMargin)
    assertEquals(120, r)

  @Test def thisInsideNestedObjectBody(): Unit =
    // A singleton's `this` *is* the module: plain `this` in the body
    // rewrites to the object's own name.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    val base = 20
        |    def calc(): Int = eval[Int]("this.base + Outer.base + 2")
        |  def run(): Any = Outer.calc()
        |""".stripMargin)
    assertEquals(42, r)

  @Test def givenInNestedObject(): Unit =
    // The injected `import Outer.{given, *}` carries the `given`
    // selector, so the body's implicit search resolves the object's
    // given members.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    given Int = 7
        |    def calc(): Int = eval[Int]("summon[Int] * 6")
        |  def run(): Any = Outer.calc()
        |""".stripMargin)
    assertEquals(42, r)

  @Test def classInsideNestedObject(): Unit =
    // Mixed nesting: the marker is in a method of a *class* declared
    // inside a nested object. The class lift composes with the module
    // lift: the lifted def's `__this__` parameter is typed
    // `Outer.W` (a static path through the dropped object's import),
    // and the body reaches the object's members through that import.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    val bonus = 5
        |    class W(val v: Int):
        |      def m(): Int = eval[Int]("v + bonus")
        |  def run(): Any = new Outer.W(37).m()
        |""".stripMargin)
    assertEquals(42, r)

  @Test def returnFromNestedObjectMethod(): Unit =
    // The non-local-return wrap composes with the module lift: the
    // body's `return` targets the hoisted def and lowers to the
    // keyed control throw the call site catches.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  object Outer:
        |    def f(): Int = eval[Int]("return 42")
        |  def run(): Any = Outer.f()
        |""".stripMargin)
    assertEquals(42, r)

  // ===========================================================================
  // Eval inside a local class's methods, and body-defined case classes
  // (standalone twins of the REPL tests in DynamicEvalTests).
  // ===========================================================================

  @Test def evalInsideLocalClassMethod(): Unit =
    // Bare `k` in the body carries an implicit `L.this` prefix; it
    // lowers through the captured `__this__` (the live instance)
    // with receiver-class reflection.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def run(): Any =
        |    class L(val k: Int):
        |      def reach: Int = eval[Int]("k * 3")
        |    (new L(11)).reach
        |""".stripMargin)
    assertEquals(33, r)

  @Test def bodyDefinesCaseClass(): Unit =
    // The case class (and its synthesized companion) is declared
    // inside the body string itself; its `this` references are
    // ordinary same-class reads, and the whole bundle moves into
    // `evaluate` with the body.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def run(): Any =
        |    eval[Int]("case class Local(a: Int, b: Int); val l = Local(7, 8); l.a * l.b")
        |""".stripMargin)
    assertEquals(56, r)

  @Test def returnInsideBodyLocalDefWorks(): Unit =
    // The boundary of the limitation: a `return` from a def declared
    // *inside* the body stays a local return. The def moves into
    // `evaluate` together with its return, so nothing crosses the
    // method boundary.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |object Main:
        |  def run(): Any =
        |    eval[Int]("def g(x: Int): Int = { if x > 0 then return x * 2; -1 }; g(21)")
        |""".stripMargin)
    assertEquals(42, r)

  // ===========================================================================
  // Eval.topLevel: compile-once definitions with shared state
  // ===========================================================================

  @Test def topLevelDefsSeePackageContextNotLocals(): Unit =
    // The defs compile in the file-level context of the `topLevel`
    // call site (here: the top-level `helper` def, reachable through
    // the embedded `<file>$package` import), while a method-local is
    // rejected at construction with the standard diagnostic.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.{topLevel, topLevelSafe}
        |def helper(x: Int): Int = x * 10
        |object Main:
        |  def run(): Any =
        |    val h = topLevel("def f(x: Int): Int = helper(x) + 1")
        |    val mystery = 5
        |    val ok = h.eval[Int]("f(mystery)")
        |    val iso = topLevelSafe("def g = mystery") match
        |      case r if r.isFailure => r.error.errors.head.linesIterator.next()
        |      case _ => "LEAKED"
        |    (ok, iso)
        |""".stripMargin)
    assertEquals((51, "Not found: mystery"), r)

  @Test def topLevelModuleStateSharedAcrossCallSites(): Unit =
    // The defs compile once per handle: two different `.eval` call
    // sites mutate one copy of the module state.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.topLevel
        |object Main:
        |  def run(): Any =
        |    val h = topLevel("object C { var n = 0 }\ndef inc(): Int = { C.n += 1; C.n }")
        |    val warm = (1 to 3).map(_ => h.eval[Int]("inc()")).toList
        |    val next = h.eval[Int]("inc()")
        |    (warm, next)
        |""".stripMargin)
    assertEquals((List(1, 2, 3), 4), r)

  @Test def topLevelClassIdentityStableAcrossCalls(): Unit =
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.topLevel
        |object Main:
        |  def run(): Any =
        |    val h = topLevel("class Box(var v: Int)")
        |    val b = h.eval[Any]("new Box(41)")
        |    h.eval[Int]("b.asInstanceOf[Box].v + 1")
        |""".stripMargin)
    assertEquals(42, r)

  // ===========================================================================
  // Flag gating
  // ===========================================================================

  @Test def withoutFlagNoBindingsAreFilled(): Unit =
    // Without -Xdynamic-eval the rewriter never runs: the call still
    // compiles (it's an ordinary method call) but the body is compiled
    // without the call site's lexical context, so a body that needs a
    // captured local fails at runtime with a compile error.
    val outDir = Files.createTempDirectory("standalone-eval-test-")
    val srcFile = Files.createTempFile("standalone-eval-", ".scala")
    Files.writeString(srcFile,
      """import dotty.tools.eval.Eval.eval
        |import dotty.tools.eval.EvalCompileException
        |object Main:
        |  def run(): Any =
        |    val n = 10
        |    try eval[Int]("n * 4")
        |    catch case e: EvalCompileException => "not rewritten"
        |""".stripMargin)
    val reporter = new StoreReporter(null)
    val driver = new Driver {}
    driver.process(Array("-d", outDir.toString, "-classpath", testClassPath, srcFile.toString), reporter)
    val errors = reporter.removeBufferedMessages(using dotc.core.Contexts.NoContext)
      .collect { case e: Diagnostic.Error => e.message }
    assertTrue(s"program failed to compile:\n${errors.mkString("\n")}", errors.isEmpty)
    assertEquals("not rewritten", invokeRun(outDir, "Main"))

  // ===========================================================================
  // Settings forwarding via system property
  // ===========================================================================

  @Test def forwardsSettingsFromSystemProperty(): Unit =
    val propName = "dotty.tools.eval.settings"
    val saved = System.getProperty(propName)
    System.setProperty(propName, "-Yexplicit-nulls")
    try
      val r = compileAndRun(
        """import dotty.tools.eval.Eval
          |object Main:
          |  def run(): Any =
          |    Eval.evalSafe[String]("null: String") match
          |      case r if r.isSuccess => "accepted"
          |      case r => "rejected"
          |""".stripMargin,
        extraFlags = Array("-Yexplicit-nulls"))
      assertEquals("rejected", r)
    finally
      if saved == null then System.clearProperty(propName)
      else System.setProperty(propName, saved)
