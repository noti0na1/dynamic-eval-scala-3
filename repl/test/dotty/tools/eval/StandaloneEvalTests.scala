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
 *  Mirrors the behaviour axes of [[DynamicEvalTests]] on the
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

  @Test def readsSiblingModuleMember(): Unit =
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

  @Test def callsSiblingModuleMethodByRuntimeName(): Unit =
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
  // Known limitation: `return` in the body
  // ===========================================================================

  @Test def returnFromEnclosingMethodIsRejected(): Unit =
    // A body cannot `return` from the method enclosing the eval call:
    // at runtime the body executes inside `__Expression.evaluate`, so
    // the target frame is gone by construction. The pipeline rejects it
    // with a deliberate diagnostic (not an internal compiler error, and
    // not a crash), and the program can catch and continue.
    val r = compileAndRun(
      """import dotty.tools.eval.Eval.eval
        |import dotty.tools.eval.EvalCompileException
        |object Main:
        |  // Check `e.errors` (the diagnostics alone), not `e.getMessage`:
        |  // the message also embeds the generated source, which contains
        |  // this very catch block and its string literals.
        |  def f(): Int =
        |    try eval[Int]("return 42")
        |    catch case e: EvalCompileException =>
        |      if e.errors.exists(_.contains("Internal compiler error")) then -2
        |      else if e.errors.exists(_.contains("cannot `return` from the method enclosing the eval call")) then -1
        |      else -3
        |  def run(): Any = f()
        |""".stripMargin)
    assertEquals(-1, r)

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
