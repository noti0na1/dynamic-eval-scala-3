package dotty.tools
package repl
package eval

import org.junit.Test
import org.junit.Assert.*

import java.net.URLClassLoader
import java.nio.file.{Files, Path}

/** Tests for the eval pipeline.
 *
 *  Phase 1 covers splice mechanics: the wrapped enclosing source
 *  compiles successfully when the marker is replaced by the spliced
 *  body block, and both the enclosing class and the synthesised
 *  `__Expression` class produce `.class` files.
 *
 *  Phase 2 covers extract mechanics: the spliced body's typed rhs is
 *  drained from `val __evalResult` and installed as the rhs of
 *  `__Expression.evaluate`. End-to-end tests load the synthesised
 *  class and invoke `evaluate()` to verify the body runs and returns
 *  the expected value. Phase 2 only handles bodies whose references
 *  are body-local or globally-static — outer captures fail-fast as
 *  compile errors.
 */
class EvalCompilerBridgeTest:

  /** Scala/JVM classpath for the spawned `Driver`. The compiler internals
   *  and standard library must be visible — `java.class.path` covers
   *  this because sbt runs tests with the full module classpath set
   *  there.
   */
  private val testClassPath: String = System.getProperty("java.class.path")

  private case class SpliceResult(
      outputDir: Path,
      sourceFile: Path,
      outputClassName: String,
      ok: Boolean,
      errors: String
  )

  private def runSplice(body: String, enclosing: String): SpliceResult =
    val outputDir = Files.createTempDirectory("eval-test-")
    val sourceFile = Files.createTempFile("eval-enclosing-", ".scala")
    Files.writeString(sourceFile, enclosing)
    val errors = new StringBuilder
    val outputClassName = "__EvalExpression_" + java.util.UUID.randomUUID.toString.replace('-', '_')
    val config = EvalCompilerConfig(
      outputClassName = outputClassName,
      body = body,
      testMode = true,
      errorReporter = s => errors.append(s).append('\n')
    )
    val ok = EvalCompilerBridge().run(outputDir, testClassPath, Array.empty, sourceFile, config)
    SpliceResult(outputDir, sourceFile, outputClassName, ok, errors.toString)

  /** Build a fresh URLClassLoader rooted at the eval output directory,
   *  with the test JVM's classloader as parent so dotty types
   *  (`Eval.Binding`, `Eval.VarRef`) resolve to the same `Class`
   *  objects on both sides of the loader boundary.
   *
   *  Returning the loader (rather than a one-shot class lookup) lets
   *  callers load multiple classes from the same loader so they
   *  share `Class` objects — critical when a Phase 5 test wants to
   *  pass a `Holder`-style instance as `thisObject` and have
   *  `asInstanceOf[Holder]` succeed inside `__Expression.evaluate`.
   */
  private def newLoader(outputDir: Path): URLClassLoader =
    val parent = classOf[EvalCompilerBridgeTest].getClassLoader
    new URLClassLoader(Array(outputDir.toUri.toURL), parent)

  /** Result of running splice + extract + resolve: a freshly loaded
   *  `__Expression` class plus the loader that produced it, so a test
   *  can load sibling classes (a captured `this`'s enclosing class)
   *  through the *same* loader.
   */
  private case class Loaded(loader: URLClassLoader, exprClass: Class[?])

  private def loadExpression(outputDir: Path, outputClassName: String): Loaded =
    val cl = newLoader(outputDir)
    Loaded(cl, cl.loadClass(outputClassName))

  /** Instantiate `__Expression` and call its `evaluate()`. The
   *  optional `thisObject` is passed through to the synthesised
   *  constructor (Phase 5 onwards uses it for `getThisObject`).
   */
  private def invoke(
      loaded: Loaded,
      bindings: Array[Eval.Binding],
      thisObject: AnyRef | Null
  ): Any =
    val ctor = loaded.exprClass.getDeclaredConstructor(classOf[Object], classOf[Array[Eval.Binding]])
    val instance = ctor.newInstance(thisObject, bindings).asInstanceOf[AnyRef]
    val evaluate = loaded.exprClass.getMethod("evaluate")
    evaluate.invoke(instance)

  /** Convenience for the common path: load + invoke with a default
   *  null thisObject. Phase 1 - 4 tests use this form.
   */
  private def loadAndInvoke(
      outputDir: Path,
      outputClassName: String,
      bindings: Array[Eval.Binding] = Array.empty,
      thisObject: AnyRef | Null = null
  ): Any =
    invoke(loadExpression(outputDir, outputClassName), bindings, thisObject)

  // ===========================================================================
  // Phase 1: splice mechanics
  // ===========================================================================

  @Test def splicesIntoExpressionContext(): Unit =
    val enclosing =
      s"""object EnclosingTest {
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val r = runSplice(body = "1 + 2", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    assertTrue("EnclosingTest$.class missing", Files.exists(r.outputDir.resolve("EnclosingTest$.class")))
    assertTrue("__Expression class file missing",
      Files.exists(r.outputDir.resolve(s"${r.outputClassName}.class")))

  @Test def splicesAtAssignmentPosition(): Unit =
    val enclosing =
      s"""object EnclosingAssign {
         |  val r: Int = { ${EvalContext.placeholder} ; 42 }
         |}
         |""".stripMargin
    val r = runSplice(body = "println(\"hi\")", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)

  @Test def acceptsBodyWithBlockOfStatements(): Unit =
    val enclosing =
      s"""object EnclosingBlock {
         |  def f(): Unit = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val body =
      """val a = 1
        |val b = 2
        |println(a + b)
        |""".stripMargin
    val r = runSplice(body = body, enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)

  @Test def reportsMissingMarkerInTestMode(): Unit =
    val enclosing =
      """object EnclosingNoMarker {
        |  def f(): Int = 42
        |}
        |""".stripMargin
    val r = runSplice(body = "1 + 2", enclosing = enclosing)
    assertFalse("compile should have flagged the missing marker", r.ok)
    assertTrue(s"expected marker error, got:\n${r.errors}", r.errors.contains("not found"))

  // ===========================================================================
  // Phase 2: extract mechanics — load __Expression and invoke evaluate()
  // ===========================================================================

  /** Convenience: splice + assert compile success + return loadable result. */
  private def evalNoCaptures(body: String, enclosing: String): Any =
    val r = runSplice(body, enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    loadAndInvoke(r.outputDir, r.outputClassName)

  @Test def evaluatesSimpleArithmetic(): Unit =
    val enclosing =
      s"""object PhaseTwoSimple {
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val result = evalNoCaptures(body = "1 + 2", enclosing = enclosing)
    assertEquals(java.lang.Integer.valueOf(3), result)

  @Test def evaluatesBlockWithLocalVals(): Unit =
    val enclosing =
      s"""object PhaseTwoLocals {
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val body =
      """val a = 10
        |val b = 32
        |a + b
        |""".stripMargin
    val result = evalNoCaptures(body = body, enclosing = enclosing)
    assertEquals(java.lang.Integer.valueOf(42), result)

  @Test def evaluatesBlockWithLocalDef(): Unit =
    val enclosing =
      s"""object PhaseTwoLocalDef {
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val body =
      """def double(x: Int) = x * 2
        |double(7)
        |""".stripMargin
    val result = evalNoCaptures(body = body, enclosing = enclosing)
    assertEquals(java.lang.Integer.valueOf(14), result)

  @Test def evaluatesRecursiveLocalDef(): Unit =
    val enclosing =
      s"""object PhaseTwoRecursive {
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val body =
      """def fact(n: Int): Int = if n <= 0 then 1 else n * fact(n - 1)
        |fact(5)
        |""".stripMargin
    val result = evalNoCaptures(body = body, enclosing = enclosing)
    assertEquals(java.lang.Integer.valueOf(120), result)

  @Test def evaluatesMutuallyRecursiveLocalDefs(): Unit =
    val enclosing =
      s"""object PhaseTwoMutual {
         |  def f(): Boolean = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val body =
      """def even(n: Int): Boolean = if n == 0 then true else odd(n - 1)
        |def odd(n: Int): Boolean = if n == 0 then false else even(n - 1)
        |even(4)
        |""".stripMargin
    val result = evalNoCaptures(body = body, enclosing = enclosing)
    assertEquals(java.lang.Boolean.TRUE, result)

  @Test def evaluatesStaticModuleCall(): Unit =
    // Body uses `math.max(3, 5)` — `math` is a static package member
    // reachable as a fully qualified path, no capture needed.
    val enclosing =
      s"""object PhaseTwoStaticModule {
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val result = evalNoCaptures(body = "math.max(3, 5)", enclosing = enclosing)
    assertEquals(java.lang.Integer.valueOf(5), result)

  @Test def evaluatesLiteralMemberCall(): Unit =
    // Body uses `42.toString` — qualifier is a literal whose type's
    // class symbol (Int) is globally accessible. Member `toString`
    // is owned by class Int, also globally accessible.
    val enclosing =
      s"""object PhaseTwoLiteral {
         |  def f(): String = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val result = evalNoCaptures(body = "42.toString", enclosing = enclosing)
    assertEquals("42", result)

  // Originally deferred (task #8). The body chains a static-module
  // call (`List(1,2,3)`), a member access (`.sum`), and an implicit
  // Numeric witness expansion. All pieces should be globally
  // accessible — re-enabled now to verify the Select handling does
  // the right thing through the implicit-witness layers.
  @Test def evaluatesGloballyAccessibleCalls(): Unit =
    val enclosing =
      s"""object PhaseTwoGlobal {
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val result = evalNoCaptures(body = "List(1, 2, 3).sum", enclosing = enclosing)
    assertEquals(java.lang.Integer.valueOf(6), result)

  // ===========================================================================
  // Phase 3: LocalValue strategy — outer method params and method-locals
  //          captured via getValue("name") + asInstanceOf cast.
  // ===========================================================================

  /** Run the splice + load + invoke flow with a single Int binding. */
  private def runWithIntBinding(body: String, enclosing: String, name: String, value: Int): Any =
    val r = runSplice(body, enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val binding = new Eval.Binding(name, java.lang.Integer.valueOf(value), false, false)
    loadAndInvoke(r.outputDir, r.outputClassName, Array(binding))

  @Test def evaluatesMethodParamCapture(): Unit =
    val enclosing =
      s"""object PhaseThreeMethodParam {
         |  def f(arg: Int): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val result = runWithIntBinding(body = "arg + 1", enclosing = enclosing, name = "arg", value = 41)
    assertEquals(java.lang.Integer.valueOf(42), result)

  @Test def evaluatesLocalValCapture(): Unit =
    val enclosing =
      s"""object PhaseThreeLocalVal {
         |  def f(): Int = {
         |    val k = 7
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val result = runWithIntBinding(body = "k * 6", enclosing = enclosing, name = "k", value = 7)
    assertEquals(java.lang.Integer.valueOf(42), result)

  @Test def evaluatesLambdaParamCapture(): Unit =
    val enclosing =
      s"""object PhaseThreeLambda {
         |  val it: Int => Int = (z: Int) => ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val result = runWithIntBinding(body = "z * z", enclosing = enclosing, name = "z", value = 8)
    assertEquals(java.lang.Integer.valueOf(64), result)

  @Test def evaluatesMultipleCaptures(): Unit =
    val enclosing =
      s"""object PhaseThreeMulti {
         |  def f(a: Int, b: Int): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val r = runSplice(body = "a * b + 1", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val bindings: Array[Eval.Binding] = Array(
      new Eval.Binding("a", java.lang.Integer.valueOf(6), false, false),
      new Eval.Binding("b", java.lang.Integer.valueOf(7), false, false)
    )
    val result = loadAndInvoke(r.outputDir, r.outputClassName, bindings)
    assertEquals(java.lang.Integer.valueOf(43), result)

  @Test def evaluatesCapturedAndBodyLocalMix(): Unit =
    // Captures `n` from outside; body declares its own `factor` local.
    val enclosing =
      s"""object PhaseThreeMixed {
         |  def f(n: Int): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val body =
      """val factor = 3
        |n * factor + n
        |""".stripMargin
    val result = runWithIntBinding(body = body, enclosing = enclosing, name = "n", value = 10)
    assertEquals(java.lang.Integer.valueOf(40), result)

  @Test def evaluatesCapturedStringMember(): Unit =
    val enclosing =
      s"""object PhaseThreeString {
         |  def f(name: String): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val r = runSplice(body = "name.length", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val binding = new Eval.Binding("name", "hello world", false, false)
    val result = loadAndInvoke(r.outputDir, r.outputClassName, Array(binding))
    assertEquals(java.lang.Integer.valueOf(11), result)

  // ===========================================================================
  // Phase 5: This strategy — captured outer `this` for class-member access.
  //          (Outer-chain navigation, Field/MethodCall for inaccessible
  //          members, ClassCapture, etc. arrive in a follow-up phase.)
  // ===========================================================================

  /** Run splice + extract + resolve, load both the enclosing class
   *  and __Expression through one shared classloader, instantiate
   *  the holder, and invoke `evaluate()`. Returns
   *  `(holder, result)` so the test can assert post-call state on
   *  the holder (e.g. that a captured-var write took effect).
   */
  private def runWithHolder(
      body: String,
      enclosing: String,
      holderClassName: String,
      bindings: Array[Eval.Binding] = Array.empty
  ): (AnyRef, Any) =
    val r = runSplice(body, enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val holder = loaded.loader.loadClass(holderClassName).getDeclaredConstructor().newInstance().asInstanceOf[AnyRef]
    val result = invoke(loaded, bindings, holder)
    (holder, result)

  @Test def evaluatesClassMemberValRead(): Unit =
    val enclosing =
      s"""class PhaseFiveValHolder { val v: Int = 99; def f(): Int = ({ ${EvalContext.placeholder} }) }
         |""".stripMargin
    val (_, result) = runWithHolder(body = "v + 1", enclosing = enclosing, holderClassName = "PhaseFiveValHolder")
    assertEquals(java.lang.Integer.valueOf(100), result)

  @Test def evaluatesClassMemberMethodCall(): Unit =
    val enclosing =
      s"""class PhaseFiveMethodHolder {
         |  def m(): Int = 7
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val (_, result) = runWithHolder(body = "m() + 1", enclosing = enclosing, holderClassName = "PhaseFiveMethodHolder")
    assertEquals(java.lang.Integer.valueOf(8), result)

  @Test def evaluatesClassMemberVarWrite(): Unit =
    val enclosing =
      s"""class PhaseFiveVarHolder {
         |  var v: Int = 0
         |  def f(): Unit = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val (holder, _) = runWithHolder(body = "v = 42", enclosing = enclosing, holderClassName = "PhaseFiveVarHolder")
    val v = holder.getClass.getMethod("v").invoke(holder)
    assertEquals(java.lang.Integer.valueOf(42), v)

  @Test def evaluatesClassMemberMixedWithLocalCapture(): Unit =
    // `mult` is the class member; `n` is a method-local captured via
    // LocalValue. Body multiplies them.
    val enclosing =
      s"""class PhaseFiveMixed {
         |  val mult: Int = 6
         |  def f(n: Int): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val binding = new Eval.Binding("n", java.lang.Integer.valueOf(7), false, false)
    val (_, result) = runWithHolder(
      body = "mult * n",
      enclosing = enclosing,
      holderClassName = "PhaseFiveMixed",
      bindings = Array(binding)
    )
    assertEquals(java.lang.Integer.valueOf(42), result)

  // ===========================================================================
  // Phase 5b: Outer-chain navigation for nested-class eval bodies.
  // ===========================================================================

  /** Instantiate an inner-class instance via reflection. The Scala 3
   *  inner-class constructor takes the outer instance as its first
   *  argument; both classes must be loaded through the same loader.
   */
  private def newInnerInstance(loader: URLClassLoader, outerName: String, innerName: String): AnyRef =
    val outerCls = loader.loadClass(outerName)
    val outer = outerCls.getDeclaredConstructor().newInstance().asInstanceOf[AnyRef]
    val innerCls = loader.loadClass(innerName)
    innerCls.getDeclaredConstructor(outerCls).newInstance(outer).asInstanceOf[AnyRef]

  @Test def evaluatesOuterClassMemberFromNestedClass(): Unit =
    val enclosing =
      s"""class PhaseFiveBOuter {
         |  val outerVal: Int = 5
         |  class Inner { def f(): Int = ({ ${EvalContext.placeholder} }) }
         |}
         |""".stripMargin
    val r = runSplice(body = "outerVal + 1", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val inner = newInnerInstance(loaded.loader, "PhaseFiveBOuter", "PhaseFiveBOuter$Inner")
    val result = invoke(loaded, Array.empty, inner)
    assertEquals(java.lang.Integer.valueOf(6), result)

  @Test def evaluatesOuterClassMethodFromNestedClass(): Unit =
    val enclosing =
      s"""class PhaseFiveBOuterMethod {
         |  def factor: Int = 3
         |  class Inner { def f(): Int = ({ ${EvalContext.placeholder} }) }
         |}
         |""".stripMargin
    val r = runSplice(body = "factor * 7", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val inner = newInnerInstance(loaded.loader, "PhaseFiveBOuterMethod", "PhaseFiveBOuterMethod$Inner")
    val result = invoke(loaded, Array.empty, inner)
    assertEquals(java.lang.Integer.valueOf(21), result)

  @Test def evaluatesNestedAndImmediateClassMix(): Unit =
    // Body references `outerVal` from PhaseFiveBMix (outer step) AND
    // `innerVal` from Inner (immediate-this); validates that the
    // chain length is computed per This target.
    val enclosing =
      s"""class PhaseFiveBMix {
         |  val outerVal: Int = 10
         |  class Inner {
         |    val innerVal: Int = 32
         |    def f(): Int = ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "outerVal + innerVal", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val inner = newInnerInstance(loaded.loader, "PhaseFiveBMix", "PhaseFiveBMix$Inner")
    val result = invoke(loaded, Array.empty, inner)
    assertEquals(java.lang.Integer.valueOf(42), result)

  // ===========================================================================
  // Phase 5c: Field/FieldAssign/MethodCall for inaccessible (private)
  //           members. Reflectively access private fields/methods that
  //           a direct getfield/invokevirtual would reject at JVM link.
  // ===========================================================================

  @Test def evaluatesPrivateValRead(): Unit =
    // Read a private val. The val is referenced by a public `peek`
    // method too — without that, ExtractEvalBody drains the eval
    // body's reference to `secret`, leaving the val apparently-unused
    // from the typed-tree perspective, and Scala 3's optimiser
    // elides the backing field entirely. With the extra accessor, the
    // field stays around for our reflective lookup.
    val enclosing =
      s"""class PhaseFiveCPrivVal {
         |  private val secret: Int = compute()
         |  private def compute(): Int = 99
         |  def peek: Int = secret
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val (_, result) = runWithHolder(
      body = "secret + 1",
      enclosing = enclosing,
      holderClassName = "PhaseFiveCPrivVal"
    )
    assertEquals(java.lang.Integer.valueOf(100), result)

  @Test def evaluatesPrivateVarWrite(): Unit =
    // `private var` always emits a field (vars are mutable so the
    // optimiser can't fold them away), and `get` keeps reads alive.
    val enclosing =
      s"""class PhaseFiveCPrivVar {
         |  private var counter: Int = 0
         |  def f(): Unit = ({ ${EvalContext.placeholder} })
         |  def get: Int = counter
         |}
         |""".stripMargin
    val (holder, _) = runWithHolder(
      body = "counter = 7",
      enclosing = enclosing,
      holderClassName = "PhaseFiveCPrivVar"
    )
    val v = holder.getClass.getMethod("get").invoke(holder)
    assertEquals(java.lang.Integer.valueOf(7), v)

  @Test def evaluatesPrivateMethodCall(): Unit =
    val enclosing =
      s"""class PhaseFiveCPrivMethod {
         |  private def secret(): Int = 42
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val (_, result) = runWithHolder(
      body = "secret() + 1",
      enclosing = enclosing,
      holderClassName = "PhaseFiveCPrivMethod"
    )
    assertEquals(java.lang.Integer.valueOf(43), result)

  @Test def evaluatesPrivateMethodWithIntArgs(): Unit =
    val enclosing =
      s"""class PhaseFiveCPrivMethodArgs {
         |  private def add(a: Int, b: Int): Int = a + b
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val (_, result) = runWithHolder(
      body = "add(20, 22)",
      enclosing = enclosing,
      holderClassName = "PhaseFiveCPrivMethodArgs"
    )
    assertEquals(java.lang.Integer.valueOf(42), result)

  // ===========================================================================
  // Phase 4: LocalValueAssign + var captures via VarRef
  // ===========================================================================

  /** Mutable Int cell wrapped in an [[Eval.VarRef]] so the captured
   *  binding presents the get/set API the body's `x` and `x = v`
   *  rewrites compile against.
   */
  private final class IntVarRef(initial: Int) extends Eval.VarRef[java.lang.Integer]:
    private var v: Int = initial
    def get(): java.lang.Integer = v
    def set(newV: java.lang.Integer): Unit = v = newV.intValue
    def current: Int = v

  @Test def evaluatesCapturedVarRead(): Unit =
    val enclosing =
      s"""object PhaseFourVarRead {
         |  def f(): Int = {
         |    var x: Int = 41
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "x + 1", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val ref = new IntVarRef(41)
    val binding = new Eval.Binding("x", ref, isVar = true, isGiven = false)
    val result = loadAndInvoke(r.outputDir, r.outputClassName, Array(binding))
    assertEquals(java.lang.Integer.valueOf(42), result)

  @Test def evaluatesCapturedVarWrite(): Unit =
    val enclosing =
      s"""object PhaseFourVarWrite {
         |  def f(): Unit = {
         |    var x: Int = 0
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "x = 99", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val ref = new IntVarRef(0)
    val binding = new Eval.Binding("x", ref, isVar = true, isGiven = false)
    loadAndInvoke(r.outputDir, r.outputClassName, Array(binding))
    assertEquals("var write should have updated the cell", 99, ref.current)

  @Test def evaluatesCapturedVarIncrement(): Unit =
    // Read + write in one body. The result expression returns the
    // post-increment value; the cell's value should end at 11.
    val enclosing =
      s"""object PhaseFourVarIncrement {
         |  def f(): Int = {
         |    var x: Int = 10
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val body =
      """x = x + 1
        |x
        |""".stripMargin
    val r = runSplice(body = body, enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val ref = new IntVarRef(10)
    val binding = new Eval.Binding("x", ref, isVar = true, isGiven = false)
    val result = loadAndInvoke(r.outputDir, r.outputClassName, Array(binding))
    assertEquals(java.lang.Integer.valueOf(11), result)
    assertEquals("var increment should have updated the cell", 11, ref.current)

  @Test def evaluatesMixedValAndVarCaptures(): Unit =
    // val `factor` (immutable) plus var `total` (mutable). The body
    // writes through `total` while reading `factor`.
    val enclosing =
      s"""object PhaseFourMixed {
         |  def f(): Int = {
         |    val factor: Int = 5
         |    var total: Int = 0
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val body =
      """total = factor * 7
        |total + 1
        |""".stripMargin
    val r = runSplice(body = body, enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val totalRef = new IntVarRef(0)
    val bindings: Array[Eval.Binding] = Array(
      new Eval.Binding("factor", java.lang.Integer.valueOf(5), false, false),
      new Eval.Binding("total", totalRef, true, false)
    )
    val result = loadAndInvoke(r.outputDir, r.outputClassName, bindings)
    assertEquals(java.lang.Integer.valueOf(36), result)
    assertEquals(35, totalRef.current)

  // ===========================================================================
  // Phase 6: Captured local-class instance access via reflective dispatch.
  //          A class declared inside a method has a term-owned symbol; the
  //          wrapper compile re-elaborates it as a fresh JVM class, so a
  //          binding whose runtime class is the *original* (different JVM
  //          class with same shape) cannot be reached via direct getfield/
  //          invokevirtual. ExtractEvalBody routes such accesses through
  //          the reflective `getField` / `callMethod` helpers, which use
  //          `obj.getClass` rather than the wrapper's symbol-derived JVM
  //          name.
  // ===========================================================================

  @Test def evaluatesCapturedLocalClassFieldRead(): Unit =
    // `HelperC` is a top-level peer class with the same `i: Int`
    // public val shape as the term-owned `C` defined inside `f`.
    // We pass a `HelperC` instance as the `c` binding to simulate
    // the runtime (rs$line$N) / wrapper-compile JVM-class divergence.
    val enclosing =
      s"""object PhaseSixLocalClass {
         |  class HelperC(val i: Int)
         |  def f(): Int = {
         |    case class C(i: Int)
         |    val c = new C(0)
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "c.i + 1", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val helperCls = loaded.loader.loadClass("PhaseSixLocalClass$HelperC")
    val helperInstance = helperCls
      .getDeclaredConstructor(classOf[Int])
      .newInstance(java.lang.Integer.valueOf(42))
      .asInstanceOf[AnyRef]
    val binding = new Eval.Binding("c", helperInstance, false, false)
    val result = invoke(loaded, Array(binding), null)
    assertEquals(java.lang.Integer.valueOf(43), result)

  @Test def evaluatesCapturedLocalClassMethodCall(): Unit =
    // Same shape but the body calls a method (`describe`) rather
    // than a val. Method calls go through the same reflective
    // dispatch path with `useReceiverClass = true`.
    val enclosing =
      s"""object PhaseSixLocalClassMethod {
         |  class HelperC(val i: Int) {
         |    def describe: String = s"helper(" + i + ")"
         |  }
         |  def f(): String = {
         |    case class C(i: Int) {
         |      def describe: String = s"local(" + i + ")"
         |    }
         |    val c = new C(0)
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "c.describe", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val helperCls = loaded.loader.loadClass("PhaseSixLocalClassMethod$HelperC")
    val helperInstance = helperCls
      .getDeclaredConstructor(classOf[Int])
      .newInstance(java.lang.Integer.valueOf(7))
      .asInstanceOf[AnyRef]
    val binding = new Eval.Binding("c", helperInstance, false, false)
    val result = invoke(loaded, Array(binding), null)
    // The runtime instance is HelperC, so its `describe` method —
    // not the wrapper's `C.describe` — gets called.
    assertEquals("helper(7)", result)

  @Test def evaluatesCapturedLocalClassMultiArgListMethod(): Unit =
    // Curried method `f(a: Int)(b: Int)`: the typed AST is
    // `Apply(Apply(Select(c, f), List(1)), List(2))`, but after
    // erasure the JVM method takes two flat params. transformed-
    // MethodArgs walks down the nested Applies in source order,
    // so reflective dispatch sees the flattened param list.
    val enclosing =
      s"""object PhaseSixCurried {
         |  class HelperC(val k: Int) {
         |    def f(a: Int)(b: Int): Int = a * b + k
         |  }
         |  def f(): Int = {
         |    class C(val k: Int) {
         |      def f(a: Int)(b: Int): Int = a * b + k
         |    }
         |    val c = new C(0)
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "c.f(3)(4)", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val helperCls = loaded.loader.loadClass("PhaseSixCurried$HelperC")
    val helperInstance = helperCls
      .getDeclaredConstructor(classOf[Int])
      .newInstance(java.lang.Integer.valueOf(7))
      .asInstanceOf[AnyRef]
    val binding = new Eval.Binding("c", helperInstance, false, false)
    val result = invoke(loaded, Array(binding), null)
    // 3 * 4 + 7 = 19
    assertEquals(java.lang.Integer.valueOf(19), result)

  @Test def evaluatesCapturedLocalClassPolyMultiArgListMethod(): Unit =
    // Polymorphic curried method `g[T](a: T)[U](b: U): String` on a
    // local class. Tests both the type-arg traversal in transformed-
    // MethodArgs and that the encoded JVM signature matches after
    // erasure (`g(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/String;`).
    val enclosing =
      s"""object PhaseSixPolyCurried {
         |  class HelperC {
         |    def g[T](a: T)[U](b: U): String = a.toString + "-" + b.toString
         |  }
         |  def f(): String = {
         |    class C {
         |      def g[T](a: T)[U](b: U): String = a.toString + "-" + b.toString
         |    }
         |    val c = new C
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "c.g[Int](7)[String](\"x\")", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val helperCls = loaded.loader.loadClass("PhaseSixPolyCurried$HelperC")
    val helperInstance = helperCls.getDeclaredConstructor().newInstance().asInstanceOf[AnyRef]
    val binding = new Eval.Binding("c", helperInstance, false, false)
    val result = invoke(loaded, Array(binding), null)
    assertEquals("7-x", result)

  @Test def evaluatesCapturedLocalClassWithCapturedFreeVar(): Unit =
    // The local class `Counter` closes over `n` from its enclosing
    // method. The runtime instance carries `n` in its own field
    // (via the synthesised `$outer` chain or a captured-var box),
    // so reflective dispatch on `obj.getClass` reaches the right
    // captured state — the wrapper's `Counter` symbol is irrelevant.
    val enclosing =
      s"""object PhaseSixCaptured {
         |  class HelperCounter(n: Int) {
         |    def add(x: Int): Int = x + n
         |  }
         |  def f(n: Int): Int = {
         |    class Counter {
         |      def add(x: Int): Int = x + n
         |    }
         |    val c = new Counter
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "c.add(5)", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val helperCls = loaded.loader.loadClass("PhaseSixCaptured$HelperCounter")
    val helperInstance = helperCls
      .getDeclaredConstructor(classOf[Int])
      .newInstance(java.lang.Integer.valueOf(10))
      .asInstanceOf[AnyRef]
    val binding = new Eval.Binding("c", helperInstance, false, false)
    val result = invoke(loaded, Array(binding), null)
    // HelperCounter(10).add(5) = 15
    assertEquals(java.lang.Integer.valueOf(15), result)

  @Test def evaluatesCapturedLocalClassMethodReturningSelf(): Unit =
    // Method on a local class that returns another instance of the
    // same class (`copy`-style). Tests that the chained access
    // `c.copy(8).i` flows correctly: the outer `copy` reflective
    // call returns a HelperC instance, then `.i` reflectively reads
    // its field.
    val enclosing =
      s"""object PhaseSixSelfReturn {
         |  class HelperC(val i: Int) {
         |    def copy(newI: Int): HelperC = new HelperC(newI)
         |  }
         |  def f(): Int = {
         |    class C(val i: Int) {
         |      def copy(newI: Int): C = new C(newI)
         |    }
         |    val c = new C(0)
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "c.copy(8).i + 1", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val helperCls = loaded.loader.loadClass("PhaseSixSelfReturn$HelperC")
    val helperInstance = helperCls
      .getDeclaredConstructor(classOf[Int])
      .newInstance(java.lang.Integer.valueOf(0))
      .asInstanceOf[AnyRef]
    val binding = new Eval.Binding("c", helperInstance, false, false)
    val result = invoke(loaded, Array(binding), null)
    assertEquals(java.lang.Integer.valueOf(9), result)

  @Test def evaluatesBodyLocalNewExpressionOfLocalClass(): Unit =
    // Construct via `new C(...)` instead of `C(...)`. The `new` form
    // bypasses the companion's lazy-ref (no `apply` call through
    // `C$lzy1.get.apply(...)`), so LambdaLift's proxy-chain issue
    // doesn't bite. The plain-`C(8)` apply form still fails — see
    // the TODO below.
    val enclosing =
      s"""object PhaseSixBodyLocalNew {
         |  def f(): Int = {
         |    class C(val i: Int)
         |    val _unused: Int = 0
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "new C(8).i + 1", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val result = loadAndInvoke(r.outputDir, r.outputClassName)
    assertEquals(java.lang.Integer.valueOf(9), result)

  /* TODO: body-local construction via the case-class apply form —
   *   `case class C(i: Int); eval[Int]("C(8).i + 1")` — currently
   *   fails at LambdaLift with "Could not find proxy for lazy var
   *   C$lzy1". The body's `Ident(C)` references the companion's
   *   lazy-ref, which is owned by `f`; once ExtractEvalBody moves
   *   the body into `__Expression.evaluate`, LambdaLift's proxy
   *   chain for the lazy ref breaks because `evaluate` isn't
   *   reachable along `f`'s call chain. The `new C(...)` form works
   *   today (see `evaluatesBodyLocalNewExpressionOfLocalClass`); the
   *   apply form would need either (a) reflective companion-module
   *   lookup at runtime, or (b) eagerly hoisting the local class
   *   in the wrapper compile so the companion is top-level. Captured-
   *   instance access (the `evaluatesCapturedLocalClass*` tests) is
   *   the primary use case and already works.
   */

  // ===========================================================================
  // Phase 7: behavioural tests for tuples, named tuples, type aliases,
  //          type members, self types, and path-dependent inner classes.
  // ===========================================================================

  @Test def evaluatesTupleConstructionAndDestructuring(): Unit =
    // Construct a tuple inside the body and read fields; tuples
    // erase to a runtime class (`scala.Tuple2`) accessible from the
    // wrapper, no special handling needed.
    val enclosing =
      s"""object PhaseSevenTuple {
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val r = runSplice(body = "val t = (3, 4); t._1 + t._2", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val result = loadAndInvoke(r.outputDir, r.outputClassName)
    assertEquals(java.lang.Integer.valueOf(7), result)

  @Test def evaluatesCapturedTuple(): Unit =
    // Tuple capture: passing in a Scala 3 tuple instance via the
    // bindings array. The cast is to scala.Tuple2 (a globally-named
    // class), so reflective routing isn't needed.
    val enclosing =
      s"""object PhaseSevenTupleCapture {
         |  def f(t: (Int, Int)): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val r = runSplice(body = "t._1 * t._2", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val tuple = scala.Tuple2(java.lang.Integer.valueOf(6), java.lang.Integer.valueOf(7))
    val binding = new Eval.Binding("t", tuple.asInstanceOf[Object], false, false)
    val result = loadAndInvoke(r.outputDir, r.outputClassName, Array(binding))
    assertEquals(java.lang.Integer.valueOf(42), result)

  @Test def evaluatesGlobalTypeAliasNoEffect(): Unit =
    // A top-level type alias `type IntPair = (Int, Int)` shouldn't
    // change behaviour — the alias dealiases to the underlying tuple
    // type and reflective access stays on the tuple's JVM class.
    val enclosing =
      s"""object PhaseSevenAliasGlobal {
         |  type IntPair = (Int, Int)
         |  def f(p: IntPair): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val r = runSplice(body = "p._1 + p._2", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val tuple = scala.Tuple2(java.lang.Integer.valueOf(10), java.lang.Integer.valueOf(11))
    val binding = new Eval.Binding("p", tuple.asInstanceOf[Object], false, false)
    val result = loadAndInvoke(r.outputDir, r.outputClassName, Array(binding))
    assertEquals(java.lang.Integer.valueOf(21), result)

  @Test def evaluatesLocalTypeAliasNoEffect(): Unit =
    // Local type alias inside a method. EvalTypeAnnotate's
    // `dealiasLocalAliases` peels term-owned aliases so the
    // expected-type rendering doesn't get stuck on a name that
    // wouldn't resolve in the wrapper.
    val enclosing =
      s"""object PhaseSevenAliasLocal {
         |  def f(): Int = {
         |    type Score = Int
         |    val s: Score = 21
         |    ({ ${EvalContext.placeholder} })
         |  }
         |}
         |""".stripMargin
    val r = runSplice(body = "s + s", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val binding = new Eval.Binding("s", java.lang.Integer.valueOf(21), false, false)
    val result = loadAndInvoke(r.outputDir, r.outputClassName, Array(binding))
    assertEquals(java.lang.Integer.valueOf(42), result)

  @Test def evaluatesTypeMemberAbstractCompiles(): Unit =
    // Compile-only: an abstract type member doesn't break the eval
    // driver's expected-type rendering. After erasure the type
    // member is `Object`; reflective dispatch on `b.get` would find
    // the method on the runtime concrete subclass at runtime.
    val enclosing =
      s"""object PhaseSevenTypeMemberAbstract {
         |  abstract class Box {
         |    type T
         |    val value: T
         |    def get: T = value
         |  }
         |  def f(b: Box): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val r = runSplice(body = "b.get.toString.length", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)

  @Test def evaluatesTypeMemberConcreteAccess(): Unit =
    // Class with a concrete type member resolved to Int. Reflective
    // dispatch reads the field/method named `value`; the type member
    // is purely a typer-level construct.
    val enclosing =
      s"""object PhaseSevenTypeMemberConcrete {
         |  class IntBox(val value: Int) {
         |    type T = Int
         |    def get: T = value
         |  }
         |  def f(b: IntBox): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val r = runSplice(body = "b.get + 1", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val boxCls = loaded.loader.loadClass("PhaseSevenTypeMemberConcrete$IntBox")
    val box = boxCls.getDeclaredConstructor(classOf[Int]).newInstance(java.lang.Integer.valueOf(41)).asInstanceOf[AnyRef]
    val binding = new Eval.Binding("b", box, false, false)
    val result = invoke(loaded, Array(binding), null)
    assertEquals(java.lang.Integer.valueOf(42), result)

  @Test def evaluatesSelfTypedClassMemberAccess(): Unit =
    // A class with a self type. The captured instance's reflective
    // dispatch only depends on the runtime class's method table, so
    // the self-type annotation is a typer-only construct and doesn't
    // affect the JVM access path.
    val enclosing =
      s"""object PhaseSevenSelfType {
         |  trait Named { def name: String }
         |  class Greeter(theName: String) { self: Named =>
         |    def name: String = theName
         |    def greet(s: String): String = s + ", " + name + "!"
         |  }
         |  def f(g: Greeter): String = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val r = runSplice(body = "g.greet(\"Hi\")", enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val loaded = loadExpression(r.outputDir, r.outputClassName)
    val greeterCls = loaded.loader.loadClass("PhaseSevenSelfType$Greeter")
    val greeterInstance = greeterCls
      .getDeclaredConstructor(classOf[String])
      .newInstance("World")
      .asInstanceOf[AnyRef]
    val binding = new Eval.Binding("g", greeterInstance, false, false)
    val result = invoke(loaded, Array(binding), null)
    assertEquals("Hi, World!", result)

  // ===========================================================================
  // Phase 8: behavioural tests for inline def, match types, and other
  //          type-level features. None of these should affect the
  //          reflective-dispatch / capture pipeline; the tests document
  //          that the eval driver is transparent to them.
  // ===========================================================================

  @Test def evaluatesInlineDefDeclaredInBody(): Unit =
    // `inline def` declared inside the eval body itself. The inline
    // expansion happens during the wrapper compile (the body is
    // typed there), so the call is materialised before the body
    // becomes `__Expression.evaluate`'s rhs.
    val enclosing =
      s"""object PhaseEightInlineBody {
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val body =
      """inline def double(x: Int): Int = x * 2
        |double(7) + 1
        |""".stripMargin
    val r = runSplice(body = body, enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val result = loadAndInvoke(r.outputDir, r.outputClassName)
    assertEquals(java.lang.Integer.valueOf(15), result)

  @Test def evaluatesInlineParameter(): Unit =
    // `inline` parameter in a body-local def. Inline params are
    // resolved at the call site during typing — the expansion is
    // visible to the wrapper compile, no special pipeline support
    // is required.
    val enclosing =
      s"""object PhaseEightInlineParam {
         |  def f(): Int = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val body =
      """inline def addLiteral(x: Int, inline n: Int): Int = x + n
        |addLiteral(40, 2)
        |""".stripMargin
    val r = runSplice(body = body, enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val result = loadAndInvoke(r.outputDir, r.outputClassName)
    assertEquals(java.lang.Integer.valueOf(42), result)

  @Test def evaluatesMatchTypeAlias(): Unit =
    // Match types are typer-only constructs — they reduce to a
    // concrete type at typecheck time. The eval driver should be
    // transparent: the body uses the alias the same way it would
    // use a regular type alias.
    val enclosing =
      s"""object PhaseEightMatchType {
         |  type Elem[X] = X match
         |    case String => Char
         |    case Array[t] => t
         |  def f(): Char = ({ ${EvalContext.placeholder} })
         |}
         |""".stripMargin
    val body = """val s: String = "hi"; (s.head: Elem[String])""".stripMargin
    val r = runSplice(body = body, enclosing = enclosing)
    assertTrue(s"compile failed:\n${r.errors}", r.ok)
    val result = loadAndInvoke(r.outputDir, r.outputClassName)
    assertEquals(java.lang.Character.valueOf('h'), result)

