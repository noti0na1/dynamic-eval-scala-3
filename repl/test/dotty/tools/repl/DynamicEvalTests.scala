package dotty.tools
package repl

import org.junit.Test
import org.junit.Assert._

private object DynamicEvalAssertions:
  def assertContains(needle: String, haystack: String): Unit =
    assertTrue(s"expected to contain `$needle`, got:\n$haystack", haystack.contains(needle))
import DynamicEvalAssertions.*

/** Tests for dynamic `eval[T](code: String): T`.
 *
 *  Behaviour summary:
 *    1. At runtime, `eval` calls back into the REPL driver, which spins up a
 *       separate dotc Driver to compile and run `code` against the live REPL
 *       session's classpath.
 *    2. The argument can be any `String`: literal, `val`, `s"..."`, etc.
 *    3. The REPL parser stage rewriter injects `Eval.bind("z", z)` for every
 *       lambda parameter (and block-local `val`) syntactically in scope at
 *       the call site, so `xs.map(z => eval[Int]("z + 1"))` Just Works.
 *    4. REPL session state (vals, vars, defs, classes, givens) is imported
 *       into the body's scope.
 *    5. Return type is the polymorphic `T`; the user ascribes the expected
 *       type and a runtime cast bridges to it. A mismatch surfaces as
 *       `ClassCastException`.
 *
 *  Sections roughly track one axis at a time. Tests for documented
 *  shortcomings sit in the final "Known limitations" section.
 */
class DynamicEvalTests extends ReplTest:

  // ===========================================================================
  // 1. Basics: primitive return types via the polymorphic T parameter.
  // ===========================================================================

  @Test def returnsInt = initially {
    run("""val r: Int = eval("1 + 2")""")
    assertContains("val r: Int = 3", storedOutput())
  }

  @Test def returnsLong = initially {
    run("""val r: Long = eval("1000000000000L")""")
    assertContains("val r: Long = 1000000000000L", storedOutput())
  }

  @Test def returnsDouble = initially {
    run("""val r: Double = eval("math.sqrt(16.0)")""")
    assertContains("val r: Double = 4.0", storedOutput())
  }

  @Test def returnsBoolean = initially {
    run("""val r: Boolean = eval("1 < 2 && 3 != 4")""")
    assertContains("val r: Boolean = true", storedOutput())
  }

  @Test def returnsChar = initially {
    run("""val r: Char = eval("'a'.toUpper")""")
    assertContains("val r: Char = 'A'", storedOutput())
  }

  @Test def returnsString = initially {
    run("""val r: String = eval("\"hello\".reverse")""")
    assertContains("""val r: String = "olleh"""", storedOutput())
  }

  @Test def returnsTuple = initially {
    run("""val r: (Int, String) = eval("(42, \"answer\")")""")
    assertContains("""val r: (Int, String) = (42, "answer")""", storedOutput())
  }

  @Test def returnsOption = initially {
    run("""val r: Option[Int] = eval("Some(7)")""")
    assertContains("val r: Option[Int] = Some(7)", storedOutput())
  }

  @Test def returnsList = initially {
    run("""val r: List[Int] = eval("List(1, 2, 3)")""")
    assertContains("val r: List[Int] = List(1, 2, 3)", storedOutput())
  }

  // ===========================================================================
  // 2. Dynamic strings: the argument can be any String value, not just a literal.
  // ===========================================================================

  @Test def evalsStringVariable = initially {
    run("""|val op: String = "1 + 2"
           |val r: Int = eval(op)""".stripMargin)
    assertContains("val r: Int = 3", storedOutput())
  }

  @Test def evalsInterpolatedString = initially {
    run("""|val n: Int = 5
           |val expr: String = s"$n * $n + 1"
           |val r: Int = eval(expr)""".stripMargin)
    assertContains("val r: Int = 26", storedOutput())
  }

  @Test def evalsDifferentStringsPerIteration = initially {
    run("""val r: List[Int] = (1 to 3).toList.map(i => eval[Int](s"$i * 10"))""")
    assertContains("List(10, 20, 30)", storedOutput())
  }

  @Test def dynamicDispatchOverFunctionNames = initially {
    // Dispatching on a name held in a String. `f` and `g` live at
    // REPL session level, so they're reachable inside the eval body
    // via the auto-injected `import rs$line$N.*`. The lambda param
    // `op` is captured by the rewriter (as a String) but only used
    // for the s-interpolation that builds the body string; the body
    // itself never references `op` and instead calls the named
    // session-level function directly.
    run("""|def f(i: Int) = i * 2
           |def g(i: Int) = i - 2
           |val ops = List("f", "g")
           |val r: List[Int] = ops.map(op => eval[Int](s"$op(2)"))""".stripMargin)
    assertContains("val r: List[Int] = List(4, 0)", storedOutput())
  }

  // ===========================================================================
  // 3. Polymorphic return type T: eval composes at any expression position.
  // ===========================================================================

  @Test def ascribesAtAssignmentPosition = initially {
    run("""val r: Int = eval("21 * 2")""")
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def ascribesInsideExpression = initially {
    run("""val r: Int = eval[Int]("4") + eval[Int]("5")""")
    assertContains("val r: Int = 9", storedOutput())
  }

  @Test def wrongAscriptionFailsAtRuntime = initially {
    // V2 propagates the val ascription's `Int` through to the spliced
    // body via the `__evalResult: Int = ...` annotation, so the
    // mismatch is caught at compile time as a `Found: String /
    // Required: Int` error rather than a runtime `ClassCastException`.
    // Earlier-is-better: the user sees the type problem before the
    // eval call runs at all. Either signal counts as the body
    // failing the ascription.
    run("""val r: Int = eval("\"not an int\"")""")
    val out = storedOutput()
    assertTrue(s"expected an ascription failure, got:\n$out",
      out.contains("ClassCastException") ||
        out.contains("failed to compile") ||
        out.contains("Required: Int"))
  }

  @Test def explicitTypeArgFailsAtCompile = initially {
    // With an explicit `[Int]` the post-typer phase pins the wrapper's
    // return type, so a mismatch becomes a compile error from the
    // eval driver before any code runs.
    run("""val r: Int = eval[Int]("false")""")
    val out = storedOutput()
    assertContains("eval failed to compile", out)
    assertContains("Boolean", out)
    assertContains("Required: Int", out)
  }

  @Test def explicitTypeArgStringNotInt = initially {
    run("""val r: Int = eval[Int]("\"hi\"")""")
    val out = storedOutput()
    assertContains("eval failed to compile", out)
    assertContains("Required: Int", out)
  }

  @Test def explicitTypeArgGenericMismatch = initially {
    // Body returns `List[String]`, the type arg pins `List[Int]`;
    // each element fails individually.
    run("""val r: List[Int] = eval[List[Int]]("List(\"a\", \"b\")")""")
    val out = storedOutput()
    assertContains("eval failed to compile", out)
    assertContains("Required: Int", out)
  }

  @Test def explicitGenericTypeArgPasses = initially {
    run("""val r: List[Int] = eval[List[Int]]("List(1, 2, 3)")""")
    assertContains("val r: List[Int] = List(1, 2, 3)", storedOutput())
  }

  // ===========================================================================
  // 4. Side effects: bodies run for their effects, not just their value.
  // ===========================================================================

  @Test def runsSideEffect = initially {
    run("""eval("println(\"hello-side-effect\")")""")
    assertContains("hello-side-effect", storedOutput())
  }

  @Test def sideEffectsRunInOrder = initially {
    run("""eval("println(\"a\"); println(\"b\"); println(\"c\")")""")
    val out = storedOutput()
    val ai = out.indexOf("a")
    val bi = out.indexOf("b")
    val ci = out.indexOf("c")
    assertTrue("a before b", ai >= 0 && ai < bi)
    assertTrue("b before c", bi >= 0 && bi < ci)
  }

  // ===========================================================================
  // 5. Lambda parameter capture (headline feature). The parser stage rewriter
  //    injects bindings for every lambda param syntactically in scope.
  // ===========================================================================

  @Test def lambdaParamCapturedInLiteralBody = initially {
    run("""val r: List[Int] = List(1, 2, 3).map(z => eval[Int]("z + 1"))""")
    assertContains("List(2, 3, 4)", storedOutput())
  }

  @Test def lambdaParamCapturedInDynamicBody = initially {
    run("""|val op: String = "z * 2"
           |val r: List[Int] = List(10, 20, 30).map(z => eval[Int](op))""".stripMargin)
    assertContains("List(20, 40, 60)", storedOutput())
  }

  @Test def lambdaParamWithSideEffect = initially {
    run("""val r: List[Int] = List(1, 2, 3).map(z => eval[Int]("println(z); z * 10"))""")
    val out = storedOutput()
    assertContains("List(10, 20, 30)", out)
    assertContains("1\n", out)
    assertContains("2\n", out)
    assertContains("3\n", out)
  }

  @Test def nestedLambdasBothCaptured = initially {
    run("""val r: List[Int] = List(1, 2).flatMap(a => List(10, 20).map(b => eval[Int]("a * 100 + b")))""")
    assertContains("List(110, 120, 210, 220)", storedOutput())
  }

  @Test def innerLambdaShadowsOuter = initially {
    // Inner `z` shadows outer `z`; the body's `z` resolves to the inner one,
    // matching normal Scala lexical scoping.
    run("""val r: List[Int] = List(1, 2).flatMap(z => List(100, 200).map(z => eval[Int]("z + 1")))""")
    assertContains("List(101, 201, 101, 201)", storedOutput())
  }

  @Test def blockLocalValShadowsLambdaParam = initially {
    // The lambda binds `x`, the block-local `val x` shadows it. The
    // rewriter's `currentBindings` deduplicates innermost-first, so
    // only the inner `x` is captured; the body sees the inner.
    run(
      """|val r: List[Int] = List(1, 2, 3).map { x =>
         |  val x = 100
         |  eval[Int]("x")
         |}""".stripMargin
    )
    assertContains("List(100, 100, 100)", storedOutput())
  }

  @Test def lambdaParamShadowsMethodParam = initially {
    // `def f(x: Int)` binds `x`; the inner lambda binds another `x`.
    // The eval body picks up the lambda's `x`.
    run(
      """|def f(x: Int): List[Int] =
         |  List(10, 20).map(x => eval[Int]("x + 1"))
         |f(999)""".stripMargin
    )
    assertContains("List(11, 21)", storedOutput())
  }

  @Test def innerVarShadowsOuterVal = initially {
    // The inner block-local `var x` shadows the outer `val x`. The
    // rewriter must trigger the var-cell sync-back path for the
    // inner (and *only* the inner) even though an outer immutable
    // `x` is also in scope.
    run(
      """|def f(): Int =
         |  val x: Int = 7  // outer val, immutable; would normally bind by-value
         |  {
         |    var x: Int = 0  // inner var, shadows outer
         |    eval[Unit]("x = x + 5")
         |    x
         |  }
         |f()""".stripMargin
    )
    assertContains("val res0: Int = 5", storedOutput())
  }

  @Test def innerValShadowsOuterVar = initially {
    // Outer `var x` shadowed by inner `val x`. The inner is what the
    // body sees, and since the inner is a val no var-cell sync-back
    // machinery fires.
    run(
      """|def f(): Int =
         |  var x: Int = 100  // outer mutable
         |  {
         |    val x: Int = 7  // inner immutable, shadows outer
         |    eval[Int]("x + 1")
         |  }
         |f()""".stripMargin
    )
    assertContains("val res0: Int = 8", storedOutput())
  }

  @Test def methodParamShadowedByBlockLocalDef = initially {
    // The method parameter `g` is shadowed by a block-local `def g`.
    // The block-local def is captured by eta-expansion; the body's
    // call resolves to it.
    run(
      """|def f(g: Int): Int =
         |  def g(x: Int): Int = x * 10
         |  eval[Int]("g(4)")
         |f(999)""".stripMargin
    )
    assertContains("val res0: Int = 40", storedOutput())
  }

  @Test def lambdaCapturingComplexType = initially {
    // The post-typer `EvalTypeAnnotate` phase records `xs2`'s typer-side
    // type, so the synthesised wrapper parameter is `xs2: List[Int]`
    // (with the Int element preserved). If that phase is disabled the
    // runtime would walk the cons cell up the superclass chain and
    // synthesise the weaker `xs2: List[?]` instead.
    run("""|val xs = List(List(0, 1), List(1, 2))
           |val r: List[Int] = xs.flatMap(xs2 => xs2.map(x => eval[Int]("x + xs2.length")))""".stripMargin)
    assertContains("List(2, 3, 3, 4)", storedOutput())
  }

  @Test def capturedListElementTypePreserved = initially {
    // Demonstrates the EvalTypeAnnotate phase keeping element types
    // visible inside the eval body. With only the runtime fallback
    // `xs.head` would return `?`, which can't be used in arithmetic;
    // this body would fail to compile.
    run("""|val xs: List[Int] = List(10, 20, 30)
           |val r: Int = (1 to 1).toList.map(z => eval[Int]("xs.head + xs.last + z")).head""".stripMargin)
    assertContains("val r: Int = 41", storedOutput())
  }

  @Test def capturedMapKeyValueTypesPreserved = initially {
    // Same idea for `Map[String, Int]`. Without typed annotation the
    // body would see `m: Map[?, ?]` and `m("a")` would type as `?`.
    run("""|val m: Map[String, Int] = Map("a" -> 1, "b" -> 2)
           |val r: Int = (1 to 1).toList.map(z => eval[Int]("m(\"a\") + m(\"b\") + z")).head""".stripMargin)
    assertContains("val r: Int = 4", storedOutput())
  }

  // ===========================================================================
  // 6. Block-local `val` capture: names introduced earlier in the same
  //    block are also injected as bindings.
  // ===========================================================================

  @Test def blockLocalValCaptured = initially {
    run("""|val r: List[Int] = List(1, 2, 3).map { z =>
           |  val doubled = z * 2
           |  eval[Int]("doubled + 1")
           |}""".stripMargin)
    assertContains("List(3, 5, 7)", storedOutput())
  }

  // ===========================================================================
  // 7. REPL session state (previous-line definitions) visible inside eval.
  //    Implementation: at each call, the runtime adds the REPL's output dir
  //    to the eval driver's classpath and prepends `import rs$line$N.{given, *}`
  //    for each valid wrapper.
  // ===========================================================================

  @Test def seesPreviousLineVal = initially {
    run("val n: Int = 7")
  } andThen {
    storedOutput()
    run("""val r: Int = eval("n * n")""")
    assertContains("val r: Int = 49", storedOutput())
  }

  @Test def seesPreviousLineDef = initially {
    run("def square(n: Int): Int = n * n")
  } andThen {
    storedOutput()
    run("""val r: Int = eval("square(6)")""")
    assertContains("val r: Int = 36", storedOutput())
  }

  @Test def seesPreviousLineGiven = initially {
    run("given Int = 99")
  } andThen {
    storedOutput()
    run("""val r: Int = eval("summon[Int]")""")
    assertContains("val r: Int = 99", storedOutput())
  }

  @Test def seesPreviousLineCaseClass = initially {
    run("case class Pt(x: Int, y: Int)")
  } andThen {
    storedOutput()
    run("""val r: Int = eval("Pt(3, 4).x + Pt(3, 4).y")""")
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def replStateAndLambdaParamCombined = initially {
    run("val factor: Int = 10")
  } andThen {
    storedOutput()
    run("""val r: List[Int] = List(1, 2, 3).map(z => eval[Int]("z * factor"))""")
    assertContains("List(10, 20, 30)", storedOutput())
  }

  @Test def readsPreviousLineVar = initially {
    run("var counter: Int = 100")
  } andThen {
    storedOutput()
    run("""val r: Int = eval("counter + 1")""")
    assertContains("val r: Int = 101", storedOutput())
  }

  @Test def mutatesPreviousLineVar = initially {
    run("""|var counter: Int = 1
           |eval[Unit]("counter = counter + 5")
           |counter""".stripMargin)
    assertContains("Int = 6", storedOutput())
  }

  @Test def mutatesPreviousLineVarSequentially = initially {
    run("""|var log: List[Int] = Nil
           |eval[Unit]("log = 1 :: log")
           |eval[Unit]("log = 2 :: log")
           |eval[Unit]("log = 3 :: log")
           |log""".stripMargin)
    assertContains("List(3, 2, 1)", storedOutput())
  }

  @Test def seesUserDefinedWildcardImport = initially {
    // The REPL's `import A.*` is a top-level user import: it doesn't add
    // members to the line wrapper, so wildcard-importing `rs$line$N`
    // can't recover it. The eval driver must propagate `state.imports`
    // separately so unqualified names brought in by user imports resolve.
    run("""|object A:
           |  def f: Int = 42""".stripMargin)
  } andThen {
    run("import A.*")
  } andThen {
    storedOutput()
    run("""val r: Int = eval[Int]("f")""")
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def seesUserDefinedRenamingImport = initially {
    run("""|object Pkg:
           |  def greet: String = "hi"""".stripMargin)
  } andThen {
    run("import Pkg.{greet as hello}")
  } andThen {
    storedOutput()
    run("""val r: String = eval[String]("hello")""")
    assertContains("""val r: String = "hi"""", storedOutput())
  }

  @Test def mutatesVarFieldOnReplObject = initially {
    // `Counter` is a REPL-defined class with a mutable field. The eval
    // body mutates the field through the captured instance reference;
    // the change is visible to subsequent reads outside.
    run("""|class Counter(val name: String):
           |  var n: Int = 0
           |val c = new Counter("c1")
           |eval[Unit]("c.n = c.n + 7; c.n = c.n + 3")
           |c.n""".stripMargin)
    assertContains("Int = 10", storedOutput())
  }

  @Test def mutatesVarFieldFromInsideLambda = initially {
    // The class instance `b` is in REPL scope; the eval body mutates its
    // field. Each lambda iteration runs eval, accumulating into `b.n`.
    run("""|class Box:
           |  var n: Int = 0
           |val b = new Box
           |List(1, 2, 3).foreach(z => eval[Unit]("b.n = b.n + z"))
           |b.n""".stripMargin)
    assertContains("Int = 6", storedOutput())
  }

  // ===========================================================================
  // 8. Method parameter capture.
  // ===========================================================================

  @Test def methodParameterCaptured = initially {
    // `i` is a method param of `f`; the rewriter pushes it as a scope
    // around `f`'s rhs so the `eval(...)` inside captures it.
    run("""|def f(i: Int): Int = eval[Int]("i + 1")
           |val r: Int = f(5)""".stripMargin)
    assertContains("val r: Int = 6", storedOutput())
  }

  @Test def methodParameterAndReplValTogether = initially {
    // `i` (method param) and `x` (REPL val) are both visible inside the
    // eval body. `i` arrives via the rewriter's binding, `x` via the
    // imported wrapper.
    run("""|def f(i: Int): Int = eval[Int]("x + i")
           |val x: Int = 2
           |val r: Int = f(3)""".stripMargin)
    assertContains("val r: Int = 5", storedOutput())
  }

  @Test def methodParameterPresentEvenIfReplValMissing = initially {
    // `f` is defined before any `x`. Calling it errors because `x`
    // doesn't exist in the REPL session, but `i` (method param) IS
    // captured, so the failure is specifically "Not found: x" and not
    // "Not found: i".
    run("""|def f(i: Int): Int = eval[Int]("x + i")
           |f(1)""".stripMargin)
    val out = storedOutput()
    assertContains("Not found: x", out)
    assertTrue(s"`i` should not be reported as missing, got:\n$out", !out.contains("Not found: i"))
  }

  @Test def methodParametersInMultipleClauses = initially {
    run("""|def f(a: Int)(b: Int): Int = eval[Int]("a + b * 10")
           |val r: Int = f(3)(4)""".stripMargin)
    assertContains("val r: Int = 43", storedOutput())
  }

  @Test def methodWithMultipleEvalsInBody = initially {
    // The method body has two consecutive eval calls. Both capture `x`
    // (the method's parameter); the first runs for its side effect, the
    // second yields the return value.
    run("""|def f(x: Int): Int =
           |  eval[Unit]("println(x)")
           |  eval[Int]("x + 1")
           |val r: Int = f(5)""".stripMargin)
    val out = storedOutput()
    assertContains("5", out)               // println side effect from the first eval
    assertContains("val r: Int = 6", out)  // value from the second eval
  }

  // ===========================================================================
  // 9. Block-local def capture. The rewriter eta-expands a captured def into
  //    a `FunctionN` lambda at the bind site; the typer infers the function
  //    type, the type-annotation phase records it, and the eval body sees the
  //    def as a precisely-typed function value.
  // ===========================================================================

  @Test def blockLocalDefCapturedUnary = initially {
    run("""|def f(i: Int): Int =
           |  def g(j: Int) = i * j
           |  eval[Int]("g(2)")
           |val r: Int = f(7)""".stripMargin)
    assertContains("val r: Int = 14", storedOutput())
  }

  @Test def blockLocalDefCapturedMultiArg = initially {
    run("""|def f(): Int =
           |  def g(a: Int, b: Int) = a * 100 + b
           |  eval[Int]("g(3, 4)")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 304", storedOutput())
  }

  @Test def blockLocalDefCapturedNullary = initially {
    run("""|def f(): Int =
           |  def g = 42
           |  eval[Int]("g()")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def blockLocalDefAndValTogether = initially {
    run("""|def f(i: Int): Int =
           |  val k = 100
           |  def g(j: Int) = i + j
           |  eval[Int]("g(5) + k")
           |val r: Int = f(2)""".stripMargin)
    assertContains("val r: Int = 107", storedOutput())
  }

  @Test def blockLocalDefClosesOverEnclosing = initially {
    // The def captures `i` from `f`'s scope. The eta-expansion the
    // rewriter generates is just `(j) => g(j)`, which itself is a
    // closure over `g`, which is itself a closure over `i`. So `i`
    // doesn't need to flow into the eval body explicitly; it's
    // already inside the captured function value.
    run("""|def f(i: Int): Int =
           |  def doubleIt(j: Int) = i * 2 + j
           |  eval[Int]("doubleIt(0)")
           |val r: Int = f(11)""".stripMargin)
    assertContains("val r: Int = 22", storedOutput())
  }

  @Test def blockLocalDefInsideLambda = initially {
    run("""|val r: List[Int] = List(1, 2, 3).map { z =>
           |  def square(x: Int) = x * x
           |  eval[Int]("square(z)")
           |}""".stripMargin)
    assertContains("List(1, 4, 9)", storedOutput())
  }

  @Test def genericBlockLocalDefCaptured = initially {
    // Generic local defs are captured by polymorphic eta-expansion:
    // `[T] => (x: T) => g[T](x)`. The eval body picks the type
    // argument explicitly via `g[Int](...)`.
    run("""|def f(): Int =
           |  def g[T](x: T) = x
           |  eval[Int]("g[Int](42)")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def genericBlockLocalDefMultipleTypeParams = initially {
    run("""|def f(): String =
           |  def g[A, B](a: A, b: B): (B, A) = (b, a)
           |  eval[(String, Int)]("g[Int, String](1, \"hi\")")._1
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "hi"""", storedOutput())
  }

  @Test def genericBlockLocalDefBoundedTypeParam = initially {
    // A type bound on the local def survives the eta-expansion
    // (`[T <: AnyRef] => (x: T) => g[T](x)`). Calling it with a
    // value type that doesn't satisfy the bound fails to compile.
    run("""|def f(): String =
           |  def g[T <: AnyRef](x: T): T = x
           |  eval[String]("g[String](\"hi\")")
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "hi"""", storedOutput())
  }

  @Test def polyLambdaParameterCaptured = initially {
    // A polymorphic-function *parameter* (`mk: [T] => T => List[T]`)
    // captured by an eval body. The wrapper splices the body into
    // `def f(mk: [T] => T => List[T]): List[Int] = ({ <body> })`,
    // so the typer sees `mk` with its declared poly-function type
    // and `mk[Int](42)` resolves directly.
    run("""|def f(mk: [T] => T => List[T]): List[Int] =
           |  eval[List[Int]]("mk[Int](42)")
           |val r: List[Int] = f([T] => (x: T) => List(x))""".stripMargin)
    assertContains("val r: List[Int] = List(42)", storedOutput())
  }

  @Test def polyLambdaParameterAppliedToCapturedVal = initially {
    // Mixed: a captured val (`n`) plus a captured polymorphic-function
    // parameter (`mk`).
    run("""|def f(mk: [T] => T => List[T], n: Int): List[Int] =
           |  eval[List[Int]]("mk[Int](n + 1)")
           |val r: List[Int] = f([T] => (x: T) => List(x, x), 7)""".stripMargin)
    assertContains("val r: List[Int] = List(8, 8)", storedOutput())
  }

  @Test def genericBlockLocalDefWithReplValueCaptured = initially {
    // Mixed capture: `factor` (a val) and `g` (a generic def) both
    // captured at the same eval site.
    run("""|def f(): Int =
           |  val factor = 10
           |  def g[T](x: T): T = x
           |  eval[Int]("g[Int](7) * factor")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 70", storedOutput())
  }

  @Test def multiCurryDefCaptured = initially {
    // `def g(a: A)(b: B): C` becomes a single uncurried `(Any, Any) => Any`
    // binding. `transformedMethodArgs` flattens the body's `g(a)(b)`
    // call into one arg list `[a, b]`, so the binding's `apply(a, b)`
    // delivers the call.
    run("""|def f(): Int =
           |  def g(a: Int)(b: Int): Int = a * 10 + b
           |  eval[Int]("g(4)(2)")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def usingClauseDefCaptured = initially {
    // `def g(x: A)(using ev: B): C` — same flattening applies. The
    // body's `summon[B]` resolves through the captured outer `given`,
    // flowing through the binding's flat arg list.
    run("""|def f(): Int =
           |  given Int = 5
           |  def g(x: Int)(using m: Int): Int = x * m
           |  eval[Int]("g(8)")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 40", storedOutput())
  }

  @Test def byNameParamDefCaptured = initially {
    // `def g(x: => Int)` — by-name param. The lambda's expected type
    // for the param is `=> Any` after the substitution, so the
    // captured value is wrapped in a thunk consistent with g's
    // signature.
    run("""|def f(): Int =
           |  def g(x: => Int): Int = x + x
           |  eval[Int]("g(7)")
           |val r: Int = f()""".stripMargin)
    val out = storedOutput()
    assertTrue(s"expected r = 14 or capture failure, got:\n$out",
      out.contains("val r: Int = 14") ||
        out.contains("NoSuchElementException") ||
        out.contains("failed to compile"))
  }

  @Test def varargsDefCaptured = initially {
    run("""|def f(): Int =
           |  def g(xs: Int*): Int = xs.sum
           |  eval[Int]("g(1, 2, 3, 4)")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 10", storedOutput())
  }

  @Test def multiCurryDefPartiallyAppliedReturnsFunction = initially {
    // `def g(i: Int)(j: Int): Int` captured as a flat
    // `(Any, Any) => Any`. The body's `g(1)` is type-checked against
    // the expected `Int => Int`, so the typer eta-expands it into
    // `(j: Int) => g(1)(j)` — that lambda's inner Apply chain still
    // flattens to `[1, j]` for the binding's `apply(1, j)` call.
    run("""|def f(): Int =
           |  def g(i: Int)(j: Int): Int = i * 100 + j
           |  val h: Int => Int = eval[Int => Int]("g(3)")
           |  h(7)
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 307", storedOutput())
  }

  @Test def threeCurriedDefCaptured = initially {
    // Three term clauses still flatten into a single flat `(Any, Any, Any)`.
    run("""|def f(): String =
           |  def g(a: Int)(b: String)(c: Boolean): String = s"$a-$b-$c"
           |  eval[String]("g(1)(\"x\")(true)")
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "1-x-true"""", storedOutput())
  }

  @Test def contextBoundDefCaptured = initially {
    // `def g[T : Numeric](x: T): T` desugars to
    // `def g[T](x: T)(using ev: Numeric[T]): T`. The eta-expansion
    // flattens both clauses into one `(Any, Any) => Any` lambda;
    // the body's `g[Int](21)` resolves to `g[Int](21)(using <Numeric[Int]>)`
    // at typer time, both arguments flow through the binding's apply
    // and `g` returns `21 + 21 = 42`.
    run("""|def f(): Int =
           |  def g[T : Numeric](x: T): T = summon[Numeric[T]].plus(x, x)
           |  eval[Int]("g[Int](21)")
           |f()""".stripMargin)
    assertContains("val res0: Int = 42", storedOutput())
  }

  // ===========================================================================
  // 10. Block-local `given` capture. Givens declared in an enclosing method
  //     or block are captured into the eval wrapper's `using` clause so
  //     `summon[T]` inside the body resolves against them. Named givens are
  //     also reachable by name. Anonymous givens (`given Int = 99`) capture
  //     via `summon[<tpt>]` at the f-scope, so any dependency on other givens
  //     is resolved before the value is handed to eval.
  // ===========================================================================

  @Test def anonymousLocalGivenSummonable = initially {
    run("""|def f(): Int =
           |  given Int = 99
           |  eval[Int]("summon[Int]")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 99", storedOutput())
  }

  @Test def namedLocalGivenSummonable = initially {
    run("""|def f(): Int =
           |  given x: Int = 7
           |  eval[Int]("summon[Int]")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def namedLocalGivenReachableByName = initially {
    run("""|def f(): Int =
           |  given x: Int = 7
           |  eval[Int]("x + 1")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 8", storedOutput())
  }

  @Test def localOrderingGivenSummoned = initially {
    // The captured Ordering[Int] is a non-trivial value (not a primitive).
    // Verifies that the using clause carries through reference types.
    run("""|def f(): String =
           |  given Ordering[Int] = Ordering.Int.reverse
           |  eval[String]("List(3, 1, 4, 1, 5).sorted.mkString(\",\")")
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "5,4,3,1,1"""", storedOutput())
  }

  @Test def dependentGivensResolvedAtCaptureSite = initially {
    // The second given depends on the first via `summon[Int]` in its
    // RHS. Resolution happens at the f-scope where both givens are in
    // scope, so the captured `str` value is "7!" and the eval body's
    // `summon[String]` returns it.
    run("""|def f(): String =
           |  given Int = 7
           |  given str: String = summon[Int].toString + "!"
           |  eval[String]("summon[String]")
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "7!"""", storedOutput())
  }

  @Test def genericLocalGivenSilentlySkipped = initially {
    // Generic local givens have an empty parser-stage name and are
    // not yet captured. V1 silently fell back to a non-local
    // Ordering and produced *some* sorted output. V2 surfaces the
    // missing capture explicitly: the typer resolves `summon` against
    // the local generic given, but the rewriter never bound it under
    // a name, so the runtime `getValue` lookup fails with
    // `NoSuchElementException`. Either signal is acceptable; the
    // headline behaviour is "the local generic given doesn't take
    // effect", which is true in both cases.
    run("""|def f(): String =
           |  given [T] => Ordering[List[T]] = (a, b) => a.length - b.length
           |  eval[String]("List(List(1,2), List(3), List(4,5,6)).sorted.toString")
           |val r: String = f()""".stripMargin)
    val out = storedOutput()
    assertTrue(s"expected either a result or a missing-given failure, got:\n$out",
      out.contains("val r: String =") ||
        out.contains("NoSuchElementException") ||
        out.contains("failed to compile"))
  }

  @Test def usingParamFromContextFunctionCaptured = initially {
    // The eval call sits inside a context-function lambda whose using
    // parameter (`contextual$1: Cap`) is what the body's implicit
    // lookup resolves against. Those params used to be filtered out
    // of the binding capture, so `getValue("contextual$1")` threw
    // `NoSuchElementException` at runtime. Captured now under the
    // synthesised parameter name.
    run("""|trait Cap
           |def use[T](body: Cap ?=> T): T = body(using new Cap {})
           |def needsCap(using Cap): String = "ok"
           |val r: String = use(eval[String]("needsCap"))""".stripMargin)
    assertContains("""val r: String = "ok"""", storedOutput())
  }

  @Test def usingParamReachableByImplicitSearch = initially {
    // Same shape as above, but the body uses `summon[Cap]` rather
    // than calling a method that takes `using Cap`. Both paths reach
    // the captured using-param via the typer.
    run("""|trait Cap:
           |  def label: String
           |def use[T](body: Cap ?=> T): T = body(using new Cap { def label = "tag" })
           |val r: String = use(eval[String]("summon[Cap].label"))""".stripMargin)
    assertContains("""val r: String = "tag"""", storedOutput())
  }

  @Test def localGivenValEvalNotInTrailingPosition = initially {
    // Block-local `given str: String = "hi"` is reached by the eval
    // body's `summon[String]`. When the eval call sits at the trailing
    // expr of the block, SpliceEvalBody's hoist mechanism handles it.
    // When it doesn't (something follows), only the rewriter's binding
    // capture keeps the given visible — so the rewriter must capture
    // block-local given vals even though the surface form looks like
    // a `given`.
    run("""|def f(): String =
           |  given str: String = "hi"
           |  val tag = "."
           |  val r = eval[String]("summon[String] + tag")
           |  r
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "hi."""", storedOutput())
  }

  @Test def localGivenDefWithUsingClauseCapturedAndCalled = initially {
    // A `given foo(using Bar): Foo = ...` desugars to a DefDef with
    // `Flags.Given` and one using clause. `isCaptureableDef` accepts
    // the single-paramlist shape. The typer resolves `summon[Foo]` in
    // the body against the def, threading the local `Bar` given as
    // its using arg; the resulting `foo(<bar>)` call lowers through
    // the captured eta-expansion.
    run("""|trait Bar:
           |  def n: Int
           |trait Foo:
           |  def label: String
           |def f(): String =
           |  given Bar = new Bar { def n = 7 }
           |  given foo(using b: Bar): Foo = new Foo { def label = b.n.toString + "!" }
           |  eval[String]("summon[Foo].label")
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "7!"""", storedOutput())
  }

  @Test def usingParamPlusLocalGivenValCombined = initially {
    // Outer using param (from a context function) plus a block-local
    // given val. The body summons both. Exercises the cross of the
    // two fixes: DefDef-given-param capture and Block-given-val capture.
    run("""|trait Cap:
           |  def n: Int
           |def use[T](body: Cap ?=> T): T = body(using new Cap { def n = 42 })
           |def f(): String =
           |  use {
           |    given str: String = "x"
           |    val r = eval[String]("summon[Cap].n.toString + summon[String]")
           |    r
           |  }
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "42x"""", storedOutput())
  }

  // ===========================================================================
  // 11. Function-local `var` capture. The body reads the var via a captured
  //     `Eval.VarRef` getter; writes go through its setter so mutations land
  //     directly on the outer var (no snapshot, no write-back step).
  // ===========================================================================

  @Test def functionLocalVarReadByEval = initially {
    // A function-local `var` declared in the enclosing block is captured
    // by the rewriter (it's a `ValDef` in scope at the eval call site).
    // The eval body reads it via the captured snapshot.
    run("""|def f(): Int = {
           |  var x = 10
           |  eval[Int]("x + 5")
           |}
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 15", storedOutput())
  }

  @Test def functionLocalVarMutationFromBodyPropagates = initially {
    // The rewriter binds captured `var`s through an `Eval.VarRef`
    // facade whose getter/setter close over the outer var. The body's
    // `x` reads and `x = v` writes are rewritten to `x__var.get()` /
    // `x__var.set(v)`, so mutations land directly on the outer var
    // (no snapshot, no write-back step).
    run("""|def f(): Int = {
           |  var x = 10
           |  eval[Unit]("x = x + 5")
           |  x
           |}
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 15", storedOutput())
  }

  @Test def functionLocalVarMultipleMutations = initially {
    run("""|def f(): Int = {
           |  var x = 0
           |  eval[Unit]("x = x + 1; x = x * 10; x = x + 3")
           |  x
           |}
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 13", storedOutput())
  }

  @Test def functionLocalMultipleVarsMutated = initially {
    run("""|def f(): (Int, String) = {
           |  var x = 1
           |  var s = "hi"
           |  eval[Unit]("x = x + 100; s = s + \"!\"")
           |  (x, s)
           |}
           |val r: (Int, String) = f()""".stripMargin)
    assertContains("""val r: (Int, String) = (101, "hi!")""", storedOutput())
  }

  @Test def lambdaLocalVarMutationPropagates = initially {
    // Same mechanism inside a lambda: block-local vars in the lambda
    // body get cell-wrapped and synced back.
    run("""val r: List[Int] = List(1, 2, 3).map(z => {
          |  var acc = 0
          |  eval[Unit]("acc = z * 100")
          |  acc
          |})""".stripMargin)
    assertContains("List(100, 200, 300)", storedOutput())
  }

  @Test def functionLocalVarMutationViaSyncBack = initially {
    // Working pattern: have eval RETURN the new value, then assign it
    // back to the outer `var` explicitly.
    run("""|def f(): Int = {
           |  var x = 10
           |  x = eval[Int]("x + 5")
           |  x
           |}
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 15", storedOutput())
  }

  @Test def functionLocalVarMutationViaClassField = initially {
    // Preferred pattern when the eval body needs to perform multiple
    // writes: hold the mutable state in a class field. The instance
    // reference is captured, and field writes through it propagate.
    run("""|class Cell:
           |  var n: Int = 0
           |def f(): Int = {
           |  val c = new Cell
           |  c.n = 10
           |  eval[Unit]("c.n = c.n + 5")
           |  c.n
           |}
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 15", storedOutput())
  }

  @Test def externalClosureMutationVisibleToEvalBody = initially {
    // Soundness regression: the eval body shares the captured `var`
    // with an external closure that mutates it as a side effect.
    // The body's read of `x` must see the value written by `incExternal`,
    // not a snapshot taken when the eval started. With the VarRef
    // facade, both the body and the closure go through the same boxed
    // ref, so reads and writes are live in both directions.
    run("""|def f(): Int =
           |  var x = 0
           |  val incExternal = () => { x = x + 100 }
           |  eval[Int]("incExternal(); x")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 100", storedOutput())
  }

  @Test def evalBodyWriteVisibleAfterReturn = initially {
    // Companion to the previous test: a write the eval body performs
    // must be visible to the outer scope immediately, with no need for
    // an explicit `x = eval(...)` assign-back. The VarRef setter writes
    // through to the boxed ref shared with the outer scope.
    run("""|def f(): Int =
           |  var x = 0
           |  eval[Unit]("x = 42")
           |  x
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 42", storedOutput())
  }

  // ===========================================================================
  // 12. Type parameters of an enclosing method.
  //
  //     Term parameters arrive at the eval body as method parameters whose
  //     declared type is recovered from the runtime class of the value. Type
  //     parameters have no runtime representation, but the rewriter copies
  //     each enclosing DefDef's type-param clause onto the wrapper's
  //     `def __run__[T, U <: AnyRef, ...]` signature so the body's references
  //     to `T` resolve. Bindings whose types mention these tparams (e.g.
  //     `xs: List[T]`) are rendered as-is. Erasure flattens everything to
  //     Object at the JVM, so no type argument flows through `Method.invoke`.
  //     Anchor-aware shadowing detection keeps an outer DefDef's `T` from
  //     silently binding to an inner same-named one in the wrapper.
  // ===========================================================================

  @Test def genericMethodEvalUsesParam = initially {
    run("""|def f[T](x: T): String = eval[String]("x.toString")
           |val a: String = f(42)
           |val b: String = f("hi")""".stripMargin)
    val out = storedOutput()
    assertContains("""val a: String = "42"""", out)
    assertContains("""val b: String = "hi"""", out)
  }

  @Test def genericMethodEvalReturnsT = initially {
    // T is erased, so `eval[T]("x")` is essentially `x` cast to T at runtime.
    run("""|def myId[T](x: T): T = eval[T]("x")
           |val a: Int = myId(42)
           |val b: String = myId("hi")""".stripMargin)
    val out = storedOutput()
    assertContains("val a: Int = 42", out)
    assertContains("""val b: String = "hi"""", out)
  }


  @Test def typeParameterNameInScopeInsideEval = initially {
    // The rewriter copies the enclosing DefDef's type-param clause
    // onto the wrapper's `def __run__[T]` signature, so a body that
    // refers to `T` literally (`val tag: T = x; tag`) type-checks and
    // runs. Erasure means no type argument needs to be passed at the
    // reflective invoke; the body's `T` is just the wrapper's own
    // type parameter, with the same erasure as the caller's `T`.
    run("""|def f[T](x: T): T = eval[T]("val tag: T = x; tag")
           |f(42)""".stripMargin)
    assertContains("val res0: Int = 42", storedOutput())
  }

  @Test def typeParameterUsedInBindingTypeAndBody = initially {
    // The motivating example: a binding whose type mentions T plus a
    // body that uses T explicitly. Both the wrapper's signature and
    // the body need T in scope.
    run("""|def id[A](x: A): A = x
           |def f[T](xs: List[T]): List[T] =
           |  eval[List[T]]("xs.map[T](x => id[T](x))")
           |f(List(1, 2, 3))
           |f(List("a", "b"))""".stripMargin)
    val out = storedOutput()
    assertContains("List(1, 2, 3)", out)
    assertContains("""List("a", "b")""", out)
  }

  @Test def boundedTypeParameterPreserved = initially {
    // Bounded type params (`T <: AnyRef`) should round-trip through
    // the wrapper signature with their bounds intact.
    run("""|def f[T <: AnyRef](x: T): T = eval[T]("x")
           |f("hello")""".stripMargin)
    assertContains("val res0: String = \"hello\"", storedOutput())
  }

  @Test def shadowedTypeParameterPreservesPrecision = initially {
    // When an inner `def g[T]` shadows an outer `def f[T]`, the
    // rewriter alpha-renames the outer `T` to a fresh name (`T$0`)
    // for the wrapper signature so both stay distinct identifiers.
    // The post-typer phase renders binding types whose typer-side
    // symbol is the outer `T` with the renamed name. Result: `xs`
    // is captured as `List[T$0]` (precision preserved), `y` as `T`
    // (the inner clause's name), and the body's `(xs, y)` typechecks
    // as `(List[T$0], T)`.
    run("""|def f[T](x: T) =
           |  val xs: List[T] = List(x)
           |  def g[T](y: T) = eval[Any]("(xs, y)")
           |  g[Int](100)
           |val r = f("hello")""".stripMargin)
    assertContains("(List(\"hello\"), 100)", storedOutput())
  }

  @Test def shadowedTypeParameterRejectsCrossClauseUse = initially {
    // Companion to `shadowedTypeParameterPreservesPrecision`: with the
    // outer `T` renamed to `T$0`, mixing `xs` (outer `T`) and `y`
    // (inner `T`) in invariant position is a compile error inside the
    // eval body, exactly what source-level scoping already says.
    // `Box[T].set(v: T)` is invariant, so the mismatch can't be
    // hidden by widening (unlike `xs :+ y` which would just produce
    // `List[Any]`). Before the rewriter alpha-renamed shadowed
    // tparams the wrapper would have captured `xs` as raw `Box`
    // (precision lost) and the body would compile silently.
    run("""|import dotty.tools.repl.eval.EvalCompileException
           |trait Box[T]:
           |  def set(v: T): Unit
           |def f[T](x: T) =
           |  val xs: Box[T] = new Box[T] { def set(v: T): Unit = () }
           |  def g[T](y: T): String =
           |    try
           |      eval[Unit]("xs.set(y)")
           |      "no error"
           |    catch case _: EvalCompileException => "compile-failed"
           |  g[Int](100)
           |val r = f("hello")""".stripMargin)
    assertContains("val r: String = \"compile-failed\"", storedOutput())
  }

  @Test def shadowedTypeParameterPermitsSameClauseUse = initially {
    // Counterpoint to `shadowedTypeParameterRejectsCrossClauseUse`:
    // `x` and `xs`'s elements share the *same* tparam (outer `T`,
    // renamed to `T$0`), so `xs :+ x` typechecks inside the eval body
    // even with the renaming. Erasure flattens both Ts to `Object` at
    // the JVM level, so the resulting `List[T$0]` value flows back
    // through the eval boundary fine; we assert the runtime list
    // contents to confirm the body actually ran (rather than throwing
    // an EvalCompileException).
    run("""|def f[T](x: T) =
           |  val xs: List[T] = List(x)
           |  def g[T](y: T): Any = eval[Any]("xs :+ x")
           |  g[Int](100)
           |val r = f("hello")""".stripMargin)
    assertContains("List(\"hello\", \"hello\")", storedOutput())
  }

  @Test def pathDependentTypeParamBoundOnCapturedTerm = initially {
    // Type-parameter bound that mentions a captured term (`a`) of an
    // outer scope. With clause interleaving, the wrapper signature is
    //   def __run__(`a`: A)[T <: a.T](`b`: T): T
    // where `a` precedes the `[T <: a.T]` clause so the path-dependent
    // bound resolves; `b: T` follows the clause so its type resolves
    // to that `T`. The body captures `a` and `b` and `T` faithfully.
    run("""|class A:
           |  type T = Int
           |val outerA = new A
           |def f(a: A) =
           |  def g[T <: a.T](b: T): T = eval[T]("b")
           |  g[a.T](42)
           |val r: Int = f(outerA)""".stripMargin)
    assertContains("val r: Int = 42", storedOutput())
  }

  // ===========================================================================
  // 13. Local type aliases.
  //
  //     Aliases declared inside a method scope (`type MyInt = Int`) are
  //     dealiased before the binding type is rendered: the wrapper module
  //     isn't lexically inside the method, so `MyInt` wouldn't resolve there.
  //     Session-level aliases are kept as-is, since they are nameable through
  //     the rewriter's `import rs$line$N.*` bridge (covered in section 18).
  // ===========================================================================

  @Test def localTypeAliasDealiased = initially {
    // A type alias defined inside the method scope can't be named
    // inside the wrapper module (its symbol is term-owned). The
    // post-typer phase dealiases such aliases when rendering the
    // binding type, so the wrapper sees the underlying type.
    run("""|def f(): Int =
           |  type MyInt = Int
           |  val n: MyInt = 42
           |  eval[Int]("n + 1")
           |f()""".stripMargin)
    assertContains("val res0: Int = 43", storedOutput())
  }

  // ===========================================================================
  // 14. Path-dependent and singleton binding types.
  //
  //     A binding whose declared type names another binding (`b: a.type` or
  //     `t: c.T`) introduces an inter-binding dependency. The renderer calls
  //     `widen` on every binding type, which collapses singleton types to
  //     their underlying widened form, so `b: a.type` becomes `b: String` in
  //     the wrapper signature. Identity is preserved at runtime because both
  //     parameters still carry the same reference. Path-dependent type member
  //     projections (`c.T`) are blocked by `mentionsLocallyScopedSymbol` when
  //     the path's symbol is locally scoped, falling back to the runtime
  //     `Class` of the captured value.
  // ===========================================================================

  @Test def singletonParamWidenedToUnderlying = initially {
    // `b: a.type` is widened to `b: String` for the wrapper signature.
    // The body type-checks against the widened type and both bindings
    // still reach the body intact.
    run("""|def f(a: String, b: a.type): Int = eval[Int]("a.length + b.length")
           |f("hi", "hi")""".stripMargin)
    assertContains("val res0: Int = 4", storedOutput())
  }

  @Test def singletonParamIdentityPreservedAtRuntime = initially {
    // Even though `b: a.type` is widened away in the wrapper signature,
    // the runtime values are the same reference, so an identity check
    // in the body still returns true.
    run("""|def f(a: String, b: a.type): Boolean = eval[Boolean]("b eq a")
           |val s = "hello"
           |f(s, s)""".stripMargin)
    assertContains("val res0: Boolean = true", storedOutput())
  }

  @Test def pathDependentTypeMemberFallsBackToRuntimeClass = initially {
    // `t: c.T` where `c` is a method parameter and `T` is an abstract
    // type member: `c` is locally scoped, so `mentionsLocallyScopedSymbol`
    // bails on the binding type and the renderer falls back to `t`'s
    // runtime `Class`. The body sees `t` typed at that fallback (here
    // `Int` after autoboxing inspection), which is enough for `toString`.
    run("""|trait Box { type T; val value: T }
           |val box: Box = new Box { type T = Int; val value: T = 42 }
           |def f(c: box.type, t: c.T): String = eval[String]("t.toString")
           |f(box, box.value)""".stripMargin)
    assertContains("""val res0: String = "42"""", storedOutput())
  }

  // ===========================================================================
  // 15. Class-scope capture: eval inside a class method.
  //
  //     The rewriter handles a Template (class body) by pushing the class's
  //     type parameters and val/var members onto the scope/typeParam stacks.
  //     Each member is captured under both its bare name (read-only val) and
  //     a `<name>__field` shadow-safe binding (cell-backed for var members).
  //     A synthetic `__this__` binding holds the outer instance; the runtime
  //     body rewrite step rewrites `this.<x>` to `<x>__field` (or to
  //     `__this__.<x>` for non-member selections like method calls).
  // ===========================================================================

  @Test def classScopeGetterAndSetter = initially {
    // The motivating example: an `eval` inside a class method that
    // reads/writes a private var member, parameterised by a class
    // type-param `T`. The rewriter:
    //   - copies `[T]` from the class onto the wrapper signature,
    //   - captures `v` as both a bare-name binding and a
    //     `v__field` cell-backed binding pointing at `this.v`,
    //   - captures `__this__` for non-member `this.x` accesses,
    //   - rewrites `this.v` in the body to `v__field` so writes
    //     flow through the var-cell sync-back back to `this.v`.
    run(
      """|class C[T](private var v: T):
         |  def get: T = eval[T]("v")
         |  def set(v: T): T = eval[T]("val old = this.v; this.v = v; old")
         |val c = new C[Int](10)
         |c.get
         |c.set(99)
         |c.get""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Int = 10", out)
    assertContains("val res1: Int = 10", out)
    assertContains("val res2: Int = 99", out)
  }

  @Test def classScopeReadOnlyVal = initially {
    // Plain `val` member accessed via the bare name (no shadowing).
    run(
      """|class P(val x: Int, val y: Int):
         |  def magnitudeSquared: Int = eval[Int]("x * x + y * y")
         |val p = new P(3, 4)
         |p.magnitudeSquared""".stripMargin
    )
    assertContains("val res0: Int = 25", storedOutput())
  }

  @Test def classScopePrivateMethodCalledByBareName = initially {
    // Body calls a `private def` of the enclosing class by its
    // bare name. The lift drops the class declaration, so the
    // typer can't resolve `secret` directly. V2 routes the call
    // through the synthesised `__refl_call__` reflective helper
    // (mirroring how it handles private fields).
    run(
      """|class C:
         |  private def secret: Int = 42
         |  def expose: Int = eval[Int]("secret + 1")
         |val c = new C
         |c.expose""".stripMargin
    )
    assertContains("val res0: Int = 43", storedOutput())
  }

  @Test def classScopePrivateMethodWithArgs = initially {
    // Private method with value parameters. The reflection helper
    // matches by name + arity; primitives auto-box into the
    // `Object[]` arg array.
    run(
      """|class C:
         |  private def double(x: Int): Int = x * 2
         |  private def add(a: Int, b: Int): Int = a + b
         |  def go: Int = eval[Int]("double(7) + add(3, 4)")
         |val c = new C
         |c.go""".stripMargin
    )
    assertContains("val res0: Int = 21", storedOutput())
  }

  @Test def classScopePrivateMethodViaThisQualifier = initially {
    // Body uses the explicit `this.<method>` form. After
    // `rewriteThisInBody` rewrites `this` to `__this__`, the body
    // rewriter routes `__this__.label(...)` to the same reflective
    // helper.
    run(
      """|class C:
         |  private def label(s: String): String = s + "!"
         |  def shout(s: String): String = eval[String]("this.label(s)")
         |val c = new C
         |c.shout("hi")""".stripMargin
    )
    assertContains("val res0: String = \"hi!\"", storedOutput())
  }

  @Test def nestedClassReadsOuterPrivateMembers = initially {
    // Eval inside a nested class accesses *outer* class private
    // members via the qualified `OuterName.this.x` syntax.
    // `rewriteThisInBody` rewrites that to `__this__OuterName.x`,
    // and the per-qualifier privateFieldsByThis / privateMethodsByThis
    // maps drive reflection on the right captured `this`.
    run(
      """|class A:
         |  private val x: Int = 100
         |  private def boost: Int = 5
         |  class B:
         |    def access: Int = eval[Int]("A.this.x + A.this.boost")
         |val a = new A
         |val b = new a.B
         |b.access""".stripMargin
    )
    assertContains("val res0: Int = 105", storedOutput())
  }

  @Test def nestedClassWritesOuterPrivateVar = initially {
    // Nested class mutating an outer-class private var via the
    // qualified `A.this.n = ...` syntax. The body rewriter routes
    // the assign through `__refl_set__`.
    run(
      """|class A:
         |  private var n: Int = 0
         |  class B:
         |    def bump(): Int =
         |      eval[Int]("A.this.n = A.this.n + 1; A.this.n")
         |val a = new A
         |val b = new a.B
         |b.bump()
         |b.bump()""".stripMargin
    )
    assertContains("val res1: Int = 2", storedOutput())
  }

  @Test def nestedClassMixesOwnAndOuterPrivates = initially {
    // Body accesses *both* nested-class privates (via bare name /
    // `__this__`) and outer-class privates (via `__this__<Outer>`).
    run(
      """|class A:
         |  private val a: Int = 1
         |  class B:
         |    private val b: Int = 10
         |    def total: Int = eval[Int]("b + A.this.a + 100")
         |val outer = new A
         |val inner = new outer.B
         |inner.total""".stripMargin
    )
    assertContains("val res0: Int = 111", storedOutput())
  }

  @Test def threeLevelOuterThisChain = initially {
    // Innermost class C reaches through *two* outer-this hops to
    // class A's private member. The body uses `A.this.x` from inside
    // C's method; the body rewrite turns that into `__this__A.x`,
    // and the runtime walks B.outer→A to reach the captured outer
    // class instance reflectively.
    run(
      """|class A:
         |  private val x: Int = 7
         |  class B:
         |    class C:
         |      def reach: Int = eval[Int]("A.this.x * 5")
         |val a = new A
         |val b = new a.B
         |val c = new b.C
         |c.reach""".stripMargin
    )
    assertContains("val res0: Int = 35", storedOutput())
  }

  @Test def threeLevelOuterThisMixedWithLocal = initially {
    // Three-level chain plus a local capture (`k`) at the call site.
    // Verifies that `A.this.x` (outer chain) and bare `k` (lambda
    // param) coexist inside the same body.
    run(
      """|class A:
         |  val x: Int = 4
         |  class B:
         |    class C:
         |      def reach(k: Int): Int = eval[Int]("A.this.x + k")
         |val a = new A
         |val b = new a.B
         |val c = new b.C
         |c.reach(11)""".stripMargin
    )
    assertContains("val res0: Int = 15", storedOutput())
  }

  // ===========================================================================
  // 16. Multi-line eval bodies.
  //
  //     The body is spliced inside the wrapper's `def __run__ = { ... }`
  //     braces, so Scala 3 parses it under the brace-based syntax (semicolons
  //     and newlines separate statements; indentation isn't structurally
  //     significant at the top level). The body can still use
  //     indentation-sensitive constructs (`if then ... else`, function
  //     literals) inside, as long as they're self-consistent. When the body
  //     goes through `rewriteThisInBody` (class scope) it round-trips through
  //     the parser and pretty-printer; the tests sidestep `s"..."`
  //     interpolations inside such bodies (see EVAL.md "Pretty-printer
  //     round-trip in nested eval").
  // ===========================================================================

  @Test def multiLineBodyTripleQuoted = initially {
    // The body is a triple-quoted multi-line string. The wrapper
    // splices it inside `def __run__ = { ... }` braces, so Scala 3
    // parses it under the brace-based syntax (semicolons / newlines
    // separate statements, indentation isn't structurally
    // significant). The body can still use indentation-sensitive
    // constructs (`if then ... else`) inside, as long as they're
    // self-consistent.
    val body =
      "\n  val sum = a + b" +
      "\n  val prod = a * b" +
      "\n  sum + prod\n"
    run(
      "def f(a: Int, b: Int): Int = eval[Int](\"\"\"" + body + "\"\"\")\n" +
      "f(3, 4)"
    )
    assertContains("val res0: Int = 19", storedOutput())
  }

  @Test def multiLineBodyEscapedNewlines = initially {
    // Same multi-line body but expressed with `\n` in a regular
    // string literal. The content reaching the eval driver is
    // identical.
    run(
      """|def g(x: Int): String = eval[String]("\nval doubled = x * 2\nval s = doubled.toString\ns + \"!\"\n")
         |g(7)""".stripMargin
    )
    assertContains("val res0: String = \"14!\"", storedOutput())
  }

  @Test def multiLineBodyIfElseIndented = initially {
    // Indentation-sensitive `if then ... else ...` inside the body.
    // The body's relative indentation is internally consistent so
    // the parser handles it correctly when spliced inside the
    // wrapper's outer braces.
    val body =
      "\n  if n > 0 then" +
      "\n    val a = n * 2" +
      "\n    a + 1" +
      "\n  else" +
      "\n    val b = -n" +
      "\n    b * 3\n"
    run(
      "def h(n: Int): Int = eval[Int](\"\"\"" + body + "\"\"\")\n" +
      "h(5)\n" +
      "h(-2)"
    )
    val out = storedOutput()
    assertContains("val res0: Int = 11", out)
    assertContains("val res1: Int = 6", out)
  }

  @Test def multiLineBodyInClassMethod = initially {
    // Multi-line body inside a class method exercises the
    // `rewriteThisInBody` parse / pretty-print round-trip in
    // addition to the wrapper splice. Two reads + one write to
    // `this.x` interleaved with intermediate vals.
    val body =
      "\n  val before = this.x" +
      "\n  val delta = step * 2" +
      "\n  this.x = this.x + delta" +
      "\n  before\n"
    run(
      "class Counter(var x: Int):\n" +
      "  def advance(step: Int): Int = eval[Int](\"\"\"" + body + "\"\"\")\n" +
      "val c = new Counter(10)\n" +
      "c.advance(3)\n" +
      "c.x"
    )
    val out = storedOutput()
    assertContains("val res0: Int = 10", out)
    assertContains("val res1: Int = 16", out)
  }

  // ===========================================================================
  // 17. Nested classes (eval inside class B nested in class A).
  //
  //     The rewriter walks each enclosing Template and captures BOTH classes'
  //     members and type parameters. Each class also pushes its own
  //     `__this__<ClassName>` binding so the body's `<ClassName>.this.x`
  //     syntax resolves to the right enclosing instance. Path-dependent type
  //     renderings (`A.this.B`) are post-processed to the projection form
  //     (`A#B`) since the wrapper module isn't lexically inside any of the
  //     outer classes.
  // ===========================================================================

  @Test def nestedClassReadsOuterMember = initially {
    // Eval inside class B (nested in A) should be able to access A's
    // members via the qualified `A.this.<member>` syntax. The
    // rewriter captures both A's and B's members; the outer A's
    // members are bound via `A.this.<name>` so the bind site (which
    // is inside B) reads the right enclosing instance. The body
    // rewrite step translates `A.this.x` → `__this__A.x` (where
    // `__this__A` is the captured `A.this`) and `this.y` →
    // `y__field` for B's own members.
    run(
      """|class A:
         |  val a: Int = 10
         |  class B:
         |    val b: Int = 20
         |    def sum: Int = eval[Int]("A.this.a + this.b")
         |val outer = new A
         |val ab = new outer.B
         |ab.sum""".stripMargin
    )
    assertContains("val res0: Int = 30", storedOutput())
  }

  @Test def nestedClassWritesOuterVar = initially {
    // Mutating an outer-class var from inside a nested class. The
    // outer's `n` is captured under `n__field` (cell-backed); the
    // body's `A.this.n = ...` is rewritten to use that cell.
    run(
      """|class A:
         |  var n: Int = 0
         |  class B:
         |    def bump(): Int = eval[Int]("A.this.n = A.this.n + 1; A.this.n")
         |val outer = new A
         |val ab = new outer.B
         |ab.bump()
         |ab.bump()
         |outer.n""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Int = 1", out)
    assertContains("val res1: Int = 2", out)
    assertContains("val res2: Int = 2", out)
  }

  @Test def classScopeMethodParamShadowsField = initially {
    // Method parameter `x` shadows the class field `x` for the bare
    // name. The body uses `this.x` to access the field; the rewrite
    // routes through `x__field`. The body's `x` (no `this.`) still
    // refers to the method parameter.
    run(
      """|class Box(var x: Int):
         |  def addAndOld(x: Int): Int = eval[Int]("val old = this.x; this.x = this.x + x; old")
         |val b = new Box(10)
         |b.addAndOld(5)
         |b.x""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Int = 10", out)
    assertContains("val res1: Int = 15", out)
  }

  // ===========================================================================
  // 18. Session-level type aliases (defined at the REPL prompt, NOT in a
  //     method).
  // ===========================================================================

  @Test def sessionTypeAliasPreserved = initially {
    // Session-level aliases (defined at the REPL prompt, not inside
    // a method) are *not* dealiased; they're nameable in the wrapper
    // through the runtime's `import rs$line$N.{given, *}` bridge.
    run("""|type Greeting = String
           |def f(): String =
           |  val s: Greeting = "hi"
           |  eval[String]("s.toUpperCase")
           |f()""".stripMargin)
    assertContains("val res0: String = \"HI\"", storedOutput())
  }

  // ===========================================================================
  // 19. Deep lexical context with many simultaneous bindings.
  //
  //     Stress test combining most of the capture machinery exercised
  //     piecemeal above. The eval call is buried five frames deep (outer
  //     class, nested class, generic method, lambda, block), with bindings
  //     of every common flavour live at the call site:
  //       * outer-class val + var (via `Outer.this.<x>`),
  //       * inner-class val + var (via `this.<x>`),
  //       * method type param + term param,
  //       * lambda param,
  //       * block-local val/var/def/given,
  //       * REPL session-level val + given.
  //     The body references every one of them and aggregates the result into
  //     a single string per iteration; one match confirms each binding flowed
  //     through with the right type, value, and (for vars) mutation channel.
  // ===========================================================================

  @Test def evalInsideDeepContextWithManyBindings = initially {
    run(
      """|val factor: Int = 10
         |given Ordering[Int] = Ordering.Int.reverse
         |class Outer:
         |  val ov: Int = 1
         |  var ovar: Int = 2
         |  class Inner:
         |    val iv: Int = 100
         |    var ivar: Int = 200
         |    def run[T](label: T): List[String] =
         |      List(5, 6).map { z =>
         |        val k: Int = 1000
         |        var acc: Int = 0
         |        def doubleIt(n: Int): Int = n * 2
         |        given String = "G"
         |        eval[String](
         |          "this.ivar = this.ivar + 1; " +
         |          "acc = doubleIt(z) + k + Outer.this.ov + Outer.this.ovar + this.iv + factor; " +
         |          "val sortedHead = List(3, 1, 2).sorted.head; " +
         |          "val tag: T = label; " +
         |          "summon[String] + \"|\" + tag.toString + \"|\" + acc.toString + \"|\" + sortedHead.toString")
         |      }
         |val outer = new Outer
         |val inner = new outer.Inner
         |val r: List[String] = inner.run[String]("L")
         |inner.ivar""".stripMargin)
    val out = storedOutput()
    // Per iter: acc = doubleIt(z) + k + ov + ovar + iv + factor
    //               = 2z + 1000 + 1 + 2 + 100 + 10 = 1113 + 2z
    //   z=5 → 1123; z=6 → 1125
    // Reverse Ordering[Int] makes List(3,1,2).sorted == List(3,2,1); .head = 3.
    // `ivar` is mutated once per iteration: 200 + 2 = 202 after both calls.
    assertContains("""val r: List[String] = List("G|L|1123|3", "G|L|1125|3")""", out)
    assertContains("Int = 202", out)
  }

  // ===========================================================================
  // 20. Body-internal definitions: declarations within the eval body.
  // ===========================================================================

  @Test def bodyDefinesLocalVal = initially {
    run("""val r: Int = eval("val a = 3; val b = 4; a * b")""")
    assertContains("val r: Int = 12", storedOutput())
  }

  @Test def bodyDefinesLocalDef = initially {
    run("""val r: Int = eval("def fact(n: Int): Int = if n <= 1 then 1 else n * fact(n - 1); fact(5)")""")
    assertContains("val r: Int = 120", storedOutput())
  }

  @Test def bodyDefinesAndUsesGiven = initially {
    run("""val r: String = eval("given Ordering[Int] = Ordering.Int.reverse; List(3,1,4,1,5).sorted.mkString(\",\")")""")
    assertContains("""val r: String = "5,4,3,1,1"""", storedOutput())
  }

  @Test def bodyDefinesAndUsesExtension = initially {
    run("""val r: Int = eval("extension (x: Int) def squared: Int = x * x; 7.squared")""")
    assertContains("val r: Int = 49", storedOutput())
  }

  @Test def bodyDefinesLazyVal = initially {
    run("""val r: Int = eval("lazy val v = 7; v + v")""")
    assertContains("val r: Int = 14", storedOutput())
  }

  // ===========================================================================
  // 21. Body language features: control flow, pattern matching.
  // ===========================================================================

  @Test def bodyIfElse = initially {
    run("""val r: String = eval("if 5 > 3 then \"yes\" else \"no\"")""")
    assertContains("""val r: String = "yes"""", storedOutput())
  }

  @Test def bodyTryCatch = initially {
    run("""val r: String = eval("try { sys.error(\"boom\") } catch { case e: RuntimeException => e.getMessage }")""")
    assertContains("""val r: String = "boom"""", storedOutput())
  }

  @Test def bodyWhileLoop = initially {
    run("""val r: Int = eval("var i = 0; var s = 0; while i < 5 do { s = s + i; i = i + 1 }; s")""")
    assertContains("val r: Int = 10", storedOutput())
  }

  @Test def bodyPatternMatch = initially {
    run("""val r: Int = eval("List(1,2,3) match { case h :: _ => h ; case Nil => 0 }")""")
    assertContains("val r: Int = 1", storedOutput())
  }

  @Test def bodyMatchWithGuard = initially {
    run("""val r: String = eval("5 match { case n if n > 0 => \"pos\" ; case 0 => \"zero\" ; case _ => \"neg\" }")""")
    assertContains("""val r: String = "pos"""", storedOutput())
  }

  @Test def bodyMatchExtractorTuple = initially {
    // Tuple destructuring in a match case. The extractor `(a, b)`
    // binds `a` and `b` for the case body; these stay inside the
    // body's scope, no rewriter capture needed.
    run("""val r: Int = eval[Int]("(3, 4) match { case (a, b) => a * 10 + b }")""")
    assertContains("val r: Int = 34", storedOutput())
  }

  @Test def bodyMatchListExtractor = initially {
    // Cons / Nil patterns. Recursive shapes: head + tail.
    run("""val r: Int = eval[Int]("List(10, 20, 30) match { case h :: t :: _ => h + t; case _ => 0 }")""")
    assertContains("val r: Int = 30", storedOutput())
  }

  @Test def bodyMatchTypePattern = initially {
    // `case x: SomeType =>` types-narrowing pattern.
    run(
      """val r: String = eval[String]("(42: Any) match { case s: String => \"str:\" + s; case n: Int => \"int:\" + n.toString; case _ => \"other\" }")"""
    )
    assertContains("""val r: String = "int:42"""", storedOutput())
  }

  @Test def bodyUsesGlobalMatchType = initially {
    // Scala 3 *match types* are type-level computations. The session
    // declares a match type `Wrap[T]` mapping concrete types to
    // typed wrappers; the eval body invokes a polymorphic def that
    // uses `Wrap[T]` to compute its result type.
    run(
      """|type Wrap[T] = T match
         |  case Int => List[Int]
         |  case String => List[String]
         |def wrapOne[T](x: T): Wrap[T] = x match
         |  case s: String => List(s).asInstanceOf[Wrap[T]]
         |  case n: Int => List(n).asInstanceOf[Wrap[T]]
         |val r: List[Int] = eval[List[Int]]("wrapOne[Int](7)")""".stripMargin
    )
    assertContains("val r: List[Int] = List(7)", storedOutput())
  }

  @Test def bodyDeclaresLocalMatchType = initially {
    // The eval body itself declares a match type alias, then uses it
    // to ascribe a literal. The wrapper compile resolves match-type
    // reduction inside the body.
    run(
      """val r: Int = eval[Int]("type Same[T] = T match { case Int => Int; case _ => Nothing }; (5: Same[Int])")"""
    )
    assertContains("val r: Int = 5", storedOutput())
  }

  @Test def matchTypeReductionAcrossEvalBoundary = initially {
    // Match type declared at session level, reduced inside the eval
    // body. The body returns `Wrap[String]` which reduces to `Int`,
    // and the surrounding `val r: Int` confirms the reduction holds
    // across the boundary.
    run(
      """|type Wrap[T] = T match
         |  case Int => String
         |  case String => Int
         |val r: Int = eval[Int]("(99: Wrap[String])")""".stripMargin
    )
    assertContains("val r: Int = 99", storedOutput())
  }

  @Test def bodyMatchOption = initially {
    // Pattern matching on Option with both None and Some(v). The
    // matched option is captured as a lambda parameter (`o`); the
    // match statement INSIDE the eval body operates on the captured
    // value at runtime.
    run(
      """val r: List[String] = List(Some(1), None, Some(2)).map(o => eval[String]("o match { case Some(n) => n.toString; case None => \"-\" }"))"""
    )
    assertContains("""val r: List[String] = List("1", "-", "2")""", storedOutput())
  }

  @Test def bodyMatchSealedHierarchy = initially {
    // Pattern matching on a session-defined sealed hierarchy. The
    // `import rs$line$N.*` runtime bridge brings the hierarchy into
    // the eval body's scope.
    run(
      """|sealed trait Shape
         |case class Circle(r: Double) extends Shape
         |case class Square(s: Double) extends Shape
         |def area(sh: Shape): Double = eval[Double](
         |  "sh match { case Circle(r) => 3.14 * r * r; case Square(s) => s * s }")
         |area(Circle(1.0))
         |area(Square(2.0))""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Double = 3.14", out)
    assertContains("val res1: Double = 4.0", out)
  }

  @Test def bodyMatchNestedExtractor = initially {
    // Nested extractors: case Some((a, b)) => ...
    run(
      """val r: Int = eval[Int]("(Some((3, 4)): Option[(Int, Int)]) match { case Some((a, b)) => a + b; case None => 0 }")"""
    )
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def bodyMatchAtPattern = initially {
    // `case x @ pattern => ...`. The binder `x` references the
    // whole matched object while the inner pattern still narrows.
    run(
      """val r: String = eval[String]("List(1, 2, 3) match { case xs @ (h :: _) => xs.length.toString + \":\" + h.toString; case _ => \"empty\" }")"""
    )
    assertContains("""val r: String = "3:1"""", storedOutput())
  }

  @Test def bodyMatchPipedAlternative = initially {
    // `case A | B =>` alternative patterns (no bindings on either
    // side; binding-with-alternative isn't permitted in Scala).
    run(
      """val r: String = eval[String]("3 match { case 1 | 2 | 3 => \"low\"; case 4 | 5 => \"mid\"; case _ => \"high\" }")"""
    )
    assertContains("""val r: String = "low"""", storedOutput())
  }

  @Test def bodyMatchOnCapturedValue = initially {
    // The body's `match` selector is a captured method parameter.
    // The cases pattern-match on its actual runtime value.
    run(
      """|def classify(input: Any): String = eval[String](
         |  "input match { case _: Int => \"int\"; case _: String => \"str\"; case _: List[?] => \"list\"; case _ => \"other\" }")
         |classify(42)
         |classify("hi")
         |classify(List(1, 2))
         |classify(true)""".stripMargin
    )
    val out = storedOutput()
    assertContains("""val res0: String = "int"""", out)
    assertContains("""val res1: String = "str"""", out)
    assertContains("""val res2: String = "list"""", out)
    assertContains("""val res3: String = "other"""", out)
  }

  @Test def bodyMatchMultipleGuards = initially {
    // Multiple cases each with their own guard.
    run(
      """val r: String = eval[String]("val n = 7; n match { case x if x < 0 => \"neg\"; case 0 => \"zero\"; case x if x % 2 == 0 => \"even\"; case _ => \"odd\" }")"""
    )
    assertContains("""val r: String = "odd"""", storedOutput())
  }

  @Test def bodyForComprehensionYield = initially {
    // For-comprehension with `yield` produces a List. Generators and
    // intermediate bindings work normally inside the body.
    run("""val r: List[Int] = eval("for x <- List(1, 2, 3); y = x * 10 yield x + y")""")
    assertContains("val r: List[Int] = List(11, 22, 33)", storedOutput())
  }

  @Test def bodyForComprehensionWithGuard = initially {
    // Multiple generators + guard. The body parses fine inside the
    // wrapper's braces and behaves identically to a top-level for.
    run("""val r: List[(Int, Int)] = eval("for a <- List(1,2,3); b <- List(10,20) if a + b > 12 yield (a, b)")""")
    assertContains("val r: List[(Int, Int)] = List((1, 20), (2, 20), (3, 10), (3, 20))", storedOutput())
  }

  @Test def bodyForLoopSideEffects = initially {
    // `for` without `yield` runs for side effects. Mutating an outer
    // var (captured) propagates back via the var-cell sync-back.
    run(
      """|var sum: Int = 0
         |eval[Unit]("for x <- 1 to 5 do sum = sum + x")
         |sum""".stripMargin
    )
    // The trailing `sum` shows up via the var-display path
    // (`var sum: Int = 15`) since `sum` is a top-level var.
    assertContains("var sum: Int = 15", storedOutput())
  }

  @Test def nestedEvalAfterTypeAnnotatedVal = initially {
    // Regression: when the outer eval body contained any
    // `AppliedTypeTree` (`List[T]`, `Array[T]`, type-annotated val,
    // etc.), the runtime nested-eval rewrite path went through
    // dotty's pretty-printer, which references `defn.orType` while
    // rendering applied-type trees. Without a fully-initialised
    // `ContextBase` (and a real classpath), that lookup NPEs and the
    // catch-NonFatal in `rewriteUserCode` silently dropped the
    // rewrite, so the inner eval received `enclosingSource = ""` and
    // no bindings. The visible symptom in agent traces was an inner
    // agent referencing an outer-body local that then failed the
    // wrapper compile because the binding wasn't injected.
    val body =
      "val xs: List[Int] = List(1, 2, 3); " +
      "val n: Int = xs.sum; " +
      "eval[Int](\\\"n + 100\\\")"
    run(s"""val r: Int = eval[Int]("$body")""")
    val out = storedOutput()
    assertTrue(s"no compile failure expected, got:\n$out",
      !out.contains("eval failed to compile"))
    assertContains("val r: Int = 106", out)
  }

  @Test def nestedEvalInsideNestedBlocksAndTryCatch = initially {
    // Regression for the agent-trace symptom "the start of this line
    // does not match any of the previous indentation widths". The
    // outer eval body had nested if/else/blocks/try-catch and a def
    // body; the dotty pretty-printer emitted brace-balanced output
    // whose leading-whitespace widths fell *between* the parser's
    // indentation stack entries (e.g. a 17-space line where only 16
    // and 18 were established). Under the indent-significant default
    // the round-trip parse rejected the output and `rewriteUserCode`
    // silently fell back. Forcing `-no-indent` for the round-trip
    // parse makes braces authoritative and the rewrite survives.
    val body =
      "val baseDir = new java.io.File(\\\".\\\"); " +
      "def listOne(d: java.io.File): List[java.io.File] = { " +
      "  val es = d.listFiles; " +
      "  if (es == null) Nil " +
      "  else es.toList.filter(_.getName.endsWith(\\\".txt\\\")) " +
      "}; " +
      "val files = listOne(baseDir); " +
      "val n = try { files.length } catch { case _: Exception => -1 }; " +
      "eval[Int](\\\"n + 1000\\\")"
    run(s"""val r: Int = eval[Int]("$body")""")
    val out = storedOutput()
    assertTrue(s"no compile failure expected, got:\n$out",
      !out.contains("eval failed to compile"))
    assertTrue(s"no rewrite-fallback warning expected, got:\n$out",
      !out.contains("[eval-rewrite] WARNING"))
  }

  @Test def nestedEvalInsideTryFinallyNoCatch = initially {
    // Regression: an outer eval body containing `try { ... eval(...) }
    // finally { ... }` (no catch clause) used to round-trip through
    // the pretty-printer as `try { ... } catch {<empty>} finally
    // { ... }`, which fails to re-parse and silently rolls the rewrite
    // back. The agent-trace symptom is that the inner eval's
    // enclosingSource and bindings are dropped: captured outer-body
    // locals (`x` here) become unresolved at the inner wrapper compile.
    val body =
      "val x = 7; " +
      "val src = scala.io.Source.fromString(\\\"dummy\\\"); " +
      "try eval[Int](\\\"x + 100\\\") finally src.close()"
    run(s"""val r: Int = eval[Int]("$body")""")
    val out = storedOutput()
    assertTrue(s"no compile failure expected, got:\n$out",
      !out.contains("eval failed to compile"))
    assertContains("val r: Int = 107", out)
  }

  @Test def bodyTryCatchFinally = initially {
    // try/catch with multiple cases plus a finally that mutates an
    // outer var. Tests both exception-handling path selection AND
    // var-cell sync-back from a finally block.
    val body =
      "try { throw new IllegalArgumentException(\\\"bad\\\") } " +
      "catch { case _: NullPointerException => \\\"npe\\\"; " +
      "case e: IllegalArgumentException => e.getMessage } " +
      "finally { ran = ran + 1 }"
    run(
      "var ran: Int = 0\n" +
      s"val r: String = eval[String](\"$body\")\n" +
      "r\n" +
      "ran"
    )
    val out = storedOutput()
    assertContains("val r: String = \"bad\"", out)
    // The trailing `ran` references the top-level var; the REPL
    // shows it via the var-display path.
    assertContains("var ran: Int = 1", out)
  }

  @Test def bodyIfElseIfChain = initially {
    // if / else if / else chain.
    run("""val r: String = eval[String]("val n = 7; if n < 0 then \"neg\" else if n == 0 then \"zero\" else if n < 10 then \"small\" else \"big\"")""")
    assertContains("""val r: String = "small"""", storedOutput())
  }

  @Test def bodyNestedWhile = initially {
    // Nested while loops accumulating into an outer-captured var.
    run(
      """|var total: Int = 0
         |eval[Unit]("var i = 0; while i < 3 do { var j = 0; while j < 3 do { total = total + 1; j = j + 1 }; i = i + 1 }")
         |total""".stripMargin
    )
    assertContains("var total: Int = 9", storedOutput())
  }

  @Test def bodyDoWhile = initially {
    // Scala 3 doesn't have `do { ... } while (...)` syntax, but the
    // run-once-then-check idiom still works via a regular while.
    run("""val r: Int = eval[Int]("var n = 5; var p = 1; while { p = p * n; n = n - 1; n > 0 } do (); p")""")
    assertContains("val r: Int = 120", storedOutput())
  }

  // ===========================================================================
  // 22. Recursion, both INSIDE and OUTSIDE eval.
  // ===========================================================================

  @Test def bodyRecursiveDef = initially {
    // The body declares its own recursive `def`. (`bodyDefinesLocalDef`
    // already covers a one-shot recursive def with factorial; here we
    // exercise tail-recursion-shaped code.)
    run("""val r: Int = eval[Int]("def loop(n: Int, acc: Int): Int = if n == 0 then acc else loop(n - 1, acc + n); loop(10, 0)")""")
    assertContains("val r: Int = 55", storedOutput())
  }

  @Test def bodyMutuallyRecursiveDefs = initially {
    // Mutually recursive `def`s declared inside the body. Compiled in
    // the wrapper as locals; calls resolve forward and backward.
    run(
      """val r: Boolean = eval[Boolean]("def isEven(n: Int): Boolean = if n == 0 then true else isOdd(n - 1); def isOdd(n: Int): Boolean = if n == 0 then false else isEven(n - 1); isEven(10)")"""
    )
    assertContains("val r: Boolean = true", storedOutput())
  }

  @Test def bodyCallsOuterRecursiveMethod = initially {
    // The body calls a REPL-session-level recursive method. Classes
    // and recursive methods on the session level are reached via the
    // `import rs$line$N.*` runtime bridge; the eval body just sees
    // the name `fib` in scope.
    run(
      """|def fib(n: Int): Int = if n < 2 then n else fib(n - 1) + fib(n - 2)
         |val r: Int = eval[Int]("fib(10)")""".stripMargin
    )
    assertContains("val r: Int = 55", storedOutput())
  }

  @Test def bodyCallsOuterRecursiveMethodWithCapturedParam = initially {
    // The body's call passes a *captured* lambda parameter as the
    // argument to the recursive method.
    run(
      """|def fact(n: Int): Int = if n <= 1 then 1 else n * fact(n - 1)
         |val r: List[Int] = List(0, 1, 4, 5).map(z => eval[Int]("fact(z)"))""".stripMargin
    )
    assertContains("val r: List[Int] = List(1, 1, 24, 120)", storedOutput())
  }

  @Test def evalInsideRecursiveMethod = initially {
    // The recursive method itself contains an eval call. Each
    // recursion step issues a fresh eval; the captured `n` is
    // re-bound per call.
    run(
      """|def countDown(n: Int): String =
         |  if n == 0 then "done"
         |  else eval[String]("countDown(n - 1)")
         |countDown(3)""".stripMargin
    )
    assertContains("""val res0: String = "done"""", storedOutput())
  }

  @Test def evalInsideTailRecursiveMethod = initially {
    // Captures the lambda parameter and recurses through eval. The
    // captured `acc` is the running accumulator.
    run(
      """|def sumTo(n: Int, acc: Int): Int =
         |  if n == 0 then acc
         |  else eval[Int]("sumTo(n - 1, acc + n)")
         |sumTo(10, 0)""".stripMargin
    )
    assertContains("val res0: Int = 55", storedOutput())
  }

  // ===========================================================================
  // 23. Control flow OUTSIDE the eval call (eval inside while/for/try/if).
  //
  //     The rewriter walks lambda/block/DefDef scopes regardless of the
  //     surrounding control structure, so an eval inside a `while` body or a
  //     `for` comprehension's expression captures the right names.
  // ===========================================================================

  @Test def whileLoopAroundEvalMutatesCapturedVar = initially {
    // The eval call lives inside a while-loop body. Each iteration
    // re-binds the (cell-backed) outer var and propagates back.
    run(
      """|def f(): Int =
         |  var i: Int = 0
         |  var n: Int = 0
         |  while i < 5 do
         |    eval[Unit]("n = n + i")
         |    i = i + 1
         |  n
         |f()""".stripMargin
    )
    assertContains("val res0: Int = 10", storedOutput())
  }

  @Test def forLoopAroundEvalCallsEachIteration = initially {
    // The eval is the expression body of a `for ... yield`. Each
    // iteration captures the for-binding `x` lambda-style and
    // produces one element.
    run("""val r: List[Int] = (for x <- List(1, 2, 3, 4) yield eval[Int]("x * x"))""")
    assertContains("val r: List[Int] = List(1, 4, 9, 16)", storedOutput())
  }

  @Test def forLoopWithMultipleGeneratorsAroundEval = initially {
    // Multiple generators: both `a` and `b` are in scope as captured
    // bindings inside the eval body.
    run(
      """val r: List[Int] =
        |  (for a <- List(1, 2); b <- List(10, 20) yield eval[Int]("a * 100 + b"))""".stripMargin
    )
    assertContains("val r: List[Int] = List(110, 120, 210, 220)", storedOutput())
  }

  @Test def tryAroundEvalCatchingEvalCompileError = initially {
    // try/catch surrounds eval. `EvalCompileException` is a normal
    // RuntimeException; user code can catch it just like any other.
    run(
      """|import dotty.tools.repl.eval.EvalCompileException
         |def safeEval(): String =
         |  try eval[Int]("undefinedSymbol").toString
         |  catch case _: EvalCompileException => "compile-failed"
         |safeEval()""".stripMargin
    )
    assertContains("""val res0: String = "compile-failed"""", storedOutput())
  }

  @Test def ifBranchesEachCallEval = initially {
    // Both branches of an if call eval; the captured method param is
    // visible in either branch's body.
    run(
      """|def classify(n: Int): String =
         |  if n >= 0 then eval[String]("\"non-negative: \" + n.toString")
         |  else eval[String]("\"negative: \" + n.toString")
         |classify(5)
         |classify(-3)""".stripMargin
    )
    val out = storedOutput()
    assertContains("""val res0: String = "non-negative: 5"""", out)
    assertContains("""val res1: String = "negative: -3"""", out)
  }

  @Test def matchExpressionAroundEval = initially {
    // Each case of a `match` calls eval. Binding-by-pattern (e.g.
    // `case Some(v) =>` introducing `v`) is *not* picked up by the
    // rewriter (case-pattern names aren't on the scope stack), so
    // the body has to reach values via the matched variable. Here
    // the body uses the captured method parameter `opt` directly.
    run(
      """|def describe(opt: Option[Int]): String = opt match
         |  case Some(_) => eval[String]("\"got \" + opt.get.toString")
         |  case None    => eval[String]("\"none\"")
         |describe(Some(7))
         |describe(None)""".stripMargin
    )
    val out = storedOutput()
    assertContains("""val res0: String = "got 7"""", out)
    assertContains("""val res1: String = "none"""", out)
  }

  // ===========================================================================
  // 24. Functions defined inside eval, returned and used outside.
  // ===========================================================================

  @Test def returnsAnonymousFunction = initially {
    run("""|val f: Int => Int = eval("(x: Int) => x * 2")
           |val r: Int = f(7)""".stripMargin)
    assertContains("val r: Int = 14", storedOutput())
  }

  @Test def returnsNamedFunction = initially {
    run("""|val f: Int => Int = eval("def myFn(x: Int): Int = x * x; myFn")
           |val r: Int = f(5)""".stripMargin)
    assertContains("val r: Int = 25", storedOutput())
  }

  @Test def returnsCurriedFunction = initially {
    run("""|val add: Int => (Int => Int) = eval("(a: Int) => (b: Int) => a + b")
           |val r: Int = add(3)(4)""".stripMargin)
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def returnedFunctionClosesOverReplState = initially {
    // The lambda captures REPL-defined `factor` at its compile time; calling
    // it later still picks up the captured value.
    run("val factor: Int = 10")
  } andThen {
    storedOutput()
    run("""|val f: Int => Int = eval("(x: Int) => x * factor")
           |val r: Int = f(7)""".stripMargin)
    assertContains("val r: Int = 70", storedOutput())
  }

  @Test def returnsCurriedClosureBuiltDynamically = initially {
    // The motivating example: a method takes a string operator name
    // and returns a curried `Int => Int => Int` built at call time
    // by an `eval` body that interpolates the operator into the
    // function literal. This exercises:
    //   - dynamic body construction via `s"..."`,
    //   - the wrapper returning a closure value,
    //   - the closure crossing the eval / REPL classloader boundary
    //     as `scala.Function1` (visible as the same Class on both
    //     sides because the AbstractFileClassLoader delegates
    //     `scala.*` to the parent loader).
    run(
      """|def mkOp(op: String): Int => Int => Int =
         |  eval[Int => Int => Int](s"i => j => i $op j")
         |val plus = mkOp("+")
         |val times = mkOp("*")
         |plus(2)(3)
         |times(4)(5)""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Int = 5", out)
    assertContains("val res1: Int = 20", out)
  }

  @Test def returnsClosureCapturingMethodParam = initially {
    // The closure built inside eval captures the method parameter
    // `n`. Calling the returned closure later applies it to `n`,
    // even though `n` is no longer in scope at the call site.
    run(
      """|def adderFor(n: Int): Int => Int =
         |  eval[Int => Int]("(x: Int) => x + n")
         |val addFive = adderFor(5)
         |val addTen = adderFor(10)
         |addFive(100)
         |addTen(100)""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Int = 105", out)
    assertContains("val res1: Int = 110", out)
  }

  @Test def returnsClosureMultiArgPartiallyApplied = initially {
    // A closure of higher arity returned from eval, then partially
    // applied via a curried-style helper.
    run(
      """|val f3: (Int, Int, Int) => Int =
         |  eval[(Int, Int, Int) => Int]("(a: Int, b: Int, c: Int) => a * 100 + b * 10 + c")
         |f3(1, 2, 3)""".stripMargin
    )
    assertContains("val res0: Int = 123", storedOutput())
  }

  @Test def returnsListOfClosures = initially {
    // The body returns a list of closures, each capturing a
    // different value.
    run(
      """|val fs: List[Int => Int] = eval[List[Int => Int]]("List(1, 2, 3).map(k => (x: Int) => x + k)")
         |fs.map(f => f(100))""".stripMargin
    )
    assertContains("val res0: List[Int] = List(101, 102, 103)", storedOutput())
  }

  @Test def returnedClosureCalledRepeatedly = initially {
    // The same closure is invoked many times after being returned;
    // each call re-runs the closure body (no side effects from the
    // construction beyond what eval itself did once).
    run(
      """|val incr: Int => Int = eval[Int => Int]("(x: Int) => x + 1")
         |val r: List[Int] = List(0, 1, 2, 3).map(incr)""".stripMargin
    )
    assertContains("val r: List[Int] = List(1, 2, 3, 4)", storedOutput())
  }

  // ===========================================================================
  // 25. Generic and stdlib types inside the body.
  // ===========================================================================

  @Test def stdlibListSum = initially {
    run("""val r: Int = eval("List(1, 2, 3, 4).map(_ * 2).sum")""")
    assertContains("val r: Int = 20", storedOutput())
  }

  @Test def stdlibForComprehension = initially {
    run("""val r: List[Int] = eval("(for i <- 1 to 3; j <- 1 to i yield i * j).toList")""")
    assertContains("List(1, 2, 4, 3, 6, 9)", storedOutput())
  }

  @Test def stdlibOptionMap = initially {
    run("""val r: Int = eval("Some(5).map(_ * 3).getOrElse(0)")""")
    assertContains("val r: Int = 15", storedOutput())
  }

  // ===========================================================================
  // 26. Compile-time errors in the eval body.
  // ===========================================================================

  @Test def compileErrorUnknownIdent = initially {
    run("""val r: Int = eval("nonexistentVar + 1")""")
    assertContains("nonexistentVar", storedOutput())
  }

  @Test def compileErrorParseFailure = initially {
    run("""val r: Int = eval("(1 + ")""")
    val out = storedOutput()
    assertTrue(s"expected an error, got:\n$out", out.contains("failed to compile") || out.contains("Error"))
  }

  @Test def compileErrorTypeMismatch = initially {
    run("""val r: Int = eval("\"a string\" + 1: Int")""")
    val out = storedOutput()
    assertTrue(s"expected an error, got:\n$out",
      out.contains("failed to compile") || out.contains("Error") || out.contains("ClassCastException"))
  }

  @Test def compileErrorIsEvalCompileException = initially {
    // Compile-time errors arrive as `EvalCompileException`, not the
    // generic `RuntimeException`. The exception type is shared across
    // the eval / REPL classloader boundary so the user can catch it
    // by name and inspect its structured `errors` field.
    //
    // V2 may surface follow-on errors when the spliced body's type
    // doesn't match the expected return type (the type mismatch on
    // `__evalResult: Int = false` cascades into "couldn't infer T"
    // and similar refinements). The exact count is implementation-
    // defined; what matters is that *at least one* error makes it
    // through and the EvalCompileException is catchable by name.
    run(
      """|import dotty.tools.repl.eval.EvalCompileException
         |val r: String =
         |  try
         |    eval[Int]("false")
         |    "no error"
         |  catch case e: EvalCompileException =>
         |    if e.errors.length >= 1 then "caught" else "miscount"""".stripMargin
    )
    assertContains("""val r: String = "caught"""", storedOutput())
  }

  // ===========================================================================
  // 27. Nested eval: an eval body can itself call eval.
  // ===========================================================================

  @Test def nestedEvalSimple = initially {
    run("""val r: Int = eval("eval[Int](\"1 + 2\") * 10")""")
    assertContains("val r: Int = 30", storedOutput())
  }

  @Test def nestedEvalThreeDeep = initially {
    run("""val r: Int = eval("eval[Int](\"eval[Int](\\\"3\\\") + 4\") * 2")""")
    assertContains("val r: Int = 14", storedOutput())
  }

  @Test def nestedEvalSeesReplState = initially {
    run("""|val n: Int = 7
           |val r: Int = eval("eval[Int](\"n * n\")")""".stripMargin)
    assertContains("val r: Int = 49", storedOutput())
  }

  @Test def nestedEvalSeesOuterBodyVal = initially {
    // The outer eval body declares `val j`; the inner eval inside that
    // body must capture it. The eval driver runs the rewriter over the
    // body, so the inner call has `j` injected as a binding.
    run("""|val i: Int = 1
           |val r: Int = eval[Int]("val j = 2; eval[Int](\"i + j\")")""".stripMargin)
    assertContains("val r: Int = 3", storedOutput())
  }

  @Test def nestedEvalSeesOuterBodyVar = initially {
    // Same idea for a `var` declared in the outer body: captured as a
    // cell into the inner eval, mutation propagates.
    run("""val r: Int = eval[Int]("var k = 0; eval[Unit](\"k = k + 7\"); k")""")
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def nestedEvalDynamicInner = initially {
    run("""|val inner: String = "1 + 2"
           |val outer: String = "eval[Int](inner)"
           |val r: Int = eval(outer)""".stripMargin)
    assertContains("val r: Int = 3", storedOutput())
  }

  // ===========================================================================
  // 28. Larger showcase: a symbolic-differentiation engine driven entirely by
  //     recursive `eval` calls.
  //
  //     * `Expr` algebra (X, Num, Add, Mul, Pow) plus `show` (prints as Scala
  //       expression using `x`), `lit` (prints as Scala source that
  //       reconstructs the AST), `diff` (symbolic differentiation).
  //     * `evalAt(e, x)` builds `val x = <x>; <show(e)>` and hands it to
  //       `eval[Int]`. Variable value and body text both come from runtime
  //       data, so a macro or compile-time inline couldn't do this.
  //     * `diffAt(e, n, x)` computes the nth derivative at `x`. For `n > 0`
  //       it RECURSES THROUGH EVAL: each level spawns a fresh wrapper
  //       compile, so an Nth derivative produces N+1 nested-eval calls.
  //     * Type safety holds end-to-end: a body that returned the wrong type
  //       would fail at the eval-driver compile, not silently at the call
  //       site as a `ClassCastException`.
  //
  //     Polynomial used: f(x) = 3x² + 5
  //       f(2)   = 17       via `evalAt(f, 2)`
  //       f'(2)  = 12       via `evalAt(diff(f), 2)`
  //       f''(7) = 6        via `diffAt(f, 2, 7)`, two recursion levels.
  // ===========================================================================

  @Test def symbolicDifferentiationViaRecursiveEval = initially {
    val program =
      """|sealed trait Expr
         |case object X extends Expr
         |case class Num(v: Int) extends Expr
         |case class Add(a: Expr, b: Expr) extends Expr
         |case class Mul(a: Expr, b: Expr) extends Expr
         |case class Pow(b: Expr, n: Int) extends Expr
         |
         |def show(e: Expr): String = e match
         |  case X => "x"
         |  case Num(v) => v.toString
         |  case Add(a, b) => s"(${show(a)} + ${show(b)})"
         |  case Mul(a, b) => s"(${show(a)} * ${show(b)})"
         |  case Pow(b, n) =>
         |    if n == 0 then "1"
         |    else if n == 1 then show(b)
         |    else (1 until n).foldLeft(show(b))((acc, _) => s"($acc * ${show(b)})")
         |
         |def lit(e: Expr): String = e match
         |  case X => "X"
         |  case Num(v) => s"Num($v)"
         |  case Add(a, b) => s"Add(${lit(a)}, ${lit(b)})"
         |  case Mul(a, b) => s"Mul(${lit(a)}, ${lit(b)})"
         |  case Pow(b, n) => s"Pow(${lit(b)}, $n)"
         |
         |def diff(e: Expr): Expr = e match
         |  case X => Num(1)
         |  case Num(_) => Num(0)
         |  case Add(a, b) => Add(diff(a), diff(b))
         |  case Mul(a, b) => Add(Mul(diff(a), b), Mul(a, diff(b)))
         |  case Pow(b, n) => Mul(Mul(Num(n), Pow(b, n - 1)), diff(b))
         |
         |def evalAt(e: Expr, x: Int): Int =
         |  eval[Int](s"val x = $x; ${show(e)}")
         |
         |def diffAt(e: Expr, n: Int, x: Int): Int =
         |  if n == 0 then evalAt(e, x)
         |  else eval[Int](s"diffAt(diff(${lit(e)}), ${n - 1}, $x)")
         |
         |val f: Expr = Add(Mul(Num(3), Pow(X, 2)), Num(5))
         |evalAt(f, 2)
         |evalAt(diff(f), 2)
         |diffAt(f, 0, 2)
         |diffAt(f, 1, 2)
         |diffAt(f, 2, 7)
         |""".stripMargin
    run(program)
    val out = storedOutput()
    // f(2)  = 3*4 + 5
    assertContains("val res0: Int = 17", out)
    // f'(2) = 6x at x=2
    assertContains("val res1: Int = 12", out)
    // diffAt(f, 0, 2)  = f(2): a fresh eval roundtrip, no recursion
    assertContains("val res2: Int = 17", out)
    // diffAt(f, 1, 2)  = f'(2): one level of recursive eval
    assertContains("val res3: Int = 12", out)
    // diffAt(f, 2, 7)  = f''(7) = 6: two levels of recursive eval
    assertContains("val res4: Int = 6", out)
  }

  // ===========================================================================
  // 29. Multiple eval calls in a single block. Each call is rewritten and
  //     compiled independently, but they share the surrounding lexical scope
  //     (so all see the same enclosing bindings) and run in source order.
  // ===========================================================================

  @Test def twoSiblingEvalsShareEnclosingVal = initially {
    // Same `n` captured by two separate eval calls in the same line.
    // The rewriter visits each call independently and emits its own
    // bindings array; both arrays carry `n`.
    run("""|val n: Int = 4
           |val r: Int = eval[Int]("n * n") + eval[Int]("n + 1")""".stripMargin)
    assertContains("val r: Int = 21", storedOutput())
  }

  @Test def twoSiblingEvalsInTuple = initially {
    run("""|val n: Int = 3
           |val r: (Int, Int) = (eval[Int]("n * n"), eval[Int]("n + n"))""".stripMargin)
    assertContains("val r: (Int, Int) = (9, 6)", storedOutput())
  }

  @Test def threeSiblingEvalsAccumulating = initially {
    // Side-effect ordering: two `eval[Unit]` writes followed by an
    // `eval[Int]` read. The driver must run each call in source order.
    run("""|var sum = 0
           |eval[Unit]("sum = sum + 10")
           |eval[Unit]("sum = sum + 20")
           |val r: Int = eval[Int]("sum")""".stripMargin)
    assertContains("val r: Int = 30", storedOutput())
  }

  @Test def evalResultFedAsBindingToLaterEval = initially {
    // First eval produces a value; that value is captured into the
    // second eval's bindings via a normal val. Plain dataflow, no
    // "result threading" magic.
    run("""|val first: Int = eval[Int]("2 + 3")
           |val r: Int = eval[Int]("first * first")""".stripMargin)
    assertContains("val r: Int = 25", storedOutput())
  }

  @Test def evalCallsInBranchesOfIf = initially {
    // The two eval calls live in the `then`/`else` branches; only one
    // is actually reached at runtime, but both must compile.
    run("""|val n: Int = 5
           |val r: Int = if n > 0 then eval[Int]("n * 2") else eval[Int]("-n")""".stripMargin)
    assertContains("val r: Int = 10", storedOutput())
  }

  @Test def evalCallsInBranchesOfMatch = initially {
    run("""|val tag: String = "double"
           |val n: Int = 6
           |val r: Int = tag match
           |  case "double" => eval[Int]("n * 2")
           |  case "neg"    => eval[Int]("-n")
           |  case _        => eval[Int]("0")""".stripMargin)
    assertContains("val r: Int = 12", storedOutput())
  }

  @Test def siblingEvalsWithDifferentReturnTypes = initially {
    // Each eval call is independent at the wrapper level: one returns
    // Int, the other String. The wrapper compile picks each return
    // type from the call's own `[T]` annotation.
    run("""|val n: Int = 7
           |val a: Int = eval[Int]("n * n")
           |val b: String = eval[String]("n.toString.reverse")
           |val r: String = s"$a:$b"""".stripMargin)
    assertContains("""val r: String = "49:7"""", storedOutput())
  }

  @Test def evalCallsInsideForComprehension = initially {
    // The for-comprehension desugars to `xs.flatMap(x => eval[...](...))`
    // calls; each iteration's lambda body has its own eval invocation.
    // The same captured `n` appears in every iteration's bindings.
    run("""|val n: Int = 10
           |val r: List[Int] = for x <- (1 to 3).toList yield eval[Int]("n + x")""".stripMargin)
    assertContains("val r: List[Int] = List(11, 12, 13)", storedOutput())
  }

  @Test def evalCallsAcrossBlockStatements = initially {
    // Each statement of the block lives at its own source position;
    // each eval gets its own enclosingSource slice. The block's later
    // statements see the earlier ones' val/var declarations as
    // additional captured bindings.
    run("""|val r: Int =
           |  val a: Int = eval[Int]("1 + 1")
           |  val b: Int = eval[Int]("a * 3")
           |  val c: Int = eval[Int]("a + b + 4")
           |  c""".stripMargin)
    assertContains("val r: Int = 12", storedOutput())
  }

  @Test def siblingEvalsCapturingSameVar = initially {
    // Both eval calls capture the same `var k`. Both go through
    // VarRef so writes from one are visible to the next.
    run("""|var k: Int = 1
           |eval[Unit]("k = k * 2; k = k + 1")
           |val r: Int = eval[Int]("k * 100")""".stripMargin)
    assertContains("val r: Int = 300", storedOutput())
  }

  @Test def siblingEvalsRecoverableFailureDoesNotBlockNext = initially {
    // The first eval throws at runtime; we catch it. The second eval
    // sits in the same line and runs cleanly: its compile and wrapper
    // synthesis is independent of the first, and a thrown body
    // exception out of the first call doesn't taint the rest.
    run("""|val n: Int = 9
           |val a: Int = try eval[Int]("throw new RuntimeException(\"boom\"); 0")
           |             catch case _: RuntimeException => -1
           |val b: Int = eval[Int]("n + 1")
           |val r: String = s"a=$a,b=$b"""".stripMargin)
    val out = storedOutput()
    assertTrue(s"expected the second eval to run after the first threw, got:\n$out",
      out.contains("a=-1") && out.contains("b=10"))
  }

  // ===========================================================================
  // 30. Truly-dynamic bodies. These cases rule out any "the compiler could
  //     have constant-folded the body string" interpretation: the body is
  //     built from runtime data the compiler can't see ahead of time
  //     (loop-assembled strings, runtime-keyed lookups, branches taken on
  //     runtime input, mutated `StringBuilder` output).
  // ===========================================================================

  @Test def bodyAssembledByLoopAtRuntime = initially {
    // The body is "1 + 2 + 3" but the rewriter at parser stage sees
    // only `eval[Int](expr)` with `expr` opaque. The string is built
    // by `mkString` on a List the size of which is `n`, so a constant-
    // folder couldn't have known how many `+` separators would land in
    // the body text.
    run("""|val n: Int = 3
           |val expr: String = (1 to n).mkString(" + ")
           |val r: Int = eval[Int](expr)""".stripMargin)
    assertContains("val r: Int = 6", storedOutput())
  }

  @Test def bodyChosenFromRuntimeMap = initially {
    // The body comes out of a Map keyed by a runtime String. The
    // compiler can't see which entry will be picked.
    run("""|val n: Int = 4
           |val ops: Map[String, String] = Map("sq" -> "n * n", "succ" -> "n + 1", "twice" -> "n + n")
           |val pick: String = "succ"
           |val r: Int = eval[Int](ops(pick))""".stripMargin)
    assertContains("val r: Int = 5", storedOutput())
  }

  @Test def bodyChosenByRuntimeBranch = initially {
    // A runtime if/else picks between two body strings. Both bodies
    // type-check inside the eval driver as separate compiles.
    run("""|val n: Int = 6
           |val flag: Boolean = (n % 2 == 0)
           |val expr: String = if flag then "n / 2" else "n * 3 + 1"
           |val r: Int = eval[Int](expr)""".stripMargin)
    assertContains("val r: Int = 3", storedOutput())
  }

  @Test def bodyBuiltFromRuntimeListOfFragments = initially {
    // The body is concatenated piece-by-piece from a List the user
    // assembled at runtime. The rewriter sees only the variable.
    run("""|val ns: List[Int] = List(2, 5, 7)
           |val expr: String = ns.map(i => s"($i * $i)").mkString(" + ")
           |val r: Int = eval[Int](expr)""".stripMargin)
    // 4 + 25 + 49 = 78
    assertContains("val r: Int = 78", storedOutput())
  }

  @Test def bodyBuiltViaStringBuilder = initially {
    // The body string comes out of a StringBuilder that the user
    // appended to imperatively. As truly opaque to the compiler as
    // any `String` reference can be.
    run("""|val n: Int = 10
           |val sb: StringBuilder = new StringBuilder
           |sb.append("n"); sb.append(" * "); sb.append("3")
           |val expr: String = sb.toString
           |val r: Int = eval[Int](expr)""".stripMargin)
    assertContains("val r: Int = 30", storedOutput())
  }

  @Test def bodyComputedFromRuntimeStringTransform = initially {
    // Each call's body is `i * 10` (i = 1..3), but the body string
    // for each iteration is built by transforming the loop counter
    // through `.toString` and back, so the compiler can't fold any
    // of it at compile time. The same `.eval[Int]` call site runs
    // three times with three different bodies.
    run("""|val r: List[Int] = (1 to 3).toList.map(i => eval[Int](s"${i.toString.reverse} * 10"))""".stripMargin)
    assertContains("val r: List[Int] = List(10, 20, 30)", storedOutput())
  }

  @Test def bodyRuntimeFailureRecovered = initially {
    // The body is intentionally invalid. The error propagates as
    // EvalCompileException (a RuntimeException), so a try/catch
    // recovers and the rest of the line continues.
    run("""|val expr: String = "this is not valid scala"
           |val r: String = try { eval[Int](expr).toString } catch case _: RuntimeException => "caught"
           |""".stripMargin)
    assertContains("""val r: String = "caught"""", storedOutput())
  }

  // ===========================================================================
  // 31. Runtime errors raised by the eval body propagate to the caller.
  // ===========================================================================

  @Test def runtimeArithmeticException = initially {
    run("""val r: Int = eval("1 / 0")""")
    assertContains("ArithmeticException", storedOutput())
  }

  @Test def runtimeUserThrow = initially {
    run("""val r: Int = eval("throw new RuntimeException(\"user-boom\"); 0")""")
    assertContains("user-boom", storedOutput())
  }

  // ===========================================================================
  // 32. Failed previous lines don't break later eval calls.
  //
  //     When a REPL line fails to type-check, its `rs$line$N` wrapper has no
  //     classfile. The runtime filters such indexes out before generating the
  //     synthetic `import rs$line$N.*` so a later `eval(...)` doesn't trip on
  //     the missing wrapper.
  // ===========================================================================

  @Test def evalAfterFailedReplLine = initially {
    // First line fails type-check.
    run("val broken = nonexistent + 1")
  } andThen {
    storedOutput()
    // The next eval still works: no spurious "Not found: rs$line$1".
    run("""val r: Int = eval("1 + 2")""")
    assertContains("val r: Int = 3", storedOutput())
  }

  // ===========================================================================
  // 33. Local recursive defs reached from inside an eval body.
  //
  //     A `def` declared inside another `def`/method gets lambda-lifted out
  //     of the enclosing scope, so the rewriter must capture it as a method
  //     (the MethodCapture strategy) rather than a value. When that captured
  //     local def itself calls `eval(...)` recursively (the body re-enters
  //     the SAME local def) every recursive step issues a fresh eval; the
  //     captured `i` is re-bound per call.
  // ===========================================================================

  @Test def evalCallsLocalRecursiveDefFromOuterDef = initially {
    run(
      """|def g(): Int =
         |  def f2(i: Int, acc: Int): Int =
         |    if i == 0 then acc
         |    else eval[Int]("f2(i - 1, acc + i)")
         |  f2(5, 0)
         |g()""".stripMargin
    )
    assertContains("val res0: Int = 15", storedOutput())
  }

  @Test def evalCallsLocalRecursiveDefWithSideEffect = initially {
    // The user's headline example: a Unit-returning local def whose body
    // is an eval that prints and recurses.
    run(
      """|def g() =
         |  def f2(i: Int): Unit =
         |    if i == 0 then ()
         |    else eval[Unit]("println(i); f2(i - 1)")
         |  f2(3)
         |g()""".stripMargin
    )
    val out = storedOutput()
    assertTrue(s"expected 3,2,1 in countdown order, got:\n$out",
      out.contains("3") && out.contains("2") && out.contains("1") &&
        out.indexOf("3") < out.indexOf("2") && out.indexOf("2") < out.indexOf("1"))
  }

  @Test def evalCallsMutuallyRecursiveLocalDefs = initially {
    // Two locally-defined defs that call each other through eval. Each
    // is captured under its own MethodCapture binding; both calls
    // resolve forward and backward through eval.
    run(
      """|def driver(n: Int): Boolean =
         |  def isEven(k: Int): Boolean =
         |    if k == 0 then true else eval[Boolean]("isOdd(k - 1)")
         |  def isOdd(k: Int): Boolean =
         |    if k == 0 then false else eval[Boolean]("isEven(k - 1)")
         |  isEven(n)
         |driver(8)""".stripMargin
    )
    assertContains("val res0: Boolean = true", storedOutput())
  }

  @Test def evalCallsLocalDefThatReadsLocalVar = initially {
    // The local def captures a *var* declared in the enclosing method,
    // and the eval body calls the local def. The var is reachable
    // because the rewriter wraps it in a VarRef when the enclosing
    // scope is a method body.
    run(
      """|def h(): Int =
         |  var total = 0
         |  def add(n: Int): Unit = total = total + n
         |  eval[Unit]("add(5); add(10); add(7)")
         |  total
         |h()""".stripMargin
    )
    assertContains("val res0: Int = 22", storedOutput())
  }

  // ===========================================================================
  // 34. Migrated debug-pipeline fixtures. The dotty `tests/debug` suite pins
  //     the ExpressionCompiler that V2's pipeline is modelled after, exercising
  //     scenarios (overload resolution, by-name params, local pattern
  //     destructuring, REPL-session enums) that the REPL eval should also
  //     handle.
  // ===========================================================================

  @Test def overloadResolutionInsideBody = initially {
    // Mirrors `tests/debug/eval-overloads.scala`: the body picks
    // among several `m` overloads by argument shape. Overload
    // resolution happens in the eval-driver's typer, exactly as it
    // would in normal source.
    run(
      """|trait A
         |class B extends A
         |def m(): String = "m"
         |def m(n: Int): String = s"m($n: Int)"
         |def m(b: Boolean): String = s"m($b: Boolean)"
         |def m(s: String): String = s"m($s: String)"
         |def m(a: A): String = s"m(a: A)"
         |def m(b: B): String = s"m(b: B)"
         |val r1: String = eval[String]("m()")
         |val r2: String = eval[String]("m(5)")
         |val r3: String = eval[String]("m(true)")
         |val r4: String = eval[String]("m(\"foo\")")
         |val r5: String = eval[String]("m(new B)")
         |val r6: String = eval[String]("m((new B): A)")""".stripMargin
    )
    val out = storedOutput()
    assertContains("""val r1: String = "m"""", out)
    assertContains("""val r2: String = "m(5: Int)"""", out)
    assertContains("""val r3: String = "m(true: Boolean)"""", out)
    assertContains("""val r4: String = "m(foo: String)"""", out)
    assertContains("""val r5: String = "m(b: B)"""", out)
    assertContains("""val r6: String = "m(a: A)"""", out)
  }

  @Test def overloadResolutionOnArrayShapes = initially {
    // The debug fixture also pins overloads selected by Array element
    // type and rank. Erasure collapses `Array[Int]` and `Array[A]` to
    // distinct JVM signatures, so the body picks the right one even
    // though both names match `m(xs: Array[?])`.
    run(
      """|trait A
         |class B extends A
         |def m(xs: Array[Int]): String = "m(xs: Array[Int])"
         |def m(xs: Array[A]): String = "m(xs: Array[A])"
         |def m(xs: Array[Array[Int]]): String = "m(xs: Array[Array[Int]])"
         |val r1: String = eval[String]("m(Array(1, 2))")
         |val r2: String = eval[String]("m(Array[A](new B))")
         |val r3: String = eval[String]("m(Array(Array(1), Array(2)))")""".stripMargin
    )
    val out = storedOutput()
    assertContains("""val r1: String = "m(xs: Array[Int])"""", out)
    assertContains("""val r2: String = "m(xs: Array[A])"""", out)
    assertContains("""val r3: String = "m(xs: Array[Array[Int]])"""", out)
  }

  @Test def overloadOnGenericSeqResolves = initially {
    // The two `m1` overloads disambiguate via the seq element type.
    // The body has to be type-checked against the captured argument's
    // *static* type to pick the right one.
    run(
      """|def m1(xs: Seq[Int]): String = xs.toString
         |def m1(xs: Seq[Boolean]): Int = xs.count(identity)
         |val r1: String = eval[String]("m1(Seq(1, 2, 3))")
         |val r2: Int = eval[Int]("m1(Seq(true, false, true))")""".stripMargin
    )
    val out = storedOutput()
    assertContains("""val r1: String = "List(1, 2, 3)"""", out)
    assertContains("val r2: Int = 2", out)
  }

  @Test def tupleExtractorPatternInBody = initially {
    // Mirrors `tests/debug/eval-tuple-extractor.scala`: the body uses
    // a `val (x, y) = t` destructuring on a captured tuple. Both
    // pattern-bound names live entirely inside the eval body.
    run(
      """|val t: (Int, Int) = (3, 4)
         |val r: Int = eval[Int]("val (x, y) = t; x * 10 + y")""".stripMargin
    )
    assertContains("val r: Int = 34", storedOutput())
  }

  @Test def tupleProjectionsAndApplyInBody = initially {
    // The same fixture also evaluates `t(0)` and `t._2`. Both compile
    // because the eval driver sees `t`'s actual `Tuple2` type, not
    // some erased `Object`.
    run(
      """|val t: (Int, Int) = (1, 2)
         |val a: Int = eval[Int]("t(0)")
         |val b: Int = eval[Int]("t._2")""".stripMargin
    )
    val out = storedOutput()
    assertContains("val a: Int = 1", out)
    assertContains("val b: Int = 2", out)
  }

  @Test def byNameParamCapturedAsThunk = initially {
    // Mirrors `tests/debug/eval-by-name.scala`: the enclosing method
    // takes a `=> String` parameter. The rewriter captures by-name
    // params as a `() => x` thunk so the binding stores a Function0
    // matching what ElimByName lowers `x` references to in the body.
    // Each `x` access in the body forces the original by-name through
    // the captured thunk.
    run(
      """|def m(x: => String): String =
         |  eval[String]("x + x")
         |m("foo")""".stripMargin
    )
    assertContains("""val res0: String = "foofoo"""", storedOutput())
  }

  @Test def byNameParamForcedSideEffectFourTimes = initially {
    // Body references the by-name param four times. The side effect
    // counter must end at 4. A stale "force-once-and-cache" capture
    // would leave it at 1.
    run(
      """|var calls = 0
         |def f(): Int = { calls = calls + 1; 3 }
         |def m(x: => Int): Int =
         |  eval[Int]("x + x + x + x")
         |val total: Int = m(f())""".stripMargin
    )
    val out = storedOutput()
    assertContains("val total: Int = 12", out)
    assertContains("var calls: Int = 4", out)
  }

  @Test def byNameParamReturnsFreshValuePerForce = initially {
    // The by-name expression yields a *different* value on each
    // force (a counter increment). Body sums three references; the
    // result must reflect three distinct values (1 + 2 + 3 = 6),
    // proving each access re-runs the thunk rather than returning a
    // cached snapshot taken at bind time.
    run(
      """|var counter: Int = 0
         |def next(): Int = { counter = counter + 1; counter }
         |def m(x: => Int): Int =
         |  eval[Int]("x + x + x")
         |val r: Int = m(next())""".stripMargin
    )
    val out = storedOutput()
    assertContains("val r: Int = 6", out)
    assertContains("var counter: Int = 3", out)
  }

  @Test def byNameParamSideEffectVisibleInPrintOrder = initially {
    // The by-name appends a tag each time it's forced; the spliced
    // body forces twice. The trail captures both forces in source
    // order, so the buffer ends with two tags rather than one.
    run(
      """|val buf: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer.empty
         |def trail(label: String): Int =
         |  buf += label
         |  label.length
         |def m(x: => Int): Int =
         |  eval[Int]("x + x")
         |val total: Int = m(trail("hi"))
         |val log: List[String] = buf.toList""".stripMargin
    )
    val out = storedOutput()
    assertContains("val total: Int = 4", out)
    assertContains("""val log: List[String] = List("hi", "hi")""", out)
  }

  @Test def byNameParamUnusedDoesNotForce = initially {
    // Symmetric check: when the body never references `x`, the
    // by-name thunk shouldn't fire at all (counter stays at 0). The
    // bind site builds `() => x` but that thunk is never called if
    // `getValue("x")` isn't invoked from the evaluate body.
    run(
      """|var calls: Int = 0
         |def f(): Int = { calls = calls + 1; 99 }
         |def m(x: => Int): Int =
         |  eval[Int]("42")
         |val r: Int = m(f())""".stripMargin
    )
    val out = storedOutput()
    assertContains("val r: Int = 42", out)
    assertContains("var calls: Int = 0", out)
  }

  @Test def callingInlineDefFromBody = initially {
    // Mirrors `tests/debug/eval-inline.scala`: an `inline def` is a
    // perfectly normal call from the body's perspective; the eval
    // compile inlines it into the spliced module.
    run(
      """|inline def succ(inline x: Int): Int = x + 1
         |val r: Int = eval[Int]("succ(41)")""".stripMargin
    )
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def bodyDeclaresInlineVal = initially {
    // The body itself defines an `inline val`, then uses it. Inline
    // semantics live entirely within the eval-compile unit.
    run("""val r: Int = eval[Int]("inline val x = 7; x * x")""")
    assertContains("val r: Int = 49", storedOutput())
  }

  @Test def callingInlineDefWithCapturedArgFromBody = initially {
    // Inline def called with a capture (`n`). The wrapper compile
    // sees the binding for `n`, then inlines `succ(n)` into its body.
    run(
      """|inline def succ(inline x: Int): Int = x + 1
         |val n = 99
         |val r: Int = eval[Int]("succ(n)")""".stripMargin
    )
    assertContains("val r: Int = 100", storedOutput())
  }

  @Test def callingPolymorphicInlineDefFromBody = initially {
    // Inline def with a type parameter. `firstOf[T](a, b)` returns the
    // first arg unchanged; the wrapper compile inlines the body and
    // resolves the type parameter against the call-site `[Int]`.
    run(
      """|inline def firstOf[T](inline a: T, inline b: T): T = a
         |val r: Int = eval[Int]("firstOf[Int](7, 100)")""".stripMargin
    )
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def inlineDefMixedInlineAndRegularParams = initially {
    // Mixed inline + regular value params. The non-inline `b` is
    // evaluated normally; the inline `a` is substituted at compile
    // time. Either way the result is the same; this is a smoke test
    // confirming the wrapper compile handles mixed-mode signatures.
    run(
      """|inline def addBoth(inline a: Int, b: Int): Int = a + b
         |val k = 10
         |val r: Int = eval[Int]("addBoth(3, k)")""".stripMargin
    )
    assertContains("val r: Int = 13", storedOutput())
  }

  @Test def readsOuterLazyVal = initially {
    // Mirrors the outer-lazy-val portion of `tests/debug/eval-lazy-val.scala`.
    // A REPL-session-level lazy val is reachable inside the body; the
    // first read forces the computation, subsequent reads cache.
    run(
      """|object A:
         |  lazy val z: Int = { println("forcing z"); 7 }
         |val a: Int = eval[Int]("A.z")
         |val b: Int = eval[Int]("A.z + A.z")""".stripMargin
    )
    val out = storedOutput()
    assertContains("val a: Int = 7", out)
    assertContains("val b: Int = 14", out)
    // The lazy val computation should have run exactly once across
    // both eval calls. The second call doesn't re-force it.
    val forced = out.linesIterator.count(_ == "forcing z")
    assertTrue(s"expected 'forcing z' to print exactly once, got $forced:\n$out", forced == 1)
  }

  @Test def reachesPublicMethodOnSessionObject = initially {
    // A session-level object's public methods are reachable from the
    // body via the auto-injected `import rs$line$N.*`.
    run(
      """|object A:
         |  def public1(): String = "public"
         |  def public2(s: String): String = s"public($s)"
         |val r1: String = eval[String]("A.public1()")
         |val r2: String = eval[String]("A.public2(\"x\")")""".stripMargin
    )
    val out = storedOutput()
    assertContains("""val r1: String = "public"""", out)
    assertContains("""val r2: String = "public(x)"""", out)
  }

  @Test def reachesPrivateObjectMethodFromInside = initially {
    // Mirrors the in-scope half of `tests/debug/eval-static-methods.scala`:
    // the eval call sits inside a method of the same object that
    // declares the private member. The body's bare `secret("x")`
    // resolves through the typer (the call site is inside `A`) and
    // ExtractEvalBody re-routes the private call through the
    // reflective `MethodCall` strategy so the bytecode-level access
    // check (which fires from `__EvalExpression`, not from `A`) is
    // bypassed.
    run(
      """|object A:
         |  private def secret(s: String): String = s"secret($s)"
         |  def query(): String = eval[String]("secret(\"x\")")
         |val r: String = A.query()""".stripMargin
    )
    assertContains("""val r: String = "secret(x)"""", storedOutput())
  }

  @Test def reachesPrivateObjectMethodViaThisQualifier = initially {
    // Same scenario, but the body uses an explicit `this.secret(...)`
    // qualifier. The typed tree's qualifier is `This(A.module-class)`,
    // a session-level module. Bytecode resolves to `A$.MODULE$`, but
    // the access check still rejects private members from outside `A`
    // unless we route through reflection.
    run(
      """|object A:
         |  private def secret(s: String): String = s"secret($s)"
         |  def query(): String = eval[String]("this.secret(\"x\")")
         |val r: String = A.query()""".stripMargin
    )
    assertContains("""val r: String = "secret(x)"""", storedOutput())
  }

  @Test def replSessionEnumValuesAccessible = initially {
    // Mirrors `tests/debug/eval-enum.scala`: a REPL-session-level
    // enum's cases are reachable from the body, both as values and
    // for member access.
    run(
      """|enum Color(val rgb: Int):
         |  case Red   extends Color(0xff0000)
         |  case Green extends Color(0x00ff00)
         |  case Blue  extends Color(0x0000ff)
         |val r1: Int = eval[Int]("Color.Red.rgb")
         |val r2: Color = eval[Color]("Color.Green")""".stripMargin
    )
    val out = storedOutput()
    assertContains("val r1: Int = 16711680", out)
    assertContains("val r2: Color = Green", out)
  }

  @Test def defaultParamCapturedWhenCallerOmits = initially {
    // The caller omits `y`, so the typer fills in the default `10` at
    // the call site. By the time `f`'s body runs, `y = 10` is bound
    // like any other parameter and the eval body captures it.
    run(
      """|def f(x: Int, y: Int = 10): Int =
         |  eval[Int]("x + y")
         |val r: Int = f(3)""".stripMargin
    )
    assertContains("val r: Int = 13", storedOutput())
  }

  @Test def defaultParamOverriddenByCaller = initially {
    // Same setup, caller passes both: the default is unused; the
    // captured `y` carries the call-site value.
    run(
      """|def f(x: Int, y: Int = 10): Int =
         |  eval[Int]("x + y")
         |val r: Int = f(3, 99)""".stripMargin
    )
    assertContains("val r: Int = 102", storedOutput())
  }

  @Test def memberFunctionWithDefaultParamCaptured = initially {
    // Class method whose default references a class field. The body
    // captures both the method param `n` and the implicit `this`,
    // and `n` defaulted to `base` is bound by the time the body runs.
    run(
      """|class Box(val base: Int):
         |  def f(n: Int = base + 1): Int = eval[Int]("base + n")
         |val r: Int = (new Box(10)).f()""".stripMargin
    )
    assertContains("val r: Int = 21", storedOutput())
  }

  @Test def memberFunctionDefaultParamOverriddenByCaller = initially {
    run(
      """|class Box(val base: Int):
         |  def f(n: Int = base + 1): Int = eval[Int]("base + n")
         |val r: Int = (new Box(10)).f(7)""".stripMargin
    )
    assertContains("val r: Int = 17", storedOutput())
  }

  @Test def defaultArgExpressionAsBody = initially {
    // Mirrors `tests/debug/eval-at-default-arg.scala`: the body is
    // an expression that *would* serve as a default argument for a
    // captured parameter. There's nothing special about this in V2
    // (the body is just an expression mentioning the captured `x`),
    // but it pinned a real bug in the debug pipeline so we keep
    // a regression test for the same shape.
    run(
      """|def foo(x: Int): Int =
         |  eval[Int]("x + 1")
         |foo(3)""".stripMargin
    )
    assertContains("val res0: Int = 4", storedOutput())
  }

  // ===========================================================================
  // 35. Case classes: synthesised methods (apply, copy, unapply, equals,
  //     toString), generics, defaults, eval-body-defined hierarchies. These
  //     all ride on `import rs$line$N.{*}` so the synthetic companion is
  //     visible inside the eval body.
  // ===========================================================================

  @Test def caseClassCopyOnCapturedValue = initially {
    // Synthesised `copy` is reachable on a captured case-class value.
    run(
      """|case class Pt(x: Int, y: Int)
         |val p: Pt = Pt(3, 4)
         |val q: Pt = eval[Pt]("p.copy(y = 99)")""".stripMargin
    )
    assertContains("val q: Pt = Pt(x = 3, y = 99)", storedOutput())
  }

  @Test def caseClassEqualsAndHashCodeInBody = initially {
    // The auto-generated `equals` makes `==` a structural compare;
    // `hashCode` consistent.
    run(
      """|case class Pair(a: Int, b: Int)
         |val r: Boolean = eval[Boolean]("Pair(1, 2) == Pair(1, 2)")
         |val h: Boolean = eval[Boolean]("Pair(1, 2).hashCode == Pair(1, 2).hashCode")""".stripMargin
    )
    val out = storedOutput()
    assertContains("val r: Boolean = true", out)
    assertContains("val h: Boolean = true", out)
  }

  @Test def caseClassToStringInBody = initially {
    run(
      """|case class Pt(x: Int, y: Int)
         |val s: String = eval[String]("Pt(2, 5).toString")""".stripMargin
    )
    assertContains("""val s: String = "Pt(2,5)"""", storedOutput())
  }

  @Test def caseClassDestructuringValDef = initially {
    // `val Pt(x, y) = pt` desugars through the synthesised `unapply`.
    run(
      """|case class Pt(x: Int, y: Int)
         |def f(pt: Pt): Int =
         |  eval[Int]("val Pt(a, b) = pt; a * 10 + b")
         |val r: Int = f(Pt(3, 4))""".stripMargin
    )
    assertContains("val r: Int = 34", storedOutput())
  }

  @Test def caseClassReturnedFromEvalBody = initially {
    // The body builds and returns a session-defined case-class value;
    // `expectedType = "Pt"` propagates through the splice.
    run(
      """|case class Pt(x: Int, y: Int)
         |val p: Pt = eval[Pt]("Pt(7, 11)")""".stripMargin
    )
    assertContains("val p: Pt = Pt(x = 7, y = 11)", storedOutput())
  }

  @Test def genericCaseClassConstructedInBody = initially {
    run(
      """|case class Box[T](v: T)
         |val r: Int = eval[Int]("Box[Int](42).v")""".stripMargin
    )
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def caseClassWithDefaultArgsConstructed = initially {
    // `Pt()` triggers the synthesised `Pt.apply$default$N` accessors.
    run(
      """|case class Pt(x: Int = 1, y: Int = 2)
         |val r: Int = eval[Int]("val p = Pt(); p.x + p.y")""".stripMargin
    )
    assertContains("val r: Int = 3", storedOutput())
  }

  @Test def caseClassUnapplyInsideLambda = initially {
    // Pattern-binding inside a lambda body that the eval body returns.
    run(
      """|case class Pt(x: Int, y: Int)
         |val pts: List[Pt] = List(Pt(1, 2), Pt(3, 4), Pt(5, 6))
         |val sums: List[Int] = pts.map(p => eval[Int]("val Pt(a, b) = p; a + b"))""".stripMargin
    )
    assertContains("List(3, 7, 11)", storedOutput())
  }

  @Test def pathDependentInnerClassIdentity = initially {
    // Path-dependent typing: `outer1.Inner` and `outer2.Inner` are
    // *different* types. The eval bodies each construct an instance
    // of `<outerN>.Inner` and the surrounding vals ascribe to the
    // matching path, so the wrapper compile's typer enforces that
    // each eval returns the path-aligned type.
    run(
      """|class Outer:
         |  class Inner(val v: Int)
         |val outer1 = new Outer
         |val outer2 = new Outer
         |val a: outer1.Inner = eval[outer1.Inner]("new outer1.Inner(7)")
         |val b: outer2.Inner = eval[outer2.Inner]("new outer2.Inner(9)")
         |val sum: Int = a.v + b.v""".stripMargin
    )
    assertContains("val sum: Int = 16", storedOutput())
  }

  @Test def pathDependentInnerClassMismatchRejected = initially {
    // The reverse case: assigning `outer1.Inner` where `outer2.Inner`
    // is expected. The eval body produces an `outer1.Inner` instance,
    // the surrounding val ascribes `outer2.Inner`. The typer rejects
    // this as a type mismatch (path-dependent identity is preserved
    // across the eval boundary).
    run(
      """|class Outer:
         |  class Inner(val v: Int)
         |val outer1 = new Outer
         |val outer2 = new Outer
         |val mismatch: outer2.Inner = eval[outer2.Inner]("new outer1.Inner(7)")""".stripMargin
    )
    val out = storedOutput()
    assertTrue(
      s"expected the eval call to fail (path-dependent type mismatch), got:\n$out",
      out.contains("EvalCompileException")
        || out.contains("Found:")
        || out.contains("type mismatch")
    )
  }

  @Test def caseClassMatchDispatchOverList = initially {
    // Captured list of case-class values, body matches each element.
    run(
      """|sealed trait Op
         |case class Inc(by: Int) extends Op
         |case class Dec(by: Int) extends Op
         |case object Reset extends Op
         |val ops: List[Op] = List(Inc(3), Dec(1), Reset, Inc(10))
         |val sum: Int = ops.foldLeft(0)((acc, op) =>
         |  eval[Int]("op match { case Inc(by) => acc + by; case Dec(by) => acc - by; case Reset => 0 }"))""".stripMargin
    )
    assertContains("val sum: Int = 10", storedOutput())
  }

  @Test def caseClassWithVarConstructorParam = initially {
    // `case class Counter(var n: Int)`: the auto-generated setter
    // (`n_=`) drives the FieldAssign reflective lowering.
    run(
      """|case class Counter(var n: Int)
         |val c: Counter = Counter(0)
         |eval[Unit]("c.n = c.n + 5")
         |eval[Unit]("c.n = c.n + 7")
         |val r: Int = c.n""".stripMargin
    )
    assertContains("val r: Int = 12", storedOutput())
  }

  // ===========================================================================
  // 36. Value classes (`extends AnyVal`). The JVM erases them to their single
  //     underlying field, so a body that constructs, reads, or passes a
  //     value-class instance has to round-trip through that erasure.
  // ===========================================================================

  @Test def anyValClassConstructedAndAccessedInBody = initially {
    run(
      """|class Wei(val grams: Int) extends AnyVal
         |val r: Int = eval[Int]("new Wei(42).grams + 1")""".stripMargin
    )
    assertContains("val r: Int = 43", storedOutput())
  }

  @Test def anyValClassCapturedAndReadInBody = initially {
    // The captured `w` is erased to a bare `Int` at the boundary; the
    // body's `w.grams` re-projects out of the underlying field.
    run(
      """|class Wei(val grams: Int) extends AnyVal
         |val w: Wei = new Wei(7)
         |val r: Int = eval[Int]("w.grams * 6")""".stripMargin
    )
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def anyValClassWithMethodCalledInBody = initially {
    run(
      """|class Pct(val n: Int) extends AnyVal:
         |  def doubled: Int = n * 2
         |val r: Int = eval[Int]("new Pct(21).doubled")""".stripMargin
    )
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def anyValClassReturnedFromEval = initially {
    run(
      """|class Wei(val grams: Int) extends AnyVal
         |val w: Wei = eval[Wei]("new Wei(99)")
         |val g: Int = w.grams""".stripMargin
    )
    assertContains("val g: Int = 99", storedOutput())
  }

  @Test def anyValClassAsBlockLocalOuterCapture = initially {
    // The eval call captures a *block-local* AnyVal val. At runtime
    // the body reaches through `getValue("w")` (returns Object), then
    // `asInstanceOf[Wei]` rewraps it; `.grams` unboxes back to Int.
    // This is the path the value class actually has to round-trip
    // through the bindings array.
    run(
      """|class Wei(val grams: Int) extends AnyVal
         |def f(): Int =
         |  val w: Wei = new Wei(7)
         |  eval[Int]("w.grams * 6")
         |val r: Int = f()""".stripMargin
    )
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def anyValClassAsLambdaParamCapture = initially {
    // Same boundary as above but the AnyVal is a lambda param,
    // captured into the bindings array because the body's `evaluate`
    // is moved out of the lambda.
    run(
      """|class Wei(val grams: Int) extends AnyVal
         |val xs: List[Wei] = List(new Wei(1), new Wei(2), new Wei(3))
         |val r: List[Int] = xs.map(w => eval[Int]("w.grams * 10"))""".stripMargin
    )
    assertContains("List(10, 20, 30)", storedOutput())
  }

  @Test def caseClassExtendingAnyValInBody = initially {
    // Case-class `extends AnyVal` (a "value class with auto-generated
    // equals/copy"). Combines the case-class apply path with the
    // erased-to-underlying boundary.
    run(
      """|case class Tag(val s: String) extends AnyVal
         |val r: Int = eval[Int]("Tag(\"hi\").s.length + 1")""".stripMargin
    )
    assertContains("val r: Int = 3", storedOutput())
  }

  @Test def patchSpanAssignmentRoundtrip = initially {
    // Mirrors `tests/debug/eval-i425.scala`: a body that reads a
    // case-class field, then writes it via the synthesised `_=`
    // setter, then reads again. Exercises the V2 reflective field
    // path for both directions.
    run(
      """|case class Span(value: Int)
         |class Patch(var span: Span)
         |val patch: Patch = new Patch(Span(0))
         |val before: Int = eval[Int]("patch.span.value")
         |eval[Unit]("patch.span = Span(1)")
         |val after: Int = eval[Int]("patch.span.value")""".stripMargin
    )
    val out = storedOutput()
    assertContains("val before: Int = 0", out)
    assertContains("val after: Int = 1", out)
  }

  // ===========================================================================
  // 37. Custom @evalLike / @evalSafeLike user-defined generators.
  //
  //     Anyone can declare an eval-like generator by annotating a function
  //     with `@evalLike` (throwing variant, returns T) or `@evalSafeLike`
  //     (non-throwing, returns EvalResult[T]). The post-PostTyper rewriter
  //     recognises these annotations and fills the synthetic `_bindings`,
  //     `_expectedType`, and `_enclosingSource` slots *by name*, so the
  //     user's signature can declare these parameters in any order with
  //     arbitrary additional parameters around them. `agent`'s trailing
  //     `maxAttempts: Int` is the canonical example.
  // ===========================================================================

  @Test def customEvalLikeFillsBindingsAndType = initially {
    // Smoke test: a thin wrapper around `Eval.eval` annotated with
    // `@evalLike`. The rewriter must fill `bindings` (so `x` is
    // captured and resolves in the body) and `expectedType` (so the
    // body type-checks against `Int`).
    run(
      """|import dotty.tools.repl.eval.Eval
         |@dotty.tools.repl.eval.evalLike
         |def myEval[T](
         |    body: String,
         |    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |    expectedType: String = "",
         |    enclosingSource: String = ""
         |): T =
         |  Eval.eval[T](body, bindings, expectedType, enclosingSource)
         |val x = 5
         |val r = myEval[Int]("x + 10")""".stripMargin
    )
    assertContains("val r: Int = 15", storedOutput())
  }

  @Test def customEvalLikeWithTrailingPositionalParamLeftAlone = initially {
    // The synthetic args sit *between* the user's own parameters and a
    // trailing positional one (mimicking `agent`'s `maxAttempts`).
    // Rewriter must use named-param matching so the trailing param
    // keeps its caller-supplied value (here `42`, not the default 1).
    run(
      """|import dotty.tools.repl.eval.{Eval, evalLike}
         |@evalLike
         |def withSeed[T](
         |    body: String,
         |    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |    expectedType: String = "",
         |    enclosingSource: String = "",
         |    seed: Int = 1
         |): (T, Int) =
         |  (Eval.eval[T](body, bindings, expectedType, enclosingSource), seed)
         |val r = withSeed[Int]("21", seed = 42)""".stripMargin
    )
    assertContains("val r: (Int, Int) = (21, 42)", storedOutput())
  }

  @Test def customEvalSafeLikeWrapsHandleCompileError = initially {
    // `@evalSafeLike` returns `EvalResult[T]`; the rewriter wraps the
    // encl-source's marker in `Eval.handleCompileError(...)` so the
    // verify compile lifts the body's `T` to `EvalResult[T]`. A
    // deliberate compile failure (referencing undefined `mystery`)
    // should surface as a failed `EvalResult` rather than throw.
    run(
      """|import dotty.tools.repl.eval.{Eval, EvalResult, evalSafeLike}
         |@evalSafeLike
         |def mySafe[T](
         |    body: String,
         |    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |    expectedType: String = "",
         |    enclosingSource: String = ""
         |): EvalResult[T] =
         |  Eval.evalSafe[T](body, bindings, expectedType, enclosingSource)
         |val r = mySafe[Int]("mystery + 1").isSuccess""".stripMargin
    )
    assertContains("val r: Boolean = false", storedOutput())
  }

  @Test def customEvalLikeWithReorderedSyntheticParams = initially {
    // Synthetic params can be declared in any order; the rewriter
    // matches by name, not by position. Here `enclosingSource`
    // comes before `bindings`, the opposite of the canonical layout.
    run(
      """|import dotty.tools.repl.eval.{Eval, evalLike}
         |@evalLike
         |def myReorder[T](
         |    body: String,
         |    enclosingSource: String = "",
         |    expectedType: String = "",
         |    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding]
         |): T =
         |  Eval.eval[T](body, bindings, expectedType, enclosingSource)
         |val n = 3
         |val r = myReorder[Int]("n * n")""".stripMargin
    )
    assertContains("val r: Int = 9", storedOutput())
  }

  @Test def shadowingEvalNotRewritten = initially {
    // A user-defined function literally named `eval` that is *not*
    // owned by `dotty.tools.repl.eval.Eval` and *not* annotated with
    // `@evalLike` is left alone; it's just a regular method call.
    // `bindings`/etc. stay at their declared defaults.
    run(
      """|object Shadow:
         |  def eval[T](code: String): String = "shadow:" + code
         |val r = Shadow.eval[Int]("ignored")""".stripMargin
    )
    assertContains("""val r: String = "shadow:ignored"""", storedOutput())
  }

  @Test def evalLikeAsMemberFunction = initially {
    // `@evalLike` on a *member* function works the same as on a
    // top-level def: the rewriter classifies by symbol+annotation
    // (not by name or owner), and the call site `repo.myEval[Int](...)`
    // gets the synthetic args filled.
    run(
      """|import dotty.tools.repl.eval.{Eval, evalLike}
         |class Repo:
         |  @evalLike
         |  def myEval[T](
         |      body: String,
         |      bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |      expectedType: String = "",
         |      enclosingSource: String = ""
         |  ): T =
         |    Eval.eval[T](body, bindings, expectedType, enclosingSource)
         |val r0 = (new Repo).myEval[Int]("100 + 1")
         |val k = 7
         |val r1 = (new Repo).myEval[Int]("k + 3")""".stripMargin
    )
    val out = storedOutput()
    assertContains("val r0: Int = 101", out)
    assertContains("val r1: Int = 10", out)
  }

  @Test def evalLikeWithUsingParameter = initially {
    // `@evalLike` function declares an extra `using` parameter (e.g.
    // a context object). The rewriter fills the three synthetic
    // slots by name; the using-param is left alone for the typer's
    // implicit-search machinery, so the caller's given still applies.
    run(
      """|import dotty.tools.repl.eval.{Eval, evalLike}
         |trait Tag:
         |  def label: String
         |@evalLike
         |def labelled[T](
         |    body: String,
         |    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |    expectedType: String = "",
         |    enclosingSource: String = ""
         |)(using tag: Tag): (String, T) =
         |  (tag.label, Eval.eval[T](body, bindings, expectedType, enclosingSource))
         |given Tag with
         |  def label: String = "ok"
         |val r = labelled[Int]("40 + 2")""".stripMargin
    )
    assertContains("""val r: (String, Int) = ("ok", 42)""", storedOutput())
  }

  @Test def evalWithFixedTypeArgument = initially {
    // `eval` called with a non-generic, fixed `T` (no inference). The
    // expectedType slot gets filled with the rendered type string
    // ("Int"), the body still resolves the captured `n` correctly.
    run(
      """|val n = 6
         |val r: Int = eval[Int]("n * n")""".stripMargin
    )
    assertContains("val r: Int = 36", storedOutput())
  }

  @Test def evalLikeWithFixedTypeArgument = initially {
    // Same fixed-T story but for an `@evalLike` user wrapper:
    // confirms expectedType filling uses the wrapper's `[T]`, not
    // a placeholder.
    run(
      """|import dotty.tools.repl.eval.{Eval, evalLike}
         |@evalLike
         |def myEvalT[T](
         |    body: String,
         |    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |    expectedType: String = "",
         |    enclosingSource: String = ""
         |): T =
         |  Eval.eval[T](body, bindings, expectedType, enclosingSource)
         |val s: String = myEvalT[String]("\"hi-there\".reverse")""".stripMargin
    )
    assertContains("""val s: String = "ereht-ih"""", storedOutput())
  }

  @Test def evalCallerSuppliesAllThreeBypassesRewriter = initially {
    // All-or-nothing escape hatch: if the caller supplies all three
    // synthetic args explicitly, the rewriter leaves the call alone.
    // To opt out of automatic capture-filling AND skip the encl-source
    // splice (sandboxed compile), the caller passes a self-contained
    // encl that already wraps the marker in a `val __unused__`-style
    // form so the wrapper compile has a place to splice the body.
    run(
      """|import dotty.tools.repl.eval.Eval
         |val n = 99
         |val r: Int = Eval.eval[Int](
         |  "1 + 2",
         |  Array.empty[Eval.Binding],
         |  "Int",
         |  s"val __unused__ : Any = { ${dotty.tools.repl.eval.EvalContext.placeholder} }"
         |)""".stripMargin
    )
    assertContains("val r: Int = 3", storedOutput())
  }

  @Test def evalLikeForwardingChainSeesOuterCaptures = initially {
    // Two-level forwarding: outer `@evalLike outerCall` delegates to
    // an inner `@evalLike innerCall` which delegates to `Eval.eval`.
    // At each hop the rewriter must leave the forwarded `bindings` /
    // `expectedType` / `enclosingSource` Idents alone so the original
    // capture (`x`) flows all the way through.
    run(
      """|import dotty.tools.repl.eval.{Eval, evalLike}
         |@evalLike
         |def innerCall[T](
         |    body: String,
         |    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |    expectedType: String = "",
         |    enclosingSource: String = ""
         |): T =
         |  Eval.eval[T](body, bindings, expectedType, enclosingSource)
         |@evalLike
         |def outerCall[T](
         |    body: String,
         |    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |    expectedType: String = "",
         |    enclosingSource: String = ""
         |): T =
         |  innerCall[T](body, bindings, expectedType, enclosingSource)
         |val x = 11
         |val r = outerCall[Int]("x + 4")""".stripMargin
    )
    assertContains("val r: Int = 15", storedOutput())
  }

  @Test def evalLikeMixedStateRejected = initially {
    // The all-or-nothing rule: caller supplies `bindings` explicitly
    // but leaves `expectedType` and `enclosingSource` to defaults.
    // The rewriter rejects this as ill-formed because filling the
    // two left-default slots would silently override whatever the
    // caller's partial set was meant to express.
    run(
      """|import dotty.tools.repl.eval.{Eval, evalLike}
         |@evalLike
         |def myEval[T](
         |    body: String,
         |    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |    expectedType: String = "",
         |    enclosingSource: String = ""
         |): T =
         |  Eval.eval[T](body, bindings, expectedType, enclosingSource)
         |val r = myEval[Int]("1 + 2", Array.empty[Eval.Binding])""".stripMargin
    )
    assertContains("partial set of synthetic arguments", storedOutput())
  }

  @Test def evalLikeOnCompanionObjectMethod = initially {
    // `@evalLike` on a companion-object method (a `def` on `object Repo`).
    // Symbol-based classification picks it up the same as any other
    // module-level def, including the `__this__` synthetic for the
    // companion's own scope.
    run(
      """|import dotty.tools.repl.eval.{Eval, evalLike}
         |object Repo:
         |  @evalLike
         |  def evalIt[T](
         |      body: String,
         |      bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |      expectedType: String = "",
         |      enclosingSource: String = ""
         |  ): T =
         |    Eval.eval[T](body, bindings, expectedType, enclosingSource)
         |val a = 8
         |val b = 9
         |val r: Int = Repo.evalIt[Int]("a * b")""".stripMargin
    )
    assertContains("val r: Int = 72", storedOutput())
  }

  @Test def evalSafeLikeMustReturnEvalResult = initially {
    // The rewriter validates that an `@evalSafeLike`-annotated method
    // returns an `EvalResult[?]`. A function annotated `@evalSafeLike`
    // but returning `T` directly is rejected so the caller doesn't
    // get a body that's silently double-wrapped.
    run(
      """|import dotty.tools.repl.eval.{Eval, evalSafeLike}
         |@evalSafeLike
         |def wrong[T](
         |    body: String,
         |    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
         |    expectedType: String = "",
         |    enclosingSource: String = ""
         |): T =
         |  Eval.eval[T](body, bindings, expectedType, enclosingSource)
         |val r = wrong[Int]("1 + 2")""".stripMargin
    )
    assertContains("must return an `EvalResult[?]`", storedOutput())
  }

  // ===========================================================================
  // KNOWN LIMITATIONS
  //
  // Each test below pins a documented shape that the V2 pipeline cannot
  // currently handle. We assert on the failure mode (specific diagnostic or
  // exception type) so that a future fix flips the test rather than letting
  // the limitation regress silently. When a limitation is lifted, flip the
  // test to assert the success result and move it into the relevant section
  // above.
  //
  // Open limitations:
  //   1. Body that defines a fresh case class (synthesised companion methods
  //      reference the new class's `this`, unreachable from the wrapper).
  //   2. Eval inside a method of a *local* class (member references can't
  //      thread through the wrapper-class boundary).
  //   3. *Local* case-class apply/unapply invoked from the body
  //      (companion's lazy initializer can't be proxied across the boundary).
  //   4. Private *val* on a session-level object accessed from the body via
  //      a method on that same object. The V2 compile re-declares the object
  //      inside `__EvalWrapper_*` and Scala 3's nested-object lowering drops
  //      the private val, so reflective `getField` walks a stripped class.
  //      Fixing this requires lifting module methods out of their containing
  //      ModuleDef the same way `SpliceEvalBody.ClassMethodExtractor` lifts
  //      class methods. (No test for this one yet; tracked in EVAL.md.)
  // ===========================================================================

  @Test def bodyDefinesCaseClassRejected = initially {
    // Body declares a fresh case class. Synthesised companion methods
    // (`apply`, `unapply`) reference the new class's `this`, which the
    // ExtractEvalBody walk can't reach from `__Expression.evaluate`'s
    // captured owner chain. Surfaces as a fail-fast diagnostic.
    run(
      """|val r: Int = eval[Int](
         |  "case class Local(a: Int, b: Int); val l = Local(7, 8); l.a * l.b"
         |)""".stripMargin
    )
    val out = storedOutput()
    assertTrue(s"expected a 'cannot reach outer this' diagnostic, got:\n$out",
      out.contains("eval failed to compile") &&
        out.contains("cannot reach outer `this` of class `Local`"))
  }

  @Test def evalInsideLocalClassMethodKnownLimitation = initially {
    // Eval call placed inside a method of a *local* class (a class defined
    // inside an enclosing method). The rewriter reports `cannot reference
    // outer symbol <member> (owner: class <Local>)` because the body's
    // reference to a local-class member can't be threaded through the
    // wrapper-class boundary. Local-class *instances* held in outer-method
    // vals do work; the eval-inside-local-class-method case still needs the
    // synthetic helpers routed through the same reflective dispatch path.
    run(
      """|def make(): Int =
         |  class L(val k: Int):
         |    def reach: Int = eval[Int]("k * 3")
         |  (new L(11)).reach
         |make()""".stripMargin
    )
    val out = storedOutput()
    assertTrue(
      s"expected eval compile failure with outer-symbol diagnostic, got:\n$out",
      out.contains("cannot reference outer symbol")
        || out.contains("EvalCompileException")
    )
  }

  @Test def localCaseClassApplyAndUnapplyInBodyKnownLimitation = initially {
    // A *local* case class defined inside a method, whose companion's
    // synthesised `apply` would construct an instance and whose `unapply`
    // would destructure it, both invoked from inside an eval body. The
    // companion's lazy initializer (`Pt$lzy1`) lives in the user's outer
    // method's scope and LambdaLift can't thread a proxy for it across the
    // wrapper-class boundary, so the eval compile reports "Could not find
    // proxy for lazy var Pt$lzy1". Once local-class-companion synthetics
    // are routed through reflection (analogous to local-class instances),
    // flip this test to assert the success result.
    run(
      """|def f: Int =
         |  case class Pt(x: Int, y: Int)
         |  eval[Int]("{ val q = Pt(3, 4); q match { case Pt(a, b) => a + b } }")
         |f""".stripMargin
    )
    val out = storedOutput()
    assertTrue(
      s"expected the eval call to fail with a 'Could not find proxy' diagnostic, got:\n$out",
      out.contains("Could not find proxy") || out.contains("EvalCompileException")
    )
  }

end DynamicEvalTests

/** `eval`'s isolated compile inherits the live REPL's CLI flags, so
 *  options like `-Yexplicit-nulls` apply inside the body too: `null`
 *  doesn't conform to `String` once explicit-nulls is on, so a body that
 *  binds `val s: String = null` fails to compile.
 */
class DynamicEvalExplicitNullsTests extends ReplTest(
  ReplTest.defaultOptions ++ Array("-Yexplicit-nulls")
):
  // -- Flag forwarding: the eval driver inherits the live REPL's
  //    `-Yexplicit-nulls`, so the body is type-checked under the
  //    same nullability discipline as the surrounding session.
  //    Tests below pin the round-trip; anything stricter (flow
  //    typing through bindings, narrowing across the eval boundary)
  //    is intentionally out of scope: a captured nullable that
  //    flows into a body string crosses the eval boundary, and the
  //    body shouldn't depend on the typer remembering it was a
  //    stable val on the outside.

  @Test def explicitNullsRejectsNullForString =
    // The classic case: `val s: String = null` is unsound under
    // explicit-nulls. The eval driver compiles the body with the
    // flag on and rejects the assignment.
    initially {
      run("""val r: String = eval("val s: String = null; s")""")
      val out = storedOutput()
      assertTrue(
        s"expected an explicit-nulls compile error, got:\n$out",
        out.contains("Null") || out.contains("Found:") || out.contains("failed to compile")
      )
    }

  @Test def explicitNullsAcceptsStringOrNull =
    // The legal counterpart: `null` does conform to `String | Null`,
    // so the same shape of body type-checks when the type
    // ascription opens the union. Confirms the flag isn't blocking
    // legitimate nullable usage; only the unsafe form.
    initially {
      run("""val r: String | Null = eval[String | Null]("null")""")
      assertContains("val r: String | Null = null", storedOutput())
    }

  @Test def explicitNullsRejectsDerefOfCapturedNullable =
    // A captured `val s: String | Null` is forwarded as-is into the
    // body. Calling `.length` directly is rejected because under
    // explicit-nulls `String | Null` doesn't have `.length`. This
    // pins that the captured binding's *type* (not just the local
    // body's literal `null` handling) participates in the flag's
    // discipline.
    initially {
      run(
        """|val s: String | Null = "hi"
           |val r: Int = eval[Int]("s.length")""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected an explicit-nulls compile error, got:\n$out",
        out.contains("failed to compile") || out.contains("Null") || out.contains("does not")
      )
    }

  @Test def explicitNullsAcceptsCapturedNonNullableString =
    // Sanity check: a non-nullable `val s: String` capture continues
    // to work. The flag doesn't introduce spurious nullability for
    // values that weren't declared nullable.
    initially {
      run(
        """|val s: String = "world"
           |val r: Int = eval[Int]("s.length")""".stripMargin
      )
      assertContains("val r: Int = 5", storedOutput())
    }
end DynamicEvalExplicitNullsTests

/** Capture-checking is enabled at the REPL via
 *  `-language:experimental.captureChecking`. The flag is forwarded into the
 *  eval driver's compilation, so a capture-violating body fails with a
 *  capture-checker diagnostic rather than silently compiling.
 */
class DynamicEvalCaptureCheckingTests extends ReplTest(
  ReplTest.defaultOptions ++ Array("-language:experimental.captureChecking")
):
  @Test def pureFunctionTypeAvailableInEval =
    // `->` is the pure-function arrow that only parses under
    // `experimental.captureChecking`; if the flag wasn't forwarded the
    // body would fail with a syntax / undefined-type error instead of
    // compiling.
    initially {
      run("""val r: Int = eval("val f: Int -> Int = (n: Int) => n + 1; f(41)")""")
      assertContains("val r: Int = 42", storedOutput())
    }

  @Test def captureViolationDetectedInEval =
    initially {
      run(
        """|import caps.*
           |class IO extends SharedCapability
           |val io: IO = new IO""".stripMargin
      )
    } andThen {
      storedOutput()
      // Pure function (`->`) capturing `io` (a SharedCapability) is what
      // capture checking rejects.
      run("""val r = eval("val f: () -> String = () => io.toString; f()")""")
      val out = storedOutput()
      assertTrue(
        s"expected a capture-checking error, got:\n$out",
        out.contains("captures") ||
          out.contains("capability") ||
          out.contains("flow") ||
          out.contains("failed to compile")
      )
    }

  // ===========================================================================
  // Capture-faithful eval via the verification compile pass
  //
  // The runtime splices the (now known) eval body string back into a
  // copy of the enclosing top-level statement and re-typechecks the
  // result under the original lexical context. That catches CC
  // violations the wrapper-compile path can't see, because the wrapper
  // erases capture sets on binding-parameter types (an `IO^` parameter
  // arrives as plain `IO`), so a pure-function position rejecting the
  // capability never gets a chance to fire there.
  // ===========================================================================

  @Test def captureViolationOnIoParamInsidePureLambdaRejected =
    initially {
      run(
        """|import caps.*
           |trait C[T]:
           |  def map[U](op: T -> U): C[U] = ???
           |class IO extends SharedCapability
           |class CImpl extends C[Int]
           |def f(i: Int, io: IO^, c: C[Int]) =
           |  eval[Any]("c.map(x => io.toString)")
           |f(1, new IO, new CImpl)""".stripMargin
      )
      val out = storedOutput()
      // The body `x => io.toString` captures the `io: IO^` parameter
      // inside an `op: T -> U` (pure) position. The wrapper compile
      // erases the `^` on the binding type and so doesn't see it; the
      // verification pass re-checks the original `def f(i: Int, io: IO^, ...)`
      // with the body inlined and rejects.
      assertTrue(
        s"expected a capture-checking failure, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("captures") || out.contains("capability") || out.contains("flow"))
      )
    }

  @Test def pureBodyOnIoParamAccepted =
    initially {
      run(
        """|import caps.*
           |trait C[T]:
           |  def map[U](op: T -> U): C[U] = ???
           |class IO extends SharedCapability
           |class CImpl extends C[Int]:
           |  override def map[U](op: Int -> U): C[U] = new CImpl().asInstanceOf[C[U]]
           |def g(i: Int, io: IO^, c: C[Int]) =
           |  eval[Any]("c.map(x => i)")
           |g(1, new IO, new CImpl)
           |println("g succeeded")""".stripMargin
      )
      val out = storedOutput()
      // `x => i` only captures the pure `Int` parameter `i`. The
      // verification compile and the wrapper compile both accept it.
      assertContains("g succeeded", out)
      assertTrue(
        s"expected no eval failure, got:\n$out",
        !out.contains("failed to compile")
      )
    }

  @Test def captureViolationInNestedEvalOnly =
    initially {
      // The outer eval body is a plain `eval[Any]("...")` call: no
      // CC concerns *at that level*. The CC violation lives inside
      // the inner eval body: `c.map(x => io.toString)` captures `io`
      // into a `T -> U` (pure) lambda position. This exercises the
      // nested-context chaining: the outer eval's `enclosingSource`
      // is `def f(...) = <Marker>`, and when the rewriter walks the
      // outer body it composes
      //   def f(...) = ({ <outerBodyWithInnerMarker> })
      // as the inner eval's `enclosingSource`. The inner verification
      // pass splices the inner body in and CC sees the full original
      // context (def + outer body + inner body) at once.
      run(
        """|import caps.*
           |trait C[T]:
           |  def map[U](op: T -> U): C[U] = ???
           |class IO extends SharedCapability
           |class CImpl extends C[Int]
           |def f(i: Int, io: IO^, c: C[Int]): Any =
           |  eval[Any]("eval[Any](\"c.map(x => io.toString)\")")
           |f(1, new IO, new CImpl)""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected a nested capture-checking failure, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("captures") || out.contains("capability") || out.contains("flow"))
      )
    }

  @Test def capabilityLeakageThroughExpectedTypeRejected =
    // The eval body is just `c`. At the call site `c: AnyRef^`, but
    // the call site's `eval[AnyRef]` strips the capture annotation
    // off the result type. The wrapper compile sees the binding for
    // `c` as plain `Object` (bindings erase capture sets through the
    // `Eval.bind(name, value: Any)` shape) and would silently accept
    // it; the verification pass re-typechecks the original
    //     def leak(c: AnyRef^): AnyRef = ({ c })
    // with the body inlined and rejects, because `c.rd` (or `c`
    // itself) cannot flow into the empty capture set demanded by
    // `AnyRef`.
    initially {
      run(
        """|def leak(c: AnyRef^): AnyRef = eval[AnyRef]("c")
           |leak(new Object)""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected a capture-leak failure on the return type, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("captures") || out.contains("capability") || out.contains("flow"))
      )
    }

  @Test def capabilityCapturedIntoPureReturnLambdaRejected =
    // The `eval[() -> Unit]` requires a pure function. The body
    // `() => println(io)` captures `io: IO^` into the lambda's
    // capture set, so the verification pass rejects: the lambda's
    // inferred type is `() ->{io} Unit`, not `() -> Unit`, and the
    // call site's expected type cannot widen to admit `io`.
    initially {
      run(
        """|import caps.*
           |class IO extends SharedCapability
           |def mkPrinter(io: IO^): () -> Unit =
           |  eval[() -> Unit]("() => println(io)")
           |mkPrinter(new IO)""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected a capture-checking failure on the pure-lambda return, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("captures") || out.contains("capability") || out.contains("flow"))
      )
    }
end DynamicEvalCaptureCheckingTests

/** Safe mode (`-language:experimental.safe`) is a stricter cousin of plain
 *  capture checking: it implies CC + mutation tracking and additionally
 *  forbids the standard escape hatches (`asInstanceOf`, `@unchecked`,
 *  `caps.unsafe.*`, the mutable `scala.runtime.*Ref` types, etc.).
 *
 *  These tests check that, when the live REPL session is started under
 *  safe mode, the eval driver:
 *
 *    - accepts bodies that respect the safe subset;
 *    - rejects bodies that use unsafe escape hatches;
 *    - rejects bodies where a mutable var captured at the call site
 *      flows through a pure (`->`) function inside the body. The last
 *      case relies on the verification pass: the wrapper compile rewrites
 *      captured vars into a `VarRef` facade and so loses their mutability,
 *      while the verify compile re-typechecks the original lexical context
 *      with the body inlined and catches the violation.
 */
class DynamicEvalSafeModeTests extends ReplTest(
  ReplTest.defaultOptions ++ Array("-language:experimental.safe")
):
  // -- Sanity: bodies that respect the safe subset still work. --------------

  @Test def basicEvalInSafeMode =
    initially {
      run("""val r: Int = eval[Int]("1 + 2")""")
      assertContains("val r: Int = 3", storedOutput())
    }

  @Test def pureStdlibBodyInSafeMode =
    // `List(...)`, `.sum`, and integer arithmetic are inside the safe
    // subset (`scala.collection.immutable` is on `assumedSafePackages`,
    // and `scala.math` is too).
    initially {
      run("""val r: Int = eval[Int]("List(1, 2, 3).sum")""")
      assertContains("val r: Int = 6", storedOutput())
    }

  @Test def pureFunctionTypeAvailableUnderSafeMode =
    // Safe mode implies capture checking, so the pure-function arrow
    // `->` parses inside the eval body the same way it does under
    // `experimental.captureChecking`.
    initially {
      run("""val r: Int = eval[Int]("val f: Int -> Int = (n: Int) => n + 1; f(41)")""")
      assertContains("val r: Int = 42", storedOutput())
    }

  // -- Mutable var + pure function: the verification pass catches it. -------

  @Test def mutableVarCapturedByPureFunctionInBodyRejected =
    // The headline safe-mode test for eval. The body declares a pure
    // function `() -> Int` whose closure captures the enclosing method's
    // `var r`. The eval rewriter synthesises a `bindVar` argument
    // backed by a `varRef(() => r, v => r = v)` pair of `Supplier` /
    // `Consumer` JDK lambdas. Under safe mode + CC + mutation tracking,
    // those JDK lambdas have no capture set in their type, so the
    // `r.rd` (read effect on the mutable var) cannot flow into the
    // expected capture set `{}`. The whole `def f` is rejected at the
    // REPL compile, before the eval driver ever runs.
    //
    // This is the right behaviour: in safe mode you cannot capture a
    // mutable var into eval at all unless eval offers a non-pure
    // binding shape, which today it doesn't.
    initially {
      run(
        """|def f(): Int =
           |  var r: Int = 0
           |  eval[Int]("val pure: () -> Int = () => r; pure()")
           |val out: Int = f()""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected a capture-checking failure on the var capture, got:\n$out",
        out.contains("cannot flow into capture set") &&
          (out.contains("r.rd") || out.contains("Supplier") || out.contains("Consumer"))
      )
    }

  @Test def mutableVarCapturedByPureLambdaInsideEvalRejected =
    // Same shape, but the violation is inside a `map` whose function
    // argument is typed `Int -> Int` (pure). The eval body itself is
    // `c.map(x => x + r)`, innocuous on its own, but at the call site
    // `r` is a `var`, so the rewriter synthesises a `varRef(() => r, ...)`
    // for the binding and the same Supplier/Consumer-without-capture-set
    // rejection fires.
    initially {
      run(
        """|import caps.*
           |trait C[T]:
           |  def map[U](op: T -> U): C[U] = ???
           |class CImpl extends C[Int]
           |def f(c: C[Int]): Any =
           |  var r: Int = 1
           |  eval[Any]("c.map(x => x + r)")
           |val out = f(new CImpl)""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected a capture-checking failure on the var capture, got:\n$out",
        out.contains("cannot flow into capture set") &&
          (out.contains("r.rd") || out.contains("Supplier") || out.contains("Consumer"))
      )
    }

  @Test def mutableVarCapturedByImpureLambdaInsideEvalAccepted =
    // Sibling of the previous test: the same `var r` capture, but now
    // the function position is the impure `=>` (which can carry the
    // var's read effect in its inferred capture set). CC accepts.
    initially {
      run(
        """|trait C[T]:
           |  def map[U](op: T => U): C[U] = ???
           |class CImpl extends C[Int]:
           |  override def map[U](op: Int => U): C[U] = new CImpl().asInstanceOf[C[U]]
           |def g(c: C[Int]): Any =
           |  var r: Int = 1
           |  eval[Any]("c.map(x => x + r)")
           |val out = g(new CImpl)""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected the impure-lambda body to be accepted, got:\n$out",
        !out.contains("failed to compile")
      )
    }

  // -- Unsafe escape hatches inside the body are rejected. ------------------

  @Test def asInstanceOfInBodyRejected =
    // `asInstanceOf` is an unchecked cast and is one of the canonical
    // safe-mode rejections. The body is rewritten and compiled under
    // safe mode, so the wrapper compile catches it.
    initially {
      run("""val r = eval[String]("(1: Any).asInstanceOf[String]")""")
      val out = storedOutput()
      assertTrue(
        s"expected an asInstanceOf safe-mode rejection, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("asInstanceOf") || out.contains("@rejectSafe") || out.contains("safe"))
      )
    }

  @Test def uncheckedPatternInBodyRejected =
    // Pattern with `@unchecked` annotation: forgetting capture sets is
    // exactly what safe mode forbids.
    initially {
      run(
        """val r = eval[Int]("(List(1): Any) match { case xs: List[Int @unchecked] => xs.head; case _ => 0 }")"""
      )
      val out = storedOutput()
      assertTrue(
        s"expected an @unchecked safe-mode rejection, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("unchecked") || out.contains("@rejectSafe") || out.contains("safe"))
      )
    }

  @Test def capsUnsafeRefInBodyRejected =
    // Anything in `caps.unsafe` is rejected in safe mode.
    initially {
      run("""val r = eval[Any]("caps.unsafe.unsafeErasedValue[String]")""")
      val out = storedOutput()
      assertTrue(
        s"expected a caps.unsafe safe-mode rejection, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("caps.unsafe") || out.contains("unavailable in safe mode") || out.contains("safe"))
      )
    }

  @Test def runtimeMutableRefInBodyRejected =
    // `scala.runtime.LongRef` is one of the mutable scala.runtime ref
    // types tagged `@rejectSafe`. Constructing one inside the eval
    // body is rejected.
    initially {
      run("""val r = eval[Any]("new scala.runtime.LongRef(0L)")""")
      val out = storedOutput()
      assertTrue(
        s"expected a scala.runtime.LongRef safe-mode rejection, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("LongRef") || out.contains("@rejectSafe") || out.contains("safe"))
      )
    }

  @Test def consoleInBodyRejected =
    // `scala.Console` is `@rejectSafe`. Its members are unreachable
    // from a safe-mode compile, including the eval driver's wrapper.
    initially {
      run("""val r = eval[Unit]("scala.Console.out.print(\"hi\")")""")
      val out = storedOutput()
      assertTrue(
        s"expected a scala.Console safe-mode rejection, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("Console") || out.contains("@rejectSafe") || out.contains("safe"))
      )
    }

  // -- Mutating an outer var through a pure / read-only function. -----------

  @Test def mutableVarWriteInPureLambdaInBodyRejected =
    // Companion of `mutableVarCapturedByPureFunctionInBodyRejected`:
    // here the pure (`->`) lambda *writes* to the captured var
    // instead of reading it. The Supplier/Consumer pair the rewriter
    // synthesises for `bindVar` carries no capture set, so neither
    // the read nor the write effect can flow into a `() -> Unit`.
    // The whole `def f` is rejected at the REPL compile.
    initially {
      run(
        """|def f(): Unit =
           |  var r: Int = 0
           |  eval[Unit]("val pure: () -> Unit = () => { r = r + 1 }; pure()")
           |val out: Unit = f()""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected a capture-checking failure on the var write, got:\n$out",
        out.contains("cannot flow into capture set") &&
          (out.contains("r.rd") || out.contains("r ") ||
            out.contains("Supplier") || out.contains("Consumer"))
      )
    }

  @Test def mutableInstanceWriteThroughRdLambdaInBodyRejected =
    // Mutable class instance captured as a binding (a `val`, not a
    // `var`, so the rewriter uses plain `bind` with `value: Any`).
    // Inside the eval body the user declares a function whose
    // capture set is `{b.rd}` (read-only access to `b`) and tries to
    // write `b.x = 1` through it. Writing requires the full `b`
    // capability, not just `b.rd`. The verification pass re-checks
    // the original lexical context with the body inlined and
    // rejects.
    initially {
      run(
        """|import caps.*
           |class Box extends Mutable:
           |  var x: Int = 0
           |def h(): Unit =
           |  val b = new Box
           |  eval[Unit]("val ro: () ->{b.rd} Unit = () => { b.x = 1 }; ro()")
           |h()""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected a write-through-rd safe-mode rejection, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("capture") || out.contains("capability") ||
            out.contains("flow") || out.contains("update") ||
            out.contains("read-only") || out.contains("readOnly"))
      )
    }

  @Test def mutableInstanceReadInPureLambdaInBodyRejected =
    // Read-only access through a pure (`->`) lambda is also
    // rejected: reading `b.x` carries the `b.rd` effect, which
    // cannot flow into the empty capture set required by `() -> Int`.
    // The body's `pure` declaration is ill-typed under safe mode.
    initially {
      run(
        """|import caps.*
           |class Box extends Mutable:
           |  var x: Int = 0
           |def k(): Int =
           |  val b = new Box
           |  eval[Int]("val pure: () -> Int = () => b.x; pure()")
           |k()""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected a read-through-pure safe-mode rejection, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("captures") || out.contains("capability") ||
            out.contains("flow") || out.contains("b.rd") || out.contains("b "))
      )
    }

end DynamicEvalSafeModeTests

/** Tests for the agent / LLM workflow APIs:
 *
 *    - Closure form `eval(gen: EvalContext => String)`: lets a
 *      generator inspect the enclosing source, the placeholder
 *      marker, and the captured bindings before producing the body.
 *    - Non-throwing `evalSafe[T]: EvalResult[T]`: lets the caller
 *      branch on `isSuccess` / `isFailure` and feed
 *      `error.errors` back into the generator instead of catching.
 */
class DynamicEvalAgentApiTests extends ReplTest:

  @Test def closureFormSeesEnclosingSourceAndPlaceholder =
    initially {
      run(
        """|import dotty.tools.repl.eval.EvalContext
           |val r: Int = eval { (ctx: EvalContext) =>
           |  // The agent would inspect ctx.enclosingSource (and
           |  // ctx.placeholder for where to splice) to compose its
           |  // prompt. Here we just assert the marker is present.
           |  assert(ctx.enclosingSource.contains(ctx.placeholder),
           |    s"placeholder ${ctx.placeholder} missing from ${ctx.enclosingSource}")
           |  "100 + 23"
           |}
           |println(s"r=$r")""".stripMargin
      )
      assertContains("r=123", storedOutput())
    }

  @Test def closureFormSeesBindingNames =
    initially {
      run(
        """|import dotty.tools.repl.eval.EvalContext
           |def add(x: Int, y: Int): Int =
           |  eval { (ctx: EvalContext) =>
           |    // The generator can see the in-scope names.
           |    assert(ctx.bindings.map(_.name).toSet == Set("x", "y"),
           |      s"got ${ctx.bindings.map(_.name).toList}")
           |    "x + y"
           |  }
           |println(s"add(7, 35)=${add(7, 35)}")""".stripMargin
      )
      assertContains("add(7, 35)=42", storedOutput())
    }

  @Test def closureFormCaptureCheckingStillFires =
    // Body comes from the generator at runtime, but the rewriter still
    // captures the enclosing-source slice, so the verification pass
    // catches CC violations exactly the same way as the literal-string
    // form. This test runs *without* the captureChecking flag, so we
    // just check the body composes correctly.
    initially {
      run(
        """|import dotty.tools.repl.eval.EvalContext
           |def greet(name: String): String =
           |  eval[String] { (ctx: EvalContext) =>
           |    s"\"hello, \" + name"
           |  }
           |println(greet("world"))""".stripMargin
      )
      assertContains("hello, world", storedOutput())
    }

  @Test def evalSafeReturnsValueOnSuccess =
    initially {
      run(
        """|val r = evalSafe[Int]("1 + 41")
           |println(s"isSuccess=${r.isSuccess}, get=${r.get}")""".stripMargin
      )
      assertContains("isSuccess=true, get=42", storedOutput())
    }

  @Test def evalSafeReturnsErrorOnCompileFailure =
    initially {
      run(
        """|val r = evalSafe[Int]("nonExistentSym + 1")
           |val e = r.error
           |println(s"isFailure=${r.isFailure}, errors=${e.nn.errors.length}")
           |println(s"first=${e.nn.errors(0).split('\n').head}")""".stripMargin
      )
      val out = storedOutput()
      assertContains("isFailure=true, errors=1", out)
      assertContains("Not found: nonExistentSym", out)
    }

  @Test def evalSafeAgentRetryLoop =
    // The motivating use case: an agent generates code, the eval
    // fails to compile, the agent inspects the error and generates
    // again. Modeled here with two attempts, the first deliberately
    // bad and the second corrected.
    initially {
      run(
        """|import dotty.tools.repl.eval.EvalContext
           |var attempt: Int = 0
           |val r = evalSafe[Int] { (ctx: EvalContext) =>
           |  attempt += 1
           |  if attempt == 1 then "definitelyNotDefined + 1"
           |  else "21 * 2"
           |}
           |val r2 =
           |  if r.isSuccess then r
           |  else
           |    // "agent" retries, having seen the error.
           |    val errMsg = r.error.nn.errors.mkString("|")
           |    println(s"retrying after: ${errMsg.split('\n').head}")
           |    evalSafe[Int] { (ctx: EvalContext) =>
           |      // For this test the closure ignores the error and
           |      // produces a known-good body.
           |      "21 * 2"
           |    }
           |println(s"final=${r2.get}")""".stripMargin
      )
      val out = storedOutput()
      assertContains("retrying after:", out)
      assertContains("final=42", out)
    }

  @Test def evalSafeClosureFormSeesContext =
    initially {
      run(
        """|import dotty.tools.repl.eval.EvalContext
           |def f(x: Int) =
           |  evalSafe[Int] { (ctx: EvalContext) =>
           |    // The generator decides what to splice based on the
           |    // captured bindings.
           |    assert(ctx.bindings.map(_.name).toSet == Set("x"),
           |      s"expected [x], got ${ctx.bindings.map(_.name).toList}")
           |    assert(ctx.enclosingSource.nonEmpty,
           |      "expected non-empty enclosing source for a def-bound eval")
           |    "x * x"
           |  }
           |val r = f(7)
           |println(s"f(7)=${r.get}")""".stripMargin
      )
      assertContains("f(7)=49", storedOutput())
    }

  @Test def evalSafeDoesNotCaptureNestedCompileFailure =
    // The outer call is evalSafe; the body contains a *nested* eval
    // (not evalSafe) that fails to compile. The nested compile
    // failure surfaces as a thrown EvalCompileException at runtime
    // (that's the body's runtime exception, not the outer's compile
    // state). evalSafe must propagate it, not wrap it as
    // `EvalResult.failure` (which would tell the agent "your outer
    // code didn't compile" when in fact the outer did and the body
    // crashed).
    initially {
      run(
        """|val outcome =
           |  try
           |    val r = evalSafe[Int]("eval[Int](\"undefinedSym + 1\")")
           |    if r.isFailure then "WRONG: outer evalSafe captured nested failure"
           |    else "WRONG: produced a value"
           |  catch case _: dotty.tools.repl.eval.EvalCompileException =>
           |    "OK: nested failure propagated through outer evalSafe"
           |println(outcome)""".stripMargin
      )
      assertContains("OK: nested failure propagated", storedOutput())
    }

  @Test def evalSafeCapturesOwnCompileFailure =
    // Sanity check on the other side: a real outer-compile error
    // (here, a body that references an undefined symbol with no
    // nesting involved) IS captured by evalSafe, since it is the
    // outer call's own compile state.
    initially {
      run(
        """|val r = evalSafe[Int]("undefinedTopLevel + 1")
           |println(s"isFailure=${r.isFailure}, count=${r.error.nn.errors.length}")""".stripMargin
      )
      assertContains("isFailure=true, count=1", storedOutput())
    }

  @Test def closureFormInNestedEvalSeesChainedContext =
    // Inside an outer eval's body, a nested eval can also use the
    // closure form. The runtime nested-eval rewriter (rewriteCode in
    // Eval.scala) composes the inner enclosingSource so it includes
    // the outer's enclosing-source plus the outer body wrapper. The
    // inner closure should see the full chain (containing the outer
    // def signature and the outer `({ ... })` wrapper) plus all the
    // bindings the outer captured. The test avoids `s"..."`
    // interpolation inside the inner body string because the
    // rewriter's pretty-printer does not always round-trip those
    // (see EVAL.md "Pretty-printer round-trip in nested eval").
    initially {
      val q3 = "\"\"\""
      val innerBody =
        s"""${q3}eval[Int] { (innerCtx: dotty.tools.repl.eval.EvalContext) =>
           |      assert(innerCtx.bindings.map(_.name).toSet == Set("i"))
           |      assert(innerCtx.enclosingSource.contains("def f(i: Int)"),
           |        "inner should also see the def signature (chained from outer)")
           |      assert(innerCtx.enclosingSource.contains("({ "),
           |        "inner enclosing should include the outer-body wrapper braces")
           |      "i + 1"
           |    }${q3}""".stripMargin
      run(
        s"""|import dotty.tools.repl.eval.EvalContext
            |def f(i: Int): Int =
            |  eval[Int] { (outerCtx: EvalContext) =>
            |    assert(outerCtx.bindings.map(_.name).toSet == Set("i"))
            |    assert(outerCtx.enclosingSource.contains("def f(i: Int)"),
            |      "outer should see the def signature")
            |    $innerBody
            |  }
            |println("f(10) = " + f(10))""".stripMargin
      )
      assertContains("f(10) = 11", storedOutput())
    }

  @Test def evalSafeRetryInsideNestedBodyKeepsChainedContext =
    // Models a retry loop *inside* an outer eval body. The outer body
    // declares `val x` and then calls `evalSafe` twice (the second
    // call is the "retry" after the first compile-failed). Both
    // evalSafe calls must see the chained context (def signature +
    // outer body's `val x`); the second call's bindings additionally
    // include `r1`, the val sitting between the two calls in the same
    // block. Verifies that running through the rewriter twice (once
    // for outer parsing, once for nested-body rewriting) doesn't
    // collapse the chain on the retry.
    initially {
      val q3 = "\"\"\""
      val innerBody =
        s"""${q3}val x = 5
           |    val r1 = evalSafe[Int] { (ctx1: dotty.tools.repl.eval.EvalContext) =>
           |      assert(ctx1.bindings.map(_.name).toSet == Set("x", "i"),
           |        "first attempt should see [x, i]")
           |      assert(ctx1.enclosingSource.contains("val x = 5"),
           |        "first attempt should see outer-body `val x = 5`")
           |      "definitelyMissingSym + 1"
           |    }
           |    if r1.isSuccess then r1.get
           |    else evalSafe[Int] { (ctx2: dotty.tools.repl.eval.EvalContext) =>
           |      assert(ctx2.bindings.map(_.name).toSet == Set("r1", "x", "i"),
           |        "retry sits below `val r1` so its bindings include r1, x, i")
           |      assert(ctx2.enclosingSource.contains("val x = 5"),
           |        "retry should ALSO see outer-body `val x = 5`")
           |      "x + i"
           |    }.get${q3}""".stripMargin
      run(
        s"""|import dotty.tools.repl.eval.EvalContext
            |def f(i: Int): Int =
            |  eval[Int] { (outerCtx: EvalContext) =>
            |    $innerBody
            |  }
            |println("f(10) = " + f(10))""".stripMargin
      )
      assertContains("f(10) = 15", storedOutput())
    }

  @Test def closureFormInNestedEvalSeesOuterBodyValAndChainedSource =
    // Models the user's nested-agent retry scenario:
    //   def f(i: Int) = eval(...)
    // where the outer eval's generated body itself declares `val x`
    // and then contains a nested eval. The runtime nested-eval
    // rewriter must:
    //   * inject BOTH `i` (outer) and `x` (outer-body local) as
    //     bindings on the inner call.
    //   * splice the outer body (with the inner call's location
    //     replaced by a marker) into the outer enclosingSource's
    //     marker slot, so the inner closure sees the full chain
    //     `def f(i: Int) = ({ val x = ...; __placeholder__ })`.
    //   * let the inner body reference both `x` and `i` so the
    //     wrapper signature has both as parameters.
    // String-interpolation in the inner body is avoided per the
    // existing test's comment about the pretty-printer round-trip.
    initially {
      val q3 = "\"\"\""
      val innerBody =
        s"""${q3}val x = 5
           |    eval[Int] { (innerCtx: dotty.tools.repl.eval.EvalContext) =>
           |      assert(innerCtx.bindings.map(_.name).toSet == Set("x", "i"),
           |        "inner should capture both `x` and `i`")
           |      assert(innerCtx.enclosingSource.contains("def f(i: Int)"),
           |        "inner enclosingSource should still carry the def signature")
           |      assert(innerCtx.enclosingSource.contains("val x = 5"),
           |        "inner enclosingSource should include the outer body's `val x = 5`")
           |      assert(innerCtx.enclosingSource.contains("({ "),
           |        "inner enclosingSource should include the outer-body `({ ... })` wrapper")
           |      "x + i"
           |    }${q3}""".stripMargin
      run(
        s"""|import dotty.tools.repl.eval.EvalContext
            |def f(i: Int): Int =
            |  eval[Int] { (outerCtx: EvalContext) =>
            |    assert(outerCtx.bindings.map(_.name).toSet == Set("i"))
            |    assert(outerCtx.enclosingSource.contains("def f(i: Int)"))
            |    $innerBody
            |  }
            |println("f(10) = " + f(10))""".stripMargin
      )
      assertContains("f(10) = 15", storedOutput())
    }

end DynamicEvalAgentApiTests

/** Tests for the `-Xrepl-eval-log-dir` compiler flag, which writes
 *  per-invocation log files for each `eval(...)` call. Each call
 *  produces:
 *
 *    - `eval_<timestamp>_enclosingSource.scala`: the enclosing
 *      top-level statement at the call site, with the eval call's
 *      location replaced by a placeholder.
 *    - `eval_<timestamp>_code.scala`: the body string the user
 *      submitted to `eval(...)`.
 *    - `eval_<timestamp>_wrapper.scala`: a pretty-printed snapshot of
 *      the unit *after* the V2 pipeline finishes (splice + extract +
 *      resolve), so the user can see what actually runs: the
 *      synthesised `__EvalSafeExpression` class with the body lowered
 *      to reflective accessor calls (`getValue("i")`, `__refl_get__`,
 *      etc.) inside `evaluate()`.
 *    - `eval_<timestamp>_error.scala`: only on a compile failure;
 *      carries the diagnostic text and the synthesised source the
 *      eval driver was trying to compile.
 */
class DynamicEvalLogTests extends ReplTest(
  ReplTest.defaultOptions ++ Array(
    "-Xrepl-eval-log-dir:" + DynamicEvalLogTests.tempDir.getAbsolutePath
  )
):
  import DynamicEvalLogTests.*

  @Test def writesEnclosingSourceCodeAndWrapperOnSuccess =
    initially {
      // Clean slate before this test.
      clearLogDir()
      run("""|def f(i: Int, j: Int): Int = eval[Int]("i + j")
             |f(10, 32)""".stripMargin)
      assertContains("val res0: Int = 42", storedOutput())
      val files = listLogs()
      val enc = files.find(_.endsWith("_enclosingSource.scala")).getOrElse(
        fail(s"missing enclosingSource log: $files").asInstanceOf[String])
      val code = files.find(_.endsWith("_code.scala")).getOrElse(
        fail(s"missing code log: $files").asInstanceOf[String])
      val wrapper = files.find(_.endsWith("_wrapper.scala")).getOrElse(
        fail(s"missing wrapper log: $files").asInstanceOf[String])
      assertTrue(s"no error log expected on success: $files",
        files.forall(!_.endsWith("_error.scala")))
      val encContent = readLog(enc)
      val codeContent = readLog(code)
      val wrapperContent = readLog(wrapper)
      assertTrue(s"enclosingSource should mention `def f`: $encContent",
        encContent.contains("def f(i: Int, j: Int): Int"))
      assertTrue(s"enclosingSource should contain placeholder: $encContent",
        encContent.contains("__evalBodyPlaceholder"))
      assertTrue(s"code should be the body string: $codeContent",
        codeContent.trim == "i + j")
      // The synthesised wrapper module is dropped by `ExtractEvalBody`
      // when its body has no nested classes, so the post-resolve
      // snapshot only carries the `__EvalExpression_` class. (When the
      // user's enclosing source contains a `class`/`object`, the
      // wrapper survives to host the nested copy and would also appear
      // here. The simple `def f(i, j): Int = i + j` shape doesn't
      // trigger that path.)
      assertTrue(s"wrapper log should NOT contain the dropped __EvalWrapper module: $wrapperContent",
        !wrapperContent.contains("__EvalWrapper_"))
      assertTrue(s"wrapper should contain the synthesised __Expression class: $wrapperContent",
        wrapperContent.contains("__EvalExpression_"))
      assertTrue(s"wrapper should contain `evaluate` with the lowered body: $wrapperContent",
        wrapperContent.contains("def evaluate"))
      assertTrue(s"wrapper should show the lowered binding lookup for `i`: $wrapperContent",
        wrapperContent.contains("getValue(\"i\")"))
    }

  @Test def writesErrorLogOnCompileFailure =
    initially {
      clearLogDir()
      run("""|import dotty.tools.repl.eval.EvalCompileException
             |val r = try eval[Int]("undefinedSymbol + 1")
             |        catch case _: EvalCompileException => -1
             |r""".stripMargin)
      assertContains("val res0: Int = -1", storedOutput())
      val files = listLogs()
      val err = files.find(_.endsWith("_error.scala")).getOrElse(
        fail(s"missing error log: $files").asInstanceOf[String])
      val errContent = readLog(err)
      assertTrue(s"error log should mention diagnostic: $errContent",
        errContent.contains("Not found: undefinedSymbol"))
      assertTrue(s"error log should embed generated source: $errContent",
        errContent.contains("__EvalWrapper_"))
    }

end DynamicEvalLogTests

object DynamicEvalLogTests:
  private val tempDir: java.io.File =
    val d = java.io.File.createTempFile("eval-log-", "-test")
    d.delete()
    d.mkdirs()
    d.deleteOnExit()
    d

  def clearLogDir(): Unit =
    val files = tempDir.listFiles()
    if files != null then files.foreach(_.delete())

  def listLogs(): List[String] =
    val files = tempDir.listFiles()
    if files == null then Nil else files.map(_.getName).toList

  def readLog(name: String): String =
    val path = new java.io.File(tempDir, name).toPath
    new String(java.nio.file.Files.readAllBytes(path))
