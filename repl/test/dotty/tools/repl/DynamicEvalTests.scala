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
 *    - At runtime, `eval` calls back into the REPL driver, which spins
 *      up a separate dotc Driver to compile + run `code` against the
 *      live REPL session's classpath.
 *    - The argument can be any `String`: literal, `val`, `s"..."`, etc.
 *    - The REPL parser-stage rewriter injects `Eval.bind("z", z)` for
 *      every lambda parameter (and block-local `val`) syntactically in
 *      scope at the call site, so `xs.map(z => eval[Int]("z + 1"))` Just
 *      Works.
 *    - REPL session state (vals, vars, defs, classes, givens) is
 *      imported into the body's scope.
 *    - Return type is the polymorphic `T`; the user ascribes the
 *      expected type and a runtime cast bridges to it. A mismatch
 *      surfaces as `ClassCastException`.
 *
 *  Each section below targets one axis of the behaviour.
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
    run("""val r: Int = eval("\"not an int\"")""")
    assertContains("ClassCastException", storedOutput())
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
  // 5. Lambda parameter capture (the headline feature). The parser-stage
  //    rewriter injects bindings for every lambda param syntactically in scope.
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
  // 7. REPL session state (previous-line definitions) is visible inside eval.
  //    Implementation: at each call, the runtime adds the REPL's output dir to
  //    the eval driver's classpath and prepends `import rs$line$N.{given, *}`
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

  // ---------------------------------------------------------------------------
  // Block-local def capture. The rewriter eta-expands a captured def
  // into a `FunctionN` lambda at the bind site; the typer infers the
  // function type, the type-annotation phase records it, and the eval
  // body sees the def as a precisely-typed function value.
  // ---------------------------------------------------------------------------

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

  @Test def genericBlockLocalDefSkipped = initially {
    // Type-parametric defs are NOT captured: eta-expansion would
    // need a concrete T instantiation, which the rewriter can't pick
    // at parser stage. The eval body fails with "Not found: g" as
    // before.
    run("""|def f(): Int =
           |  def g[T](x: T) = x
           |  eval[Int]("g[Int](42)")
           |f()""".stripMargin)
    val out = storedOutput()
    assertContains("Not found: g", out)
  }

  // ---------------------------------------------------------------------------
  // Function-local `var`: the eval body captures it by value.
  // ---------------------------------------------------------------------------

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
    // The rewriter wraps captured `var`s in `Eval.VarCell`s, declares a
    // local var in the eval body initialised from `cell.value`, runs
    // the body, then writes the local back to the cell. After eval
    // returns the call site reads `cell.value` into the outer var, so
    // mutation propagates.
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

  // ---------------------------------------------------------------------------
  // Type parameters of an enclosing method.
  //
  // Term parameters are captured by the rewriter and arrive at the eval body
  // as method parameters whose declared type is recovered from the runtime
  // class of the value. Type parameters themselves are *not* in scope inside
  // the eval body: they have no runtime representation, and the body is
  // compiled in a fresh wrapper module that has no notion of the caller's
  // type variables. But because T is erased on the JVM:
  //   * a body that uses `x: T` at runtime sees the erased value, so methods
  //     available on the runtime class (or `Any`) work;
  //   * `eval[T]("...")` at the call site casts the result to T, so a body
  //     can return its parameter and the value flows through.
  // ---------------------------------------------------------------------------

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

  @Test def typeParameterNameNotInScopeInsideEval = initially {
    // Trying to reference the enclosing method's `T` from inside the eval
    // body fails: type parameters aren't injected as bindings, and the
    // body's compilation is independent of the caller's type variables.
    run("""|def f[T](x: T): T = eval[T]("val tag: T = x; tag")
           |f(42)""".stripMargin)
    val out = storedOutput()
    assertContains("Not found: type T", out)
  }

  // ===========================================================================
  // 8. Body-internal definitions: declarations within the eval body.
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
  // 9. Body language features: control flow, pattern matching.
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

  // ===========================================================================
  // 10. Functions defined inside eval, returned and used outside.
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

  // ===========================================================================
  // 11. Generic and stdlib types inside the body.
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
  // 12. Compile-time errors in the eval body.
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

  // ===========================================================================
  // 13a. Nested eval: an eval body can itself call eval.
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
  // 13. Runtime errors raised by the eval body propagate to the caller.
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
  // 14. Failed previous lines don't break later eval calls.
  //
  // When a REPL line fails to type-check, its `rs$line$N` wrapper has no
  // classfile. The runtime filters such indexes out before generating the
  // synthetic `import rs$line$N.*` so a later `eval(...)` doesn't trip on
  // the missing wrapper.
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

end DynamicEvalTests

/** `eval`'s isolated compile inherits the live REPL's CLI flags, so
 *  options like `-Yexplicit-nulls` apply inside the body too: `null`
 *  doesn't conform to `String` once explicit-nulls is on, so a body that
 *  binds `val s: String = null` fails to compile.
 */
class DynamicEvalExplicitNullsTests extends ReplTest(
  ReplTest.defaultOptions ++ Array("-Yexplicit-nulls")
):
  @Test def explicitNullsForwardedToEval =
    initially {
      run("""val r: String = eval("val s: String = null; s")""")
      val out = storedOutput()
      assertTrue(
        s"expected an explicit-nulls error, got:\n$out",
        out.contains("Null") || out.contains("Found:") || out.contains("failed to compile")
      )
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
end DynamicEvalCaptureCheckingTests
