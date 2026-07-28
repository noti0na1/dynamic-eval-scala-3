package dotty.tools
package eval

import org.junit.Assert.*
import org.junit.Test

import dotty.tools.repl.{ReplTest, State}

/** End-to-end tests for the `Eval.topLevel` primitive (root-imported in
 *  the REPL as `topLevel` / `topLevelSafe`, next to `eval`).
 *
 *  `topLevel(defs)` compiles definitions ONCE, in the *global* context
 *  of its call site (package members, file-level imports, previous REPL
 *  lines; never method locals), into a persistent output dir with a
 *  dedicated classloader. `handle.eval(expr)` then runs an expression
 *  at its own call site's *local* context with the handle's definitions
 *  in scope, like calls to global functions. Every eval call through
 *  one handle links against the same loaded classes.
 *
 *  Behaviour axes pinned here:
 *    - defs applied to call-site locals and local type arguments
 *    - compile-once identity: module state in the defs is shared across
 *      *different* `.eval` call sites, and instances of a defs-defined
 *      class flow between calls (one `Class` per handle)
 *    - name-resolution order from the wrapper-level import: call-site
 *      locals shadow the handle's defs; the handle's defs shadow
 *      same-named session definitions
 *    - `using`/`given` across the boundary in every direction,
 *      including a call-site local given resolving a topLevel def's
 *      `using` clause and outranking a defs-side given
 *    - isolation and eagerness: defs referencing a call-site local are
 *      rejected at `topLevel(...)` time with the standard diagnostic
 */
class TopLevelEvalTests extends ReplTest:

  /** Run each line as its own REPL unit, threading the session state,
   *  and assert every `expected` string appears in the combined output.
   */
  private def expectSteps(lines: String*)(expected: String*)(using st0: State): Unit =
    var st = st0
    val sb = new StringBuilder
    for line <- lines do
      st = run(line)(using st)
      sb ++= storedOutput()
    val out = sb.toString
    for e <- expected do
      assertTrue(s"expected `$e` in:\n$out", out.contains(e))

  // ===========================================================================
  // 1. Basics: defs at top level, expressions at the local context.
  // ===========================================================================

  @Test def defsAppliedToCallSiteLocals = initially {
    expectSteps(
      """val h = topLevel("def f[T](x: List[T]): T = x.head")""",
      """val r = List(10, 20, 30).map(z => h.eval[Int]("f(List(z))"))""")(
      "List(10, 20, 30)")
  }

  @Test def defsAppliedToLocalTypeArgument = initially {
    // The user-facing shape `topLevel("def f[T](...)").eval("f[A](a)")`:
    // the def's type parameter instantiates from a *method type
    // parameter* of the call site, a type the defs compile could never
    // name.
    expectSteps(
      """val h = topLevel("def f[T](x: List[T]): T = x.head")""",
      """def m[A](a: A): A = h.eval[A]("f[A](List(a, a))")""",
      """val ri = m(5)""",
      """val rs = m("hi")""")(
      "val ri: Int = 5",
      """val rs: String = "hi"""")
  }

  @Test def defsSeeSessionGlobals = initially {
    // Session lines are the REPL's "top level of the current package",
    // so the defs may call them.
    expectSteps(
      """def helper(x: Int): Int = x * 10""",
      """val h = topLevel("def f(x: Int): Int = helper(x) + 1")""",
      """val r = h.eval[Int]("f(2)")""")(
      "val r: Int = 21")
  }

  @Test def mutualRecursionBetweenDefs = initially {
    expectSteps(
      """val h = topLevel("def even(n: Int): Boolean = if n == 0 then true else odd(n - 1)\ndef odd(n: Int): Boolean = if n == 0 then false else even(n - 1)")""",
      """val r = h.eval[Boolean]("even(10)")""")(
      "val r: Boolean = true")
  }

  @Test def classDefinedInDefs = initially {
    expectSteps(
      """val h = topLevel("case class P(x: Int, y: Int) { def sum: Int = x + y }")""",
      """val r = h.eval[Int]("P(40, 2).sum")""")(
      "val r: Int = 42")
  }

  @Test def nestedEvalOnSameHandle = initially {
    // A body may call `.eval` on the same handle again; the adapter
    // deduplicates the handle binding the nested call carries twice
    // (propagated with the outer scope plus its own appended copy).
    expectSteps(
      """val h = topLevel("def f[T](x: List[T]): T = x.head")""",
      """val r = h.eval[Int]("f(List(h.eval[Int](\"f(List(7))\") + 1))")""")(
      "val r: Int = 8")
  }

  // ===========================================================================
  // 2. Compile-once identity: shared state and stable classes per handle.
  // ===========================================================================

  @Test def moduleStateSharedAcrossInvocationsOfOneCallSite = initially {
    expectSteps(
      """val h = topLevel("object C { var n = 0 }\ndef inc(): Int = { C.n += 1; C.n }")""",
      """val r = (1 to 3).map(_ => h.eval[Int]("inc()"))""")(
      "Vector(1, 2, 3)")
  }

  @Test def moduleStateSharedAcrossCallSites = initially {
    // The defs compile once per handle, so a *different* `.eval` call
    // site continues from the same module state instead of starting
    // from a fresh copy.
    expectSteps(
      """val h = topLevel("object C { var n = 0 }\ndef inc(): Int = { C.n += 1; C.n }")""",
      """val warm = (1 to 3).map(_ => h.eval[Int]("inc()"))""",
      """val r = h.eval[Int]("inc()")""")(
      "val r: Int = 4")
  }

  @Test def topLevelVarSharedAcrossCallSites = initially {
    // A bare `var` in the defs (a member of the handle's object) is
    // one JVM field, mutated in place by every call site.
    expectSteps(
      """val h = topLevel("var total = 0\ndef add(n: Int): Int = { total += n; total }")""",
      """val a = h.eval[Int]("add(40)")""",
      """val b = h.eval[Int]("add(2)")""")(
      "val a: Int = 40",
      "val b: Int = 42")
  }

  @Test def classIdentityStableAcrossCalls = initially {
    // An instance created by one `.eval` call is usable by another:
    // both calls link against the same loaded `Box` class, so the
    // cast succeeds instead of failing on two same-named classes from
    // different loaders.
    expectSteps(
      """val h = topLevel("class Box(var v: Int)")""",
      """val b = h.eval[Any]("new Box(41)")""",
      """val r = h.eval[Int]("b.asInstanceOf[Box].v + 1")""")(
      "val r: Int = 42")
  }

  @Test def independentHandlesHaveIndependentState = initially {
    // Two `topLevel` calls with the same defs text are two compiles
    // and two handles: like defining the object twice.
    expectSteps(
      """val defs = "object C { var n = 0 }\ndef inc(): Int = { C.n += 1; C.n }"""",
      """val h1 = topLevel(defs)""",
      """val h2 = topLevel(defs)""",
      """val a = h1.eval[Int]("inc()")""",
      """val b = h2.eval[Int]("inc()")""")(
      "val a: Int = 1",
      "val b: Int = 1")
  }

  // ===========================================================================
  // 3. using / given across the boundary.
  // ===========================================================================

  @Test def givenInDefsSummonedInBody = initially {
    expectSteps(
      """val h = topLevel("case class Conf(tag: String)\ngiven Conf = Conf(\"prod\")")""",
      """val r = h.eval[String]("summon[Conf].tag")""")(
      """val r: String = "prod"""")
  }

  @Test def usingClauseResolvedByCallSiteLocalGiven = initially {
    // A topLevel def declares a `using` clause over a session-level
    // type class; the instance is a *local* given at the `.eval` call
    // site, reaching the def through the given-binding capture.
    expectSteps(
      """trait Show[T] { def show(t: T): String }""",
      """val h = topLevel("def render[T](x: T)(using s: Show[T]): String = s.show(x)")""",
      """|def demo(): String =
         |  given Show[Int] = new Show[Int] { def show(t: Int) = "int:" + t }
         |  h.eval[String]("render(5)")""".stripMargin,
      """val r = demo()""")(
      """val r: String = "int:5"""")
  }

  @Test def usingClauseResolvedByDefsOwnGiven = initially {
    expectSteps(
      """val h = topLevel("trait Fmt { def s: String }\ngiven Fmt = new Fmt { def s = \"fmt\" }\ndef fmt()(using f: Fmt): String = f.s")""",
      """val r = h.eval[String]("fmt()")""")(
      """val r: String = "fmt"""")
  }

  @Test def usingClauseResolvedBySessionGiven = initially {
    expectSteps(
      """given Int = 21""",
      """val h = topLevel("def dub()(using n: Int): Int = n * 2")""",
      """val r = h.eval[Int]("dub()")""")(
      "val r: Int = 42")
  }

  @Test def callSiteLocalGivenOutranksDefsGiven = initially {
    // Both the call site and the defs provide a given of the same
    // type; the local one is captured into the wrapper's `using`
    // clause, which sits inner to the wrapper-level defs import, so
    // it wins.
    expectSteps(
      """trait Fmt2 { def s: String }""",
      """val h = topLevel("given Fmt2 = new Fmt2 { def s = \"defs\" }")""",
      """|def demo(): String =
         |  given Fmt2 = new Fmt2 { def s = "local" }
         |  h.eval[String]("summon[Fmt2].s")""".stripMargin,
      """val r = demo()""")(
      """val r: String = "local"""")
  }

  // ===========================================================================
  // 4. Name resolution: local first, then handle, then session.
  // ===========================================================================

  @Test def localDefShadowsTopLevelDef = initially {
    expectSteps(
      """val h = topLevel("def sq(x: Int): Int = x * x")""",
      """|def demo(): Int =
         |  def sq(x: Int): Int = x + 1000
         |  h.eval[Int]("sq(5)")""".stripMargin,
      """val r = demo()""")(
      "val r: Int = 1005")
  }

  @Test def localValShadowsTopLevelDef = initially {
    expectSteps(
      """val h = topLevel("def sq(x: Int): Int = x * x")""",
      """|def demo(): Int =
         |  val sq = 7
         |  h.eval[Int]("sq")""".stripMargin,
      """val r = demo()""")(
      "val r: Int = 7")
  }

  @Test def lambdaParamShadowsTopLevelDef = initially {
    expectSteps(
      """val h = topLevel("def sq(x: Int): Int = x * x")""",
      """val r = List(5).map(sq => h.eval[Int]("sq + 1"))""")(
      "val r: List[Int] = List(6)")
  }

  @Test def topLevelDefShadowsSessionDef = initially {
    // With no local in sight, the handle's def is the nearer import
    // and wins over a same-named session definition; the session copy
    // stays untouched outside the eval.
    expectSteps(
      """def sq(x: Int): Int = x + 1000""",
      """val h = topLevel("def sq(x: Int): Int = x * x")""",
      """val fromHandle = h.eval[Int]("sq(5)")""",
      """val fromSession = sq(5)""")(
      "val fromHandle: Int = 25",
      "val fromSession: Int = 1005")
  }

  // ===========================================================================
  // 5. Isolation and eagerness.
  // ===========================================================================

  @Test def defsCannotSeeCallSiteLocals = initially {
    // `topLevel` deliberately ignores the captured bindings: the
    // lambda parameter `z` is in scope at the call site, but the defs
    // compile against the global context only and fail with the
    // standard diagnostic.
    expectSteps(
      """val msgs = List(1).map(z => topLevelSafe("def g = z")).map(r => r.error.errors.head.linesIterator.next())""")(
      "Not found: z")
  }

  @Test def defsErrorSurfacesAtConstruction = initially {
    // The defs compile runs at `topLevel(...)` time; no `.eval` is
    // needed for the failure to surface, and the throwing form throws
    // right there.
    expectSteps(
      """val r = topLevelSafe("def g = undefinedName123").isFailure""",
      """val thrown = try { topLevel("def g = undefinedName123"); false } catch { case e: dotty.tools.eval.EvalCompileException => true }""")(
      "val r: Boolean = true",
      "val thrown: Boolean = true")
  }

  @Test def bodyCompileFailureIsAValue = initially {
    expectSteps(
      """val h = topLevel("def f(x: Int): Int = x")""",
      """val r = h.evalSafe[Int]("undefinedName123").isFailure""")(
      "val r: Boolean = true")
  }

  // ===========================================================================
  // 6. Captures keep the link: a bindings array taken inside a
  //    handle's world stays usable after the call returns.
  // ===========================================================================

  @Test def captureInsideBodyKeepsHandleBinding = initially {
    // `snap()` is an @evalLike capture taken INSIDE `h.eval`'s body.
    // The rewriter cannot see the handle binding (it has no name in
    // the source), so the runtime re-attaches it: the captured array
    // carries a `__evalTopLevel_…` entry, and a later eval against
    // that array still resolves the defs.
    expectSteps(
      """import dotty.tools.eval.{Eval, evalLike}""",
      """@evalLike def snap(bindings: Array[Eval.Binding] = Array.empty[Eval.Binding], expectedType: String = "", enclosingSource: String = ""): Array[Eval.Binding] = bindings""",
      """val h = topLevel("case class N(v: Int)\ndef mk(v: Int): N = N(v)")""",
      """val st = h.eval[Array[Eval.Binding]]("snap()")""",
      """val kept = st.exists(_.name.startsWith("__evalTopLevel_"))""",
      """val out = Eval.eval[Int]("mk(20).v + 1", st, "", "")""")(
      "val kept: Boolean = true",
      "val out: Int = 21")
  }

  @Test def captureInsideDefsRelinks = initially {
    // The defs compile runs the rewrite phase too: an @evalLike call
    // inside a defs member captures the member's own scope (the
    // parameter `v`), its slice (the member around the marker, with
    // the defs-object import in front), and — at runtime, through the
    // registry — the handle of the defs object it was compiled
    // inside. Splicing a later eval against the captured slice runs
    // inside `probe`'s body: the parameter and the defs' classes both
    // resolve. This is what makes an agent-style `getState()` written
    // inside a topLevel function work.
    // `probe` declares `Any`, so a later body of any type conforms in
    // the captured slice's result position — the same shape an
    // embedder's chain wrapper uses.
    expectSteps(
      """import dotty.tools.eval.{Eval, evalLike}""",
      """@evalLike def snap2(bindings: Array[Eval.Binding] = Array.empty[Eval.Binding], expectedType: String = "", enclosingSource: String = ""): (Array[Eval.Binding], String) = (bindings, enclosingSource)""",
      """val h = topLevel("case class M(v: Int)\ndef probe(v: Int): Any = snap2()")""",
      """val st = h.eval[Any]("probe(7)").asInstanceOf[(Array[Eval.Binding], String)]""",
      """val hasParam = st._1.exists(_.name == "v")""",
      """val hasHandle = st._1.exists(_.name.startsWith("__evalTopLevel_"))""",
      """val hasMarker = st._2.contains("__evalBodyPlaceholder__")""",
      """val out = Eval.eval[Int]("M(v).v + 1", st._1, "", st._2)""")(
      "val hasParam: Boolean = true",
      "val hasHandle: Boolean = true",
      "val hasMarker: Boolean = true",
      "val out: Int = 8")
  }

  @Test def defsImportsReachACapturedSlice = initially {
    // Leading import lines of the defs sit at FILE level of the
    // synthesised unit, so the rewriter records them into slices: a
    // capture inside the defs re-resolves `ListBuffer` when a later
    // eval splices against it.
    expectSteps(
      """import dotty.tools.eval.{Eval, evalLike}""",
      """@evalLike def snap3(bindings: Array[Eval.Binding] = Array.empty[Eval.Binding], expectedType: String = "", enclosingSource: String = ""): (Array[Eval.Binding], String) = (bindings, enclosingSource)""",
      """val h = topLevel("import scala.collection.mutable.ListBuffer\ndef gather(seed: Int): Any = { val buf = ListBuffer(seed); snap3() }")""",
      """val st = h.eval[Any]("gather(5)").asInstanceOf[(Array[Eval.Binding], String)]""",
      """val out = Eval.eval[Int]("buf.sum + ListBuffer(1).sum", st._1, "", st._2)""")(
      "val out: Int = 6")
  }
