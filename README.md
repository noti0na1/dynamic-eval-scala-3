# Dynamic `eval` in Scala 3

`eval[T](code: String): T` compiles and runs an arbitrary string of
Scala source at runtime against the live program, returning a value
typed as `T`. It works in the REPL out of the box, and in any Scala
program compiled with `-Xdynamic-eval`.

```scala
scala> val r: Int = eval("1 + 2")
val r: Int = 3

scala> List(1, 2, 3).map(z => eval[Int]("z * z"))
val res0: List[Int] = List(1, 4, 9)
```

The argument is any `String`, not a string literal. A `val`, an
`s"..."` interpolation, an LLM generated piece of source, anything
computable at runtime. Identifiers in scope at the call site
(lambda parameters, block locals, method parameters, REPL line
definitions, class members) are visible inside the body by their
source name.

## Outside the REPL: `-Xdynamic-eval`

The same feature is available to ordinary programs. The eval
machinery lives in the compiler (`dotty.tools.eval`), so a program
that has the Scala 3 compiler on its runtime classpath can call
`eval` directly:

```scala
// Square.scala, compiled with: scalac -Xdynamic-eval Square.scala
import dotty.tools.eval.Eval.eval

def square(x: Int): Int = eval[Int]("x * x")

object Main:
  def main(args: Array[String]): Unit =
    val n = args(0).toInt
    println(List(1, 2, 3).map(z => eval[Int]("z * z + n")))
```

Two pieces make this work:

1. **Compile time.** `-Xdynamic-eval` enables the `EvalRewriteTyped`
   phase in the main compiler pipeline (the REPL enables it
   unconditionally). It fills each call's `bindings`,
   `expectedType`, and `enclosingSource` from the typed scope, and
   embeds the file's lexical context into the enclosing-source
   slice: top-level imports, an `import <pkg>.{given, *}` for the
   call site's package, and an `import <Obj>.{given, *}` for an
   enclosing top-level object. That last import is the standalone
   analogue of the REPL's `import rs$line$N.{given, *}` session
   imports: sibling members resolve against the *runtime* module on
   the classpath, so reads and writes hit live state.

2. **Run time.** With no REPL driver installed, `Eval` falls back to
   a self-initialising standalone adapter. It compiles the body with
   a classpath synthesised from the calling frame's classloader plus
   `java.class.path`, and loads the result next to the program's own
   classes. Optional system properties tune it:

   | Property                    | Meaning                                                            |
   |-----------------------------|--------------------------------------------------------------------|
   | `dotty.tools.eval.settings` | whitespace-separated compiler options for the body compile (pass the language options the program was compiled with, e.g. `-Yexplicit-nulls`) |
   | `dotty.tools.eval.classpath`| explicit classpath for the body compile (overrides the synthesised one) |
   | `dotty.tools.eval.logDir`   | per-invocation log directory (same files as `-Xrepl-eval-log-dir`) |

Without `-Xdynamic-eval` the call still compiles (it is an ordinary
method call), but the rewriter never runs: the body is compiled in
an isolated context where only globally reachable names resolve, and
a body mentioning a call-site local fails at runtime with a compile
diagnostic.

Known standalone limitations: a body cannot see `private` members of
an *enclosing top-level object* (they come in through the embedded
wildcard import, which, unlike the REPL's reflective class-member
path, does not expose privates), and a nested (non-top-level)
`object`'s state is re-elaborated rather than shared. Class members,
including private ones, go through the same reflective machinery as
in the REPL and behave identically.

An eval body *can* `return` from the method enclosing the eval call.
At runtime the body executes inside a separate `evaluate()` method,
so the return cannot be an ordinary JVM return; instead the rewriter
wraps every eval call that sits directly inside a method in a
`try`/`catch` keyed on a per-execution `__evalReturnKey__` synthetic
binding, and the body's `return` compiles to an `EvalNonLocalReturn`
control throw that the call-site catch turns back into a real
`return`. A `return` at a position where source code couldn't return
either (top level, inside a lambda) is still rejected with the
standard diagnostic.

## The idea

Picture writing a program with a *hole* in it. Some expression in
some method is left as a `???` placeholder, typed `T`, sitting in
its own lexical context: locals, parameters, members, imports, all
visible at the hole's position. As the program runs, eventually
control reaches the hole, and at *that* moment the source that
should fill it arrives as a `String`, computed from whatever
runtime values the program happens to have. The compiler takes
that source, drops it into the hole, type checks it against `T` in
the original lexical context, runs the result, and returns the
value. The rest of the program continues, oblivious that part of
it was unknown until a moment ago.

That is what `eval[T](code: String): T` is. The placeholder is the
call site, `T` is the expected type, the captured lexical context
is the surrounding source, and `code` is the runtime computed
source filling the hole.

### Runtime computed bodies

The body string itself is typically built from values the program
does not know at the time it is written:

```scala
scala> def f(j: Int) = j + 1
def f(j: Int): Int

scala> def g(j: Int) = j * j
def g(j: Int): Int

scala> val toCall = StdIn.readLine()  // user types: f
val toCall: String = "f"

scala> val arg = Random.nextInt(10)
val arg: Int = 7

scala> eval[Int](s"$toCall($arg)")
val res0: Int = 8
```

No Scala value at the eval call site refers to `f` or `g` directly:
the *name of the function to call* arrives over stdin. There is no
compile time literal `7`; the argument comes from an RNG. Yet the
body that ends up running, `f(7)`, is typed Scala source, checked
against `Int`, calling a `def` defined two lines up. If the user
had typed `g` instead, the body would have been `g(7)` and the
result `49`. If they had typed `nope`, the call would have failed
with a "not found: nope" diagnostic at runtime. Whatever the
runtime computes fills the hole, and the type system catches
mistakes the same way it catches them in hand written source.

The body can come from anywhere: a file, a network socket, a
database row, an LLM response. None of those sources can produce a
typed `Expr[T]`. They can all produce a `String`, and that is what
eval consumes.

### The lexical context survives

Each combination of runtime body and capture site goes through a
real Scala compile:

```scala
scala> val xss = List(List(0, 1), List(2))
scala> val i = 2
scala> xss.flatMap(xs => xs.map(x => eval[Int]("(x + xs.length) * i")))
val res1: List[Int] = List(4, 6, 6)
```

`x` from the inner lambda, `xs` from the outer lambda, and `i`
from a previous REPL line all resolve inside the body string. The
eval driver does *not* compile the body in isolation. It compiles
the body *spliced back into the source of the enclosing top level
statement*, with the eval call's location replaced by the body.
The compiler sees the original method's signature, its parameters,
the enclosing class, the nearby imports. The body resolves
identifiers against that context and is type checked against `T`
exactly as if the user had written the expression in place.

## Safety

"Run a string of code" is a category of feature that often plays
fast and loose with the type system. Dynamic `eval` does not.
Every guarantee Scala 3 provides about ordinary code applies to
eval bodies, because the body goes through the same compiler with
the same flags, in the same lexical context. The clearest way to
see that is to look at what gets caught.

### Type mismatch in the body

A body that does not satisfy the expected `T` is rejected with a
real type error, in the same shape the user would have seen from
typing the expression in place:

```scala
scala> def f(j: Int) = j + 1
def f(j: Int): Int

scala> def g(j: Int) = j - 1
def g(j: Int): Int

scala> val fs = List("f", "g")
val fs: List[String] = List("f", "g")

scala> val s = "hello"
val s: String = "hello"

scala> fs.map(fun => eval[Int](s"$fun(s)"))
java.lang.RuntimeException: eval failed to compile:
Found:    (s : String)
Required: Int

Generated source:
import rs$line$1.{given, *}
import rs$line$2.{given, *}
...
object __EvalWrapper_... {
  def __run__(`fun`: String): Any = {
    f(s)
  }
}
```

The body composed at runtime, `f(s)`, is not a Scala expression
the user could have written either: `f` expects an `Int`, `s` is a
`String`. The wrapper compile sees this and produces the standard
"Found: String, Required: Int" diagnostic, with the offending
synthesised source attached so the developer can see what was
actually compiled.

The same applies when the call site pins a stricter `[T]` than the
body produces:

```scala
scala> eval[Int]("\"abc\"")
java.lang.RuntimeException: eval failed to compile:
Found:    ("abc" : String)
Required: Int
```

### Capture checking on the body

When the live REPL session has capture checking enabled, the body
is rechecked under `cc` in its original lexical context, so a body
that would leak a capability is rejected:

```scala
scala> import language.experimental.captureChecking

scala> trait IO extends caps.SharedCapability
trait IO

scala> def withIO[T](op: IO^ => T): T = op(new IO {})
def withIO[T](op: IO^ => T): T

scala> withIO { io =>
     |   eval[() => Unit]("() => println(io)")
     | }
java.lang.RuntimeException: eval failed to compile:
Found:    () ->{io} Unit
Required: () => Unit
```

The body is `() => println(io)`. Inside its lexical context `io`
is a `SharedCapability`, so the lambda's type is `() ->{io} Unit`,
not `() => Unit`. The call site asked for `() => Unit`, which
cannot capture `io`, and `cc` rejects the conversion. This is the
same diagnostic the user would have seen if they had written the
lambda in place.

A bare leak fails for the same reason:

```scala
scala> def leak(c: AnyRef^): AnyRef = eval[AnyRef]("c")
                                                   ^
   error: Found:    (c : AnyRef^)
          Required: AnyRef
```

The result type `AnyRef` carries no capture set, so returning a
`c: AnyRef^` through it would silently broaden the capture. `cc`
flags this on the spliced body before any runtime work happens.

### Why this works: the body is recompiled in the original lexical context

Both diagnostics above are not produced by a custom checker
sitting in front of the eval driver. They are the standard `dotc`
diagnostics, on the same trees, in the same context the user
would have produced by writing the expression in place. The eval
driver achieves that by treating the body string as opaque only
through the parser stage. From PostTyper onward the body has been
parsed, spliced into the enclosing source, and type checked as
ordinary Scala. The wrapper compile sees:

```scala
import rs$line$1.{given, *}
import rs$line$2.{given, *}
import dotty.tools.eval.Eval.{eval, evalSafe}

object __EvalWrapper_<uuid>:
  // exact text of the user's enclosing method, with
  // __evalBodyPlaceholder__ where the eval call stood
  def f(j: Int): Int = ({ __evalBodyPlaceholder__ })

class __EvalExpression_<uuid>(thisObject: Object | Null,
                              bindings: Array[Eval.Binding]):
  def evaluate(): Any = ()
```

`SpliceEvalBody` parses the body string, replaces the placeholder,
and the rest of the front end runs as usual. Type errors in the
body surface as ordinary compile diagnostics. Capture checking
applies. Safe mode applies. Inline expansion applies. Macro
expansion applies. The body is not eval'd through some shadow type
system; it is just Scala source the compiler is asked to compile,
and the user's `eval[T]` call site dictates the expected type.

### Failures are typed; runtime and compile time stay distinct

Two flavours of failure surface, and they are kept apart deliberately:

| Flavour                                | Throwing form                           | Non throwing form                |
|----------------------------------------|-----------------------------------------|----------------------------------|
| *This* call's compile error            | `EvalCompileException(errors, source)`  | `EvalResult.Failure(failure)`    |
| The body's runtime exception           | propagates as the original exception    | propagates as the same exception |

A nested eval inside the body that fails to compile shows up to
the outer caller as a runtime exception, not as the outer call's
compile error. Without that split, a `try { ... } catch case e:
EvalCompileException => failure(e)` inside `evalSafe` would
silently swallow a nested eval failure as if it were the
surrounding call's. The compile failure is carried as a
`Left(CompileFailure)` *value* through the `Adapter` boundary so
the runtime can decide which flavour to surface.

### Iterating with `evalSafe` when the body might be wrong

If the body comes from a source you cannot fully trust to produce
correct Scala (an LLM, a templating layer, user input, even your
own runtime string concatenation), use `evalSafe` and react to the
errors instead of catching exceptions. `EvalResult` carries the
diagnostics as data so you can feed them back into whatever
produced the body and try again:

```scala
scala> Eval.evalSafe[Int]("1 + ") match
     |   case EvalResult.Success(v) => v
     |   case EvalResult.Failure(f) =>
     |     println("compile errors:")
     |     f.errors.foreach(e => println(s"  $e"))
     |     -1
compile errors:
  expression expected but eof found
val res0: Int = -1
```

This same pattern is the natural shape for an agent loop. Ask the
LLM for code, run it through `evalSafe`, and on failure feed
`failure.errors` (and optionally `failure.source`) back into the
prompt so the next attempt has the diagnostics to react to:

```scala
def adaptiveAgent[T](task: String, maxAttempts: Int = 3): EvalResult[T] =
  var lastErrors: Array[String] = Array.empty
  var attempt = 0
  while attempt < maxAttempts do
    val result = Eval.evalSafe[T]: ctx =>
      val feedback =
        if lastErrors.isEmpty then ""
        else s"\nPrevious attempt failed with:\n${lastErrors.mkString("\n")}\nFix it."
      llm.complete(s"$task$feedback\n\n${ctx.enclosingSource}")
    result match
      case EvalResult.Success(_)    => return result
      case EvalResult.Failure(fail) => lastErrors = fail.errors
    attempt += 1
  EvalResult.Failure(new Eval.CompileFailure(lastErrors, ""))
```

This converges in practice because the diagnostics the agent sees
are the *same* `dotc` diagnostics a developer would see, in the
*same* lexical context, with the same expected `T`. The error text
already tells the next attempt what to fix.

### Implementation invariants

The rewriter in `EvalRewriteTyped` enforces a few invariants that
keep the surface API honest.

**Classification by symbol identity, never by name.**
`EvalRewriteTyped` runs after PostTyper, so it classifies an Apply
into the eval pipeline using the resolved `Symbol` of the call
target. Plain `Eval.eval` and `Eval.evalSafe` are matched by
`sym.owner == Eval.moduleClass`. User defined generators are
matched by an `@evalLike` or `@evalSafeLike` annotation on the
method. A user who defines `def eval(s: String): Int` shadowing
the import does *not* trigger the rewriter; the warning instead
says

> `eval` here resolves to `Foo.eval`, not
> `dotty.tools.eval.Eval.eval`; the eval rewriter is leaving
> this call alone. If you intended a custom eval generator,
> annotate the function with `@evalLike` (or `@evalSafeLike`).

The rewriter cannot mis fire on unrelated code that happens to use
the name `eval`.

**All or nothing on synthetic arguments.** Three of `eval`'s
arguments are *synthetic*: `bindings`, `expectedType`, and
`enclosingSource`. The rewriter fills them from the typed scope.
If a caller already supplied any of them, the rewriter must not
silently overwrite it. The rule is:

| State of the three slots                | Rewriter behaviour |
|-----------------------------------------|--------------------|
| All three are typer supplied defaults   | Fill them          |
| All three are explicitly supplied       | Leave them alone   |
| Mixed                                   | Reject as ill formed |

The "all explicitly supplied" case is what makes user defined
`@evalLike` wrappers (such as `agent`) compose cleanly: the
wrapper receives the three values as its own parameters and
forwards them to `Eval.eval`, and the rewriter fills the wrapper's
call site once without rewriting the inner forward.

**`evalSafe` and `@evalSafeLike` must return `EvalResult[T]`.** The
non throwing flavour relies on a different verify shape: the inner
verification compile wraps the placeholder in
`Eval.handleCompileError(...)` so the spliced body's `T` is lifted
to `EvalResult[T]`. That makes sense only when the surrounding
call *returns* `EvalResult[T]`. The rewriter checks this on every
`@evalSafeLike` annotated method and rejects misuse early, with a
diagnostic that names the actual return type.

**`@caps.assumeSafe` does *not* relax checks on the body.** `object
Eval` is annotated `@caps.assumeSafe` so safe mode user code can
call `eval` and `evalSafe`. The annotation only exempts the eval
driver's *own* surface API from safe mode rejection. The eval
driver recompiles the body string under the live REPL's flags, so
a safe mode session still applies safe mode checks to the body
itself. The annotation does not punch a hole in safe mode; it only
permits the call to the driver to typecheck.

### Two compile passes, one source of truth

Every eval call goes through two compile passes:

1. **Outer compile.** Runs as part of the live REPL session.
   `EvalRewriteTyped` (a phase that runs after PostTyper) fills
   the synthetic arguments. The rest of the REPL phase chain runs
   as usual, so a compile error in the user's outer line reports
   against the user's line.

2. **Inner compile.** The wrapper compile triggered when the body
   executes. It is driven by `EvalCompiler`, which inserts
   `SpliceEvalBody` (after parser), runs `EvalRewriteTyped` again
   on the wrapper (so nested eval calls in the body get their own
   bindings filled), then `ExtractEvalBody` (after `cc`),
   `ResolveEvalAccess` (post erasure), and `LogExecutedTree`
   (last, when logging is enabled). `cc` runs *before*
   `ExtractEvalBody` precisely so capture checking sees the body
   in its original lexical context, with the original `^`
   annotations intact, before the body is moved into
   `__Expression.evaluate`.

This guarantees that a `cc` violation in the body is rejected the
same way it would be rejected if the user had written the
expression in place: by `cc`, on the same tree, in the same
context.

### Compared with `eval` in dynamic languages

Most dynamic languages ship `eval` as a built in or near built in.
Python's `eval` and `exec`, JavaScript's `eval` and `new Function`,
Ruby's `eval` and `instance_eval`, Lisp's `(eval ...)`. They all
take a string and run it in some flavour of the surrounding scope.
Implementing eval in those languages is comparatively easy: the
language is already interpreted (or JIT compiled) from a parsed
AST; the runtime is already prepared for symbols, scopes, and
methods to come and go. There is no static contract for eval to
preserve, because the language as a whole has no static contract
to preserve.

That ease comes with a price, and the price is paid at runtime
when something is wrong. In Python:

```python
def f(j): return j + 1
def g(j): return j * j

names = ["f", "g"]
arg = "hello"
for name in names:
    print(eval(f"{name}({arg!r})"))   # `f("hello")` → TypeError mid loop
```

`f` is happy to be called and crashes inside its body when `+`
fails on `int + str`. `g` is happy to be called and crashes
inside *its* body. The interpreter has no way to know in advance
that the eval'd expression `f("hello")` is wrong. By the time the
`TypeError` fires, the loop has already run iterations, perhaps
written to files, perhaps issued network requests, perhaps mutated
shared state. The exception interrupts the program; it does not
undo the side effects.

The pattern repeats across the family. A misspelled identifier
(`evel("...")`), a missing attribute (`obj.foo` when the runtime
object has only `bar`), a type mismatch between an argument and a
parameter, a mismatch between the eval'd value and the surrounding
context that consumes it, a call to a function that does not
exist: all are observed *during* execution, after the program has
made progress. There is no rollback. The developer learns the code
is wrong by running enough of the program for the wrongness to
surface.

Dynamic `eval` in Scala 3 sits on the other side of this trade
off. The body is compiled before any of its statements run. A
misspelled identifier surfaces as a "not found" diagnostic
*before* the wrapper class is even loaded. A type mismatch between
the body and the expected `T` is rejected by the standard typer
*before* `evaluate()` is invoked. A capture violation is rejected
by `cc` *before* the body has a chance to leak the capability.
Whatever side effecting work the user's outer code did *up to* the
eval call has already happened, but the eval call itself either
returns a typed value of `T` or fails with a typed `CompileFailure`
describing exactly what is wrong. The body's own statements do not
get to run partway through and stop.

The cost of this design is the wrapper compile pass on every
unique call. That is a real cost, and the LRU cache exists to
amortise it. The benefit is the property that gives this section
its name: the bodies eval runs are bodies the compiler agreed to
run. By the time control reaches `evaluate()`, every check the
ambient language would apply to a literal expression has already
been applied to the runtime computed source.

## Examples

### Capturing previous lines

A val captured from a previous line:

```scala
scala> val n = 10
val n: Int = 10

scala> eval[Int]("n * 4")
val res0: Int = 40
```

Capture earlier `def`s and call them through their names:

```scala
scala> def f(j: Int) = j + 1
def f(j: Int): Int

scala> def g(j: Int) = j - 1
def g(j: Int): Int

scala> List("f", "g").map(fun => eval[Int](s"$fun(1)"))
val res1: List[Int] = List(2, 0)
```

Capture mutable state and write through a `var`:

```scala
scala> var n = 10
var n: Int = 10

scala> eval[Unit]("n = n + 5")

scala> n
val res2: Int = 15
```

Capture a `given`:

```scala
scala> given Int = 7

scala> eval[Int]("summon[Int] * 6")
val res3: Int = 42
```

`evalSafe` for non throwing use:

```scala
scala> Eval.evalSafe[Int]("not real code") match
     |   case r if r.isSuccess => r.get
     |   case _                => -1
val res4: Int = -1
```

### Eval inside a class method, accessing private members

```scala
scala> class Box(private var v: Int):
     |   def show(): Int = eval[Int]("v + 1")
     |
class Box

scala> new Box(41).show()
val res5: Int = 42
```

The class method is hoisted out of `Box` for the inner compile so
the wrapper does not mint a duplicate `Class` for `Box`. References
to `v` are routed through reflection so a `private var` remains
reachable.

### Nested eval calls

An eval body can itself call eval. Each inner call has its own
captured bindings, and the inner verification compile sees a
composed `enclosingSource` chained off the outer call's:

```scala
scala> List(1, 2, 3).map(x => eval[Int](s"List(${x}, ${x*2}).map(y => eval[Int](\"y + 1\")).sum"))
val res6: List[Int] = List(5, 7, 9)
```

The outer `eval[Int]` captures `x`. The body it generates also
contains `eval[Int]("y + 1")`, which captures `y` from the inner
lambda. The runtime nested rewrite happens as a tree transform
inside `SpliceEvalBody`, so the inner eval's bindings list
includes both the names already captured by the outer call and any
new in scope names introduced inside the body.

### An agent that decomposes a task into sub agent calls

`agent[T]` is a user defined `@evalLike` wrapper that asks an LLM
to fill the placeholder. The same recursive shape lets a single
agent call drive a whole pipeline:

```scala
scala> agent[List[(String, String)]](
     |   "Find 3 largest scala files in `compiler/src`, and summarize each in one sentence")

// Code the agent generated and ran:
//   val dir = new java.io.File("compiler/src")
//   def listScalaFiles(d: java.io.File): List[java.io.File] =
//     val entries = d.listFiles()
//     if entries == null then Nil
//     else entries.toList.flatMap: f =>
//       if f.isDirectory then listScalaFiles(f)
//       else if f.getName.endsWith(".scala") then List(f)
//       else Nil
//   val top3 = listScalaFiles(dir).sortBy(-_.length()).take(3)
//   top3.map: file =>
//     val name = file.getName
//     val content = scala.io.Source.fromFile(file, "UTF-8").mkString.take(10000)
//     val summary = agent[String](s"summarize this scala file content in one sentence: $content")
//     (name, summary)

val res7: List[(String, String)] = List(
  ("Types.scala",   "..."),
  ("Typer.scala",   "..."),
  ("Parsers.scala", "..."))
```

The outer `agent` call generates Scala that walks the file system,
sorts, slices, and *recursively calls `agent[String]`* on each
file's contents. Each inner call is an independent eval through
the same machinery, with its own bindings (`file`, `name`,
`content`) captured at its lexical site.

## Pipeline

```
                 ┌────────────────────────┐
                 │   user types code      │
                 │   at REPL prompt       │
                 └──────────┬─────────────┘
                            │  parser
                            ▼
                   ┌────────────────────┐
                   │  ReplCompiler      │   standard REPL phase chain
                   │  + EvalRewriteTyped│   plus EvalRewriteTyped after
                   │   (after PostTyper)│   PostTyper: fill bindings,
                   └──────────┬─────────┘   expectedType, enclosingSource
                              │
                              ▼  bytecode lands in the REPL output dir
                 ┌────────────────────────┐
                 │  user code runs        │
                 │  -> Eval.eval(code)    │
                 └──────────┬─────────────┘
                            │  evalAdapter: ReplDriver.evalDynamic
                            ▼
                   ┌────────────────────┐
                   │  EvalAdapter       │  wrap inputs, look up cached
                   │  (in process)      │  __Expression class (or compile
                   └──────────┬─────────┘  a fresh one)
                              │            │
                              ▼  EvalCompilerBridge
                   ┌────────────────────┐
                   │  EvalCompiler      │
                   │   custom phases:   │
                   │   SpliceEvalBody   │  parse body, splice into
                   │   EvalRewriteTyped │  enclosingSource, fill nested
                   │   ExtractEvalBody  │  evals, drain typed body into
                   │   ResolveEvalAccess│  __Expression.evaluate, lower
                   │   LogExecutedTree  │  outer references through
                   └──────────┬─────────┘  reflective accessors
                              │
                              ▼
                   load __Expression, instantiate, call evaluate()
                              │
                              ▼
                        result : T
```

### Outer compile: the call site rewrite

`EvalRewriteTyped` is the rewriter. There is no parser stage
counterpart any more; everything happens after PostTyper, when the
typed tree carries resolved symbols. For every call classified as
`PlainEval`, `PlainEvalSafe`, `EvalLike`, or `EvalSafeLike`, it:

1. Expands a single argument closure form
   (`eval[T]({ ctx => ... })`) to the four argument overload.
2. Builds an `Array[Eval.Binding]` from the typed scope. The
   walker maintains a stack of in scope frames as it descends
   through `Block`, `DefDef`, and `Template` nodes. Bindings come
   in two kinds. *Visible* bindings are user nameable values:
   * block local vals, vars, defs, and givens
   * method val parameters (including by name, lifted to a thunk
     so that post `ElimByName` `apply()` works)
   * lambda parameters

   *Synthetic* bindings (flagged `isSynthetic`, reserved
   `__eval*__` / `__this__*` names) carry values the *compiler*
   needs to link the body's generated code back to the live
   program; the body never names them directly:
   * `__this__` and `__this__<ClassName>` for the enclosing class
     chain (the body's `this` references rewrite against these)
   * class members, emitted as `bindSynthetic("x", this.x)` to
     keep the member alive across the method lift / DCE pipeline
     that the inner compile does
   * per local (method scoped) class `C`: `__evalClass_C__` =
     `classOf[C]` (type tests against the *original* lifted
     class) and `__evalNew_C__$i` = one factory closure per
     constructor (so `new C(...)` in the body constructs the
     original class, with `C`'s captured environment threaded by
     LambdaLift through the closure)
   * per local `object M` (including synthesized companions):
     `__evalModule_M__` = the live module instance, so module
     state is shared rather than re elaborated
   * `__evalReturnKey__`: a fresh key object for non local
     `return` (see above)
3. Renders the `[T]` argument back to source for `expectedType`,
   keeping capture annotations when capture checking is enabled
   in the live session, and dropping degenerate types
   (`Nothing`/`Null`, error types, types referencing locally
   scoped symbols the wrapper could not resolve). When `[T]` was
   left to inference in a context that only bounds it from above
   (`val a: A = eval("...")` constrains `T <: A`, which the typer
   minimizes to `Nothing`), the rewriter renders the call's
   *prototype* instead: a small hook at the end of the typer's
   `Applications.typedApply` attaches the expected type of every
   eval-like call to its tree (inference itself is untouched), and
   the rewriter resolves that type against the final
   instantiations. This covers argument positions too: an eval
   passed to a generic method records the method's type variable,
   which resolves to whatever inference settled on. Prototypes
   that don't fully resolve degrade to the empty string; for safe
   flavour calls the `EvalResult[...]` wrapper is unwrapped first.
4. Slices the source of the enclosing top level statement,
   replaces the eval call's span with the placeholder, and stores
   the result as `enclosingSource`. For safe flavour calls
   (`evalSafe` and `@evalSafeLike`) the placeholder is wrapped in
   `Eval.handleCompileError(...)` so the inner verification lifts
   `T` to `EvalResult[T]`.

When the outer compile is *itself* the wrapper compile of an
outer eval call (chained mode), `enclosingSource` is composed
against the outer call's `enclosingSource` so each level of
nesting carries the full lexical context down.

### Inner compile

`EvalCompiler` is a `Compiler` subclass that installs the eval
specific phases on top of the standard chain.

#### `SpliceEvalBody` (after parser)

Parses the body string, replaces the marker, appends the
synthesised `__Expression` class to the package, and lifts class
methods that contain the marker out of their enclosing class
(needed so the wrapper does not mint a duplicate `Class` for the
class the REPL session has already compiled). After the lift,
`this` references in the body are rewritten to `__this__`, and
private field/method accesses are rerouted through synthesised
`__refl_get__`, `__refl_set__`, and `__refl_call__` helpers.

#### `EvalRewriteTyped` (after PostTyper, on the wrapper)

Same phase as the outer compile, run again on the spliced wrapper
so any nested eval calls inside the body get their bindings,
`expectedType`, and chained `enclosingSource` filled.

#### `ExtractEvalBody` (after `cc`)

Drains the typed body out of `val __evalResult: T = ...` into
`__Expression.evaluate`'s rhs and rewrites every outer scope
reference into a `reflectEval(...)` placeholder carrying a
`ReflectEvalStrategy` attachment:

| Strategy             | Source case                       | Lowered to                                                   |
|----------------------|-----------------------------------|--------------------------------------------------------------|
| `LocalValue`         | outer method local val/var read   | `getValue("name")`                                           |
| `LocalValueAssign`   | outer var write                   | `getRaw("name").asInstanceOf[VarRef].set(rhs)`               |
| `This`               | `this` of an enclosing class      | `getThisObject()`                                            |
| `Outer`              | one `$outer` walk                 | `getOuter(qual, outerCls)`                                   |
| `Field`              | private/protected field read      | `getField(qual, className, fieldName)` or getter call        |
| `FieldAssign`        | private/protected field write     | `setField(qual, className, fieldName, v)` or setter call     |
| `MethodCall`         | private/protected method call     | `callMethod(qual, className, name, paramTpes, retTpe, args)` |
| `MethodCapture`      | outer block local def call        | `getValue(name).asInstanceOf[FunctionN].apply(args*)`        |
| `ConstructLocal`     | `new C(...)` on a linked local class | `getValue("__evalNew_C__$i").asInstanceOf[FunctionN].apply(args*)` |
| `BindingValue`       | linked module read / return key   | `getRaw("__evalModule_M__")` / `getRaw("__evalReturnKey__")` |

Anchored after `cc` so capture checking sees the body in its
original lexical context, with the original `^` annotations on def
parameters intact, before the body is moved. A body `return`
targeting the enclosing method lowers here to
`throw new EvalNonLocalReturn(<key>, expr)`, and body local symbols
whose signature mentions a linked local class have that class
substituted with `Object` at the denotation level (the runtime
values belong to the *original* class, so a descriptor naming the
wrapper's re elaborated copy would force a wrong `checkcast`).

`ResolveEvalAccess` additionally runs a post erasure sweep over
`__Expression`: type tests and casts that PatternMatcher generated
against a re elaborated local class lower to `isLinkedInstance` /
`castLinked` (backed by the `__evalClass_C__` binding), and member
accesses on linked receivers lower to receiver class reflection, so
no emitted bytecode references the re minted class.

#### `ResolveEvalAccess` (post erasure)

Walks `__Expression.evaluate` and lowers every `reflectEval(...)`
Apply into the concrete accessor call from the table above. Runs
after erasure so cast types match the JVM level shapes the
reflective helpers operate on.

#### `LogExecutedTree` (conditional, last)

When `Xrepl-eval-log-dir` is set, snapshots the post resolve tree
into `eval_<timestamp>_wrapper.scala`. The same flag also
produces:

| File                                        | Content                                                                |
|---------------------------------------------|------------------------------------------------------------------------|
| `eval_<timestamp>_enclosingSource.scala`    | The enclosing top level source with the placeholder.                   |
| `eval_<timestamp>_code.scala`               | The body string the user submitted.                                    |
| `eval_<timestamp>_wrapper.scala`            | Post pipeline tree (`__Expression.evaluate` with reflective lowerings).|
| `eval_<timestamp>_error.scala`              | Diagnostics + the offending source on compile failure.                 |

`Xrepl-history-file <path>` writes a transcript of the live
session to `<path>` in append mode, ANSI codes stripped.

### Runtime evaluator

`EvalAdapter` is the REPL side implementation of `Eval.Adapter`.
`ReplDriver` installs it via `Eval.withAdapter` for the duration
of every user line evaluation. When the body reaches `eval(...)`
at runtime, the adapter:

1. Writes per invocation log files if `Xrepl-eval-log-dir` is
   set.
2. Looks the call up in a per session LRU cache keyed on
   `(code, enclosingSource, bindingNames, imports, settings,
   sessionLoader)`. A cache hit reuses the loaded `__Expression`
   class and reflected handles.
3. On a cache miss, builds an `EvalCompilerConfig`, drives an
   `EvalCompilerBridge.compile` (the inner compile through
   `EvalCompiler`), loads `__Expression` via
   `AbstractFileClassLoader`, and snapshots its constructor and
   `evaluate` method.
4. Instantiates `__Expression` with the captured `__this__` (or
   `null`) and the bindings array, invokes `evaluate()`, and
   returns the result.

A body runtime exception arrives wrapped in
`InvocationTargetException`; the adapter unwraps and rethrows the
cause so callers see the original exception.

### Caching compiled wrappers

A naive eval would pay a full `dotc` compile on every call. That
is prohibitively expensive when eval sits inside a loop or a
combinator (`xs.map(x => eval[Int]("..."))`). `EvalAdapter` keeps
a per session LRU of compiled `__Expression` classes so identical
call sites recompile zero times after the first.

The cache key is a `WrapperKey(code, enclosingSource, bindingsKey,
importsKey, settingsKey, sessionLoader)`. Two calls share an
entry iff every input to the wrapper compile is identical: same
body, same surrounding source slice, same bindings shape (names
plus kinds, not values, since values change per call by design),
same REPL session imports, same compiler flags, same session
classloader. The classloader is part of the key by *reference
identity* so two REPL sessions that happen to share imports and
flags do not bleed state across each other.

The cache stores both successes and compile failures, so a body
that doesn't compile won't retrigger the same diagnostic on every
call. Internal compiler errors are *not* cached (a transient
classpath issue or assertion failure shouldn't become a permanent
"this body doesn't compile"). Bodies with an empty
`enclosingSource` (direct callers, runtime rewritten nested
evals) are not cached either, since the discriminator is too weak
to be safe.

`EvalAdapter.cache` is wrapped in `Collections.synchronizedMap`
so parallel calls are safe. The bound is `cacheCapacity = 128`
entries, each of which pins a classloader and one `__Expression`
class, so this also bounds metaspace growth from caching.
`EvalAdapter.clearCache()` flushes everything (used by `:reset`
and tests).

### Classloader bridging

The eval output classloader (`AbstractFileClassLoader`) routes
class lookups through the parent for several name patterns to
keep `Class` objects shared across the REPL / eval boundary:

* `dotty.tools.repl.*`. The eval infrastructure types
  (`Eval.Binding`, `EvalContext`, `EvalResult`, `VarRef`,
  `EvalCompileException`, ...). User wrappers reference them and
  need the *same* `Class` the driver itself uses, otherwise
  mutable state like `Eval.active` (a `ThreadLocal`) splits into
  independent copies. Routed through the classloader that loaded
  `AbstractFileClassLoader` itself.
* `scala.*`, `dotty.*`. Values produced by `eval` cross the
  boundary (a lambda `Int => Int` returned from eval is a
  `scala.Function1` the REPL also recognises). Loaded by the
  parent.
* `rs$line$N.*`. REPL session line wrappers. Already compiled
  and loaded by the parent; loading via `findClass` would mint a
  duplicate `Class` and break `checkcast`.

This is why the public API uses JDK functional interfaces
(`Supplier`, `Consumer`, `Function`) instead of Scala's
`Function0` / `Function1` / `Function2`, and why `Eval.Binding`,
`EvalContext`, `EvalResult`, and `VarRef` live in
`dotty.tools.eval`.

## Related approaches: staging and the debugger expression compiler

Dynamic `eval` is not the first time the Scala 3 ecosystem has
turned the compiler back on at runtime. Two close cousins are
worth comparing, since they each cover a different slice of the
same design space.

### Multi stage programming with `scala.quoted`

Scala 3 ships first class staging via `scala.quoted` and
`scala.quoted.staging`. The shape is:

```scala
import scala.quoted.*
import scala.quoted.staging.*

given Compiler = Compiler.make(getClass.getClassLoader)

val powerOf3: Int => Int = run:
  '{ (x: Int) => ${ powerCode(3, 'x) } }

def powerCode(n: Int, x: Expr[Int])(using Quotes): Expr[Int] =
  if n == 0 then '{ 1 }
  else '{ $x * ${ powerCode(n - 1, x) } }
```

The differences from dynamic `eval` go to the heart of what each
feature is for:

| Aspect                      | `scala.quoted` staging                              | Dynamic `eval`                                           |
|-----------------------------|-----------------------------------------------------|----------------------------------------------------------|
| Input to the next stage     | A typed `Expr[T]` built with `'{ }` / `${ }`        | An arbitrary `String` of Scala source                    |
| When the input is checked   | At the outer stage, with full type info             | When the wrapper compiles, in the REPL session's context |
| Hygiene                     | Hygienic by construction                            | Hygienic by lexical splice                               |
| Where the source comes from | Has to be statically expressible in `'{ }`          | Anything computable at runtime, including LLM output     |
| Cross stage references      | Lift via `Expr.apply` / `Liftable`                  | Captured automatically via the bindings array            |
| Use case                    | Specialisation, partial evaluation, DSLs            | REPL exploration, agents, runtime synthesis from text    |

The two systems answer different questions. Staging asks: how can
a program *describe* a future stage of itself, so that the
compiler checks the description and then runs it later? Dynamic
`eval` asks: how can a program *receive* an opaque chunk of
source, hand it to the compiler, and use the result as if the
developer had written it in place? Staging trades flexibility for
static guarantees; eval trades static guarantees for the ability
to consume strings whose contents are not known until runtime.

Notably, eval recovers a large chunk of staging's static safety
*per call* by recompiling against the original lexical context.
Once the body has parsed, the rest of the pipeline is pure typed
Scala, with `cc`, safe mode, and the same diagnostics as ordinary
code. Where staging gives you static guarantees on a program that
is fully described in `'{ }`, eval gives you the same local
guarantees as soon as the body is in hand.

A natural composition: an `@evalLike` agent that *generates*
`scala.quoted` code, hands it to `staging.run`, and returns the
specialised function. The agent supplies the source that staging
needs but cannot write itself.

### The debugger expression compiler

The Scala 3 debug adapter (`scalacenter/scala-debug-adapter`) has
an "expression compiler" module. Its job: when the developer
pauses a JVM under the debugger and types an expression into the
"evaluate" UI, compile that expression against the *current stack
frame* and return its value. The shape is recognisable from this
document:

* The user's expression is parsed as Scala source.
* It is spliced into a synthesised method whose lexical context
  mirrors the frame at the breakpoint: the same enclosing class,
  the same locals, the same `this`.
* Local variables are forwarded as parameters to the method,
  much like eval's bindings array.
* Private fields and methods are reached via reflective accessors
  so they remain visible from the compiled wrapper.
* The wrapper is loaded into a fresh classloader and
  instantiated; the resulting value is sent back to the debugger.

The eval pipeline draws direct inspiration from the debugger
expression compiler, and owes much of its shape to that design.
The class method lift in `SpliceEvalBody`, the `__this__`
synthetic, the `reflectEval` strategy table in
`ResolveEvalAccess`, the `getValue` / `getField` / `callMethod`
accessors on `__Expression`, even the choice to return `Object`
and cast at the call site to dodge classloader constraint
problems, are all techniques the debugger expression compiler
used first, adapted here for a live REPL session rather than a
frozen JDI frame.

Where the two diverge:

| Aspect                  | Debugger expression compiler                         | Dynamic `eval`                                                                                                       |
|-------------------------|------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------|
| Trigger                 | A breakpoint plus a developer typing into the IDE    | A source level `eval(...)` call                                                                                      |
| Frame model             | Frozen JVM frame snapshot from JDI                   | Live REPL session, mutable through `VarRef`                                                                          |
| Lifetime                | One off; result returned to the debugger             | Bytecode cached and reused; `eval` results flow back into the running program                                        |
| Driver                  | An external service over the Debug Adapter Protocol  | An adapter installed in process via `Eval.withAdapter`                                                               |
| Capture story           | Locals lifted from the frame via JDI                 | Bindings filled at compile time from the typed scope                                                                 |
| Composability           | Standalone REPL like prompt                          | Composes with the host program: nested `eval`, `agent`, `evalSafe`, parallel collections, captured `var`s, ...        |

The debugger expression compiler is "compile against a frozen
context, return one value." Dynamic `eval` extends that idea into
"compile against a live context, return a typed value into the
running program, and let the program call eval again, recursively,
with composed context." The kinship is technical; the use cases
sit on either side of a familiar split between observation
(debugging) and construction (programming).

## Running the tests

The eval implementation lives in the compiler
(`compiler/src/dotty/tools/eval/`); the REPL glue lives in the
`scala3-repl` sbt subproject (`repl/` in the tree, wired up in
`project/Build.scala`). All eval tests (REPL end-to-end, standalone
end-to-end, and pipeline unit tests) run from the `scala3-repl`
project, which builds on the bootstrapped compiler. The tests are
ordinary JUnit `@Test` methods run through `junit-interface`, so the
usual dotty `testOnly <fully.qualified.Class> -- *methodGlob` form
applies.

Open an sbt shell (`sbt`) and run the commands below at the
`sbt:scala3>` prompt, or pass them as a single quoted argument
(`sbt "scala3-repl/testOnly ..."`).

All eval tests live under the `dotty.tools.eval` package (in
`repl/test/dotty/tools/eval/`), so a single package glob runs
every one of them — the end-to-end REPL suites and the lower-level
pipeline unit tests alike:

```
# Every eval test (all suites in both tables below)
scala3-repl/testOnly dotty.tools.eval.*

# One suite
scala3-repl/testOnly dotty.tools.eval.DynamicEvalTests

# One test method (junit-interface glob on the method name)
scala3-repl/testOnly dotty.tools.eval.DynamicEvalTests -- *returnsInt

# The whole REPL test suite (eval tests plus everything else)
scala3-repl/test
```

The eval tests are split into suites by axis. The end-to-end
suites (in `repl/test/dotty/tools/eval/DynamicEvalTests.scala`)
drive the real REPL by feeding it source lines and asserting on
the session output:

| Suite                            | What it covers                                                                 | REPL flags exercised                          |
|----------------------------------|--------------------------------------------------------------------------------|-----------------------------------------------|
| `DynamicEvalTests`               | Core behaviour: return types, capturing previous lines / locals / `var`s / `given`s, class members, nested eval, compile-error reporting. | (defaults)                                    |
| `DynamicEvalExplicitNullsTests`  | Flag forwarding into the body compile: `null` no longer conforms to `String`.  | `-Yexplicit-nulls`                            |
| `DynamicEvalCaptureCheckingTests`| Capture checking on the spliced body (the `cc` examples above).                | `-language:experimental.captureChecking`      |
| `DynamicEvalSafeModeTests`       | Safe-mode checks applied to the body, including the verify-compile pass.        | `-language:experimental.safe`                 |
| `DynamicEvalAgentApiTests`       | The `@evalLike` / `@evalSafeLike` wrapper API, the `eval { ctx => ... }` closure form, and `EvalContext`. | (defaults)                                    |
| `DynamicEvalLogTests`            | The per-invocation log files written by `-Xrepl-eval-log-dir`.                 | `-Xrepl-eval-log-dir:<dir>`                   |

`StandaloneEvalTests` (in
`repl/test/dotty/tools/eval/StandaloneEvalTests.scala`) covers the
standalone path end-to-end: each test compiles a complete program
with the main `dotc` pipeline under `-Xdynamic-eval`, loads the
classes, and invokes an entry point whose `eval(...)` calls go
through the standalone adapter. It covers basics, captures of every
kind, live module state, top-level definitions, named packages,
class members, nested eval, compile-error reporting (including the
`return` limitation), flag gating, and settings forwarding via
`dotty.tools.eval.settings`.

The lower-level unit tests (in
`repl/test/dotty/tools/eval/`) exercise the pipeline without
the full REPL:

| Suite                      | What it covers                                                                                  |
|----------------------------|-------------------------------------------------------------------------------------------------|
| `EvalCompilerBridgeTest`   | Splice + extract mechanics: spliced enclosing source compiles, `__Expression` is emitted, `evaluate()` returns the body's value. |
| `EvalAdapterTest`          | The `EvalAdapter.evalIsolated` entry point on the basic shapes (no captures, simple captures, `expectedType` cast). |

`ReplHistoryTests` (in the parent `dotty.tools.repl` package, so
not matched by the `eval.*` glob above) covers the related
`-Xrepl-history-file` transcript feature.

Because `scala3-repl` runs on the bootstrapped compiler, the first
invocation in a fresh sbt session builds `scala3-compiler-bootstrapped`
and can take a while; subsequent `testOnly` runs are incremental.

---------------------------

Scala 3
=====
[![Scala 3 CI](https://github.com/scala/scala3/actions/workflows/ci.yaml/badge.svg?branch=main)](https://github.com/scala/scala3/actions/workflows/ci.yaml?query=branch%3Amain)
[![Join the chat at https://discord.com/invite/scala](https://img.shields.io/discord/632150470000902164)](https://discord.com/invite/scala)

This is the home of the [Scala 3](https://www.scala-lang.org) standard library, compiler, and language spec.
More documentation [here](https://docs.scala-lang.org/scala3/).

Building a Local Distribution
=============================
1. `sbt dist/Universal/packageBin`
2. Find the newly-built distributions in `dist/target/`

Code of Conduct
===============
Scala 3 uses the [Scala Code of Conduct](https://www.scala-lang.org/conduct.html)
for all communication and discussion. This includes both GitHub, Discord and
other more direct lines of communication such as email.

How to Contribute
=================
* [Contributing guide](./CONTRIBUTING.md)
* [Issues where help is wanted](https://github.com/scala/scala3/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22)
* [Policy regarding the use of LLM-based tools](LLM_POLICY.md)

License
=======
Scala 3 is licensed under the [Apache License Version 2.0](https://www.apache.org/licenses/LICENSE-2.0)
