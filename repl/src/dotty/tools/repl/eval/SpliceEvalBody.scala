package dotty.tools
package repl
package eval

import dotty.tools.dotc.ast.untpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SrcPos

/** Parser-stage phase that splices the eval body into the enclosing
 *  source at the marker position and appends a synthesised
 *  `__Expression` class to the same package.
 *
 *  The marker is a valid identifier ([[EvalContext.placeholder]] =
 *  `__evalBodyPlaceholder__`), so after parsing it appears as
 *  `Ident(<marker>)` exactly where the user's `eval(...)` call stood.
 *
 *  Input:
 *  {{{
 *  object EnclosingTest:
 *    def f(): Int = ({ __evalBodyPlaceholder__ })
 *  }}}
 *
 *  Output:
 *  {{{
 *  object EnclosingTest:
 *    def f(): Int = ({
 *      val __evalResult: Int = { <parsed body> }
 *      scala.Predef.print("")  // suppress constant folding
 *      __evalResult
 *    })
 *
 *  class __Expression(thisObject: Object | Null,
 *                     bindings: Array[Eval.Binding]):
 *    def evaluate(): Any = ()
 *    // ... reflective accessor helpers
 *  }}}
 *
 *  [[ExtractEvalBody]] later drains the spliced val's rhs into
 *  `__Expression.evaluate`, and [[ResolveEvalAccess]] lowers
 *  outer-scope references to reflective accessor calls.
 *
 *  Each compilation should hit exactly one marker. Extras are an
 *  error in test mode, a warning otherwise.
 */
private[eval] class SpliceEvalBody(config: EvalCompilerConfig) extends Phase:
  import SpliceEvalBody.*

  override def phaseName: String = SpliceEvalBody.name
  override def isCheckable: Boolean = false

  /** Sticky for the duration of the run: true once the marker has been
   *  found and replaced. Read by the post-run "marker missing" check.
   */
  private var spliced = false

  /** Resets per `PackageDef` so the `__Expression` class is appended
   *  to exactly one package — the one that contained the spliced
   *  marker — even if the compilation unit has multiple PackageDefs.
   */
  private var expressionAppended = false

  protected def run(using Context): Unit =
    spliced = false
    expressionAppended = false
    privateFieldsByThis = Map.empty
    privateMethodsByThis = Map.empty
    liftedDefParamNames = Set.empty
    // Pre-pass: lift each marker-bearing method out of its enclosing
    // class. Otherwise the wrapper's compile emits a second class
    // under the same source name as the REPL session's already-loaded
    // copy, and instances crossing the two loaders fail `checkcast`
    // ("X cannot be cast to X (different loaders)").
    //
    // Gated on `import rs$line$N.{*}`: the lift drops the class
    // declaration, so it's only safe when the class is reachable
    // elsewhere (the REPL session's compiled copy). Bridge-test
    // fixtures don't have that import and rely on the wrapper to be
    // the only source of the class.
    val hasReplSessionImport = sourceImportsReplSession(ctx.compilationUnit.untpdTree)
    val extractor = new ClassMethodExtractor
    val lifted =
      if hasReplSessionImport then extractor.transform(ctx.compilationUnit.untpdTree)
      else ctx.compilationUnit.untpdTree
    // Lifted methods have no enclosing `This` for the original class,
    // so rewrite plain `this` in the body to the injected `__this__`
    // param. Private member accesses also need reflective indirection:
    // `import __this__.*` doesn't expose privates, and the typer
    // rejects a direct `__this__.v` Select for private `v`.
    val parsedBody =
      if extractor.didLift then rewritePrivateAccessInBody(rewriteThisInBody(parseBody))
      else parseBody
    val expressionClass = parseExpressionClass
    val needsHelpers =
      privateFieldsByThis.values.exists(_.nonEmpty) ||
        privateMethodsByThis.values.exists(_.nonEmpty)
    val withReflectionHelpers =
      if needsHelpers then injectReflectionHelpers(lifted) else lifted
    val splicer = new Splicer(parsedBody, expressionClass)
    ctx.compilationUnit.untpdTree = splicer.transform(withReflectionHelpers)
    if !spliced && config.testMode then
      report.error(
        s"eval body marker `${config.marker}` not found in enclosing source",
        ctx.compilationUnit.untpdTree.srcPos
      )

  /** Walk the parsed body and redirect references to private class
   *  members through the synthesised reflection helpers. Three
   *  helpers cover all access patterns:
   *
   *    - `__refl_get__(qual, name)` for private field reads
   *    - `__refl_set__(qual, name, rhs)` for private field writes
   *    - `__refl_call__(qual, name, args)` for private method calls
   *
   *  `qual` is the lifted-def parameter that captured the relevant
   *  `this`: `__this__` for the innermost class, `__this__<OuterName>`
   *  for each enclosing scope (matching what
   *  [[rewriteThisInBody]] produces from `OuterName.this`). The
   *  body-shape patterns recognised:
   *
   *    Field reads:
   *      - `__this__.x`  →  `__refl_get__(__this__, "x").asInstanceOf[T]`
   *      - bare `x`      →  same (only if the innermost scope owns `x`
   *                         and no method param shadows it)
   *
   *    Field writes:
   *      - `__this__.x = rhs`        →  `__refl_set__(__this__, "x", rhs)`
   *
   *    Method calls:
   *      - `__this__.m(args)`        →  `__refl_call__(__this__, "m",
   *                                       Array(args*)).asInstanceOf[R]`
   *      - bare `m(args)`            →  same (innermost-scope only,
   *                                       not shadowed by a param)
   *      - `__this__.m`/bare `m`     →  same with empty args (for
   *                                       parens-omitted nullary defs)
   */
  private def rewritePrivateAccessInBody(body: Tree)(using Context): Tree =
    if privateFieldsByThis.isEmpty && privateMethodsByThis.isEmpty then body
    else
      val rewriter = new UntypedTreeMap:
        override def transform(tree: Tree)(using Context): Tree = tree match
          // Write: `qual.name = rhs` for any qual we know about.
          case Assign(Select(Ident(qual), name), rhs)
              if privateFieldsByThis.get(qual.toTermName).exists(_.contains(name.toTermName)) =>
            mkReflSet(qual.toTermName, name.toTermName, transform(rhs)).withSpan(tree.span)

          // Method call: `qual.m(args*)`.
          case tree @ Apply(Select(Ident(qual), name), args)
              if privateMethodsByThis.get(qual.toTermName).exists(_.contains(name.toTermName)) =>
            val q = qual.toTermName
            val n = name.toTermName
            mkReflCall(q, n, args.map(transform), privateMethodsByThis(q)(n)).withSpan(tree.span)

          // Method call: bare `m(args*)` (innermost-scope only).
          case tree @ Apply(Ident(name), args)
              if privateMethodsInScope.contains(name.toTermName)
                && !liftedDefParamNames.contains(name.toTermName) =>
            val n = name.toTermName
            mkReflCall(termName("__this__"), n, args.map(transform), privateMethodsInScope(n)).withSpan(tree.span)

          // Field read: `qual.name` (post-This-rewrite, qual is
          // `__this__` or `__this__<Outer>`).
          case Select(Ident(qual), name)
              if privateFieldsByThis.get(qual.toTermName).exists(_.contains(name.toTermName)) =>
            val q = qual.toTermName
            val n = name.toTermName
            mkReflGet(q, n, privateFieldsByThis(q)(n)).withSpan(tree.span)

          // Method reference: `qual.name` for a parens-omitted
          // method (used as a value or invoked with auto-application).
          case Select(Ident(qual), name)
              if privateMethodsByThis.get(qual.toTermName).exists(_.contains(name.toTermName)) =>
            val q = qual.toTermName
            val n = name.toTermName
            mkReflCall(q, n, Nil, privateMethodsByThis(q)(n)).withSpan(tree.span)

          // Bare `name`: read of a private innermost-scope field.
          case id @ Ident(name)
              if privateFieldsInScope.contains(name.toTermName)
                && !liftedDefParamNames.contains(name.toTermName) =>
            mkReflGet(termName("__this__"), name.toTermName,
              privateFieldsInScope(name.toTermName)).withSpan(id.span)

          // Bare `name`: nullary call to a private innermost-scope
          // method.
          case id @ Ident(name)
              if privateMethodsInScope.contains(name.toTermName)
                && !liftedDefParamNames.contains(name.toTermName) =>
            mkReflCall(termName("__this__"), name.toTermName, Nil,
              privateMethodsInScope(name.toTermName)).withSpan(id.span)

          // Don't rewrite declaration-position Idents.
          case vd: ValDef
              if privateFieldsInScope.contains(vd.name) ||
                privateMethodsInScope.contains(vd.name) =>
            cpy.ValDef(vd)(vd.name, vd.tpt, transform(vd.rhs))

          case _ => super.transform(tree)
      rewriter.transform(body)

  /** `__refl_get__(__this__, "name").asInstanceOf[T]`. The cast hands
   *  the reflective `Any` back at the field's declared type so the
   *  surrounding expression keeps its type. The `tpt` is duplicated
   *  to avoid sharing tree nodes with the dropped class declaration.
   */
  private def mkReflGet(thisName: TermName, fieldName: TermName, tpt: Tree)(using Context): Tree =
    val call = Apply(
      Ident(termName("__refl_get__")),
      List(Ident(thisName), Literal(Constant(fieldName.toString)))
    )
    TypeApply(Select(call, termName("asInstanceOf")), List(freshTypeTree(tpt)))

  /** Fresh copy of an untyped type tree, recursively duplicating
   *  nested `Ident`/`AppliedTypeTree`/`Select` shapes so no node
   *  identity is shared with the original ClassDef's tparam list.
   */
  private def freshTypeTree(tpt: Tree)(using Context): Tree = tpt match
    case Ident(n) => Ident(n).withSpan(tpt.span)
    case AppliedTypeTree(tycon, args) =>
      AppliedTypeTree(freshTypeTree(tycon), args.map(freshTypeTree)).withSpan(tpt.span)
    case Select(qual, n) => Select(qual, n).withSpan(tpt.span)
    case _ => tpt

  /** `__refl_set__(__this__, "name", rhs)` (returns Unit). */
  private def mkReflSet(thisName: TermName, fieldName: TermName, rhs: Tree)(using Context): Tree =
    Apply(
      Ident(termName("__refl_set__")),
      List(Ident(thisName), Literal(Constant(fieldName.toString)), rhs)
    )

  /** `__refl_call__(qual, "name", Array[Object](args*)).asInstanceOf[R]`.
   *  Method args are wrapped in an `Object` array; primitives get
   *  cast to `AnyRef` so Scala auto-boxes them on the way in. The
   *  outer cast hands the reflective `Any` back at the method's
   *  declared return type.
   */
  private def mkReflCall(qual: TermName, methodName: TermName, args: List[Tree], retTpt: Tree)(using Context): Tree =
    val boxedArgs = args.map { a =>
      // `arg.asInstanceOf[Object]` triggers Scala's value-class
      // boxing for primitives so `Int` arrives as `java.lang.Integer`
      // (which Method.invoke expects for an `Int` parameter).
      TypeApply(Select(a, termName("asInstanceOf")), List(Ident(typeName("Object"))))
    }
    val argArray =
      JavaSeqLiteral(boxedArgs, Ident(typeName("Object")))
    val rawCall = Apply(
      Ident(termName("__refl_call__")),
      List(
        Ident(qual),
        Literal(Constant(methodName.toString)),
        argArray
      )
    )
    TypeApply(Select(rawCall, termName("asInstanceOf")), List(freshTypeTree(retTpt)))

  /** Inject typer-only stubs of `__refl_get__/_set__/_call__` into the
   *  wrapper module. The bodies are `???` because no one ever calls
   *  these at runtime: `ExtractEvalBody.retargetReflHelpers` rewrites
   *  every body reference to point at the inherited copies on
   *  [[EvalExpressionBase]] (which delegate to `getField` / `setField` /
   *  `callMethod` with an empty className). The stubs exist purely so
   *  the body type-checks against the wrapper's lifted def at typer
   *  time, when the body still lives in the wrapper's scope.
   */
  private def injectReflectionHelpers(tree: Tree)(using Context): Tree =
    val helpersSrc =
      """|object __EvalSafeReflHelpers__ {
         |  def __refl_get__(obj: Object, name: String): Any = ???
         |  def __refl_set__(obj: Object, name: String, value: Any): Unit = ???
         |  def __refl_call__(obj: Object, name: String, args: Array[Object]): Any = ???
         |}
         |""".stripMargin
    val source = SourceFile.virtual("<eval-refl-helpers>", helpersSrc)
    val newCtx = ctx.fresh.setSource(source)
    val parser = Parsers.Parser(source)(using newCtx)
    val parsed = parser.parse().asInstanceOf[PackageDef]
    val helpersModule = parsed.stats.head.asInstanceOf[ModuleDef]
    val helperMethods = helpersModule.impl.body
    // Place the methods at the top of the wrapper module's body so
    // they're in scope wherever the marker lands.
    val rewriter = new UntypedTreeMap:
      override def transform(t: Tree)(using Context): Tree = t match
        case mod: ModuleDef =>
          val newImpl = cpy.Template(mod.impl)(
            constr = mod.impl.constr,
            parents = mod.impl.parents,
            derived = mod.impl.derived,
            self = mod.impl.self,
            body = helperMethods ++ mod.impl.body
          )
          ModuleDef(mod.name, newImpl).withMods(mod.mods).withSpan(mod.span)
        case _ => super.transform(t)
    rewriter.transform(tree)

  /** Strip `Apply(Ident(name), Nil)` → `Ident(name)` for any name in
   *  `parenless`. Lets a body reference a parens-omitted sibling
   *  `def g = 42` as either `g` or `g()`; without this, the typer
   *  rejects `g()` with "method g does not take parameters" because
   *  the actual splice exposes the original def.
   */
  private def stripEmptyApplyFor(body: Tree, parenless: Set[TermName])(using Context): Tree =
    val rewriter = new UntypedTreeMap:
      override def transform(tree: Tree)(using Context): Tree = tree match
        case Apply(id @ Ident(n), Nil) if parenless.contains(n.toTermName) =>
          Ident(n).withSpan(tree.span)
        case _ => super.transform(tree)
    rewriter.transform(body)

  /** Walk the parsed body and replace each plain `This(EmptyType)`
   *  node with `Ident(__this__)`. Used after [[ClassMethodExtractor]]
   *  lifts one or more methods out of an enclosing class — the body's
   *  original `this` no longer has the class as its enclosing
   *  `This` after the lift, so leaving them in would resolve to
   *  the wrapper module.
   *
   *  *Qualified* `OuterClass.this` is left alone; those resolve via
   *  the typer's outer-chain machinery and are subsequently lowered
   *  by [[ExtractEvalBody]]'s `This` strategy.
   */
  private def rewriteThisInBody(body: Tree)(using Context): Tree =
    val thisName = termName("__this__")
    val rewriter = new UntypedTreeMap:
      override def transform(tree: Tree)(using Context): Tree = tree match
        case t @ This(qual) if qual.name.isEmpty =>
          Ident(thisName).withSpan(t.span)
        case t @ This(qual) =>
          // Qualified `OuterName.this` — point at the corresponding
          // `__this__<OuterName>` parameter the lifted def carries.
          // Matches `EvalRewriteTyped's `__this__<OuterName>``.
          Ident(termName(s"__this__${qual.name}")).withSpan(t.span)
        // Don't descend into a body-local class/object: its inner
        // `this` refers to that nested type, not to the enclosing
        // class we lifted out of.
        case _: TypeDef | _: ModuleDef => tree
        case _ => super.transform(tree)
    rewriter.transform(body)

  private class Splicer(body: Tree, expressionClass: Seq[Tree]) extends UntypedTreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case pkg: PackageDef =>
          val transformed = super.transform(pkg).asInstanceOf[PackageDef]
          // Append the synthesised __Expression class as a sibling
          // of the enclosing source. We append to the first PackageDef
          // that contains a splice; subsequent PackageDefs (rare) are
          // left alone. `expressionAppended` is kept distinct from
          // `spliced` so the latter stays a sticky "did the marker
          // ever appear" signal for the post-run check.
          if spliced && !expressionAppended then
            expressionAppended = true
            cpy.PackageDef(transformed)(
              transformed.pid,
              transformed.stats ++ expressionClass.map(_.withSpan(pkg.span))
            )
          else transformed

        // Block whose trailing expression is the marker: splice the
        // body in *and* hoist any sibling `given` declarations from
        // earlier stats into the eval body. Without the hoist, the
        // typer compiles the givens at the def's top-level scope but
        // ExtractEvalBody only moves the eval body to evaluate, so
        // the body's `summon[T]` references a given that lives
        // outside evaluate's scope at runtime — the bindings array's
        // synthetic `__given_<n>` name doesn't match the typer's
        // `given_<T>` synthesised name, and the lookup fails.
        // Folding the givens into the spliced body keeps everything
        // co-located: the typer compiles `given Int = 99` inside
        // the body, ExtractEvalBody drains the whole block (givens
        // and `summon[Int]`) into evaluate, and the resolution
        // stays self-contained.
        case bk @ Block(stats, expr: Ident) if expr.name.toString == config.marker =>
          val givens = stats.collect {
            case vd: ValDef if vd.mods.flags.is(Given) => vd
          }
          // Sibling defs declared without parens (`def g = 42`):
          // strip any `g()` use in the body to a bare `g`. Otherwise
          // the typer rejects the empty-Apply since the actual def
          // doesn't take parameters.
          val parenslessDefNames = stats.collect {
            case dd: DefDef if dd.name != nme.CONSTRUCTOR && !dd.name.isEmpty && dd.paramss.isEmpty =>
              dd.name
          }.toSet
          val effectiveBody =
            if parenslessDefNames.isEmpty then body
            else stripEmptyApplyFor(body, parenslessDefNames)
          val markerReplacement = mkExprBlock(effectiveBody, expr, hoistedGivens = givens)
          val keptStats = stats.filterNot(s => s.isInstanceOf[ValDef] && s.asInstanceOf[ValDef].mods.flags.is(Given))
          if keptStats.isEmpty then markerReplacement
          else cpy.Block(bk)(keptStats.map(transform), markerReplacement)

        // Marker found at expression position. Replace with the spliced
        // block. The marker name comes from `EvalContext.placeholder`
        // by default; configurable via `EvalCompilerConfig.marker`.
        case id: Ident if id.name.toString == config.marker =>
          mkExprBlock(body, id)

        case _ => super.transform(tree)

  /** Per-`__this__` qualifier maps for each lifted class's private
   *  members. Keys are the lifted def's parameter name — `__this__`
   *  for the innermost class, `__this__<OuterName>` for each outer
   *  scope. Drive the body rewriter when redirecting `__this__.v`
   *  / bare-name references to reflective helpers. Reset per `run`.
   */
  private var privateFieldsByThis: Map[TermName, Map[TermName, Tree]] = Map.empty
  private var privateMethodsByThis: Map[TermName, Map[TermName, Tree]] = Map.empty

  /** Value-param names on the lifted def. The body rewriter consults
   *  this to avoid shadowing a method param with a same-named private
   *  class member. Reset per `run`.
   */
  private var liftedDefParamNames: Set[TermName] = Set.empty

  /** Innermost-scope privates — those reachable without an explicit
   *  `this.` qualifier in the original class body.
   */
  private def privateFieldsInScope: Map[TermName, Tree] =
    privateFieldsByThis.getOrElse(termName("__this__"), Map.empty)
  private def privateMethodsInScope: Map[TermName, Tree] =
    privateMethodsByThis.getOrElse(termName("__this__"), Map.empty)

  /** Pre-pass that lifts each marker-bearing method out of its
   *  enclosing class:
   *
   *    - The DefDef is hoisted to the surrounding scope.
   *    - For non-singleton classes, a `__this__: <ClassName>`
   *      parameter is prepended and the body is wrapped in
   *      `import __this__.*` so unqualified member references
   *      resolve through it.
   *    - Singletons need no `__this__`: they're reachable via
   *      `import rs$line$N.X.*`.
   *    - The class declaration itself is dropped — the REPL session
   *      already has it compiled, and a duplicate would mint a
   *      second `X.class` under a different fully-qualified JVM name,
   *      breaking `checkcast`.
   *
   *  Sibling methods that don't carry the marker are also dropped:
   *  only `__Expression.evaluate` runs at runtime.
   */
  private class ClassMethodExtractor extends UntypedTreeMap:
    /** True once any class-method lift has fired during the walk.
     *  Used by [[run]] to gate the `this` → `__this__` body rewrite.
     */
    var didLift: Boolean = false

    override def transform(tree: Tree)(using Context): Tree = tree match
      case pkg: PackageDef =>
        cpy.PackageDef(pkg)(pkg.pid, pkg.stats.flatMap(transformTopStat))

      case mod: ModuleDef =>
        val newImpl = cpy.Template(mod.impl)(
          constr = mod.impl.constr,
          parents = mod.impl.parents,
          derived = mod.impl.derived,
          self = mod.impl.self,
          body = mod.impl.body.flatMap(transformTopStat)
        )
        ModuleDef(mod.name, newImpl).withMods(mod.mods).withSpan(mod.span)

      case _ => super.transform(tree)

    private def transformTopStat(stat: Tree)(using Context): List[Tree] = stat match
      case td: TypeDef if td.isClassDef && containsMarker(td) =>
        didLift = true
        liftClassMethods(td)
      case dd: DefDef if containsMarker(dd) =>
        // Top-level marker-bearing def: rename it so the wrapper's
        // copy doesn't shadow `import rs$line$N.{*}`'s already-
        // compiled version. Body code calling the def by its
        // original name resolves to the session-level one — which
        // is essential for recursive eval (the body re-enters the
        // *real* function, not the wrapper's stub whose RHS gets
        // drained into `__Expression.evaluate` by ExtractEvalBody).
        List(renameTopLevelDef(dd))
      case mod: ModuleDef => List(transform(mod))
      case other => List(other)
  end ClassMethodExtractor

  /** Rename a top-level marker-bearing DefDef so the original name
   *  resolves through the wrapper's auto-import to the REPL session
   *  version. The renamed copy still carries the marker, so the
   *  splice still has a place to land.
   */
  private def renameTopLevelDef(dd: DefDef)(using Context): DefDef =
    val newName = termName(s"__eval_${dd.name}__")
    DefDef(newName, dd.paramss, dd.tpt, dd.rhs).withMods(dd.mods).withSpan(dd.span)

  /** True iff the parsed source has an `import rs$line$N.{...}` —
   *  the signal that we're compiling a body in the context of a live
   *  REPL session whose classes are reachable through the auto-import.
   *  Bridge-test fixtures and other standalone callers of the
   *  eval pipeline omit this import; they rely on the wrapper
   *  compile to be the only source of the class declarations they
   *  reference, so the lift would drop fixtures the test loads
   *  directly.
   */
  private def sourceImportsReplSession(tree: Tree)(using Context): Boolean =
    var found = false
    val finder = new UntypedTreeTraverser:
      override def traverse(t: Tree)(using Context): Unit =
        if found then ()
        else t match
          case Import(expr, _) =>
            // The import expression is a Select chain; the head Ident
            // is the leftmost qualifier. Match `rs$line$<digits>` —
            // the REPL line wrapper module name.
            def head(e: Tree): String = e match
              case Ident(n) => n.toString
              case Select(q, _) => head(q)
              case _ => ""
            if head(expr).startsWith("rs$line$") then found = true
            else traverseChildren(t)
          case _ => traverseChildren(t)
    finder.traverse(tree)
    found

  /** True iff the tree transitively contains an `Ident(<marker>)`. */
  private def containsMarker(tree: Tree)(using Context): Boolean =
    var found = false
    val finder = new UntypedTreeTraverser:
      override def traverse(t: Tree)(using Context): Unit =
        if found then ()
        else t match
          case id: Ident if id.name.toString == config.marker =>
            found = true
          case _ =>
            traverseChildren(t)
    finder.traverse(tree)
    found

  /** Lift each marker-bearing DefDef out of the given ClassDef.
   *  Returns the lifted defs, with the original class declaration
   *  dropped. The class is reachable via the wrapper's
   *  `import rs$line$N.{*}`, so dropping the local declaration is
   *  the whole point of the rewrite.
   */
  private def liftClassMethods(td: TypeDef)(using Context): List[Tree] =
    liftClassMethodsRec(td, outerScopes = Nil)

  /** Recursively walk into nested classes to find marker-bearing
   *  DefDefs, lifting each one out of all of its enclosing classes
   *  in a single pass. The accumulated `outerScopes` carries the
   *  type params, type ref, and class name of each enclosing class
   *  (innermost first), so the lifted def can take a `__this__`
   *  parameter for the immediate class plus `__this__<OuterName>`
   *  parameters for every outer class — matching the binding names
   *  the parser-stage rewriter populates in the bindings array.
   */
  private def liftClassMethodsRec(td: TypeDef, outerScopes: List[OuterScope])(using Context): List[Tree] =
    val tmpl = td.rhs.asInstanceOf[Template]
    val tparams = classTypeParams(td)
    // For nested classes, the type-ref of the inner `__this__`
    // parameter needs a path-dependent prefix on the outer
    // `__this__<OuterName>` parameter — `def m(__this__A: A,
    // __this__: __this__A.B)` — because `A.B` (bare static path)
    // doesn't resolve when A is a class, only when A is an object.
    // The prefix-paramName comes from the immediate outer scope
    // (the parent class), if any.
    val outerThisNames = outerScopes.map(s => termName(s"__this__${s.name}"))
    val typeRef = applyClassTypeParams(td, tparams, outerThisNames)
    val members = classMemberNames(td)
    val privateFields = classPrivateFields(td)
    val privateMethods = classPrivateMethods(td)
    val scope = OuterScope(td.name, typeRef, tparams, members, privateFields, privateMethods)
    val newOuters = scope :: outerScopes
    tmpl.body.flatMap {
      case dd: DefDef if containsMarker(dd) =>
        // Record per-qualifier private fields/methods for the body
        // rewriter. The innermost class is reached via `__this__`;
        // each outer enclosing class via `__this__<OuterName>`.
        // These are the qualifiers `rewriteThisInBody` produces for
        // `OuterName.this`-style references.
        val innermostThis = termName("__this__")
        privateFieldsByThis = privateFieldsByThis.updated(innermostThis, privateFields)
        privateMethodsByThis = privateMethodsByThis.updated(innermostThis, privateMethods)
        outerScopes.foreach { s =>
          val q = termName(s"__this__${s.name}")
          privateFieldsByThis = privateFieldsByThis.updated(q, s.privateFields)
          privateMethodsByThis = privateMethodsByThis.updated(q, s.privateMethods)
        }
        // Record the lifted def's value-param names so the body
        // rewriter can skip bare-Ident rewrites that would otherwise
        // shadow a method param sharing a name with a private
        // class member.
        liftedDefParamNames = dd.paramss.flatMap { ps =>
          ps.collect { case vd: ValDef => vd.name }
        }.toSet ++ liftedDefParamNames
        List(liftDef(dd, newOuters))
      case nested: TypeDef if nested.isClassDef && containsMarker(nested) =>
        liftClassMethodsRec(nested, newOuters)
      case _ => Nil
    }

  /** One layer of enclosing-class context for the lifted-method
   *  synthesis. `outerScopes` is built innermost-first as we recurse
   *  into nested classes; each scope contributes a `__this__` (or
   *  `__this__<Name>` for outer scopes) parameter to the final
   *  lifted def.
   *
   *  `privateFields` carries the names of private val/var members
   *  (with their declared type tree) so the body rewriter can
   *  redirect references to them through the synthesised reflective
   *  accessors — `import __this__.*` doesn't expose private members,
   *  and the typer rejects direct `__this__.v` accesses when v is
   *  private. Reflection sidesteps both checks.
   */
  private case class OuterScope(
      name: TypeName,
      typeRef: Tree,
      tparams: List[TypeDef],
      memberNames: Set[TermName],
      privateFields: Map[TermName, Tree],
      privateMethods: Map[TermName, Tree]
  )

  /** Map of private term-member names → declared type tree. Used by
   *  the body rewriter to route bare or `__this__.x` references to
   *  reflective accessors when `x` is private. Includes both ctor
   *  val/var params and body val/var declarations.
   */
  private def classPrivateFields(td: TypeDef)(using Context): Map[TermName, Tree] =
    val tmpl = td.rhs.asInstanceOf[Template]
    val builder = collection.mutable.LinkedHashMap.empty[TermName, Tree]
    tmpl.constr.paramss.foreach { ps =>
      ps.foreach {
        case vd: ValDef if vd.mods.flags.is(Private) && !vd.name.isEmpty =>
          builder.put(vd.name, vd.tpt)
        case _ =>
      }
    }
    tmpl.body.foreach {
      case vd: ValDef if vd.mods.flags.is(Private) && !vd.name.isEmpty =>
        builder.put(vd.name, vd.tpt)
      case _ =>
    }
    builder.toMap

  /** Map of private method names → declared return type tree.
   *  Mirrors [[classPrivateFields]] for `def`s, so the body rewriter
   *  can route private method calls through the synthesised
   *  `__refl_call__` helper.
   */
  private def classPrivateMethods(td: TypeDef)(using Context): Map[TermName, Tree] =
    val tmpl = td.rhs.asInstanceOf[Template]
    val builder = collection.mutable.LinkedHashMap.empty[TermName, Tree]
    tmpl.body.foreach {
      case dd: DefDef
          if dd.mods.flags.is(Private) && !dd.name.isEmpty
            && dd.name != nme.CONSTRUCTOR =>
        builder.put(dd.name.asTermName, dd.tpt)
      case _ =>
    }
    builder.toMap

  /** Term-member names of `td` (ctor val/var params and body
   *  val/var/def names). Drives the lifted def's `import __this__.*`:
   *  only method params that shadow a class member need hiding, and
   *  hiding non-shadowing params confuses the resolver into routing
   *  the bare reference back through the import.
   */
  private def classMemberNames(td: TypeDef)(using Context): Set[TermName] =
    val tmpl = td.rhs.asInstanceOf[Template]
    val ctorParams = tmpl.constr.paramss.flatMap { ps =>
      ps.collect { case vd: ValDef => vd.name }
    }
    val bodyMembers = tmpl.body.collect {
      case vd: ValDef => vd.name
      case dd: DefDef if dd.name != nme.CONSTRUCTOR => dd.name.asTermName
    }
    (ctorParams ++ bodyMembers).toSet

  /** Extract the leading type-param clause of a class declaration
   *  (`class C[T1, T2](...)`) — these become type params on every
   *  lifted method so its `__this__: C[T1, T2]` annotation type-
   *  checks. Returns `Nil` for monomorphic classes.
   */
  private def classTypeParams(td: TypeDef)(using Context): List[TypeDef] =
    val tmpl = td.rhs.asInstanceOf[Template]
    tmpl.constr.paramss match
      case (head @ ((_: TypeDef) :: _)) :: _ =>
        head.collect { case t: TypeDef => t }
      case _ => Nil

  /** Build a (possibly path-dependent) class-type reference for the
   *  lifted `__this__` parameter:
   *
   *    - For top-level `class C` it's `Ident(C)` (or
   *      `AppliedTypeTree(Ident(C), [T1, ..., Tn])` if generic).
   *    - For nested `class A: class B`, it's `Select(Ident(__this__A), B)`
   *      — a path-dependent type on the outer `__this__A`
   *      parameter, since `A.B` (bare prefix) doesn't resolve when
   *      A is a class. The lifted def declares `__this__A` ahead of
   *      `__this__: __this__A.B` so the prefix is in scope.
   *
   *  `outerThisNames` lists the `__this__<Outer>` parameter names
   *  innermost-first (i.e. closest enclosing class first); the
   *  *immediate* outer class's parameter is the qualifier we use.
   */
  private def applyClassTypeParams(
      td: TypeDef,
      tparams: List[TypeDef],
      outerThisNames: List[TermName]
  )(using Context): Tree =
    val baseName = td.name
    val base: Tree = outerThisNames match
      case Nil => Ident(baseName)
      case head :: _ => Select(Ident(head), baseName)
    if tparams.isEmpty then base
    else AppliedTypeTree(base, tparams.map(t => Ident(t.name)))

  /** Lift a marker-bearing DefDef out of its enclosing class
   *  (potentially several layers deep). The `outerScopes` list runs
   *  innermost-first; for `class A: class B: def m`, it's
   *  `[OuterScope(B), OuterScope(A)]`.
   *
   *  The lifted def takes:
   *    - one `__this__` parameter for the innermost class,
   *    - one `__this__<OuterName>` parameter per outer class,
   *  matching the bindings the parser-stage rewriter populates.
   *  Class type params from each scope are carried over as type
   *  params of the lifted def. The body is wrapped in a Block
   *  starting with `import __this__.*` (member names that shadow
   *  method params are hidden).
   */
  private def liftDef(dd: DefDef, outerScopes: List[OuterScope])(using Context): Tree =
    val innermost = outerScopes.head
    val span = dd.span
    val thisName = termName("__this__")
    // Hide only the method params that *also* exist as class
    // members — those are the ambiguity-prone names. Hiding ones
    // that aren't class members would still hide them at the
    // import level, which dotty's resolver treats as a routing
    // hint that leads to the bare reference being sought through
    // `__this__` and missing the actual method param.
    val paramNames: List[TermName] = dd.paramss.flatMap { ps =>
      ps.collect { case vd: ValDef => vd.name }
    }
    val shadowingNames = paramNames.filter(innermost.memberNames.contains)
    val hideSelectors: List[ImportSelector] = shadowingNames.map(n =>
      ImportSelector(Ident(n), Ident(nme.WILDCARD), EmptyTree)
    )
    val wildcardSelector: ImportSelector = ImportSelector(Ident(nme.WILDCARD))
    val importSelectors: List[ImportSelector] = hideSelectors :+ wildcardSelector
    val importStat: Tree =
      Import(Ident(thisName), importSelectors).withSpan(span)
    val newRhs = dd.rhs match
      case EmptyTree => dd.rhs
      case Block(stats, expr) => Block(importStat :: stats, expr).withSpan(dd.rhs.span)
      case other => Block(importStat :: Nil, other).withSpan(dd.rhs.span)
    val (existingTparams, valueClauses) =
      dd.paramss match
        case (head @ ((_: TypeDef) :: _)) :: rest =>
          (head.collect { case t: TypeDef => t }, rest)
        case other => (Nil, other)
    // Fresh copies of every enclosing class's type params so they
    // aren't aliased back to their original (now-dropped) class
    // scopes. Outermost-first to match Scala's left-to-right
    // type-param ordering.
    //
    // Use a fresh untyped Modifiers value (drop the original ClassDef
    // tparam mods, which carry `Param | Variance` bits whose typer
    // resolution refers back to the dropped ClassDef). The new
    // tparam needs only the standard `Param` modifier so the def's
    // own typer scope binds it cleanly.
    val carriedTparams: List[TypeDef] = outerScopes.reverse.flatMap(_.tparams).map(t =>
      TypeDef(t.name, t.rhs)
        .withMods(Modifiers(Param))
        .withSpan(span)
    )
    // Lifted `__this__` parameters, declared **outermost-first** so
    // path-dependent type refs on outer params (`__this__: __this__A.B`)
    // see their prefix already in scope. Names match
    // `EvalRewriteTyped's `__this__<OuterName>``: `__this__<OuterName>` for
    // outer classes, plain `__this__` for the immediate innermost.
    val outerParams = outerScopes.tail.reverse.map { scope =>
      val name = termName(s"__this__${scope.name}")
      ValDef(name, scope.typeRef, EmptyTree)
        .withMods(Modifiers(Param)).withSpan(span)
    }
    val innermostParam = ValDef(thisName, innermost.typeRef, EmptyTree)
      .withMods(Modifiers(Param)).withSpan(span)
    // Rewrite parameter defaults to qualify class-member references
    // through `__this__`. Param defaults sit *outside* the body's
    // `import __this__.*`, so a bare `base` reference would no
    // longer resolve after the lift drops the enclosing class.
    val rewrittenValueClauses: List[ParamClause] =
      // Walk outer-most-first so the innermost class's mapping wins
      // when a member name appears in multiple enclosing classes.
      val outerThisByMember: Map[TermName, TermName] =
        outerScopes.reverse.foldLeft(Map.empty[TermName, TermName]) { (acc, scope) =>
          val qual =
            if scope eq innermost then thisName
            else termName(s"__this__${scope.name}")
          scope.memberNames.foldLeft(acc) { (m, n) => m.updated(n, qual) }
        }
      val seenParams = collection.mutable.Set.empty[TermName]
      valueClauses.map { clause =>
        val rewritten: List[ValDef] = clause.collect {
          case vd: ValDef =>
            val r = qualifyMembersInDefault(vd.rhs, outerThisByMember -- seenParams)
            seenParams += vd.name
            cpy.ValDef(vd)(rhs = r)
        }
        rewritten: ParamClause
      }
    val newParamss: List[ParamClause] =
      val tparamClause: List[ParamClause] =
        val combined = carriedTparams ++ existingTparams
        if combined.isEmpty then Nil
        else List(combined)
      val thisClause: ParamClause = outerParams :+ innermostParam
      tparamClause ++ (thisClause :: rewrittenValueClauses)
    DefDef(dd.name, newParamss, dd.tpt, newRhs).withMods(dd.mods).withSpan(span)

  /** Walk a parameter default expression, rewriting any bare
   *  `Ident(name)` where `name` is a captured class member into
   *  `Select(Ident(__this__OrOuter), name)`. Plain `This(_)` references
   *  are also redirected to the matching `__this__` parameter, mirroring
   *  the body rewrite.
   */
  private def qualifyMembersInDefault(
      tree: Tree, memberToQualifier: Map[TermName, TermName]
  )(using Context): Tree =
    if tree.isEmpty || memberToQualifier.isEmpty then tree
    else
      val rewriter = new UntypedTreeMap:
        override def transform(t: Tree)(using Context): Tree = t match
          case t @ This(qual) if qual.name.isEmpty =>
            Ident(termName("__this__")).withSpan(t.span)
          case t @ This(qual) =>
            Ident(termName(s"__this__${qual.name}")).withSpan(t.span)
          case id @ Ident(name) =>
            memberToQualifier.get(name.toTermName) match
              case Some(q) =>
                Select(Ident(q).withSpan(id.span), name).withSpan(id.span)
              case None => id
          case _ => super.transform(t)
      rewriter.transform(tree)

  /** Parse `config.body` as a block expression. Spans on the result
   *  are relative to `config.body`. Inner `eval` / `agent` calls
   *  inside the body have their `enclosingSource` argument filled in
   *  by [[EvalRewriteTyped]] (which receives `config` so it can
   *  compose the chained slice when needed). No rewrite happens here.
   */
  private def parseBody(using Context): Tree =
    val source = SourceFile.virtual("<eval-body>", config.body)
    val newCtx = ctx.fresh.setSource(source)
    val parser = Parsers.Parser(source)(using newCtx)
    parser.block()

  /** Parse the synthesised __Expression class declaration. Returns the
   *  list of top-level stats from the synthetic source, suitable for
   *  appending to a PackageDef's stats.
   */
  private def parseExpressionClass(using Context): Seq[Tree] =
    val source = SourceFile.virtual("<eval-expression-class>", expressionClassSource)
    val newCtx = ctx.fresh.setSource(source)
    val parser = Parsers.Parser(source)(using newCtx)
    parser.parse().asInstanceOf[PackageDef].stats

  /** Source for the synthesised `__Expression` class.
   *
   *  The class is a thin subclass of [[EvalExpressionBase]] — all
   *  reflection helpers (`getValue`, `getField`, `setField`,
   *  `callMethod`, `getOuter`, `reflectEval`, …) live on the
   *  pre-compiled `@caps.assumeSafe` base, so a safe-mode REPL
   *  session can extend it without re-checking the helpers'
   *  `@rejectSafe` reflection calls. The synthesised subclass only
   *  carries `evaluate()`, which [[ExtractEvalBody]] later fills in
   *  with the typed user body.
   */
  private def expressionClassSource: String =
    s"""class ${config.outputClassName}(thisObject: Object | Null, bindings: Array[dotty.tools.repl.eval.Eval.Binding])
       |  extends dotty.tools.repl.eval.EvalExpressionBase(thisObject, bindings) {
       |  def evaluate(): Any = ()
       |}
       |""".stripMargin

  /** Build the splice block for the marker site:
   *
   *  ```
   *  {
   *    val __evalResult = { <body> }
   *    scala.Predef.print("")
   *    __evalResult
   *  }
   *  ```
   *
   *  The val carries the body's value, captured for [[ExtractEvalBody]]
   *  to drain into `__Expression.evaluate` later. The `print` effect
   *  prevents constant-folding of the surrounding expression during
   *  firstTransform, mirroring the trick from the debug pipeline's
   *  `InsertExpression`.
   *
   *  The block ends in `__evalResult` (a back-reference to the val)
   *  so the spliced position takes on the body's type — the marker
   *  could have sat in any position, and the typer needs the splice's
   *  type to match the surrounding expectation. With `Literal(())` as
   *  the tail the splice would always be `Unit`, breaking any
   *  enclosing `def f(): Int = ({ <marker> })`.
   */
  private def mkExprBlock(body: Tree, markerTree: Tree, hoistedGivens: List[ValDef] = Nil)(using Context): Tree =
    val span = markerTree.span
    if spliced then
      warnOrError(s"eval body marker `${config.marker}` appears more than once", markerTree.srcPos)
      Literal(Constant(())).withSpan(span)
    else
      spliced = true
      val effectiveBody: Tree =
        if hoistedGivens.isEmpty then body
        else Block(hoistedGivens, body).withSpan(body.span)
      // Use the eval call's expected return type as the val's type
      // annotation when known. This drives the typer's expected-type
      // propagation into the body — needed for body shapes like
      // `s"i => j => i $$op j"` whose lambda-parameter types only
      // resolve when an outer `Int => Int => Int` is expected.
      val valTpt: Tree =
        if config.expectedType.isEmpty then TypeTree()
        else parseTypeFromString(config.expectedType, span)
      val valDef = ValDef(EvalResultName, valTpt, effectiveBody).withSpan(span)
      // `Eval.__noFold__()`, a no-op on the `@caps.assumeSafe` Eval` module:
      // an opaque side-effecting call that prevents the surrounding expression
      // from being constant-folded before `ExtractEvalBody` drains the spliced
      // `val __evalResult`'s rhs.
      val effect = Apply(
        ReplCompiler.selectFqn("dotty.tools.repl.eval.Eval.__noFold__", span),
        Nil
      ).withSpan(span)
      val tail = Ident(EvalResultName).withSpan(span)
      Block(List(valDef, effect), tail).withSpan(span)

  /** Parse a type-source string into an untyped Tree. Wraps the
   *  string in a synthetic `val __t__ : <typeStr> = ???` source so
   *  the standard parser's type-position machinery picks it up;
   *  the wrapper's tpt is then extracted. Falls back to an empty
   *  `TypeTree()` if the string can't be parsed.
   */
  private def parseTypeFromString(typeStr: String, span: Span)(using Context): Tree =
    val source = SourceFile.virtual("<eval-expected-type>", s"val __t__ : $typeStr = ???\n")
    val newCtx = ctx.fresh.setSource(source)
    val parser = Parsers.Parser(source)(using newCtx)
    val parsed =
      try parser.parse()
      catch case _: Throwable => null
    parsed match
      case pkg: PackageDef =>
        pkg.stats.headOption match
          case Some(vd: ValDef) => vd.tpt.withSpan(span)
          case _ => TypeTree().withSpan(span)
      case _ => TypeTree().withSpan(span)

  private def warnOrError(msg: String, srcPos: SrcPos)(using Context): Unit =
    if config.testMode then report.error(msg, srcPos)
    else report.warning(msg, srcPos)

private[eval] object SpliceEvalBody:
  val name: String = "spliceEvalBody"

  /** Name of the val we splice the body's value into. The
   *  [[ExtractEvalBody]] phase identifies the spliced val by this name
   *  and drains its rhs into `__Expression.evaluate`.
   */
  val EvalResultName: TermName = termName("__evalResult")
