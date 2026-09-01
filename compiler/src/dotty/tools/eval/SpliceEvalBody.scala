package dotty.tools
package eval

import dotty.tools.dotc.ast.desugar
import dotty.tools.dotc.ast.untpd.*
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.UniqueName
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.report
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SrcPos

/** Parses and inserts an eval body at [[EvalContext.placeholder]], then appends
 *  the generated expression class to the compilation unit.
 *
 *  The marker is a valid identifier, allowing tree-level replacement without
 *  modifying occurrences in strings or comments. The replacement is a block
 *  containing a marked result value, an opaque no-op that prevents constant
 *  folding, and a read of the result.
 *
 *  [[ExtractEvalBody]] later moves the result's right-hand side into `evaluate`,
 *  and [[ResolveEvalAccess]] lowers outer references. A compilation is expected
 *  to contain exactly one marker.
 */
private[eval] class SpliceEvalBody(config: EvalCompilerConfig) extends Phase:
  import SpliceEvalBody.*

  override def phaseName: String = SpliceEvalBody.name
  override def isCheckable: Boolean = false

  /** Whether this run found and replaced a marker. */
  private var spliced = false

  /** Whether the expression class has been appended to a package in this run. */
  private var expressionAppended = false

  protected def run(using Context): Unit =
    spliced = false
    expressionAppended = false
    privateFieldsByThis = Map.empty
    privateMethodsByThis = Map.empty
    overloadedPrivateMethodsByThis = Map.empty
    liftedDefParamNames = Set.empty
    bodyThisReceiver = termName(EvalNames.ThisBinding)
    liftedScopeReceivers = Map.empty
    selfAliasReceivers = Map.empty
    bareLookupLayers = Nil
    // Lift marker-bearing methods out of classes and nested modules to avoid
    // creating wrapper copies with different class identity or module state.
    //
    // Dropping declarations is safe only when the original classes are reachable
    // through a REPL session import or the standalone runtime classpath.
    val hasReplSessionImport = sourceImportsReplSession(ctx.compilationUnit.untpdTree)
    val extractor = new ClassMethodExtractor
    val lifted =
      if hasReplSessionImport || config.standalone then extractor.transform(ctx.compilationUnit.untpdTree)
      else ctx.compilationUnit.untpdTree
    // Lifted methods receive explicit enclosing instances. Private and protected
    // members are not imported, so their accesses use reflection helpers.
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
    // Report a missing marker here instead of failing later while loading the
    // expression class.
    if !spliced then
      report.error(
        s"eval body marker `${EvalContext.placeholder}` not found in enclosing source",
        ctx.compilationUnit.untpdTree.srcPos
      )

  /** Redirects private and protected member access in a lifted body through
   *  reflection helpers:
   *
   *    - `__refl_get__(qual, name)` for private field reads
   *    - `__refl_set__(qual, name, rhs)` for private field writes
   *    - `__refl_call__(qual, name, args)` for private method calls
   *
   *  The qualifier identifies the captured enclosing instance. Bare names use
   *  the nearest dropped scope unless a body-local definition or parameter
   *  shadows them.
 *
   *  Curried calls, explicit type applications, and fully qualified nested paths
   *  are left for the typer to reject. Overloaded methods are diagnosed when the
   *  body refers to them because the parser-stage rewrite cannot preserve static
   *  overload selection.
   */
  private def rewritePrivateAccessInBody(body: Tree)(using Context): Tree =
    if privateFieldsByThis.isEmpty && privateMethodsByThis.isEmpty then body
    else
      val rewriter = new ShadowTrackingMap:
        /** Whether a bare term name is not shadowed by body or method bindings. */
        private def bareCandidate(name: Name): Boolean =
          name.isTermName
            && !shadowed.contains(name.asTermName)
            && !liftedDefParamNames.contains(name.asTermName)

        private def knownQual(qual: Name): Boolean =
          qual.isTermName && !shadowed.contains(qual.asTermName)

        override def transform(tree: Tree)(using Context): Tree = tree match
          // Qualified field write.
          case Assign(Select(Ident(qual), name), rhs)
              if name.isTermName && knownQual(qual)
                && privateFieldsByThis.get(qual.toTermName).exists(_.contains(name.toTermName)) =>
            mkReflSet(qual.toTermName, name.toTermName, transform(rhs)).withSpan(tree.span)

          // Bare field write, resolved from the innermost dropped scope.
          case Assign(Ident(name), rhs) if bareCandidate(name) =>
            lookupBareMember(name.toTermName) match
              case Some((receiver, _, false)) =>
                mkReflSet(receiver, name.toTermName, transform(rhs)).withSpan(tree.span)
              case _ => super.transform(tree)

          // Compound assignment `x op= rhs` to a private field. The typer
          // expands it against the reflective read, which is not assignable,
          // so expand it here: `__refl_set__(recv, "x", __refl_get__(..) op rhs)`.
          case InfixOp(Select(Ident(qual), name), op: Ident, rhs)
              if isAssignmentOperator(op.name) && name.isTermName && knownQual(qual)
                && privateFieldsByThis.get(qual.toTermName).exists(_.contains(name.toTermName)) =>
            val q = qual.toTermName
            val n = name.toTermName
            mkReflCompoundSet(q, n, privateFieldsByThis(q)(n), op, transform(rhs)).withSpan(tree.span)

          case InfixOp(Ident(name), op: Ident, rhs)
              if isAssignmentOperator(op.name) && bareCandidate(name) =>
            lookupBareMember(name.toTermName) match
              case Some((receiver, tpt, false)) =>
                mkReflCompoundSet(receiver, name.toTermName, tpt, op, transform(rhs)).withSpan(tree.span)
              case _ => super.transform(tree)

          // Qualified method call.
          case Apply(Select(Ident(qual), name), args)
              if name.isTermName && knownQual(qual)
                && privateMethodsByThis.get(qual.toTermName).exists(_.contains(name.toTermName)) =>
            val q = qual.toTermName
            val n = name.toTermName
            reportUnsupportedOverload(q, n, tree.srcPos)
            mkReflCall(q, n, args.map(transform), privateMethodsByThis(q)(n)).withSpan(tree.span)

          // Bare method call.
          case Apply(Ident(name), args) if bareCandidate(name) =>
            lookupBareMember(name.toTermName) match
              case Some((receiver, tpt, true)) =>
                reportUnsupportedOverload(receiver, name.toTermName, tree.srcPos)
                mkReflCall(receiver, name.toTermName, args.map(transform), tpt).withSpan(tree.span)
              case _ => super.transform(tree)

          // Qualified field read or parameterless method call.
          case Select(Ident(qual), name) if name.isTermName && knownQual(qual) =>
            val q = qual.toTermName
            val n = name.toTermName
            privateFieldsByThis.get(q).flatMap(_.get(n)) match
              case Some(tpt) => mkReflGet(q, n, tpt).withSpan(tree.span)
              case None =>
                privateMethodsByThis.get(q).flatMap(_.get(n)) match
                  case Some(tpt) =>
                    reportUnsupportedOverload(q, n, tree.srcPos)
                    mkReflCall(q, n, Nil, tpt).withSpan(tree.span)
                  case None => super.transform(tree)

          // Bare field read or parameterless method call.
          case id @ Ident(name) if bareCandidate(name) =>
            lookupBareMember(name.toTermName) match
              case Some((receiver, tpt, false)) =>
                mkReflGet(receiver, name.toTermName, tpt).withSpan(id.span)
              case Some((receiver, tpt, true)) =>
                reportUnsupportedOverload(receiver, name.toTermName, id.srcPos)
                mkReflCall(receiver, name.toTermName, Nil, tpt).withSpan(id.span)
              case None => id

          case _ => transformScoped(tree)
      rewriter.transform(body)

  /** Reports unsupported private or protected overloads only when referenced by
   *  the eval body, so unrelated declarations do not reject the call.
   */
  private def reportUnsupportedOverload(
      receiver: TermName,
      method: TermName,
      pos: SrcPos
  )(using Context): Unit =
    if diagnoseOverloads && overloadedPrivateMethodsByThis.get(receiver).exists(_.contains(method)) then
      report.error(
        s"dynamic eval does not support overloaded private/protected method `$method`; " +
          "the selected static signature cannot be preserved by the reflective lift",
        pos
      )

  /** Overload diagnostics apply to the body only. The enclosing code of a
   *  lifted def is type-checked but never runs, so its reflective calls need
   *  no faithful dispatch.
   */
  private var diagnoseOverloads: Boolean = true

  /** Resolves a bare name from inner to outer dropped scopes. Public declarations
   *  stop the search without a rewrite; inaccessible declarations return their
   *  receiver, type tree, and method flag.
   */
  private def lookupBareMember(name: TermName): Option[(TermName, Tree, Boolean)] =
    var layers = bareLookupLayers
    while layers.nonEmpty do
      val (receiver, scope) = layers.head
      scope.privateFields.get(name) match
        case Some(tpt) => return Some((receiver, tpt, false))
        case None =>
      scope.privateMethods.get(name) match
        case Some(tpt) => return Some((receiver, tpt, true))
        case None =>
      if scope.memberNames.contains(name) then return None
      layers = layers.tail
    None

  /** Tracks source-level bindings so name-based rewrites preserve shadowing.
   *  Block bindings shadow the entire block because an earlier reference is a
   *  forward-reference error, not a reference to the outer member.
   */
  private abstract class ShadowTrackingMap extends UntypedTreeMap:
    protected var shadowed: Set[TermName] = Set.empty

    protected final def withShadowed(names: Set[TermName])(op: => Tree): Tree =
      if names.isEmpty then op
      else
        val saved = shadowed
        shadowed ++= names
        try op finally shadowed = saved

    /** Handles scope-introducing trees for all body rewriters. */
    protected final def transformScoped(tree: Tree)(using Context): Tree = tree match
      case bk: Block =>
        withShadowed(blockBinders(bk.stats))(super.transform(bk))
      case fn: Function =>
        withShadowed(lambdaBinders(fn.args))(super.transform(fn))
      case dd: DefDef =>
        withShadowed(paramBinders(dd.paramss))(super.transform(dd))
      case cd: CaseDef =>
        withShadowed(patternBinders(cd.pat))(super.transform(cd))
      // Conservatively treat all for-comprehension binders as visible throughout
      // the tree. This may preserve a typer error but cannot select a wrong receiver.
      case fy: ForYield =>
        withShadowed(enumBinders(fy.enums))(super.transform(fy))
      case fd: ForDo =>
        withShadowed(enumBinders(fd.enums))(super.transform(fd))
      case td: TypeDef if td.isClassDef =>
        withShadowed(templateBinders(td.rhs))(super.transform(td))
      case mod: ModuleDef =>
        withShadowed(templateBinders(mod.impl))(super.transform(mod))
      case New(tmpl: Template) =>
        withShadowed(templateBinders(tmpl))(super.transform(tree))
      case _ => super.transform(tree)
  end ShadowTrackingMap

  private def blockBinders(stats: List[Tree]): Set[TermName] =
    stats.collect {
      case vd: ValDef => vd.name
      case dd: DefDef if dd.name != nme.CONSTRUCTOR => dd.name.asTermName
      case md: ModuleDef => md.name.toTermName
    }.toSet

  private def lambdaBinders(args: List[Tree]): Set[TermName] =
    args.collect {
      case vd: ValDef => vd.name
      case id: Ident if id.name.isTermName => id.name.asTermName
    }.toSet

  private def paramBinders(paramss: List[ParamClause]): Set[TermName] =
    paramss.flatMap(_.collect { case vd: ValDef => vd.name }).toSet

  /** Collects binders and variable identifiers from a pattern. */
  private def patternBinders(pat: Tree)(using Context): Set[TermName] =
    val acc = collection.mutable.Set.empty[TermName]
    val finder = new UntypedTreeTraverser:
      override def traverse(t: Tree)(using Context): Unit = t match
        case Bind(name, body) =>
          if name.isTermName then acc += name.asTermName
          traverse(body)
        case id: Ident if isVarPattern(id) && id.name != nme.WILDCARD =>
          acc += id.name.asTermName
        case _ => traverseChildren(t)
    finder.traverse(pat)
    acc.toSet

  private def enumBinders(enums: List[Tree])(using Context): Set[TermName] =
    enums.flatMap {
      case GenFrom(pat, _, _) => patternBinders(pat)
      case GenAlias(pat, _) => patternBinders(pat)
      case _ => Set.empty[TermName]
    }.toSet

  private def templateBinders(rhs: Tree)(using Context): Set[TermName] = rhs match
    case tmpl: Template => templateMemberNames(tmpl)
    case _ => Set.empty

  /** Builds a reflective field read and restores its declared type when known.
   *  The type tree is copied because the original class declaration is dropped.
   */
  private def mkReflGet(thisName: TermName, fieldName: TermName, tpt: Tree)(using Context): Tree =
    val nameLit = Literal(Constant(fieldName.toString))
    def untyped = Apply(Ident(termName("__refl_get__")), List(Ident(thisName), nameLit))
    tpt match
      case InferredFrom(rhs) =>
        // Pin the type through a never-invoked copy of the initializer:
        // `__refl_get_as__(recv, "x", () => <initializer>)`.
        val sample = initializerSample(fieldName, rhs)
        if sample.isEmpty then untyped
        else Apply(Ident(termName("__refl_get_as__")),
          List(Ident(thisName), nameLit, Function(Nil, sample).withSpan(sample.span)))
      case _ if tpt.isEmpty => untyped
      case _ =>
        TypeApply(Select(untyped, termName("asInstanceOf")), List(freshTypeTree(tpt)))

  /** Members whose initializer copies are being rewritten, to cut cycles
   *  between mutually dependent inferred members.
   */
  private var expandingInitializers: Set[TermName] = Set.empty

  /** A fresh copy of a member initializer usable as a type sample: the
   *  source text of `rhs` is re-parsed (so no tree is shared with the dropped
   *  declaration) and then given the body rewrites, since an initializer may
   *  itself read `this` or other private members. Empty when the text is
   *  unavailable, does not re-parse, or the member is already being expanded.
   */
  private def initializerSample(fieldName: TermName, rhs: Tree)(using Context): Tree =
    if expandingInitializers.contains(fieldName) then EmptyTree
    else
      val span = rhs.span
      val content = ctx.compilationUnit.source.content
      if !span.exists || span.start < 0 || span.end > content.length || span.start >= span.end then EmptyTree
      else
        val text = String.valueOf(content, span.start, span.end - span.start)
        val source = SourceFile.virtual("<eval-member-initializer>", text)
        val parseReporter = new dotc.reporting.StoreReporter(null)
        val parseCtx = ctx.fresh.setSource(source).setReporter(parseReporter)
        val parsed =
          try Parsers.Parser(source)(using parseCtx).expr(Parsers.Location.Elsewhere)
          catch case scala.util.control.NonFatal(_) => EmptyTree
        if parseReporter.hasErrors || parsed.isEmpty then EmptyTree
        else
          expandingInitializers += fieldName
          try rewritePrivateAccessInBody(rewriteThisInBody(parsed))
          finally expandingInitializers -= fieldName

  /** Copies type-tree shapes whose identity must not be shared with a dropped
   *  class declaration. Other untyped shapes are safe to reuse.
   */
  private def freshTypeTree(tpt: Tree)(using Context): Tree = tpt match
    case Ident(n) => Ident(n).withSpan(tpt.span)
    case AppliedTypeTree(tycon, args) =>
      AppliedTypeTree(freshTypeTree(tycon), args.map(freshTypeTree)).withSpan(tpt.span)
    case Select(qual, n) => Select(freshTypeTree(qual), n).withSpan(tpt.span)
    case _ => tpt

  /** Whether `op` is an assignment operator (`+=`, `::=`, ...): it ends in
   *  `=`, does not start with `=`, and is not a comparison.
   */
  private def isAssignmentOperator(op: Name): Boolean =
    val s = op.toString
    s.length > 1 && s.endsWith("=") && !s.startsWith("=")
      && s != "<=" && s != ">=" && s != "!="

  /** `__refl_set__(recv, "x", __refl_get__(recv, "x").asInstanceOf[T] op rhs)`
   *  for a compound assignment `x op= rhs`.
   */
  private def mkReflCompoundSet(
      receiver: TermName, fieldName: TermName, tpt: Tree, op: Ident, rhs: Tree
  )(using Context): Tree =
    val plainOp = Ident(termName(op.name.toString.dropRight(1))).withSpan(op.span)
    mkReflSet(receiver, fieldName,
      InfixOp(mkReflGet(receiver, fieldName, tpt), plainOp, rhs).withSpan(rhs.span))

  /** `__refl_set__(__this__, "name", rhs)` (returns Unit). */
  private def mkReflSet(thisName: TermName, fieldName: TermName, rhs: Tree)(using Context): Tree =
    Apply(
      Ident(termName("__refl_set__")),
      List(Ident(thisName), Literal(Constant(fieldName.toString)), rhs)
    )

  /** Builds a reflective method call, boxing arguments and restoring the
   *  declared return type when known.
   */
  private def mkReflCall(qual: TermName, methodName: TermName, args: List[Tree], retTpt: Tree)(using Context): Tree =
    val boxedArgs = args.map { a =>
      // Casting to Object boxes primitives for Method.invoke.
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
    if retTpt.isEmpty then rawCall
    else TypeApply(Select(rawCall, termName("asInstanceOf")), List(freshTypeTree(retTpt)))

  /** Injects typer-only reflection stubs into the wrapper. Extraction retargets
   *  all references to [[EvalExpressionBase]], so these bodies never run.
   */
  private def injectReflectionHelpers(tree: Tree)(using Context): Tree =
    val helpersSrc =
      """|object __EvalSafeReflHelpers__ {
         |  def __refl_get__(obj: Object, name: String): Any = ???
         |  def __refl_get_as__[T](obj: Object, name: String, sample: java.util.function.Supplier[T]): T = ???
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
    // Place helpers before the enclosing source so every splice site sees them.
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

  /** Walk the parsed body and redirect `this` references at the
   *  receivers the lift left behind. Used after
   *  [[ClassMethodExtractor]] lifts one or more methods out of an
   *  enclosing class or object: the body's original `this` no longer
   *  has the declaration as its enclosing `This` after the lift, so
   *  leaving it in would resolve to the wrapper module.
   *
   *    - plain `this` → the innermost receiver ([[bodyThisReceiver]]);
   *    - qualified `<Name>.this` of a dropped layer → that layer's
   *      receiver ([[liftedScopeReceivers]]);
   *    - a declared self alias → its layer's receiver
   *      ([[selfAliasReceivers]]).
   *
   *  Inside a type declared by the body, plain `this` remains local to that type,
   *  while qualified references and aliases to dropped scopes are still rewritten.
   */
  private def rewriteThisInBody(body: Tree)(using Context): Tree =
    val rewriter = new ShadowTrackingMap:
      /** Whether plain `this` belongs to a type declared by the eval body. */
      private var inBodyLocalClass = false

      private def suspendingThis(binders: Set[TermName], tree: Tree)(using Context): Tree =
        val saved = inBodyLocalClass
        inBodyLocalClass = true
        try withShadowed(binders)(super.transform(tree))
        finally inBodyLocalClass = saved

      override def transform(tree: Tree)(using Context): Tree = tree match
        case t @ This(qual) if qual.name.isEmpty =>
          // Class lifts use __this__; module lifts use the singleton name.
          if inBodyLocalClass then t
          else Ident(bodyThisReceiver).withSpan(t.span)
        case t @ This(qual) if liftedScopeReceivers.contains(qual.name.toTypeName) =>
          // Redirect qualified this to the receiver recorded for its dropped scope.
          Ident(liftedScopeReceivers(qual.name.toTypeName)).withSpan(t.span)
        case t: This =>
          // Other qualified this references retain their original owner.
          t
        // Redirect unshadowed self aliases of dropped scopes.
        case id @ Ident(name)
            if name.isTermName && !shadowed.contains(name.asTermName)
              && selfAliasReceivers.contains(name.toTermName) =>
          Ident(selfAliasReceivers(name.toTermName)).withSpan(id.span)
        // Suspend plain-this rewriting inside types declared by the body.
        case td: TypeDef if td.isClassDef =>
          suspendingThis(templateBinders(td.rhs), td)
        case mod: ModuleDef =>
          suspendingThis(templateBinders(mod.impl), mod)
        case New(tmpl: Template) =>
          suspendingThis(templateBinders(tmpl), tree)
        case _ => transformScoped(tree)
    rewriter.transform(body)

  private class Splicer(body: Tree, expressionClass: Seq[Tree]) extends UntypedTreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case pkg: PackageDef =>
          val transformed = super.transform(pkg).asInstanceOf[PackageDef]
          // Append the expression class to the first package containing a splice.
          // Keep this state separate from marker detection for the final check.
          if spliced && !expressionAppended then
            expressionAppended = true
            cpy.PackageDef(transformed)(
              transformed.pid,
              transformed.stats ++ expressionClass.map(_.withSpan(pkg.span))
            )
          else transformed

        // Copy preceding givens into the inserted body so they move with it to
        // evaluate. The original enclosing method is checked but never run, so
        // this does not evaluate an initializer twice.
        case bk @ Block(stats, expr) if isMarkerIdent(expr) || stats.exists(isMarkerIdent) =>
          val markerIdx =
            if isMarkerIdent(expr) then stats.length else stats.indexWhere(isMarkerIdent)
          // Given values are visible only after their declaration.
          val hoistedGivens: List[Tree] = stats.take(markerIdx).collect {
            case vd: ValDef if vd.mods.flags.is(Given) => vd
            case dd: DefDef if dd.mods.flags.is(Given) => dd
          }
          // A parameterless sibling def may be referenced as g() in generated
          // text; normalize it to g. Defs may be forward-referenced.
          val parenslessDefNames = stats.collect {
            case dd: DefDef if dd.name != nme.CONSTRUCTOR && !dd.name.isEmpty && dd.paramss.isEmpty =>
              dd.name
          }.toSet
          val effectiveBody =
            if parenslessDefNames.isEmpty then body
            else stripEmptyApplyFor(body, parenslessDefNames)
          val newStats = stats.map {
            case s if isMarkerIdent(s) => mkExprBlock(effectiveBody, s, hoistedGivens)
            case s => transform(s)
          }
          val newExpr =
            if isMarkerIdent(expr) then mkExprBlock(effectiveBody, expr, hoistedGivens)
            else transform(expr)
          cpy.Block(bk)(newStats, newExpr)

        // Replace a marker in any remaining expression position.
        case id: Ident if id.name.toString == EvalContext.placeholder =>
          mkExprBlock(body, id)

        case _ => super.transform(tree)

  /** Inaccessible members grouped by the receiver introduced for each dropped
   *  scope. Reset for every run.
   */
  private var privateFieldsByThis: Map[TermName, Map[TermName, Tree]] = Map.empty
  private var privateMethodsByThis: Map[TermName, Map[TermName, Tree]] = Map.empty
  private var overloadedPrivateMethodsByThis: Map[TermName, Set[TermName]] = Map.empty

  /** Receiver replacing plain `this` after a class or module lift.
   */
  private var bodyThisReceiver: TermName = termName(EvalNames.ThisBinding)

  /** Receiver for each dropped class or module scope. Inner scopes take
   *  precedence when source names collide.
   */
  private var liftedScopeReceivers: Map[TypeName, TermName] = Map.empty

  /** Receiver for each self alias declared by a dropped scope.
   */
  private var selfAliasReceivers: Map[TermName, TermName] = Map.empty

  /** Parameters that shadow inaccessible members after lifting.
   */
  private var liftedDefParamNames: Set[TermName] = Set.empty

  /** Dropped scopes in lexical lookup order with their runtime receivers.
   */
  private var bareLookupLayers: List[(TermName, OuterScope)] = Nil

  /** Lifts marker-bearing methods out of their enclosing classes and modules.
   *
   *  Class methods receive explicit enclosing-instance parameters; modules use
   *  their static paths. Dropping the copied declaration prevents duplicate JVM
   *  class identities. Non-marker siblings are unnecessary because only
   *  `evaluate` runs.
   */
  private class ClassMethodExtractor extends UntypedTreeMap:
    /** Whether the body requires receiver and inaccessible-member rewriting. */
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
      case td: TypeDef if td.isClassDef && containsMarker(td) && classLiftable(td) =>
        didLift = true
        liftClassMethods(td)
      case td: TypeDef if td.isClassDef && containsMarker(td) =>
        // Marker in a position the class lift can't hoist (a
        // template-level statement, a parent argument): keep the
        // re-elaborated class so the splice lands in place. The
        // typed path still threads `this` through the captured
        // owner chain; only duplicate class generation remains.
        List(td)
      case dd: DefDef if containsMarker(dd) =>
        // Top-level marker-bearing def: rename it so the wrapper's
        // copy doesn't shadow `import rs$line$N.{*}`'s already-
        // compiled version. Body code calling the def by its
        // original name resolves to the session-level one — which
        // is essential for recursive eval (the body re-enters the
        // *real* function, not the wrapper stub whose RHS moves into
        // `__Expression.evaluate` in ExtractEvalBody).
        List(renameTopLevelDef(dd))
      case mod: ModuleDef
          if containsMarker(mod) && !isWrapperModule(mod) && moduleLiftable(mod) =>
        // Marker inside a nested (non-top-level) `object`: drop the
        // declaration and lift, so the body links against the *live*
        // module on the classpath instead of a re-elaborated copy
        // with fresh state and duplicate nested classes.
        didLift = true
        liftModuleMethods(mod, outerScopes = Nil)
      case mod: ModuleDef if containsMarker(mod) && !isWrapperModule(mod) =>
        // Lift veto (private object, marker outside a def, ...):
        // keep the re-elaborated module so the splice lands in
        // place. Descending would rename the marker def, breaking
        // recursion and sibling references against the kept copy.
        List(mod)
      case mod: ModuleDef => List(transform(mod))
      case ext: ExtMethods if containsMarker(ext) =>
        // Marker inside a top-level extension method: rename the
        // marker-bearing member like a top-level def, so a recursive
        // call in the body resolves through the session import to
        // the live extension instead of the wrapper's emptied stub.
        List(ExtMethods(ext.paramss, ext.methods.map {
          case m: DefDef if containsMarker(m) => renameTopLevelDef(m)
          case m => m
        }).withSpan(ext.span))
      case other => List(other)
  end ClassMethodExtractor

  /** The synthesized `__EvalWrapper_<uuid>` module that hosts the
   *  spliced enclosing source. It always contains the marker but must
   *  never be dropped by the module lift.
   */
  private def isWrapperModule(mod: ModuleDef): Boolean =
    mod.name.toString.contains(ExtractEvalBody.WrapperMarker)

  /** True iff the module lift can handle every marker occurrence
   *  inside `mod`: the object is reachable by name at runtime (not
   *  `private`), and the marker sits inside a `def`, either directly
   *  or through nested object/class layers the lifts know how to
   *  hoist through. A marker anywhere else (an object-level `val`
   *  initializer, a parent clause, an extension method) keeps the
   *  re-elaboration path for the whole object. A `private` object's
   *  veto is compensated on the typed side: the rewriter captures
   *  the live module instance (`__this__`, `__evalModule_<M>__`)
   *  and ExtractEvalBody links the re-elaborated copy's members
   *  against it reflectively.
   */
  private def moduleLiftable(mod: ModuleDef)(using Context): Boolean =
    !mod.mods.flags.is(Private)
      && !mod.impl.parents.exists(containsMarker)
      && mod.impl.body.forall {
        case stat if !containsMarker(stat) => true
        case dd: DefDef => dd.name != nme.CONSTRUCTOR
        case nested: ModuleDef => moduleLiftable(nested)
        case td: TypeDef if td.isClassDef =>
          // A private nested class would make the lifted def's
          // `__this__` parameter type inaccessible from the wrapper.
          !td.mods.flags.is(Private) && classLiftable(td)
        case _ => false
      }

  /** True iff the class lift can hoist every marker occurrence inside
   *  `td`: markers must sit in a `def`, a `val`/`var` initializer
   *  (lifted as a def), or a nested class satisfying the same rule.
   *  A marker anywhere else (a template-level statement, a parent
   *  argument) vetoes the lift; the class is then kept so the splice
   *  lands in place instead of being dropped with the declaration.
   */
  private def classLiftable(td: TypeDef)(using Context): Boolean =
    td.rhs match
      case tmpl: Template =>
        !tmpl.parents.exists(containsMarker)
          && !tmpl.constr.paramss.exists(_.exists(containsMarker))
          && tmpl.body.forall {
            case stat if !containsMarker(stat) => true
            // A marker in a secondary constructor cannot be lifted:
            // the mandatory `this(...)` self-invocation has no
            // enclosing class after the hoist.
            case dd: DefDef => dd.name != nme.CONSTRUCTOR
            case _: ValDef => true
            case nested: TypeDef if nested.isClassDef =>
              // A private nested class would make the lifted def's
              // `__this__` parameter type inaccessible.
              !nested.mods.flags.is(Private) && classLiftable(nested)
            case _ => false
          }
      case _ => false

  /** Lift the marker-bearing method out of a *static* nested
   *  `object` (every enclosing layer up to the wrapper is also an
   *  object). The runtime module is reachable on the classpath (or
   *  through the REPL session imports), so instead of re-elaborating
   *  the object (which would create a second module class whose state
   *  is fresh and whose nested classes duplicate the originals), we:
   *
   *    - drop the object declaration entirely;
   *    - emit a sibling `import <Obj>.{given, *}` in its place, so
   *      the hoisted def's signature and body resolve members
   *      against the *live* module;
   *    - hoist the marker-bearing def, renamed like a top-level def
   *      so body references to its original name resolve to the
   *      live module's copy (recursion re-enters the real method);
   *    - record the object's private members keyed by its name, so
   *      the body rewriter reroutes them through the reflective
   *      helpers (the wildcard import does not expose privates).
   *
   *  `outerScopes` carries the enclosing module scopes
   *  innermost-first, mirroring [[liftClassMethodsRec]].
   */
  private def liftModuleMethods(mod: ModuleDef, outerScopes: List[OuterScope])(using Context): List[Tree] =
    // An anonymous given instance is still unnamed after parsing; the
    // desugarer invents its module name, so derive the same name here.
    val modName =
      if mod.name.isEmpty then desugar.inventGivenName(mod.impl).toTermName
      else mod.name.toTermName
    val tmpl = mod.impl
    val scope = OuterScope(
      modName.toTypeName, Ident(modName), Nil,
      templateMemberNames(tmpl), templatePrivateFields(tmpl), templatePrivateMethods(tmpl),
      templateOverloadedPrivateMethods(tmpl),
      isModule = true,
      imports = templateImports(tmpl), typeMemberNames = templateTypeMemberNames(tmpl))
    val newOuters = scope :: outerScopes
    // Keyed by the object's own name: that's the qualifier the body
    // rewriter sees for `M.x`, post-This-rewrite `this` references,
    // and (as the innermost receiver) bare member references.
    privateFieldsByThis = privateFieldsByThis.updated(modName, scope.privateFields)
    privateMethodsByThis = privateMethodsByThis.updated(modName, scope.privateMethods)
    overloadedPrivateMethodsByThis =
      overloadedPrivateMethodsByThis.updated(modName, scope.overloadedPrivateMethods)
    liftedScopeReceivers = liftedScopeReceivers.updated(modName.toTypeName, modName)
    templateSelfAlias(tmpl).foreach { alias =>
      selfAliasReceivers = selfAliasReceivers.updated(alias, modName)
    }
    val importStat: Tree = Import(
      Ident(modName),
      List(ImportSelector(Ident(nme.EMPTY)), ImportSelector(Ident(nme.WILDCARD)))
    ).withSpan(mod.span)
    val hoisted = tmpl.body.flatMap {
      case dd: DefDef if containsMarker(dd) =>
        bodyThisReceiver = modName
        bareLookupLayers = newOuters.map(s => (receiverName(s), s))
        liftedDefParamNames = dd.paramss.flatMap { ps =>
          ps.collect { case vd: ValDef => vd.name }
        }.toSet ++ liftedDefParamNames
        List(rewriteLiftedEnclosingCode(renameLiftedModuleDef(dd)))
      case nested: ModuleDef if containsMarker(nested) =>
        liftModuleMethods(nested, newOuters)
      case td: TypeDef if td.isClassDef && containsMarker(td) =>
        liftClassMethodsRec(td, newOuters)
      case _ => Nil
    }
    // The dropped object's own imports follow its member import, so
    // the hoisted defs see the same names the original body did.
    importStat :: scope.imports ::: hoisted

  /** [[renameTopLevelDef]] for a def hoisted out of a static module,
   *  additionally stripping `override` (the parent the def overrode
   *  is gone after the lift).
   */
  private def renameLiftedModuleDef(dd: DefDef)(using Context): DefDef =
    val renamed = renameTopLevelDef(dd)
    renamed.withMods(renamed.mods.withFlags(renamed.mods.flags &~ Override))

  /** Rename a top-level marker-bearing DefDef so the original name
   *  resolves through the wrapper's auto-import to the REPL session
   *  version. The renamed copy still carries the marker, so the
   *  splice still has a place to land. The `Given` flag is stripped:
   *  a renamed given would still win implicit search over the
   *  session's live copy (a local given beats an imported one), and
   *  its rhs is the emptied stub, so a body `summon` would observe
   *  the stub instead of the live instance.
   */
  private def renameTopLevelDef(dd: DefDef)(using Context): DefDef =
    val newName = termName(s"__eval_${dd.name}__")
    val newMods = dd.mods.withFlags(dd.mods.flags &~ Given)
    DefDef(newName, dd.paramss, dd.tpt, dd.rhs).withMods(newMods).withSpan(dd.span)

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

  /** True iff `t` is exactly the marker identifier. */
  private def isMarkerIdent(t: Tree): Boolean = t match
    case id: Ident => id.name.toString == EvalContext.placeholder
    case _ => false

  /** True iff the tree transitively contains an `Ident(<marker>)`. */
  private def containsMarker(tree: Tree)(using Context): Boolean =
    var found = false
    val finder = new UntypedTreeTraverser:
      override def traverse(t: Tree)(using Context): Unit =
        if found then ()
        else t match
          case id: Ident if id.name.toString == EvalContext.placeholder =>
            found = true
          case _ =>
            traverseChildren(t)
    finder.traverse(tree)
    found

  /** Lift each marker-bearing DefDef out of the given ClassDef.
   *  Returns the lifted defs, with the original class declaration
   *  dropped. The class is reachable via the wrapper's
   *  `import rs$line$N.{*}`; dropping the local declaration prevents a
   *  duplicate runtime class.
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
   *  [[EvalRewriteTyped]] populates in the bindings array.
   */
  private def liftClassMethodsRec(td: TypeDef, outerScopes: List[OuterScope])(using Context): List[Tree] =
    val tmpl = td.rhs.asInstanceOf[Template]
    val tparams = templateTypeParams(tmpl)
    // For nested classes, the type-ref of the inner `__this__`
    // parameter needs a path-dependent prefix on the outer
    // `__this__<OuterName>` parameter — `def m(__this__A: A,
    // __this__: __this__A.B)` — because `A.B` (bare static path)
    // doesn't resolve when A is a class, only when A is an object.
    // The prefix comes from the immediate outer scope: for a class
    // it's its `__this__<Name>` parameter, for a static module it's
    // the object's own name (`Select(Ident(M), C)` resolves through
    // the sibling `import` the module lift emits).
    val outerReceiverNames = outerScopes.map(receiverName)
    val typeRef = applyClassTypeParams(td, tparams, outerReceiverNames)
    val scope = OuterScope(td.name, typeRef, tparams,
      templateMemberNames(tmpl), templatePrivateFields(tmpl), templatePrivateMethods(tmpl),
      templateOverloadedPrivateMethods(tmpl),
      selfAlias = templateSelfAlias(tmpl),
      imports = templateImports(tmpl), typeMemberNames = templateTypeMemberNames(tmpl),
      companionImport = enumCompanionImport(td, outerReceiverNames))
    val newOuters = scope :: outerScopes
    // Record per-qualifier private fields/methods for the body
    // rewriter. The innermost class is reached via `__this__`; each
    // outer enclosing class via `__this__<OuterName>`. These are the
    // qualifiers `rewriteThisInBody` produces for
    // `OuterName.this`-style references. A declared self alias
    // (`self =>`) maps to the same receiver, so a body naming the
    // alias resolves after the class declaration is dropped.
    def recordScopes(): Unit =
      val innermostThis = termName(EvalNames.ThisBinding)
      bareLookupLayers = (innermostThis, scope) :: outerScopes.map(s => (receiverName(s), s))
      privateFieldsByThis = privateFieldsByThis.updated(innermostThis, scope.privateFields)
      privateMethodsByThis = privateMethodsByThis.updated(innermostThis, scope.privateMethods)
      overloadedPrivateMethodsByThis =
        overloadedPrivateMethodsByThis.updated(innermostThis, scope.overloadedPrivateMethods)
      templateSelfAlias(tmpl).foreach { alias =>
        selfAliasReceivers = selfAliasReceivers.updated(alias, innermostThis)
      }
      // Outer layers first, the innermost last, so an inner scope
      // wins a name collision (matching lexical shadowing).
      outerScopes.reverse.foreach { s =>
        val q = receiverName(s)
        privateFieldsByThis = privateFieldsByThis.updated(q, s.privateFields)
        privateMethodsByThis = privateMethodsByThis.updated(q, s.privateMethods)
        overloadedPrivateMethodsByThis =
          overloadedPrivateMethodsByThis.updated(q, s.overloadedPrivateMethods)
        liftedScopeReceivers = liftedScopeReceivers.updated(s.name, q)
        s.selfAlias.foreach { alias =>
          selfAliasReceivers = selfAliasReceivers.updated(alias, q)
        }
      }
      liftedScopeReceivers = liftedScopeReceivers.updated(td.name.toTypeName, innermostThis)
    tmpl.body.flatMap {
      case dd: DefDef if containsMarker(dd) =>
        recordScopes()
        // Record the lifted def's value-param names so the body
        // rewriter can skip bare-Ident rewrites that would otherwise
        // shadow a method param sharing a name with a private
        // class member.
        liftedDefParamNames = dd.paramss.flatMap { ps =>
          ps.collect { case vd: ValDef => vd.name }
        }.toSet ++ liftedDefParamNames
        List(liftDef(dd, newOuters))
      case vd: ValDef if containsMarker(vd) =>
        // Marker in a member val/var *initializer* (the eval runs
        // during construction). Lift the initializer as a def: the
        // hoisted host only exists to carry the marker into the
        // wrapper, so the val-ness (laziness, mutability) of the
        // original member is irrelevant here.
        recordScopes()
        val asDef = DefDef(vd.name.toTermName, Nil, vd.tpt, vd.rhs)
          .withMods(Modifiers()).withSpan(vd.span)
        List(liftDef(asDef, newOuters))
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
   *  redirect references to them through the synthesized reflective
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
      privateMethods: Map[TermName, Tree],
      overloadedPrivateMethods: Set[TermName],
      isModule: Boolean = false,
      selfAlias: Option[TermName] = None,
      imports: List[Tree] = Nil,
      typeMemberNames: Set[TypeName] = Set.empty,
      /** For an enum, the companion import that makes its cases visible. */
      companionImport: Option[Tree] = None
  )

  /** An enum body sees its cases unqualified through an import of the
   *  companion that desugaring adds to the template. The lift re-creates it
   *  as a wildcard import of the companion, reached the same way as the
   *  class itself: by name at the top level, or through the immediate
   *  outer receiver when nested.
   */
  private def enumCompanionImport(td: TypeDef, outerReceiverNames: List[TermName])(using Context): Option[Tree] =
    if !td.mods.is(Enum) then None
    else
      val companion: Tree = outerReceiverNames match
        case Nil => Ident(td.name.toTermName)
        case head :: _ => Select(Ident(head), td.name.toTermName)
      Some(Import(companion, List(ImportSelector(Ident(nme.WILDCARD)))).withSpan(td.span))

  /** Applies the body rewrites to the enclosing code of a lifted def. After
   *  the lift, the code around the marker no longer sits inside its class or
   *  module: a plain `this` would resolve to the wrapper module, and a
   *  private member is not visible through the wildcard import of the
   *  receiver. The marker identifier is not a member, so it passes through
   *  untouched and the separately rewritten body is spliced into it later.
   */
  private def rewriteLiftedEnclosingCode(dd: DefDef)(using Context): DefDef =
    // Rewrite the right-hand sides rather than the whole def so the lifted
    // receiver parameters are not treated as shadowing binders; the def's own
    // value parameters are already excluded through `liftedDefParamNames`.
    def rewrite(t: Tree): Tree = rewritePrivateAccessInBody(rewriteThisInBody(t))
    diagnoseOverloads = false
    try
      val paramss = dd.paramss.map(_.map {
        case vd: ValDef if !vd.rhs.isEmpty => cpy.ValDef(vd)(rhs = rewrite(vd.rhs))
        case other => other
      })
      cpy.DefDef(dd)(paramss = paramss.asInstanceOf[List[ParamClause]], rhs = rewrite(dd.rhs))
    finally diagnoseOverloads = true

  /** Imports declared in a template's body. A lift drops the
   *  declaration, so any import the original method body relied on
   *  must be re-emitted next to the lifted def or its bare
   *  references stop resolving.
   */
  private def templateImports(tmpl: Template)(using Context): List[Tree] =
    tmpl.body.collect { case imp: Import => imp }

  /** Type-member names of a template (aliases, abstract types, inner
   *  classes). A lifted def's signature referencing one bare must be
   *  re-qualified through the layer's receiver: the signature sits
   *  outside the body's `import __this__.*`.
   */
  private def templateTypeMemberNames(tmpl: Template)(using Context): Set[TypeName] =
    tmpl.body.collect { case td: TypeDef => td.name.asTypeName }.toSet

  /** The declared self-alias name of a template (`self =>`), if any. */
  private def templateSelfAlias(tmpl: Template): Option[TermName] =
    val self = tmpl.self
    if self.isEmpty || self.name.isEmpty || self.name == nme.WILDCARD then None
    else Some(self.name.toTermName)

  /** The term the lifted code uses to reach an enclosing scope's
   *  members: the `__this__<Name>` parameter for a class scope, the
   *  object's own (statically reachable) name for a module scope.
   */
  private def receiverName(s: OuterScope): TermName =
    if s.isModule then s.name.toTermName else termName(EvalNames.thisBinding(s.name))

  /** Map of private term-member names → declared type tree. Used by
   *  the body rewriter to route bare or `__this__.x` references to
   *  reflective accessors when `x` is private. Includes both ctor
   *  val/var params and body val/var declarations.
   */
  private def templatePrivateFields(tmpl: Template)(using Context): Map[TermName, Tree] =
    // `Protected` members need the same reflective reroute as
    // `Private` ones: the wildcard `import __this__.*` exposes
    // neither, and the typer rejects a direct `__this__.x` Select
    // for both.
    val builder = collection.mutable.LinkedHashMap.empty[TermName, Tree]
    tmpl.constr.paramss.foreach { ps =>
      ps.foreach {
        case vd: ValDef if vd.mods.flags.isOneOf(Private | Protected) && !vd.name.isEmpty =>
          builder.put(vd.name, memberTpt(vd.tpt, vd.rhs))
        case _ =>
      }
    }
    tmpl.body.foreach {
      case vd: ValDef if vd.mods.flags.isOneOf(Private | Protected) && !vd.name.isEmpty =>
        builder.put(vd.name, memberTpt(vd.tpt, vd.rhs))
      case _ =>
    }
    builder.toMap

  /** Marks a member type that must be inferred from the initializer `rhs`.
   *  Represented as a `Typed` node with an empty type, a shape that never
   *  occurs as a real type tree.
   */
  private object InferredFrom:
    def apply(rhs: Tree)(using Context): Tree = Typed(rhs, EmptyTree)
    def unapply(tree: Tree): Option[Tree] = tree match
      case Typed(rhs, tpt) if tpt.isEmpty => Some(rhs)
      case _ => None

  /** Declared type of a member, or a literal-derived one when the
   *  declaration relies on inference (`private var count = 1`): the
   *  reflective rewrite needs *some* static type for its cast, and a
   *  literal initializer pins it. Any other inferred field initializer is
   *  returned as [[InferredFrom]], which [[mkReflGet]] turns into a typed
   *  read through a copy of the initializer. Returns `EmptyTree` when
   *  nothing is available; the reference then types as `Any`.
   */
  private def memberTpt(declared: Tree, rhs: Tree)(using Context): Tree =
    val fromLiteral = literalTpt(rhs)
    if !declared.isEmpty then declared
    else if !fromLiteral.isEmpty then fromLiteral
    else if rhs.isEmpty then EmptyTree
    else InferredFrom(rhs)

  private def literalTpt(rhs: Tree)(using Context): Tree =
    rhs match
      case Number(_, kind) =>
        kind match
          case NumberKind.Whole(_) => Ident(typeName("Int"))
          case _ => Ident(typeName("Double"))
      case Literal(c) =>
        val name = c.tag match
          case IntTag => "Int"
          case LongTag => "Long"
          case FloatTag => "Float"
          case DoubleTag => "Double"
          case BooleanTag => "Boolean"
          case CharTag => "Char"
          case ByteTag => "Byte"
          case ShortTag => "Short"
          case StringTag => "String"
          case _ => ""
        if name.isEmpty then EmptyTree else Ident(typeName(name))
      case _ => EmptyTree

  /** Map of private method names → declared return type tree.
   *  Mirrors [[templatePrivateFields]] for `def`s, so the body rewriter
   *  can route private method calls through the synthesized
   *  `__refl_call__` helper.
   */
  private def templatePrivateMethods(tmpl: Template)(using Context): Map[TermName, Tree] =
    val builder = collection.mutable.LinkedHashMap.empty[TermName, Tree]
    tmpl.body.foreach {
      case dd: DefDef
          if dd.mods.flags.isOneOf(Private | Protected) && !dd.name.isEmpty
            && dd.name != nme.CONSTRUCTOR =>
        builder.put(dd.name.asTermName, memberTpt(dd.tpt, dd.rhs))
      case _ =>
    }
    builder.toMap

  /** Private/protected names that participate in an overload set. The count
   *  includes public members of the same name because they are candidates in
   *  the original static overload resolution too.
   */
  private def templateOverloadedPrivateMethods(tmpl: Template)(using Context): Set[TermName] =
    val methodCounts = tmpl.body.collect {
      case dd: DefDef if dd.name != nme.CONSTRUCTOR && !dd.name.isEmpty => dd.name.asTermName
    }.groupMapReduce(identity)(_ => 1)(_ + _)
    tmpl.body.collect {
      case dd: DefDef
          if dd.mods.flags.isOneOf(Private | Protected)
            && dd.name != nme.CONSTRUCTOR
            && !dd.name.isEmpty
            && methodCounts.getOrElse(dd.name.asTermName, 0) > 1 =>
        dd.name.asTermName
    }.toSet

  /** Term-member names of a template (ctor val/var params and body
   *  val/var/def names). Drives the lifted def's `import __this__.*`:
   *  only method params that shadow a class member need hiding, and
   *  hiding non-shadowing params confuses the resolver into routing
   *  the bare reference back through the import.
   */
  private def templateMemberNames(tmpl: Template)(using Context): Set[TermName] =
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
  private def templateTypeParams(tmpl: Template)(using Context): List[TypeDef] =
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
   *  `outerReceiverNames` lists the receiver names innermost-first
   *  (`__this__<Outer>` parameters for class scopes, the object's
   *  own name for static module scopes); the *immediate* outer
   *  scope's receiver is the qualifier we use.
   */
  private def applyClassTypeParams(
      td: TypeDef,
      tparams: List[TypeDef],
      outerReceiverNames: List[TermName]
  )(using Context): Tree =
    val baseName = td.name
    val base: Tree = outerReceiverNames match
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
   *  matching the bindings [[EvalRewriteTyped]] populates.
   *  Class type params from each scope are carried over as type
   *  params of the lifted def. The body is wrapped in a Block
   *  starting with `import __this__.*` (member names that shadow
   *  method params are hidden).
   */
  private def liftDef(dd: DefDef, outerScopes: List[OuterScope])(using Context): Tree =
    val innermost = outerScopes.head
    val span = dd.span
    val thisName = termName(EvalNames.ThisBinding)
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
    // The member import goes into the innermost block, and each dropped
    // layer's template imports (plus an enum's companion import) into a
    // block of their own around it, outermost layer outermost. Members
    // therefore shadow every import, and an inner layer's imports shadow
    // an outer layer's, as in the original nesting. Two wildcard imports
    // in one block that both provide a name would be an ambiguity error.
    val memberRhs = dd.rhs match
      case EmptyTree => dd.rhs
      case Block(stats, expr) => Block(importStat :: stats, expr).withSpan(dd.rhs.span)
      case other => Block(importStat :: Nil, other).withSpan(dd.rhs.span)
    val newRhs = outerScopes.foldLeft(memberRhs) { (acc, scope) =>
      val prefix = scope.companionImport.toList ::: scope.imports
      if prefix.isEmpty || acc.isEmpty then acc
      else Block(prefix, acc).withSpan(dd.rhs.span)
    }
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
    // see their prefix already in scope. Names match the bindings
    // `EvalRewriteTyped` emits: `__this__<OuterName>` for outer
    // classes, plain `__this__` for the immediate innermost. Static
    // module scopes contribute no parameter: the object is reachable
    // by name through the sibling import the module lift emits.
    val outerParams = outerScopes.tail.reverse.filterNot(_.isModule).map { scope =>
      val name = termName(EvalNames.thisBinding(scope.name))
      ValDef(name, scope.typeRef, EmptyTree)
        .withMods(Modifiers(Param)).withSpan(span)
    }
    val innermostParam = ValDef(thisName, innermost.typeRef, EmptyTree)
      .withMods(Modifiers(Param)).withSpan(span)
    // Rewrite parameter defaults to qualify class-member references
    // through `__this__`. Param defaults sit *outside* the body's
    // `import __this__.*`, so a bare `base` reference would no
    // longer resolve after the lift drops the enclosing class.
    // The lifted signature sits outside the body's `import __this__.*`,
    // so a return or parameter type naming a dropped layer's type
    // member (`def m: T` for a class-level `type T`) must be
    // re-qualified through the layer's receiver. The def's own type
    // params (and the carried class type params, which stay bare by
    // construction) shadow same-named members.
    val typeQualByMember: Map[TypeName, TermName] =
      outerScopes.reverse.foldLeft(Map.empty[TypeName, TermName]) { (acc, scope) =>
        val qual =
          if scope eq innermost then thisName
          else receiverName(scope)
        scope.typeMemberNames.foldLeft(acc) { (m, n) => m.updated(n, qual) }
      } -- (carriedTparams ++ existingTparams).map(_.name.asTypeName)
    val rewrittenValueClauses: List[ParamClause] =
      // Walk outer-most-first so the innermost class's mapping wins
      // when a member name appears in multiple enclosing classes.
      val outerThisByMember: Map[TermName, TermName] =
        outerScopes.reverse.foldLeft(Map.empty[TermName, TermName]) { (acc, scope) =>
          val qual =
            if scope eq innermost then thisName
            else receiverName(scope)
          scope.memberNames.foldLeft(acc) { (m, n) => m.updated(n, qual) }
        }
      val seenParams = collection.mutable.Set.empty[TermName]
      valueClauses.map { clause =>
        val rewritten: List[ValDef] = clause.collect {
          case vd: ValDef =>
            val r = qualifyMembersInDefault(vd.rhs, outerThisByMember -- seenParams)
            seenParams += vd.name
            cpy.ValDef(vd)(tpt = qualifyTypeMembers(vd.tpt, typeQualByMember), rhs = r)
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
    // Rename like [[renameTopLevelDef]]: with the original name kept,
    // a body reference to it would be ambiguous between the hoisted
    // def and the live member coming in through `import __this__.*`
    // (an inner wildcard import cannot override an outer definition).
    // After the rename, recursion in the body resolves through the
    // import and re-enters the live method. `override` is stripped:
    // the parent the def overrode is gone after the lift.
    val newName = termName(s"__eval_${dd.name}__")
    val newMods = dd.mods.withFlags(dd.mods.flags &~ Override)
    val lifted = DefDef(newName, newParamss, qualifyTypeMembers(dd.tpt, typeQualByMember), newRhs)
      .withMods(newMods).withSpan(span)
    rewriteLiftedEnclosingCode(lifted)

  /** Rewrite bare `Ident(T)` type references in a lifted signature
   *  tree, where `T` is a type member of a dropped layer, into the
   *  path-dependent `<receiver>.T`.
   */
  private def qualifyTypeMembers(
      tpt: Tree, typeQual: Map[TypeName, TermName]
  )(using Context): Tree =
    if tpt.isEmpty || typeQual.isEmpty then tpt
    else
      val rewriter = new UntypedTreeMap:
        override def transform(t: Tree)(using Context): Tree = t match
          case id @ Ident(name) if name.isTypeName && typeQual.contains(name.asTypeName) =>
            Select(Ident(typeQual(name.asTypeName)).withSpan(id.span), name).withSpan(id.span)
          case _ => super.transform(t)
      rewriter.transform(tpt)

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
            Ident(termName(EvalNames.ThisBinding)).withSpan(t.span)
          case t @ This(qual) if liftedScopeReceivers.contains(qual.name.toTypeName) =>
            // Module layers map to the object's own name, class
            // layers to their `__this__<Name>` parameter.
            Ident(liftedScopeReceivers(qual.name.toTypeName)).withSpan(t.span)
          case t @ This(qual) =>
            Ident(termName(EvalNames.thisBinding(qual.name))).withSpan(t.span)
          case id @ Ident(name) =>
            memberToQualifier.get(name.toTermName) match
              case Some(q) =>
                Select(Ident(q).withSpan(id.span), name).withSpan(id.span)
              case None => id
          case _ => super.transform(t)
      rewriter.transform(tree)

  /** Parse `config.body` as a block expression. Spans on the result
   *  are relative to `config.body`. Inner `eval` and `@evalLike` calls
   *  inside the body have their `enclosingSource` argument filled in
   *  by [[EvalRewriteTyped]] (which receives `config` so it can
   *  compose the chained slice when needed). No rewrite happens here.
   */
  private def parseBody(using Context): Tree =
    val source = SourceFile.virtual("<eval-body>", config.body)
    val newCtx = ctx.fresh.setSource(source)
    val parser = Parsers.Parser(source)(using newCtx)
    parser.block()

  /** Parse the synthesized __Expression class declaration. Returns the
   *  list of top-level stats from the synthetic source, suitable for
   *  appending to a PackageDef's stats.
   */
  private def parseExpressionClass(using Context): Seq[Tree] =
    val source = SourceFile.virtual("<eval-expression-class>", expressionClassSource)
    val newCtx = ctx.fresh.setSource(source)
    val parser = Parsers.Parser(source)(using newCtx)
    parser.parse().asInstanceOf[PackageDef].stats

  /** Source for the synthesized `__Expression` class.
   *
   *  The class is a thin subclass of [[EvalExpressionBase]] — all
   *  reflection helpers (`getValue`, `getField`, `setField`,
   *  `callMethod`, `getOuter`, `reflectEval`, …) live on the
   *  pre-compiled `@caps.assumeSafe` base, so a safe-mode REPL
   *  session can extend it without re-checking the helpers'
   *  `@rejectSafe` reflection calls. The synthesized subclass only
   *  carries `evaluate()`, which [[ExtractEvalBody]] later fills in
   *  with the typed user body.
   */
  private def expressionClassSource: String =
    // `_root_`-rooted: the class is appended after the wrapper's
    // session imports, so an imported user binding named `dotty`
    // would otherwise capture the path head.
    s"""class ${config.outputClassName}(thisObject: Object | Null, bindings: Array[_root_.dotty.tools.eval.Eval.Binding])
       |  extends _root_.dotty.tools.eval.EvalExpressionBase(thisObject, bindings) {
       |  def evaluate(): Any = ()
       |}
       |""".stripMargin

  /** Build the splice block for the marker site:
   *
   *  ```
   *  {
   *    val __evalResult = { <body> }
   *    Eval.__noFold__()
   *    __evalResult
   *  }
   *  ```
   *
   *  The val carries the body's value, captured for [[ExtractEvalBody]]
   *  to drain into `__Expression.evaluate` later. The `__noFold__()`
   *  effect prevents constant-folding of the surrounding expression
   *  during firstTransform, mirroring the trick from the debug
   *  pipeline's `InsertExpression`.
   *
   *  The block ends in `__evalResult` (a back-reference to the val)
   *  so the spliced position takes on the body's type — the marker
   *  could have sat in any position, and the typer needs the splice's
   *  type to match the surrounding expectation. With `Literal(())` as
   *  the tail the splice would always be `Unit`, breaking any
   *  enclosing `def f(): Int = ({ <marker> })`.
   */
  private def mkExprBlock(body: Tree, markerTree: Tree, hoistedGivens: List[Tree] = Nil)(using Context): Tree =
    val span = markerTree.span
    if spliced then
      warnOrError(s"eval body marker `${EvalContext.placeholder}` appears more than once", markerTree.srcPos)
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
      val resultName = UniqueName.fresh(EvalResultName)
      val valDef = ValDef(resultName, valTpt, effectiveBody)
        .withSpan(span)
        .withAttachment(EvalResultTree, ())
      // `Eval.__noFold__()`, a no-op on the `@caps.assumeSafe` `Eval` module:
      // an opaque side-effecting call that prevents the surrounding expression
      // from being constant-folded before `ExtractEvalBody` drains the spliced
      // `val __evalResult`'s rhs.
      val effect = Apply(
        SpliceEvalBody.selectFqn("dotty.tools.eval.Eval.__noFold__", span),
        Nil
      ).withSpan(span)
      val tail = Ident(resultName).withSpan(span)
      Block(List(valDef, effect), tail).withSpan(span)

  /** Parse a type-source string into an untyped Tree. Wraps the
   *  string in a synthetic `val __t__ : <typeStr> = ???` source so
   *  the standard parser's type-position machinery picks it up;
   *  the wrapper's tpt is then extracted. Falls back to an empty
   *  `TypeTree()` (an unascribed `__evalResult`) if the string can't
   *  be parsed. The parse runs against a private reporter: the
   *  rendered expected type is best-effort (compiler printers can
   *  emit non-source syntax), and its syntax errors must not fail
   *  the wrapper compile.
   */
  private def parseTypeFromString(typeStr: String, span: Span)(using Context): Tree =
    val source = SourceFile.virtual("<eval-expected-type>", s"val __t__ : $typeStr = ???\n")
    val parseReporter = new dotc.reporting.StoreReporter(null)
    val newCtx = ctx.fresh.setSource(source).setReporter(parseReporter)
    val parser = Parsers.Parser(source)(using newCtx)
    val parsed =
      try parser.parse()
      catch case scala.util.control.NonFatal(_) => null
    if parseReporter.hasErrors then TypeTree().withSpan(span)
    else parsed match
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

  /** Build an untyped `Select` chain for a dotted FQN, rooted at
   *  `_root_` (e.g. `"dotty.tools.eval.Eval"` becomes
   *  `Select(Select(Select(Select(Ident(_root_), dotty), tools), eval), Eval)`).
   *  Rooting matters: the chain is spliced where the wrapper's
   *  session imports are in scope, and a user binding named `dotty`
   *  would otherwise capture the head of the path.
   */
  private[eval] def selectFqn(fqn: String, span: Span)(using Context): Tree =
    val parts = fqn.split('.').toList
    parts.foldLeft[Tree](Ident(nme.ROOTPKG).withSpan(span)) { (acc, part) =>
      Select(acc, part.toTermName).withSpan(span)
    }

  /** Name of the val we splice the body's value into. This is only a
   *  readable implementation name: [[ExtractEvalBody]] identifies the
   *  generated val by [[EvalResultTree]], so a user val with the same
   *  name cannot be mistaken for the splice anchor.
   */
  val EvalResultName: TermName = termName("__evalResult")

  /** Marks the generated eval-result val across parser and typer tree
   *  copies. A sticky attachment is needed because extraction runs on
   *  the typed tree, several phases after the untyped splice.
   */
  val EvalResultTree: Property.StickyKey[Unit] = Property.StickyKey()
