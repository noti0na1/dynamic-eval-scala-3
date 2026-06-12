package dotty.tools
package eval

import dotc.Compiler
import dotc.cc.CheckCaptures
import dotc.core.Phases.Phase

/** Compiler subclass that drives the eval pipeline. On top of the
 *  standard frontend / transform chain it inserts:
 *
 *    - [[SpliceEvalBody]] (after parser): parses the eval body string
 *      and splices it into `enclosingSource` at the marker, then
 *      appends the synthesised `__Expression` class to the package.
 *    - [[EvalRewriteTyped]] (after PostTyper): fills the bindings
 *      array, the expected-type sentinel, and (for `evalSafe` /
 *      `agentSafe`) wraps the verify-marker in
 *      `Eval.handleCompileError(...)` so the inner verification's
 *      spliced body lifts from `T` to `EvalResult[T]`. Symbol-checks
 *      `eval` / `evalSafe` against the `Eval` module owner.
 *    - [[ExtractEvalBody]] (after `cc`): moves the typed body into
 *      `__Expression.evaluate`'s rhs and rewrites outer-scope
 *      references into `reflectEval` placeholders carrying
 *      [[ReflectEvalStrategy]] attachments. Running *after* the
 *      capture-checking phase lets `cc` see the body in its
 *      original lexical context (with the `enclosingSource`'s
 *      `^` annotations on def parameters intact) and reject
 *      capture violations the post-extract reflective shape
 *      can no longer express.
 *    - [[ResolveEvalAccess]] (post-erasure): lowers each `reflectEval`
 *      placeholder into a reflective accessor call on `__Expression`
 *      (`getValue`, `getField`, `callMethod`, ...).
 *    - [[LogExecutedTree]] (last, conditional): writes the post-resolve
 *      tree to `-Xrepl-eval-log-dir` for inspection.
 */
class EvalCompiler(config: EvalCompilerConfig) extends Compiler:

  override protected def frontendPhases: List[List[Phase]] =
    val parser :: others = super.frontendPhases: @unchecked
    // Replace the base pipeline's (flag-gated) EvalRewriteTyped with
    // the config-carrying instance *in place* (right after PostTyper,
    // before UnrollDefinitions and the pickler/Inlining groups). Both
    // compiles then run the rewriter at the same pipeline point: the
    // design relies on the inner and outer elaborations of the same
    // source agreeing (e.g. linked-class constructor indices), and at
    // that point typed symbols are resolved (so the eval / evalSafe
    // call's owner can be verified against `Eval.moduleClass`) while
    // inline / macro expansion hasn't yet synthesised
    // compiler-introduced variables we'd accidentally capture as
    // bindings.
    parser :: List(SpliceEvalBody(config)) :: others.map(_.map {
      case p if p.phaseName == EvalRewriteTyped.name => new EvalRewriteTyped(Some(config))
      case p => p
    })

  override protected def transformPhases: List[List[Phase]] =
    val store = EvalStore()
    val transformPhases = super.transformPhases
    // Anchor [[ExtractEvalBody]] right after the capture-checking
    // group so `cc` sees the body in its original lexical context.
    // The group is always *present* in the plan (whether it runs is
    // decided per run, in `Phases.fusePhases`, by `isEnabled`), so
    // the anchor always resolves.
    val ccIndex = transformPhases.indexWhere(_.exists(_.phaseName == CheckCaptures.name))
    assert(ccIndex >= 0, s"phase ${CheckCaptures.name} not found in transform plan")
    val (before, after) = transformPhases.splitAt(ccIndex + 1)
    // ResolveEvalAccess sits at the very end of the transform chain,
    // *after* the Constructors group, despite the warning in
    // `Compiler.transformPhases` that no InfoTransformer should
    // follow it (Constructors changes class decls in
    // transformTemplate). That is sound here because its
    // `infoMayChange` is restricted to non-class symbols owned by
    // `__Expression` or by body-local classes when linked classes
    // exist, so the decls Constructors rewrites are never re-derived
    // through it.
    val resolveGroup = List(ResolveEvalAccess(config, store))
    val logGroup =
      if config.evalLogDir.isEmpty || config.evalLogTimestamp.isEmpty then Nil
      else List(List(LogExecutedTree(config)))
    (before :+ List(ExtractEvalBody(config, store))) ++ (after :+ resolveGroup) ++ logGroup
