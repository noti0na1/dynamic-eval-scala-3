package dotty.tools
package repl
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
    // [[EvalRewriteTyped]] runs at the *end* of frontend (after
    // PostTyper, before any transformPhases like Inlining, macro
    // expansion, cc). At that point typed symbols are resolved (so
    // the eval / evalSafe call's owner can be verified against
    // `Eval.moduleClass`) but inline / macro expansion hasn't yet
    // synthesised compiler-introduced variables we'd otherwise
    // accidentally capture as bindings.
    parser :: List(SpliceEvalBody(config)) :: (others :+ List(new EvalRewriteTyped(Some(config))))

  override protected def transformPhases: List[List[Phase]] =
    val store = EvalStore()
    val transformPhases = super.transformPhases
    // Anchor [[ExtractEvalBody]] relative to the capture-checking
    // phase so `cc` sees the body in its original lexical context.
    // `cc` only runs when capture checking is enabled in the
    // session; fall back to placing the eval phase right after the
    // typer's transform group when it isn't there.
    val ccIndex = transformPhases.indexWhere(_.exists(_.phaseName == CheckCaptures.name))
    val anchor =
      if ccIndex >= 0 then ccIndex
      else 0
    val (before, after) = transformPhases.splitAt(anchor + 1)
    val resolveGroup = List(ResolveEvalAccess(config, store))
    val logGroup =
      if config.evalLogDir.isEmpty || config.evalLogTimestamp.isEmpty then Nil
      else List(List(LogExecutedTree(config)))
    (before :+ List(ExtractEvalBody(config, store))) ++ (after :+ resolveGroup) ++ logGroup
