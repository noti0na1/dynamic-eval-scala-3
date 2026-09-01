package dotty.tools
package eval

import dotc.Compiler
import dotc.cc.CheckCaptures
import dotc.core.Phases.Phase

/** Compiler used for generated eval wrappers. It adds these phases to the
 *  standard pipeline:
 *
 *    - [[SpliceEvalBody]] parses and inserts the submitted body after parsing.
 *    - [[EvalRewriteTyped]] fills synthetic context arguments after PostTyper.
 *    - [[EvalCaptureInlined]] appends values introduced by inline expansion.
 *    - [[ExtractEvalBody]] moves the checked body to `evaluate` after capture
 *      checking and marks outer references for runtime access.
 *    - [[ResolveEvalAccess]] lowers those references after erasure.
 *    - [[LogExecutedTree]], when enabled, records the final transformed tree.
 */
class EvalCompiler(config: EvalCompilerConfig) extends Compiler:

  override protected def frontendPhases: List[List[Phase]] =
    val parser :: others = super.frontendPhases: @unchecked
    // Replace the flag-gated phase in place. The outer and wrapper compiles
    // must rewrite at the same point so their symbols and linked-constructor
    // ordering agree. This point is late enough to identify eval methods by
    // symbol, but early enough to exclude values introduced by inlining.
    parser :: List(SpliceEvalBody(config)) :: others.map(_.map {
      case p if p.phaseName == EvalRewriteTyped.name => new EvalRewriteTyped(Some(config))
      case p => p
    })

  override protected def transformPhases: List[List[Phase]] =
    val store = EvalStore()
    // Swap the base pipeline's flag-gated [[EvalCaptureInlined]] for
    // the config-carrying instance, so nested eval calls inside the
    // body get inliner-introduced bindings appended too.
    val basePhases = super.transformPhases.map(_.map {
      case p if p.phaseName == EvalCaptureInlined.name =>
        new EvalCaptureInlined(Some(config))
      case p => p
    })
    // Extract only after capture checking has seen the body in its original
    // lexical context. Disabled phases remain present in the phase plan, so
    // this anchor is available in every run.
    val ccIndex = basePhases.indexWhere(_.exists(_.phaseName == CheckCaptures.name))
    assert(ccIndex >= 0, s"phase ${CheckCaptures.name} not found in transform plan")
    val (before, after) = basePhases.splitAt(ccIndex + 1)
    // ResolveEvalAccess runs after Constructors, but changes information only
    // for non-class symbols owned by the expression or its body-local classes.
    // It therefore cannot invalidate class declarations transformed there.
    val resolveGroup = List(ResolveEvalAccess(config, store))
    val logGroup =
      if config.evalLogDir.isEmpty || config.evalLogTimestamp.isEmpty then Nil
      else List(List(LogExecutedTree(config)))
    (before :+ List(ExtractEvalBody(config, store))) ++ (after :+ resolveGroup) ++ logGroup
