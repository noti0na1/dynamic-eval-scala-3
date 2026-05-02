package dotty.tools
package repl

import dotc.ast.tpd
import dotc.core.Contexts.*
import dotc.core.Phases.Phase

import scala.compiletime.uninitialized

/** A phase that collects user defined top level imports.
 *
 *  These imports must be collected as typed trees and therefore
 *  after Typer.
 */
class CollectTopLevelImports extends Phase {
  import tpd.*

  def phaseName: String = "collectTopLevelImports"

  private var myImports: List[Import] = uninitialized
  def imports: List[Import] = myImports

  protected def run(using Context): Unit = {
    def topLevelImports(tree: Tree) = {
      val PackageDef(_, _ :: TypeDef(_, rhs: Template) :: _) = tree: @unchecked
      rhs.body.collect {
        // Skip the synthetic `eval` import the REPL injects into every
        // wrapper. It's an implementation detail, not a user-visible import.
        case tree: Import if !isSyntheticEvalImport(tree) => tree
      }
    }

    val tree = ctx.compilationUnit.tpdTree
    myImports = topLevelImports(tree)
  }

  private def isSyntheticEvalImport(imp: Import)(using Context): Boolean =
    imp.expr.symbol.fullName.toString == "dotty.tools.repl.Eval"
}
