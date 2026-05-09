package dotty.tools
package repl
package eval

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Contexts.*

/** Per-compile state shared between [[ExtractEvalBody]] and
 *  [[ResolveEvalAccess]]. Mirrors `dotty.tools.debug.ExpressionStore`.
 *
 *  Populated by Extract once it finds the spliced `val __evalResult`:
 *
 *    - `symbol`: the val symbol; used as the owner-chain anchor when
 *      classifying body-local vs outer references.
 *    - `classOwners`: enclosing classes from innermost out; Resolve
 *      walks this chain to lower `This(C)` / `Outer(_, C)` placeholders.
 */
private[eval] class EvalStore:
  var symbol: TermSymbol | Null = null
  var classOwners: Seq[ClassSymbol] = Seq.empty

  def store(exprSym: Symbol)(using Context): Unit =
    symbol = exprSym.asTerm
    classOwners = exprSym.ownersIterator.collect { case cls: ClassSymbol => cls }.toSeq
