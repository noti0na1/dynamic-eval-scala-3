package dotty.tools
package repl
package eval

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.NameTransformer

/** Encode dotty types and symbols to the JVM-level names that
 *  reflective `Class.getDeclaredField` / `getDeclaredMethod` lookups
 *  expect. Adapted from `dotty.tools.debug.JavaEncoding`, narrowed to
 *  what the eval reflective helpers need (Field, FieldAssign,
 *  MethodCall).
 */
private[eval] object JavaEncoding:

  /** Encode a type to the form matched by `Class.getName`:
   *    - primitives: `int`, `boolean`, ...
   *    - reference types: `java.lang.String`, `pkg.A$B`
   *    - arrays: `[Lpkg.A;`, `[I` for primitive arrays
   *
   *  Used for parameter and return types of `callMethod` lookups.
   */
  def encode(tpe: Type)(using Context): String =
    tpe.widenDealias match
      case JavaArrayType(el) => s"[${binaryName(el)}"
      case tpe: TypeRef => encode(tpe.symbol.asType)
      case AnnotatedType(t, _) => encode(t)
      case _ => "java.lang.Object"

  /** Encode a type symbol — class or primitive — to its JVM name. */
  def encode(sym: TypeSymbol)(using Context): String =
    if !sym.isClass then "java.lang.Object"
    else if sym.isPrimitiveValueClass then primitiveName(sym)
    else className(sym)

  /** Encode a term name as it appears in JVM bytecode. */
  def encode(name: TermName)(using Context): String =
    NameTransformer.encode(name.toSimpleName).toString

  private def binaryName(tpe: Type)(using Context): String =
    tpe match
      case JavaArrayType(el) => s"[${binaryName(el)}"
      case tpe: TypeRef =>
        if tpe.symbol.isPrimitiveValueClass then primitiveBinaryName(tpe.symbol)
        else classBinaryName(tpe.symbol)
      case AnnotatedType(t, _) => binaryName(t)
      case _ => "Ljava/lang/Object;"

  private def primitiveName(sym: Symbol)(using Context): String =
    if sym == defn.UnitClass then "void"
    else if sym == defn.BooleanClass then "boolean"
    else if sym == defn.CharClass then "char"
    else if sym == defn.ByteClass then "byte"
    else if sym == defn.ShortClass then "short"
    else if sym == defn.IntClass then "int"
    else if sym == defn.LongClass then "long"
    else if sym == defn.FloatClass then "float"
    else if sym == defn.DoubleClass then "double"
    else throw new AssertionError(s"unknown primitive value class: $sym")

  private def primitiveBinaryName(sym: Symbol)(using Context): String =
    if sym == defn.BooleanClass then "Z"
    else if sym == defn.CharClass then "C"
    else if sym == defn.ByteClass then "B"
    else if sym == defn.ShortClass then "S"
    else if sym == defn.IntClass then "I"
    else if sym == defn.LongClass then "J"
    else if sym == defn.FloatClass then "F"
    else if sym == defn.DoubleClass then "D"
    else throw new AssertionError(s"unknown primitive value class: $sym")

  private def className(sym: Symbol)(using Context): String =
    val sym1 =
      if sym.isAllOf(ModuleClass | JavaDefined) then sym.linkedClass
      else sym
    if sym1 == defn.NothingClass then "scala.runtime.Nothing$"
    else if sym1 == defn.NullClass then "scala.runtime.Null$"
    else sym1.javaClassName

  private def classBinaryName(sym: Symbol)(using Context): String =
    s"L${className(sym)};"
