package dotty.tools
package repl

import scala.util.control.NonFatal

import dotc.Driver
import dotc.classpath.ClassPathFactory
import dotc.core.Contexts.{Context, ContextBase, inContext}
import dotc.core.Symbols.defn
import dotc.core.SymbolLoaders
import dotc.reporting.StoreReporter
import dotc.util.ClasspathFromClassloader
import io.{AbstractFile, AbstractFileClassLoader, ClassPath, VirtualDirectory}

/** Runtime `eval` for the dotty REPL.
 *
 *  `Eval.eval(code)` compiles and runs `code` at runtime, returning the
 *  result as the polymorphic type `T`. The argument can be any `String`.
 *
 *  The REPL parser injects bindings automatically for every lambda
 *  parameter (and block-local val/def) syntactically in scope at the call
 *  site, so `xs.map(z => eval[Int]("z + 1"))` works without the user
 *  writing the bindings explicitly.
 *
 *  The public surface intentionally avoids any Scala-library types
 *  (`Seq`, `ClassTag`, etc.). User wrappers and `Eval` are loaded by
 *  different classloaders, and Scala types resolve to different `Class`
 *  objects across the boundary, causing `LinkageError`. We use only
 *  JVM-intrinsic types (`String`, `Object`, `Array`) on the API surface.
 *
 *  Outside an active REPL session this throws.
 */
object Eval:

  /** A captured binding.
   *
   *  @param name        the source-level name as it appears at the call site.
   *  @param value       the runtime value, or for var captures the
   *                     `VarCell` the eval body mutates and the call site
   *                     reads back from.
   *  @param isVar       true iff this represents a `var` capture.
   *  @param sourceType  the source-level Scala type the typer inferred
   *                     for this capture, e.g. `"Int"`, `"Int => Int"`,
   *                     `"List[Int]"`. The empty string is the sentinel
   *                     meaning "fall back to runtime `Class`-walking";
   *                     it appears when this binding wasn't annotated
   *                     by `EvalTypeAnnotate` (e.g. nested-eval
   *                     captures, which never reach the typed pipeline).
   */
  final class Binding(
      val name: String,
      val value: Any,
      val isVar: Boolean,
      val sourceType: String
  ):
    override def toString = s"Binding($name, $value, isVar=$isVar, sourceType=$sourceType)"

  /** Mutable cell wrapping a captured `var`. We use the JDK's
   *  `AtomicReference` rather than a class of our own: JDK types are
   *  loaded by the bootstrap classloader, so the cell class resolves
   *  to the same `Class` object on both sides of the eval-driver and
   *  REPL classloader boundary. A REPL-defined cell type would resolve
   *  to two distinct `Class` objects (one per loader) and `Method.invoke`
   *  would reject the call with "argument type mismatch".
   */
  type VarCell[T] = java.util.concurrent.atomic.AtomicReference[T]

  object VarCell:
    def apply[T](initial: T): VarCell[T] =
      new java.util.concurrent.atomic.AtomicReference[T](initial)

  /** Capture an immutable binding. The 3-arg form is what the
   *  parser-stage rewriter actually emits; the post-typer phase
   *  `EvalTypeAnnotate` populates `sourceType` with the typer-known
   *  Scala source type. The 2-arg overload is kept for callers who
   *  don't care about the type-annotation pass (notably the nested-eval
   *  rewrite, which runs at runtime against an untyped tree).
   */
  def bind(name: String, value: Any): Binding =
    new Binding(name, value, isVar = false, sourceType = "")

  def bind(name: String, value: Any, sourceType: String): Binding =
    new Binding(name, value, isVar = false, sourceType)

  /** Capture a mutable (`var`) binding via an `AtomicReference`. The
   *  eval body receives the cell, declares a local var initialised from
   *  `cell.get()`, runs, and writes back via `cell.set(...)`. The
   *  call-site rewriter then assigns `cell.get()` to the outer var.
   */
  def bindVar(name: String, cell: VarCell[?]): Binding =
    new Binding(name, cell, isVar = true, sourceType = "")

  def bindVar(name: String, cell: VarCell[?], sourceType: String): Binding =
    new Binding(name, cell, isVar = true, sourceType)

  /** Adapter installed by the running REPL driver. */
  trait Adapter:
    def evalCode(code: String, bindings: Array[Binding]): Any

  private val active = new ThreadLocal[Adapter]

  def withAdapter[T](adapter: Adapter)(thunk: => T): T =
    val prev = active.get
    active.set(adapter)
    try thunk
    finally if prev == null then active.remove() else active.set(prev)

  /** Compile and run `code` against the current REPL session.
   */
  def eval[T](code: String): T =
    evalImpl[T](code, Array.empty[Binding])

  /** Like `eval(code)` but with explicit bindings. The REPL parser-stage
   *  rewriter dispatches to this overload when there are lambda-local
   *  names to capture.
   */
  def eval[T](code: String, bindings: Array[Binding]): T =
    evalImpl[T](code, bindings)

  private def evalImpl[T](code: String, bindings: Array[Binding]): T =
    val a = active.get
    if a == null then
      throw new IllegalStateException(
        "eval(...) requires an active dotty REPL session"
      )
    a.evalCode(code, bindings).asInstanceOf[T]

  /** Compile `code` against `classLoader`'s classpath using a fresh,
   *  standalone Driver, with each `Binding` exposed as a method parameter
   *  whose declared type is recovered from its runtime `Class`. Loads
   *  and invokes the compiled function with the captured values.
   *
   *  When `replOutDir` is non-null and `replWrapperImports` is non-empty,
   *  the body is also given access to the running REPL session's
   *  user-defined symbols: `replOutDir` is added to the compiler's
   *  classpath so symbols in `rs$line$N` are resolvable, and the import
   *  statements bring those symbols into the body's lexical scope.
   */
  def evalIsolated(
      code: String,
      classLoader: ClassLoader,
      bindings: Array[Binding],
      replOutDir: AbstractFile,
      replWrapperImports: Array[String],
      compilerSettings: Array[String]
  ): Any =
    val outDir = new VirtualDirectory("<eval-output>")
    val wrapperName = s"__EvalWrapper_${java.util.UUID.randomUUID.toString.replace('-', '_')}"

    // Pre-compute the source-level type name for each binding once.
    // Prefer the typer-supplied `sourceType` (filled in by the
    // `EvalTypeAnnotate` phase). When that's empty, fall back to
    // walking the runtime `Class` of the captured value. For vars the
    // fallback path also pins the cell's inner type for both the
    // parameter signature and the body-local var declaration, so a
    // racing write between the two reads can't desynchronise them.
    val bindingTypes: Array[String] = bindings.map { b =>
      if b.sourceType.nonEmpty then b.sourceType
      else if b.isVar then
        val v = b.value.asInstanceOf[VarCell[?]].get()
        if v == null then "Any" else classToTypeName(v.getClass)
      else if b.value == null then "Any"
      else classToTypeName(b.value.getClass)
    }

    val params = bindings.iterator.zipWithIndex.map {
      case (b, i) if b.isVar =>
        s"`${b.name}__cell`: java.util.concurrent.atomic.AtomicReference[${bindingTypes(i)}]"
      case (b, i) =>
        s"`${b.name}`: ${bindingTypes(i)}"
    }.mkString(", ")

    // For var bindings, declare a body-local `var` initialised from the
    // cell, run the body, then write the local back to the cell. This
    // lets the user's body use `name = ...` syntax naturally.
    val varPrelude = bindings.iterator.zipWithIndex.collect {
      case (b, i) if b.isVar =>
        s"  var `${b.name}`: ${bindingTypes(i)} = `${b.name}__cell`.get()"
    }.mkString("\n")

    val varPostlude = bindings.iterator.collect {
      case b if b.isVar =>
        s"    `${b.name}__cell`.set(`${b.name}`)"
    }.mkString("\n")

    // Always import `Eval.eval` so the body itself can call `eval(...)`,
    // i.e. nested evals work. (The REPL's own ReplCompiler injects this
    // import into every user-line wrapper for the same reason.)
    val evalImport = "import dotty.tools.repl.Eval.eval\n"
    val importBlock =
      if replWrapperImports.length == 0 then evalImport
      else evalImport + replWrapperImports.mkString("", "\n", "\n")

    // Run the rewriter on the user's code so any nested `eval(...)`
    // calls inside the body capture the outer bindings AND any local
    // val/var the body itself declares. This makes a body like
    // `val j = 2; eval("i + j")` work out of the box: the inner eval
    // receives `j` as a binding the same way an outer eval would.
    val rewrittenCode = rewriteUserCode(code, bindings)

    val bodyBlock =
      if varPrelude.isEmpty && varPostlude.isEmpty then rewrittenCode
      else
        s"""$varPrelude
           |  val __eval_result__ : Any = {
           |    $rewrittenCode
           |  }
           |$varPostlude
           |  __eval_result__""".stripMargin

    val source =
      s"""${importBlock}object $wrapperName {
         |  def __run__($params): Any = {
         |    $bodyBlock
         |  }
         |}
         |""".stripMargin

    compileSource(source, classLoader, outDir, replOutDir, compilerSettings) match
      case Left(errs) =>
        throw new RuntimeException(
          s"eval failed to compile:\n${errs.mkString("\n")}\n\nGenerated source:\n$source"
        )
      case Right(()) =>

    // Use a custom classloader for the eval-compiled wrapper that
    // routes `dotty.tools.repl.*` lookups through the AppClassLoader,
    // including JVM-internal `loadClass(name, resolve)` calls during
    // link-time resolution. Without that bridging, `Eval.Binding` would
    // resolve to two different `Class` objects on the eval-output side
    // (URLClassLoader's copy) versus the REPL side (AppClassLoader's
    // copy), and the JVM would reject crossing-the-boundary calls with
    // a `LinkageError`.
    val cl = new EvalOutputClassLoader(outDir, classLoader)
    val cls = cl.loadClass(s"$wrapperName$$")
    val module = cls.getField("MODULE$").get(null)
    val method = cls.getMethods.find(_.getName == "__run__").getOrElse(
      throw new RuntimeException("__run__ method not found in compiled wrapper")
    )
    val args = bindings.map(_.value.asInstanceOf[AnyRef])
    try method.invoke(module, args*)
    catch case e: java.lang.reflect.InvocationTargetException =>
      // Preserve the user-visible cause; reflection wraps it in an ITE
      // whose `getCause` is normally non-null, but we guard against the
      // pathological case where it isn't.
      val cause = e.getCause
      if cause != null then throw cause else throw e
  end evalIsolated

  /** Best-effort conversion from a runtime `Class` to a Scala source-level
   *  type name. Generic type info is erased on the JVM, so parameterized
   *  types come back as their raw form (e.g. `List[Int]` becomes `List`).
   *
   *  When the runtime class is an implementation detail whose JVM name
   *  isn't valid Scala source (e.g. `scala.collection.immutable.::` is
   *  encoded as `scala.collection.immutable.$colon$colon`, and lambda
   *  classes have names like `pkg$$Lambda/0x...`), we walk up the
   *  superclass chain to find the first ancestor with a referenceable
   *  name. So `::` becomes `List`, `Map$Map2` becomes `AbstractMap`, etc.
   */
  private def classToTypeName(c: Class[?]): String =
    if c.isArray then s"Array[${classToTypeName(c.getComponentType)}]"
    else c.getName match
      case "boolean"             => "Boolean"
      case "byte"                => "Byte"
      case "short"               => "Short"
      case "char"                => "Char"
      case "int"                 => "Int"
      case "long"                => "Long"
      case "float"               => "Float"
      case "double"              => "Double"
      case "void"                => "Unit"
      case "java.lang.Boolean"   => "Boolean"
      case "java.lang.Byte"      => "Byte"
      case "java.lang.Short"     => "Short"
      case "java.lang.Character" => "Char"
      case "java.lang.Integer"   => "Int"
      case "java.lang.Long"      => "Long"
      case "java.lang.Float"     => "Float"
      case "java.lang.Double"    => "Double"
      case "java.lang.String"    => "String"
      case _ =>
        // REPL-defined classes are nested in a `rs$line$N` wrapper
        // module; their JVM name is `rs$line$N$<rest>`. Translate that
        // back into Scala source form (`rs$line$N.<rest>`) so we don't
        // walk past it to `Object`.
        replWrapperInnerName(c.getName) match
          case Some(name) =>
            val arity = c.getTypeParameters.length
            if arity == 0 then name
            else s"$name[${Array.fill(arity)("?").mkString(", ")}]"
          case None =>
            val target = referenceableClass(c)
            if target == null then "Any"
            else
              val arity = target.getTypeParameters.length
              val n = target.getName
              if arity == 0 then n
              else s"$n[${Array.fill(arity)("?").mkString(", ")}]"

  private def replWrapperInnerName(jvmName: String): Option[String] =
    val prefix = "rs$line$"
    if !jvmName.startsWith(prefix) then return None
    val rest = jvmName.drop(prefix.length)
    val dollarIdx = rest.indexOf('$')
    if dollarIdx <= 0 then return None
    val moduleNum = rest.substring(0, dollarIdx)
    if !moduleNum.forall(_.isDigit) then return None
    val inner = rest.substring(dollarIdx + 1).replace('$', '.')
    if inner.isEmpty then None
    else Some(s"$prefix$moduleNum.$inner")

  /** A class's name is "referenceable" as Scala source if it doesn't
   *  contain any of the special characters the JVM uses to encode names
   *  the source language can't otherwise express ($ for inner classes
   *  and operator-name encoding, / for hidden lambdas, etc.).
   */
  private def isReferenceableName(name: String): Boolean =
    !name.contains('$') && !name.contains('/')

  /** Walk `c`'s superclass chain and (failing that) its declared
   *  interfaces, returning the first ancestor that has a referenceable
   *  name. Returns `null` if nothing matches.
   */
  private def referenceableClass(c: Class[?]): Class[?] | Null =
    if isReferenceableName(c.getName) then return c
    var sup: Class[?] | Null = c.getSuperclass
    while sup != null && !isReferenceableName(sup.getName) do
      sup = sup.getSuperclass
    if sup != null then return sup
    c.getInterfaces.nn.iterator.find(i => isReferenceableName(i.nn.getName)).orNull

  /** Run the parser-stage rewriter over `code` so any nested `eval(...)`
   *  inside it gets bindings injected. The seeded scope contains the
   *  outer bindings (so an inner eval can also see them) plus the
   *  rewriter naturally tracks any val/var declared in the body.
   *
   *  Rewriting goes through dotty's pretty-printer which doesn't
   *  always round-trip cleanly (`match { ... }` may come back as
   *  `match\n {`, which the parser then chokes on). To avoid breaking
   *  bodies that don't actually need rewriting, we only rewrite when
   *  the body looks like it contains a nested `eval(...)` call, and we
   *  fall back to the original code if the rewritten form fails to
   *  re-parse.
   */
  private def rewriteUserCode(code: String, bindings: Array[Binding]): String =
    if !mightContainNestedEval(code) then return code
    val ctxBase = new ContextBase
    val ctx0 = ctxBase.initialCtx
    // Disable colors so `tree.show` round-trips through the parser
    // without ANSI escape sequences confusing it.
    val ctx = ctx0.fresh.setSetting(ctx0.settings.color, "never")
    val initialScope: Array[(String, Boolean)] =
      bindings.map(b => (b.name, b.isVar))
    try
      val rewritten = EvalRewriter.rewriteCode(code, initialScope)(using ctx)
      if isParseable(rewritten)(using ctx) then rewritten else code
    catch case NonFatal(_) =>
      code

  private def mightContainNestedEval(code: String): Boolean =
    code.contains("eval(") || code.contains("eval[")

  private def isParseable(code: String)(using Context): Boolean =
    try
      val source = dotty.tools.dotc.util.SourceFile.virtual("<verify>", code)
      val parser = new dotty.tools.dotc.parsing.Parsers.Parser(source)
      parser.block()
      true
    catch case NonFatal(_) => false

  /** Classloader for the eval-output VirtualDirectory. Overrides
   *  `loadClass(name, resolve)` (the entry point JVM-internal
   *  link-time class resolution uses) so references to
   *  `dotty.tools.repl.*` from the compiled wrapper are routed
   *  through the *same* classloader the running ReplDriver itself
   *  uses, rather than picked up from the compiler-classpath
   *  URLClassLoader. Otherwise `Eval$Binding`, `Eval$VarCell`, etc.
   *  would resolve to two different `Class` objects on the two
   *  sides of the eval boundary and the JVM would refuse to let the
   *  values cross.
   */
  private final class EvalOutputClassLoader(root: AbstractFile, parent: ClassLoader)
      extends AbstractFileClassLoader(root, parent):

    private val replInfraLoader: ClassLoader =
      classOf[EvalOutputClassLoader].getClassLoader

    override protected def loadClass(name: String, resolve: Boolean): Class[?] =
      val loaded = findLoadedClass(name)
      if loaded != null then
        if resolve then resolveClass(loaded)
        return loaded

      // `dotty.tools.repl.*` always routes through the loader that
      // loaded this class, sharing `Class` objects with the running
      // ReplDriver no matter how the call arrives (user
      // `cl.loadClass(...)` or JVM-internal link-time lookup).
      val c =
        if name.startsWith("dotty.tools.repl.") then
          replInfraLoader.loadClass(name)
        else
          // Child-first: try our own outDir, fall back to parent chain.
          // Going through `getParent` directly avoids re-entering this
          // method via the io.AFCL parent-chain logic.
          try findClass(name)
          catch case _: ClassNotFoundException => getParent.loadClass(name)

      if resolve then resolveClass(c)
      c
  end EvalOutputClassLoader

  private class EvalDriver(classLoader: ClassLoader) extends Driver:
    override def sourcesRequired: Boolean = false

    override def initCtx: Context =
      val ictx = (new ContextBase).initialCtx
      val cp = ClasspathFromClassloader(classLoader)
      val sysCp = Option(System.getProperty("java.class.path")).getOrElse("")
      val sep = java.io.File.pathSeparator
      val combined = Seq(cp, sysCp).filter(_.nonEmpty).mkString(sep)
      ictx.settings.classpath.update(combined)(using ictx)
      ictx
  end EvalDriver

  private def compileSource(
      source: String,
      classLoader: ClassLoader,
      outDir: AbstractFile,
      replOutDir: AbstractFile,
      compilerSettings: Array[String]
  ): Either[Seq[String], Unit] =
    val driver = new EvalDriver(classLoader)
    driver.setup(compilerSettings, driver.initCtx) match
      case Some((_, ctx0)) =>
        val storeReporter = new StoreReporter(null)
        val freshCtx = ctx0.fresh
          .setSetting(ctx0.settings.outputDir, outDir)
          .setReporter(storeReporter)
        // Splice the running REPL's output dir onto the compile-time
        // classpath so the body can resolve symbols defined in earlier
        // REPL lines (`rs$line$N`). `platform` requires initialize();
        // `mergeNewEntries` registers the entries in the symbol loader
        // so name resolution actually finds them.
        if replOutDir != null then
          freshCtx.base.initialize()(using freshCtx)
          val replClassPath = ClassPathFactory.newClassPath(replOutDir)(using freshCtx)
          freshCtx.platform.addToClassPath(replClassPath)(using freshCtx)
          SymbolLoaders.mergeNewEntries(
            defn(using freshCtx).RootClass,
            ClassPath.RootPackage,
            replClassPath,
            freshCtx.platform.classPath(using freshCtx)
          )(using freshCtx)
        try
          val compiler = new dotty.tools.dotc.Compiler
          val run = compiler.newRun(using freshCtx)
          run.compileFromStrings(source :: Nil)
          if storeReporter.hasErrors then
            Left(storeReporter.removeBufferedMessages(using freshCtx).map(_.message))
          else
            Right(())
        catch case NonFatal(e) =>
          Left(Seq(s"Internal compiler error: ${e.getMessage}"))
      case None =>
        Left(Seq("Failed to set up eval driver"))
  end compileSource

end Eval
