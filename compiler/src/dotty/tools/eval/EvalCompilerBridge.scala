package dotty.tools
package eval

import java.nio.file.Path

import scala.util.control.NonFatal

import dotty.tools.dotc.{Compiler, Driver}
import dotty.tools.dotc.classpath.ClassPathFactory
import dotty.tools.dotc.core.Contexts.{Context, ContextBase, inContext}
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.SymbolLoaders
import dotty.tools.dotc.reporting.{Diagnostic, StoreReporter}
import dotty.tools.dotc.util.ClasspathFromClassloader
import dotty.tools.io.{AbstractFile, ClassPath}

/** Compile an eval body through the eval pipeline.
 *
 *  Two entry points cover the two callers we have:
 *
 *    - [[run]] for the file-based path used by unit tests: reads a
 *      source file off disk, writes class files to a real
 *      `outputDir: Path`, with a string classpath argument.
 *    - [[compile]] for in-process use by the REPL adapter: takes an
 *      [[AbstractFile]] `outDir` (which can be an in-memory
 *      `VirtualDirectory`) and an optional [[AbstractFile]]
 *      `replOutDir` registered directly via
 *      `ClassPathFactory.newClassPath` so REPL-line modules are
 *      visible without materialising them to disk.
 */
class EvalCompilerBridge:

  /** File-based path for unit tests. Spawns a fresh `Driver` via
   *  `process(args, reporter)` with a string classpath and a real
   *  `outputDir`. Cannot accept in-memory VirtualDirectory entries.
   */
  def run(
      outputDir: Path,
      classPath: String,
      options: Array[String],
      sourceFile: Path,
      config: EvalCompilerConfig
  ): Boolean =
    val args = Array(
      "-d", outputDir.toString,
      "-classpath", classPath
    ) ++ options :+ sourceFile.toString
    val driver = new Driver:
      override protected def newCompiler(using Context): EvalCompiler = EvalCompiler(config)
    val reporter = EvalReporter(error => config.errorReporter.accept(error))
    driver.process(args, reporter)
    !reporter.hasErrors

  /** In-process compile entry point. The REPL session's `replOutDir`
   *  (an in-memory `VirtualDirectory`) is added to the inner
   *  compile's classpath via `ClassPathFactory.newClassPath` +
   *  `SymbolLoaders.mergeNewEntries` so body code referencing
   *  `rs$line$N` modules resolves without writing them to disk.
   *
   *  Returns `Right(())` on success — the caller loads classes
   *  from `outDir` via [[AbstractFileClassLoader]]. On failure
   *  returns `Left` with the compile errors as messages.
   *
   *  @param source            the synthesised compilation unit text.
   *  @param outDir            destination for compiled class files.
   *                           A `VirtualDirectory` is fine.
   *  @param classLoader       session classloader, used to derive
   *                           the base classpath.
   *  @param replOutDir        REPL session output dir (may hold
   *                           `rs$line$N` modules). May be `null`.
   *  @param compilerSettings  forwarded `-X…` settings from the
   *                           REPL session.
   *  @param replClasspath     when non-empty, used verbatim as the
   *                           inner compile's classpath. The REPL
   *                           caller passes
   *                           `state.context.settings.classpath.value`
   *                           here so the inner compile sees exactly
   *                           the same classpath the REPL itself
   *                           successfully compiles against. Empty
   *                           string falls back to the synthesised
   *                           cliCp + classloader + java.class.path
   *                           path used by tests / direct callers.
   *  @param config            eval config (splice marker, output
   *                           class name, body, error reporter).
   */
  def compile(
      source: String,
      outDir: AbstractFile,
      classLoader: ClassLoader,
      replOutDir: AbstractFile | Null,
      compilerSettings: Array[String],
      replClasspath: String,
      config: EvalCompilerConfig,
      extraClassDirs: List[AbstractFile] = Nil
  ): Either[Seq[String], Unit] =
    compileWith(source, outDir, classLoader, replOutDir, extraClassDirs,
      compilerSettings, replClasspath, () => new EvalCompiler(config))

  /** Compile plain definitions (no eval body, no splice marker)
   *  through the standard `Compiler` pipeline against the same
   *  classpath view as [[compile]]. Used for the one-time
   *  `Eval.topLevel` defs compile: the output classes persist for the
   *  handle's lifetime and later [[compile]] calls see them via
   *  `extraClassDirs`.
   *
   *  The eval rewrite phase is forced on ([[DefsCompiler]]) rather
   *  than left to `-Xdynamic-eval`: a REPL session compiles defs
   *  without the flag, and an eval-like call written inside the defs
   *  must still capture its scope — its slice re-links later through
   *  the handle the capture re-attaches at runtime (see
   *  `Eval.withInheritedHandles`).
   */
  def compileDefs(
      source: String,
      outDir: AbstractFile,
      classLoader: ClassLoader,
      replOutDir: AbstractFile | Null,
      compilerSettings: Array[String],
      replClasspath: String
  ): Either[Seq[String], Unit] =
    compileWith(source, outDir, classLoader, replOutDir, Nil,
      compilerSettings, replClasspath, () => new EvalCompilerBridge.DefsCompiler)

  private def compileWith(
      source: String,
      outDir: AbstractFile,
      classLoader: ClassLoader,
      replOutDir: AbstractFile | Null,
      extraClassDirs: List[AbstractFile],
      compilerSettings: Array[String],
      replClasspath: String,
      mkCompiler: () => Compiler
  ): Either[Seq[String], Unit] =
    val driver = new EvalDriver
    val (classpath, settingsWithoutCp) =
      if replClasspath.nonEmpty then (replClasspath, splitClasspathFlag(compilerSettings)._2)
      else composeClasspath(compilerSettings, classLoader)
    // The classpath goes through `setup`'s argument parsing (not a
    // direct `settings.classpath.update`, whose copy-on-write result
    // is silently dropped once any setting has been read). Setup
    // diagnostics (e.g. a malformed forwarded option) are buffered so
    // a failure can report the actual cause.
    val setupReporter = new StoreReporter(null)
    val setupCtx = driver.initCtx.fresh.setReporter(setupReporter)
    driver.setup(settingsWithoutCp ++ Array("-classpath", classpath), setupCtx) match
      case Some((_, ctx0)) =>
        val storeReporter = new StoreReporter(null)
        // The inner compile is a continuation of the live REPL session:
        // it recompiles an eval body inside a `rs$line$<uuid>$__Eval…`
        // wrapper whose name is an `isReplWrapperName`. Run it in
        // `Mode.Interactive` (as `ReplDriver` does for the session
        // itself) so those reserved `$`-containing wrapper names are
        // accepted — `SafeRefs.allowDollarIn` and `Namer.checkDefName`
        // both exempt REPL wrapper names only in interactive mode.
        val freshCtx = ctx0.fresh
          .addMode(Mode.Interactive)
          .setSetting(ctx0.settings.outputDir, outDir)
          .setReporter(storeReporter)
        // In-memory dirs joining the compile's classpath: the REPL
        // session output (rs$line$N modules) plus any `Eval.topLevel`
        // handle output dirs the call links against.
        val classDirs =
          (if replOutDir == null then Nil else List(replOutDir)) ++ extraClassDirs
        if classDirs.nonEmpty then
          freshCtx.base.initialize()(using freshCtx)
          for dir <- classDirs do
            val dirClassPath = ClassPathFactory.newClassPath(dir)(using freshCtx)
            freshCtx.platform.addToClassPath(dirClassPath)(using freshCtx)
            SymbolLoaders.mergeNewEntries(
              defn(using freshCtx).RootClass,
              ClassPath.RootPackage,
              dirClassPath,
              freshCtx.platform.classPath(using freshCtx)
            )(using freshCtx)
        try
          val compiler = mkCompiler()
          val run = compiler.newRun(using freshCtx)
          run.compileFromStrings(source :: Nil)
          if storeReporter.hasErrors then
            // The reporter buffers warnings alongside errors; keep only
            // the errors so `CompileFailure.errors` (which agent
            // retry loops feed back into generators) isn't diluted
            // with lint output about code the user didn't write.
            Left(storeReporter.removeBufferedMessages(using freshCtx)
              .collect { case err: Diagnostic.Error => err.message })
          else Right(())
        catch case NonFatal(e) =>
          val sw = new java.io.StringWriter
          e.printStackTrace(new java.io.PrintWriter(sw))
          Left(Seq(s"Internal compiler error: ${e.getClass.getName}: ${e.getMessage}\n${sw.toString}"))
      case None =>
        Left(Seq("Failed to set up eval driver"))
  end compileWith

  /** Driver subclass exposing `initCtx` so [[compile]] can install a
   *  buffering reporter before `setup`.
   */
  private class EvalDriver extends Driver:
    override def sourcesRequired: Boolean = false
    override def initCtx: Context = super.initCtx

  /** Build the inner compile's classpath from three sources:
   *    1. any `-classpath` flag in `compilerSettings`,
   *    2. the session classloader (`ClasspathFromClassloader`),
   *    3. the host JVM's `java.class.path`.
   *
   *  Returns the combined path plus settings with `-classpath` /
   *  `-cp` removed so they don't override the composed path when the
   *  settings are re-parsed by `Driver.setup`.
   */
  private def composeClasspath(
      compilerSettings: Array[String],
      classLoader: ClassLoader
  ): (String, Array[String]) =
    val (cliCp, stripped) = splitClasspathFlag(compilerSettings)
    val cp = ClasspathFromClassloader(classLoader)
    val sysCp = Option(System.getProperty("java.class.path")).getOrElse("")
    val sep = java.io.File.pathSeparator
    val combined = (cliCp.toSeq ++ Seq(cp, sysCp)).filter(_.nonEmpty).mkString(sep)
    (combined, stripped)

  /** Split off every classpath flag in `args`, in both the two-token
   *  (`-classpath <path>`) and colon (`-classpath:<path>`) forms the
   *  CLI accepts. Returns the first path found (if any) and the
   *  remaining arguments.
   */
  private def splitClasspathFlag(args: Array[String]): (Option[String], Array[String]) =
    val kept = Array.newBuilder[String]
    var found: Option[String] = None
    var i = 0
    while i < args.length do
      val a = args(i)
      if a == "-classpath" || a == "-cp" then
        if i + 1 < args.length then
          if found.isEmpty then found = Some(args(i + 1))
          i += 1
      else if a.startsWith("-classpath:") || a.startsWith("-cp:") then
        if found.isEmpty then found = Some(a.substring(a.indexOf(':') + 1))
      else kept += a
      i += 1
    (found, kept.result())

object EvalCompilerBridge:
  /** The standard pipeline with the eval rewrite phase forced on.
   *  The `Eval.topLevel` defs compile goes through this: an
   *  eval-like call written inside the defs must capture its scope
   *  like any other call site, and gating on `-Xdynamic-eval` would
   *  leave it unrewritten in a REPL session, which never passes the
   *  flag. */
  private[eval] class DefsCompiler extends Compiler:
    override protected def frontendPhases: List[List[dotty.tools.dotc.core.Phases.Phase]] =
      super.frontendPhases.map(_.map {
        case p if p.phaseName == EvalRewriteTyped.name =>
          new EvalRewriteTyped(None, alwaysEnabled = true)
        case p => p
      })
