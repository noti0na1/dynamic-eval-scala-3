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

/** Entry points for compiling generated eval sources.
 *
 *  [[run]] uses filesystem paths for tests and command-line integration.
 *  [[compile]] accepts in-memory output and REPL directories for runtime use.
 */
class EvalCompilerBridge:

  /** Compiles a source file to a filesystem output directory. */
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

  /** Compiles generated source in process. `replOutDir` is added directly to
   *  the classpath so generated code can resolve in-memory `rs$line$N` modules.
   *
   *  Returns `Right(())` on success or diagnostic messages on failure.
   *
   *  @param source            generated compilation unit text.
   *  @param outDir            destination for compiled class files.
   *                           A `VirtualDirectory` is fine.
   *  @param classLoader       session classloader, used to derive
   *                           the base classpath.
   *  @param replOutDir        optional REPL session output directory.
   *  @param compilerSettings  compiler settings forwarded by the session.
   *  @param replClasspath     live session classpath, or empty to synthesize one
   *                           from settings, classloaders, and `java.class.path`.
   *  @param config            splice and output configuration.
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

  /** Compiles persistent [[Eval.topLevel]] definitions through the standard
   *  pipeline and the same classpath setup as [[compile]].
   *
   *  [[DefsCompiler]] always enables eval rewriting because a nested eval call
   *  must capture its scope even when the REPL did not pass `-Xdynamic-eval`.
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
    // Pass the classpath through setup: direct setting updates are copy-on-write
    // and can be lost after settings have been read. Buffer setup diagnostics so
    // invalid forwarded options report their actual cause.
    val setupReporter = new StoreReporter(null)
    val setupCtx = driver.initCtx.fresh.setReporter(setupReporter)
    val setupResult = driver.setup(settingsWithoutCp ++ Array("-classpath", classpath), setupCtx)
    val setupErrors = setupReporter.removeBufferedMessages(using setupCtx)
      .collect { case err: Diagnostic.Error => err.message }
    if setupErrors.nonEmpty then Left(setupErrors)
    else setupResult match
      case Some((_, ctx0)) =>
        val storeReporter = new StoreReporter(null)
        // Interactive mode admits the reserved `$` names used by generated REPL
        // wrappers, matching the context of the outer session compile.
        val freshCtx = ctx0.fresh
          .addMode(Mode.Interactive)
          .setSetting(ctx0.settings.outputDir, outDir)
          .setReporter(storeReporter)
        // Add in-memory REPL output and linked top-level definitions.
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
            // CompileFailure contains errors only; warnings are not part of the
            // dynamic-eval result.
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

  /** Exposes `initCtx` so setup diagnostics can be buffered. */
  private class EvalDriver extends Driver:
    override def sourcesRequired: Boolean = false
    override def initCtx: Context = super.initCtx

  /** Builds the inner classpath from three sources:
   *    1. any `-classpath` flag in `compilerSettings`,
   *    2. the session classloader (`ClasspathFromClassloader`),
   *    3. the host JVM's `java.class.path`.
   *
   *  Classpath flags are removed from the returned settings so `Driver.setup`
   *  cannot override the combined path.
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

  /** Removes classpath flags in either CLI form and returns the first path.
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
  /** Standard compiler with eval rewriting enabled for top-level definitions. */
  private[eval] class DefsCompiler extends Compiler:
    override protected def frontendPhases: List[List[dotty.tools.dotc.core.Phases.Phase]] =
      super.frontendPhases.map(_.map {
        case p if p.phaseName == EvalRewriteTyped.name =>
          new EvalRewriteTyped(None, alwaysEnabled = true)
        case p => p
      })
