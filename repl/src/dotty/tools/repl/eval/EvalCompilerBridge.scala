package dotty.tools
package repl
package eval

import java.nio.file.Path

import scala.util.control.NonFatal

import dotty.tools.dotc.Driver
import dotty.tools.dotc.classpath.ClassPathFactory
import dotty.tools.dotc.core.Contexts.{Context, ContextBase, inContext}
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.SymbolLoaders
import dotty.tools.dotc.reporting.StoreReporter
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
      "-classpath", classPath,
      "-Yskip:pureStats"
    ) ++ options :+ sourceFile.toString
    val driver = new Driver:
      override protected def newCompiler(using Context): EvalCompiler = EvalCompiler(config)
    val reporter = EvalReporter(error => config.errorReporter.accept(error))
    try
      driver.process(args, reporter)
      !reporter.hasErrors
    catch case cause: Exception =>
      cause.printStackTrace()
      throw cause

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
      config: EvalCompilerConfig
  ): Either[Seq[String], Unit] =
    val driver = new EvalDriver
    val (classpath, settingsWithoutCp) =
      if replClasspath.nonEmpty then (replClasspath, stripClasspathFlag(compilerSettings))
      else composeClasspath(compilerSettings, classLoader)
    val initCtx = driver.initCtx
    initCtx.settings.classpath.update(classpath)(using initCtx)
    driver.setup(settingsWithoutCp, initCtx) match
      case Some((_, ctx0)) =>
        val storeReporter = new StoreReporter(null)
        val freshCtx = ctx0.fresh
          .setSetting(ctx0.settings.outputDir, outDir)
          .setReporter(storeReporter)
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
          val compiler = new EvalCompiler(config)
          val run = compiler.newRun(using freshCtx)
          run.compileFromStrings(source :: Nil)
          if storeReporter.hasErrors then
            Left(storeReporter.removeBufferedMessages(using freshCtx).map(_.message))
          else Right(())
        catch case NonFatal(e) =>
          val sw = new java.io.StringWriter
          e.printStackTrace(new java.io.PrintWriter(sw))
          Left(Seq(s"Internal compiler error: ${e.getClass.getName}: ${e.getMessage}\n${sw.toString}"))
      case None =>
        Left(Seq("Failed to set up eval driver"))
  end compile

  /** Driver subclass that exposes `initCtx` so [[compile]] can
   *  pre-set the composed classpath before `setup`.
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
   *  `-cp` removed so `Driver.setup` doesn't try to re-set it.
   */
  private def composeClasspath(
      compilerSettings: Array[String],
      classLoader: ClassLoader
  ): (String, Array[String]) =
    val cliCp = extractClasspathArg(compilerSettings)
    val cp = ClasspathFromClassloader(classLoader)
    val sysCp = Option(System.getProperty("java.class.path")).getOrElse("")
    val sep = java.io.File.pathSeparator
    val combined = (cliCp.toSeq ++ Seq(cp, sysCp)).filter(_.nonEmpty).mkString(sep)
    (combined, stripClasspathFlag(compilerSettings))

  private def extractClasspathArg(args: Array[String]): Option[String] =
    val i = args.indexWhere(a => a == "-classpath" || a == "-cp")
    if i < 0 || i + 1 >= args.length then None
    else Some(args(i + 1))

  private def stripClasspathFlag(args: Array[String]): Array[String] =
    val i = args.indexWhere(a => a == "-classpath" || a == "-cp")
    if i < 0 then args
    else if i + 1 >= args.length then args.take(i)
    else args.take(i) ++ args.drop(i + 2)
