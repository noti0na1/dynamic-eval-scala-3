package dotty.tools
package repl

import scala.util.control.NonFatal

import java.io.{File => JFile, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.regex.Pattern

import dotc.ast.Trees.*
import dotc.ast.{tpd, untpd}
import dotc.classpath.ClassPathFactory
import dotc.config.CommandLineParser.tokenize
import dotc.config.Properties.{javaVersion, javaVmName, simpleVersionString}
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Phases.{unfusedPhases, typerPhase, checkCapturesPhase}
import dotc.core.Denotations.Denotation
import dotc.core.Flags.*
import dotc.core.Mode
import dotc.core.NameKinds.SimpleNameKind
import dotc.core.NameKinds.DefaultGetterName
import dotc.core.NameOps.*
import dotc.core.Names.Name
import dotc.core.StdNames.*
import dotc.core.Symbols.{Symbol, defn}
import dotc.core.SymbolLoaders
import dotc.interfaces
import dotc.interactive.Completion
import dotc.printing.SyntaxHighlighting
import dotc.reporting.{ConsoleReporter, StoreReporter}
import dotc.reporting.Diagnostic
import dotc.util.Spans.Span
import dotc.util.{SourceFile, SourcePosition}
import dotc.{CompilationUnit, Driver}
import dotc.config.{CompilerCommand, Feature}
import dotty.tools.io
import dotty.tools.io.{AbstractFileClassLoader => _, *}
import dotty.tools.dotc.classpath.FileUtils.isClassContainer
import dotty.tools.eval.{Eval, EvalAdapter}
import dotty.tools.repl.ScalaClassLoader.*
import AbstractFileClassLoader.InterruptInstrumentation

import org.jline.reader.*

import Rendering.showUser

import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*
import org.objectweb.asm.ClassReader
import scala.util.Using

/** The state of the REPL contains necessary bindings instead of having to have
 *  mutation
 *
 *  The compiler in the REPL needs to do some wrapping in order to compile
 *  valid code. This wrapping occurs when a single `MemberDef` that cannot be
 *  top-level needs to be compiled. In order to do this, we need some unique
 *  identifier for each of these wrappers. That identifier is `objectIndex`.
 *
 *  Free expressions such as `1 + 1` needs to have an assignment in order to be
 *  of use. These expressions are therefore given a identifier on the format
 *  `resX` where `X` starts at 0 and each new expression that needs an
 *  identifier is given the increment of the old identifier. This identifier is
 *  `valIndex`.
 *
 *  @param objectIndex the index of the next wrapper
 *  @param valIndex    the index of next value binding for free expressions
 *  @param imports     a map from object index to the list of user defined imports
 *  @param invalidObjectIndexes the set of object indexes that failed to compile or initialize
 *  @param quiet       whether we print evaluation results
 *  @param context     the latest compiler context
 *  @param pastInputs  the replayable inputs of the session, most recent first
 *  @param repositories the repositories added to dependency resolution, in the order they were added
 */
case class State(objectIndex: Int,
                 valIndex: Int,
                 imports: Map[Int, List[tpd.Import]],
                 invalidObjectIndexes: Set[Int],
                 quiet: Boolean,
                 context: Context,
                 pastInputs: List[String] = Nil,
                 repositories: List[coursierapi.Repository] = Nil):
  def validObjectIndexes = (1 to objectIndex).filterNot(invalidObjectIndexes.contains(_))

  def recordInput(input: String): State = copy(pastInputs = input :: pastInputs)

  def invalidateCurrentObject: State =
    copy(invalidObjectIndexes = invalidObjectIndexes + objectIndex)

  def afterFailedCompilation(failedObjectIndex: Int): State =
    copy(objectIndex = failedObjectIndex).invalidateCurrentObject

/** Main REPL instance, orchestrating input, compilation and presentation */
class ReplDriver(settings: Array[String],
                 out: PrintStream = System.out,
                 classLoader: Option[ClassLoader] = None,
                 extraPredef: String = "") extends Driver:

  /** The REPL output stream. It forwards to the caller-supplied stream while
   *  allowing [[ReplHistory]] to capture one submission at a time.
   */
  private[repl] val replOut: ReplHistory.TeePrintStream =
    ReplHistory.TeePrintStream(out)

  /** Overridden to `false` in order to not have to give sources on the
   *  commandline
   */
  override def sourcesRequired: Boolean = false

  /** Create a fresh and initialized context with IDE mode enabled */
  private def initialCtx(additionalSettings: List[String]): (Context, Boolean) = {
    val baseCtx = initCtx.fresh.addMode(Mode.ReadPositions | Mode.Interactive)
    baseCtx.setRetainedSymbolLoadingFailures(mutable.WeakHashMap.empty)
    baseCtx.setSetting(baseCtx.settings.XcookComments, true)
    baseCtx.setSetting(baseCtx.settings.XreadComments, true)
    setupRootCtx(this.settings ++ additionalSettings, baseCtx)
  }

  private val incompatibleOptions: Seq[String] = Seq(
    initCtx.settings.YbestEffort.name,
    initCtx.settings.YwithBestEffortTasty.name
  )

  private def setupRootCtx(settings: Array[String], baseCtx: Context): (Context, Boolean) = {
    val incompatible = settings.intersect(incompatibleOptions)
    val filteredSettings =
      if incompatible.nonEmpty then
        inContext(baseCtx) {
          replOut.println(i"Options incompatible with repl will be ignored: ${incompatible.mkString(", ")}")
        }
        settings.filter(!incompatible.contains(_))
      else settings
    setup(filteredSettings, baseCtx) match
      case Some((files, ictx)) => inContext(ictx) {
        shouldStart = true
        if files.nonEmpty then replOut.println(i"Ignoring spurious arguments: $files%, %")
        val finalCtx =
          // If the user hasn't configured warnings, enable -deprecation and -feature by default
          if !ctx.settings.Wconf.wasSetByUser && !ctx.settings.Wall.wasSetByUser then
            val c = ictx.fresh
            if !ctx.settings.deprecation.wasSetByUser then c.setSetting(c.settings.deprecation, true)
            if !ctx.settings.feature.wasSetByUser then c.setSetting(c.settings.feature, true)
            c
          else ictx
        finalCtx.base.initialize()
        (finalCtx, true)
      }
      case None =>
        shouldStart = false
        (baseCtx, false)
  }

  /** the initial, empty state of the REPL session */
  final def initialState: State =
    val emptyState = State(0, 0, Map.empty, Set.empty, false, rootCtx)
    val initScript = rootCtx.settings.replInitScript.value(using rootCtx)
    val combinedScript = initScript.trim() match
      case "" => extraPredef
      case script => s"$extraPredef\n$script"
    // Initialization scripts are session setup, not user submissions.
    val wasHistorySuppressed = historySuppressed
    historySuppressed = true
    try run(combinedScript)(using emptyState).copy(pastInputs = Nil)
    finally historySuppressed = wasHistorySuppressed

  /** Rebuild the compiler and renderer from the constructor options plus `settings`. */
  protected def resetToInitial(settings: List[String] = Nil): Unit = {
    val (attemptedCtx, settingsAccepted) = initialCtx(settings)
    if settingsAccepted then
      rootCtx = attemptedCtx
      effectiveSettings = this.settings ++ settings
    else
      // Rejected reset options must not reach later dynamic compilations.
      // Rebuild the context from the constructor settings instead.
      val (baselineCtx, constructorSettingsAccepted) = initialCtx(Nil)
      rootCtx = baselineCtx
      effectiveSettings = if constructorSettingsAccepted then this.settings else Array.empty
    if (rootCtx.settings.outputDir.isDefault(using rootCtx))
      rootCtx = rootCtx.fresh
        .setSetting(rootCtx.settings.outputDir, io.virtualDirectory("<REPL compilation output>"))
    compiler = new ReplCompiler
    rendering = new Rendering(classLoader)
  }

  /** Start a new session and discard cached wrappers tied to the old loader. */
  private def resetSession(settings: List[String]): State =
    resetToInitial(settings)
    EvalAdapter.clearCache()
    initialState

  private var rootCtx: Context = uninitialized
  private var shouldStart: Boolean = uninitialized
  private var compiler: ReplCompiler = uninitialized
  protected var rendering: Rendering = uninitialized
  // Settings accepted for this session and inherited by dynamic compilations.
  private var effectiveSettings: Array[String] = settings
  private var historySuppressed: Boolean = false

  // initialize the REPL session as part of the constructor so that once `run`
  // is called, we're in business
  resetToInitial()

  override protected def command: CompilerCommand = ReplCommand

  /** Try to run REPL if there is nothing that prevents us doing so.
   *
   *  Possible reason for unsuccessful run are raised flags in CLI like --help or --version
   */
  final def tryRunning = if shouldStart then
    if rootCtx.settings.replQuitAfterInit.value(using rootCtx) then initialState
    else runUntilQuit()

  /** Run REPL with `state` until `:quit` command found
   *
   *  This method is the main entry point into the REPL. Its effects are not
   *  observable outside of the CLI, for this reason, most helper methods are
   *  `protected final` to facilitate testing.
   */
  def runUntilQuit(using initialState: State = initialState)(): State = {
    val terminal = new JLineTerminal

    replOut.println(
      s"""Welcome to Scala $simpleVersionString ($javaVersion, Java $javaVmName).
         |Type in expressions for evaluation. Or try :help.""".stripMargin)

    /** Blockingly read a line, getting back a parse result */
    def readLine()(using state: State): ParseResult = {
      given Context = state.context
      val completer: Completer = { (lineReader, line, candidates) =>
        def makeCandidate(label: String) = {
          new Candidate(
            /* value    = */ label,
            /* displ    = */ stripBackTicks(label), // displayed value
            /* group    = */ null,  // can be used to group completions together
            /* descr    = */ null,  // TODO use for documentation?
            /* suffix   = */ null,
            /* key      = */ null,
            /* complete = */ false  // if true adds space when completing
          )
        }
        val comps = completions(line.cursor, line.line, state, lineReader.printAbove(_))
        candidates.addAll(comps.map(_.label).distinct.map(makeCandidate).asJava)
        val lineWord = line.word()
        comps.filter(c => c.label == lineWord && c.symbols.nonEmpty) match
          case Nil =>
          case exachMatches =>
            val terminal = lineReader.nn.getTerminal
            lineReader.callWidget(LineReader.CLEAR)
            terminal.writer.println()
            exachMatches.foreach: exact =>
              exact.symbols.foreach: sym =>
                terminal.writer.println(SyntaxHighlighting.highlight(sym.showUser))
            lineReader.callWidget(LineReader.REDRAW_LINE)
            lineReader.callWidget(LineReader.REDISPLAY)
            terminal.flush()
      }

      try {
        val line = terminal.readLine(completer)
        ParseResult(line)
      } catch {
        case _: EndOfFileException => // Ctrl+D
          Quit
        case _: UserInterruptException => // Ctrl+C at prompt - clear and continue
          SigKill
      }
    }

    @tailrec def loop(using state: State)(): State = {

      val res = readLine()
      if (res == Quit) state
      // Ctrl-C pressed at prompt - just continue with same state (line is cleared by JLine)
      else if (res == SigKill) loop(using state)()
      else {
        // Set up interrupt handler for command execution
        var firstCtrlCEntered = false
        val thread = Thread.currentThread()

        // Clear the stop flag before executing new code
        ReplBytecodeInstrumentation.setStopFlag(rendering.classLoader()(using state.context), false)

        val newState = terminal.withMonitoringCtrlC(
          handler = () =>
            if (!firstCtrlCEntered) {
              firstCtrlCEntered = true
              // Set the stop flag to trigger throwIfReplStopped() in instrumented code
              ReplBytecodeInstrumentation.setStopFlag(rendering.classLoader()(using state.context), true)
              // Also interrupt the thread as a fallback for non-instrumented code, e.g. IO/sleeps
              thread.interrupt()
              replOut.println("\nAttempting to interrupt running REPL command")
            } else {
              replOut.println("\nTerminating REPL Process...")
              System.exit(130)  // Standard exit code for SIGINT
            }
        ) {
          val savedIn = System.in
          val replIn = terminal.userInputStream
          try
            System.setIn(replIn)
            scala.Console.withIn(replIn) {
              interpret(res)
            }
          finally
            System.setIn(savedIn)
        }

        loop(using newState)()
      }
    }

    try runBody { loop() }
    finally terminal.close()
  }

  final def run(input: String)(using state: State): State = runBody {
    interpret(ParseResult.complete(input))
  }

  protected def runBody(body: => State): State =
    Eval.withAdapter(evalAdapter):
      rendering.classLoader()(using rootCtx).asContext(withRedirectedOutput(body))

  /** The latest state published by [[interpret]]. Dynamic evaluation callbacks use
   *  it to compile against the current session.
   */
  @volatile private var currentState: State | Null = null

  /** Adapter installed while this driver executes user code. */
  private val evalAdapter: Eval.Adapter = new Eval.Adapter {
    def evalCode(
        code: String,
        bindings: Array[Eval.Binding],
        expectedType: String,
        enclosingSource: String
    ): Either[Eval.CompileFailure, Any] =
      evalDynamic(code, bindings, expectedType, enclosingSource)

    override def compileTopLevel(
        defs: String,
        contextHeader: String
    ): Either[Eval.CompileFailure, Eval.TopLevel] =
      compileTopLevelDynamic(defs, contextHeader)
  }

  private val isolatedEvalAdapter = new EvalAdapter

  /** A snapshot of the live session required by an isolated dynamic compilation. */
  private final class SessionCompileInputs(state: State):
    private val ctx = state.context
    val classLoader: ClassLoader = rendering.classLoader()(using ctx)
    val outputDir = ctx.settings.outputDir.value(using ctx)

    // Only import wrappers present in the current output directory. A missing
    // wrapper would otherwise produce an unrelated import error.
    private def hasClassFile(idx: Int): Boolean =
      ReplCompiler.objectNames.get(idx).exists { wrapperName =>
        outputDir.lookupName(s"$wrapperName$$.class", directory = false) != null
      }

    // Import each emitted wrapper, then repeat imports written inside it;
    // wildcard-importing a wrapper does not re-export its imports. Disable
    // color while rendering because these strings become compiler input.
    val sessionImports: Array[String] =
      val printCtx = ctx.fresh.setSetting(ctx.settings.color, "never")
      state.validObjectIndexes.flatMap { i =>
        val wrapperImport =
          if hasClassFile(i) then Some(s"import ${ReplCompiler.objectNames(i)}.{given, *}")
          else None
        val userImports = state.imports.getOrElse(i, Nil).map(_.show(using printCtx))
        wrapperImport ++ userImports
      }.toArray

    // Standalone compiler setup cannot accept these REPL-incompatible options.
    val forwardedSettings: Array[String] = effectiveSettings.filterNot(incompatibleOptions.contains)
    val evalLogDir: String = ctx.settings.XreplEvalLogDir.value(using ctx)

    // Materialize entries added by `:jar`, `:dep`, and directives. The
    // in-memory output directory has no URL and is supplied separately. Keep
    // the configured classpath as a fallback when no file URLs are available.
    val classpath: String =
      val entries = ctx.platform.classPath(using ctx).asURLs.flatMap: url =>
        try
          if url.getProtocol == "file" then Some(Paths.get(url.toURI).toString)
          else None
        catch case NonFatal(_) => None
      val materialized = entries.distinct.mkString(JFile.pathSeparator)
      if materialized.nonEmpty then materialized else ctx.settings.classpath.value(using ctx)
  end SessionCompileInputs

  private def currentSessionInputs(): SessionCompileInputs =
    val state = currentState
    if state == null then
      throw new IllegalStateException("Dynamic evaluation has no current REPL state")
    new SessionCompileInputs(state)

  /** Compile an `eval` body with a fresh driver because the active compiler run
   *  is not re-entrant. [[SessionCompileInputs]] supplies the live session's
   *  classes, imports, settings, and classpath to that isolated compilation.
   */
  private def evalDynamic(
      code: String,
      bindings: Array[Eval.Binding],
      expectedType: String,
      enclosingSource: String
  ): Either[Eval.CompileFailure, Any] =
    val inputs = currentSessionInputs()
    isolatedEvalAdapter.evalIsolated(
      code,
      inputs.classLoader,
      bindings,
      inputs.outputDir,
      inputs.sessionImports,
      inputs.forwardedSettings,
      expectedType,
      enclosingSource,
      inputs.classpath,
      inputs.evalLogDir
    )
  end evalDynamic

  /** Compile top-level definitions against the live session with a fresh driver. */
  private def compileTopLevelDynamic(
      defs: String,
      contextHeader: String
  ): Either[Eval.CompileFailure, Eval.TopLevel] =
    val inputs = currentSessionInputs()
    isolatedEvalAdapter.compileTopLevel(
      defs,
      inputs.classLoader,
      inputs.outputDir,
      inputs.sessionImports,
      inputs.forwardedSettings,
      contextHeader,
      inputs.classpath,
      inputs.evalLogDir
    )
  end compileTopLevelDynamic

  // TODO: i5069
  final def bind(name: String, value: Any)(using state: State): State = state

  /**
   * Controls whether the `System.out` and `System.err` streams are set to the provided constructor parameter instance
   * of [[java.io.PrintStream]] during the execution of the repl. On by default.
   *
   * Disabling this can be beneficial when executing a repl instance inside a concurrent environment, for example a
   * thread pool (such as the Scala compile server in the Scala Plugin for IntelliJ IDEA).
   *
   * In such environments, indepently executing `System.setOut` and `System.setErr` without any synchronization can
   * lead to unpredictable results when restoring the original streams (dependent on the order of execution), leaving
   * the Java process in an inconsistent state.
   */
  protected def redirectOutput: Boolean = true

  // redirecting the output allows us to test `println` in scripted tests
  private def withRedirectedOutput(op: => State): State = {
    if redirectOutput then
      val savedOut = System.out
      val savedErr = System.err
      try {
        System.setOut(replOut)
        System.setErr(replOut)
        op
      }
      finally {
        System.setOut(savedOut)
        System.setErr(savedErr)
      }
    else op
  }

  private def newRun(state: State, reporter: StoreReporter = newStoreReporter) = {
    val run = compiler.newRun(rootCtx.fresh.setReporter(reporter), state)
    state.copy(context = run.runContext)
  }

  /** Enable a language feature for subsequent REPL and dynamic compilations. */
  private def enableLanguageFeature(feature: String): Unit =
    val option = s"-language:$feature"
    val summary = rootCtx.settings.processArguments(List(option), true, rootCtx.settingsState)
    rootCtx = rootCtx.fresh.setSettings(summary.sstate)
    if !effectiveSettings.contains(option) then effectiveSettings :+= option

  /** Detect global language imports in parsed trees and enable them in rootCtx
   *  so subsequent parses and compilations see them (i16250).
   */
  private def propagateLanguageImports(trees: List[untpd.Tree]): Unit =
    import dotc.core.NameKinds.QualifiedName
    for case untpd.Import(expr, selectors) <- trees do
      untpd.languageImport(expr) match
        case Some(prefix) =>
          for case untpd.ImportSelector(untpd.Ident(imported), untpd.EmptyTree, _) <- selectors do
            val qual = QualifiedName(prefix, imported.asTermName)
            if Feature.globalLanguageImports.contains(qual) then
              enableLanguageFeature(qual.toString)
        case _ =>

  private def stripBackTicks(label: String) =
    if label.startsWith("`") && label.endsWith("`") then
      label.drop(1).dropRight(1)
    else
      label

  /** Extract possible completions at the index of `cursor` in `expr` */
  protected final def completions(cursor: Int, expr: String, state0: State): List[Completion] =
    completions(cursor, expr, state0, replOut.println(_))

  private def completions(
    cursor: Int,
    expr: String,
    state0: State,
    displayLoadingErrors: String => Unit
  ): List[Completion] =
    if expr.startsWith(":") then
      ReplCommands.names.collect:
        case command if command.startsWith(expr) => Completion(command, "", List())
    else
      val typecheckReporter = newStoreReporter
      given state: State = newRun(state0)
      val result = compiler
        .typeCheck(expr, errorsAllowed = true, reporter = typecheckReporter)
        .map { (untpdTree, tpdTree) =>
          val file = SourceFile.virtual("<completions>", expr, maybeIncomplete = true)
          val unit = CompilationUnit(file)(using state.context)
          unit.untpdTree = untpdTree
          unit.tpdTree = tpdTree
          given Context = state.context.fresh.setCompilationUnit(unit)
          val srcPos = SourcePosition(file, Span(cursor))
          try
            Completion.completions(srcPos)._2
          catch case NonFatal(_) =>
            List(Completion("<Error while fetching completions. Please report it to the Scala 3 maintainers at https://github.com/scala/scala3/issues>", "", Nil))
        }
        .getOrElse(Nil)
      // Candidate discovery can load symbols unrelated to the qualifier. Discard its
      // diagnostics; failed loads are retained and reported if the symbol is requested later.
      state.context.reporter.removeBufferedMessages(using state.context)
      val loadingErrors = typecheckReporter.removeBufferedMessages(using state.context).collect:
        case error: Diagnostic.LoadingError => error
      if loadingErrors.nonEmpty then
        given Context = state.context
        displayLoadingErrors(loadingErrors.map(ReplConsoleReporter.messageAndPos).mkString("\n"))
      result
  end completions

  protected def interpret(res: ParseResult)(using state: State): State =
    val historyFile =
      if historySuppressed then ""
      else state.context.settings.XreplHistoryFile.value(using state.context)
    ReplHistory.captureLine(replOut, historyFile, parseResultInput(res))(interpretImpl(res))

  /** Recover input text for the transcript. Parsed commands retain their
   *  arguments but not the user's exact alias or spacing, so command entries
   *  use a canonical spelling. Results without input return an empty string.
   */
  private def parseResultInput(res: ParseResult): String = res match
    case p: Parsed                  => p.source.content().mkString
    case s: SyntaxErrors            => s.sourceCode
    case CommandThenCode(cmd, code) => s"${commandInput(cmd)}\n$code"
    case cmd: Command               => commandInput(cmd)
    case _                          => ""

  private def commandInput(cmd: Command): String =
    def withArg(name: String, arg: String) =
      if arg.isEmpty then name else s"$name $arg"
    cmd match
      case UnknownCommand(c)      => c
      case AmbiguousCommand(c, _) => c
      case Dep(dep)               => withArg(Dep.command, dep)
      case Save(path)             => withArg(Save.command, path)
      case Load(path)             => withArg(Load.command, path)
      case Require(path)          => withArg(Require.command, path)
      case JarCmd(path)           => withArg(JarCmd.command, path)
      case ToolkitCmd(coords)     => withArg(ToolkitCmd.command, coords)
      case RepoCmd(repositories)  => withArg(RepoCmd.command, repositories)
      case KindOf(expr)           => withArg(KindOf.command, expr)
      case TypeOf(expr)           => withArg(TypeOf.command, expr)
      case DocOf(expr)            => withArg(DocOf.command, expr)
      case Settings(arg)          => withArg(Settings.command, arg)
      case Reset(arg)             => withArg(Reset.command, arg)
      case Replay(arg)            => withArg(Replay.command, arg)
      case Sh(expr)               => withArg(Sh.command, expr)
      case Imports                => Imports.command
      case Paste                  => Paste.command
      case Silent                 => Silent.command
      case Quit                   => Quit.command
      case Help                   => Help.command

  private def interpretImpl(res: ParseResult)(using state: State): State = {
    currentState = state
    val newState = res match {
      case parsed: Parsed =>
        for diag <- parsed.directiveDiagnostics do
          replOut.println(s"[warn] ${diag.message}")
        val src = parsed.source.content().mkString
        val classified = ReplDirectives.classify(src)
        if classified.hasDirectives then
          val stateAfterDirectives = interpretDirectives(classified)
          if parsed.trees.nonEmpty then
            propagateLanguageImports(parsed.trees)
            compile(parsed, stateAfterDirectives)
          else stateAfterDirectives.recordInput(src.strip)
        else if parsed.trees.nonEmpty then
          propagateLanguageImports(parsed.trees)
          compile(parsed, state)
        else state

      case SyntaxErrors(_, errs, _) =>
        // if there is a Run that is tracking suspended parse warnings, ignore (drop) them when erroring
        val run = state.context.run
        if run != null then
          run.suppressions.initSuspendedMessages(oldRun = null)
        displayErrors(errs, state)

      case CommandThenCode(cmd, code) =>
        val stateAfterCommand = interpretCommand(cmd)
        val recorded = cmd.replayLine.fold(stateAfterCommand)(line => stateAfterCommand.recordInput(line.strip))
        interpret(ParseResult(code)(using recorded))(using recorded)

      case MixedCommandsAndDirectives =>
        replOut.println(
          """Cannot mix `:` commands and `//> using` directives in the same REPL input.
            |Submit them as separate inputs.""".stripMargin)
        state

      case cmd: Command =>
        val next = interpretCommand(cmd)
        cmd.replayLine.fold(next)(line => next.recordInput(line.strip))

      case SigKill => // TODO
        state

      case _ => // new line, empty tree
        state
    }
    currentState = newState
    newState
  }

  /** Compile `parsed` trees and evolve `state` in accordance */
  private def compile(parsed: Parsed, istate: State): State = {
    def extractNewestWrapper(tree: untpd.Tree): Name = tree match {
      case PackageDef(_, (obj: untpd.ModuleDef) :: Nil) => obj.name.moduleClassName
      case _ => nme.NO_NAME
    }

    def extractTopLevelImports(ctx: Context): List[tpd.Import] =
      unfusedPhases(using ctx).collectFirst { case phase: CollectTopLevelImports => phase.imports }.get

    def contextWithNewImports(ctx: Context, imports: List[tpd.Import]): Context =
      if imports.isEmpty then ctx
      else
        imports.foldLeft(ctx.fresh.setNewScope)((ctx, imp) =>
          ctx.importContext(imp, imp.symbol(using ctx)))

    given State = {
      val state0 = newRun(istate, parsed.reporter)
      state0.copy(context = state0.context.withSource(parsed.source))
    }
    compiler
      .compile(parsed)
      .fold(
        (errs, errState) =>
          displayErrors(errs, errState)
          istate.afterFailedCompilation(errState.objectIndex)
        ,
        (unit, newState) =>
          val newestWrapper = extractNewestWrapper(unit.untpdTree)
          val newImports = extractTopLevelImports(newState.context)
          var allImports = newState.imports
          if (newImports.nonEmpty)
            allImports += (newState.objectIndex -> newImports)
          val newStateWithImports = newState.copy(
            imports = allImports,
            context = contextWithNewImports(newState.context, newImports)
          )

          val warnings = newState.context.reporter
            .removeBufferedMessages(using newState.context)

          inContext(newState.context):
            // Rendering can execute an eval call from the newly compiled line,
            // so publish its wrapper and imports first. The callback compiles
            // with a separate driver and does not re-enter this run.
            currentState = newStateWithImports
            val (updatedState, definitions) =
              if (!ctx.settings.XreplDisableDisplay.value)
                renderDefinitions(unit.tpdTree, newestWrapper)(using newStateWithImports)
              else
                (newStateWithImports, Seq.empty)

            // output is printed in the order it was put in. warnings should be
            // shown before infos (e.g. typedefs) for the same line.
            // column ordering is mostly to make tests deterministic
            given Ordering[Diagnostic] =
              Ordering[(Int, Int, Int)].on(d => (d.pos.line, -d.level, d.pos.column))

            (if istate.quiet then warnings else definitions ++ warnings)
              .sorted
              .foreach(printDiagnostic)

            if updatedState.invalidObjectIndexes.contains(updatedState.objectIndex) then updatedState
            else updatedState.recordInput(parsed.source.content().mkString)
      )
  }

  private def renderDefinitions(tree: tpd.Tree, newestWrapper: Name)(using state: State): (State, Seq[Diagnostic]) = {
    given Context = state.context

    def resAndUnit(denot: Denotation)(using Context) = {
      import scala.util.{Success, Try}
      val sym = denot.symbol
      val name = sym.name.show
      val hasValidNumber = Try(name.drop(3).toInt) match {
        case Success(num) => num < state.valIndex
        case _ => false
      }
      name.startsWith(str.REPL_RES_PREFIX) && hasValidNumber && sym.info == defn.UnitType
    }

    def extractAndFormatMembers(symbol: Symbol)(using Context): (State, Seq[Diagnostic]) = if (tree.symbol.info.exists) {
      val info = symbol.info
      val defs =
        info.bounds.hi.finalResultType
          .membersBasedOnFlags(required = Method, excluded = Accessor | ParamAccessor | Synthetic | Private)
          .filterNot { denot =>
            defn.topClasses.contains(denot.symbol.owner) || denot.symbol.isConstructor
             || denot.symbol.name.is(DefaultGetterName)
          }

      val vals =
        info.fields
          .filterNot(_.symbol.isOneOf(ParamAccessor | Private | Synthetic | Artifact | Module))
          .filter(_.symbol.name.is(SimpleNameKind))

      val typeAliases =
        info.bounds.hi.typeMembers.filter(_.symbol.info.isTypeAlias)

      // The wrapper object may fail to initialize if the rhs of a ValDef throws.
      // In that case, don't attempt to render any subsequent vals, and mark this
      // wrapper object index as invalid.
      var failedInit = false
      val renderedVals =
        val buf = mutable.ListBuffer[Diagnostic]()
        for d <- vals do if !failedInit then rendering.renderVal(d) match
          case Right(Some(v)) =>
            buf += v
          case Left(e) =>
            buf += rendering.renderError(e, d)
            failedInit = true
          case _ =>
        buf.toList

      if failedInit then
        // We limit the returned diagnostics here to `renderedVals`, which will contain the rendered error
        // for the val which failed to initialize. Since any other defs, aliases, imports, etc. from this
        // input line will be inaccessible, we avoid rendering those so as not to confuse the user.
        (state.invalidateCurrentObject, renderedVals)
      else
        val formattedMembers =
          typeAliases.map(rendering.renderTypeAlias)
          ++ defs.map(rendering.renderMethod)
          ++ renderedVals
        val diagnostics = if formattedMembers.isEmpty then rendering.forceModule(symbol) else formattedMembers
        (state.copy(valIndex = state.valIndex - vals.count(resAndUnit)), diagnostics)
    }
    else (state, Seq.empty)

    def isSyntheticCompanion(sym: Symbol) =
      sym.is(Module) && sym.is(Synthetic)

    def typeDefs(sym: Symbol)(using Context): Seq[Diagnostic] = sym.info.memberClasses
      .collect {
        case x if !isSyntheticCompanion(x.symbol) && !x.symbol.name.isReplWrapperName =>
          rendering.renderTypeDef(x)
      }

    val renderPhase =
      if Feature.ccEnabledSomewhere && checkCapturesPhase.exists
      then checkCapturesPhase
      else typerPhase.next
    atPhase(renderPhase) {
      // Display members of wrapped module:
      tree.symbol.info.memberClasses
        .find(_.symbol.name == newestWrapper.moduleClassName)
        .map { wrapperModule =>
          val (newState, formattedMembers) = extractAndFormatMembers(wrapperModule.symbol)
          val formattedTypeDefs =  // don't render type defs if wrapper initialization failed
            if newState.invalidObjectIndexes.contains(state.objectIndex) then Seq.empty
            else typeDefs(wrapperModule.symbol)
          (newState, formattedTypeDefs ++ formattedMembers)
        }
        .getOrElse {
          // user defined a trait/class/object, so no module needed
          (state, Seq.empty)
        }
    }
  }

  /** Replay a file saved by `:save`: each entry runs as its own compilation unit. */
  private def loadSavedEntries(contents: String, state: State): State =
    val separatorLine = s"(?m)^${Pattern.quote(Save.entrySeparator)}$$"
    val entries = contents.stripPrefix(Save.sessionHeader).split(separatorLine).toList
      .map(_.strip).filter(_.nonEmpty)
    replayEntries(entries, state)

  private def replayEntries(entries: List[String], state: State): State =
    entries.foldLeft(state) { (st, entry) =>
      if ParseResult.isCommand(entry) then interpret(ParseResult(entry)(using st))(using st)
      else run(entry)(using st)
    }

  /** Interpret `cmd` to action and propagate potentially new `state` */
  private def interpretCommand(cmd: Command)(using state: State): State = cmd match {
    case UnknownCommand(cmd) =>
      replOut.println(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      state

    case AmbiguousCommand(cmd, matching) =>
      replOut.println(s""""$cmd" matches ${matching.mkString(", ")}. Try typing a few more characters. Run ":help" for a list of commands""")
      state

    case Help =>
      replOut.println(Help.text)
      state

    case Reset(arg) =>
      val tokens = tokenize(arg)

      if tokens.nonEmpty then
        replOut.println(s"""|Resetting REPL state with the following settings:
                        |  ${tokens.mkString("\n  ")}
                        |""".stripMargin)
      else
        replOut.println("Resetting REPL state.")

      resetSession(tokens)

    case Replay(arg) =>
      val tokens = tokenize(arg)

      if tokens.nonEmpty then
        replOut.println(s"""|Replaying REPL session with the following settings:
                        |  ${tokens.mkString("\n  ")}
                        |""".stripMargin)
      else
        replOut.println("Replaying REPL session.")

      replayEntries(state.pastInputs.reverse, resetSession(tokens))

    case Imports =>
      for {
        objectIndex <- state.validObjectIndexes
        imp <- state.imports.getOrElse(objectIndex, Nil)
      } replOut.println(imp.show(using state.context))
      state

    case Save(path) =>
      if path.isEmpty then
        replOut.println("File name is required.")
      else if state.pastInputs.isEmpty then
        replOut.println("Nothing to save.")
      else
        try
          val body = state.pastInputs.reverse.map(entry => s"${Save.entrySeparator}\n$entry").mkString("\n")
          val content = s"${Save.sessionHeader}\n$body"
          Files.writeString(new JFile(path).toPath, content, StandardCharsets.UTF_8)
        catch case NonFatal(e) =>
          replOut.println(s"""Couldn't save session to "$path": ${e.getMessage}""")
      state

    case Load(path) =>
      val file = new JFile(path)
      if (file.exists) {
        val contents = Using(scala.io.Source.fromFile(file, StandardCharsets.UTF_8.name))(_.mkString).get
        val loaded =
          if contents.linesIterator.nextOption().contains(Save.sessionHeader) then loadSavedEntries(contents, state)
          else run(contents)(using state)
        loaded.copy(pastInputs = state.pastInputs)
      }
      else {
        replOut.println(s"""Couldn't find file "${file.getCanonicalPath}"""")
        state
      }

    case Require(path) =>
      replOut.println(":require is no longer supported, but has been replaced with :jar. Please use :jar")
      state

    case JarCmd(path) =>
      val jarFile = AbstractFile.getDirectory(path, state.context.settings.javaOutputVersion.value(using state.context))
      if (jarFile == null)
        replOut.println(s"""Cannot add "$path" to classpath.""")
        state
      else
        def flatten(f: AbstractFile): Iterator[AbstractFile] =
          if (f.isClassContainer) f.iterator.flatMap(flatten)
          else Iterator(f)

        def tryClassLoad(classFile: AbstractFile): Option[String] = {
          val input = classFile.input
          try {
            val reader = new ClassReader(input)
            val clsName = reader.getClassName.replace('/', '.')
            rendering.myClassLoader.loadClass(clsName)
            Some(clsName)
          } catch
            case _: ClassNotFoundException => None
          finally {
            input.close()
          }
        }

        try {
          val entries = flatten(jarFile)

          val existingClass = entries.filter(_.ext.isClass).find(tryClassLoad(_).isDefined)
          if (existingClass.nonEmpty)
            replOut.println(s"The path '$path' cannot be loaded, it contains a classfile that already exists on the classpath: ${existingClass.get}")
          else inContext(state.context):
            val jarClassPath = ClassPathFactory.newClassPath(jarFile)
            val prevOutputDir = ctx.settings.outputDir.value

            // Add the JAR to the compiler classpath.
            ctx.platform.addToClassPath(jarClassPath)
            SymbolLoaders.mergeNewEntries(defn.RootClass, ClassPath.RootPackage, jarClassPath, ctx.platform.classPath)

            // Use the expanded classpath for code compiled after this command.
            val prevClassLoader = rendering.classLoader()
            val jarClassLoader = fromURLsParallelCapable(
              jarClassPath.asURLs, prevClassLoader)
            rendering.myClassLoader = new AbstractFileClassLoader(
              prevOutputDir,
              jarClassLoader,
              InterruptInstrumentation.fromString(ctx.settings.XreplInterruptInstrumentation.value),
              prevClassLoader
            )

            replOut.println(s"Added '$path' to classpath.")
        } catch {
          case e: Throwable =>
            replOut.println(s"Failed to load '$path' to classpath: ${e.getMessage}")
        }
        state

    case KindOf(expr) =>
      replOut.println(s"""The :kind command is not currently supported.""")
      state
    case TypeOf(expr) =>
      expr match
        case "" =>
          replOut.println(s":type <expression>")
          state
        case _  =>
          val queryState = newRun(state)
          try
            compiler.typeOf(expr)(using queryState).fold(
              errs => displayErrors(errs, queryState),
              res => replOut.println(res)  // result has some highlights
            )
          catch case NonFatal(ex) =>
            replOut.println(s"Error: ${ex.getMessage}")
          queryState

    case DocOf(expr) =>
      expr match
        case "" =>
          replOut.println(s":doc <expression>")
          state
        case _  =>
          val queryState = newRun(state)
          try
            compiler.docOf(expr)(using queryState).fold(
              errs => displayErrors(errs, queryState),
              res => replOut.println(res)
            )
          catch case NonFatal(ex) =>
            replOut.println(s"Error: ${ex.getMessage}")
          queryState

    case Sh(expr) =>
      replOut.println(s"""The :sh command is deprecated. Use `import scala.sys.process._` and `"command".!` instead.""")
      state

    case Paste =>
      replOut.println("The :paste command is deprecated. It is no longer needed, since the REPL supports multiline editing.")
      state

    case Settings(arg) => arg match
      case "" =>
        given ctx: Context = state.context
        for (s <- ctx.settings.userSetSettings(ctx.settingsState).sortBy(_.name))
          replOut.println(s"${s.name} = ${if s.value == "" then "\"\"" else s.value}")
        state
      case _  =>
        val tokens = tokenize(arg).toArray
        val (nextCtx, settingsAccepted) = setupRootCtx(tokens, rootCtx)
        rootCtx = nextCtx
        if settingsAccepted then effectiveSettings ++= tokens
        state.copy(context = rootCtx)

    case Silent => state.copy(quiet = !state.quiet)

    case Dep(dep) => resolveAndAddDeps(List(dep))

    case RepoCmd(repositories) => repositories.split("\\s+").filter(_.nonEmpty).toList match
      case Nil =>
        replOut.println(s"${RepoCmd.command} <url>|<alias> ...")
        state
      case repositoryStrings => addRepositories(repositoryStrings)

    case ToolkitCmd(coordinates) =>
      val singleValue = coordinates.split("\\s+").filter(_.nonEmpty).toList match
        case coords :: Nil => Some(coords)
        case _ => None
      singleValue.flatMap(ReplDirectives.toolkitCoordinates) match
        case Some(dependencies) =>
          replOut.println(ReplDirectives.Warning.NoSeparateTestScope.toString)
          resolveAndAddDeps(dependencies)
        case None =>
          replOut.println(
            s"""${ToolkitCmd.command} expects a single version or <flavor>:<version>.
               |Example: ${ToolkitCmd.command} default""".stripMargin)
          state

    case Quit =>
      // end of the world!
      state
  }

  private def interpretDirectives(classified: ReplDirectives.DirectiveClassification)(using state: State): State =
    import ReplDirectives.ReplDirective.*

    classified.warnings.foreach(warning => replOut.println(warning.toString))
    val dependencies = classified.directives.collect:
      case Dependency(coordinate) => coordinate
    val jars = classified.directives.collect:
      case Jar(path) => path
    val repositories = classified.directives.collect:
      case Repository(repository) => repository
    val stateWithRepositories = addRepositories(repositories)
    val stateWithDependencies = resolveAndAddDeps(dependencies)(using stateWithRepositories)
    jars.foldLeft(stateWithDependencies): (currentState, path) =>
      interpretCommand(JarCmd(path))(using currentState)

  private def addRepositories(repositoryStrings: List[String])(using state: State): State =
    repositoryStrings.foldLeft(state): (currentState, repositoryString) =>
      DependencyResolver.parseRepository(repositoryString) match
        case Some(repository) =>
          replOut.println(s"Added repository '$repositoryString'.")
          currentState.copy(repositories = (currentState.repositories :+ repository).distinct)
        case None =>
          replOut.println(s"Unable to parse repository '$repositoryString'.")
          currentState

  private def resolveAndAddDeps(depStrings: List[String])(using state: State): State =
    if depStrings.isEmpty then state
    else
      val deps = depStrings.flatMap(DependencyResolver.parseDependency)
      if deps.isEmpty then state
      else
        DependencyResolver.resolveDependencies(deps, state.repositories) match
          case Right(files) =>
            if files.nonEmpty then
              val classpathState = newRun(state)
              inContext(classpathState.context):
                val prevOutputDir = ctx.settings.outputDir.value
                val prevClassLoader = rendering.classLoader()
                rendering.myClassLoader = DependencyResolver.addToCompilerClasspath(
                  files,
                  prevClassLoader,
                  prevOutputDir
                )
                val depsDescription = if deps.size == 1 then "a dependency" else s"${deps.size} dependencies"
                replOut.println(s"Resolved $depsDescription (${files.size} JARs)")
              classpathState
            else state
          case Left(error) =>
            replOut.println(s"Error resolving dependencies: $error")
            state

  /** shows all errors nicely formatted */
  private def displayErrors(errs: Seq[Diagnostic], state: State): State = {
    errs.foreach(printDiagnostic(_)(using state))
    state
  }

  /** Like ConsoleReporter, but without file paths, -Xprompt displaying,
   *  and using a PrintStream rather than a PrintWriter so messages aren't re-encoded. */
  private object ReplConsoleReporter extends ConsoleReporter.AbstractConsoleReporter {
    override def posFileStr(pos: SourcePosition) = "" // omit file paths
    override def printMessage(msg: String): Unit = replOut.println(msg)
    override def echoMessage(msg: String): Unit  = printMessage(msg)
    override def flush()(using Context): Unit    = replOut.flush()
  }

  /** Print warnings & errors using ReplConsoleReporter, and info straight to replOut */
  private def printDiagnostic(dia: Diagnostic)(using state: State) = dia.level match
    case interfaces.Diagnostic.INFO => replOut.println(dia.msg) // print REPL's special info diagnostics directly to replOut
    case _                          => ReplConsoleReporter.doReport(dia)(using state.context)

end ReplDriver
object ReplDriver:
  def pprintImport = "import dotty.vendored.pprint.pprintln\n"
