package dotty.tools
package repl
package eval

import java.util.UUID

import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.util.ClasspathFromClassloader
import dotty.tools.io.{AbstractFile, VirtualDirectory}

/** REPL-side implementation of [[Eval.Adapter]]. Wraps a call's
 *  inputs (code, classloader, bindings, REPL session imports +
 *  output dir, compiler settings, expected type, enclosing source)
 *  into an [[EvalCompilerConfig]], drives an [[EvalCompilerBridge]]
 *  compile, loads the synthesised `__Expression` class, instantiates
 *  it with the captured this + bindings, and calls `evaluate()`.
 *
 *  Distinguishing properties of this pipeline:
 *
 *    - The body is *tree-spliced* at the marker rather than text-
 *      substituted, so comments / string literals containing the
 *      marker substring don't get corrupted.
 *    - Each binding's source type comes from the typer
 *      ([[EvalTypeAnnotate]]), not from `value.getClass`. Soundness
 *      for path-dependent and abstract types is preserved.
 *    - The wrapper is a real class; each eval call instantiates a
 *      fresh `__Expression` and invokes its `evaluate()` method.
 *    - Identical call sites cache the compiled `__Expression` class.
 */
class EvalAdapter:

  /** Compile and run one eval call in-process. The REPL session's
   *  in-memory output dir is forwarded to the inner compile via
   *  [[EvalCompilerBridge.compile]] so `rs$line$N` modules are
   *  visible without being materialised to disk.
   *
   *  When called from the REPL driver, `replClasspath` should be
   *  `state.context.settings.classpath.value` — using the REPL's
   *  own classpath verbatim is safer than synthesising one from the
   *  classloader plus `java.class.path` (which can introduce
   *  duplicate-stdlib symbol-table corruption). The empty string
   *  falls back to the synthesised path used by tests / direct
   *  callers.
   */
  def evalIsolated(
      code: String,
      classLoader: ClassLoader,
      bindings: Array[Eval.Binding],
      replOutDir: AbstractFile | Null,
      replWrapperImports: Array[String],
      compilerSettings: Array[String],
      expectedType: String,
      enclosingSource: String,
      replClasspath: String = "",
      evalLogDir: String = ""
  ): Either[Eval.CompileFailure, Any] =
    // Per-invocation log files (when `-Xrepl-eval-log-dir` is set):
    // capture the enclosing source, the body the user submitted,
    // the synthesised wrapper module, and — on failure — the
    // diagnostics. Useful for inspecting what the eval driver
    // actually compiled. Failure-tolerant: any IO error in the
    // logger is reported once via stderr and the eval call
    // continues.
    val logTimestamp =
      if evalLogDir.isEmpty then ""
      else writeEvalLogStart(evalLogDir, enclosingSource, code)

    // The runtime nested-eval rewrite used to happen here: parse
    // `code`, rewrite inner eval calls, pretty-print the result back
    // to a string, and pass the string as `config.body`. The
    // pretty-printer is lossy on a few constructs (interpolated
    // strings, try/finally without catch, …), so the round-trip
    // would silently fall back to the un-rewritten body and drop
    // bindings on inner calls. The rewrite now runs as a tree
    // transform inside [[SpliceEvalBody]] (after the body parses).
    // We just hand the original `code` plus the captures (names of
    // bindings already in the outer scope) and the outer
    // enclosingSource through `EvalCompilerConfig`.
    val initialScope: Array[(String, Boolean)] =
      bindings.map(b => (b.name, b.isVar))

    // Cache key spans everything that affects the synthesised
    // bytecode: the body, its lexical context, the session imports,
    // the compiler settings, and the session classloader
    // (identity-based, scoping entries to one REPL session).
    //
    // The bindings *array contents* are NOT in the key — only the
    // names, captured via `bindingsKey`. The names appear in the body
    // as identifier references already, but a defensive fingerprint
    // helps catch the rare case where two call sites have the same
    // body+enclosing but the rewriter chose different captures. The
    // binding *values* differ across calls (that's the point of
    // caching) and aren't part of compile output.
    //
    // Skip caching when `enclosingSource` is empty: that's either a
    // direct caller (no rewriter) or a runtime-rewritten nested eval,
    // and we don't have a tight enough discriminator to cache safely.
    val cacheKey: EvalAdapter.WrapperKey | Null =
      if enclosingSource.isEmpty then null
      else EvalAdapter.WrapperKey(
        code = code,
        enclosingSource = enclosingSource,
        bindingsKey = EvalAdapter.bindingsFingerprint(bindings),
        importsKey = replWrapperImports.mkString("\n"),
        settingsKey = compilerSettings.mkString(" "),
        sessionLoader = classLoader
      )

    if cacheKey != null then
      EvalAdapter.cache.get(cacheKey) match
        case null => () // miss; fall through
        case Left(failure) => return Left(failure)
        case Right(compiled) => return invokeCached(compiled, bindings)

    val outputClassName = s"__EvalExpression_${UUID.randomUUID().toString.replace('-', '_')}"
    val wrapperName = s"__EvalWrapper_${UUID.randomUUID().toString.replace('-', '_')}"

    // Always import the `Eval.{eval, evalSafe}` bridge. It's needed for
    // any nested `eval(...)` call inside the body or alongside the
    // marker in the enclosing source, and harmless when the wrapper
    // never references `eval`.
    val evalImport = "import dotty.tools.repl.eval.Eval.{eval, evalSafe}\n"
    val importBlock =
      if replWrapperImports.isEmpty then evalImport
      else evalImport + replWrapperImports.mkString("", "\n", "\n")

    // Wrap the enclosing source in a synthesised object so the
    // top-level statements (typically a `def` or `val`) become valid
    // module-body members. The marker stays as an Ident for the
    // tree-level splice phase.
    val wrappedSource =
      s"""${importBlock}object $wrapperName {
         |$enclosingSource
         |}
         |""".stripMargin

    val outDir = new VirtualDirectory("<eval-output>")
    val config = EvalCompilerConfig(
      outputClassName = outputClassName,
      body = code,
      expectedType = expectedType,
      initialScope = initialScope,
      outerEnclosingSource = enclosingSource,
      evalLogDir = evalLogDir,
      evalLogTimestamp = logTimestamp
    )

    val bridge = EvalCompilerBridge()
    bridge.compile(wrappedSource, outDir, classLoader, replOutDir, compilerSettings, replClasspath, config) match
      case Left(errors) =>
        // Splice the body into the placeholder for display: a wrapper
        // showing `def f(...) = __evalBodyPlaceholder__` is opaque
        // about which body the user submitted.
        val displaySource = EvalAdapter.spliceBodyForDisplay(wrappedSource, code)
        val failure = new Eval.CompileFailure(errors.toArray, displaySource)
        if logTimestamp.nonEmpty then writeEvalLogError(evalLogDir, logTimestamp, failure)
        // Don't cache transient compiler-internal errors — a stray
        // classpath blip or assertion failure inside dotc shouldn't
        // become a permanent "this body doesn't compile".
        val isTransient = errors.exists(_.startsWith("Internal compiler error:"))
        if cacheKey != null && !isTransient then EvalAdapter.cache.put(cacheKey, Left(failure))
        Left(failure)
      case Right(()) =>
        val compiled = loadCompiled(outDir, classLoader, outputClassName)
        if cacheKey != null then EvalAdapter.cache.put(cacheKey, Right(compiled))
        invokeCached(compiled, bindings)

  /** Load `__Expression` from the in-memory output dir and snapshot
   *  its constructor + `evaluate` method. Uses
   *  [[AbstractFileClassLoader]] rooted at `outDir` with the session
   *  classloader as parent, so `Eval.Binding`/`VarRef` resolve to
   *  the same `Class` objects on both sides of the loader boundary.
   *
   *  The returned [[EvalAdapter.CompiledExpression]] is cacheable:
   *  it pins the classloader (so the class can't be unloaded) and
   *  the reflected handles are reused across invocations.
   */
  private def loadCompiled(
      outDir: AbstractFile,
      parent: ClassLoader,
      outputClassName: String
  ): EvalAdapter.CompiledExpression =
    val cl = new AbstractFileClassLoader(outDir, parent)
    val cls = cl.loadClass(outputClassName)
    val ctor = cls.getDeclaredConstructor(classOf[Object], classOf[Array[Eval.Binding]])
    val evaluate = cls.getMethod("evaluate")
    new EvalAdapter.CompiledExpression(cls, ctor, evaluate, cl)

  /** Instantiate the cached expression class with the call's bindings
   *  and invoke `evaluate()`. Body runtime exceptions surface as their
   *  cause (reflection wraps them in `InvocationTargetException`).
   */
  private def invokeCached(
      compiled: EvalAdapter.CompiledExpression,
      bindings: Array[Eval.Binding]
  ): Either[Eval.CompileFailure, Any] =
    val thisObject = extractThisObject(bindings)
    val instance = compiled.ctor.newInstance(thisObject, bindings).asInstanceOf[AnyRef]
    try Right(compiled.evaluate.invoke(instance))
    catch case e: java.lang.reflect.InvocationTargetException =>
      val cause = e.getCause
      if cause != null then throw cause else throw e

  /** Write the per-invocation log files for an eval call:
   *
   *    - `eval_<timestamp>_enclosingSource.scala`: the source of the
   *      enclosing top-level statement at the call site, with the
   *      eval call's span replaced by a placeholder. Useful for
   *      replaying the call's lexical context.
   *    - `eval_<timestamp>_code.scala`: the body string the user
   *      submitted to `eval(...)`.
   *
   *  Returns the timestamp used in the filenames so the wrapper /
   *  error log files (written later) can share it. Returns the
   *  empty string when logging fails for any reason — we don't want
   *  logging IO errors to fail the eval call itself.
   */
  private def writeEvalLogStart(evalLogDir: String, enclosingSource: String, code: String): String =
    try
      val dir = new java.io.File(evalLogDir)
      if !dir.exists then dir.mkdirs()
      val now = java.time.Instant.now()
      val ts = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss-SSS")
        .withZone(java.time.ZoneId.systemDefault())
        .format(now)
      val unique = s"$ts-${java.util.UUID.randomUUID.toString.take(8)}"
      java.nio.file.Files.write(
        new java.io.File(dir, s"eval_${unique}_enclosingSource.scala").toPath,
        enclosingSource.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      )
      java.nio.file.Files.write(
        new java.io.File(dir, s"eval_${unique}_code.scala").toPath,
        code.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      )
      unique
    catch case scala.util.control.NonFatal(e) =>
      System.err.println(s"[eval-log] WARNING: failed to write log files under '$evalLogDir': ${e.getMessage}")
      ""

  /** Write the diagnostic + offending source that the eval driver's
   *  compile rejected. Only fires on compile failure — the success
   *  path doesn't produce an error log.
   */
  private def writeEvalLogError(evalLogDir: String, timestamp: String, failure: Eval.CompileFailure): Unit =
    try
      val sb = new StringBuilder
      sb ++= "// errors:\n"
      failure.errors.foreach { e =>
        sb ++= "//   "
        sb ++= e.replace("\n", "\n//   ")
        sb ++= "\n"
      }
      sb ++= "// generated source:\n"
      sb ++= failure.source
      java.nio.file.Files.write(
        new java.io.File(evalLogDir, s"eval_${timestamp}_error.scala").toPath,
        sb.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      )
    catch case scala.util.control.NonFatal(_) => ()

  /** Pull the `__this__` binding out of the bindings array. The
   *  parser-stage rewriter inserts it whenever the eval call sits
   *  inside a class method, so its presence indicates the captured
   *  outer instance.
   */
  private def extractThisObject(bindings: Array[Eval.Binding]): AnyRef | Null =
    val it = bindings.iterator
    while it.hasNext do
      val b = it.next()
      if b.name == "__this__" then return b.value.asInstanceOf[AnyRef]
    null

end EvalAdapter

object EvalAdapter:

  /** Names that trigger nested-eval parsing. The textual fast path
   *  in [[mightContainNestedEval]] only parses bodies that contain
   *  one of these as a candidate call.
   */
  private[eval] val NestedEvalNames: Array[String] = Array("eval", "evalSafe", "agent", "agentSafe")

  /** Replace the first `__evalBodyPlaceholder__` in `wrappedSource`
   *  with `body` so compile-error messages show the actual code that
   *  triggered the failure rather than the placeholder. We replace
   *  only the first occurrence: the placeholder marks the splice
   *  site, while later occurrences (rare) might be inside a string
   *  literal in the body itself.
   */
  private[eval] def spliceBodyForDisplay(wrappedSource: String, body: String): String =
    val marker = EvalContext.placeholder
    val idx = wrappedSource.indexOf(marker)
    if idx < 0 then wrappedSource
    else wrappedSource.substring(0, idx) + body + wrappedSource.substring(idx + marker.length)

  /** Cache key for a compiled `__Expression` class. Two calls with
   *  the same key produce identical wrapper bytecode (modulo the
   *  UUID-named output class), so the second reuses the first's
   *  loaded class + reflected handles.
   *
   *  `sessionLoader` scopes entries to the running REPL session by
   *  reference identity: two sessions can share imports + settings
   *  yet have different `rs$line$N` definitions, so we treat loaders
   *  as opaque to prevent cross-session bleed.
   */
  private[eval] case class WrapperKey(
      code: String,
      enclosingSource: String,
      bindingsKey: String,
      importsKey: String,
      settingsKey: String,
      sessionLoader: ClassLoader
  )

  /** Cached output of a successful compile. Holds strong refs to the
   *  classloader + Class so the JVM can't unload them while the entry
   *  is live. The reflected ctor + `evaluate` method are snapshotted
   *  once at compile time and reused.
   */
  private[eval] final class CompiledExpression(
      val cls: Class[?],
      val ctor: java.lang.reflect.Constructor[?],
      val evaluate: java.lang.reflect.Method,
      val classLoader: ClassLoader
  )

  /** Maximum number of distinct call sites we keep wrappers for. Each
   *  entry pins one classloader + one wrapper class, so this also
   *  bounds metaspace growth from caching.
   */
  private val cacheCapacity = 128

  /** Access-order LRU. `LinkedHashMap` with `accessOrder=true` reorders
   *  on every `get`, so all access must be synchronised; the
   *  `synchronizedMap` wrapper handles per-call locking, which is
   *  enough since we only do single `get` and `put` operations.
   */
  private[eval] val cache: java.util.Map[WrapperKey, Either[Eval.CompileFailure, CompiledExpression]] =
    java.util.Collections.synchronizedMap(
      new java.util.LinkedHashMap[WrapperKey, Either[Eval.CompileFailure, CompiledExpression]](16, 0.75f, true) {
        override def removeEldestEntry(
            eldest: java.util.Map.Entry[WrapperKey, Either[Eval.CompileFailure, CompiledExpression]]
        ): Boolean = size() > cacheCapacity
      }
    )

  /** Discard all cached wrappers. Useful for `:reset` and tests.
   *  Cached classloaders become unreachable and eligible for GC.
   */
  def clearCache(): Unit = cache.clear()

  /** Stable fingerprint of the binding *names* and *kinds* for cache
   *  keying. Binding values change across calls (that's the point of
   *  caching) and aren't part of compile output; names and kinds
   *  (val / var / given) are — they shape the wrapper's parameter
   *  clauses.
   */
  private[eval] def bindingsFingerprint(bindings: Array[Eval.Binding]): String =
    val sb = new StringBuilder
    var i = 0
    while i < bindings.length do
      if i > 0 then sb += ','
      sb ++= bindings(i).name
      sb += ':'
      val b = bindings(i)
      sb += (if b.isVar then 'v' else if b.isGiven then 'g' else 'l')
      i += 1
    sb.toString
