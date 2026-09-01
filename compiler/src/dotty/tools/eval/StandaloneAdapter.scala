package dotty.tools
package eval

/** [[Eval.Adapter]] used by programs compiled with `-Xdynamic-eval` when no
 *  REPL driver has installed an adapter.
 *
 *  The nearest caller classloader supplies the program classes needed by the
 *  compiler classpath and generated wrapper.
 *
 *  Optional configuration is read from these system properties:
 *
 *    - `dotty.tools.eval.settings`: whitespace-separated compiler
 *      options forwarded to the inner compile (e.g.
 *      `-Yexplicit-nulls -language:experimental.captureChecking`).
 *      Values are split on whitespace, so options containing spaces are not
 *      supported. Use the dedicated properties for paths.
 *    - `dotty.tools.eval.classpath`: classpath for the inner
 *      compile. When unset, one is synthesized from the caller's
 *      classloader chain plus `java.class.path`.
 *    - `dotty.tools.eval.logDir`: per-invocation log directory
 *      (same files as the REPL's `-Xrepl-eval-log-dir`).
 */
private[eval] object StandaloneAdapter:

  val instance: Eval.Adapter = new Eval.Adapter:
    private val adapter = new EvalAdapter

    def evalCode(
        code: String,
        bindings: Array[Eval.Binding],
        expectedType: String,
        enclosingSource: String
    ): Either[Eval.CompileFailure, Any] =
      adapter.evalIsolated(
        code = code,
        classLoader = callerClassLoader(),
        bindings = bindings,
        replOutDir = null,
        replWrapperImports = Array.empty,
        compilerSettings = settingsFromProperty(),
        expectedType = expectedType,
        enclosingSource = enclosingSource,
        replClasspath = property("dotty.tools.eval.classpath"),
        evalLogDir = property("dotty.tools.eval.logDir"),
        standalone = true
      )

    override def compileTopLevel(
        defs: String,
        contextHeader: String
    ): Either[Eval.CompileFailure, Eval.TopLevel] =
      adapter.compileTopLevel(
        defs = defs,
        classLoader = callerClassLoader(),
        replOutDir = null,
        replWrapperImports = Array.empty,
        compilerSettings = settingsFromProperty(),
        contextHeader = contextHeader,
        replClasspath = property("dotty.tools.eval.classpath"),
        evalLogDir = property("dotty.tools.eval.logDir")
      )

  private def property(name: String): String =
    val v = System.getProperty(name)
    if v == null then "" else v

  private def settingsFromProperty(): Array[String] =
    val raw = property("dotty.tools.eval.settings").trim
    if raw.isEmpty then Array.empty[String]
    else raw.split("\\s+").nn.map(_.nn)

  /** Finds the nearest caller classloader, falling back to the thread context
   *  loader and then the eval infrastructure loader.
   */
  private def callerClassLoader(): ClassLoader =
    def isInfraFrame(className: String): Boolean =
      className.startsWith("dotty.tools.eval.")
        || className.startsWith("java.")
        || className.startsWith("jdk.")
    val walker = java.lang.StackWalker.getInstance(java.lang.StackWalker.Option.RETAIN_CLASS_REFERENCE)
    val found: ClassLoader | Null = walker.walk { frames =>
      frames.nn
        .map[ClassLoader | Null] { frame =>
          val cls = frame.nn.getDeclaringClass.nn
          if isInfraFrame(cls.getName.nn) then null else cls.getClassLoader
        }
        .filter(_ != null)
        .findFirst()
        .orElse(null)
    }
    if found != null then found
    else
      val ctxLoader = Thread.currentThread.nn.getContextClassLoader
      if ctxLoader != null then ctxLoader
      else getClass.getClassLoader.nn
