package dotty.tools
package eval

/** Self-initialising [[Eval.Adapter]] used when no driver has
 *  installed one via [[Eval.withAdapter]], i.e. when `eval(...)`
 *  runs inside an ordinary Scala program compiled with
 *  `-Xdynamic-eval`, outside any REPL session.
 *
 *  The inner compile's classloader is the *caller's* classloader
 *  (found by walking the stack past the eval infrastructure frames):
 *  by definition it can see the program's classes, which both the
 *  inner compile's classpath synthesis and the wrapper's parent
 *  loader need.
 *
 *  Optional configuration comes from system properties, since a
 *  compiled program no longer knows its compile-time flags:
 *
 *    - `dotty.tools.eval.settings`: whitespace-separated compiler
 *      options forwarded to the inner compile (e.g.
 *      `-Yexplicit-nulls -language:experimental.captureChecking`).
 *      Pass the same language options the program was compiled with
 *      so eval bodies are checked under the same rules.
 *    - `dotty.tools.eval.classpath`: classpath for the inner
 *      compile. When unset, one is synthesised from the caller's
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

  private def property(name: String): String =
    val v = System.getProperty(name)
    if v == null then "" else v

  private def settingsFromProperty(): Array[String] =
    val raw = property("dotty.tools.eval.settings").trim
    if raw.isEmpty then Array.empty[String]
    else raw.split("\\s+").nn.map(_.nn)

  /** Classloader of the nearest stack frame outside the eval
   *  infrastructure (and the JDK). Falls back to the thread context
   *  classloader, then to the loader of the eval infrastructure
   *  itself.
   */
  private def callerClassLoader(): ClassLoader =
    def isInfraFrame(className: String): Boolean =
      className.startsWith("dotty.tools.eval.")
        || className.startsWith("java.")
        || className.startsWith("jdk.")
    val walker = java.lang.StackWalker.getInstance(java.lang.StackWalker.Option.RETAIN_CLASS_REFERENCE)
    var found: ClassLoader | Null = null
    walker.forEach { frame =>
      if found == null then
        val cls = frame.nn.getDeclaringClass.nn
        if !isInfraFrame(cls.getName.nn) then
          val loader = cls.getClassLoader
          if loader != null then found = loader
    }
    val result = found
    if result != null then result
    else
      val ctxLoader = Thread.currentThread.nn.getContextClassLoader
      if ctxLoader != null then ctxLoader
      else getClass.getClassLoader.nn
