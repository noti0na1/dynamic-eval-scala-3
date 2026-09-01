/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools
package repl

import dotty.tools.dotc.config.ScalaSettings

import io.AbstractFile

import AbstractFileClassLoader.InterruptInstrumentation

object AbstractFileClassLoader:
  enum InterruptInstrumentation(val stringValue: String):
    case Disabled extends InterruptInstrumentation("false")
    case Enabled extends InterruptInstrumentation("true")
    case Local extends InterruptInstrumentation("local")

    def is(value: InterruptInstrumentation): Boolean = this == value
    def isOneOf(others: InterruptInstrumentation*): Boolean = others.contains(this)

  object InterruptInstrumentation:
    def fromString(string: String): InterruptInstrumentation = string match {
      case "false" => Disabled
      case "true" => Enabled
      case "local" => Local
      case _ => throw new IllegalArgumentException(s"Invalid interrupt instrumentation value: $string")
    }

class AbstractFileClassLoader(
    root: AbstractFile,
    parent: ClassLoader,
    interruptInstrumentation: InterruptInstrumentation,
    previousSessionLoaderHint: AbstractFileClassLoader | Null
)
  extends io.AbstractFileClassLoader(root, parent):

  /** Retains the original three-argument constructor. A direct output loader
   *  parent is recognized automatically as the preceding REPL loader.
   */
  def this(
      root: AbstractFile,
      parent: ClassLoader,
      interruptInstrumentation: InterruptInstrumentation
  ) = this(root, parent, interruptInstrumentation, null)

  private val knownPreviousSessionLoader: AbstractFileClassLoader | Null =
    if previousSessionLoaderHint != null then previousSessionLoaderHint
    else parent match
      case previous: AbstractFileClassLoader => previous
      case _ => null

  /** The previous output loader in this REPL session. Classpath additions can
   *  insert URL class loaders between two output loaders.
   */
  private lazy val previousSessionLoader: AbstractFileClassLoader | Null =
    if knownPreviousSessionLoader != null then knownPreviousSessionLoader
    else
      var loader = getParent
      var previous: AbstractFileClassLoader | Null = null
      while loader != null && previous == null do
        loader match
          case outputLoader: AbstractFileClassLoader => previous = outputLoader
          case _ => loader = loader.getParent
      previous

  private def classNamesUnder(dir: AbstractFile, packagePrefix: String): Iterator[String] =
    dir.iterator.flatMap: file =>
      if file.isDirectory then classNamesUnder(file, packagePrefix + file.name + ".")
      else if file.name.endsWith(".class") then
        Iterator.single(packagePrefix + file.name.stripSuffix(".class"))
      else Iterator.empty

  /** Classes already present when this loader was created.
   *
   *  `:jar` and `:dep` keep the output directory but replace its loader. The
   *  snapshot routes existing classes through the previous loader to preserve
   *  their identity; classes compiled later remain in this loader and can link
   *  against the expanded classpath.
   */
  private val inheritedClasses: Set[String] =
    knownPreviousSessionLoader match
      case previous: AbstractFileClassLoader if previous.root == root =>
        classNamesUnder(root, "").toSet
      case _ => Set.empty

  def this(root: AbstractFile, parent: ClassLoader) =
    this(root, parent, InterruptInstrumentation.fromString(ScalaSettings.XreplInterruptInstrumentation.default), null)

  override protected def defineClass(name: String, bytes: Array[Byte]): Class[?] =
    if interruptInstrumentation.is(InterruptInstrumentation.Enabled) then defineClassInstrumented(name, bytes)
    else super.defineClass(name, bytes)

  private def defineClassInstrumented(name: String, originalBytes: Array[Byte]) = {
    val instrumentedBytes = ReplBytecodeInstrumentation.instrument(originalBytes)
    defineClass(name, instrumentedBytes, 0, instrumentedBytes.length)
  }

  override def loadClass(name: String): Class[?] =
    getClassLoadingLock(name).synchronized:
      val loaded = findLoadedClass(name) // Check if already loaded
      if loaded != null then return loaded

      // Route every class from an earlier epoch through its original loader.
      // This includes line wrappers and user classes nested within them.
      if inheritedClasses.contains(name) then
        previousSessionLoader match
          case previous: AbstractFileClassLoader => return previous.loadClass(name)
          case null => ()

      name match {
      // Keep compiler and runtime interfaces in their parent loaders so values
      // crossing the dynamic evaluation boundary retain the same class identities.
      case "dotty.tools.repl.StopRepl"
          if !interruptInstrumentation.is(InterruptInstrumentation.Enabled) =>
        super.loadClass(name)
      case s"dotty.tools.repl.$_" if name != "dotty.tools.repl.StopRepl" =>
        classOf[AbstractFileClassLoader].getClassLoader.loadClass(name)
      case s"dotty.tools.eval.$_" =>
        classOf[AbstractFileClassLoader].getClassLoader.loadClass(name)
      case s"scala.$_" => super.loadClass(name)
      case s"dotty.$_" if name != "dotty.tools.repl.StopRepl" => super.loadClass(name)
      case s"rs$$line$$$_" =>
        // New wrappers must link against this epoch's classpath.
        try findClass(name)
        catch case _: ClassNotFoundException => super.loadClass(name)

      case _ if interruptInstrumentation.isOneOf(InterruptInstrumentation.Disabled, InterruptInstrumentation.Local) =>
        super.loadClass(name)

      // Don't instrument JDK classes. These are often restricted to load from a single classloader
      // due to the JDK module system, and so instrumenting them and loading the modified copy of the class
      // results in runtime exceptions
      case s"java.$_" => super.loadClass(name)
      case s"javax.$_" => super.loadClass(name)
      case s"sun.$_" => super.loadClass(name)
      case s"jdk.$_" => super.loadClass(name)
      case s"org.xml.sax.$_" => super.loadClass(name) // XML SAX API (part of java.xml module)
      case s"org.w3c.dom.$_" => super.loadClass(name) // W3C DOM API (part of java.xml module)
      case s"com.sun.org.apache.$_" => super.loadClass(name) // Internal Xerces implementation
      // Instrumented bytecode calls StopRepl, so instrumenting StopRepl would
      // recurse. Independent sessions define separate copies, while output
      // loaders from the same session share one stop flag.
      case "dotty.tools.repl.StopRepl" =>
        previousSessionLoader match
          case previous: AbstractFileClassLoader => previous.loadClass(name)
          case null =>
            val classFileName = name.replace('.', '/') + ".class"
            val is = Option(getParent.getResourceAsStream(classFileName))
              // The loader that defined this class also has access to StopRepl.
              .getOrElse(classOf[AbstractFileClassLoader].getClassLoader.getResourceAsStream(classFileName))

            try
              val bytes = is.readAllBytes()
              defineClass(name, bytes, 0, bytes.length)
            finally is.close()
      case _ =>
        try findClass(name)
        catch case _: ClassNotFoundException =>
          // Classes loaded from an earlier `:jar` or `:dep` have no class file
          // under `root`. Consult the previous output loader before defining
          // another instrumented copy from a parent resource.
          val inherited = previousSessionLoader match
            case previous: AbstractFileClassLoader =>
              try previous.loadClass(name)
              catch case _: ClassNotFoundException => null
            case null => null
          if inherited != null then inherited
          else
            // Instrument a newly visible classpath entry in the current epoch.
            try
              val resourceName = name.replace('.', '/') + ".class"
              getParent.getResourceAsStream(resourceName) match {
                case null => super.loadClass(name)
                case is =>
                  try defineClassInstrumented(name, is.readAllBytes())
                  finally is.close()
              }
            catch
              case _: Exception => super.loadClass(name)
      }

end AbstractFileClassLoader
