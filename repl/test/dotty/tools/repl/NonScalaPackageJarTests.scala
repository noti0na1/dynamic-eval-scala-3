package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Path}
import java.util.Comparator
import java.util.jar.{JarEntry, JarOutputStream, Manifest}
import javax.tools.ToolProvider

import org.junit.{Test, Assume}
import org.junit.Assert.{assertEquals, assertTrue, assertFalse}

/** Tests for loading JARs that contain classes in non-scala packages.
  * This tests the fix for cyclic reference issues when loading JARs
  * with overlapping packages (e.g., os-lib and os-lib-watch both defining the `os` package).
  */
class NonScalaPackageJarTests extends ReplTest:
  import NonScalaPackageJarTests.*

  @Test def `eval and topLevel see a JAR added at runtime` =
    Assume.assumeTrue("javac not available", ToolProvider.getSystemJavaCompiler != null)
    val (jar1Path, jar2Path) = createOverlappingPackageJars()
    val initializationKey = s"scala.repl.jar-loader-test.${java.util.UUID.randomUUID()}"
    try
      initially {
        val state = run(
          s"""|val previousInitializations = Option(System.getProperty("$initializationKey")).fold(0)(_.toInt)
              |System.setProperty("$initializationKey", (previousInitializations + 1).toString)
              |class BeforeJar(val value: Int)
              |val beforeJar = BeforeJar(40)
              |def liveJarValue = evalSafe[Int]("testpkg.ClassA.getValue()")
              |val jarMissingBeforeAddition = liveJarValue.isFailure
              |""".stripMargin)
        val out = storedOutput()
        assertTrue(s"expected the stable eval call site to miss the not-yet-added JAR, got: $out",
          out.contains("val jarMissingBeforeAddition: Boolean = true"))
        state
      } andThen {
        val state = run(s":jar $jar1Path")
        storedOutput()
        state
      } andThen {
        val state = run(
          "val previousWrapperValue: Int = beforeJar.value + 1; " +
            "val cachedCallSiteValue: Int = liveJarValue.get")
        val out = storedOutput()
        assertTrue(s"expected the pre-JAR wrapper identity to survive, got: $out",
          out.contains("val previousWrapperValue: Int = 41"))
        assertTrue(s"expected the cached call site to recompile against the added JAR, got: $out",
          out.contains("val cachedCallSiteValue: Int = 1"))
        assertEquals("the pre-JAR wrapper initializer must not run in the replacement loader",
          "1", System.getProperty(initializationKey))
        state
      } andThen {
        val state = run("""val evalValue: Int = eval[Int]("beforeJar.value + testpkg.ClassA.getValue() + 1")""")
        val out = storedOutput()
        assertTrue(s"expected eval to see ClassA and the pre-JAR wrapper, got: $out",
          out.contains("val evalValue: Int = 42"))
        state
      } andThen {
        val state = run("""val jarHandle = topLevel("def fromJar: Int = testpkg.ClassA.getValue()")""")
        val out = storedOutput()
        assertFalse(s"expected topLevel defs to see ClassA, got: $out", out.contains("failed to compile"))
        state
      } andThen {
        run("""val topLevelValue: Int = jarHandle.eval[Int]("fromJar")""")
        val out = storedOutput()
        assertTrue(s"expected topLevel eval result, got: $out", out.contains("val topLevelValue: Int = 1"))
      }
    finally
      System.clearProperty(initializationKey)
      Files.deleteIfExists(Path.of(jar1Path))
      Files.deleteIfExists(Path.of(jar2Path))

  @Test def `load JARs with overlapping non-scala packages` =
    // Skip if javac is not available
    Assume.assumeTrue("javac not available", ToolProvider.getSystemJavaCompiler != null)

    // Create two JARs that both define classes in the `testpkg` package
    // This simulates the case of os-lib and os-lib-watch both defining the `os` package
    val (jar1Path, jar2Path) = createOverlappingPackageJars()
    try
      initially {
        // Load the first JAR with testpkg.ClassA
        val state = run(s":jar $jar1Path")
        val output = storedOutput()
        assertTrue(s"expected success message, got: $output", output.contains("Added") && output.contains("to classpath"))
        state
      } andThen {
        // Use ClassA to ensure the package is loaded
        val state = run(
          "val jar1Instance = new testpkg.ClassA; " +
            "val jar1Class = jar1Instance.getClass; " +
            "val jar1Value = testpkg.ClassA.getValue()")
        val output = storedOutput()
        assertTrue(s"expected ClassA value, got: $output",
          output.contains("val jar1Value: Int = 1"))
        state
      } andThen {
        // Load the second JAR after resolving a class from the first.
        val state = run(s":jar $jar2Path")
        val output = storedOutput()
        assertFalse(s"should not have cyclic reference error, got: $output",
          output.contains("CyclicReference") || output.contains("Cyclic reference"))
        assertTrue(s"expected success message, got: $output", output.contains("Added") && output.contains("to classpath"))
        state
      } andThen {
        // Use ClassB and carry a ClassA instance across the loader epoch.
        run(
          "val sameJar1Class = jar1Class eq classOf[testpkg.ClassA]; " +
            "val sameJar1Instance = jar1Instance.isInstanceOf[testpkg.ClassA]; " +
            "val jar2Value = testpkg.ClassB.getValue()")
        val output = storedOutput()
        assertTrue(s"expected the first JAR's class identity to survive, got: $output",
          output.contains("val sameJar1Class: Boolean = true"))
        assertTrue(s"expected the first JAR's instance to cross the loader epoch, got: $output",
          output.contains("val sameJar1Instance: Boolean = true"))
        assertTrue(s"expected ClassB value, got: $output",
          output.contains("val jar2Value: Int = 2"))
      }
    finally
      Files.deleteIfExists(Path.of(jar1Path))
      Files.deleteIfExists(Path.of(jar2Path))

  @Test def `load JARs with nested overlapping packages` =
    // Skip if javac is not available
    Assume.assumeTrue("javac not available", ToolProvider.getSystemJavaCompiler != null)

    // Create two JARs where the second JAR adds both classes to an existing package
    // AND a new subpackage
    val (jar1Path, jar2Path) = createNestedOverlappingPackageJars()
    try
      initially {
        // Load the first JAR with testpkg.ClassA
        val state = run(s":jar $jar1Path")
        storedOutput() // discard output
        state
      } andThen {
        // Use ClassA to ensure the package is loaded
        val state = run("testpkg.ClassA.getValue()")
        storedOutput() // discard output
        state
      } andThen {
        // Load the second JAR with testpkg.ClassC AND testpkg.sub.ClassD
        val state = run(s":jar $jar2Path")
        val output = storedOutput()
        assertFalse(s"should not have cyclic reference error, got: $output",
          output.contains("CyclicReference") || output.contains("Cyclic reference"))
        state
      } andThen {
        // Use ClassC to ensure the second JAR was loaded correctly
        val state = run("val classCValue = testpkg.ClassC.getValue()")
        val output = storedOutput()
        assertTrue(s"expected ClassC value, got: $output",
          output.contains("val classCValue: Int = 3"))
        state
      } andThen {
        // Use ClassD from the subpackage
        run("val classDValue = testpkg.sub.ClassD.getValue()")
        val output = storedOutput()
        assertTrue(s"expected ClassD value, got: $output",
          output.contains("val classDValue: Int = 4"))
      }
    finally
      Files.deleteIfExists(Path.of(jar1Path))
      Files.deleteIfExists(Path.of(jar2Path))

object NonScalaPackageJarTests:

  /** Creates two JARs that both define classes in the `testpkg` package.
    * JAR1 contains testpkg.ClassA
    * JAR2 contains testpkg.ClassB
    */
  def createOverlappingPackageJars(): (String, String) =
    val tempDir = Files.createTempDirectory("overlapping-pkg-test")

    // Create package directory
    val pkgDir = tempDir.resolve("testpkg")
    Files.createDirectories(pkgDir)

    // Write Java source for ClassA
    val classASource = pkgDir.resolve("ClassA.java")
    Files.writeString(classASource,
      """|package testpkg;
         |public class ClassA {
         |  public static int getValue() { return 1; }
         |}
         |""".stripMargin)

    // Write Java source for ClassB
    val classBSource = pkgDir.resolve("ClassB.java")
    Files.writeString(classBSource,
      """|package testpkg;
         |public class ClassB {
         |  public static int getValue() { return 2; }
         |}
         |""".stripMargin)

    // Compile with javac
    val compiler = ToolProvider.getSystemJavaCompiler
    val fileManager = compiler.getStandardFileManager(null, null, null)
    val compilationUnits = fileManager.getJavaFileObjects(classASource.toFile, classBSource.toFile)
    val task = compiler.getTask(null, fileManager, null,
      java.util.Arrays.asList("-d", tempDir.toString), null, compilationUnits)
    val success = task.call()
    fileManager.close()

    if !success then
      throw new RuntimeException("Failed to compile test classes")

    // Create JAR1 with ClassA
    val jar1File = tempDir.resolve("jar1.jar").toFile
    createJar(jar1File, tempDir, List("testpkg/ClassA.class"))

    // Create JAR2 with ClassB
    val jar2File = tempDir.resolve("jar2.jar").toFile
    createJar(jar2File, tempDir, List("testpkg/ClassB.class"))

    (jar1File.getAbsolutePath, jar2File.getAbsolutePath)

  /** Creates two JARs where JAR2 adds both classes to an existing package AND a new subpackage.
    * JAR1 contains testpkg.ClassA
    * JAR2 contains testpkg.ClassC and testpkg.sub.ClassD
    */
  def createNestedOverlappingPackageJars(): (String, String) =
    val tempDir = Files.createTempDirectory("nested-pkg-test")

    // Create package directories
    val pkgDir = tempDir.resolve("testpkg")
    val subPkgDir = tempDir.resolve("testpkg/sub")
    Files.createDirectories(subPkgDir)

    // Write Java source for ClassA
    val classASource = pkgDir.resolve("ClassA.java")
    Files.writeString(classASource,
      """|package testpkg;
         |public class ClassA {
         |  public static int getValue() { return 1; }
         |}
         |""".stripMargin)

    // Write Java source for ClassC
    val classCSource = pkgDir.resolve("ClassC.java")
    Files.writeString(classCSource,
      """|package testpkg;
         |public class ClassC {
         |  public static int getValue() { return 3; }
         |}
         |""".stripMargin)

    // Write Java source for ClassD in subpackage
    val classDSource = subPkgDir.resolve("ClassD.java")
    Files.writeString(classDSource,
      """|package testpkg.sub;
         |public class ClassD {
         |  public static int getValue() { return 4; }
         |}
         |""".stripMargin)

    // Compile with javac
    val compiler = ToolProvider.getSystemJavaCompiler
    val fileManager = compiler.getStandardFileManager(null, null, null)
    val compilationUnits = fileManager.getJavaFileObjects(
      classASource.toFile, classCSource.toFile, classDSource.toFile)
    val task = compiler.getTask(null, fileManager, null,
      java.util.Arrays.asList("-d", tempDir.toString), null, compilationUnits)
    val success = task.call()
    fileManager.close()

    if !success then
      throw new RuntimeException("Failed to compile test classes")

    // Create JAR1 with ClassA
    val jar1File = tempDir.resolve("jar1.jar").toFile
    createJar(jar1File, tempDir, List("testpkg/ClassA.class"))

    // Create JAR2 with ClassC and ClassD
    val jar2File = tempDir.resolve("jar2.jar").toFile
    createJar(jar2File, tempDir, List("testpkg/ClassC.class", "testpkg/sub/ClassD.class"))

    (jar1File.getAbsolutePath, jar2File.getAbsolutePath)

  private def createJar(jarFile: File, baseDir: Path, entries: List[String]): Unit =
    val manifest = new Manifest()
    manifest.getMainAttributes.putValue("Manifest-Version", "1.0")

    val jos = new JarOutputStream(new FileOutputStream(jarFile), manifest)
    try
      for entry <- entries do
        val path = baseDir.resolve(entry)
        jos.putNextEntry(new JarEntry(entry))
        jos.write(Files.readAllBytes(path))
        jos.closeEntry()
    finally
      jos.close()
