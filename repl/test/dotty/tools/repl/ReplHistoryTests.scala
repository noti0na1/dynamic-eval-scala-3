package dotty.tools
package repl

import java.io.{File, PrintStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import org.junit.{After, Before, Test}
import org.junit.Assert.*

/** Tests for the `-Xrepl-history-file:<path>` flag, which appends a
 *  transcript of every REPL line (input + captured output) to a file
 *  for tools like `agent[T]` to read back as conversational context.
 */
class ReplHistoryTests
    extends ReplTest(ReplHistoryTests.optionsForTempFile, new ByteArrayOutputStream):

  @After def cleanFile(): Unit =
    val f = ReplHistoryTests.historyFile
    if f.exists() then f.delete()

  private def historyContent: String =
    val f = ReplHistoryTests.historyFile
    if !f.exists() then "" else new String(Files.readAllBytes(f.toPath), StandardCharsets.UTF_8)

  @Test def writesInputAndOutput = initially {
    run("val n = 1 + 2")
    val s = historyContent
    assertTrue(s"expected to contain `scala> val n = 1 + 2`, got:\n$s",
      s.contains("scala> val n = 1 + 2"))
    assertTrue(s"expected to contain `val n: Int = 3`, got:\n$s",
      s.contains("val n: Int = 3"))
  }

  @Test def capturesUserPrintln = initially {
    run("""println("hello world")""")
    val s = historyContent
    assertTrue(s"expected to contain `hello world`, got:\n$s",
      s.contains("hello world"))
  }

  @Test def appendsAcrossLines = initially {
    run("val a = 1")
    run("val b = 2")
    run("val c = 3")
    val s = historyContent
    assertTrue(s.contains("scala> val a = 1"))
    assertTrue(s.contains("scala> val b = 2"))
    assertTrue(s.contains("scala> val c = 3"))
    // Order check: a comes before b comes before c.
    val ia = s.indexOf("scala> val a = 1")
    val ib = s.indexOf("scala> val b = 2")
    val ic = s.indexOf("scala> val c = 3")
    assertTrue(s"expected a < b < c order, got a=$ia b=$ib c=$ic", ia < ib && ib < ic)
  }

  @Test def stripsAnsiColorCodes = initially {
    run("val s = \"hi\"")
    val s = historyContent
    assertFalse(s"expected no ANSI escapes, got:\n$s", s.contains("\u001b["))
  }

end ReplHistoryTests

object ReplHistoryTests:
  // One file per JVM run. Cleaned by the @After hook between tests so
  // each test sees only its own writes.
  val historyFile: File =
    val f = File.createTempFile("repl-history-", ".txt")
    f.deleteOnExit()
    f.delete() // start clean
    f

  val optionsForTempFile: Array[String] =
    ReplTest.defaultOptions :+ s"-Xrepl-history-file:${historyFile.getAbsolutePath}"
