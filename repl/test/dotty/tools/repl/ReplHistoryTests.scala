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

  @Test def preservesUnicodeInLiveAndRecordedOutput = initially {
    run("""val unicode = "→ 🤪 T²"""")
    val live = storedOutput()
    val recorded = historyContent
    assertTrue(s"expected Unicode in live output, got:\n$live",
      live.contains("→ 🤪 T²"))
    assertTrue(s"expected Unicode in recorded output, got:\n$recorded",
      recorded.contains("→ 🤪 T²"))
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

  @Test def multiLineInputUsesContinuationPrefix = initially {
    run("def f(x: Int) =\n  x * 2")
    val s = historyContent
    assertTrue(s"expected the first input line prefixed with `scala> `, got:\n$s",
      s.contains("scala> def f(x: Int) ="))
    assertTrue(s"expected the continuation line prefixed with `     | `, got:\n$s",
      s.contains("     |   x * 2"))
  }

  @Test def commandInputIsRecorded = initially {
    run("import scala.collection.mutable")
    run(":imports")
    val s = historyContent
    assertTrue(s"expected `scala> :imports` to be recorded, got:\n$s",
      s.contains("scala> :imports"))
  }

  @Test def allCommandShapesAreRecorded = initially {
    val afterSave = run(":save")
    val afterReplay = run(":replay")(using afterSave)
    val afterToolkit = run(":toolkit")(using afterReplay)
    val afterRepository = run(":repository")(using afterToolkit)
    run(":paste")(using afterRepository)

    val s = historyContent
    List(":save", ":replay", ":toolkit", ":repository", ":paste").foreach: command =>
      assertTrue(s"expected `scala> $command` to be recorded, got:\n$s",
        s.contains(s"scala> $command"))
  }

  @Test def commandThenCodeIsRecordedAsOneEntry = initially {
    run(":settings -deprecation\nval afterCommand = 1")
    val s = historyContent
    assertTrue(s"expected the command as the entry input, got:\n$s",
      s.contains("scala> :settings -deprecation"))
    assertTrue(s"expected the code as a continuation line, got:\n$s",
      s.contains("     | val afterCommand = 1"))
    assertFalse(s"expected no detached entry for the trailing code, got:\n$s",
      s.contains("scala> val afterCommand = 1"))
  }

  @Test def replayIsRecordedAsOneEntry = initially {
    val populated = run("val replayed = 42")
    ReplHistoryTests.historyFile.delete()

    run(":replay")(using populated)
    val s = historyContent
    assertTrue(s"expected the replay command as the entry input, got:\n$s",
      s.startsWith("scala> :replay\n"))
    assertTrue(s"expected replayed output in the command entry, got:\n$s",
      s.contains("val replayed: Int = 42"))
    assertEquals(s"expected no nested transcript entries, got:\n$s",
      1, "scala> ".r.findAllMatchIn(s).size)
  }

  @Test def oversizedOutputIsTruncatedWithMarker = initially {
    // Print more than `ReplHistory.captureLimit` bytes in one line; the
    // live stream gets everything, the history entry is capped and ends
    // with a truncation marker.
    run(s"""print("x" * ${ReplHistory.captureLimit + 1024})""")
    val s = historyContent
    assertTrue(s"expected a truncation marker, got tail:\n${s.takeRight(200)}",
      s.contains("[... output truncated:"))
    assertTrue(s"expected the entry to be capped near the limit, size was ${s.length}",
      s.length < ReplHistory.captureLimit + 4096)
  }

  @Test def capturesEvalLineOutput = initially {
    // An eval line runs a nested compile through a separate Driver;
    // its rendering must still land in the same per-line capture.
    run("""val r: Int = eval("21 * 2")""")
    val s = historyContent
    assertTrue(s"expected the eval input to be recorded, got:\n$s",
      s.contains("""scala> val r: Int = eval("21 * 2")"""))
    assertTrue(s"expected the eval result to be recorded, got:\n$s",
      s.contains("val r: Int = 42"))
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

/** ANSI stripping needs a session that actually colors its output;
 *  `ReplTest.defaultOptions` forces `-color:never`, which would make
 *  the assertion vacuous.
 */
class ReplHistoryColorTests
    extends ReplTest(ReplHistoryColorTests.options, ReplHistoryColorTests.rawOut):

  @After def cleanFile(): Unit =
    val f = ReplHistoryColorTests.historyFile
    if f.exists() then f.delete()

  @Test def stripsAnsiColorCodes = initially {
    run("val s = \"hi\"")
    // `storedOutput()` strips color, so assert ANSI presence on the
    // raw buffer (before the @After cleanup resets it).
    val live = ReplHistoryColorTests.rawOut.toString(StandardCharsets.UTF_8)
    assertTrue(s"expected ANSI escapes on the live stream, got:\n$live",
      live.contains("\u001b["))
    val f = ReplHistoryColorTests.historyFile
    val recorded = new String(Files.readAllBytes(f.toPath), StandardCharsets.UTF_8)
    assertTrue(s"expected the input to be recorded, got:\n$recorded",
      recorded.contains("val s"))
    assertFalse(s"expected no ANSI escapes in the file, got:\n$recorded",
      recorded.contains("\u001b["))
  }

end ReplHistoryColorTests

object ReplHistoryColorTests:
  val rawOut: ByteArrayOutputStream = new ByteArrayOutputStream

  val historyFile: File =
    val f = File.createTempFile("repl-history-color-", ".txt")
    f.deleteOnExit()
    f.delete()
    f

  val options: Array[String] =
    ReplTest.defaultOptions.filterNot(_.startsWith("-color")) ++
      Array("-color:always", s"-Xrepl-history-file:${historyFile.getAbsolutePath}")

/** A history path that cannot be created (its parent is a regular
 *  file) must warn and leave the REPL fully functional.
 */
class ReplHistoryUnwritableTests
    extends ReplTest(ReplHistoryUnwritableTests.options, new ByteArrayOutputStream):

  @Test def ioFailureDoesNotBreakTheRepl = initially {
    run("val ok = 6 * 7")
    val out = storedOutput()
    assertTrue(s"expected the line to evaluate normally, got:\n$out",
      out.contains("val ok: Int = 42"))
    assertFalse("expected no history file at the unwritable path",
      new File(ReplHistoryUnwritableTests.unwritablePath).exists())
  }

end ReplHistoryUnwritableTests

object ReplHistoryUnwritableTests:
  // A regular file used as a directory component makes every append fail.
  val blockingFile: File =
    val f = File.createTempFile("repl-history-blocker-", ".txt")
    f.deleteOnExit()
    f

  val unwritablePath: String =
    s"${blockingFile.getAbsolutePath}${File.separator}sub${File.separator}history.txt"

  val options: Array[String] =
    ReplTest.defaultOptions :+ s"-Xrepl-history-file:$unwritablePath"
