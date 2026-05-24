// Self-contained smoke test for the Lacuna `agent` primitive.
//
// This file is `:load`ed INTO a running REPL (by test_agent.sh) so the
// `agent[String]` call site is compiled through the eval-rewrite phase — the
// same reason Bench.scala is `:load`ed rather than passed as a file argument.
//
// The task is deliberately shaped to FORCE recursive agent calls: three known
// topics, each summarised by its own nested `agent[String]` sub-call, then
// assembled mechanically. A healthy run therefore shows agentCalls >= 2 and
// leaves several generated `eval_*_code.scala` snippets in the eval-log dir.
//
// It needs no retriever server — the task is pure compute + LLM — so it
// exercises the whole pipeline (prompt build, recursion, depth guard, history
// stack, retries, token/stat accounting) in isolation.

import scala.util.control.NonFatal

// Clean slate so the numbers below describe THIS run only.
LLMUsage.reset()
AgentStats.reset()
AgentDepth.reset()
AgentHistory.reset()

val task =
  """Build a short "mini encyclopedia". Steps:
    |  1. Use exactly these three topics, in order: "the Moon", "the Sun", "Mars".
    |  2. For EACH topic, obtain one concise factual sentence by delegating to a
    |     nested call `agent[String]("one concise factual sentence about <topic>")`
    |     — one sub-call per topic (map/iterate over the three topics). You MUST
    |     get each sentence from such a sub-call; do NOT write the facts inline.
    |  3. Assemble the results into a titled, numbered, multi-line string:
    |       Mini Encyclopedia:
    |       1. <fact about the Moon>
    |       2. <fact about the Sun>
    |       3. <fact about Mars>
    |Return only that multi-line string.""".stripMargin

println("[test] ============================================================")
println(s"[test] model    = ${sys.env.getOrElse("AGENT_MODEL", "deepseek-v4-flash")}")
println(s"[test] base-url = ${sys.env.getOrElse("AGENT_BASE_URL", "https://api.deepseek.com")}")
println(s"[test] maxDepth = ${AgentDepth.max}")
println("[test] running agent[String] task (expects recursive sub-calls)...")
println("[test] ============================================================")

var threw = false
val startedMs = System.currentTimeMillis()
val result: String =
  try agent[String](task)
  catch
    case NonFatal(e) =>
      threw = true
      println(s"[test] !! agent threw ${e.getClass.getName}: ${e.getMessage}")
      e.printStackTrace()
      ""
val elapsedMs = System.currentTimeMillis() - startedMs

println("\n[test] ===== RESULT =====")
println(result)

println("\n[test] ===== AgentStats =====")
println(s"  agentCalls     = ${AgentStats.agentCalls}   (top-level + recursive)")
println(s"  attempts       = ${AgentStats.attempts}   (code generations, incl. retries)")
println(s"  retries        = ${AgentStats.retries}")
println(s"  compilesOk     = ${AgentStats.compilesOk}")
println(s"  compilesFailed = ${AgentStats.compilesFailed}")

println("\n[test] ===== LLMUsage =====")
println(s"  calls            = ${LLMUsage.calls}")
println(s"  promptTokens     = ${LLMUsage.promptTokens}")
println(s"  completionTokens = ${LLMUsage.completionTokens}")
println(s"  totalTokens      = ${LLMUsage.totalTokens}")

println("\n[test] ===== invariants after run =====")
println(s"  AgentDepth.depth      = ${AgentDepth.depth}   (expect 0 — balanced)")
println(s"  AgentHistory.snapshot = ${AgentHistory.snapshot.length} entries   (expect 0 — balanced)")
println(s"  elapsed               = ${elapsedMs} ms")

val checks: List[(String, Boolean)] = List(
  "task ran without throwing"        -> !threw,
  "result is non-empty"              -> result.nonEmpty,
  "at least one agent call"          -> (AgentStats.agentCalls >= 1),
  "RECURSIVE agent calls happened"   -> (AgentStats.agentCalls >= 2),
  "at least one snippet compiled"    -> (AgentStats.compilesOk >= 1),
  "attempts >= agentCalls"           -> (AgentStats.attempts >= AgentStats.agentCalls),
  "LLM chat calls recorded"          -> (LLMUsage.calls >= 1),
  "token usage recorded"             -> (LLMUsage.totalTokens > 0),
  "depth counter balanced to 0"      -> (AgentDepth.depth == 0),
  "history stack balanced (empty)"   -> AgentHistory.snapshot.isEmpty
)

println("\n[test] ===== CHECKS =====")
checks.foreach((name, ok) => println(s"  [${if ok then "PASS" else "FAIL"}] $name"))

val allPass = checks.forall(_._2)
println("\n[test] ============================================================")
println(s"[test] OVERALL: ${if allPass then "PASS" else "FAIL"}")
// Machine-greppable verdict for test_agent.sh:
println(s"TEST_RESULT=${if allPass then "PASS" else "FAIL"}")
println("[test] ============================================================")
