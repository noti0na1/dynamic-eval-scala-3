// tau2-bench per-turn driver.
//
// `:load`ed INTO the running REPL (NOT passed to `scala-cli repl` as a file
// argument) so the `agent[String]` call site (in `callAgent`, which `turn` calls)
// is compiled through the REPL's eval-rewrite phase. As a plain file argument it
// would compile with the ordinary pipeline and the rewriter would never fire.
//
// REDESIGN: the conversation loop is OUTSIDE the REPL, in repl_bench.py. That
// orchestrator feeds ONE `turn(i)` command per customer turn and reads back the
// reply, so each turn is its own top-level REPL command — which is what makes the
// `-Xrepl-history-file` transcript (and thus the agent's cross-turn memory) work.
// `turn` does NOT loop, does NOT message the customer, and does NOT thread a
// transcript: it answers the single message for turn `i` and returns the reply.
//
// Wired up by repl_bench.py roughly as:
//   :load Bench.scala
//   facadeInfo()                       // orchestrator checks facade vs domain
//   Tau.beginTask(sid, dom, dir, pol)  // once per task
//   turn(0); turn(1); ...              // once per customer turn
//   Tau.endTask(statsFile)             // once per task

/** Print the loaded facade's domain + tool count so the orchestrator can fail
 *  fast if the co-loaded facade (facades/<Domain>.scala) doesn't match the domain
 *  it's about to run — otherwise the agent would be shown the wrong tools. */
def facadeInfo(): Unit =
  println(s"__TAU_FACADE__ $facadeDomain " +
    s"${domainToolsDoc.linesIterator.count(_.startsWith("- "))}")

/** Handle ONE customer turn and return the agent's reply.
 *
 *  Reads turn `i`'s customer message from the question file, prints it (so it
 *  lands in the REPL transcript that becomes the next turn's memory), then makes
 *  the single eval-rewritten `agent[String]` call (via `callAgent`). The agent
 *  gathers facts via the
 *  in-scope backend tools (facades/<Domain>.scala — each prints a `[tool] ...`
 *  line) and composes this turn's reply; the orchestrator delivers it and calls
 *  `turn(i+1)` with the customer's response.
 *
 *  Always ends by writing the reply file + printing the turn-done sentinel
 *  (`Tau.writeReply`), even on error, so the orchestrator — blocked reading our
 *  stdout — never hangs. */
def turn(i: Int): String =
  // Start each task's transcript fresh: turn 0 truncates the (persistent-REPL)
  // history file, so the agent's cross-turn memory holds only THIS conversation.
  if i == 0 then Tau.resetHistory()
  val reply =
    try
      val q = Tau.question(i)
      println(s"[customer] $q")
      callAgent(tauStep(Tau.domain, Tau.policy, domainToolsDoc, domainGuidance, q))
    catch case e: Throwable =>
      println(s"[agent] turn $i error: ${e.getClass.getSimpleName}: ${e.getMessage}")
      "I'm sorry — I ran into an internal error handling that. " +
        "Could you say that again?"
  Tau.writeReply(i, reply)
  reply

def callAgent(prompt: String): String = agent[String](prompt)