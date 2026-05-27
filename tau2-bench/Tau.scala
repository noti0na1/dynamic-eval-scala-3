//> using scala 3.9.0-RC1-bin-SNAPSHOT
//> using dep com.lihaoyi::upickle:4.4.3
//> using dep com.lihaoyi::os-lib:0.11.9-M7

// tau2-bench tools + per-turn REPL helpers.
//
// REDESIGN (externally-driven conversation): the conversation loop lives OUTSIDE
// the REPL, in the Python orchestrator `repl_bench.py`. Each customer turn the
// orchestrator feeds ONE top-level REPL command — `turn(i)` (defined in the
// `:load`ed Bench.scala, where the `agent[String]` call site is eval-rewritten) —
// and reads back that turn's reply. Because each turn is its OWN REPL command and
// `-Xrepl-history-file` is enabled, the prints + rendered results of turns 1..i
// accumulate in the session transcript, which `Agent.scala`'s `readRecentReplHistory`
// feeds back to the agent on turn i+1 (AGENT_REPL_HISTORY=on). The REPL session
// IS the agent's memory — there is no hand-threaded transcript string anymore.
//
// Co-loaded into the REPL next to the shared agent primitive:
//   scala-cli repl --server=false \
//     -Xrepl-eval-log-dir:DIR/ -Xrepl-history-file:DIR/session.repl \
//     ../browsecomp-plus-bench/Agent.scala Tau.scala facades/<Domain>.scala
//   then  :load Bench.scala
//
// This object provides, for the orchestrator + the per-turn code:
//   beginTask / endTask  -- task lifecycle (set tau2 session, reset per-task
//                           agent stats, dump them for the run file)
//   question / writeReply -- the per-turn question/reply FILE channel (avoids
//                           shell-escaping customer text and multi-line replies)
//   callTool             -- low-level tau2 tool dispatch used by the typed facade
//                           (facades/<Domain>.scala); PRINTS each `[tool] ...`
//                           line so it lands in the REPL transcript / agent memory
//   ping                 -- handshake sentinel for the orchestrator
//   tauStep              -- the per-turn prompt for `agent[String]`
//
// Reward + the full conversation trace are owned by the orchestrator: it pulls
// tau2's own `simulation_run` (every user/assistant/tool message) over /trace and
// /reward, so unlike BrowseComp there is no LLM judge and no Scala-side trace.

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

object Tau:

  /** Base URL of the local tau2-bench gym shim (tau2_server.py). */
  val serverUrl: String =
    sys.env.getOrElse("TAU2_SERVER_URL", "http://127.0.0.1:8881")

  private val http =
    HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(30)).build()

  // --- per-task state (the orchestrator drives the REPL single-threaded) -------
  private var sessionId    = ""
  private var domainName   = ""
  private var policyText   = ""
  private var turnDir      = os.pwd
  private var agentDoneFlag = false
  private var toolCalls     = 0   // total backend tool calls this task
  private var turnToolCalls = 0   // backend tool calls in the CURRENT turn

  /** Two caps on backend tool calls, both defending against runaway / retry loops
   *  that hammer tools (a single turn looping can otherwise also exhaust tau2's
   *  max_steps mid-task). One is PER TURN (reset by `question` at the start of each
   *  turn — bounds one agent call's storm), the other PER TASK (cumulative).
   *  Hitting either throws, aborting that turn's agent call. Override with
   *  TAU_MAX_TURN_TOOL_CALLS / TAU_MAX_TOOL_CALLS. */
  private val maxTurnToolCalls: Int =
    sys.env.get("TAU_MAX_TURN_TOOL_CALLS").map(_.trim.toInt).getOrElse(50)
  private val maxToolCalls: Int =
    sys.env.get("TAU_MAX_TOOL_CALLS").map(_.trim.toInt).getOrElse(500)

  /** Per-result char cap for the printed `[tool] ...` transcript lines, so the
   *  per-turn REPL entry (and thus the next turn's history prompt) stays bounded.
   *  Override with TAU_TOOL_RESULT_MAX. */
  private val toolResultMax: Int =
    sys.env.get("TAU_TOOL_RESULT_MAX").map(_.trim.toInt).getOrElse(2000)

  private def post(path: String, body: ujson.Obj): ujson.Value =
    val req = HttpRequest.newBuilder()
      .uri(URI.create(serverUrl + path))
      .header("Content-Type", "application/json")
      .timeout(Duration.ofSeconds(180))
      .POST(HttpRequest.BodyPublishers.ofString(body.render()))
      .build()
    val resp = http.send(req, HttpResponse.BodyHandlers.ofString())
    if resp.statusCode() / 100 != 2 then
      throw RuntimeException(s"tau2-bench HTTP ${resp.statusCode()}: ${resp.body()}")
    ujson.read(resp.body())

  // --- task lifecycle (called by the orchestrator as REPL commands) ------------

  /** Open a task in this persistent REPL: bind the tau2 session the orchestrator
   *  already reset, the domain, the per-task turn directory (question/reply file
   *  channel) and the policy (read from a file to dodge shell-escaping), and
   *  reset the per-task agent accounting. Prints a sentinel for the orchestrator.
   *  `turn(0)` then calls `resetHistory()` so turn 0 starts with an empty
   *  transcript (this command's own entry included). */
  def beginTask(sid: String, dom: String, turnDirPath: String, policyFile: String): Unit =
    sessionId     = sid
    domainName    = dom
    turnDir       = os.Path(turnDirPath)
    policyText    = os.read(os.Path(policyFile))
    agentDoneFlag = false
    toolCalls     = 0
    turnToolCalls = 0
    LLMUsage.reset()    // per-task LLM token accounting
    AgentStats.reset()  // per-task retry / compile accounting
    AgentDepth.reset()  // per-task agent recursion-depth guard
    println(s"__TAU_OK__ beginTask $dom $sid")

  /** Dump this task's agent accounting (token usage + retry/compile stats) to a
   *  file the orchestrator reads into the run file's metadata — these counters
   *  live only in the JVM, so the orchestrator can't compute them itself. */
  def endTask(statsFile: String): Unit =
    val obj = ujson.Obj(
      "agent_stats" -> ujson.Obj(
        "agent_calls"     -> AgentStats.agentCalls,
        "attempts"        -> AgentStats.attempts,
        "retries"         -> AgentStats.retries,
        "compiles_ok"     -> AgentStats.compilesOk,
        "compiles_failed" -> AgentStats.compilesFailed),
      "llm_usage" -> ujson.Obj(
        "calls"             -> LLMUsage.calls,
        "prompt_tokens"     -> LLMUsage.promptTokens,
        "completion_tokens" -> LLMUsage.completionTokens,
        "total_tokens"      -> LLMUsage.totalTokens))
    os.write.over(os.Path(statsFile), obj.render())
    println(s"__TAU_OK__ endTask")

  /** Handshake sentinel: the orchestrator sends `Tau.ping("...")` after startup
   *  to confirm the REPL has compiled the file args and `:load`ed Bench.scala. */
  def ping(tag: String): Unit = println(s"__TAU_PONG__$tag")

  /** Truncate the `-Xrepl-history-file` transcript (the same file Agent.scala's
   *  `readRecentReplHistory` reads). The REPL is persistent across tasks, so each
   *  task must start with an empty transcript; `turn(0)` calls this at its very
   *  start. Doing it INSIDE a REPL command — not from the orchestrator — keeps it
   *  race-free: the prior task's commands (incl. their deferred history flush) all
   *  complete before this command runs, and turn 0's own entry is appended only
   *  after this command finishes. Path derivation mirrors Agent.scala. */
  def resetHistory(): Unit =
    sys.env.get("EVAL_LOG_DIR").foreach { d =>
      val f = os.Path(s"${d.stripSuffix("/")}/session.repl", os.pwd)
      if os.exists(f) then os.write.over(f, "")
    }

  // --- per-turn question/reply FILE channel ------------------------------------

  /** The current domain (for the per-turn prompt). */
  def domain: String = domainName

  /** The domain policy for this task (for the per-turn prompt). */
  def policy: String = policyText

  /** Read the customer message for turn `i` (written by the orchestrator). Also
   *  resets the per-turn tool-call budget — `turn(i)` calls this once at the start
   *  of the turn, before the agent runs. */
  def question(i: Int): String =
    turnToolCalls = 0
    os.read(turnDir / s"$i.q.txt")

  /** Persist turn `i`'s reply (and whether the agent marked the task resolved)
   *  to the reply file, then print the turn-done sentinel so the orchestrator —
   *  which is blocked reading our stdout — knows the file is ready to read.
   *  ALWAYS the last thing `turn(i)` does, including on the error path, so the
   *  orchestrator never hangs. */
  def writeReply(i: Int, reply: String): Unit =
    val obj = ujson.Obj("reply" -> reply, "agent_done" -> agentDoneFlag)
    os.write.over(turnDir / s"$i.r.json", obj.render())
    println(s"__TAU_TURN_DONE__$i")

  // --- low-level tool dispatch (used by the typed facade) ----------------------

  /** Invoke one backend domain API (or a control tool: `done` to finish the
   *  task, `transfer_to_human_agents` to escalate). `args` is a JSON object of
   *  the tool's arguments; returns the tool's result string (JSON for most tools,
   *  but a plain value for some — e.g. `find_user_id_by_email` returns a bare id).
   *
   *  Every call PRINTS a `[tool] name(args) -> result` line. That print is the
   *  point: it is captured into this turn's REPL transcript entry, so on the next
   *  turn the agent's history prompt shows what it already looked up (and what it
   *  changed) — its cross-turn memory, with no hand-threaded transcript.
   *
   *  Example: callTool("get_order_details", ujson.Obj("order_id" -> "#W123")) */
  def callTool(name: String, args: ujson.Value = ujson.Obj()): String =
    toolCalls     += 1
    turnToolCalls += 1
    if turnToolCalls > maxTurnToolCalls then
      throw RuntimeException(
        s"tau2-bench: exceeded $maxTurnToolCalls tool calls in THIS turn — likely a " +
          "retry loop. Stop calling tools; answer with the data you already have.")
    if toolCalls > maxToolCalls then
      throw RuntimeException(
        s"tau2-bench: exceeded $maxToolCalls tool calls for this task; aborting (runaway loop?)")
    if name == "done" then
      // The agent marks the task resolved by calling done(); we do NOT end the
      // tau2 conversation here. The orchestrator delivers THIS turn's reply first
      // (a tau2 `respond`), then issues the real `done` step — so the closing
      // message lands before the chat terminates.
      agentDoneFlag = true
      println("[tool] done() -> (marked resolved; chat ends after your reply is sent)")
      "(noted: task marked resolved — your reply is sent, then the chat ends)"
    else
      val r   = post("/step", ujson.Obj(
        "session_id" -> sessionId, "name" -> name, "kwargs" -> args))
      val out = r("observation").str
      val shown =
        if out.length > toolResultMax then out.take(toolResultMax) + "…(truncated)"
        else out
      println(s"[tool] $name(${args.render()}) -> $shown")
      out

end Tau

/** The per-turn task handed to `agent[String]` inside Bench.scala's `turn(i)`.
 *  The agent is a customer-service rep handling ONE turn of an ongoing live chat:
 *  it reads the conversation so far FROM THE REPL SESSION TRANSCRIPT (injected
 *  automatically into the prompt by Agent.scala when AGENT_REPL_HISTORY=on), calls
 *  the in-scope backend tools to gather facts / take policy-confirmed actions, and
 *  returns its single next reply to the customer. The orchestrator owns the loop:
 *  it delivers that reply to the simulated customer and calls `turn` again with the
 *  next message — so the agent must NOT loop or message the customer itself.
 *  `toolsDoc` is the domain's `domainToolsDoc`; `customerMessage` is the LATEST
 *  message to answer this turn. */
def tauStep(domain: String, policy: String, toolsDoc: String, guidance: String, customerMessage: String): String =
  val guidanceBlock =
    if guidance.trim.isEmpty then ""
    else s"\n\nDOMAIN-SPECIFIC GUIDANCE for $domain — hard-won tactics distilled from " +
      s"prior runs.\nThey REFINE, never override, the policy above; apply them when relevant:\n$guidance"
  s"""You are a customer-service agent for a $domain company in a live chat with
     |one customer, handling ONE turn. Follow the company policy EXACTLY and use
     |ONLY the in-scope Scala tool functions listed below. Produce your SINGLE next
     |reply to the customer as the returned String. Do NOT loop or try to message
     |the customer yourself: the surrounding program delivers your reply and calls
     |you again with the customer's response.
     |
     |YOUR MEMORY OF THIS CONVERSATION is the REPL session transcript shown above
     |(the "Recent REPL session transcript" section — empty on the first turn). It
     |is this same live chat's earlier turns, where:
     |  - `[customer] ...`         is what the customer said on a previous turn,
     |  - `[tool] name(args) -> r` is a backend tool YOU already called and its
     |                             (possibly truncated) result — your record of
     |                             what you've already looked up or changed,
     |  - `val resN: String = "..."` is the reply YOU sent on a previous turn.
     |Use it to avoid re-investigating, to not contradict yourself, and to honour
     |confirmations already given. Do NOT write code to parse this transcript text;
     |if you need a specific field as a value this turn, just call that read tool
     |again (reads are idempotent). To make sure THIS turn is remembered next turn,
     |narrate notable findings with terse `println("[agent] ...")` — those prints
     |persist into the transcript too.
     |
     |IN-SCOPE BACKEND TOOL FUNCTIONS — call them by name with the given typed
     |parameters. Each returns the tool's result as a String — sometimes JSON,
     |sometimes a PLAIN value (an id, a status word, or an `"Error: ..."` message).
     |For example `findUserIdByEmail(...)` / `findUserIdByNameZip(...)` return the
     |BARE user id like `"sarah_doe_4827"` (NOT a JSON object). So do NOT blindly
     |`ujson.read` every result: look at the raw String, and only parse it when it
     |actually starts with `{` or `[`. A bare id IS the value you want — use it
     |directly; NEVER treat a successful non-JSON result as a failure / "not found".
     |Optional params are `Option[...]` (pass `Some(x)` or omit). Some params are
     |case classes (or a `List` of them) — construct them positionally as shown in
     |DATA TYPES / EXAMPLE VALUES below, wrapping collections in `List(...)`:
     |$toolsDoc
     |
     |Two control functions above are special:
     |  - call `done()` to END the task once the request is fully handled and the
     |    customer has nothing further. EVERY resolved task must end with `done()`.
     |  - call `transferToHumanAgents(summary)` ONLY when the policy explicitly
     |    requires escalation, or the request genuinely cannot be handled with the
     |    tools above — never as a shortcut for a request you can actually solve.
     |
     |COMPANY POLICY — follow it to the letter; it governs what you may and may not
     |do, and what you MUST confirm with the customer before doing:
     |$policy$guidanceBlock
     |
     |HOW TO HANDLE THIS TURN:
     |  - Authenticate the customer first if the policy requires it; never reveal or
     |    invent information you have not retrieved with a tool.
     |  - Call read-only tools as needed to gather the facts to answer THIS message
     |    (skip ones whose results are already in your transcript memory above).
     |  - Before ANY state-changing action (cancel, modify, refund, book, return,
     |    exchange, ...) the policy requires explicit customer confirmation: take the
     |    action ONLY if the customer's confirmation is ALREADY visible in the
     |    transcript; otherwise state the exact change and ask for it in your reply.
     |  - If that confirmation IS already visible, do NOT re-ask — call the action tool
     |    THIS turn. After any state-changing call, READ its result: if it starts with
     |    "Error", do NOT tell the customer it succeeded; fix the arguments and retry,
     |    or report the real outcome.
     |  - DUAL CONTROL: in some domains (e.g. telecom) the CUSTOMER performs actions
     |    on their OWN device/account — toggling a setting, granting a permission,
     |    restarting, reading a code off the screen. Your tools read state and make
     |    account-side changes; device-side steps are done BY THE CUSTOMER on your
     |    instruction. So GUIDE them step-by-step ("open Settings > ..., turn X on,
     |    then tell me when done") and continue once they confirm. NEVER tell the
     |    customer you "lack remote access" or "can't do that" and give up —
     |    instructing them IS how you do it. Do NOT `transferToHumanAgents` or
     |    `done()` merely because a step happens on the customer's device.
     |  - Take the required action and tell the customer the outcome on the SAME turn
     |    (so they're informed before the chat ends).
     |  - ENDING: once everything the customer asked for is handled, give your final
     |    reply AND call `done()` IN THE SAME TURN — your reply is delivered first,
     |    then the chat ends. Do NOT drag it out with "is there anything else?" /
     |    "you're welcome": a task that never calls `done()` is scored UNRESOLVED, so
     |    end as soon as the request is fully resolved.
     |  - Return a single String: your next reply to the customer for THIS turn.
     |
     |IMPLEMENTATION NOTES:
     |  - NEVER use `return` (it FAILS TO COMPILE in the REPL eval wrapper). Write
     |    expression-oriented code — the last expression is the returned reply.
     |  - CRITICAL — keep your code DEAD SIMPLE. Over-elaborate code is the #1 cause
     |    of failures here:
     |    • NEVER define a local `class` / `case class` / `trait` — it CRASHES the
     |      eval compiler ("cannot reach outer ..."). Hold structured data in tuples
     |      (`val o = (id, status, total)`, then `o._1` or destructure), or just read
     |      fields straight off the ujson value when you need them.
     |    • Keep the reply a plain String. To put a VALUE into the reply, use an
     |      s-string with `$${...}`, e.g. `s"Your total is $${total} dollars ($${pct}% off)."`
     |      — the `s` prefix is REQUIRED: without it, `"...$${total}..."` sends the
     |      customer the LITERAL text `$${total}`. Do NOT use `%`/`%.2f` `f"..."`
     |      formatting (write plain numbers like 12.50; a literal `%` inside an
     |      `s"..."` is fine), fancy unicode (bullets, ellipses, smart quotes,
     |      non-breaking hyphens), or heavy markdown templates — they break Scala
     |      string literals or leak into the reply. Build a longer reply by
     |      concatenating simple `s"..."` pieces.
     |  - A tool result String may be JSON or a plain value (id / status /
     |    "Error: ..."). Only `ujson.read(...)` it when it starts with `{`/`[`;
     |    otherwise use the raw String as-is. From a parsed value, read fields with
     |    `j("k").str` / `.num` / `.arr` / `.obj` (coerce before any collection op).
     |    Do NOT wrap a read in a try/catch that turns a SUCCESSFUL non-JSON result
     |    into a "not found". Reply in plain prose — NEVER paste raw tool JSON or an
     |    `"Error: ..."` string as your message to the customer.
     |  - You MAY use a focused recursive `agent[T2]("...")` for a sub-decision (e.g.
     |    `agent[Boolean]` to judge whether the customer just confirmed) — always pin
     |    a concrete `T2`.
     |  - The benchmark scores backend DB state + actions + required outputs
     |    AUTOMATICALLY; your returned String is just your message for this turn.
     |
     |THE CUSTOMER'S LATEST MESSAGE (answer THIS):
     |$customerMessage
     |Your reply to the customer for this turn (return a single String):""".stripMargin
