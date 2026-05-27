//> using scala 3.9.0-RC1-bin-SNAPSHOT
//> using dep com.lihaoyi::upickle:4.4.3
//> using dep com.lihaoyi::os-lib:0.11.9-M7
//> using dep org.scala-lang:scala3-compiler_3:3.9.0-RC1-bin-SNAPSHOT
//> using dep org.scala-lang:scala3-repl_3:3.9.0-RC1-bin-SNAPSHOT

// NOTE: this is the benchmark copy of Agent.scala. Differences from the original:
//   * `-Xrepl-eval-log-dir` / `-Xrepl-history-file` are NOT fixed here — they
//     are passed per run on the scala-cli command line (run_bench.sh), so
//     sharded runs get isolated log/history dirs.
//   * the LLM client reads its key/base-url/model from env vars (see env.sh).
//   * `LLMUsage` accumulates per-call token counts for the bench trace.

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import java.util.concurrent.atomic.AtomicInteger

object LLMChat:

  final case class Message(role: String, content: String)

  final class Client(
      apiKey: String,
      baseUrl: String,
      defaultModel: String,
      http: HttpClient,
      // Client-level reasoning defaults; each `chat` inherits them unless it
      // passes its own `thinking`/`effort` below.
      defaultThinking: Boolean = false,
      defaultEffort: String = "high",
      // How "reasoning on" is encoded in the request body:
      //   "effort"          → reasoning_effort + thinking:{type:enabled}
      //                       (DeepSeek / GLM / Qwen3.6 / Gemini)
      //   "enable_thinking" → a single enable_thinking:true flag, no effort knob
      //                       (Qwen3.5-flash and similar)
      defaultThinkingStyle: String = "effort",
      // Sampling temperature; None omits it (provider default). The official
      // tau2 agent uses 0.0 — set AGENT_TEMPERATURE to match (see fromEnv).
      defaultTemperature: Option[Double] = None
  ):

    def chat(prompt: String): String =
      chat(Seq(Message("user", prompt)))

    def chat(
        messages: Seq[Message],
        model: String = defaultModel,
        thinking: Boolean = defaultThinking,
        effort: String = defaultEffort,
        thinkingStyle: String = defaultThinkingStyle,
        temperature: Option[Double] = defaultTemperature,
        stream: Boolean = false
    ): String =
      val body = ujson.Obj(
        "model"    -> model,
        "messages" -> ujson.Arr(messages.map(m =>
          ujson.Obj("role" -> m.role, "content" -> m.content))*),
        "stream"   -> stream
      )
      // Greedy decoding when requested (official tau2 default). Providers that
      // ignore temperature for reasoning models simply drop it.
      temperature.foreach(t => body("temperature") = ujson.Num(t))
      // Off by default → a plain OpenAI body. The on-encoding is provider-specific:
      // most providers take `reasoning_effort` plus a `thinking` object, while
      // Qwen3.5-flash toggles a single `enable_thinking` flag with no effort knob
      // (AGENT_THINKING_STYLE selects which).
      if thinking then
        thinkingStyle match
          case "enable_thinking" =>
            body("enable_thinking") = true
          case _ =>
            body("reasoning_effort") = effort
            body("thinking")         = ujson.Obj("type" -> "enabled")

      val req = HttpRequest.newBuilder()
        .uri(URI.create(endpoint))
        .header("Content-Type",  "application/json")
        .header("Authorization", s"Bearer $apiKey")
        .timeout(Duration.ofSeconds(120))
        .POST(HttpRequest.BodyPublishers.ofString(body.render()))
        .build()

      val resp = http.send(req, HttpResponse.BodyHandlers.ofString())
      if resp.statusCode() / 100 != 2 then
        throw RuntimeException(s"HTTP ${resp.statusCode()}: ${resp.body()}")

      val json = ujson.read(resp.body())
      json.obj.get("usage").foreach { u =>
        def tok(k: String): Int =
          u.obj.get(k).map(_.num.toInt).getOrElse(0)
        LLMUsage.record(
          tok("prompt_tokens"), tok("completion_tokens"), tok("total_tokens"))
      }
      json("choices")(0)("message")("content").str

    private def endpoint: String =
      s"${baseUrl.stripSuffix("/")}/chat/completions"

  end Client

  /** Build a client with an explicit API key, base URL, and model name.
   *  `thinking`/`effort` become the client's reasoning defaults. */
  def apply(
      apiKey: String,
      baseUrl: String = "https://api.openai.com/v1",
      model: String = "gpt-4o-mini",
      thinking: Boolean = false,
      effort: String = "high",
      thinkingStyle: String = "effort",
      temperature: Option[Double] = None
  ): Client =
    val http = HttpClient.newBuilder()
      .connectTimeout(Duration.ofSeconds(30))
      .build()
    Client(apiKey, baseUrl, model, http, thinking, effort, thinkingStyle, temperature)

  /** Build a client from an env var, defaulting to OpenAI. `thinking`/`effort`
   *  become the client's reasoning defaults. */
  def fromEnv(
      envVar: String = "OPENAI_API_KEY",
      baseUrl: String = "https://api.openai.com/v1",
      model: String = "gpt-4o-mini",
      thinking: Boolean = false,
      effort: String = "high",
      thinkingStyle: String = "effort",
      temperature: Option[Double] = None
  ): Client =
    val key = sys.env.getOrElse(envVar,
      throw IllegalStateException(s"$envVar not set"))
    apply(key, baseUrl, model, thinking, effort, thinkingStyle, temperature)

end LLMChat

/** Per-query LLM token accounting for the BrowseComp-Plus trace. The bench
 *  driver resets this before each query (`Bcp.resetTrace`) and reads it back
 *  into the run file afterwards. Thread-safe: `record` is called from whatever
 *  thread runs a `chat`, including ones a generated body forks. */
object LLMUsage:
  // AtomicInteger, not AtomicLong: upickle serialises Long as a JSON *string*
  // (precision safety), which would write token counts as "22081". Per-query
  // counts fit Int comfortably.
  private val _calls            = AtomicInteger(0)
  private val _promptTokens     = AtomicInteger(0)
  private val _completionTokens = AtomicInteger(0)
  private val _totalTokens      = AtomicInteger(0)
  def calls: Int            = _calls.get
  def promptTokens: Int     = _promptTokens.get
  def completionTokens: Int = _completionTokens.get
  def totalTokens: Int      = _totalTokens.get
  def reset(): Unit =
    _calls.set(0); _promptTokens.set(0); _completionTokens.set(0); _totalTokens.set(0)
  def record(p: Int, c: Int, t: Int): Unit =
    _calls.incrementAndGet()
    _promptTokens.addAndGet(p)
    _completionTokens.addAndGet(c)
    _totalTokens.addAndGet(t)

/** Per-query agent/eval accounting for the BrowseComp-Plus trace: how many
 *  `agentSafe` invocations ran (top-level + recursive), how many code
 *  generation attempts they made in total, how many of those were retries
 *  (a previous attempt failed), and how many generated snippets compiled
 *  (well-typed) versus failed to compile. Reset per query by `Bcp.resetTrace`.
 *  Thread-safe: a generated body may run nested `agentSafe` calls in parallel. */
object AgentStats:
  private val _agentCalls     = AtomicInteger(0)
  private val _attempts       = AtomicInteger(0)
  private val _retries        = AtomicInteger(0)
  private val _compilesOk     = AtomicInteger(0)
  private val _compilesFailed = AtomicInteger(0)
  def agentCalls: Int     = _agentCalls.get
  def attempts: Int       = _attempts.get
  def retries: Int        = _retries.get
  def compilesOk: Int     = _compilesOk.get
  def compilesFailed: Int = _compilesFailed.get
  def reset(): Unit =
    _agentCalls.set(0); _attempts.set(0); _retries.set(0)
    _compilesOk.set(0); _compilesFailed.set(0)
  def callStarted(): Unit = _agentCalls.incrementAndGet()
  def attemptMade(isRetry: Boolean): Unit =
    _attempts.incrementAndGet()
    if isRetry then _retries.incrementAndGet()
  def compileResult(ok: Boolean): Unit =
    if ok then _compilesOk.incrementAndGet() else _compilesFailed.incrementAndGet()

/** Guards against runaway recursive `agent[T2]` nesting. Every `agentSafe`
 *  invocation (top-level or nested) bumps a depth counter on entry and drops it
 *  on exit.
 *
 *  The counter lives in an [[InheritableThreadLocal]] so PARALLEL agent calls
 *  stay independent: a generated body that fans out over a `.par` collection or
 *  spawns `Future`s — each recursing into `agent[...]` — gets its own count per
 *  thread, while a freshly spawned thread still INHERITS its parent's depth, so
 *  the bound holds across thread boundaries instead of resetting to zero on
 *  every fork.
 *
 *  Two thresholds, not one:
 *    - AT the limit (`depth == max`): the call still runs, but its prompt tells
 *      the LLM to finish WITHOUT any further `agent[...]` call (see
 *      `AgentPrompt.DepthLimitGuide`).
 *    - ABOVE the limit (`depth > max`): `enter()` throws BEFORE any LLM call,
 *      so an at-limit body that ignores the instruction and recurses anyway is
 *      stopped hard.
 *  The one-level gap gives the deepest allowed call a fair chance to answer
 *  directly rather than failing outright.
 *
 *  The bench driver resets it per task; it also self-balances via `agentSafe`'s
 *  `finally`. */
object AgentDepth:
  /** Hard nesting bound. Default 32: high enough that legitimate task
   *  decomposition never trips it on its own (the prompt steers the LLM toward
   *  iteration over recursion — see `AgentPrompt.RecursionGuide`), low enough
   *  to stop an unbounded self-recursion within a bounded number of LLM calls.
   *  Override with AGENT_MAX_DEPTH. */
  val max: Int =
    sys.env.get("AGENT_MAX_DEPTH").map(_.trim.toInt).getOrElse(32)

  // Integer (not scala.Int) so the JDK generic carries a real boxed value;
  // Predef's Integer<->Int implicits make the arithmetic read normally.
  private val cur = new InheritableThreadLocal[Integer]:
    override def initialValue(): Integer = 0

  /** Current nesting depth on THIS thread (0 outside any agent call). */
  def depth: Int = cur.get

  /** True when this thread is already at the deepest allowed level — one more
   *  nested `agent[...]` would throw. */
  def atLimit: Boolean = cur.get >= max

  /** Enter a nested agent call. Returns whether the call is now AT the limit
   *  (so the prompt can forbid further recursion). Throws — before any LLM
   *  call — when entering would push PAST the limit. */
  def enter(): Boolean =
    val d = cur.get + 1
    if d > max then
      throw RuntimeException(
        s"agent recursion depth limit ($max) exceeded: too many nested " +
          "agent[...] calls. The deepest call was asked to stop recursing and " +
          "did not. Raise AGENT_MAX_DEPTH to allow deeper nesting.")
    cur.set(d)
    d == max

  def exit(): Unit =
    val d = cur.get
    if d > 0 then cur.set(d - 1)

  def reset(): Unit = cur.set(0)

/** Per-thread stack of the `agent[...]` calls currently in flight on this
 *  thread. Each `agentSafe` pushes its task and the code the LLM produced just
 *  before evaluating that code, and pops in a `finally`; so while a body runs,
 *  the stack holds exactly its ancestor chain (outermost first). The prompt
 *  builder turns the snapshot into prior conversation turns, giving a nested
 *  call the trajectory that led to it — what each ancestor was asked for and
 *  the code it wrote — without us having to splice that context into the
 *  generated source as comments.
 *
 *  [[InheritableThreadLocal]] with a COPIED child value: a body that forks a
 *  `Future`/thread and recurses there still sees the ancestor chain, but each
 *  thread mutates its OWN buffer (sharing one mutable buffer would corrupt it
 *  under concurrent push/pop). Entries are immutable, so a shallow copy is
 *  enough. The bench driver resets it per task; it also self-balances via the
 *  push/pop pair. */
object AgentHistory:
  final case class Entry(task: String, expectedType: String, code: String)

  private val stack =
    new InheritableThreadLocal[scala.collection.mutable.ArrayBuffer[Entry]]:
      override def initialValue() =
        scala.collection.mutable.ArrayBuffer.empty[Entry]
      override def childValue(parent: scala.collection.mutable.ArrayBuffer[Entry]) =
        scala.collection.mutable.ArrayBuffer.from(parent)

  /** Ancestor chain on this thread, outermost first. */
  def snapshot: List[Entry] = stack.get.toList

  def push(task: String, expectedType: String, code: String): Unit =
    stack.get += Entry(task, expectedType, code)

  def pop(): Unit =
    val s = stack.get
    if s.nonEmpty then s.remove(s.length - 1)

  def reset(): Unit = stack.get.clear()

/** Truthy env-var flag: on/1/true/yes/enabled (case-insensitive). */
private def envFlag(name: String): Boolean =
  sys.env.get(name).map(_.trim.toLowerCase).exists(Set("on", "1", "true", "yes", "enabled"))

// LLM client config from the environment (see env.sh). Reasoning is off unless
// AGENT_THINKING is set; AGENT_EFFORT sets the reasoning_effort level;
// AGENT_THINKING_STYLE picks the on-encoding ("effort" default, or
// "enable_thinking" for Qwen3.5-flash, which has no effort knob).
private val client = LLMChat.fromEnv(
  envVar        = "AGENT_API_KEY",
  baseUrl       = sys.env.getOrElse("AGENT_BASE_URL", "https://api.deepseek.com"),
  model         = sys.env.getOrElse("AGENT_MODEL", "deepseek-v4-flash"),
  thinking      = envFlag("AGENT_THINKING"),
  effort        = sys.env.getOrElse("AGENT_EFFORT", "high"),
  thinkingStyle = sys.env.getOrElse("AGENT_THINKING_STYLE", "effort"),
  // Official tau2 agent uses temperature 0.0; set AGENT_TEMPERATURE to match.
  // Unset -> omitted (provider default). Reasoning providers may ignore it.
  temperature   = sys.env.get("AGENT_TEMPERATURE").map(_.trim.toDouble)
)

def chat(prompt: String): String =
  client.chat(
    Seq(
      LLMChat.Message("system",
        "You are a helpful agent for writing Scala 3 code in a live REPL session. Follow the user's instructions carefully and produce only the requested Scala expression as output."),
      LLMChat.Message("user", prompt)
    )
  )

/** Multi-message form used by the inline agent: it builds the full message
 *  list itself — the static system guide, the ancestor `agent[...]` chain as
 *  prior user/assistant turns (so the model sees the trajectory that led
 *  here), and the current request — and passes it through verbatim. */
def chat(messages: Seq[LLMChat.Message]): String =
  client.chat(messages)

@main def demo(): Unit =
  println(chat("What is capture checking in Scala? briefly explain."))

// =============================================================================
// Inline-style LLM agent.
//
// `agent[T]("task")` asks the LLM to fill the call site with a Scala expression
// of type `T`, then compiles + runs it under the live REPL, retrying on compile
// failure with the diagnostic fed back to the model.
//
// The `@evalLike` / `@evalSafeLike` annotations let the post-PostTyper
// `EvalRewriteTyped` phase recognise these defs symbolically (not by name or
// arity) and fill in three synthetic args BY NAME at each call site:
//   - `bindings`        — every term-level name in scope at the call site.
//   - `expectedType`    — source-level rendering of `T`.
//   - `enclosingSource` — the enclosing top-level statement, with this call's
//                          span replaced by a placeholder.
// Matching by name leaves other params (`maxAttempts`) and any `using` clause
// untouched. It's all-or-nothing: omit all three (the typer fills the defaults)
// or pass all three explicitly.
//
// Only works inside a REPL session — `Eval.evalSafe` throws without an active
// adapter. `maxAttempts` is an ordinary param: `agent[T]("task", maxAttempts = 5)`.
// =============================================================================

import dotty.tools.repl.eval.{Eval, EvalContext, EvalResult, evalLike, evalSafeLike}

/** Inline-style LLM agent. Throws on final-attempt failure. */
@evalLike
def agent[T](
    task: String,
    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
    expectedType: String = "",
    enclosingSource: String = "",
    maxAttempts: Int = 8
): T =
  agentSafe[T](task, bindings, expectedType, enclosingSource, maxAttempts).get

/** REPL transcript history is OFF by default in the benchmark copy: each
 *  BrowseComp-Plus query must be answered independently, so one query's
 *  transcript must never leak into another's prompt. Set
 *  `AGENT_REPL_HISTORY=on` to restore the original behaviour. */
private val HistoryEnabled: Boolean = envFlag("AGENT_REPL_HISTORY")

/** Path of the REPL transcript file the `-Xrepl-history-file` flag writes to.
 *  run_bench.sh passes that flag as `$EVAL_LOG_DIR/session.repl`, so this
 *  derives the same path from the env var. Only consulted when
 *  [[HistoryEnabled]] is true.
 */
private val HistoryFile: String =
  sys.env.get("EVAL_LOG_DIR")
    .map(d => s"${d.stripSuffix("/")}/session.repl")
    .getOrElse("./session.repl")

/** Cap on REPL-transcript characters sent per prompt (tail-read, so older
 *  entries drop first). At ~4 chars/token, 500k ≈ 125k tokens. */
private val HistoryMaxChars: Int = 500_000

/** Read up to the trailing [[HistoryMaxChars]] of the REPL transcript file,
 *  aligned to an entry boundary where possible. "" when missing, empty, or
 *  disabled. Only completed REPL lines are flushed, so the in-progress line
 *  that triggered this call never echoes back. */
private def readRecentReplHistory(): String =
  import scala.util.control.NonFatal
  if !HistoryEnabled then return ""
  try
    val path = java.nio.file.Paths.get(HistoryFile)
    if !java.nio.file.Files.exists(path) then ""
    else
      val whole = java.nio.file.Files.readString(path)
      if whole.length <= HistoryMaxChars then whole.stripTrailing
      else
        val tail = whole.takeRight(HistoryMaxChars)
        // Align to the next `scala> ` boundary so we don't start
        // mid-output and confuse the LLM about what was input vs
        // output.
        val marker = "\nscala> "
        val cut = tail.indexOf(marker)
        val aligned = if cut >= 0 then tail.substring(cut + 1) else tail
        s"... (older entries truncated)\n${aligned.stripTrailing}"
  catch case NonFatal(_) => ""

/** Non-throwing variant: returns the [[EvalResult]] of the last attempt
 *  (success on the first attempt that compiles, otherwise the failure).
 */
@evalSafeLike
def agentSafe[T](
    task: String,
    bindings: Array[Eval.Binding] = Array.empty[Eval.Binding],
    expectedType: String = "",
    enclosingSource: String = "",
    maxAttempts: Int = 8
): EvalResult[T] =
  import scala.util.control.NonFatal
  val ctx = EvalContext(enclosingSource, bindings)

  // Read once per agent call (off by default; see HistoryEnabled). Retries
  // share this snapshot.
  val replHistory = readRecentReplHistory()
  AgentStats.callStarted()

  val atLimit = AgentDepth.enter()
  val system  = AgentPrompt.systemPrompt
  val history = AgentPrompt.historyMessages(AgentHistory.snapshot)

  @annotation.tailrec
  def attempt(n: Int, prevCode: String, prevErrors: List[String]): EvalResult[T] =
    AgentStats.attemptMade(isRetry = n > 1) // n>1: a prior attempt failed
    val user = AgentPrompt.userPrompt(
      task, ctx, prevCode, prevErrors, expectedType, atLimit, replHistory)

    // The chat call can fail transiently (HTTP error, timeout, rate limit).
    // That's an infrastructure failure, not a code problem, so we retry the
    // SAME prompt (without feeding the error back to the model) and only
    // rethrow once out of attempts.
    val msgs = LLMChat.Message("system", system) +: history :+ LLMChat.Message("user", user)
    AgentPromptLog.dump(msgs)
    val codeOrErr: Either[Throwable, String] =
      try Right(AgentPrompt.stripCodeFences(chat(msgs)).trim)
      catch case NonFatal(e) => Left(e)

    codeOrErr match
      case Left(e) if n >= maxAttempts => throw e
      case Left(_)                     => attempt(n + 1, prevCode, prevErrors)
      case Right(code) =>
        // Publish (task, code) on the thread-local stack so a nested agent[...]
        // sees it as an ancestor turn while the body runs; pop in a finally so
        // the stack tracks the live depth even when the body throws.
        AgentHistory.push(task, expectedType, code)
        // Right = this call's own compile result (success or failure); Left =
        // the body threw at runtime (kept to rethrow after the last attempt).
        val outcome: Either[Throwable, EvalResult[T]] =
          try
            try Right(Eval.evalSafe[T](code, bindings, expectedType, enclosingSource))
            catch case NonFatal(e) => Left(e)
          finally AgentHistory.pop()

        // A failing EvalResult is a compile error; a thrown exception (Left)
        // means the snippet was well-typed and only failed at runtime.
        AgentStats.compileResult(outcome match
          case Right(r) => r.isSuccess
          case Left(_)  => true)

        outcome match
          case Right(r) if r.isSuccess => r
          case _ if n >= maxAttempts =>
            // Out of retries. Surface compile failure as EvalResult; rethrow
            // any runtime exception so the caller sees the original cause
            // (the LLM was given the chance to recover and didn't).
            outcome match
              case Right(r) => r
              case Left(e)  => throw e
          case Left(e) =>
            attempt(n + 1, code, AgentPrompt.formatThrown(e))
          case Right(r) =>
            attempt(n + 1, code, r.error.nn.errors.toList)

  try attempt(1, "", Nil)
  finally AgentDepth.exit()

/** Optional dump of each agent LLM call's full prompt (system + ancestor chain +
 *  user message) to AGENT_PROMPT_LOG_DIR, one file per call. Off unless that env
 *  is set; the tau2 bench points it at the per-shard eval-log dir so dumps flow
 *  into each task's trace. Best-effort — logging must never break a call. */
private object AgentPromptLog:
  private val dir = sys.env.get("AGENT_PROMPT_LOG_DIR").map(_.trim).filter(_.nonEmpty)
  private val seq = java.util.concurrent.atomic.AtomicLong(0L)
  def dump(messages: Seq[LLMChat.Message]): Unit =
    dir.foreach { d =>
      try
        val p = java.nio.file.Paths.get(
          d, s"prompt_${System.currentTimeMillis()}_${seq.incrementAndGet()}.txt")
        val sb = StringBuilder()
        messages.foreach(m =>
          sb.append("===== ").append(m.role).append(" =====\n")
            .append(m.content).append("\n\n"))
        java.nio.file.Files.writeString(p, sb.toString)
      catch case scala.util.control.NonFatal(_) => ()
    }

private object AgentPrompt:

  private val Intro =
    """You generate ONE Scala 3 expression or code block to fill the placeholder
      |in the user's source; it is then compiled and run in a live REPL. Output
      |ONLY that expression — no markdown fences, no commentary. It MUST type-check
      |at the required type given in the request; a mismatch comes back as a
      |compile error to retry.
      |
      |Your output is Scala CODE, not the answer text. When the required type is
      |`String`, RETURN A STRING LITERAL — e.g. `s"Exact Answer: $name"`, or a
      |triple-quoted `s`-string for a multi-line/templated answer, interpolating
      |`val`s with `$x` / `${expr}`. NEVER emit bare un-quoted prose: a raw
      |`Explanation: ... Exact Answer: ...` is not valid Scala and fails to parse.
      |Inside an `s"..."` string a literal `$` must be written `$$`, and a percent
      |sign right after a value is `${x}%`.
      |
      |If the answer is trivial, return it directly; otherwise stage the work into
      |`val`s and, where a step needs the LLM, sub-calls.""".stripMargin

  private val RecursionGuide =
    """You may call `agent[T]("sub-task")` recursively inside the expression you
      |return — each call is a fresh LLM step that fills its spot with code of
      |type `T` (always pin a precise `T`: `agent[String]`, `agent[Int]`, ...).
      |Call the in-scope helpers (the functions/values in the request) by name.
      |Choose the pattern that fits the task:
      |
      |1. DIRECT — you already know the answer, or plain Scala computes it: just
      |   return it, no `agent` call.
      |     agent[Int]("the year Scala 3 was first released")   ->   2021
      |
      |2. MAP / LOOP — independent items, the SAME step on each: use a collection
      |   op or loop, calling `agent` only for the LLM-shaped part inside. Don't
      |   hand-unroll this into one recursive call per item.
      |     files.map(f => agent[String](s"summarise the contents of file $f: ..."))
      |
      |3. MID sub-task — one self-contained piece needs an LLM (write prose,
      |   score, classify, choose); bind its result and carry on:
      |     val title = agent[String](s"a punchy title for: $topic")
      |     s"# $title\n$body"
      |
      |4. TAIL CONTINUATION — a complex task whose NEXT step depends on what this
      |   step finds (search, multi-hop lookup, "look, then decide"). Do ONE round
      |   of work, then make the LAST expression a TAIL `agent[T]` that sees the
      |   intermediate results and decides what to do next — go deeper or answer.
      |   Fold the results into its task TEXT so the next call actually sees them:
      |     {
      |       val r1 = search(query).map(h => s"[${h.docid}] ${h.snippet}").mkString("\n")
      |       agent[String](s"Goal: <...>. So far r1 = $r1. If this pins the answer,
      |         give it in the required format; otherwise reformulate and continue.")
      |     }
      |   This makes control flow ADAPT to the data instead of following a fixed
      |   script — it is how you recover when a round comes back empty, rather than
      |   committing to a guess. Don't hard-code a whole multi-step plan into one
      |   straight-line body. The depth limit bounds the loop.
      |
      |The chain of sub-tasks that led here is supplied as the prior conversation
      |turns, so keep each sub-task description short — don't repeat parent context.""".stripMargin

  /** Prepended to the (dynamic) user prompt when the call is at the hard depth
   *  limit — it OVERRIDES [[RecursionGuide]] from the fixed system prompt. Kept
   *  out of the system prompt on purpose: that must stay byte-identical across
   *  calls for the input cache to hit, so call-specific state lives here. */
  private val DepthLimitGuide =
    s"""DEPTH LIMIT: despite the recursion guidance in the system message, you
       |are at the maximum agent nesting depth (${AgentDepth.max}). Do NOT call
       |`agent[...]` anywhere in your expression — a nested call will throw and
       |abort the whole computation. Produce the result DIRECTLY with plain Scala
       |and the in-scope helpers; return your best effort rather than delegating.""".stripMargin

  private val StyleGuide =
    """Keep the generated code readable and traceable — it becomes the enclosing
      |source a nested `agent[...]` sees, and what the next retry sees on failure:
      |  - Start with one short `// task: ...` comment restating the goal in your
      |    own words, then a terse `// step N: ...` comment before each non-trivial
      |    step. One line each; never paste large text or data into a comment.
      |  - Do NOT define local classes, case classes, or traits. Use tuples
      |    for structured data: e.g. `val hit: (String, Double) = ("doc42", 0.87)`
      |    then `hit._1` / `hit._2`, or destructure `val (id, score) = hit`.
      |  - Print progress with `println("[agent] ...")` at notable steps; it
      |    streams to the user. Wrap a bare value as `{ println("[agent] ..."); v }`.
      |  - Runs are non-interactive: any stdin read returns `null` — fall back to a
      |    sensible default, never block waiting for input.""".stripMargin

  private val FailureGuide =
    """On a retry, the previous attempt and its error (compile error or runtime
      |exception) are shown below. Fix the cause and emit a corrected expression.
      |If the failure is genuinely unrecoverable, emit code that throws a clear
      |`RuntimeException("agent: <reason>")` so the cause surfaces to the user.""".stripMargin

  /** The constant guidance, sent as the `system` message. FIXED — byte-identical
   *  on every call (no call-specific state, not even the depth-limit flag), so
   *  the provider's input cache hits across the whole run. The at-limit override
   *  rides in the dynamic user prompt instead (see [[DepthLimitGuide]]). */
  val systemPrompt: String =
    List(Intro, RecursionGuide, StyleGuide, FailureGuide).mkString("\n\n")

  /** The ancestor `agent[...]` chain rendered as prior conversation turns: for
   *  each ancestor (outermost first), the sub-task it was asked for (`user`)
   *  and the code it produced (`assistant`). This is how the current call's
   *  context reaches the model — the path that led here — instead of being
   *  spliced into the generated source. */
  def historyMessages(entries: List[AgentHistory.Entry]): Seq[LLMChat.Message] =
    entries.flatMap { e =>
      val typeStr = if e.expectedType.nonEmpty then s" (type `${e.expectedType}`)" else ""
      Seq(
        LLMChat.Message("user", s"Sub-task$typeStr: ${e.task}"),
        LLMChat.Message("assistant", e.code)
      )
    }

  /** The per-call request, sent as the final `user` message: the dynamic
   *  context — at-limit override when `atLimit`, expected type, enclosing
   *  source, in-scope bindings, the task, and (on a retry) the previous attempt
   *  and its errors. Everything that varies per call lives here, never in the
   *  fixed system prompt. Empty sections are dropped so we never emit doubled
   *  blank lines. */
  def userPrompt(
      task: String,
      ctx: EvalContext,
      prevCode: String,
      prevErrors: List[String],
      expectedType: String,
      atLimit: Boolean,
      replHistory: String
  ): String =
    List(
      if atLimit then DepthLimitGuide else "",
      typeSection(expectedType),
      historySection(replHistory),
      contextSection(ctx),
      bindingsSection(ctx),
      s"Task: $task",
      errorSection(prevCode, prevErrors)
    ).filter(_.nonEmpty).mkString("\n\n")

  // Just the per-call type; how the type is enforced is explained once in the
  // system prompt (Intro), so it isn't repeated here.
  private def typeSection(expectedType: String): String =
    if expectedType.nonEmpty then s"Required type of your expression: `$expectedType`"
    else
      "Type of your expression: not pinned at the call site (inferred); " +
        "prefer a precise type anyway."

  /** Recent REPL transcript section (off by default; see [[HistoryEnabled]]) —
   *  grounds the LLM in what the user did in the session before this call. */
  private def historySection(replHistory: String): String =
    if replHistory.isEmpty then ""
    else
      s"""Recent REPL session transcript (most-recent at the bottom) — the session
         |so far BEFORE the current request: earlier inputs, the results they
         |produced, and any printed output. Use it to avoid redoing work, to refer
         |to vals/defs already in scope, and to stay consistent with what is already
         |established:
         |```
         |$replHistory
         |```""".stripMargin

  private def contextSection(ctx: EvalContext): String =
    s"""Placeholder marker: ${ctx.placeholder}
       |Enclosing source:
       |${ctx.enclosingSource}""".stripMargin

  private def bindingsSection(ctx: EvalContext): String =
    if ctx.bindings.isEmpty then ""
    else s"In-scope bindings: ${ctx.bindings.iterator.map(_.name).mkString(", ")}"

  private def errorSection(prevCode: String, prevErrors: List[String]): String =
    if prevErrors.isEmpty then ""
    else
      s"""Your previous attempt:
         |$prevCode
         |
         |failed with:
         |${prevErrors.mkString("\n")}
         |
         |Emit a corrected expression (or throw a clear exception if the failure
         |is unrecoverable). Output ONLY the expression.""".stripMargin

  /** Format a body's runtime exception for the next attempt's prompt: type,
   *  message, and the user-visible stack frames (it stops at the first
   *  REPL-internal eval frame), plus the cause. */
  def formatThrown(e: Throwable): List[String] =
    val msg = Option(e.getMessage).getOrElse("(no message)")
    val head = s"Body threw ${e.getClass.getName}: $msg"
    def isReplInternal(cn: String): Boolean =
      cn.startsWith("dotty.tools.repl.") ||
        cn.startsWith("jdk.internal.reflect.") ||
        cn == "java.lang.reflect.Method"
    val frames = e.getStackTrace.iterator
      .takeWhile(f => !isReplInternal(f.getClassName))
      .take(8)
      .map(f => s"  at ${f}")
      .toList
    val cause = Option(e.getCause)
      .map(c => s"caused by ${c.getClass.getName}: ${Option(c.getMessage).getOrElse("(no message)")}")
      .toList
    head :: frames ::: cause

  /** LLM responses sometimes wrap code in ``` fences despite our
   *  instruction. Strip them defensively.
   */
  def stripCodeFences(s: String): String =
    val t = s.trim
    if t.startsWith("```") then
      t.stripPrefix("```scala").stripPrefix("```").stripSuffix("```").trim
    else t

end AgentPrompt
