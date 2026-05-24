//> using scala 3.9.0-RC1-bin-SNAPSHOT
//> using dep com.lihaoyi::upickle:4.4.3
//> using dep com.lihaoyi::os-lib:0.11.9-M7

// BrowseComp-Plus retrieval tools + per-query trace collection.
//
// Co-loaded into the REPL next to Agent.scala:
//   scala-cli repl --server=false ../Agent.scala Search.scala
//
// `search` / `getDocument` are ordinary in-scope functions (paper sec:tools):
// the generated agent code calls them by bare name. They talk to the local
// Python retriever shim (retriever_server.py) over plain HTTP, and every call
// is recorded into a per-query trace.
//
// For each query we collect, as systematically as possible (<run-id> is the
// per-run output dir, BENCH_RUN_ID — `<timestamp>_<model>` — or `lacuna`):
//   runs/<run-id>/<qid>.json        rich run file (interleaved tool trace +
//                                   timing + eval-round count + final answer)
//   runs/<run-id>/trace/<qid>/progress.log captured agent stdout ([agent] ...)
//   runs/<run-id>/trace/<qid>/eval_*.scala the LLM-generated code snippets,
//                                   enclosing sources and compile errors of
//                                   every agent/eval round (copied from the
//                                   -Xrepl-eval-log-dir directory)

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{Duration, Instant}

object Bcp:

  /** Base URL of the local retriever shim. */
  val serverUrl: String =
    sys.env.getOrElse("RETRIEVER_URL", "http://127.0.0.1:8765")

  /** Retrieval budget per search. BrowseComp-Plus fixes this at 5 across all
   *  methods — its `search` tool has no `k` parameter — so agents are compared
   *  on reasoning and tool use, not retrieval depth. Overridable via SEARCH_K
   *  for ablations only; leave at 5 for protocol-faithful runs. */
  private val searchK: Int =
    sys.env.get("SEARCH_K").map(_.trim.toInt).getOrElse(5)

  /** Model name recorded into each run file's metadata (read by the BrowseComp
   *  evaluator to label the row). Matches Agent.scala's `AGENT_MODEL`. */
  val agentModel: String =
    sys.env.getOrElse("AGENT_MODEL", "deepseek-v4-flash")

  /** Identifier for THIS run, used as the `runs/<run-id>/` output directory so
   *  separate runs (e.g. different agent models) never overwrite each other.
   *  Set by run_bench.sh / run_parallel.sh to `<timestamp>_<model>`; all shards
   *  of one parallel run share it (they write disjoint qids into the same dir).
   *  Falls back to `lacuna` for an ad-hoc REPL session that doesn't set it
   *  (matches the historical directory name). */
  val runId: String =
    sys.env.get("BENCH_RUN_ID").map(_.trim).filter(_.nonEmpty).getOrElse("lacuna")

  /** Directory the REPL writes eval logs to: the `-Xrepl-eval-log-dir` value
   *  run_bench.sh passes to scala-cli, mirrored here via `EVAL_LOG_DIR` so the
   *  trace copier reads the same per-run directory. Each agent/eval round
   *  drops a `_wrapper.scala`, `_enclosingSource.scala` and (on failure)
   *  `_error.scala` there. */
  private val logDir   = os.Path(sys.env.getOrElse("EVAL_LOG_DIR", "log"), os.pwd)
  private val runDir   = os.pwd / "runs" / runId
  private val traceDir = runDir / "trace"

  private val http =
    HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(30)).build()

  /** One corpus search hit. `snippet` is the truncated document text. */
  final case class Hit(docid: String, score: Double, snippet: String)

  // --- per-query trace state (the bench driver is single-threaded) -----------
  private val retrieved   = scala.collection.mutable.LinkedHashSet.empty[String]
  private val resultTrace = scala.collection.mutable.ArrayBuffer.empty[ujson.Value]
  private var nSearch     = 0
  private var nGetDoc     = 0
  private var logSnapshot = Set.empty[String]

  private def logFileNames: Set[String] =
    if os.exists(logDir) then os.list(logDir).iterator.map(_.last).toSet
    else Set.empty

  def resetTrace(): Unit =
    retrieved.clear(); resultTrace.clear()
    nSearch = 0; nGetDoc = 0
    LLMUsage.reset()           // per-query LLM token accounting
    AgentStats.reset()         // per-query retry / compile accounting
    AgentDepth.reset()         // per-query agent recursion-depth guard
    AgentHistory.reset()       // per-query agent-call conversation stack
    logSnapshot = logFileNames // baseline: eval logs already present pre-query

  def retrievedDocids: List[String] = retrieved.toList
  def toolCounts: (Int, Int)        = (nSearch, nGetDoc)

  private def post(path: String, body: ujson.Obj): ujson.Value =
    val req = HttpRequest.newBuilder()
      .uri(URI.create(serverUrl + path))
      .header("Content-Type", "application/json")
      .timeout(Duration.ofSeconds(120))
      .POST(HttpRequest.BodyPublishers.ofString(body.render()))
      .build()
    val resp = http.send(req, HttpResponse.BodyHandlers.ofString())
    if resp.statusCode() / 100 != 2 then
      throw RuntimeException(s"retriever HTTP ${resp.statusCode()}: ${resp.body()}")
    ujson.read(resp.body())

  /** Search the fixed BrowseComp-Plus corpus; returns the top-[[searchK]] hits. */
  def search(query: String): List[Hit] =
    nSearch += 1
    val arr = post("/search", ujson.Obj("query" -> query, "k" -> searchK)).arr
    val hits = arr.iterator
      .map(j => Hit(j("docid").str, j("score").num, j("snippet").str))
      .toList
    hits.foreach(h => retrieved += h.docid)
    resultTrace += ujson.Obj(
      "type" -> "tool_call", "tool_name" -> "search",
      "arguments" -> ujson.Obj("query" -> query, "k" -> searchK))
    resultTrace += ujson.Obj(
      "type" -> "tool_result", "tool_name" -> "search",
      "output" -> ujson.Arr(hits.map(h => ujson.Obj(
        "docid" -> h.docid, "score" -> h.score,
        "snippet_chars" -> h.snippet.length))*))
    hits

  /** Fetch the full text of a corpus document by its docid. */
  def getDocument(docid: String): String =
    nGetDoc += 1
    retrieved += docid
    val text = post("/get_document", ujson.Obj("docid" -> docid))("text").str
    resultTrace += ujson.Obj(
      "type" -> "tool_call", "tool_name" -> "get_document",
      "arguments" -> ujson.Obj("docid" -> docid))
    resultTrace += ujson.Obj(
      "type" -> "tool_result", "tool_name" -> "get_document",
      "output" -> ujson.Obj("docid" -> docid, "text_chars" -> text.length))
    text

  /** Run one query end to end: reset the trace, capture the agent's stdout,
   *  time it, then write the run file and trace folder. `body` is by-name so
   *  the `agent[String]` call inside it stays at the Bench.scala REPL call
   *  site (where the eval-rewriter fires). */
  def execute(qid: String, question: String)(body: => String): Unit =
    resetTrace()
    println(s"[bench] === query $qid ===")
    val startMs  = System.currentTimeMillis()
    val captured = new java.io.ByteArrayOutputStream()
    val realOut  = System.out
    val tee = new java.io.PrintStream(
      new java.io.OutputStream:
        override def write(b: Int): Unit = { realOut.write(b); captured.write(b) }
        override def write(b: Array[Byte], o: Int, l: Int): Unit =
          { realOut.write(b, o, l); captured.write(b, o, l) },
      true, "UTF-8")
    val answer: String =
      try
        // Capture both Java-side System.out and Scala's Console.out: the
        // agent's generated code prints `[agent] ...` via Scala `println`,
        // which goes through Console.out (System.setOut alone misses it).
        System.setOut(tee)
        try scala.Console.withOut(tee)(scala.Console.withErr(tee)(body))
        finally { System.setOut(realOut); tee.flush() }
      catch
        case e: Throwable =>
          println(s"[bench] query $qid failed: ${e.getMessage}")
          s"Explanation: agent error: ${e.getMessage}\nExact Answer: None\nConfidence: 0%"
    val endMs = System.currentTimeMillis()
    writeRun(qid, question, answer, startMs, endMs,
      captured.toString("UTF-8"))

  /** Write the run file (BrowseComp-Plus evaluator schema, enriched) plus the
   *  per-query trace folder. */
  private def writeRun(
      qid: String, question: String, answer: String,
      startMs: Long, endMs: Long, progress: String): Unit =
    val (s, g)  = toolCounts
    val newLogs = (logFileNames -- logSnapshot).toSeq.sorted
    val rounds  = newLogs.count(_.endsWith("_wrapper.scala"))

    // per-query trace folder: captured stdout + every generated-code round
    val qTrace = traceDir / qid
    os.makeDir.all(qTrace)
    os.write.over(qTrace / "progress.log", progress)
    newLogs.foreach(fn => os.copy.over(logDir / fn, qTrace / fn))

    val result = resultTrace.toList :+
      ujson.Obj("type" -> "output_text", "output" -> answer)
    val obj = ujson.Obj(
      "metadata" -> ujson.Obj(
        "model"        -> agentModel,
        "started_at"   -> Instant.ofEpochMilli(startMs).toString,
        "ended_at"     -> Instant.ofEpochMilli(endMs).toString,
        "duration_sec" -> (endMs - startMs) / 1000.0,
        "eval_rounds"  -> rounds,
        "agent_stats"  -> ujson.Obj(
          "agent_calls"     -> AgentStats.agentCalls,
          "attempts"        -> AgentStats.attempts,
          "retries"         -> AgentStats.retries,
          "compiles_ok"     -> AgentStats.compilesOk,
          "compiles_failed" -> AgentStats.compilesFailed),
        "llm_usage"    -> ujson.Obj(
          "calls"             -> LLMUsage.calls,
          "prompt_tokens"     -> LLMUsage.promptTokens,
          "completion_tokens" -> LLMUsage.completionTokens,
          "total_tokens"      -> LLMUsage.totalTokens)),
      "query_id"         -> qid,
      "question"         -> question,
      "tool_call_counts" -> ujson.Obj("search" -> s, "get_document" -> g),
      "status"           -> "completed",
      "retrieved_docids" -> ujson.Arr(retrievedDocids.map(ujson.Str(_))*),
      "result"           -> ujson.Arr(result*)
    )
    os.makeDir.all(runDir)
    os.write.over(runDir / s"$qid.json", obj.render(indent = 2))
    println(s"[bench] query $qid done -> runs/$runId/$qid.json " +
      s"(search=$s, get_document=$g, docids=${retrievedDocids.size}, " +
      s"rounds=$rounds, ${(endMs - startMs) / 1000}s)")

end Bcp

/** In-scope tool aliases so generated agent code can call them by bare name.
 *  `search` has no `k` parameter — the retrieval budget is fixed, matching the
 *  benchmark's own search tool. */
def search(query: String): List[Bcp.Hit] = Bcp.search(query)
def getDocument(docid: String): String    = Bcp.getDocument(docid)

/** The research task handed to `agent[String]` for one BrowseComp-Plus query.
 *  Tuned from a 25-query pilot run: the agent was cramming whole questions into
 *  one query and giving up with "None" / "unknown" (always scored wrong). The
 *  retrieval budget is fixed (top-5 per search, benchmark protocol), so the
 *  agent must compensate with better and more numerous queries, not deeper
 *  retrieval. */
def bcpTask(question: String): String =
  s"""You are a deep research agent answering a hard, multi-clue question over
     |a fixed ~100,000-document corpus. You should use the two functions:
     |  def search(query: String): List[Bcp.Hit]  // top-5 hits: Hit(docid, score, snippet)
     |  def getDocument(docid: String): String    // full document text
     |
     |SEARCH STRATEGY — each search returns only the top 5 hits, so search well
     |and search often:
     |  - Issue MANY short, focused queries: one distinctive fact per query
     |    (a name, date, place, event, title). Do NOT cram the whole question
     |    into a single query — keyword-salad queries retrieve badly.
     |  - Treat each clue in the question as its own search, and search several
     |    rephrasings of each clue (synonyms, broader or narrower wording).
     |  - If a query returns nothing useful, REFORMULATE and search again.
     |  - Cross-reference the docids that recur across clues.
     |  - Once you form a candidate answer, search for it BY NAME to verify the
     |    remaining clues against it.
     |  - Call getDocument on the handful (~5-10) of most promising docids to
     |    read full text before committing — do not fetch indiscriminately.
     |
     |ANSWER RULES:
     |  - You MUST commit to a single best-guess answer. NEVER answer "None",
     |    "unknown", or "insufficient evidence": if unsure, give your most
     |    likely guess and lower the Confidence score instead.
     |  - Exact Answer must be a short noun phrase / name / number — never a
     |    sentence or a description.
     |  - Set Confidence honestly: high only with direct corroborating evidence,
     |    below 40 for a weakly-supported guess.
     |  - In any recursive agent[String] call, pin a concrete type (agent[String],
     |    agent[Int], ...) — never an invented type name.
     |
     |Return your final answer as a Scala String LITERAL (a triple-quoted
     |s-string), NOT as bare prose — emitting the lines below unquoted is invalid
     |Scala and will fail to compile. The string's content must be EXACTLY:
     |Explanation: <reasoning; cite evidence docids inline in square brackets, e.g. [5412]>
     |Exact Answer: <succinct final answer>
     |Confidence: <integer 0-100>%
     |
     |Question: $question""".stripMargin
