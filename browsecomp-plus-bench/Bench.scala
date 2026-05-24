// BrowseComp-Plus benchmark driver.
//
// This file is `:load`ed INTO a running REPL session (not passed to
// `scala-cli repl` as a file argument) so that the `agent[String]` call site
// inside `runQuery` is compiled through the REPL's eval-rewrite phase. Passing
// it as a plain file argument would compile it with the ordinary pipeline and
// the rewriter would never fire.
//
// `:load` runs the whole file top to bottom, so the trailing `runBench(...)`
// statement fires the benchmark. The query count comes from the BENCH_N env
// var (default 25). run_bench.sh wires this up:
//
//   printf ':load Bench.scala\n' | BENCH_N=25 \
//     scala-cli repl --server=false ../Agent.scala Search.scala

import scala.io.Source

/** Run one BrowseComp-Plus query end to end and write its run file + trace.
 *  The `agent[String]` call is the recursive ReAct loop (paper sec:react):
 *  the generated body searches the corpus and either answers or recurses.
 *  It is passed by-name to `Bcp.execute`, so the call site stays here in
 *  REPL-compiled code where the eval-rewriter fires. */
def runQuery(qid: String, question: String): Unit =
  Bcp.execute(qid, question)(agent[String](bcpTask(question)))

/** Run `n` queries of the decrypted dataset starting at offset `start`
 *  (0-based). The offset makes it easy to shard a large run across several
 *  REPL processes. */
def runBench(n: Int, start: Int = 0): Unit =
  val src = Source.fromFile("data/queries_slim.jsonl")
  val lines =
    try src.getLines().slice(start, start + n).toList
    finally src.close()
  for line <- lines do
    val o = ujson.read(line)
    runQuery(o("query_id").str, o("query").str)
  println(s"[bench] completed ${lines.size} queries " +
    s"(offset $start) -> runs/${Bcp.runId}/")

// `:load` executes this; BENCH_N / BENCH_START select the slice to run.
runBench(
  sys.env.get("BENCH_N").map(_.trim.toInt).getOrElse(25),
  sys.env.get("BENCH_START").map(_.trim.toInt).getOrElse(0))
