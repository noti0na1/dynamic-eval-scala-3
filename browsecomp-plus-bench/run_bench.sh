#!/usr/bin/env bash
# Run the BrowseComp-Plus benchmark with the Lacuna `agent` primitive.
#
# Prerequisites:
#   1. source env.sh                            (embedding + agent + judge keys)
#   2. .venv/bin/python retriever_server.py &   (retriever shim, another shell)
#
# Usage: ./run_bench.sh [N] [START]
#   N      number of queries to run           (default 25)
#   START  0-based offset into queries_slim   (default 0; for sharding)
set -euo pipefail
cd "$(dirname "$0")"
N="${1:-25}"
START="${2:-0}"

# Per-query wall-clock budget (seconds). Each query runs in its own short-lived
# REPL, and a watchdog kills that REPL if it overruns — so a runaway agent
# (looping without converging) loses only its own query, not the shard. Raise it
# if legitimate long queries get cut (observed tail: a few queries up to ~20min).
QUERY_TIMEOUT_SEC="${QUERY_TIMEOUT_SEC:-600}"

# Logical run id, used as the runs/<id>/ output dir (Search.scala reads
# BENCH_RUN_ID). Format: <timestamp>_<model>, so separate runs — e.g. a
# different AGENT_MODEL — land in separate directories instead of overwriting
# each other. run_parallel.sh sets/exports this once so all shards of one run
# share the same output dir (they write disjoint qids); a standalone run
# computes it here. The model name is slugified (/, space, : -> -) for path
# safety.
_model_slug="$(printf '%s' "${AGENT_MODEL:-deepseek-v4-flash}" | tr '/ :' '---')"
export BENCH_RUN_ID="${BENCH_RUN_ID:-$(date +%Y%m%d-%H%M%S)_${_model_slug}}"

# Per-shard eval-log + history directory, so concurrent / sharded runs never
# collide. Search.scala reads EVAL_LOG_DIR to attribute each round's generated
# code to the query that produced it; the -s${START} suffix keeps each shard's
# logs separate even though they share BENCH_RUN_ID.
export EVAL_LOG_DIR="log/run-${BENCH_RUN_ID}-s${START}"
mkdir -p "$EVAL_LOG_DIR"
echo "[run_bench] N=$N START=$START BENCH_RUN_ID=$BENCH_RUN_ID EVAL_LOG_DIR=$EVAL_LOG_DIR"

# Each query runs in its OWN short-lived REPL (BENCH_N=1) so the timeout is
# truly per-query: a runaway loses only that query and the shard continues. The
# agent[String] call site still needs the eval-rewriter, so each query `:load`s
# Bench.scala in a REPL; queries are independent (REPL history off), so a fresh
# REPL per query is equivalent — it just pays JVM/compile startup each time.
#
# A per-query eval-log SUBDIR makes each REPL uniquely identifiable on the
# command line, so the watchdog can kill exactly that query's process tree
# (scala-cli + its JVM child) by matching the path — never the next query's.
fail=0
for (( i = 0; i < N; i++ )); do
  q=$(( START + i ))
  qLogDir="${EVAL_LOG_DIR}/q${q}"
  mkdir -p "$qLogDir"
  echo "[run_bench] === offset $q (timeout ${QUERY_TIMEOUT_SEC}s) ==="

  ( printf ':load Bench.scala\n' \
      | BENCH_N=1 BENCH_START="$q" EVAL_LOG_DIR="$qLogDir" \
        scala-cli repl --server=false \
          -O -Xrepl-eval-log-dir:"${qLogDir}/" \
          -O -Xrepl-history-file:"${qLogDir}/session.repl" \
          Agent.scala Search.scala ) &
  replPid=$!

  # Watchdog: after the budget, if the REPL is still alive, kill its tree by the
  # unique per-query log path (matches scala-cli + the JVM child, nothing else).
  ( sleep "$QUERY_TIMEOUT_SEC"
    if kill -0 "$replPid" 2>/dev/null; then
      echo "[run_bench] !! offset $q exceeded ${QUERY_TIMEOUT_SEC}s -> killing its REPL"
      pkill -f "${qLogDir}/" 2>/dev/null || true
    fi ) &
  watchdog=$!

  if wait "$replPid"; then :; else fail=1; fi   # killed query -> non-zero, don't abort
  kill "$watchdog" 2>/dev/null || true           # cancel watchdog if query finished in time
  wait "$watchdog" 2>/dev/null || true
done
echo "[run_bench] shard done: offset=$START n=$N fail=$fail"
