#!/usr/bin/env bash
# Run the tau2-bench bench sharded across parallel orchestrator+REPL processes.
#
# Each shard is one repl_bench.py orchestrator driving its own persistent
# `scala-cli repl` over a disjoint task slice of one domain (the `agent` primitive
# runs single-threaded within a REPL session). Each shard gets its own
# EVAL_LOG_DIR + history file (run_bench.sh) and writes disjoint task ids into the
# shared runs/<id>/, so they never collide. The gym shim holds one isolated env
# (with its own orchestrator thread) per session id and serves the concurrent
# load (threaded).
#
# Usage: ./run_parallel.sh [DOMAIN] [TOTAL] [SHARDS]   (defaults retail, 10, 4)
set -euo pipefail
cd "$(dirname "$0")"
DOMAIN="${1:-retail}"
TOTAL="${2:-10}"
SHARDS="${3:-4}"
BASE=$(( TOTAL / SHARDS ))
REM=$(( TOTAL % SHARDS ))
mkdir -p log

# One run id for the whole parallel run, exported so every shard's run_bench.sh
# inherits it and writes into the same runs/<id>/ (disjoint task ids). Computed
# here (not per shard) so all shards agree on the output dir. Format
# <timestamp>_<domain>_<model>.
_model_slug="$(printf '%s' "${AGENT_MODEL:-deepseek-v4-flash}" | tr '/ :' '---')"
export BENCH_RUN_ID="${BENCH_RUN_ID:-$(date +%Y%m%d-%H%M%S)_${DOMAIN}_${_model_slug}}"
echo "[parallel] DOMAIN=$DOMAIN TOTAL=$TOTAL SHARDS=$SHARDS (base=$BASE, +1 for first $REM) BENCH_RUN_ID=$BENCH_RUN_ID"

pids=()
start=0
for (( i = 0; i < SHARDS; i++ )); do
  n=$BASE
  (( i < REM )) && n=$(( BASE + 1 ))
  (( n <= 0 )) && continue
  ./run_bench.sh "$DOMAIN" "$n" "$start" > "log/shard-${BENCH_RUN_ID}-s${start}.log" 2>&1 &
  pid=$!                       # capture before append (macOS bash 3.2: no [-1])
  pids+=("$pid")
  echo "[parallel] shard $i: start=$start n=$n pid=$pid"
  start=$(( start + n ))
  sleep 0.5   # stagger to avoid a thundering herd on the scala-cli build cache
done

echo "[parallel] launched ${#pids[@]} shards; waiting..."
fail=0
for pid in "${pids[@]}"; do
  wait "$pid" || { fail=1; echo "[parallel] pid $pid exited non-zero"; }
done
echo "[parallel] ALL SHARDS DONE (fail=$fail)"
