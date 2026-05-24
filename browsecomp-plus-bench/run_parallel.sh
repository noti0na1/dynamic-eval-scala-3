#!/usr/bin/env bash
# Run the BrowseComp-Plus bench sharded across parallel REPL processes.
#
# The `agent` primitive runs single-threaded inside one REPL session, so
# parallelism = several REPL processes over disjoint query slices. Each shard
# gets its own EVAL_LOG_DIR (run_bench.sh) and writes disjoint query ids into
# the shared runs/<run-id>/, so they never collide. The retriever is threaded
# and handles the concurrent load.
#
# Usage: ./run_parallel.sh [TOTAL] [SHARDS]   (defaults 25, 8)
set -euo pipefail
cd "$(dirname "$0")"
TOTAL="${1:-25}"
SHARDS="${2:-8}"
BASE=$(( TOTAL / SHARDS ))
REM=$(( TOTAL % SHARDS ))

# One run id for the whole parallel run, exported so every shard's run_bench.sh
# inherits it and writes into the same runs/<id>/ (disjoint qids). Computed here
# (not per shard) so all shards agree on the output dir. Format <timestamp>_<model>.
_model_slug="$(printf '%s' "${AGENT_MODEL:-deepseek-v4-flash}" | tr '/ :' '---')"
export BENCH_RUN_ID="${BENCH_RUN_ID:-$(date +%Y%m%d-%H%M%S)_${_model_slug}}"
echo "[parallel] TOTAL=$TOTAL SHARDS=$SHARDS (base=$BASE, +1 for first $REM) BENCH_RUN_ID=$BENCH_RUN_ID"

pids=()
start=0
for (( i = 0; i < SHARDS; i++ )); do
  n=$BASE
  (( i < REM )) && n=$(( BASE + 1 ))
  (( n <= 0 )) && continue
  ./run_bench.sh "$n" "$start" > "data/shard-s${start}.log" 2>&1 &
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
