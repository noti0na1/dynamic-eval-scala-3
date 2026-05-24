#!/usr/bin/env bash
# Re-run the queries the first pass's watchdog killed, into the SAME run dir.
# The killed offsets are derived from data/shard-s*.log ("killing its REPL").
# Uses lower concurrency and a larger per-query timeout than the full 16-shard
# run, to recover the non-converged tail that hit the 600s cap under contention.
#
# Each offset runs as its own `run_bench.sh 1 <offset>` (BENCH_N=1, its own
# EVAL_LOG_DIR=log/run-<id>-s<offset>), all sharing the exported BENCH_RUN_ID so
# results land in runs/<id>/ alongside the first pass (disjoint qids).
#
# Usage: BENCH_RUN_ID=<run-dir> [RERUN_CONC=6] [QUERY_TIMEOUT_SEC=900] ./rerun_killed.sh
set -euo pipefail
cd "$(dirname "$0")"
: "${BENCH_RUN_ID:?export BENCH_RUN_ID=<run dir under runs/ to fill>}"
export QUERY_TIMEOUT_SEC="${QUERY_TIMEOUT_SEC:-900}"
CONC="${RERUN_CONC:-6}"

offsets=$( (grep -rh 'killing its REPL' data/shard-s*.log 2>/dev/null \
  | grep -oE 'offset [0-9]+' | awk '{print $2}' | sort -n -u) || true )
n=$( (printf '%s\n' $offsets | grep -c .) || true )
echo "[rerun] BENCH_RUN_ID=$BENCH_RUN_ID  CONC=$CONC  QUERY_TIMEOUT_SEC=${QUERY_TIMEOUT_SEC}s"
echo "[rerun] $n killed offsets to recover: $offsets"

pids=()
for q in $offsets; do
  echo "[rerun] launch offset $q -> data/rerun-s${q}.log"
  ./run_bench.sh 1 "$q" > "data/rerun-s${q}.log" 2>&1 &
  pids+=("$!")
  # Rolling window: block until fewer than CONC of OUR launched jobs are alive.
  # Count live PIDs with `kill -0` — this runs in the script's own shell, so it
  # sees the background jobs (a `$(jobs)` subshell does not). bash 3.2 safe.
  while :; do
    live=0
    for p in "${pids[@]}"; do
      if kill -0 "$p" 2>/dev/null; then live=$((live + 1)); fi
    done
    if [ "$live" -lt "$CONC" ]; then break; fi
    sleep 2
  done
done
wait
echo "[rerun] ALL DONE (recovered offsets now have <qid>.json in runs/$BENCH_RUN_ID)"
