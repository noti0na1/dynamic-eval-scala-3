#!/usr/bin/env bash
# Re-run every query that has no result file yet, into the SAME run dir.
#
# Unlike rerun_killed.sh (which sources offsets from watchdog "killing its REPL"
# log lines), this finds offsets whose query_id has no runs/<id>/<qid>.json — the
# gap left when a run is interrupted (e.g. the launcher process dies mid-run)
# rather than when individual queries hit the per-query watchdog. Uses the
# standard 600s budget by default (override with QUERY_TIMEOUT_SEC).
#
# Each offset runs as its own `run_bench.sh 1 <offset>` sharing the exported
# BENCH_RUN_ID, so results land in runs/<id>/ alongside the first pass.
#
# Usage: BENCH_RUN_ID=<run-dir> [RERUN_CONC=8] [QUERY_TIMEOUT_SEC=600] ./rerun_missing.sh
set -euo pipefail
cd "$(dirname "$0")"
: "${BENCH_RUN_ID:?export BENCH_RUN_ID=<run dir under runs/ to fill>}"
export QUERY_TIMEOUT_SEC="${QUERY_TIMEOUT_SEC:-600}"
CONC="${RERUN_CONC:-8}"
QUERIES="data/queries_slim.jsonl"

# Missing offsets: line N (1-based) -> offset N-1; missing if its query_id has
# no run file. python for robust JSON parsing of each query_id.
missing=$(.venv/bin/python - "$QUERIES" "runs/$BENCH_RUN_ID" <<'PY'
import json, os, sys
qfile, rundir = sys.argv[1], sys.argv[2]
out = []
for i, line in enumerate(open(qfile, encoding="utf-8")):
    line = line.strip()
    if not line:
        continue
    qid = str(json.loads(line)["query_id"])
    if not os.path.exists(os.path.join(rundir, f"{qid}.json")):
        out.append(str(i))
print(" ".join(out))
PY
)
n=$( (printf '%s\n' $missing | grep -c .) || true )
echo "[rerun-missing] BENCH_RUN_ID=$BENCH_RUN_ID  CONC=$CONC  QUERY_TIMEOUT_SEC=${QUERY_TIMEOUT_SEC}s"
echo "[rerun-missing] $n missing offsets: $missing"
[ "$n" -eq 0 ] && { echo "[rerun-missing] nothing to do"; exit 0; }

pids=()
for q in $missing; do
  echo "[rerun-missing] launch offset $q -> data/rerun-missing-s${q}.log"
  ./run_bench.sh 1 "$q" > "data/rerun-missing-s${q}.log" 2>&1 &
  pids+=("$!")
  # Rolling window: block until fewer than CONC of our jobs are alive (bash 3.2 safe).
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
echo "[rerun-missing] ALL DONE (missing offsets now have <qid>.json in runs/$BENCH_RUN_ID)"
