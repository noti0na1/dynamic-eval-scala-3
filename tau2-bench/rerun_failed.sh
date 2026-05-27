#!/usr/bin/env bash
# Re-run COMPLETELY-FAILED tasks — those with NO run file — for an existing run,
# back into the SAME runs/<RUN_ID>/. Use after a full run where a shard aborted
# (e.g. a REPL died under memory pressure and couldn't restart), leaving gaps in
# the task set. Tasks that DID complete (even with reward 0) are left untouched;
# only missing run files are re-run.
#
# Prerequisites (same as run_bench.sh):
#   1. source env.sh                 (agent + user-simulator keys/models)
#   2. python tau2_server.py &       (gym shim must be running)
#
# Usage: ./rerun_failed.sh DOMAIN RUN_ID [SHARDS]
#   DOMAIN  retail | airline | telecom | telecom-workflow | mock
#   RUN_ID  the runs/<RUN_ID>/ dir to backfill
#   SHARDS  parallel orchestrators (default 4; capped at #missing)
set -euo pipefail
cd "$(dirname "$0")"
DOMAIN="${1:?usage: ./rerun_failed.sh DOMAIN RUN_ID [SHARDS]}"
RUN_ID="${2:?usage: ./rerun_failed.sh DOMAIN RUN_ID [SHARDS]}"
SHARDS="${3:-4}"

# This dir's venv python (env.sh's venv activation can be unreliable; be explicit).
export PATH="$PWD/.venv/bin:$PATH"
export BENCH_RUN_ID="$RUN_ID"
export AGENT_REPL_HISTORY="${AGENT_REPL_HISTORY:-on}"
export AGENT_FILE="${AGENT_FILE:-../browsecomp-plus-bench/Agent.scala}"
CAMEL=$(echo "$DOMAIN" | awk -F'[-_]' '{for(i=1;i<=NF;i++) printf "%s", toupper(substr($i,1,1)) substr($i,2)}')
export TAU_FACADE="facades/${CAMEL}.scala"
[ -f "$TAU_FACADE" ]   || { echo "[rerun] ERROR: no facade $TAU_FACADE"; exit 1; }
[ -d "runs/$RUN_ID" ]  || { echo "[rerun] ERROR: runs/$RUN_ID not found"; exit 1; }

# Total tasks for the domain, then the indices with no run file.
NTOTAL=$(python -c "from tau2.registry import registry; print(len(list(registry.get_tasks_loader('$DOMAIN')())))")
MISSING=$(python - "$DOMAIN" "$RUN_ID" "$NTOTAL" <<'PY'
import os, sys
dom, run, n = sys.argv[1], sys.argv[2], int(sys.argv[3])
print(",".join(str(i) for i in range(n) if not os.path.exists(f"runs/{run}/{dom}-{i}.json")))
PY
)
if [ -z "$MISSING" ]; then
  echo "[rerun] runs/$RUN_ID complete for $DOMAIN ($NTOTAL tasks) — nothing to rerun"; exit 0
fi
NMISS=$(echo "$MISSING" | tr ',' ' ' | wc -w | tr -d ' ')
(( SHARDS > NMISS )) && SHARDS=$NMISS
echo "[rerun] $DOMAIN runs/$RUN_ID: $NMISS/$NTOTAL missing across $SHARDS shard(s) -> $MISSING"

# Round-robin the missing indices into SHARDS chunks.
CHUNKS=$(python - "$MISSING" "$SHARDS" <<'PY'
import sys
miss = [x for x in sys.argv[1].split(",") if x]; s = int(sys.argv[2])
chunks = [[] for _ in range(s)]
for i, x in enumerate(miss):
    chunks[i % s].append(x)
print("\n".join(",".join(c) for c in chunks if c))
PY
)

pids=(); k=0
while IFS= read -r chunk; do
  [ -z "$chunk" ] && continue
  export EVAL_LOG_DIR="log/run-${RUN_ID}-rerun-s${k}"
  mkdir -p "$EVAL_LOG_DIR"
  BENCH_TASK_INDICES="$chunk" BENCH_DOMAIN="$DOMAIN" \
    python repl_bench.py > "log/rerun-${RUN_ID}-s${k}.log" 2>&1 &
  pids+=("$!")
  echo "[rerun] shard $k: indices=$chunk pid=$! -> log/rerun-${RUN_ID}-s${k}.log"
  k=$((k + 1)); sleep 0.5
done <<< "$CHUNKS"

echo "[rerun] launched $k shard(s); waiting..."
fail=0
for p in "${pids[@]}"; do wait "$p" || { fail=1; echo "[rerun] pid $p exited non-zero"; }; done
echo "[rerun] DONE (fail=$fail)."
python - "$DOMAIN" "$RUN_ID" "$NTOTAL" <<'PY'
import os, sys
dom, run, n = sys.argv[1], sys.argv[2], int(sys.argv[3])
m = [i for i in range(n) if not os.path.exists(f"runs/{run}/{dom}-{i}.json")]
print(f"[rerun] {n - len(m)}/{n} present; still missing: {m if m else 'none'}")
PY
