#!/usr/bin/env bash
# Run the tau2-bench benchmark with the Lacuna `agent` primitive.
#
# REDESIGN: the conversation loop lives in the Python orchestrator repl_bench.py,
# which spawns a persistent `scala-cli repl`, `:load`s Bench.scala, and feeds ONE
# `turn(i)` command per customer turn (each its own REPL command, so the
# -Xrepl-history-file transcript becomes the agent's cross-turn memory). This
# script just sets the per-shard env and hands off to repl_bench.py.
#
# Prerequisites:
#   1. source env.sh                       (agent + user-simulator keys/models)
#   2. python tau2_server.py &             (tau2-bench gym shim, another shell)
#
# Usage: ./run_bench.sh [DOMAIN] [N] [START]
#   DOMAIN  mock | airline | retail | telecom | telecom-workflow  (default retail)
#   N       number of tasks to run             (default 10)
#   START   0-based offset into the task list  (default 0; for sharding)
set -euo pipefail
cd "$(dirname "$0")"
DOMAIN="${1:-retail}"
N="${2:-10}"
START="${3:-0}"

# Logical run id -> the runs/<id>/ output dir. Format <timestamp>_<domain>_<model>
# so separate runs land in separate dirs. run_parallel.sh sets/exports this once
# so all shards share runs/<id>/ (disjoint task ids); a standalone run computes it.
_model_slug="$(printf '%s' "${AGENT_MODEL:-deepseek-v4-flash}" | tr '/ :' '---')"
export BENCH_RUN_ID="${BENCH_RUN_ID:-$(date +%Y%m%d-%H%M%S)_${DOMAIN}_${_model_slug}}"

# Per-SHARD eval-log + history dir, so concurrent / sharded runs never collide.
# The orchestrator points -Xrepl-eval-log-dir / -Xrepl-history-file here; turn(0)
# truncates the history file per task (the persistent REPL is reused across tasks,
# so each task must start with an empty transcript).
export EVAL_LOG_DIR="log/run-${BENCH_RUN_ID}-s${START}"
mkdir -p "$EVAL_LOG_DIR"

# The agent recalls earlier turns of the SAME conversation purely from the REPL
# session transcript, so history MUST be on (it's off by default in the shared
# Agent.scala). The orchestrator's per-task truncation keeps tasks isolated.
export AGENT_REPL_HISTORY="${AGENT_REPL_HISTORY:-on}"

# Pick the FIXED, committed typed tool facade for this domain (facades/<Camel>.scala).
# One in-scope Scala fn per tool + `domainToolsDoc`; co-loaded by the orchestrator.
CAMEL=$(echo "$DOMAIN" | awk -F'[-_]' '{for(i=1;i<=NF;i++) printf "%s", toupper(substr($i,1,1)) substr($i,2)}')
export TAU_FACADE="facades/${CAMEL}.scala"
[ -f "$TAU_FACADE" ] || { echo "[run_bench] ERROR: no facade $TAU_FACADE — run ./regen_facades.sh"; exit 1; }
# Shared agent primitive (same copy all three benchmarks test).
export AGENT_FILE="${AGENT_FILE:-../browsecomp-plus-bench/Agent.scala}"

echo "[run_bench] DOMAIN=$DOMAIN N=$N START=$START BENCH_RUN_ID=$BENCH_RUN_ID"
echo "[run_bench] facade=$TAU_FACADE EVAL_LOG_DIR=$EVAL_LOG_DIR AGENT_REPL_HISTORY=$AGENT_REPL_HISTORY"

BENCH_DOMAIN="$DOMAIN" BENCH_N="$N" BENCH_START="$START" python repl_bench.py
