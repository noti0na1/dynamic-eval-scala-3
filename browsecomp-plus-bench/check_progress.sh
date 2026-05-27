#!/usr/bin/env bash
# Read-only progress probe for the running BrowseComp-Plus benchmark.
# Self-contained (cd's to its own dir) so it can be invoked as a single
# allowlisted command with no arguments. Touches nothing — only ls/pgrep/ps/cat.
set -euo pipefail
cd "$(dirname "$0")"

RUN_ID="$(cat data/.last_full_run_id 2>/dev/null || true)"
if [ -z "$RUN_ID" ]; then echo "no run id recorded (data/.last_full_run_id missing)"; exit 0; fi

done=$( (ls "runs/$RUN_ID"/*.json 2>/dev/null | wc -l | tr -d ' ') || true )
shards=$( (pgrep -fl 'run_bench.sh' 2>/dev/null | grep -c run_bench.sh) || true )
jvms=$( (ps aux | grep -iE 'scala-cli|bloop|[j]ava' | grep -v grep | wc -l | tr -d ' ') || true )
if pgrep -f 'run_parallel.sh' >/dev/null 2>&1; then launcher="RUNNING"; else launcher="EXITED"; fi

echo "run_id : $RUN_ID"
echo "done   : ${done:-0} / 830"
echo "launcher: $launcher    shard procs: ${shards:-0}    JVMs: ${jvms:-0}"

# Which shard slices (START offset) still have a run_bench.sh alive, and which
# query each live REPL is currently on (from its per-query -Xrepl-eval-log-dir).
live_starts=$( (ps -axo args 2>/dev/null | grep 'run_bench.sh' | grep -v grep \
  | grep -oE 'run_bench.sh +[0-9]+ +[0-9]+' | awk '{print $3}' | sort -nu | tr '\n' ' ') || true )
inflight=$( (ps -axo args 2>/dev/null | grep -oE 's[0-9]+/q[0-9]+' | sort -u | tr '\n' ' ') || true )
[ -n "${live_starts// }" ] && echo "live shard starts: ${live_starts}"
[ -n "${inflight// }" ]    && echo "in-flight queries: ${inflight}"

# Queries the per-query watchdog killed at the timeout cap — these write no run
# file, so they account for the gap between `done` and 830 once shards exit.
killed=$( (grep -rh 'killing its REPL' data/shard-s*.log 2>/dev/null | wc -l | tr -d ' ') || true )
echo "watchdog-killed : ${killed:-0}    (no result file -> expected gap below 830)"

# Terminal condition: launcher gone AND no shard/JVM activity left.
if [ "$launcher" = "EXITED" ] && [ "${shards:-0}" -eq 0 ] && [ "${jvms:-0}" -eq 0 ]; then
  echo "STATE  : FINISHED"
else
  echo "STATE  : RUNNING"
fi
