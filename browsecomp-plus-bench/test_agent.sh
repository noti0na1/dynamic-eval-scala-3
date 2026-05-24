#!/usr/bin/env bash
# End-to-end smoke test for the Lacuna `agent` primitive (no retriever needed).
#
# Usage:
#   source env.sh          # AGENT_API_KEY / AGENT_BASE_URL / AGENT_MODEL
#   ./test_agent.sh        # or just `./test_agent.sh` if env.sh is auto-sourced
#
# It boots a REPL with Agent.scala on the classpath, `:load`s test_task.scala
# (which runs one agent[String] task engineered to require recursive sub-calls),
# streams the full run, then dumps every generated code snippet from an isolated
# eval-log dir and reports PASS/FAIL.
set -uo pipefail
cd "$(dirname "$0")"

# Pick up keys if the caller hasn't sourced env.sh already.
if [ -z "${AGENT_API_KEY:-}" ] && [ -f env.sh ]; then
  echo "[test] sourcing env.sh"
  # shellcheck disable=SC1091
  source ./env.sh
fi
if [ -z "${AGENT_API_KEY:-}" ]; then
  echo "[test] ERROR: AGENT_API_KEY is not set — \`source env.sh\` first." >&2
  exit 2
fi

# Isolated eval-log + history dir for this run (auto-created; left for inspection).
EVAL_LOG_DIR="$(mktemp -d "${TMPDIR:-/tmp}/agent-test.XXXXXX")"
export EVAL_LOG_DIR
REPL_LOG="${EVAL_LOG_DIR}/repl_output.log"
echo "[test] AGENT_MODEL=${AGENT_MODEL:-deepseek-v4-flash}  EVAL_LOG_DIR=$EVAL_LOG_DIR"
echo

# `:load` compiles test_task.scala through the REPL so the agent[String] call
# site is rewritten; stdin EOF after the load makes the REPL exit. Per-run
# log/history paths are passed here (not fixed in Agent.scala's directives).
printf ':load test_task.scala\n' \
  | scala-cli repl --server=false \
      -O -Xrepl-eval-log-dir:"${EVAL_LOG_DIR}/" \
      -O -Xrepl-history-file:"${EVAL_LOG_DIR}/session.repl" \
      Agent.scala 2>&1 | tee "$REPL_LOG"

# --- dump the generated code so recursion is visible ------------------------
shopt -s nullglob
codes=( "${EVAL_LOG_DIR}"/eval_*_code.scala )
errors=( "${EVAL_LOG_DIR}"/eval_*_error.scala )
echo
echo "[test] ===== generated snippets: ${#codes[@]}   compile-error logs: ${#errors[@]} ====="
i=0
# Length-guard the expansions: macOS bash 3.2 errors on "${arr[@]}" for an
# empty array under `set -u`.
if (( ${#codes[@]} )); then
  for f in "${codes[@]}"; do
    i=$((i + 1))
    echo "----- snippet #$i: $(basename "$f") -----"
    cat "$f"
    echo
  done
fi
if (( ${#errors[@]} )); then
  for f in "${errors[@]}"; do
    echo "----- compile-error log: $(basename "$f") -----"
    cat "$f"
    echo
  done
fi

# --- verdict (test_task.scala prints TEST_RESULT=PASS|FAIL) -----------------
echo
if grep -q '^TEST_RESULT=PASS' "$REPL_LOG"; then
  echo "[test] RESULT: PASS  (logs under $EVAL_LOG_DIR)"
  exit 0
else
  echo "[test] RESULT: FAIL  (logs under $EVAL_LOG_DIR)"
  exit 1
fi
