# Copy to env.sh and fill in. `source env.sh` before running the bench.
# env.sh is gitignored — never commit real keys.

# Activate the uv venv so a plain `python` resolves to .venv/bin/python.
_envdir="${BASH_SOURCE[0]:-$0}"
_envdir="$(cd -- "$(dirname -- "$_envdir")" >/dev/null 2>&1 && pwd)"
[ -f "$_envdir/.venv/bin/activate" ] && source "$_envdir/.venv/bin/activate"
unset _envdir

# --- agent LLM (the model the `agent` primitive generates code with) ----------
# Any OpenAI-compatible chat-completions endpoint. This is the system UNDER test.
export AGENT_BASE_URL="https://api.deepseek.com"
export AGENT_API_KEY="REPLACE_ME"
export AGENT_MODEL="deepseek-v4-flash"
# Reasoning/thinking for the agent. ON here; AGENT_EFFORT sets the
# reasoning_effort level (low|medium|high). Set AGENT_THINKING=off to disable.
export AGENT_THINKING="on"
export AGENT_EFFORT="high"
# The agent's cross-turn memory is the REPL session transcript, so history MUST be
# on (it's off by default in the shared Agent.scala). run_bench.sh forces this on;
# set it here too if you drive repl_bench.py directly.
export AGENT_REPL_HISTORY="on"

# --- tau2-bench user simulator (the LLM that role-plays the customer) ---------
# Configured like the agent: an explicit base_url + key + model trio (any
# OpenAI-compatible endpoint), with NO provider default. Keep this model FIXED
# across agent comparisons so the simulated "customer" stays constant.
export TAU2_USER_BASE_URL="REPLACE_ME"      # e.g. https://api.openai.com/v1 (include /v1 if your provider needs it)
export TAU2_USER_API_KEY="REPLACE_ME"
export TAU2_USER_MODEL="REPLACE_ME"         # e.g. gpt-4.1, deepseek-chat, ...
export TAU2_USER_TEMPERATURE="0.0"

# --- tau2-bench gym shim (tau2_server.py) -------------------------------------
export TAU2_SERVER_URL="http://127.0.0.1:8881"
export TAU2_PORT="8881"
export TAU2_MAX_STEPS="100"
# Waitress worker threads — keep comfortably above your shard count so concurrent
# /step calls (a respond blocks on the user-sim LLM) don't queue.
# export TAU2_SERVER_THREADS="64"
# export TAU2_SOLO_MODE="1"                 # solo mode (no user) for telecom etc.

# --- caps ---------------------------------------------------------------------
# orchestrator (repl_bench.py):
# export TAU_MAX_TURNS="40"                 # cap on customer turns per task
# export TAU_TURN_TIMEOUT="300"             # idle secs before a turn is deemed stuck
# export TAU_READY_TIMEOUT="400"            # secs to wait for first compile + :load
# backend tool-call caps (Tau.scala): one PER TURN (bounds a single turn's retry
# storm so it can't exhaust tau2's max_steps), one PER TASK (cumulative):
# export TAU_MAX_TURN_TOOL_CALLS="50"       # cap on backend tool calls per TURN
# export TAU_MAX_TOOL_CALLS="500"           # cap on backend tool calls per TASK
