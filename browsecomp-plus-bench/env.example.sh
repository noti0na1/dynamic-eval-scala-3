# Copy to env.sh and fill in. `source env.sh` before running the bench.
# env.sh is gitignored — never commit real keys.

# Activate the uv venv so a plain `python` resolves to .venv/bin/python,
# regardless of the current working directory. Re-sourcing is idempotent.
_envdir="${BASH_SOURCE[0]:-$0}"
_envdir="$(cd -- "$(dirname -- "$_envdir")" >/dev/null 2>&1 && pwd)"
[ -f "$_envdir/.venv/bin/activate" ] && source "$_envdir/.venv/bin/activate"
unset _envdir

# --- query embedding (OpenAI-compatible provider serving Qwen3-Embedding-8B) ---
# The corpus index was built with Qwen/Qwen3-Embedding-8B; the provider must
# serve the SAME checkpoint or recall collapses (verify with check_recall.py).
export EMBED_BASE_URL="https://openrouter.ai/api/v1"
export EMBED_API_KEY="REPLACE_ME"
export EMBED_MODEL="qwen/qwen3-embedding-8b"

# --- agent LLM (the model the `agent` primitive generates code with) ----------
# Any OpenAI-compatible chat-completions endpoint.
export AGENT_BASE_URL="https://api.deepseek.com"
export AGENT_API_KEY="REPLACE_ME"
export AGENT_MODEL="deepseek-v4-flash"
# Reasoning is a client-level default applied to every agent chat call:
#   AGENT_THINKING=on|1|true  enable reasoning (default off)
#   AGENT_EFFORT=low|medium|high  reasoning_effort when thinking is on (default medium)
# export AGENT_THINKING="on"
# export AGENT_EFFORT="high"
# Optional: cap recursive agent[...] nesting (default 32; see AgentDepth).
# export AGENT_MAX_DEPTH="32"
# Per-query wall-clock budget in seconds (default 600). run_bench.sh runs each
# query in its own REPL and kills it if it overruns, so a runaway agent can't
# hang the shard. Raise it if legitimate long queries get cut.
# export QUERY_TIMEOUT_SEC="600"

# --- answer judging -----------------------------------------------------------
# evaluate.py grades via chat-completions and by default reuses EMBED_API_KEY
# on OpenRouter. Set OPENAI_API_KEY only to judge via OpenAI directly.
export OPENAI_API_KEY="REPLACE_ME"
