# tau2-bench with the Lacuna `agent` primitive

This directory runs the **latest** [tau2-bench](https://github.com/sierra-research/tau2-bench)
(the τ²/τ³ line — successor to the original τ-bench) with the Scala 3
`agent[T](task)` primitive. It complements `../browsecomp-plus-bench/` (deep
research) with a multi-turn, stateful, tool-agent-user evaluation on the current benchmark.

Because tau2 is **conversational**, we run it as a real REPL session: the
conversation loop lives in a Python orchestrator **outside** the REPL
(`repl_bench.py`), which feeds the customer's message for each turn as ONE
top-level REPL command — `turn(i)`, a single `agent[String]` call — and reads back
that turn's reply. Question `Q1` → `agent` → reply `R1` → delivered to the
customer → next question `Q2` → `agent` in the **same** REPL → `R2`, and so on.

Each turn being its own REPL command is what makes the agent's memory work:
with `-Xrepl-history-file` on, the prints and rendered results of turns `1..i`
accumulate in the session transcript, which the `agent` primitive reads back on
turn `i+1` (`AGENT_REPL_HISTORY=on`). **The REPL session itself is the agent's
cross-turn memory** — there is no hand-threaded transcript string. The typed tool
facade (`callTool`) drives tau2's **Gymnasium `AgentGymEnv`** over plain HTTP.

## What tau2 adds

tau2-bench is a tool-agent-user benchmark driven through a standard Gymnasium
interface (`reset()` / `step(action_str)`): more domains than earlier
customer-service suites, **user tools** (dual-control — e.g. telecom — handled
*inside* tau2's user simulator, transparent to our agent), and 75+ task fixes
(SABER). Installed editable from upstream with `uv pip install -e ./upstream[gym]`.

## Domains you can run

| Domain | Runnable | Notes |
|---|:---:|---|
| `retail` | ✅ | order / return / exchange customer service |
| `airline` | ✅ | reservations; nested case-class tool params |
| `telecom` | ✅ | dual-control (user tools); single-control interface to us |
| `telecom-workflow` | ✅ | telecom troubleshooting workflow |
| `mock` | ✅ | tiny smoke domain |
| `banking_knowledge` | ❌ | RAG / knowledge-base domain. Its environment build pulls in the BM25 retrieval pipeline (`rank_bm25` + knowledge extras), which isn't installed — so `gen_tools.py` can't read its schemas and there is **no committed facade**. Enable with `uv pip install rank_bm25` then `./regen_facades.sh banking_knowledge`. |

Each runnable domain has a FIXED typed facade in `facades/<Domain>.scala`;
`run_bench.sh` picks it by domain and the orchestrator (`facadeInfo()`) asserts the
loaded facade's `facadeDomain` matches, so the agent's prompt always lists that
domain's tools.

## What's a task

One customer-service scenario. A hidden user instruction drives an LLM that
role-plays the customer; the agent only sees the customer's messages. The agent
authenticates the customer, looks up state with read-only tools, takes write
actions only after the policy-required confirmation, and finishes by calling the
`done` tool. tau2 then scores the run **programmatically** — action checks +
final DB state + required communication — so there is no LLM judge.

## Architecture

The conversation loop is OUTSIDE the REPL, in the Python orchestrator
`repl_bench.py`. It owns turn-taking: it gets each customer message from tau2,
feeds it to the REPL as one `turn(i)` command, reads back the reply, and delivers
it to tau2's user simulator to get the next message.

```
repl_bench.py  (orchestrator — owns the conversation loop)
  │
  │  per task: /reset ─► opening customer message Q0 + policy
  │  per turn i:
  │     ├─ write Qi ─► runs/<id>/_turns/<task>/i.q.txt
  │     ├─ feed ONE REPL command:  turn(i)        (stdin)
  │     │        └───────────────────────────────────────────────┐
  │     │   ┌───────────────────────────────────────────────────┐│
  │     │   │ scala-cli repl  (persistent; one per shard)        ││
  │     │   │   Agent.scala + Tau.scala + facades/<Domain>.scala ││
  │     │   │   :load Bench.scala  ─► eval-rewriter on agent[String]
  │     │   │                                                    ││
  │     │   │   turn(i):                                         ││
  │     │   │     reply = agent[String](tauStep(.., Qi))         ││
  │     │   │       └─ <typed facade fn>(args) ─► Tau.callTool ──┼┼──► /step toolcall
  │     │   │            (prints `[tool] ..` → REPL transcript)  ││    (HTTP/JSON)
  │     │   │     Tau.writeReply(i, reply) ─► i.r.json + sentinel││
  │     │   │   memory = -Xrepl-history-file transcript of       ││
  │     │   │            turns 1..i-1 (AGENT_REPL_HISTORY=on)    ││
  │     │   └───────────────────────────────────────────────────┘│
  │     ├─ read Ri ◄─ i.r.json   (+ whether agent called done())  │
  │     └─ /step respond Ri ─► next customer message Qi+1 + done  ◄┘
  │  end of task: /reward + /trace ─► reward + full trajectory
  ▼
┌─────────────────────────────────────────────────────────────────────┐
│  tau2_server.py           http://127.0.0.1:8881                     │
│  AgentGymEnv(domain, task_id)   ── tau2 orchestrator in a bg thread: │
│    /reset ─► opening user message + policy + tools_info             │
│    /step  ─► respond (to LLM user simulator) OR tool call (mock DB)  │
│    /reward ─► tau2 evaluator (actions + DB state + outputs)          │
│    /trace  ─► simulation_run: every user/assistant/tool message      │
└─────────────────────────────────────────────────────────────────────┘
  │
  ▼
runs/<run-id>/<domain>-<idx>.json   trace (from /trace), timing, tokens, reward
runs/<run-id>/trace/<domain>-<idx>/ progress log + generated-code rounds
  │
  ▼
evaluate.py   ─►   aggregate reward   ─►   Avg reward · Solved% · pass^k
```

The shim drives tau2's `AgentGymEnv`: an action string is either plain text (a
`respond` to the customer, issued by the orchestrator) or a JSON tool call
`{"name":..,"arguments":{..}}` (issued by the agent's generated code via
`Tau.callTool`). The Lacuna `agent` *is* the assistant. **Dual-control domains
(telecom) work through the same interface** — the user simulator uses its own
user-tools internally,
transparent to our agent.

**Typed tool facade.** The agent does NOT call tools with raw JSON. Each domain
has a FIXED, committed typed facade in `facades/<Domain>.scala` — one ordinary
in-scope Scala function per tool, with camelCase names and fully typed params
(e.g. `getOrderDetails(orderId: String): String`, `refuelData(customerId: String,
lineId: String, gbAmount: Double)`), each dispatching to the underlying tool via
`Tau.callTool` (the agent never sees that). Nested object parameters are emitted
as `case class`es — e.g. airline's `bookReservation(... flights: List[FlightInfo],
passengers: List[Passenger], paymentMethods: List[Payment] ...)` — so the facade
never exposes `ujson.Value`. The file also exports `domainToolsDoc` (the data
types + signature list shown in the agent prompt). These files are committed and
used as-is — NOT generated per run. Regenerate from the installed tau2 schemas
with `./regen_facades.sh` only after a tau2 upgrade.

## Layout

| Path | What |
|------|------|
| `repl_bench.py` | **the orchestrator**: owns the conversation loop, spawns + drives the persistent REPL (one `turn(i)` command per customer turn), talks to tau2 (`/reset` `/step` `/reward` `/trace`), writes the run files |
| `Bench.scala` | `:load`ed into the REPL; defines `turn(i)` (the eval-rewritten `agent[String]` call site) + `facadeInfo()` |
| `Tau.scala` | low-level `Tau.callTool` (prints `[tool] ..` into the transcript) + task-lifecycle helpers (`beginTask`/`endTask`/`question`/`writeReply`/`resetHistory`/`ping`) + the `tauStep` per-turn prompt |
| `facades/<Domain>.scala` | FIXED committed typed tool facade per domain (one in-scope fn per tool + `domainToolsDoc`) |
| `gen_tools.py` | generator that builds a facade from tau2 schemas (invoked by `regen_facades.sh`) |
| `regen_facades.sh` | regenerate the committed `facades/` after a tau2 upgrade |
| `tau2_server.py` | HTTP shim wrapping tau2's `AgentGymEnv` (reset/step/reward/**trace**/tasks) |
| `run_bench.sh` | one-shot runner (`DOMAIN`, `N` tasks from `START`); picks the facade + sets env, then runs `repl_bench.py` |
| `run_parallel.sh` | sharded runner across `SHARDS` parallel orchestrator+REPL processes |
| `evaluate.py` | aggregate per-task rewards → avg reward, solved%, per-domain, pass^k |
| `env.example.sh` | template for `env.sh` (agent + user-simulator keys/models) |
| `upstream/` | the cloned tau2-bench repo (gitignored; installed editable) |
| `runs/<run-id>/*.json` | per-task run files (trace, timing, token usage, reward); `<run-id>` is `BENCH_RUN_ID` (`<timestamp>_<domain>_<model>`), or `lacuna` for an ad-hoc session |
| `runs/<run-id>/trace/<id>/` | captured agent stdout + generated-code rounds |
| `runs/<run-id>/_turns/<id>/` | per-turn question/reply file channel + per-task policy/stats (scratch the orchestrator and REPL exchange through) |
| `evals/` | evaluation output (`evaluation_summary.json`, `detailed.csv`) |
| `log/run-*/` | per-shard REPL eval-log + `session.repl` history (one per `run_bench.sh` invocation) |

The agent primitive is **not duplicated** — `repl_bench.py` co-loads the shared
bench copy from `../browsecomp-plus-bench/Agent.scala`, so all three benchmarks
test the exact same `agent` code.

## Setup (one-time)

```sh
# Python env (tau2 requires Python >=3.12,<3.14)
uv venv --python 3.12 .venv
source .venv/bin/activate

# Clone the upstream benchmark and install it editable (with the gym interface)
git clone --depth 1 https://github.com/sierra-research/tau2-bench upstream
uv pip install -e "./upstream[gym]"
uv pip install flask waitress   # waitress = production WSGI server (tau2_server.py);
                                # the flask dev server wedges under many parallel shards

cp env.example.sh env.sh        # fill in the agent + user-simulator base_url/key/model
```

## Run

```sh
source env.sh

# 1. tau2 gym shim (loads tau2 ~6s; leave running)
python tau2_server.py &

# 2. Run the agent (DOMAIN N START). run_bench.sh sets the env and launches the
#    repl_bench.py orchestrator, which spawns + drives the persistent REPL itself.
#    Each run writes to runs/<run-id>/, where <run-id> defaults to
#    <timestamp>_<domain>_<model> (the scripts print it); a parallel run shares one
#    <run-id> across shards. Set BENCH_RUN_ID=my-label to override. Domains:
#    retail | airline | telecom | telecom-workflow | mock.
./run_bench.sh retail 10            # 10 retail tasks from offset 0
./run_bench.sh airline 5 0          # 5 airline tasks
./run_bench.sh telecom 5 0          # telecom (dual-control; same interface)
./run_parallel.sh retail 114 4      # full retail set across 4 parallel orchestrators

# 3. Aggregate rewards (no LLM judge needed) — point --input_dir at the run dir
#    the scripts printed (results kept separate per run).
python evaluate.py --input_dir runs/<run-id>     # e.g. runs/20260522-040000_retail_deepseek-v4-flash
```

## What's recorded per task

The orchestrator writes one JSON in `runs/<run-id>/<domain>-<idx>.json` and one
folder in `runs/<run-id>/trace/<domain>-<idx>/`. The conversation `result[]` and
`tool_call_counts` come from tau2's own `simulation_run` (pulled over `/trace`);
the agent token/retry stats come from a per-task dump the REPL writes (`endTask`).

`<domain>-<idx>.json`:

- `metadata.model` (agent), `metadata.user_model` (user simulator),
  `started_at`, `ended_at`, `duration_sec`, `eval_rounds`, `termination_reason`
- `metadata.agent_stats`: `agent_calls`, `attempts`, `retries`, `compiles_ok`,
  `compiles_failed`
- `metadata.llm_usage`: `calls`, `prompt_tokens`, `completion_tokens`,
  `total_tokens`
- `task_id`, `task_key`, `domain`, `task_index`, `first_user_message`, `status`
- `reward` — tau2's final evaluator reward (0.0–1.0)
- `tool_call_counts` — per-tool counts, including `respond` and `done`
- `result[]` — interleaved `user_message` / `agent_message` / `tool_call` /
  `tool_result` entries, ending with `{type:"output_text", …}`

`trace/<domain>-<idx>/`: `progress.log` (the REPL stdout captured during the task —
`[customer] …` / `[agent] …` / `[tool] …`) + every `eval_*_code.scala` /
`_wrapper.scala` / `_enclosingSource.scala` / `_error.scala` round.

## Notes

### The conversation loop is OUTSIDE the REPL — why
tau2 is multi-turn, so we run it as a real REPL session and let each customer turn
be its OWN top-level REPL command (`turn(i)`). That is the whole point: with
`-Xrepl-history-file` on, the prints + rendered results of turns `1..i` accumulate
in the session transcript, and `Agent.scala`'s `readRecentReplHistory` feeds that
back to the agent on turn `i+1` (`AGENT_REPL_HISTORY=on`, which `run_bench.sh`
sets). So the REPL session IS the agent's cross-turn memory — `Tau.callTool` prints
each `[tool] … -> result` line and `turn` prints `[customer] …`, so what the agent
looked up / changed and what the customer said all persist into the transcript. If
the loop instead ran inside one big `:load`ed command, the whole conversation would
be a single un-flushed history entry and this would not work.

### Why `:load`, not a `repl` file arg
The `agent[String](…)` call site must be compiled through the REPL's eval-rewrite
phase. `Bench.scala` (defining `turn`) is `:load`ed; `Agent.scala` (shared) +
`Tau.scala` (tools/helpers) + the facade are normal file arguments.

### Per-task transcript reset
The REPL is persistent across a shard's tasks, so each task must start with an
empty transcript. `turn(0)` calls `Tau.resetHistory()` (truncates the history
file) — done INSIDE the REPL so it's race-free with the REPL's deferred per-command
history flush.

### Driving `scala-cli repl` over a pipe
Three measures make incremental stdin driving reliable:
1. **Prompt detection.** The reader is char-based and surfaces the REPL's
   `scala> ` prompt — the unambiguous "idle, ready for input" signal. A command is
   sent only once the REPL is at a prompt, so nothing is fired during JLine init
   (which silently drops pre-buffered lines) or mid-`:load`.
2. **Color off** (`-color:never` + `NO_COLOR`). Plain output makes sentinel /
   error / prompt matching exact, and keeps the captured `progress.log` clean.
3. **Resend on corruption.** JLine (dumb terminal over a pipe) sometimes eats the
   first character of a command — erratically, not just the 2nd one — turning it
   into an unresolved identifier (`Tau.beginTask` → `au.beginTask`) that never
   executes. The orchestrator detects the resulting REPL error (its trailing
   prompt) and re-sends — safe precisely because a corrupted command is a no-op. A
   warm-up command after `:load` soaks up the common 2nd-command case. A `turn(i)`
   that has already started (printed `[customer]`/`[tool]`) is never re-sent (no
   double tool calls); if it then stalls past `TAU_TURN_TIMEOUT`, the orchestrator
   restarts the REPL and moves on.

### The user simulator is an LLM — configure it like the agent, and pin it
The orchestrator's `respond` steps are answered by an LLM playing the customer,
configured with an explicit `TAU2_USER_BASE_URL` + `TAU2_USER_API_KEY` +
`TAU2_USER_MODEL` trio — the same shape as the agent, any OpenAI-compatible
endpoint, **no provider default** (the server exits if they're unset). tau2 drives
it through litellm's OpenAI-compatible provider. Keep the model **fixed** across
agent comparisons so the simulated customer is a constant; it's recorded as
`metadata.user_model` in every run file.

### Reward is programmatic and computed at the end
tau2 only evaluates once its orchestrator finishes (the agent calls `done`, the
user ends the chat, or `max_steps` is hit). The orchestrator calls `/reward` (and
`/trace`) at task end, which force-finish a stalled conversation so the evaluator
can run. The agent ends a resolved task by calling `done()`; the orchestrator
delivers that turn's reply first, then issues the real tau2 `done`.

### Finishing & escalation
The tools list includes two control tools added by tau2: `done` (end the task) and
`transfer_to_human_agents` (escalate / out-of-scope). The `tauStep` prompt
instructs the agent to use them.

### Sharding
`run_bench.sh DOMAIN N START` runs `repl_bench.py` over the task slice
`[START, START+N)`; `run_parallel.sh DOMAIN N SHARDS` launches several orchestrator
processes over disjoint slices. Each shard gets its own `EVAL_LOG_DIR` (+ its own
`session.repl`) and persistent REPL; the shim holds one isolated `AgentGymEnv` (own
orchestrator thread) per session id, so runs never collide.

## Status

Redesigned to the externally-driven model above. Validated so far without spending
LLM credits: all Scala (`Agent.scala` + `Tau.scala` + facade + `Bench.scala`)
type-checks under the pinned `3.9.0-RC1-bin-SNAPSHOT` compiler, and a plumbing
probe drove the real loaded REPL end to end (handshake + warm-up + `facadeInfo` +
`beginTask` + the question/reply file channel + `done()` flag + `resetHistory` +
`endTask` stats) with no tau2 server and no LLM calls. A full live measurement
against the agent + user-simulator LLMs has NOT been re-run yet under the new
harness.
