#!/usr/bin/env python3
"""
HTTP shim that drives the latest tau2-bench (sierra-research/tau2-bench, the τ²/τ³
line) for the Scala `agent` primitive, via its Gymnasium `AgentGymEnv`.

tau2-bench is the successor to the original tau-bench: a tool-agent-user
benchmark where an agent plays a customer-service rep, talks to an LLM-simulated
customer, and calls backend domain APIs that read/write a mock database. Domains:
mock, airline, retail, telecom, telecom-workflow, banking_knowledge. Reward is
computed PROGRAMMATICALLY by tau2's evaluator (action checks + DB-state + required
communication), so there is no LLM judge.

We drive tau2's `AgentGymEnv` (the gym interface): `reset()` starts an
orchestrator in a background thread (agent <-> user-simulator <-> environment)
and returns the opening user message; `step(action_str)` advances it. The action
string is either plain text (a message to the customer) or a JSON tool call
`{"name": ..., "arguments": {...}}` (executed against the environment). The
Lacuna `agent` IS the assistant — we expose reset/step/reward/trace over JSON
HTTP. The Python orchestrator (repl_bench.py) drives the conversation (reset +
respond + reward/trace); the Scala REPL only issues tool-call steps.

Dual-control domains (e.g. telecom) work through the SAME interface: the user
simulator uses its own user-tools internally, transparent to our agent.

Endpoints (POST JSON in/out, except /health):
  POST /tasks   {domain}                    -> {domain, num_tasks, task_ids}
  POST /reset   {domain, task_index|task_id}-> {session_id, observation, policy,
                                                tools_info, num_tasks, task_id}
  POST /step    {session_id, name, kwargs}  -> {observation, obs_role, reward, done}
                  name=="respond" -> message (kwargs.content); else a tool call.
                  obs_role is the role of the observation ('user'/'tool'/'assistant'):
                  after a respond it should be 'user' (the customer's reply).
  POST /reward  {session_id}                -> {reward}   (forces finish if needed)
  POST /trace   {session_id}                -> {messages, termination_reason,
                                                duration, reward_info}
                  Full simulation trajectory (forces finish if needed) — the
                  orchestrator builds each task's run-file `result[]` and
                  tool-call counts from this, so the Scala side records no trace.
  POST /close   {session_id}                -> {ok}
  GET  /health                              -> {status, sessions}

Config via environment variables:
  TAU2_PORT             HTTP port                          (default 8881)
  TAU2_USER_MODEL       user-sim litellm model (REQUIRED)  (e.g. openai/deepseek-v4-flash)
  TAU2_USER_BASE_URL    user-sim endpoint base url (optional; set for a custom
                        OpenAI-compatible endpoint, e.g. https://api.deepseek.com)
  TAU2_USER_API_KEY     user-sim API key (REQUIRED)
  TAU2_USER_TEMPERATURE user-sim temperature               (default 0.0)
  TAU2_MAX_STEPS        orchestrator step cap per task     (default 100)
  TAU2_SOLO_MODE        1/true -> solo (no user) mode      (default off)

The user simulator is configured like the agent — explicit base_url + key +
model, with NO provider default — and driven through litellm's OpenAI-compatible
provider (any OpenAI-compatible chat endpoint works). Keep the model FIXED across
agent comparisons so the simulated customer stays constant.
"""

import json
import os
import sys
import time
import uuid

from flask import Flask, jsonify, request
from loguru import logger

# tau2's orchestrator logs at DEBUG/INFO very verbosely; quiet it.
logger.remove()
logger.add(sys.stderr, level=os.getenv("TAU2_LOG_LEVEL", "WARNING"))

from tau2.gym.gym_agent import AgentGymEnv  # noqa: E402
from tau2.registry import registry  # noqa: E402

TAU2_PORT = int(os.getenv("TAU2_PORT", "8881"))

# User simulator + tau2's internal LLM calls. TAU2_USER_MODEL is a full litellm
# model string (e.g. "openrouter/openai/gpt-5.4", "openai/gpt-4.1") — the
# provider it names decides routing, so OpenRouter etc. work without a fragile
# "openai/"-prefix-stripping path. No provider default is assumed. Keep the model
# FIXED across agent comparisons so the simulated "customer" is a constant.
USER_MODEL = os.getenv("TAU2_USER_MODEL")
USER_API_KEY = os.getenv("TAU2_USER_API_KEY")
USER_BASE_URL = os.getenv("TAU2_USER_BASE_URL")  # optional override
USER_TEMPERATURE = float(os.getenv("TAU2_USER_TEMPERATURE", "0.0"))
if not (USER_MODEL and USER_API_KEY):
    sys.exit(
        "tau2_server: set TAU2_USER_MODEL (a litellm model string) and "
        "TAU2_USER_API_KEY for the user-simulator LLM (see env.example.sh)."
    )
# Expose the key to whichever provider the model string selects, so no litellm
# code path (incl. concurrent client-cache misses) lacks credentials -> 500.
os.environ["OPENROUTER_API_KEY"] = USER_API_KEY
os.environ["OPENAI_API_KEY"] = USER_API_KEY
if USER_BASE_URL:
    os.environ["OPENAI_BASE_URL"] = USER_BASE_URL
USER_LLM = USER_MODEL
USER_LLM_ARGS = {"temperature": USER_TEMPERATURE, "api_key": USER_API_KEY}
if USER_BASE_URL:
    USER_LLM_ARGS["api_base"] = USER_BASE_URL

# tau2 makes several INTERNAL litellm calls whose model defaults to a hardcoded
# external model (config.py: the NL-assertion evaluator -> on /reward, the
# env-interface agent -> on /step, both gpt-4.1). Those defaults are copied at
# import time — and one is bound as a default parameter — so patching tau2.config
# after import is too late. Instead patch the single chokepoint every tau2 LLM
# call routes through (`tau2.utils.llm_utils.completion`, resolved at call time)
# and reroute any NON-user model to USER_LLM. The user sim (model == USER_LLM)
# passes through untouched.
import tau2.utils.llm_utils as _llm_utils  # noqa: E402
_orig_completion = _llm_utils.completion
def _rerouted_completion(*args, **kwargs):
    m = kwargs.get("model", "")
    if isinstance(m, str) and m != USER_LLM:
        kwargs["model"] = USER_LLM
        kwargs.setdefault("api_key", USER_API_KEY)
        if USER_BASE_URL:
            kwargs.setdefault("api_base", USER_BASE_URL)
    return _orig_completion(*args, **kwargs)
_llm_utils.completion = _rerouted_completion

MAX_STEPS = int(os.getenv("TAU2_MAX_STEPS", "200"))  # match tau2 official DEFAULT_MAX_STEPS
SOLO_MODE = os.getenv("TAU2_SOLO_MODE", "0").lower() in ("1", "true", "on", "yes")

sessions = {}  # session_id -> AgentGymEnv


def log(*a):
    print(f"[tau2 {time.strftime('%H:%M:%S')}]", *a, flush=True)


def task_ids(domain):
    return [t.id for t in registry.get_tasks_loader(domain)()]


def split_role(obs):
    """Split tau2's 'role: ...' observation tag into (role, content). tau2 prepends
    the role of the observation's last message ('user'/'tool'/'assistant'); we
    return the role so the orchestrator can tell a customer reply ('user') from a
    trailing tool/assistant message. (role is None when there's no known prefix.)"""
    if not isinstance(obs, str):
        return None, obs
    for role in ("tool", "user", "assistant"):
        p = f"{role}: "
        if obs.startswith(p):
            return role, obs[len(p):]
    return None, obs


app = Flask(__name__)


@app.get("/health")
def health():
    return jsonify(status="ok", sessions=len(sessions))


@app.post("/tasks")
def tasks():
    domain = request.get_json(force=True)["domain"]
    ids = task_ids(domain)
    return jsonify(domain=domain, num_tasks=len(ids), task_ids=ids)


@app.post("/reset")
def reset():
    body = request.get_json(force=True)
    domain = body["domain"]
    ids = task_ids(domain)
    if "task_id" in body and body["task_id"] is not None:
        tid = str(body["task_id"])
    else:
        tid = ids[int(body["task_index"])]
    env = AgentGymEnv(
        domain=domain,
        task_id=tid,
        max_steps=MAX_STEPS,
        solo_mode=SOLO_MODE,
        user_llm=USER_LLM,
        user_llm_args=USER_LLM_ARGS,
    )
    observation, info = env.reset()
    sid = uuid.uuid4().hex
    sessions[sid] = env
    log(f"reset {domain} task={tid} -> session {sid[:8]}")
    return jsonify(
        session_id=sid,
        task_id=tid,
        observation=split_role(observation)[1],
        policy=info["policy"],
        tools_info=[t.openai_schema for t in info["tools"]],
        num_tasks=len(ids),
    )


@app.post("/step")
def step():
    body = request.get_json(force=True)
    env = sessions.get(body["session_id"])
    if env is None:
        return jsonify(error="unknown session"), 404
    name = body["name"]
    kwargs = body.get("kwargs", {})
    if name == "respond":
        action = kwargs.get("content", "")
    else:
        action = json.dumps({"name": name, "arguments": kwargs})
    observation, reward, terminated, truncated, _info = env.step(action)
    obs_role, obs_text = split_role(observation)
    return jsonify(observation=obs_text, obs_role=obs_role,
                   reward=reward, done=terminated)


def _force_finish(env):
    """tau2 only populates simulation_run / evaluates once the orchestrator has
    finished. If the agent stopped without ending the conversation, inject a
    `done` action to force a clean finish so the evaluator and trajectory are
    available."""
    if not env._simulation_done.is_set():
        try:
            env.step(json.dumps({"name": "done", "arguments": {}}))
        except Exception as e:
            log(f"force-finish failed: {e}")


@app.post("/reward")
def reward():
    body = request.get_json(force=True)
    env = sessions.get(body["session_id"])
    if env is None:
        return jsonify(error="unknown session"), 404
    _force_finish(env)
    r, info = env._get_reward()
    # info is a JSON string of tau2's RewardInfo (db_check / action_checks /
    # nl_assertions / communicate_checks / reward_breakdown) — the per-task
    # failure attribution. Parse it so it lands structured in the run file.
    try:
        info = json.loads(info) if isinstance(info, str) else info
    except Exception:
        info = None
    return jsonify(reward=r, reward_info=info)


@app.post("/trace")
def trace():
    """Return the full simulation trajectory for the run file. tau2 records every
    user / assistant / tool message in `simulation_run.messages`; the orchestrator
    turns that into the run file's `result[]` and tool-call counts, so the Scala
    side keeps no trace of its own."""
    body = request.get_json(force=True)
    env = sessions.get(body["session_id"])
    if env is None:
        return jsonify(error="unknown session"), 404
    _force_finish(env)
    run = env._simulation_run
    if run is None:
        return jsonify(messages=[], termination_reason=None, duration=None)
    data = json.loads(run.model_dump_json())
    return jsonify(
        messages=data.get("messages") or [],
        termination_reason=data.get("termination_reason"),
        duration=data.get("duration"),
    )


@app.post("/close")
def close():
    sessions.pop(request.get_json(force=True).get("session_id"), None)
    return jsonify(ok=True)


def main():
    # Serve with waitress (a production WSGI server), NOT flask's dev server: under
    # many parallel shards the dev server can silently WEDGE (a respond /step blocks
    # on the user-sim LLM, holding a worker the whole time). threads must comfortably
    # exceed the shard count so concurrent /step calls don't queue. Falls back to the
    # dev server if waitress isn't installed (then keep parallelism low, < ~8).
    threads = int(os.getenv("TAU2_SERVER_THREADS", "64"))
    log(
        f"serving on http://127.0.0.1:{TAU2_PORT} "
        f"(user_llm={USER_LLM} @ {USER_BASE_URL}, max_steps={MAX_STEPS}, "
        f"solo={SOLO_MODE}, threads={threads})"
    )
    try:
        from waitress import serve
        serve(app, host="127.0.0.1", port=TAU2_PORT, threads=threads)
    except ImportError:
        log("WARNING: waitress not installed; using flask dev server "
            "(keep parallel shards below ~8 to avoid wedging)")
        app.run(host="127.0.0.1", port=TAU2_PORT, threaded=True)


if __name__ == "__main__":
    main()
