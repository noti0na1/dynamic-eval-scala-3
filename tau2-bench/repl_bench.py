#!/usr/bin/env python3
"""
tau2-bench orchestrator for the Lacuna `agent` primitive — externally-driven
conversation over a persistent Scala REPL.

REDESIGN. tau2 is a multi-turn, stateful tool-agent-user benchmark, so we run the
conversation as a real REPL session: the loop lives HERE (outside the REPL), and
each customer turn is fed as ONE top-level REPL command `turn(i)`. Because every
turn is its own command and the REPL runs with `-Xrepl-history-file`, the prints
and rendered results of turns 1..i accumulate in the session transcript, which the
`agent` primitive reads back on turn i+1 (AGENT_REPL_HISTORY=on). The REPL session
itself is the agent's cross-turn memory — there is no hand-threaded transcript.

Per task:
  1. POST /reset  -> tau2 session, opening customer message, policy.
  2. Tau.beginTask(...) in the REPL (bind session, reset agent stats). turn(0)
     then truncates the history file (Tau.resetHistory) so each task starts with an
     empty transcript — done inside the REPL, race-free with its deferred flush.
  3. For each customer turn i:
       - write the message to <turndir>/<i>.q.txt
       - feed `turn(i)` to the REPL; wait for the `__TAU_TURN_DONE__i` sentinel
       - read the reply from <turndir>/<i>.r.json
       - POST /step respond  -> next customer message + done flag
       - if the agent called done(): deliver the reply, issue /step done, stop.
  4. POST /reward  -> tau2's programmatic reward (forces a clean finish).
  5. POST /trace   -> tau2's own simulation_run (every user/assistant/tool
                      message); we build the run file's result[] + tool counts.
  6. Tau.endTask(statsFile) -> agent token/retry stats (JVM-only counters).
  7. write runs/<run-id>/<domain>-<idx>.json  (+ trace/<...>/ generated code).

Driving a `scala-cli repl` over a pipe needs care (see the Repl class): we read
char-by-char to detect the `scala> ` prompt and only send a command when the REPL
is idle, run with `-color:never` + NO_COLOR so sentinel/prompt matching is exact,
and RE-SEND any command that comes back as a REPL error before producing output
(JLine intermittently eats a command's first char -> an unresolved identifier that
never executed, so re-sending is safe). A warm-up command after `:load` absorbs
the common case.

Config (env; see env.example.sh / run_bench.sh):
  BENCH_DOMAIN BENCH_N BENCH_START   slice of tasks to run
  BENCH_TASK_INDICES                 explicit comma-sep indices (overrides the
                                     slice; used by rerun_failed.sh to backfill)
  BENCH_RUN_ID                       runs/<id>/ output dir
  BENCH_NUM_TRIALS                   run each task N times (default 1); files are
                                     <domain>-<i>.t<n>.json. evaluate.py -k N -> pass^N
  EVAL_LOG_DIR                       per-shard eval-log + history dir
  TAU_FACADE                         facades/<Domain>.scala (co-loaded)
  AGENT_FILE                         shared Agent.scala (default ../browsecomp-...)
  TAU2_SERVER_URL                    tau2_server.py shim (default 127.0.0.1:8881)
  AGENT_MODEL TAU2_USER_MODEL        recorded into run-file metadata
  TAU_MAX_TURNS                      cap on customer turns per task (default 40)
  TAU_TURN_TIMEOUT                   idle seconds before a turn is deemed stuck
  TAU_TURN_HARD_TIMEOUT              hard wall-clock cap per turn (default 900s) —
                                     bounds a chatty runaway the idle timer can't catch
  TAU_READY_TIMEOUT                  seconds to wait for first compile + :load
"""

import json
import os
import queue
import shutil
import subprocess
import sys
import threading
import time
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

STOP_TOKEN = "###STOP###"
# Lines a running turn prints — seeing any one proves `turn(i)` started executing
# (so a later error is NOT command corruption).
TURN_OUTPUT_MARKERS = ("[customer]", "[agent]", "[tool]")
# Internal queue sentinel the reader emits when it sees the REPL's `scala> ` prompt
# (REPL idle, ready for input). Not real REPL output; never printed/captured.
PROMPT = "\x00PROMPT\x00\n"


def log(*a):
    print(f"[orch {time.strftime('%H:%M:%S')}]", *a, flush=True)


# --------------------------------------------------------------------------- HTTP

class Tau2Client:
    def __init__(self, base_url):
        self.base = base_url.rstrip("/")

    def _post(self, path, body, timeout=300):
        data = json.dumps(body).encode()
        req = urllib.request.Request(
            self.base + path, data=data,
            headers={"Content-Type": "application/json"}, method="POST")
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return json.loads(r.read().decode())

    def reset(self, domain, task_index):
        return self._post("/reset", {"domain": domain, "task_index": task_index})

    def respond(self, session_id, content):
        return self._post("/step", {"session_id": session_id, "name": "respond",
                                     "kwargs": {"content": content}})

    def done(self, session_id):
        return self._post("/step", {"session_id": session_id, "name": "done",
                                    "kwargs": {}})

    def reward(self, session_id):
        # Full response: {"reward": float, "reward_info": {...}} (tau2 RewardInfo).
        return self._post("/reward", {"session_id": session_id})

    def trace(self, session_id):
        return self._post("/trace", {"session_id": session_id})

    def close(self, session_id):
        try:
            self._post("/close", {"session_id": session_id}, timeout=30)
        except Exception:
            pass


# ---------------------------------------------------------------------- REPL pipe

class ReplDied(Exception):
    pass


class ReplError(Exception):
    pass


class Repl:
    """A persistent `scala-cli repl` we drive one command at a time over stdin,
    reading stdout up to a per-command sentinel.

    Every command we send MUST be a single self-contained call on one line
    (`turn(0)`, `Tau.beginTask(..)`, ...). Do NOT send multi-statement
    `{ a; b; c }` blocks: the dotty REPL's JLine reader mangles a brace-block sent
    over a pipe (it drops a leading chunk → syntax error). All side-effecting work
    lives behind a single REPL-rewritten def (`turn`) or a `Tau.*` helper instead."""

    def __init__(self, scala_cmd, cwd):
        self.scala_cmd = scala_cmd
        self.cwd = cwd
        self.p = None
        self.q = queue.Queue()
        self._sink = None   # per-task line capture (for progress.log)
        self._spawn()

    def _spawn(self):
        log("spawning REPL:", " ".join(self.scala_cmd))
        # NO_COLOR + `-color:never` (in scala_cmd) keep the REPL output plain, so
        # our sentinel / prompt matching and the captured progress.log are clean.
        env = {**os.environ, "NO_COLOR": "1"}
        self.p = subprocess.Popen(
            self.scala_cmd, cwd=self.cwd, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT, text=True, bufsize=1, env=env)
        self.q = queue.Queue()
        t = threading.Thread(target=self._reader, daemon=True)
        t.start()

    def _reader(self):
        """Read char-by-char so we can surface the REPL's `scala> ` prompt — the
        unambiguous "ready for input" signal — in addition to whole lines. Without
        it, the only way to know a command finished is its output, but `:load`'s
        echo is ambiguous; the prompt is not. Emits PROMPT as a sentinel line."""
        cur = ""
        while True:
            ch = self.p.stdout.read(1)
            if ch == "":
                if cur:
                    self.q.put(cur + "\n")
                self.q.put(None)  # EOF
                return
            if ch == "\n":
                self.q.put(cur + "\n")
                cur = ""
            else:
                cur += ch
                if cur == "scala> ":   # fresh prompt at line start -> REPL is idle
                    self.q.put(PROMPT)
                    cur = ""

    def _raw(self, line):
        self.p.stdin.write(line + "\n")
        self.p.stdin.flush()

    def _emit(self, line):
        if line == PROMPT:   # internal readiness marker, not REPL output
            return
        sys.stdout.write("  | " + line)
        sys.stdout.flush()
        if self._sink is not None:
            self._sink.append(line)

    def _drain_to_prompt(self, max_wait):
        """Read (emitting output) until the REPL prints its `scala> ` prompt, i.e.
        it is idle and ready for the next command. Returns True at the prompt,
        False on timeout."""
        start = time.time()
        while time.time() - start < max_wait:
            try:
                line = self.q.get(timeout=1.0)
            except queue.Empty:
                continue
            if line is None:
                raise ReplDied("REPL stdout closed")
            if line == PROMPT:
                return True
            self._emit(line)
        return False

    def capture_start(self):
        self._sink = []

    def capture_take(self):
        out = "".join(self._sink) if self._sink is not None else ""
        self._sink = None
        return out

    def _read_until(self, sentinel, idle_timeout):
        """Read until `sentinel` ('ok'). If a REPL diagnostic appears the command
        was corrupted (JLine ate its first char → unresolved identifier) or failed;
        we return 'error' as soon as the error block ends at the next prompt, so the
        caller can re-send promptly. 'timeout' on idle with no sentinel/error."""
        last = time.time()
        saw_error = False
        while True:
            if time.time() - last > idle_timeout:
                return "error" if saw_error else "timeout"
            try:
                line = self.q.get(timeout=1.0)
            except queue.Empty:
                continue
            if line is None:
                raise ReplDied("REPL stdout closed")
            last = time.time()
            if line == PROMPT:
                if saw_error:
                    return "error"   # error block ended; resend now (don't wait out idle)
                continue
            self._emit(line)
            if sentinel in line:
                return "ok"
            if "-- [E" in line or "error:" in line:
                saw_error = True

    def cmd(self, line, sentinel, idle_timeout, resends=3):
        """Send a quick, resend-safe command (ping/beginTask/endTask/facadeInfo)
        and wait for its sentinel; re-send on a corrupted/failed read. After the
        sentinel, drain to the next prompt so the following command is always sent
        with the REPL idle."""
        for attempt in range(resends):
            self._raw(line)
            r = self._read_until(sentinel, idle_timeout)
            if r == "ok":
                self._drain_to_prompt(30)
                return
            log(f"repl: {line!r} -> {r} (attempt {attempt + 1}/{resends}); re-sending")
        raise ReplError(f"command did not complete: {line!r}")

    def handshake(self, ready_timeout):
        """Bring a freshly-spawned REPL to ready: wait for the FIRST `scala> `
        prompt (so nothing is sent during JLine init, which would drop pre-buffered
        lines), `:load Bench.scala`, a warm-up command (its first char is eaten by
        JLine's 2nd-command glitch — harmless), then a ping. Each command is sent
        only once the REPL is back at a prompt."""
        if not self._drain_to_prompt(ready_timeout):
            raise ReplError("REPL never reached its first prompt")
        self._raw(":load Bench.scala")
        if not self._drain_to_prompt(ready_timeout):
            raise ReplError(":load Bench.scala did not return to a prompt")
        self._raw("42")  # warm-up: absorbs JLine's 2nd-command first-char glitch
        self._drain_to_prompt(30)
        self.cmd('Tau.ping("ready")', "__TAU_PONG__ready", 60)

    def run_turn(self, i, idle_timeout, hard_timeout=None, resends=3):
        """Feed `turn(i)` and wait for its done sentinel. Re-send only if the
        command was corrupted BEFORE it started executing (safe: it never ran);
        if it started but then stalls, raise ReplDied so the caller restarts.

        Two independent bounds once the turn is executing: `idle_timeout` (no
        output for that long) AND `hard_timeout` (total wall-clock, regardless of
        output). The hard cap is essential because a *chatty* runaway (a loop that
        keeps emitting [tool]/[agent] lines, or repeated agent[...] calls) resets
        the idle timer on every line and would otherwise never trip it."""
        sentinel = f"__TAU_TURN_DONE__{i}"
        for attempt in range(resends):
            self._raw(f"turn({i})")
            started = False
            last = time.time()
            exec_start = last
            while True:
                now = time.time()
                if started and hard_timeout and now - exec_start > hard_timeout:
                    raise ReplDied(
                        f"turn({i}) exceeded hard {hard_timeout:.0f}s wall cap "
                        f"(runaway code that keeps emitting output)")
                if now - last > idle_timeout:
                    if started:
                        raise ReplDied(f"turn({i}) stalled after starting")
                    log(f"repl: turn({i}) no output (attempt {attempt + 1}); re-sending")
                    break
                try:
                    line = self.q.get(timeout=1.0)
                except queue.Empty:
                    continue
                if line is None:
                    raise ReplDied("REPL stdout closed")
                last = time.time()
                self._emit(line)
                if sentinel in line:
                    self._drain_to_prompt(30)  # consume the rendered reply + prompt
                    return
                if any(m in line for m in TURN_OUTPUT_MARKERS):
                    started = True
                elif not started and ("-- [E" in line or "error:" in line):
                    log(f"repl: turn({i}) corrupted before start (attempt "
                        f"{attempt + 1}); re-sending")
                    self._drain_to_prompt(10)  # consume the rest of the error block
                    break
        raise ReplError(f"turn({i}) did not complete")

    def restart(self):
        log("restarting REPL")
        self.kill()
        self._spawn()

    def kill(self):
        try:
            if self.p and self.p.poll() is None:
                self.p.kill()
        except Exception:
            pass

    def close(self):
        try:
            if self.p and self.p.poll() is None:
                self.p.stdin.close()
                self.p.wait(timeout=10)
        except Exception:
            self.kill()


# -------------------------------------------------------------- trace -> run file

def build_result(messages, final_reply):
    """Turn tau2's simulation_run messages into the run file's interleaved
    result[] (user / agent / tool_call / tool_result), ending with output_text."""
    id_to_name = {}
    out = []
    last_agent_text = None
    for m in messages:
        role = m.get("role")
        content = m.get("content")
        tcs = m.get("tool_calls")
        if role == "user" and not tcs:
            if content:
                out.append({"type": "user_message", "output": content})
        elif role == "assistant":
            if tcs:
                for tc in tcs:
                    id_to_name[tc.get("id", "")] = tc.get("name", "")
                    out.append({"type": "tool_call", "tool_name": tc.get("name"),
                                "arguments": tc.get("arguments", {})})
            elif content and content != STOP_TOKEN:
                last_agent_text = content
                out.append({"type": "agent_message", "output": content})
        elif role == "tool":
            out.append({"type": "tool_result",
                        "tool_name": id_to_name.get(m.get("id", ""), ""),
                        "output": content})
    out.append({"type": "output_text",
                "output": final_reply or last_agent_text or ""})
    return out


def tool_call_counts(messages):
    """Per-tool call counts (incl. `respond` and `done`) from the trajectory."""
    counts = {}
    for m in messages:
        if m.get("role") != "assistant":
            continue
        if m.get("tool_calls"):
            for tc in m["tool_calls"]:
                n = tc.get("name", "?")
                counts[n] = counts.get(n, 0) + 1
        elif m.get("content") == STOP_TOKEN:
            counts["done"] = counts.get("done", 0) + 1
        elif m.get("content"):
            counts["respond"] = counts.get("respond", 0) + 1
    return counts


# ----------------------------------------------------------------------- the loop

def main():
    domain = os.getenv("BENCH_DOMAIN", "retail")
    n = int(os.getenv("BENCH_N", "10"))
    start = int(os.getenv("BENCH_START", "0"))
    # Explicit, possibly non-contiguous task indices (comma-separated) override the
    # [start, start+n) slice — used by rerun_failed.sh to backfill missing tasks.
    indices_env = os.getenv("BENCH_TASK_INDICES", "").strip()
    indices = ([int(x) for x in indices_env.split(",") if x.strip()]
               if indices_env else list(range(start, start + n)))
    run_id = os.getenv("BENCH_RUN_ID", "lacuna")
    eval_log_dir = Path(os.getenv("EVAL_LOG_DIR", "log")).resolve()
    facade = os.getenv("TAU_FACADE")
    agent_file = os.getenv("AGENT_FILE", "../browsecomp-plus-bench/Agent.scala")
    server_url = os.getenv("TAU2_SERVER_URL", "http://127.0.0.1:8881")
    agent_model = os.getenv("AGENT_MODEL", "deepseek-v4-flash")
    user_model = os.getenv("TAU2_USER_MODEL", "unknown")
    max_turns = int(os.getenv("TAU_MAX_TURNS", "40"))
    turn_timeout = float(os.getenv("TAU_TURN_TIMEOUT", "300"))
    # Hard per-turn wall-clock cap (regardless of output) — bounds a chatty
    # runaway that the idle timeout above can't catch. Generous so legitimate
    # long turns (thinking + many tool calls) aren't killed.
    turn_hard_timeout = float(os.getenv("TAU_TURN_HARD_TIMEOUT", "900"))
    ready_timeout = float(os.getenv("TAU_READY_TIMEOUT", "400"))
    # Run each task this many times (fresh session + history each time). pass^1 is
    # the mean over trials (headline); evaluate.py also derives pass^k. Default 1.
    num_trials = int(os.getenv("BENCH_NUM_TRIALS", "1"))

    if not facade or not Path(facade).is_file():
        sys.exit(f"repl_bench: TAU_FACADE not set or missing: {facade!r}")

    cwd = Path(__file__).resolve().parent
    eval_log_dir.mkdir(parents=True, exist_ok=True)
    history_file = eval_log_dir / "session.repl"
    run_dir = (cwd / "runs" / run_id).resolve()
    trace_root = run_dir / "trace"
    turns_root = run_dir / "turns"
    run_dir.mkdir(parents=True, exist_ok=True)

    # Cap each REPL JVM's heap so many shards (e.g. 16) don't over-subscribe RAM;
    # without this scala-cli lets each JVM default to ~25% of physical memory.
    repl_xmx = os.getenv("REPL_XMX", "4g")
    scala_cmd = [
        "scala-cli", "repl", "--server=false",
        "--java-opt", f"-Xmx{repl_xmx}",
        "-O", "-color:never",   # plain output -> clean sentinel/prompt matching
        "-O", f"-Xrepl-eval-log-dir:{eval_log_dir}/",
        "-O", f"-Xrepl-history-file:{history_file}",
        agent_file, "Tau.scala", facade,
    ]
    # Agent.scala dumps each LLM call's full prompt here (so it flows into the
    # per-task trace dir alongside the generated code). Same dir as the eval log.
    os.environ["AGENT_PROMPT_LOG_DIR"] = str(eval_log_dir)

    tau = Tau2Client(server_url)
    repl = Repl(scala_cmd, str(cwd))

    # --- startup handshake: :load + warm-up + ping + facade check ---------------
    repl.handshake(ready_timeout)
    # Verify the co-loaded facade matches the domain, else the agent sees wrong tools.
    fac = {"line": None}

    def grab_facade():
        repl._raw("facadeInfo()")
        # facadeInfo prints "__TAU_FACADE__ <domain> <ntools>"
        last = time.time()
        while time.time() - last < 30:
            try:
                line = repl.q.get(timeout=1.0)
            except queue.Empty:
                continue
            if line is None:
                raise ReplDied("REPL closed during facade check")
            if line == PROMPT:
                continue
            repl._emit(line)
            if "__TAU_FACADE__" in line:
                fac["line"] = line.split("__TAU_FACADE__", 1)[1].strip()
                repl._drain_to_prompt(30)
                return
        raise ReplError("no facade info")

    grab_facade()
    fac_domain = fac["line"].split()[0] if fac["line"] else "?"
    if fac_domain != domain:
        repl.kill()
        sys.exit(f"repl_bench: facade is for {fac_domain!r} but BENCH_DOMAIN={domain!r}")
    log(f"facade ok: {fac['line']}  | domain={domain} run_id={run_id}")

    ok = 0
    # Flat (task, trial) work list so a REPL restart resumes at the next unit.
    work = [(idx, t) for idx in indices for t in range(num_trials)]
    for idx, trial in work:
        label = f"{domain}-{idx}" + (f" (trial {trial})" if num_trials > 1 else "")
        try:
            run_one(tau, repl, domain, idx, run_id, run_dir, trace_root, turns_root,
                    eval_log_dir, agent_model, user_model,
                    max_turns, turn_timeout, ready_timeout, trial, num_trials,
                    turn_hard_timeout)
            ok += 1
        except (ReplDied, ReplError) as e:
            log(f"task {label}: REPL failure ({e}); restarting REPL")
            try:
                repl.restart()
                repl.handshake(ready_timeout)
            except Exception as e2:
                log(f"REPL restart failed: {e2}; aborting shard")
                break
        except Exception as e:
            log(f"task {label} errored: {type(e).__name__}: {e}")

    repl.close()
    log(f"completed {ok}/{len(work)} runs ({domain}, {num_trials} trial(s)) -> runs/{run_id}/")


def run_one(tau, repl, domain, idx, run_id, run_dir, trace_root, turns_root,
            eval_log_dir, agent_model, user_model,
            max_turns, turn_timeout, ready_timeout, trial=0, num_trials=1,
            turn_hard_timeout=900):
    task_key = f"{domain}-{idx}"
    # Per-trial key so trials don't clobber each other's files/dirs; the run file
    # still records task_id, so evaluate.py groups trials per task for pass^k.
    run_key = task_key if num_trials <= 1 else f"{task_key}.t{trial}"
    log(f"=== task {run_key} ===")
    started_at = datetime.now(timezone.utc)
    t0 = time.time()

    # 1. reset the tau2 session for this task.
    info = tau.reset(domain, idx)
    session_id = info["session_id"]
    task_id = info.get("task_id", "")
    first_msg = info.get("observation", "") or ""
    policy = info.get("policy", "") or ""

    turn_dir = turns_root / run_key
    turn_dir.mkdir(parents=True, exist_ok=True)
    policy_file = turn_dir / "policy.txt"
    policy_file.write_text(policy, encoding="utf-8")

    # 2. bind the session in the REPL. The per-task history reset happens INSIDE
    #    the REPL (turn(0) calls Tau.resetHistory) so it's race-free with the
    #    REPL's deferred transcript flush — the persistent REPL is reused across
    #    tasks, and each task's agent must see only its own conversation.
    repl.cmd(f'Tau.beginTask("{session_id}", "{domain}", '
             f'"{turn_dir}", "{policy_file}")',
             "__TAU_OK__ beginTask", 60)

    # snapshot eval-log files so we can attribute this task's generated-code rounds.
    pre_logs = set(os.listdir(eval_log_dir)) if eval_log_dir.exists() else set()
    repl.capture_start()

    # 3. conversation loop (OUTSIDE the REPL).
    question = first_msg
    reply = ""
    agent_done = False
    convo_done = not bool(first_msg)
    i = 0
    while not convo_done and i < max_turns:
        (turn_dir / f"{i}.q.txt").write_text(question, encoding="utf-8")
        repl.run_turn(i, turn_timeout, turn_hard_timeout)
        reply_obj = json.loads((turn_dir / f"{i}.r.json").read_text(encoding="utf-8"))
        reply = reply_obj.get("reply", "")
        agent_done = bool(reply_obj.get("agent_done"))

        # deliver this turn's reply to the simulated customer.
        step = tau.respond(session_id, reply)
        question = step.get("observation", "") or ""
        convo_done = bool(step.get("done"))

        # Guard against a tau2 gym quirk: when a turn ends right after tool calls
        # (e.g. it errored), the respond step's observation can come back as the
        # trailing tool/assistant message instead of the customer's reply. Never
        # feed that as the next turn's customer message — end the task instead
        # (reward/trace below still score whatever happened).
        if not convo_done and step.get("obs_role") in ("tool", "assistant"):
            log(f"turn {i}: respond observation role={step.get('obs_role')!r} "
                f"(not a customer reply); ending task")
            convo_done = True

        if agent_done:
            # the agent resolved the task: its reply was delivered above; now end
            # the tau2 conversation (if the user simulator hasn't already).
            if not convo_done:
                tau.done(session_id)
            convo_done = True
        i += 1

    # 4-5. authoritative reward + full trajectory from tau2 (forces a clean finish).
    reward_info = None
    try:
        rr = tau.reward(session_id)
        reward = float(rr.get("reward", 0.0))
        reward_info = rr.get("reward_info")  # tau2 RewardInfo: which check failed
    except Exception as e:
        log(f"reward failed: {e}")
        reward = 0.0
    try:
        tr = tau.trace(session_id)
        messages = tr.get("messages") or []
        termination = tr.get("termination_reason")
    except Exception as e:
        log(f"trace failed: {e}")
        messages, termination = [], None

    # 6. agent token/retry stats (JVM-only counters) via a stats-dump command.
    stats_file = turn_dir / "stats.json"
    agent_stats, llm_usage = {}, {}
    try:
        repl.cmd(f'Tau.endTask("{stats_file}")', "__TAU_OK__ endTask", 60)
        s = json.loads(stats_file.read_text(encoding="utf-8"))
        agent_stats = s.get("agent_stats", {})
        llm_usage = s.get("llm_usage", {})
    except Exception as e:
        log(f"endTask/stats failed: {e}")

    progress = repl.capture_take()
    tau.close(session_id)

    # generated-code rounds produced during this task.
    post_logs = set(os.listdir(eval_log_dir)) if eval_log_dir.exists() else set()
    new_logs = sorted(post_logs - pre_logs)
    eval_rounds = sum(1 for f in new_logs if f.endswith("_wrapper.scala"))

    # trace dir: captured stdout + this task's generated-code rounds.
    qtrace = trace_root / run_key
    qtrace.mkdir(parents=True, exist_ok=True)
    (qtrace / "progress.log").write_text(progress, encoding="utf-8")
    for f in new_logs:
        try:
            shutil.copyfile(eval_log_dir / f, qtrace / f)
        except Exception:
            pass

    # 7. run file.
    ended_at = datetime.now(timezone.utc)
    result = build_result(messages, reply)
    counts = tool_call_counts(messages)
    obj = {
        "metadata": {
            "model": agent_model,
            "user_model": user_model,
            "thinking": os.getenv("AGENT_THINKING", ""),
            "temperature": os.getenv("AGENT_TEMPERATURE", ""),
            "num_trials": num_trials,
            "started_at": started_at.isoformat(),
            "ended_at": ended_at.isoformat(),
            "duration_sec": round(time.time() - t0, 3),
            "eval_rounds": eval_rounds,
            "termination_reason": termination,
            "agent_stats": agent_stats,
            "llm_usage": llm_usage,
        },
        "task_id": task_id,
        "task_key": task_key,
        "trial": trial,
        "domain": domain,
        "task_index": idx,
        "first_user_message": first_msg,
        "status": "completed",
        "reward": reward,
        "reward_info": reward_info,
        "tool_call_counts": counts,
        "result": result,
    }
    out_path = run_dir / f"{run_key}.json"
    out_path.write_text(json.dumps(obj, indent=2), encoding="utf-8")
    n_tools = sum(counts.values())
    log(f"task {task_key} done -> runs/{run_id}/{task_key}.json "
        f"(reward={reward}, turns={i}, tool_calls={n_tools}, "
        f"rounds={eval_rounds}, {round(time.time() - t0)}s)")


if __name__ == "__main__":
    main()
