#!/usr/bin/env python3
"""
Reproduce every tau2-bench number reported in the paper, straight from the run
dirs — both the Table~\\ref{tab:tau2} cells and the prose statistics.

For each run dir (one domain) it computes:
  - pass^1  = solved-rate (reward == 1.0)            [the table cell]
  - mean user turns / task                           [prose]
  - mean backend tool calls / task                   [prose]
  - retries / task, compile-reject %, compile-success % (from agent_stats) [prose]
  - mean tokens / task, reward_info failure breakdown
Pass several dirs (a model's 4 domains) to also get the pooled Overall and the
aggregate code-gen stats across the whole model.

  python paper_stats.py runs/<retail> runs/<airline> runs/<telecom> runs/<twf>
  python paper_stats.py runs/2026*deepseek*            # shell-glob the 4 domains
"""

import glob
import json
import sys
from collections import defaultdict


def load(run_dir):
    return [json.loads(open(f).read()) for f in glob.glob(f"{run_dir}/*.json")]


def domain_stats(runs):
    n = len(runs)
    solved = sum(1 for x in runs if x["reward"] >= 1.0)
    turns = sum(sum(1 for r in x["result"] if r.get("type") == "user_message") for x in runs)
    tools = sum(sum(x.get("tool_call_counts", {}).values()) for x in runs)
    # code-gen counters from the agent's JVM stats
    att = ok = fail = ret = 0
    for x in runs:
        s = (x["metadata"] or {}).get("agent_stats") or {}
        att += s.get("attempts", 0)
        ok += s.get("compiles_ok", 0)
        fail += s.get("compiles_failed", 0)
        ret += s.get("retries", 0)
    tok = defaultdict(int)
    for x in runs:
        u = (x["metadata"] or {}).get("llm_usage") or {}
        for k in ("prompt_tokens", "completion_tokens", "total_tokens"):
            tok[k] += u.get(k, 0)
    fails = defaultdict(int)
    for x in runs:
        if x["reward"] < 1.0:
            bd = ((x.get("reward_info") or {}).get("reward_breakdown") or {}) if isinstance(x.get("reward_info"), dict) else {}
            hit = [str(k) for k, v in bd.items() if v is not None and v < 1.0]
            for k in (hit or ["unknown"]):
                fails[k] += 1
    return dict(n=n, solved=solved, turns=turns, tools=tools, attempts=att,
                ok=ok, fail=fail, retries=ret, tok=tok, fails=dict(fails))


def main():
    dirs = sys.argv[1:]
    if not dirs:
        raise SystemExit(__doc__)
    G = dict(n=0, solved=0, turns=0, tools=0, attempts=0, ok=0, fail=0, retries=0)
    print(f"{'domain':>26} {'n':>4} {'pass^1':>7} {'turns':>6} {'tools':>6} "
          f"{'retry':>6} {'rej%':>6} {'cok%':>6} {'tok/task':>9}")
    for d in dirs:
        runs = load(d)
        if not runs:
            print(f"{d.split('/')[-1]:>26}  (empty)"); continue
        s = domain_stats(runs)
        gen = s["attempts"] or 1
        name = (runs[0].get("domain") or d.split('/')[-1])
        print(f"{name:>26} {s['n']:>4} {100*s['solved']/s['n']:>6.1f}% "
              f"{s['turns']/s['n']:>6.1f} {s['tools']/s['n']:>6.1f} "
              f"{s['retries']/s['n']:>6.1f} {100*s['fail']/gen:>5.1f}% "
              f"{100*s['ok']/gen:>5.1f}% {s['tok']['total_tokens']//s['n']:>9}")
        if s["fails"]:
            print(f"{'':>26}   failures: {s['fails']}")
        for k in ("n", "solved", "turns", "tools", "attempts", "ok", "fail", "retries"):
            G[k] += s[k]
    if len(dirs) > 1 and G["n"]:
        gen = G["attempts"] or 1
        print("-" * 86)
        print(f"{'OVERALL (pooled)':>26} {G['n']:>4} {100*G['solved']/G['n']:>6.1f}% "
              f"{G['turns']/G['n']:>6.1f} {G['tools']/G['n']:>6.1f} "
              f"{G['retries']/G['n']:>6.1f} {100*G['fail']/gen:>5.1f}% {100*G['ok']/gen:>5.1f}%")
        print(f"  (reject% = compiles_failed/attempts; cok% = compile-success = compiles_ok/attempts)")


if __name__ == "__main__":
    main()
