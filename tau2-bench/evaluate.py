#!/usr/bin/env python3
"""
Aggregate tau2-bench run files produced by the Lacuna `agent` benchmark.

tau2-bench reward is computed PROGRAMMATICALLY by tau2's evaluator (action checks
+ DB-state match + required communication), so there is NO LLM judge here: the
gym env already wrote a per-task `reward` into each run file. This script reads
them and reports the average reward (the headline metric), a per-domain
breakdown, a solved rate (reward == 1.0), and pass^k when a task was run k times.

  python evaluate.py --input_dir runs/lacuna
  python evaluate.py --input_dir runs/lacuna -k 4     # pass^4 (4 trials/task)
"""

import argparse
import csv
import json
from collections import defaultdict
from datetime import datetime
from pathlib import Path


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input_dir", default="runs/lacuna")
    ap.add_argument("--eval_dir", default="evals")
    ap.add_argument("-k", type=int, default=1,
                    help="pass^k: tasks must score full reward in all k trials")
    args = ap.parse_args()

    run_files = sorted(Path(args.input_dir).glob("*.json"))
    if not run_files:
        raise SystemExit(f"no run files in {args.input_dir}")

    rows = []
    by_domain = defaultdict(list)
    by_task = defaultdict(list)   # (domain, task_id) -> [reward, ...]
    model = None
    for fp in run_files:
        run = json.loads(fp.read_text(encoding="utf-8"))
        model = model or (run.get("metadata") or {}).get("model")
        reward = float(run.get("reward", 0.0))
        domain = run.get("domain", "?")
        rows.append((run.get("task_key"), domain, run.get("task_id"), reward,
                     run.get("tool_call_counts", {})))
        by_domain[domain].append(reward)
        by_task[(domain, run.get("task_id"))].append(reward)

    n = len(rows)
    avg = sum(r for _, _, _, r, _ in rows) / n if n else 0.0
    solved = sum(1 for _, _, _, r, _ in rows if r >= 1.0)

    # pass^k: of tasks with >=k trials, the fraction whose first k trials all
    # scored a full reward. (tau2-bench's reliability metric.)
    passk = None
    if args.k > 1:
        full = [t for t, v in by_task.items() if len(v) >= args.k]
        if full:
            passk = sum(
                1 for t in full if all(x >= 1.0 for x in by_task[t][:args.k])
            ) / len(full)

    out_dir = Path(args.eval_dir) / Path(args.input_dir).name
    out_dir.mkdir(parents=True, exist_ok=True)

    summary = {
        "Benchmark": "tau2-bench",
        "LLM": model or "unknown",
        "n_tasks": n,
        "Avg reward": round(avg, 4),
        "Solved (reward==1.0) %": round(100 * solved / n, 2) if n else 0.0,
        "By domain (avg reward)": {d: round(sum(v) / len(v), 4)
                                   for d, v in by_domain.items()},
        f"pass^{args.k} (%)": round(100 * passk, 2) if passk is not None else None,
        "Evaluation Date": datetime.now().date().isoformat(),
    }
    (out_dir / "evaluation_summary.json").write_text(
        json.dumps(summary, indent=2), encoding="utf-8")

    with (out_dir / "detailed.csv").open("w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["task_key", "domain", "task_id", "reward", "n_tool_calls"])
        for key, domain, tid, reward, tc in rows:
            w.writerow([key, domain, tid, reward, sum(tc.values())])

    print("\n=== tau2-bench evaluation ===")
    for k, v in summary.items():
        print(f"  {k}: {v}")
    print(f"\nwritten to {out_dir}/")


if __name__ == "__main__":
    main()
