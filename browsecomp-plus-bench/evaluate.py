#!/usr/bin/env python3
"""
Evaluate BrowseComp-Plus run files produced by the Lacuna `agent` benchmark.

Adapted from BrowseComp-Plus's scripts_evaluation/evaluate_with_openai.py. The
only substantive change: the upstream script grades via the OpenAI *Responses*
API (`client.responses.create`), which is OpenAI-only. This version uses the
chat-completions API so any OpenAI-compatible endpoint works (OpenRouter,
DeepInfra, OpenAI, ...). Grading prompt, parsing, and metrics are unchanged.

  source env.sh
  # --input_dir is the runs/<run-id>/ directory of the run you want to grade
  # (e.g. runs/20260521-004530_deepseek-v4-flash). Results land in
  # evals/<run-id>/, so each run's evaluation is kept separate too.
  .venv/bin/python evaluate.py --input_dir runs/<run-id> \
      --judge_base_url https://openrouter.ai/api/v1 \
      --judge_key_env EMBED_API_KEY --model openai/gpt-4.1

Metrics: Accuracy (LLM judge), retrieval Recall vs qrel_evidence, avg tool
calls, citation precision/recall, calibration error (needs >=100 queries).
"""

import argparse
import csv
import json
import os
import re
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
from pathlib import Path
from typing import Dict, List

import numpy as np
import openai
from tqdm import tqdm

# Verbatim from BrowseComp-Plus search_agent/prompts.py
GRADER_TEMPLATE = """
Judge whether the following [response] to [question] is correct or not based on the precise and unambiguous [correct_answer] below.

[question]: {question}

[response]: {response}

Your judgement must be in the format and criteria specified below:

extracted_final_answer: The final exact answer extracted from the [response]. Put the extracted answer as 'None' if there is no exact, final answer to extract from the response.

[correct_answer]: {correct_answer}

reasoning: Explain why the extracted_final_answer is correct or incorrect based on [correct_answer], focusing only on if there are meaningful differences between [correct_answer] and the extracted_final_answer. Do not comment on any background to the problem, do not attempt to solve the problem, do not argue for any answer different than [correct_answer], focus only on whether the answers match.

correct: Answer 'yes' if extracted_final_answer matches the [correct_answer] given above, or is within a small margin of error for numerical problems. Answer 'no' otherwise, i.e. if there if there is any inconsistency, ambiguity, non-equivalency, or if the extracted answer is incorrect.


confidence: The extracted confidence score between 0|\\%| and 100|\\%| from [response]. Put 100 if there is no confidence score available.
""".strip()


def load_ground_truth(path: Path) -> Dict[str, Dict[str, str]]:
    gt = {}
    with path.open(encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            o = json.loads(line)
            gt[str(o["query_id"])] = {"question": o["query"], "answer": o["answer"]}
    return gt


def load_qrel(path: Path) -> Dict[str, List[str]]:
    qrel = defaultdict(list)
    if not path.exists():
        return dict(qrel)
    with path.open(encoding="utf-8") as f:
        for line in f:
            parts = line.split()
            if len(parts) == 4:
                qrel[parts[0]].append(parts[2])
    return dict(qrel)


def parse_judge_response(text: str) -> dict:
    r = {"extracted_final_answer": None, "reasoning": None, "correct": None,
         "confidence": None, "parse_error": False}
    if not text:
        r["parse_error"] = True
        return r
    m = re.search(r"extracted_final_answer:?\**\s*(.*?)(?=\n|$)", text,
                  re.IGNORECASE | re.DOTALL)
    if m:
        r["extracted_final_answer"] = m.group(1).strip().lstrip("*").strip()
    m = re.search(r"\bcorrect:?\**\s*(yes|no)", text, re.IGNORECASE)
    if m:
        r["correct"] = m.group(1).lower() == "yes"
    m = re.search(r"confidence:?\**\s*(\d+(?:\.\d+)?)", text, re.IGNORECASE)
    if m:
        r["confidence"] = min(float(m.group(1)), 100.0)
    if r["correct"] is None:
        r["parse_error"] = True
    return r


def extract_citations(text: str) -> List[str]:
    if not text:
        return []
    docids = set(re.findall(r"\[(\d+)\]", text))
    for grp in re.findall(r"\[([^\[\]]*?)\]", text):
        docids.update(re.findall(r"\d+", grp))
    return list(docids)


# calibration error (from BrowseComp-Plus, originally hendrycks/outlier-exposure)
def calib_err(confidence, correct, p="2", beta=100):
    idxs = np.argsort(confidence)
    confidence, correct = confidence[idxs], correct[idxs]
    bins = [[i * beta, (i + 1) * beta] for i in range(len(confidence) // beta)]
    if not bins:
        return 0.0
    bins[-1] = [bins[-1][0], len(confidence)]
    cerr, total = 0.0, len(confidence)
    for lo, hi in bins[:-1] if len(bins) > 1 else bins:
        bc, bk = confidence[lo:hi], correct[lo:hi]
        if len(bc):
            cerr += len(bc) / total * np.square(
                abs(np.nanmean(bc) - np.nanmean(bk)))
    return float(np.sqrt(cerr))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input_dir", required=True)
    ap.add_argument("--ground_truth", default="data/queries_slim.jsonl")
    ap.add_argument("--qrel_evidence",
                    default="BrowseComp-Plus/topics-qrels/qrel_evidence.txt")
    ap.add_argument("--eval_dir", default="evals")
    ap.add_argument("--model", default="openai/gpt-4.1",
                    help="judge model id on the chosen endpoint")
    ap.add_argument("--judge_base_url", default="https://openrouter.ai/api/v1")
    ap.add_argument("--judge_key_env", default="EMBED_API_KEY",
                    help="env var holding the judge API key")
    ap.add_argument("--max_output_tokens", type=int, default=1024)
    ap.add_argument("--workers", type=int, default=1,
                    help="parallel judge requests (thread pool); 1 = sequential")
    args = ap.parse_args()

    gt = load_ground_truth(Path(args.ground_truth))
    qrel = load_qrel(Path(args.qrel_evidence))

    key = os.getenv(args.judge_key_env)
    if not key:
        raise SystemExit(f"{args.judge_key_env} not set")
    client = openai.OpenAI(base_url=args.judge_base_url, api_key=key)

    out_dir = Path(args.eval_dir) / Path(args.input_dir).name
    out_dir.mkdir(parents=True, exist_ok=True)

    run_files = sorted(Path(args.input_dir).glob("*.json"))
    if not run_files:
        raise SystemExit(f"no run files in {args.input_dir}")

    def judge_one(fp):
        run = json.loads(fp.read_text(encoding="utf-8"))
        model = (run.get("metadata") or {}).get("model")
        qid = str(run.get("query_id"))
        if qid not in gt:
            print(f"  skip {fp.name}: no ground truth")
            return None

        retrieved = set(run.get("retrieved_docids", []))
        evid = qrel.get(qid, [])
        recall = (len(retrieved & set(evid)) / len(evid)) if evid else None

        response = ""
        res = run.get("result", [])
        if res and res[-1].get("type") == "output_text":
            response = res[-1].get("output", "")

        judge = {"parse_error": True}
        if response and run.get("status") == "completed":
            prompt = GRADER_TEMPLATE.format(
                question=gt[qid]["question"], response=response,
                correct_answer=gt[qid]["answer"])
            try:
                resp = client.chat.completions.create(
                    model=args.model,
                    messages=[{"role": "user", "content": prompt}],
                    max_tokens=args.max_output_tokens,
                    temperature=0.0)
                judge = parse_judge_response(resp.choices[0].message.content or "")
            except Exception as e:
                print(f"  judge error {fp.name}: {e}")

        cited = extract_citations(response)
        cite_prec = (len(set(cited) & set(evid)) / len(cited)) if cited else 0.0
        cite_rec = (len(set(cited) & set(evid)) / len(evid)) if evid else 0.0
        rec = {
            "query_id": qid, "response": response,
            "correct_answer": gt[qid]["answer"], "judge": judge,
            "recall": recall, "tool_call_counts": run.get("tool_call_counts", {}),
            "cited_docids": cited, "cite_precision": cite_prec,
            "cite_recall": cite_rec,
        }
        (out_dir / f"{qid}_eval.json").write_text(
            json.dumps(rec, indent=2, ensure_ascii=False), encoding="utf-8")
        return rec, model

    # Judge calls are independent, IO-bound API requests, so --workers>1 fans
    # them out over a thread pool (the OpenAI client is thread-safe). ex.map
    # yields in input order, so results stay in sorted-filename order as before.
    # Default --workers 1 preserves the original sequential behavior.
    results = []
    detected_model = None
    if args.workers > 1:
        with ThreadPoolExecutor(max_workers=args.workers) as ex:
            outs = tqdm(ex.map(judge_one, run_files),
                        total=len(run_files), desc="judging")
            collected = list(outs)
    else:
        collected = [judge_one(fp) for fp in tqdm(run_files, desc="judging")]
    for out in collected:
        if out is None:
            continue
        rec, model = out
        detected_model = detected_model or model
        results.append(rec)

    n = len(results)
    correct = sum(1 for r in results if r["judge"].get("correct"))
    recalls = [r["recall"] for r in results if r["recall"] is not None]
    tool_tot = defaultdict(float)
    for r in results:
        for t, c in r["tool_call_counts"].items():
            tool_tot[t] += c
    avg_tools = {t: round(c / n, 2) for t, c in tool_tot.items()} if n else {}

    confs = [r["judge"]["confidence"] for r in results
             if not r["judge"].get("parse_error") and r["judge"].get("confidence")
             is not None and r["judge"].get("correct") is not None]
    corrs = [r["judge"]["correct"] for r in results
             if not r["judge"].get("parse_error") and r["judge"].get("confidence")
             is not None and r["judge"].get("correct") is not None]
    calib = (round(calib_err(np.array(confs) / 100.0,
                             np.array(corrs, dtype=float)) * 100, 2)
             if len(confs) >= 100 else None)

    summary = {
        "LLM": detected_model or "unknown",
        "Retriever": "Qwen3-Embedding-8B (OpenRouter)",
        "n_queries": n,
        "Accuracy (%)": round(100 * correct / n, 2) if n else 0.0,
        "Recall (%)": round(100 * float(np.mean(recalls)), 2) if recalls else None,
        "avg_tool_calls": avg_tools,
        "Calibration Error (%)": calib,
        "Citation Precision (%)": round(
            100 * np.mean([r["cite_precision"] for r in results]), 2) if n else 0.0,
        "Citation Recall (%)": round(
            100 * np.mean([r["cite_recall"] for r in results]), 2) if n else 0.0,
        "Evaluation Date": datetime.now().date().isoformat(),
    }
    (out_dir / "evaluation_summary.json").write_text(
        json.dumps(summary, indent=2), encoding="utf-8")

    with (out_dir / "detailed.csv").open("w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["query_id", "correct", "confidence", "recall",
                    "predicted_answer", "correct_answer"])
        for r in results:
            w.writerow([r["query_id"], r["judge"].get("correct"),
                        r["judge"].get("confidence"), r["recall"],
                        r["judge"].get("extracted_final_answer"),
                        r["correct_answer"]])

    print("\n=== BrowseComp-Plus evaluation ===")
    for k, v in summary.items():
        print(f"  {k}: {v}")
    print(f"\nwritten to {out_dir}/")


if __name__ == "__main__":
    main()
