#!/usr/bin/env python3
"""
Sanity-check that API-embedded queries are compatible with the published 8B
corpus index.

The corpus vectors were produced by a local `Qwen/Qwen3-Embedding-8B` (EOS
pooling, L2-normalized). Here we embed the *query* through a hosted
OpenAI-compatible API instead. If the provider serves a compatible checkpoint
this should retrieve the right documents; a mismatch (wrong pooling / model /
normalization) collapses recall to near zero.

We measure recall@k of retrieved docids against each query's `evidence_docids`
and print the mean. Run this BEFORE trusting any benchmark numbers.

  RECALL_SAMPLE=50 .venv/bin/python check_recall.py
"""

import json
import os

import numpy as np

from retriever_server import load_index, make_embedder

SAMPLE = int(os.getenv("RECALL_SAMPLE", "50"))
KS = [5, 20, 100]


def main():
    index, lookup = load_index()
    embed = make_embedder()

    rows = [json.loads(l) for l in open("data/queries_slim.jsonl")][:SAMPLE]
    maxk = max(KS)
    recalls = {k: [] for k in KS}

    for i, row in enumerate(rows):
        ev = set(row.get("evidence_docids") or [])
        if not ev:
            continue
        q = embed(row["query"]).reshape(1, -1)
        _, idxs = index.search(q, maxk)
        ranked = [str(lookup[j]) for j in idxs[0] if j >= 0]
        for k in KS:
            recalls[k].append(len(ev.intersection(ranked[:k])) / len(ev))
        print(
            f"  [{i + 1}/{len(rows)}] q{row['query_id']}: "
            f"R@5={recalls[5][-1]:.2f} R@20={recalls[20][-1]:.2f} "
            f"R@100={recalls[100][-1]:.2f}",
            flush=True,
        )

    n = len(recalls[KS[0]])
    print(f"\n=== mean recall over {n} queries with evidence qrels ===")
    for k in KS:
        print(f"  recall@{k}: {np.mean(recalls[k]) if recalls[k] else 0.0:.4f}")
    print(
        "\nInterpretation: near-zero recall => embedding incompatible "
        "(check model id / pooling / normalization). Non-trivial recall "
        "(R@100 well above 0) => API embedding is compatible with the index."
    )


if __name__ == "__main__":
    main()
