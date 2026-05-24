#!/usr/bin/env python3
"""
Lightweight retrieval HTTP shim for running BrowseComp-Plus with the Scala
`agent` primitive.

BrowseComp-Plus ships its retriever as an MCP server backed by Pyserini /
FAISS with a *local* GPU embedding model. We keep the canonical corpus and the
canonical pre-built embedding index, but:

  * the corpus document vectors come straight from the published
    `qwen3-embedding-8b` index shards (no GPU needed to read them);
  * the *query* is embedded through an OpenAI-compatible embeddings API
    (`Qwen/Qwen3-Embedding-8B` hosted by a model provider) instead of a local
    GPU model;
  * search is a plain CPU FAISS inner-product lookup;
  * the two operations are exposed as plain JSON HTTP endpoints so the Scala
    REPL side can call them with `java.net.http` (no MCP client needed).

Endpoints (all POST, JSON in / JSON out):
  POST /search        {"query": str, "k": int?}  -> [{docid, score, snippet}]
  POST /get_document  {"docid": str}             -> {docid, text} | {error}
  GET  /health        -> {status, num_docs, dim}

Config via environment variables:
  EMBED_BASE_URL   OpenAI-compatible base URL of the embedding provider
  EMBED_API_KEY    API key for that provider
  EMBED_MODEL      embedding model id        (default: Qwen/Qwen3-Embedding-8B)
  INDEX_GLOB       glob for the corpus shards
                   (default: indexes/qwen3-embedding-8b/corpus.shard*.pkl)
  CORPUS_GLOB      glob for the corpus text parquet files
                   (default: corpus/data/*.parquet)
  RETRIEVER_PORT   HTTP port                 (default: 8765)
  SNIPPET_MAX_CHARS  char cap for search-result snippets (default: 2400,
                   ~512 tokens; the agent can call get_document for full text)
"""

import glob
import json
import os
import pickle
import sys
import time

import numpy as np

# --- query instruction prefix --------------------------------------------------
# Verbatim from BrowseComp-Plus searcher/searchers/faiss_searcher.py: the
# query-side instruction Qwen3-Embedding expects. The corpus was encoded with an
# empty passage prefix, so only the query carries an instruction.
TASK_PREFIX = (
    "Instruct: Given a web search query, retrieve relevant passages "
    "that answer the query\nQuery:"
)

EMBED_BASE_URL = os.getenv("EMBED_BASE_URL", "").rstrip("/")
EMBED_API_KEY = os.getenv("EMBED_API_KEY", "")
EMBED_MODEL = os.getenv("EMBED_MODEL", "Qwen/Qwen3-Embedding-8B")
INDEX_GLOB = os.getenv("INDEX_GLOB", "indexes/qwen3-embedding-8b/corpus.shard*.pkl")
CORPUS_GLOB = os.getenv("CORPUS_GLOB", "corpus/data/*.parquet")
RETRIEVER_PORT = int(os.getenv("RETRIEVER_PORT", "8765"))
SNIPPET_MAX_CHARS = int(os.getenv("SNIPPET_MAX_CHARS", "2400"))


def log(*a):
    print(f"[retriever {time.strftime('%H:%M:%S')}]", *a, flush=True)


# --- corpus vectors -> FAISS index ---------------------------------------------
def load_index():
    import faiss

    files = sorted(glob.glob(INDEX_GLOB))
    if not files:
        sys.exit(f"No index shards match {INDEX_GLOB!r}")
    log(f"loading {len(files)} index shard(s)")
    index = None
    lookup = []
    for fp in files:
        with open(fp, "rb") as f:
            reps, shard_lookup = pickle.load(f)
        reps = np.asarray(reps, dtype=np.float32)
        if index is None:
            index = faiss.IndexFlatIP(reps.shape[1])
        index.add(reps)
        lookup.extend(shard_lookup)
        log(f"  {os.path.basename(fp)}: +{reps.shape[0]} vecs (dim={reps.shape[1]})")
    log(f"index ready: {index.ntotal} vectors, dim={index.d}")
    return index, lookup


# --- corpus text (docid -> full text) ------------------------------------------
def load_corpus_text():
    files = sorted(glob.glob(CORPUS_GLOB))
    if not files:
        sys.exit(f"No corpus parquet files match {CORPUS_GLOB!r}")
    log(f"loading corpus text from {len(files)} parquet file(s)")
    import pyarrow.parquet as pq

    docid_to_text = {}
    for fp in files:
        table = pq.read_table(fp, columns=["docid", "text"])
        for docid, text in zip(
            table.column("docid").to_pylist(), table.column("text").to_pylist()
        ):
            docid_to_text[str(docid)] = text or ""
    log(f"corpus text ready: {len(docid_to_text)} documents")
    return docid_to_text


# --- query embedding via OpenAI-compatible API ---------------------------------
def make_embedder():
    if not EMBED_BASE_URL or not EMBED_API_KEY:
        sys.exit("EMBED_BASE_URL and EMBED_API_KEY must be set")
    from openai import OpenAI

    client = OpenAI(base_url=EMBED_BASE_URL, api_key=EMBED_API_KEY)

    def embed(query: str) -> np.ndarray:
        resp = client.embeddings.create(model=EMBED_MODEL, input=TASK_PREFIX + query)
        vec = np.asarray(resp.data[0].embedding, dtype=np.float32)
        norm = np.linalg.norm(vec)  # corpus vectors were L2-normalized
        if norm > 0:
            vec = vec / norm
        return vec

    return embed


def main():
    index, lookup = load_index()
    docid_to_text = load_corpus_text()
    embed = make_embedder()

    from flask import Flask, jsonify, request

    app = Flask(__name__)

    @app.get("/health")
    def health():
        return jsonify(status="ok", num_docs=index.ntotal, dim=index.d)

    @app.post("/search")
    def search():
        body = request.get_json(force=True)
        query = body["query"]
        k = int(body.get("k", 5))
        q = embed(query).reshape(1, -1)
        scores, idxs = index.search(q, k)
        hits = []
        for score, idx in zip(scores[0].tolist(), idxs[0].tolist()):
            if idx < 0:
                continue
            docid = str(lookup[idx])
            text = docid_to_text.get(docid, "")
            snippet = text[:SNIPPET_MAX_CHARS]
            hits.append({"docid": docid, "score": float(score), "snippet": snippet})
        return jsonify(hits)

    @app.post("/get_document")
    def get_document():
        body = request.get_json(force=True)
        docid = str(body["docid"])
        text = docid_to_text.get(docid)
        if text is None:
            return jsonify(error=f"docid {docid} not found"), 404
        return jsonify(docid=docid, text=text)

    log(f"serving on http://127.0.0.1:{RETRIEVER_PORT}  (model={EMBED_MODEL})")
    app.run(host="127.0.0.1", port=RETRIEVER_PORT, threaded=True)


if __name__ == "__main__":
    main()
