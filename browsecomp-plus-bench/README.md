# BrowseComp-Plus with the Lacuna `agent` primitive

This directory runs the [BrowseComp-Plus](https://github.com/texttron/BrowseComp-Plus)
deep-research benchmark with the Scala 3 `agent[T](task)` primitive — the
experiment behind section 5.2 of the *Compiler in the Loop* paper.

The agent is a recursive `agent[String]` call (a ReAct loop); corpus
`search` / `getDocument` are ordinary in-scope Scala functions backed by the
canonical BrowseComp-Plus 8B-embedding corpus index.

## What's a task

Each of the 830 queries is a hard, multi-clue identification question:
several independent clues that only one entity in the corpus satisfies. One
real example:

> **q769** — "Please tell me the name of the learning institution that fits
> the following criteria: **A.** In 2002, it held a three-day event from
> Thursday to Saturday … showing support to a group of people. **B.** In 2003,
> it held its graduation ceremony on the fourth Sunday of a particular month.
> **C.** In 2022, an article was published on this institution's website about
> a trip to gather plant samples. **D.** Seven days after that article, an
> academic division organized a ceremony to pay tribute to the management of a
> bank … **E.** The country's capital city as of 2023 is where the institution
> is situated."  →  **`Queen Arwa University`**

Some are simpler (q773: identify a missing teen and report the shirt colour
→ `Red`); all share the shape — **one question, one short ground-truth string**
(a name, a date, a colour, a number, a title).

The corpus is a fixed ~100k-document set
([HF](https://huggingface.co/datasets/Tevatron/browsecomp-plus-corpus));
each query is annotated with `evidence_docs` (documents that actually contain
the supporting facts) and lots of hard negatives (distractors mined to keep
retrieval non-trivial). Everyone searches the same corpus, so the metric
captures reasoning and tool use, not live-web drift.

The agent has two tools — `search(query)` returns the top-5 corpus hits with
snippets; `getDocument(docid)` returns one full document — and must
(1) decompose the question into per-clue sub-searches, (2) cross-reference the
docids that recur across clues, (3) read full documents on the most promising
hits, (4) iterate or reformulate when a search returns nothing, and finally
(5) commit to a single short answer in the format:

```
Explanation: <reasoning, with inline [docid] citations>
Exact Answer: <short noun phrase / name / number>
Confidence: <integer 0-100>%
```

The LLM judge (`evaluate.py`) regex-extracts `Exact Answer:` and decides
`correct: yes/no` by string-equivalence to the ground truth. From that we get
**Accuracy**, **Retrieval Recall** (`|retrieved ∩ evidence| / |evidence|`),
**Calibration Error**, and **Citation Precision/Recall**.

## Architecture

```
queries_slim.jsonl   (830 decrypted BrowseComp-Plus queries)
         │
         ▼
┌─────────────────────────────────────────────────────────────────────┐
│  scala-cli repl    Agent.scala  +  Search.scala                     │
├─────────────────────────────────────────────────────────────────────┤
│  Bench.scala :load   ─►   eval-rewriter fires on agent[String]      │
│                                                                     │
│    runQuery(qid, question)                                          │
│      └─ agent[String](task)        ◄── recursive ReAct loop         │
│           ├─ search(query)                                          │
│           └─ getDocument(docid)                                     │
└──────────────────────────────────┬──────────────────────────────────┘
                                   │  HTTP / JSON
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│  retriever_server.py      http://127.0.0.1:8765                     │
├─────────────────────────────────────────────────────────────────────┤
│  POST /search           embed query  ─►  OpenRouter Qwen3-Embed-8B  │
│                         FAISS flat IP ─►  8B corpus index           │
│  POST /get_document     docid lookup  ─►  corpus parquet (~100k)    │
└──────────────────────────────────┬──────────────────────────────────┘
                                   │
                                   ▼
runs/<run-id>/<qid>.json           tool trace, timing, tokens, answer
runs/<run-id>/trace/<qid>/         progress log + generated-code rounds
         │
         ▼
evaluate.py   ─►   gpt-4.1 judge   ─►   Accuracy · Recall · Calibration
```

The retriever keeps BrowseComp-Plus's canonical 8B corpus vectors but embeds
the *query* through a hosted API (OpenRouter `Qwen/Qwen3-Embedding-8B`) instead
of a local GPU model. It is exposed as plain HTTP (not MCP) so the Scala REPL
can call it with `java.net.http`.

## Layout

| Path | What |
|------|------|
| `BrowseComp-Plus/` | upstream repo (clone) |
| `pyproject.toml`, `.python-version`, `uv.lock` | uv project config: pins Python 3.12 + all deps; `uv sync` provisions `.venv` |
| `.venv/` | uv-managed env (created by `uv sync`) — faiss-cpu, pyarrow, openai, flask, tqdm |
| `data/queries_slim.jsonl` | 830 decrypted queries (id, query, answer, qrels) |
| `indexes/`, `corpus/` | 8B embedding index + corpus text |
| `retriever_server.py` | retrieval HTTP shim |
| `check_recall.py` | embedding-compatibility sanity check |
| `Agent.scala` | bench copy of the `agent` primitive — keys from env, per-run log dir, `LLMUsage` + `AgentStats` accounting, REPL history off |
| `Search.scala` | `search`/`getDocument` tools + per-query trace + run-file writer |
| `Bench.scala` | `runQuery`/`runBench` driver (`:load`ed into the REPL) |
| `run_bench.sh` | one-shot runner (`N` queries from `START`) |
| `run_parallel.sh` | sharded runner across `SHARDS` parallel REPL processes |
| `evaluate.py` | LLM-judge evaluation (chat-completions, any provider) |
| `runs/<run-id>/*.json` | per-query run files (tool trace, timing, token usage); `<run-id>` is `<timestamp>_<model>` per run |
| `runs/<run-id>/trace/<qid>/` | captured agent stdout + generated-code rounds |
| `evals/` | evaluation output (`evaluation_summary.json`, `detailed.csv`, per-query judge files) |
| `log/run-*/` | per-run REPL eval-log directory (one per `run_bench.sh` invocation) |

## Setup (one-time)

```sh
# Python env — pyproject.toml + .python-version pin Python 3.12 and every
# dependency, so one command creates .venv and installs everything (uv
# auto-fetches a managed CPython 3.12 if the host lacks one). The `setup` group
# adds the one-time data-prep tools (datasets, huggingface_hub) the decrypt +
# download steps below need; drop `--group setup` once the data is in place.
uv sync --group setup
source .venv/bin/activate          # `source env.sh` re-activates it in later shells

# Decrypt the 830 queries (BrowseComp-Plus ships them encrypted) and the slim file
python BrowseComp-Plus/scripts_build_index/decrypt_dataset.py \
  --output data/browsecomp_plus_decrypted.jsonl --generate-tsv data/queries.tsv
# (queries_slim.jsonl is then built once — see git history for the one-liner)

# Download the 8B embedding index + canonical corpus (~3 GB)
python -c "from huggingface_hub import snapshot_download as d; \
  d('Tevatron/browsecomp-plus-indexes', repo_type='dataset', \
    allow_patterns=['qwen3-embedding-8b/*'], local_dir='indexes'); \
  d('Tevatron/browsecomp-plus-corpus',  repo_type='dataset', \
    allow_patterns=['data/*.parquet'], local_dir='corpus')"

cp env.example.sh env.sh           # fill in EMBED_API_KEY, AGENT_API_KEY, ...
```

## Run

```sh
source env.sh

# 1. Retriever shim (loads index+corpus ~10s; leave running)
python retriever_server.py &

# 2. Verify the API embedding matches the index checkpoint. Run once after a
#    provider or model change. Near-zero recall = incompatible.
RECALL_SAMPLE=30 python check_recall.py

# 3. Run the agent. Each run writes to runs/<timestamp>_<AGENT_MODEL>/ — so a
#    second run (e.g. a different AGENT_MODEL) lands in its own directory and
#    never overwrites the first. The scripts print the BENCH_RUN_ID they chose;
#    set BENCH_RUN_ID=my-label yourself to override it.
./run_bench.sh 25             # 25 queries from offset 0, single-threaded
./run_bench.sh 13 0           # one shard: 13 queries starting at offset 0
./run_parallel.sh 100 8       # 100 queries across 8 parallel REPL processes
./run_parallel.sh 830 8       # full benchmark (~75 min wall-clock at 8 shards)

# 4. Judge the answers — point --input_dir at the run you want (see the path
#    each run printed). Results go to evals/<run-id>/, kept separate per run.
python evaluate.py --input_dir runs/20260521-004530_deepseek-v4-flash
```

## What's recorded per query

Each query writes one JSON in `runs/<run-id>/<qid>.json` and one folder in
`runs/<run-id>/trace/<qid>/`, where `<run-id>` is the `BENCH_RUN_ID` of the run
(`<timestamp>_<model>` by default; `lacuna` for an ad-hoc REPL session that
doesn't set it). All shards of a parallel run share one `<run-id>`.

`<qid>.json` (BrowseComp-Plus evaluator schema, enriched):

- `metadata.model`, `started_at`, `ended_at`, `duration_sec`, `eval_rounds`
- `metadata.agent_stats`: `agent_calls`, `attempts`, `retries`, `compiles_ok`,
  `compiles_failed`
- `metadata.llm_usage`: `calls`, `prompt_tokens`, `completion_tokens`,
  `total_tokens`
- `query_id`, `question`, `status`, `retrieved_docids`, `tool_call_counts`
- `result[]`: interleaved `tool_call` / `tool_result` entries for every
  `search` and `getDocument`, ending with `{type:"output_text", output: …}`

`trace/<qid>/`:

- `progress.log` — captured `[agent] …` narration (Scala `Console.out` is tee'd)
- `eval_*_code.scala` — every LLM-generated code snippet for this query
- `eval_*_wrapper.scala` — REPL eval wrapper that ran each snippet
- `eval_*_enclosingSource.scala` — source context the rewriter showed the LLM
- `eval_*_error.scala` — compile errors for any failed attempt

## Notes

### Why `:load`, not a `repl` file arg
The `agent[String](…)` call site must be compiled through the REPL's
eval-rewrite phase. Files passed to `scala-cli repl` are compiled with the
ordinary pipeline, so `Bench.scala` is `:load`ed instead; `Agent.scala` +
`Search.scala` (the primitive and the plain tools, no `agent` call sites) are
normal file arguments.

### Protocol-faithful retrieval
BrowseComp-Plus deliberately fixes the retrieval budget — its `search(query)`
tool returns top-5 with no `k` parameter, "across all methods", so agents are
compared on reasoning and tool use rather than retrieval depth. We match this:
`search(query: String)` has no `k` parameter; `k` is hardwired to 5. An ablation
escape hatch exists via the `SEARCH_K` env var, but the default and the §5.2
numbers use k=5.

### Sharding
`runBench(n, start)` takes an offset; `run_bench.sh N START` exposes it, and
`run_parallel.sh N SHARDS` launches several REPL processes over disjoint
slices. `run_parallel.sh` computes one `BENCH_RUN_ID` and exports it to every
shard, so they all write into the **same** `runs/<run-id>/` (disjoint qids);
each shard still gets its **own** `EVAL_LOG_DIR` (`log/run-<run-id>-s<start>/`),
so generated-code logs never collide. The retriever is `threaded=True` and
serves the concurrent load comfortably. Under 8-way contention, per-query
latency is ~5 min (vs ~2.6 min single-threaded), so the full 830 finishes in
~75 min.

### Retriever fidelity
Corpus vectors are the published `qwen3-embedding-8b` index unchanged; only
query embedding is swapped to the OpenRouter API. `check_recall.py` confirms
compatibility — a mismatch (wrong checkpoint, wrong pooling, wrong
normalization) collapses recall to near zero. The query-side instruction
prefix matches `searcher/searchers/faiss_searcher.py` exactly.

### Judge
`evaluate.py` grades via chat-completions and reuses the OpenRouter key
(`EMBED_API_KEY`) by default. The upstream `evaluate_with_openai.py` needs
OpenAI's Responses API, which not all OpenAI-compatible providers offer.

### Snippets
`search` results are character-capped (`SNIPPET_MAX_CHARS`, ~512 tokens, the
benchmark standard); the upstream MCP server caps by Qwen3 tokens. The agent
can always call `getDocument` for full text.

### REPL history is off
The bench `Agent.scala` disables `-Xrepl-history-file` reading by default
(`AGENT_REPL_HISTORY=on` to re-enable). Each BrowseComp-Plus query must be
answered independently, with no leakage from prior queries' transcripts.

## Status

Latest measurement (protocol-faithful, k=5), full 830-query run:

| | n | Accuracy | Recall | Calibration | Cite P / R | avg search | avg get_document |
|---|---:|---:|---:|---:|---:|---:|---:|
| `deepseek-v4-flash (high)` + 8B retriever + `gpt-4.1` judge | **824** | **27.1%** | **34.5%** | 37.5% | 24.7% / 10.6% | 15.5 | 23.2 |
| `gemini-3.1-flash-lite (high)` + 8B retriever + `gpt-4.1` judge | **829** | **26.2%** | **27.9%** | 48.1% | 32.1% / 12.9% | 10.7 | 8.2 |
| `gpt-5.4-mini (low)` + 8B retriever + `gpt-4.1` judge | **830** | **9.2%** | **16.2%** | 28.1% | 11.9% / 5.6% | 4.6 | 3.6 |
| _earlier 100-query sample (deepseek)_ | 100 | 24.0% | 26.7% | 26.6% | — | 20.5 | 43.8 |

Full run `full830-20260522-002949_deepseek-v4-flash`, launched with
`./run_parallel.sh 830 16` and judged with `evaluate.py --workers 16`. Six
queries (offsets 165, 177, 192, 356, 633, 766) never converged within the
per-query watchdog — they timed out at both 600s under 16-way contention and
900s under a 6-way re-run — so they have no answer file and are excluded
(n=824); counting them as wrong puts accuracy at ~26.9% over all 830.

Second full run `full830-20260522-181425_google-gemini-3.1-flash-lite`
(`AGENT_MODEL=google/gemini-3.1-flash-lite` via OpenRouter, same launch and
judge). One query (offset 264) never converged at 600s or in the 900s re-run,
so n=829; counting it as wrong puts accuracy at ~26.1% over all 830.
`gemini-3.1-flash-lite` matches `deepseek-v4-flash` on accuracy (26.2% vs
27.1%) despite far fewer tool calls (10.7 vs 15.5 searches, 8.2 vs 23.2
document reads) and a cheaper retry footprint (0.4 vs 0.7 retries/query); it
retrieves less (recall 27.9%) and answers from a more selective evidence set
(higher citation precision, 32.1%), but is more overconfident (calibration
error 48.1%). An abandoned `openai/gpt-oss-120b` run trailed both (~11%
accuracy at 167/830) and was stopped early.

Third full run `full830-20260522-225501_openai-gpt-5.4-mini-low`
(`AGENT_MODEL=openai/gpt-5.4-mini`, `AGENT_EFFORT=low` via OpenRouter) — the
first run to complete all 830 with no exclusions (0 watchdog kills); the 10
offsets still open when the launcher died were recovered with the new
`rerun_missing.sh` (fills gaps from an interrupted run, vs `rerun_killed.sh` for
watchdog timeouts). gpt-5.4-mini is acutely effort-sensitive here: at **low**
effort it barely explores (1.6 rounds, 4.6 searches/query), so recall and
accuracy collapse to 16.2% / 9.2%; a 50-query **medium**-effort pilot searched
~6× more (28 searches, 55 doc reads) for 35.3% recall / 17.8% accuracy but at
~224s/query; **high** effort timed out ~58% of queries at the 600s cap. It also
had the highest compile-reject rate of any model (22% at low effort), all caught
by the verifier and fixed in 0.5 retries/query.

Both accuracy and recall came in higher than the 100-query sample suggested,
while calibration error is worse (the model is more overconfident across the
full set). Retrieval recall 34.5% still caps accuracy hard regardless of
synthesis: the main lever now is a stronger agent model (try the EPFL GLM
endpoint via `AGENT_MODEL`).
