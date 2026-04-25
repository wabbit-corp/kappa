#!/usr/bin/env python3
"""Export weighted training samples from the corpus SQLite store."""

from __future__ import annotations

import argparse
import json
import math
import sqlite3
import subprocess
from collections import Counter, defaultdict
from pathlib import Path


RESULT_SCORES = {
    "crash": 6.0,
    "failure": 4.0,
    "ok": 1.75,
    "timeout": 1.0,
    "diagnostic": 0.35,
}


def repo_root_from_script() -> Path:
    return Path(__file__).resolve().parents[1]


def current_git_commit(repo_root: Path) -> str:
    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            cwd=repo_root,
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout.strip()
    except subprocess.SubprocessError:
        return "unknown"


def load_rows(connection: sqlite3.Connection, query: str) -> list[sqlite3.Row]:
    connection.row_factory = sqlite3.Row
    return list(connection.execute(query))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--db", default="artifacts/fuzzball-kappa/corpus.sqlite")
    parser.add_argument("--out", default="artifacts/fuzzball-kappa/weighted-training-samples.jsonl")
    parser.add_argument("--min-weight", type=float, default=0.5)
    parser.add_argument("--max-weight", type=float, default=8.0)
    parser.add_argument("--preferred-commit", default=None)
    args = parser.parse_args()

    repo_root = repo_root_from_script()
    db_path = Path(args.db)
    out_path = Path(args.out)

    if not db_path.is_absolute():
        db_path = repo_root / db_path

    if not out_path.is_absolute():
        out_path = repo_root / out_path

    preferred_commit = args.preferred_commit or current_git_commit(repo_root)

    connection = sqlite3.connect(db_path)
    connection.row_factory = sqlite3.Row

    try:
        sample_rows = load_rows(connection, "SELECT sample_sha1, text FROM samples ORDER BY sample_sha1")
        provenance_rows = load_rows(
            connection,
            """
            SELECT sample_sha1, provenance_kind, source_group, source_path, used_for_training, metadata_json
            FROM provenance
            ORDER BY sample_sha1, source_group, source_path
            """,
        )
        result_rows = load_rows(
            connection,
            """
            SELECT sample_sha1, compiler_commit, result_kind, terminal_signature, meta_json
            FROM test_results
            ORDER BY sample_sha1, compiler_commit, result_kind
            """,
        )
    finally:
        connection.close()

    provenance_by_sample: dict[str, list[dict]] = defaultdict(list)
    trace_hash_frequency: Counter[str] = Counter()

    for row in provenance_rows:
        metadata = json.loads(row["metadata_json"])
        record = {
            "kind": row["provenance_kind"],
            "source_group": row["source_group"],
            "source_path": row["source_path"],
            "used_for_training": bool(row["used_for_training"]),
            "metadata": metadata,
        }
        provenance_by_sample[row["sample_sha1"]].append(record)

        trace_hash = metadata.get("trace_hash")
        if isinstance(trace_hash, str) and trace_hash:
            trace_hash_frequency[trace_hash] += 1

    results_by_sample: dict[str, list[dict]] = defaultdict(list)
    terminal_frequency: Counter[str] = Counter()

    for row in result_rows:
        metadata = json.loads(row["meta_json"])
        record = {
            "compiler_commit": row["compiler_commit"],
            "result_kind": row["result_kind"],
            "terminal_signature": row["terminal_signature"],
            "metadata": metadata,
        }
        results_by_sample[row["sample_sha1"]].append(record)
        terminal_frequency[row["terminal_signature"]] += 1

    out_path.parent.mkdir(parents=True, exist_ok=True)
    exported = 0
    top_weights: list[tuple[float, str]] = []

    with out_path.open("w", encoding="utf-8") as handle:
        for row in sample_rows:
            sample_sha1 = row["sample_sha1"]
            text = row["text"]
            provenance = provenance_by_sample[sample_sha1]
            test_results = results_by_sample[sample_sha1]

            has_static_training = any(entry["used_for_training"] for entry in provenance)
            has_pending_failure = any(entry["kind"] == "pending-failure" for entry in provenance)
            has_test_results = bool(test_results)

            if not (has_static_training or has_pending_failure or has_test_results):
                continue

            weight = 0.0

            if has_static_training:
                weight += 1.0

            preferred_results = [result for result in test_results if result["compiler_commit"] == preferred_commit]
            historical_results = [result for result in test_results if result["compiler_commit"] != preferred_commit]
            effective_results = preferred_results if preferred_results else historical_results
            history_factor = 1.0 if preferred_results else 0.35

            if has_pending_failure:
                weight += 0.3 if preferred_results else 1.2

            best_depth_score = 0.0
            best_terminal_rarity = 1.0

            for result in effective_results:
                depth_score = RESULT_SCORES.get(result["result_kind"], 0.25)
                terminal_count = terminal_frequency[result["terminal_signature"]]
                terminal_rarity = 1.0 / math.sqrt(max(1, terminal_count))

                if depth_score * terminal_rarity > best_depth_score * best_terminal_rarity:
                    best_depth_score = depth_score
                    best_terminal_rarity = terminal_rarity

            weight += best_depth_score * best_terminal_rarity * history_factor

            trace_boost = 0.0
            for entry in provenance:
                trace_hash = entry["metadata"].get("trace_hash")
                if isinstance(trace_hash, str) and trace_hash:
                    trace_boost = max(trace_boost, 1.0 / math.sqrt(max(1, trace_hash_frequency[trace_hash])))

            weight += trace_boost * 2.0

            weight = max(args.min_weight, min(args.max_weight, weight))

            record = {
                "sample_sha1": sample_sha1,
                "weight": round(weight, 4),
                "text": text,
                "has_static_training": has_static_training,
                "has_pending_failure": has_pending_failure,
                "test_result_kinds": sorted({result["result_kind"] for result in test_results}),
                "provenance_count": len(provenance),
                "test_result_count": len(test_results),
            }
            handle.write(json.dumps(record, sort_keys=True) + "\n")
            exported += 1
            top_weights.append((weight, sample_sha1))

    top_weights.sort(reverse=True)

    summary = {
        "db": str(db_path),
        "out": str(out_path),
        "preferred_commit": preferred_commit,
        "exported": exported,
        "top_weighted_samples": [
            {"sample_sha1": sample_sha1, "weight": round(weight, 4)}
            for weight, sample_sha1 in top_weights[:10]
        ],
    }
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
