#!/usr/bin/env python3
"""Populate a SQLite corpus store and export a JSONL view."""

from __future__ import annotations

import argparse
import json
import sqlite3
from collections import defaultdict
from pathlib import Path

from kappa_fuzz_lib import (
    extract_inline_kappa_samples_from_fs,
    load_json,
    repo_relative,
    repo_root_from_script,
    sample_sha1,
    terminal_signature_from_case,
)


def ensure_schema(connection: sqlite3.Connection) -> None:
    connection.executescript(
        """
        CREATE TABLE IF NOT EXISTS samples (
            sample_sha1 TEXT PRIMARY KEY,
            text TEXT NOT NULL
        );

        CREATE TABLE IF NOT EXISTS provenance (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            sample_sha1 TEXT NOT NULL,
            provenance_kind TEXT NOT NULL,
            source_group TEXT NOT NULL,
            source_path TEXT NOT NULL,
            used_for_training INTEGER NOT NULL DEFAULT 0,
            metadata_json TEXT NOT NULL,
            UNIQUE(sample_sha1, provenance_kind, source_path)
        );

        CREATE TABLE IF NOT EXISTS test_results (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            sample_sha1 TEXT NOT NULL,
            compiler_commit TEXT NOT NULL,
            result_kind TEXT NOT NULL,
            returncode INTEGER,
            timed_out INTEGER NOT NULL DEFAULT 0,
            terminal_signature TEXT NOT NULL,
            artifact_path TEXT NOT NULL,
            run_path TEXT NOT NULL,
            meta_json TEXT NOT NULL,
            UNIQUE(sample_sha1, compiler_commit, result_kind, artifact_path)
        );
        """
    )
    connection.commit()


def upsert_sample(connection: sqlite3.Connection, sha1: str, text: str) -> None:
    connection.execute(
        """
        INSERT INTO samples (sample_sha1, text)
        VALUES (?, ?)
        ON CONFLICT(sample_sha1) DO UPDATE SET text = excluded.text
        """,
        (sha1, text),
    )


def add_provenance(
    connection: sqlite3.Connection,
    *,
    sample_sha1_value: str,
    provenance_kind: str,
    source_group: str,
    source_path: str,
    used_for_training: bool,
    metadata: dict,
) -> None:
    connection.execute(
        """
        INSERT OR IGNORE INTO provenance (
            sample_sha1,
            provenance_kind,
            source_group,
            source_path,
            used_for_training,
            metadata_json
        ) VALUES (?, ?, ?, ?, ?, ?)
        """,
        (
            sample_sha1_value,
            provenance_kind,
            source_group,
            source_path,
            1 if used_for_training else 0,
            json.dumps(metadata, sort_keys=True),
        ),
    )


def add_test_result(
    connection: sqlite3.Connection,
    *,
    sample_sha1_value: str,
    compiler_commit: str,
    result_kind: str,
    returncode: int | None,
    timed_out: bool,
    terminal_signature: str,
    artifact_path: str,
    run_path: str,
    metadata: dict,
) -> None:
    connection.execute(
        """
        INSERT OR IGNORE INTO test_results (
            sample_sha1,
            compiler_commit,
            result_kind,
            returncode,
            timed_out,
            terminal_signature,
            artifact_path,
            run_path,
            meta_json
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """,
        (
            sample_sha1_value,
            compiler_commit,
            result_kind,
            returncode,
            1 if timed_out else 0,
            terminal_signature,
            artifact_path,
            run_path,
            json.dumps(metadata, sort_keys=True),
        ),
    )


def add_static_corpus_root(connection: sqlite3.Connection, repo_root: Path, root: Path, source_group: str) -> int:
    if not root.exists():
        return 0

    added = 0

    for path in sorted(root.rglob("*.kp")):
        text = path.read_text(encoding="utf-8", errors="replace")
        sha1 = sample_sha1(text)
        upsert_sample(connection, sha1, text)
        add_provenance(
            connection,
            sample_sha1_value=sha1,
            provenance_kind="corpus-file",
            source_group=source_group,
            source_path=repo_relative(repo_root, path),
            used_for_training=True,
            metadata={},
        )
        added += 1

    return added


def find_artifact_roots(repo_root: Path, name: str) -> list[Path]:
    artifacts_root = repo_root / "artifacts"

    if not artifacts_root.exists():
        return []

    return sorted(path for path in artifacts_root.rglob(name) if path.is_dir())


def scan_fuzz_run(connection: sqlite3.Connection, repo_root: Path, run_root: Path) -> int:
    run_metadata = load_json(run_root / "run.json")
    compiler_commit = str(run_metadata.get("compiler_commit", "unknown"))
    added = 0

    for bucket in ["failures", "crashes", "diagnostics", "successes", "timeouts"]:
        bucket_root = run_root / bucket

        if not bucket_root.exists():
            continue

        for case_dir in sorted(path for path in bucket_root.iterdir() if path.is_dir() and (path / "main.kp").exists()):
            text = (case_dir / "main.kp").read_text(encoding="utf-8", errors="replace")
            sha1 = sample_sha1(text)
            upsert_sample(connection, sha1, text)

            meta = load_json(case_dir / "meta.json")
            case_commit = str(meta.get("compiler_commit", compiler_commit))
            result_kind = str(meta.get("kind", bucket.rstrip("s")))

            add_provenance(
                connection,
                sample_sha1_value=sha1,
                provenance_kind="fuzz-artifact",
                source_group=bucket,
                source_path=repo_relative(repo_root, case_dir),
                used_for_training=False,
                metadata={
                    "run_root": repo_relative(repo_root, run_root),
                },
            )

            add_test_result(
                connection,
                sample_sha1_value=sha1,
                compiler_commit=case_commit,
                result_kind=result_kind,
                returncode=meta.get("returncode"),
                timed_out=bool(meta.get("timed_out", False)),
                terminal_signature=terminal_signature_from_case(case_dir),
                artifact_path=repo_relative(repo_root, case_dir),
                run_path=repo_relative(repo_root, run_root),
                metadata=meta,
            )
            added += 1

    return added


def add_pending_failures(connection: sqlite3.Connection, repo_root: Path, pending_root: Path) -> int:
    if not pending_root.exists():
        return 0

    added = 0

    for case_dir in sorted(path for path in pending_root.iterdir() if path.is_dir() and (path / "main.kp").exists()):
        text = (case_dir / "main.kp").read_text(encoding="utf-8", errors="replace")
        sha1 = sample_sha1(text)
        upsert_sample(connection, sha1, text)
        add_provenance(
            connection,
            sample_sha1_value=sha1,
            provenance_kind="pending-failure",
            source_group="pending-failures",
            source_path=repo_relative(repo_root, case_dir),
            used_for_training=False,
            metadata=load_json(case_dir / "cluster.json"),
        )
        added += 1

    return added


def add_inline_test_sources(connection: sqlite3.Connection, repo_root: Path, tests_root: Path) -> int:
    if not tests_root.exists():
        return 0

    added = 0

    for path in sorted(tests_root.rglob("*.fs")):
        for sample in extract_inline_kappa_samples_from_fs(path):
            sha1 = sample_sha1(sample.text)
            upsert_sample(connection, sha1, sample.text)
            add_provenance(
                connection,
                sample_sha1_value=sha1,
                provenance_kind="inline-test",
                source_group="inline-tests",
                source_path=f"{repo_relative(repo_root, path)}:{sample.line}",
                used_for_training=True,
                metadata={"source_label": sample.source_label},
            )
            added += 1

    return added


def export_jsonl(connection: sqlite3.Connection, jsonl_path: Path) -> dict:
    sample_rows = connection.execute(
        "SELECT sample_sha1, text FROM samples ORDER BY sample_sha1"
    ).fetchall()

    provenance_rows = connection.execute(
        """
        SELECT sample_sha1, provenance_kind, source_group, source_path, used_for_training, metadata_json
        FROM provenance
        ORDER BY sample_sha1, source_group, source_path
        """
    ).fetchall()

    result_rows = connection.execute(
        """
        SELECT sample_sha1, compiler_commit, result_kind, returncode, timed_out, terminal_signature, artifact_path, run_path, meta_json
        FROM test_results
        ORDER BY sample_sha1, compiler_commit, result_kind, artifact_path
        """
    ).fetchall()

    provenance_by_sample: dict[str, list[dict]] = defaultdict(list)
    for row in provenance_rows:
        provenance_by_sample[row[0]].append(
            {
                "kind": row[1],
                "source_group": row[2],
                "source_path": row[3],
                "used_for_training": bool(row[4]),
                "metadata": json.loads(row[5]),
            }
        )

    results_by_sample: dict[str, list[dict]] = defaultdict(list)
    for row in result_rows:
        results_by_sample[row[0]].append(
            {
                "compiler_commit": row[1],
                "result_kind": row[2],
                "returncode": row[3],
                "timed_out": bool(row[4]),
                "terminal_signature": row[5],
                "artifact_path": row[6],
                "run_path": row[7],
                "metadata": json.loads(row[8]),
            }
        )

    jsonl_path.parent.mkdir(parents=True, exist_ok=True)

    with jsonl_path.open("w", encoding="utf-8") as handle:
        for sha1, text in sample_rows:
            provenance = provenance_by_sample[sha1]
            results = results_by_sample[sha1]
            training_groups = sorted(
                {
                    entry["source_group"]
                    for entry in provenance
                    if entry["used_for_training"]
                }
            )

            record = {
                "sample_sha1": sha1,
                "text": text,
                "training_groups": training_groups,
                "provenance": provenance,
                "test_results": results,
            }
            handle.write(json.dumps(record, sort_keys=True) + "\n")

    return {
        "sample_count": len(sample_rows),
        "training_sample_count": sum(
            1 for sha1, _text in sample_rows if any(entry["used_for_training"] for entry in provenance_by_sample[sha1])
        ),
        "tested_sample_count": sum(1 for sha1, _text in sample_rows if results_by_sample[sha1]),
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--db", default="artifacts/fuzzball-kappa/corpus.sqlite")
    parser.add_argument("--jsonl", default="artifacts/fuzzball-kappa/corpus.jsonl")
    args = parser.parse_args()

    repo_root = repo_root_from_script()
    db_path = Path(args.db)
    jsonl_path = Path(args.jsonl)

    if not db_path.is_absolute():
        db_path = repo_root / db_path

    if not jsonl_path.is_absolute():
        jsonl_path = repo_root / jsonl_path

    db_path.parent.mkdir(parents=True, exist_ok=True)

    connection = sqlite3.connect(db_path)

    try:
        ensure_schema(connection)

        counts = {
            "fixtures": add_static_corpus_root(connection, repo_root, repo_root / "tests/Kappa.Compiler.Tests/Fixtures", "fixtures"),
            "new_tests": add_static_corpus_root(connection, repo_root, repo_root / "new-tests", "new-tests"),
            "inline_tests": add_inline_test_sources(connection, repo_root, repo_root / "tests"),
            "recycled_seeds": sum(
                add_static_corpus_root(connection, repo_root, root, "recycled-seeds")
                for root in find_artifact_roots(repo_root, "recycled-seeds")
            ),
            "crash_boosted_seeds": sum(
                add_static_corpus_root(connection, repo_root, root, "crash-boosted-seeds")
                for root in find_artifact_roots(repo_root, "crash-boosted-seeds")
            ),
            "pending_failures": add_pending_failures(connection, repo_root, repo_root / "pending-failures"),
            "fuzz_cases": sum(
                scan_fuzz_run(connection, repo_root, root)
                for root in sorted(path for path in (repo_root / "artifacts").rglob("fuzz-run*") if path.is_dir())
            ),
        }

        connection.commit()
        export_summary = export_jsonl(connection, jsonl_path)
    finally:
        connection.close()

    summary = {
        "db": str(db_path),
        "jsonl": str(jsonl_path),
        "counts": counts,
        "export": export_summary,
    }
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
