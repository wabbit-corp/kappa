#!/usr/bin/env python3
"""Run the end-to-end Kappa fuzzball pipeline with sensible defaults."""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import sys
from datetime import datetime
from pathlib import Path

from kappa_fuzz_lib import current_git_commit, repo_root_from_script


def resolve_path(repo_root: Path, value: str) -> Path:
    path = Path(value)
    return path if path.is_absolute() else repo_root / path


def preferred_python(repo_root: Path) -> str:
    venv_python = repo_root / "artifacts/fuzzball-kappa/.venv/bin/python"
    if venv_python.exists():
        return str(venv_python)

    return sys.executable


def run_command(command: list[str], *, cwd: Path) -> None:
    print("$", " ".join(command))
    subprocess.run(command, cwd=cwd, check=True)


def json_command(command: list[str], *, cwd: Path) -> dict:
    print("$", " ".join(command))
    result = subprocess.run(command, cwd=cwd, capture_output=True, text=True, check=True)
    return json.loads(result.stdout)


def cleanup_artifacts(repo_root: Path, *, keep_model_dir: Path, keep_run_dir: Path | None) -> dict:
    artifacts_root = repo_root / "artifacts"
    removed: list[str] = []

    for path in sorted(artifacts_root.iterdir()) if artifacts_root.exists() else []:
        if path == keep_model_dir:
            continue

        if path.name == "fuzzball-kappa":
            for child in sorted(path.iterdir()):
                if child.name in {".venv", "corpus.sqlite", "corpus.jsonl", "weighted-training-samples.jsonl"}:
                    continue

                if child.is_dir():
                    shutil.rmtree(child)
                    removed.append(str(child))
                elif child.is_file() and child.name not in {"corpus.sqlite", "corpus.jsonl", "weighted-training-samples.jsonl"}:
                    child.unlink()
                    removed.append(str(child))

            continue

        if path.is_dir() and path.name.startswith("fuzzball-kappa"):
            shutil.rmtree(path)
            removed.append(str(path))

    if keep_model_dir.exists():
        for child in sorted(keep_model_dir.iterdir()):
            if child == keep_run_dir:
                continue

            if child.name in {
                "kappa-char-lstm.pt",
                "metadata.json",
                "vocab.json",
                "keyword_codes.json",
                "samples.kp.txt",
                "samples.encoded.txt",
                "corpus.txt",
            }:
                continue

            if child.is_dir():
                shutil.rmtree(child)
                removed.append(str(child))

    return {"removed_count": len(removed), "removed": removed}


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--count", type=int, default=2000)
    parser.add_argument("--steps", type=int, default=500)
    parser.add_argument("--temperature", type=float, default=0.55)
    parser.add_argument("--timeout-seconds", type=float, default=3.0)
    parser.add_argument("--sample-length", type=int, default=1000)
    parser.add_argument("--cli", default="src/Kappa.Compiler.Cli/bin/Debug/net10.0/Kappa.Compiler.Cli")
    parser.add_argument("--corpus-db", default="artifacts/fuzzball-kappa/corpus.sqlite")
    parser.add_argument("--model-dir", default="artifacts/fuzzball-kappa-weighted-current")
    parser.add_argument("--skip-build", action="store_true")
    args = parser.parse_args()

    repo_root = repo_root_from_script()
    cli_path = resolve_path(repo_root, args.cli)
    model_dir = resolve_path(repo_root, args.model_dir)
    corpus_db = resolve_path(repo_root, args.corpus_db)
    corpus_jsonl = corpus_db.with_suffix(".jsonl")
    weighted_jsonl = corpus_db.parent / "weighted-training-samples.jsonl"
    python_bin = preferred_python(repo_root)
    compiler_commit = current_git_commit(repo_root)
    timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
    run_dir = model_dir / f"fuzz-run-{timestamp}-{compiler_commit[:8]}"
    retest_dir = model_dir / f"retest-pending-{timestamp}-{compiler_commit[:8]}"

    model_dir.mkdir(parents=True, exist_ok=True)

    if not args.skip_build:
        run_command(
            ["dotnet", "build", "src/Kappa.Compiler.Cli/Kappa.Compiler.Cli.fsproj", "-v", "q"],
            cwd=repo_root,
        )

    if (repo_root / "pending-failures").exists():
        run_command(
            [
                python_bin,
                "scripts/retest_kappa_cases.py",
                "pending-failures",
                "--cli",
                str(cli_path),
                "--out-dir",
                str(retest_dir),
                "--timeout-seconds",
                str(args.timeout_seconds),
            ],
            cwd=repo_root,
        )

    corpus_before = json_command(
        [
            python_bin,
            "scripts/update_kappa_corpus_store.py",
            "--db",
            str(corpus_db),
            "--jsonl",
            str(corpus_jsonl),
        ],
        cwd=repo_root,
    )

    weighting = json_command(
        [
            python_bin,
            "scripts/export_weighted_training_samples.py",
            "--db",
            str(corpus_db),
            "--out",
            str(weighted_jsonl),
            "--preferred-commit",
            compiler_commit,
        ],
        cwd=repo_root,
    )

    run_command(
        [
            python_bin,
            "scripts/train_kappa_fuzzball.py",
            "--weighted-samples",
            str(weighted_jsonl),
            "--out-dir",
            str(model_dir),
            "--steps",
            str(args.steps),
        ],
        cwd=repo_root,
    )

    run_command(
        [
            python_bin,
            "scripts/fuzz_kappa_from_model.py",
            "--checkpoint",
            str(model_dir / "kappa-char-lstm.pt"),
            "--cli",
            str(cli_path),
            "--out-dir",
            str(run_dir),
            "--count",
            str(args.count),
            "--timeout-seconds",
            str(args.timeout_seconds),
            "--temperature",
            str(args.temperature),
            "--sample-length",
            str(args.sample_length),
            "--keep-diagnostics",
            "10",
            "--keep-successes",
            "5",
        ],
        cwd=repo_root,
    )

    promotion = json_command(
        [
            python_bin,
            "scripts/promote_pending_failures.py",
            str(run_dir),
            "--cli",
            str(cli_path),
            "--timeout-seconds",
            str(args.timeout_seconds),
        ],
        cwd=repo_root,
    )

    corpus_after = json_command(
        [
            python_bin,
            "scripts/update_kappa_corpus_store.py",
            "--db",
            str(corpus_db),
            "--jsonl",
            str(corpus_jsonl),
        ],
        cwd=repo_root,
    )

    cleanup = cleanup_artifacts(repo_root, keep_model_dir=model_dir, keep_run_dir=run_dir)

    summary = {
        "compiler_commit": compiler_commit,
        "cli": str(cli_path),
        "python": python_bin,
        "model_dir": str(model_dir),
        "run_dir": str(run_dir),
        "retest_dir": str(retest_dir),
        "corpus_before": corpus_before,
        "weighting": weighting,
        "promotion": promotion,
        "corpus_after": corpus_after,
        "cleanup": cleanup,
    }
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
