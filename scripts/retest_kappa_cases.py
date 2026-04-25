#!/usr/bin/env python3
"""Retest saved Kappa case directories against the current compiler build."""

from __future__ import annotations

import argparse
import hashlib
import json
import time
from pathlib import Path

from kappa_fuzz_lib import (
    bucket_for_kind,
    current_git_commit,
    discover_case_dirs,
    repo_root_from_script,
    run_cli_source,
)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("roots", nargs="+")
    parser.add_argument("--cli", default="src/Kappa.Compiler.Cli/bin/Debug/net10.0/Kappa.Compiler.Cli")
    parser.add_argument("--out-dir", default="artifacts/fuzzball-kappa/retest-run")
    parser.add_argument("--timeout-seconds", type=float, default=5.0)
    parser.add_argument("--verify-clean-checkpoint", action="append", default=["KBackendIR"])
    args = parser.parse_args()

    repo_root = repo_root_from_script()
    cli_path = Path(args.cli)
    out_dir = Path(args.out_dir)

    if not cli_path.is_absolute():
        cli_path = repo_root / cli_path

    if not out_dir.is_absolute():
        out_dir = repo_root / out_dir

    for name in ["crashes", "timeouts", "diagnostics", "successes", "failures"]:
        (out_dir / name).mkdir(parents=True, exist_ok=True)

    compiler_commit = current_git_commit(repo_root)
    case_dirs = discover_case_dirs(repo_root, args.roots)

    (out_dir / "run.json").write_text(
        json.dumps(
            {
                "compiler_commit": compiler_commit,
                "cli": str(cli_path),
                "timeout_seconds": args.timeout_seconds,
                "case_count": len(case_dirs),
                "verify_clean_checkpoint": args.verify_clean_checkpoint,
            },
            indent=2,
        ),
        encoding="utf-8",
    )

    stats = {"ok": 0, "diagnostic": 0, "crash": 0, "timeout": 0, "failure": 0}

    for index, case_dir in enumerate(case_dirs, start=1):
        source = (case_dir / "main.kp").read_text(encoding="utf-8", errors="replace")
        started_at = time.time()
        stage = "compile"
        run = run_cli_source(cli_path, repo_root, source, stage=stage, timeout_seconds=args.timeout_seconds)
        kind = run.kind

        if kind == "ok":
            for checkpoint in args.verify_clean_checkpoint:
                verify_stage = f"verify:{checkpoint}"
                verify_run = run_cli_source(
                    cli_path,
                    repo_root,
                    source,
                    stage=verify_stage,
                    timeout_seconds=args.timeout_seconds,
                )
                verify_kind = verify_run.kind

                if verify_kind in {"crash", "timeout", "failure"}:
                    kind = verify_kind
                    run = verify_run
                    stage = verify_stage
                    break

        stats[kind] += 1
        elapsed_ms = int((time.time() - started_at) * 1000.0)
        digest = hashlib.sha1(source.encode("utf-8")).hexdigest()
        bucket = bucket_for_kind(kind)
        dest_dir = out_dir / bucket / f"{kind}-{digest}"
        dest_dir.mkdir(parents=True, exist_ok=True)
        (dest_dir / "main.kp").write_text(source.rstrip() + "\n", encoding="utf-8")
        (dest_dir / "stdout.txt").write_text(run.stdout, encoding="utf-8")
        (dest_dir / "stderr.txt").write_text(run.stderr, encoding="utf-8")
        (dest_dir / "meta.json").write_text(
            json.dumps(
                {
                    "kind": kind,
                    "sha1": digest,
                    "returncode": run.returncode,
                    "timed_out": run.timed_out,
                    "elapsed_ms": elapsed_ms,
                    "index": index,
                    "compiler_commit": compiler_commit,
                    "cli": str(cli_path),
                    "stage": stage,
                    "source_case_dir": str(case_dir),
                    "verify_clean_checkpoint": args.verify_clean_checkpoint,
                },
                indent=2,
            ),
            encoding="utf-8",
        )

    (out_dir / "summary.json").write_text(json.dumps(stats, indent=2), encoding="utf-8")
    print(json.dumps(stats, indent=2))


if __name__ == "__main__":
    main()
