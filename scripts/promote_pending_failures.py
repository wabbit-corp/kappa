#!/usr/bin/env python3
"""Minimize fuzz failures, cluster them by trace hash, and promote unique cases."""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
from pathlib import Path

from kappa_fuzz_lib import repo_root_from_script


def has_case_dirs(root: Path) -> bool:
    return root.exists() and any(path.is_dir() and (path / "main.kp").exists() for path in root.iterdir())


def run_json_command(command: list[str], workdir: Path) -> dict:
    result = subprocess.run(command, cwd=workdir, capture_output=True, text=True, check=True)
    return json.loads(result.stdout)

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("roots", nargs="+", help="Fuzz-run roots to process.")
    parser.add_argument("--cli", default="src/Kappa.Compiler.Cli/bin/Debug/net10.0/Kappa.Compiler.Cli")
    parser.add_argument("--pending-dir", default="pending-failures")
    parser.add_argument("--timeout-seconds", type=float, default=5.0)
    args = parser.parse_args()

    repo_root = repo_root_from_script()
    pending_dir = Path(args.pending_dir)

    if not pending_dir.is_absolute():
        pending_dir = repo_root / pending_dir

    pending_dir.mkdir(parents=True, exist_ok=True)

    minimized_roots: list[Path] = []

    for root_arg in args.roots:
        run_root = Path(root_arg)

        if not run_root.is_absolute():
            run_root = repo_root / run_root

        for bucket_name, minimized_name in [
            ("failures", "minimized-failures"),
            ("crashes", "minimized-crashes"),
            ("timeouts", "minimized-timeouts"),
        ]:
            bucket_dir = run_root / bucket_name
            minimized_dir = run_root / minimized_name

            if not has_case_dirs(bucket_dir):
                continue

            subprocess.run(
                [
                    "python3",
                    "scripts/minimize_kappa_cases.py",
                    "--in-dir",
                    str(bucket_dir),
                    "--out-dir",
                    str(minimized_dir),
                    "--seed-dir",
                    str(run_root / "recycled-seeds"),
                    "--cli",
                    args.cli,
                    "--timeout-seconds",
                    str(args.timeout_seconds),
                ],
                cwd=repo_root,
                check=True,
                capture_output=True,
                text=True,
            )

            if has_case_dirs(minimized_dir):
                minimized_roots.append(minimized_dir)

    if not minimized_roots:
        print(json.dumps({"promoted": 0, "clusters": 0, "pending_dir": str(pending_dir)}, indent=2))
        return

    cluster_summary = run_json_command(
        [
            "python3",
            "scripts/cluster_fuzz_cases_by_trace.py",
            *[str(root) for root in minimized_roots],
            "--cli",
            args.cli,
            "--timeout-seconds",
            str(args.timeout_seconds),
            "--group-by",
            "trace+terminal",
        ],
        repo_root,
    )

    promoted = []

    for cluster in cluster_summary["clusters"]:
        terminal_signatures = cluster["terminal_signatures"]
        dominant_terminal = max(terminal_signatures.items(), key=lambda item: item[1])[0]
        kind = max(cluster["kinds"].items(), key=lambda item: item[1])[0]
        terminal_hash = hashlib.sha1(dominant_terminal.encode("utf-8")).hexdigest()
        dir_name = f"{kind}-{cluster['trace_hash'][:12]}-{terminal_hash[:12]}"
        destination = pending_dir / dir_name
        destination.mkdir(parents=True, exist_ok=True)

        representative_path = Path(cluster["representative_path"])
        shutil.copy2(representative_path / "main.kp", destination / "main.kp")

        expected_path = representative_path / "expected.txt"
        if expected_path.exists():
            shutil.copy2(expected_path, destination / "expected.txt")
        else:
            (destination / "expected.txt").write_text(dominant_terminal + "\n", encoding="utf-8")

        cluster_record = dict(cluster)
        cluster_record["source_case_dir"] = str(representative_path)
        (destination / "cluster.json").write_text(json.dumps(cluster_record, indent=2), encoding="utf-8")
        promoted.append({"destination": str(destination), **cluster_record})

    summary = {
        "promoted": len(promoted),
        "clusters": len(cluster_summary["clusters"]),
        "pending_dir": str(pending_dir),
        "entries": promoted,
    }
    (pending_dir / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
