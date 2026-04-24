#!/usr/bin/env python3
"""Minimize saved fuzz failures and export recycled seed programs."""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path


def repo_root_from_script() -> Path:
    return Path(__file__).resolve().parents[1]


def last_nonblank_line(text: str) -> str:
    lines = [line.strip() for line in text.splitlines() if line.strip()]
    return lines[-1] if lines else ""


def run_verify(cli_path: Path, repo_root: Path, source: str, checkpoint: str, timeout_seconds: float) -> tuple[int, str, str, bool]:
    with tempfile.TemporaryDirectory(prefix="kappa-min-") as temp_dir:
        temp_root = Path(temp_dir)
        source_path = temp_root / "main.kp"
        source_path.write_text(source.rstrip() + "\n", encoding="utf-8")

        try:
            result = subprocess.run(
                [str(cli_path), "--source-root", str(temp_root), "--verify", checkpoint, str(source_path)],
                cwd=repo_root,
                capture_output=True,
                text=True,
                timeout=timeout_seconds,
            )
            return result.returncode, result.stdout, result.stderr, False
        except subprocess.TimeoutExpired as ex:
            return -1, ex.stdout or "", ex.stderr or "", True


def preserves_failure(cli_path: Path, repo_root: Path, source: str, checkpoint: str, expected_message: str, timeout_seconds: float) -> bool:
    returncode, stdout, _stderr, timed_out = run_verify(cli_path, repo_root, source, checkpoint, timeout_seconds)

    if timed_out or returncode == 0:
        return False

    return last_nonblank_line(stdout) == expected_message


def split_blocks(text: str) -> list[str]:
    blocks: list[str] = []
    current: list[str] = []

    for line in text.splitlines():
        if line.strip() == "":
            if current:
                blocks.append("\n".join(current))
                current = []
        else:
            current.append(line)

    if current:
        blocks.append("\n".join(current))

    return blocks


def join_blocks(blocks: list[str]) -> str:
    return "\n\n".join(blocks).strip()


def ddmin_list(items: list[str], test) -> list[str]:
    if len(items) <= 1:
        return items

    n = 2

    while len(items) >= 2:
        chunk_size = max(1, len(items) // n)
        reduced = False

        for start in range(0, len(items), chunk_size):
            complement = items[:start] + items[start + chunk_size :]

            if complement and test(complement):
                items = complement
                n = max(2, n - 1)
                reduced = True
                break

        if not reduced:
            if n >= len(items):
                break

            n = min(len(items), n * 2)

    return items


def minimize_failure_source(cli_path: Path, repo_root: Path, source: str, checkpoint: str, expected_message: str, timeout_seconds: float) -> str:
    blocks = split_blocks(source)

    def block_test(candidate_blocks: list[str]) -> bool:
        candidate = join_blocks(candidate_blocks)
        return preserves_failure(cli_path, repo_root, candidate, checkpoint, expected_message, timeout_seconds)

    if len(blocks) > 1:
        blocks = ddmin_list(blocks, block_test)

    source = join_blocks(blocks)
    lines = source.splitlines()

    def line_test(candidate_lines: list[str]) -> bool:
        candidate = "\n".join(candidate_lines).strip()
        return preserves_failure(cli_path, repo_root, candidate, checkpoint, expected_message, timeout_seconds)

    if len(lines) > 1:
        lines = ddmin_list(lines, line_test)

    return "\n".join(lines).strip()


@dataclass
class CaseResult:
    name: str
    expected_message: str
    original_lines: int
    minimized_lines: int
    changed: bool


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--in-dir", default="artifacts/fuzzball-kappa/fuzz-run/failures")
    parser.add_argument("--out-dir", default="artifacts/fuzzball-kappa/minimized-failures")
    parser.add_argument("--seed-dir", default="artifacts/fuzzball-kappa/recycled-seeds")
    parser.add_argument("--cli", default="src/Kappa.Compiler.Cli/bin/Debug/net10.0/Kappa.Compiler.Cli")
    parser.add_argument("--checkpoint", default="KBackendIR")
    parser.add_argument("--timeout-seconds", type=float, default=5.0)
    args = parser.parse_args()

    repo_root = repo_root_from_script()
    in_dir = Path(args.in_dir)
    out_dir = Path(args.out_dir)
    seed_dir = Path(args.seed_dir)
    cli_path = Path(args.cli)

    if not in_dir.is_absolute():
        in_dir = repo_root / in_dir

    if not out_dir.is_absolute():
        out_dir = repo_root / out_dir

    if not seed_dir.is_absolute():
        seed_dir = repo_root / seed_dir

    if not cli_path.is_absolute():
        cli_path = repo_root / cli_path

    out_dir.mkdir(parents=True, exist_ok=True)
    seed_dir.mkdir(parents=True, exist_ok=True)

    results: list[CaseResult] = []

    for case_dir in sorted(path for path in in_dir.iterdir() if path.is_dir()):
        source = (case_dir / "main.kp").read_text(encoding="utf-8")
        stdout = (case_dir / "stdout.txt").read_text(encoding="utf-8")
        expected_message = last_nonblank_line(stdout)

        minimized = minimize_failure_source(
            cli_path,
            repo_root,
            source,
            args.checkpoint,
            expected_message,
            args.timeout_seconds,
        )

        out_case_dir = out_dir / case_dir.name
        out_case_dir.mkdir(parents=True, exist_ok=True)
        (out_case_dir / "main.kp").write_text(minimized + "\n", encoding="utf-8")
        (out_case_dir / "expected.txt").write_text(expected_message + "\n", encoding="utf-8")

        seed_case_dir = seed_dir / "failures" / case_dir.name
        seed_case_dir.mkdir(parents=True, exist_ok=True)
        (seed_case_dir / "main.kp").write_text(minimized + "\n", encoding="utf-8")

        results.append(
            CaseResult(
                name=case_dir.name,
                expected_message=expected_message,
                original_lines=len(source.splitlines()),
                minimized_lines=len(minimized.splitlines()),
                changed=minimized.strip() != source.strip(),
            )
        )

    success_dir = in_dir.parent / "successes"

    if success_dir.exists():
        for case_dir in sorted(path for path in success_dir.iterdir() if path.is_dir()):
            seed_case_dir = seed_dir / "successes" / case_dir.name
            seed_case_dir.mkdir(parents=True, exist_ok=True)
            shutil.copy2(case_dir / "main.kp", seed_case_dir / "main.kp")

    summary = {
        "count": len(results),
        "changed": sum(1 for result in results if result.changed),
        "results": [result.__dict__ for result in results],
    }
    (out_dir / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
