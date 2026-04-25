#!/usr/bin/env python3
"""Minimize saved fuzz cases and export recycled seed programs."""

from __future__ import annotations

import argparse
import json
import re
import shutil
from dataclasses import dataclass
from pathlib import Path

from kappa_fuzz_lib import (
    first_nonblank_line,
    kind_from_bucket_name,
    last_nonblank_line,
    load_json,
    repo_root_from_script,
    run_cli_source,
    terminal_signature_from_case,
)


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


def load_case_expectation(case_dir: Path, kind: str) -> tuple[str, str]:
    meta = load_json(case_dir / "meta.json")
    stage = str(meta.get("stage") or ("verify:KBackendIR" if kind == "failure" else "compile"))

    if kind == "failure":
        expected = terminal_signature_from_case(case_dir)
    elif kind == "crash":
        expected = first_nonblank_line((case_dir / "stderr.txt").read_text(encoding="utf-8", errors="replace"))
        if not expected:
            expected = first_nonblank_line((case_dir / "stdout.txt").read_text(encoding="utf-8", errors="replace"))
    elif kind == "timeout":
        expected = stage
    else:
        raise ValueError(f"Unsupported case kind: {kind}")

    return stage, expected or "<empty>"


def preserves_case(cli_path: Path, repo_root: Path, source: str, kind: str, stage: str, expected: str, timeout_seconds: float) -> bool:
    run = run_cli_source(cli_path, repo_root, source, stage=stage, timeout_seconds=timeout_seconds)

    if run.kind != kind:
        return False

    if kind == "failure":
        return last_nonblank_line(run.stdout) == expected

    if kind == "crash":
        combined = f"{run.stdout}\n{run.stderr}"
        return expected in combined if expected and expected != "<empty>" else True

    if kind == "timeout":
        return run.timed_out

    return False


def rewrite_candidates_for_crash(source: str) -> list[str]:
    candidates: list[str] = []
    lines = [line for line in source.splitlines() if line.strip()]

    if not lines:
        return candidates

    without_module = [line for line in lines if not line.startswith("module ")]
    if without_module != lines:
        candidates.append("\n".join(without_module))

    type_line = next((line for line in lines if line.startswith("type ")), "")
    let_line = next((line for line in lines if line.startswith("let ")), "")

    type_match = re.match(r"type\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*\((.*)\)", type_line)
    if type_match:
        type_name = type_match.group(1)
        candidates.append("\n".join([f"type {type_name} = (left : {type_name})"] + [line for line in lines if line != type_line]))

        if let_line:
            let_match = re.match(
                r"let\s+([A-Za-z_][A-Za-z0-9_]*)(?:\s*\([^)]*\))?\s*:\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*)",
                let_line,
            )

            if let_match:
                binding_name = let_match.group(1)
                result_type = let_match.group(2)
                candidates.append(f"type {type_name} = (left : {type_name})\nlet {binding_name} : {result_type} = ()")

    if let_line:
        let_match = re.match(
            r"let\s+([A-Za-z_][A-Za-z0-9_]*)(?:\s*\([^)]*\))?\s*:\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*)",
            let_line,
        )
        if let_match:
            binding_name = let_match.group(1)
            result_type = let_match.group(2)
            simplified_let = f"let {binding_name} : {result_type} = ()"
            candidates.append("\n".join([simplified_let if line == let_line else line for line in lines]))

    unique: list[str] = []
    seen: set[str] = set()

    for candidate in candidates:
        normalized = candidate.strip()
        if normalized and normalized != source.strip() and normalized not in seen:
            seen.add(normalized)
            unique.append(normalized)

    return unique


def minimize_source(cli_path: Path, repo_root: Path, source: str, kind: str, stage: str, expected: str, timeout_seconds: float) -> str:
    def check(candidate_source: str) -> bool:
        return preserves_case(cli_path, repo_root, candidate_source, kind, stage, expected, timeout_seconds)

    blocks = split_blocks(source)
    if len(blocks) > 1:
        blocks = ddmin_list(blocks, lambda candidate: check(join_blocks(candidate)))

    source = join_blocks(blocks)
    lines = source.splitlines()

    if len(lines) > 1:
        lines = ddmin_list(lines, lambda candidate: check("\n".join(candidate).strip()))

    source = "\n".join(lines).strip()

    if kind == "crash":
        changed = True
        while changed:
            changed = False
            for candidate in rewrite_candidates_for_crash(source):
                if len(candidate) >= len(source):
                    continue
                if check(candidate):
                    source = candidate
                    changed = True
                    break

    return source


@dataclass
class CaseResult:
    name: str
    kind: str
    stage: str
    expectation: str
    original_lines: int
    minimized_lines: int
    changed: bool


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--in-dir", required=True)
    parser.add_argument("--out-dir", required=True)
    parser.add_argument("--seed-dir", default="")
    parser.add_argument("--cli", default="src/Kappa.Compiler.Cli/bin/Debug/net10.0/Kappa.Compiler.Cli")
    parser.add_argument("--timeout-seconds", type=float, default=5.0)
    args = parser.parse_args()

    repo_root = repo_root_from_script()
    in_dir = Path(args.in_dir) if Path(args.in_dir).is_absolute() else repo_root / args.in_dir
    out_dir = Path(args.out_dir) if Path(args.out_dir).is_absolute() else repo_root / args.out_dir
    seed_dir = Path(args.seed_dir) if args.seed_dir else None
    cli_path = Path(args.cli) if Path(args.cli).is_absolute() else repo_root / args.cli
    kind = kind_from_bucket_name(in_dir.name)

    if seed_dir is not None and not seed_dir.is_absolute():
        seed_dir = repo_root / seed_dir

    out_dir.mkdir(parents=True, exist_ok=True)
    if seed_dir is not None:
        seed_dir.mkdir(parents=True, exist_ok=True)

    results: list[CaseResult] = []

    for case_dir in sorted(path for path in in_dir.iterdir() if path.is_dir() and (path / "main.kp").exists()):
        source = (case_dir / "main.kp").read_text(encoding="utf-8", errors="replace")
        stage, expected = load_case_expectation(case_dir, kind)
        minimized = minimize_source(cli_path, repo_root, source, kind, stage, expected, args.timeout_seconds)

        out_case_dir = out_dir / case_dir.name
        out_case_dir.mkdir(parents=True, exist_ok=True)
        (out_case_dir / "main.kp").write_text(minimized + "\n", encoding="utf-8")
        (out_case_dir / "expected.txt").write_text(expected + "\n", encoding="utf-8")
        (out_case_dir / "meta.json").write_text(json.dumps({"kind": kind, "stage": stage}, indent=2), encoding="utf-8")

        if seed_dir is not None:
            seed_case_dir = seed_dir / kind / case_dir.name
            seed_case_dir.mkdir(parents=True, exist_ok=True)
            (seed_case_dir / "main.kp").write_text(minimized + "\n", encoding="utf-8")

        results.append(
            CaseResult(
                name=case_dir.name,
                kind=kind,
                stage=stage,
                expectation=expected,
                original_lines=len(source.splitlines()),
                minimized_lines=len(minimized.splitlines()),
                changed=minimized.strip() != source.strip(),
            )
        )

    if seed_dir is not None and kind == "failure":
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
