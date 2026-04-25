#!/usr/bin/env python3
"""Cluster saved fuzz cases by portable pipeline trace hash."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import subprocess
import tempfile
from collections import Counter, defaultdict
from pathlib import Path

from kappa_fuzz_lib import kind_from_bucket_name, last_nonblank_line


TRACE_LINE_PATTERN = re.compile(
    r"^\s*(\S+)\s+(\S+)\s+(\S+)\s+->\s+(\S+)\s+changed=(true|false)(?:\s+verify=(\S+))?$"
)
TEMP_PATH_PATTERN = re.compile(r"/var/folders/[^ ]+/main\.kp")

def repo_root_from_script() -> Path:
    return Path(__file__).resolve().parents[1]


def terminal_signature(case_dir: Path) -> str:
    expected_path = case_dir / "expected.txt"
    stderr_path = case_dir / "stderr.txt"
    stdout_path = case_dir / "stdout.txt"
    expected = expected_path.read_text(encoding="utf-8", errors="replace") if expected_path.exists() else ""
    stderr = stderr_path.read_text(encoding="utf-8", errors="replace") if stderr_path.exists() else ""
    stdout = stdout_path.read_text(encoding="utf-8", errors="replace") if stdout_path.exists() else ""
    signature = (
        last_nonblank_line(expected)
        or last_nonblank_line(stderr)
        or last_nonblank_line(stdout)
        or "<empty>"
    )
    return TEMP_PATH_PATTERN.sub("<temp>/main.kp", signature)


def trace_steps_from_stdout(stdout: str) -> list[str]:
    in_trace = False
    steps: set[str] = set()

    for raw_line in stdout.splitlines():
        line = raw_line.rstrip()

        if line.strip() == "Pipeline trace":
            in_trace = True
            continue

        if not in_trace:
            continue

        if not line.strip():
            if steps:
                break
            continue

        match = TRACE_LINE_PATTERN.match(line)

        if not match:
            continue

        event, subject, input_checkpoint, output_checkpoint, changed, verify = match.groups()
        verify_text = verify or "-"
        steps.add(
            "|".join(
                [
                    event,
                    subject,
                    input_checkpoint,
                    output_checkpoint,
                    changed,
                    verify_text,
                ]
            )
        )

    return sorted(steps)


def trace_hash(trace_steps: list[str]) -> str:
    payload = "\n".join(trace_steps).encode("utf-8")
    return hashlib.sha1(payload).hexdigest()


def run_trace(cli_path: Path, repo_root: Path, source: str, verify_checkpoint: str | None, timeout_seconds: float) -> tuple[int, str, str, bool]:
    with tempfile.TemporaryDirectory(prefix="kappa-trace-") as temp_dir:
        temp_root = Path(temp_dir)
        source_path = temp_root / "main.kp"
        source_path.write_text(source.rstrip() + "\n", encoding="utf-8")

        command = [str(cli_path), "--source-root", str(temp_root), "--trace"]

        if verify_checkpoint:
            command.extend(["--verify", verify_checkpoint])

        command.append(str(source_path))

        try:
            result = subprocess.run(
                command,
                cwd=repo_root,
                capture_output=True,
                text=True,
                timeout=timeout_seconds,
            )
            return result.returncode, result.stdout, result.stderr, False
        except subprocess.TimeoutExpired as ex:
            return -1, ex.stdout or "", ex.stderr or "", True


def verify_checkpoint_for_kind(kind: str) -> str | None:
    if kind in {"failure", "ok", "success"}:
        return "KBackendIR"

    return None


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("roots", nargs="+", help="Case directories or fuzz-run roots.")
    parser.add_argument("--cli", default="src/Kappa.Compiler.Cli/bin/Debug/net10.0/Kappa.Compiler.Cli")
    parser.add_argument("--timeout-seconds", type=float, default=5.0)
    parser.add_argument("--out", default="")
    parser.add_argument("--group-by", choices=["trace", "trace+terminal"], default="trace+terminal")
    args = parser.parse_args()

    repo_root = repo_root_from_script()
    cli_path = Path(args.cli)

    if not cli_path.is_absolute():
        cli_path = repo_root / cli_path

    case_dirs: list[Path] = []

    for root_arg in args.roots:
        root = Path(root_arg)

        if not root.is_absolute():
            root = repo_root / root

        if (root / "main.kp").exists():
            case_dirs.append(root)
            continue

        found_branch = False

        for kind_dir in ["failures", "crashes", "diagnostics", "successes", "timeouts"]:
            branch = root / kind_dir

            if not branch.exists():
                continue

            found_branch = True
            case_dirs.extend(sorted(path for path in branch.iterdir() if path.is_dir() and (path / "main.kp").exists()))

        if not found_branch and root.exists():
            case_dirs.extend(sorted(path for path in root.iterdir() if path.is_dir() and (path / "main.kp").exists()))

    results = []
    clusters: dict[str, list[dict]] = defaultdict(list)

    for case_dir in sorted(case_dirs):
        parent_name = case_dir.parent.name
        kind = kind_from_bucket_name(parent_name)
        source = (case_dir / "main.kp").read_text(encoding="utf-8", errors="replace")
        verify_checkpoint = verify_checkpoint_for_kind(kind)
        returncode, stdout, stderr, timed_out = run_trace(
            cli_path,
            repo_root,
            source,
            verify_checkpoint,
            args.timeout_seconds,
        )
        steps = trace_steps_from_stdout(stdout)
        step_hash = trace_hash(steps)
        terminal = terminal_signature(case_dir)

        if args.group_by == "trace+terminal":
            cluster_key = f"{step_hash}:{terminal}"
        else:
            cluster_key = step_hash

        result = {
            "case": case_dir.name,
            "path": str(case_dir),
            "kind": kind,
            "trace_hash": step_hash,
            "cluster_key": cluster_key,
            "trace_step_count": len(steps),
            "trace_steps": steps,
            "terminal_signature": terminal,
            "returncode": returncode,
            "timed_out": timed_out,
            "trace_stdout_missing": not bool(steps) and "Pipeline trace" not in stdout,
            "trace_stderr_head": last_nonblank_line(stderr),
        }
        results.append(result)
        clusters[cluster_key].append(result)

    cluster_records = []

    for cluster_key, members in sorted(clusters.items(), key=lambda item: (-len(item[1]), item[0])):
        representative = min(members, key=lambda member: (len(Path(member["path"]).joinpath("main.kp").read_text(encoding="utf-8", errors="replace")), member["case"]))
        kind_counts = Counter(member["kind"] for member in members)
        terminal_counts = Counter(member["terminal_signature"] for member in members)
        cluster_records.append(
            {
                "cluster_key": cluster_key,
                "count": len(members),
                "trace_hash": members[0]["trace_hash"],
                "trace_step_count": members[0]["trace_step_count"],
                "kinds": dict(kind_counts),
                "terminal_signatures": dict(terminal_counts),
                "representative_case": representative["case"],
                "representative_path": representative["path"],
            }
        )

    summary = {
        "case_count": len(results),
        "cluster_count": len(cluster_records),
        "group_by": args.group_by,
        "clusters": cluster_records,
        "cases": results,
    }

    if args.out:
        out_path = Path(args.out)

        if not out_path.is_absolute():
            out_path = repo_root / out_path

        out_path.parent.mkdir(parents=True, exist_ok=True)
        out_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")

    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
