#!/usr/bin/env python3
"""Generate small well-typed Kappa programs from a Lambda Prolog model."""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_ELPI_PROGRAM = REPO_ROOT / "tools" / "lambda_prolog" / "kappa_generator.elpi"
LOCAL_ELPI = REPO_ROOT / ".tools" / "elpi-switch" / "_opam" / "bin" / "elpi"
RELEASE_CLI_DIR = REPO_ROOT / "src" / "Kappa.Compiler.Cli" / "bin" / "Release"
SAMPLE_START = re.compile(r"^%%KAPPA-SAMPLE\s+(\d+)\s*$")
SAMPLE_END = "%%END-KAPPA-SAMPLE"


@dataclass(frozen=True)
class GeneratedSample:
    index: int
    source: str


def resolve_elpi(explicit: str | None = None) -> Path | None:
    candidates: list[Path] = []

    if explicit:
        candidates.append(Path(explicit))

    env_elpi = os.environ.get("ELPI")
    if env_elpi:
        candidates.append(Path(env_elpi))

    path_elpi = shutil.which("elpi")
    if path_elpi:
        candidates.append(Path(path_elpi))

    candidates.append(LOCAL_ELPI)

    for candidate in candidates:
        resolved = candidate.expanduser()
        if resolved.exists() and os.access(resolved, os.X_OK):
            return resolved

    return None


def resolve_kappa_cli(explicit: str | None = None) -> Path | None:
    candidates: list[Path] = []

    if explicit:
        candidates.append(Path(explicit))

    env_cli = os.environ.get("KAPPA_CLI")
    if env_cli:
        candidates.append(Path(env_cli))

    if RELEASE_CLI_DIR.exists():
        release_binaries = [
            path
            for path in RELEASE_CLI_DIR.rglob("Kappa.Compiler.Cli")
            if path.is_file() and os.access(path, os.X_OK)
        ]
        candidates.extend(sorted(release_binaries, key=lambda path: path.stat().st_mtime, reverse=True))

    for candidate in candidates:
        resolved = candidate.expanduser()
        if resolved.exists() and os.access(resolved, os.X_OK):
            return resolved

    return None


def run_elpi(elpi: Path, program: Path, *, count: int, depth: int, start_index: int = 0) -> str:
    command = [
        str(elpi),
        str(program),
        "-exec",
        "emit-samples",
        "--",
        str(count),
        str(depth),
        str(start_index),
    ]
    result = subprocess.run(
        command,
        cwd=REPO_ROOT,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )

    if result.returncode != 0:
        raise RuntimeError(
            "ELPI generation failed with exit code "
            f"{result.returncode}\nSTDOUT:\n{result.stdout}\nSTDERR:\n{result.stderr}"
        )

    return result.stdout


def parse_elpi_samples(text: str) -> list[GeneratedSample]:
    samples: list[GeneratedSample] = []
    current_index: int | None = None
    current_lines: list[str] = []

    for raw_line in text.splitlines():
        start_match = SAMPLE_START.match(raw_line)
        if start_match:
            if current_index is not None:
                raise ValueError(f"nested sample marker before {SAMPLE_END}")
            current_index = int(start_match.group(1))
            current_lines = []
            continue

        if raw_line == SAMPLE_END:
            if current_index is None:
                raise ValueError(f"{SAMPLE_END} without a sample start")
            samples.append(GeneratedSample(current_index, "\n".join(current_lines).rstrip() + "\n"))
            current_index = None
            current_lines = []
            continue

        if current_index is not None:
            current_lines.append(raw_line)
        elif raw_line.strip():
            raise ValueError(f"unexpected ELPI output outside a sample block: {raw_line!r}")

    if current_index is not None:
        raise ValueError(f"sample {current_index} was not closed by {SAMPLE_END}")

    return samples


def write_fixture_cases(samples: list[GeneratedSample], out_dir: Path) -> list[Path]:
    out_dir.mkdir(parents=True, exist_ok=True)
    case_dirs: list[Path] = []

    for sample in samples:
        case_dir = out_dir / f"lp_kappa_{sample.index:03d}"
        case_dir.mkdir(parents=True, exist_ok=True)
        (case_dir / "main.kp").write_text(sample.source, encoding="utf-8")
        case_dirs.append(case_dir)

    return case_dirs


def validate_case(case_dir: Path, *, cli: Path | None = None) -> None:
    kappa_cli = cli or resolve_kappa_cli()
    if kappa_cli is None:
        raise RuntimeError(
            "Could not find a release Kappa CLI binary. Build or provide "
            "src/Kappa.Compiler.Cli/bin/Release/.../Kappa.Compiler.Cli, "
            "or pass --kappa-cli /path/to/Kappa.Compiler.Cli."
        )

    command = [
        str(kappa_cli),
        "--source-root",
        str(case_dir),
        str(case_dir),
    ]
    result = subprocess.run(
        command,
        cwd=REPO_ROOT,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )

    if result.returncode != 0:
        raise RuntimeError(
            f"Kappa validation failed for {case_dir}\n"
            f"STDOUT:\n{result.stdout}\nSTDERR:\n{result.stderr}"
        )


def validate_cases(case_dirs: list[Path], *, cli: Path | None = None) -> None:
    for case_dir in case_dirs:
        validate_case(case_dir, cli=cli)


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--count", type=int, default=8, help="number of programs to emit")
    parser.add_argument("--depth", type=int, default=4, help="maximum synthesis depth")
    parser.add_argument("--start-index", type=int, default=0, help="first generated candidate index")
    parser.add_argument("--program", type=Path, default=DEFAULT_ELPI_PROGRAM, help="ELPI program to execute")
    parser.add_argument("--elpi", default=None, help="path to the elpi executable")
    parser.add_argument("--kappa-cli", default=None, help="path to a release Kappa CLI binary used for --validate")
    parser.add_argument("--out-dir", type=Path, default=None, help="directory for generated fixture cases")
    parser.add_argument("--validate", action="store_true", help="compile each generated case with the Kappa CLI")
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_arg_parser().parse_args(argv)

    if args.count < 1:
        raise SystemExit("--count must be at least 1")
    if args.depth < 1:
        raise SystemExit("--depth must be at least 1")
    if args.start_index < 0:
        raise SystemExit("--start-index must be non-negative")

    elpi = resolve_elpi(args.elpi)
    if elpi is None:
        raise SystemExit(
            "Could not find elpi. Install it with the local opam switch under "
            ".tools/elpi-switch or pass --elpi /path/to/elpi."
        )

    output = run_elpi(elpi, args.program, count=args.count, depth=args.depth, start_index=args.start_index)
    samples = parse_elpi_samples(output)

    if len(samples) != args.count:
        raise SystemExit(
            f"ELPI emitted {len(samples)} samples; expected {args.count}. "
            "Lower --count or expand the ELPI candidate set."
        )

    if args.out_dir is None:
        sys.stdout.write(output)
        return 0

    case_dirs = write_fixture_cases(samples, args.out_dir)

    if args.validate:
        validate_cases(case_dirs, cli=resolve_kappa_cli(args.kappa_cli))

    print(f"wrote {len(case_dirs)} Lambda Prolog generated Kappa cases to {args.out_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
