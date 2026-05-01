#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
STDLIB_ROOT = REPO_ROOT / "src" / "Kappa.Compiler" / "Stdlib"
DEFAULT_SCAN_ROOTS = [
    REPO_ROOT / "src" / "Kappa.Compiler",
    REPO_ROOT / "src" / "Kappa.Runtime",
    REPO_ROOT / "src" / "Kappa.Compiler.Cli",
]

TEXT_SUFFIXES = {".fs", ".cs", ".py"}
IGNORE_DIR_NAMES = {".git", ".idea", ".vs", "__pycache__", "bin", "obj"}

ALLOWED_FILES = {
    Path("src/Kappa.Compiler/CompilerKnownSymbols.fs"),
    Path("src/Kappa.Compiler/StandardLibraryCatalog.fs"),
    Path("src/Kappa.Compiler/Stdlib.fs"),
    Path("src/Kappa.Compiler/IntrinsicCatalog.fs"),
    Path("src/Kappa.Runtime/KappaRuntime.cs"),
}

EXTRA_FORBIDDEN_NAMES = {
    "Universe",
    "IsProp",
    "Quantity",
    "std",
    "prelude",
    "std.prelude",
}

TYPE_DECLARATION_RE = re.compile(
    r"^\s*(?:expect\s+type|data|effect|type)\s+([A-Z][A-Za-z0-9_']*)\b"
)


@dataclass(frozen=True)
class Problem:
    path: Path
    line: int
    name: str


def repo_relative(path: Path) -> Path:
    return path.resolve().relative_to(REPO_ROOT)


def iter_source_files(root: Path) -> list[Path]:
    results: list[Path] = []

    for child in sorted(root.iterdir(), key=lambda candidate: (candidate.is_file(), candidate.name.lower())):
        if child.is_dir():
            if child.name in IGNORE_DIR_NAMES:
                continue

            results.extend(iter_source_files(child))
            continue

        if child.suffix.lower() in TEXT_SUFFIXES:
            results.append(child)

    return results


def collect_forbidden_names() -> set[str]:
    names = set(EXTRA_FORBIDDEN_NAMES)

    for path in sorted(STDLIB_ROOT.rglob("*.kp")):
        text = path.read_text(encoding="utf-8")

        for line in text.splitlines():
            match = TYPE_DECLARATION_RE.match(line)

            if match is not None:
                names.add(match.group(1))

    return names


def exact_string_tokens(name: str) -> tuple[str, ...]:
    return (
        f'"{name}"',
        f'@"{name}"',
        f'$"{name}"',
        f'$@"{name}"',
        f'@$"{name}"',
        f'"""{name}"""',
    )


def scan_file(path: Path, forbidden_names: set[str]) -> list[Problem]:
    relative = repo_relative(path)

    if relative in ALLOWED_FILES:
        return []

    text = path.read_text(encoding="utf-8")
    problems: list[Problem] = []

    for line_number, line in enumerate(text.splitlines(), start=1):
        for name in forbidden_names:
            if any(token in line for token in exact_string_tokens(name)):
                problems.append(Problem(relative, line_number, name))

    return problems


def resolve_roots(raw_roots: list[str]) -> list[Path]:
    if not raw_roots:
        return DEFAULT_SCAN_ROOTS

    roots: list[Path] = []

    for raw_root in raw_roots:
        root = Path(raw_root).resolve()

        if not root.exists():
            raise FileNotFoundError(f"scan root does not exist: {raw_root}")

        roots.append(root)

    return roots


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Validate that compiler/runtime source files do not compare symbolic Kappa names "
            "via raw string literals outside the explicit prelude-validation boundary."
        )
    )
    parser.add_argument("roots", nargs="*", help="Optional scan roots. Defaults to compiler/runtime source trees.")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    roots = resolve_roots(args.roots)
    forbidden_names = collect_forbidden_names()
    problems: list[Problem] = []

    for root in roots:
        for path in iter_source_files(root):
            problems.extend(scan_file(path, forbidden_names))

    problems.sort(key=lambda problem: (problem.path.as_posix().lower(), problem.line, problem.name))

    if not problems:
        print("No forbidden raw symbolic names found outside the allowlist.")
        return 0

    for problem in problems:
        print(f"{problem.path.as_posix()}:{problem.line}: forbidden raw symbolic name string {problem.name!r}")

    print()
    print(f"Found {len(problems)} forbidden raw symbolic name occurrence(s).", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
