#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import sys
import zipfile
from pathlib import Path


SOURCE_ROOT_NAMES = ("src", "tests")
TOP_LEVEL_FILE_NAMES = ("Kappa.Compiler.sln", "Spec.md")
GENERATED_README_NAME = "README.md"
EXCLUDED_DIR_NAMES = {
    ".pytest_cache",
    "__pycache__",
    "bin",
    "obj",
    "scripts",
}
EXCLUDED_RELATIVE_DIRS = {
    "tests/python",
}
DEFAULT_OUTPUT_NAME = "kappa-sources.zip"
ZIP_TIMESTAMP = (1980, 1, 1, 0, 0, 0)


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent


def sort_key(path: Path, root: Path) -> str:
    return path.relative_to(root).as_posix()


def iter_files_under(root: Path) -> list[Path]:
    results: list[Path] = []
    for dirpath, dirnames, filenames in os.walk(root):
        current = Path(dirpath)
        dirnames[:] = sorted(
            dirname
            for dirname in dirnames
            if dirname not in EXCLUDED_DIR_NAMES
            and (current / dirname).relative_to(root.parent).as_posix()
            not in EXCLUDED_RELATIVE_DIRS
        )
        for filename in sorted(filenames):
            results.append(Path(dirpath) / filename)
    return results


def collect_source_files(root: Path) -> list[Path]:
    root = root.resolve()
    results: list[Path] = []

    for filename in TOP_LEVEL_FILE_NAMES:
        path = root / filename
        if path.is_file():
            results.append(path)

    for dirname in SOURCE_ROOT_NAMES:
        directory = root / dirname
        if directory.is_dir():
            results.extend(iter_files_under(directory))

    return sorted(set(results), key=lambda path: sort_key(path, root))


def create_zip_info(relative_path: str) -> zipfile.ZipInfo:
    info = zipfile.ZipInfo(relative_path, date_time=ZIP_TIMESTAMP)
    info.compress_type = zipfile.ZIP_DEFLATED
    info.external_attr = 0o644 << 16
    return info


def package_readme() -> str:
    return """# Kappa Source Package

This archive contains a source snapshot for the Kappa compiler and language
specification.

Top-level files:
- README.md: this package overview.
- Kappa.Compiler.sln: the .NET solution for the compiler and tests.
- Spec.md: the language specification.

Directories:
- src/: compiler, CLI, runtime, and bundled standard-library sources.
- tests/: compiler test projects and test data.
- tests/Kappa.Compiler.Tests/Fixtures/: Kappa fixture cases used by the
  compiler harness.

Excluded:
- scripts/: local maintenance and automation scripts.
- tests/python/: Python tests for local scripts.
- bin/, obj/, __pycache__/, and .pytest_cache/ build or cache directories.
"""


def create_source_zip(root: Path, output: Path) -> list[Path]:
    root = root.resolve()
    output = output.resolve()
    files = [
        path
        for path in collect_source_files(root)
        if path.resolve() != output
    ]

    output.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(output, mode="w") as archive:
        archive.writestr(create_zip_info(GENERATED_README_NAME), package_readme())
        for path in files:
            relative_path = path.relative_to(root).as_posix()
            archive.writestr(create_zip_info(relative_path), path.read_bytes())

    return files


def validate_inputs(root: Path) -> list[str]:
    problems: list[str] = []
    for dirname in SOURCE_ROOT_NAMES:
        path = root / dirname
        if not path.is_dir():
            problems.append(f"missing source directory: {path}")
    for filename in TOP_LEVEL_FILE_NAMES:
        path = root / filename
        if not path.is_file():
            problems.append(f"missing top-level source file: {path}")
    return problems


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Package Kappa source files from src/, tests/, Spec.md, and the "
            "solution file into one deterministic zip archive."
        )
    )
    parser.add_argument(
        "--root",
        type=Path,
        default=repo_root(),
        help="Repository root to package. Defaults to the root containing this script.",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=None,
        help=f"Output zip path. Defaults to {DEFAULT_OUTPUT_NAME} under --root.",
    )
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    root = args.root.resolve()
    output = args.output or (root / DEFAULT_OUTPUT_NAME)
    if not output.is_absolute():
        output = root / output

    problems = validate_inputs(root)
    if problems:
        for problem in problems:
            print(problem, file=sys.stderr)
        return 2

    files = create_source_zip(root, output)
    print(f"Wrote {output} ({len(files)} file(s)).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
