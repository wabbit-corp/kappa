#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import shutil
import subprocess
import sys
import textwrap
from collections import Counter
from pathlib import Path


DEFAULT_TIMEOUT_SECONDS = 25.0


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent


def fixtures_root(root: Path) -> Path:
    return root / "tests" / "Kappa.Compiler.Tests" / "Fixtures"


def new_tests_root(root: Path) -> Path:
    return root / "new-tests"


def compiler_dll_path(root: Path) -> Path:
    return root / "src" / "Kappa.Compiler" / "bin" / "Debug" / "net10.0" / "Kappa.Compiler.dll"


def tests_dll_path(root: Path) -> Path:
    return (
        root
        / "tests"
        / "Kappa.Compiler.Tests"
        / "bin"
        / "Debug"
        / "net10.0"
        / "Kappa.Compiler.Tests.dll"
    )


def escape_fsharp_string(value: str) -> str:
    return value.replace("\\", "\\\\").replace('"', '\\"')


def build_fsi_script(fixture_name: str, compiler_dll: Path, tests_dll: Path) -> str:
    escaped_name = escape_fsharp_string(fixture_name)
    return textwrap.dedent(
        f"""\
        #r "{compiler_dll.as_posix()}"
        #r "{tests_dll.as_posix()}"
        open System

        let name = "{escaped_name}"
        let fixture =
            Harness.discoverKpFixtureCases ()
            |> Seq.find (fun item -> item.Name = name)

        try
            Harness.runKpFixtureCase fixture
            printfn "PASS"
        with ex ->
            printfn "FAIL: %s" ex.Message
            exit 1
        """
    )


def candidate_directories(root: Path, selected_names: set[str] | None) -> list[Path]:
    candidates = [path for path in root.iterdir() if path.is_dir()]
    if selected_names is not None:
        candidates = [path for path in candidates if path.name in selected_names]
    return sorted(candidates, key=lambda path: path.name)


def iter_directory_entries(root: Path) -> list[Path]:
    return sorted(root.rglob("*"), key=lambda path: path.relative_to(root).as_posix())


def directory_content_hash(root: Path) -> str:
    digest = hashlib.sha256()
    for path in iter_directory_entries(root):
        relative = path.relative_to(root).as_posix().encode("utf-8")
        if path.is_dir():
            digest.update(b"D\0")
            digest.update(relative)
            digest.update(b"\0")
            continue
        if not path.is_file():
            continue

        digest.update(b"F\0")
        digest.update(relative)
        digest.update(b"\0")
        with path.open("rb") as handle:
            for chunk in iter(lambda: handle.read(1024 * 1024), b""):
                digest.update(chunk)
        digest.update(b"\0")
    return digest.hexdigest()


def fixture_content_hash_index(root: Path) -> dict[str, str]:
    index: dict[str, str] = {}
    fixture_dirs = (path for path in root.iterdir() if path.is_dir())
    for path in sorted(fixture_dirs, key=lambda path: path.name):
        index.setdefault(directory_content_hash(path), path.name)
    return index


def summarize_output(output: str) -> str:
    return " ".join(output.splitlines()[:4])[:500]


def summarize_statuses(results: list[tuple[str, str]]) -> Counter[str]:
    return Counter(status.split()[0] for _, status in results)


def run_fixture_case(
    root: Path,
    fixture_name: str,
    timeout_seconds: float,
) -> subprocess.CompletedProcess[str]:
    script = build_fsi_script(
        fixture_name=fixture_name,
        compiler_dll=compiler_dll_path(root),
        tests_dll=tests_dll_path(root),
    )
    return subprocess.run(
        ["dotnet", "fsi", "--quiet"],
        input=script,
        text=True,
        cwd=root,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        timeout=timeout_seconds,
        check=False,
    )


def precheck_fixture_directory(
    src: Path,
    dst: Path,
    existing_names: set[str],
    existing_content_hashes: dict[str, str],
) -> tuple[str | None, str]:
    name = src.name
    content_hash = directory_content_hash(src)
    if name in existing_names:
        return "SKIP_EXISTING", content_hash
    if dst.exists():
        return "SKIP_TARGET_EXISTS", content_hash

    duplicate_name = existing_content_hashes.get(content_hash)
    if duplicate_name is not None:
        return f"SKIP_DUPLICATE_CONTENT {duplicate_name}", content_hash

    return None, content_hash


def dry_run_fixture_directory(
    src: Path,
    dst: Path,
    existing_names: set[str],
    existing_content_hashes: dict[str, str],
) -> tuple[str, str]:
    status, content_hash = precheck_fixture_directory(
        src=src,
        dst=dst,
        existing_names=existing_names,
        existing_content_hashes=existing_content_hashes,
    )
    if status is not None:
        return status, content_hash

    existing_content_hashes.setdefault(content_hash, src.name)
    return "WOULD_MOVE", content_hash


def promote_fixture_directory(
    root: Path,
    src: Path,
    dst: Path,
    existing_names: set[str],
    existing_content_hashes: dict[str, str],
    timeout_seconds: float,
) -> tuple[str, str]:
    status, content_hash = precheck_fixture_directory(
        src=src,
        dst=dst,
        existing_names=existing_names,
        existing_content_hashes=existing_content_hashes,
    )
    if status is not None:
        return status, content_hash

    try:
        shutil.copytree(src, dst)
        proc = run_fixture_case(root=root, fixture_name=src.name, timeout_seconds=timeout_seconds)
        if proc.returncode == 0 and "PASS" in proc.stdout:
            shutil.rmtree(src)
            return "PASS", content_hash

        shutil.rmtree(dst)
        return f"FAIL {proc.returncode}: {summarize_output(proc.stdout)}", content_hash
    except subprocess.TimeoutExpired:
        if dst.exists():
            shutil.rmtree(dst)
        return "TIMEOUT", content_hash
    except Exception as exc:
        if dst.exists():
            shutil.rmtree(dst)
        return f"ERROR {type(exc).__name__}: {exc}", content_hash


def validate_inputs(root: Path, require_build_outputs: bool) -> list[str]:
    problems: list[str] = []
    for label, path in [
        ("fixtures directory", fixtures_root(root)),
        ("new-tests directory", new_tests_root(root)),
    ]:
        if not path.exists():
            problems.append(f"missing {label}: {path}")

    if not require_build_outputs:
        return problems

    for label, path in [
        ("compiler DLL", compiler_dll_path(root)),
        ("tests DLL", tests_dll_path(root)),
    ]:
        if not path.exists():
            problems.append(f"missing {label}: {path}")
    return problems


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Promote directories from new-tests/ into Kappa.Compiler test fixtures "
            "after verifying each case."
        )
    )
    parser.add_argument(
        "names",
        nargs="*",
        help="Optional fixture directory names under new-tests/ to process.",
    )
    parser.add_argument(
        "--timeout-seconds",
        type=float,
        default=DEFAULT_TIMEOUT_SECONDS,
        help=(
            "Per-fixture timeout in seconds when --move is set "
            f"(default: {DEFAULT_TIMEOUT_SECONDS:g})."
        ),
    )
    parser.add_argument(
        "--move",
        action="store_true",
        help="Actually promote fixtures. Without this flag, only report what would move.",
    )
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    root = repo_root()
    problems = validate_inputs(root, require_build_outputs=args.move)
    if problems:
        for problem in problems:
            print(problem, file=sys.stderr)
        return 2

    selected_names = set(args.names) if args.names else None
    candidates = candidate_directories(new_tests_root(root), selected_names)
    if selected_names is not None:
        found_names = {path.name for path in candidates}
        missing_names = sorted(selected_names - found_names)
        if missing_names:
            for name in missing_names:
                print(f"missing new-tests fixture: {name}", file=sys.stderr)
            return 2

    existing_names = {
        path.name for path in fixtures_root(root).iterdir() if path.is_dir()
    }
    existing_content_hashes = fixture_content_hash_index(fixtures_root(root))

    results: list[tuple[str, str]] = []
    for src in candidates:
        if args.move:
            status, content_hash = promote_fixture_directory(
                root=root,
                src=src,
                dst=fixtures_root(root) / src.name,
                existing_names=existing_names,
                existing_content_hashes=existing_content_hashes,
                timeout_seconds=args.timeout_seconds,
            )
        else:
            status, content_hash = dry_run_fixture_directory(
                src=src,
                dst=fixtures_root(root) / src.name,
                existing_names=existing_names,
                existing_content_hashes=existing_content_hashes,
            )

        if status in {"PASS", "WOULD_MOVE"}:
            existing_names.add(src.name)
            existing_content_hashes.setdefault(content_hash, src.name)
        results.append((src.name, status))

    for name, status in results:
        print(f"{status}\t{name}")

    print("SUMMARY")
    for status, count in sorted(summarize_statuses(results).items()):
        print(f"{status} {count}")

    successful_statuses = {
        "PASS",
        "WOULD_MOVE",
        "SKIP_EXISTING",
        "SKIP_TARGET_EXISTS",
        "SKIP_DUPLICATE_CONTENT",
    }
    return 0 if all(status.split()[0] in successful_statuses for _, status in results) else 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
