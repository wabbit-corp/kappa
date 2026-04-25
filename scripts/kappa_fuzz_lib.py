#!/usr/bin/env python3
"""Shared helpers for the Kappa fuzzball training and fuzzing pipeline."""

from __future__ import annotations

import hashlib
import json
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path


BUCKETS = ("crashes", "timeouts", "diagnostics", "successes", "failures")
KIND_TO_BUCKET = {
    "ok": "successes",
    "success": "successes",
    "diagnostic": "diagnostics",
    "crash": "crashes",
    "timeout": "timeouts",
    "failure": "failures",
}
BUCKET_TO_KIND = {
    "successes": "success",
    "diagnostics": "diagnostic",
    "crashes": "crash",
    "timeouts": "timeout",
    "failures": "failure",
    "minimized-crashes": "crash",
    "minimized-failures": "failure",
    "minimized-timeouts": "timeout",
}
CODEISH_PREFIXES = (
    "module ",
    "@PrivateByDefault module ",
    "import ",
    "export ",
    "data ",
    "type ",
    "trait ",
    "let ",
    "match ",
    "case ",
    "infix ",
    "prefix ",
    "postfix ",
)


@dataclass(frozen=True)
class InlineSample:
    text: str
    source_label: str
    source_path: str
    line: int


@dataclass(frozen=True)
class StringLiteral:
    value: str
    start: int
    end: int
    line: int


@dataclass(frozen=True)
class CaseRunResult:
    returncode: int
    stdout: str
    stderr: str
    timed_out: bool
    kind: str
    stage: str


def repo_root_from_script() -> Path:
    return Path(__file__).resolve().parents[1]


def current_git_commit(repo_root: Path) -> str:
    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            cwd=repo_root,
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout.strip()
    except subprocess.SubprocessError:
        return "unknown"


def sample_sha1(text: str) -> str:
    return hashlib.sha1(text.encode("utf-8")).hexdigest()


def repo_relative(repo_root: Path, path: Path) -> str:
    try:
        return str(path.relative_to(repo_root))
    except ValueError:
        return str(path)


def load_json(path: Path) -> dict:
    if not path.exists():
        return {}

    return json.loads(path.read_text(encoding="utf-8"))


def last_nonblank_line(text: str) -> str:
    lines = [line.strip() for line in text.splitlines() if line.strip()]
    return lines[-1] if lines else ""


def first_nonblank_line(text: str) -> str:
    for line in text.splitlines():
        stripped = line.strip()

        if stripped:
            return stripped

    return ""


def terminal_signature_from_case(case_dir: Path) -> str:
    expected_path = case_dir / "expected.txt"
    stderr_path = case_dir / "stderr.txt"
    stdout_path = case_dir / "stdout.txt"

    if expected_path.exists():
        signature = last_nonblank_line(expected_path.read_text(encoding="utf-8", errors="replace"))
        if signature:
            return signature

    if stderr_path.exists():
        signature = last_nonblank_line(stderr_path.read_text(encoding="utf-8", errors="replace"))
        if signature:
            return signature

    if stdout_path.exists():
        signature = last_nonblank_line(stdout_path.read_text(encoding="utf-8", errors="replace"))
        if signature:
            return signature

    return "<empty>"


def classify_result(returncode: int, stdout: str, stderr: str, timed_out: bool) -> str:
    if timed_out:
        return "timeout"

    lowered = f"{stdout}\n{stderr}".lower()

    if "stack overflow" in lowered:
        return "crash"

    if "unhandled exception" in lowered or "stack trace" in lowered or "system." in stderr.lower():
        return "crash"

    if returncode < 0:
        return "crash"

    if returncode == 0:
        return "ok"

    if "\nDiagnostics\n" in stdout or "\nDiagnostics\r\n" in stdout:
        return "diagnostic"

    return "failure"


def stage_to_args(stage: str) -> list[str]:
    if stage == "compile":
        return []

    if stage.startswith("verify:"):
        return ["--verify", stage.split(":", 1)[1]]

    raise ValueError(f"Unsupported stage: {stage}")


def run_cli_source(
    cli_path: Path,
    repo_root: Path,
    source: str,
    *,
    stage: str,
    timeout_seconds: float,
) -> CaseRunResult:
    with tempfile.TemporaryDirectory(prefix="kappa-fuzz-case-") as temp_dir:
        temp_root = Path(temp_dir)
        source_path = temp_root / "main.kp"
        source_path.write_text(source.rstrip() + "\n", encoding="utf-8")

        try:
            result = subprocess.run(
                [str(cli_path), "--source-root", str(temp_root), *stage_to_args(stage), str(source_path)],
                cwd=repo_root,
                capture_output=True,
                text=True,
                timeout=timeout_seconds,
            )
            return CaseRunResult(
                returncode=result.returncode,
                stdout=result.stdout,
                stderr=result.stderr,
                timed_out=False,
                kind=classify_result(result.returncode, result.stdout, result.stderr, False),
                stage=stage,
            )
        except subprocess.TimeoutExpired as ex:
            stdout = ex.stdout or ""
            stderr = ex.stderr or ""
            return CaseRunResult(
                returncode=-1,
                stdout=stdout,
                stderr=stderr,
                timed_out=True,
                kind=classify_result(-1, stdout, stderr, True),
                stage=stage,
            )


def bucket_for_kind(kind: str) -> str:
    return KIND_TO_BUCKET[kind]


def kind_from_bucket_name(name: str) -> str:
    return BUCKET_TO_KIND.get(name, name)


def discover_case_dirs(repo_root: Path, roots: list[str]) -> list[Path]:
    case_dirs: list[Path] = []

    for root_arg in roots:
        root = Path(root_arg)

        if not root.is_absolute():
            root = repo_root / root

        if (root / "main.kp").exists():
            case_dirs.append(root)
            continue

        if not root.exists():
            continue

        found_bucket = False
        for bucket in BUCKETS:
            branch = root / bucket

            if not branch.exists():
                continue

            found_bucket = True
            case_dirs.extend(sorted(path for path in branch.iterdir() if path.is_dir() and (path / "main.kp").exists()))

        if not found_bucket:
            case_dirs.extend(sorted(path for path in root.iterdir() if path.is_dir() and (path / "main.kp").exists()))

    return case_dirs


def skip_ws(text: str, index: int) -> int:
    while index < len(text):
        if text[index].isspace():
            index += 1
            continue

        if text.startswith("//", index):
            newline = text.find("\n", index)
            return len(text) if newline < 0 else newline + 1

        if text.startswith("(*", index):
            depth = 1
            index += 2

            while index < len(text) and depth > 0:
                if text.startswith("(*", index):
                    depth += 1
                    index += 2
                elif text.startswith("*)", index):
                    depth -= 1
                    index += 2
                else:
                    index += 1

            continue

        break

    return index


def decode_standard_string(payload: str) -> str:
    result: list[str] = []
    index = 0

    while index < len(payload):
        char = payload[index]

        if char != "\\" or index + 1 >= len(payload):
            result.append(char)
            index += 1
            continue

        next_char = payload[index + 1]
        escapes = {
            "\\": "\\",
            '"': '"',
            "'": "'",
            "n": "\n",
            "r": "\r",
            "t": "\t",
            "b": "\b",
            "f": "\f",
        }
        result.append(escapes.get(next_char, next_char))
        index += 2

    return "".join(result)


def parse_string_literal(text: str, index: int) -> tuple[StringLiteral, int] | None:
    line = text.count("\n", 0, index) + 1

    if text.startswith('"""', index):
        end = text.find('"""', index + 3)
        if end < 0:
            return None

        value = text[index + 3 : end]
        return StringLiteral(value=value, start=index, end=end + 3, line=line), end + 3

    if text.startswith('@\"', index):
        current = index + 2
        pieces: list[str] = []

        while current < len(text):
            if text.startswith('""', current):
                pieces.append('"')
                current += 2
                continue

            if text[current] == '"':
                return (
                    StringLiteral(value="".join(pieces), start=index, end=current + 1, line=line),
                    current + 1,
                )

            pieces.append(text[current])
            current += 1

        return None

    if text[index] != '"':
        return None

    current = index + 1
    escaped = False

    while current < len(text):
        char = text[current]
        if escaped:
            escaped = False
            current += 1
            continue

        if char == "\\":
            escaped = True
            current += 1
            continue

        if char == '"':
            payload = text[index + 1 : current]
            return (
                StringLiteral(value=decode_standard_string(payload), start=index, end=current + 1, line=line),
                current + 1,
            )

        current += 1

    return None


def parse_string_list_concat(text: str, index: int) -> tuple[StringLiteral, int] | None:
    if text[index] != "[":
        return None

    current = skip_ws(text, index + 1)
    items: list[StringLiteral] = []

    while current < len(text) and text[current] != "]":
        parsed = parse_string_literal(text, current)
        if parsed is None:
            return None

        literal, current = parsed
        items.append(literal)
        current = skip_ws(text, current)

        if current < len(text) and text[current] == ";":
            current = skip_ws(text, current + 1)

    if current >= len(text) or text[current] != "]" or not items:
        return None

    current = skip_ws(text, current + 1)

    if current >= len(text) or text[current] != "|":
        return None

    current = skip_ws(text, current + 1)
    if current >= len(text) or text[current] != ">":
        return None

    current = skip_ws(text, current + 1)

    if not text.startswith("String.concat", current):
        return None

    current = skip_ws(text, current + len("String.concat"))
    parsed = parse_string_literal(text, current)

    if parsed is None:
        return None

    separator, current = parsed

    if separator.value != "\n":
        return None

    value = "\n".join(item.value for item in items)
    return StringLiteral(value=value, start=index, end=current, line=items[0].line), current


def looks_like_kappa_program(text: str) -> bool:
    stripped = text.replace("\r\n", "\n").strip()

    if not stripped:
        return False

    lines = [line.strip() for line in stripped.splitlines() if line.strip()]
    if not lines:
        return False

    codeish_lines = sum(1 for line in lines if line.startswith(CODEISH_PREFIXES))

    if lines[0].startswith(("module ", "@PrivateByDefault module ")):
        return True

    return codeish_lines >= 2 and len(lines) >= 2


def extract_inline_kappa_samples_from_fs(path: Path) -> list[InlineSample]:
    text = path.read_text(encoding="utf-8", errors="replace")
    samples: list[InlineSample] = []
    seen: set[str] = set()
    index = 0
    relative_path = str(path)

    while index < len(text):
        combined = parse_string_list_concat(text, index)
        if combined is not None:
            literal, next_index = combined
            candidate = literal.value.replace("\r\n", "\n").strip()

            if looks_like_kappa_program(candidate) and candidate not in seen:
                seen.add(candidate)
                samples.append(
                    InlineSample(
                        text=candidate,
                        source_label="inline-string-list",
                        source_path=relative_path,
                        line=literal.line,
                    )
                )

            index = next_index
            continue

        parsed = parse_string_literal(text, index)
        if parsed is not None:
            literal, next_index = parsed
            candidate = literal.value.replace("\r\n", "\n").strip()

            if "\n" in candidate and looks_like_kappa_program(candidate) and candidate not in seen:
                seen.add(candidate)
                samples.append(
                    InlineSample(
                        text=candidate,
                        source_label="inline-string",
                        source_path=relative_path,
                        line=literal.line,
                    )
                )

            index = next_index
            continue

        index += 1

    return samples

