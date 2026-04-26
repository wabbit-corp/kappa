#!/usr/bin/env python3
"""Library support for the Kappa fuzzing pipeline."""

from __future__ import annotations

import hashlib
import json
import math
import random
import re
import shutil
import sqlite3
import subprocess
import sys
import tempfile
import time
from collections import Counter, defaultdict
from dataclasses import asdict, dataclass
from datetime import datetime
from pathlib import Path
from typing import Iterable

try:
    import torch
    from torch import nn
    from torch.nn import functional as F
except ModuleNotFoundError:
    torch = None
    nn = None
    F = None


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

CHUNK_SEPARATOR = "\n\1\n"
KAPPA_WORDS = {
    "_", "module", "import", "export", "as", "except", "unhide", "clarify", "term", "type", "trait",
    "ctor", "public", "private", "data", "let", "infix", "left", "right", "prefix", "postfix",
    "match", "case", "if", "then", "elif", "else", "is", "do", "pure", "handle", "return", "effect",
    "handler", "resume", "for", "in", "while", "loop", "break", "continue", "var", "this", "opaque",
    "refl", "summon", "True", "False", "Bool", "Int", "Integer", "Float", "Double", "String", "Char",
    "Unit", "Type", "Nat", "IO", "EffRow", "Universe", "Syntax", "Dict", "Maybe", "Some", "None",
    "List", "Nil", "Cons", "Eq", "Show", "main", "result",
}
IDENTITY_KEYWORD_CODES = {word: word for word in KAPPA_WORDS}
BORING_KEYWORDS = {
    "_",
    "module",
    "main",
    "let",
    "result",
    "Int",
    "String",
    "Type",
    "Unit",
    "True",
    "False",
}
KEYWORD_CODE_PATTERN = re.compile(r"k(\d+)")
TRACE_LINE_PATTERN = re.compile(
    r"^\s*(\S+)\s+(\S+)\s+(\S+)\s+->\s+(\S+)\s+changed=(true|false)(?:\s+verify=(\S+))?$"
)
TEMP_PATH_PATTERN = re.compile(r"/var/folders/[^ ]+/main\.kp")
MAIN_SYMBOL_PATTERN = re.compile(r"\bmain\.([A-Za-z_][A-Za-z0-9_]*)(\[\d+\])?")


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


@dataclass(frozen=True)
class ModelConfig:
    vocab_size: int
    embedding_size: int
    hidden_size: int
    layers: int
    dropout: float
    model: str


def require_torch() -> None:
    if torch is None or nn is None or F is None:
        raise RuntimeError("PyTorch is required for this command. Use the project venv or install torch.")


class CharRnn(nn.Module if nn is not None else object):
    def __init__(self, config: ModelConfig) -> None:
        require_torch()
        super().__init__()
        self.embedding = nn.Embedding(config.vocab_size, config.embedding_size)
        recurrent_kwargs = {
            "input_size": config.embedding_size,
            "hidden_size": config.hidden_size,
            "num_layers": config.layers,
            "dropout": config.dropout if config.layers > 1 else 0.0,
            "batch_first": True,
        }

        if config.model == "gru":
            self.rnn = nn.GRU(**recurrent_kwargs)
        elif config.model == "lstm":
            self.rnn = nn.LSTM(**recurrent_kwargs)
        else:
            raise ValueError(f"Unsupported model kind: {config.model}")

        self.projection = nn.Linear(config.hidden_size, config.vocab_size)

    def forward(self, input_ids: torch.Tensor) -> torch.Tensor:
        embedded = self.embedding(input_ids)
        output, _ = self.rnn(embedded)
        return self.projection(output)

    def sample(self, *, prime_ids: list[int], max_length: int, temperature: float, device: torch.device) -> list[int]:
        self.eval()
        with torch.no_grad():
            ids = list(prime_ids)
            hidden = None

            for prime_id in prime_ids[:-1]:
                current = torch.tensor([[prime_id]], dtype=torch.long, device=device)
                _, hidden = self.rnn(self.embedding(current), hidden)

            current = torch.tensor([[prime_ids[-1]]], dtype=torch.long, device=device)

            for _ in range(max_length):
                output, hidden = self.rnn(self.embedding(current), hidden)
                logits = self.projection(output[:, -1, :]).squeeze(0)

                if temperature <= 0.0:
                    next_id = int(torch.argmax(logits).item())
                else:
                    probabilities = F.softmax(logits / temperature, dim=0)
                    next_id = int(torch.multinomial(probabilities, 1).item())

                ids.append(next_id)
                current = torch.tensor([[next_id]], dtype=torch.long, device=device)

            return ids


def repo_root_from_script() -> Path:
    return Path(__file__).resolve().parents[1]


def resolve_path(repo_root: Path, value: str | Path) -> Path:
    path = Path(value)
    return path if path.is_absolute() else repo_root / path


def preferred_python(repo_root: Path) -> str:
    venv_python = repo_root / "artifacts/fuzzball-kappa/.venv/bin/python"
    if venv_python.exists():
        return str(venv_python)
    return sys.executable


def current_git_commit(repo_root: Path) -> str:
    try:
        result = subprocess.run(["git", "rev-parse", "HEAD"], cwd=repo_root, capture_output=True, text=True, check=True)
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
    for filename in ("expected.txt", "stderr.txt", "stdout.txt"):
        path = case_dir / filename
        if path.exists():
            signature = last_nonblank_line(path.read_text(encoding="utf-8", errors="replace"))
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
        target = stage.split(":", 1)[1]
        checkpoint, separator, backend = target.partition("@")
        args: list[str] = []
        if separator and backend:
            args.extend(["--backend", backend])
        args.extend(["--verify", checkpoint])
        return args
    raise ValueError(f"Unsupported stage: {stage}")


def build_cli_command(cli_path: Path, temp_root: Path, source_path: Path, *, stage: str, trace: bool) -> list[str]:
    command = [str(cli_path), "--source-root", str(temp_root)]
    if trace:
        command.append("--trace")
    command.extend(stage_to_args(stage))
    command.append(str(source_path))
    return command


def run_cli_source(cli_path: Path, repo_root: Path, source: str, *, stage: str, timeout_seconds: float, trace: bool = False) -> CaseRunResult:
    with tempfile.TemporaryDirectory(prefix="kappa-fuzz-case-") as temp_dir:
        temp_root = Path(temp_dir)
        source_path = temp_root / "main.kp"
        source_path.write_text(source.rstrip() + "\n", encoding="utf-8")

        try:
            result = subprocess.run(
                build_cli_command(cli_path, temp_root, source_path, stage=stage, trace=trace),
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


def terminal_signature_from_run(run: CaseRunResult) -> str:
    terminal = last_nonblank_line(run.stderr) or last_nonblank_line(run.stdout) or "<empty>"
    if run.stage != "compile":
        return f"{run.stage}: {terminal}"
    return terminal


def kind_from_bucket_name(name: str) -> str:
    return BUCKET_TO_KIND.get(name, name)


def kind_from_case_dir(case_dir: Path) -> str:
    for prefix, kind in (
        ("failure-", "failure"),
        ("crash-", "crash"),
        ("timeout-", "timeout"),
        ("diagnostic-", "diagnostic"),
        ("ok-", "ok"),
        ("success-", "ok"),
    ):
        if case_dir.name.startswith(prefix):
            return kind

    meta_kind = str(load_json(case_dir / "meta.json").get("kind", "")).strip()
    if meta_kind in {"failure", "crash", "timeout", "diagnostic", "ok", "success"}:
        return "ok" if meta_kind == "success" else meta_kind

    expected = terminal_signature_from_case(case_dir)
    if expected == "compile":
        return "timeout"
    lowered = expected.lower()
    if "stack overflow" in lowered or "unhandled exception" in lowered or "system." in lowered:
        return "crash"
    return "failure"


def discover_case_dirs(repo_root: Path, roots: list[str]) -> list[Path]:
    case_dirs: list[Path] = []

    for root_arg in roots:
        root = resolve_path(repo_root, root_arg)

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
        escapes = {"\\": "\\", '"': '"', "'": "'", "n": "\n", "r": "\r", "t": "\t", "b": "\b", "f": "\f"}
        result.append(escapes.get(next_char, next_char))
        index += 2

    return "".join(result)


def parse_string_literal(text: str, index: int) -> tuple[StringLiteral, int] | None:
    line = text.count("\n", 0, index) + 1

    if text.startswith('"""', index):
        end = text.find('"""', index + 3)
        if end < 0:
            return None
        return StringLiteral(text[index + 3 : end], index, end + 3, line), end + 3

    if text.startswith('@\"', index):
        current = index + 2
        pieces: list[str] = []
        while current < len(text):
            if text.startswith('""', current):
                pieces.append('"')
                current += 2
                continue
            if text[current] == '"':
                return StringLiteral("".join(pieces), index, current + 1, line), current + 1
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
        elif char == "\\":
            escaped = True
        elif char == '"':
            payload = text[index + 1 : current]
            return StringLiteral(decode_standard_string(payload), index, current + 1, line), current + 1
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

    return StringLiteral("\n".join(item.value for item in items), index, current, items[0].line), current


def looks_like_kappa_program(text: str) -> bool:
    stripped = text.replace("\r\n", "\n").strip()
    if not stripped:
        return False

    lines = [line.strip() for line in stripped.splitlines() if line.strip()]
    if not lines:
        return False
    if lines[0].startswith(("module ", "@PrivateByDefault module ")):
        return True

    return sum(1 for line in lines if line.startswith(CODEISH_PREFIXES)) >= 2 and len(lines) >= 2


def extract_inline_kappa_samples_from_fs(path: Path) -> list[InlineSample]:
    text = path.read_text(encoding="utf-8", errors="replace")
    samples: list[InlineSample] = []
    seen: set[str] = set()
    index = 0

    while index < len(text):
        combined = parse_string_list_concat(text, index)
        if combined is not None:
            literal, next_index = combined
            candidate = literal.value.replace("\r\n", "\n").strip()
            if looks_like_kappa_program(candidate) and candidate not in seen:
                seen.add(candidate)
                samples.append(InlineSample(candidate, "inline-string-list", str(path), literal.line))
            index = next_index
            continue

        parsed = parse_string_literal(text, index)
        if parsed is not None:
            literal, next_index = parsed
            candidate = literal.value.replace("\r\n", "\n").strip()
            if "\n" in candidate and looks_like_kappa_program(candidate) and candidate not in seen:
                seen.add(candidate)
                samples.append(InlineSample(candidate, "inline-string", str(path), literal.line))
            index = next_index
            continue

        index += 1

    return samples


def source_files(roots: Iterable[Path], include_suite: bool) -> list[Path]:
    files: list[Path] = []
    for root in roots:
        if root.is_file():
            if root.suffix == ".kp" or (include_suite and root.name == "suite.ktest"):
                files.append(root)
            continue
        if not root.exists():
            continue
        files.extend(root.rglob("*.kp"))
        if include_suite:
            files.extend(root.rglob("suite.ktest"))
    return sorted(set(path.resolve() for path in files))


def load_weighted_sample_records(path: Path) -> list[dict]:
    records: list[dict] = []
    with path.open(encoding="utf-8") as handle:
        for line in handle:
            if line.strip():
                records.append(json.loads(line))
    return records


def is_identifier_start(char: str) -> bool:
    return char == "_" or char.isalpha()


def is_identifier_part(char: str) -> bool:
    return char == "_" or char.isalnum()


def read_backtick_identifier(text: str, index: int) -> tuple[str, int]:
    end = index + 1
    while end < len(text):
        if text[end] == "`":
            return text[index : end + 1], end + 1
        end += 1
    return text[index:end], end


def read_identifier(text: str, index: int) -> tuple[str, int]:
    end = index + 1
    while end < len(text) and is_identifier_part(text[end]):
        end += 1
    return text[index:end], end


def remove_block_comments(text: str) -> str:
    result: list[str] = []
    index = 0
    depth = 0
    while index < len(text):
        if depth > 0:
            if text.startswith("{-", index):
                depth += 1
                index += 2
            elif text.startswith("-}", index):
                depth -= 1
                index += 2
            else:
                if text[index] == "\n":
                    result.append("\n")
                index += 1
        elif text.startswith("{-", index):
            depth = 1
            index += 2
        else:
            result.append(text[index])
            index += 1
    return "".join(result)


def collect_keyword_frequency(source: str) -> Counter[str]:
    frequency: Counter[str] = Counter()
    for match in re.finditer(r"`[^`\n]*`|[A-Za-z_][A-Za-z0-9_]*", source):
        word = match.group(0)
        if word in KAPPA_WORDS:
            frequency[word] += 1
    return frequency


def extract_kappa_keywords(source: str) -> set[str]:
    keywords: set[str] = set()
    for match in re.finditer(r"`[^`\n]*`|[A-Za-z_][A-Za-z0-9_]*", remove_block_comments(source)):
        word = match.group(0)
        if word in KAPPA_WORDS:
            keywords.add(word)
    return keywords


def build_keyword_codes(raw_sources: Iterable[str]) -> dict[str, str]:
    frequency: Counter[str] = Counter()
    for source in raw_sources:
        frequency.update(collect_keyword_frequency(remove_block_comments(source)))
    ordered = sorted(KAPPA_WORDS, key=lambda word: (-frequency[word], word))
    return {word: f"k{index}" for index, word in enumerate(ordered)}


def normalize_name(name: str, identifiers: dict[str, str], keyword_codes: dict[str, str]) -> str:
    if name in keyword_codes:
        return keyword_codes[name]
    if name not in identifiers:
        identifiers[name] = f"{'I' if name[0].isupper() else 'i'}{len(identifiers)}"
    return identifiers[name]


def parse_string_opener(text: str, index: int) -> tuple[str, str, int] | None:
    hash_index = index
    while hash_index < len(text) and text[hash_index] == "#":
        hash_index += 1
    if hash_index >= len(text) or text[hash_index] != '"':
        return None
    hashes = text[index:hash_index]
    if text.startswith('"""', hash_index):
        return hashes, '"""', hash_index + 3
    return hashes, '"', hash_index + 1


def compact_literal_segment(segment: str) -> str:
    if not segment:
        return ""
    if segment.isspace():
        return " "
    return "s"


def find_interpolation_end(text: str, index: int) -> int:
    depth = 0
    current = index
    in_string = False
    in_char = False
    in_backtick = False
    escaped = False
    while current < len(text):
        char = text[current]
        if escaped:
            escaped = False
        elif char == "\\" and (in_string or in_char):
            escaped = True
        elif in_string:
            if char == '"':
                in_string = False
        elif in_char:
            if char == "'":
                in_char = False
        elif in_backtick:
            if char == "`":
                in_backtick = False
        elif char == '"':
            in_string = True
        elif char == "'":
            in_char = True
        elif char == "`":
            in_backtick = True
        elif char == "{":
            depth += 1
        elif char == "}":
            if depth == 0:
                return current
            depth -= 1
        current += 1
    return current


def normalize_interpolation_body(body: str, identifiers: dict[str, str], keyword_codes: dict[str, str]) -> str:
    return normalize_text(body, identifiers, keyword_codes, strip_comments=False).strip()


def normalize_string_literal(
    text: str,
    index: int,
    prefix: str,
    hashes: str,
    quote: str,
    content_start: int,
    identifiers: dict[str, str],
    keyword_codes: dict[str, str],
    interpolated: bool,
) -> tuple[str, int]:
    close = quote + hashes
    current = content_start
    literal_start = current
    pieces = [prefix, hashes, quote]
    raw_interpolation_opener = hashes + "{"

    while current < len(text):
        if text.startswith(close, current):
            pieces.append(compact_literal_segment(text[literal_start:current]))
            pieces.append(close)
            return "".join(pieces), current + len(close)

        if quote == '"' and text[current] == "\\":
            current = min(len(text), current + 2)
            continue

        if interpolated and not hashes and text.startswith("${", current):
            pieces.append(compact_literal_segment(text[literal_start:current]))
            expression_start = current + 2
            expression_end = find_interpolation_end(text, expression_start)
            body = normalize_interpolation_body(text[expression_start:expression_end], identifiers, keyword_codes)
            pieces.extend(["${", body, "}"])
            current = min(len(text), expression_end + 1)
            literal_start = current
            continue

        if interpolated and not hashes and current + 1 < len(text) and text[current] == "$":
            next_char = text[current + 1]
            if next_char == "`":
                pieces.append(compact_literal_segment(text[literal_start:current]))
                name, next_index = read_backtick_identifier(text, current + 1)
                pieces.extend(["$", normalize_name(name, identifiers, keyword_codes)])
                current = next_index
                literal_start = current
                continue
            if is_identifier_start(next_char):
                pieces.append(compact_literal_segment(text[literal_start:current]))
                name, next_index = read_identifier(text, current + 1)
                pieces.extend(["$", normalize_name(name, identifiers, keyword_codes)])
                current = next_index
                literal_start = current
                continue

        if interpolated and hashes and text.startswith(raw_interpolation_opener, current):
            pieces.append(compact_literal_segment(text[literal_start:current]))
            expression_start = current + len(raw_interpolation_opener)
            expression_end = find_interpolation_end(text, expression_start)
            body = normalize_interpolation_body(text[expression_start:expression_end], identifiers, keyword_codes)
            pieces.extend([raw_interpolation_opener, body, "}"])
            current = min(len(text), expression_end + 1)
            literal_start = current
            continue

        current += 1

    pieces.append(compact_literal_segment(text[literal_start:current]))
    return "".join(pieces), current


def normalize_text(text: str, identifiers: dict[str, str], keyword_codes: dict[str, str], *, strip_comments: bool) -> str:
    result: list[str] = []
    index = 0

    while index < len(text):
        current = text[index]

        if strip_comments and current == "-" and index + 1 < len(text) and text[index + 1] == "-":
            line_end = text.find("\n", index)
            if line_end < 0:
                break
            result.append("\n")
            index = line_end + 1
        elif current == "`":
            name, next_index = read_backtick_identifier(text, index)
            opener = parse_string_opener(text, next_index)
            if opener is not None:
                hashes, quote, content_start = opener
                prefix = normalize_name(name, identifiers, keyword_codes)
                normalized, index = normalize_string_literal(
                    text, next_index, prefix, hashes, quote, content_start, identifiers, keyword_codes, interpolated=True
                )
                result.append(normalized)
            else:
                result.append(normalize_name(name, identifiers, keyword_codes))
                index = next_index
        elif is_identifier_start(current):
            name, next_index = read_identifier(text, index)
            opener = parse_string_opener(text, next_index)
            if opener is not None:
                hashes, quote, content_start = opener
                prefix = normalize_name(name, identifiers, keyword_codes)
                normalized, index = normalize_string_literal(
                    text, next_index, prefix, hashes, quote, content_start, identifiers, keyword_codes, interpolated=True
                )
                result.append(normalized)
            else:
                result.append(normalize_name(name, identifiers, keyword_codes))
                index = next_index
        elif current == "#":
            opener = parse_string_opener(text, index)
            if opener is not None:
                hashes, quote, content_start = opener
                normalized, index = normalize_string_literal(
                    text, index, "", hashes, quote, content_start, identifiers, keyword_codes, interpolated=False
                )
                result.append(normalized)
            else:
                result.append(current)
                index += 1
        elif current == '"':
            opener = parse_string_opener(text, index)
            if opener is not None:
                hashes, quote, content_start = opener
                normalized, index = normalize_string_literal(
                    text, index, "", hashes, quote, content_start, identifiers, keyword_codes, interpolated=False
                )
                result.append(normalized)
            else:
                result.append(current)
                index += 1
        elif current == "'":
            end = index + 1
            escaped = False
            while end < len(text):
                if escaped:
                    escaped = False
                elif text[end] == "\\":
                    escaped = True
                elif text[end] == "'":
                    end += 1
                    break
                end += 1
            result.append("'c'")
            index = end
        else:
            result.append(current)
            index += 1

    return "".join(result)


def normalize_source(source: str, keyword_codes: dict[str, str]) -> str:
    source = remove_block_comments(source).replace("\r\n", "\n").strip()
    identifiers: dict[str, str] = {}
    normalized = normalize_text(source, identifiers, keyword_codes, strip_comments=True)
    normalized_lines = [line.rstrip() for line in normalized.split("\n")]
    while normalized_lines and not normalized_lines[0].strip():
        normalized_lines.pop(0)
    while normalized_lines and not normalized_lines[-1].strip():
        normalized_lines.pop()
    return "\n".join(normalized_lines)


def alpha_normalize_source(source: str) -> str:
    return normalize_source(source, IDENTITY_KEYWORD_CODES)


def alpha_source_hash(source: str) -> str:
    return hashlib.sha1(alpha_normalize_source(source).encode("utf-8")).hexdigest()


def canonicalize_terminal_signature(signature: str) -> str:
    signature = TEMP_PATH_PATTERN.sub("<temp>/main.kp", signature)
    return MAIN_SYMBOL_PATTERN.sub(lambda match: f"main.<sym>{match.group(2) or ''}", signature)


def load_corpus(files: list[Path]) -> str:
    chunks: list[str] = []
    for path in files:
        text = path.read_text(encoding="utf-8").replace("\r\n", "\n").strip()
        if text:
            chunks.append(text)
    return CHUNK_SEPARATOR.join(chunks) + CHUNK_SEPARATOR


def load_normalized_corpus(files: list[Path], keyword_codes: dict[str, str]) -> str:
    chunks: list[str] = []
    for path in files:
        normalized = normalize_source(path.read_text(encoding="utf-8"), keyword_codes)
        if normalized:
            chunks.append(normalized)
    return CHUNK_SEPARATOR.join(dict.fromkeys(chunks)) + CHUNK_SEPARATOR


def weighted_repeat_count(weight: float) -> int:
    normalized = max(0.0, min(1.0, float(weight)))
    return 1 + int(round(normalized * 7.0))


def normalize_trace_hash_value(trace_hash_value: str, trace_step_count: int) -> str:
    if not trace_hash_value or trace_step_count <= 0 or trace_hash_value == hashlib.sha1(b"").hexdigest():
        return "<no-trace>"
    return trace_hash_value


def compute_normalized_sample_weights(
    samples: list[dict],
    *,
    trace_records_by_sample: dict[str, list[dict]],
    preferred_commit: str,
) -> dict[str, dict]:
    eligible_samples = [sample for sample in samples if sample.get("eligible", False)]
    if not eligible_samples:
        return {}

    syntax_hash_by_sample = {sample["sample_sha1"]: alpha_source_hash(str(sample["text"])) for sample in eligible_samples}
    syntax_group_sizes = Counter(syntax_hash_by_sample.values())

    keywords_by_sample: dict[str, set[str]] = {}
    keyword_document_frequency: Counter[str] = Counter()
    for sample in eligible_samples:
        sample_sha1_value = str(sample["sample_sha1"])
        keywords = {keyword for keyword in extract_kappa_keywords(str(sample["text"])) if keyword not in BORING_KEYWORDS}
        keywords_by_sample[sample_sha1_value] = keywords
        keyword_document_frequency.update(keywords)

    normalized_trace_groups: list[str] = []
    traces_for_sample: dict[str, list[str]] = {}
    for sample in eligible_samples:
        sample_sha1_value = str(sample["sample_sha1"])
        trace_records = trace_records_by_sample.get(sample_sha1_value, [])
        preferred = [record for record in trace_records if record.get("compiler_commit") == preferred_commit]
        effective = preferred if preferred else trace_records
        normalized_hashes = sorted(
            {
                normalize_trace_hash_value(str(record.get("trace_hash", "")), int(record.get("trace_step_count", 0)))
                for record in effective
            }
        )
        if not normalized_hashes:
            normalized_hashes = ["<no-trace>"]
        traces_for_sample[sample_sha1_value] = normalized_hashes
        normalized_trace_groups.extend(normalized_hashes)

    trace_group_sizes = Counter(normalized_trace_groups)
    default_keyword_score = 1.0 / math.log(2.0)

    raw_scores: dict[str, dict] = {}
    max_raw_score = 0.0
    for sample in eligible_samples:
        sample_sha1_value = str(sample["sample_sha1"])
        trace_hashes = traces_for_sample[sample_sha1_value]
        trace_group_size = min(trace_group_sizes[trace_hash] for trace_hash in trace_hashes)
        syntax_group_size = syntax_group_sizes[syntax_hash_by_sample[sample_sha1_value]]
        novelty = 1.0 / max(1, trace_group_size, syntax_group_size)

        keyword_score = 0.0
        for keyword in sorted(keywords_by_sample[sample_sha1_value]):
            other_samples = max(0, keyword_document_frequency[keyword] - 1)
            keyword_score += 1.0 / math.log(2.0 + other_samples)
        if keyword_score == 0.0:
            keyword_score = default_keyword_score

        raw_score = novelty * keyword_score
        max_raw_score = max(max_raw_score, raw_score)
        raw_scores[sample_sha1_value] = {
            "raw_weight": raw_score,
            "trace_group_size": trace_group_size,
            "syntax_group_size": syntax_group_size,
            "keyword_score": keyword_score,
            "keywords": sorted(keywords_by_sample[sample_sha1_value]),
            "normalized_trace_hashes": trace_hashes,
            "syntax_hash": syntax_hash_by_sample[sample_sha1_value],
        }

    scale = max_raw_score or 1.0
    for sample_sha1_value, details in raw_scores.items():
        details["weight"] = details["raw_weight"] / scale
    return raw_scores


def load_weighted_corpus(records: list[dict], keyword_codes: dict[str, str], raw_source: bool) -> tuple[str, dict]:
    chunks: list[str] = []
    summary = {"record_count": len(records), "expanded_chunk_count": 0, "max_weight": 0.0}
    for record in records:
        text = str(record["text"])
        weight = float(record.get("weight", 1.0))
        repeat = weighted_repeat_count(weight)
        chunk = text.replace("\r\n", "\n").strip() if raw_source else normalize_source(text, keyword_codes)
        if not chunk:
            continue
        chunks.extend([chunk] * repeat)
        summary["expanded_chunk_count"] += repeat
        summary["max_weight"] = max(summary["max_weight"], weight)
    return CHUNK_SEPARATOR.join(chunks) + CHUNK_SEPARATOR, summary


def decode_keywords(text: str, code_to_keyword: dict[str, str]) -> str:
    return KEYWORD_CODE_PATTERN.sub(lambda match: code_to_keyword.get(match.group(0), match.group(0)), text)


def encode_corpus(corpus: str) -> tuple[dict[str, int], list[str], torch.Tensor]:
    require_torch()
    vocab = sorted(set(corpus))
    char_to_id = {char: index for index, char in enumerate(vocab)}
    ids = torch.tensor([char_to_id[char] for char in corpus], dtype=torch.long)
    return char_to_id, vocab, ids


def random_batch(data: torch.Tensor, batch_size: int, sequence_length: int, device: torch.device) -> tuple[torch.Tensor, torch.Tensor]:
    require_torch()
    max_start = data.numel() - sequence_length - 1
    if max_start <= 0:
        raise ValueError("Corpus is too small for the requested sequence length.")
    starts = torch.randint(0, max_start, (batch_size,))
    inputs = torch.stack([data[start : start + sequence_length] for start in starts]).to(device)
    targets = torch.stack([data[start + 1 : start + sequence_length + 1] for start in starts]).to(device)
    return inputs, targets


def decode(ids: Iterable[int], vocab: list[str]) -> str:
    return "".join(vocab[index] for index in ids)


def pick_device(requested: str) -> torch.device:
    require_torch()
    if requested != "auto":
        return torch.device(requested)
    if torch.cuda.is_available():
        return torch.device("cuda")
    mps = getattr(torch.backends, "mps", None)
    if mps is not None and mps.is_available():
        return torch.device("mps")
    return torch.device("cpu")


def write_samples(
    *,
    model: CharRnn,
    vocab: list[str],
    char_to_id: dict[str, int],
    keyword_codes: dict[str, str],
    out_dir: Path,
    sample_count: int,
    sample_length: int,
    temperature: float,
    prime: str,
    device: torch.device,
) -> None:
    require_torch()
    model = model.to(device)
    code_to_keyword = {code: keyword for keyword, code in keyword_codes.items()}
    encoded_prime = normalize_source(prime, keyword_codes) if keyword_codes else prime
    prime_ids = [char_to_id[char] for char in encoded_prime if char in char_to_id] or [char_to_id["\n"]]

    samples = []
    encoded_samples = []
    for index in range(sample_count):
        ids = model.sample(prime_ids=prime_ids, max_length=sample_length, temperature=temperature, device=device)
        encoded_text = decode(ids, vocab).split("\1", 1)[0].strip()
        text = decode_keywords(encoded_text, code_to_keyword)
        encoded_samples.append(f"-- sample {index + 1}\n{encoded_text}\n")
        samples.append(f"-- sample {index + 1}\n{text}\n")

    (out_dir / "samples.kp.txt").write_text("\n".join(samples), encoding="utf-8")
    (out_dir / "samples.encoded.txt").write_text("\n".join(encoded_samples), encoding="utf-8")


def ensure_schema(connection: sqlite3.Connection) -> None:
    connection.executescript(
        """
        CREATE TABLE IF NOT EXISTS samples (
            sample_sha1 TEXT PRIMARY KEY,
            text TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS provenance (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            sample_sha1 TEXT NOT NULL,
            provenance_kind TEXT NOT NULL,
            source_group TEXT NOT NULL,
            source_path TEXT NOT NULL,
            used_for_training INTEGER NOT NULL DEFAULT 0,
            metadata_json TEXT NOT NULL,
            UNIQUE(sample_sha1, provenance_kind, source_path)
        );
        CREATE TABLE IF NOT EXISTS test_results (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            sample_sha1 TEXT NOT NULL,
            compiler_commit TEXT NOT NULL,
            result_kind TEXT NOT NULL,
            returncode INTEGER,
            timed_out INTEGER NOT NULL DEFAULT 0,
            terminal_signature TEXT NOT NULL,
            artifact_path TEXT NOT NULL,
            run_path TEXT NOT NULL,
            meta_json TEXT NOT NULL,
            UNIQUE(sample_sha1, compiler_commit, result_kind, artifact_path)
        );

        CREATE TABLE IF NOT EXISTS trace_results (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            sample_sha1 TEXT NOT NULL,
            compiler_commit TEXT NOT NULL,
            stage TEXT NOT NULL,
            trace_hash TEXT NOT NULL,
            trace_step_count INTEGER NOT NULL,
            trace_steps_json TEXT NOT NULL,
            terminal_signature TEXT NOT NULL,
            returncode INTEGER,
            timed_out INTEGER NOT NULL DEFAULT 0,
            UNIQUE(sample_sha1, compiler_commit, stage, trace_hash, terminal_signature)
        );
        """
    )
    connection.commit()


def upsert_sample(connection: sqlite3.Connection, sha1: str, text: str) -> None:
    connection.execute(
        """
        INSERT INTO samples (sample_sha1, text)
        VALUES (?, ?)
        ON CONFLICT(sample_sha1) DO UPDATE SET text = excluded.text
        """,
        (sha1, text),
    )


def add_provenance(
    connection: sqlite3.Connection,
    *,
    sample_sha1_value: str,
    provenance_kind: str,
    source_group: str,
    source_path: str,
    used_for_training: bool,
    metadata: dict,
) -> None:
    connection.execute(
        """
        INSERT OR IGNORE INTO provenance (
            sample_sha1, provenance_kind, source_group, source_path, used_for_training, metadata_json
        ) VALUES (?, ?, ?, ?, ?, ?)
        """,
        (
            sample_sha1_value,
            provenance_kind,
            source_group,
            source_path,
            1 if used_for_training else 0,
            json.dumps(metadata, sort_keys=True),
        ),
    )


def add_test_result(
    connection: sqlite3.Connection,
    *,
    sample_sha1_value: str,
    compiler_commit: str,
    result_kind: str,
    returncode: int | None,
    timed_out: bool,
    terminal_signature: str,
    artifact_path: str,
    run_path: str,
    metadata: dict,
) -> None:
    connection.execute(
        """
        INSERT OR IGNORE INTO test_results (
            sample_sha1, compiler_commit, result_kind, returncode, timed_out, terminal_signature, artifact_path, run_path, meta_json
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """,
        (
            sample_sha1_value,
            compiler_commit,
            result_kind,
            returncode,
            1 if timed_out else 0,
            terminal_signature,
            artifact_path,
            run_path,
            json.dumps(metadata, sort_keys=True),
        ),
    )


def add_trace_result(
    connection: sqlite3.Connection,
    *,
    sample_sha1_value: str,
    compiler_commit: str,
    stage: str,
    trace_hash_value: str,
    trace_step_count: int,
    trace_steps: list[str],
    terminal_signature: str,
    returncode: int | None,
    timed_out: bool,
) -> None:
    connection.execute(
        """
        INSERT OR IGNORE INTO trace_results (
            sample_sha1, compiler_commit, stage, trace_hash, trace_step_count, trace_steps_json, terminal_signature, returncode, timed_out
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """,
        (
            sample_sha1_value,
            compiler_commit,
            stage,
            trace_hash_value,
            trace_step_count,
            json.dumps(trace_steps, sort_keys=True),
            terminal_signature,
            returncode,
            1 if timed_out else 0,
        ),
    )


def add_static_corpus_root(connection: sqlite3.Connection, repo_root: Path, root: Path, source_group: str) -> int:
    if not root.exists():
        return 0
    added = 0
    for path in sorted(root.rglob("*.kp")):
        text = path.read_text(encoding="utf-8", errors="replace")
        sha1 = sample_sha1(text)
        upsert_sample(connection, sha1, text)
        add_provenance(
            connection,
            sample_sha1_value=sha1,
            provenance_kind="corpus-file",
            source_group=source_group,
            source_path=repo_relative(repo_root, path),
            used_for_training=True,
            metadata={},
        )
        added += 1
    return added


def add_inline_test_sources(connection: sqlite3.Connection, repo_root: Path, tests_root: Path) -> int:
    if not tests_root.exists():
        return 0
    added = 0
    for path in sorted(tests_root.rglob("*.fs")):
        for sample in extract_inline_kappa_samples_from_fs(path):
            sha1 = sample_sha1(sample.text)
            upsert_sample(connection, sha1, sample.text)
            add_provenance(
                connection,
                sample_sha1_value=sha1,
                provenance_kind="inline-test",
                source_group="inline-tests",
                source_path=f"{repo_relative(repo_root, path)}:{sample.line}",
                used_for_training=True,
                metadata={"source_label": sample.source_label},
            )
            added += 1
    return added


def find_artifact_roots(repo_root: Path, name: str) -> list[Path]:
    artifacts_root = repo_root / "artifacts"
    if not artifacts_root.exists():
        return []
    return sorted(path for path in artifacts_root.rglob(name) if path.is_dir())


def scan_fuzz_run(connection: sqlite3.Connection, repo_root: Path, run_root: Path) -> int:
    run_metadata = load_json(run_root / "run.json")
    compiler_commit = str(run_metadata.get("compiler_commit", "unknown"))
    added = 0
    for bucket in ["failures", "crashes", "diagnostics", "successes", "timeouts"]:
        bucket_root = run_root / bucket
        if not bucket_root.exists():
            continue
        for case_dir in sorted(path for path in bucket_root.iterdir() if path.is_dir() and (path / "main.kp").exists()):
            text = (case_dir / "main.kp").read_text(encoding="utf-8", errors="replace")
            sha1 = sample_sha1(text)
            upsert_sample(connection, sha1, text)
            meta = load_json(case_dir / "meta.json")
            case_commit = str(meta.get("compiler_commit", compiler_commit))
            result_kind = str(meta.get("kind", bucket.rstrip("s")))
            add_provenance(
                connection,
                sample_sha1_value=sha1,
                provenance_kind="fuzz-artifact",
                source_group=bucket,
                source_path=repo_relative(repo_root, case_dir),
                used_for_training=False,
                metadata={"run_root": repo_relative(repo_root, run_root)},
            )
            add_test_result(
                connection,
                sample_sha1_value=sha1,
                compiler_commit=case_commit,
                result_kind=result_kind,
                returncode=meta.get("returncode"),
                timed_out=bool(meta.get("timed_out", False)),
                terminal_signature=terminal_signature_from_case(case_dir),
                artifact_path=repo_relative(repo_root, case_dir),
                run_path=repo_relative(repo_root, run_root),
                metadata=meta,
            )
            added += 1
    return added


def add_pending_failures(connection: sqlite3.Connection, repo_root: Path, pending_root: Path) -> int:
    if not pending_root.exists():
        return 0
    added = 0
    for case_dir in sorted(path for path in pending_root.iterdir() if path.is_dir() and (path / "main.kp").exists()):
        text = (case_dir / "main.kp").read_text(encoding="utf-8", errors="replace")
        sha1 = sample_sha1(text)
        upsert_sample(connection, sha1, text)
        add_provenance(
            connection,
            sample_sha1_value=sha1,
            provenance_kind="pending-failure",
            source_group="pending-failures",
            source_path=repo_relative(repo_root, case_dir),
            used_for_training=False,
            metadata=load_json(case_dir / "cluster.json"),
        )
        added += 1
    return added


def export_jsonl(connection: sqlite3.Connection, jsonl_path: Path) -> dict:
    sample_rows = connection.execute("SELECT sample_sha1, text FROM samples ORDER BY sample_sha1").fetchall()
    provenance_rows = connection.execute(
        "SELECT sample_sha1, provenance_kind, source_group, source_path, used_for_training, metadata_json FROM provenance ORDER BY sample_sha1, source_group, source_path"
    ).fetchall()
    result_rows = connection.execute(
        "SELECT sample_sha1, compiler_commit, result_kind, returncode, timed_out, terminal_signature, artifact_path, run_path, meta_json FROM test_results ORDER BY sample_sha1, compiler_commit, result_kind, artifact_path"
    ).fetchall()
    trace_rows = connection.execute(
        "SELECT sample_sha1, compiler_commit, stage, trace_hash, trace_step_count, trace_steps_json, terminal_signature, returncode, timed_out FROM trace_results ORDER BY sample_sha1, compiler_commit, stage, trace_hash"
    ).fetchall()

    provenance_by_sample: dict[str, list[dict]] = defaultdict(list)
    for row in provenance_rows:
        provenance_by_sample[row[0]].append(
            {
                "kind": row[1],
                "source_group": row[2],
                "source_path": row[3],
                "used_for_training": bool(row[4]),
                "metadata": json.loads(row[5]),
            }
        )

    results_by_sample: dict[str, list[dict]] = defaultdict(list)
    for row in result_rows:
        results_by_sample[row[0]].append(
            {
                "compiler_commit": row[1],
                "result_kind": row[2],
                "returncode": row[3],
                "timed_out": bool(row[4]),
                "terminal_signature": row[5],
                "artifact_path": row[6],
                "run_path": row[7],
                "metadata": json.loads(row[8]),
            }
        )

    traces_by_sample: dict[str, list[dict]] = defaultdict(list)
    for row in trace_rows:
        traces_by_sample[row[0]].append(
            {
                "compiler_commit": row[1],
                "stage": row[2],
                "trace_hash": row[3],
                "trace_step_count": row[4],
                "trace_steps": json.loads(row[5]),
                "terminal_signature": row[6],
                "returncode": row[7],
                "timed_out": bool(row[8]),
            }
        )

    jsonl_path.parent.mkdir(parents=True, exist_ok=True)
    with jsonl_path.open("w", encoding="utf-8") as handle:
        for sha1, text in sample_rows:
            provenance = provenance_by_sample[sha1]
            results = results_by_sample[sha1]
            training_groups = sorted({entry["source_group"] for entry in provenance if entry["used_for_training"]})
            handle.write(
                json.dumps(
                    {
                        "sample_sha1": sha1,
                        "text": text,
                        "training_groups": training_groups,
                        "provenance": provenance,
                        "test_results": results,
                        "trace_results": traces_by_sample[sha1],
                    },
                    sort_keys=True,
                )
                + "\n"
            )

    return {
        "sample_count": len(sample_rows),
        "training_sample_count": sum(
            1 for sha1, _text in sample_rows if any(entry["used_for_training"] for entry in provenance_by_sample[sha1])
        ),
        "tested_sample_count": sum(1 for sha1, _text in sample_rows if results_by_sample[sha1]),
    }


def update_corpus_store(repo_root: Path, *, db_path: Path, jsonl_path: Path) -> dict:
    db_path.parent.mkdir(parents=True, exist_ok=True)
    connection = sqlite3.connect(db_path)
    try:
        ensure_schema(connection)
        counts = {
            "fixtures": add_static_corpus_root(connection, repo_root, repo_root / "tests/Kappa.Compiler.Tests/Fixtures", "fixtures"),
            "new_tests": add_static_corpus_root(connection, repo_root, repo_root / "new-tests", "new-tests"),
            "inline_tests": add_inline_test_sources(connection, repo_root, repo_root / "tests"),
            "recycled_seeds": sum(add_static_corpus_root(connection, repo_root, root, "recycled-seeds") for root in find_artifact_roots(repo_root, "recycled-seeds")),
            "crash_boosted_seeds": sum(add_static_corpus_root(connection, repo_root, root, "crash-boosted-seeds") for root in find_artifact_roots(repo_root, "crash-boosted-seeds")),
            "pending_failures": add_pending_failures(connection, repo_root, repo_root / "pending-failures"),
            "fuzz_cases": sum(scan_fuzz_run(connection, repo_root, root) for root in sorted(path for path in (repo_root / "artifacts").rglob("fuzz-run*") if path.is_dir())),
        }
        connection.commit()
        export_summary = export_jsonl(connection, jsonl_path)
    finally:
        connection.close()
    return {"db": str(db_path), "jsonl": str(jsonl_path), "counts": counts, "export": export_summary}


def export_weighted_training_samples(repo_root: Path, *, db_path: Path, out_path: Path, preferred_commit: str | None) -> dict:
    preferred_commit = preferred_commit or current_git_commit(repo_root)

    connection = sqlite3.connect(db_path)
    connection.row_factory = sqlite3.Row
    try:
        sample_rows = list(connection.execute("SELECT sample_sha1, text FROM samples ORDER BY sample_sha1"))
        provenance_rows = list(connection.execute("SELECT sample_sha1, provenance_kind, source_group, source_path, used_for_training, metadata_json FROM provenance ORDER BY sample_sha1, source_group, source_path"))
        result_rows = list(connection.execute("SELECT sample_sha1, compiler_commit, result_kind, terminal_signature, meta_json FROM test_results ORDER BY sample_sha1, compiler_commit, result_kind"))
        trace_rows = list(connection.execute("SELECT sample_sha1, compiler_commit, stage, trace_hash, trace_step_count FROM trace_results ORDER BY sample_sha1, compiler_commit, stage"))
    finally:
        connection.close()

    provenance_by_sample: dict[str, list[dict]] = defaultdict(list)
    for row in provenance_rows:
        metadata = json.loads(row["metadata_json"])
        record = {
            "kind": row["provenance_kind"],
            "source_group": row["source_group"],
            "source_path": row["source_path"],
            "used_for_training": bool(row["used_for_training"]),
            "metadata": metadata,
        }
        provenance_by_sample[row["sample_sha1"]].append(record)

    trace_records_by_sample: dict[str, list[dict]] = defaultdict(list)
    for row in trace_rows:
        record = {
            "compiler_commit": row["compiler_commit"],
            "stage": row["stage"],
            "trace_hash": row["trace_hash"],
            "trace_step_count": row["trace_step_count"],
        }
        trace_records_by_sample[row["sample_sha1"]].append(record)

    test_result_counts: Counter[str] = Counter(row["sample_sha1"] for row in result_rows)

    sample_inputs: list[dict] = []
    for row in sample_rows:
        sample_sha1_value = row["sample_sha1"]
        provenance = provenance_by_sample[sample_sha1_value]
        has_static_training = any(entry["used_for_training"] for entry in provenance)
        has_pending_failure = any(entry["kind"] == "pending-failure" for entry in provenance)
        has_test_results = test_result_counts[sample_sha1_value] > 0
        sample_inputs.append(
            {
                "sample_sha1": sample_sha1_value,
                "text": row["text"],
                "eligible": has_static_training or has_pending_failure or has_test_results,
                "has_static_training": has_static_training,
                "has_pending_failure": has_pending_failure,
                "test_result_count": test_result_counts[sample_sha1_value],
            }
        )

    weights_by_sample = compute_normalized_sample_weights(
        sample_inputs,
        trace_records_by_sample=trace_records_by_sample,
        preferred_commit=preferred_commit,
    )

    out_path.parent.mkdir(parents=True, exist_ok=True)
    exported = 0
    top_weights: list[tuple[float, str]] = []
    with out_path.open("w", encoding="utf-8") as handle:
        for sample in sample_inputs:
            sample_sha1_value = str(sample["sample_sha1"])
            if sample_sha1_value not in weights_by_sample:
                continue
            weight_record = weights_by_sample[sample_sha1_value]
            weight = float(weight_record["weight"])

            handle.write(
                json.dumps(
                    {
                        "sample_sha1": sample_sha1_value,
                        "weight": round(weight, 4),
                        "text": sample["text"],
                        "has_static_training": bool(sample["has_static_training"]),
                        "has_pending_failure": bool(sample["has_pending_failure"]),
                        "trace_group_size": int(weight_record["trace_group_size"]),
                        "syntax_group_size": int(weight_record["syntax_group_size"]),
                        "normalized_trace_hashes": list(weight_record["normalized_trace_hashes"]),
                        "syntax_hash": str(weight_record["syntax_hash"]),
                        "keywords": list(weight_record["keywords"]),
                        "keyword_score": round(float(weight_record["keyword_score"]), 6),
                        "raw_weight": round(float(weight_record["raw_weight"]), 8),
                        "test_result_count": int(sample["test_result_count"]),
                    },
                    sort_keys=True,
                )
                + "\n"
            )
            exported += 1
            top_weights.append((weight, sample_sha1_value))

    top_weights.sort(reverse=True)
    return {
        "db": str(db_path),
        "out": str(out_path),
        "preferred_commit": preferred_commit,
        "exported": exported,
        "top_weighted_samples": [{"sample_sha1": sha1, "weight": round(weight, 4)} for weight, sha1 in top_weights[:10]],
    }


def reset_fuzz_state(repo_root: Path, *, corpus_db: Path, model_dir: Path, pending_dir: Path) -> dict:
    removed: list[str] = []

    if pending_dir.exists():
        for child in sorted(pending_dir.iterdir()):
            if child.is_dir():
                shutil.rmtree(child)
            else:
                child.unlink()
            removed.append(str(child))

    for path in [corpus_db, corpus_db.with_suffix(".jsonl"), corpus_db.parent / "weighted-training-samples.jsonl"]:
        if path.exists():
            path.unlink()
            removed.append(str(path))

    if model_dir.exists():
        for child in sorted(model_dir.iterdir()):
            if child.is_dir() and (child.name.startswith("fuzz-run-") or child.name.startswith("retest-")):
                shutil.rmtree(child)
                removed.append(str(child))

    return {
        "pending_dir": str(pending_dir),
        "corpus_db": str(corpus_db),
        "model_dir": str(model_dir),
        "removed_count": len(removed),
        "removed": removed,
    }


def train_model(
    repo_root: Path,
    *,
    out_dir: Path,
    weighted_samples_path: Path | None = None,
    roots: list[str] | None = None,
    raw_source: bool = False,
    include_suite: bool = False,
    model: str = "lstm",
    steps: int = 1200,
    batch_size: int = 16,
    sequence_length: int = 384,
    embedding_size: int = 400,
    hidden_size: int = 400,
    layers: int = 2,
    dropout: float = 0.0,
    learning_rate: float = 0.01,
    seed: int = 214421,
    device_name: str = "auto",
    sample_count: int = 8,
    sample_length: int = 900,
    temperature: float = 0.85,
    prime: str = "module main\n",
) -> dict:
    random.seed(seed)
    require_torch()
    torch.manual_seed(seed)

    out_dir.mkdir(parents=True, exist_ok=True)
    device = pick_device(device_name)
    weighted_summary = None
    files: list[Path] = []
    roots = roots or ["tests/Kappa.Compiler.Tests/Fixtures", "new-tests"]
    absolute_roots = [resolve_path(repo_root, root) for root in roots]

    if weighted_samples_path is not None:
        records = load_weighted_sample_records(weighted_samples_path)
        raw_sources = [str(record["text"]) for record in records]
        keyword_codes = {} if raw_source else build_keyword_codes(raw_sources)
        corpus, weighted_summary = load_weighted_corpus(records, keyword_codes, raw_source)
        source_count = len(records)
    else:
        files = source_files(absolute_roots, include_suite=include_suite)
        raw_sources = [path.read_text(encoding="utf-8") for path in files]
        keyword_codes = {} if raw_source else build_keyword_codes(raw_sources)
        corpus = load_corpus(files) if raw_source else load_normalized_corpus(files, keyword_codes)
        source_count = len(files)

    corpus_hash = hashlib.sha256(corpus.encode("utf-8")).hexdigest()
    (out_dir / "corpus.txt").write_text(corpus, encoding="utf-8")
    (out_dir / "keyword_codes.json").write_text(json.dumps(keyword_codes, indent=2, sort_keys=True), encoding="utf-8")

    char_to_id, vocab, data = encode_corpus(corpus)
    config = ModelConfig(
        vocab_size=len(vocab),
        embedding_size=embedding_size,
        hidden_size=hidden_size,
        layers=layers,
        dropout=dropout,
        model=model,
    )

    model_instance = CharRnn(config).to(device)
    optimizer = torch.optim.AdamW(model_instance.parameters(), lr=learning_rate)
    losses: list[float] = []
    report_every = max(1, steps // 12)

    print(f"files={source_count} chars={len(corpus)} vocab={len(vocab)} sha256={corpus_hash[:12]} device={device}")
    for step in range(1, steps + 1):
        model_instance.train()
        inputs, targets = random_batch(data, batch_size, sequence_length, device)
        logits = model_instance(inputs)
        loss = F.cross_entropy(logits.reshape(-1, len(vocab)), targets.reshape(-1))
        optimizer.zero_grad(set_to_none=True)
        loss.backward()
        nn.utils.clip_grad_norm_(model_instance.parameters(), 1.0)
        optimizer.step()

        loss_value = float(loss.item())
        losses.append(loss_value)
        if step == 1 or step % report_every == 0 or step == steps:
            print(f"step={step:5d} loss={loss_value:.4f} ppl={math.exp(min(loss_value, 20.0)):.2f}")

    metadata = {
        "config": asdict(config),
        "source_roots": [str(root.relative_to(repo_root) if root.is_relative_to(repo_root) else root) for root in absolute_roots],
        "weighted_samples": (
            str(weighted_samples_path.relative_to(repo_root) if weighted_samples_path and weighted_samples_path.is_relative_to(repo_root) else weighted_samples_path)
            if weighted_samples_path is not None
            else None
        ),
        "file_count": source_count,
        "corpus_chars": len(corpus),
        "corpus_sha256": corpus_hash,
        "seed": seed,
        "steps": steps,
        "batch_size": batch_size,
        "sequence_length": sequence_length,
        "learning_rate": learning_rate,
        "device": str(device),
        "final_loss": losses[-1],
        "normalized": not raw_source,
        "keyword_count": len(keyword_codes),
        "weighted_summary": weighted_summary,
    }

    checkpoint_path = out_dir / "kappa-char-lstm.pt"
    torch.save(
        {"model_state": model_instance.cpu().state_dict(), "metadata": metadata, "vocab": vocab, "char_to_id": char_to_id, "keyword_codes": keyword_codes},
        checkpoint_path,
    )
    (out_dir / "vocab.json").write_text(json.dumps(vocab, indent=2), encoding="utf-8")
    (out_dir / "metadata.json").write_text(json.dumps(metadata, indent=2), encoding="utf-8")
    write_samples(
        model=model_instance,
        vocab=vocab,
        char_to_id=char_to_id,
        keyword_codes=keyword_codes,
        out_dir=out_dir,
        sample_count=sample_count,
        sample_length=sample_length,
        temperature=temperature,
        prime=prime,
        device=device,
    )
    return {"checkpoint": str(checkpoint_path), "metadata": metadata}


def load_checkpoint(path: Path) -> tuple[CharRnn, dict]:
    require_torch()
    checkpoint = torch.load(path, map_location="cpu", weights_only=False)
    config_data = dict(checkpoint["metadata"]["config"])
    if "model" not in config_data:
        config_data["model"] = "lstm"
    config = ModelConfig(**config_data)
    model = CharRnn(config)
    model.load_state_dict(checkpoint["model_state"])
    model.eval()
    return model, checkpoint


def remap_identifiers(text: str, rng: random.Random, max_ids: int) -> str:
    ids = sorted(set(re.findall(r"\bi\d+\b", text)))
    if not ids:
        return text
    k = min(len(ids), max_ids)
    combinations = []
    next_id = 0
    capitals = [rng.randint(0, 1) == 1]
    for _ in ids:
        combinations.append(next_id)
        if rng.randint(0, 1) == 1 and next_id + 1 < k:
            next_id += 1
            capitals.append(rng.randint(0, 1) == 1)
    mapping = {
        identifier: f"{'I' if capitals[min(combinations[index], len(capitals) - 1)] else 'i'}{combinations[index]}"
        for index, identifier in enumerate(ids)
    }
    return re.sub(r"\bi\d+\b", lambda match: mapping.get(match.group(0), match.group(0)), text)


def fuzz_from_checkpoint(
    repo_root: Path,
    *,
    checkpoint_path: Path,
    cli_path: Path,
    out_dir: Path,
    count: int,
    sample_length: int,
    temperature: float,
    prime: str,
    seed: int,
    timeout_seconds: float,
    max_ids: int,
    keep_diagnostics: int,
    keep_successes: int,
    verify_stages: list[str],
) -> dict:
    rng = random.Random(seed)
    require_torch()
    out_dir.mkdir(parents=True, exist_ok=True)
    for name in BUCKETS:
        (out_dir / name).mkdir(parents=True, exist_ok=True)

    model, checkpoint = load_checkpoint(checkpoint_path)
    compiler_commit = current_git_commit(repo_root)
    vocab = checkpoint["vocab"]
    char_to_id = checkpoint["char_to_id"]
    keyword_codes = checkpoint.get("keyword_codes", {})
    code_to_keyword = {code: keyword for keyword, code in keyword_codes.items()}
    encoded_prime = normalize_source(prime, keyword_codes) if keyword_codes else prime
    prime_ids = [char_to_id[char] for char in encoded_prime if char in char_to_id] or [char_to_id["\n"]]

    stats = {"ok": 0, "diagnostic": 0, "crash": 0, "timeout": 0, "failure": 0}
    saved_diagnostics = 0
    saved_successes = 0

    (out_dir / "run.json").write_text(
        json.dumps(
            {
                "compiler_commit": compiler_commit,
                "checkpoint": str(checkpoint_path),
                "cli": str(cli_path),
                "count": count,
                "sample_length": sample_length,
                "temperature": temperature,
                "prime": prime,
                "seed": seed,
                "timeout_seconds": timeout_seconds,
                "max_ids": max_ids,
                "verify_stages": verify_stages,
            },
            indent=2,
        ),
        encoding="utf-8",
    )

    for index in range(1, count + 1):
        ids = model.sample(prime_ids=prime_ids, max_length=sample_length, temperature=temperature, device=torch.device("cpu"))
        encoded = decode(ids, vocab).split("\1", 1)[0].strip()
        encoded = remap_identifiers(encoded, rng, max_ids).strip()
        decoded = decode_keywords(encoded, code_to_keyword)
        digest = hashlib.sha1(decoded.encode("utf-8")).hexdigest()
        started_at = time.time()

        stage = "compile"
        run = run_cli_source(cli_path, repo_root, decoded, stage=stage, timeout_seconds=timeout_seconds)
        kind = run.kind
        verification_results: list[dict] = []

        if kind == "ok":
            failing_runs: list[CaseRunResult] = []
            for verify_stage in verify_stages:
                verify_run = run_cli_source(cli_path, repo_root, decoded, stage=verify_stage, timeout_seconds=timeout_seconds)
                verification_results.append(
                    {
                        "stage": verify_stage,
                        "kind": verify_run.kind,
                        "returncode": verify_run.returncode,
                        "timed_out": verify_run.timed_out,
                        "terminal_signature": terminal_signature_from_run(verify_run),
                    }
                )
                if verify_run.kind in {"crash", "timeout", "failure"}:
                    failing_runs.append(verify_run)
            if failing_runs:
                severity_order = {"crash": 0, "timeout": 1, "failure": 2}
                run = min(
                    failing_runs,
                    key=lambda candidate: (severity_order.get(candidate.kind, 99), verify_stages.index(candidate.stage)),
                )
                stage = run.stage
                kind = run.kind

        stats[kind] += 1
        should_save = False
        target_dir = out_dir / "diagnostics"
        if kind in {"crash", "timeout", "failure"}:
            should_save = True
            target_dir = out_dir / bucket_for_kind(kind)
        elif kind == "diagnostic" and saved_diagnostics < keep_diagnostics:
            should_save = True
            saved_diagnostics += 1
            target_dir = out_dir / "diagnostics"
        elif kind == "ok" and saved_successes < keep_successes:
            should_save = True
            saved_successes += 1
            target_dir = out_dir / "successes"

        if should_save:
            case_dir = target_dir / f"{kind}-{digest}"
            case_dir.mkdir(parents=True, exist_ok=True)
            (case_dir / "main.kp").write_text(decoded + "\n", encoding="utf-8")
            (case_dir / "main.encoded.kp").write_text(encoded + "\n", encoding="utf-8")
            (case_dir / "stdout.txt").write_text(run.stdout, encoding="utf-8")
            (case_dir / "stderr.txt").write_text(run.stderr, encoding="utf-8")
            (case_dir / "expected.txt").write_text(terminal_signature_from_run(run) + "\n", encoding="utf-8")
            (case_dir / "meta.json").write_text(
                json.dumps(
                    {
                        "kind": kind,
                        "sha1": digest,
                        "returncode": run.returncode,
                        "timed_out": run.timed_out,
                        "elapsed_ms": int((time.time() - started_at) * 1000.0),
                        "index": index,
                        "temperature": temperature,
                        "compiler_commit": compiler_commit,
                        "checkpoint": str(checkpoint_path),
                        "cli": str(cli_path),
                        "stage": stage,
                        "prime": prime,
                        "sample_length": sample_length,
                        "seed": seed,
                        "verify_stages": verify_stages,
                        "verification_results": verification_results,
                    },
                    indent=2,
                ),
                encoding="utf-8",
            )

        if index == 1 or index % 25 == 0 or kind in {"crash", "timeout"}:
            print(
                f"sample={index} kind={kind} ok={stats['ok']} diag={stats['diagnostic']} "
                f"crash={stats['crash']} timeout={stats['timeout']} failure={stats['failure']}"
            )

    (out_dir / "summary.json").write_text(json.dumps(stats, indent=2), encoding="utf-8")
    return stats


def retest_cases(
    repo_root: Path,
    *,
    roots: list[str],
    cli_path: Path,
    out_dir: Path,
    timeout_seconds: float,
    verify_stages: list[str],
) -> dict:
    out_dir.mkdir(parents=True, exist_ok=True)
    for name in BUCKETS:
        (out_dir / name).mkdir(parents=True, exist_ok=True)

    compiler_commit = current_git_commit(repo_root)
    case_dirs = discover_case_dirs(repo_root, roots)
    (out_dir / "run.json").write_text(
        json.dumps(
            {
                "compiler_commit": compiler_commit,
                "cli": str(cli_path),
                "timeout_seconds": timeout_seconds,
                "case_count": len(case_dirs),
                "verify_stages": verify_stages,
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
        run = run_cli_source(cli_path, repo_root, source, stage=stage, timeout_seconds=timeout_seconds)
        kind = run.kind
        verification_results: list[dict] = []

        if kind == "ok":
            failing_runs: list[CaseRunResult] = []
            for verify_stage in verify_stages:
                verify_run = run_cli_source(cli_path, repo_root, source, stage=verify_stage, timeout_seconds=timeout_seconds)
                verification_results.append(
                    {
                        "stage": verify_stage,
                        "kind": verify_run.kind,
                        "returncode": verify_run.returncode,
                        "timed_out": verify_run.timed_out,
                        "terminal_signature": terminal_signature_from_run(verify_run),
                    }
                )
                if verify_run.kind in {"crash", "timeout", "failure"}:
                    failing_runs.append(verify_run)
            if failing_runs:
                severity_order = {"crash": 0, "timeout": 1, "failure": 2}
                run = min(
                    failing_runs,
                    key=lambda candidate: (severity_order.get(candidate.kind, 99), verify_stages.index(candidate.stage)),
                )
                stage = run.stage
                kind = run.kind

        stats[kind] += 1
        digest = sample_sha1(source)
        dest_dir = out_dir / bucket_for_kind(kind) / f"{kind}-{digest}"
        dest_dir.mkdir(parents=True, exist_ok=True)
        (dest_dir / "main.kp").write_text(source.rstrip() + "\n", encoding="utf-8")
        (dest_dir / "stdout.txt").write_text(run.stdout, encoding="utf-8")
        (dest_dir / "stderr.txt").write_text(run.stderr, encoding="utf-8")
        (dest_dir / "expected.txt").write_text(terminal_signature_from_run(run) + "\n", encoding="utf-8")
        (dest_dir / "meta.json").write_text(
            json.dumps(
                {
                    "kind": kind,
                    "sha1": digest,
                    "returncode": run.returncode,
                    "timed_out": run.timed_out,
                    "elapsed_ms": int((time.time() - started_at) * 1000.0),
                    "index": index,
                    "compiler_commit": compiler_commit,
                    "cli": str(cli_path),
                    "stage": stage,
                    "source_case_dir": str(case_dir),
                    "verify_stages": verify_stages,
                    "verification_results": verification_results,
                },
                indent=2,
            ),
            encoding="utf-8",
        )

    (out_dir / "summary.json").write_text(json.dumps(stats, indent=2), encoding="utf-8")
    return stats


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
    stage = str(meta.get("stage") or ("verify:KBackendIR@dotnet-il" if kind == "failure" else "compile"))
    if kind == "failure":
        expected = terminal_signature_from_case(case_dir)
    elif kind == "crash":
        expected = first_nonblank_line((case_dir / "stderr.txt").read_text(encoding="utf-8", errors="replace")) or first_nonblank_line((case_dir / "stdout.txt").read_text(encoding="utf-8", errors="replace"))
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
        return terminal_signature_from_run(run) == expected
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
        let_match = re.match(r"let\s+([A-Za-z_][A-Za-z0-9_]*)(?:\s*\([^)]*\))?\s*:\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*)", let_line)
        if let_match:
            candidates.append(f"type {type_name} = (left : {type_name})\nlet {let_match.group(1)} : {let_match.group(2)} = ()")
    let_match = re.match(r"let\s+([A-Za-z_][A-Za-z0-9_]*)(?:\s*\([^)]*\))?\s*:\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*)", let_line)
    if let_match:
        simplified_let = f"let {let_match.group(1)} : {let_match.group(2)} = ()"
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
    check = lambda candidate_source: preserves_case(cli_path, repo_root, candidate_source, kind, stage, expected, timeout_seconds)
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
                if len(candidate) < len(source) and check(candidate):
                    source = candidate
                    changed = True
                    break
    return source


def minimize_cases(repo_root: Path, *, in_dir: Path, out_dir: Path, seed_dir: Path | None, cli_path: Path, timeout_seconds: float) -> dict:
    kind = kind_from_bucket_name(in_dir.name)
    out_dir.mkdir(parents=True, exist_ok=True)
    if seed_dir is not None:
        seed_dir.mkdir(parents=True, exist_ok=True)

    results = []
    for case_dir in sorted(path for path in in_dir.iterdir() if path.is_dir() and (path / "main.kp").exists()):
        source = (case_dir / "main.kp").read_text(encoding="utf-8", errors="replace")
        stage, expected = load_case_expectation(case_dir, kind)
        minimized = minimize_source(cli_path, repo_root, source, kind, stage, expected, timeout_seconds)
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
            {
                "name": case_dir.name,
                "kind": kind,
                "stage": stage,
                "expectation": expected,
                "original_lines": len(source.splitlines()),
                "minimized_lines": len(minimized.splitlines()),
                "changed": minimized.strip() != source.strip(),
            }
        )

    if seed_dir is not None and kind == "failure":
        success_dir = in_dir.parent / "successes"
        if success_dir.exists():
            for case_dir in sorted(path for path in success_dir.iterdir() if path.is_dir()):
                seed_case_dir = seed_dir / "successes" / case_dir.name
                seed_case_dir.mkdir(parents=True, exist_ok=True)
                shutil.copy2(case_dir / "main.kp", seed_case_dir / "main.kp")

    summary = {"count": len(results), "changed": sum(1 for result in results if result["changed"]), "results": results}
    (out_dir / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    return summary


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
        steps.add("|".join([event, subject, input_checkpoint, output_checkpoint, changed, verify or "-"]))
    return sorted(steps)


def trace_hash(trace_steps: list[str]) -> str:
    return hashlib.sha1("\n".join(trace_steps).encode("utf-8")).hexdigest()


def verify_checkpoint_for_kind(kind: str) -> str | None:
    return "KBackendIR" if kind in {"failure", "ok", "success"} else None


def verification_stage_for_case(case_dir: Path, kind: str) -> str:
    meta = load_json(case_dir / "meta.json")
    stage = str(meta.get("stage", "")).strip()
    if stage:
        return stage
    verify_checkpoint = verify_checkpoint_for_kind(kind)
    return f"verify:{verify_checkpoint}@dotnet-il" if verify_checkpoint else "compile"


def has_case_dirs(root: Path) -> bool:
    return root.exists() and any(path.is_dir() and (path / "main.kp").exists() for path in root.iterdir())


def cluster_cases_by_trace(repo_root: Path, *, roots: list[Path], cli_path: Path, timeout_seconds: float, group_by: str = "trace+terminal") -> dict:
    case_dirs: list[Path] = []
    for root in roots:
        if (root / "main.kp").exists():
            case_dirs.append(root)
            continue
        found_branch = False
        for kind_dir in ["failures", "crashes", "diagnostics", "successes", "timeouts", "minimized-failures", "minimized-crashes", "minimized-timeouts"]:
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
        kind = kind_from_case_dir(case_dir)
        source = (case_dir / "main.kp").read_text(encoding="utf-8", errors="replace")
        source_hash = alpha_source_hash(source)
        stage = verification_stage_for_case(case_dir, kind)
        run = run_cli_source(cli_path, repo_root, source, stage=stage, timeout_seconds=timeout_seconds, trace=True)
        steps = trace_steps_from_stdout(run.stdout)
        step_hash = trace_hash(steps)
        raw_terminal = terminal_signature_from_case(case_dir)
        terminal = canonicalize_terminal_signature(raw_terminal)
        cluster_key = f"{step_hash}:{terminal}" if group_by == "trace+terminal" else step_hash
        result = {
            "case": case_dir.name,
            "path": str(case_dir),
            "kind": kind,
            "trace_hash": step_hash,
            "alpha_source_hash": source_hash,
            "cluster_key": cluster_key,
            "trace_step_count": len(steps),
            "trace_steps": steps,
            "terminal_signature": raw_terminal,
            "canonical_terminal_signature": terminal,
            "returncode": run.returncode,
            "timed_out": run.timed_out,
            "trace_stdout_missing": not bool(steps) and "Pipeline trace" not in run.stdout,
            "trace_stderr_head": last_nonblank_line(run.stderr),
        }
        results.append(result)
        clusters[cluster_key].append(result)

    cluster_records = []
    for cluster_key, members in sorted(clusters.items(), key=lambda item: (-len(item[1]), item[0])):
        representative = min(
            members,
            key=lambda member: (len(Path(member["path"]).joinpath("main.kp").read_text(encoding="utf-8", errors="replace")), member["case"]),
        )
        cluster_records.append(
            {
                "cluster_key": cluster_key,
                "count": len(members),
                "trace_hash": members[0]["trace_hash"],
                "alpha_source_hash": members[0]["alpha_source_hash"],
                "trace_step_count": members[0]["trace_step_count"],
                "kinds": dict(Counter(member["kind"] for member in members)),
                "terminal_signatures": dict(Counter(member["terminal_signature"] for member in members)),
                "canonical_terminal_signatures": dict(Counter(member["canonical_terminal_signature"] for member in members)),
                "representative_case": representative["case"],
                "representative_path": representative["path"],
            }
        )

    return {"case_count": len(results), "cluster_count": len(cluster_records), "group_by": group_by, "clusters": cluster_records, "cases": results}


def backfill_traces(
    repo_root: Path,
    *,
    db_path: Path,
    cli_path: Path,
    timeout_seconds: float,
    compiler_commit: str | None = None,
    limit: int | None = None,
) -> dict:
    compiler_commit = compiler_commit or current_git_commit(repo_root)
    connection = sqlite3.connect(db_path)
    connection.row_factory = sqlite3.Row
    try:
        ensure_schema(connection)
        rows = list(
            connection.execute(
                """
                SELECT DISTINCT s.sample_sha1, s.text, tr.meta_json
                FROM samples s
                JOIN test_results tr ON tr.sample_sha1 = s.sample_sha1
                WHERE tr.compiler_commit = ?
                ORDER BY s.sample_sha1
                """,
                (compiler_commit,),
            )
        )

        staged: dict[tuple[str, str], dict] = {}
        for row in rows:
            meta = json.loads(row["meta_json"])
            stage = str(meta.get("stage", "compile"))
            key = (row["sample_sha1"], stage)
            if key not in staged:
                staged[key] = {"sample_sha1": row["sample_sha1"], "text": row["text"], "stage": stage}

        items = list(staged.values())
        if limit is not None:
            items = items[:limit]

        inserted = 0
        attempted = 0
        for item in items:
            attempted += 1
            run = run_cli_source(cli_path, repo_root, item["text"], stage=item["stage"], timeout_seconds=timeout_seconds, trace=True)
            steps = trace_steps_from_stdout(run.stdout)
            add_trace_result(
                connection,
                sample_sha1_value=item["sample_sha1"],
                compiler_commit=compiler_commit,
                stage=item["stage"],
                trace_hash_value=trace_hash(steps),
                trace_step_count=len(steps),
                trace_steps=steps,
                terminal_signature=last_nonblank_line(run.stderr) or last_nonblank_line(run.stdout) or "<empty>",
                returncode=run.returncode,
                timed_out=run.timed_out,
            )
            inserted += 1

        connection.commit()
        total = connection.execute(
            "SELECT COUNT(*) FROM trace_results WHERE compiler_commit = ?",
            (compiler_commit,),
        ).fetchone()[0]
    finally:
        connection.close()

    return {
        "db": str(db_path),
        "compiler_commit": compiler_commit,
        "attempted": attempted,
        "inserted_or_seen": inserted,
        "trace_result_count_for_commit": total,
    }


def promote_pending_failures(repo_root: Path, *, roots: list[Path], cli_path: Path, pending_dir: Path, timeout_seconds: float) -> dict:
    pending_dir.mkdir(parents=True, exist_ok=True)
    minimized_roots: list[Path] = []

    for run_root in roots:
        for bucket_name, minimized_name in [("failures", "minimized-failures"), ("crashes", "minimized-crashes"), ("timeouts", "minimized-timeouts")]:
            bucket_dir = run_root / bucket_name
            minimized_dir = run_root / minimized_name
            if not bucket_dir.exists() or not any(path.is_dir() and (path / "main.kp").exists() for path in bucket_dir.iterdir()):
                continue
            minimize_cases(repo_root, in_dir=bucket_dir, out_dir=minimized_dir, seed_dir=run_root / "recycled-seeds", cli_path=cli_path, timeout_seconds=timeout_seconds)
            if any(path.is_dir() and (path / "main.kp").exists() for path in minimized_dir.iterdir()):
                minimized_roots.append(minimized_dir)

    cluster_roots = list(minimized_roots)
    if has_case_dirs(pending_dir):
        cluster_roots.append(pending_dir)

    if not cluster_roots:
        summary = {"promoted": 0, "clusters": 0, "pending_dir": str(pending_dir)}
        (pending_dir / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
        return summary

    cluster_summary = cluster_cases_by_trace(repo_root, roots=cluster_roots, cli_path=cli_path, timeout_seconds=timeout_seconds)
    staging_dir = pending_dir.parent / f".{pending_dir.name}.staging"
    if staging_dir.exists():
        shutil.rmtree(staging_dir)
    staging_dir.mkdir(parents=True, exist_ok=True)
    promoted = []
    for cluster in cluster_summary["clusters"]:
        dominant_terminal = max(cluster["canonical_terminal_signatures"].items(), key=lambda item: item[1])[0]
        kind = max(cluster["kinds"].items(), key=lambda item: item[1])[0]
        terminal_hash = hashlib.sha1(dominant_terminal.encode("utf-8")).hexdigest()
        destination_name = f"{kind}-{cluster['trace_hash'][:12]}-{terminal_hash[:12]}"
        destination = staging_dir / destination_name
        final_destination = pending_dir / destination_name
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
        promoted.append({"destination": str(final_destination), **cluster_record})

    summary = {"promoted": len(promoted), "clusters": len(cluster_summary["clusters"]), "pending_dir": str(pending_dir), "entries": promoted}
    (staging_dir / "summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    for child in list(pending_dir.iterdir()):
        if child.is_dir():
            shutil.rmtree(child)
        else:
            child.unlink()
    for child in sorted(staging_dir.iterdir()):
        shutil.move(str(child), pending_dir / child.name)
    shutil.rmtree(staging_dir)
    return summary


def cleanup_artifacts(repo_root: Path, *, keep_model_dir: Path, keep_run_dir: Path | None) -> dict:
    artifacts_root = repo_root / "artifacts"
    removed: list[str] = []
    for path in sorted(artifacts_root.iterdir()) if artifacts_root.exists() else []:
        if path == keep_model_dir:
            continue
        if path.name == "fuzzball-kappa":
            for child in sorted(path.iterdir()):
                if child.name in {".venv", "corpus.sqlite", "corpus.jsonl", "weighted-training-samples.jsonl"}:
                    continue
                if child.is_dir():
                    shutil.rmtree(child)
                    removed.append(str(child))
                elif child.is_file():
                    child.unlink()
                    removed.append(str(child))
            continue
        if path.is_dir() and path.name.startswith("fuzzball-kappa"):
            shutil.rmtree(path)
            removed.append(str(path))

    if keep_model_dir.exists():
        for child in sorted(keep_model_dir.iterdir()):
            if child == keep_run_dir:
                continue
            if child.name in {"kappa-char-lstm.pt", "metadata.json", "vocab.json", "keyword_codes.json", "samples.kp.txt", "samples.encoded.txt", "corpus.txt"}:
                continue
            if child.is_dir():
                shutil.rmtree(child)
                removed.append(str(child))

    return {"removed_count": len(removed), "removed": removed}


def pipeline(
    repo_root: Path,
    *,
    cli_path: Path,
    corpus_db: Path,
    model_dir: Path,
    count: int,
    steps: int,
    temperature: float,
    timeout_seconds: float,
    sample_length: int,
    skip_build: bool = False,
    skip_train: bool = False,
    cleanup: bool = True,
) -> dict:
    verify_stages = ["verify:KBackendIR@dotnet-il", "verify:KBackendIR@zig"]
    compiler_commit = current_git_commit(repo_root)
    timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
    run_dir = model_dir / f"fuzz-run-{timestamp}-{compiler_commit[:8]}"
    retest_dir = model_dir / f"retest-pending-{timestamp}-{compiler_commit[:8]}"
    model_dir.mkdir(parents=True, exist_ok=True)
    corpus_jsonl = corpus_db.with_suffix(".jsonl")
    weighted_jsonl = corpus_db.parent / "weighted-training-samples.jsonl"

    if not skip_build:
        subprocess.run(["dotnet", "build", "src/Kappa.Compiler.Cli/Kappa.Compiler.Cli.fsproj", "-c", "Release", "-v", "q"], cwd=repo_root, check=True)

    retest_summary = None
    if (repo_root / "pending-failures").exists():
        retest_summary = retest_cases(repo_root, roots=["pending-failures"], cli_path=cli_path, out_dir=retest_dir, timeout_seconds=timeout_seconds, verify_stages=verify_stages)

    corpus_before = update_corpus_store(repo_root, db_path=corpus_db, jsonl_path=corpus_jsonl)
    weighting = export_weighted_training_samples(repo_root, db_path=corpus_db, out_path=weighted_jsonl, preferred_commit=compiler_commit)

    train_summary = None
    if not skip_train:
        train_summary = train_model(repo_root, out_dir=model_dir, weighted_samples_path=weighted_jsonl, steps=steps)

    fuzz_summary = fuzz_from_checkpoint(
        repo_root,
        checkpoint_path=model_dir / "kappa-char-lstm.pt",
        cli_path=cli_path,
        out_dir=run_dir,
        count=count,
        sample_length=sample_length,
        temperature=temperature,
        prime="module main\n\n",
        seed=214421,
        timeout_seconds=timeout_seconds,
        max_ids=8,
        keep_diagnostics=10,
        keep_successes=5,
        verify_stages=verify_stages,
    )

    promotion = promote_pending_failures(repo_root, roots=[run_dir], cli_path=cli_path, pending_dir=repo_root / "pending-failures", timeout_seconds=timeout_seconds)
    corpus_after = update_corpus_store(repo_root, db_path=corpus_db, jsonl_path=corpus_jsonl)
    cleanup_summary = cleanup_artifacts(repo_root, keep_model_dir=model_dir, keep_run_dir=run_dir) if cleanup else {"removed_count": 0, "removed": []}

    return {
        "compiler_commit": compiler_commit,
        "cli": str(cli_path),
        "model_dir": str(model_dir),
        "run_dir": str(run_dir),
        "retest_dir": str(retest_dir),
        "retest": retest_summary,
        "corpus_before": corpus_before,
        "weighting": weighting,
        "train": train_summary,
        "fuzz": fuzz_summary,
        "promotion": promotion,
        "corpus_after": corpus_after,
        "cleanup": cleanup_summary,
    }
