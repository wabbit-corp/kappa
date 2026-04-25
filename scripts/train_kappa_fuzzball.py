#!/usr/bin/env python3
"""Train a small fuzzball-style character model on Kappa test fixtures.

The model is intentionally simple: a character embedding, an LSTM, and a linear
projection back to characters. It writes checkpoints under artifacts/ by
default, keeping generated binaries out of git.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import random
import re
from collections import Counter
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Iterable

import torch
from torch import nn
from torch.nn import functional as F


CHUNK_SEPARATOR = "\n\1\n"

KAPPA_WORDS = {
    "_",
    "module",
    "import",
    "export",
    "as",
    "except",
    "unhide",
    "clarify",
    "term",
    "type",
    "trait",
    "ctor",
    "public",
    "private",
    "data",
    "let",
    "infix",
    "left",
    "right",
    "prefix",
    "postfix",
    "match",
    "case",
    "if",
    "then",
    "elif",
    "else",
    "is",
    "do",
    "pure",
    "handle",
    "return",
    "effect",
    "handler",
    "resume",
    "for",
    "in",
    "while",
    "loop",
    "break",
    "continue",
    "var",
    "this",
    "opaque",
    "refl",
    "summon",
    "True",
    "False",
    "Bool",
    "Int",
    "Integer",
    "Float",
    "Double",
    "String",
    "Char",
    "Unit",
    "Type",
    "Nat",
    "IO",
    "EffRow",
    "Universe",
    "Syntax",
    "Dict",
    "Maybe",
    "Some",
    "None",
    "List",
    "Nil",
    "Cons",
    "Eq",
    "Show",
    "main",
    "result",
}

SCANNER = re.Scanner(
    [
        (r"`[^`\n]*`", lambda scanner, token: ("BTID", token)),
        (r'"(?:\\.|[^"\\])*"', lambda scanner, token: ("STR", token)),
        (r"'(?:\\.|[^'\\])+'", lambda scanner, token: ("CHR", token)),
        (r"[A-Za-z_][A-Za-z0-9_]*", lambda scanner, token: ("ID", token)),
        (r"\d+(?:\.\d+)?(?:[A-Za-z_][A-Za-z0-9_]*)?", lambda scanner, token: ("NUM", token)),
        (r"[ \t]+", lambda scanner, token: ("SP", token)),
        (r".", lambda scanner, token: ("O", token)),
    ]
)

KEYWORD_CODE_PATTERN = re.compile(r"k(\d+)")


@dataclass(frozen=True)
class ModelConfig:
    vocab_size: int
    embedding_size: int
    hidden_size: int
    layers: int
    dropout: float
    model: str


class CharRnn(nn.Module):
    def __init__(self, config: ModelConfig) -> None:
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

    @torch.no_grad()
    def sample(
        self,
        *,
        prime_ids: list[int],
        max_length: int,
        temperature: float,
        device: torch.device,
    ) -> list[int]:
        self.eval()
        ids = list(prime_ids)
        hidden = None

        for prime_id in prime_ids[:-1]:
            current = torch.tensor([[prime_id]], dtype=torch.long, device=device)
            embedded = self.embedding(current)
            _, hidden = self.rnn(embedded, hidden)

        current = torch.tensor([[prime_ids[-1]]], dtype=torch.long, device=device)

        for _ in range(max_length):
            embedded = self.embedding(current)
            output, hidden = self.rnn(embedded, hidden)
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


def source_files(roots: Iterable[Path], include_suite: bool) -> list[Path]:
    files: list[Path] = []

    for root in roots:
        if root.is_file():
            if root.suffix == ".kp" or (include_suite and root.name == "suite.ktest"):
                files.append(root)
            continue

        if not root.exists():
            raise FileNotFoundError(root)

        files.extend(root.rglob("*.kp"))

        if include_suite:
            files.extend(root.rglob("suite.ktest"))

    return sorted(set(path.resolve() for path in files))


def load_weighted_sample_records(path: Path) -> list[dict]:
    records: list[dict] = []

    with path.open(encoding="utf-8") as handle:
        for line in handle:
            if not line.strip():
                continue

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


def strip_line_comment(line: str) -> str:
    index = 0
    in_string = False
    in_char = False
    in_backtick = False
    escaped = False

    while index < len(line):
        current = line[index]

        if escaped:
            escaped = False
        elif current == "\\" and (in_string or in_char):
            escaped = True
        elif in_string:
            if current == '"':
                in_string = False
        elif in_char:
            if current == "'":
                in_char = False
        elif in_backtick:
            if current == "`":
                in_backtick = False
        elif current == '"':
            in_string = True
        elif current == "'":
            in_char = True
        elif current == "`":
            in_backtick = True
        elif current == "-" and index + 1 < len(line) and line[index + 1] == "-":
            return line[:index]

        index += 1

    return line


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
        prefix = "I" if name[0].isupper() else "i"
        identifiers[name] = f"{prefix}{len(identifiers)}"

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
            pieces.append("${")
            pieces.append(body)
            pieces.append("}")
            current = min(len(text), expression_end + 1)
            literal_start = current
            continue

        if interpolated and not hashes and current + 1 < len(text) and text[current] == "$":
            next_char = text[current + 1]

            if next_char == "`":
                pieces.append(compact_literal_segment(text[literal_start:current]))
                name, next_index = read_backtick_identifier(text, current + 1)
                pieces.append("$")
                pieces.append(normalize_name(name, identifiers, keyword_codes))
                current = next_index
                literal_start = current
                continue

            if is_identifier_start(next_char):
                pieces.append(compact_literal_segment(text[literal_start:current]))
                name, next_index = read_identifier(text, current + 1)
                pieces.append("$")
                pieces.append(normalize_name(name, identifiers, keyword_codes))
                current = next_index
                literal_start = current
                continue

        if interpolated and hashes and text.startswith(raw_interpolation_opener, current):
            pieces.append(compact_literal_segment(text[literal_start:current]))
            expression_start = current + len(raw_interpolation_opener)
            expression_end = find_interpolation_end(text, expression_start)
            body = normalize_interpolation_body(text[expression_start:expression_end], identifiers, keyword_codes)
            pieces.append(raw_interpolation_opener)
            pieces.append(body)
            pieces.append("}")
            current = min(len(text), expression_end + 1)
            literal_start = current
            continue

        current += 1

    pieces.append(compact_literal_segment(text[literal_start:current]))
    return "".join(pieces), current


def normalize_text(
    text: str,
    identifiers: dict[str, str],
    keyword_codes: dict[str, str],
    *,
    strip_comments: bool,
) -> str:
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
                    text,
                    next_index,
                    prefix,
                    hashes,
                    quote,
                    content_start,
                    identifiers,
                    keyword_codes,
                    interpolated=True,
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
                    text,
                    next_index,
                    prefix,
                    hashes,
                    quote,
                    content_start,
                    identifiers,
                    keyword_codes,
                    interpolated=True,
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
                    text,
                    index,
                    "",
                    hashes,
                    quote,
                    content_start,
                    identifiers,
                    keyword_codes,
                    interpolated=False,
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
                    text,
                    index,
                    "",
                    hashes,
                    quote,
                    content_start,
                    identifiers,
                    keyword_codes,
                    interpolated=False,
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
        text = path.read_text(encoding="utf-8")
        normalized = normalize_source(text, keyword_codes)

        if normalized:
            chunks.append(normalized)

    return CHUNK_SEPARATOR.join(dict.fromkeys(chunks)) + CHUNK_SEPARATOR


def weighted_repeat_count(weight: float) -> int:
    return max(1, min(8, math.ceil(weight)))


def load_weighted_corpus(records: list[dict], keyword_codes: dict[str, str], raw_source: bool) -> tuple[str, dict]:
    chunks: list[str] = []
    summary = {
        "record_count": len(records),
        "expanded_chunk_count": 0,
        "max_weight": 0.0,
    }

    for record in records:
        text = str(record["text"])
        weight = float(record.get("weight", 1.0))
        repeat = weighted_repeat_count(weight)

        if raw_source:
            chunk = text.replace("\r\n", "\n").strip()
        else:
            chunk = normalize_source(text, keyword_codes)

        if not chunk:
            continue

        chunks.extend([chunk] * repeat)
        summary["expanded_chunk_count"] += repeat
        summary["max_weight"] = max(summary["max_weight"], weight)

    return CHUNK_SEPARATOR.join(chunks) + CHUNK_SEPARATOR, summary


def decode_keywords(text: str, code_to_keyword: dict[str, str]) -> str:
    return KEYWORD_CODE_PATTERN.sub(lambda match: code_to_keyword.get(match.group(0), match.group(0)), text)


def encode_corpus(corpus: str) -> tuple[dict[str, int], list[str], torch.Tensor]:
    vocab = sorted(set(corpus))
    char_to_id = {char: index for index, char in enumerate(vocab)}
    ids = torch.tensor([char_to_id[char] for char in corpus], dtype=torch.long)
    return char_to_id, vocab, ids


def random_batch(data: torch.Tensor, batch_size: int, sequence_length: int, device: torch.device) -> tuple[torch.Tensor, torch.Tensor]:
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
    model = model.to(device)
    code_to_keyword = {code: keyword for keyword, code in keyword_codes.items()}
    encoded_prime = normalize_source(prime, keyword_codes) if keyword_codes else prime
    prime_ids = [char_to_id[char] for char in encoded_prime if char in char_to_id]

    if not prime_ids:
        prime_ids = [char_to_id["\n"]]

    samples = []
    encoded_samples = []

    for index in range(sample_count):
        ids = model.sample(
            prime_ids=prime_ids,
            max_length=sample_length,
            temperature=temperature,
            device=device,
        )
        encoded_text = decode(ids, vocab).split("\1", 1)[0].strip()
        text = decode_keywords(encoded_text, code_to_keyword)
        encoded_samples.append(f"-- sample {index + 1}\n{encoded_text}\n")
        samples.append(f"-- sample {index + 1}\n{text}\n")

    (out_dir / "samples.kp.txt").write_text("\n".join(samples), encoding="utf-8")
    (out_dir / "samples.encoded.txt").write_text("\n".join(encoded_samples), encoding="utf-8")
    print(f"wrote {out_dir / 'samples.kp.txt'}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--generate-only", action="store_true")
    parser.add_argument("--checkpoint", default=None)
    parser.add_argument("--raw-source", action="store_true")
    parser.add_argument("--roots", nargs="*", default=["tests/Kappa.Compiler.Tests/Fixtures", "new-tests"])
    parser.add_argument("--weighted-samples", default=None)
    parser.add_argument("--include-suite", action="store_true")
    parser.add_argument("--out-dir", default="artifacts/fuzzball-kappa")
    parser.add_argument("--model", choices=["lstm", "gru"], default="lstm")
    parser.add_argument("--steps", type=int, default=1200)
    parser.add_argument("--batch-size", type=int, default=16)
    parser.add_argument("--sequence-length", type=int, default=384)
    parser.add_argument("--embedding-size", type=int, default=400)
    parser.add_argument("--hidden-size", type=int, default=400)
    parser.add_argument("--layers", type=int, default=2)
    parser.add_argument("--dropout", type=float, default=0.0)
    parser.add_argument("--learning-rate", type=float, default=0.01)
    parser.add_argument("--seed", type=int, default=214421)
    parser.add_argument("--device", default="auto")
    parser.add_argument("--sample-count", type=int, default=8)
    parser.add_argument("--sample-length", type=int, default=900)
    parser.add_argument("--temperature", type=float, default=0.85)
    parser.add_argument("--prime", default="module main\n")
    args = parser.parse_args()

    random.seed(args.seed)
    torch.manual_seed(args.seed)

    repo_root = repo_root_from_script()
    out_dir = Path(args.out_dir) if Path(args.out_dir).is_absolute() else repo_root / args.out_dir
    out_dir.mkdir(parents=True, exist_ok=True)
    device = pick_device(args.device)

    if args.generate_only:
        checkpoint_path = Path(args.checkpoint) if args.checkpoint is not None else out_dir / "kappa-char-lstm.pt"

        if not checkpoint_path.is_absolute():
            checkpoint_path = repo_root / checkpoint_path

        checkpoint = torch.load(checkpoint_path, map_location="cpu", weights_only=False)
        vocab = checkpoint["vocab"]
        char_to_id = checkpoint["char_to_id"]
        config_data = dict(checkpoint["metadata"]["config"])

        if "model" not in config_data:
            config_data["model"] = "lstm"

        config = ModelConfig(**config_data)
        keyword_codes = checkpoint.get("keyword_codes", {})
        model = CharRnn(config)
        model.load_state_dict(checkpoint["model_state"])

        write_samples(
            model=model,
            vocab=vocab,
            char_to_id=char_to_id,
            keyword_codes=keyword_codes,
            out_dir=out_dir,
            sample_count=args.sample_count,
            sample_length=args.sample_length,
            temperature=args.temperature,
            prime=args.prime,
            device=device,
        )
        return

    weighted_summary = None
    files: list[Path] = []
    roots = [Path(root) if Path(root).is_absolute() else repo_root / root for root in args.roots]

    if args.weighted_samples is not None:
        weighted_samples_path = Path(args.weighted_samples)

        if not weighted_samples_path.is_absolute():
            weighted_samples_path = repo_root / weighted_samples_path

        records = load_weighted_sample_records(weighted_samples_path)
        raw_sources = [str(record["text"]) for record in records]
        keyword_codes = {} if args.raw_source else build_keyword_codes(raw_sources)
        corpus, weighted_summary = load_weighted_corpus(records, keyword_codes, args.raw_source)
    else:
        files = source_files(roots, include_suite=args.include_suite)
        raw_sources = [path.read_text(encoding="utf-8") for path in files]
        keyword_codes = {} if args.raw_source else build_keyword_codes(raw_sources)
        corpus = load_corpus(files) if args.raw_source else load_normalized_corpus(files, keyword_codes)

    corpus_hash = hashlib.sha256(corpus.encode("utf-8")).hexdigest()

    (out_dir / "corpus.txt").write_text(corpus, encoding="utf-8")
    (out_dir / "keyword_codes.json").write_text(json.dumps(keyword_codes, indent=2, sort_keys=True), encoding="utf-8")

    char_to_id, vocab, data = encode_corpus(corpus)
    config = ModelConfig(
        vocab_size=len(vocab),
        embedding_size=args.embedding_size,
        hidden_size=args.hidden_size,
        layers=args.layers,
        dropout=args.dropout,
        model=args.model,
    )

    model = CharRnn(config).to(device)
    optimizer = torch.optim.AdamW(model.parameters(), lr=args.learning_rate)

    losses: list[float] = []
    report_every = max(1, args.steps // 12)

    source_count = weighted_summary["record_count"] if weighted_summary is not None else len(files)
    print(f"files={source_count} chars={len(corpus)} vocab={len(vocab)} sha256={corpus_hash[:12]} device={device}")

    for step in range(1, args.steps + 1):
        model.train()
        inputs, targets = random_batch(data, args.batch_size, args.sequence_length, device)
        logits = model(inputs)
        loss = F.cross_entropy(logits.reshape(-1, len(vocab)), targets.reshape(-1))

        optimizer.zero_grad(set_to_none=True)
        loss.backward()
        nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        optimizer.step()

        loss_value = float(loss.item())
        losses.append(loss_value)

        if step == 1 or step % report_every == 0 or step == args.steps:
            perplexity = math.exp(min(loss_value, 20.0))
            print(f"step={step:5d} loss={loss_value:.4f} ppl={perplexity:.2f}")

    metadata = {
        "config": asdict(config),
        "source_roots": [str(root.relative_to(repo_root) if root.is_relative_to(repo_root) else root) for root in roots],
        "weighted_samples": (
            str(weighted_samples_path.relative_to(repo_root) if weighted_samples_path.is_relative_to(repo_root) else weighted_samples_path)
            if args.weighted_samples is not None
            else None
        ),
        "file_count": source_count,
        "corpus_chars": len(corpus),
        "corpus_sha256": corpus_hash,
        "seed": args.seed,
        "steps": args.steps,
        "batch_size": args.batch_size,
        "sequence_length": args.sequence_length,
        "learning_rate": args.learning_rate,
        "device": str(device),
        "final_loss": losses[-1],
        "normalized": not args.raw_source,
        "keyword_count": len(keyword_codes),
        "weighted_summary": weighted_summary,
    }

    torch.save(
        {
            "model_state": model.cpu().state_dict(),
            "metadata": metadata,
            "vocab": vocab,
            "char_to_id": char_to_id,
            "keyword_codes": keyword_codes,
        },
        out_dir / "kappa-char-lstm.pt",
    )

    (out_dir / "vocab.json").write_text(json.dumps(vocab, indent=2), encoding="utf-8")
    (out_dir / "metadata.json").write_text(json.dumps(metadata, indent=2), encoding="utf-8")

    print(f"wrote {out_dir / 'kappa-char-lstm.pt'}")
    write_samples(
        model=model,
        vocab=vocab,
        char_to_id=char_to_id,
        keyword_codes=keyword_codes,
        out_dir=out_dir,
        sample_count=args.sample_count,
        sample_length=args.sample_length,
        temperature=args.temperature,
        prime=args.prime,
        device=device,
    )


if __name__ == "__main__":
    main()
