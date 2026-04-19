#!/usr/bin/env python3
from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass, field
from pathlib import Path
import re


IGNORE_DIR_NAMES = {
    ".git",
    ".idea",
    ".vs",
    ".dotnet",
    "__pycache__",
    "bin",
    "obj",
}

IGNORE_FILE_NAMES = {
    ".dotnet-install.ps1",
}

TEXT_SUFFIXES = {
    ".fs",
    ".fsproj",
    ".json",
    ".kp",
    ".md",
    ".props",
    ".ps1",
    ".py",
    ".sln",
    ".targets",
    ".toml",
    ".txt",
    ".xml",
    ".yaml",
    ".yml",
}

FORBIDDEN_CHARS = {
    0x00A0: "NO-BREAK SPACE",
    0x200B: "ZERO WIDTH SPACE",
    0x200C: "ZERO WIDTH NON-JOINER",
    0x200D: "ZERO WIDTH JOINER",
    0xFEFF: "BYTE ORDER MARK",
    0xFFFD: "REPLACEMENT CHARACTER",
}

ALLOWED_LATIN1_SUPPLEMENT = {
    0x00A7,  # section sign
    0x00B7,  # middle dot
}

FENCE_RE = re.compile(r"^\s*(```|~~~)")
WIKI_REFERENCE_RE = re.compile(r"\[\[([^\[\]:]+\.md)(?::((?:\d+\.)*\d+))?\]\]")
SECTION_SIGN = "\u00A7"
EN_DASH = "\u2013"
EM_DASH = "\u2014"

NUMERIC_HEADING_RE = re.compile(
    r"^(?P<hashes>#{2,6})\s+(?P<section>\d+(?:\.\d+)*)(?:\.)?\s+(?P<title>\S.*\S|\S)\s*$"
)
APPENDIX_HEADING_RE = re.compile(
    r"^(?P<hashes>##)\s+Appendix\s+(?P<section>[A-Z])\.\s+(?P<title>\S.*\S|\S)\s*$"
)
APPENDIX_SUBHEADING_RE = re.compile(
    r"^(?P<hashes>#{3,6})\s+(?P<section>[A-Z](?:\.\d+)*)\s+(?P<title>\S.*\S|\S)\s*$"
)

SECTION_TOKEN = r"(?:[A-Z](?:\.\d+)*|\d+(?:\.\d+)*)"
SECTION_GROUP_RE = re.compile(
    rf"{SECTION_SIGN}{{1,2}}{SECTION_TOKEN}"
    rf"(?:\s*(?:-|{EN_DASH}|{EM_DASH}|,|and|or)\s*(?:{SECTION_SIGN}{{1,2}})?{SECTION_TOKEN})*"
)
SECTION_TOKEN_RE = re.compile(SECTION_TOKEN)


@dataclass(frozen=True)
class Problem:
    path: Path
    line: int
    message: str


@dataclass(frozen=True)
class Reference:
    path: Path
    line: int
    target: Path
    section: str | None
    raw: str


@dataclass
class ParsedMarkdown:
    path: Path
    sections: set[str] = field(default_factory=set)
    references: list[Reference] = field(default_factory=list)
    problems: list[Problem] = field(default_factory=list)


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent


def is_ignored_file(path: Path) -> bool:
    return path.name in IGNORE_FILE_NAMES


def is_text_file(path: Path) -> bool:
    return path.suffix.lower() in TEXT_SUFFIXES and path.is_file() and not is_ignored_file(path)


def iter_text_files(root: Path) -> list[Path]:
    results: list[Path] = []
    for child in sorted(root.iterdir(), key=lambda p: (p.is_file(), p.name.lower())):
        if child.is_dir():
            if child.name in IGNORE_DIR_NAMES:
                continue
            results.extend(iter_text_files(child))
            continue
        if is_text_file(child):
            results.append(child)
    return results


def resolve_paths(raw_paths: list[str]) -> list[Path]:
    if not raw_paths:
        return iter_text_files(repo_root())

    results: list[Path] = []
    for raw in raw_paths:
        path = Path(raw).resolve()
        if path.is_dir():
            results.extend(iter_text_files(path))
            continue
        if is_text_file(path):
            results.append(path)
            continue
        raise FileNotFoundError(f"not a supported text file or directory: {raw}")

    deduped: list[Path] = []
    seen: set[Path] = set()
    for path in results:
        if path not in seen:
            deduped.append(path)
            seen.add(path)
    return deduped


def decode_utf8(path: Path) -> tuple[str | None, list[Problem]]:
    try:
        raw = path.read_bytes()
    except OSError as exc:
        return None, [Problem(path, 1, f"unable to read file: {exc}")]

    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError as exc:
        return None, [
            Problem(
                path,
                1,
                "file is not valid UTF-8 "
                f"(byte offset {exc.start}, reason: {exc.reason})",
            )
        ]

    return text, []


def validate_unicode_characters(path: Path, text: str) -> list[Problem]:
    problems: list[Problem] = []
    line = 1

    for ch in text:
        codepoint = ord(ch)

        if codepoint in FORBIDDEN_CHARS:
            problems.append(
                Problem(
                    path,
                    line,
                    f"contains forbidden character U+{codepoint:04X} "
                    f"({FORBIDDEN_CHARS[codepoint]})",
                )
            )
        elif 0x0080 <= codepoint <= 0x00FF and codepoint not in ALLOWED_LATIN1_SUPPLEMENT:
            problems.append(
                Problem(
                    path,
                    line,
                    "contains suspicious Latin-1 Supplement character "
                    f"U+{codepoint:04X} ({repr(ch)})",
                )
            )

        if ch == "\n":
            line += 1

    return problems


def strip_inline_code(line: str) -> str:
    result: list[str] = []
    i = 0
    n = len(line)

    while i < n:
        if line[i] != "`":
            result.append(line[i])
            i += 1
            continue

        tick_count = 1
        while i + tick_count < n and line[i + tick_count] == "`":
            tick_count += 1

        closing = line.find("`" * tick_count, i + tick_count)
        if closing == -1:
            result.append(line[i])
            i += 1
            continue

        result.append(" " * (closing + tick_count - i))
        i = closing + tick_count

    return "".join(result)


def iter_non_fenced_lines(lines: list[str], strip_inline: bool = False) -> list[tuple[int, str]]:
    items: list[tuple[int, str]] = []
    in_fence = False

    for index, line in enumerate(lines, start=1):
        if FENCE_RE.match(line):
            in_fence = not in_fence
            continue
        if in_fence:
            continue
        items.append((index, strip_inline_code(line) if strip_inline else line))

    return items


def validate_line_lengths(path: Path, lines: list[str], max_length: int) -> list[Problem]:
    if max_length <= 0:
        return []

    problems: list[Problem] = []
    in_fence = False

    for index, line in enumerate(lines, start=1):
        if FENCE_RE.match(line):
            in_fence = not in_fence
            continue
        if in_fence:
            continue
        if len(line) > max_length:
            problems.append(
                Problem(
                    path,
                    index,
                    f"line length {len(line)} exceeds {max_length} characters",
                )
            )

    return problems


def validate_numeric_heading(
    path: Path,
    line_number: int,
    section: str,
    seen_numeric_children: dict[tuple[int, ...], int],
    seen_numeric_sections: set[tuple[int, ...]],
    sections: set[str],
) -> list[Problem]:
    problems: list[Problem] = []
    parts = tuple(int(part) for part in section.split("."))
    parent = parts[:-1]

    if parent and parent not in seen_numeric_sections:
        problems.append(
            Problem(
                path,
                line_number,
                f"section {section} is missing its numbered parent {'.'.join(str(part) for part in parent)}",
            )
        )

    current = parts[-1]
    expected = seen_numeric_children.get(parent, 0) + 1
    if current != expected:
        scope = f"within section {'.'.join(str(part) for part in parent)}" if parent else "at the top level"
        problems.append(
            Problem(
                path,
                line_number,
                f"section number {section} is not consecutive {scope}; expected {expected}",
            )
        )
    seen_numeric_children[parent] = current
    seen_numeric_sections.add(parts)
    sections.add(section)
    return problems


def validate_appendix_heading(
    path: Path,
    line_number: int,
    section: str,
    seen_appendix_children: dict[tuple[str, ...], int],
    seen_appendices: set[str],
    seen_appendix_sections: set[tuple[str, ...]],
    sections: set[str],
) -> list[Problem]:
    problems: list[Problem] = []
    parts = tuple(section.split("."))

    if len(parts) == 1:
        appendix = parts[0]
        if appendix in seen_appendices:
            problems.append(
                Problem(path, line_number, f"appendix {appendix} is declared more than once")
            )
        seen_appendices.add(appendix)
        seen_appendix_sections.add(parts)
        sections.add(section)
        return problems

    parent = parts[:-1]
    if parent not in seen_appendix_sections:
        problems.append(
            Problem(
                path,
                line_number,
                f"appendix section {section} is missing its numbered parent {'.'.join(parent)}",
            )
        )

    current = int(parts[-1])
    expected = seen_appendix_children.get(parent, 0) + 1
    if current != expected:
        problems.append(
            Problem(
                path,
                line_number,
                f"appendix section {section} is not consecutive within {'.'.join(parent)}; expected {expected}",
            )
        )
    seen_appendix_children[parent] = current
    seen_appendix_sections.add(parts)
    sections.add(section)
    return problems


def validate_markdown_sections(path: Path, lines: list[str]) -> tuple[list[Problem], set[str]]:
    problems: list[Problem] = []
    sections: set[str] = set()
    seen_numeric_children: dict[tuple[int, ...], int] = {}
    seen_appendix_children: dict[tuple[str, ...], int] = {}
    seen_numeric_sections: set[tuple[int, ...]] = set()
    seen_appendix_sections: set[tuple[str, ...]] = set()
    seen_appendices: set[str] = set()

    for index, line in iter_non_fenced_lines(lines):
        if not line.startswith("#"):
            continue

        numeric = NUMERIC_HEADING_RE.match(line)
        if numeric:
            problems.extend(
                validate_numeric_heading(
                    path=path,
                    line_number=index,
                    section=numeric.group("section"),
                    seen_numeric_children=seen_numeric_children,
                    seen_numeric_sections=seen_numeric_sections,
                    sections=sections,
                )
            )
            continue

        appendix = APPENDIX_HEADING_RE.match(line)
        if appendix:
            problems.extend(
                validate_appendix_heading(
                    path=path,
                    line_number=index,
                    section=appendix.group("section"),
                    seen_appendix_children=seen_appendix_children,
                    seen_appendices=seen_appendices,
                    seen_appendix_sections=seen_appendix_sections,
                    sections=sections,
                )
            )
            continue

        appendix_sub = APPENDIX_SUBHEADING_RE.match(line)
        if appendix_sub:
            problems.extend(
                validate_appendix_heading(
                    path=path,
                    line_number=index,
                    section=appendix_sub.group("section"),
                    seen_appendix_children=seen_appendix_children,
                    seen_appendices=seen_appendices,
                    seen_appendix_sections=seen_appendix_sections,
                    sections=sections,
                )
            )
            continue

        heading_match = re.match(r"^(#{1,6})\s+(\S.*\S|\S)\s*$", line)
        if heading_match is None:
            problems.append(Problem(path, index, "heading must have a title"))

    return problems, sections


def parse_section_group(group: str) -> list[str]:
    return [match.group(0).lstrip(SECTION_SIGN) for match in SECTION_TOKEN_RE.finditer(group)]


def collect_markdown_references(path: Path, lines: list[str]) -> list[Reference]:
    references: list[Reference] = []

    for line_number, line in iter_non_fenced_lines(lines, strip_inline=True):
        for match in WIKI_REFERENCE_RE.finditer(line):
            raw_target = match.group(1)
            references.append(
                Reference(
                    path=path,
                    line=line_number,
                    target=(path.parent / raw_target).resolve(),
                    section=match.group(2),
                    raw=match.group(0),
                )
            )

        for match in SECTION_GROUP_RE.finditer(line):
            for section in parse_section_group(match.group(0)):
                references.append(
                    Reference(
                        path=path,
                        line=line_number,
                        target=path,
                        section=section,
                        raw=f"{SECTION_SIGN}{section}",
                    )
                )

    return references


def parse_markdown(path: Path, text: str, max_line_length: int) -> ParsedMarkdown:
    lines = text.splitlines()
    section_problems, sections = validate_markdown_sections(path, lines)
    problems = validate_line_lengths(path, lines, max_line_length) + section_problems
    references = collect_markdown_references(path, lines)
    return ParsedMarkdown(path=path, sections=sections, references=references, problems=problems)


def load_markdown(
    path: Path,
    documents: dict[Path, ParsedMarkdown],
    max_line_length: int,
) -> ParsedMarkdown | None:
    existing = documents.get(path)
    if existing is not None:
        return existing

    text, decode_problems = decode_utf8(path)
    if decode_problems or text is None:
        document = ParsedMarkdown(path=path, problems=decode_problems)
        documents[path] = document
        return document

    document = parse_markdown(path, text, max_line_length)
    document.problems = validate_unicode_characters(path, text) + document.problems
    documents[path] = document
    return document


def validate_markdown_references(
    documents: dict[Path, ParsedMarkdown],
    max_line_length: int,
) -> list[Problem]:
    problems: list[Problem] = []

    for document in list(documents.values()):
        for reference in document.references:
            target_doc = documents.get(reference.target)

            if target_doc is None:
                if not reference.target.is_file():
                    problems.append(
                        Problem(
                            reference.path,
                            reference.line,
                            f"referenced file does not exist: {reference.raw}",
                        )
                    )
                    continue
                if reference.target.suffix.lower() != ".md":
                    problems.append(
                        Problem(
                            reference.path,
                            reference.line,
                            f"referenced file is not markdown: {reference.raw}",
                        )
                    )
                    continue
                loaded = load_markdown(reference.target, documents, max_line_length)
                if loaded is None:
                    continue
                target_doc = loaded

            if reference.section is not None and reference.section not in target_doc.sections:
                problems.append(
                    Problem(
                        reference.path,
                        reference.line,
                        f"referenced section does not exist: {reference.raw}",
                    )
                )

    return problems


def validate_file(
    path: Path,
    markdown_documents: dict[Path, ParsedMarkdown],
    max_line_length: int,
) -> list[Problem]:
    text, decode_problems = decode_utf8(path)
    if decode_problems or text is None:
        return decode_problems

    problems = validate_unicode_characters(path, text)

    if path.suffix.lower() == ".md":
        document = parse_markdown(path, text, max_line_length)
        markdown_documents[path] = document
        problems.extend(document.problems)

    return problems


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Validate Kappa spec files for UTF-8 hygiene, mojibake, markdown section "
            "numbering, and section references."
        )
    )
    parser.add_argument(
        "paths",
        nargs="*",
        help=(
            "Files or directories to validate. Defaults to the repository root "
            "containing this script."
        ),
    )
    parser.add_argument(
        "--max-line-length",
        type=int,
        default=0,
        help=(
            "Optional markdown line-length limit outside fenced code blocks. "
            "Use 0 to disable the check. Recommended future target: 120."
        ),
    )
    args = parser.parse_args(argv)

    try:
        files = resolve_paths(args.paths)
    except FileNotFoundError as exc:
        print(exc, file=sys.stderr)
        return 2

    if not files:
        print("no supported text files found", file=sys.stderr)
        return 2

    all_problems: list[Problem] = []
    markdown_documents: dict[Path, ParsedMarkdown] = {}

    for path in files:
        all_problems.extend(validate_file(path, markdown_documents, args.max_line_length))

    all_problems.extend(validate_markdown_references(markdown_documents, args.max_line_length))
    all_problems.sort(key=lambda problem: (str(problem.path).lower(), problem.line, problem.message))

    if not all_problems:
        print(f"Validation passed: checked {len(files)} file(s).")
        return 0

    for problem in all_problems:
        print(f"{problem.path}:{problem.line}: {problem.message}")
    print(f"Validation failed: {len(all_problems)} problem(s) across {len(files)} file(s).")
    return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
