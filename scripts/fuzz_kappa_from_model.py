#!/usr/bin/env python3
"""Generate Kappa programs from the fuzzball-style model and probe the compiler.

This script uses the existing checkpoint and the already-built CLI binary. It is
careful not to touch compiler sources and writes all generated artifacts under
artifacts/.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import random
import shutil
import subprocess
import tempfile
import time
from pathlib import Path

import torch

from train_kappa_fuzzball import CharRnn, ModelConfig, decode, decode_keywords, normalize_source


def repo_root_from_script() -> Path:
    return Path(__file__).resolve().parents[1]


def load_checkpoint(path: Path) -> tuple[CharRnn, dict]:
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
    import re

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

    mapping = {identifier: f"{'I' if capitals[min(combinations[index], len(capitals) - 1)] else 'i'}{combinations[index]}" for index, identifier in enumerate(ids)}

    def replace(match: re.Match[str]) -> str:
        return mapping.get(match.group(0), match.group(0))

    return re.sub(r"\bi\d+\b", replace, text)


def classify_result(returncode: int, stdout: str, stderr: str, timed_out: bool) -> str:
    if timed_out:
        return "timeout"

    lowered = f"{stdout}\n{stderr}".lower()

    if "unhandled exception" in lowered or "system." in stderr.lower() or "stack trace" in lowered:
        return "crash"

    if returncode < 0:
        return "crash"

    if returncode == 0:
        return "ok"

    if "\nDiagnostics\n" in stdout or "\nDiagnostics\r\n" in stdout:
        return "diagnostic"

    return "failure"


def run_cli(cli_path: Path, repo_root: Path, temp_root: Path, source_path: Path, extra_args: list[str], timeout_seconds: float) -> tuple[int, str, str, bool]:
    timed_out = False

    try:
        result = subprocess.run(
            [str(cli_path), "--source-root", str(temp_root), *extra_args, str(source_path)],
            cwd=repo_root,
            capture_output=True,
            text=True,
            timeout=timeout_seconds,
        )
        return result.returncode, result.stdout, result.stderr, timed_out
    except subprocess.TimeoutExpired as ex:
        timed_out = True
        return -1, ex.stdout or "", ex.stderr or "", timed_out


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--checkpoint", default="artifacts/fuzzball-kappa/kappa-char-lstm.pt")
    parser.add_argument("--cli", default="src/Kappa.Compiler.Cli/bin/Debug/net10.0/Kappa.Compiler.Cli")
    parser.add_argument("--out-dir", default="artifacts/fuzzball-kappa/fuzz-run")
    parser.add_argument("--count", type=int, default=200)
    parser.add_argument("--sample-length", type=int, default=1000)
    parser.add_argument("--temperature", type=float, default=0.45)
    parser.add_argument("--prime", default="module main\n\n")
    parser.add_argument("--seed", type=int, default=214421)
    parser.add_argument("--timeout-seconds", type=float, default=5.0)
    parser.add_argument("--max-ids", type=int, default=8)
    parser.add_argument("--keep-diagnostics", type=int, default=20)
    parser.add_argument("--keep-successes", type=int, default=10)
    parser.add_argument("--verify-clean-checkpoint", action="append", default=["KBackendIR"])
    args = parser.parse_args()

    rng = random.Random(args.seed)
    repo_root = repo_root_from_script()
    checkpoint_path = Path(args.checkpoint)
    cli_path = Path(args.cli)
    out_dir = Path(args.out_dir)

    if not checkpoint_path.is_absolute():
        checkpoint_path = repo_root / checkpoint_path

    if not cli_path.is_absolute():
        cli_path = repo_root / cli_path

    if not out_dir.is_absolute():
        out_dir = repo_root / out_dir

    out_dir.mkdir(parents=True, exist_ok=True)
    for name in ["crashes", "timeouts", "diagnostics", "successes", "failures"]:
        (out_dir / name).mkdir(parents=True, exist_ok=True)

    model, checkpoint = load_checkpoint(checkpoint_path)
    vocab = checkpoint["vocab"]
    char_to_id = checkpoint["char_to_id"]
    keyword_codes = checkpoint.get("keyword_codes", {})
    code_to_keyword = {code: keyword for keyword, code in keyword_codes.items()}
    encoded_prime = normalize_source(args.prime, keyword_codes) if keyword_codes else args.prime
    prime_ids = [char_to_id[char] for char in encoded_prime if char in char_to_id]

    if not prime_ids:
        prime_ids = [char_to_id["\n"]]

    stats = {"ok": 0, "diagnostic": 0, "crash": 0, "timeout": 0, "failure": 0}
    saved_diagnostics = 0
    saved_successes = 0

    for index in range(1, args.count + 1):
        ids = model.sample(
            prime_ids=prime_ids,
            max_length=args.sample_length,
            temperature=args.temperature,
            device=torch.device("cpu"),
        )
        encoded = decode(ids, vocab).split("\1", 1)[0].strip()
        encoded = remap_identifiers(encoded, rng, args.max_ids).strip()
        decoded = decode_keywords(encoded, code_to_keyword)

        digest = hashlib.sha1(decoded.encode("utf-8")).hexdigest()
        started_at = time.time()

        with tempfile.TemporaryDirectory(prefix="kappa-fuzz-") as temp_dir:
            temp_root = Path(temp_dir)
            source_path = temp_root / "main.kp"
            source_path.write_text(decoded + "\n", encoding="utf-8")

            returncode, stdout, stderr, timed_out = run_cli(
                cli_path,
                repo_root,
                temp_root,
                source_path,
                [],
                args.timeout_seconds,
            )

        kind = classify_result(returncode, stdout, stderr, timed_out)

        if kind == "ok":
            for checkpoint in args.verify_clean_checkpoint:
                with tempfile.TemporaryDirectory(prefix="kappa-fuzz-verify-") as temp_dir:
                    temp_root = Path(temp_dir)
                    source_path = temp_root / "main.kp"
                    source_path.write_text(decoded + "\n", encoding="utf-8")
                    verify_returncode, verify_stdout, verify_stderr, verify_timed_out = run_cli(
                        cli_path,
                        repo_root,
                        temp_root,
                        source_path,
                        ["--verify", checkpoint],
                        args.timeout_seconds,
                    )

                verify_kind = classify_result(verify_returncode, verify_stdout, verify_stderr, verify_timed_out)

                if verify_kind in {"crash", "timeout", "failure"}:
                    kind = verify_kind
                    stdout = verify_stdout
                    stderr = verify_stderr
                    returncode = verify_returncode
                    timed_out = verify_timed_out
                    break

        stats[kind] += 1

        should_save = False
        target_dir = out_dir / "diagnostics"

        if kind == "crash":
            should_save = True
            target_dir = out_dir / "crashes"
        elif kind == "timeout":
            should_save = True
            target_dir = out_dir / "timeouts"
        elif kind == "failure":
            should_save = True
            target_dir = out_dir / "failures"
        elif kind == "diagnostic" and saved_diagnostics < args.keep_diagnostics:
            should_save = True
            saved_diagnostics += 1
            target_dir = out_dir / "diagnostics"
        elif kind == "ok" and saved_successes < args.keep_successes:
            should_save = True
            saved_successes += 1
            target_dir = out_dir / "successes"

        elapsed_ms = int((time.time() - started_at) * 1000.0)

        if should_save:
            case_dir = target_dir / f"{kind}-{digest}"
            case_dir.mkdir(parents=True, exist_ok=True)
            (case_dir / "main.kp").write_text(decoded + "\n", encoding="utf-8")
            (case_dir / "main.encoded.kp").write_text(encoded + "\n", encoding="utf-8")
            (case_dir / "stdout.txt").write_text(stdout, encoding="utf-8")
            (case_dir / "stderr.txt").write_text(stderr, encoding="utf-8")
            (case_dir / "meta.json").write_text(
                json.dumps(
                    {
                        "kind": kind,
                        "sha1": digest,
                        "returncode": returncode,
                        "timed_out": timed_out,
                        "elapsed_ms": elapsed_ms,
                        "index": index,
                        "temperature": args.temperature,
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
    print(json.dumps(stats, indent=2))


if __name__ == "__main__":
    main()
