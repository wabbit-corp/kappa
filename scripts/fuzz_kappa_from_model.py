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
import time
from pathlib import Path

import torch

from kappa_fuzz_lib import (
    bucket_for_kind,
    current_git_commit,
    repo_root_from_script,
    run_cli_source,
)
from train_kappa_fuzzball import CharRnn, ModelConfig, decode, decode_keywords, normalize_source


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
    compiler_commit = current_git_commit(repo_root)
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

    (out_dir / "run.json").write_text(
        json.dumps(
            {
                "compiler_commit": compiler_commit,
                "checkpoint": str(checkpoint_path),
                "cli": str(cli_path),
                "count": args.count,
                "sample_length": args.sample_length,
                "temperature": args.temperature,
                "prime": args.prime,
                "seed": args.seed,
                "timeout_seconds": args.timeout_seconds,
                "max_ids": args.max_ids,
                "verify_clean_checkpoint": args.verify_clean_checkpoint,
            },
            indent=2,
        ),
        encoding="utf-8",
    )

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
        stage = "compile"
        run = run_cli_source(
            cli_path,
            repo_root,
            decoded,
            stage=stage,
            timeout_seconds=args.timeout_seconds,
        )
        kind = run.kind

        if kind == "ok":
            for checkpoint in args.verify_clean_checkpoint:
                verify_stage = f"verify:{checkpoint}"
                verify_run = run_cli_source(
                    cli_path,
                    repo_root,
                    decoded,
                    stage=verify_stage,
                    timeout_seconds=args.timeout_seconds,
                )
                verify_kind = verify_run.kind
                if verify_kind in {"crash", "timeout", "failure"}:
                    kind = verify_kind
                    run = verify_run
                    stage = verify_stage
                    break

        stats[kind] += 1

        should_save = False
        target_dir = out_dir / "diagnostics"

        if kind == "crash":
            should_save = True
            target_dir = out_dir / bucket_for_kind(kind)
        elif kind == "timeout":
            should_save = True
            target_dir = out_dir / bucket_for_kind(kind)
        elif kind == "failure":
            should_save = True
            target_dir = out_dir / bucket_for_kind(kind)
        elif kind == "diagnostic" and saved_diagnostics < args.keep_diagnostics:
            should_save = True
            saved_diagnostics += 1
            target_dir = out_dir / bucket_for_kind(kind)
        elif kind == "ok" and saved_successes < args.keep_successes:
            should_save = True
            saved_successes += 1
            target_dir = out_dir / bucket_for_kind(kind)

        elapsed_ms = int((time.time() - started_at) * 1000.0)

        if should_save:
            case_dir = target_dir / f"{kind}-{digest}"
            case_dir.mkdir(parents=True, exist_ok=True)
            (case_dir / "main.kp").write_text(decoded + "\n", encoding="utf-8")
            (case_dir / "main.encoded.kp").write_text(encoded + "\n", encoding="utf-8")
            (case_dir / "stdout.txt").write_text(run.stdout, encoding="utf-8")
            (case_dir / "stderr.txt").write_text(run.stderr, encoding="utf-8")
            (case_dir / "meta.json").write_text(
                json.dumps(
                    {
                        "kind": kind,
                        "sha1": digest,
                        "returncode": run.returncode,
                        "timed_out": run.timed_out,
                        "elapsed_ms": elapsed_ms,
                        "index": index,
                        "temperature": args.temperature,
                        "compiler_commit": compiler_commit,
                        "checkpoint": str(checkpoint_path),
                        "cli": str(cli_path),
                        "stage": stage,
                        "prime": args.prime,
                        "sample_length": args.sample_length,
                        "seed": args.seed,
                        "verify_clean_checkpoint": args.verify_clean_checkpoint,
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
