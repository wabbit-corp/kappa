#!/usr/bin/env python3
"""Single entrypoint for Kappa fuzzing, corpus management, and model training."""

from __future__ import annotations

import argparse
import json
from datetime import datetime
from pathlib import Path

from kappa_fuzz_lib import (
    backfill_traces,
    current_git_commit,
    export_weighted_training_samples,
    fuzz_from_checkpoint,
    pipeline,
    promote_pending_failures,
    reset_fuzz_state,
    repo_root_from_script,
    resolve_path,
    retest_cases,
    train_model,
    update_corpus_store,
)

VERIFY_STAGES = ["verify:KBackendIR@dotnet-il", "verify:KBackendIR@zig"]


def add_common_paths(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--cli", default="src/Kappa.Compiler.Cli/bin/Release/net10.0/Kappa.Compiler.Cli")
    parser.add_argument("--corpus-db", default="artifacts/fuzzball-kappa/corpus.sqlite")
    parser.add_argument("--model-dir", default="artifacts/fuzzball-kappa-weighted-current")


def main() -> None:
    repo_root = repo_root_from_script()

    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    pipeline_parser = subparsers.add_parser("pipeline", help="Refresh corpus, train, fuzz, promote, and clean up.")
    add_common_paths(pipeline_parser)
    pipeline_parser.add_argument("--count", type=int, default=2000)
    pipeline_parser.add_argument("--steps", type=int, default=500)
    pipeline_parser.add_argument("--temperature", type=float, default=0.55)
    pipeline_parser.add_argument("--timeout-seconds", type=float, default=3.0)
    pipeline_parser.add_argument("--sample-length", type=int, default=1000)
    pipeline_parser.add_argument("--skip-build", action="store_true")
    pipeline_parser.add_argument("--skip-train", action="store_true")
    pipeline_parser.add_argument("--no-cleanup", action="store_true")

    continue_parser = subparsers.add_parser("continue", help="Fuzz from the latest checkpoint, then promote and refresh the corpus.")
    add_common_paths(continue_parser)
    continue_parser.add_argument("--count", type=int, default=2000)
    continue_parser.add_argument("--temperature", type=float, default=0.55)
    continue_parser.add_argument("--timeout-seconds", type=float, default=3.0)
    continue_parser.add_argument("--sample-length", type=int, default=1000)
    continue_parser.add_argument("--checkpoint", default=None)

    corpus_parser = subparsers.add_parser("update-corpus", help="Refresh the SQLite/JSONL sample store.")
    add_common_paths(corpus_parser)

    weights_parser = subparsers.add_parser("weights", help="Export weighted training samples from the corpus store.")
    add_common_paths(weights_parser)
    weights_parser.add_argument("--out", default=None)
    weights_parser.add_argument("--preferred-commit", default=None)

    train_parser = subparsers.add_parser("train", help="Train or retrain the latest model.")
    add_common_paths(train_parser)
    train_parser.add_argument("--steps", type=int, default=500)
    train_parser.add_argument("--weighted-samples", default=None)

    fuzz_parser = subparsers.add_parser("fuzz", help="Generate programs from a checkpoint and run them through the compiler.")
    add_common_paths(fuzz_parser)
    fuzz_parser.add_argument("--count", type=int, default=2000)
    fuzz_parser.add_argument("--temperature", type=float, default=0.55)
    fuzz_parser.add_argument("--timeout-seconds", type=float, default=3.0)
    fuzz_parser.add_argument("--sample-length", type=int, default=1000)
    fuzz_parser.add_argument("--checkpoint", default=None)
    fuzz_parser.add_argument("--out-dir", default=None)

    retest_parser = subparsers.add_parser("retest", help="Retest pending or saved cases on the current compiler.")
    retest_parser.add_argument("roots", nargs="+")
    retest_parser.add_argument("--cli", default="src/Kappa.Compiler.Cli/bin/Release/net10.0/Kappa.Compiler.Cli")
    retest_parser.add_argument("--out-dir", required=True)
    retest_parser.add_argument("--timeout-seconds", type=float, default=3.0)

    promote_parser = subparsers.add_parser("promote", help="Minimize, cluster, and promote unique failures/crashes/timeouts.")
    promote_parser.add_argument("roots", nargs="+")
    promote_parser.add_argument("--cli", default="src/Kappa.Compiler.Cli/bin/Release/net10.0/Kappa.Compiler.Cli")
    promote_parser.add_argument("--pending-dir", default="pending-failures")
    promote_parser.add_argument("--timeout-seconds", type=float, default=3.0)

    trace_parser = subparsers.add_parser("backfill-traces", help="Replay tested samples with --trace and store trace hashes in the corpus DB.")
    add_common_paths(trace_parser)
    trace_parser.add_argument("--timeout-seconds", type=float, default=3.0)
    trace_parser.add_argument("--compiler-commit", default=None)
    trace_parser.add_argument("--limit", type=int, default=None)

    reset_parser = subparsers.add_parser("reset", help="Clear pending failures, corpus DB exports, and old fuzz/retest runs.")
    add_common_paths(reset_parser)
    reset_parser.add_argument("--pending-dir", default="pending-failures")

    args = parser.parse_args()

    if args.command == "pipeline":
        summary = pipeline(
            repo_root,
            cli_path=resolve_path(repo_root, args.cli),
            corpus_db=resolve_path(repo_root, args.corpus_db),
            model_dir=resolve_path(repo_root, args.model_dir),
            count=args.count,
            steps=args.steps,
            temperature=args.temperature,
            timeout_seconds=args.timeout_seconds,
            sample_length=args.sample_length,
            skip_build=args.skip_build,
            skip_train=args.skip_train,
            cleanup=not args.no_cleanup,
        )
        print(json.dumps(summary, indent=2))
        return

    if args.command == "continue":
        model_dir = resolve_path(repo_root, args.model_dir)
        cli_path = resolve_path(repo_root, args.cli)
        corpus_db = resolve_path(repo_root, args.corpus_db)
        checkpoint_path = resolve_path(repo_root, args.checkpoint) if args.checkpoint else model_dir / "kappa-char-lstm.pt"
        compiler_commit = current_git_commit(repo_root)
        timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
        run_dir = model_dir / f"fuzz-run-{timestamp}-{Path(checkpoint_path).stem}-{compiler_commit[:8]}-{args.count}"
        fuzz_summary = fuzz_from_checkpoint(
            repo_root,
            checkpoint_path=checkpoint_path,
            cli_path=cli_path,
            out_dir=run_dir,
            count=args.count,
            sample_length=args.sample_length,
            temperature=args.temperature,
            prime="module main\n\n",
            seed=214421,
            timeout_seconds=args.timeout_seconds,
            max_ids=8,
            keep_diagnostics=10,
            keep_successes=5,
            verify_stages=VERIFY_STAGES,
        )
        promotion = promote_pending_failures(
            repo_root,
            roots=[run_dir],
            cli_path=cli_path,
            pending_dir=repo_root / "pending-failures",
            timeout_seconds=args.timeout_seconds,
        )
        corpus = update_corpus_store(
            repo_root,
            db_path=corpus_db,
            jsonl_path=corpus_db.with_suffix(".jsonl"),
        )
        print(json.dumps({"run_dir": str(run_dir), "fuzz": fuzz_summary, "promotion": promotion, "corpus": corpus}, indent=2))
        return

    if args.command == "update-corpus":
        corpus_db = resolve_path(repo_root, args.corpus_db)
        print(json.dumps(update_corpus_store(repo_root, db_path=corpus_db, jsonl_path=corpus_db.with_suffix(".jsonl")), indent=2))
        return

    if args.command == "weights":
        corpus_db = resolve_path(repo_root, args.corpus_db)
        out_path = resolve_path(repo_root, args.out) if args.out else corpus_db.parent / "weighted-training-samples.jsonl"
        print(json.dumps(export_weighted_training_samples(repo_root, db_path=corpus_db, out_path=out_path, preferred_commit=args.preferred_commit), indent=2))
        return

    if args.command == "reset":
        print(
            json.dumps(
                reset_fuzz_state(
                    repo_root,
                    corpus_db=resolve_path(repo_root, args.corpus_db),
                    model_dir=resolve_path(repo_root, args.model_dir),
                    pending_dir=resolve_path(repo_root, args.pending_dir),
                ),
                indent=2,
            )
        )
        return

    if args.command == "train":
        corpus_db = resolve_path(repo_root, args.corpus_db)
        weighted = resolve_path(repo_root, args.weighted_samples) if args.weighted_samples else corpus_db.parent / "weighted-training-samples.jsonl"
        print(json.dumps(train_model(repo_root, out_dir=resolve_path(repo_root, args.model_dir), weighted_samples_path=weighted, steps=args.steps), indent=2))
        return

    if args.command == "fuzz":
        model_dir = resolve_path(repo_root, args.model_dir)
        checkpoint = resolve_path(repo_root, args.checkpoint) if args.checkpoint else model_dir / "kappa-char-lstm.pt"
        out_dir = resolve_path(repo_root, args.out_dir) if args.out_dir else model_dir / f"fuzz-run-manual-{args.count}"
        print(
            json.dumps(
                fuzz_from_checkpoint(
                    repo_root,
                    checkpoint_path=checkpoint,
                    cli_path=resolve_path(repo_root, args.cli),
                    out_dir=out_dir,
                    count=args.count,
                    sample_length=args.sample_length,
                    temperature=args.temperature,
                    prime="module main\n\n",
                    seed=214421,
                    timeout_seconds=args.timeout_seconds,
                    max_ids=8,
                    keep_diagnostics=10,
                    keep_successes=5,
                    verify_stages=VERIFY_STAGES,
                ),
                indent=2,
            )
        )
        return

    if args.command == "retest":
        print(
            json.dumps(
                retest_cases(
                    repo_root,
                    roots=args.roots,
                    cli_path=resolve_path(repo_root, args.cli),
                    out_dir=resolve_path(repo_root, args.out_dir),
                    timeout_seconds=args.timeout_seconds,
                    verify_stages=VERIFY_STAGES,
                ),
                indent=2,
            )
        )
        return

    if args.command == "promote":
        print(
            json.dumps(
                promote_pending_failures(
                    repo_root,
                    roots=[resolve_path(repo_root, root) for root in args.roots],
                    cli_path=resolve_path(repo_root, args.cli),
                    pending_dir=resolve_path(repo_root, args.pending_dir),
                    timeout_seconds=args.timeout_seconds,
                ),
                indent=2,
            )
        )
        return

    if args.command == "backfill-traces":
        print(
            json.dumps(
                backfill_traces(
                    repo_root,
                    db_path=resolve_path(repo_root, args.corpus_db),
                    cli_path=resolve_path(repo_root, args.cli),
                    timeout_seconds=args.timeout_seconds,
                    compiler_commit=args.compiler_commit,
                    limit=args.limit,
                ),
                indent=2,
            )
        )
        return


if __name__ == "__main__":
    main()
