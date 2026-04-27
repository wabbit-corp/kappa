import json
import sqlite3
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from scripts.kappa_fuzz_lib import (
    CaseRunResult,
    DEFAULT_EXECUTION_ORACLE_TEMPLATES,
    add_provenance,
    add_test_result,
    alpha_normalize_source,
    build_vocabulary,
    build_cli_command,
    build_execution_oracle_source,
    canonicalize_terminal_signature,
    classify_result,
    collapse_normalize_source,
    compute_normalized_sample_weights,
    ensure_schema,
    export_jsonl,
    export_weighted_training_samples,
    extract_inline_kappa_samples_from_fs,
    extract_kappa_keywords,
    family_normalize_source,
    is_runtime_seed_source,
    kind_from_case_dir,
    oracle_entry_bias_penalty,
    oracle_literal_bias_penalty,
    oracle_result_length_penalty,
    prepare_oracle_training_text,
    promotion_source_roots,
    reservoir_sample_slot,
    remap_identifiers,
    reset_fuzz_state,
    run_cli_source,
    runnable_entry_kind,
    stage_requires_repo_zig,
    sanitize_execution_oracle_body,
    sanitize_execution_oracle_preamble,
    seed_oracle_runtime_results,
    split_weighted_records_for_validation,
    summarize_execution_oracle_results,
    trace_steps_from_stdout,
    upsert_sample,
)


class InlineSampleExtractionTests(unittest.TestCase):
    def write_fs(self, text: str) -> Path:
        temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(temp_dir.cleanup)
        path = Path(temp_dir.name) / "SampleTests.fs"
        path.write_text(text, encoding="utf-8")
        return path

    def test_extracts_string_concat_programs(self) -> None:
        path = self.write_fs(
            """
module SampleTests

let program =
    [
        "module main"
        "let result = 40 + 2"
    ]
    |> String.concat "\\n"
"""
        )

        samples = extract_inline_kappa_samples_from_fs(path)

        self.assertEqual(["module main\nlet result = 40 + 2"], [sample.text for sample in samples])
        self.assertEqual(["inline-string-list"], [sample.source_label for sample in samples])

    def test_extracts_multiline_literal_programs(self) -> None:
        path = self.write_fs(
            """
module SampleTests

let source =
    createSource
        "main.kp"
        "module main\\nlet answer = 42"
"""
        )

        samples = extract_inline_kappa_samples_from_fs(path)

        self.assertEqual(["module main\nlet answer = 42"], [sample.text for sample in samples])
        self.assertEqual(["inline-string"], [sample.source_label for sample in samples])

    def test_ignores_non_program_strings(self) -> None:
        path = self.write_fs(
            """
module SampleTests

let message = "Unexpected declarations: %A"
let pathText = "main.kp"
"""
        )

        samples = extract_inline_kappa_samples_from_fs(path)

        self.assertEqual([], samples)


class ResultClassificationTests(unittest.TestCase):
    def test_timeout_wins(self) -> None:
        self.assertEqual("timeout", classify_result(0, "", "", True))

    def test_stack_overflow_counts_as_crash(self) -> None:
        self.assertEqual("crash", classify_result(134, "", "Stack overflow.", False))

    def test_diagnostics_are_not_failures(self) -> None:
        self.assertEqual("diagnostic", classify_result(1, "\nDiagnostics\nE", "", False))


class KeywordExtractionTests(unittest.TestCase):
    def test_extracts_kappa_keywords_from_source(self) -> None:
        keywords = extract_kappa_keywords(
            """
module main
trait Show a
handler Console
let result = match value
"""
        )
        self.assertEqual({"module", "main", "trait", "Show", "handler", "let", "result", "match"}, keywords)

    def test_extracts_newer_compiler_keywords_from_source(self) -> None:
        keywords = extract_kappa_keywords(
            """
module main
expect Foo
defer cleanup
using resource
yield value
instance Show Int
projection lens
"""
        )
        self.assertTrue({"expect", "defer", "using", "yield", "instance", "projection"}.issubset(keywords))


class RuntimeSeedTests(unittest.TestCase):
    def test_detects_runnable_entry_kind(self) -> None:
        self.assertEqual("result-int", runnable_entry_kind("module main\nlet result = 42"))
        self.assertEqual("main-io-int", runnable_entry_kind("module main\nmain : IO Int\nlet main = do\n    pure 42"))
        self.assertEqual("main-io-unit", runnable_entry_kind("module main\nmain : IO Unit\nlet main = do\n    pure ()"))

    def test_detects_runtime_seed_sources(self) -> None:
        self.assertTrue(is_runtime_seed_source("tests/foo.runtime_positive.bar/main.kp", "module main\nlet result = 42"))
        self.assertTrue(is_runtime_seed_source("tests/foo/main.kp", "module main\nlet result = 42\n--! assertExecute result 42"))
        self.assertFalse(is_runtime_seed_source("tests/foo/main.kp", "module main\nlet result = 42"))


class CanonicalizationTests(unittest.TestCase):
    def test_build_vocabulary_keeps_extra_operator_chars(self) -> None:
        char_to_id, vocab = build_vocabulary("module main\nlet result = 0\n", extra_chars=["#", "$", "/", "^", "~", "%"])
        del char_to_id
        for char in ["#", "$", "/", "^", "~", "%"]:
            self.assertIn(char, vocab)

    def test_alpha_normalize_source_preserves_keywords_and_renames_identifiers(self) -> None:
        left = """
module main
type Foo = (left : Foo)
let bar : Foo = bar
"""
        right = """
module main
type Baz = (left : Baz)
let qux : Baz = qux
"""

        self.assertEqual(alpha_normalize_source(left), alpha_normalize_source(right))
        self.assertIn("type", alpha_normalize_source(left))
        self.assertIn("let", alpha_normalize_source(left))

    def test_collapse_normalize_source_erases_literal_only_variation(self) -> None:
        left = """
module main

i0 : Int -> Int
let i0 i0 = i0 + 2
result : Int
let result = 42
"""
        right = """
module main

i7 : Int -> Int
let i7 i7 = i7 + 9
result : Int
let result = 0
"""

        self.assertEqual(collapse_normalize_source(left), collapse_normalize_source(right))

    def test_family_normalize_source_collapses_short_result_wrapper_family(self) -> None:
        left = """
module main

i0 : Int -> Int
let i0 i0 = i0 + 2
result : Int
let result = 42
"""
        right = """
module main

i7 : Int -> Int
let i7 i7 = i7 * 9
result : Int
let result = 0
"""

        self.assertEqual(family_normalize_source(left), family_normalize_source(right))

    def test_terminal_canonicalization_rewrites_main_symbols(self) -> None:
        left = "error: Checkpoint 'KBackendIR' requires calls in 'main.i3' to have an argument count matching the calling convention arity."
        right = "error: Checkpoint 'KBackendIR' requires calls in 'main.I0' to have an argument count matching the calling convention arity."

        self.assertEqual(canonicalize_terminal_signature(left), canonicalize_terminal_signature(right))
        self.assertIn("main.<sym>", canonicalize_terminal_signature(left))

    def test_validation_split_keeps_collapse_group_together(self) -> None:
        records = [
            {"sample_sha1": "a", "text": "module main\nlet result = 42\n", "collapse_hash": "g1"},
            {"sample_sha1": "b", "text": "module main\nlet result = 0\n", "collapse_hash": "g1"},
            {"sample_sha1": "c", "text": "module main\nlet result = 1 + 1\n", "collapse_hash": "g2"},
            {"sample_sha1": "d", "text": "module main\nlet result = 2 + 2\n", "collapse_hash": "g3"},
        ]

        train_records, validation_records = split_weighted_records_for_validation(
            records,
            validation_fraction=0.34,
            seed=1,
        )

        train_ids = {record["sample_sha1"] for record in train_records}
        validation_ids = {record["sample_sha1"] for record in validation_records}
        self.assertTrue({"a", "b"}.issubset(train_ids) or {"a", "b"}.issubset(validation_ids))
        self.assertTrue(train_ids)
        self.assertTrue(validation_ids)

    def test_kind_from_case_dir_falls_back_to_expected_signature(self) -> None:
        temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(temp_dir.cleanup)
        case_dir = Path(temp_dir.name) / "pending-failures-a655"
        case_dir.mkdir()
        (case_dir / "expected.txt").write_text("Stack overflow.\n", encoding="utf-8")

        self.assertEqual("crash", kind_from_case_dir(case_dir))


class WeightingTests(unittest.TestCase):
    def test_weighting_prefers_rare_trace_and_keywords(self) -> None:
        samples = [
            {"sample_sha1": "a", "text": "module main\ntrait Show a\nlet result = value", "eligible": True},
            {"sample_sha1": "b", "text": "module main\nlet result = value", "eligible": True},
            {"sample_sha1": "c", "text": "module main\ntrait Show a\nhandler Console\nlet result = value", "eligible": True},
        ]
        trace_records = {
            "a": [{"compiler_commit": "head", "trace_hash": "rare", "trace_step_count": 5}],
            "b": [{"compiler_commit": "head", "trace_hash": "common", "trace_step_count": 5}],
            "c": [{"compiler_commit": "head", "trace_hash": "common", "trace_step_count": 5}],
        }

        weights = compute_normalized_sample_weights(samples, trace_records_by_sample=trace_records, preferred_commit="head")

        self.assertGreater(weights["a"]["weight"], weights["b"]["weight"])
        self.assertGreater(weights["c"]["weight"], weights["b"]["weight"])

    def test_weighting_penalizes_syntactic_duplicates(self) -> None:
        samples = [
            {"sample_sha1": "a", "text": "module main\nlet foo = bar", "eligible": True},
            {"sample_sha1": "b", "text": "module main\nlet baz = qux", "eligible": True},
            {"sample_sha1": "c", "text": "module main\ntrait Show a\nlet quux = value", "eligible": True},
        ]

        weights = compute_normalized_sample_weights(samples, trace_records_by_sample={}, preferred_commit="head")

        self.assertEqual(weights["a"]["syntax_group_size"], 2)
        self.assertEqual(weights["b"]["syntax_group_size"], 2)
        self.assertEqual(weights["c"]["syntax_group_size"], 1)
        self.assertLess(weights["a"]["weight"], weights["c"]["weight"])

    def test_oracle_weighting_prefers_runtime_consensus_programs(self) -> None:
        samples = [
            {
                "sample_sha1": "a",
                "text": "module main\nlet result = 42",
                "eligible": True,
                "runtime_seed": True,
                "runnable_entry": True,
                "runtime_consensus_ok": True,
                "runtime_interpreter_codegen_ok": True,
            },
            {
                "sample_sha1": "b",
                "text": "module main\nlet result = value",
                "eligible": True,
                "runtime_seed": False,
                "runnable_entry": True,
                "runtime_consensus_ok": False,
                "runtime_interpreter_codegen_ok": False,
            },
        ]

        weights = compute_normalized_sample_weights(samples, trace_records_by_sample={}, preferred_commit="head", profile="oracle")

        self.assertAlmostEqual(1.0, weights["a"]["weight"])
        self.assertGreater(weights["a"]["runtime_multiplier"], weights["b"]["runtime_multiplier"])

    def test_oracle_weighting_downweights_trivial_literal_seeds(self) -> None:
        samples = [
            {
                "sample_sha1": "literal",
                "text": "module main\n\nmain : IO Int\nlet main = do\n    i0 <- pure 42\n    pure i0\n",
                "eligible": True,
                "runtime_seed": True,
                "runtime_assert_seed": True,
                "runnable_entry": True,
                "runtime_consensus_ok": True,
                "runtime_interpreter_codegen_ok": True,
            },
            {
                "sample_sha1": "structured",
                "text": "module main\n\nmain : IO Int\nlet main = do\n    i0 <- pure 40\n    pure (i0 + 2)\n",
                "eligible": True,
                "runtime_seed": True,
                "runtime_assert_seed": True,
                "runnable_entry": True,
                "runtime_consensus_ok": True,
                "runtime_interpreter_codegen_ok": True,
            },
        ]

        weights = compute_normalized_sample_weights(samples, trace_records_by_sample={}, preferred_commit="head", profile="oracle")

        self.assertLess(weights["literal"]["weight"], weights["structured"]["weight"])
        self.assertLess(weights["literal"]["literal_bias_penalty"], 1.0)
        self.assertEqual(1.0, weights["structured"]["literal_bias_penalty"])

    def test_oracle_weighting_prefers_result_entries_over_io_wrappers(self) -> None:
        samples = [
            {
                "sample_sha1": "main-wrapper",
                "text": "module main\n\nmain : IO Int\nlet main = do\n    let i0 = 40\n    pure (i0 + 2)\n",
                "eligible": True,
                "runtime_seed": True,
                "runtime_assert_seed": True,
                "runnable_entry": True,
                "runtime_consensus_ok": True,
                "runtime_interpreter_codegen_ok": True,
            },
            {
                "sample_sha1": "result-entry",
                "text": "module main\n\nresult : Int\nlet result = block\n    let base : Int = 40\n    let delta : Int = 2\n    base + delta\n",
                "eligible": True,
                "runtime_seed": True,
                "runtime_assert_seed": True,
                "runnable_entry": True,
                "runtime_consensus_ok": True,
                "runtime_interpreter_codegen_ok": True,
            },
        ]

        weights = compute_normalized_sample_weights(samples, trace_records_by_sample={}, preferred_commit="head", profile="oracle")

        self.assertLess(weights["main-wrapper"]["weight"], weights["result-entry"]["weight"])
        self.assertLess(weights["main-wrapper"]["entry_bias_penalty"], 1.0)
        self.assertEqual(1.0, weights["result-entry"]["entry_bias_penalty"])

    def test_oracle_weighting_penalizes_overly_short_result_bodies(self) -> None:
        samples = [
            {
                "sample_sha1": "short-result",
                "text": "module main\n\nresult : Int\nlet result = 0\n",
                "eligible": True,
                "runtime_seed": True,
                "runtime_assert_seed": True,
                "runnable_entry": True,
                "runtime_consensus_ok": True,
                "runtime_interpreter_codegen_ok": True,
            },
            {
                "sample_sha1": "longer-result",
                "text": "module main\n\nresult : Int\nlet result = block\n    let base : Int = 40\n    let delta : Int = 2\n    base + delta\n",
                "eligible": True,
                "runtime_seed": True,
                "runtime_assert_seed": True,
                "runnable_entry": True,
                "runtime_consensus_ok": True,
                "runtime_interpreter_codegen_ok": True,
            },
        ]

        weights = compute_normalized_sample_weights(samples, trace_records_by_sample={}, preferred_commit="head", profile="oracle")

        self.assertLess(weights["short-result"]["weight"], weights["longer-result"]["weight"])
        self.assertLess(oracle_result_length_penalty(samples[0]["text"]), 1.0)
        self.assertEqual(1.0, oracle_result_length_penalty(samples[1]["text"]))


class OracleSamplingTests(unittest.TestCase):
    def test_oracle_templates_bias_toward_result_entries(self) -> None:
        self.assertNotIn("main-io-unit", DEFAULT_EXECUTION_ORACLE_TEMPLATES)
        self.assertGreaterEqual(DEFAULT_EXECUTION_ORACLE_TEMPLATES.count("result-int"), 5)
        self.assertLessEqual(DEFAULT_EXECUTION_ORACLE_TEMPLATES.count("main-io-int"), 1)

    def test_literal_bias_penalty_catches_trivial_42_entry(self) -> None:
        self.assertLess(
            oracle_literal_bias_penalty("module main\n\nmain : IO Int\nlet main = do\n    i0 <- pure 42\n    pure i0\n"),
            1.0,
        )
        self.assertEqual(
            1.0,
            oracle_literal_bias_penalty("module main\n\nresult : Int\nlet result = 40 + 2\n"),
        )

    def test_entry_bias_penalty_downweights_io_entries(self) -> None:
        self.assertLess(
            oracle_entry_bias_penalty("module main\n\nmain : IO Int\nlet main = do\n    pure 42\n"),
            1.0,
        )
        self.assertEqual(
            1.0,
            oracle_entry_bias_penalty("module main\n\nresult : Int\nlet result = 40 + 2\n"),
        )

    def test_reservoir_sample_slot_can_replace_existing_audit_case(self) -> None:
        rng = mock.Mock()
        rng.randrange.return_value = 3
        slot = reservoir_sample_slot(seen_count=25, keep_limit=5, rng=rng)
        self.assertIsNotNone(slot)
        assert slot is not None
        self.assertGreaterEqual(slot, 0)
        self.assertLess(slot, 5)


class ResetTests(unittest.TestCase):
    def test_reset_clears_generated_state_but_keeps_checkpoint(self) -> None:
        temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(temp_dir.cleanup)
        repo_root = Path(temp_dir.name)
        pending = repo_root / "pending-failures"
        pending.mkdir()
        (pending / "summary.json").write_text("{}", encoding="utf-8")
        (pending / "failure-demo").mkdir()
        corpus_dir = repo_root / "artifacts" / "fuzzball-kappa"
        corpus_dir.mkdir(parents=True)
        for name in ["corpus.sqlite", "corpus.jsonl", "weighted-training-samples.jsonl"]:
            (corpus_dir / name).write_text("x", encoding="utf-8")
        model_dir = repo_root / "artifacts" / "fuzzball-kappa-weighted-current"
        model_dir.mkdir(parents=True)
        (model_dir / "kappa-char-lstm.pt").write_text("checkpoint", encoding="utf-8")
        (model_dir / "fuzz-run-demo").mkdir()
        (model_dir / "retest-demo").mkdir()

        reset_fuzz_state(repo_root, corpus_db=corpus_dir / "corpus.sqlite", model_dir=model_dir, pending_dir=pending)

        self.assertTrue((model_dir / "kappa-char-lstm.pt").exists())
        self.assertFalse((corpus_dir / "corpus.sqlite").exists())
        self.assertFalse((model_dir / "fuzz-run-demo").exists())
        self.assertEqual([], list(pending.iterdir()))


class PromotionPlanningTests(unittest.TestCase):
    def test_promotion_sources_include_raw_oracles(self) -> None:
        temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(temp_dir.cleanup)
        run_root = Path(temp_dir.name)

        failure_case = run_root / "failures" / "failure-demo"
        failure_case.mkdir(parents=True)
        (failure_case / "main.kp").write_text("module main\nlet result = 42\n", encoding="utf-8")

        oracle_case = run_root / "oracles" / "oracle-demo"
        oracle_case.mkdir(parents=True)
        (oracle_case / "main.kp").write_text("module main\nlet result = 42\n", encoding="utf-8")

        plans = promotion_source_roots(run_root)

        self.assertEqual(
            [
                {
                    "bucket_name": "failures",
                    "source_dir": run_root / "failures",
                    "minimized_dir": run_root / "minimized-failures",
                    "seed_dir": run_root / "recycled-seeds",
                },
                {
                    "bucket_name": "oracles",
                    "source_dir": run_root / "oracles",
                    "minimized_dir": None,
                    "seed_dir": None,
                },
            ],
            plans,
        )


class TraceReplayTests(unittest.TestCase):
    def test_build_cli_command_includes_trace_when_requested(self) -> None:
        command = build_cli_command(Path("/cli"), Path("/tmp/root"), Path("/tmp/root/main.kp"), stage="verify:KBackendIR", trace=True)
        self.assertEqual(
            ["/cli", "--source-root", "/tmp/root", "--trace", "--verify", "KBackendIR", "/tmp/root/main.kp"],
            command,
        )

    def test_build_cli_command_includes_backend_when_stage_requests_it(self) -> None:
        command = build_cli_command(Path("/cli"), Path("/tmp/root"), Path("/tmp/root/main.kp"), stage="verify:KBackendIR@dotnet-il", trace=False)
        self.assertEqual(
            ["/cli", "--source-root", "/tmp/root", "--backend", "dotnet-il", "--verify", "KBackendIR", "/tmp/root/main.kp"],
            command,
        )

    def test_build_cli_command_supports_runtime_stage(self) -> None:
        command = build_cli_command(Path("/cli"), Path("/tmp/root"), Path("/tmp/root/main.kp"), stage="run:zig:main.main", trace=False)
        self.assertEqual(
            ["/cli", "--source-root", "/tmp/root", "--backend", "zig", "--run", "main.main", "/tmp/root/main.kp"],
            command,
        )

    def test_stage_requires_repo_zig_for_runtime_and_verify_zig(self) -> None:
        self.assertTrue(stage_requires_repo_zig("run:zig:main.result"))
        self.assertTrue(stage_requires_repo_zig("verify:KBackendIR@zig"))
        self.assertFalse(stage_requires_repo_zig("run:dotnet-il:main.result"))
        self.assertFalse(stage_requires_repo_zig("verify:KBackendIR@dotnet-il"))

    def test_run_cli_source_injects_repo_zig_for_zig_stage(self) -> None:
        temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(temp_dir.cleanup)
        repo_root = Path(temp_dir.name)
        cli_path = repo_root / "cli"
        cli_path.write_text("", encoding="utf-8")

        captured_env: dict[str, str] = {}

        def fake_subprocess_run(command, cwd, capture_output, text, timeout, env):
            del command, cwd, capture_output, text, timeout
            captured_env.update(env)

            class Result:
                returncode = 0
                stdout = ""
                stderr = ""

            return Result()

        with mock.patch("scripts.kappa_fuzz_lib.resolve_repo_zig_executable", return_value="/tmp/repo-zig"), mock.patch(
            "scripts.kappa_fuzz_lib.subprocess.run",
            side_effect=fake_subprocess_run,
        ):
            result = run_cli_source(
                cli_path,
                repo_root,
                "module main\nlet result = 42\n",
                stage="run:zig:main.result",
                timeout_seconds=1.0,
            )

        self.assertEqual("ok", result.kind)
        self.assertEqual("/tmp/repo-zig", captured_env["KAPPA_ZIG_EXE"])

    def test_trace_parser_extracts_steps(self) -> None:
        stdout = """
Pipeline trace
parse file surface-source -> surface-source changed=true
advancePhase module KFrontIR.RAW -> KFrontIR.IMPORTS changed=true verify=KBackendIR
"""
        steps = trace_steps_from_stdout(stdout)
        self.assertEqual(
            [
                "advancePhase|module|KFrontIR.RAW|KFrontIR.IMPORTS|true|KBackendIR",
                "parse|file|surface-source|surface-source|true|-",
            ],
            steps,
        )


class ExecutionOracleTests(unittest.TestCase):
    def test_prepare_oracle_training_text_rewrites_module_and_drops_uio_main(self) -> None:
        prepared = prepare_oracle_training_text(
            """
module cases.e54_unit_lambda.main

-- Manual oracle seed.
twice : Int -> Int
let twice value = value * 2

result : Int
let result = twice 21

main : UIO Unit
let main = do
    println "noise"

--! assertExecute cases.e54_unit_lambda.main.result 42
"""
        )

        self.assertEqual(
            "\n".join(
                [
                    "module main",
                    "",
                    "twice : Int -> Int",
                    "let twice value = value * 2",
                    "",
                    "result : Int",
                    "let result = twice 21",
                ]
            ),
            prepared,
        )

    def test_prepare_oracle_training_text_rejects_host_imports_and_x_asserts(self) -> None:
        self.assertIsNone(
            prepare_oracle_training_text(
                """
module main
import host.dotnet.Sample.(term new)
result : Int
let result = 42
"""
            )
        )
        self.assertIsNone(
            prepare_oracle_training_text(
                """
module main
result : Int
let result = 42
--! x-assertEval result 42
"""
            )
        )

    def test_prepare_oracle_training_text_rejects_trivial_unit_and_constant_wrappers(self) -> None:
        self.assertIsNone(
            prepare_oracle_training_text(
                """
module main
main : IO Unit
let main = do
    pure ()
"""
            )
        )
        self.assertIsNone(
            prepare_oracle_training_text(
                """
module main
main : IO Int
let main = do
    i0 <- pure 42
    pure i0
"""
            )
        )
        self.assertIsNone(
            prepare_oracle_training_text(
                """
module main
result : Int
let result = 42
"""
            )
        )

    def test_prepare_oracle_training_text_rejects_result_do_wrapper(self) -> None:
        self.assertIsNone(
            prepare_oracle_training_text(
                """
module main
result : Int
let result = do
    i0 <- pure 42
    pure i0
"""
            )
        )

    def test_sanitize_execution_oracle_preamble_keeps_only_complete_leading_declarations(self) -> None:
        preamble = sanitize_execution_oracle_preamble(
            """
module main

sumList : List Int -> Int
let sumList xs =
    match xs
      case Nil -> 0
      case head :: tail -> head + sumList tail

broken : List Int -> Int
let broken xs =
    match xs
"""
        )

        self.assertEqual(
            "\n".join(
                [
                    "sumList : List Int -> Int",
                    "let sumList xs =",
                    "    match xs",
                    "      case Nil -> 0",
                    "      case head :: tail -> head + sumList tail",
                ]
            ),
            preamble,
        )

    def test_sanitize_execution_oracle_body_cuts_at_new_top_level_entry(self) -> None:
        body = sanitize_execution_oracle_body(
            """
match value
  case Nil -> 0
  case head :: tail -> head

result : Int
let result = 99
"""
        )

        self.assertEqual(
            "\n".join(
                [
                    "match value",
                    "  case Nil -> 0",
                    "  case head :: tail -> head",
                ]
            ),
            body,
        )

    def test_build_execution_oracle_source_inserts_entry_after_preamble(self) -> None:
        source = build_execution_oracle_source(
            "main-io-unit",
            "module main\n\ntwice : Int -> Int\nlet twice value = value * 2\nlet main = do\n    printInt 0",
            "let total = twice 21\n    printInt total\n\nmain : IO Unit\nlet main = do\n    printInt 0",
        )

        self.assertEqual(
            "\n".join(
                [
                    "module main",
                    "",
                    "twice : Int -> Int",
                    "let twice value = value * 2",
                    "main : IO Unit",
                    "let main = do",
                    "    let total = twice 21",
                    "    printInt total",
                    "",
                ]
            ),
            source,
        )

    def test_build_execution_oracle_source_result_template_rejects_do_body(self) -> None:
        source = build_execution_oracle_source(
            "result-int",
            "module main\n\ntwice : Int -> Int\nlet twice value = value * 2",
            "do\n    i0 <- pure 42\n    pure i0",
        )

        self.assertEqual(
            "\n".join(
                [
                    "module main",
                    "",
                    "twice : Int -> Int",
                    "let twice value = value * 2",
                    "result : Int",
                    "let result = 0",
                    "",
                ]
            ),
            source,
        )

    def test_oracle_identifier_remap_does_not_introduce_capitals(self) -> None:
        remapped = remap_identifiers("let i0 i1 = i0 + i1", rng=__import__("random").Random(1), max_ids=8, allow_capitalization=False)
        self.assertNotIn("I0", remapped)
        self.assertNotIn("I1", remapped)

    def test_execution_oracle_flags_output_mismatch(self) -> None:
        summary = summarize_execution_oracle_results(
            {
                "interpreter": CaseRunResult(0, "42\n", "", False, "ok", "run:interpreter:main.result"),
                "dotnet-il": CaseRunResult(0, "43\n", "", False, "ok", "run:dotnet-il:main.result"),
                "zig": CaseRunResult(0, "42\n", "", False, "ok", "run:zig:main.result"),
            }
        )

        self.assertEqual("oracle", summary["kind"])
        self.assertIn("output mismatch", summary["signature"])
        self.assertIn("dotnet-il", summary["signature"])

    def test_execution_oracle_flags_backend_disagreement(self) -> None:
        summary = summarize_execution_oracle_results(
            {
                "interpreter": CaseRunResult(0, "42\n", "", False, "ok", "run:interpreter:main.result"),
                "dotnet-il": CaseRunResult(1, "\nDiagnostics\nerror", "", False, "diagnostic", "run:dotnet-il:main.result"),
                "zig": CaseRunResult(0, "42\n", "", False, "ok", "run:zig:main.result"),
            }
        )

        self.assertEqual("oracle", summary["kind"])
        self.assertIn("backend disagreement", summary["signature"])

    def test_execution_oracle_treats_single_backend_crash_as_crash(self) -> None:
        summary = summarize_execution_oracle_results(
            {
                "interpreter": CaseRunResult(0, "42\n", "", False, "ok", "run:interpreter:main.result"),
                "dotnet-il": CaseRunResult(134, "", "Stack overflow.", False, "crash", "run:dotnet-il:main.result"),
                "zig": CaseRunResult(0, "42\n", "", False, "ok", "run:zig:main.result"),
            }
        )

        self.assertEqual("crash", summary["kind"])
        self.assertIn("backend disagreement", summary["signature"])

    def test_execution_oracle_normalizes_temp_paths_in_diagnostics(self) -> None:
        summary = summarize_execution_oracle_results(
            {
                "interpreter": CaseRunResult(
                    1,
                    "Diagnostics\n/var/folders/a/main.kp(1,1): error: nope",
                    "",
                    False,
                    "diagnostic",
                    "run:interpreter:main.main",
                ),
                "dotnet-il": CaseRunResult(
                    1,
                    "Diagnostics\n/var/folders/b/main.kp(1,1): error: nope",
                    "",
                    False,
                    "diagnostic",
                    "run:dotnet-il:main.main",
                ),
                "zig": CaseRunResult(
                    1,
                    "Diagnostics\n/var/folders/c/main.kp(1,1): error: nope",
                    "",
                    False,
                    "diagnostic",
                    "run:zig:main.main",
                ),
            }
        )

        self.assertEqual("diagnostic", summary["kind"])


class OracleExportTests(unittest.TestCase):
    def test_oracle_export_filters_and_rewrites_training_text(self) -> None:
        temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(temp_dir.cleanup)
        repo_root = Path(temp_dir.name)
        db_path = repo_root / "corpus.sqlite"
        out_path = repo_root / "oracle-weighted.jsonl"
        connection = sqlite3.connect(db_path)
        try:
            ensure_schema(connection)

            samples = {
                "path-only": (
                    "module main\nresult : Int\nlet result = 42\n",
                    "new-tests/foo.runtime_positive.bar/main.kp",
                ),
                "rewritable": (
                    """
module cases.example.main

twice : Int -> Int
let twice value = value * 2

result : Int
let result = twice 21

main : UIO Unit
let main = do
    println "ignored"

--! assertExecute cases.example.main.result 42
""",
                    "new-tests/cases.example/main.kp",
                ),
                "x-assert": (
                    """
module main
result : Int
let result = 42
--! x-assertEval result 42
""",
                    "new-tests/xassert/main.kp",
                ),
                "consensus": (
                    """
module package.sample.main

twice : Int -> Int
let twice value = value * 2

result : Int
let result = twice 21
""",
                    "new-tests/package.sample/main.kp",
                ),
            }

            for name, (text, source_path) in samples.items():
                sha1 = name
                upsert_sample(connection, sha1, text)
                add_provenance(
                    connection,
                    sample_sha1_value=sha1,
                    provenance_kind="runtime-seed",
                    source_group="runtime-seeds",
                    source_path=source_path,
                    used_for_training=True,
                    metadata={"entry_kind": runnable_entry_kind(text)},
                )

            add_test_result(
                connection,
                sample_sha1_value="consensus",
                compiler_commit="head",
                result_kind="ok",
                returncode=0,
                timed_out=False,
                terminal_signature="oracle: all backends agree",
                artifact_path="artifacts/oracle-seed/consensus",
                run_path="artifacts/oracle-seed",
                metadata={
                    "backend_results": {
                        "interpreter": {"kind": "ok"},
                        "dotnet-il": {"kind": "ok"},
                        "zig": {"kind": "ok"},
                    }
                },
            )
            connection.commit()
        finally:
            connection.close()

        summary = export_weighted_training_samples(
            repo_root,
            db_path=db_path,
            out_path=out_path,
            preferred_commit="head",
            profile="oracle",
        )

        self.assertEqual(1, summary["exported"])
        self.assertEqual(1, summary["deduplicated"])
        records = [json.loads(line) for line in out_path.read_text(encoding="utf-8").splitlines() if line.strip()]
        texts = {record["sample_sha1"]: record["text"] for record in records}

        self.assertNotIn("path-only", texts)
        self.assertNotIn("x-assert", texts)
        self.assertIn("consensus", texts)
        self.assertTrue(texts["consensus"].startswith("module main\n"))


class DefaultExportDedupTests(unittest.TestCase):
    def test_default_export_deduplicates_collapse_equivalent_samples(self) -> None:
        temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(temp_dir.cleanup)
        repo_root = Path(temp_dir.name)
        db_path = repo_root / "corpus.sqlite"
        out_path = repo_root / "weighted.jsonl"
        connection = sqlite3.connect(db_path)
        try:
            ensure_schema(connection)
            samples = {
                "plain": """
module main

i0 : Int -> Int
let i0 i0 = i0 + 2
result : Int
let result = 42
""",
                "tested": """
module main

i7 : Int -> Int
let i7 i7 = i7 + 9
result : Int
let result = 0
""",
            }
            for name, text in samples.items():
                upsert_sample(connection, name, text)
                add_provenance(
                    connection,
                    sample_sha1_value=name,
                    provenance_kind="fixture",
                    source_group="fixtures",
                    source_path=f"tests/{name}/main.kp",
                    used_for_training=True,
                    metadata={},
                )
            add_test_result(
                connection,
                sample_sha1_value="tested",
                compiler_commit="head",
                result_kind="ok",
                returncode=0,
                timed_out=False,
                terminal_signature="ok",
                artifact_path="artifacts/tested",
                run_path="artifacts",
                metadata={},
            )
            connection.commit()
        finally:
            connection.close()

        summary = export_weighted_training_samples(
            repo_root,
            db_path=db_path,
            out_path=out_path,
            preferred_commit="head",
            profile="default",
        )

        self.assertEqual(1, summary["exported"])
        self.assertEqual(1, summary["deduplicated"])
        records = [json.loads(line) for line in out_path.read_text(encoding="utf-8").splitlines() if line.strip()]
        self.assertEqual(["tested"], [record["sample_sha1"] for record in records])


class CorpusExportSummaryTests(unittest.TestCase):
    def test_export_jsonl_reports_static_and_retrain_eligible_counts_separately(self) -> None:
        temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(temp_dir.cleanup)
        db_path = Path(temp_dir.name) / "corpus.sqlite"
        jsonl_path = Path(temp_dir.name) / "corpus.jsonl"
        connection = sqlite3.connect(db_path)
        try:
            ensure_schema(connection)

            static_text = "module main\nlet result = 42\n"
            fuzz_text = "module main\nlet result = i0 + 1\n"
            oracle_text = """module main

result : Int
let result = block
    let base : Int = 40
    let delta : Int = 2
    base + delta
"""

            upsert_sample(connection, "static", static_text)
            add_provenance(
                connection,
                sample_sha1_value="static",
                provenance_kind="corpus-file",
                source_group="fixtures",
                source_path="tests/static/main.kp",
                used_for_training=True,
                metadata={},
            )

            upsert_sample(connection, "fuzz", fuzz_text)
            add_provenance(
                connection,
                sample_sha1_value="fuzz",
                provenance_kind="fuzz-artifact",
                source_group="successes",
                source_path="artifacts/fuzz/successes/fuzz",
                used_for_training=False,
                metadata={},
            )
            add_test_result(
                connection,
                sample_sha1_value="fuzz",
                compiler_commit="head",
                result_kind="ok",
                returncode=0,
                timed_out=False,
                terminal_signature="ok",
                artifact_path="artifacts/fuzz/successes/fuzz",
                run_path="artifacts/fuzz",
                metadata={},
            )

            upsert_sample(connection, "oracle", oracle_text)
            add_provenance(
                connection,
                sample_sha1_value="oracle",
                provenance_kind="fuzz-artifact",
                source_group="successes",
                source_path="artifacts/oracle/successes/oracle",
                used_for_training=False,
                metadata={},
            )
            add_test_result(
                connection,
                sample_sha1_value="oracle",
                compiler_commit="head",
                result_kind="ok",
                returncode=0,
                timed_out=False,
                terminal_signature="oracle: all backends agree",
                artifact_path="artifacts/oracle/successes/oracle",
                run_path="artifacts/oracle",
                metadata={
                    "backend_results": {
                        "interpreter": {"kind": "ok"},
                        "dotnet-il": {"kind": "ok"},
                        "zig": {"kind": "ok"},
                    }
                },
            )
            connection.commit()

            summary = export_jsonl(connection, jsonl_path)
        finally:
            connection.close()

        self.assertEqual(3, summary["sample_count"])
        self.assertEqual(1, summary["training_sample_count"])
        self.assertEqual(1, summary["static_training_sample_count"])
        self.assertEqual(3, summary["default_retrain_eligible_sample_count"])
        self.assertEqual(1, summary["oracle_retrain_eligible_sample_count"])

        records = [json.loads(line) for line in jsonl_path.read_text(encoding="utf-8").splitlines() if line.strip()]
        by_id = {record["sample_sha1"]: record for record in records}
        self.assertEqual(
            {
                "static_training": True,
                "default_retrain_eligible": True,
                "oracle_retrain_eligible": False,
            },
            by_id["static"]["eligibility"],
        )
        self.assertEqual(
            {
                "static_training": False,
                "default_retrain_eligible": True,
                "oracle_retrain_eligible": False,
            },
            by_id["fuzz"]["eligibility"],
        )
        self.assertEqual(
            {
                "static_training": False,
                "default_retrain_eligible": True,
                "oracle_retrain_eligible": True,
            },
            by_id["oracle"]["eligibility"],
        )


class OracleRetestSeedingTests(unittest.TestCase):
    def test_seed_oracle_runtime_results_retests_cleaned_runtime_seed(self) -> None:
        temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(temp_dir.cleanup)
        repo_root = Path(temp_dir.name)
        db_path = repo_root / "corpus.sqlite"
        out_dir = repo_root / "artifacts" / "oracle-seed-retests-head"
        cli_path = repo_root / "cli"
        cli_path.write_text("", encoding="utf-8")
        connection = sqlite3.connect(db_path)
        try:
            ensure_schema(connection)
            text = """
module cases.example.main

twice : Int -> Int
let twice value = value * 2

result : Int
let result = twice 21

main : UIO Unit
let main = do
    println "ignored"

--! assertExecute cases.example.main.result 42
"""
            upsert_sample(connection, "rewritable", text)
            add_provenance(
                connection,
                sample_sha1_value="rewritable",
                provenance_kind="runtime-seed",
                source_group="runtime-seeds",
                source_path="new-tests/cases.example/main.kp",
                used_for_training=True,
                metadata={"entry_kind": runnable_entry_kind(text)},
            )
            connection.commit()
        finally:
            connection.close()

        calls: list[tuple[str, str]] = []

        def fake_run_cli_source(_cli_path, _repo_root, source, *, stage, timeout_seconds, environment=None):
            del timeout_seconds, environment
            calls.append((stage, source))
            if stage == "compile":
                return CaseRunResult(0, "", "", False, "ok", stage)
            return CaseRunResult(0, "42\n", "", False, "ok", stage)

        with mock.patch("scripts.kappa_fuzz_lib.current_git_commit", return_value="head"), mock.patch(
            "scripts.kappa_fuzz_lib.resolve_repo_zig_executable",
            return_value="/tmp/zig",
        ), mock.patch(
            "scripts.kappa_fuzz_lib.run_cli_source",
            side_effect=fake_run_cli_source,
        ):
            summary = seed_oracle_runtime_results(
                repo_root,
                db_path=db_path,
                cli_path=cli_path,
                out_dir=out_dir,
                timeout_seconds=5.0,
            )

        self.assertEqual(1, summary["retested"])
        self.assertEqual(1, summary["ok"])
        self.assertEqual(
            "module main\n\ntwice : Int -> Int\nlet twice value = value * 2\n\nresult : Int\nlet result = twice 21",
            calls[0][1].strip(),
        )
        self.assertEqual(
            ["compile", "run:interpreter:main.result", "run:dotnet-il:main.result", "run:zig:main.result"],
            [stage for stage, _source in calls],
        )


if __name__ == "__main__":
    unittest.main()
