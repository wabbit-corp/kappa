import tempfile
import unittest
from pathlib import Path

from scripts.kappa_fuzz_lib import (
    alpha_normalize_source,
    build_cli_command,
    canonicalize_terminal_signature,
    classify_result,
    compute_normalized_sample_weights,
    extract_inline_kappa_samples_from_fs,
    extract_kappa_keywords,
    kind_from_case_dir,
    reset_fuzz_state,
    trace_steps_from_stdout,
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


class CanonicalizationTests(unittest.TestCase):
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

    def test_terminal_canonicalization_rewrites_main_symbols(self) -> None:
        left = "error: Checkpoint 'KBackendIR' requires calls in 'main.i3' to have an argument count matching the calling convention arity."
        right = "error: Checkpoint 'KBackendIR' requires calls in 'main.I0' to have an argument count matching the calling convention arity."

        self.assertEqual(canonicalize_terminal_signature(left), canonicalize_terminal_signature(right))
        self.assertIn("main.<sym>", canonicalize_terminal_signature(left))

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
        self.assertAlmostEqual(1.0, weights["a"]["weight"])

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


class TraceReplayTests(unittest.TestCase):
    def test_build_cli_command_includes_trace_when_requested(self) -> None:
        command = build_cli_command(Path("/cli"), Path("/tmp/root"), Path("/tmp/root/main.kp"), stage="verify:KBackendIR", trace=True)
        self.assertEqual(
            ["/cli", "--source-root", "/tmp/root", "--trace", "--verify", "KBackendIR", "/tmp/root/main.kp"],
            command,
        )

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


if __name__ == "__main__":
    unittest.main()
