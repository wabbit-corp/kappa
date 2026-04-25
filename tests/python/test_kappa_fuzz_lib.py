import tempfile
import unittest
from pathlib import Path

from scripts.kappa_fuzz_lib import classify_result, extract_inline_kappa_samples_from_fs


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


if __name__ == "__main__":
    unittest.main()
