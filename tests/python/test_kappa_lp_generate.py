import tempfile
import unittest
from pathlib import Path

from scripts import kappa_lp_generate


class LambdaPrologKappaGeneratorTests(unittest.TestCase):
    def test_parse_elpi_samples_extracts_fixture_source(self) -> None:
        text = """%%KAPPA-SAMPLE 0
module main

result : Unit
let result = ()
%%END-KAPPA-SAMPLE
"""

        samples = kappa_lp_generate.parse_elpi_samples(text)

        self.assertEqual(1, len(samples))
        self.assertEqual(0, samples[0].index)
        self.assertEqual("module main\n\nresult : Unit\nlet result = ()\n", samples[0].source)

    def test_local_elpi_can_generate_kappa_when_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=2,
            depth=3,
        )
        samples = kappa_lp_generate.parse_elpi_samples(output)

        self.assertEqual(2, len(samples))
        for sample in samples:
            self.assertIn("module main", sample.source)
            self.assertIn("--! assertNoErrors", sample.source)
            self.assertIn("--! assertType result", sample.source)

    def test_local_elpi_emits_broader_spec_features_when_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=38,
            depth=4,
        )
        combined_source = "\n".join(sample.source for sample in kappa_lp_generate.parse_elpi_samples(output))

        self.assertIn("type GeneratedTuple = (Int, Int)", combined_source)
        self.assertIn("trait GeneratedScore a", combined_source)
        self.assertIn("generated_15 : Int", combined_source)
        self.assertIn("let (left, right) = pair", combined_source)
        self.assertIn("generated_18 : List Int", combined_source)
        self.assertIn("[ for i in 1 .. 3, yield (i + 1) ]", combined_source)
        self.assertIn("generated_19 : IO Int", combined_source)
        self.assertIn("summon (GeneratedScore Int)", combined_source)
        self.assertIn("generatedResize { height = 3, image = 1, width = 2 }", combined_source)
        self.assertIn("((generatedMakeConst 41) ())", combined_source)
        self.assertIn("((\\() -> 42) ())", combined_source)
        self.assertIn("let (@x : Int) = 7", combined_source)
        self.assertIn("match 0x100", combined_source)
        self.assertIn("{ \"ada\": 1, \"grace\": 2 }", combined_source)
        self.assertIn("base.{ y := 41 }", combined_source)
        self.assertIn("42.generatedEcho", combined_source)
        self.assertIn("generatedSize (1 :: 2 :: 3 :: Nil)", combined_source)

    def test_local_elpi_can_start_from_later_candidate_when_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=1,
            depth=4,
            start_index=18,
        )
        samples = kappa_lp_generate.parse_elpi_samples(output)

        self.assertEqual(1, len(samples))
        self.assertEqual(18, samples[0].index)
        self.assertIn("generated_18 : List Int", samples[0].source)

    def test_generated_samples_compile_when_elpi_is_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        cli = kappa_lp_generate.resolve_kappa_cli()
        if cli is None:
            self.skipTest("release Kappa CLI binary is not available")

        output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=2,
            depth=3,
        )
        samples = kappa_lp_generate.parse_elpi_samples(output)

        with tempfile.TemporaryDirectory() as temp_dir:
            case_dirs = kappa_lp_generate.write_fixture_cases(samples, Path(temp_dir))
            kappa_lp_generate.validate_cases(case_dirs, cli=cli)


if __name__ == "__main__":
    unittest.main()
