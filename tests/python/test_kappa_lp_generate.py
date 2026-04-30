import re
import tempfile
import unittest
from pathlib import Path

from scripts import kappa_lp_generate


def canonical_sample_shape(source: str) -> str:
    source = re.sub(r"generated_\d+", "generated_N", source)
    source = re.sub(r'"(?:[^"\\]|\\.)*"', '"S"', source)
    return re.sub(r"\b(?:0x[0-9A-Fa-f]+|0o[0-7]+|\d+)\b", "N", source)


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

    def test_local_elpi_generated_templates_are_structurally_distinct_when_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=500,
            depth=5,
        )
        samples = kappa_lp_generate.parse_elpi_samples(output)
        seen: dict[str, int] = {}
        duplicates: list[tuple[int, int]] = []

        for sample in samples:
            shape = canonical_sample_shape(sample.source)
            previous = seen.get(shape)
            if previous is None:
                seen[shape] = sample.index
            else:
                duplicates.append((previous, sample.index))

        self.assertEqual([], duplicates)

    def test_local_elpi_depth_unlocks_ranked_synthesis_when_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=92,
            depth=5,
        )
        samples = kappa_lp_generate.parse_elpi_samples(output)

        self.assertEqual(92, len(samples))
        self.assertIn("generated_38 : Int", samples[38].source)
        self.assertIn("generated_39 : Bool", samples[39].source)
        self.assertIn("generated_40 : Unit", samples[40].source)
        self.assertIn("generated_41 : GeneratedTuple", samples[41].source)
        self.assertIn("generated_42 : Option Int", samples[42].source)
        self.assertIn("generated_43 : (Int -> Int)", samples[43].source)
        self.assertIn("block", samples[44].source)
        self.assertIn("let\n", samples[45].source)
        self.assertIn("let (@x : Int)", samples[46].source)
        self.assertIn("[ for x in [1, 2, 3]", samples[47].source)
        self.assertIn("do\n", samples[48].source)
        self.assertIn("summon (GeneratedScore Int)", samples[49].source)
        self.assertIn(".generatedEcho", samples[50].source)
        self.assertIn("match (GeneratedLeft", samples[51].source)
        self.assertIn(".{ right = ", samples[52].source)
        self.assertIn("generatedResize { image = ", samples[53].source)
        self.assertIn("generatedMakeConst", samples[54].source)
        self.assertIn("{| for x in [1, 2, 3]", samples[55].source)
        self.assertIn("{ for x in [1, 2, 3]", samples[56].source)
        self.assertIn("rankedHelper : (Int -> Int)", samples[57].source)
        self.assertIn("trait RankedScore a", samples[58].source)
        self.assertIn('#"raw lambda-prolog"#', samples[59].source)
        self.assertIn("match ((0 + 0) == (0 + 0))", samples[60].source)
        self.assertIn("let (x = captured, y = other) = pair", samples[61].source)
        self.assertIn("base.{ y := ", samples[62].source)
        self.assertIn("let staged = ", samples[63].source)
        self.assertIn("let (@x : Int) = ", samples[64].source)
        self.assertIn("((.generatedEcho) ", samples[65].source)
        self.assertIn("[ yield ", samples[66].source)
        self.assertIn("{| yield ", samples[67].source)
        self.assertIn("{ yield \"only\" : ", samples[68].source)
        self.assertIn("match GeneratedRight", samples[69].source)
        self.assertIn("type RankedAlias = Int", samples[70].source)
        self.assertIn("data RankedBox : Type", samples[71].source)
        self.assertIn("generatedNeed @(", samples[72].source)
        self.assertIn(")?.generatedEcho", samples[73].source)
        self.assertIn("let (captured : Int) =", samples[74].source)
        self.assertIn("whole@(x = captured, y = other)", samples[75].source)
        self.assertIn("let (left, right) = pair", samples[76].source)
        self.assertIn("match (Some ", samples[77].source)
        self.assertIn("case head :: tail -> head", samples[78].source)
        self.assertIn("generatedMakeConst @Int", samples[79].source)
        self.assertIn("generatedUseWidth { width }", samples[80].source)
        self.assertIn("on conflict keep first", samples[81].source)
        self.assertIn("skip 1, take 2", samples[82].source)
        self.assertIn("order by x desc", samples[83].source)
        self.assertIn("scoped effect Ask", samples[84].source)
        self.assertIn("scoped effect State (s : Type)", samples[85].source)
        self.assertIn("handle Ask (k True) with", samples[86].source)
        self.assertIn("Eff <[Ask : Ask, Other : Other]> Int", samples[87].source)
        self.assertIn("deep handle l comp with", samples[88].source)
        self.assertIn("deep handle pkg.label comp with", samples[89].source)
        self.assertIn("ω choose : Unit -> Bool", samples[90].source)
        self.assertIn("let c = Choice", samples[91].source)

    def test_local_elpi_emits_eff_handler_families_when_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        expected_by_start = {
            84: ("scoped effect Ask", "deep handle Ask comp with", "runPure handled"),
            85: ("scoped effect State (s : Type)", "State.get ()", "case get () k -> k"),
            86: ("handle Ask (k True) with", "case ask () k -> rehandle k"),
            87: ("Eff <[Ask : Ask, Other : Other]> Int", "Eff <[Other : Other]> Int"),
            88: ("let l = Ask", "l.ask ()", "deep handle l comp with"),
            89: ("let pkg = (label = Ask)", "pkg.label.ask ()", "deep handle pkg.label comp with"),
            90: ("ω choose : Unit -> Bool", "let a <- k True", "let b <- k False"),
            91: ("let c = Choice", "c.choose ()", "deep handle Choice comp with"),
        }

        for start_index, expected_fragments in expected_by_start.items():
            with self.subTest(start_index=start_index):
                output = kappa_lp_generate.run_elpi(
                    elpi,
                    kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
                    count=1,
                    depth=5,
                    start_index=start_index,
                )
                sample = kappa_lp_generate.parse_elpi_samples(output)[0]

                for expected in expected_fragments:
                    self.assertIn(expected, sample.source)

    def test_local_elpi_ranked_int_synthesis_uses_collection_and_pattern_shapes_when_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        expected_by_start = {
            92: "generatedListLength",
            146: "generatedSetCount",
            200: "generatedMapCount",
            254: "match (Some ",
            308: "case head :: tail -> head",
            362: "let (x = captured, y = other) = pair",
        }

        for start_index, expected in expected_by_start.items():
            with self.subTest(start_index=start_index):
                output = kappa_lp_generate.run_elpi(
                    elpi,
                    kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
                    count=1,
                    depth=5,
                    start_index=start_index,
                )
                sample = kappa_lp_generate.parse_elpi_samples(output)[0]

                self.assertIn(expected, sample.source)

    def test_local_elpi_nested_multiline_int_synthesis_preserves_indentation_when_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=1,
            depth=5,
            start_index=98,
        )
        sample = kappa_lp_generate.parse_elpi_samples(output)[0]

        self.assertIn("let ranked = (block\n            generatedListLength", sample.source)
        self.assertNotIn("let ranked = (block\n    generatedListLength", sample.source)

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

    def test_local_elpi_omits_unused_preamble_declarations_when_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=1,
            depth=4,
            start_index=0,
        )
        sample = kappa_lp_generate.parse_elpi_samples(output)[0]

        self.assertNotIn("GeneratedPair", sample.source)
        self.assertNotIn("GeneratedChoice", sample.source)
        self.assertNotIn("generatedResize", sample.source)
        self.assertIn("--! assertDeclKinds signature, let, signature, let", sample.source)

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
