import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from scripts import kappa_lp_generate


REPO_ROOT = kappa_lp_generate.REPO_ROOT
FSHARP_SCRIPT = REPO_ROOT / "scripts" / "kappa_lp_generate.fsx"


class FSharpLambdaPrologKappaGeneratorParityTests(unittest.TestCase):
    def require_fsharp(self) -> None:
        if shutil.which("dotnet") is None:
            self.skipTest("dotnet is not installed")
        if not FSHARP_SCRIPT.exists():
            self.skipTest("F# generator script is not available")

    def run_fsharp(self, *args: str) -> subprocess.CompletedProcess[str]:
        self.require_fsharp()

        result = subprocess.run(
            ["dotnet", "fsi", str(FSHARP_SCRIPT), "--", *args],
            cwd=REPO_ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        self.assertEqual(
            0,
            result.returncode,
            msg=f"F# generator failed\nSTDOUT:\n{result.stdout}\nSTDERR:\n{result.stderr}",
        )
        return result

    def test_fsharp_generation_matches_python_output_when_elpi_is_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        python_output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=4,
            depth=4,
            start_index=22,
        )
        fsharp_output = self.run_fsharp("--count", "4", "--depth", "4", "--start-index", "22").stdout

        self.assertEqual(python_output, fsharp_output)

    def test_fsharp_accepts_argparse_style_equals_options_when_elpi_is_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        python_output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=2,
            depth=4,
            start_index=36,
        )
        fsharp_output = self.run_fsharp("--count=2", "--depth=4", "--start-index=36").stdout

        self.assertEqual(python_output, fsharp_output)

    def test_fsharp_writes_same_fixture_layout_as_python_when_elpi_is_available(self) -> None:
        elpi = kappa_lp_generate.resolve_elpi()
        if elpi is None:
            self.skipTest("elpi is not installed")

        python_output = kappa_lp_generate.run_elpi(
            elpi,
            kappa_lp_generate.DEFAULT_ELPI_PROGRAM,
            count=3,
            depth=4,
            start_index=30,
        )
        expected_samples = kappa_lp_generate.parse_elpi_samples(python_output)

        with tempfile.TemporaryDirectory() as temp_dir:
            out_dir = Path(temp_dir)
            self.run_fsharp("--count", "3", "--depth", "4", "--start-index", "30", "--out-dir", str(out_dir))

            for sample in expected_samples:
                case_file = out_dir / f"lp_kappa_{sample.index:03d}" / "main.kp"
                self.assertTrue(case_file.exists(), msg=f"missing generated case {case_file}")
                self.assertEqual(sample.source, case_file.read_text(encoding="utf-8"))

    def test_fsharp_validation_path_uses_release_cli_when_available(self) -> None:
        if kappa_lp_generate.resolve_elpi() is None:
            self.skipTest("elpi is not installed")
        if kappa_lp_generate.resolve_kappa_cli() is None:
            self.skipTest("release Kappa CLI binary is not available")

        with tempfile.TemporaryDirectory() as temp_dir:
            self.run_fsharp(
                "--count",
                "1",
                "--depth",
                "4",
                "--start-index",
                "19",
                "--out-dir",
                temp_dir,
                "--validate",
            )


if __name__ == "__main__":
    unittest.main()
