from __future__ import annotations

import sys
from pathlib import Path


SCRIPTS_DIR = Path(__file__).resolve().parent / "scripts"
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

import promote_new_tests as promote_new_tests


def test_escape_fsharp_string_escapes_backslashes_and_quotes() -> None:
    raw = 'bridge.standard_module\\"quoted"'

    escaped = promote_new_tests.escape_fsharp_string(raw)

    assert escaped == 'bridge.standard_module\\\\\\"quoted\\"'


def test_build_fsi_script_embeds_fixture_name_and_references() -> None:
    compiler_dll = Path("/repo/src/Kappa.Compiler/bin/Debug/net10.0/Kappa.Compiler.dll")
    tests_dll = Path("/repo/tests/Kappa.Compiler.Tests/bin/Debug/net10.0/Kappa.Compiler.Tests.dll")

    script = promote_new_tests.build_fsi_script(
        fixture_name='effects.return.runtime_positive_labeled_lambda_"quoted"',
        compiler_dll=compiler_dll,
        tests_dll=tests_dll,
    )

    assert f'#r "{compiler_dll.as_posix()}"' in script
    assert f'#r "{tests_dll.as_posix()}"' in script
    assert 'let name = "effects.return.runtime_positive_labeled_lambda_\\"quoted\\""' in script
    assert "Harness.runKpFixtureCase fixture" in script


def test_directory_content_hash_matches_equivalent_trees(tmp_path: Path) -> None:
    left = tmp_path / "left"
    right = tmp_path / "right"
    (left / "nested").mkdir(parents=True)
    (right / "nested").mkdir(parents=True)
    (left / "main.kp").write_text("let x = 1\n", encoding="utf-8")
    (right / "main.kp").write_text("let x = 1\n", encoding="utf-8")
    (left / "nested" / "suite.ktest").write_text("expect pass\n", encoding="utf-8")
    (right / "nested" / "suite.ktest").write_text("expect pass\n", encoding="utf-8")

    assert (
        promote_new_tests.directory_content_hash(left)
        == promote_new_tests.directory_content_hash(right)
    )


def test_directory_content_hash_includes_relative_paths(tmp_path: Path) -> None:
    left = tmp_path / "left"
    right = tmp_path / "right"
    (left / "nested").mkdir(parents=True)
    (right / "other").mkdir(parents=True)
    (left / "nested" / "main.kp").write_text("let x = 1\n", encoding="utf-8")
    (right / "other" / "main.kp").write_text("let x = 1\n", encoding="utf-8")

    assert (
        promote_new_tests.directory_content_hash(left)
        != promote_new_tests.directory_content_hash(right)
    )


def test_dry_run_fixture_directory_reports_would_move_without_mutating(tmp_path: Path) -> None:
    fixtures = tmp_path / "fixtures"
    fixtures.mkdir()
    src = tmp_path / "new-tests" / "candidate"
    src.mkdir(parents=True)
    (src / "main.kp").write_text("let x = 1\n", encoding="utf-8")
    content_hashes = promote_new_tests.fixture_content_hash_index(fixtures)

    status, digest = promote_new_tests.dry_run_fixture_directory(
        src=src,
        dst=fixtures / src.name,
        existing_names=set(),
        existing_content_hashes=content_hashes,
    )

    assert status == "WOULD_MOVE"
    assert digest in content_hashes
    assert src.exists()
    assert not (fixtures / src.name).exists()


def test_dry_run_fixture_directory_skips_duplicate_content(tmp_path: Path) -> None:
    fixtures = tmp_path / "fixtures"
    existing = fixtures / "existing"
    src = tmp_path / "new-tests" / "candidate"
    existing.mkdir(parents=True)
    src.mkdir(parents=True)
    (existing / "main.kp").write_text("let x = 1\n", encoding="utf-8")
    (src / "main.kp").write_text("let x = 1\n", encoding="utf-8")
    content_hashes = promote_new_tests.fixture_content_hash_index(fixtures)

    status, _ = promote_new_tests.dry_run_fixture_directory(
        src=src,
        dst=fixtures / src.name,
        existing_names={existing.name},
        existing_content_hashes=content_hashes,
    )

    assert status == "SKIP_DUPLICATE_CONTENT existing"


def test_validate_inputs_dry_run_does_not_require_build_outputs(tmp_path: Path) -> None:
    (tmp_path / "new-tests").mkdir()
    (tmp_path / "tests" / "Kappa.Compiler.Tests" / "Fixtures").mkdir(parents=True)

    assert promote_new_tests.validate_inputs(tmp_path, require_build_outputs=False) == []
    assert promote_new_tests.validate_inputs(tmp_path, require_build_outputs=True) == [
        f"missing compiler DLL: {promote_new_tests.compiler_dll_path(tmp_path)}",
        f"missing tests DLL: {promote_new_tests.tests_dll_path(tmp_path)}",
    ]
