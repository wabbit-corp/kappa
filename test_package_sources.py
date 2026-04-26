from __future__ import annotations

import sys
import zipfile
from pathlib import Path


SCRIPTS_DIR = Path(__file__).resolve().parent / "scripts"
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

import package_sources as package_sources


def write(path: Path, text: str = "content\n") -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


def test_collect_source_files_includes_src_tests_and_fixtures(tmp_path: Path) -> None:
    write(tmp_path / "Kappa.Compiler.sln")
    write(tmp_path / "Spec.md")
    write(tmp_path / "src" / "Compiler.fs")
    write(tmp_path / "tests" / "Compiler.Tests.fs")
    write(tmp_path / "tests" / "Kappa.Compiler.Tests" / "Fixtures" / "case" / "main.kp")

    paths = package_sources.collect_source_files(tmp_path)

    assert [path.relative_to(tmp_path).as_posix() for path in paths] == [
        "Kappa.Compiler.sln",
        "Spec.md",
        "src/Compiler.fs",
        "tests/Compiler.Tests.fs",
        "tests/Kappa.Compiler.Tests/Fixtures/case/main.kp",
    ]


def test_collect_source_files_excludes_scripts_and_build_outputs(tmp_path: Path) -> None:
    write(tmp_path / "src" / "Compiler.fs")
    write(tmp_path / "src" / "bin" / "Debug" / "Compiler.dll")
    write(tmp_path / "src" / "obj" / "project.assets.json")
    write(tmp_path / "tests" / "__pycache__" / "test.cpython.pyc")
    write(tmp_path / "tests" / "python" / "test_helper.py")
    write(tmp_path / "scripts" / "validate.py")

    paths = package_sources.collect_source_files(tmp_path)

    assert [path.relative_to(tmp_path).as_posix() for path in paths] == [
        "src/Compiler.fs"
    ]


def test_create_source_zip_writes_single_deterministic_archive(tmp_path: Path) -> None:
    write(tmp_path / "Kappa.Compiler.sln")
    write(tmp_path / "Spec.md")
    write(tmp_path / "src" / "B.fs", "module B\n")
    write(tmp_path / "src" / "A.fs", "module A\n")
    write(tmp_path / "tests" / "Kappa.Compiler.Tests" / "Fixtures" / "case" / "main.kp")
    output = tmp_path / "package.zip"

    files = package_sources.create_source_zip(tmp_path, output)

    assert [path.relative_to(tmp_path).as_posix() for path in files] == [
        "Kappa.Compiler.sln",
        "Spec.md",
        "src/A.fs",
        "src/B.fs",
        "tests/Kappa.Compiler.Tests/Fixtures/case/main.kp",
    ]
    assert output.exists()
    with zipfile.ZipFile(output) as archive:
        assert archive.namelist() == [
            "README.md",
            "Kappa.Compiler.sln",
            "Spec.md",
            "src/A.fs",
            "src/B.fs",
            "tests/Kappa.Compiler.Tests/Fixtures/case/main.kp",
        ]
        readme = archive.read("README.md").decode("utf-8")
        assert "Kappa.Compiler.sln" in readme
        assert "src/" in readme
        assert "tests/Kappa.Compiler.Tests/Fixtures/" in readme
        assert "scripts/" in readme
        assert all(info.date_time == package_sources.ZIP_TIMESTAMP for info in archive.infolist())
