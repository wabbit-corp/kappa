#!/usr/bin/env python3

from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import shutil
import stat
import sys
import urllib.request
from pathlib import Path
from urllib.parse import urlparse

DEFAULT_VERSION = "0.16.0"
INDEX_URL = "https://ziglang.org/download/index.json"


def fail(message: str) -> "NoReturn":
    print(message, file=sys.stderr)
    raise SystemExit(1)


def detect_asset_key() -> str:
    machine = platform.machine().lower()
    system = platform.system()

    architecture_aliases = {
        "amd64": "x86_64",
        "x86_64": "x86_64",
        "arm64": "aarch64",
        "aarch64": "aarch64",
        "x86": "x86",
        "i386": "x86",
        "i686": "x86",
    }
    system_aliases = {
        "Windows": "windows",
        "Linux": "linux",
        "Darwin": "macos",
    }

    architecture = architecture_aliases.get(machine)
    system_name = system_aliases.get(system)

    if architecture is None or system_name is None:
        fail(f"Unsupported platform for Zig bootstrap: os={system!r}, arch={machine!r}.")

    return f"{architecture}-{system_name}"


def executable_name() -> str:
    return "zig.exe" if os.name == "nt" else "zig"


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent


def zig_root() -> Path:
    return repo_root() / ".tools" / "zig"


def sha256_of_file(path: Path) -> str:
    digest = hashlib.sha256()

    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)

    return digest.hexdigest()


def download_json(url: str) -> object:
    request = urllib.request.Request(url, headers={"User-Agent": "kappa-zig-bootstrap"})

    with urllib.request.urlopen(request) as response:
        return json.load(response)


def download_file(url: str, destination: Path) -> None:
    request = urllib.request.Request(url, headers={"User-Agent": "kappa-zig-bootstrap"})

    with urllib.request.urlopen(request) as response, destination.open("wb") as handle:
        shutil.copyfileobj(response, handle)


def ensure_archive(asset: dict[str, object], archive_path: Path) -> None:
    expected_sha = str(asset["shasum"]).lower()

    if archive_path.exists() and sha256_of_file(archive_path) != expected_sha:
        archive_path.unlink()

    if not archive_path.exists():
        download_file(str(asset["tarball"]), archive_path)

    actual_sha = sha256_of_file(archive_path).lower()

    if actual_sha != expected_sha:
        archive_path.unlink(missing_ok=True)
        fail(f"Checksum mismatch for Zig archive {archive_path.name}. Expected {expected_sha} but got {actual_sha}.")


def install_archive(archive_path: Path, install_root: Path, executable_path: Path) -> None:
    extract_root = install_root.parent / f".extract-{install_root.name}"

    shutil.rmtree(extract_root, ignore_errors=True)
    shutil.rmtree(install_root, ignore_errors=True)
    extract_root.mkdir(parents=True, exist_ok=True)

    try:
        shutil.unpack_archive(str(archive_path), str(extract_root))

        extracted_children = [child for child in extract_root.iterdir()]

        if len(extracted_children) != 1 or not extracted_children[0].is_dir():
            fail(f"Unexpected archive layout in {archive_path.name}.")

        extracted_root = extracted_children[0]
        shutil.move(str(extracted_root), str(install_root))
    finally:
        shutil.rmtree(extract_root, ignore_errors=True)

    if not executable_path.exists():
        fail(f"Zig download completed, but '{executable_path}' was not found.")

    if os.name != "nt":
        current_mode = executable_path.stat().st_mode
        executable_path.chmod(current_mode | stat.S_IXUSR)


def resolve_release(version: str, asset_key: str) -> tuple[dict[str, object], dict[str, object]]:
    index = download_json(INDEX_URL)

    if not isinstance(index, dict):
        fail("Unexpected Zig download index format.")

    release = index.get(version)

    if not isinstance(release, dict):
        fail(f"Zig version '{version}' was not found in the official download index.")

    asset = release.get(asset_key)

    if not isinstance(asset, dict):
        fail(f"Zig version '{version}' does not expose a '{asset_key}' build in the official download index.")

    return release, asset


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Resolve a repo-local Zig toolchain for the current platform.")
    parser.add_argument("version", nargs="?", default=DEFAULT_VERSION)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    version = args.version
    asset_key = detect_asset_key()
    _, asset = resolve_release(version, asset_key)

    install_root = zig_root() / version / asset_key
    executable_path = install_root / executable_name()

    if executable_path.exists():
        print(executable_path)
        return 0

    archive_name = Path(urlparse(str(asset["tarball"])).path).name
    archive_path = zig_root() / archive_name
    archive_path.parent.mkdir(parents=True, exist_ok=True)
    install_root.parent.mkdir(parents=True, exist_ok=True)

    ensure_archive(asset, archive_path)
    install_archive(archive_path, install_root, executable_path)

    print(executable_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
