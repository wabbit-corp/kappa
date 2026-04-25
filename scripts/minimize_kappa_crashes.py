#!/usr/bin/env python3
"""Backward-compatible wrapper for crash minimization."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path


def main() -> int:
    script = Path(__file__).with_name("minimize_kappa_cases.py")
    result = subprocess.run([sys.executable, str(script), *sys.argv[1:]])
    return result.returncode


if __name__ == "__main__":
    raise SystemExit(main())
