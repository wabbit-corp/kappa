#!/usr/bin/env sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ZIG_EXE=$("$SCRIPT_DIR/ensure-zig.sh")

exec "$ZIG_EXE" "$@"

