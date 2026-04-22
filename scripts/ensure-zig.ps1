param(
    [string]$Version = "0.16.0"
)

$ErrorActionPreference = "Stop"

$scriptPath = Join-Path $PSScriptRoot "ensure-zig.py"

if (Get-Command py -ErrorAction SilentlyContinue) {
    & py -3 $scriptPath $Version
    exit $LASTEXITCODE
}

if (Get-Command python -ErrorAction SilentlyContinue) {
    & python $scriptPath $Version
    exit $LASTEXITCODE
}

if (Get-Command python3 -ErrorAction SilentlyContinue) {
    & python3 $scriptPath $Version
    exit $LASTEXITCODE
}

throw "Python 3 is required to bootstrap the Zig toolchain."

