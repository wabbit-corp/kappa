param(
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$Args
)

$ErrorActionPreference = "Stop"

$zigExe = & (Join-Path $PSScriptRoot "ensure-zig.ps1")
& $zigExe @Args
exit $LASTEXITCODE
