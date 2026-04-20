param(
    [string]$Version = "0.16.0"
)

$ErrorActionPreference = "Stop"

Add-Type -AssemblyName System.IO.Compression.FileSystem

$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
$toolsRoot = Join-Path $repoRoot ".tools"
$zigRoot = Join-Path $toolsRoot "zig"
$installRoot = Join-Path $zigRoot $Version
$zigExe = Join-Path $installRoot "zig.exe"

if (Test-Path -LiteralPath $zigExe) {
    Write-Output $zigExe
    exit 0
}

New-Item -ItemType Directory -Force -Path $zigRoot | Out-Null

$index = Invoke-RestMethod -Uri "https://ziglang.org/download/index.json"
$releaseProperty = $index.PSObject.Properties | Where-Object Name -eq $Version | Select-Object -First 1

if ($null -eq $releaseProperty) {
    throw "Zig version '$Version' was not found in the official download index."
}

$release = $releaseProperty.Value
$windowsAsset = $release."x86_64-windows"

if ($null -eq $windowsAsset) {
    throw "Zig version '$Version' does not expose an x86_64-windows build in the official download index."
}

$archivePath = Join-Path $zigRoot ("zig-x86_64-windows-{0}.zip" -f $Version)
$legacyExtractRoot = Join-Path $zigRoot ("extract-{0}" -f $Version)

if (Test-Path -LiteralPath $installRoot) {
    Remove-Item -LiteralPath $installRoot -Recurse -Force
}

if (-not (Test-Path -LiteralPath $archivePath)) {
    Invoke-WebRequest -Uri $windowsAsset.tarball -OutFile $archivePath
}

$expectedSha = $windowsAsset.shasum.ToLowerInvariant()
$actualSha = (Get-FileHash -LiteralPath $archivePath -Algorithm SHA256).Hash.ToLowerInvariant()

if ($actualSha -ne $expectedSha) {
    throw "Checksum mismatch for Zig $Version. Expected $expectedSha but got $actualSha."
}

New-Item -ItemType Directory -Force -Path $installRoot | Out-Null

$archive = [System.IO.Compression.ZipFile]::OpenRead($archivePath)

try {
    foreach ($entry in $archive.Entries) {
        $relativePath = $entry.FullName -replace '^[^/]+/', ''

        if ([string]::IsNullOrWhiteSpace($relativePath)) {
            continue
        }

        $normalizedRelativePath = $relativePath -replace '/', '\'
        $targetPath = Join-Path $installRoot $normalizedRelativePath
        $targetDirectory = Split-Path -Path $targetPath -Parent

        if (-not [string]::IsNullOrWhiteSpace($targetDirectory)) {
            New-Item -ItemType Directory -Force -Path $targetDirectory | Out-Null
        }

        if ([string]::IsNullOrEmpty($entry.Name)) {
            continue
        }

        [System.IO.Compression.ZipFileExtensions]::ExtractToFile($entry, $targetPath, $true)
    }
}
finally {
    $archive.Dispose()
}

if (Test-Path -LiteralPath $legacyExtractRoot) {
    Remove-Item -LiteralPath $legacyExtractRoot -Recurse -Force -ErrorAction SilentlyContinue
}

if (Test-Path -LiteralPath $archivePath) {
    Remove-Item -LiteralPath $archivePath -Force -ErrorAction SilentlyContinue
}

if (-not (Test-Path -LiteralPath $zigExe)) {
    throw "Zig download completed, but '$zigExe' was not found."
}

Write-Output $zigExe
