param(
    [string]$CodeCommand = "code",
    [switch]$Insiders,
    [switch]$SkipNpmInstall
)

$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
$extensionDir = Join-Path $repoRoot "editors\vscode\kappa"
$serverProject = Join-Path $repoRoot "src\Kappa.LSP\Kappa.LSP.fsproj"
$serverDll = Join-Path $repoRoot "src\Kappa.LSP\bin\Debug\net10.0\Kappa.LSP.dll"
$artifactDir = Join-Path $repoRoot "artifacts\vscode"
$vsixPath = Join-Path $artifactDir "kappa-vscode.vsix"

if ($Insiders -and $CodeCommand -eq "code") {
    $CodeCommand = "code-insiders"
}

if (-not (Test-Path $extensionDir)) {
    throw "VS Code extension directory not found: $extensionDir"
}

if (-not (Test-Path $serverProject)) {
    throw "Kappa LSP project not found: $serverProject"
}

$code = Get-Command $CodeCommand -ErrorAction SilentlyContinue

if (-not $code) {
    throw "Could not find '$CodeCommand' on PATH. In VS Code, run 'Shell Command: Install code command in PATH', or pass -CodeCommand with the full path."
}

$npm = Get-Command "npm.cmd" -ErrorAction SilentlyContinue

if (-not $npm) {
    $npm = Get-Command "npm" -ErrorAction SilentlyContinue
}

if (-not $npm) {
    throw "npm is required to package the Kappa VS Code extension."
}

$dotnet = Get-Command "dotnet" -ErrorAction SilentlyContinue

if (-not $dotnet) {
    throw "dotnet is required to build and run the Kappa language server."
}

if (-not $SkipNpmInstall) {
    Push-Location $extensionDir
    try {
        & $npm.Source install
    }
    finally {
        Pop-Location
    }

    if ($LASTEXITCODE -ne 0) {
        throw "npm install failed."
    }
}

& $dotnet.Source build $serverProject /p:BuildProjectReferences=false

if ($LASTEXITCODE -ne 0) {
    throw "Kappa LSP build failed."
}

if (-not (Test-Path $serverDll)) {
    throw "Expected built Kappa LSP server DLL not found: $serverDll"
}

$isWindows = [System.Environment]::OSVersion.Platform -eq "Win32NT"
$vsceCommandName = if ($isWindows) { "vsce.cmd" } else { "vsce" }
$vsce = Join-Path $extensionDir "node_modules\.bin\$vsceCommandName"

if (-not (Test-Path $vsce)) {
    throw "vsce was not found under node_modules. Run this script without -SkipNpmInstall first."
}

New-Item -ItemType Directory -Force -Path $artifactDir | Out-Null

Push-Location $extensionDir
try {
    & $vsce package --allow-missing-repository --out $vsixPath
}
finally {
    Pop-Location
}

if ($LASTEXITCODE -ne 0) {
    throw "VS Code extension packaging failed."
}

& $code.Source --install-extension $vsixPath --force

if ($LASTEXITCODE -ne 0) {
    throw "VS Code extension install failed."
}

function Get-VsCodeSettingsPath {
    param(
        [bool]$UseInsiders
    )

    $isWindowsPlatform = [System.Environment]::OSVersion.Platform -eq "Win32NT"

    if ($isWindowsPlatform) {
        $appData = [Environment]::GetFolderPath("ApplicationData")
        $productDirectory = if ($UseInsiders) { "Code - Insiders" } else { "Code" }
        return Join-Path $appData "$productDirectory\User\settings.json"
    }

    if ($PSVersionTable.Platform -eq "Unix" -and (uname) -eq "Darwin") {
        $productDirectory = if ($UseInsiders) { "Code - Insiders" } else { "Code" }
        return Join-Path $HOME "Library\Application Support\$productDirectory\User\settings.json"
    }

    $productDirectory = if ($UseInsiders) { "Code - Insiders" } else { "Code" }
    return Join-Path $HOME ".config\$productDirectory\User\settings.json"
}

$settingsPath = Get-VsCodeSettingsPath -UseInsiders:$Insiders.IsPresent
$settingsDirectory = Split-Path -Parent $settingsPath
New-Item -ItemType Directory -Force -Path $settingsDirectory | Out-Null

if (Test-Path $settingsPath) {
    $settingsText = Get-Content $settingsPath -Raw
    $settings =
        if ([string]::IsNullOrWhiteSpace($settingsText)) {
            [pscustomobject]@{}
        }
        else {
            $settingsText | ConvertFrom-Json
        }
}
else {
    $settings = [pscustomobject]@{}
}

function Set-JsonSetting {
    param(
        [object]$Settings,
        [string]$Name,
        [string]$Value
    )

    if ($Settings.PSObject.Properties.Name -contains $Name) {
        $Settings.PSObject.Properties[$Name].Value = $Value
    }
    else {
        Add-Member -InputObject $Settings -NotePropertyName $Name -NotePropertyValue $Value
    }
}

$projectSettingValue = (Resolve-Path $serverProject).Path
$dllSettingValue = (Resolve-Path $serverDll).Path

Set-JsonSetting -Settings $settings -Name "kappa.languageServer.projectPath" -Value $projectSettingValue
Set-JsonSetting -Settings $settings -Name "kappa.languageServer.serverDllPath" -Value $dllSettingValue

$settings | ConvertTo-Json -Depth 20 | Set-Content -Path $settingsPath -Encoding UTF8

Write-Host "Installed Kappa VS Code extension from $vsixPath"
Write-Host "Configured kappa.languageServer.projectPath = $projectSettingValue"
Write-Host "Configured kappa.languageServer.serverDllPath = $dllSettingValue"
