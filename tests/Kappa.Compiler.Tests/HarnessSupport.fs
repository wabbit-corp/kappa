module HarnessSupport

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Runtime.Loader

let rootPath (rootName: string) =
    Path.GetFullPath(rootName)

let rootedFilePath (root: string) (filePath: string) =
    let relativePath =
        filePath.Replace('/', Path.DirectorySeparatorChar)
                .Replace('\\', Path.DirectorySeparatorChar)

    Path.Combine(root, relativePath)

type ProcessResult =
    { ExitCode: int
      StandardOutput: string
      StandardError: string }

let private scratchRoot =
    Path.Combine(Path.GetTempPath(), "kappa-tests")

let createScratchDirectory (name: string) =
    Directory.CreateDirectory(scratchRoot) |> ignore

    let safeName =
        name
        |> Seq.map (fun ch ->
            if Char.IsLetterOrDigit(ch) then ch else '-')
        |> Array.ofSeq
        |> System.String

    let directory =
        Path.Combine(scratchRoot, $"{safeName}-{Guid.NewGuid():N}")

    Directory.CreateDirectory(directory).FullName

let currentRid () =
    let suffix =
        match RuntimeInformation.ProcessArchitecture with
        | Architecture.X64 -> "x64"
        | Architecture.Arm64 -> "arm64"
        | Architecture.X86 -> "x86"
        | architecture -> invalidOp $"Unsupported test architecture '{architecture}'."

    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        $"win-{suffix}"
    elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
        $"linux-{suffix}"
    elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
        $"osx-{suffix}"
    else
        invalidOp "Unsupported test operating system."

let writeWorkspaceFiles (root: string) (files: (string * string) list) =
    for filePath, text in files do
        let fullPath = rootedFilePath root filePath
        let directory = Path.GetDirectoryName(fullPath)

        if not (String.IsNullOrWhiteSpace(directory)) then
            Directory.CreateDirectory(directory) |> ignore

        File.WriteAllText(fullPath, text.Replace("\r\n", "\n"))

let executablePath (directory: string) (baseName: string) =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        Path.Combine(directory, $"{baseName}.exe")
    else
        Path.Combine(directory, baseName)

let private runProcessCore
    (workingDirectory: string)
    (fileName: string)
    (arguments: string)
    (environmentVariables: (string * string) list)
    (standardInputText: string option)
    =
    let startInfo = ProcessStartInfo()
    startInfo.WorkingDirectory <- workingDirectory
    startInfo.FileName <- fileName
    startInfo.Arguments <- arguments
    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.RedirectStandardInput <- standardInputText.IsSome

    for name, value in environmentVariables do
        startInfo.Environment[name] <- value

    use child = new Process()
    child.StartInfo <- startInfo

    if not (child.Start()) then
        invalidOp $"Failed to start process '{fileName}'."

    match standardInputText with
    | Some inputText ->
        child.StandardInput.Write(inputText)
        child.StandardInput.Close()
    | None ->
        ()

    let standardOutput = child.StandardOutput.ReadToEnd()
    let standardError = child.StandardError.ReadToEnd()
    child.WaitForExit()

    { ExitCode = child.ExitCode
      StandardOutput = standardOutput.Replace("\r\n", "\n")
      StandardError = standardError.Replace("\r\n", "\n") }

let runProcess (workingDirectory: string) (fileName: string) (arguments: string) =
    runProcessCore workingDirectory fileName arguments [] None

let runProcessWithEnvironment
    (workingDirectory: string)
    (fileName: string)
    (arguments: string)
    (environmentVariables: (string * string) list)
    =
    runProcessCore workingDirectory fileName arguments environmentVariables None

let runProcessWithInput
    (workingDirectory: string)
    (fileName: string)
    (arguments: string)
    (standardInputText: string option)
    =
    runProcessCore workingDirectory fileName arguments [] standardInputText

let runProcessWithEnvironmentAndInput
    (workingDirectory: string)
    (fileName: string)
    (arguments: string)
    (environmentVariables: (string * string) list)
    (standardInputText: string option)
    =
    runProcessCore workingDirectory fileName arguments environmentVariables standardInputText

let private repoRoot =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

let repoZigBootstrapCommand () =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        let scriptPath = Path.Combine(repoRoot, "scripts", "ensure-zig.ps1")
        "powershell", $"-ExecutionPolicy Bypass -File \"{scriptPath}\""
    else
        let scriptPath = Path.Combine(repoRoot, "scripts", "ensure-zig.sh")
        "sh", $"\"{scriptPath}\""

let private zigExecutablePath =
    lazy
        (let shellProgram, shellArguments = repoZigBootstrapCommand ()
         let ensureResult =
             runProcess
                 repoRoot
                 shellProgram
                 shellArguments

         if ensureResult.ExitCode <> 0 then
             failwithf "Failed to resolve the repo-local Zig toolchain: %s" ensureResult.StandardError

         let zigPath = ensureResult.StandardOutput.Trim()

         if String.IsNullOrWhiteSpace(zigPath) then
             failwith "The repo-local Zig bootstrap script did not return an executable path."

         zigPath)

let ensureRepoZigExecutablePath () =
    zigExecutablePath.Value

type private TestAssemblyLoadContext(mainAssemblyPath: string) =
    inherit AssemblyLoadContext($"kappa-test-{Guid.NewGuid():N}", isCollectible = true)

    let resolver = AssemblyDependencyResolver(mainAssemblyPath)

    override this.Load(assemblyName: AssemblyName) =
        let resolvedPath = resolver.ResolveAssemblyToPath(assemblyName)

        if String.IsNullOrWhiteSpace(resolvedPath) then
            null
        else
            this.LoadFromAssemblyPath(resolvedPath)

type LoadedManagedAssembly =
    { Context: AssemblyLoadContext
      Assembly: Assembly }
    interface IDisposable with
        member this.Dispose() =
            this.Context.Unload()

let loadManagedAssembly (assemblyPath: string) =
    let resolvedPath = Path.GetFullPath(assemblyPath)
    let loadContext = new TestAssemblyLoadContext(resolvedPath)
    let assembly = loadContext.LoadFromAssemblyPath(resolvedPath)

    { Context = loadContext
      Assembly = assembly }
