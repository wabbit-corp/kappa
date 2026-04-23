// Provides shared filesystem, process, RID, and toolchain helpers for tests.
module HarnessSupport

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Runtime.Loader
open Kappa.Compiler

let rootPath (rootName: string) =
    Path.GetFullPath(rootName)

let rootedFilePath (root: string) (filePath: string) =
    let relativePath =
        filePath.Replace('/', Path.DirectorySeparatorChar)
                .Replace('\\', Path.DirectorySeparatorChar)

    Path.Combine(root, relativePath)

type ProcessResult = HostSupport.ProcessResult

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

let currentRid = HostSupport.currentRid

let writeWorkspaceFiles (root: string) (files: (string * string) list) =
    for filePath, text in files do
        let fullPath = rootedFilePath root filePath
        let directory = Path.GetDirectoryName(fullPath)

        if not (String.IsNullOrWhiteSpace(directory)) then
            Directory.CreateDirectory(directory) |> ignore

        File.WriteAllText(fullPath, text.Replace("\r\n", "\n"))

let executablePath = HostSupport.executablePath
let runProcess = HostSupport.runProcess
let runProcessWithEnvironment = HostSupport.runProcessWithEnvironment
let runProcessWithInput = HostSupport.runProcessWithInput
let runProcessWithEnvironmentAndInput = HostSupport.runProcessWithEnvironmentAndInput

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
