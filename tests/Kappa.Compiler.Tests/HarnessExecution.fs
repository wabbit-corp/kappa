module HarnessExecution

open System
open System.IO
open System.Text
open HarnessFixtureModel
open Kappa.Compiler

let private rootedFilePath = HarnessSupport.rootedFilePath

let formatDiagnostics (diagnostics: Diagnostic list) =
    diagnostics
    |> List.map (fun diagnostic ->
        let locationText =
            match diagnostic.Location with
            | Some location ->
                $"{location.FilePath}({location.Start.Line},{location.Start.Column})"
            | None ->
                "<unknown>"

        $"{locationText}: {diagnostic.Message}")
    |> String.concat Environment.NewLine

let countDiagnosticsBySeverity severity (diagnostics: Diagnostic list) =
    diagnostics |> List.filter (fun diagnostic -> diagnostic.Severity = severity) |> List.length

let normalizeLineEndings (text: string) =
    text.Replace("\r\n", "\n")

let normalizeExecutionOutput (text: string) =
    normalizeLineEndings text
    |> fun normalized -> normalized.TrimEnd([| '\r'; '\n' |])

let executeBindingWithCapturedOutput (workspace: WorkspaceCompilation) (entryPoint: string) =
    let builder = StringBuilder()

    let output: RuntimeOutput =
        { Write = fun text -> builder.Append(text) |> ignore
          WriteLine = fun text -> builder.AppendLine(text) |> ignore }

    let result = Interpreter.executeBindingWithOutput workspace output entryPoint

    result, normalizeExecutionOutput (builder.ToString())

let compileFixtureWorkspace (fixtureCase: KpFixtureCase) =
    let options =
        { CompilationOptions.create fixtureCase.Root with
            PackageMode = fixtureCase.Configuration.PackageMode
            BackendProfile = fixtureCase.Configuration.BackendProfile }

    Compilation.parse options [ fixtureCase.Root ]

type FixtureRunResult =
    { ExitCode: int
      StandardOutput: string
      StandardError: string }

let private executeInterpreterRun (workspace: WorkspaceCompilation) (entryPoint: string) =
    let builder = StringBuilder()

    let output: RuntimeOutput =
        { Write = fun text -> builder.Append(text) |> ignore
          WriteLine = fun text -> builder.AppendLine(text) |> ignore }

    match Interpreter.executeBindingWithOutput workspace output entryPoint with
    | Result.Ok value ->
        if Interpreter.shouldPrintResult value then
            builder.AppendLine(RuntimeValue.format value) |> ignore

        { ExitCode = 0
          StandardOutput = normalizeLineEndings (builder.ToString())
          StandardError = "" }
    | Result.Error issue ->
        { ExitCode = 1
          StandardOutput = normalizeLineEndings (builder.ToString())
          StandardError = normalizeLineEndings issue.Message }

let private readFixtureStandardInput (fixtureCase: KpFixtureCase) =
    fixtureCase.Configuration.StdinFile
    |> Option.map (fun relativePath ->
        let fullPath = rootedFilePath fixtureCase.Root relativePath
        File.ReadAllText(fullPath))

let private renderProcessArguments (arguments: string list) =
    arguments
    |> List.map (fun argument ->
        let escapedArgument =
            argument.Replace("\\", "\\\\").Replace("\"", "\\\"")

        $"\"{escapedArgument}\"")
    |> String.concat " "

let private executeBackendRun (fixtureCase: KpFixtureCase) (workspace: WorkspaceCompilation) (entryPoint: string) =
    let outputDirectory =
        HarnessSupport.createScratchDirectory $"fixture-run-{fixtureCase.Name}-{fixtureCase.Configuration.BackendProfile}"

    let renderedArguments = renderProcessArguments fixtureCase.Configuration.RunArgs
    let stdinText = readFixtureStandardInput fixtureCase

    match fixtureCase.Configuration.BackendProfile with
    | "interpreter" ->
        executeInterpreterRun workspace entryPoint
    | "zig" ->
        let artifact =
            match Backend.emitZigArtifact workspace entryPoint outputDirectory with
            | Result.Ok artifact -> artifact
            | Result.Error message -> invalidOp message

        let compileResult =
            HarnessSupport.runProcessWithEnvironment
                artifact.OutputDirectory
                (HarnessSupport.ensureRepoZigExecutablePath ())
                $"cc -std=c11 -O0 -o \"{artifact.ExecutableFilePath}\" \"{artifact.SourceFilePath}\""
                []

        if compileResult.ExitCode <> 0 then
            { ExitCode = compileResult.ExitCode
              StandardOutput = compileResult.StandardOutput
              StandardError = compileResult.StandardError }
        else
            let runResult =
                HarnessSupport.runProcessWithInput
                    artifact.OutputDirectory
                    artifact.ExecutableFilePath
                    renderedArguments
                    stdinText

            { ExitCode = runResult.ExitCode
              StandardOutput = runResult.StandardOutput
              StandardError = runResult.StandardError }
    | "dotnet" ->
        let artifact =
            match Backend.emitDotNetArtifact workspace entryPoint outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> invalidOp message

        let runResult =
            HarnessSupport.runProcessWithInput
                outputDirectory
                "dotnet"
                ($"run --project \"{artifact.ProjectFilePath}\" -c Release"
                 + if String.IsNullOrWhiteSpace(renderedArguments) then "" else $" -- {renderedArguments}")
                stdinText

        { ExitCode = runResult.ExitCode
          StandardOutput = runResult.StandardOutput
          StandardError = runResult.StandardError }
    | other ->
        invalidOp $"mode run does not support backend '{other}' yet."

let executeFixtureRun (fixtureCase: KpFixtureCase) (workspace: WorkspaceCompilation) =
    match fixtureCase.Configuration.Mode, fixtureCase.Configuration.EntryPoint with
    | KpFixtureMode.Run, Some entryPoint ->
        executeBackendRun fixtureCase workspace entryPoint
    | KpFixtureMode.Run, None ->
        invalidOp $"Fixture '{fixtureCase.Name}' selected mode run but did not configure an entry point."
    | _ ->
        { ExitCode = 0
          StandardOutput = ""
          StandardError = "" }
