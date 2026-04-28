open System
open System.IO
open System.Text.Json
open Kappa.Compiler

type ObservabilityTopic =
    | AnalysisSessionTopic
    | CheckpointsTopic
    | CheckpointContractsTopic
    | VerifyAllCheckpointsTopic
    | RuntimeObligationsTopic
    | QuerySketchTopic
    | CompilerFingerprintsTopic
    | IncrementalUnitsTopic

module private ObservabilityTopic =
    let all =
        [
            AnalysisSessionTopic
            CheckpointsTopic
            CheckpointContractsTopic
            VerifyAllCheckpointsTopic
            RuntimeObligationsTopic
            QuerySketchTopic
            CompilerFingerprintsTopic
            IncrementalUnitsTopic
        ]

    let toPortableName topic =
        match topic with
        | AnalysisSessionTopic -> "analysis-session"
        | CheckpointsTopic -> "checkpoints"
        | CheckpointContractsTopic -> "checkpoint-contracts"
        | VerifyAllCheckpointsTopic -> "verify-all"
        | RuntimeObligationsTopic -> "runtime-obligations"
        | QuerySketchTopic -> "query-sketch"
        | CompilerFingerprintsTopic -> "fingerprints"
        | IncrementalUnitsTopic -> "incremental-units"

    let parse (text: string) =
        match text.Trim().ToLowerInvariant() with
        | "analysis-session" -> Some AnalysisSessionTopic
        | "checkpoints" -> Some CheckpointsTopic
        | "checkpoint-contracts" -> Some CheckpointContractsTopic
        | "verify-all" -> Some VerifyAllCheckpointsTopic
        | "runtime-obligations" -> Some RuntimeObligationsTopic
        | "query-sketch" -> Some QuerySketchTopic
        | "fingerprints" -> Some CompilerFingerprintsTopic
        | "incremental-units" -> Some IncrementalUnitsTopic
        | _ -> None

    let supportedText () =
        all
        |> List.map toPortableName
        |> String.concat ", "

type CliOptions =
    { SourceRoot: string
      BackendProfile: string
      EmitDirectory: string option
      NativeAot: bool
      DumpTokens: bool
      DumpAst: bool
      DumpStage: string option
      DumpFormat: StageDumpFormat
      DumpObservability: ObservabilityTopic option
      PrintTrace: bool
      VerifyCheckpoint: string option
      RunBinding: string option
      Inputs: string list }

// Implements the command-line entry point and wires compiler services to CLI commands.
module private Cli =
    let usage () =
        [
            "kp [--source-root <path>] [--backend <profile>] [--emit-dir <path>] [--native-aot] [--dump-tokens] [--dump-ast] [--dump-stage <checkpoint>] [--dump-format <json|sexpr>] [--dump-observability <topic>] [--trace] [--verify <checkpoint>] [--run <binding>] [inputs...]"
            ""
            "If no input paths are supplied, the compiler scans the source root for *.kp files."
            "Runtime backends: interpreter | dotnet | zig (alias: zigcc)"
            $"Observability topics: {ObservabilityTopic.supportedText ()}"
        ]
        |> String.concat Environment.NewLine

    let parse (argv: string array) : Result<CliOptions, string> =
        let inputs = ResizeArray<string>()
        let mutable sourceRoot = Directory.GetCurrentDirectory()
        let mutable backendProfile = "interpreter"
        let mutable emitDirectory = None
        let mutable nativeAot = false
        let mutable dumpTokens = false
        let mutable dumpAst = false
        let mutable dumpStage = None
        let mutable dumpFormat = StageDumpFormat.Json
        let mutable dumpObservability = None
        let mutable printTrace = false
        let mutable verifyCheckpoint = None
        let mutable runBinding = None
        let mutable index = 0
        let mutable error: string option = None

        while index < argv.Length && error.IsNone do
            match argv[index] with
            | "--source-root" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing path after --source-root."
                else
                    sourceRoot <- argv[index + 1]
                    index <- index + 2
            | "--backend" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing backend profile after --backend."
                else
                    backendProfile <- argv[index + 1]
                    index <- index + 2
            | "--emit-dir" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing path after --emit-dir."
                else
                    emitDirectory <- Some argv[index + 1]
                    index <- index + 2
            | "--native-aot" ->
                nativeAot <- true
                index <- index + 1
            | "--dump-tokens" ->
                dumpTokens <- true
                index <- index + 1
            | "--dump-ast" ->
                dumpAst <- true
                index <- index + 1
            | "--dump-stage" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing checkpoint name after --dump-stage."
                else
                    dumpStage <- Some argv[index + 1]
                    index <- index + 2
            | "--dump-observability" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing topic after --dump-observability."
                else
                    match ObservabilityTopic.parse argv[index + 1] with
                    | Some topic ->
                        dumpObservability <- Some topic
                        index <- index + 2
                    | None ->
                        error <-
                            Some
                                $"Unsupported observability topic '{argv[index + 1]}'. Expected one of: {ObservabilityTopic.supportedText ()}."
            | "--dump-format" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing format after --dump-format."
                else
                    match argv[index + 1].ToLowerInvariant() with
                    | "json" ->
                        dumpFormat <- StageDumpFormat.Json
                        index <- index + 2
                    | "sexpr" ->
                        dumpFormat <- StageDumpFormat.SExpression
                        index <- index + 2
                    | value ->
                        error <- Some $"Unsupported dump format '{value}'. Expected 'json' or 'sexpr'."
            | "--trace" ->
                printTrace <- true
                index <- index + 1
            | "--verify" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing checkpoint name after --verify."
                else
                    verifyCheckpoint <- Some argv[index + 1]
                    index <- index + 2
            | "--run" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing binding name after --run."
                else
                    runBinding <- Some argv[index + 1]
                    index <- index + 2
            | "--help"
            | "-h" ->
                error <- Some(usage ())
                index <- argv.Length
            | value ->
                inputs.Add(value)
                index <- index + 1

        match error with
        | Some message -> Result.Error message
        | None ->
            Result.Ok
                { SourceRoot = sourceRoot
                  BackendProfile = backendProfile
                  EmitDirectory = emitDirectory
                  NativeAot = nativeAot
                  DumpTokens = dumpTokens
                  DumpAst = dumpAst
                  DumpStage = dumpStage
                  DumpFormat = dumpFormat
                  DumpObservability = dumpObservability
                  PrintTrace = printTrace
                  VerifyCheckpoint = verifyCheckpoint
                  RunBinding = runBinding
                  Inputs = List.ofSeq inputs }

let private severityText severity =
    match severity with
    | DiagnosticSeverity.Info -> "info"
    | DiagnosticSeverity.Warning -> "warning"
    | DiagnosticSeverity.Error -> "error"

let private formatLocation (location: SourceLocation) =
    $"{location.FilePath}({location.Start.Line},{location.Start.Column})"

let private writeDiagnostic (writer: TextWriter) (diagnostic: Diagnostic) =
    match diagnostic.Location with
    | Some location ->
        writer.WriteLine($"{formatLocation location}: {severityText diagnostic.Severity}: {diagnostic.Message}")
    | None ->
        writer.WriteLine($"{severityText diagnostic.Severity}: {diagnostic.Message}")

let private printDiagnostic (diagnostic: Diagnostic) =
    writeDiagnostic Console.Out diagnostic

let private declarationLabel declaration =
    match declaration with
    | ImportDeclaration (isExport, _) -> if isExport then "export" else "import"
    | FixityDeclarationNode _ -> "fixity"
    | ExpectDeclarationNode _ -> "expect"
    | SignatureDeclaration _ -> "signature"
    | LetDeclaration _ -> "let"
    | ProjectionDeclarationNode _ -> "projection"
    | DataDeclarationNode _ -> "data"
    | TypeAliasNode _ -> "type"
    | EffectDeclarationNode _ -> "effect"
    | TraitDeclarationNode _ -> "trait"
    | InstanceDeclarationNode _ -> "instance"
    | UnknownDeclaration _ -> "unknown"

let private printDocumentSummary (document: ParsedDocument) =
    let moduleName =
        document.ModuleName
        |> Option.map SyntaxFacts.moduleNameToText
        |> Option.defaultValue "<unknown>"

    let counts =
        document.Syntax.Declarations
        |> List.countBy declarationLabel
        |> List.sortBy fst
        |> List.map (fun (name, count) -> $"{name}={count}")
        |> String.concat ", "

    printfn "%s -> %s%s" document.Source.FilePath moduleName (if String.IsNullOrWhiteSpace counts then "" else $" ({counts})")

let private printTokens (document: ParsedDocument) =
    printfn ""
    printfn "Tokens for %s" document.Source.FilePath

    for token in document.Syntax.Tokens do
        printfn "  %-14A %s" token.Kind token.Text

let private printAst (document: ParsedDocument) =
    printfn ""
    printfn "Top-level declarations for %s" document.Source.FilePath

    for declaration in document.Syntax.Declarations do
        match declaration with
        | ImportDeclaration (isExport, specs) ->
            let prefix = if isExport then "export" else "import"
            printfn "  %s %d spec(s)" prefix specs.Length
        | FixityDeclarationNode declaration ->
            printfn "  fixity %s (%d)" declaration.OperatorName declaration.Precedence
        | ExpectDeclarationNode (ExpectTypeDeclaration declaration) ->
            printfn "  expect type %s" declaration.Name
        | ExpectDeclarationNode (ExpectTraitDeclaration declaration) ->
            printfn "  expect trait %s" declaration.Name
        | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
            printfn "  expect term %s" declaration.Name
        | SignatureDeclaration signature ->
            printfn "  signature %s" signature.Name
        | ProjectionDeclarationNode declaration ->
            printfn "  projection %s" declaration.Name
        | LetDeclaration definition ->
            printfn "  let %s" (defaultArg definition.Name "<pattern>")
        | DataDeclarationNode declaration ->
            printfn "  data %s (%d constructor line(s))" declaration.Name declaration.Constructors.Length
        | TypeAliasNode declaration ->
            printfn "  type %s" declaration.Name
        | EffectDeclarationNode declaration ->
            printfn "  effect %s (%d operation line(s))" declaration.Name declaration.Operations.Length
        | TraitDeclarationNode declaration ->
            printfn "  trait %s (%d member line(s))" declaration.Name declaration.Members.Length
        | InstanceDeclarationNode declaration ->
            printfn "  instance %s (%d member line(s))" declaration.TraitName declaration.Members.Length
        | UnknownDeclaration tokens ->
            printfn "  unknown (%d token(s))" tokens.Length

let private printTraceStep (step: PipelineTraceStep) =
    let verificationText =
        if not step.VerificationAttempted then
            ""
        else
            match step.VerificationSucceeded with
            | Some true -> " verify=ok"
            | Some false -> " verify=failed"
            | None -> " verify=attempted"

    printfn
        "  %-18s %-12s %s -> %s changed=%b%s"
        (PipelineTraceEvent.toPortableName step.Event)
        (PipelineTraceSubject.toPortableName step.Subject)
        step.InputCheckpoint
        step.OutputCheckpoint
        step.ChangedRepresentation
        verificationText

let private printPipelineTrace workspace =
    printfn ""
    printfn "Pipeline trace"
    workspace
    |> Compilation.pipelineTrace
    |> List.iter printTraceStep

let private printVerification checkpoint diagnostics =
    printfn ""
    printfn "Verification for %s" checkpoint

    if List.isEmpty diagnostics then
        printfn "  ok"
    else
        diagnostics |> List.iter printDiagnostic

let private jsonOptions =
    JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

let private serializeJson value =
    JsonSerializer.Serialize(value, jsonOptions)

let private runtimeObligationOwnerText owner =
    match owner with
    | KBackendIRGuaranteed -> "KBackendIRGuaranteed"
    | BackendSpecificRuntime -> "BackendSpecificRuntime"
    | DeferredRuntimeObligation -> "DeferredRuntimeObligation"

let private jsonLocation (location: SourceLocation) =
    {| filePath = location.FilePath
       startLine = location.Start.Line
       startColumn = location.Start.Column
       endLine = location.End.Line
       endColumn = location.End.Column |}

let private jsonRelatedLocation (related: DiagnosticRelatedLocation) =
    {| filePath = related.Location.FilePath
       message = related.Message
       startLine = related.Location.Start.Line
       startColumn = related.Location.Start.Column
       endLine = related.Location.End.Line
       endColumn = related.Location.End.Column |}

let private jsonDiagnostic (diagnostic: Diagnostic) =
    {| severity = severityText diagnostic.Severity
       code = DiagnosticCode.toIdentifier diagnostic.Code
       stage = diagnostic.Stage |> Option.toObj
       phase = diagnostic.Phase |> Option.toObj
       message = diagnostic.Message
       location = diagnostic.Location |> Option.map jsonLocation |> Option.toObj
       relatedLocations = diagnostic.RelatedLocations |> List.map jsonRelatedLocation |}

let private renderObservabilityDumpJson workspace topic =
    let value =
        match topic with
        | AnalysisSessionTopic ->
            let session = Compilation.analysisSession workspace

            box
                {| identity = session.Identity
                   sourceRoot = session.SourceRoot
                   packageMode = session.PackageMode
                   buildConfigurationIdentity = session.BuildConfigurationIdentity
                   backendProfile = session.BackendProfile
                   backendIntrinsicSet = session.BackendIntrinsicSet
                   deploymentMode = session.DeploymentMode |}
        | CheckpointsTopic ->
            box (Compilation.availableCheckpoints workspace)
        | CheckpointContractsTopic ->
            box
                (Compilation.checkpointContracts workspace
                 |> List.map (fun contract ->
                     {| name = contract.Name
                        kind = CheckpointKind.toPortableName contract.CheckpointKind
                        inputCheckpoint = contract.InputCheckpoint |> Option.toObj
                        requiredBySpec = contract.RequiredBySpec
                        profileSpecific = contract.ProfileSpecific |}))
        | VerifyAllCheckpointsTopic ->
            box
                (Compilation.verifyAllCheckpoints workspace
                 |> List.map (fun result ->
                     {| checkpoint = result.Checkpoint
                        succeeded = result.Succeeded
                        diagnostics = result.Diagnostics |> List.map jsonDiagnostic |}))
        | RuntimeObligationsTopic ->
            box
                (Compilation.portableRuntimeObligations workspace
                 |> List.map (fun obligation ->
                     {| name = obligation.Name
                        owner = runtimeObligationOwnerText obligation.Owner
                        description = obligation.Description |}))
        | QuerySketchTopic ->
            box
                (Compilation.querySketch workspace
                 |> List.map (fun query ->
                     {| id = query.Id
                        queryKind = QueryKind.toPortableName query.QueryKind
                        inputKey = query.InputKey
                        outputCheckpoint = query.OutputCheckpoint
                        dependencyModel = QueryDependencyModel.toPortableName query.DependencyModel
                        requiredPhase = query.RequiredPhase |> Option.map KFrontIRPhase.phaseName |> Option.toObj
                        analysisSessionIdentity = query.AnalysisSessionIdentity
                        buildConfigurationIdentity = query.BuildConfigurationIdentity
                        backendProfile = query.BackendProfile
                        backendIntrinsicSet = query.BackendIntrinsicSet
                        dependencyIds = query.DependencyIds |}))
        | CompilerFingerprintsTopic ->
            box
                (Compilation.compilerFingerprints workspace
                 |> List.map (fun fingerprint ->
                     {| id = fingerprint.Id
                        fingerprintKind = CompilerFingerprintKind.toPortableName fingerprint.FingerprintKind
                        inputKey = fingerprint.InputKey
                        identity = fingerprint.Identity
                        analysisSessionIdentity = fingerprint.AnalysisSessionIdentity
                        buildConfigurationIdentity = fingerprint.BuildConfigurationIdentity
                        backendProfile = fingerprint.BackendProfile
                        backendIntrinsicSet = fingerprint.BackendIntrinsicSet
                        dependencyFingerprintIds = fingerprint.DependencyFingerprintIds |}))
        | IncrementalUnitsTopic ->
            box
                (Compilation.incrementalUnits workspace
                 |> List.map (fun unit ->
                     {| id = unit.Id
                        unitKind = IncrementalUnitKind.toPortableName unit.UnitKind
                        inputKey = unit.InputKey
                        outputCheckpoint = unit.OutputCheckpoint |> Option.toObj
                        fingerprintIds = unit.FingerprintIds
                        dependencyUnitIds = unit.DependencyUnitIds
                        analysisSessionIdentity = unit.AnalysisSessionIdentity
                        buildConfigurationIdentity = unit.BuildConfigurationIdentity
                        backendProfile = unit.BackendProfile
                        backendIntrinsicSet = unit.BackendIntrinsicSet |}))

    serializeJson
        {| topic = ObservabilityTopic.toPortableName topic
           value = value |}

let private runProcess = HostSupport.runProcess
let private currentRid = HostSupport.currentRid
let private executablePath = HostSupport.executablePath

let private printProcessFailure (heading: string) (result: HostSupport.ProcessResult) =
    if not (String.IsNullOrWhiteSpace(result.StandardOutput)) then
        Console.Error.WriteLine(result.StandardOutput.TrimEnd())

    if not (String.IsNullOrWhiteSpace(result.StandardError)) then
        Console.Error.WriteLine(result.StandardError.TrimEnd())

    if String.IsNullOrWhiteSpace(result.StandardOutput) && String.IsNullOrWhiteSpace(result.StandardError) then
        Console.Error.WriteLine(heading)

let private runDotNetBackend
    (emitArtifact:
        WorkspaceCompilation -> string -> string -> DotNetDeployment -> Result<DotNetArtifact, string>)
    (workspace: WorkspaceCompilation)
    (entryPoint: string)
    (emitDirectory: string option)
    (nativeAot: bool)
    =
    if nativeAot then
        Console.Error.WriteLine(DotNetDeployment.unsupportedForDotNetMessage)
        1
    else
        let outputDirectory =
            emitDirectory
            |> Option.defaultWith (fun () ->
                Path.Combine(Path.GetTempPath(), "kappa-cli", Guid.NewGuid().ToString("N")))

        let deployment = DotNetDeployment.Managed

        match emitArtifact workspace entryPoint outputDirectory deployment with
        | Result.Error message ->
            Console.Error.WriteLine(message)
            1
        | Result.Ok artifact ->
            let buildResult =
                runProcess artifact.ProjectDirectory "dotnet" $"build \"{artifact.ProjectFilePath}\" -c Release"

            if buildResult.ExitCode <> 0 then
                printProcessFailure "Managed dotnet build failed." buildResult
                1
            else
                let outputDirectory =
                    Path.Combine(artifact.ProjectDirectory, "bin", "Release", "net10.0")

                let executable =
                    executablePath outputDirectory (Path.GetFileNameWithoutExtension(artifact.ProjectFilePath))

                let runResult = runProcess outputDirectory executable ""

                if not (String.IsNullOrWhiteSpace(runResult.StandardOutput)) then
                    Console.Out.Write(runResult.StandardOutput)

                if not (String.IsNullOrWhiteSpace(runResult.StandardError)) then
                    Console.Error.Write(runResult.StandardError)

                runResult.ExitCode

let private resolveZigExecutable () =
    let configuredPath = Environment.GetEnvironmentVariable("KAPPA_ZIG_EXE")

    if String.IsNullOrWhiteSpace(configuredPath) then
        Result.Ok "zig"
    else
        let resolvedPath = configuredPath.Trim()
        let hasDirectoryComponent =
            resolvedPath.IndexOf(Path.DirectorySeparatorChar) >= 0
            || resolvedPath.IndexOf(Path.AltDirectorySeparatorChar) >= 0

        if not hasDirectoryComponent then
            Result.Ok resolvedPath
        elif File.Exists(resolvedPath) then
            Result.Ok resolvedPath
        else
            Result.Error $"Configured Zig executable '{resolvedPath}' does not exist. Set KAPPA_ZIG_EXE to a valid zig executable path."

let private runZigBackend
    (workspace: WorkspaceCompilation)
    (entryPoint: string)
    (emitDirectory: string option)
    (nativeAot: bool)
    =
    if nativeAot then
        Console.Error.WriteLine("The zig backend does not use --native-aot. Native code is its default output.")
        1
    else
        let outputDirectory =
            emitDirectory
            |> Option.defaultWith (fun () ->
                Path.Combine(Path.GetTempPath(), "kappa-cli-zig", Guid.NewGuid().ToString("N")))

        match resolveZigExecutable () with
        | Result.Error message ->
            Console.Error.WriteLine(message)
            1
        | Result.Ok zigExecutable ->
            match Backend.emitZigArtifact workspace entryPoint outputDirectory with
            | Result.Error message ->
                Console.Error.WriteLine(message)
                1
            | Result.Ok artifact ->
                try
                    let compileResult =
                        runProcess
                            artifact.OutputDirectory
                            zigExecutable
                            $"cc -std=c11 -O0 -o \"{artifact.ExecutableFilePath}\" \"{artifact.SourceFilePath}\""

                    if compileResult.ExitCode <> 0 then
                        printProcessFailure "zig build failed." compileResult
                        1
                    else
                        let runResult = runProcess artifact.OutputDirectory artifact.ExecutableFilePath ""

                        if not (String.IsNullOrWhiteSpace(runResult.StandardOutput)) then
                            Console.Out.Write(runResult.StandardOutput)

                        if not (String.IsNullOrWhiteSpace(runResult.StandardError)) then
                            Console.Error.Write(runResult.StandardError)

                        runResult.ExitCode
                with ex ->
                    Console.Error.WriteLine($"Could not launch Zig toolchain '{zigExecutable}': {ex.Message}")
                    1

[<EntryPoint>]
let main argv =
    match Cli.parse argv with
    | Result.Error message ->
        Console.Error.WriteLine(message)
        if message.Contains("kp [--source-root") then 0 else 1
    | Result.Ok options ->
        let observabilityConflicts =
            match options.DumpObservability with
            | None ->
                []
            | Some _ ->
                [
                    if options.DumpTokens then "--dump-tokens"
                    if options.DumpAst then "--dump-ast"
                    if options.DumpStage.IsSome then "--dump-stage"
                    if options.PrintTrace then "--trace"
                    if options.VerifyCheckpoint.IsSome then "--verify"
                    if options.RunBinding.IsSome then "--run"
                ]

        if not (List.isEmpty observabilityConflicts) then
            let conflictText = String.concat ", " observabilityConflicts
            Console.Error.WriteLine(
                $"--dump-observability cannot be combined with {conflictText} because it reserves stdout for machine-readable JSON."
            )

            1
        elif options.DumpObservability.IsSome && options.DumpFormat <> StageDumpFormat.Json then
            Console.Error.WriteLine("--dump-observability currently supports only --dump-format json.")
            1
        else
            let compilationOptions =
                { CompilationOptions.create options.SourceRoot with
                    BackendProfile = options.BackendProfile }

            let workspace = Compilation.parse compilationOptions options.Inputs

            if List.isEmpty workspace.Documents then
                Console.Error.WriteLine($"No .kp files were found under {compilationOptions.SourceRoot}.")
                1
            else
                let explicitObservabilityRequest =
                    options.DumpStage.IsSome || options.PrintTrace || options.VerifyCheckpoint.IsSome || options.DumpObservability.IsSome

                let shouldPrintSummaries =
                    (options.RunBinding.IsNone && not explicitObservabilityRequest)
                    || options.DumpTokens
                    || options.DumpAst

                if shouldPrintSummaries then
                    for document in workspace.Documents do
                        printDocumentSummary document

                if options.DumpTokens then
                    workspace.Documents |> List.iter printTokens

                if options.DumpAst then
                    workspace.Documents |> List.iter printAst

                if options.PrintTrace then
                    printPipelineTrace workspace

                let observabilityDumpFailed =
                    match options.DumpObservability with
                    | Some topic ->
                        let dump = renderObservabilityDumpJson workspace topic
                        Console.Out.WriteLine(dump)
                        false
                    | None ->
                        false

                let verificationFailed =
                    match options.VerifyCheckpoint with
                    | Some checkpoint ->
                        if workspace.HasErrors then
                            false
                        else
                            let diagnostics = Compilation.verifyCheckpoint workspace checkpoint
                            printVerification checkpoint diagnostics
                            not (List.isEmpty diagnostics)
                    | None ->
                        false

                let dumpFailed =
                    match options.DumpStage with
                    | Some checkpoint ->
                        match Compilation.dumpStage workspace checkpoint options.DumpFormat with
                        | Result.Ok dump ->
                            printfn ""
                            printfn "%s" dump
                            false
                        | Result.Error message ->
                            Console.Error.WriteLine(message)
                            true
                    | None ->
                        false

                if not (List.isEmpty workspace.Diagnostics) then
                    if options.DumpObservability.IsSome then
                        Console.Error.WriteLine("Diagnostics")
                        workspace.Diagnostics |> List.iter (writeDiagnostic Console.Error)
                    else
                        printfn ""
                        printfn "Diagnostics"
                        workspace.Diagnostics |> List.iter printDiagnostic

                if workspace.HasErrors || verificationFailed || dumpFailed || observabilityDumpFailed then
                    1
                else
                    match options.RunBinding with
                    | None ->
                        0
                    | Some entryPoint ->
                        match Stdlib.normalizeBackendProfile options.BackendProfile with
                        | "interpreter" ->
                            match Interpreter.executeBinding workspace entryPoint with
                            | Result.Ok value ->
                                if Interpreter.shouldPrintResult value then
                                    printfn "%s" (RuntimeValue.format value)

                                0
                            | Result.Error issue ->
                                Console.Error.WriteLine($"runtime error: {issue.Message}")
                                1
                        | "dotnet" ->
                            runDotNetBackend Backend.emitDotNetArtifact workspace entryPoint options.EmitDirectory options.NativeAot
                        | "zig" ->
                            runZigBackend workspace entryPoint options.EmitDirectory options.NativeAot
                        | other ->
                            Console.Error.WriteLine($"Unsupported runtime backend '{other}'. Expected interpreter, dotnet, or zig.")
                            1
