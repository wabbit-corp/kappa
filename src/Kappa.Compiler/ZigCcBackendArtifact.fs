namespace Kappa.Compiler

open System
open System.IO

module internal ZigCcBackendArtifact =
    open ZigCcBackendSupport
    open ZigCcBackendEmit
    open ZigCcBackendRuntime

    let internal buildTranslationUnit (workspace: WorkspaceCompilation) =
        result {
            if workspace.HasErrors then
                return!
                    Result.Error
                        $"Cannot emit native code for a workspace with diagnostics:{Environment.NewLine}{aggregateDiagnostics workspace.Diagnostics}"

            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                return!
                    Result.Error
                        $"Cannot emit native code from malformed KBackendIR:{Environment.NewLine}{aggregateDiagnostics verificationDiagnostics}"

            let context = buildContext workspace

            let topLevelFunctions =
                workspace.KBackendIR
                |> List.collect (fun moduleDump ->
                    moduleDump.Functions
                    |> List.filter (fun functionDump -> not functionDump.Intrinsic)
                    |> List.map (fun functionDump -> moduleDump.Name, functionDump))

            let! emittedTopLevelFunctions =
                topLevelFunctions
                |> List.fold
                    (fun state (moduleName, functionDump) ->
                        result {
                            let! emitted = state
                            let! emittedFunction = emitFunctionDefinition context moduleName functionDump
                            return emitted @ [ emittedFunction ]
                        })
                    (Result.Ok [])

            let closureFunctions =
                context.GeneratedClosures |> Seq.toList

            let prototypes =
                emittedTopLevelFunctions
                |> List.map (fun functionDump -> functionDump.Prototype)
                |> List.append (closureFunctions |> List.map (fun functionDump -> functionDump.Prototype))

            let supportDefinitions =
                closureFunctions
                |> List.collect (fun functionDump -> functionDump.SupportDefinitions)

            let traitDispatchDefinitions =
                emitTraitDispatchFunctions context

            let definitions =
                emittedTopLevelFunctions
                |> List.map (fun functionDump -> functionDump.Definition)
                |> List.append (closureFunctions |> List.map (fun functionDump -> functionDump.Definition))

            let entrySymbols =
                workspace.KBackendIR
                |> List.collect (fun moduleDump ->
                    moduleDump.EntryPoints
                    |> List.choose (fun entryPointName ->
                        lookupFunctionName context moduleDump.Name entryPointName))
                |> List.distinct
                |> List.sort

            let functionSymbols =
                context.FunctionNames
                |> Map.toList
                |> List.map snd
                |> List.sort

            let sourceText =
                [
                    yield emitRuntimePrelude context
                    if not (List.isEmpty prototypes) then
                        yield joinLines prototypes
                    if not (List.isEmpty traitDispatchDefinitions) then
                        yield joinLines traitDispatchDefinitions
                    if not (List.isEmpty supportDefinitions) then
                        yield joinLines supportDefinitions
                    if not (List.isEmpty definitions) then
                        yield joinLines definitions
                ]
                |> String.concat (Environment.NewLine + Environment.NewLine)

            return
                { ArtifactKind = "c-translation-unit"
                  TranslationUnitName = "kappa.generated.c"
                  InputCheckpoint = "KBackendIR"
                  EntrySymbols = entrySymbols
                  FunctionSymbols = functionSymbols
                  SourceText = sourceText }
        }

    let emitTranslationUnit (workspace: WorkspaceCompilation) =
        buildTranslationUnit workspace

    let emitArtifact (workspace: WorkspaceCompilation) (entryPoint: string) (outputDirectory: string) =
        result {
            let! translationUnit = buildTranslationUnit workspace
            let! entryModuleName, entryBindingName = resolveEntryPoint workspace entryPoint
            let context = buildContext workspace

            let! entryFunctionName =
                lookupFunctionName context entryModuleName entryBindingName
                |> resultOfOption $"zig could not resolve emitted entry point '{entryPoint}'."

            let resolvedOutputDirectory = Path.GetFullPath(outputDirectory)
            Directory.CreateDirectory(resolvedOutputDirectory) |> ignore

            let sourceFilePath = Path.Combine(resolvedOutputDirectory, translationUnit.TranslationUnitName)
            let executableFilePath = executablePath resolvedOutputDirectory "kappa.generated"

            let sourceText =
                translationUnit.SourceText
                + (Environment.NewLine + Environment.NewLine)
                + emitEntryPointProgram entryFunctionName

            File.WriteAllText(sourceFilePath, sourceText)

            return
                { OutputDirectory = resolvedOutputDirectory
                  SourceFilePath = sourceFilePath
                  ExecutableFilePath = executableFilePath
                  EntryPoint = entryPoint }
        }
