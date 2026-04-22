namespace Kappa.Compiler

module internal CompilationSnapshots =
    let private explicitImportSpecs (document: ParsedDocument) =
        document.Syntax.Declarations
        |> List.collect (function
            | ImportDeclaration (_, specs) -> specs
            | _ -> [])

    let private diagnosticVisibleAtPhase phase (diagnostic: Diagnostic) =
        match diagnostic.Phase with
        | Some phaseName ->
            match KFrontIRPhase.tryParsePhaseName phaseName with
            | Some diagnosticPhase ->
                KFrontIRPhase.ordinal diagnosticPhase <= KFrontIRPhase.ordinal phase
            | None ->
                true
        | None ->
            true

    let private documentSnapshot ownershipFactsByFile phase (document: ParsedDocument) =
        let imports =
            if KFrontIRPhase.ordinal phase >= KFrontIRPhase.ordinal IMPORTS then
                CompilationFrontend.collectImportSpecs document
            else
                explicitImportSpecs document

        { FilePath = document.Source.FilePath
          ModuleHeader = document.Syntax.ModuleHeader
          InferredModuleName = document.InferredModuleName
          ModuleIdentity = document.ModuleName
          ModuleAttributes = document.Syntax.ModuleAttributes
          Imports = imports
          Tokens = document.Syntax.Tokens
          Declarations = document.Syntax.Declarations
          Diagnostics = document.Diagnostics |> List.filter (diagnosticVisibleAtPhase phase)
          Ownership =
            if KFrontIRPhase.ordinal phase >= KFrontIRPhase.ordinal BODY_RESOLVE then
                Map.tryFind document.Source.FilePath ownershipFactsByFile
            else
                None
          ResolvedPhases = KFrontIRPhase.phasesThrough phase |> Set.ofList }

    let buildFrontendSnapshots ownershipFactsByFile (diagnostics: Diagnostic list) (documents: ParsedDocument list) =
        KFrontIRPhase.all
        |> List.map (fun phase ->
            let modules =
                documents
                |> List.map (documentSnapshot ownershipFactsByFile phase)
                |> List.sortBy (fun document -> document.FilePath)

            let snapshotDiagnostics =
                diagnostics
                |> List.filter (diagnosticVisibleAtPhase phase)

            phase,
            { Modules = modules
              Diagnostics = snapshotDiagnostics })
        |> Map.ofList
