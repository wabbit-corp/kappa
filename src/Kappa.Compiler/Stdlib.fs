namespace Kappa.Compiler

open System
module Stdlib =
    type BackendIntrinsicSet =
        { Identity: string
          TypeNames: Set<string>
          TraitNames: Set<string>
          PreludeTermNames: Set<string>
          ModuleLocalTermNames: Set<string>
          RuntimeTermNames: Set<string>
          ElaborationAvailableTermNames: Set<string> }

    let PreludeModuleName = [ "std"; "prelude" ]
    let PreludeModuleText = SyntaxFacts.moduleNameToText PreludeModuleName

    let PreludeImportSpec =
        { Source = Dotted PreludeModuleName
          Alias = None
          Selection = All }

    let FixedPreludeConstructors =
        [ "True"
          "False"
          "None"
          "Some"
          "Ok"
          "Err"
          "Nil"
          "::"
          "LT"
          "EQ"
          "GT"
          "refl" ]

    let PreludeConstructorImportSpec =
        { Source = Dotted PreludeModuleName
          Alias = None
          Selection =
            Items(
                FixedPreludeConstructors
                |> List.map (fun name ->
                    { Modifiers = []
                      Namespace = Some ImportNamespace.Constructor
                      Name = name })
            ) }

    let BundledPreludeVirtualPath =
        BundledPrelude.virtualPath

    let ZigTargetCheckpointName = "zig.c"
    let ClrTargetCheckpointName = "dotnet.clr"

    let loadBundledPreludeText () = BundledPrelude.loadText ()

    let shouldImplicitlyImportPrelude moduleName =
        moduleName <> Some PreludeModuleName

    let implicitImportsFor moduleName =
        if shouldImplicitlyImportPrelude moduleName then
            [ PreludeImportSpec; PreludeConstructorImportSpec ]
        else
            []

    let private preludeIntrinsicSet =
        let contract = IntrinsicCatalog.bundledPreludeExpectContract ()

        { Identity = IntrinsicCatalog.bootstrapIntrinsicIdentity
          TypeNames = contract.TypeNames
          TraitNames = contract.TraitNames
          PreludeTermNames = contract.TermNames
          ModuleLocalTermNames = IntrinsicCatalog.moduleLocalIntrinsicTermNames
          RuntimeTermNames = IntrinsicCatalog.runtimePreludeIntrinsicTermNames ()
          ElaborationAvailableTermNames = IntrinsicCatalog.elaborationAvailableIntrinsicTermNames () }

    let private emptyIntrinsicSet =
        { Identity = "none"
          TypeNames = Set.empty
          TraitNames = Set.empty
          PreludeTermNames = Set.empty
          ModuleLocalTermNames = Set.empty
          RuntimeTermNames = Set.empty
          ElaborationAvailableTermNames = Set.empty }

    let normalizeBackendProfile (backendProfile: string) =
        if String.IsNullOrWhiteSpace(backendProfile) then
            "interpreter"
        else
            match backendProfile.Trim().ToLowerInvariant() with
            | "zigcc" -> "zig"
            | normalized -> normalized

    let intrinsicSetForBackendProfile backendProfile =
        match normalizeBackendProfile backendProfile with
        | "interpreter"
        | "dotnet"
        | "dotnet-hosted"
        | "dotnet-il"
        | "zig" ->
            preludeIntrinsicSet
        | _ ->
            emptyIntrinsicSet

    let intrinsicTermNamesFor backendProfile moduleName =
        if moduleName = PreludeModuleName then
            (intrinsicSetForBackendProfile backendProfile).PreludeTermNames
        else
            Set.empty

    let runtimeIntrinsicTermNamesFor backendProfile moduleName =
        if moduleName = PreludeModuleName then
            (intrinsicSetForBackendProfile backendProfile).RuntimeTermNames
        else
            Set.empty

    let targetCheckpointNamesFor backendProfile =
        match normalizeBackendProfile backendProfile with
        | "zig" ->
            [ ZigTargetCheckpointName ]
        | "dotnet"
        | "dotnet-il" ->
            [ ClrTargetCheckpointName ]
        | _ ->
            []

    let private isPreludeExpectation moduleName =
        moduleName = PreludeModuleName

    let intrinsicTermNamesAvailableInModule backendProfile moduleName =
        let intrinsicSet = intrinsicSetForBackendProfile backendProfile

        if isPreludeExpectation moduleName then
            intrinsicSet.PreludeTermNames
        else
            intrinsicSet.ModuleLocalTermNames

    let intrinsicTermNamesAvailableInModuleText backendProfile moduleName =
        let intrinsicSet = intrinsicSetForBackendProfile backendProfile

        if moduleName = PreludeModuleText then
            intrinsicSet.PreludeTermNames
        else
            intrinsicSet.ModuleLocalTermNames

    let intrinsicallySatisfiesExpect backendProfile moduleName declaration =
        let intrinsicSet = intrinsicSetForBackendProfile backendProfile

        match declaration with
        | ExpectTypeDeclaration declaration ->
            isPreludeExpectation moduleName && intrinsicSet.TypeNames.Contains(declaration.Name)
        | ExpectTraitDeclaration declaration ->
            isPreludeExpectation moduleName && intrinsicSet.TraitNames.Contains(declaration.Name)
        | ExpectTermDeclaration declaration ->
            intrinsicTermNamesAvailableInModule backendProfile moduleName
            |> Set.contains declaration.Name
