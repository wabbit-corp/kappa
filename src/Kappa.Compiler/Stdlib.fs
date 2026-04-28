namespace Kappa.Compiler

open System
// Models the bundled bootstrap prelude and compiler-known intrinsic/prelude surface.
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
          ":&"
          "LT"
          "EQ"
          "GT"
          "Reusable"
          "OneShot"
          "QZero"
          "QOne"
          "QZeroOrOne"
          "QOneOrMore"
          "QZeroOrMore"
          "QueryMode"
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
                      Name = name
                      IncludeConstructors = false
                      Alias = None })
            ) }

    let BundledPreludeVirtualPath =
        BundledPrelude.virtualPath

    let ZigTargetCheckpointName = "zig.c"
    let ClrTargetCheckpointName = "dotnet.clr"

    let UnsafeConsumeTermName = "unsafeConsume"

    let private unsafeConsumePreludeText =
        $"{Environment.NewLine}expect term {UnsafeConsumeTermName} : (1 x : a) -> Unit{Environment.NewLine}"

    let loadBundledPreludeText () = BundledPrelude.loadText ()

    let loadBundledPreludeTextForOptions allowUnsafeConsume =
        if allowUnsafeConsume then
            BundledPrelude.loadText () + unsafeConsumePreludeText
        else
            BundledPrelude.loadText ()

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

    let private withUnsafeConsume (intrinsicSet: BackendIntrinsicSet) =
        { intrinsicSet with
            Identity = $"{intrinsicSet.Identity}+unsafe-consume"
            PreludeTermNames = Set.add UnsafeConsumeTermName intrinsicSet.PreludeTermNames
            RuntimeTermNames = Set.add UnsafeConsumeTermName intrinsicSet.RuntimeTermNames }

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
            | "dotnet-il" -> "dotnet"
            | normalized -> normalized

    let intrinsicSetForBackendProfile backendProfile =
        match normalizeBackendProfile backendProfile with
        | "interpreter"
        | "dotnet"
        | "zig" ->
            preludeIntrinsicSet
        | _ ->
            emptyIntrinsicSet

    let intrinsicSetForCompilation backendProfile allowUnsafeConsume =
        let intrinsicSet = intrinsicSetForBackendProfile backendProfile

        if allowUnsafeConsume && intrinsicSet.Identity <> emptyIntrinsicSet.Identity then
            withUnsafeConsume intrinsicSet
        else
            intrinsicSet

    let intrinsicTermNamesFor backendProfile moduleName =
        if moduleName = PreludeModuleName then
            (intrinsicSetForBackendProfile backendProfile).PreludeTermNames
        else
            Set.empty

    let intrinsicTermNamesForCompilation backendProfile allowUnsafeConsume moduleName =
        if moduleName = PreludeModuleName then
            (intrinsicSetForCompilation backendProfile allowUnsafeConsume).PreludeTermNames
        else
            Set.empty

    let runtimeIntrinsicTermNamesFor backendProfile moduleName =
        let moduleNameText = SyntaxFacts.moduleNameToText moduleName

        if moduleName = PreludeModuleName then
            (intrinsicSetForBackendProfile backendProfile).RuntimeTermNames
        else
            StandardModules.byName
            |> Map.tryFind moduleNameText
            |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList)
            |> Option.defaultValue Set.empty

    let runtimeIntrinsicTermNamesForCompilation backendProfile allowUnsafeConsume moduleName =
        let moduleNameText = SyntaxFacts.moduleNameToText moduleName

        if moduleName = PreludeModuleName then
            (intrinsicSetForCompilation backendProfile allowUnsafeConsume).RuntimeTermNames
        else
            StandardModules.byName
            |> Map.tryFind moduleNameText
            |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList)
            |> Option.defaultValue Set.empty

    let targetCheckpointNamesFor backendProfile =
        match normalizeBackendProfile backendProfile with
        | "zig" ->
            [ ZigTargetCheckpointName ]
        | "dotnet" ->
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
            moduleName
            |> SyntaxFacts.moduleNameToText
            |> fun moduleNameText ->
                StandardModules.byName
                |> Map.tryFind moduleNameText
                |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList)
                |> Option.defaultValue intrinsicSet.ModuleLocalTermNames

    let intrinsicTermNamesAvailableInModuleText backendProfile moduleName =
        let intrinsicSet = intrinsicSetForBackendProfile backendProfile

        if moduleName = PreludeModuleText then
            intrinsicSet.PreludeTermNames
        else
            StandardModules.byName
            |> Map.tryFind moduleName
            |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList)
            |> Option.defaultValue intrinsicSet.ModuleLocalTermNames

    let intrinsicTermNamesAvailableInModuleForCompilation backendProfile allowUnsafeConsume moduleName =
        let intrinsicSet = intrinsicSetForCompilation backendProfile allowUnsafeConsume

        if moduleName = PreludeModuleName then
            intrinsicSet.PreludeTermNames
        else
            moduleName
            |> SyntaxFacts.moduleNameToText
            |> fun moduleNameText ->
                StandardModules.byName
                |> Map.tryFind moduleNameText
                |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList)
                |> Option.defaultValue intrinsicSet.ModuleLocalTermNames

    let intrinsicTermNamesAvailableInModuleTextForCompilation backendProfile allowUnsafeConsume moduleName =
        let intrinsicSet = intrinsicSetForCompilation backendProfile allowUnsafeConsume

        if moduleName = PreludeModuleText then
            intrinsicSet.PreludeTermNames
        else
            StandardModules.byName
            |> Map.tryFind moduleName
            |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList)
            |> Option.defaultValue intrinsicSet.ModuleLocalTermNames

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

    let intrinsicallySatisfiesExpectForCompilation backendProfile allowUnsafeConsume moduleName declaration =
        let intrinsicSet = intrinsicSetForCompilation backendProfile allowUnsafeConsume

        match declaration with
        | ExpectTypeDeclaration declaration ->
            isPreludeExpectation moduleName && intrinsicSet.TypeNames.Contains(declaration.Name)
        | ExpectTraitDeclaration declaration ->
            isPreludeExpectation moduleName && intrinsicSet.TraitNames.Contains(declaration.Name)
        | ExpectTermDeclaration declaration ->
            intrinsicTermNamesAvailableInModuleForCompilation backendProfile allowUnsafeConsume moduleName
            |> Set.contains declaration.Name
