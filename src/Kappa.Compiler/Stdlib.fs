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

    let standardModuleTermNames moduleName =
        BundledStandardModules.tryTermNames moduleName
        |> Option.orElseWith (fun () ->
            StandardModules.byName
            |> Map.tryFind moduleName
            |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList))
        |> Option.defaultValue Set.empty

    let standardModuleTypeNames moduleName =
        BundledStandardModules.tryTypeNames moduleName
        |> Option.orElseWith (fun () ->
            StandardModules.byName
            |> Map.tryFind moduleName
            |> Option.map (fun description -> description.Types |> Set.ofList))
        |> Option.defaultValue Set.empty

    let standardModuleTraitNames moduleName =
        BundledStandardModules.tryTraitNames moduleName
        |> Option.orElseWith (fun () ->
            StandardModules.byName
            |> Map.tryFind moduleName
            |> Option.map (fun description -> description.Traits |> List.map (fun traitInfo -> traitInfo.Name) |> Set.ofList))
        |> Option.defaultValue Set.empty

    let tryStandardModuleTermTypeText moduleName termName =
        BundledStandardModules.tryTermTypeText moduleName termName
        |> Option.orElseWith (fun () ->
            StandardModules.byName
            |> Map.tryFind moduleName
            |> Option.bind (fun description ->
                description.Terms
                |> List.tryFind (fun term -> String.Equals(term.Name, termName, StringComparison.Ordinal))
                |> Option.map (fun term -> term.TypeText)))

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
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> BackendProfile.toPortableName

    let intrinsicSetForBackend backendProfile =
        match backendProfile with
        | BackendProfile.Interpreter
        | BackendProfile.DotNet
        | BackendProfile.Zig ->
            preludeIntrinsicSet
        | BackendProfile.Unknown _ ->
            emptyIntrinsicSet

    let intrinsicSetForBackendProfile backendProfile =
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> intrinsicSetForBackend

    let intrinsicSetForCompilationProfile backendProfile allowUnsafeConsume =
        let intrinsicSet = intrinsicSetForBackend backendProfile

        if allowUnsafeConsume && intrinsicSet.Identity <> emptyIntrinsicSet.Identity then
            withUnsafeConsume intrinsicSet
        else
            intrinsicSet

    let intrinsicSetForCompilation backendProfile allowUnsafeConsume =
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> fun profile -> intrinsicSetForCompilationProfile profile allowUnsafeConsume

    let intrinsicTermNamesForBackend backendProfile moduleName =
        if moduleName = PreludeModuleName then
            (intrinsicSetForBackend backendProfile).PreludeTermNames
        else
            Set.empty

    let intrinsicTermNamesFor backendProfile moduleName =
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> fun profile -> intrinsicTermNamesForBackend profile moduleName

    let intrinsicTermNamesForCompilation backendProfile allowUnsafeConsume moduleName =
        if moduleName = PreludeModuleName then
            (intrinsicSetForCompilation backendProfile allowUnsafeConsume).PreludeTermNames
        else
            Set.empty

    let runtimeIntrinsicTermNamesForBackend backendProfile moduleName =
        if moduleName = PreludeModuleName then
            (intrinsicSetForBackend backendProfile).RuntimeTermNames
        else
            BundledStandardModules.tryIntrinsicTermNames moduleName
            |> Option.orElseWith (fun () ->
                let moduleNameText = SyntaxFacts.moduleNameToText moduleName

                StandardModules.byName
                |> Map.tryFind moduleNameText
                |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList))
            |> Option.defaultValue Set.empty

    let runtimeIntrinsicTermNamesFor backendProfile moduleName =
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> fun profile -> runtimeIntrinsicTermNamesForBackend profile moduleName

    let runtimeIntrinsicTermNamesForCompilationProfile backendProfile allowUnsafeConsume moduleName =
        if moduleName = PreludeModuleName then
            (intrinsicSetForCompilationProfile backendProfile allowUnsafeConsume).RuntimeTermNames
        else
            BundledStandardModules.tryIntrinsicTermNames moduleName
            |> Option.orElseWith (fun () ->
                let moduleNameText = SyntaxFacts.moduleNameToText moduleName

                StandardModules.byName
                |> Map.tryFind moduleNameText
                |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList))
            |> Option.defaultValue Set.empty

    let runtimeIntrinsicTermNamesForCompilation backendProfile allowUnsafeConsume moduleName =
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> fun profile -> runtimeIntrinsicTermNamesForCompilationProfile profile allowUnsafeConsume moduleName

    let targetCheckpointNamesForBackend backendProfile =
        match backendProfile with
        | BackendProfile.Zig ->
            [ ZigTargetCheckpointName ]
        | BackendProfile.DotNet ->
            [ ClrTargetCheckpointName ]
        | BackendProfile.Interpreter
        | BackendProfile.Unknown _ ->
            []

    let targetCheckpointNamesFor backendProfile =
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> targetCheckpointNamesForBackend

    let private isPreludeExpectation moduleName =
        moduleName = PreludeModuleName

    let intrinsicTermNamesAvailableInModuleForBackend backendProfile moduleName =
        let intrinsicSet = intrinsicSetForBackend backendProfile

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

    let intrinsicTermNamesAvailableInModule backendProfile moduleName =
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> fun profile -> intrinsicTermNamesAvailableInModuleForBackend profile moduleName

    let intrinsicTermNamesAvailableInModuleText backendProfile moduleName =
        let intrinsicSet =
            backendProfile
            |> BackendProfile.normalizeConfigured
            |> intrinsicSetForBackend

        if moduleName = PreludeModuleText then
            intrinsicSet.PreludeTermNames
        else
            BundledStandardModules.tryIntrinsicTermNamesText moduleName
            |> Option.orElseWith (fun () ->
                StandardModules.byName
                |> Map.tryFind moduleName
                |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList))
            |> Option.defaultValue intrinsicSet.ModuleLocalTermNames

    let intrinsicTermNamesAvailableInModuleForCompilationProfile backendProfile allowUnsafeConsume moduleName =
        let intrinsicSet = intrinsicSetForCompilationProfile backendProfile allowUnsafeConsume

        if moduleName = PreludeModuleName then
            intrinsicSet.PreludeTermNames
        else
            BundledStandardModules.tryIntrinsicTermNames moduleName
            |> Option.orElseWith (fun () ->
                moduleName
                |> SyntaxFacts.moduleNameToText
                |> fun moduleNameText ->
                    StandardModules.byName
                    |> Map.tryFind moduleNameText
                    |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList))
            |> Option.defaultValue intrinsicSet.ModuleLocalTermNames

    let intrinsicTermNamesAvailableInModuleForCompilation backendProfile allowUnsafeConsume moduleName =
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> fun profile -> intrinsicTermNamesAvailableInModuleForCompilationProfile profile allowUnsafeConsume moduleName

    let intrinsicTermNamesAvailableInModuleTextForCompilation backendProfile allowUnsafeConsume moduleName =
        let intrinsicSet =
            backendProfile
            |> BackendProfile.normalizeConfigured
            |> fun profile -> intrinsicSetForCompilationProfile profile allowUnsafeConsume

        if moduleName = PreludeModuleText then
            intrinsicSet.PreludeTermNames
        else
            BundledStandardModules.tryIntrinsicTermNamesText moduleName
            |> Option.orElseWith (fun () ->
                StandardModules.byName
                |> Map.tryFind moduleName
                |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList))
            |> Option.defaultValue intrinsicSet.ModuleLocalTermNames

    let intrinsicTermNamesAvailableInModuleTextForCompilationProfile backendProfile allowUnsafeConsume moduleName =
        let intrinsicSet = intrinsicSetForCompilationProfile backendProfile allowUnsafeConsume

        if moduleName = PreludeModuleText then
            intrinsicSet.PreludeTermNames
        else
            BundledStandardModules.tryIntrinsicTermNamesText moduleName
            |> Option.orElseWith (fun () ->
                StandardModules.byName
                |> Map.tryFind moduleName
                |> Option.map (fun description -> description.Terms |> List.map (fun term -> term.Name) |> Set.ofList))
            |> Option.defaultValue intrinsicSet.ModuleLocalTermNames

    let intrinsicallySatisfiesExpectForBackend backendProfile moduleName declaration =
        let intrinsicSet = intrinsicSetForBackend backendProfile

        match declaration with
        | ExpectTypeDeclaration declaration ->
            (isPreludeExpectation moduleName && intrinsicSet.TypeNames.Contains(declaration.Name))
            || (standardModuleTypeNames (SyntaxFacts.moduleNameToText moduleName) |> Set.contains declaration.Name)
        | ExpectTraitDeclaration declaration ->
            (isPreludeExpectation moduleName && intrinsicSet.TraitNames.Contains(declaration.Name))
            || (standardModuleTraitNames (SyntaxFacts.moduleNameToText moduleName) |> Set.contains declaration.Name)
        | ExpectTermDeclaration declaration ->
            intrinsicTermNamesAvailableInModuleForBackend backendProfile moduleName
            |> Set.contains declaration.Name

    let intrinsicallySatisfiesExpect backendProfile moduleName declaration =
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> fun profile -> intrinsicallySatisfiesExpectForBackend profile moduleName declaration

    let intrinsicallySatisfiesExpectForCompilationProfile backendProfile allowUnsafeConsume moduleName declaration =
        let intrinsicSet = intrinsicSetForCompilationProfile backendProfile allowUnsafeConsume

        match declaration with
        | ExpectTypeDeclaration declaration ->
            (isPreludeExpectation moduleName && intrinsicSet.TypeNames.Contains(declaration.Name))
            || (standardModuleTypeNames (SyntaxFacts.moduleNameToText moduleName) |> Set.contains declaration.Name)
        | ExpectTraitDeclaration declaration ->
            (isPreludeExpectation moduleName && intrinsicSet.TraitNames.Contains(declaration.Name))
            || (standardModuleTraitNames (SyntaxFacts.moduleNameToText moduleName) |> Set.contains declaration.Name)
        | ExpectTermDeclaration declaration ->
            intrinsicTermNamesAvailableInModuleForCompilationProfile backendProfile allowUnsafeConsume moduleName
            |> Set.contains declaration.Name

    let intrinsicallySatisfiesExpectForCompilation backendProfile allowUnsafeConsume moduleName declaration =
        backendProfile
        |> BackendProfile.normalizeConfigured
        |> fun profile -> intrinsicallySatisfiesExpectForCompilationProfile profile allowUnsafeConsume moduleName declaration
