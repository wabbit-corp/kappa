namespace Kappa.Compiler

open System
open System.IO
open System.Reflection

module Stdlib =
    type BackendIntrinsicSet =
        { Identity: string
          TypeNames: Set<string>
          TraitNames: Set<string>
          TermNames: Set<string>
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

    let private bundledPreludeResourceName = "Kappa.Compiler.Stdlib.std.prelude.kp"

    let private bundledPreludeText =
        lazy
            (let assembly = Assembly.GetExecutingAssembly()
             use stream = assembly.GetManifestResourceStream(bundledPreludeResourceName)

             if isNull stream then
                 invalidOp $"Could not load bundled prelude resource '{bundledPreludeResourceName}'."

             use reader = new StreamReader(stream)
             reader.ReadToEnd().Replace("\r\n", "\n"))

    let BundledPreludeVirtualPath =
        Path.Combine(Path.GetFullPath("__kappa_stdlib__"), "std", "prelude.kp")

    let ZigTargetCheckpointName = "zig.c"
    let ClrTargetCheckpointName = "dotnet.clr"

    let loadBundledPreludeText () = bundledPreludeText.Value

    let shouldImplicitlyImportPrelude moduleName =
        moduleName <> Some PreludeModuleName

    let implicitImportsFor moduleName =
        if shouldImplicitlyImportPrelude moduleName then
            [ PreludeImportSpec; PreludeConstructorImportSpec ]
        else
            []

    let private intrinsicTypeNames =
        Set.ofList [ "Unit"; "Char"; "String"; "Int"; "Nat"; "Float"; "Bytes"; "Array"; "Ref"; "IO"; "Regex" ]

    let private intrinsicTraitNames =
        Set.ofList
            [
                "Eq"
                "Ord"
                "Show"
                "Functor"
                "Applicative"
                "Monad"
                "MonadRef"
                "Foldable"
                "Traversable"
                "FromInteger"
                "FromFloat"
                "FromString"
                "MonadError"
            ]

    let private intrinsicTermNames =
        Set.ofList
            [
                "pure"
                ">>="
                ">>"
                "True"
                "False"
                "not"
                "and"
                "or"
                "negate"
                "println"
                "print"
                "printInt"
                "printString"
                "primitiveIntToString"
                "openFile"
                "primitiveReadData"
                "readData"
                "primitiveCloseFile"
                "newRef"
                "readRef"
                "writeRef"
            ]

    let private runtimeOnlyIntrinsicTermNames =
        Set.ofList [ "+"; "-"; "*"; "/"; "&&"; "||"; "=="; "!="; "<"; "<="; ">"; ">=" ]

    let private elaborationAvailableIntrinsicTermNames =
        Set.ofList [ "True"; "False"; "not"; "and"; "or"; "negate"; "+"; "-"; "*"; "/"; "&&"; "||"; "=="; "!="; "<"; "<="; ">"; ">=" ]

    let private moduleLocalIntrinsicTermNames =
        Set.ofList [ "openFile"; "primitiveReadData"; "readData"; "primitiveCloseFile" ]

    let private preludeIntrinsicSet =
        { Identity = "bootstrap-prelude-v2"
          TypeNames = intrinsicTypeNames
          TraitNames = intrinsicTraitNames
          TermNames = intrinsicTermNames
          RuntimeTermNames = Set.union intrinsicTermNames runtimeOnlyIntrinsicTermNames
          ElaborationAvailableTermNames = elaborationAvailableIntrinsicTermNames }

    let private emptyIntrinsicSet =
        { Identity = "none"
          TypeNames = Set.empty
          TraitNames = Set.empty
          TermNames = Set.empty
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
            (intrinsicSetForBackendProfile backendProfile).TermNames
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
            intrinsicSet.TermNames
        else
            Set.intersect intrinsicSet.TermNames moduleLocalIntrinsicTermNames

    let intrinsicTermNamesAvailableInModuleText backendProfile moduleName =
        let intrinsicSet = intrinsicSetForBackendProfile backendProfile

        if moduleName = PreludeModuleText then
            intrinsicSet.TermNames
        else
            Set.intersect intrinsicSet.TermNames moduleLocalIntrinsicTermNames

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
