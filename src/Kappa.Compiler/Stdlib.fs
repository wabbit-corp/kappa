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
                "Foldable"
                "Traversable"
                "FromInteger"
                "FromFloat"
                "FromString"
                "MonadError"
            ]

    let private intrinsicTermNames =
        Set.ofList [ "pure"; ">>="; ">>"; "True"; "False"; "not"; "and"; "or"; "negate"; "println"; "print"; "printInt" ]

    let private preludeIntrinsicSet =
        { Identity = "prelude-core-v1"
          TypeNames = intrinsicTypeNames
          TraitNames = intrinsicTraitNames
          TermNames = intrinsicTermNames
          ElaborationAvailableTermNames = intrinsicTermNames }

    let private emptyIntrinsicSet =
        { Identity = "none"
          TypeNames = Set.empty
          TraitNames = Set.empty
          TermNames = Set.empty
          ElaborationAvailableTermNames = Set.empty }

    let normalizeBackendProfile (backendProfile: string) =
        if String.IsNullOrWhiteSpace(backendProfile) then
            "interpreter"
        else
            backendProfile.Trim().ToLowerInvariant()

    let intrinsicSetForBackendProfile backendProfile =
        match normalizeBackendProfile backendProfile with
        | "interpreter"
        | "dotnet"
        | "dotnet-hosted"
        | "dotnet-il" ->
            preludeIntrinsicSet
        | _ ->
            emptyIntrinsicSet

    let intrinsicTermNamesFor backendProfile moduleName =
        if moduleName = PreludeModuleName then
            (intrinsicSetForBackendProfile backendProfile).TermNames
        else
            Set.empty

    let private isPreludeExpectation moduleName =
        moduleName = PreludeModuleName

    let intrinsicallySatisfiesExpect backendProfile moduleName declaration =
        if not (isPreludeExpectation moduleName) then
            false
        else
            let intrinsicSet = intrinsicSetForBackendProfile backendProfile

            match declaration with
            | ExpectTypeDeclaration declaration ->
                intrinsicSet.TypeNames.Contains(declaration.Name)
            | ExpectTraitDeclaration declaration ->
                intrinsicSet.TraitNames.Contains(declaration.Name)
            | ExpectTermDeclaration declaration ->
                intrinsicSet.TermNames.Contains(declaration.Name)
