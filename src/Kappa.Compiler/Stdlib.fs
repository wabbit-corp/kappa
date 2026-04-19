namespace Kappa.Compiler

open System
open System.IO
open System.Reflection

module Stdlib =
    let PreludeModuleName = [ "std"; "prelude" ]
    let PreludeModuleText = SyntaxFacts.moduleNameToText PreludeModuleName

    let PreludeImportSpec =
        { Source = Dotted PreludeModuleName
          Alias = None
          Selection = All }

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
            [ PreludeImportSpec ]
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

    let IntrinsicTermNames =
        Set.ofList [ "pure"; ">>="; ">>"; "True"; "False"; "not"; "and"; "or"; "negate"; "println"; "print" ]

    let private isPreludeExpectation moduleName =
        moduleName = PreludeModuleName

    let intrinsicallySatisfiesExpect moduleName declaration =
        if not (isPreludeExpectation moduleName) then
            false
        else
            match declaration with
            | ExpectTypeDeclaration declaration ->
                intrinsicTypeNames.Contains(declaration.Name)
            | ExpectTraitDeclaration declaration ->
                intrinsicTraitNames.Contains(declaration.Name)
            | ExpectTermDeclaration declaration ->
                IntrinsicTermNames.Contains(declaration.Name)
