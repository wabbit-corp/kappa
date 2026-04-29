namespace Kappa.Compiler

open System
open System.IO
open System.Reflection

// Loads bundled standard-library source modules other than the bootstrap prelude.
module internal BundledStandardModules =
    type BundledStandardTermDescription =
        { Name: string
          TypeText: string }

    type BundledStandardTraitDescription =
        { Name: string
          TypeParameterCount: int }

    type BundledStandardModule =
        { ModuleName: string list
          VirtualPath: string
          Types: string list
          Terms: BundledStandardTermDescription list
          Traits: BundledStandardTraitDescription list
          IntrinsicTermNames: Set<string>
          LoadText: unit -> string }

    let private loadResourceText resourceName =
        lazy
            (let assembly = Assembly.GetExecutingAssembly()
             use stream = assembly.GetManifestResourceStream(resourceName)

             if isNull stream then
                 invalidOp $"Could not load bundled stdlib resource '{resourceName}'."

             use reader = new StreamReader(stream)
             reader.ReadToEnd().Replace("\r\n", "\n"))

    let private derivingShapeText =
        loadResourceText "Kappa.Compiler.Stdlib.std.deriving.shape.kp"

    let private derivingShapeModule =
        { ModuleName = [ "std"; "deriving"; "shape" ]
          VirtualPath = Path.Combine(Path.GetFullPath("__kappa_stdlib__"), "std", "deriving", "shape.kp")
          Types = []
          Terms = []
          Traits = []
          IntrinsicTermNames =
            Set.ofList
                [ "tryInspectAdt"
                  "inspectAdt"
                  "tryInspectRecord"
                  "inspectRecord"
                  "runtimeConstructorFields"
                  "runtimeRecordFields"
                  "requiredRuntimeFieldConstraints"
                  "requireRuntimeFieldInstances"
                  "requireRecordFieldInstances"
                  "fieldArgument"
                  "omitImplicitFieldArgument"
                  "matchAdt"
                  "matchAdt2"
                  "constructAdt"
                  "matchRecord"
                  "constructRecord"
                  "stringSyntax"
                  "natSyntax"
                  "boolSyntax"
                  "unitSyntax" ]
          LoadText = fun () -> derivingShapeText.Value }

    let private hashText =
        loadResourceText "Kappa.Compiler.Stdlib.std.hash.kp"

    let private hashModule =
        { ModuleName = [ "std"; "hash" ]
          VirtualPath = Path.Combine(Path.GetFullPath("__kappa_stdlib__"), "std", "hash.kp")
          Types = [ "HashSeed"; "HashState"; "HashCode" ]
          Terms =
            [ { Name = "defaultHashSeed"; TypeText = "HashSeed" }
              { Name = "newHashState"; TypeText = "HashSeed -> HashState" }
              { Name = "finishHashState"; TypeText = "(1 state : HashState) -> HashCode" }
              { Name = "hashUnit"; TypeText = "(1 state : HashState) -> HashState" }
              { Name = "hashBool"; TypeText = "Bool -> (1 state : HashState) -> HashState" }
              { Name = "hashUnicodeScalar"; TypeText = "UnicodeScalar -> (1 state : HashState) -> HashState" }
              { Name = "hashGrapheme"; TypeText = "Grapheme -> (1 state : HashState) -> HashState" }
              { Name = "hashString"; TypeText = "String -> (1 state : HashState) -> HashState" }
              { Name = "hashBytes"; TypeText = "Bytes -> (1 state : HashState) -> HashState" }
              { Name = "hashInt"; TypeText = "Int -> (1 state : HashState) -> HashState" }
              { Name = "hashInteger"; TypeText = "Integer -> (1 state : HashState) -> HashState" }
              { Name = "hashFloatRaw"; TypeText = "Float -> (1 state : HashState) -> HashState" }
              { Name = "hashDoubleRaw"; TypeText = "Double -> (1 state : HashState) -> HashState" }
              { Name = "hashNatTag"; TypeText = "Nat -> (1 state : HashState) -> HashState" }
              { Name = "hashField"
                TypeText = "forall (a : Type). (@_ : Hashable a) -> (& value : a) -> (1 state : HashState) -> HashState" }
              { Name = "hashWith"
                TypeText = "forall (a : Type). (@_ : Hashable a) -> HashSeed -> (& value : a) -> HashCode" } ]
          Traits = [ { Name = "Hashable"; TypeParameterCount = 1 } ]
          IntrinsicTermNames =
            Set.ofList
                [ "defaultHashSeed"
                  "newHashState"
                  "finishHashState"
                  "hashUnit"
                  "hashBool"
                  "hashUnicodeScalar"
                  "hashGrapheme"
                  "hashString"
                  "hashBytes"
                  "hashInt"
                  "hashInteger"
                  "hashFloatRaw"
                  "hashDoubleRaw"
                  "hashNatTag" ]
          LoadText = fun () -> hashText.Value }

    let all =
        [ derivingShapeModule
          hashModule ]

    let tryFind moduleName =
        all |> List.tryFind (fun bundled -> bundled.ModuleName = moduleName)

    let tryFindText moduleName =
        all
        |> List.tryFind (fun bundled -> SyntaxFacts.moduleNameToText bundled.ModuleName = moduleName)

    let tryIntrinsicTermNames moduleName =
        tryFind moduleName |> Option.map _.IntrinsicTermNames

    let tryIntrinsicTermNamesText moduleName =
        tryFindText moduleName |> Option.map _.IntrinsicTermNames

    let tryTermNames moduleName =
        tryFindText moduleName |> Option.map (fun bundled -> bundled.Terms |> List.map (fun term -> term.Name) |> Set.ofList)

    let tryTypeNames moduleName =
        tryFindText moduleName |> Option.map (fun bundled -> bundled.Types |> Set.ofList)

    let tryTraitNames moduleName =
        tryFindText moduleName |> Option.map (fun bundled -> bundled.Traits |> List.map (fun traitInfo -> traitInfo.Name) |> Set.ofList)

    let tryTermTypeText moduleName termName =
        tryFindText moduleName
        |> Option.bind (fun bundled ->
            bundled.Terms
            |> List.tryFind (fun term -> String.Equals(term.Name, termName, StringComparison.Ordinal))
            |> Option.map (fun term -> term.TypeText))
