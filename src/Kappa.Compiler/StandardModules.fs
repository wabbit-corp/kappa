namespace Kappa.Compiler

open System

// Describes compiler-synthesized standard modules such as std.unicode and std.hash.
module internal StandardModules =
    type StandardTermDescription =
        { Name: string
          TypeText: string }

    type StandardTraitMemberDescription =
        { Name: string
          TypeText: string }

    type StandardTraitDescription =
        { Name: string
          TypeParameterCount: int
          Members: StandardTraitMemberDescription list }

    type StandardModuleDescription =
        { ModuleName: string
          Types: string list
          Terms: StandardTermDescription list
          Traits: StandardTraitDescription list }

    let private unicodeModule =
        { ModuleName = "std.unicode"
          Types =
            [ "UnicodeVersion"
              "NormalizationForm"
              "UnicodeDecodeError" ]
          Terms =
            [ { Name = "unicodeVersion"; TypeText = "UnicodeVersion" }
              { Name = "NFC"; TypeText = "NormalizationForm" }
              { Name = "NFD"; TypeText = "NormalizationForm" }
              { Name = "NFKC"; TypeText = "NormalizationForm" }
              { Name = "NFKD"; TypeText = "NormalizationForm" }
              { Name = "utf8Bytes"; TypeText = "String -> Bytes" }
              { Name = "decodeUtf8"; TypeText = "Bytes -> Result UnicodeDecodeError String" }
              { Name = "decodeUtf8Lossy"; TypeText = "Bytes -> String" }
              { Name = "byteLength"; TypeText = "String -> Nat" }
              { Name = "scalarCount"; TypeText = "String -> Nat" }
              { Name = "graphemeCount"; TypeText = "String -> Nat" }
              { Name = "scalars"; TypeText = "String -> Query UnicodeScalar" }
              { Name = "graphemes"; TypeText = "String -> Query Grapheme" }
              { Name = "words"; TypeText = "String -> Query String" }
              { Name = "sentences"; TypeText = "String -> Query String" }
              { Name = "scalarValue"; TypeText = "UnicodeScalar -> Nat" }
              { Name = "unicodeScalarFromValue"; TypeText = "Nat -> Option UnicodeScalar" }
              { Name = "scalarToString"; TypeText = "UnicodeScalar -> String" }
              { Name = "graphemeToString"; TypeText = "Grapheme -> String" }
              { Name = "graphemeFromString"; TypeText = "String -> Option Grapheme" }
              { Name = "normalize"; TypeText = "NormalizationForm -> String -> String" }
              { Name = "isNormalized"; TypeText = "NormalizationForm -> String -> Bool" }
              { Name = "canonicalEquivalent"; TypeText = "String -> String -> Bool" }
              { Name = "show"; TypeText = "a -> String" } ]
          Traits = [] }

    let private bytesModule =
        { ModuleName = "std.bytes"
          Types = [ "BytesBuilder" ]
          Terms = []
          Traits = [] }

    let private hashModule =
        { ModuleName = "std.hash"
          Types = [ "HashSeed"; "HashState"; "HashCode" ]
          Terms =
            [ { Name = "defaultHashSeed"; TypeText = "HashSeed" }
              { Name = "newHashState"; TypeText = "HashSeed -> HashState" }
              { Name = "finishHashState"; TypeText = "HashState -> HashCode" }
              { Name = "hashUnit"; TypeText = "HashSeed -> Unit -> HashCode" }
              { Name = "hashBool"; TypeText = "HashSeed -> Bool -> HashCode" }
              { Name = "hashChar"; TypeText = "HashSeed -> Char -> HashCode" }
              { Name = "hashString"; TypeText = "HashSeed -> String -> HashCode" }
              { Name = "hashBytes"; TypeText = "HashSeed -> Bytes -> HashCode" }
              { Name = "hashInt"; TypeText = "HashSeed -> Int -> HashCode" }
              { Name = "hashInteger"; TypeText = "HashSeed -> Integer -> HashCode" }
              { Name = "hashFloatRaw"; TypeText = "HashSeed -> Float -> HashCode" }
              { Name = "hashDoubleRaw"; TypeText = "HashSeed -> Double -> HashCode" }
              { Name = "hashNatTag"; TypeText = "HashSeed -> Nat -> HashCode" }
              { Name = "hashField"; TypeText = "HashState -> String -> HashCode -> HashState" }
              { Name = "hashWith"; TypeText = "HashSeed -> a -> HashCode" } ]
          Traits = [ { Name = "Hashable"; TypeParameterCount = 1; Members = [] } ] }

    let all =
        [ unicodeModule; bytesModule; hashModule ]

    let byName =
        all |> List.map (fun description -> description.ModuleName, description) |> Map.ofList

    let inventories =
        byName
        |> Map.map (fun _ description ->
            Set.ofList (description.Terms |> List.map (fun term -> term.Name)),
            Set.ofList description.Types,
            Set.ofList (description.Traits |> List.map (fun traitInfo -> traitInfo.Name)))

    let toRuntimeModule (description: StandardModuleDescription) =
        { Name = description.ModuleName
          SourceFile = $"<std:{description.ModuleName}>"
          Imports = []
          Exports = description.Terms |> List.map (fun term -> term.Name)
          IntrinsicTerms = description.Terms |> List.map (fun term -> term.Name)
          DataTypes =
            description.Types
            |> List.map (fun typeName ->
                { Name = typeName
                  TypeParameters = []
                  Constructors = []
                  ExternalRuntimeTypeName = None })
          Traits =
            description.Traits
            |> List.map (fun traitInfo ->
                { Name = traitInfo.Name
                  TypeParameterCount = traitInfo.TypeParameterCount })
          TraitInstances = []
          Constructors = []
          Bindings = [] }
