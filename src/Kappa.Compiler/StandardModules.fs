namespace Kappa.Compiler

open System

// Describes compiler-synthesized standard modules such as std.unicode and std.testing.
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
        { ModuleIdentity: ModuleIdentity
          Types: string list
          Terms: StandardTermDescription list
          Traits: StandardTraitDescription list }

    let private unicodeModule =
        { ModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Unicode
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
              { Name = "show"; TypeText = "forall (a : Type). a -> String" } ]
          Traits = [] }

    let private bytesModule =
        { ModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Bytes
          Types = [ "BytesBuilder" ]
          Terms = []
          Traits = [] }

    let private testingModule =
        { ModuleIdentity = ModuleIdentity.ofSegments [ "std"; "testing" ]
          Types = []
          Terms = [ { Name = "failNow"; TypeText = "forall (a : Type). String -> a" } ]
          Traits = [] }

    let all =
        [ unicodeModule; bytesModule; testingModule ]

    let byIdentity =
        all |> List.map (fun description -> description.ModuleIdentity, description) |> Map.ofList

    let byText =
        all
        |> List.map (fun description -> ModuleIdentity.text description.ModuleIdentity, description)
        |> Map.ofList

    let inventories =
        byIdentity
        |> Map.map (fun _ description ->
            Set.ofList (description.Terms |> List.map (fun term -> term.Name)),
            Set.ofList description.Types,
            Set.ofList (description.Traits |> List.map (fun traitInfo -> traitInfo.Name)))

    let toRuntimeModule (description: StandardModuleDescription) =
        let moduleNameText = ModuleIdentity.text description.ModuleIdentity

        { Name = moduleNameText
          SourceFile = $"<std:{moduleNameText}>"
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
