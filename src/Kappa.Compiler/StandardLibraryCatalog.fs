namespace Kappa.Compiler

open System
open System.IO
open System.Reflection

// Canonical catalog of compiler-known standard-library modules, whether source-backed or synthetic.
module StandardLibraryCatalog =
    type StandardLibraryTermDescription =
        { Name: string
          TypeText: string }

    type StandardLibraryTraitMemberDescription =
        { Name: string
          TypeText: string }

    type StandardLibraryTraitDescription =
        { Name: string
          TypeParameterCount: int
          Members: StandardLibraryTraitMemberDescription list }

    type StandardLibrarySurface =
        { ModuleIdentity: ModuleIdentity
          Types: string list
          Terms: StandardLibraryTermDescription list
          Traits: StandardLibraryTraitDescription list
          IntrinsicTermNames: Set<string> }

    type SourceBackedStandardLibraryModule =
        { Surface: StandardLibrarySurface
          VirtualPath: string
          LoadText: unit -> string }

    type SyntheticStandardLibraryModule =
        { Surface: StandardLibrarySurface }

    type StandardLibraryModule =
        | SourceBacked of SourceBackedStandardLibraryModule
        | Synthetic of SyntheticStandardLibraryModule

    let private preludeModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Prelude
    let private hashModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Hash
    let private unicodeModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Unicode
    let private bytesModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Bytes
    let private testingModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Testing
    let private derivingShapeModuleIdentity = ModuleIdentity.ofSegments [ "std"; "deriving"; "shape" ]

    let private loadResourceText resourceName =
        lazy
            (let assembly = Assembly.GetExecutingAssembly()
             use stream = assembly.GetManifestResourceStream(resourceName)

             if isNull stream then
                 invalidOp $"Could not load bundled stdlib resource '{resourceName}'."

             use reader = new StreamReader(stream)
             reader.ReadToEnd().Replace("\r\n", "\n"))

    let private sourceBackedModule
        moduleIdentity
        resourceName
        relativePath
        (types: string list)
        (terms: StandardLibraryTermDescription list)
        (traits: StandardLibraryTraitDescription list)
        intrinsicTermNames
        =
        let text = loadResourceText resourceName

        SourceBacked
            { Surface =
                { ModuleIdentity = moduleIdentity
                  Types = types
                  Terms = terms
                  Traits = traits
                  IntrinsicTermNames = intrinsicTermNames }
              VirtualPath = Path.Combine(Path.GetFullPath("__kappa_stdlib__"), relativePath)
              LoadText = fun () -> text.Value }

    let private syntheticModule
        moduleIdentity
        (types: string list)
        (terms: StandardLibraryTermDescription list)
        (traits: StandardLibraryTraitDescription list)
        intrinsicTermNames
        =
        let effectiveIntrinsicTermNames =
            if Set.isEmpty intrinsicTermNames then
                terms |> List.map (fun term -> term.Name) |> Set.ofList
            else
                intrinsicTermNames

        Synthetic
            { Surface =
                { ModuleIdentity = moduleIdentity
                  Types = types
                  Terms = terms
                  Traits = traits
                  IntrinsicTermNames = effectiveIntrinsicTermNames } }

    let Prelude =
        sourceBackedModule
            preludeModuleIdentity
            "Kappa.Compiler.Stdlib.std.prelude.kp"
            (Path.Combine("std", "prelude.kp"))
            []
            []
            []
            Set.empty

    let private derivingShapeModule =
        sourceBackedModule
            derivingShapeModuleIdentity
            "Kappa.Compiler.Stdlib.std.deriving.shape.kp"
            (Path.Combine("std", "deriving", "shape.kp"))
            []
            []
            []
            (Set.ofList
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
                  "unitSyntax" ])

    let private hashModule =
        sourceBackedModule
            hashModuleIdentity
            "Kappa.Compiler.Stdlib.std.hash.kp"
            (Path.Combine("std", "hash.kp"))
            [ CompilerKnownSymbols.KnownTypeNames.HashCode
              "HashSeed"
              "HashState" ]
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
            [ { Name = "Hashable"
                TypeParameterCount = 1
                Members = [] } ]
            (Set.ofList
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
                  "hashNatTag" ])

    let private unicodeModule =
        syntheticModule
            unicodeModuleIdentity
            [ "UnicodeVersion"
              "NormalizationForm"
              "UnicodeDecodeError" ]
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
            []
            Set.empty

    let private bytesModule =
        syntheticModule
            bytesModuleIdentity
            [ "BytesBuilder" ]
            []
            []
            Set.empty

    let private testingModule =
        syntheticModule
            testingModuleIdentity
            []
            [ { Name = "failNow"; TypeText = "forall (a : Type). String -> a" } ]
            []
            Set.empty

    let all =
        [ Prelude
          derivingShapeModule
          hashModule
          unicodeModule
          bytesModule
          testingModule ]

    let surface moduleInfo =
        match moduleInfo with
        | SourceBacked moduleInfo -> moduleInfo.Surface
        | Synthetic moduleInfo -> moduleInfo.Surface

    let moduleIdentity moduleInfo =
        moduleInfo |> surface |> _.ModuleIdentity

    let moduleName moduleInfo =
        moduleInfo |> moduleIdentity |> ModuleIdentity.segments

    let moduleText moduleInfo =
        moduleInfo |> moduleIdentity |> ModuleIdentity.text

    let isSourceBacked moduleInfo =
        match moduleInfo with
        | SourceBacked _ -> true
        | Synthetic _ -> false

    let isSynthetic moduleInfo =
        moduleInfo |> isSourceBacked |> not

    let trySourceBacked moduleInfo =
        match moduleInfo with
        | SourceBacked moduleInfo -> Some moduleInfo
        | Synthetic _ -> None

    let sourceBackedModules =
        all |> List.choose trySourceBacked

    let syntheticModules =
        all
        |> List.choose (function
            | Synthetic moduleInfo -> Some moduleInfo
            | SourceBacked _ -> None)

    let byIdentity =
        all |> List.map (fun moduleInfo -> moduleIdentity moduleInfo, moduleInfo) |> Map.ofList

    let byText =
        all |> List.map (fun moduleInfo -> moduleText moduleInfo, moduleInfo) |> Map.ofList

    let tryFindByIdentity moduleIdentity =
        byIdentity |> Map.tryFind moduleIdentity

    let tryFindBySegments moduleName =
        moduleName |> ModuleIdentity.ofSegments |> tryFindByIdentity

    let tryFindByText moduleName =
        byText |> Map.tryFind moduleName

    let private termsSet (surfaceInfo: StandardLibrarySurface) =
        surfaceInfo.Terms |> List.map (fun term -> term.Name) |> Set.ofList

    let private traitsSet (surfaceInfo: StandardLibrarySurface) =
        surfaceInfo.Traits |> List.map (fun traitInfo -> traitInfo.Name) |> Set.ofList

    let tryIntrinsicTermNames moduleName =
        tryFindBySegments moduleName |> Option.map (surface >> _.IntrinsicTermNames)

    let tryIntrinsicTermNamesText moduleName =
        tryFindByText moduleName |> Option.map (surface >> _.IntrinsicTermNames)

    let tryTermNames moduleName =
        tryFindBySegments moduleName |> Option.map (surface >> termsSet)

    let tryTermNamesText moduleName =
        tryFindByText moduleName |> Option.map (surface >> termsSet)

    let tryTypeNames moduleName =
        tryFindBySegments moduleName |> Option.map (surface >> _.Types >> Set.ofList)

    let tryTypeNamesText moduleName =
        tryFindByText moduleName |> Option.map (surface >> _.Types >> Set.ofList)

    let tryTraitNames moduleName =
        tryFindBySegments moduleName |> Option.map (surface >> traitsSet)

    let tryTraitNamesText moduleName =
        tryFindByText moduleName |> Option.map (surface >> traitsSet)

    let tryTermTypeText moduleName termName =
        tryFindBySegments moduleName
        |> Option.bind (fun moduleInfo ->
            moduleInfo
            |> surface
            |> _.Terms
            |> List.tryFind (fun term -> String.Equals(term.Name, termName, StringComparison.Ordinal))
            |> Option.map (fun term -> term.TypeText))

    let tryTermTypeTextText moduleName termName =
        tryFindByText moduleName
        |> Option.bind (fun moduleInfo ->
            moduleInfo
            |> surface
            |> _.Terms
            |> List.tryFind (fun term -> String.Equals(term.Name, termName, StringComparison.Ordinal))
            |> Option.map (fun term -> term.TypeText))

    let preludeVirtualPath =
        match Prelude with
        | SourceBacked moduleInfo -> moduleInfo.VirtualPath
        | Synthetic _ -> invalidOp "Prelude must be source-backed."

    let loadPreludeText () =
        match Prelude with
        | SourceBacked moduleInfo -> moduleInfo.LoadText ()
        | Synthetic _ -> invalidOp "Prelude must be source-backed."

    let toRuntimeModule (moduleInfo: SyntheticStandardLibraryModule) : KRuntimeModule =
        let surfaceInfo = moduleInfo.Surface
        let moduleNameText = ModuleIdentity.text surfaceInfo.ModuleIdentity

        { Name = moduleNameText
          SourceFile = $"<std:{moduleNameText}>"
          Imports = []
          Exports = surfaceInfo.Terms |> List.map (fun term -> term.Name)
          IntrinsicTerms = surfaceInfo.Terms |> List.map (fun term -> term.Name)
          DataTypes =
            surfaceInfo.Types
            |> List.map (fun typeName ->
                { Name = typeName
                  TypeParameters = []
                  Constructors = []
                  ExternalRuntimeTypeName = None })
          Traits =
            surfaceInfo.Traits
            |> List.map (fun traitInfo ->
                { Name = traitInfo.Name
                  TypeParameterCount = traitInfo.TypeParameterCount })
          TraitInstances = []
          Constructors = []
          Bindings = [] }
