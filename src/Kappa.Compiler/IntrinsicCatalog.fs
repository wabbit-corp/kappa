namespace Kappa.Compiler

open System
open TypeSignatures

type internal BundledPreludeExpectContract =
    { TypeNames: Set<string>
      TraitNames: Set<string>
      TermNames: Set<string> }

// Centralizes true compiler/runtime intrinsic contracts.
module internal IntrinsicCatalog =
    type IntrinsicSpec =
        { RuntimeArity: int
          LoweredResultRepresentation: KBackendRepresentationClass option
          ExecutedResultRepresentation: KBackendRepresentationClass option }

    let private bundledPreludeExpectContractValue =
        lazy
            (let declarations =
                 StandardLibrarySourceParsing.trySourceBackedDeclarations CompilerKnownSymbols.KnownModules.Prelude
                 |> Option.defaultWith (fun () -> invalidOp "Bundled prelude must be source-backed.")

             let collect chooser =
                 declarations
                 |> List.choose chooser
                 |> Set.ofList

             { TypeNames =
                 collect (function
                     | ExpectDeclarationNode (ExpectTypeDeclaration declaration) -> Some declaration.Name
                     | _ -> None)
               TraitNames =
                 collect (function
                     | ExpectDeclarationNode (ExpectTraitDeclaration declaration) -> Some declaration.Name
                     | _ -> None)
               TermNames =
                 collect (function
                     | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> Some declaration.Name
                     | _ -> None) })

    let bundledPreludeExpectContract () =
        bundledPreludeExpectContractValue.Value

    let bootstrapIntrinsicIdentity = "bootstrap-prelude-v2"

    let moduleLocalIntrinsicTermNames =
        Set.ofList [ "openFile"; "primitiveReadData"; "readData"; "primitiveCloseFile" ]

    let runtimePreludeIntrinsicTermNames () =
        let contract = bundledPreludeExpectContract ()
        Set.unionMany
            [ contract.TermNames
              KnownPreludeSemantics.runtimeSpecialBinaryOperatorNames
              KnownPreludeSemantics.hiddenRuntimeHelperTermNames ]

    let namedIntrinsicTermNames () =
        let contract = bundledPreludeExpectContract ()
        Set.unionMany
            [ contract.TermNames
              moduleLocalIntrinsicTermNames
              KnownPreludeSemantics.hiddenRuntimeHelperTermNames ]

    let elaborationAvailableIntrinsicTermNames () =
        let contract = bundledPreludeExpectContract ()
        KnownPreludeSemantics.elaborationAvailableTermNames contract.TermNames

    let private intrinsicSpec runtimeArity loweredResultRepresentation executedResultRepresentation =
        { RuntimeArity = runtimeArity
          LoweredResultRepresentation = loweredResultRepresentation
          ExecutedResultRepresentation = executedResultRepresentation }

    let private plainIntrinsicSpec runtimeArity resultRepresentation =
        intrinsicSpec runtimeArity resultRepresentation resultRepresentation

    let private intrinsicSpecsValue =
        lazy
            (let specs =
                 [
                     "negate", plainIntrinsicSpec 1 None
                     "pure", plainIntrinsicSpec 1 (Some BackendRepIOAction)
                     "print", plainIntrinsicSpec 1 None
                     "println", plainIntrinsicSpec 1 None
                     "primitiveIntToString", plainIntrinsicSpec 1 (Some BackendRepString)
                     "closeCode", plainIntrinsicSpec 1 None
                     "genlet", plainIntrinsicSpec 1 None
                     "runCode", plainIntrinsicSpec 1 None
                     "unsafeConsume", intrinsicSpec 1 (Some BackendRepUnit) (Some BackendRepUnit)
                     "openFile", intrinsicSpec 1 (Some BackendRepIOAction) (Some(BackendRepOpaque(Some "File")))
                     "primitiveReadData", intrinsicSpec 1 (Some BackendRepIOAction) (Some BackendRepString)
                     "readData", intrinsicSpec 1 (Some BackendRepIOAction) (Some BackendRepString)
                     "primitiveCloseFile", intrinsicSpec 1 (Some BackendRepIOAction) (Some BackendRepUnit)
                     "newRef", intrinsicSpec 1 (Some BackendRepIOAction) (Some(BackendRepOpaque(Some "Ref")))
                     "readRef", intrinsicSpec 1 (Some BackendRepIOAction) (Some(BackendRepOpaque None))
                     "unicodeVersion", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "UnicodeVersion")))
                     "defaultHashSeed", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "HashSeed")))
                     "NFC", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "NormalizationForm")))
                     "NFD", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "NormalizationForm")))
                     "NFKC", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "NormalizationForm")))
                     "NFKD", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "NormalizationForm")))
                     "utf8Bytes", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "Bytes")))
                     "decodeUtf8", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "Result")))
                     "decodeUtf8Lossy", plainIntrinsicSpec 1 (Some BackendRepString)
                     "byteLength", plainIntrinsicSpec 1 (Some BackendRepInt64)
                     "scalarCount", plainIntrinsicSpec 1 (Some BackendRepInt64)
                     "graphemeCount", plainIntrinsicSpec 1 (Some BackendRepInt64)
                     "scalars", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "Query")))
                     "graphemes", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "Query")))
                     "words", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "Query")))
                     "sentences", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "Query")))
                     "scalarValue", plainIntrinsicSpec 1 (Some BackendRepInt64)
                     "unicodeScalarFromValue", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "Option")))
                     "scalarToString", plainIntrinsicSpec 1 (Some BackendRepString)
                     "graphemeToString", plainIntrinsicSpec 1 (Some BackendRepString)
                     "graphemeFromString", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "Option")))
                     "show", plainIntrinsicSpec 1 (Some BackendRepString)
                     KnownPreludeSemantics.BuiltinPreludeShowHelperName, plainIntrinsicSpec 1 (Some BackendRepString)
                     "newHashState", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "HashState")))
                     "finishHashState", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "HashCode")))
                     "hashUnit", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "HashState")))
                     "writeRef", intrinsicSpec 2 (Some BackendRepIOAction) (Some BackendRepUnit)
                     ">>=", plainIntrinsicSpec 2 (Some BackendRepIOAction)
                     "compare", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "Ordering")))
                     KnownPreludeSemantics.BuiltinPreludeCompareHelperName, plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "Ordering")))
                     "normalize", plainIntrinsicSpec 2 (Some BackendRepString)
                     "isNormalized", plainIntrinsicSpec 2 (Some BackendRepBoolean)
                     "canonicalEquivalent", plainIntrinsicSpec 2 (Some BackendRepBoolean)
                     "hashBool", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "HashState")))
                     "hashUnicodeScalar", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "HashState")))
                     "hashGrapheme", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "HashState")))
                     "hashString", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "HashState")))
                     "hashBytes", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "HashState")))
                     "hashInt", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "HashState")))
                     "hashInteger", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "HashState")))
                     "hashFloatRaw", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "HashState")))
                     "hashDoubleRaw", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "HashState")))
                     "hashNatTag", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "HashState")))
                 ]

             specs |> Map.ofList)

    let private derivedStandardLibraryIntrinsicSpecsValue =
        lazy
            (StandardLibraryCatalog.all
             |> List.collect (fun moduleInfo ->
                 let moduleName = StandardLibraryCatalog.moduleName moduleInfo
                 let intrinsicTermNames =
                     StandardLibrarySourceParsing.tryEffectiveIntrinsicTermNames moduleName
                     |> Option.defaultValue Set.empty

                 intrinsicTermNames
                 |> Set.toList
                 |> List.choose (fun termName ->
                     StandardLibrarySourceParsing.tryIntrinsicTermTypeTokens moduleName termName
                     |> Option.map (fun typeTokens ->
                         termName,
                         intrinsicSpec (SignatureTokenAnalysis.runtimeArityFromTypeTokens typeTokens) None None)))
             |> Map.ofList)

    let tryFindIntrinsicSpec name =
        intrinsicSpecsValue.Value
        |> Map.tryFind name
        |> Option.orElseWith (fun () -> derivedStandardLibraryIntrinsicSpecsValue.Value |> Map.tryFind name)

    let intrinsicRuntimeArity name =
        match tryFindIntrinsicSpec name with
        | Some spec -> spec.RuntimeArity
        | None -> invalidArg (nameof name) $"Unknown intrinsic '{name}'."

    let intrinsicResultRepresentation name =
        tryFindIntrinsicSpec name
        |> Option.bind (fun spec -> spec.LoweredResultRepresentation)

    let executedIntrinsicResultRepresentation name =
        tryFindIntrinsicSpec name
        |> Option.bind (fun spec -> spec.ExecutedResultRepresentation)
