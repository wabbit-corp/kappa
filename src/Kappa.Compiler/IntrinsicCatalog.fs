namespace Kappa.Compiler

open System
open TypeSignatures

type internal BundledPreludeExpectContract =
    { TypeNames: Set<string>
      TraitNames: Set<string>
      TermNames: Set<string> }

// Centralizes intrinsic names, operator classes, and backend-visible builtin metadata.
module internal IntrinsicCatalog =
    type IntrinsicSpec =
        { RuntimeArity: int
          LoweredResultRepresentation: KBackendRepresentationClass option
          ExecutedResultRepresentation: KBackendRepresentationClass option }

    type BuiltinPreludeTraitMemberLowering =
        | ForwardToBuiltinBinaryOperator of string
        | ForwardToIntrinsicTerm of string

    type BuiltinPreludeTraitInstanceSpec =
        { TraitName: string
          HeadTypeText: string
          MemberLowerings: (string * BuiltinPreludeTraitMemberLowering) list }

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

    let builtinUnaryIntrinsicNames =
        Set.ofList [ "not"; "negate" ]

    let builtinBinaryOperatorNames =
        Set.ofList [ "+"; "-"; "*"; "/"; "=="; "!="; "<"; "<="; ">"; ">="; "&&"; "||"; "is" ]

    [<Literal>]
    let BuiltinPreludeShowIntrinsicName = "__kappa_builtin_show"

    [<Literal>]
    let BuiltinPreludeCompareIntrinsicName = "__kappa_builtin_compare"

    let private eqBuiltinPreludeHeadTypeTexts =
        [ "Unit"
          "Bool"
          "Byte"
          "Bytes"
          "UnicodeScalar"
          "Grapheme"
          "String"
          "Int"
          "Nat"
          "Integer"
          "Double"
          "Real"
          "Ordering"
          "HashCode" ]

    let private ordBuiltinPreludeHeadTypeTexts =
        [ "Bool"
          "Byte"
          "Bytes"
          "UnicodeScalar"
          "String"
          "Int"
          "Nat"
          "Integer"
          "Double"
          "Real"
          "Ordering"
          "HashCode" ]

    let private showBuiltinPreludeHeadTypeTexts =
        [ "Unit"
          "Bool"
          "Byte"
          "Bytes"
          "UnicodeScalar"
          "Grapheme"
          "String"
          "Int"
          "Nat"
          "Integer"
          "Double"
          "Real"
          "Ordering" ]

    let private rangeableBuiltinPreludeHeadTypeTexts =
        [ "Nat"
          "Int"
          "Integer"
          "UnicodeScalar" ]

    let private builtinPreludeTraitInstanceSpecsValue =
        lazy
            ([
                 for headTypeText in eqBuiltinPreludeHeadTypeTexts do
                     { TraitName = "Eq"
                       HeadTypeText = headTypeText
                       MemberLowerings = [ "==", ForwardToBuiltinBinaryOperator "==" ] }
                 for headTypeText in ordBuiltinPreludeHeadTypeTexts do
                     { TraitName = "Ord"
                       HeadTypeText = headTypeText
                       MemberLowerings = [ "compare", ForwardToIntrinsicTerm BuiltinPreludeCompareIntrinsicName ] }
                 for headTypeText in showBuiltinPreludeHeadTypeTexts do
                     { TraitName = "Show"
                       HeadTypeText = headTypeText
                       MemberLowerings = [ "show", ForwardToIntrinsicTerm BuiltinPreludeShowIntrinsicName ] }
             ]
             |> List.sortBy (fun spec -> spec.TraitName, spec.HeadTypeText))

    let builtinPreludeTraitInstanceSpecs () =
        builtinPreludeTraitInstanceSpecsValue.Value

    let private matchesBuiltinPreludeTypeName expectedName nameSegments =
        let actualName = SyntaxFacts.moduleNameToText nameSegments

        String.Equals(actualName, expectedName, StringComparison.Ordinal)
        || String.Equals(actualName, $"std.prelude.{expectedName}", StringComparison.Ordinal)

    let private matchesBuiltinStdHashTypeName expectedName nameSegments =
        let actualName = SyntaxFacts.moduleNameToText nameSegments

        String.Equals(actualName, expectedName, StringComparison.Ordinal)
        || String.Equals(actualName, $"std.hash.{expectedName}", StringComparison.Ordinal)

    let private tryCanonicalBuiltinPreludeHeadTypeText
        (canonicalNames: string list)
        (normalizedTypeExpr: TypeExpr)
        =
        match normalizedTypeExpr with
        | TypeName(nameSegments, []) ->
            canonicalNames
            |> List.tryFind (fun canonicalName ->
                matchesBuiltinPreludeTypeName canonicalName nameSegments
                || matchesBuiltinStdHashTypeName canonicalName nameSegments)
        | _ ->
            None

    let tryCanonicalBuiltinPreludeEqHeadTypeText normalizedTypeExpr =
        tryCanonicalBuiltinPreludeHeadTypeText eqBuiltinPreludeHeadTypeTexts normalizedTypeExpr

    let tryCanonicalBuiltinPreludeOrdHeadTypeText normalizedTypeExpr =
        tryCanonicalBuiltinPreludeHeadTypeText ordBuiltinPreludeHeadTypeTexts normalizedTypeExpr

    let tryCanonicalBuiltinPreludeShowHeadTypeText normalizedTypeExpr =
        tryCanonicalBuiltinPreludeHeadTypeText showBuiltinPreludeHeadTypeTexts normalizedTypeExpr

    let tryCanonicalBuiltinPreludeRangeableHeadTypeText normalizedTypeExpr =
        tryCanonicalBuiltinPreludeHeadTypeText rangeableBuiltinPreludeHeadTypeTexts normalizedTypeExpr

    let hiddenRuntimePreludeIntrinsicTermNames =
        builtinPreludeTraitInstanceSpecs ()
        |> List.collect (fun spec ->
            spec.MemberLowerings
            |> List.choose (fun (_, lowering) ->
                match lowering with
                | ForwardToIntrinsicTerm intrinsicName -> Some intrinsicName
                | ForwardToBuiltinBinaryOperator _ -> None))
        |> Set.ofList

    let shortCircuitBinaryOperatorNames =
        Set.ofList [ "&&"; "||" ]

    let eagerBuiltinBinaryOperatorNames =
        Set.difference builtinBinaryOperatorNames shortCircuitBinaryOperatorNames

    let private booleanResultBinaryOperatorNames =
        Set.ofList [ "&&"; "||"; "=="; "!="; "<"; "<="; ">"; ">="; "is" ]

    let private elaborationAvailablePreludeTermNames =
        Set.ofList
            [ "True"
              "False"
              "not"
              "and"
              "or"
              "negate"
              "f"
              "re"
              "b"
              "type"
              "failElab"
              "failElabWith"
              "warnElab"
              "warnElabWith" ]

    let elaborationAvailableIntrinsicTermNames () =
        let contract = bundledPreludeExpectContract ()

        Set.intersect contract.TermNames elaborationAvailablePreludeTermNames
        |> Set.union builtinBinaryOperatorNames

    let runtimePreludeIntrinsicTermNames () =
        let contract = bundledPreludeExpectContract ()
        Set.unionMany [ contract.TermNames; builtinBinaryOperatorNames; hiddenRuntimePreludeIntrinsicTermNames ]

    let namedIntrinsicTermNames () =
        let contract = bundledPreludeExpectContract ()
        Set.unionMany [ contract.TermNames; moduleLocalIntrinsicTermNames; hiddenRuntimePreludeIntrinsicTermNames ]

    let isBuiltinUnaryIntrinsic name =
        builtinUnaryIntrinsicNames.Contains name

    let isBuiltinBinaryOperator name =
        builtinBinaryOperatorNames.Contains name

    let isShortCircuitBinaryOperator name =
        shortCircuitBinaryOperatorNames.Contains name

    let isEagerBuiltinBinaryOperator name =
        eagerBuiltinBinaryOperatorNames.Contains name

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
                     "True", plainIntrinsicSpec 0 (Some BackendRepBoolean)
                     "False", plainIntrinsicSpec 0 (Some BackendRepBoolean)
                     "not", plainIntrinsicSpec 1 (Some BackendRepBoolean)
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
                     BuiltinPreludeShowIntrinsicName, plainIntrinsicSpec 1 (Some BackendRepString)
                     "newHashState", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "HashState")))
                     "finishHashState", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "HashCode")))
                     "hashUnit", plainIntrinsicSpec 1 (Some(BackendRepOpaque(Some "HashState")))
                     "and", plainIntrinsicSpec 2 (Some BackendRepBoolean)
                     "or", plainIntrinsicSpec 2 (Some BackendRepBoolean)
                     "writeRef", intrinsicSpec 2 (Some BackendRepIOAction) (Some BackendRepUnit)
                     ">>=", plainIntrinsicSpec 2 (Some BackendRepIOAction)
                     "compare", plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "Ordering")))
                     BuiltinPreludeCompareIntrinsicName, plainIntrinsicSpec 2 (Some(BackendRepOpaque(Some "Ordering")))
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

             let operatorSpecs =
                 builtinBinaryOperatorNames
                 |> Seq.map (fun name ->
                     let resultRepresentation =
                         if booleanResultBinaryOperatorNames.Contains name then
                             Some BackendRepBoolean
                         else
                             None

                     name, plainIntrinsicSpec 2 resultRepresentation)

             Seq.append specs operatorSpecs |> Map.ofSeq)

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
