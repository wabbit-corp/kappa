namespace Kappa.Compiler

open System
open TypeSignatures

type internal BundledPreludeExpectContract =
    { TypeNames: Set<string>
      TraitNames: Set<string>
      TermNames: Set<string> }

// Centralizes intrinsic names, operator classes, and backend-visible builtin metadata.
module internal IntrinsicCatalog =
    type BuiltinPreludeTraitMemberLowering =
        | ForwardToBuiltinBinaryOperator of string
        | ForwardToIntrinsicTerm of string

    type BuiltinPreludeTraitInstanceSpec =
        { TraitName: string
          HeadTypeText: string
          MemberLowerings: (string * BuiltinPreludeTraitMemberLowering) list }

    let private aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let private parseBundledPreludeDeclarations () =
        let source = SourceText.From(BundledPrelude.virtualPath, BundledPrelude.loadText ())
        let lexed = Lexer.tokenize source

        if not (List.isEmpty lexed.Diagnostics) then
            invalidOp
                $"Bundled prelude failed to lex for intrinsic catalog extraction:{Environment.NewLine}{aggregateDiagnostics lexed.Diagnostics}"

        let parsed = Parser.parseWithInitialFixities FixityTable.empty source lexed.Tokens

        if not (List.isEmpty parsed.Diagnostics) then
            invalidOp
                $"Bundled prelude failed to parse for intrinsic catalog extraction:{Environment.NewLine}{aggregateDiagnostics parsed.Diagnostics}"

        parsed.Syntax.Declarations

    let private bundledPreludeExpectContractValue =
        lazy
            (let declarations = parseBundledPreludeDeclarations ()

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

    let intrinsicRuntimeArity name =
        match name with
        | "True"
        | "False" ->
            0
        | "not"
        | "negate"
        | "pure"
        | "print"
        | "println"
        | "primitiveIntToString"
        | "closeCode"
        | "genlet"
        | "runCode"
        | "unsafeConsume"
        | "openFile"
        | "primitiveReadData"
        | "readData"
        | "primitiveCloseFile"
        | "print"
        | "println"
        | "newRef"
        | "readRef"
        | "unicodeVersion"
        | "defaultHashSeed"
        | "NFC"
        | "NFD"
        | "NFKC"
        | "NFKD"
        | "utf8Bytes"
        | "decodeUtf8"
        | "decodeUtf8Lossy"
        | "byteLength"
        | "scalarCount"
        | "graphemeCount"
        | "scalars"
        | "graphemes"
        | "words"
        | "sentences"
        | "scalarValue"
        | "unicodeScalarFromValue"
        | "scalarToString"
        | "graphemeToString"
        | "graphemeFromString"
        | "show"
        | BuiltinPreludeShowIntrinsicName
        | "newHashState"
        | "finishHashState" ->
            1
        | "and"
        | "or"
        | "writeRef"
        | ">>="
        | "compare"
        | BuiltinPreludeCompareIntrinsicName
        | "normalize"
        | "isNormalized"
        | "canonicalEquivalent"
        | "hashBool"
        | "hashUnicodeScalar"
        | "hashGrapheme"
        | "hashString"
        | "hashBytes"
        | "hashInt"
        | "hashInteger"
        | "hashFloatRaw"
        | "hashDoubleRaw"
        | "hashNatTag" ->
            2
        | "hashUnit" ->
            1
        | _ when isBuiltinBinaryOperator name ->
            2
        | _ ->
            0

    let intrinsicResultRepresentation name =
        match name with
        | "True"
        | "False"
        | "not"
        | "and"
        | "or"
        | _ when booleanResultBinaryOperatorNames.Contains name ->
            Some BackendRepBoolean
        | "primitiveIntToString" ->
            Some BackendRepString
        | "compare"
        | BuiltinPreludeCompareIntrinsicName ->
            Some(BackendRepOpaque(Some "Ordering"))
        | "utf8Bytes" ->
            Some(BackendRepOpaque(Some "Bytes"))
        | "decodeUtf8" ->
            Some(BackendRepOpaque(Some "Result"))
        | "decodeUtf8Lossy" ->
            Some BackendRepString
        | "byteLength"
        | "scalarCount"
        | "graphemeCount"
        | "scalarValue" ->
            Some BackendRepInt64
        | "scalars"
        | "graphemes"
        | "words"
        | "sentences" ->
            Some(BackendRepOpaque(Some "Query"))
        | "unicodeScalarFromValue"
        | "graphemeFromString" ->
            Some(BackendRepOpaque(Some "Option"))
        | "scalarToString"
        | "graphemeToString"
        | "show"
        | BuiltinPreludeShowIntrinsicName
        | "normalize" ->
            Some BackendRepString
        | "isNormalized"
        | "canonicalEquivalent" ->
            Some BackendRepBoolean
        | "unicodeVersion" ->
            Some(BackendRepOpaque(Some "UnicodeVersion"))
        | "NFC"
        | "NFD"
        | "NFKC"
        | "NFKD" ->
            Some(BackendRepOpaque(Some "NormalizationForm"))
        | "defaultHashSeed" ->
            Some(BackendRepOpaque(Some "HashSeed"))
        | "newHashState"
        | "hashUnit"
        | "hashBool"
        | "hashUnicodeScalar"
        | "hashGrapheme"
        | "hashString"
        | "hashBytes"
        | "hashInt"
        | "hashInteger"
        | "hashFloatRaw"
        | "hashDoubleRaw"
        | "hashNatTag" ->
            Some(BackendRepOpaque(Some "HashState"))
        | "finishHashState" ->
            Some(BackendRepOpaque(Some "HashCode"))
        | "unsafeConsume" ->
            Some BackendRepUnit
        | "openFile"
        | "primitiveReadData"
        | "readData"
        | "primitiveCloseFile"
        | "newRef"
        | "readRef"
        | "writeRef"
        | "pure"
        | ">>=" ->
            Some BackendRepIOAction
        | _ ->
            None

    let executedIntrinsicResultRepresentation name =
        match name with
        | "writeRef" ->
            Some BackendRepUnit
        | "primitiveIntToString" ->
            Some BackendRepString
        | "unsafeConsume" ->
            Some BackendRepUnit
        | "openFile" ->
            Some(BackendRepOpaque(Some "File"))
        | "primitiveReadData"
        | "readData" ->
            Some BackendRepString
        | "primitiveCloseFile" ->
            Some BackendRepUnit
        | "newRef" ->
            Some(BackendRepOpaque(Some "Ref"))
        | "readRef" ->
            Some(BackendRepOpaque None)
        | _ ->
            intrinsicResultRepresentation name
