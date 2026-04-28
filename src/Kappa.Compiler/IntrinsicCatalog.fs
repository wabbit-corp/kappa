namespace Kappa.Compiler

open System

type internal BundledPreludeExpectContract =
    { TypeNames: Set<string>
      TraitNames: Set<string>
      TermNames: Set<string> }

// Centralizes intrinsic names, operator classes, and backend-visible builtin metadata.
module internal IntrinsicCatalog =
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
        Set.union contract.TermNames builtinBinaryOperatorNames

    let namedIntrinsicTermNames () =
        let contract = bundledPreludeExpectContract ()
        Set.union contract.TermNames moduleLocalIntrinsicTermNames

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
        | "printlnString"
        | "printInt"
        | "printString"
        | "primitiveIntToString"
        | "closeCode"
        | "genlet"
        | "runCode"
        | "unsafeConsume"
        | "openFile"
        | "primitiveReadData"
        | "readData"
        | "primitiveCloseFile"
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
        | "newHashState"
        | "finishHashState" ->
            1
        | "and"
        | "or"
        | "writeRef"
        | ">>="
        | ">>"
        | "compare"
        | "normalize"
        | "isNormalized"
        | "canonicalEquivalent"
        | "hashWith"
        | "hashBool"
        | "hashChar"
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
        | "hashField" ->
            2
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
        | "compare" ->
            Some(BackendRepOpaque(Some "Ordering"))
        | "printlnString" ->
            Some BackendRepIOAction
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
        | "hashChar"
        | "hashString"
        | "hashBytes"
        | "hashInt"
        | "hashInteger"
        | "hashFloatRaw"
        | "hashDoubleRaw"
        | "hashNatTag"
        | "hashField" ->
            Some(BackendRepOpaque(Some "HashState"))
        | "finishHashState"
        | "hashWith" ->
            Some(BackendRepOpaque(Some "HashCode"))
        | "unsafeConsume" ->
            Some BackendRepUnit
        | "openFile"
        | "primitiveReadData"
        | "readData"
        | "primitiveCloseFile"
        | "print"
        | "println"
        | "printlnString"
        | "printInt"
        | "printString"
        | "newRef"
        | "readRef"
        | "writeRef"
        | "pure"
        | ">>="
        | ">>" ->
            Some BackendRepIOAction
        | _ ->
            None

    let executedIntrinsicResultRepresentation name =
        match name with
        | "print"
        | "println"
        | "printInt"
        | "printString"
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
