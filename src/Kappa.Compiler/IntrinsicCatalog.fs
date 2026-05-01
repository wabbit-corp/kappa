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

    let private aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let private trimSignificantTokens (tokens: Token list) =
        tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)

    let private splitTopLevelArrows (tokens: Token list) =
        let rec loop depth current remaining segments =
            match remaining with
            | [] ->
                List.rev ((List.rev current) :: segments)
            | token :: tail when token.Kind = LeftParen ->
                loop (depth + 1) (token :: current) tail segments
            | token :: tail when token.Kind = RightParen ->
                loop (max 0 (depth - 1)) (token :: current) tail segments
            | token :: tail when token.Kind = Arrow && depth = 0 ->
                loop depth [] tail ((List.rev current) :: segments)
            | token :: tail ->
                loop depth (token :: current) tail segments

        loop 0 [] tokens []

    let private bindingSignatureBodyTokens (tokens: Token list) =
        let tokenArray = trimSignificantTokens tokens |> List.toArray

        let mutable parenDepth = 0
        let mutable braceDepth = 0
        let mutable bracketDepth = 0
        let mutable bodyStart = 0

        for index = 0 to tokenArray.Length - 1 do
            match tokenArray[index].Kind with
            | LeftParen -> parenDepth <- parenDepth + 1
            | RightParen -> parenDepth <- max 0 (parenDepth - 1)
            | LeftBrace -> braceDepth <- braceDepth + 1
            | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
            | LeftBracket -> bracketDepth <- bracketDepth + 1
            | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
            | Operator when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 && String.Equals(tokenArray[index].Text, "=>", StringComparison.Ordinal) ->
                bodyStart <- index + 1
            | Equals when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 && index + 1 < tokenArray.Length ->
                match tokenArray[index + 1] with
                | { Kind = Operator; Text = ">" } ->
                    bodyStart <- index + 2
                | _ ->
                    ()
            | _ ->
                ()

        let stripLeadingForall startIndex =
            if startIndex >= tokenArray.Length then
                startIndex
            else
                match tokenArray[startIndex] with
                | token when Token.isKeyword Keyword.Forall token ->
                    let mutable index = startIndex + 1
                    let mutable parsed = true
                    let mutable foundDot = false
                    let mutable nextIndex = startIndex

                    while parsed && not foundDot && index < tokenArray.Length do
                        match tokenArray[index] with
                        | token when Token.isName token ->
                            index <- index + 1
                        | { Kind = LeftParen } ->
                            let mutable depth = 1
                            let mutable innerIndex = index + 1

                            while depth > 0 && innerIndex < tokenArray.Length do
                                match tokenArray[innerIndex].Kind with
                                | LeftParen -> depth <- depth + 1
                                | RightParen -> depth <- depth - 1
                                | _ -> ()

                                innerIndex <- innerIndex + 1

                            if depth = 0 then
                                index <- innerIndex
                            else
                                parsed <- false
                        | { Kind = Dot } ->
                            foundDot <- true
                            nextIndex <- index + 1
                        | _ ->
                            parsed <- false

                    if parsed && foundDot then nextIndex else startIndex
                | _ ->
                    startIndex

        let bodyStart = stripLeadingForall bodyStart
        List.ofArray tokenArray[bodyStart..]

    let private parameterHasErasedRuntimeQuantity (tokens: Token list) =
        let significant = trimSignificantTokens tokens

        let binderTokens =
            match significant with
            | { Kind = LeftParen } :: rest ->
                match List.rev rest with
                | { Kind = RightParen } :: reversedInner -> trimSignificantTokens (List.rev reversedInner)
                | _ -> significant
            | _ ->
                significant

        match binderTokens with
        | { Kind = AtSign } :: { Kind = IntegerLiteral; Text = "0" } :: _ -> true
        | { Kind = IntegerLiteral; Text = "0" } :: _ -> true
        | _ -> false

    let private runtimeArityFromTypeTokens tokens =
        let segments =
            tokens
            |> bindingSignatureBodyTokens
            |> splitTopLevelArrows
            |> List.filter (List.isEmpty >> not)

        if List.length segments <= 1 then
            0
        else
            segments
            |> List.take (segments.Length - 1)
            |> List.sumBy (fun segment -> if parameterHasErasedRuntimeQuantity segment then 0 else 1)

    let private parseSourceBackedDeclarations virtualPath loadText =
        let source = SourceText.From(virtualPath, loadText ())
        let lexed = Lexer.tokenize source

        if not (List.isEmpty lexed.Diagnostics) then
            invalidOp
                $"Bundled stdlib module '{virtualPath}' failed to lex for intrinsic catalog extraction:{Environment.NewLine}{aggregateDiagnostics lexed.Diagnostics}"

        let parsed = Parser.parseWithInitialFixities FixityTable.empty source lexed.Tokens

        if not (List.isEmpty parsed.Diagnostics) then
            invalidOp
                $"Bundled stdlib module '{virtualPath}' failed to parse for intrinsic catalog extraction:{Environment.NewLine}{aggregateDiagnostics parsed.Diagnostics}"

        parsed.Syntax.Declarations

    let private parseBundledPreludeDeclarations () =
        parseSourceBackedDeclarations StandardLibraryCatalog.preludeVirtualPath StandardLibraryCatalog.loadPreludeText

    let private tokenizeTypeText context typeText =
        let source = SourceText.From($"__intrinsic_catalog__.{context}.kp", typeText)
        let lexed = Lexer.tokenize source

        if not (List.isEmpty lexed.Diagnostics) then
            invalidOp
                $"Standard-library intrinsic type '{context}' failed to lex for intrinsic catalog extraction:{Environment.NewLine}{aggregateDiagnostics lexed.Diagnostics}"

        lexed.Tokens

    let private tryCatalogSurfaceIntrinsicSpec
        (intrinsicTermNames: Set<string>)
        (term: StandardLibraryCatalog.StandardLibraryTermDescription)
        =
        if intrinsicTermNames.Contains term.Name then
            Some(
                term.Name,
                { RuntimeArity = runtimeArityFromTypeTokens (tokenizeTypeText term.Name term.TypeText)
                  LoweredResultRepresentation = None
                  ExecutedResultRepresentation = None }
            )
        else
            None

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
             |> List.collect (function
                 | StandardLibraryCatalog.SourceBacked moduleInfo ->
                     let intrinsicTermNames = moduleInfo.Surface.IntrinsicTermNames
                     let catalogSurfaceTerms =
                         moduleInfo.Surface.Terms
                         |> List.choose (tryCatalogSurfaceIntrinsicSpec intrinsicTermNames)

                     if not (List.isEmpty catalogSurfaceTerms) then
                         catalogSurfaceTerms
                     else
                         parseSourceBackedDeclarations moduleInfo.VirtualPath moduleInfo.LoadText
                         |> List.choose (function
                             | ExpectDeclarationNode (ExpectTermDeclaration declaration)
                                 when Set.isEmpty intrinsicTermNames || intrinsicTermNames.Contains declaration.Name ->
                                 Some(
                                     declaration.Name,
                                     intrinsicSpec (runtimeArityFromTypeTokens declaration.TypeTokens) None None
                                 )
                             | _ -> None)
                 | StandardLibraryCatalog.Synthetic moduleInfo ->
                     let intrinsicTermNames = moduleInfo.Surface.IntrinsicTermNames

                     moduleInfo.Surface.Terms
                     |> List.choose (tryCatalogSurfaceIntrinsicSpec intrinsicTermNames))
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
