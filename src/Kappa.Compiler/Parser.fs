namespace Kappa.Compiler

open System
open System.Collections.Generic

type ParseResult =
    { Syntax: CompilationUnit
      Diagnostics: Diagnostic list }

type private ModifierState =
    { Visibility: Visibility option
      IsOpaque: bool }

type private TokenParser(tokens: Token list, source: SourceText) =
    let tokenArray = List.toArray tokens
    let diagnostics = DiagnosticBag()
    let mutable fixities = FixityTable.defaultPrelude
    let mutable position = 0

    member private _.Current =
        let index = min position (tokenArray.Length - 1)
        tokenArray[index]

    member private _.Peek(offset: int) =
        let index = min (position + offset) (tokenArray.Length - 1)
        tokenArray[index]

    member private this.Advance() =
        let current = this.Current

        if position < tokenArray.Length - 1 then
            position <- position + 1

        current

    member private this.SkipLayout() =
        while (match this.Current.Kind with
               | Newline
               | Dedent -> true
               | _ -> false) do
            this.Advance() |> ignore

    member private this.TryConsume(kind: TokenKind) =
        if this.Current.Kind = kind then
            Some(this.Advance())
        else
            None

    member private this.TryConsumeKeyword(keyword: Keyword) =
        if Token.isKeyword keyword this.Current then
            Some(this.Advance())
        else
            None

    member private this.Expect(kind: TokenKind, message: string) =
        if this.Current.Kind = kind then
            this.Advance()
        else
            diagnostics.AddError(message, source.GetLocation(this.Current.Span))
            { Kind = kind
              Text = ""
              Span = this.Current.Span }

    member private this.ExpectKeyword(keyword: Keyword, message: string) =
        if Token.isKeyword keyword this.Current then
            this.Advance()
        else
            diagnostics.AddError(message, source.GetLocation(this.Current.Span))
            { Kind = Keyword keyword
              Text = Keyword.toText keyword
              Span = this.Current.Span }

    member private this.ConsumeName(message: string) =
        if Token.isName this.Current then
            let token = this.Advance()
            SyntaxFacts.trimIdentifierQuotes token.Text
        else
            diagnostics.AddError(message, source.GetLocation(this.Current.Span))
            "<missing>"

    member private this.TryConsumeOperatorName() =
        if this.Current.Kind = LeftParen && this.Peek(1).Kind = Operator && this.Peek(2).Kind = RightParen then
            this.Advance() |> ignore
            let operatorName = this.Advance().Text
            this.Advance() |> ignore
            Some operatorName
        else
            None

    member private this.ConsumeTermBindingName(message: string) =
        if Token.isName this.Current then
            this.ConsumeName(message)
        else
            match this.TryConsumeOperatorName() with
            | Some operatorName -> operatorName
            | None ->
                diagnostics.AddError(message, source.GetLocation(this.Current.Span))
                "<missing>"

    member private this.TryConsumeTermBindingName() =
        if Token.isName this.Current then
            Some(this.ConsumeName("Expected a binding name."))
        else
            this.TryConsumeOperatorName()

    member private _.ParseConstructorNameAndArity(lineTokens: Token list) =
        let significantTokens =
            lineTokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> false
                | _ -> true)

        let name, argumentTokens =
            match significantTokens with
            | leftToken :: operatorToken :: rightToken :: rest
                when leftToken.Kind = LeftParen && operatorToken.Kind = Operator && rightToken.Kind = RightParen ->
                operatorToken.Text, rest
            | head :: rest when Token.isName head ->
                SyntaxFacts.trimIdentifierQuotes head.Text, rest
            | _ ->
                "<anonymous>", []

        let tokenArray = List.toArray argumentTokens
        let mutable arity = 0
        let mutable index = 0

        while index < tokenArray.Length do
            match tokenArray[index].Kind with
            | LeftParen ->
                arity <- arity + 1
                let mutable depth = 1
                index <- index + 1

                while index < tokenArray.Length && depth > 0 do
                    match tokenArray[index].Kind with
                    | LeftParen -> depth <- depth + 1
                    | RightParen -> depth <- depth - 1
                    | _ -> ()

                    index <- index + 1
            | RightParen ->
                index <- index + 1
            | _ ->
                arity <- arity + 1
                index <- index + 1

        name, arity

    member private this.IsSignatureStart() =
        (this.Current.Kind = Identifier || (match this.Current.Kind with | Keyword _ -> true | _ -> false))
        && this.Peek(1).Kind = Colon
        || (this.Current.Kind = LeftParen && this.Peek(1).Kind = Operator && this.Peek(2).Kind = RightParen && this.Peek(3).Kind = Colon)

    member private this.IsProbableTopLevelStart(token: Token, nextToken: Token option) =
        match token.Kind with
        | Keyword Keyword.Import
        | Keyword Keyword.Export
        | Keyword Keyword.Expect
        | Keyword Keyword.Let
        | Keyword Keyword.Data
        | Keyword Keyword.Type
        | Keyword Keyword.Trait
        | Keyword Keyword.Instance
        | Keyword Keyword.Infix
        | Keyword Keyword.Prefix
        | Keyword Keyword.Postfix
        | Keyword Keyword.Public
        | Keyword Keyword.Private
        | Keyword Keyword.Opaque -> true
        | Identifier
        | Keyword _ ->
            match nextToken with
            | Some next -> next.Kind = Colon
            | None -> false
        | _ -> false

    member private this.NextNonLayout(offset: int) =
        let mutable currentOffset = offset
        let mutable candidate = this.Peek(currentOffset)

        while (match candidate.Kind with
               | Newline
               | Indent
               | Dedent -> true
               | _ -> false) do
            currentOffset <- currentOffset + 1
            candidate <- this.Peek(currentOffset)

        candidate

    member private this.CollectUntilTopLevelBoundary() =
        let collected = ResizeArray<Token>()
        let mutable localIndents = 0
        let mutable keepCollecting = true

        while keepCollecting && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | Newline when localIndents = 0 ->
                let next = this.NextNonLayout(1)
                let afterNext = this.NextNonLayout(2)

                if this.Peek(1).Kind = Indent then
                    collected.Add(this.Advance())
                elif this.IsProbableTopLevelStart(next, Some afterNext) then
                    keepCollecting <- false
                else
                    collected.Add(this.Advance())
            | Indent ->
                localIndents <- localIndents + 1
                collected.Add(this.Advance())
            | Dedent when localIndents > 0 ->
                localIndents <- localIndents - 1
                collected.Add(this.Advance())

                if localIndents = 0 then
                    let next = this.NextNonLayout(0)
                    let afterNext = this.NextNonLayout(1)

                    if this.IsProbableTopLevelStart(next, Some afterNext) then
                        keepCollecting <- false
            | _ ->
                collected.Add(this.Advance())

        collected
        |> Seq.toList
        |> List.rev
        |> List.skipWhile (fun token -> token.Kind = Newline)
        |> List.rev

    member private this.ParseDottedName() =
        let segments = ResizeArray<string>()
        segments.Add(this.ConsumeName("Expected a module name."))

        while this.Current.Kind = Dot && Token.isName (this.Peek(1)) do
            this.Advance() |> ignore
            segments.Add(this.ConsumeName("Expected a module name segment."))

        List.ofSeq segments

    member private this.ParseNameList() =
        let items = ResizeArray<string>()
        this.Expect(LeftParen, "Expected '(' to start the list.") |> ignore

        while this.Current.Kind <> RightParen && this.Current.Kind <> EndOfFile do
            items.Add(this.ConsumeName("Expected a name in the list."))

            if this.TryConsume(Comma).IsNone && this.Current.Kind <> RightParen then
                diagnostics.AddError("Expected ',' between list items.", source.GetLocation(this.Current.Span))

        this.Expect(RightParen, "Expected ')' to close the list.") |> ignore
        List.ofSeq items

    member private this.ParseImportItem() =
        let modifiers = ResizeArray<ImportItemModifier>()

        let rec consumeModifiers () =
            match this.Current.Kind with
            | Keyword Keyword.Unhide ->
                modifiers.Add(Unhide)
                this.Advance() |> ignore
                consumeModifiers ()
            | Keyword Keyword.Clarify ->
                modifiers.Add(Clarify)
                this.Advance() |> ignore
                consumeModifiers ()
            | _ -> ()

        consumeModifiers ()

        let importNamespace =
            match this.Current.Kind with
            | Keyword Keyword.Term ->
                this.Advance() |> ignore
                Some ImportNamespace.Term
            | Keyword Keyword.Type ->
                this.Advance() |> ignore
                Some ImportNamespace.Type
            | Keyword Keyword.Trait ->
                this.Advance() |> ignore
                Some ImportNamespace.Trait
            | Keyword Keyword.Ctor ->
                this.Advance() |> ignore
                Some ImportNamespace.Constructor
            | _ ->
                None

        { Modifiers = List.ofSeq modifiers
          Namespace = importNamespace
          Name = this.ConsumeName("Expected an imported name.") }

    member private this.ParseImportItems() =
        let items = ResizeArray<ImportItem>()
        this.Expect(LeftParen, "Expected '(' to start the import item list.") |> ignore

        while this.Current.Kind <> RightParen && this.Current.Kind <> EndOfFile do
            items.Add(this.ParseImportItem())

            if this.TryConsume(Comma).IsNone && this.Current.Kind <> RightParen then
                diagnostics.AddError("Expected ',' between import items.", source.GetLocation(this.Current.Span))

        this.Expect(RightParen, "Expected ')' to close the import item list.") |> ignore
        List.ofSeq items

    member private this.ParseImportSpec() =
        let moduleSource =
            match this.Current.Kind with
            | StringLiteral ->
                let token = this.Advance()

                let value =
                    match SyntaxFacts.tryDecodeStringLiteral token.Text with
                    | Result.Ok decoded -> decoded
                    | Result.Error message ->
                        diagnostics.AddError(message, source.GetLocation(token.Span))
                        SyntaxFacts.trimStringQuotes token.Text

                Url value
            | _ ->
                Dotted(this.ParseDottedName())

        if this.TryConsumeKeyword(Keyword.As).IsSome then
            { Source = moduleSource
              Alias = Some(this.ConsumeName("Expected an alias after 'as'."))
              Selection = QualifiedOnly }
        elif this.TryConsume(Dot).IsSome then
            match this.Current.Kind with
            | Operator when this.Current.Text = "*" ->
                this.Advance() |> ignore

                let selection =
                    if this.TryConsumeKeyword(Keyword.Except).IsSome then
                        AllExcept(this.ParseNameList())
                    else
                        All

                { Source = moduleSource
                  Alias = None
                  Selection = selection }
            | LeftParen ->
                { Source = moduleSource
                  Alias = None
                  Selection = Items(this.ParseImportItems()) }
            | _ ->
                diagnostics.AddError("Expected '*' or '(...)' after '.'.", source.GetLocation(this.Current.Span))

                { Source = moduleSource
                  Alias = None
                  Selection = QualifiedOnly }
        else
            { Source = moduleSource
              Alias = None
              Selection = QualifiedOnly }

    member private this.ParseModifiers() =
        let mutable visibility = None
        let mutable isOpaque = false
        let mutable keepGoing = true

        while keepGoing do
            match this.Current.Kind with
            | Keyword Keyword.Public ->
                visibility <- Some Visibility.Public
                this.Advance() |> ignore
            | Keyword Keyword.Private ->
                visibility <- Some Visibility.Private
                this.Advance() |> ignore
            | Keyword Keyword.Opaque ->
                isOpaque <- true
                this.Advance() |> ignore
            | _ ->
                keepGoing <- false

        { Visibility = visibility
          IsOpaque = isOpaque }

    member private this.ParseSignature(modifiers: ModifierState) =
        let name = this.ConsumeTermBindingName("Expected a name in the signature declaration.")
        this.Expect(Colon, "Expected ':' in the signature declaration.") |> ignore

        SignatureDeclaration
            { Visibility = modifiers.Visibility
              IsOpaque = modifiers.IsOpaque
              Name = name
              TypeTokens = this.CollectUntilTopLevelBoundary() }

    member private this.ParseExpectDeclaration() =
        this.ExpectKeyword(Keyword.Expect, "Expected 'expect'.") |> ignore

        match this.Current.Kind with
        | Keyword Keyword.Type ->
            this.Advance() |> ignore
            let nameSpan = this.Current.Span
            let name = this.ConsumeName("Expected a type name after 'expect type'.")

            ExpectDeclarationNode
                (ExpectTypeDeclaration
                    { Name = name
                      HeaderTokens = this.CollectUntilTopLevelBoundary()
                      Span = nameSpan })
        | Keyword Keyword.Trait ->
            this.Advance() |> ignore
            let nameSpan = this.Current.Span
            let name = this.ConsumeName("Expected a trait name after 'expect trait'.")

            ExpectDeclarationNode
                (ExpectTraitDeclaration
                    { Name = name
                      HeaderTokens = this.CollectUntilTopLevelBoundary()
                      Span = nameSpan })
        | Keyword Keyword.Term ->
            this.Advance() |> ignore

            let currentSpan = this.Current.Span
            let name = this.ConsumeTermBindingName("Expected a term name after 'expect term'.")
            this.Expect(Colon, "Expected ':' in the expect term declaration.") |> ignore

            ExpectDeclarationNode
                (ExpectTermDeclaration
                    { Name = name
                      TypeTokens = this.CollectUntilTopLevelBoundary()
                      Span = currentSpan })
        | _ ->
            diagnostics.AddError(
                "Expected 'type', 'trait', or 'term' after 'expect'.",
                source.GetLocation(this.Current.Span)
            )

            ExpectDeclarationNode
                (ExpectTypeDeclaration
                    { Name = "<missing>"
                      HeaderTokens = []
                      Span = this.Current.Span })

    member private this.ParseFixityDeclaration() =
        let kind =
            match this.Current.Kind with
            | Keyword Keyword.Infix ->
                this.Advance() |> ignore

                let associativity =
                    match this.Current.Kind with
                    | Keyword Keyword.Left ->
                        this.Advance() |> ignore
                        LeftAssociative
                    | Keyword Keyword.Right ->
                        this.Advance() |> ignore
                        RightAssociative
                    | _ ->
                        NonAssociative

                Infix associativity
            | Keyword Keyword.Prefix ->
                this.Advance() |> ignore
                Prefix
            | Keyword Keyword.Postfix ->
                this.Advance() |> ignore
                Postfix
            | _ ->
                diagnostics.AddError("Expected a fixity declaration.", source.GetLocation(this.Current.Span))
                Infix NonAssociative

        let precedenceToken =
            this.Expect(IntegerLiteral, "Expected a numeric precedence in the fixity declaration.")

        let precedence =
            match Int32.TryParse(precedenceToken.Text) with
            | true, value -> value
            | _ ->
                diagnostics.AddError("Expected a valid integer precedence in the fixity declaration.", source.GetLocation(precedenceToken.Span))
                0

        this.Expect(LeftParen, "Expected '(' before the operator token in the fixity declaration.") |> ignore

        let operatorName =
            if this.Current.Kind = Operator then
                this.Advance().Text
            else
                diagnostics.AddError("Expected an operator token in the fixity declaration.", source.GetLocation(this.Current.Span))
                "<missing-operator>"

        this.Expect(RightParen, "Expected ')' after the operator token in the fixity declaration.") |> ignore

        let declaration =
            { Fixity = kind
              Precedence = precedence
              OperatorName = operatorName }

        fixities <- FixityTable.add declaration fixities
        FixityDeclarationNode declaration

    member private this.ParseLetDeclaration(modifiers: ModifierState) =
        this.ExpectKeyword(Keyword.Let, "Expected 'let'.") |> ignore

        let headerTokens = ResizeArray<Token>()

        let name = this.TryConsumeTermBindingName()

        while (this.Current.Kind <> Equals
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            headerTokens.Add(this.Advance())

        let bodyTokens =
            if this.TryConsume(Equals).IsSome then
                this.CollectUntilTopLevelBoundary()
            else
                diagnostics.AddError("Expected '=' in the let declaration.", source.GetLocation(this.Current.Span))
                []

        let parsedHeader = CoreParsing.parseLetHeader source diagnostics (List.ofSeq headerTokens)
        let parsedBody = CoreParsing.parseExpression fixities source diagnostics bodyTokens

        LetDeclaration
            { Visibility = modifiers.Visibility
              IsOpaque = modifiers.IsOpaque
              Name = name
              Parameters = parsedHeader.Parameters
              HeaderTokens = List.ofSeq headerTokens
              ReturnTypeTokens = parsedHeader.ReturnTypeTokens
              BodyTokens = bodyTokens
              Body = parsedBody }

    member private this.ParseIndentedLines() =
        let lines = ResizeArray<Token list>()

        if this.TryConsume(Newline).IsSome then
            match this.TryConsume(Indent) with
            | Some _ ->
                let currentLine = ResizeArray<Token>()
                let mutable nestedIndents = 0

                let flushLine () =
                    if currentLine.Count > 0 then
                        lines.Add(List.ofSeq currentLine)
                        currentLine.Clear()

                while not (this.Current.Kind = Dedent && nestedIndents = 0) && this.Current.Kind <> EndOfFile do
                    match this.Current.Kind with
                    | Newline when nestedIndents = 0 ->
                        flushLine ()
                        this.Advance() |> ignore
                    | Indent ->
                        nestedIndents <- nestedIndents + 1
                        currentLine.Add(this.Advance())
                    | Dedent when nestedIndents > 0 ->
                        nestedIndents <- nestedIndents - 1
                        currentLine.Add(this.Advance())
                    | _ ->
                        currentLine.Add(this.Advance())

                flushLine ()
                this.Expect(Dedent, "Expected the declaration block to dedent.") |> ignore
            | None ->
                diagnostics.AddError("Expected an indented block.", source.GetLocation(this.Current.Span))
        else
            let inlineBody = this.CollectUntilTopLevelBoundary()

            if not (List.isEmpty inlineBody) then
                lines.Add(inlineBody)

        List.ofSeq lines

    member private this.ParseDataDeclaration(modifiers: ModifierState) =
        this.ExpectKeyword(Keyword.Data, "Expected 'data'.") |> ignore
        let name = this.ConsumeName("Expected a data type name.")

        let headerTokens = ResizeArray<Token>()

        while (this.Current.Kind <> Equals
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            headerTokens.Add(this.Advance())

        let constructors: DataConstructor list =
            if this.TryConsume(Equals).IsSome then
                this.ParseIndentedLines()
                |> List.map (fun lineTokens ->
                    let constructorName, arity =
                        this.ParseConstructorNameAndArity(lineTokens)

                    ({ Name = constructorName
                       Tokens = lineTokens
                       Arity = arity }: DataConstructor))
            else
                diagnostics.AddError("Expected '=' in the data declaration.", source.GetLocation(this.Current.Span))
                []

        DataDeclarationNode
            { Visibility = modifiers.Visibility
              IsOpaque = modifiers.IsOpaque
              Name = name
              HeaderTokens = List.ofSeq headerTokens
              Constructors = constructors }

    member private this.ParseTypeAlias(modifiers: ModifierState) =
        this.ExpectKeyword(Keyword.Type, "Expected 'type'.") |> ignore
        let name = this.ConsumeName("Expected a type alias name.")

        let headerTokens = ResizeArray<Token>()

        while (this.Current.Kind <> Equals
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            headerTokens.Add(this.Advance())

        let bodyTokens =
            if this.TryConsume(Equals).IsSome then
                Some(this.CollectUntilTopLevelBoundary())
            else
                None

        TypeAliasNode
            { Visibility = modifiers.Visibility
              IsOpaque = modifiers.IsOpaque
              Name = name
              HeaderTokens = List.ofSeq headerTokens
              BodyTokens = bodyTokens }

    member private this.ParseTraitDeclaration(modifiers: ModifierState) =
        this.ExpectKeyword(Keyword.Trait, "Expected 'trait'.") |> ignore
        let name = this.ConsumeName("Expected a trait name.")

        let headerTokens = ResizeArray<Token>()

        while (this.Current.Kind <> Equals
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            headerTokens.Add(this.Advance())

        let members: TraitMember list =
            if this.TryConsume(Equals).IsSome then
                this.ParseIndentedLines()
                |> List.map (fun lineTokens ->
                    let memberName =
                        match lineTokens with
                        | head :: second :: _ when Token.isKeyword Keyword.Let head && Token.isName second ->
                            Some(SyntaxFacts.trimIdentifierQuotes second.Text)
                        | head :: _ when Token.isName head ->
                            Some(SyntaxFacts.trimIdentifierQuotes head.Text)
                        | _ ->
                            None

                    ({ Name = memberName
                       Tokens = lineTokens }: TraitMember))
            else
                diagnostics.AddError("Expected '=' in the trait declaration.", source.GetLocation(this.Current.Span))
                []

        TraitDeclarationNode
            { Visibility = modifiers.Visibility
              Name = name
              HeaderTokens = List.ofSeq headerTokens
              Members = members }

    member private this.ParseInstanceMember(lineTokens: Token list) =
        match lineTokens with
        | letToken :: rest when Token.isKeyword Keyword.Let letToken ->
            let memberTokens = ResizeArray<Token>(rest)
            let headerTokens = ResizeArray<Token>()

            let mutable position = 0

            let current () =
                if position < memberTokens.Count then
                    memberTokens[position]
                else
                    { Kind = EndOfFile
                      Text = ""
                      Span = letToken.Span }

            let advance () =
                let token = current ()

                if position < memberTokens.Count then
                    position <- position + 1

                token

            let tryConsume kind =
                if (current ()).Kind = kind then
                    Some(advance ())
                else
                    None

            let tryConsumeTermBindingName () =
                let token = current ()

                if Token.isName token then
                    advance () |> ignore
                    Some(SyntaxFacts.trimIdentifierQuotes token.Text)
                elif token.Kind = LeftParen && position + 2 < memberTokens.Count && memberTokens[position + 1].Kind = Operator && memberTokens[position + 2].Kind = RightParen then
                    advance () |> ignore
                    let operatorToken = advance ()
                    advance () |> ignore
                    Some operatorToken.Text
                else
                    None

            let name = tryConsumeTermBindingName ()

            while (current ()).Kind <> Equals
                   && (current ()).Kind <> Newline
                   && (current ()).Kind <> EndOfFile do
                headerTokens.Add(advance ())

            let bodyTokens =
                if tryConsume Equals |> Option.isSome then
                    [ while (current ()).Kind <> EndOfFile do
                          yield advance () ]
                else
                    diagnostics.AddError("Expected '=' in the instance member declaration.", source.GetLocation((current ()).Span))
                    []

            let parsedHeader = CoreParsing.parseLetHeader source diagnostics (List.ofSeq headerTokens)
            let parsedBody = CoreParsing.parseExpression fixities source diagnostics bodyTokens

            Some
                { Visibility = None
                  IsOpaque = false
                  Name = name
                  Parameters = parsedHeader.Parameters
                  HeaderTokens = List.ofSeq headerTokens
                  ReturnTypeTokens = parsedHeader.ReturnTypeTokens
                  BodyTokens = bodyTokens
                  Body = parsedBody }
        | _ ->
            diagnostics.AddError("Expected an instance member definition starting with 'let'.", source.GetLocation(this.Current.Span))
            None

    member private this.ParseInstanceDeclaration() =
        this.ExpectKeyword(Keyword.Instance, "Expected 'instance'.") |> ignore
        let traitName = this.ConsumeName("Expected a trait name in the instance head.")

        let headerTokens = ResizeArray<Token>()

        while (this.Current.Kind <> Equals
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            headerTokens.Add(this.Advance())

        let members =
            if this.TryConsume(Equals).IsSome then
                this.ParseIndentedLines()
                |> List.choose this.ParseInstanceMember
            else
                diagnostics.AddError("Expected '=' in the instance declaration.", source.GetLocation(this.Current.Span))
                []

        InstanceDeclarationNode
            { TraitName = traitName
              HeaderTokens = List.ofSeq headerTokens
              Members = members }

    member private this.ParseImportOrExport(isExport: bool) =
        let keyword = if isExport then Keyword.Export else Keyword.Import
        this.ExpectKeyword(keyword, $"Expected '{Keyword.toText keyword}'.") |> ignore

        let specs = ResizeArray<ImportSpec>()
        specs.Add(this.ParseImportSpec())

        while this.TryConsume(Comma).IsSome do
            specs.Add(this.ParseImportSpec())

        ImportDeclaration(isExport, List.ofSeq specs)

    member private this.ParseUnknownDeclaration() =
        UnknownDeclaration(this.CollectUntilTopLevelBoundary())

    member private this.ParseTopLevelDeclaration() =
        let modifiers = this.ParseModifiers()

        match this.Current.Kind with
        | Keyword Keyword.Import -> this.ParseImportOrExport(false)
        | Keyword Keyword.Export -> this.ParseImportOrExport(true)
        | Keyword Keyword.Expect -> this.ParseExpectDeclaration()
        | Keyword Keyword.Infix
        | Keyword Keyword.Prefix
        | Keyword Keyword.Postfix -> this.ParseFixityDeclaration()
        | Keyword Keyword.Let -> this.ParseLetDeclaration(modifiers)
        | Keyword Keyword.Data -> this.ParseDataDeclaration(modifiers)
        | Keyword Keyword.Type -> this.ParseTypeAlias(modifiers)
        | Keyword Keyword.Trait -> this.ParseTraitDeclaration(modifiers)
        | Keyword Keyword.Instance ->
            if modifiers.Visibility.IsSome || modifiers.IsOpaque then
                diagnostics.AddError("Visibility and opacity modifiers do not apply to instance declarations.", source.GetLocation(this.Current.Span))

            this.ParseInstanceDeclaration()
        | _ when this.IsSignatureStart() -> this.ParseSignature(modifiers)
        | _ -> this.ParseUnknownDeclaration()

    member this.ParseCompilationUnit() =
        this.SkipLayout()

        let moduleAttributes = ResizeArray<string>()

        while this.Current.Kind = AtSign do
            this.Advance() |> ignore
            moduleAttributes.Add(this.ConsumeName("Expected a module attribute name after '@'."))

        let moduleHeader =
            if Token.isKeyword Keyword.Module this.Current then
                this.Advance() |> ignore
                Some(this.ParseDottedName())
            else
                None

        let declarations = ResizeArray<TopLevelDeclaration>()

        while this.Current.Kind <> EndOfFile do
            this.SkipLayout()

            if this.Current.Kind <> EndOfFile then
                declarations.Add(this.ParseTopLevelDeclaration())

        { ModuleAttributes = List.ofSeq moduleAttributes
          ModuleHeader = moduleHeader
          Declarations = List.ofSeq declarations
          Tokens = List.ofArray tokenArray },
        diagnostics.Items

module Parser =
    let parse source tokens =
        let parser = TokenParser(tokens, source)
        let syntax, diagnostics = parser.ParseCompilationUnit()

        { Syntax = syntax
          Diagnostics = diagnostics }
