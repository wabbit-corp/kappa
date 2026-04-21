namespace Kappa.Compiler

open System

module ResourceChecking =
    let private linearDropCode = "E_QTT_LINEAR_DROP"
    let private linearOveruseCode = "E_QTT_LINEAR_OVERUSE"
    let private borrowEscapeCode = "E_QTT_BORROW_ESCAPE"

    type CheckResult =
        { Diagnostics: Diagnostic list
          OwnershipFactsByFile: Map<string, OwnershipFactSet> }

    type private FunctionSignature =
        { Name: string
          ParameterQuantities: Quantity option list }

    type private ResourcePlace =
        { Root: string
          Path: string list }

    type private BorrowRegion =
        { Id: string
          ExplicitName: string option
          OwnerScope: string }

    type private ResourceBinding =
        { Id: string
          Name: string
          DeclaredQuantity: Quantity option
          Place: ResourcePlace
          BorrowRegion: BorrowRegion option
          CapturedRegions: Set<string>
          CapturedBindingOrigins: SourceLocation list
          UseMinimum: int
          UseMaximum: int
          CheckLinearDrop: bool
          ClosureFactId: string option
          Origin: SourceLocation option
          FirstConsumeOrigin: SourceLocation option }

    type private CheckState =
        { ScopeId: string
          Bindings: Map<string, ResourceBinding>
          Diagnostics: Diagnostic list
          Events: OwnershipUseFact list
          BorrowRegions: OwnershipBorrowRegionFact list
          UsingScopes: OwnershipUsingScopeFact list
          Closures: OwnershipClosureFact list
          DeferredFacts: string list
          NextBindingId: int
          NextEventId: int
          NextRegionId: int
          NextUsingScopeId: int
          NextClosureId: int }

    let private emptyState scopeId =
        { ScopeId = scopeId
          Bindings = Map.empty
          Diagnostics = []
          Events = []
          BorrowRegions = []
          UsingScopes = []
          Closures = []
          DeferredFacts = []
          NextBindingId = 0
          NextEventId = 0
          NextRegionId = 0
          NextUsingScopeId = 0
          NextClosureId = 0 }

    let private diagnosticLocation (document: ParsedDocument) =
        document.Source.GetLocation(TextSpan.FromBounds(0, 0))

    let private tokenName (token: Token) =
        match token.Kind with
        | Identifier
        | Keyword _ ->
            Some(SyntaxFacts.trimIdentifierQuotes token.Text)
        | Operator ->
            Some token.Text
        | _ ->
            None

    let private tokenMatchesName name (token: Token) =
        tokenName token
        |> Option.exists (fun tokenName -> String.Equals(tokenName, name, StringComparison.Ordinal))

    let private tokenLocation (document: ParsedDocument) (token: Token) =
        document.Source.GetLocation(token.Span)

    let private tokenLine (document: ParsedDocument) (token: Token) =
        (tokenLocation document token).Start.Line

    let private tokensByLine (document: ParsedDocument) =
        document.Syntax.Tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)
        |> List.groupBy (tokenLine document)
        |> Map.ofList

    let private isAssignmentBoundary (token: Token) =
        token.Kind = Equals || (token.Kind = Operator && token.Text = "<-")

    let private binderPrefixTokens lineTokens =
        let rec takeUntilBoundary tokens collected =
            match tokens with
            | [] ->
                List.rev collected
            | token :: _ when isAssignmentBoundary token ->
                List.rev collected
            | token :: rest ->
                takeUntilBoundary rest (token :: collected)

        match lineTokens with
        | first :: _ when Token.isKeyword Keyword.Let first ->
            takeUntilBoundary lineTokens []
        | first :: _ when Token.isKeyword Keyword.Using first ->
            takeUntilBoundary lineTokens []
        | first :: _ when Token.isKeyword Keyword.Var first ->
            takeUntilBoundary lineTokens []
        | _ ->
            []

    let private isBinderToken lineMap (document: ParsedDocument) token =
        let lineTokens =
            lineMap
            |> Map.tryFind (tokenLine document token)
            |> Option.defaultValue []

        binderPrefixTokens lineTokens
        |> List.exists (fun candidate -> candidate.Span = token.Span)

    let private findBinderLocation (document: ParsedDocument) name =
        let lineMap = tokensByLine document

        document.Syntax.Tokens
        |> List.tryPick (fun token ->
            if tokenMatchesName name token && isBinderToken lineMap document token then
                Some(tokenLocation document token)
            else
                None)

    let private findUseLocation (document: ParsedDocument) name ordinal =
        let lineMap = tokensByLine document

        document.Syntax.Tokens
        |> List.choose (fun token ->
            if tokenMatchesName name token && not (isBinderToken lineMap document token) then
                Some(tokenLocation document token)
            else
                None)
        |> List.tryItem (max 0 (ordinal - 1))

    let private addDiagnostic code message location relatedLocations (document: ParsedDocument) (state: CheckState) =
        let diagnostic: Diagnostic =
            { Severity = Error
              Code = code
              Stage = Some "KFrontIR"
              Phase = Some(KFrontIRPhase.phaseName BODY_RESOLVE)
              Message = message
              Location = location |> Option.orElseWith (fun () -> Some(diagnosticLocation document))
              RelatedLocations = relatedLocations }

        { state with
            Diagnostics = state.Diagnostics @ [ diagnostic ] }

    let private quantityConsumes quantity =
        match quantity with
        | Some QuantityOne -> true
        | _ -> false

    let private quantityBorrows quantity =
        match quantity with
        | Some(QuantityBorrow _) -> true
        | _ -> false

    let private quantityText quantity =
        quantity |> Option.map Quantity.toSurfaceText

    let private bindingDemand binding =
        match binding.BorrowRegion with
        | Some _ -> "&"
        | None -> $"[{binding.UseMinimum},{binding.UseMaximum}]"

    let private bindingState binding =
        match binding.BorrowRegion with
        | Some _ -> "borrowed"
        | None when binding.UseMaximum > 0 -> "consumed"
        | None when binding.CheckLinearDrop -> "unconsumed"
        | None -> "available"

    let private bindingFact binding =
        { BindingId = binding.Id
          BindingName = binding.Name
          BindingKind = "local"
          BindingDeclaredQuantity = quantityText binding.DeclaredQuantity
          BindingInferredDemand = bindingDemand binding
          BindingState = bindingState binding
          BindingPlaceRoot = binding.Place.Root
          BindingPlacePath = binding.Place.Path
          BindingBorrowRegionId = binding.BorrowRegion |> Option.map (fun region -> region.Id)
          BindingOrigin = binding.Origin }

    let private addEvent useKind origin binding (state: CheckState) =
        let event: OwnershipUseFact =
            { UseId = $"{state.ScopeId}.u{state.NextEventId}"
              UseKindName = useKind
              UseTargetBindingId = Some binding.Id
              UseTargetName = binding.Name
              UsePlaceRoot = binding.Place.Root
              UsePlacePath = binding.Place.Path
              UseOrigin = origin }

        { state with
            Events = state.Events @ [ event ]
            NextEventId = state.NextEventId + 1 }

    let private addNamedEvent useKind origin name (state: CheckState) =
        match Map.tryFind name state.Bindings with
        | Some binding -> addEvent useKind origin binding state
        | None ->
            let event: OwnershipUseFact =
                { UseId = $"{state.ScopeId}.u{state.NextEventId}"
                  UseKindName = useKind
                  UseTargetBindingId = None
                  UseTargetName = name
                  UsePlaceRoot = name
                  UsePlacePath = []
                  UseOrigin = origin }

            { state with
                Events = state.Events @ [ event ]
                NextEventId = state.NextEventId + 1 }

    let private addBorrowRegionFact (region: BorrowRegion) state =
        let fact: OwnershipBorrowRegionFact =
            { BorrowRegionId = region.Id
              BorrowRegionExplicitName = region.ExplicitName
              BorrowRegionIntroductionOrigin = None
              BorrowRegionOwnerScope = region.OwnerScope }

        { state with
            BorrowRegions = state.BorrowRegions @ [ fact ] }

    let private addUsingScopeFact usingScopeId hiddenOwnedBinding (region: BorrowRegion) state =
        let fact: OwnershipUsingScopeFact =
            { UsingScopeId = usingScopeId
              UsingScopeSurfaceOrigin = None
              UsingScopeHiddenOwnedBinding = hiddenOwnedBinding
              UsingScopeSharedRegionId = region.Id
              UsingScopeHiddenReleaseObligation = "deferred" }

        { state with
            UsingScopes = state.UsingScopes @ [ fact ] }

    let private addDeferredFact fact state =
        if state.DeferredFacts |> List.contains fact then
            state
        else
            { state with DeferredFacts = state.DeferredFacts @ [ fact ] }

    let private addClosureFact name capturedBindings capturedRegions (state: CheckState) =
        let closureId = $"{state.ScopeId}.c{state.NextClosureId}"

        let fact: OwnershipClosureFact =
            { ClosureId = closureId
              ClosureName = name
              ClosureCaptureBindingIds = capturedBindings |> List.map (fun binding -> binding.Id) |> List.distinct |> List.sort
              ClosureCaptureNames = capturedBindings |> List.map (fun binding -> binding.Name) |> List.distinct |> List.sort
              ClosureRegionEnvironment = capturedRegions |> Set.toList |> List.sort
              ClosureEscapeStatus = "contained"
              ClosureOrigin = None }

        closureId,
        { state with
            Closures = state.Closures @ [ fact ]
            NextClosureId = state.NextClosureId + 1 }

    let private markClosureEscaped closureId state =
        let closures =
            state.Closures
            |> List.map (fun closure ->
                if String.Equals(closure.ClosureId, closureId, StringComparison.Ordinal) then
                    { closure with ClosureEscapeStatus = "escaped" }
                else
                    closure)

        { state with Closures = closures }

    let private collectPatternNames (pattern: CorePattern) =
        let rec loop current =
            seq {
                match current with
                | NamePattern name -> yield name
                | ConstructorPattern(_, arguments) ->
                    for argument in arguments do
                        yield! loop argument
                | WildcardPattern
                | LiteralPattern _ -> ()
            }

        loop pattern |> Seq.toList

    let rec private expressionNames (expression: CoreExpression) =
        seq {
            match expression with
            | Literal _ -> ()
            | Name [ name ] -> yield name
            | Name _ -> ()
            | Lambda(parameters, body) ->
                let parameterNames =
                    parameters
                    |> List.map (fun parameter -> parameter.Name)
                    |> Set.ofList

                for name in expressionNames body do
                    if not (Set.contains name parameterNames) then
                        yield name
            | IfThenElse(condition, whenTrue, whenFalse) ->
                yield! expressionNames condition
                yield! expressionNames whenTrue
                yield! expressionNames whenFalse
            | Match(scrutinee, cases) ->
                yield! expressionNames scrutinee

                for caseClause in cases do
                    yield! expressionNames caseClause.Body
            | Do statements ->
                for statement in statements do
                    yield! doStatementNames statement
            | MonadicSplice inner
            | InoutArgument inner
            | Unary(_, inner) ->
                yield! expressionNames inner
            | Apply(callee, arguments) ->
                yield! expressionNames callee

                for argument in arguments do
                    yield! expressionNames argument
            | Binary(left, _, right) ->
                yield! expressionNames left
                yield! expressionNames right
            | PrefixedString(_, parts) ->
                for part in parts do
                    match part with
                    | StringText _ -> ()
                    | StringInterpolation inner -> yield! expressionNames inner
        }

    and private doStatementNames (statement: DoStatement) =
        seq {
            match statement with
            | DoLet(_, expression)
            | DoBind(_, expression)
            | DoVar(_, expression)
            | DoAssign(_, expression)
            | DoUsing(_, expression)
            | DoExpression expression ->
                yield! expressionNames expression
            | DoWhile(condition, body) ->
                yield! expressionNames condition

                for statement in body do
                    yield! doStatementNames statement
        }

    let private capturedBindings (state: CheckState) expression =
        expressionNames expression
        |> Seq.choose (fun name -> Map.tryFind name state.Bindings)
        |> Seq.distinctBy (fun binding -> binding.Id)
        |> Seq.toList

    let private capturedRegions (state: CheckState) expression =
        capturedBindings state expression
        |> Seq.collect (fun binding ->
            seq {
                match binding.BorrowRegion with
                | Some region -> yield region.Id
                | None -> ()

                yield! binding.CapturedRegions
            })
        |> Set.ofSeq

    let private addBinding name quantity borrowRegion capturedRegions capturedBindingOrigins checkDrop closureFactId origin (state: CheckState) =
        let place: ResourcePlace =
            { Root = name
              Path = [] }

        let binding: ResourceBinding =
            { Id = $"{state.ScopeId}.b{state.NextBindingId}"
              Name = name
              DeclaredQuantity = quantity
              Place = place
              BorrowRegion = borrowRegion
              CapturedRegions = capturedRegions
              CapturedBindingOrigins = capturedBindingOrigins
              UseMinimum = 0
              UseMaximum = 0
              CheckLinearDrop = checkDrop
              ClosureFactId = closureFactId
              Origin = origin
              FirstConsumeOrigin = None }

        { state with
            Bindings = Map.add name binding state.Bindings
            NextBindingId = state.NextBindingId + 1 }

    let private addPatternBindings (document: ParsedDocument) (binding: BindPattern) quantity capturedRegions capturedBindingOrigins checkDrop closureFactId state =
        collectPatternNames binding.Pattern
        |> List.fold (fun current name ->
            addBinding
                name
                quantity
                None
                capturedRegions
                capturedBindingOrigins
                checkDrop
                closureFactId
                (findBinderLocation document name)
                current) state

    let private consumeBinding (document: ParsedDocument) name (state: CheckState) =
        match Map.tryFind name state.Bindings with
        | None -> state
        | Some binding ->
            let consumeOrigin = findUseLocation document name (binding.UseMaximum + 1)

            let state =
                if binding.UseMaximum >= 1 then
                    let relatedLocations =
                        [
                            match binding.FirstConsumeOrigin with
                            | Some location ->
                                { Message = "First consume of the linear resource."
                                  Location = location }
                            | None -> ()

                            match binding.Origin with
                            | Some location ->
                                { Message = "Linear resource binding."
                                  Location = location }
                            | None -> ()
                        ]

                    addDiagnostic
                        linearOveruseCode
                        $"Linear resource '{name}' is consumed more than once."
                        consumeOrigin
                        relatedLocations
                        document
                        state
                else
                    state

            let state = addEvent "consume" consumeOrigin binding state

            let updated =
                { binding with
                    UseMinimum = binding.UseMinimum + 1
                    UseMaximum = binding.UseMaximum + 1
                    FirstConsumeOrigin = binding.FirstConsumeOrigin |> Option.orElse consumeOrigin }

            { state with
                Bindings = Map.add name updated state.Bindings }

    let private mergeBranchState (left: CheckState) (right: CheckState) =
        let mergeBinding name leftBinding =
            match Map.tryFind name right.Bindings with
            | Some rightBinding ->
                { leftBinding with
                    UseMinimum = min leftBinding.UseMinimum rightBinding.UseMinimum
                    UseMaximum = max leftBinding.UseMaximum rightBinding.UseMaximum
                    CapturedRegions = Set.union leftBinding.CapturedRegions rightBinding.CapturedRegions }
            | None -> leftBinding

        let mergedBindings =
            left.Bindings
            |> Map.map mergeBinding

        { left with
            Bindings = mergedBindings
            Diagnostics = left.Diagnostics @ right.Diagnostics
            Events = left.Events @ right.Events
            BorrowRegions = left.BorrowRegions @ right.BorrowRegions
            UsingScopes = left.UsingScopes @ right.UsingScopes
            Closures = left.Closures @ right.Closures
            DeferredFacts = (left.DeferredFacts @ right.DeferredFacts) |> List.distinct |> List.sort
            NextBindingId = max left.NextBindingId right.NextBindingId
            NextEventId = max left.NextEventId right.NextEventId
            NextRegionId = max left.NextRegionId right.NextRegionId
            NextUsingScopeId = max left.NextUsingScopeId right.NextUsingScopeId
            NextClosureId = max left.NextClosureId right.NextClosureId }

    let private escapedBorrowRegions state expression =
        capturedRegions state expression

    let private checkEscape (document: ParsedDocument) expression state =
        let escaped = escapedBorrowRegions state expression

        if Set.isEmpty escaped then
            state
        else
            let escapeOrigin =
                match expression with
                | Name [ name ] -> findUseLocation document name 1
                | _ -> None

            let relatedLocations =
                capturedBindings state expression
                |> List.collect (fun binding ->
                    [
                        match binding.Origin with
                        | Some location ->
                            { Message = $"Captured binding '{binding.Name}'."
                              Location = location }
                        | None -> ()

                        for location in binding.CapturedBindingOrigins do
                            { Message = "Borrow introduction captured by the escaping value."
                              Location = location }
                    ])
                |> List.distinctBy (fun related -> related.Location.FilePath, related.Location.Span.Start, related.Message)

            let state =
                match expression with
                | Name [ name ] ->
                    match Map.tryFind name state.Bindings with
                    | Some binding ->
                        let state = addEvent "escape" escapeOrigin binding state

                        match binding.ClosureFactId with
                        | Some closureId -> markClosureEscaped closureId state
                        | None -> state
                    | None -> addNamedEvent "escape" escapeOrigin name state
                | _ -> state

            addDiagnostic
                borrowEscapeCode
                "A value that captures a borrowed region cannot escape its protected scope."
                escapeOrigin
                relatedLocations
                document
                state

    let private tryCalleeName (expression: CoreExpression) =
        match expression with
        | Name [ name ] -> Some name
        | _ -> None

    let rec private checkExpression (document: ParsedDocument) (signatures: Map<string, FunctionSignature>) state expression =
        match expression with
        | Literal _
        | Name _ ->
            state
        | Lambda _ ->
            state
        | IfThenElse(condition, whenTrue, whenFalse) ->
            let state = checkExpression document signatures state condition
            let left = checkExpression document signatures state whenTrue
            let right = checkExpression document signatures state whenFalse
            mergeBranchState left right
        | Match(scrutinee, cases) ->
            let state =
                checkExpression document signatures state scrutinee
                |> addDeferredFact "match-pattern-resource-checking"

            match cases with
            | [] -> state
            | first :: rest ->
                let firstState = checkExpression document signatures state first.Body

                rest
                |> List.fold (fun current caseClause ->
                    mergeBranchState current (checkExpression document signatures state caseClause.Body)) firstState
        | Do statements ->
            checkDoStatements document signatures state statements
        | MonadicSplice inner
        | InoutArgument inner
        | Unary(_, inner) ->
            checkExpression document signatures state inner
        | Binary(left, _, right) ->
            state
            |> fun state -> checkExpression document signatures state left
            |> fun state -> checkExpression document signatures state right
        | PrefixedString(_, parts) ->
            parts
            |> List.fold (fun current part ->
                match part with
                | StringText _ -> current
                | StringInterpolation inner -> checkExpression document signatures current inner) state
        | Apply(callee, arguments) ->
            let state = checkExpression document signatures state callee
            let calleeName = tryCalleeName callee

            let parameterQuantities =
                calleeName
                |> Option.bind (fun name -> Map.tryFind name signatures)
                |> Option.map (fun signature -> signature.ParameterQuantities)
                |> Option.defaultValue []

            ((state, 0), arguments)
            ||> List.fold (fun (current, index) argument ->
                let quantity =
                    parameterQuantities
                    |> List.tryItem index

                let next =
                    match quantity, argument with
                    | Some quantity, Name [ name ] when quantityConsumes quantity ->
                        consumeBinding document name current
                    | Some quantity, InoutArgument(Name [ name ]) when quantityConsumes quantity ->
                        consumeBinding document name current
                    | Some quantity, Name [ name ] when quantityBorrows quantity ->
                        current
                        |> addNamedEvent "borrow" (findUseLocation document name 1) name
                        |> fun current -> checkExpression document signatures current argument
                    | Some quantity, _ when quantityBorrows quantity ->
                        checkExpression document signatures current argument
                    | _ ->
                        let current = checkEscape document argument current
                        checkExpression document signatures current argument

                next, index + 1)
            |> fst

    and private checkDoStatements (document: ParsedDocument) (signatures: Map<string, FunctionSignature>) state statements =
        let rec loop current remaining =
            match remaining with
            | [] -> current
            | DoLet(binding, expression) :: rest ->
                let captured =
                    match expression with
                    | Lambda _ -> capturedRegions current expression
                    | _ -> Set.empty

                let capturedBindings =
                    match expression with
                    | Lambda _ -> capturedBindings current expression
                    | _ -> []

                let current =
                    match expression with
                    | Lambda _ -> current
                    | _ -> checkExpression document signatures current expression

                let current =
                    capturedBindings
                    |> List.fold (fun state binding ->
                        addEvent "capture" (findUseLocation document binding.Name 1) binding state) current

                let closureName =
                    match expression, collectPatternNames binding.Pattern with
                    | Lambda _, [ name ] -> Some name
                    | _ -> None

                let closureFactId, current =
                    match expression with
                    | Lambda _ ->
                        let closureId, current = addClosureFact closureName capturedBindings captured current
                        Some closureId, current
                    | _ -> None, current

                let checkDrop = binding.Quantity = Some QuantityOne
                let capturedBindingOrigins =
                    capturedBindings
                    |> List.choose (fun binding -> binding.Origin)

                let current = addPatternBindings document binding binding.Quantity captured capturedBindingOrigins checkDrop closureFactId current
                loop current rest
            | DoBind(binding, expression) :: rest ->
                let current = checkExpression document signatures current expression
                let checkDrop = binding.Quantity = Some QuantityOne
                let current = addPatternBindings document binding binding.Quantity Set.empty [] checkDrop None current
                loop current rest
            | DoUsing(pattern, expression) :: rest ->
                let current = checkExpression document signatures current expression

                let region: BorrowRegion =
                    { Id = $"rho{current.NextRegionId}"
                      ExplicitName = None
                      OwnerScope = "using" }

                let usingScopeId = $"{current.ScopeId}.using{current.NextUsingScopeId}"
                let hiddenOwnedBinding = $"{usingScopeId}.owned"

                let current =
                    { current with
                        NextRegionId = current.NextRegionId + 1
                        NextUsingScopeId = current.NextUsingScopeId + 1 }
                    |> addBorrowRegionFact region
                    |> addUsingScopeFact usingScopeId hiddenOwnedBinding region

                let current =
                    collectPatternNames pattern
                    |> List.fold (fun state name ->
                        addBinding
                            name
                            (Some(QuantityBorrow None))
                            (Some region)
                            Set.empty
                            []
                            false
                            None
                            (findBinderLocation document name)
                            state) current

                loop current rest
            | DoVar(name, expression) :: rest ->
                let current = checkExpression document signatures current expression
                let current = addBinding name None None Set.empty [] false None (findBinderLocation document name) current
                loop current rest
            | DoAssign(_, expression) :: rest ->
                let current = checkExpression document signatures current expression
                loop current rest
            | DoExpression expression :: rest ->
                let current = checkExpression document signatures current expression
                loop current rest
            | DoWhile(condition, body) :: rest ->
                let current =
                    checkExpression document signatures current condition
                    |> addDeferredFact "while-resource-fixed-point"

                let bodyState = checkDoStatements document signatures current body
                let current = mergeBranchState current bodyState
                loop current rest

        let checkedState = loop state statements

        checkedState.Bindings
        |> Map.fold (fun current _ binding ->
            if binding.CheckLinearDrop && binding.UseMinimum < 1 then
                addDiagnostic
                    linearDropCode
                    $"Linear resource '{binding.Name}' is not consumed on every path."
                    binding.Origin
                    []
                    document
                    current
            else
                current) checkedState

    let private signatureName (moduleName: string list option) bindingName =
        let simple = bindingName

        match moduleName with
        | Some segments -> [ simple; $"{SyntaxFacts.moduleNameToText segments}.{simple}" ]
        | None -> [ simple ]

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

    let private quantityFromSignatureSegment (tokens: Token list) =
        let significant =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        if significant |> List.exists (fun token -> token.Text = "&") then
            Some(QuantityBorrow None)
        else
            match significant with
            | token :: _ when token.Text = "1" -> Some QuantityOne
            | token :: _ when token.Text = "0" -> Some QuantityZero
            | token :: _ when token.Text = "omega" -> Some QuantityOmega
            | _ -> None

    let private signatureParameterQuantities tokens =
        let segments =
            splitTopLevelArrows tokens
            |> List.filter (List.isEmpty >> not)

        if List.length segments <= 1 then
            []
        else
            segments
            |> List.take (segments.Length - 1)
            |> List.map quantityFromSignatureSegment

    let private signatureEntries (document: ParsedDocument) name quantities =
        signatureName document.ModuleName name
        |> List.map (fun signatureName ->
            signatureName,
            { Name = signatureName
              ParameterQuantities = quantities })

    let private collectSignatures (documents: ParsedDocument list) =
        documents
        |> List.collect (fun document ->
            document.Syntax.Declarations
            |> List.choose (function
                | SignatureDeclaration declaration ->
                    Some(signatureEntries document declaration.Name (signatureParameterQuantities declaration.TypeTokens))
                | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
                    Some(signatureEntries document declaration.Name (signatureParameterQuantities declaration.TypeTokens))
                | LetDeclaration definition ->
                    definition.Name
                    |> Option.map (fun name ->
                        signatureEntries
                            document
                            name
                            (definition.Parameters
                             |> List.map (fun (parameter: Parameter) ->
                                 if parameter.IsInout then
                                     Some QuantityOne
                                 else
                                     parameter.Quantity)))
                | _ -> None)
            |> List.concat)
        |> Map.ofList

    let private addParameterBinding (document: ParsedDocument) (parameter: Parameter) state =
        let quantity =
            if parameter.IsInout then Some QuantityOne else parameter.Quantity

        let region: BorrowRegion option =
            match quantity with
            | Some(QuantityBorrow explicitRegion) ->
                Some
                    { Id = defaultArg explicitRegion $"rho_param_{parameter.Name}"
                      ExplicitName = explicitRegion
                      OwnerScope = $"parameter:{parameter.Name}" }
            | _ -> None

        let state =
            match region with
            | Some region -> addBorrowRegionFact region state
            | None -> state

        addBinding parameter.Name quantity region Set.empty [] false None (findBinderLocation document parameter.Name) state

    let private checkDefinition (signatures: Map<string, FunctionSignature>) (document: ParsedDocument) scopeId (definition: LetDefinition) =
        match definition.Body with
        | Some body ->
            let initialState =
                definition.Parameters
                |> List.fold (fun state parameter -> addParameterBinding document parameter state) (emptyState scopeId)

            checkExpression document signatures initialState body
        | None ->
            emptyState scopeId

    let private definitionScopeId (document: ParsedDocument) index (definition: LetDefinition) =
        let moduleName =
            document.ModuleName
            |> Option.map SyntaxFacts.moduleNameToText
            |> Option.defaultValue "<anonymous>"

        let definitionName = definition.Name |> Option.defaultValue $"anon{index}"
        $"{moduleName}.{definitionName}"

    let private factsForDocument (document: ParsedDocument) states =
        let bindings =
            states
            |> List.collect (fun state -> state.Bindings |> Map.toList |> List.map snd)
            |> List.map bindingFact
            |> List.sortBy (fun fact -> fact.BindingId)

        let uses =
            states
            |> List.collect (fun state -> state.Events)
            |> List.sortBy (fun fact -> fact.UseId)

        let borrowRegions =
            states
            |> List.collect (fun state -> state.BorrowRegions)
            |> List.distinctBy (fun fact -> fact.BorrowRegionId)
            |> List.sortBy (fun fact -> fact.BorrowRegionId)

        let usingScopes =
            states
            |> List.collect (fun state -> state.UsingScopes)
            |> List.sortBy (fun fact -> fact.UsingScopeId)

        let closures =
            states
            |> List.collect (fun state -> state.Closures)
            |> List.sortBy (fun fact -> fact.ClosureId)

        let diagnosticCodes =
            states
            |> List.collect (fun state -> state.Diagnostics)
            |> List.map (fun diagnostic -> diagnostic.Code)

        let deferred =
            states
            |> List.collect (fun state -> state.DeferredFacts)
            |> List.distinct
            |> List.sort

        { OwnershipBindings = bindings
          OwnershipUses = uses
          OwnershipBorrowRegions = borrowRegions
          OwnershipUsingScopes = usingScopes
          OwnershipClosures = closures
          OwnershipDeferred = deferred
          OwnershipDiagnostics = diagnosticCodes }

    let private checkDocument signatures (document: ParsedDocument) =
        let states =
            document.Syntax.Declarations
            |> List.mapi (fun index declaration ->
                match declaration with
                | LetDeclaration definition ->
                    checkDefinition signatures document (definitionScopeId document index definition) definition
                | _ ->
                    emptyState $"{document.Source.FilePath}.decl{index}")

        let diagnostics =
            states |> List.collect (fun state -> state.Diagnostics)

        diagnostics, factsForDocument document states

    let checkDocumentsWithFacts (documents: ParsedDocument list) =
        let signatures = collectSignatures documents

        let checkedDocuments =
            documents
            |> List.map (fun document ->
                let diagnostics, facts = checkDocument signatures document
                document.Source.FilePath, diagnostics, facts)

        { Diagnostics =
            checkedDocuments
            |> List.collect (fun (_, diagnostics, _) -> diagnostics)
          OwnershipFactsByFile =
            checkedDocuments
            |> List.map (fun (filePath, _, facts) -> filePath, facts)
            |> Map.ofList }

    let checkDocuments documents =
        (checkDocumentsWithFacts documents).Diagnostics
