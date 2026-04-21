namespace Kappa.Compiler

open System

module ResourceChecking =
    let private linearDropCode = "E_QTT_LINEAR_DROP"
    let private linearOveruseCode = "E_QTT_LINEAR_OVERUSE"
    let private borrowEscapeCode = "E_QTT_BORROW_ESCAPE"

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
        { Name: string
          DeclaredQuantity: Quantity option
          Place: ResourcePlace
          BorrowRegion: BorrowRegion option
          CapturedRegions: Set<string>
          UseMinimum: int
          UseMaximum: int
          CheckLinearDrop: bool }

    type private CheckState =
        { Bindings: Map<string, ResourceBinding>
          Diagnostics: Diagnostic list
          NextRegionId: int }

    let private emptyState =
        { Bindings = Map.empty
          Diagnostics = []
          NextRegionId = 0 }

    let private diagnosticLocation (document: ParsedDocument) =
        document.Source.GetLocation(TextSpan.FromBounds(0, 0))

    let private addDiagnostic code message (document: ParsedDocument) (state: CheckState) =
        let diagnostic: Diagnostic =
            { Severity = Error
              Code = code
              Stage = Some "KFrontIR"
              Phase = Some(KFrontIRPhase.phaseName BODY_RESOLVE)
              Message = message
              Location = Some(diagnosticLocation document) }

        { state with
            Diagnostics = state.Diagnostics @ [ diagnostic ] }

    let private quantityConsumes quantity =
        match quantity with
        | Some QuantityOne ->
            true
        | _ ->
            false

    let private quantityBorrows quantity =
        match quantity with
        | Some(QuantityBorrow _) ->
            true
        | _ ->
            false

    let private collectPatternNames (pattern: CorePattern) =
        let rec loop current =
            seq {
                match current with
                | NamePattern name ->
                    yield name
                | ConstructorPattern(_, arguments) ->
                    for argument in arguments do
                        yield! loop argument
                | WildcardPattern
                | LiteralPattern _ ->
                    ()
            }

        loop pattern |> Seq.toList

    let rec private expressionNames (expression: CoreExpression) =
        seq {
            match expression with
            | Literal _ ->
                ()
            | Name [ name ] ->
                yield name
            | Name _ ->
                ()
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

    let private capturedRegions (state: CheckState) expression =
        expressionNames expression
        |> Seq.collect (fun name ->
            match Map.tryFind name state.Bindings with
            | Some binding ->
                seq {
                    match binding.BorrowRegion with
                    | Some region -> yield region.Id
                    | None -> ()

                    yield! binding.CapturedRegions
                }
            | None ->
                Seq.empty)
        |> Set.ofSeq

    let private addBinding name quantity borrowRegion capturedRegions checkDrop (state: CheckState) =
        let place: ResourcePlace =
            { Root = name
              Path = [] }

        let binding: ResourceBinding =
            { Name = name
              DeclaredQuantity = quantity
              Place = place
              BorrowRegion = borrowRegion
              CapturedRegions = capturedRegions
              UseMinimum = 0
              UseMaximum = 0
              CheckLinearDrop = checkDrop }

        { state with
            Bindings = Map.add name binding state.Bindings }

    let private addPatternBindings (binding: BindPattern) quantity capturedRegions checkDrop state =
        collectPatternNames binding.Pattern
        |> List.fold (fun current name ->
            addBinding name quantity None capturedRegions checkDrop current) state

    let private consumeBinding (document: ParsedDocument) name (state: CheckState) =
        match Map.tryFind name state.Bindings with
        | None ->
            state
        | Some binding ->
            let state =
                if binding.UseMaximum >= 1 then
                    addDiagnostic
                        linearOveruseCode
                        $"Linear resource '{name}' is consumed more than once."
                        document
                        state
                else
                    state

            let updated =
                { binding with
                    UseMinimum = binding.UseMinimum + 1
                    UseMaximum = binding.UseMaximum + 1 }

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
            | None ->
                leftBinding

        let mergedBindings =
            left.Bindings
            |> Map.map mergeBinding

        { left with
            Bindings = mergedBindings
            Diagnostics = left.Diagnostics @ right.Diagnostics
            NextRegionId = max left.NextRegionId right.NextRegionId }

    let private escapedBorrowRegions state expression =
        capturedRegions state expression

    let private checkEscape (document: ParsedDocument) expression state =
        let escaped = escapedBorrowRegions state expression

        if Set.isEmpty escaped then
            state
        else
            addDiagnostic
                borrowEscapeCode
                "A value that captures a borrowed region cannot escape its protected scope."
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
            // Lambda bodies are checked when called; here we only compute captures when
            // the lambda is bound or escapes.
            state
        | IfThenElse(condition, whenTrue, whenFalse) ->
            let state = checkExpression document signatures state condition
            let left = checkExpression document signatures state whenTrue
            let right = checkExpression document signatures state whenFalse
            mergeBranchState left right
        | Match(scrutinee, cases) ->
            let state = checkExpression document signatures state scrutinee

            match cases with
            | [] ->
                state
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
            | [] ->
                current
            | DoLet(binding, expression) :: rest ->
                let captured =
                    match expression with
                    | Lambda _ -> capturedRegions current expression
                    | _ -> Set.empty

                let current =
                    match expression with
                    | Lambda _ -> current
                    | _ -> checkExpression document signatures current expression

                let checkDrop = binding.Quantity = Some QuantityOne
                let current = addPatternBindings binding binding.Quantity captured checkDrop current
                loop current rest
            | DoBind(binding, expression) :: rest ->
                let current = checkExpression document signatures current expression
                let checkDrop = binding.Quantity = Some QuantityOne
                let current = addPatternBindings binding binding.Quantity Set.empty checkDrop current
                loop current rest
            | DoUsing(pattern, expression) :: rest ->
                let current = checkExpression document signatures current expression
                let region =
                    { Id = $"rho{current.NextRegionId}"
                      ExplicitName = None
                      OwnerScope = "using" }

                let current =
                    { current with
                        NextRegionId = current.NextRegionId + 1 }

                let current =
                    collectPatternNames pattern
                    |> List.fold (fun state name ->
                        addBinding name (Some(QuantityBorrow None)) (Some region) Set.empty false state) current

                loop current rest
            | DoVar(name, expression) :: rest ->
                let current = checkExpression document signatures current expression
                let current = addBinding name None None Set.empty false current
                loop current rest
            | DoAssign(_, expression) :: rest ->
                let current = checkExpression document signatures current expression
                loop current rest
            | DoExpression expression :: rest ->
                let current = checkExpression document signatures current expression
                loop current rest
            | DoWhile(condition, body) :: rest ->
                let current = checkExpression document signatures current condition
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
                    document
                    current
            else
                current) checkedState

    let private signatureName (moduleName: string list option) bindingName =
        let simple = bindingName

        match moduleName with
        | Some segments -> [ simple; $"{SyntaxFacts.moduleNameToText segments}.{simple}" ]
        | None -> [ simple ]

    let private collectSignatures (documents: ParsedDocument list) =
        documents
        |> List.collect (fun document ->
            document.Syntax.Declarations
            |> List.choose (function
                | LetDeclaration definition ->
                    definition.Name
                    |> Option.map (fun name ->
                        signatureName document.ModuleName name
                        |> List.map (fun signatureName ->
                            signatureName,
                            { Name = signatureName
                              ParameterQuantities =
                                definition.Parameters
                                |> List.map (fun (parameter: Parameter) ->
                                    if parameter.IsInout then
                                        Some QuantityOne
                                    else
                                        parameter.Quantity) }))
                | _ ->
                    None)
            |> List.concat)
        |> Map.ofList

    let private checkDefinition (signatures: Map<string, FunctionSignature>) (document: ParsedDocument) (definition: LetDefinition) =
        match definition.Body with
        | Some body ->
            let initialState =
                definition.Parameters
                |> List.fold (fun state (parameter: Parameter) ->
                    let quantity =
                        if parameter.IsInout then Some QuantityOne else parameter.Quantity

                    let region =
                        match quantity with
                        | Some(QuantityBorrow explicitRegion) ->
                            Some
                                { Id = defaultArg explicitRegion $"rho_param_{parameter.Name}"
                                  ExplicitName = explicitRegion
                                  OwnerScope = $"parameter:{parameter.Name}" }
                        | _ ->
                            None

                    addBinding parameter.Name quantity region Set.empty false state) emptyState

            checkExpression document signatures initialState body
            |> fun state -> state.Diagnostics
        | None ->
            []

    let checkDocuments (documents: ParsedDocument list) =
        let signatures = collectSignatures documents

        documents
        |> List.collect (fun document ->
            document.Syntax.Declarations
            |> List.collect (function
                | LetDeclaration definition ->
                    checkDefinition signatures document definition
                | _ ->
                    []))
