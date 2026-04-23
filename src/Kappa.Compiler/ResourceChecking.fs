namespace Kappa.Compiler

open System
open Kappa.Compiler.ResourceModel
open Kappa.Compiler.ResourceCheckingDiagnostics
open Kappa.Compiler.ResourceCheckingSignatures
open Kappa.Compiler.ResourceCheckingSurface

// Runs the BODY_RESOLVE ownership checker and produces resource diagnostics and facts.
module ResourceChecking =
    type CheckResult =
        { Diagnostics: Diagnostic list
          OwnershipFactsByFile: Map<string, OwnershipFactSet> }

    type private CheckState = ResourceContext

    let private emptyState scopeId =
        ResourceModel.ResourceContext.empty scopeId

    let private currentScopeId (state: CheckState) =
        state.ActiveScopes
        |> List.head
        |> fun scope -> scope.Id

    let private pushScope label (state: CheckState) =
        let scopeId = $"{currentScopeId state}.{label}{state.NextScopeId}"

        { state with
            ActiveScopes =
                { Id = scopeId
                  IntroducedBindings = [] }
                :: state.ActiveScopes
            NextScopeId = state.NextScopeId + 1 }

    let private popActiveBinding name (activeBindingIds: Map<string, string list>) =
        match Map.tryFind name activeBindingIds with
        | Some(_ :: rest) when List.isEmpty rest ->
            Map.remove name activeBindingIds
        | Some(_ :: rest) ->
            Map.add name rest activeBindingIds
        | _ ->
            activeBindingIds

    let private popScope (state: CheckState) =
        match state.ActiveScopes with
        | [] ->
            state
        | _rootOnly when List.length state.ActiveScopes = 1 ->
            state
        | scope :: rest ->
            let activeBindingIds =
                scope.IntroducedBindings
                |> List.fold (fun current (name, _) -> popActiveBinding name current) state.ActiveBindingIds

            { state with
                ActiveScopes = rest
                ActiveBindingIds = activeBindingIds }

    let private withScope label checker state =
        state
        |> pushScope label
        |> checker
        |> popScope

    let private tryFindBindingId name (state: CheckState) =
        state.ActiveBindingIds
        |> Map.tryFind name
        |> Option.bind List.tryHead

    let private tryFindBinding name (state: CheckState) =
        tryFindBindingId name state
        |> Option.bind (fun bindingId -> Map.tryFind bindingId state.Bindings)

    let private updateBinding bindingId updater (state: CheckState) =
        match Map.tryFind bindingId state.Bindings with
        | Some binding ->
            let updated = updater binding
            { state with
                Bindings = Map.add bindingId updated state.Bindings }
        | None ->
            state

    let private quantityConsumes quantity =
        match quantity with
        | Some quantity -> ResourceQuantity.isExactOne quantity
        | _ -> false

    let private quantityBorrows quantity =
        match quantity with
        | Some quantity -> ResourceQuantity.isBorrow quantity
        | _ -> false

    let private concreteQuantityCountsUse quantity =
        match quantity with
        | ResourceQuantity.Interval(0, Some 0) -> false
        | ResourceQuantity.Interval _ -> true
        | _ -> false

    let private quantityText quantity =
        quantity |> Option.map ResourceQuantity.toSurfaceText

    let private combineLambdaBindingQuantity left right =
        match left, right with
        | Some(ResourceQuantity.Interval(1, Some 1)), _
        | _, Some(ResourceQuantity.Interval(1, Some 1)) ->
            Some ResourceQuantity.one
        | Some(ResourceQuantity.Interval(0, Some 1)), Some(ResourceQuantity.Interval(1, None))
        | Some(ResourceQuantity.Interval(1, None)), Some(ResourceQuantity.Interval(0, Some 1)) ->
            Some ResourceQuantity.one
        | Some(ResourceQuantity.Interval(0, Some 1)), _
        | _, Some(ResourceQuantity.Interval(0, Some 1)) ->
            Some ResourceQuantity.atMostOne
        | Some(ResourceQuantity.Interval(1, None)), _
        | _, Some(ResourceQuantity.Interval(1, None)) ->
            Some ResourceQuantity.atLeastOne
        | Some(ResourceQuantity.Interval(0, Some 0)), other
        | other, Some(ResourceQuantity.Interval(0, Some 0)) ->
            other
        | Some(ResourceQuantity.Interval(0, None)), other
        | other, Some(ResourceQuantity.Interval(0, None)) ->
            other
        | Some(ResourceQuantity.Borrow _), other
        | other, Some(ResourceQuantity.Borrow _) ->
            other
        | Some(ResourceQuantity.Variable _), _
        | _, Some(ResourceQuantity.Variable _) ->
            left |> Option.orElse right
        | None, None ->
            None
        | _ ->
            left |> Option.orElse right

    let private inferLambdaBindingQuantity (capturedBindings: ResourceBinding list) =
        capturedBindings
        |> List.fold (fun current binding -> combineLambdaBindingQuantity current binding.DeclaredQuantity) None

    let private bindingDemand (binding: ResourceBinding) =
        match binding.BorrowRegion with
        | Some _ -> "&"
        | None -> $"[{binding.UseMinimum},{binding.UseMaximum}]"

    let private bindingState (binding: ResourceBinding) =
        match binding.BorrowRegion with
        | Some _ -> "borrowed"
        | None when binding.UseMaximum > 0 -> "consumed"
        | None when binding.CheckLinearDrop -> "unconsumed"
        | None -> "available"

    let private wouldOveruseBinding (binding: ResourceBinding) =
        let capability =
            binding.DeclaredQuantity
            |> Option.defaultValue ResourceQuantity.omega

        let nextDemand = ResourceQuantity.exact (binding.UseMaximum + 1)
        not (ResourceQuantity.satisfies capability nextDemand)

    let private bindingFact (binding: ResourceBinding) =
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

    let private addEvent useKind origin (binding: ResourceBinding) (state: CheckState) =
        let event: OwnershipUseFact =
            { UseId = $"{currentScopeId state}.u{state.NextEventId}"
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
        match tryFindBinding name state with
        | Some binding -> addEvent useKind origin binding state
        | None ->
            let event: OwnershipUseFact =
                { UseId = $"{currentScopeId state}.u{state.NextEventId}"
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

    let private introduceBorrowRegion ownerScope explicitRegion (state: CheckState) =
        let region: BorrowRegion =
            { Id = defaultArg explicitRegion $"rho{state.NextRegionId}"
              ExplicitName = explicitRegion
              OwnerScope = ownerScope }

        let nextState =
            match explicitRegion with
            | Some _ -> state
            | None -> { state with NextRegionId = state.NextRegionId + 1 }

        region, addBorrowRegionFact region nextState

    let private introduceBorrowRegionForQuantity ownerScope quantity state =
        match quantity with
        | Some(ResourceQuantity.Borrow explicitRegion) ->
            let region, state = introduceBorrowRegion ownerScope explicitRegion state
            Some region, state
        | _ -> None, state

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

    let private addClosureFact name (capturedBindings: ResourceBinding list) capturedRegions (state: CheckState) =
        let closureId = $"{currentScopeId state}.c{state.NextClosureId}"

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

    let private collectPatternNames (pattern: SurfacePattern) =
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

    let rec private expressionNames (expression: SurfaceExpression) =
        seq {
            match expression with
            | Literal _ -> ()
            | Name [ name ] -> yield name
            | Name _ -> ()
            | LocalLet(bindingName, value, body) ->
                yield! expressionNames value

                for name in expressionNames body do
                    if not (String.Equals(name, bindingName, StringComparison.Ordinal)) then
                        yield name
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
                    match caseClause.Guard with
                    | Some guard -> yield! expressionNames guard
                    | None -> ()

                    yield! expressionNames caseClause.Body
            | RecordUpdate(receiver, fields) ->
                yield! expressionNames receiver

                for field in fields do
                    yield! expressionNames field.Value
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

    and private doStatementNames (statement: SurfaceDoStatement) =
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
        |> Seq.choose (fun name -> tryFindBinding name state)
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

    let private addBinding name quantity borrowRegion capturedRegions capturedBindingOrigins checkDrop closureFactId localLambda origin (state: CheckState) =
        let place: ResourcePlace =
            { Root = name
              Path = [] }

        let bindingId = $"{currentScopeId state}.b{state.NextBindingId}"

        let binding: ResourceBinding =
            { Id = bindingId
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
              LocalLambda = localLambda
              Origin = origin
              FirstConsumeOrigin = None }

        let activeBindingIds =
            state.ActiveBindingIds
            |> Map.change name (fun existing -> Some(bindingId :: Option.defaultValue [] existing))

        let activeScopes =
            match state.ActiveScopes with
            | currentScope :: rest ->
                { currentScope with
                    IntroducedBindings = (name, bindingId) :: currentScope.IntroducedBindings }
                :: rest
            | [] ->
                []

        { state with
            ActiveScopes = activeScopes
            ActiveBindingIds = activeBindingIds
            Bindings = Map.add bindingId binding state.Bindings
            NextBindingId = state.NextBindingId + 1 }

    let private addPatternBindings (document: ParsedDocument) (binding: SurfaceBindPattern) quantity borrowRegion capturedRegions capturedBindingOrigins checkDrop closureFactId localLambda state =
        collectPatternNames binding.Pattern
        |> List.fold (fun current name ->
            addBinding
                name
                quantity
                borrowRegion
                capturedRegions
                capturedBindingOrigins
                checkDrop
                closureFactId
                localLambda
                (findBinderLocation document name)
                current) state

    let private tryMovedLinearBinding expression (state: CheckState) =
        match expression with
        | Name [ name ] ->
            tryFindBinding name state
            |> Option.filter (fun binding ->
                binding.DeclaredQuantity
                |> Option.exists ResourceQuantity.isExactOne)
        | _ ->
            None

    let private consumeBinding (document: ParsedDocument) name (state: CheckState) =
        match tryFindBinding name state with
        | None -> state
        | Some binding ->
            let consumeOrigin = findBindingUseLocation document binding (binding.UseMaximum + 1)

            let state =
                if binding.BorrowRegion.IsSome then
                    let relatedLocations =
                        [
                            match binding.Origin with
                            | Some location ->
                                { Message = "Borrowed resource binding."
                                  Location = location }
                            | None -> ()
                        ]

                    addDiagnostic
                        borrowConsumeCode
                        $"Borrowed resource '{name}' cannot be consumed."
                        consumeOrigin
                        relatedLocations
                        document
                        state
                elif wouldOveruseBinding binding then
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
                Bindings = Map.add binding.Id updated state.Bindings }

    let private mergeBindingSnapshots (leftBinding: ResourceBinding) (rightBinding: ResourceBinding) =
        { leftBinding with
            CapturedRegions = Set.union leftBinding.CapturedRegions rightBinding.CapturedRegions
            CapturedBindingOrigins =
                leftBinding.CapturedBindingOrigins @ rightBinding.CapturedBindingOrigins
                |> List.distinctBy (fun location -> location.FilePath, location.Span.Start, location.Span.Length)
            UseMinimum = min leftBinding.UseMinimum rightBinding.UseMinimum
            UseMaximum = max leftBinding.UseMaximum rightBinding.UseMaximum
            FirstConsumeOrigin = leftBinding.FirstConsumeOrigin |> Option.orElse rightBinding.FirstConsumeOrigin }

    let private mergeBranchState (left: CheckState) (right: CheckState) =
        let mergedBindings =
            let combined =
                right.Bindings
                |> Map.fold (fun current bindingId binding -> Map.add bindingId binding current) left.Bindings

            left.ActiveBindingIds
            |> Map.fold (fun current name leftBindingIds ->
                match leftBindingIds, Map.tryFind name right.ActiveBindingIds with
                | leftBindingId :: _, Some(rightBindingId :: _)
                    when String.Equals(leftBindingId, rightBindingId, StringComparison.Ordinal) ->
                    match Map.tryFind leftBindingId left.Bindings, Map.tryFind rightBindingId right.Bindings with
                    | Some leftBinding, Some rightBinding ->
                        Map.add leftBindingId (mergeBindingSnapshots leftBinding rightBinding) current
                    | _ ->
                        current
                | _ ->
                    current) combined

        { left with
            Bindings = mergedBindings
            Diagnostics = left.Diagnostics @ right.Diagnostics
            Events = left.Events @ right.Events
            BorrowRegions = left.BorrowRegions @ right.BorrowRegions
            UsingScopes = left.UsingScopes @ right.UsingScopes
            Closures = left.Closures @ right.Closures
            DeferredFacts = (left.DeferredFacts @ right.DeferredFacts) |> List.distinct |> List.sort
            NextScopeId = max left.NextScopeId right.NextScopeId
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
                    match tryFindBinding name state with
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

    let private checkEscapingLambda (document: ParsedDocument) expression state =
        match expression with
        | Lambda _ ->
            let captured = capturedRegions state expression

            if Set.isEmpty captured then
                state
            else
                let capturedBindingList = capturedBindings state expression

                let state =
                    capturedBindingList
                    |> List.fold (fun current binding ->
                        addEvent "capture" (findUseLocation document binding.Name 1) binding current) state

                let closureId, state = addClosureFact None capturedBindingList captured state

                let escapeOrigin =
                    capturedBindingList
                    |> List.tryPick (fun binding -> findUseLocation document binding.Name 1)

                let relatedLocations =
                    capturedBindingList
                    |> List.collect (fun binding ->
                        [
                            match binding.Origin with
                            | Some location ->
                                { Message = $"Captured binding '{binding.Name}'."
                                  Location = location }
                            | None -> ()
                        ])
                    |> List.distinctBy (fun related -> related.Location.FilePath, related.Location.Span.Start, related.Message)

                addDiagnostic
                    borrowEscapeCode
                    "A lambda that captures a borrowed region cannot escape its protected scope."
                    escapeOrigin
                    relatedLocations
                    document
                    state
                |> markClosureEscaped closureId
        | _ ->
            state

    let private currentScopeBindingIds (state: CheckState) =
        state.ActiveScopes
        |> List.head
        |> fun scope -> scope.IntroducedBindings |> List.map snd

    let private checkScopeLinearDrops (document: ParsedDocument) (state: CheckState) =
        currentScopeBindingIds state
        |> List.fold (fun current bindingId ->
            let binding = current.Bindings[bindingId]

            if binding.CheckLinearDrop && binding.UseMinimum < 1 then
                addDiagnostic
                    linearDropCode
                    $"Linear resource '{binding.Name}' is not consumed on every path."
                    binding.Origin
                    []
                    document
                    current
            else
                current) state

    let private parameterQuantity (parameter: Parameter) =
        if parameter.IsInout then
            Some ResourceQuantity.one
        else
            parameter.Quantity |> Option.map ResourceQuantity.ofSurface

    let rec private checkLocalLambdaInvocation
        (document: ParsedDocument)
        (signatures: Map<string, FunctionSignature>)
        state
        (lambdaValue: LocalLambda)
        arguments
        =
        if List.length arguments <> List.length lambdaValue.Parameters then
            state
        else
            withScope "lambda_call" (fun scopedState ->
                let scopedState =
                    (scopedState, List.zip lambdaValue.Parameters arguments)
                    ||> List.fold (fun current (parameter, argument) ->
                        let quantity = parameterQuantity parameter
                        let checkDrop = quantity |> Option.exists ResourceQuantity.requiresUse

                        let borrowRegion, current =
                            match quantity, argument with
                            | Some(ResourceQuantity.Borrow _), Name [ name ] ->
                                match tryFindBinding name current with
                                | Some binding ->
                                    binding.BorrowRegion, current
                                | None ->
                                    introduceBorrowRegionForQuantity $"{currentScopeId current}.param" quantity current
                            | _ ->
                                introduceBorrowRegionForQuantity $"{currentScopeId current}.param" quantity current

                        let capturedRegions, capturedBindingOrigins =
                            match argument with
                            | Name [ name ] ->
                                match tryFindBinding name current with
                                | Some binding ->
                                    let regions =
                                        seq {
                                            match binding.BorrowRegion with
                                            | Some region -> yield region.Id
                                            | None -> ()

                                            yield! binding.CapturedRegions
                                        }
                                        |> Set.ofSeq

                                    let origins =
                                        [
                                            match binding.Origin with
                                            | Some origin -> yield origin
                                            | None -> ()

                                            yield! binding.CapturedBindingOrigins
                                        ]

                                    regions, origins
                                | None ->
                                    Set.empty, []
                            | _ ->
                                Set.empty, []

                        addBinding
                            parameter.Name
                            quantity
                            borrowRegion
                            capturedRegions
                            capturedBindingOrigins
                            checkDrop
                            None
                            None
                            None
                            current)

                checkExpression document signatures scopedState lambdaValue.Body
                |> checkScopeLinearDrops document) state

    and private checkExpression (document: ParsedDocument) (signatures: Map<string, FunctionSignature>) state expression =
        match expression with
        | Literal _
        | Name _ ->
            state
        | LocalLet(bindingName, value, body) ->
            let state = checkExpression document signatures state value

            withScope $"let_{bindingName}" (fun scopedState ->
                let scopedState =
                    addBinding
                        bindingName
                        None
                        None
                        Set.empty
                        []
                        false
                        None
                        None
                        (findBinderLocation document bindingName)
                        scopedState

                checkExpression document signatures scopedState body
                |> checkScopeLinearDrops document) state
        | Lambda _ ->
            state
        | IfThenElse(condition, whenTrue, whenFalse) ->
            let state = checkExpression document signatures state condition
            let left = checkExpressionInScope "if_then" document signatures state whenTrue
            let right = checkExpressionInScope "if_else" document signatures state whenFalse
            mergeBranchState left right
        | Match(scrutinee, cases) ->
            let state =
                checkExpression document signatures state scrutinee
                |> addDeferredFact "match-pattern-resource-checking"

            match cases with
            | [] -> state
            | first :: rest ->
                let checkCase index (caseClause: SurfaceMatchCase) =
                    withScope $"match_case{index}" (fun scopedState ->
                        let scopedState =
                            match caseClause.Guard with
                            | Some guard -> checkExpression document signatures scopedState guard
                            | None -> scopedState

                        checkExpression document signatures scopedState caseClause.Body
                        |> checkScopeLinearDrops document) state

                let firstState = checkCase 0 first

                rest
                |> List.mapi (fun index caseClause ->
                    index + 1, caseClause)
                |> List.fold (fun current (index, caseClause) ->
                    mergeBranchState current (checkCase index caseClause)) firstState
        | RecordUpdate(receiver, fields) ->
            fields
            |> List.fold (fun current field ->
                checkExpression document signatures current field.Value) (checkExpression document signatures state receiver)
        | Do statements ->
            checkDoStatementsInScope "do" document signatures state statements
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
            let calleeBinding =
                match callee with
                | Name [ name ] -> tryFindBinding name state
                | _ -> None

            let localLambda =
                match calleeBinding with
                | Some binding ->
                    binding.LocalLambda
                | None ->
                    match callee with
                    | Lambda(parameters, body) ->
                        Some
                            { Parameters = parameters
                              Body = body }
                    | _ ->
                        None

            let state =
                match calleeBinding with
                | Some binding when binding.DeclaredQuantity |> Option.exists concreteQuantityCountsUse ->
                    consumeBinding document binding.Name state
                | Some binding when quantityBorrows binding.DeclaredQuantity ->
                    addNamedEvent "borrow" (findUseLocation document binding.Name 1) binding.Name state
                | _ ->
                    state

            let parameterQuantities =
                match localLambda with
                | Some lambdaValue ->
                    lambdaValue.Parameters |> List.map parameterQuantity
                | None ->
                    calleeName
                    |> Option.bind (fun name -> Map.tryFind name signatures)
                    |> Option.map (fun signature -> signature.ParameterQuantities)
                    |> Option.defaultValue []

            let parameterInout =
                match localLambda with
                | Some lambdaValue ->
                    lambdaValue.Parameters |> List.map (fun parameter -> parameter.IsInout)
                | None ->
                    calleeName
                    |> Option.bind (fun name -> Map.tryFind name signatures)
                    |> Option.map (fun signature -> signature.ParameterInout)
                    |> Option.defaultValue []

            let state =
                ((state, 0), arguments)
                ||> List.fold (fun (current, index) argument ->
                    let quantity =
                        parameterQuantities
                        |> List.tryItem index

                    let expectsInout =
                        parameterInout
                        |> List.tryItem index
                        |> Option.defaultValue false

                    let hasInoutMarker =
                        match argument with
                        | InoutArgument _ -> true
                        | _ -> false

                    let current =
                        if expectsInout && not hasInoutMarker then
                            addDiagnostic
                                inoutMarkerRequiredCode
                                "An argument supplied to an 'inout' parameter must be marked with '~'."
                                (argumentLocation document argument)
                                []
                                document
                                current
                        elif hasInoutMarker && not expectsInout then
                            addDiagnostic
                                inoutMarkerUnexpectedCode
                                "The '~' marker can only be used for an 'inout' parameter."
                                (argumentLocation document argument)
                                []
                                document
                                current
                        else
                            current

                    let next =
                        match quantity, argument with
                        | Some quantity, Name [ name ] when quantity |> Option.exists concreteQuantityCountsUse ->
                            consumeBinding document name current
                        | Some quantity, InoutArgument(Name [ name ]) when quantity |> Option.exists concreteQuantityCountsUse ->
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

            match localLambda with
            | Some lambdaValue ->
                checkLocalLambdaInvocation document signatures state lambdaValue arguments
            | None ->
                state

    and private checkExpressionInScope scopeLabel (document: ParsedDocument) (signatures: Map<string, FunctionSignature>) state expression =
        withScope scopeLabel (fun scopedState -> checkExpression document signatures scopedState expression) state

    and private checkDoStatements (document: ParsedDocument) (signatures: Map<string, FunctionSignature>) state statements =
        let rec loop current remaining =
            match remaining with
            | [] -> current
            | DoLet(binding, expression) :: rest ->
                let movedLinearBinding = tryMovedLinearBinding expression current

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

                let localLambda =
                    match expression, collectPatternNames binding.Pattern with
                    | Lambda(parameters, body), [ _ ] ->
                        Some
                            { Parameters = parameters
                              Body = body }
                    | _ ->
                        None

                let closureFactId, current =
                    match expression with
                    | Lambda _ ->
                        let closureId, current = addClosureFact closureName capturedBindings captured current
                        Some closureId, current
                    | _ -> None, current

                let declaredQuantity =
                    binding.Quantity
                    |> Option.map ResourceQuantity.ofSurface
                    |> Option.orElseWith (fun () ->
                        match expression with
                        | Lambda _ -> inferLambdaBindingQuantity capturedBindings
                        | _ -> None)
                    |> Option.orElseWith (fun () ->
                        movedLinearBinding
                        |> Option.bind (fun sourceBinding -> sourceBinding.DeclaredQuantity))

                let current =
                    movedLinearBinding
                    |> Option.map (fun sourceBinding ->
                        current
                        |> addEvent "move" (findBindingUseLocation document sourceBinding 1) sourceBinding
                        |> consumeBinding document sourceBinding.Name)
                    |> Option.defaultValue current

                let checkDrop = declaredQuantity |> Option.exists ResourceQuantity.requiresUse
                let capturedBindingOrigins =
                    capturedBindings
                    |> List.choose (fun binding -> binding.Origin)

                let borrowRegion, current =
                    introduceBorrowRegionForQuantity $"{currentScopeId current}.let" declaredQuantity current

                let current = addPatternBindings document binding declaredQuantity borrowRegion captured capturedBindingOrigins checkDrop closureFactId localLambda current
                loop current rest
            | DoBind(binding, expression) :: rest ->
                let current = checkExpression document signatures current expression
                let declaredQuantity = binding.Quantity |> Option.map ResourceQuantity.ofSurface
                let checkDrop = declaredQuantity |> Option.exists ResourceQuantity.requiresUse

                let borrowRegion, current =
                    introduceBorrowRegionForQuantity $"{currentScopeId current}.bind" declaredQuantity current

                let current = addPatternBindings document binding declaredQuantity borrowRegion Set.empty [] checkDrop None None current
                loop current rest
            | DoUsing(pattern, expression) :: rest ->
                let current = checkExpression document signatures current expression

                let region, current =
                    introduceBorrowRegion "using" None current

                let usingScopeId = $"{currentScopeId current}.using{current.NextUsingScopeId}"
                let hiddenOwnedBinding = $"{usingScopeId}.owned"

                let current =
                    { current with
                        NextUsingScopeId = current.NextUsingScopeId + 1 }
                    |> addUsingScopeFact usingScopeId hiddenOwnedBinding region

                let current =
                    collectPatternNames pattern
                    |> List.fold (fun state name ->
                        addBinding
                            name
                            (Some(ResourceQuantity.Borrow None))
                            (Some region)
                            Set.empty
                            []
                            false
                            None
                            None
                            (findBinderLocation document name)
                            state) current

                loop current rest
            | DoVar(name, expression) :: rest ->
                let current = checkExpression document signatures current expression
                let current = addBinding name None None Set.empty [] false None None (findBinderLocation document name) current
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

                let bodyState = checkDoStatementsInScope "while" document signatures current body
                let current = mergeBranchState current bodyState
                loop current rest

        let checkedState = loop state statements

        checkScopeLinearDrops document checkedState

    and private checkDoStatementsInScope scopeLabel (document: ParsedDocument) (signatures: Map<string, FunctionSignature>) state statements =
        withScope scopeLabel (fun scopedState -> checkDoStatements document signatures scopedState statements) state

    let private addParameterBinding (document: ParsedDocument) (parameter: Parameter) state =
        let quantity =
            if parameter.IsInout then
                Some ResourceQuantity.one
            else
                parameter.Quantity |> Option.map ResourceQuantity.ofSurface

        let checkDrop = quantity |> Option.exists ResourceQuantity.requiresUse

        let region: BorrowRegion option =
            match quantity with
            | Some(ResourceQuantity.Borrow explicitRegion) ->
                Some
                    { Id = defaultArg explicitRegion $"rho_param_{parameter.Name}"
                      ExplicitName = explicitRegion
                      OwnerScope = $"parameter:{parameter.Name}" }
            | _ -> None

        let state =
            match region with
            | Some region -> addBorrowRegionFact region state
            | None -> state

        addBinding parameter.Name quantity region Set.empty [] checkDrop None None (findBinderLocation document parameter.Name) state

    let private checkDefinition (signatures: Map<string, FunctionSignature>) (document: ParsedDocument) scopeId (definition: LetDefinition) =
        match definition.Body with
        | Some body ->
            let initialState =
                definition.Parameters
                |> List.fold (fun state parameter -> addParameterBinding document parameter state) (emptyState scopeId)

            match body with
            | Lambda _ -> checkEscapingLambda document body initialState
            | _ -> checkExpression document signatures initialState body
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
