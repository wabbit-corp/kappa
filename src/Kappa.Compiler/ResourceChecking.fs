namespace Kappa.Compiler

open System
open System.Threading
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

    type private ProjectionSummary =
        { Name: string
          Binders: ProjectionBinder list
          Body: SurfaceProjectionBody option }

    type private ProjectionDescriptorAlias =
        { ProjectionName: string
          SuppliedArguments: SurfaceExpression list
          RemainingPlaceBinders: ProjectionPlaceBinder list }

    type private TypeAliasSummary =
        { Parameters: string list
          Body: TypeSignatures.TypeExpr }

    let private scopedEffects = AsyncLocal<Map<string, EffectDeclaration> option>()
    let private backendProfile = AsyncLocal<BackendProfile option>()

    let private currentScopedEffects () =
        scopedEffects.Value |> Option.defaultValue Map.empty

    let private currentBackendProfile () =
        backendProfile.Value |> Option.defaultValue BackendProfile.defaultValue

    let private tryFindScopedEffectDeclaration name =
        currentScopedEffects () |> Map.tryFind name

    let private withScopedEffectDeclaration (declaration: EffectDeclaration) work =
        let saved = currentScopedEffects ()
        scopedEffects.Value <- Some(Map.add declaration.Name declaration saved)

        try
            work ()
        finally
            scopedEffects.Value <- Some saved

    let private withScopedEffectDeclarations (declarations: EffectDeclaration list) work =
        let saved = currentScopedEffects ()

        let merged =
            declarations
            |> List.fold (fun state declaration -> Map.add declaration.Name declaration state) saved

        scopedEffects.Value <- Some merged

        try
            work ()
        finally
            scopedEffects.Value <- Some saved

    let private withBackendProfile configuredBackendProfile work =
        let saved = currentBackendProfile ()
        backendProfile.Value <- Some configuredBackendProfile

        try
            work ()
        finally
            backendProfile.Value <- Some saved

    let private backendSupportsMultishotEffects configuredBackendProfile =
        match configuredBackendProfile with
        | BackendProfile.Interpreter
        | BackendProfile.DotNet
        | BackendProfile.Zig -> true
        | BackendProfile.Unknown _ -> false

    let private isMultiShotResumptionQuantity quantity =
        match quantity |> Option.defaultValue QuantityOne with
        | QuantityOmega
        | QuantityAtLeastOne
        | QuantityVariable _ ->
            true
        | _ -> false

    let private isPrivateByDefault (document: ParsedDocument) =
        document.Syntax.ModuleAttributes
        |> List.exists (fun attributeName -> String.Equals(attributeName, ModuleAttribute.PrivateByDefault, StringComparison.Ordinal))

    let private isExportedVisibility privateByDefault visibility =
        match visibility with
        | Some Visibility.Public -> true
        | Some Visibility.Private -> false
        | None -> not privateByDefault

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
                  IntroducedBindings = []
                  IntroducedBorrowLocks = []
                  IntroducedUsingObligations = [] }
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
            let state =
                scope.IntroducedUsingObligations
                |> List.fold
                    (fun current obligationId ->
                        match Map.tryFind obligationId current.UsingObligations with
                        | Some obligation ->
                            match Map.tryFind obligation.HiddenOwnedBindingId current.Bindings with
                            | Some binding ->
                                let updatedBinding =
                                    { binding with
                                        ConsumedPaths =
                                            if binding.ConsumedPaths |> List.contains [] then
                                                binding.ConsumedPaths
                                            else
                                                [] :: binding.ConsumedPaths
                                        UseMinimum = max binding.UseMinimum 1
                                        UseMaximum = max binding.UseMaximum 1
                                        FirstConsumeOrigin = binding.FirstConsumeOrigin }

                                let releaseEvent: OwnershipUseFact =
                                    { UseId = $"{currentScopeId current}.u{current.NextEventId}"
                                      UseKind = OwnershipUseKind.Release
                                      UseTargetBindingId = Some binding.Id
                                      UseTargetName = binding.Name
                                      UsePlaceRoot = binding.Place.Root
                                      UsePlacePath = binding.Place.Path
                                      UseOrigin = obligation.SurfaceOrigin }

                                { current with
                                    Bindings = Map.add binding.Id updatedBinding current.Bindings
                                    Events = current.Events @ [ releaseEvent ]
                                    NextEventId = current.NextEventId + 1 }
                            | None ->
                                current
                        | None ->
                            current)
                    state

            let activeBindingIds =
                scope.IntroducedBindings
                |> List.fold (fun current (name, _) -> popActiveBinding name current) state.ActiveBindingIds

            let borrowLocks =
                scope.IntroducedBorrowLocks
                |> List.fold (fun current lockId -> Map.remove lockId current) state.BorrowLocks

            let usingObligations =
                scope.IntroducedUsingObligations
                |> List.fold (fun current obligationId -> Map.remove obligationId current) state.UsingObligations

            { state with
                ActiveScopes = rest
                ActiveBindingIds = activeBindingIds
                BorrowLocks = borrowLocks
                UsingObligations = usingObligations }

    let private withScope label checker state =
        state
        |> pushScope label
        |> checker
        |> popScope

    let private tryFindBindingId name (state: CheckState) =
        state.ActiveBindingIds
        |> Map.tryFind name
        |> Option.bind List.tryHead

    let private tryFindBindingIdExcluding excludedBindingId name (state: CheckState) =
        state.ActiveBindingIds
        |> Map.tryFind name
        |> Option.bind (List.tryFind (fun bindingId -> not (String.Equals(bindingId, excludedBindingId, StringComparison.Ordinal))))

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

    let private closureCaptureTransfersOwnership quantity =
        match quantity with
        | Some(ResourceQuantity.Interval(0, Some 0))
        | Some(ResourceQuantity.Interval(0, None))
        | Some(ResourceQuantity.Borrow _) ->
            false
        | Some(ResourceQuantity.Interval _)
        | Some(ResourceQuantity.Variable _) ->
            true
        | None ->
            false

    let private isExactOneBinding (binding: ResourceBinding) =
        binding.DeclaredQuantity
        |> Option.exists ResourceQuantity.isExactOne

    let private bindingConsumptionAdvanced before after =
        after.UseMinimum > before.UseMinimum
        || after.UseMaximum > before.UseMaximum
        || List.length after.ConsumedPaths > List.length before.ConsumedPaths

    let private quantityFact quantity =
        quantity |> Option.map ResourceQuantity.toOwnershipQuantity

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

    let private refType argumentType =
        TypeSignatures.TypeName([ "Ref" ], [ argumentType ])

    let private unwrapIoType typeExpr =
        match typeExpr with
        | TypeSignatures.TypeName([ "IO" ], [ inner ]) -> inner
        | TypeSignatures.TypeName([ "IO" ], [ _; inner ]) -> inner
        | TypeSignatures.TypeName([ "UIO" ], [ inner ]) -> inner
        | other -> other

    let private unwrapBindPayloadType typeExpr =
        match typeExpr with
        | TypeSignatures.TypeName(_, [ inner ]) -> inner
        | other -> unwrapIoType other

    let private inferLambdaBindingQuantity (capturedBindings: ResourceBinding list) =
        capturedBindings
        |> List.fold (fun current binding -> combineLambdaBindingQuantity current binding.DeclaredQuantity) None

    let private bindingDemand (binding: ResourceBinding) =
        match binding.BorrowRegion with
        | Some _ -> OwnershipBindingDemand.Borrow
        | None -> OwnershipBindingDemand.Interval(binding.UseMinimum, binding.UseMaximum)

    let private bindingState (binding: ResourceBinding) =
        match binding.BorrowRegion with
        | Some _ -> OwnershipBindingState.Borrowed
        | None when binding.UseMaximum > 0 || not (List.isEmpty binding.ConsumedPaths) -> OwnershipBindingState.Consumed
        | None when binding.CheckLinearDrop -> OwnershipBindingState.Unconsumed
        | None -> OwnershipBindingState.Available

    let private wouldOveruseBinding (binding: ResourceBinding) =
        let capability =
            binding.DeclaredQuantity
            |> Option.defaultValue ResourceQuantity.omega

        let nextDemand = ResourceQuantity.exact (binding.UseMaximum + 1)
        not (ResourceQuantity.satisfies capability nextDemand)

    let private bindingFact (binding: ResourceBinding) =
        { BindingId = binding.Id
          BindingName = binding.Name
          BindingKind = ResourceBindingKind.toOwnershipKind binding.BindingKind
          BindingDeclaredQuantity = quantityFact binding.DeclaredQuantity
          BindingInferredDemand = bindingDemand binding
          BindingState = bindingState binding
          BindingPlaceRoot = binding.Place.Root
          BindingPlacePath = binding.Place.Path
          BindingBorrowRegionId = binding.BorrowRegion |> Option.map (fun region -> region.Id)
          BindingOrigin = binding.Origin }

    let private addEventAtPlace useKind origin placeRoot placePath (binding: ResourceBinding) (state: CheckState) =
        let event: OwnershipUseFact =
            { UseId = $"{currentScopeId state}.u{state.NextEventId}"
              UseKind = useKind
              UseTargetBindingId = Some binding.Id
              UseTargetName = binding.Name
              UsePlaceRoot = placeRoot
              UsePlacePath = placePath
              UseOrigin = origin }

        { state with
            Events = state.Events @ [ event ]
            NextEventId = state.NextEventId + 1 }

    let private addEvent useKind origin (binding: ResourceBinding) (state: CheckState) =
        addEventAtPlace useKind origin binding.Place.Root binding.Place.Path binding state

    let private addCopyForbiddenEventAtPlace origin placeRoot placePath binding state =
        addEventAtPlace OwnershipUseKind.CopyForbidden origin placeRoot placePath binding state

    let private addNamedEvent useKind origin name (state: CheckState) =
        match tryFindBinding name state with
        | Some binding -> addEvent useKind origin binding state
        | None ->
            let event: OwnershipUseFact =
                { UseId = $"{currentScopeId state}.u{state.NextEventId}"
                  UseKind = useKind
                  UseTargetBindingId = None
                  UseTargetName = name
                  UsePlaceRoot = name
                  UsePlacePath = []
                  UseOrigin = origin }

            { state with
                Events = state.Events @ [ event ]
                NextEventId = state.NextEventId + 1 }

    let private addNonConsumingDemand document name state =
        let origin = findUseLocation document name 1

        match tryFindBinding name state with
        | Some binding ->
            state
            |> addEvent OwnershipUseKind.Borrow origin binding
            |> updateBinding binding.Id (fun current ->
                { current with
                    UseMinimum = max current.UseMinimum 1 })
        | None ->
            addNamedEvent OwnershipUseKind.Borrow origin name state

    let private addBorrowRegionFact introductionOrigin (region: BorrowRegion) state =
        let fact: OwnershipBorrowRegionFact =
            { BorrowRegionId = region.Id
              BorrowRegionExplicitName = region.ExplicitName
              BorrowRegionIntroductionOrigin = introductionOrigin
              BorrowRegionOwnerScope = region.OwnerScope }

        { state with
            BorrowRegions = state.BorrowRegions @ [ fact ] }

    let private introduceBorrowRegion ownerScope explicitRegion introductionOrigin (state: CheckState) =
        let region: BorrowRegion =
            { Id = defaultArg explicitRegion $"rho{state.NextRegionId}"
              ExplicitName = explicitRegion
              OwnerScope = ownerScope }

        let nextState =
            match explicitRegion with
            | Some _ -> state
            | None -> { state with NextRegionId = state.NextRegionId + 1 }

        region, addBorrowRegionFact introductionOrigin region nextState

    let private introduceBorrowRegionForQuantity ownerScope quantity introductionOrigin state =
        match quantity with
        | Some(ResourceQuantity.Borrow explicitRegion) ->
            let region, state = introduceBorrowRegion ownerScope explicitRegion introductionOrigin state
            Some region, state
        | _ -> None, state

    let private addUsingScopeFact usingScopeId surfaceOrigin hiddenOwnedBinding hiddenReleaseObligation (region: BorrowRegion) state =
        let fact: OwnershipUsingScopeFact =
            { UsingScopeId = usingScopeId
              UsingScopeSurfaceOrigin = surfaceOrigin
              UsingScopeHiddenOwnedBinding = hiddenOwnedBinding
              UsingScopeSharedRegionId = region.Id
              UsingScopeHiddenReleaseObligation = hiddenReleaseObligation }

        { state with
            UsingScopes = state.UsingScopes @ [ fact ] }

    let private addUsingObligation obligationId scopeId hiddenOwnedBindingId hiddenOwnedBindingName sharedRegionId surfaceOrigin (state: CheckState) =
        let obligation: UsingObligation =
            { Id = obligationId
              ScopeId = scopeId
              HiddenOwnedBindingId = hiddenOwnedBindingId
              HiddenOwnedBindingName = hiddenOwnedBindingName
              SharedRegionId = sharedRegionId
              SurfaceOrigin = surfaceOrigin }

        let activeScopes =
            match state.ActiveScopes with
            | currentScope :: rest ->
                { currentScope with
                    IntroducedUsingObligations = obligationId :: currentScope.IntroducedUsingObligations }
                :: rest
            | [] ->
                []

        { state with
            ActiveScopes = activeScopes
            UsingObligations = Map.add obligationId obligation state.UsingObligations }

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
              ClosureEscapeStatus = OwnershipClosureEscapeStatus.Contained
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
                    { closure with ClosureEscapeStatus = OwnershipClosureEscapeStatus.Escaped }
                else
                    closure)

        { state with Closures = closures }

    let private collectPatternBindings (pattern: SurfacePattern) =
        let rec loop path current =
            seq {
                match current with
                | NamePattern name ->
                    yield name, path
                | AsPattern(name, inner) ->
                    yield name, path
                    yield! loop path inner
                | TypedPattern(inner, _) ->
                    yield! loop path inner
                | ConstructorPattern(_, arguments) ->
                    for index, argument in arguments |> List.indexed do
                        yield! loop (path @ [ $"#{index}" ]) argument
                | NamedConstructorPattern(_, fields) ->
                    for field in fields do
                        yield! loop (path @ [ field.Name ]) field.Pattern
                | TuplePattern elements ->
                    for index, element in elements |> List.indexed do
                        yield! loop (path @ [ $"_{index + 1}" ]) element
                | VariantPattern(BoundVariantPattern(name, _))
                | VariantPattern(RestVariantPattern name) ->
                    yield name, path
                | VariantPattern(WildcardVariantPattern _) ->
                    ()
                | OrPattern alternatives ->
                    match alternatives with
                    | first :: _ ->
                        yield! loop path first
                    | [] ->
                        ()
                | AnonymousRecordPattern(fields, rest) ->
                    for field in fields do
                        yield! loop (path @ [ field.Name ]) field.Pattern
                    match rest with
                    | Some(BindRecordPatternRest name) -> yield name, path
                    | _ -> ()
                | WildcardPattern
                | LiteralPattern _ -> ()
            }

        loop [] pattern |> Seq.toList

    let private collectPatternNames (pattern: SurfacePattern) =
        collectPatternBindings pattern |> List.map fst

    let private bindPatternNameLocation (document: ParsedDocument) (binding: SurfaceBindPattern) name =
        binding.BinderSpans
        |> Map.tryFind name
        |> Option.bind List.tryHead
        |> Option.map document.Source.GetLocation
        |> Option.orElseWith (fun () -> findBinderLocation document name)

    let private bindPatternIntroductionOrigin (document: ParsedDocument) (binding: SurfaceBindPattern) =
        collectPatternNames binding.Pattern
        |> List.tryPick (bindPatternNameLocation document binding)

    let private tryDirectSyntaxSpliceBody expression =
        match expression with
        | TopLevelSyntaxSplice(SyntaxQuote inner)
        | TopLevelSyntaxSplice(SyntaxSplice inner) ->
            Some inner
        | _ ->
            None

    let rec private expressionNames (expression: SurfaceExpression) =
        let expressionNamesInComprehension (comprehension: SurfaceComprehension) =
            let yieldNames shadowed =
                seq {
                    match comprehension.Yield with
                    | YieldValue value ->
                        for name in expressionNames value do
                            if not (Set.contains name shadowed) then
                                yield name
                    | YieldKeyValue(key, value) ->
                        for expression in [ key; value ] do
                            for name in expressionNames expression do
                                if not (Set.contains name shadowed) then
                                    yield name
                }

            let rec loop shadowed clauses =
                seq {
                    match clauses with
                    | [] ->
                        yield! yieldNames shadowed
                    | clause :: rest ->
                        match clause with
                        | ForClause(_, _, binding, source) ->
                            for name in expressionNames source do
                                if not (Set.contains name shadowed) then
                                    yield name

                            let nextShadowed =
                                shadowed
                                |> Set.union (collectPatternNames binding.Pattern |> Set.ofList)

                            yield! loop nextShadowed rest
                        | JoinClause(binding, source, condition) ->
                            for expression in [ source; condition ] do
                                for name in expressionNames expression do
                                    if not (Set.contains name shadowed) then
                                        yield name

                            let nextShadowed =
                                shadowed
                                |> Set.union (collectPatternNames binding.Pattern |> Set.ofList)

                            yield! loop nextShadowed rest
                        | LetClause(_, binding, value) ->
                            for name in expressionNames value do
                                if not (Set.contains name shadowed) then
                                    yield name

                            let nextShadowed =
                                shadowed
                                |> Set.union (collectPatternNames binding.Pattern |> Set.ofList)

                            yield! loop nextShadowed rest
                        | IfClause condition ->
                            for name in expressionNames condition do
                                if not (Set.contains name shadowed) then
                                    yield name
                            yield! loop shadowed rest
                        | GroupByClause(key, aggregations, intoName) ->
                            for name in expressionNames key do
                                if not (Set.contains name shadowed) then
                                    yield name
                            for field in aggregations do
                                for name in expressionNames field.Value do
                                    if not (Set.contains name shadowed) then
                                        yield name
                            yield! loop (Set.singleton intoName) rest
                        | OrderByClause(_, key) ->
                            for name in expressionNames key do
                                if not (Set.contains name shadowed) then
                                    yield name
                            yield! loop shadowed rest
                        | DistinctClause ->
                            yield! loop shadowed rest
                        | DistinctByClause key ->
                            for name in expressionNames key do
                                if not (Set.contains name shadowed) then
                                    yield name
                            yield! loop shadowed rest
                        | SkipClause count ->
                            for name in expressionNames count do
                                if not (Set.contains name shadowed) then
                                    yield name
                            yield! loop shadowed rest
                        | TakeClause count ->
                            for name in expressionNames count do
                                if not (Set.contains name shadowed) then
                                    yield name
                            yield! loop shadowed rest
                        | LeftJoinClause(binding, source, condition, intoName) ->
                            for name in expressionNames source do
                                if not (Set.contains name shadowed) then
                                    yield name

                            let joinShadowed =
                                shadowed
                                |> Set.union (collectPatternNames binding.Pattern |> Set.ofList)

                            for name in expressionNames condition do
                                if not (Set.contains name joinShadowed) then
                                    yield name

                            yield! loop (Set.add intoName shadowed) rest
                }

            loop Set.empty comprehension.Clauses

        seq {
            match expression with
            | Literal _ -> ()
            | NumericLiteral _ -> ()
            | KindQualifiedName _ -> ()
            | Name(root :: _) -> yield root
            | Name [] -> ()
            | SyntaxQuote _
            | SyntaxSplice _ ->
                ()
            | TopLevelSyntaxSplice inner ->
                match tryDirectSyntaxSpliceBody expression with
                | Some quotedBody ->
                    yield! expressionNames quotedBody
                | None ->
                    yield! expressionNames inner
            | CodeQuote inner
            | CodeSplice inner ->
                yield! expressionNames inner
            | LocalLet(binding, value, body) ->
                yield! expressionNames value

                let boundNames =
                    collectPatternNames binding.Pattern
                    |> Set.ofList

                for name in expressionNames body do
                    if not (Set.contains name boundNames) then
                        yield name
            | LocalSignature(declaration, body) ->
                for name in expressionNames body do
                    if not (String.Equals(name, declaration.Name, StringComparison.Ordinal)) then
                        yield name
            | LocalTypeAlias(_, body) ->
                yield! expressionNames body
            | LocalScopedEffect(_, body) ->
                yield! expressionNames body
            | Handle(_, label, body, returnClause, operationClauses) ->
                yield! expressionNames label
                yield! expressionNames body
                yield! expressionNames returnClause.Body

                for clause in operationClauses do
                    yield! expressionNames clause.Body
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
            | RecordLiteral fields ->
                for field in fields do
                    yield! expressionNames field.Value
            | Seal(value, _) ->
                yield! expressionNames value
            | RecordUpdate(receiver, fields) ->
                yield! expressionNames receiver

                for field in fields do
                    yield! expressionNames field.Value
            | MemberAccess(receiver, _, arguments) ->
                yield! expressionNames receiver

                for argument in arguments do
                    yield! expressionNames argument
            | SafeNavigation(receiver, navigation) ->
                yield! expressionNames receiver

                for argument in navigation.Arguments do
                    yield! expressionNames argument
            | TagTest(receiver, _) ->
                yield! expressionNames receiver
            | Do statements ->
                for statement in statements do
                    yield! doStatementNames statement
            | MonadicSplice inner
            | ExplicitImplicitArgument inner
            | InoutArgument inner
            | Unary(_, inner) ->
                yield! expressionNames inner
            | NamedApplicationBlock fields ->
                for field in fields do
                    yield! expressionNames field.Value
            | Apply(callee, arguments) ->
                yield! expressionNames callee

                for argument in arguments do
                    yield! expressionNames argument
            | Binary(left, _, right) ->
                yield! expressionNames left
                yield! expressionNames right
            | Elvis(left, right) ->
                yield! expressionNames left
                yield! expressionNames right
            | Comprehension comprehension ->
                yield! expressionNamesInComprehension comprehension
            | PrefixedString(_, parts) ->
                for part in parts do
                    match part with
                    | StringText _ -> ()
                    | StringInterpolation(inner, _) -> yield! expressionNames inner
        }

    and private doStatementNames (statement: SurfaceDoStatement) =
        seq {
            match statement with
            | DoLet(_, expression)
            | DoBind(_, expression)
            | DoVar(_, expression)
            | DoAssign(_, expression)
            | DoUsing(_, expression)
            | DoDefer expression
            | DoReturn expression
            | DoExpression expression ->
                yield! expressionNames expression
            | DoLetQuestion(_, expression, failure) ->
                yield! expressionNames expression

                match failure with
                | Some failure ->
                    for statement in failure.Body do
                        yield! doStatementNames statement
                | None -> ()
            | DoIf(condition, whenTrue, whenFalse) ->
                yield! expressionNames condition

                for statement in whenTrue do
                    yield! doStatementNames statement

                for statement in whenFalse do
                    yield! doStatementNames statement
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

    let private continuationCaptureHazards (state: CheckState) (statements: SurfaceDoStatement list) =
        let riskyBindings =
            Do statements
            |> capturedBindings state
            |> List.filter (fun binding -> isExactOneBinding binding || binding.BorrowRegion.IsSome)

        let linearNames =
            riskyBindings
            |> List.filter isExactOneBinding
            |> List.map (fun binding -> binding.Name)
            |> List.distinct

        let borrowedNames =
            riskyBindings
            |> List.filter (fun binding -> binding.BorrowRegion.IsSome)
            |> List.map (fun binding -> binding.Name)
            |> List.distinct

        riskyBindings, linearNames, borrowedNames

    let private tryFindMultiShotScopedOperation expression =
        let tryResolveHandledEffectName expression =
            match expression with
            | Name [ effectName ]
            | KindQualifiedName(EffectLabelKind, [ effectName ]) ->
                tryFindScopedEffectDeclaration effectName |> Option.map (fun _ -> effectName)
            | Name nameSegments when not (List.isEmpty nameSegments) ->
                let qualifiedName = SyntaxFacts.moduleNameToText nameSegments
                tryFindScopedEffectDeclaration qualifiedName |> Option.map (fun _ -> qualifiedName)
            | _ ->
                None

        let tryResolveScopedOperation effectName operationName =
            tryFindScopedEffectDeclaration effectName
            |> Option.bind (fun declaration ->
                declaration.Operations
                |> List.tryFind (fun operation -> String.Equals(operation.Name, operationName, StringComparison.Ordinal))
                |> Option.map (fun operation -> declaration, operation))

        match expression with
        | Apply(Name nameSegments, _) ->
            match List.rev nameSegments with
            | operationName :: reversedEffectSegments when not (List.isEmpty reversedEffectSegments) ->
                let effectName = List.rev reversedEffectSegments |> SyntaxFacts.moduleNameToText

                tryResolveScopedOperation effectName operationName
                |> Option.filter (fun (_, operation) -> isMultiShotResumptionQuantity operation.ResumptionQuantity)
                |> Option.map (fun (_, operation) -> effectName, operation)
            | _ ->
                None
        | Apply(MemberAccess(receiver, [ operationName ], []), _) ->
            tryResolveHandledEffectName receiver
            |> Option.bind (fun effectName ->
                tryResolveScopedOperation effectName operationName
                |> Option.filter (fun (_, operation) -> isMultiShotResumptionQuantity operation.ResumptionQuantity)
                |> Option.map (fun (_, operation) -> effectName, operation))
        | _ ->
            None

    let private tryParseHandlerArgumentName (tokens: Token list) =
        let significantTokens =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        match significantTokens with
        | [ token ] when Token.isName token ->
            let name = SyntaxFacts.trimIdentifierQuotes token.Text

            if String.Equals(name, "_", StringComparison.Ordinal) then
                None
            else
                Some name
        | leftParen :: nameToken :: colonToken :: _
            when leftParen.Kind = LeftParen && Token.isName nameToken && colonToken.Kind = Colon ->
            let name = SyntaxFacts.trimIdentifierQuotes nameToken.Text

            if String.Equals(name, "_", StringComparison.Ordinal) then
                None
            else
                Some name
        | _ ->
            None

    let private inferredClosureQuantity state expression =
        match expression with
        | Name [ name ] ->
            tryFindBinding name state
            |> Option.bind (fun binding ->
                match binding.LocalLambda, binding.ClosureFactId with
                | Some _, _
                | _, Some _ ->
                    binding.DeclaredQuantity
                | _ ->
                    None)
        | Lambda _ ->
            capturedBindings state expression
            |> inferLambdaBindingQuantity
        | _ ->
            None

    let private tryDelayedExpressionBody expression =
        match expression with
        | Apply(Name [ calleeName ], [ body ])
            when String.Equals(calleeName, "thunk", StringComparison.Ordinal)
                 || String.Equals(calleeName, "lazy", StringComparison.Ordinal) ->
            Some body
        | _ ->
            None

    let private tryForcedExpressionArgument expression =
        match expression with
        | Apply(Name [ calleeName ], [ argument ]) when String.Equals(calleeName, "force", StringComparison.Ordinal) ->
            Some argument
        | _ ->
            None

    let private lowerBoundIsPositive quantity =
        quantity |> Option.exists ResourceQuantity.requiresUse

    let private demandedParameterQuantity quantity =
        match quantity with
        | Some(Some demandQuantity) -> Some demandQuantity
        | Some None -> Some ResourceQuantity.omega
        | None -> None

    let private quantityFromFunctionSignatureQuantity quantity =
        match quantity with
        | ResourceQuantity.Interval(0, Some 0) -> QuantityZero
        | ResourceQuantity.Interval(1, Some 1) -> QuantityOne
        | ResourceQuantity.Interval(0, None) -> QuantityOmega
        | ResourceQuantity.Interval(0, Some 1) -> QuantityAtMostOne
        | ResourceQuantity.Interval(1, None) -> QuantityAtLeastOne
        | ResourceQuantity.Borrow explicitRegion -> QuantityBorrow explicitRegion
        | ResourceQuantity.Variable name -> QuantityVariable name
        | ResourceQuantity.Interval(minimum, Some maximum) when minimum = maximum ->
            if minimum = 0 then QuantityZero elif minimum = 1 then QuantityOne else QuantityVariable(ResourceQuantity.toSurfaceText quantity)
        | _ ->
            QuantityVariable(ResourceQuantity.toSurfaceText quantity)

    let private tryBuildFunctionTypeFromSignature (signature: FunctionSignature) =
        let returnType = signature.ReturnTypeTokens |> Option.bind TypeSignatures.parseType

        let rec build index currentResult =
            if index < 0 then
                Some currentResult
            else
                match signature.ParameterTypeTokens |> List.tryItem index |> Option.flatten with
                | Some parameterTokens ->
                    match TypeSignatures.parseType parameterTokens with
                    | Some parameterType ->
                        let quantity =
                            signature.ParameterQuantities
                            |> List.tryItem index
                            |> Option.flatten
                            |> Option.map quantityFromFunctionSignatureQuantity
                            |> Option.defaultValue QuantityOmega

                        build (index - 1) (TypeSignatures.TypeArrow(quantity, parameterType, currentResult))
                    | None ->
                        None
                | None ->
                    None

        match returnType with
        | Some resultType ->
            build (signature.ParameterTypeTokens.Length - 1) resultType
        | None ->
            None

    let private functionParameterInfoFromType typeExpr =
        let rec loop current quantities parameterTypes =
            match current with
            | TypeSignatures.TypeArrow(quantity, parameterType, resultType) ->
                loop
                    resultType
                    (Some(ResourceQuantity.ofSurface quantity) :: quantities)
                    (Some parameterType :: parameterTypes)
            | _ ->
                List.rev quantities, List.rev parameterTypes

        loop typeExpr [] []

    let private tryInferConservativeExpressionType
        (signatures: Map<string, FunctionSignature>)
        (localTypes: Map<string, TypeSignatures.TypeExpr>)
        expression
        =
        let tryOptionPayloadType typeExpr =
            match typeExpr with
            | TypeSignatures.TypeName([ "Option" ], [ payloadType ])
            | TypeSignatures.TypeName([ "std"; "prelude"; "Option" ], [ payloadType ]) ->
                Some payloadType
            | _ ->
                None

        let stringType = TypeSignatures.TypeName([ "String" ], [])
        let unicodeScalarType = TypeSignatures.TypeName([ "UnicodeScalar" ], [])
        let graphemeType = TypeSignatures.TypeName([ "Grapheme" ], [])
        let byteType = TypeSignatures.TypeName([ "Byte" ], [])
        let unitType = TypeSignatures.TypeName([ "Unit" ], [])
        let intType = TypeSignatures.TypeName([ "Int" ], [])
        let doubleType = TypeSignatures.TypeName([ "Double" ], [])

        let rec tryProjectRecordType currentType path =
            match path, currentType with
            | [], _ ->
                Some currentType
            | fieldName :: rest, TypeSignatures.TypeRecord fields ->
                fields
                |> List.tryFind (fun field -> String.Equals(field.Name, fieldName, StringComparison.Ordinal))
                |> Option.bind (fun field -> tryProjectRecordType field.Type rest)
            | _ ->
                None

        let rec loop locals current =
            match current with
            | Name(root :: path) ->
                locals
                |> Map.tryFind root
                |> Option.orElseWith (fun () ->
                    signatures
                    |> Map.tryFind root
                    |> Option.bind tryBuildFunctionTypeFromSignature)
                |> Option.bind (fun rootType -> tryProjectRecordType rootType path)
            | Name [] ->
                None
            | Literal(String _) ->
                Some stringType
            | Literal(Character _) ->
                Some unicodeScalarType
            | Literal(Grapheme _) ->
                Some graphemeType
            | Literal(Byte _) ->
                Some byteType
            | Literal(Unit) ->
                Some unitType
            | NumericLiteral(SurfaceIntegerLiteral(_, _, None)) ->
                Some intType
            | NumericLiteral(SurfaceRealLiteral(_, _, None)) ->
                Some doubleType
            | RecordLiteral fields ->
                let inferredFields =
                    fields
                    |> List.map (fun field ->
                        loop locals field.Value
                        |> Option.map (fun fieldType ->
                            ({ Name = field.Name
                               Quantity = if field.IsImplicit then QuantityZero else QuantityOmega
                               Type = fieldType }: TypeSignatures.RecordField)))

                if inferredFields |> List.forall Option.isSome then
                    inferredFields
                    |> List.choose id
                    |> TypeSignatures.TypeRecord
                    |> Some
                else
                    None
            | Seal(value, _) ->
                loop locals value
            | Apply(callee, arguments) ->
                loop locals callee
                |> Option.bind (fun calleeType -> tryApplyType locals calleeType arguments)
            | Lambda(parameters, body) ->
                let parameterTypes =
                    parameters
                    |> List.choose (fun parameter ->
                        parameter.TypeTokens
                        |> Option.bind TypeSignatures.parseType
                        |> Option.map (fun parameterType -> parameter, parameterType))

                if List.length parameterTypes <> List.length parameters then
                    None
                else
                    let lambdaLocals =
                        parameterTypes
                        |> List.fold (fun state (parameter, parameterType) -> Map.add parameter.Name parameterType state) locals

                    loop lambdaLocals body
                    |> Option.map (fun resultType ->
                        parameterTypes
                        |> List.rev
                        |> List.fold (fun state (parameter, parameterType) ->
                            let quantity =
                                if parameter.IsInout then
                                    QuantityOne
                                else
                                    parameter.Quantity |> Option.defaultValue QuantityOmega

                            TypeSignatures.TypeArrow(quantity, parameterType, state)) resultType)
            | SafeNavigation(receiver, navigation) ->
                loop locals receiver
                |> Option.bind (fun receiverType ->
                    tryOptionPayloadType receiverType
                    |> Option.bind (fun payloadType ->
                        tryProjectRecordType payloadType navigation.Segments
                        |> Option.bind (fun memberType -> tryApplyType locals memberType navigation.Arguments)
                        |> Option.map (fun memberType ->
                            match tryOptionPayloadType memberType with
                            | Some _ -> memberType
                            | None -> TypeSignatures.TypeName([ "Option" ], [ memberType ]))))
            | Elvis(left, right) ->
                match loop locals left, loop locals right with
                | Some leftType, Some defaultType ->
                    tryOptionPayloadType leftType
                    |> Option.filter (fun payloadType -> payloadType = defaultType)
                | _ ->
                    None
            | _ ->
                None

        and tryApplyType locals currentType arguments =
            match currentType, arguments with
            | currentType, [] ->
                Some currentType
            | TypeSignatures.TypeArrow(_, parameterType, resultType), argument :: rest ->
                match loop locals argument with
                | Some argumentType ->
                    match TypeSignatures.tryUnifyMany [ parameterType, argumentType ] with
                    | Some substitution ->
                        let nextResultType = TypeSignatures.applySubstitution substitution resultType
                        tryApplyType locals nextResultType rest
                    | None ->
                        None
                | None ->
                    None
            | _ ->
                None

        loop localTypes expression

    let private extendPatternLocalTypes
        (signatures: Map<string, FunctionSignature>)
        (localTypes: Map<string, TypeSignatures.TypeExpr>)
        expectedType
        pattern
        =
        let tryLookupSignature nameSegments =
            let qualifiedName = SyntaxFacts.moduleNameToText nameSegments

            signatures
            |> Map.tryFind qualifiedName
            |> Option.orElseWith (fun () ->
                match List.rev nameSegments with
                | name :: _ -> Map.tryFind name signatures
                | [] -> None)

        let rec splitArrowTypes current =
            match current with
            | TypeSignatures.TypeArrow(_, parameterType, resultType) ->
                let parameterTypes, finalResultType = splitArrowTypes resultType
                parameterType :: parameterTypes, finalResultType
            | _ ->
                [], current

        let rec loop locals expected currentPattern =
            match currentPattern with
            | WildcardPattern
            | LiteralPattern _ ->
                locals
            | NamePattern name ->
                match expected with
                | Some expectedType -> Map.add name expectedType locals
                | None -> locals
            | AsPattern(name, inner) ->
                let locals =
                    match expected with
                    | Some expectedType -> Map.add name expectedType locals
                    | None -> locals

                loop locals expected inner
            | TypedPattern(inner, typeTokens) ->
                loop locals (TypeSignatures.parseType typeTokens |> Option.orElse expected) inner
            | TuplePattern elements ->
                let tupleFields =
                    elements
                    |> List.mapi (fun index element ->
                        { Name = $"_{index + 1}"
                          IsImplicit = false
                          Pattern = element })

                loop locals expected (AnonymousRecordPattern(tupleFields, None))
            | AnonymousRecordPattern(fields, rest) ->
                let locals =
                    match expected with
                    | Some(TypeSignatures.TypeRecord recordFields) ->
                        fields
                        |> List.fold (fun current field ->
                            let fieldType =
                                recordFields
                                |> List.tryFind (fun recordField -> String.Equals(recordField.Name, field.Name, StringComparison.Ordinal))
                                |> Option.map (fun recordField -> recordField.Type)

                            loop current fieldType field.Pattern) locals
                    | _ ->
                        fields |> List.fold (fun current field -> loop current None field.Pattern) locals

                match rest with
                | Some(BindRecordPatternRest name) ->
                    match expected with
                    | Some(TypeSignatures.TypeRecord recordFields) ->
                        let mentioned = fields |> List.map (fun field -> field.Name) |> Set.ofList
                        let residualFields =
                            recordFields
                            |> List.filter (fun field -> not (Set.contains field.Name mentioned))

                        Map.add name (TypeSignatures.TypeRecord residualFields) locals
                    | _ ->
                        Map.add name (TypeSignatures.TypeRecord []) locals
                | _ ->
                    locals
            | ConstructorPattern(nameSegments, arguments) ->
                let argumentTypes =
                    match expected, tryLookupSignature nameSegments |> Option.bind tryBuildFunctionTypeFromSignature with
                    | Some expectedType, Some constructorType ->
                        let parameterTypes, resultType = splitArrowTypes constructorType

                        match TypeSignatures.tryUnifyMany [ resultType, expectedType ] with
                        | Some substitution when List.length parameterTypes = arguments.Length ->
                            parameterTypes
                            |> List.map (TypeSignatures.applySubstitution substitution >> Some)
                        | None ->
                            List.replicate arguments.Length None
                        | Some _ ->
                            List.replicate arguments.Length None
                    | _ ->
                        List.replicate arguments.Length None

                List.zip arguments argumentTypes
                |> List.fold (fun current (argumentPattern, argumentType) -> loop current argumentType argumentPattern) locals
            | NamedConstructorPattern(_, fields) ->
                fields |> List.fold (fun current field -> loop current None field.Pattern) locals
            | VariantPattern(BoundVariantPattern(name, Some typeTokens)) ->
                match TypeSignatures.parseType typeTokens with
                | Some parsedType -> Map.add name parsedType locals
                | None -> locals
            | VariantPattern(BoundVariantPattern(name, None))
            | VariantPattern(RestVariantPattern name) ->
                match expected with
                | Some expectedType -> Map.add name expectedType locals
                | None -> locals
            | VariantPattern(WildcardVariantPattern _) ->
                locals
            | OrPattern alternatives ->
                match alternatives with
                | first :: _ -> loop locals expected first
                | [] -> locals

        loop localTypes expectedType pattern

    let private extendBindingLocalTypes
        (signatures: Map<string, FunctionSignature>)
        (localTypes: Map<string, TypeSignatures.TypeExpr>)
        valueType
        (binding: SurfaceBindPattern)
        =
        let valueType =
            binding.TypeTokens
            |> Option.bind TypeSignatures.parseType
            |> Option.orElse valueType

        extendPatternLocalTypes signatures localTypes valueType binding.Pattern

    let private inferConservativeFallbackBindingType value =
        match value with
        | Lambda(parameters, _) ->
            let resultType = TypeSignatures.TypeVariable "__kappa_shadow_result"

            parameters
            |> List.rev
            |> List.fold
                (fun current parameter ->
                    let parameterType =
                        parameter.TypeTokens
                        |> Option.bind TypeSignatures.parseType
                        |> Option.defaultValue (TypeSignatures.TypeVariable $"__kappa_shadow_param_{parameter.Name}")

                    let quantity =
                        if parameter.IsInout then
                            QuantityOne
                        else
                            parameter.Quantity |> Option.defaultValue QuantityOmega

                    TypeSignatures.TypeArrow(quantity, parameterType, current))
                resultType
        | _ ->
            TypeSignatures.TypeVariable "__kappa_shadow_local"

    let rec private typeContainsCaptureAnnotation typeExpr =
        match typeExpr with
        | TypeSignatures.TypeCapture _ ->
            true
        | TypeSignatures.TypeApply(callee, arguments) ->
            typeContainsCaptureAnnotation callee || (arguments |> List.exists typeContainsCaptureAnnotation)
        | TypeSignatures.TypeLambda(_, parameterSort, body) ->
            typeContainsCaptureAnnotation parameterSort || typeContainsCaptureAnnotation body
        | TypeSignatures.TypeDelay inner
        | TypeSignatures.TypeMemo inner
        | TypeSignatures.TypeForce inner ->
            typeContainsCaptureAnnotation inner
        | TypeSignatures.TypeProject(target, _) ->
            typeContainsCaptureAnnotation target
        | TypeSignatures.TypeArrow(_, parameterType, resultType) ->
            typeContainsCaptureAnnotation parameterType || typeContainsCaptureAnnotation resultType
        | TypeSignatures.TypeEquality(left, right) ->
            typeContainsCaptureAnnotation left || typeContainsCaptureAnnotation right
        | TypeSignatures.TypeName(_, arguments) ->
            arguments |> List.exists typeContainsCaptureAnnotation
        | TypeSignatures.TypeEffectRow(entries, tail) ->
            entries |> List.exists (fun entry -> typeContainsCaptureAnnotation entry.Label || typeContainsCaptureAnnotation entry.Effect)
            || tail |> Option.exists typeContainsCaptureAnnotation
        | TypeSignatures.TypeRecord fields ->
            fields |> List.exists (fun field -> typeContainsCaptureAnnotation field.Type)
        | TypeSignatures.TypeUnion members ->
            members |> List.exists typeContainsCaptureAnnotation
        | TypeSignatures.TypeLevelLiteral _
        | TypeSignatures.TypeUniverse _
        | TypeSignatures.TypeIntrinsic _
        | TypeSignatures.TypeVariable _ ->
            false

    let private captureSetFromTypeWithAliases typeAliasMap typeExpr =
        let rec loop visited current =
            seq {
                match current with
                | TypeSignatures.TypeCapture(inner, captures) ->
                    yield! captures
                    yield! loop visited inner
                | TypeSignatures.TypeApply(callee, arguments) ->
                    yield! loop visited callee

                    for argument in arguments do
                        yield! loop visited argument
                | TypeSignatures.TypeLambda(_, parameterSort, body) ->
                    yield! loop visited parameterSort
                    yield! loop visited body
                | TypeSignatures.TypeDelay inner
                | TypeSignatures.TypeMemo inner
                | TypeSignatures.TypeForce inner ->
                    yield! loop visited inner
                | TypeSignatures.TypeProject(target, _) ->
                    yield! loop visited target
                | TypeSignatures.TypeArrow(_, parameterType, resultType) ->
                    yield! loop visited parameterType
                    yield! loop visited resultType
                | TypeSignatures.TypeEquality(left, right) ->
                    yield! loop visited left
                    yield! loop visited right
                | TypeSignatures.TypeName(([ "BorrowView" ] | [ "std"; "prelude"; "BorrowView" ]), TypeSignatures.TypeVariable regionName :: arguments) ->
                    yield regionName

                    for argument in arguments do
                        yield! loop visited argument
                | TypeSignatures.TypeName(name, arguments) ->
                    let aliasName = SyntaxFacts.moduleNameToText name

                    match typeAliasMap |> Map.tryFind aliasName with
                    | Some aliasInfo
                        when not (Set.contains aliasName visited)
                             && List.length aliasInfo.Parameters = List.length arguments ->
                        let substitution = List.zip aliasInfo.Parameters arguments |> Map.ofList

                        yield!
                            aliasInfo.Body
                            |> TypeSignatures.applySubstitution substitution
                            |> loop (Set.add aliasName visited)
                    | _ ->
                        for argument in arguments do
                            yield! loop visited argument
                | TypeSignatures.TypeRecord fields ->
                    for field in fields do
                        yield! loop visited field.Type
                | TypeSignatures.TypeEffectRow(entries, tail) ->
                    for entry in entries do
                        yield! loop visited entry.Label
                        yield! loop visited entry.Effect

                    match tail with
                    | Some tailType -> yield! loop visited tailType
                    | None -> ()
                | TypeSignatures.TypeUnion members ->
                    for memberType in members do
                        yield! loop visited memberType
                | TypeSignatures.TypeLevelLiteral _
                | TypeSignatures.TypeUniverse _
                | TypeSignatures.TypeIntrinsic _
                | TypeSignatures.TypeVariable _ ->
                    ()
            }

        loop Set.empty typeExpr |> Set.ofSeq

    let private captureSetFromType typeExpr =
        captureSetFromTypeWithAliases Map.empty typeExpr

    let private applicationBoundaryTypesCompatible demandedType actualType =
        if TypeSignatures.definitionallyEqual demandedType actualType then
            true
        elif TypeSignatures.tryUnifyMany [ demandedType, actualType ] |> Option.isSome then
            true
        else
            match demandedType, actualType with
            | TypeSignatures.TypeArrow(demandedInnerQuantity, demandedParameterType, demandedResultType),
              TypeSignatures.TypeArrow(capabilityInnerQuantity, capabilityParameterType, capabilityResultType) ->
                TypeSignatures.definitionallyEqual demandedParameterType capabilityParameterType
                && TypeSignatures.definitionallyEqual demandedResultType capabilityResultType
                && ResourceQuantity.satisfies
                    (ResourceQuantity.ofSurface demandedInnerQuantity)
                    (ResourceQuantity.ofSurface capabilityInnerQuantity)
            | _ ->
                false

    let rec typeIsReliableForApplicationBoundary typeExpr =
        match typeExpr with
        | TypeSignatures.TypeVariable _
        | TypeSignatures.TypeIntrinsic _
        | TypeSignatures.TypeUniverse _ ->
            false
        | TypeSignatures.TypeName([ "IO" ], _)
        | TypeSignatures.TypeName([ "UIO" ], _) ->
            false
        | TypeSignatures.TypeName(_, arguments) ->
            arguments |> List.forall typeIsReliableForApplicationBoundary
        | TypeSignatures.TypeApply(callee, arguments) ->
            typeIsReliableForApplicationBoundary callee
            && (arguments |> List.forall typeIsReliableForApplicationBoundary)
        | TypeSignatures.TypeLambda(_, parameterSort, body) ->
            typeIsReliableForApplicationBoundary parameterSort
            && typeIsReliableForApplicationBoundary body
        | TypeSignatures.TypeDelay inner
        | TypeSignatures.TypeMemo inner
        | TypeSignatures.TypeForce inner ->
            typeIsReliableForApplicationBoundary inner
        | TypeSignatures.TypeProject(target, _) ->
            typeIsReliableForApplicationBoundary target
        | TypeSignatures.TypeArrow(_, parameterType, resultType)
        | TypeSignatures.TypeEquality(parameterType, resultType) ->
            typeIsReliableForApplicationBoundary parameterType
            && typeIsReliableForApplicationBoundary resultType
        | TypeSignatures.TypeCapture(inner, _) ->
            typeIsReliableForApplicationBoundary inner
        | TypeSignatures.TypeEffectRow(entries, tail) ->
            entries
            |> List.forall (fun entry ->
                typeIsReliableForApplicationBoundary entry.Label
                && typeIsReliableForApplicationBoundary entry.Effect)
            && tail |> Option.forall typeIsReliableForApplicationBoundary
        | TypeSignatures.TypeRecord fields ->
            fields |> List.forall (fun field -> typeIsReliableForApplicationBoundary field.Type)
        | TypeSignatures.TypeUnion members ->
            members |> List.forall typeIsReliableForApplicationBoundary
        | TypeSignatures.TypeLevelLiteral _ ->
            true

    let private isSingleLetterTypeParameterLike (nameSegments: string list) =
        match nameSegments with
        | [ name ] -> name.Length = 1 && Char.IsUpper name[0]
        | _ -> false

    let private isSuspensionWrapperName (nameSegments: string list) =
        match nameSegments with
        | [ "Need" ]
        | [ "Thunk" ]
        | [ "std"; "prelude"; "Need" ]
        | [ "std"; "prelude"; "Thunk" ] ->
            true
        | _ ->
            false

    let rec private hasConservativeBoundarySpecialCase typeExpr =
        match typeExpr with
        | TypeSignatures.TypeApply _ ->
            true
        | TypeSignatures.TypeName(nameSegments, arguments) ->
            isSingleLetterTypeParameterLike nameSegments
            || isSuspensionWrapperName nameSegments
            || (arguments |> List.exists hasConservativeBoundarySpecialCase)
        | TypeSignatures.TypeArrow(_, parameterType, resultType)
        | TypeSignatures.TypeEquality(parameterType, resultType) ->
            hasConservativeBoundarySpecialCase parameterType
            || hasConservativeBoundarySpecialCase resultType
        | TypeSignatures.TypeCapture(inner, _) ->
            hasConservativeBoundarySpecialCase inner
        | TypeSignatures.TypeEffectRow(entries, tail) ->
            entries
            |> List.exists (fun entry ->
                hasConservativeBoundarySpecialCase entry.Label
                || hasConservativeBoundarySpecialCase entry.Effect)
            || tail |> Option.exists hasConservativeBoundarySpecialCase
        | TypeSignatures.TypeRecord fields ->
            fields |> List.exists (fun field -> hasConservativeBoundarySpecialCase field.Type)
        | TypeSignatures.TypeUnion members ->
            members |> List.exists hasConservativeBoundarySpecialCase
        | TypeSignatures.TypeDelay inner
        | TypeSignatures.TypeMemo inner
        | TypeSignatures.TypeForce inner
        | TypeSignatures.TypeProject(inner, _) ->
            hasConservativeBoundarySpecialCase inner
        | TypeSignatures.TypeLambda(_, parameterSort, body) ->
            hasConservativeBoundarySpecialCase parameterSort
            || hasConservativeBoundarySpecialCase body
        | TypeSignatures.TypeLevelLiteral _
        | TypeSignatures.TypeUniverse _
        | TypeSignatures.TypeIntrinsic _
        | TypeSignatures.TypeVariable _ ->
            false

    let private shouldCheckApplicationTypeCompatibility argument demandedType actualType =
        let mixesNamedAndStructuralRecord =
            match demandedType, actualType with
            | TypeSignatures.TypeRecord _, TypeSignatures.TypeName _
            | TypeSignatures.TypeName _, TypeSignatures.TypeRecord _ ->
                true
            | _ ->
                false

        let isFunctionBoundary =
            match demandedType, actualType with
            | TypeSignatures.TypeArrow _, TypeSignatures.TypeArrow _ -> true
            | _ -> false

        match argument with
        | NumericLiteral _
        | Literal _
        | ExplicitImplicitArgument _
        | MonadicSplice _ ->
            false
        | Lambda _ when isFunctionBoundary ->
            false
        | Name _ when isFunctionBoundary ->
            false
        | _ when hasConservativeBoundarySpecialCase demandedType || hasConservativeBoundarySpecialCase actualType ->
            false
        | _ when mixesNamedAndStructuralRecord ->
            false
        | _ ->
            typeIsReliableForApplicationBoundary demandedType
            && typeIsReliableForApplicationBoundary actualType

    let private tryOutermostFunctionTypeFromArgument signatures argument =
        match argument with
        | Name [ name ] ->
            signatures
            |> Map.tryFind name
            |> Option.bind tryBuildFunctionTypeFromSignature
            |> Option.bind (function
                | TypeSignatures.TypeArrow(quantity, parameterType, resultType) ->
                    Some(quantity, parameterType, resultType)
                | _ ->
                    None)
        | _ ->
            None

    let private isProjectionCall projectionSummaries expression =
        match expression with
        | Apply(Name [ calleeName ], _) -> Map.containsKey calleeName projectionSummaries
        | _ -> false

    let private makePlace root path : ResourcePlace =
        { Root = root
          Path = path }

    let private tryExpressionPlace expression : ResourcePlace option =
        match expression with
        | Name(root :: path) ->
            Some(makePlace root path)
        | _ ->
            None

    let private pathHasPrefix prefix path =
        List.length prefix <= List.length path
        && List.forall2 (=) prefix (path |> List.take (List.length prefix))

    let private pathsOverlap left right =
        pathHasPrefix left right || pathHasPrefix right left

    let private distinctPaths paths =
        paths
        |> List.distinct
        |> List.sortBy (fun path -> String.concat "." path)

    let private distinctPlaces (places: ResourcePlace list) =
        places
        |> List.distinctBy (fun place -> place.Root, place.Path)
        |> List.sortBy (fun place -> String.concat "." (place.Root :: place.Path))

    let private expandBorrowFootprintPaths (state: CheckState) (place: ResourcePlace) =
        match tryFindBinding place.Root state with
        | Some binding ->
            match place.Path with
            | [] ->
                [ [] ]
            | [ fieldName ] ->
                let rec expandField seen fieldName =
                    if Set.contains fieldName seen then
                        [ [ fieldName ] ]
                    else
                        let nextSeen = Set.add fieldName seen
                        let dependencies =
                            binding.RecordFieldDependencies
                            |> Map.tryFind fieldName
                            |> Option.defaultValue Set.empty

                        [ yield [ fieldName ]
                          for dependency in dependencies do
                              yield! expandField nextSeen dependency ]

                expandField Set.empty fieldName |> distinctPaths
            | _ ->
                [ place.Path ]
        | None ->
            [ place.Path ]

    let private tryFindBorrowOverlap (place: ResourcePlace) (state: CheckState) =
        state.BorrowLocks
        |> Map.values
        |> Seq.tryFind (fun borrowLock ->
            String.Equals(borrowLock.Root, place.Root, StringComparison.Ordinal)
            && (borrowLock.FootprintPaths |> List.exists (fun footprintPath -> pathsOverlap footprintPath place.Path)))

    let private fieldPathRequiresUse (state: CheckState) (place: ResourcePlace) =
        match place.Path, tryFindBinding place.Root state with
        | fieldName :: _, Some binding ->
            binding.RecordFieldQuantities
            |> Map.tryFind fieldName
            |> Option.exists ResourceQuantity.requiresUse
        | _ ->
            false

    let rec private tryBorrowablePlacesWithEnv
        (env: Map<string, ResourcePlace list>)
        projectionSummaries
        (state: CheckState)
        expression
        : ResourcePlace list option
        =
        match expression with
        | Name(root :: path) ->
            match Map.tryFind root env with
            | Some places ->
                places
                |> List.map (fun place -> makePlace place.Root (place.Path @ path))
                |> distinctPlaces
                |> Some
            | None ->
                Some [ makePlace root path ]
        | Apply(Name [ calleeName ], arguments) ->
            projectionSummaries
            |> Map.tryFind calleeName
            |> Option.bind (fun projectionSummary ->
                tryProjectionCallPlacesWithEnv env projectionSummaries state projectionSummary arguments)
        | _ ->
            None

    and private tryProjectionCallPlacesWithEnv
        (env: Map<string, ResourcePlace list>)
        projectionSummaries
        (state: CheckState)
        (projectionSummary: ProjectionSummary)
        arguments
        : ResourcePlace list option
        =
        if List.length projectionSummary.Binders <> List.length arguments then
            None
        else
            (Some Map.empty, List.zip projectionSummary.Binders arguments)
            ||> List.fold (fun current (binder, argument) ->
                current
                |> Option.bind (fun binderEnv ->
                    match binder with
                    | ProjectionPlaceBinder binder ->
                        tryBorrowablePlacesWithEnv env projectionSummaries state argument
                        |> Option.map (fun places -> Map.add binder.Name places binderEnv)
                    | ProjectionValueBinder _ ->
                        Some binderEnv))
            |> Option.bind (fun binderEnv ->
                let combinedEnv =
                    binderEnv
                    |> Map.fold (fun current name places -> Map.add name places current) env

                tryProjectionBodyPlacesWithEnv combinedEnv projectionSummaries state projectionSummary.Body)

    and private tryProjectionBodyPlacesWithEnv
        (env: Map<string, ResourcePlace list>)
        projectionSummaries
        (state: CheckState)
        body
        : ResourcePlace list option
        =
        match body with
        | None ->
            None
        | Some(ProjectionYield expression) ->
            tryBorrowablePlacesWithEnv env projectionSummaries state expression
        | Some(ProjectionIfThenElse(_, whenTrue, whenFalse)) ->
            match
                tryProjectionBodyPlacesWithEnv env projectionSummaries state (Some whenTrue),
                tryProjectionBodyPlacesWithEnv env projectionSummaries state (Some whenFalse)
            with
            | Some leftPlaces, Some rightPlaces ->
                Some(distinctPlaces (leftPlaces @ rightPlaces))
            | _ ->
                None
        | Some(ProjectionMatch(_, cases)) ->
            cases
            |> List.fold (fun current caseClause ->
                match current, tryProjectionBodyPlacesWithEnv env projectionSummaries state (Some caseClause.Body) with
                | Some places, Some casePlaces ->
                    Some(distinctPlaces (places @ casePlaces))
                | None, Some casePlaces ->
                    Some casePlaces
                | current, None ->
                    current) None
        | Some(ProjectionAccessors _) ->
            None

    let private accessorCapabilities body =
        match body with
        | Some(ProjectionAccessors clauses) ->
            let direct =
                clauses
                |> List.fold (fun capabilities clause ->
                    match clause with
                    | ProjectionGet _ -> Set.add "get" capabilities
                    | ProjectionInout _ -> Set.add "open" capabilities
                    | ProjectionSet _ -> Set.add "set" capabilities
                    | ProjectionSink _ -> Set.add "sink" capabilities) Set.empty

            let synthesized =
                direct
                |> fun capabilities ->
                    if Set.contains "get" capabilities && Set.contains "set" capabilities then
                        Set.add "open" capabilities
                    else
                        capabilities
                |> fun capabilities ->
                    if Set.contains "open" capabilities && not (Set.contains "set" capabilities) then
                        Set.add "set" capabilities
                    else
                        capabilities

            Some synthesized
        | _ ->
            None

    let private projectionPlaceArgumentPlaces projectionSummaries state (projectionSummary: ProjectionSummary) arguments =
        if List.length projectionSummary.Binders <> List.length arguments then
            None
        else
            (Some [], List.zip projectionSummary.Binders arguments)
            ||> List.fold (fun current (binder, argument) ->
                current
                |> Option.bind (fun places ->
                    match binder with
                    | ProjectionPlaceBinder _ ->
                        tryBorrowablePlacesWithEnv Map.empty projectionSummaries state argument
                        |> Option.map (fun argumentPlaces -> places @ argumentPlaces)
                    | ProjectionValueBinder _ ->
                        Some places))
            |> Option.map distinctPlaces

    let private tryInoutArgumentPlaces
        (projectionSummaries: Map<string, ProjectionSummary>)
        (state: CheckState)
        (expression: SurfaceExpression)
        =
        match expression with
        | Apply(Name [ projectionName ], projectionArguments) when Map.containsKey projectionName projectionSummaries ->
            let projectionSummary = projectionSummaries[projectionName]

            match accessorCapabilities projectionSummary.Body with
            | Some _ -> projectionPlaceArgumentPlaces projectionSummaries state projectionSummary projectionArguments
            | None -> tryBorrowablePlacesWithEnv Map.empty projectionSummaries state expression
        | _ ->
            tryExpressionPlace expression |> Option.map List.singleton

    let private sequenceOptions values =
        (Some [], values)
        ||> List.fold (fun current value ->
            match current, value with
            | Some items, Some item -> Some(items @ [ item ])
            | _ -> None)

    let private tryProjectionDescriptorAlias projectionSummaries expression =
        let tryBuild projectionName suppliedArguments =
            projectionSummaries
            |> Map.tryFind projectionName
            |> Option.bind (fun projectionSummary ->
                if List.length suppliedArguments >= List.length projectionSummary.Binders then
                    None
                else
                    let suppliedBinders = projectionSummary.Binders |> List.take (List.length suppliedArguments)
                    let remainingBinders = projectionSummary.Binders |> List.skip (List.length suppliedArguments)

                    let suppliedAreValues =
                        suppliedBinders
                        |> List.forall (function
                            | ProjectionValueBinder _ -> true
                            | ProjectionPlaceBinder _ -> false)

                    let remainingPlaceBinders =
                        remainingBinders
                        |> List.choose (function
                            | ProjectionPlaceBinder binder -> Some binder
                            | ProjectionValueBinder _ -> None)

                    if suppliedAreValues && List.length remainingPlaceBinders = List.length remainingBinders then
                        Some
                            { ProjectionName = projectionName
                              SuppliedArguments = suppliedArguments
                              RemainingPlaceBinders = remainingPlaceBinders }
                    else
                        None)

        match expression with
        | Name [ projectionName ] -> tryBuild projectionName []
        | Apply(Name [ projectionName ], suppliedArguments) -> tryBuild projectionName suppliedArguments
        | _ -> None

    let private tryProjectionDescriptorApplication (aliasInfo: ProjectionDescriptorAlias) rootsArgument =
        let recordFieldsByName (fields: SurfaceRecordLiteralField list) =
            fields
            |> List.map (fun field -> field.Name, field.Value)
            |> Map.ofList

        let trySingleRoot (binder: ProjectionPlaceBinder) =
            match rootsArgument with
            | RecordLiteral fields when fields |> List.exists (fun field -> String.Equals(field.Name, binder.Name, StringComparison.Ordinal)) ->
                recordFieldsByName fields |> Map.tryFind binder.Name |> Option.map List.singleton
            | RecordLiteral _ ->
                None
            | _ ->
                Some [ rootsArgument ]

        let tryMultiRoot (binders: ProjectionPlaceBinder list) =
            match rootsArgument with
            | RecordLiteral fields ->
                let fieldMap = recordFieldsByName fields
                let fieldNames = fieldMap |> Map.keys |> Set.ofSeq
                let binderNames = binders |> List.map (fun binder -> binder.Name) |> Set.ofList

                if fieldNames = binderNames then
                    binders
                    |> List.map (fun binder -> fieldMap |> Map.tryFind binder.Name)
                    |> sequenceOptions
                else
                    None
            | _ ->
                None

        match aliasInfo.RemainingPlaceBinders with
        | [] ->
            None
        | [ binder ] ->
            trySingleRoot binder
        | binders ->
            tryMultiRoot binders
        |> Option.map (fun placeArguments ->
            Apply(Name [ aliasInfo.ProjectionName ], aliasInfo.SuppliedArguments @ placeArguments))

    let rec private rewriteProjectionDescriptorApplications (aliases: Map<string, ProjectionDescriptorAlias>) (expression: SurfaceExpression) =
        let rewrite = rewriteProjectionDescriptorApplications aliases

        match expression with
        | Apply(Name [ aliasName ], [ rootsArgument ]) ->
            match aliases |> Map.tryFind aliasName with
            | Some aliasInfo ->
                tryProjectionDescriptorApplication aliasInfo rootsArgument
                |> Option.map (rewriteProjectionDescriptorApplications aliases)
                |> Option.defaultValue expression
            | None ->
                Apply(Name [ aliasName ], [ rewrite rootsArgument ])
        | SyntaxQuote inner ->
            SyntaxQuote(rewrite inner)
        | SyntaxSplice inner ->
            SyntaxSplice(rewrite inner)
        | TopLevelSyntaxSplice inner ->
            TopLevelSyntaxSplice(rewrite inner)
        | CodeQuote inner ->
            CodeQuote(rewrite inner)
        | CodeSplice inner ->
            CodeSplice(rewrite inner)
        | Apply(callee, arguments) ->
            Apply(rewrite callee, arguments |> List.map rewrite)
        | LocalLet(binding, value, body) ->
            let shadowedNames = collectPatternNames binding.Pattern

            let bodyAliases =
                shadowedNames
                |> List.fold (fun current name -> Map.remove name current) aliases

            LocalLet(binding, rewrite value, rewriteProjectionDescriptorApplications bodyAliases body)
        | LocalSignature(declaration, body) ->
            LocalSignature(declaration, rewriteProjectionDescriptorApplications (Map.remove declaration.Name aliases) body)
        | LocalTypeAlias(declaration, body) ->
            LocalTypeAlias(declaration, rewrite body)
        | LocalScopedEffect(declaration, body) ->
            LocalScopedEffect(declaration, rewrite body)
        | Handle(isDeep, label, body, returnClause, operationClauses) ->
            let rewriteClause (clause: SurfaceEffectHandlerClause) =
                { clause with
                    Body = rewrite clause.Body }

            Handle(
                isDeep,
                rewrite label,
                rewrite body,
                rewriteClause returnClause,
                operationClauses |> List.map rewriteClause
            )
        | Lambda(parameters, body) ->
            let bodyAliases =
                parameters
                |> List.fold (fun current parameter -> Map.remove parameter.Name current) aliases

            Lambda(parameters, rewriteProjectionDescriptorApplications bodyAliases body)
        | IfThenElse(condition, whenTrue, whenFalse) ->
            IfThenElse(rewrite condition, rewrite whenTrue, rewrite whenFalse)
        | Match(scrutinee, cases) ->
            let rewriteCase (caseClause: SurfaceMatchCase) =
                { caseClause with
                    Guard = caseClause.Guard |> Option.map rewrite
                    Body = rewrite caseClause.Body }

            Match(rewrite scrutinee, cases |> List.map rewriteCase)
        | RecordLiteral fields ->
            RecordLiteral(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
        | Seal(value, ascriptionTokens) ->
            Seal(rewrite value, ascriptionTokens)
        | RecordUpdate(receiver, fields) ->
            RecordUpdate(receiver |> rewrite, fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
        | MemberAccess(receiver, segments, arguments) ->
            MemberAccess(receiver |> rewrite, segments, arguments |> List.map rewrite)
        | SafeNavigation(receiver, navigation) ->
            SafeNavigation(receiver |> rewrite, { navigation with Arguments = navigation.Arguments |> List.map rewrite })
        | TagTest(receiver, constructorName) ->
            TagTest(rewrite receiver, constructorName)
        | Do statements ->
            let rec rewriteStatements activeAliases statements =
                match statements with
                | [] ->
                    []
                | statement :: rest ->
                    match statement with
                    | DoLet(binding, value) ->
                        let restAliases =
                            collectPatternNames binding.Pattern
                            |> List.fold (fun current name -> Map.remove name current) activeAliases

                        DoLet(binding, rewriteProjectionDescriptorApplications activeAliases value)
                        :: rewriteStatements restAliases rest
                    | DoBind(binding, value) ->
                        let restAliases =
                            collectPatternNames binding.Pattern
                            |> List.fold (fun current name -> Map.remove name current) activeAliases

                        DoBind(binding, rewriteProjectionDescriptorApplications activeAliases value)
                        :: rewriteStatements restAliases rest
                    | DoLetQuestion(binding, value, failure) ->
                        let restAliases =
                            collectPatternNames binding.Pattern
                            |> List.fold (fun current name -> Map.remove name current) activeAliases

                        let failure =
                            failure
                            |> Option.map (fun block ->
                                { block with
                                    Body = rewriteStatements activeAliases block.Body })

                        DoLetQuestion(binding, rewriteProjectionDescriptorApplications activeAliases value, failure)
                        :: rewriteStatements restAliases rest
                    | DoUsing(binding, value) ->
                        let restAliases =
                            collectPatternNames binding.Pattern
                            |> List.fold (fun current name -> Map.remove name current) activeAliases

                        DoUsing(binding, rewriteProjectionDescriptorApplications activeAliases value)
                        :: rewriteStatements restAliases rest
                    | DoVar(name, value) ->
                        DoVar(name, rewriteProjectionDescriptorApplications activeAliases value)
                        :: rewriteStatements (Map.remove name activeAliases) rest
                    | DoAssign(target, value) ->
                        DoAssign(target, rewriteProjectionDescriptorApplications activeAliases value)
                        :: rewriteStatements activeAliases rest
                    | DoDefer value ->
                        DoDefer(rewriteProjectionDescriptorApplications activeAliases value)
                        :: rewriteStatements activeAliases rest
                    | DoIf(condition, whenTrue, whenFalse) ->
                        DoIf(
                            rewriteProjectionDescriptorApplications activeAliases condition,
                            rewriteStatements activeAliases whenTrue,
                            rewriteStatements activeAliases whenFalse
                        )
                        :: rewriteStatements activeAliases rest
                    | DoWhile(condition, body) ->
                        DoWhile(rewriteProjectionDescriptorApplications activeAliases condition, rewriteStatements activeAliases body)
                        :: rewriteStatements activeAliases rest
                    | DoExpression value ->
                        DoExpression(rewriteProjectionDescriptorApplications activeAliases value)
                        :: rewriteStatements activeAliases rest
                    | DoReturn value ->
                        DoReturn(rewriteProjectionDescriptorApplications activeAliases value)
                        :: rewriteStatements activeAliases rest

            Do(rewriteStatements aliases statements)
        | MonadicSplice inner ->
            MonadicSplice(rewrite inner)
        | ExplicitImplicitArgument inner ->
            ExplicitImplicitArgument(rewrite inner)
        | NamedApplicationBlock fields ->
            NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
        | InoutArgument inner ->
            InoutArgument(rewrite inner)
        | Unary(operatorName, inner) ->
            Unary(operatorName, rewrite inner)
        | Binary(left, operatorName, right) ->
            Binary(rewrite left, operatorName, rewrite right)
        | Elvis(left, right) ->
            Elvis(rewrite left, rewrite right)
        | Comprehension comprehension ->
            Comprehension { comprehension with Lowered = rewrite comprehension.Lowered }
        | PrefixedString(prefix, parts) ->
            let parts =
                parts
                |> List.map (function
                    | StringInterpolation(inner, format) -> StringInterpolation(rewrite inner, format)
                    | StringText text -> StringText text)

            PrefixedString(prefix, parts)
        | Literal _
        | NumericLiteral _
        | KindQualifiedName _
        | Name _ ->
            expression

    let private addProjectionCapabilityDiagnostic (document: ParsedDocument) capability expression state =
        addDiagnostic
            DiagnosticCode.ProjectionDefinitionUnsupported
            $"Projection/accessor use requires the '{capability}' capability at this site."
            (argumentLocation document expression)
            []
            document
            state

    let private addProjectionPlaceDiagnostic (document: ParsedDocument) expression state =
        addDiagnostic
            DiagnosticCode.ProjectionDefinitionUnsupported
            "Projection place arguments must be stable places or computed selector places with stable roots."
            (argumentLocation document expression)
            []
            document
            state

    let private addBorrowLock (root: string) (footprintPaths: string list list) origin (state: CheckState) =
        let lockId = $"{currentScopeId state}.l{state.NextBorrowLockId}"

        let borrowLock =
            { Id = lockId
              Root = root
              FootprintPaths = distinctPaths footprintPaths
              Origin = origin }

        let activeScopes =
            match state.ActiveScopes with
            | currentScope :: rest ->
                { currentScope with
                    IntroducedBorrowLocks = lockId :: currentScope.IntroducedBorrowLocks }
                :: rest
            | [] ->
                []

        { state with
            ActiveScopes = activeScopes
            BorrowLocks = Map.add lockId borrowLock state.BorrowLocks
            NextBorrowLockId = state.NextBorrowLockId + 1 }

    let private trimSignificantTokens (tokens: Token list) =
        tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)

    let private splitTopLevelCommas (tokens: Token list) =
        let rec loop depth current remaining segments =
            match remaining with
            | [] ->
                List.rev ((List.rev current) :: segments)
            | token :: tail when token.Kind = LeftParen ->
                loop (depth + 1) (token :: current) tail segments
            | token :: tail when token.Kind = RightParen ->
                loop (max 0 (depth - 1)) (token :: current) tail segments
            | token :: tail when token.Kind = Comma && depth = 0 ->
                loop depth [] tail ((List.rev current) :: segments)
            | token :: tail ->
                loop depth (token :: current) tail segments

        loop 0 [] tokens []

    let private stripRecordTypeOuterParens (tokens: Token list) =
        let significant = trimSignificantTokens tokens

        match significant with
        | { Kind = LeftParen } :: rest ->
            match List.rev rest with
            | { Kind = RightParen } :: innerReversed ->
                Some(innerReversed |> List.rev |> trimSignificantTokens)
            | _ ->
                None
        | _ ->
            None

    let private collectFieldDependencies (tokens: Token list) =
        let significant = trimSignificantTokens tokens

        significant
        |> List.windowed 3
        |> List.choose (function
            | [ thisToken; dotToken; nameToken ]
                when String.Equals(thisToken.Text, "this", StringComparison.Ordinal)
                     && dotToken.Kind = Dot
                     && Token.isName nameToken ->
                Some(SyntaxFacts.trimIdentifierQuotes nameToken.Text)
            | _ ->
                None)
        |> Set.ofList

    let private tryParseRecordFieldDependencies (tokens: Token list) =
        let dropQuantityPrefix fieldTokens =
            match trimSignificantTokens fieldTokens with
            | first :: second :: rest when first.Kind = Operator && (first.Text = "<=" || first.Text = ">=") && second.Text = "1" ->
                rest
            | first :: rest when first.Kind = IntegerLiteral && (first.Text = "0" || first.Text = "1") ->
                rest
            | first :: rest when first.Kind = Operator && first.Text = "&" ->
                rest
            | first :: rest when Token.isName first && (first.Text = "omega" || first.Text = "\u03c9") ->
                rest
            | trimmed ->
                trimmed

        let parseField fieldTokens =
            let trimmed =
                fieldTokens
                |> trimSignificantTokens
                |> function
                    | { Kind = AtSign } :: rest -> rest
                    | rest -> rest
                |> dropQuantityPrefix

            match trimmed with
            | nameToken :: colonToken :: typeTokens when Token.isName nameToken && colonToken.Kind = Colon ->
                Some(
                    SyntaxFacts.trimIdentifierQuotes nameToken.Text,
                    collectFieldDependencies typeTokens
                )
            | _ ->
                None

        tokens
        |> stripRecordTypeOuterParens
        |> Option.map (fun innerTokens ->
            innerTokens
            |> splitTopLevelCommas
            |> List.choose parseField
            |> Map.ofList)
        |> Option.filter (Map.isEmpty >> not)

    let private tryParseRecordFieldQuantities (tokens: Token list) =
        let stripQuantity fieldTokens =
            match trimSignificantTokens fieldTokens with
            | { Kind = IntegerLiteral; Text = "1" } :: rest ->
                Some ResourceQuantity.one, rest
            | { Kind = IntegerLiteral; Text = "0" } :: rest ->
                Some ResourceQuantity.zero, rest
            | { Kind = Operator; Text = "&" } :: rest ->
                Some(ResourceQuantity.Borrow None), rest
            | { Kind = Operator; Text = "<=" } :: { Kind = IntegerLiteral; Text = "1" } :: rest ->
                Some ResourceQuantity.atMostOne, rest
            | { Kind = Operator; Text = ">=" } :: { Kind = IntegerLiteral; Text = "1" } :: rest ->
                Some ResourceQuantity.atLeastOne, rest
            | head :: rest when Token.isName head && (head.Text = "omega" || head.Text = "ω") ->
                Some ResourceQuantity.omega, rest
            | other ->
                None, other

        let parseField fieldTokens =
            let quantity, remaining = stripQuantity fieldTokens

            match trimSignificantTokens remaining with
            | nameToken :: colonToken :: _ when Token.isName nameToken && colonToken.Kind = Colon ->
                Some(
                    SyntaxFacts.trimIdentifierQuotes nameToken.Text,
                    quantity |> Option.defaultValue ResourceQuantity.omega
                )
            | _ ->
                None

        tokens
        |> stripRecordTypeOuterParens
        |> Option.map (fun innerTokens ->
            innerTokens
            |> splitTopLevelCommas
            |> List.choose parseField
            |> Map.ofList)
        |> Option.filter (Map.isEmpty >> not)

    let private collectRecordTypeAliases (documents: ParsedDocument list) =
        let moduleAliasName (document: ParsedDocument) (aliasName: string) : string option =
            document.ModuleName
            |> Option.map (fun moduleName -> $"{SyntaxFacts.moduleNameToText moduleName}.{aliasName}")

        documents
        |> List.collect (fun document ->
            document.Syntax.Declarations
            |> List.choose (function
                | TypeAliasNode declaration ->
                    declaration.BodyTokens
                    |> Option.bind tryParseRecordFieldDependencies
                    |> Option.map (fun dependencies ->
                        [
                            declaration.Name, dependencies
                            match moduleAliasName document declaration.Name with
                            | Some qualifiedName -> qualifiedName, dependencies
                            | None -> ()
                        ])
                | _ ->
                    None)
            |> List.concat)
        |> Map.ofList

    let private collectRecordTypeQuantityAliases (documents: ParsedDocument list) =
        let moduleAliasName (document: ParsedDocument) (aliasName: string) : string option =
            document.ModuleName
            |> Option.map (fun moduleName -> $"{SyntaxFacts.moduleNameToText moduleName}.{aliasName}")

        documents
        |> List.collect (fun document ->
            document.Syntax.Declarations
            |> List.choose (function
                | TypeAliasNode declaration ->
                    declaration.BodyTokens
                    |> Option.bind tryParseRecordFieldQuantities
                    |> Option.map (fun quantities ->
                        [
                            declaration.Name, quantities
                            match moduleAliasName document declaration.Name with
                            | Some qualifiedName -> qualifiedName, quantities
                            | None -> ()
                        ])
                | _ ->
                    None)
            |> List.concat)
        |> Map.ofList

    let private collectTypeAliases (documents: ParsedDocument list) =
        let moduleAliasName (document: ParsedDocument) (aliasName: string) : string option =
            document.ModuleName
            |> Option.map (fun moduleName -> $"{SyntaxFacts.moduleNameToText moduleName}.{aliasName}")

        documents
        |> List.collect (fun document ->
            document.Syntax.Declarations
            |> List.choose (function
                | TypeAliasNode declaration when not declaration.IsOpaque ->
                    declaration.BodyTokens
                    |> Option.bind TypeSignatures.parseType
                    |> Option.map (fun body ->
                        let aliasInfo =
                            { Parameters = TypeSignatures.collectLeadingTypeParameters declaration.HeaderTokens
                              Body = body }

                        [
                            declaration.Name, aliasInfo
                            match moduleAliasName document declaration.Name with
                            | Some qualifiedName -> qualifiedName, aliasInfo
                            | None -> ()
                        ])
                | _ ->
                    None)
            |> List.concat)
        |> Map.ofList

    let private tryResolveRecordFieldDependencies aliasMap tokens =
        match tryParseRecordFieldDependencies tokens with
        | Some dependencies ->
            dependencies
        | None ->
            match trimSignificantTokens tokens with
            | [ aliasToken ] when Token.isName aliasToken ->
                let aliasName = SyntaxFacts.trimIdentifierQuotes aliasToken.Text
                aliasMap |> Map.tryFind aliasName |> Option.defaultValue Map.empty
            | [ leftToken; dotToken; rightToken ] when Token.isName leftToken && dotToken.Kind = Dot && Token.isName rightToken ->
                let qualifiedName =
                    $"{SyntaxFacts.trimIdentifierQuotes leftToken.Text}.{SyntaxFacts.trimIdentifierQuotes rightToken.Text}"

                aliasMap |> Map.tryFind qualifiedName |> Option.defaultValue Map.empty
            | _ ->
                Map.empty

    let private tryResolveRecordFieldQuantities quantityAliasMap tokens =
        match tryParseRecordFieldQuantities tokens with
        | Some quantities ->
            quantities
        | None ->
            match trimSignificantTokens tokens with
            | [ aliasToken ] when Token.isName aliasToken ->
                let aliasName = SyntaxFacts.trimIdentifierQuotes aliasToken.Text
                quantityAliasMap |> Map.tryFind aliasName |> Option.defaultValue Map.empty
            | head :: _ when Token.isName head && SyntaxFacts.trimIdentifierQuotes head.Text = "Zipper" ->
                Map.ofList [ "fill", ResourceQuantity.one ]
            | _ ->
                Map.empty

    let private collectProjectionSummaries (documents: ParsedDocument list) =
        let projectionEntries (document: ParsedDocument) (declaration: ProjectionDeclaration) =
            let simple = declaration.Name

            match document.ModuleName with
            | Some moduleName ->
                [ simple
                  $"{SyntaxFacts.moduleNameToText moduleName}.{simple}" ]
            | None ->
                [ simple ]
            |> List.map (fun name ->
                name,
                { Name = declaration.Name
                  Binders = declaration.Binders
                  Body = declaration.Body })

        documents
        |> List.collect (fun document ->
            document.Syntax.Declarations
            |> List.choose (function
                | ProjectionDeclarationNode declaration -> Some(projectionEntries document declaration)
                | _ -> None)
            |> List.concat)
        |> Map.ofList

    let private itemImportsTypeName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Type

    let private importedItemLocalName (item: ImportItem) =
        item.Alias |> Option.defaultValue item.Name

    let private exceptMatches namespaceName name (item: ExceptItem) =
        String.Equals(item.Name, name, StringComparison.Ordinal)
        && (item.Namespace.IsNone || item.Namespace = Some namespaceName)

    let private selectionImportedName
        namespaceName
        (exportedNames: Set<string>)
        itemSelector
        (selection: ImportSelection)
        (name: string)
        =
        let exported = Set.contains name exportedNames

        match selection with
        | QualifiedOnly ->
            None
        | Items items ->
            if not exported then
                None
            else
                items
                |> List.tryFind (fun item ->
                    String.Equals(item.Name, name, StringComparison.Ordinal)
                    && itemSelector item)
                |> Option.map importedItemLocalName
        | All ->
            if exported then Some name else None
        | AllExcept excludedItems ->
            if exported && not (excludedItems |> List.exists (exceptMatches namespaceName name)) then
                Some name
            else
                None

    let private buildEffectDeclarationIndices (documents: ParsedDocument list) =
        let groupedDocuments =
            documents
            |> List.choose (fun document ->
                document.ModuleName
                |> Option.map (fun moduleName -> SyntaxFacts.moduleNameToText moduleName, document))
            |> List.groupBy fst
            |> List.map (fun (moduleName, entries) -> moduleName, entries |> List.map snd)
            |> Map.ofList

        let localEffectDeclarationsByModule =
            groupedDocuments
            |> Map.map (fun _ moduleDocuments ->
                moduleDocuments
                |> List.collect (fun document ->
                    document.Syntax.Declarations
                    |> List.choose (function
                        | EffectDeclarationNode declaration -> Some declaration
                        | _ -> None)))

        let exportedEffectDeclarationsByModule =
            groupedDocuments
            |> Map.map (fun _ moduleDocuments ->
                moduleDocuments
                |> List.collect (fun document ->
                    let privateByDefault = isPrivateByDefault document

                    document.Syntax.Declarations
                    |> List.choose (function
                        | EffectDeclarationNode declaration
                            when isExportedVisibility privateByDefault declaration.Visibility ->
                            Some declaration
                        | _ -> None)))

        localEffectDeclarationsByModule, exportedEffectDeclarationsByModule

    let private visibleTopLevelEffectDeclarations
        (localEffectDeclarationsByModule: Map<string, EffectDeclaration list>)
        (exportedEffectDeclarationsByModule: Map<string, EffectDeclaration list>)
        (document: ParsedDocument)
        =
        let moduleName =
            document.ModuleName
            |> Option.map SyntaxFacts.moduleNameToText

        let localEffects =
            moduleName
            |> Option.bind (fun currentModuleName -> Map.tryFind currentModuleName localEffectDeclarationsByModule)
            |> Option.defaultValue []

        let importedEffects =
            document.Syntax.Declarations
            |> List.choose (function
                | ImportDeclaration(false, specs) -> Some specs
                | _ -> None)
            |> List.concat
            |> List.collect (fun spec ->
                match spec.Source with
                | Url _ ->
                    []
                | Dotted moduleSegments ->
                    let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                    exportedEffectDeclarationsByModule
                    |> Map.tryFind importedModuleName
                    |> Option.defaultValue []
                    |> List.choose (fun declaration ->
                        let importedName =
                            match spec.Selection with
                            | QualifiedOnly ->
                                let qualifiedPrefix = spec.Alias |> Option.defaultValue importedModuleName
                                Some(qualifiedPrefix + "." + declaration.Name)
                            | _ ->
                                selectionImportedName
                                    ImportNamespace.Type
                                    (exportedEffectDeclarationsByModule
                                     |> Map.tryFind importedModuleName
                                     |> Option.defaultValue []
                                     |> List.map (fun effect -> effect.Name)
                                     |> Set.ofList)
                                    itemImportsTypeName
                                    spec.Selection
                                    declaration.Name

                        importedName
                        |> Option.map (fun localName ->
                            { declaration with
                                Name = localName })))

        localEffects @ importedEffects

    let private liveRegionIds (state: CheckState) =
        state.Bindings
        |> Map.values
        |> Seq.collect (fun binding ->
            seq {
                match binding.BorrowRegion with
                | Some region -> yield region.Id
                | None -> ()

                yield! binding.CapturedRegions
            })
        |> Set.ofSeq

    let private addBindingWithPlace
        kind
        place
        name
        quantity
        borrowRegion
        capturedRegions
        capturedBindingOrigins
        checkDrop
        closureFactId
        localLambda
        origin
        (state: CheckState)
        =
        let bindingId = $"{currentScopeId state}.b{state.NextBindingId}"

        let binding: ResourceBinding =
            { Id = bindingId
              BindingKind = kind
              Name = name
              DeclaredQuantity = quantity
              Place = place
              ConsumedPaths = []
              RecordFieldDependencies = Map.empty
              RecordFieldQuantities = Map.empty
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

    let private addBindingSnapshot (binding: ResourceBinding) (state: CheckState) =
        let bindingId = $"{currentScopeId state}.b{state.NextBindingId}"

        let snapshot =
            { binding with
                Id = bindingId }

        let activeBindingIds =
            state.ActiveBindingIds
            |> Map.change binding.Name (fun existing -> Some(bindingId :: Option.defaultValue [] existing))

        let activeScopes =
            match state.ActiveScopes with
            | currentScope :: rest ->
                { currentScope with
                    IntroducedBindings = (binding.Name, bindingId) :: currentScope.IntroducedBindings }
                :: rest
            | [] ->
                []

        { state with
            ActiveScopes = activeScopes
            ActiveBindingIds = activeBindingIds
            Bindings = Map.add bindingId snapshot state.Bindings
            NextBindingId = state.NextBindingId + 1 }

    let private transferCapturedBindingIntoClosure (document: ParsedDocument) (binding: ResourceBinding) (state: CheckState) =
        if not (closureCaptureTransfersOwnership binding.DeclaredQuantity) then
            state
        else
            let captureOrigin = findBindingUseLocation document binding 1

            updateBinding binding.Id (fun current ->
                { current with
                    ConsumedPaths = distinctPaths ([] :: current.ConsumedPaths)
                    UseMinimum = max current.UseMinimum 1
                    UseMaximum = max current.UseMaximum 1
                    FirstConsumeOrigin = current.FirstConsumeOrigin |> Option.orElse captureOrigin }) state

    let private transferImmediateClosureArgumentCaptures (document: ParsedDocument) argument state =
        let transferCaptured state =
            let captured = capturedBindings state argument

            captured
            |> List.fold (fun current binding ->
                addEvent OwnershipUseKind.Capture (findUseLocation document binding.Name 1) binding current) state
            |> fun current ->
                captured
                |> List.fold (fun next binding ->
                    transferCapturedBindingIntoClosure document binding next) current

        match argument with
        | Lambda _ ->
            transferCaptured state
        | _ when tryDelayedExpressionBody argument |> Option.isSome ->
            transferCaptured state
        | _ ->
            state

    let private addBinding kind name quantity borrowRegion capturedRegions capturedBindingOrigins checkDrop closureFactId localLambda origin state =
        addBindingWithPlace
            kind
            (ResourcePlace.root name)
            name
            quantity
            borrowRegion
            capturedRegions
            capturedBindingOrigins
            checkDrop
            closureFactId
            localLambda
            origin
            state

    let private addPatternBindings
        (document: ParsedDocument)
        (binding: SurfaceBindPattern)
        quantity
        borrowRegion
        (sourcePlace: ResourcePlace option)
        capturedRegions
        capturedBindingOrigins
        checkDrop
        closureFactId
        localLambda
        state
        =
        collectPatternBindings binding.Pattern
        |> List.fold (fun current (name, path) ->
            let place =
                match sourcePlace, quantity with
                | Some place, Some quantity when ResourceQuantity.isBorrow quantity ->
                    makePlace place.Root (place.Path @ path)
                | _ ->
                    ResourcePlace.root name

            addBindingWithPlace
                PatternBinding
                place
                name
                quantity
                borrowRegion
                capturedRegions
                capturedBindingOrigins
                checkDrop
                closureFactId
                localLambda
                (bindPatternNameLocation document binding name)
                current) state

    let private bindingCapturedRegions (binding: ResourceBinding) =
        seq {
            match binding.BorrowRegion with
            | Some region -> yield region.Id
            | None -> ()

            yield! binding.CapturedRegions
        }
        |> Set.ofSeq

    let private bindingCapturedOrigins (binding: ResourceBinding) =
        [
            match binding.Origin with
            | Some origin -> yield origin
            | None -> ()

            yield! binding.CapturedBindingOrigins
        ]

    let private tryMovedLinearBinding expression (state: CheckState) =
        tryExpressionPlace expression
        |> Option.bind (fun (place: ResourcePlace) ->
            match tryFindBorrowOverlap place state with
            | Some _ ->
                None
            | None ->
                tryFindBinding place.Root state
                |> Option.filter (fun binding ->
                    binding.DeclaredQuantity
                    |> Option.exists ResourceQuantity.isExactOne
                    && (List.isEmpty place.Path || fieldPathRequiresUse state place))
                |> Option.map (fun binding -> binding, place.Path))

    let rec private patternCanDischargeMovedValue pattern =
        match pattern with
        | NamePattern _ ->
            true
        | AsPattern _
        | TypedPattern _
        | NamedConstructorPattern _
        | TuplePattern _
        | VariantPattern _ ->
            true
        | ConstructorPattern _ ->
            true
        | OrPattern alternatives ->
            not (List.isEmpty alternatives)
            && (alternatives |> List.forall patternCanDischargeMovedValue)
        | AnonymousRecordPattern(_, _) ->
            // Record patterns may omit fields, and omitted fields are discard positions under §5.1.5B.
            false
        | WildcardPattern
        | LiteralPattern _ ->
            false

    let private checkShadowedLinearBindings
        (document: ParsedDocument)
        (shadowedBindings: ResourceBinding list)
        stateBefore
        stateAfter
        =
        shadowedBindings
        |> List.fold (fun current shadowedBinding ->
            let updatedBinding =
                current.Bindings
                |> Map.tryFind shadowedBinding.Id
                |> Option.defaultValue shadowedBinding

            if bindingConsumptionAdvanced shadowedBinding updatedBinding then
                current
            else
                addDiagnostic
                    linearDropCode
                    $"Shadowing binding '{shadowedBinding.Name}' must consume the previous linear value exactly once in the right-hand side."
                    (findBinderLocation document shadowedBinding.Name |> Option.orElse shadowedBinding.Origin)
                    [
                        match shadowedBinding.Origin with
                        | Some location ->
                            { Message = "Previous linear binding."
                              Location = location }
                        | None -> ()
                    ]
                    document
                    current) stateAfter

    let private tryUnwrapWholeParenthesizedTokens (tokens: Token list) =
        match trimSignificantTokens tokens with
        | { Kind = LeftParen } :: rest ->
            let rec loop depth reversedInner remaining =
                match depth, remaining with
                | 0, [] ->
                    Some(List.rev reversedInner)
                | _, [] ->
                    None
                | currentDepth, token :: tail ->
                    match token.Kind with
                    | LeftParen ->
                        loop (currentDepth + 1) (token :: reversedInner) tail
                    | RightParen when currentDepth = 1 ->
                        if List.isEmpty tail then
                            loop 0 reversedInner tail
                        else
                            None
                    | RightParen ->
                        loop (currentDepth - 1) (token :: reversedInner) tail
                    | _ ->
                        loop currentDepth (token :: reversedInner) tail

            loop 1 [] rest
        | _ ->
            None

    let private tryTrailingParenthesizedTokens (tokens: Token list) =
        let tokens = trimSignificantTokens tokens
        let tokenArray = tokens |> List.toArray

        if tokenArray.Length = 0 || tokenArray[tokenArray.Length - 1].Kind <> RightParen then
            None
        else
            let mutable depth = 0
            let mutable openIndex = -1
            let mutable index = tokenArray.Length - 1

            while index >= 0 && openIndex < 0 do
                match tokenArray[index].Kind with
                | RightParen ->
                    depth <- depth + 1
                | LeftParen ->
                    depth <- depth - 1

                    if depth = 0 then
                        openIndex <- index
                | _ ->
                    ()

                index <- index - 1

            if openIndex > 0 then
                tokenArray[openIndex + 1 .. tokenArray.Length - 2] |> Array.toList |> Some
            else
                None

    let private stripFieldQuantityPrefix (tokens: Token list) =
        match trimSignificantTokens tokens with
        | { Kind = IntegerLiteral; Text = "1" } :: rest ->
            Some ResourceQuantity.one, rest
        | { Kind = IntegerLiteral; Text = "0" } :: rest ->
            Some ResourceQuantity.zero, rest
        | { Kind = Operator; Text = "&" } :: rest ->
            Some(ResourceQuantity.Borrow None), rest
        | { Kind = Operator; Text = "<=" } :: { Kind = IntegerLiteral; Text = "1" } :: rest ->
            Some ResourceQuantity.atMostOne, rest
        | { Kind = Operator; Text = ">=" } :: { Kind = IntegerLiteral; Text = "1" } :: rest ->
            Some ResourceQuantity.atLeastOne, rest
        | head :: rest when Token.isName head && (head.Text = "omega" || head.Text = "ω") ->
            Some ResourceQuantity.omega, rest
        | other ->
            None, other

    let private constructorFieldGroups (constructor: DataConstructor) =
        let tokens =
            constructor.Tokens
            |> trimSignificantTokens
            |> List.skip 1
            |> List.toArray

        let groups = ResizeArray<Token list>()
        let mutable index = 0

        while index < tokens.Length do
            match tokens[index].Kind with
            | LeftParen ->
                let mutable depth = 1
                let startIndex = index + 1
                index <- index + 1

                while index < tokens.Length && depth > 0 do
                    match tokens[index].Kind with
                    | LeftParen -> depth <- depth + 1
                    | RightParen -> depth <- depth - 1
                    | _ -> ()

                    index <- index + 1

                if depth = 0 then
                    groups.Add(tokens[startIndex .. index - 2] |> Array.toList)
            | _ ->
                index <- index + 1

        groups |> Seq.toList

    let private constructorFieldQuantities constructor =
        constructorFieldGroups constructor
        |> List.map (fun tokens -> stripFieldQuantityPrefix tokens |> fst)

    let private tryFindConstructorFieldQuantities (document: ParsedDocument) constructorName =
        document.Syntax.Declarations
        |> List.tryPick (function
            | DataDeclarationNode declaration ->
                declaration.Constructors
                |> List.tryFind (fun constructor -> String.Equals(constructor.Name, constructorName, StringComparison.Ordinal))
                |> Option.map constructorFieldQuantities
            | _ ->
                None)

    let private constructorHasPositiveOwnedField constructor =
        constructorFieldQuantities constructor
        |> List.exists (function
            | Some quantity -> ResourceQuantity.requiresUse quantity
            | None -> false)

    let private patternConstructorNames pattern =
        let rec loop current =
            seq {
                match current with
                | AsPattern(_, inner) ->
                    yield! loop inner
                | TypedPattern(inner, _) ->
                    yield! loop inner
                | NamedConstructorPattern(segments, fields) ->
                    match List.tryLast segments with
                    | Some name -> yield name
                    | None -> ()

                    for field in fields do
                        yield! loop field.Pattern
                | TuplePattern elements ->
                    for element in elements do
                        yield! loop element
                | VariantPattern _ ->
                    ()
                | _ ->
                    ()

                match current with
                | ConstructorPattern(segments, arguments) ->
                    match List.tryLast segments with
                    | Some name -> yield name
                    | None -> ()

                    for argument in arguments do
                        yield! loop argument
                | OrPattern alternatives ->
                    for alternative in alternatives do
                        yield! loop alternative
                | AnonymousRecordPattern(fields, _) ->
                    for field in fields do
                        yield! loop field.Pattern
                | WildcardPattern
                | NamePattern _
                | LiteralPattern _
                | AsPattern _
                | TypedPattern _
                | NamedConstructorPattern _
                | TuplePattern _
                | VariantPattern _ -> ()
            }

        loop pattern |> Set.ofSeq

    let private letQuestionPlainFailureDropsPositiveResidue (document: ParsedDocument) pattern =
        let successConstructors = patternConstructorNames pattern

        if Set.isEmpty successConstructors then
            false
        else
            document.Syntax.Declarations
            |> List.exists (function
                | DataDeclarationNode declaration ->
                    let constructorNames =
                        declaration.Constructors
                        |> List.map (fun constructor -> constructor.Name)
                        |> Set.ofList

                    not (Set.isEmpty (Set.intersect successConstructors constructorNames))
                    && declaration.Constructors
                       |> List.exists (fun constructor ->
                           not (Set.contains constructor.Name successConstructors)
                           && constructorHasPositiveOwnedField constructor)
                | _ ->
                    false)

    let private tryConstructorPatternQuantity (document: ParsedDocument) pattern =
        match pattern with
        | ConstructorPattern(segments, arguments) ->
            match List.tryLast segments with
            | None -> None
            | Some constructorName ->
                document.Syntax.Declarations
                |> List.tryPick (function
                    | DataDeclarationNode declaration ->
                        declaration.Constructors
                        |> List.tryFind (fun constructor -> String.Equals(constructor.Name, constructorName, StringComparison.Ordinal))
                        |> Option.bind (fun constructor ->
                            let quantities = constructorFieldQuantities constructor

                            match arguments, quantities with
                            | [ NamePattern _ ], [ Some quantity ] -> Some quantity
                            | _ -> None)
                    | _ ->
                        None)
        | _ ->
            None

    let private returnTypeContainsThreadedField fieldName (tokens: Token list) =
        let recordTokens =
            tryUnwrapWholeParenthesizedTokens tokens
            |> Option.orElseWith (fun () -> tryTrailingParenthesizedTokens tokens)

        match recordTokens with
        | None ->
            false
        | Some recordTokens ->
            recordTokens
            |> splitTopLevelCommas
            |> List.exists (fun fieldTokens ->
                let quantity, remaining = stripFieldQuantityPrefix fieldTokens

                match quantity, trimSignificantTokens remaining with
                | Some quantity, nameToken :: colonToken :: _
                    when ResourceQuantity.isExactOne quantity
                         && Token.isName nameToken
                         && colonToken.Kind = Colon ->
                    String.Equals(SyntaxFacts.trimIdentifierQuotes nameToken.Text, fieldName, StringComparison.Ordinal)
                | _ ->
                    false)

    let private splitTopLevelArrows (tokens: Token list) =
        let tokenArray = trimSignificantTokens tokens |> List.toArray
        let segments = ResizeArray<Token list>()
        let current = ResizeArray<Token>()
        let mutable depth = 0

        for token in tokenArray do
            match token.Kind with
            | LeftParen
            | LeftBracket
            | LeftBrace
            | LeftSetBrace ->
                depth <- depth + 1
                current.Add(token)
            | RightParen
            | RightBracket
            | RightBrace
            | RightSetBrace ->
                depth <- max 0 (depth - 1)
                current.Add(token)
            | Arrow when depth = 0 ->
                segments.Add(List.ofSeq current)
                current.Clear()
            | _ ->
                current.Add(token)

        if current.Count > 0 then
            segments.Add(List.ofSeq current)

        segments |> Seq.toList

    let private definitionReturnTypeTokens (document: ParsedDocument) (definition: LetDefinition) =
        definition.ReturnTypeTokens
        |> Option.orElseWith (fun () ->
            definition.Name
            |> Option.bind (fun name ->
                document.Syntax.Declarations
                |> List.tryPick (function
                    | SignatureDeclaration signature when String.Equals(signature.Name, name, StringComparison.Ordinal) ->
                        splitTopLevelArrows signature.TypeTokens
                        |> List.tryLast
                    | _ ->
                        None)))

    let private checkInoutThreadedFieldRequirements
        (document: ParsedDocument)
        (signature: FunctionSignature option)
        (definition: LetDefinition)
        state
        =
        let returnTypeTokens = definitionReturnTypeTokens document definition

        definition.Parameters
        |> List.mapi (fun index parameter ->
            let signatureInout =
                signature
                |> Option.bind (fun functionSignature ->
                    functionSignature.ParameterInout
                    |> List.tryItem index)
                |> Option.defaultValue false

            parameter, signatureInout)
        |> List.fold (fun current parameter ->
            let parameter, signatureInout = parameter

            if not parameter.IsInout && not signatureInout then
                current
            elif returnTypeTokens |> Option.exists (returnTypeContainsThreadedField parameter.Name) then
                current
            else
                addDiagnostic
                    inoutThreadedFieldMissingCode
                    $"An 'inout' parameter '{parameter.Name}' requires the result type to contain a quantity-1 field named '{parameter.Name}' after peeling any enclosing monad."
                    (findBinderLocation document parameter.Name)
                    []
                    document
                    current) state

    let rec private consumeBindingByIdAtPlace
        (document: ParsedDocument)
        (place: ResourcePlace)
        bindingId
        originOverride
        (state: CheckState)
        =
        match Map.tryFind bindingId state.Bindings with
        | None -> state
        | Some binding ->
            let placeText = String.concat "." (place.Root :: place.Path)
            let consumeOrigin =
                originOverride
                |> Option.orElseWith (fun () -> findBindingUseLocation document binding (binding.UseMaximum + 1))

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
                        $"Borrowed resource '{place.Root}' cannot be consumed."
                        consumeOrigin
                        relatedLocations
                        document
                        state
                elif tryFindBorrowOverlap place state |> Option.isSome then
                    let borrowLock = tryFindBorrowOverlap place state |> Option.get
                    let relatedLocations =
                        [
                            match borrowLock.Origin with
                            | Some location ->
                                { Message = "Active borrowed footprint."
                                  Location = location }
                            | None -> ()
                        ]

                    addDiagnostic
                        borrowOverlapCode
                        $"Place '{placeText}' overlaps an active borrowed footprint."
                        consumeOrigin
                        relatedLocations
                        document
                        state
                elif List.isEmpty place.Path && not (List.isEmpty binding.ConsumedPaths) then
                    state
                    |> addCopyForbiddenEventAtPlace consumeOrigin place.Root place.Path binding
                    |> addDiagnostic
                        linearOveruseCode
                        $"Linear resource '{place.Root}' cannot be consumed as a whole after one of its field paths has already been consumed."
                        consumeOrigin
                        []
                        document
                elif not (List.isEmpty place.Path) && binding.UseMaximum > 0 then
                    state
                    |> addCopyForbiddenEventAtPlace consumeOrigin place.Root place.Path binding
                    |> addDiagnostic
                        linearOveruseCode
                        $"Field path '{placeText}' cannot be consumed after its root resource has already been consumed."
                        consumeOrigin
                        []
                        document
                elif not (List.isEmpty place.Path)
                     && (binding.ConsumedPaths |> List.exists (fun consumedPath -> pathsOverlap consumedPath place.Path)) then
                    state
                    |> addCopyForbiddenEventAtPlace consumeOrigin place.Root place.Path binding
                    |> addDiagnostic
                        linearOveruseCode
                        $"Field path '{placeText}' has already been consumed."
                        consumeOrigin
                        []
                        document
                elif List.isEmpty place.Path && wouldOveruseBinding binding then
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

                    state
                    |> addCopyForbiddenEventAtPlace consumeOrigin place.Root place.Path binding
                    |> addDiagnostic
                        linearOveruseCode
                        $"Linear resource '{place.Root}' is consumed more than once."
                        consumeOrigin
                        relatedLocations
                        document
                else
                    state

            let state = addEventAtPlace OwnershipUseKind.Consume consumeOrigin place.Root place.Path binding state

            let updated =
                if List.isEmpty place.Path then
                    { binding with
                        UseMinimum = binding.UseMinimum + 1
                        UseMaximum = binding.UseMaximum + 1
                        FirstConsumeOrigin = binding.FirstConsumeOrigin |> Option.orElse consumeOrigin }
                else
                    { binding with
                        ConsumedPaths = distinctPaths (place.Path :: binding.ConsumedPaths)
                        UseMinimum = max binding.UseMinimum 1
                        FirstConsumeOrigin = binding.FirstConsumeOrigin |> Option.orElse consumeOrigin }

            let state =
                { state with
                    Bindings = Map.add binding.Id updated state.Bindings }

            consumeAliasedSourcePlace document consumeOrigin binding state

    and private consumeAliasedSourcePlace document consumeOrigin (binding: ResourceBinding) state =
        let aliasesSourcePlace =
            not (String.Equals(binding.Place.Root, binding.Name, StringComparison.Ordinal))
            || not (List.isEmpty binding.Place.Path)

        if not aliasesSourcePlace || binding.BorrowRegion.IsSome then
            state
        else
            match tryFindBindingIdExcluding binding.Id binding.Place.Root state with
            | Some sourceBindingId ->
                consumeBindingByIdAtPlace document binding.Place sourceBindingId consumeOrigin state
            | None ->
                state

    let private consumeBindingAtPlace (document: ParsedDocument) (place: ResourcePlace) (state: CheckState) =
        match tryFindBindingId place.Root state with
        | Some bindingId -> consumeBindingByIdAtPlace document place bindingId None state
        | None -> state

    let private consumeBinding (document: ParsedDocument) name (state: CheckState) =
        consumeBindingAtPlace document (ResourcePlace.root name) state

    let private noteValueDemandingNameUse (document: ParsedDocument) expression state =
        match expression with
        | Name(root :: path) when not (List.isEmpty path) ->
            match tryFindBinding root state with
            | Some binding when binding.DeclaredQuantity = Some ResourceQuantity.zero ->
                addDiagnostic
                    erasedRuntimeUseCode
                    $"Quantity-0 binding '{binding.Name}' cannot be used at runtime."
                    (findUseLocation document root 1)
                    []
                    document
                    state
            | Some binding when binding.DeclaredQuantity |> Option.exists concreteQuantityCountsUse
                                && fieldPathRequiresUse state (makePlace root path) ->
                consumeBindingAtPlace document (makePlace root path) state
            | Some binding when quantityBorrows binding.DeclaredQuantity ->
                addNonConsumingDemand document binding.Name state
            | _ ->
                state
        | Name [ name ] ->
            match tryFindBinding name state with
            | Some binding when binding.DeclaredQuantity = Some ResourceQuantity.zero ->
                addDiagnostic
                    erasedRuntimeUseCode
                    $"Quantity-0 binding '{binding.Name}' cannot be used at runtime."
                    (findUseLocation document name 1)
                    []
                    document
                    state
            | Some binding when binding.DeclaredQuantity |> Option.exists concreteQuantityCountsUse ->
                consumeBinding document name state
            | Some binding when quantityBorrows binding.DeclaredQuantity ->
                addNonConsumingDemand document binding.Name state
            | _ ->
                state
        | _ ->
            state

    let rec private countProjectionDescriptorAliasUses aliasName expression =
        let recurse = countProjectionDescriptorAliasUses aliasName

        match expression with
        | Apply(Name [ name ], [ rootsArgument ]) when String.Equals(name, aliasName, StringComparison.Ordinal) ->
            1 + recurse rootsArgument
        | SyntaxQuote _
        | SyntaxSplice _ ->
            0
        | TopLevelSyntaxSplice inner ->
            match tryDirectSyntaxSpliceBody expression with
            | Some quotedBody -> recurse quotedBody
            | None -> recurse inner
        | CodeQuote inner
        | CodeSplice inner ->
            recurse inner
        | Handle(_, label, body, returnClause, operationClauses) ->
            recurse label
            + recurse body
            + recurse returnClause.Body
            + (operationClauses |> List.sumBy (fun clause -> recurse clause.Body))
        | Apply(callee, arguments) ->
            recurse callee + (arguments |> List.sumBy recurse)
        | LocalLet(binding, value, body) ->
            let valueCount = recurse value

            if collectPatternNames binding.Pattern |> List.contains aliasName then
                valueCount
            else
                valueCount + recurse body
        | LocalSignature(declaration, body) ->
            if String.Equals(declaration.Name, aliasName, StringComparison.Ordinal) then
                0
            else
                recurse body
        | LocalTypeAlias(_, body) ->
            recurse body
        | LocalScopedEffect(_, body) ->
            recurse body
        | Lambda(parameters, body) ->
            if parameters |> List.exists (fun parameter -> String.Equals(parameter.Name, aliasName, StringComparison.Ordinal)) then
                0
            else
                recurse body
        | IfThenElse(condition, whenTrue, whenFalse) ->
            recurse condition + recurse whenTrue + recurse whenFalse
        | Match(scrutinee, cases) ->
            recurse scrutinee
            + (cases
               |> List.sumBy (fun caseClause ->
                   (caseClause.Guard |> Option.map recurse |> Option.defaultValue 0)
                   + recurse caseClause.Body))
        | RecordLiteral fields ->
            fields |> List.sumBy (fun field -> recurse field.Value)
        | Seal(value, _) ->
            recurse value
        | RecordUpdate(receiver, fields) ->
            recurse receiver + (fields |> List.sumBy (fun field -> recurse field.Value))
        | MemberAccess(receiver, _, arguments) ->
            recurse receiver + (arguments |> List.sumBy recurse)
        | SafeNavigation(receiver, navigation) ->
            recurse receiver + (navigation.Arguments |> List.sumBy recurse)
        | TagTest(receiver, _) ->
            recurse receiver
        | Do statements ->
            countProjectionDescriptorAliasUsesInStatements aliasName statements
        | MonadicSplice inner
        | ExplicitImplicitArgument inner
        | InoutArgument inner
        | Unary(_, inner) ->
            recurse inner
        | NamedApplicationBlock fields ->
            fields |> List.sumBy (fun field -> recurse field.Value)
        | Binary(left, _, right)
        | Elvis(left, right) ->
            recurse left + recurse right
        | Comprehension comprehension ->
            recurse comprehension.Lowered
        | PrefixedString(_, parts) ->
            parts
            |> List.sumBy (function
                | StringInterpolation(inner, _) -> recurse inner
                | StringText _ -> 0)
        | Literal _
        | NumericLiteral _
        | KindQualifiedName _
        | Name _ ->
            0

    and private countProjectionDescriptorAliasUsesInStatements aliasName statements =
        let rec loop activeAlias remaining =
            match remaining with
            | [] ->
                0
            | statement :: rest ->
                match statement with
                | DoLet(binding, expression)
                | DoBind(binding, expression)
                | DoUsing(binding, expression) ->
                    let expressionCount =
                        if activeAlias then countProjectionDescriptorAliasUses aliasName expression else 0

                    let shadows = collectPatternNames binding.Pattern |> List.contains aliasName
                    expressionCount + loop (activeAlias && not shadows) rest
                | DoLetQuestion(binding, expression, failure) ->
                    let expressionCount =
                        if activeAlias then countProjectionDescriptorAliasUses aliasName expression else 0

                    let failureCount =
                        if activeAlias then
                            failure
                            |> Option.map (fun block -> loop activeAlias block.Body)
                            |> Option.defaultValue 0
                        else
                            0

                    let shadows = collectPatternNames binding.Pattern |> List.contains aliasName
                    expressionCount + failureCount + loop (activeAlias && not shadows) rest
                | DoVar(name, expression) ->
                    let expressionCount =
                        if activeAlias then countProjectionDescriptorAliasUses aliasName expression else 0

                    expressionCount + loop (activeAlias && not (String.Equals(name, aliasName, StringComparison.Ordinal))) rest
                | DoAssign(_, expression)
                | DoDefer expression
                | DoExpression expression
                | DoReturn expression ->
                    let expressionCount =
                        if activeAlias then countProjectionDescriptorAliasUses aliasName expression else 0

                    expressionCount + loop activeAlias rest
                | DoIf(condition, whenTrue, whenFalse) ->
                    let currentCount =
                        if activeAlias then
                            countProjectionDescriptorAliasUses aliasName condition
                            + loop activeAlias whenTrue
                            + loop activeAlias whenFalse
                        else
                            0

                    currentCount + loop activeAlias rest
                | DoWhile(condition, body) ->
                    let currentCount =
                        if activeAlias then
                            countProjectionDescriptorAliasUses aliasName condition + loop activeAlias body
                        else
                            0

                    currentCount + loop activeAlias rest

        loop true statements

    let private noteProjectionDescriptorAliasUses document aliasName useCount state =
        [ 1 .. useCount ]
        |> List.fold (fun current _ -> noteValueDemandingNameUse document (Name [ aliasName ]) current) state

    let private hasPriorBorrowOverlap message (state: CheckState) =
        state.Diagnostics
        |> List.exists (fun diagnostic ->
            diagnostic.Severity = Error
            && diagnostic.Code = borrowOverlapCode
            && diagnostic.Message = message)

    let private validatePlaceAccess (document: ParsedDocument) (place: ResourcePlace) (state: CheckState) =
        match tryFindBinding place.Root state with
        | _ when tryFindBorrowOverlap place state |> Option.isSome ->
            let borrowLock = tryFindBorrowOverlap place state |> Option.get
            let placeText = String.concat "." (place.Root :: place.Path)
            let message = $"Place '{placeText}' overlaps an active borrowed footprint."

            if hasPriorBorrowOverlap message state then
                state
            else
                addDiagnostic
                    borrowOverlapCode
                    message
                    (borrowLock.Origin |> Option.orElseWith (fun () -> Some(diagnosticLocation document)))
                    [
                        match borrowLock.Origin with
                        | Some location ->
                            { Message = "Active borrowed footprint."
                              Location = location }
                        | None -> ()
                    ]
                    document
                    state
        | Some binding when List.isEmpty place.Path && not (List.isEmpty binding.ConsumedPaths) ->
            addDiagnostic
                linearOveruseCode
                $"Linear resource '{place.Root}' cannot be used as a whole after one of its field paths has already been consumed."
                (findBindingUseLocation document binding 1)
                []
                document
                state
        | Some binding when not (List.isEmpty place.Path)
                            && (binding.ConsumedPaths |> List.exists (fun consumedPath -> pathsOverlap consumedPath place.Path)) ->
            let placeText = String.concat "." (place.Root :: place.Path)
            addDiagnostic
                linearOveruseCode
                $"Field path '{placeText}' has already been consumed."
                (findBindingUseLocation document binding 1)
                []
                document
                state
        | _ ->
            state

    let private restoreConsumedPaths restoredPaths (binding: ResourceBinding) =
        let remainingPaths =
            binding.ConsumedPaths
            |> List.filter (fun consumedPath ->
                restoredPaths
                |> List.exists (fun restoredPath -> pathHasPrefix restoredPath consumedPath)
                |> not)

        { binding with
            ConsumedPaths = remainingPaths }

    let private restoreConsumedPlace place (binding: ResourceBinding) =
        let restored = restoreConsumedPaths [ place.Path ] binding

        if List.isEmpty place.Path then
            { restored with
                UseMaximum = 0 }
        else
            restored

    let private bindMatchPatternNames (document: ParsedDocument) scrutineeName pattern state =
        match tryFindBinding scrutineeName state with
        | None ->
            state
        | Some scrutineeBinding ->
            let tryFindActivePattern headName =
                document.Syntax.Declarations
                |> List.tryPick (function
                    | LetDeclaration definition
                        when definition.IsPattern
                             && (definition.Name
                                 |> Option.exists (fun name -> String.Equals(name, headName, StringComparison.Ordinal))) ->
                        Some definition
                    | _ ->
                        None)

            let activeParameterQuantity (parameter: Parameter) =
                if parameter.IsInout then
                    Some ResourceQuantity.one
                else
                    parameter.Quantity |> Option.map ResourceQuantity.ofSurface

            let explicitParameters (definition: LetDefinition) =
                definition.Parameters
                |> List.filter (fun parameter -> not parameter.IsImplicit)

            let activePatternScrutineeQuantity definition =
                explicitParameters definition
                |> List.tryLast
                |> Option.bind activeParameterQuantity

            let activePatternSubpattern definition arguments =
                let patternArgumentCount = max 0 ((explicitParameters definition |> List.length) - 1)
                arguments |> List.skip patternArgumentCount |> List.tryHead

            let addViewPatternBindings rootQuantity borrowRegion pattern current =
                let rec bindNested defaultQuantity currentPattern currentState =
                    match currentPattern with
                    | NamePattern name ->
                        let checkDrop = defaultQuantity |> Option.exists ResourceQuantity.requiresUse

                        addBindingWithPlace
                            PatternBinding
                            (ResourcePlace.root name)
                            name
                            defaultQuantity
                            borrowRegion
                            Set.empty
                            []
                            checkDrop
                            None
                            None
                            (findBinderLocation document name)
                            currentState
                    | AsPattern(name, inner) ->
                        let withAlias =
                            let checkDrop = defaultQuantity |> Option.exists ResourceQuantity.requiresUse

                            addBindingWithPlace
                                PatternBinding
                                (ResourcePlace.root name)
                                name
                                defaultQuantity
                                borrowRegion
                                Set.empty
                                []
                                checkDrop
                                None
                                None
                                (findBinderLocation document name)
                                currentState

                        bindNested defaultQuantity inner withAlias
                    | TypedPattern(inner, _) ->
                        bindNested defaultQuantity inner currentState
                    | ConstructorPattern(segments, arguments) ->
                        let fieldQuantities =
                            segments
                            |> List.tryLast
                            |> Option.bind (tryFindConstructorFieldQuantities document)

                        arguments
                        |> List.indexed
                        |> List.fold (fun nestedState (index, argument) ->
                            let fieldQuantity =
                                fieldQuantities
                                |> Option.bind (List.tryItem index)
                                |> Option.map (Option.defaultValue ResourceQuantity.omega)
                                |> Option.orElse defaultQuantity

                            bindNested fieldQuantity argument nestedState) currentState
                    | NamedConstructorPattern(_, fields)
                    | AnonymousRecordPattern(fields, _) ->
                        fields
                        |> List.fold (fun nestedState field -> bindNested defaultQuantity field.Pattern nestedState) currentState
                    | TuplePattern elements ->
                        elements |> List.fold (fun nestedState element -> bindNested defaultQuantity element nestedState) currentState
                    | VariantPattern(BoundVariantPattern(name, _))
                    | VariantPattern(RestVariantPattern name) ->
                        let checkDrop = defaultQuantity |> Option.exists ResourceQuantity.requiresUse

                        addBindingWithPlace
                            PatternBinding
                            (ResourcePlace.root name)
                            name
                            defaultQuantity
                            borrowRegion
                            Set.empty
                            []
                            checkDrop
                            None
                            None
                            (findBinderLocation document name)
                            currentState
                    | VariantPattern(WildcardVariantPattern _) ->
                        currentState
                    | OrPattern alternatives ->
                        alternatives
                        |> List.tryHead
                        |> Option.map (fun alternative -> bindNested defaultQuantity alternative currentState)
                        |> Option.defaultValue currentState
                    | WildcardPattern
                    | LiteralPattern _ ->
                        currentState

                bindNested rootQuantity pattern current

            let patternBindings = collectPatternBindings pattern

            let constructorFieldQuantitiesForPattern =
                match pattern with
                | ConstructorPattern(segments, _) ->
                    match List.tryLast segments with
                    | Some constructorName -> tryFindConstructorFieldQuantities document constructorName
                    | None ->
                        None
                | _ ->
                    None

            let activePatternState =
                match pattern with
                | ConstructorPattern(segments, arguments) ->
                    segments
                    |> List.tryLast
                    |> Option.bind tryFindActivePattern
                    |> Option.bind (fun definition ->
                        activePatternSubpattern definition arguments
                        |> Option.map (fun subpattern -> definition, subpattern))
                    |> Option.map (fun (definition, subpattern) ->
                        let scrutineeQuantity = activePatternScrutineeQuantity definition

                        let current =
                            match scrutineeQuantity with
                            | Some quantity when ResourceQuantity.requiresUse quantity ->
                                consumeBinding document scrutineeName state
                            | Some quantity when ResourceQuantity.isBorrow quantity ->
                                addNamedEvent OwnershipUseKind.Borrow (findUseLocation document scrutineeName 1) scrutineeBinding.Name state
                            | _ ->
                                state

                        let payloadQuantity, borrowRegion =
                            match scrutineeQuantity with
                            | Some quantity when ResourceQuantity.isBorrow quantity ->
                                Some quantity, scrutineeBinding.BorrowRegion
                            | _ ->
                                Some ResourceQuantity.omega, None

                        addViewPatternBindings payloadQuantity borrowRegion subpattern current)
                | _ ->
                    None

            match activePatternState with
            | Some activeState ->
                activeState
            | None ->

                let constructorFieldQuantity (path: string list) =
                    match constructorFieldQuantitiesForPattern, path with
                    | Some quantities, fieldSegment :: _ when fieldSegment.StartsWith("#", StringComparison.Ordinal) ->
                        match Int32.TryParse(fieldSegment.Substring(1): string) with
                        | true, fieldIndex ->
                            quantities
                            |> List.tryItem fieldIndex
                            |> Option.map (Option.defaultValue ResourceQuantity.omega)
                        | _ ->
                            None
                    | _ ->
                        None

                let recordFieldQuantity (path: string list) =
                    match pattern, path with
                    | AnonymousRecordPattern(_, _), fieldName :: _ ->
                        scrutineeBinding.RecordFieldQuantities |> Map.tryFind fieldName
                    | TuplePattern _, fieldName :: _ ->
                        scrutineeBinding.RecordFieldQuantities |> Map.tryFind fieldName
                    | _ ->
                        None

                let state =
                    match scrutineeBinding.DeclaredQuantity with
                    | _ when quantityBorrows scrutineeBinding.DeclaredQuantity ->
                        addNamedEvent OwnershipUseKind.Borrow (findUseLocation document scrutineeName 1) scrutineeBinding.Name state
                    | Some quantity
                        when ResourceQuantity.requiresUse quantity
                             && List.isEmpty patternBindings ->
                        match tryFindBindingId scrutineeName state with
                        | Some bindingId ->
                            updateBinding bindingId (fun binding -> { binding with UseMinimum = max binding.UseMinimum 1 }) state
                        | None ->
                            state
                    | Some quantity
                        when ResourceQuantity.requiresUse quantity
                             && not (List.isEmpty patternBindings)
                             && (constructorFieldQuantitiesForPattern
                                 |> Option.exists (List.forall (function | Some fieldQuantity -> not (ResourceQuantity.requiresUse fieldQuantity) | None -> true))) ->
                        match tryFindBindingId scrutineeName state with
                        | Some bindingId ->
                            updateBinding bindingId (fun binding -> { binding with UseMinimum = max binding.UseMinimum 1 }) state
                        | None ->
                            state
                    | Some quantity
                        when ResourceQuantity.requiresUse quantity
                             && not (List.isEmpty patternBindings)
                             && (match pattern with
                                 | AnonymousRecordPattern _
                                 | TuplePattern _
                                 | NamedConstructorPattern _ -> true
                                 | _ -> false) ->
                        match tryFindBindingId scrutineeName state with
                        | Some bindingId ->
                            updateBinding bindingId (fun binding -> { binding with UseMinimum = max binding.UseMinimum 1 }) state
                        | None ->
                            state
                    | _ ->
                        state

                let checkDrop =
                    scrutineeBinding.DeclaredQuantity
                    |> Option.exists ResourceQuantity.requiresUse

                patternBindings
                |> List.fold (fun current (name, path) ->
                    let bindingQuantity =
                        constructorFieldQuantity path
                        |> Option.orElseWith (fun () -> recordFieldQuantity path)
                        |> Option.orElse scrutineeBinding.DeclaredQuantity

                    let checkDrop =
                        bindingQuantity
                        |> Option.exists ResourceQuantity.requiresUse

                    addBindingWithPlace
                        PatternBinding
                        (makePlace scrutineeBinding.Place.Root (scrutineeBinding.Place.Path @ path))
                        name
                        bindingQuantity
                        scrutineeBinding.BorrowRegion
                        (bindingCapturedRegions scrutineeBinding)
                        (bindingCapturedOrigins scrutineeBinding)
                        checkDrop
                        None
                        None
                        (findBinderLocation document name)
                        current) state

    let rec private patternBoundNameCount pattern =
        match pattern with
        | WildcardPattern
        | LiteralPattern _ ->
            0
        | NamePattern _ ->
            1
        | AsPattern(_, inner) ->
            1 + patternBoundNameCount inner
        | TypedPattern(inner, _) ->
            patternBoundNameCount inner
        | ConstructorPattern(_, arguments) ->
            arguments |> List.sumBy patternBoundNameCount
        | NamedConstructorPattern(_, fields) ->
            fields |> List.sumBy (fun field -> patternBoundNameCount field.Pattern)
        | TuplePattern elements ->
            elements |> List.sumBy patternBoundNameCount
        | VariantPattern(BoundVariantPattern _) ->
            1
        | VariantPattern(RestVariantPattern _) ->
            1
        | VariantPattern(WildcardVariantPattern _) ->
            0
        | OrPattern alternatives ->
            alternatives
            |> List.tryHead
            |> Option.map patternBoundNameCount
            |> Option.defaultValue 0
        | AnonymousRecordPattern(fields, rest) ->
            let fieldCount = fields |> List.sumBy (fun field -> patternBoundNameCount field.Pattern)
            let restCount =
                match rest with
                | Some(BindRecordPatternRest _) -> 1
                | _ -> 0

            fieldCount + restCount

    let private matchPatternOwnershipSupported scrutinee pattern =
        let rec supports topLevel current =
            match current with
            | WildcardPattern
            | LiteralPattern _ ->
                topLevel
            | NamePattern _ ->
                true
            | AsPattern(_, inner) ->
                supports false inner
            | TypedPattern(inner, _) ->
                supports topLevel inner
            | ConstructorPattern(_, arguments) ->
                arguments |> List.forall (supports false)
            | NamedConstructorPattern(_, fields)
            | AnonymousRecordPattern(fields, _) ->
                fields |> List.forall (fun field -> supports false field.Pattern)
            | TuplePattern elements ->
                elements |> List.forall (supports false)
            | VariantPattern(BoundVariantPattern _)
            | VariantPattern(RestVariantPattern _) ->
                true
            | VariantPattern(WildcardVariantPattern _) ->
                topLevel
            | OrPattern alternatives ->
                patternBoundNameCount current = 0 && alternatives |> List.forall (supports topLevel)

        match patternBoundNameCount pattern with
        | 0 ->
            supports true pattern
        | _ ->
            match scrutinee with
            | Name [ _ ] -> supports true pattern
            | _ -> false

    let private mergeBindingSnapshots (leftBinding: ResourceBinding) (rightBinding: ResourceBinding) =
        { leftBinding with
            ConsumedPaths = distinctPaths (leftBinding.ConsumedPaths @ rightBinding.ConsumedPaths)
            RecordFieldDependencies =
                if Map.isEmpty leftBinding.RecordFieldDependencies then
                    rightBinding.RecordFieldDependencies
                else
                    leftBinding.RecordFieldDependencies
            RecordFieldQuantities =
                if Map.isEmpty leftBinding.RecordFieldQuantities then
                    rightBinding.RecordFieldQuantities
                else
                    leftBinding.RecordFieldQuantities
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
            BorrowLocks = left.BorrowLocks
            UsingObligations =
                right.UsingObligations
                |> Map.fold (fun current obligationId obligation -> Map.add obligationId obligation current) left.UsingObligations
            Diagnostics = left.Diagnostics @ right.Diagnostics
            Events = left.Events @ right.Events
            BorrowRegions = left.BorrowRegions @ right.BorrowRegions
            UsingScopes = left.UsingScopes @ right.UsingScopes
            Closures = left.Closures @ right.Closures
            DeferredFacts = (left.DeferredFacts @ right.DeferredFacts) |> List.distinct |> List.sort
            NextScopeId = max left.NextScopeId right.NextScopeId
            NextBindingId = max left.NextBindingId right.NextBindingId
            NextBorrowLockId = max left.NextBorrowLockId right.NextBorrowLockId
            NextEventId = max left.NextEventId right.NextEventId
            NextRegionId = max left.NextRegionId right.NextRegionId
            NextUsingScopeId = max left.NextUsingScopeId right.NextUsingScopeId
            NextClosureId = max left.NextClosureId right.NextClosureId }

    let private escapedBorrowRegions state expression =
        capturedRegions state expression

    let private checkEscapeAgainstAllowed allowedRegions (document: ParsedDocument) expression state =
        let escaped =
            escapedBorrowRegions state expression
            |> Set.filter (fun regionId -> not (Set.contains regionId allowedRegions))

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
                        let state = addEvent OwnershipUseKind.Escape escapeOrigin binding state

                        match binding.ClosureFactId with
                        | Some closureId -> markClosureEscaped closureId state
                        | None -> state
                    | None -> addNamedEvent OwnershipUseKind.Escape escapeOrigin name state
                | _ -> state

            addDiagnostic
                borrowEscapeCode
                "A value that captures a borrowed region cannot escape its protected scope."
                escapeOrigin
                relatedLocations
                document
                state

    let private checkEscape (document: ParsedDocument) expression state =
        checkEscapeAgainstAllowed Set.empty document expression state

    let private addBorrowLocksForPattern (sourcePlaces: ResourcePlace list) pattern origin state =
        let boundPaths =
            match collectPatternBindings pattern with
            | [] -> [ [] ]
            | bindings -> bindings |> List.map snd

        (state, boundPaths)
        ||> List.fold (fun current boundPath ->
            sourcePlaces
            |> List.fold (fun nextState sourcePlace ->
                let place = makePlace sourcePlace.Root (sourcePlace.Path @ boundPath)

                addBorrowLock place.Root (expandBorrowFootprintPaths nextState place) origin nextState) current)

    let private introduceHiddenBorrowRoot scopeLabel state =
        let hiddenName = $"__tmp{state.NextBindingId}"

        ResourcePlace.root hiddenName,
        addBinding LocalBinding hiddenName (Some ResourceQuantity.one) None Set.empty [] false None None None state

    let private prepareBorrowPatternSource projectionSummaries scopeLabel expression state =
        match tryBorrowablePlacesWithEnv Map.empty projectionSummaries state expression with
        | Some [ place ] ->
            Some place, [ place ], state
        | Some places ->
            None, places, state
        | None ->
            let place, state = introduceHiddenBorrowRoot scopeLabel state
            Some place, [ place ], state

    let rec private expressionResultMayCarryBorrow state expression =
        match expression with
        | SyntaxQuote _
        | SyntaxSplice _ ->
            false
        | TopLevelSyntaxSplice inner ->
            match tryDirectSyntaxSpliceBody expression with
            | Some quotedBody -> expressionResultMayCarryBorrow state quotedBody
            | None -> false
        | CodeQuote inner
        | CodeSplice inner ->
            expressionResultMayCarryBorrow state inner
        | Handle(_, label, body, returnClause, operationClauses) ->
            expressionResultMayCarryBorrow state label
            || expressionResultMayCarryBorrow state body
            || expressionResultMayCarryBorrow state returnClause.Body
            || (operationClauses |> List.exists (fun clause -> expressionResultMayCarryBorrow state clause.Body))
        | Name [ name ] ->
            match tryFindBinding name state with
            | Some binding ->
                binding.BorrowRegion.IsSome
                || not (Set.isEmpty binding.CapturedRegions)
                || binding.ClosureFactId.IsSome
                || binding.LocalLambda.IsSome
            | None ->
                false
        | Lambda _ ->
            not (Set.isEmpty (capturedRegions state expression))
        | Apply _ when tryDelayedExpressionBody expression |> Option.isSome ->
            not (Set.isEmpty (capturedRegions state expression))
        | LocalLet(_, _, body) ->
            expressionResultMayCarryBorrow state body
        | LocalSignature(_, body) ->
            expressionResultMayCarryBorrow state body
        | LocalTypeAlias(_, body) ->
            expressionResultMayCarryBorrow state body
        | LocalScopedEffect(_, body) ->
            expressionResultMayCarryBorrow state body
        | IfThenElse(_, whenTrue, whenFalse) ->
            expressionResultMayCarryBorrow state whenTrue || expressionResultMayCarryBorrow state whenFalse
        | Match(_, cases) ->
            cases |> List.exists (fun caseClause -> expressionResultMayCarryBorrow state caseClause.Body)
        | RecordLiteral fields ->
            fields |> List.exists (fun field -> expressionResultMayCarryBorrow state field.Value)
        | Seal(value, _) ->
            expressionResultMayCarryBorrow state value
        | Apply(Name [ "captureBorrow" ], [ _ ]) ->
            not (Set.isEmpty (capturedRegions state expression))
        | Do statements ->
            match List.tryLast statements with
            | Some(DoExpression result)
            | Some(DoReturn result) -> expressionResultMayCarryBorrow state result
            | _ -> false
        | MonadicSplice inner
        | ExplicitImplicitArgument inner
        | InoutArgument inner
        | Unary(_, inner) ->
            expressionResultMayCarryBorrow state inner
        | NamedApplicationBlock fields ->
            fields |> List.exists (fun field -> expressionResultMayCarryBorrow state field.Value)
        | Elvis _ ->
            not (Set.isEmpty (capturedRegions state expression))
        | Comprehension comprehension ->
            expressionResultMayCarryBorrow state comprehension.Lowered
        | Literal _
        | NumericLiteral _
        | KindQualifiedName _
        | Name _
        | RecordUpdate _
        | SafeNavigation _
        | MemberAccess _
        | Apply _
        | Binary _
        | TagTest _
        | PrefixedString _ ->
            false

    let rec private expressionMayCarryEscapingBorrow expression =
        match expression with
        | SyntaxQuote _
        | SyntaxSplice _ ->
            false
        | TopLevelSyntaxSplice inner ->
            match tryDirectSyntaxSpliceBody expression with
            | Some quotedBody -> expressionMayCarryEscapingBorrow quotedBody
            | None -> false
        | CodeQuote inner
        | CodeSplice inner ->
            expressionMayCarryEscapingBorrow inner
        | Handle(_, label, body, returnClause, operationClauses) ->
            expressionMayCarryEscapingBorrow label
            || expressionMayCarryEscapingBorrow body
            || expressionMayCarryEscapingBorrow returnClause.Body
            || (operationClauses |> List.exists (fun clause -> expressionMayCarryEscapingBorrow clause.Body))
        | Lambda _ ->
            true
        | Name [ name ] ->
            true
        | LocalLet(_, _, body) ->
            expressionMayCarryEscapingBorrow body
        | LocalSignature(_, body) ->
            expressionMayCarryEscapingBorrow body
        | LocalTypeAlias(_, body) ->
            expressionMayCarryEscapingBorrow body
        | LocalScopedEffect(_, body) ->
            expressionMayCarryEscapingBorrow body
        | IfThenElse(_, whenTrue, whenFalse) ->
            expressionMayCarryEscapingBorrow whenTrue || expressionMayCarryEscapingBorrow whenFalse
        | Match(_, cases) ->
            cases |> List.exists (fun caseClause -> expressionMayCarryEscapingBorrow caseClause.Body)
        | RecordLiteral fields ->
            fields |> List.exists (fun field -> expressionMayCarryEscapingBorrow field.Value)
        | Seal(value, _) ->
            expressionMayCarryEscapingBorrow value
        | TagTest(receiver, _) ->
            expressionMayCarryEscapingBorrow receiver
        | SafeNavigation _ ->
            false
        | Do statements ->
            match List.tryLast statements with
            | Some(DoExpression result) -> expressionMayCarryEscapingBorrow result
            | Some(DoReturn result) -> expressionMayCarryEscapingBorrow result
            | _ -> false
        | Apply _ when tryDelayedExpressionBody expression |> Option.isSome ->
            true
        | MonadicSplice inner
        | ExplicitImplicitArgument inner
        | InoutArgument inner
        | Unary(_, inner) ->
            expressionMayCarryEscapingBorrow inner
        | NamedApplicationBlock fields ->
            fields |> List.exists (fun field -> expressionMayCarryEscapingBorrow field.Value)
        | Comprehension comprehension ->
            expressionMayCarryEscapingBorrow comprehension.Lowered
        | Literal _
        | NumericLiteral _
        | KindQualifiedName _
        | Name _
        | RecordUpdate _
        | SafeNavigation _
        | MemberAccess _
        | Apply _
        | Binary _
        | Elvis _
        | PrefixedString _ ->
            false

    let rec private checkResultEscapeAtBoundary allowedRegions (document: ParsedDocument) expression state =
        match expression with
        | SyntaxQuote _
        | SyntaxSplice _ ->
            state
        | TopLevelSyntaxSplice inner ->
            match tryDirectSyntaxSpliceBody expression with
            | Some quotedBody -> checkResultEscapeAtBoundary allowedRegions document quotedBody state
            | None -> state
        | CodeQuote inner
        | CodeSplice inner ->
            checkResultEscapeAtBoundary allowedRegions document inner state
        | Handle(_, label, body, returnClause, operationClauses) ->
            let nextState =
                state
                |> checkResultEscapeAtBoundary allowedRegions document label
                |> checkResultEscapeAtBoundary allowedRegions document body
                |> checkResultEscapeAtBoundary allowedRegions document returnClause.Body

            operationClauses
            |> List.fold
                (fun clauseState clause -> checkResultEscapeAtBoundary allowedRegions document clause.Body clauseState)
                nextState
        | Lambda _ ->
            checkEscapeAgainstAllowed allowedRegions document expression state
        | Name [ name ] ->
            match tryFindBinding name state with
            | Some binding when not (Set.isEmpty binding.CapturedRegions)
                                || binding.ClosureFactId.IsSome
                                || binding.LocalLambda.IsSome ->
                checkEscapeAgainstAllowed allowedRegions document expression state
            | _ ->
                state
        | LocalLet(_, _, body) ->
            checkResultEscapeAtBoundary allowedRegions document body state
        | LocalSignature(_, body) ->
            checkResultEscapeAtBoundary allowedRegions document body state
        | LocalTypeAlias(_, body) ->
            checkResultEscapeAtBoundary allowedRegions document body state
        | LocalScopedEffect(_, body) ->
            checkResultEscapeAtBoundary allowedRegions document body state
        | IfThenElse(_, whenTrue, whenFalse) ->
            state
            |> checkResultEscapeAtBoundary allowedRegions document whenTrue
            |> checkResultEscapeAtBoundary allowedRegions document whenFalse
        | Match(_, cases) ->
            (state, cases)
            ||> List.fold (fun current caseClause ->
                checkResultEscapeAtBoundary allowedRegions document caseClause.Body current)
        | RecordLiteral fields ->
            (state, fields)
            ||> List.fold (fun current field ->
                checkResultEscapeAtBoundary allowedRegions document field.Value current)
        | Seal(value, _) ->
            checkResultEscapeAtBoundary allowedRegions document value state
        | TagTest(receiver, _) ->
            checkResultEscapeAtBoundary allowedRegions document receiver state
        | Elvis _ ->
            checkEscapeAgainstAllowed allowedRegions document expression state
        | Do statements ->
            match List.tryLast statements with
            | Some(DoExpression result) ->
                checkResultEscapeAtBoundary allowedRegions document result state
            | Some(DoReturn result) ->
                checkResultEscapeAtBoundary allowedRegions document result state
            | _ ->
                state
        | MonadicSplice inner
        | ExplicitImplicitArgument inner
        | InoutArgument inner
        | Unary(_, inner) ->
            checkResultEscapeAtBoundary allowedRegions document inner state
        | NamedApplicationBlock fields ->
            (state, fields)
            ||> List.fold (fun current field ->
                checkResultEscapeAtBoundary allowedRegions document field.Value current)
        | Apply _ when tryDelayedExpressionBody expression |> Option.isSome ->
            checkEscapeAgainstAllowed allowedRegions document expression state
        | Apply(Name [ "captureBorrow" ], [ _ ]) ->
            checkEscapeAgainstAllowed allowedRegions document expression state
        | Comprehension comprehension ->
            checkResultEscapeAtBoundary allowedRegions document comprehension.Lowered state
        | Literal _
        | NumericLiteral _
        | KindQualifiedName _
        | Name _
        | Apply _
        | Binary _
        | RecordUpdate _
        | SafeNavigation _
        | MemberAccess _
        | PrefixedString _ ->
            state

    let private checkEscapingLambdaAgainst allowedRegions (document: ParsedDocument) expression state =
        match expression with
        | Lambda _ ->
            let captured = capturedRegions state expression
            let capturedBindingList = capturedBindings state expression

            let state =
                capturedBindingList
                |> List.fold (fun current binding ->
                    addEvent OwnershipUseKind.Capture (findUseLocation document binding.Name 1) binding current) state

            let state =
                capturedBindingList
                |> List.filter (fun binding -> binding.DeclaredQuantity = Some ResourceQuantity.zero)
                |> List.fold (fun current binding ->
                    addDiagnostic
                        erasedRuntimeUseCode
                        $"A runtime closure cannot capture erased quantity-0 binding '{binding.Name}'."
                        (findUseLocation document binding.Name 1)
                        []
                        document
                        current) state

            let escaped =
                captured
                |> Set.filter (fun regionId -> not (Set.contains regionId allowedRegions))

            if Set.isEmpty escaped then
                state
            else
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

    let private checkEscapingLambda document expression state =
        checkEscapingLambdaAgainst Set.empty document expression state

    let private checkLambdaParameterEscape (document: ParsedDocument) expression state =
        let addLambdaParameterBinding (parameter: Parameter) current =
            let quantity =
                if parameter.IsInout then
                    Some ResourceQuantity.one
                else
                    parameter.Quantity |> Option.map ResourceQuantity.ofSurface

            let checkDrop = quantity |> Option.exists ResourceQuantity.requiresUse
            let capturedRegions =
                parameter.TypeTokens
                |> Option.bind TypeSignatures.parseType
                |> Option.map captureSetFromType
                |> Option.defaultValue Set.empty

            let region: BorrowRegion option =
                match quantity with
                | Some(ResourceQuantity.Borrow explicitRegion) ->
                    Some
                        { Id = defaultArg explicitRegion $"rho_param_{parameter.Name}"
                          ExplicitName = explicitRegion
                          OwnerScope = $"parameter:{parameter.Name}" }
                | _ -> None

            let current =
                match region with
                | Some region -> addBorrowRegionFact (findBinderLocation document parameter.Name) region current
                | None -> current

            addBinding ParameterBinding parameter.Name quantity region capturedRegions [] checkDrop None None (findBinderLocation document parameter.Name) current

        match expression with
        | Lambda(parameters, body) ->
            let parameterState =
                parameters
                |> List.fold (fun current parameter -> addLambdaParameterBinding parameter current) (emptyState $"{currentScopeId state}.lambda")

            let checkedState = checkResultEscapeAtBoundary Set.empty document body parameterState

            { state with
                Diagnostics = state.Diagnostics @ checkedState.Diagnostics }
        | _ ->
            state

    let private currentScopeBindingIds (state: CheckState) =
        state.ActiveScopes
        |> List.head
        |> fun scope -> scope.IntroducedBindings |> List.map snd

    let private isResourceDiagnosticCode code =
        match code with
        | DiagnosticCode.QttLinearDrop
        | DiagnosticCode.QttLinearOveruse
        | DiagnosticCode.QttBorrowConsume
        | DiagnosticCode.QttBorrowOverlap
        | DiagnosticCode.QttBorrowEscape
        | DiagnosticCode.QttContinuationCapture
        | DiagnosticCode.QttErasedRuntimeUse
        | DiagnosticCode.QttInoutMarkerRequired
        | DiagnosticCode.QttInoutMarkerUnexpected
        | DiagnosticCode.QttInoutThreadedFieldMissing ->
            true
        | _ ->
            false

    let private hasPriorNonDropResourceError (state: CheckState) =
        state.Diagnostics
        |> List.exists (fun diagnostic ->
            diagnostic.Severity = Error
            && isResourceDiagnosticCode diagnostic.Code
            && diagnostic.Code <> linearOveruseCode
            && diagnostic.Code <> linearDropCode)

    let private hasPriorLinearDropAt origin (state: CheckState) =
        state.Diagnostics
        |> List.exists (fun diagnostic ->
            diagnostic.Code = linearDropCode
            && diagnostic.Location = origin)

    let private checkScopeLinearDrops (document: ParsedDocument) (state: CheckState) =
        currentScopeBindingIds state
        |> List.fold (fun current bindingId ->
            let binding = current.Bindings[bindingId]

            if binding.CheckLinearDrop
               && binding.UseMinimum < 1
               && not (hasPriorNonDropResourceError current)
               && not (hasPriorLinearDropAt binding.Origin current) then
                addDiagnostic
                    linearDropCode
                    $"Linear resource '{binding.Name}' is not consumed on every path."
                    binding.Origin
                    []
                    document
                    current
            else
                current) state

    let private allActiveScopeBindingIds (state: CheckState) =
        state.ActiveScopes
        |> List.collect (fun scope -> scope.IntroducedBindings |> List.map snd)
        |> List.distinct

    let private checkAllActiveLinearDrops (document: ParsedDocument) (state: CheckState) =
        allActiveScopeBindingIds state
        |> List.fold (fun current bindingId ->
            let binding = current.Bindings[bindingId]

            if binding.CheckLinearDrop
               && binding.UseMinimum < 1
               && not (hasPriorNonDropResourceError current)
               && not (hasPriorLinearDropAt binding.Origin current) then
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

    let private tryBuildReceiverMethodArgumentsForDocument
        (document: ParsedDocument)
        receiverExpression
        memberName
        arguments
        =
        document.Syntax.Declarations
        |> List.tryPick (function
            | LetDeclaration definition
                when definition.Name
                     |> Option.exists (fun name -> String.Equals(name, memberName, StringComparison.Ordinal)) ->
                let receiverParameters =
                    definition.Parameters
                    |> List.mapi (fun index parameter -> index, parameter)
                    |> List.filter (fun (_, parameter) -> parameter.IsReceiver)

                match receiverParameters with
                | [ receiverIndex, receiverParameter ]
                    when receiverParameter
                         |> parameterQuantity
                         |> Option.exists (fun quantity -> ResourceQuantity.requiresUse quantity || ResourceQuantity.isBorrow quantity) ->
                    let precedingExplicitCount =
                        definition.Parameters
                        |> List.take receiverIndex
                        |> List.filter (fun parameter -> not parameter.IsImplicit)
                        |> List.length

                    if List.length arguments >= precedingExplicitCount then
                        let precedingArguments = arguments |> List.take precedingExplicitCount
                        let followingArguments = arguments |> List.skip precedingExplicitCount
                        Some(precedingArguments @ [ receiverExpression ] @ followingArguments)
                    else
                        None
                | _ ->
                    None
            | _ ->
                None)

    let private addLambdaExecutionParameterBinding (document: ParsedDocument) (parameter: Parameter) current =
        let quantity = parameterQuantity parameter
        let checkDrop = quantity |> Option.exists ResourceQuantity.requiresUse
        let capturedRegions =
            parameter.TypeTokens
            |> Option.bind TypeSignatures.parseType
            |> Option.map captureSetFromType
            |> Option.defaultValue Set.empty

        let region, current =
            introduceBorrowRegionForQuantity
                $"{currentScopeId current}.lambda_param"
                quantity
                (findBinderLocation document parameter.Name)
                current

        addBinding
            ParameterBinding
            parameter.Name
            quantity
            region
            capturedRegions
            []
            checkDrop
            None
            None
            (findBinderLocation document parameter.Name)
            current

    let rec private checkLocalLambdaInvocation
        projectionSummaries
        (document: ParsedDocument)
        (signatures: Map<string, FunctionSignature>)
        (localTypes: Map<string, TypeSignatures.TypeExpr>)
        state
        (lambdaValue: LocalLambda)
        arguments
        =
        if List.length arguments <> List.length lambdaValue.Parameters then
            state
        elif
            lambdaValue.Identity
            |> Option.exists (fun identity -> List.contains identity state.ActiveLocalLambdaInvocations)
        then
            state
        else
            let allowedRegions = liveRegionIds state
            let state =
                match lambdaValue.Identity with
                | Some identity ->
                    { state with
                        ActiveLocalLambdaInvocations = identity :: state.ActiveLocalLambdaInvocations }
                | None ->
                    state

            let checkedState =
                withScope "lambda_call" (fun scopedState ->
                    let scopedState =
                        lambdaValue.CapturedBindings
                        |> List.fold (fun current binding -> addBindingSnapshot binding current) scopedState

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
                                        introduceBorrowRegionForQuantity
                                            $"{currentScopeId current}.param"
                                            quantity
                                            (argumentLocation document argument)
                                            current
                                | _ ->
                                    introduceBorrowRegionForQuantity
                                        $"{currentScopeId current}.param"
                                        quantity
                                        (argumentLocation document argument)
                                        current

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
                                ParameterBinding
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

                    let lambdaLocalTypes =
                        (localTypes, List.zip lambdaValue.Parameters arguments)
                        ||> List.fold (fun current (parameter, argument) ->
                            let parameterType =
                                parameter.TypeTokens
                                |> Option.bind TypeSignatures.parseType
                                |> Option.orElseWith (fun () -> tryInferConservativeExpressionType signatures localTypes argument)

                            match parameterType with
                            | Some parameterType -> Map.add parameter.Name parameterType current
                            | None -> current)

                    scopedState
                    |> noteValueDemandingNameUse document lambdaValue.Body
                    |> fun current -> checkExpression projectionSummaries document signatures lambdaLocalTypes current lambdaValue.Body
                    |> checkResultEscapeAtBoundary allowedRegions document lambdaValue.Body
                    |> checkScopeLinearDrops document) state

            match lambdaValue.Identity with
            | Some identity ->
                { checkedState with
                    ActiveLocalLambdaInvocations =
                        checkedState.ActiveLocalLambdaInvocations
                        |> List.filter (fun activeIdentity -> not (String.Equals(activeIdentity, identity, StringComparison.Ordinal))) }
            | None ->
                checkedState

    and private checkImmediateLambdaDemand
        projectionSummaries
        document
        signatures
        localTypes
        state
        (parameters: Parameter list)
        body
        =
        withScope "lambda_arg" (fun scopedState ->
            let lambdaLocalTypes =
                (localTypes, parameters)
                ||> List.fold (fun current parameter ->
                    match parameter.TypeTokens |> Option.bind TypeSignatures.parseType with
                    | Some parameterType -> Map.add parameter.Name parameterType current
                    | None -> current)

            parameters
            |> List.fold (fun current parameter -> addLambdaExecutionParameterBinding document parameter current) scopedState
            |> noteValueDemandingNameUse document body
            |> fun current -> checkExpression projectionSummaries document signatures lambdaLocalTypes current body
            |> checkResultEscapeAtBoundary (liveRegionIds state) document body
            |> checkScopeLinearDrops document) state

    and private checkExpression
        projectionSummaries
        (document: ParsedDocument)
        (signatures: Map<string, FunctionSignature>)
        (localTypes: Map<string, TypeSignatures.TypeExpr>)
        state
        expression
        =
        let inferQuerySourceInfo locals source =
            tryInferConservativeExpressionType signatures locals source
            |> Option.bind (QuerySemantics.tryInferBuiltinQuerySource id)

        let queryUseText useMode =
            match useMode with
            | QuerySemantics.Reusable -> "Reusable"
            | QuerySemantics.OneShot -> "OneShot"

        let queryCardText card =
            match card with
            | QuerySemantics.QZero -> "QZero"
            | QuerySemantics.QOne -> "QOne"
            | QuerySemantics.QZeroOrOne -> "QZeroOrOne"
            | QuerySemantics.QOneOrMore -> "QOneOrMore"
            | QuerySemantics.QZeroOrMore -> "QZeroOrMore"

        let queryQuantityExpr quantity =
            match quantity with
            | ResourceQuantity.Interval(0, Some 0) -> TypeSignatures.TypeName([ "0" ], [])
            | ResourceQuantity.Interval(1, Some 1) -> TypeSignatures.TypeName([ "1" ], [])
            | ResourceQuantity.Interval(0, None) -> TypeSignatures.TypeName([ "ω" ], [])
            | ResourceQuantity.Interval(0, Some 1) -> TypeSignatures.TypeName([ "<=1" ], [])
            | ResourceQuantity.Interval(1, None) -> TypeSignatures.TypeName([ ">=1" ], [])
            | ResourceQuantity.Borrow None -> TypeSignatures.TypeName([ "&" ], [])
            | ResourceQuantity.Borrow(Some regionName) -> TypeSignatures.TypeName([ $"&[{regionName}]" ], [])
            | ResourceQuantity.Variable name -> TypeSignatures.TypeName([ name ], [])
            | ResourceQuantity.Interval(minimum, Some maximum) when minimum = maximum ->
                TypeSignatures.TypeName([ string minimum ], [])
            | ResourceQuantity.Interval(minimum, Some maximum) ->
                TypeSignatures.TypeName([ $"[{minimum},{maximum}]" ], [])
            | ResourceQuantity.Interval(minimum, None) ->
                TypeSignatures.TypeName([ $"[{minimum},inf]" ], [])

        let buildQueryTypeExpr (queryInfo: QuerySemantics.QueryTypeInfo) =
            let buildModeExpr (mode: QuerySemantics.QueryModeInfo) =
                let useExpr =
                    TypeSignatures.TypeName(
                        [
                            match mode.Use with
                            | QuerySemantics.Reusable -> "Reusable"
                            | QuerySemantics.OneShot -> "OneShot"
                        ],
                        []
                    )

                let cardExpr =
                    TypeSignatures.TypeName([ queryCardText mode.Card ], [])

                TypeSignatures.TypeName([ "QueryMode" ], [ useExpr; cardExpr ])

            let baseType =
                match queryInfo.Mode.Use, queryInfo.Mode.Card, queryInfo.ItemQuantity with
                | QuerySemantics.Reusable, QuerySemantics.QZeroOrMore, ResourceQuantity.Interval(0, None) ->
                    TypeSignatures.TypeName([ "Query" ], [ queryInfo.ItemType ])
                | QuerySemantics.OneShot, QuerySemantics.QZeroOrMore, ResourceQuantity.Interval(0, None) ->
                    TypeSignatures.TypeName([ "OnceQuery" ], [ queryInfo.ItemType ])
                | QuerySemantics.Reusable, QuerySemantics.QZeroOrOne, ResourceQuantity.Interval(0, None) ->
                    TypeSignatures.TypeName([ "OptionalQuery" ], [ queryInfo.ItemType ])
                | QuerySemantics.Reusable, QuerySemantics.QOneOrMore, ResourceQuantity.Interval(0, None) ->
                    TypeSignatures.TypeName([ "NonEmptyQuery" ], [ queryInfo.ItemType ])
                | QuerySemantics.Reusable, QuerySemantics.QOne, ResourceQuantity.Interval(0, None) ->
                    TypeSignatures.TypeName([ "SingletonQuery" ], [ queryInfo.ItemType ])
                | _ ->
                    TypeSignatures.TypeName(
                        [ "QueryCore" ],
                        [ buildModeExpr queryInfo.Mode; queryQuantityExpr queryInfo.ItemQuantity; queryInfo.ItemType ]
                    )

            if Set.isEmpty queryInfo.CaptureSet then
                baseType
            else
                TypeSignatures.TypeCapture(baseType, queryInfo.CaptureSet |> Set.toList)

        let currentRowBindings current =
            currentScopeBindingIds current
            |> List.choose (fun bindingId ->
                Map.tryFind bindingId current.Bindings
                |> Option.map (fun binding -> bindingId, binding))
            |> List.filter (fun (_, binding) -> binding.BindingKind = PatternBinding)

        let currentExactOneRowBindings current =
            currentRowBindings current |> List.filter (fun (_, binding) -> isExactOneBinding binding)

        let restoreBindingSnapshots snapshots current =
            let restoredBindings =
                snapshots
                |> Map.fold (fun bindings bindingId binding -> Map.add bindingId binding bindings) current.Bindings

            { current with
                Bindings = restoredBindings }

        let addClauseDiagnostic code message clauseExpression current =
            addDiagnostic code message (argumentLocation document clauseExpression) [] document current

        let addExactOneRowClauseDiagnostic code message clauseExpression current =
            match currentExactOneRowBindings current with
            | [] ->
                current
            | (_, binding) :: _ ->
                addClauseDiagnostic code (message binding.Name) clauseExpression current

        let checkNonConsumingRowExpression clauseLabel clauseExpression currentLocals current =
            let rowSnapshots =
                currentExactOneRowBindings current |> Map.ofList

            let checkedState =
                current
                |> noteValueDemandingNameUse document clauseExpression
                |> fun next -> checkExpression projectionSummaries document signatures currentLocals next clauseExpression

            let consumedRowBindings =
                rowSnapshots
                |> Map.toList
                |> List.choose (fun (bindingId, beforeBinding) ->
                    match Map.tryFind bindingId checkedState.Bindings with
                    | Some afterBinding
                        when afterBinding.UseMaximum > beforeBinding.UseMaximum
                             || List.length afterBinding.ConsumedPaths > List.length beforeBinding.ConsumedPaths ->
                        Some beforeBinding.Name
                    | _ -> None)

            let restored = restoreBindingSnapshots rowSnapshots checkedState

            match consumedRowBindings with
            | [] ->
                restored
            | bindingName :: _ ->
                addClauseDiagnostic
                    linearOveruseCode
                    $"`{clauseLabel}` must not consume row binding '{bindingName}'."
                    clauseExpression
                    restored

        let addCardinalityDiagnostic clauseLabel card clauseExpression current =
            match currentExactOneRowBindings current with
            | [] ->
                current
            | (_, binding) :: _ ->
                let mayDrop = QuerySemantics.mayDiscardRows card
                let mayDuplicate = QuerySemantics.mayDuplicateRows card
                let code =
                    if mayDuplicate then
                        linearOveruseCode
                    else
                        linearDropCode

                let effectText =
                    match mayDrop, mayDuplicate with
                    | true, true -> "duplicate or discard"
                    | true, false -> "discard"
                    | false, true -> "duplicate"
                    | false, false -> "reorder"

                addClauseDiagnostic
                    code
                    $"`{clauseLabel}` may {effectText} linear row binding '{binding.Name}' because the clause cardinality is {queryCardText card}."
                    clauseExpression
                    current

        let addDropDiagnostic clauseLabel clauseExpression current =
            addExactOneRowClauseDiagnostic
                linearDropCode
                (fun bindingName -> $"`{clauseLabel}` may discard linear row binding '{bindingName}'.")
                clauseExpression
                current

        let addLeftJoinCaptureDiagnostic source condition current =
            let outerLinearNames =
                currentExactOneRowBindings current
                |> List.map (fun (_, binding) -> binding.Name)
                |> Set.ofList

            let referencedLinearName =
                [ source; condition ]
                |> List.collect (ResourceCheckingSurface.expressionNames >> Seq.toList)
                |> List.tryFind (fun name -> Set.contains name outerLinearNames)

            match referencedLinearName with
            | Some bindingName ->
                addClauseDiagnostic
                    linearOveruseCode
                    $"`left join ... into` would capture linear outer row binding '{bindingName}' for delayed query use."
                    condition
                    current
            | None ->
                current

        let rec checkComprehensionClauses currentLocals current (comprehension: SurfaceComprehension) clauses =
            match clauses with
            | [] ->
                let applyYield current expression =
                    current
                    |> noteValueDemandingNameUse document expression
                    |> fun next -> checkExpression projectionSummaries document signatures currentLocals next expression

                match comprehension.Yield with
                | YieldValue value ->
                    applyYield current value
                | YieldKeyValue(key, value) ->
                    applyYield current key
                    |> fun next -> applyYield next value
            | clause :: rest ->
                match clause with
                | ForClause(_, _, binding, source) ->
                    let current =
                        current
                        |> noteValueDemandingNameUse document source
                        |> fun next -> checkExpression projectionSummaries document signatures currentLocals next source

                    let current =
                        match inferQuerySourceInfo currentLocals source with
                        | Some sourceInfo when not (List.isEmpty (currentExactOneRowBindings current)) ->
                            if
                                QuerySemantics.mayDiscardRows sourceInfo.Query.Mode.Card
                                || QuerySemantics.mayDuplicateRows sourceInfo.Query.Mode.Card
                            then
                                addCardinalityDiagnostic "for" sourceInfo.Query.Mode.Card source current
                            else
                                current
                        | _ ->
                            current

                    let valueType =
                        inferQuerySourceInfo currentLocals source |> Option.map (fun sourceInfo -> sourceInfo.Query.ItemType)

                    let declaredQuantity =
                        inferQuerySourceInfo currentLocals source
                        |> Option.map (fun sourceInfo ->
                            if binding.Quantity.IsSome then
                                binding.Quantity |> Option.map ResourceQuantity.ofSurface |> Option.get
                            else
                                sourceInfo.Query.ItemQuantity)
                        |> Option.orElseWith (fun () -> binding.Quantity |> Option.map ResourceQuantity.ofSurface)

                    let capturedRegions =
                        inferQuerySourceInfo currentLocals source
                        |> Option.map (fun sourceInfo -> sourceInfo.Query.CaptureSet)
                        |> Option.defaultValue Set.empty

                    let nextState =
                        addPatternBindings
                            document
                            binding
                            declaredQuantity
                            None
                            None
                            capturedRegions
                            []
                            (declaredQuantity |> Option.exists ResourceQuantity.requiresUse)
                            None
                            None
                            current

                    let nextLocals =
                        extendBindingLocalTypes signatures currentLocals valueType binding

                    checkComprehensionClauses nextLocals nextState comprehension rest
                | LetClause(_, binding, value) ->
                    let current =
                        current
                        |> noteValueDemandingNameUse document value
                        |> fun next -> checkExpression projectionSummaries document signatures currentLocals next value

                    let valueType = tryInferConservativeExpressionType signatures currentLocals value
                    let declaredQuantity =
                        binding.Quantity
                        |> Option.map ResourceQuantity.ofSurface
                        |> Option.orElseWith (fun () -> tryConstructorPatternQuantity document binding.Pattern)

                    let nextState =
                        addPatternBindings
                            document
                            binding
                            declaredQuantity
                            None
                            None
                            Set.empty
                            []
                            (declaredQuantity |> Option.exists ResourceQuantity.requiresUse)
                            None
                            None
                            current

                    let nextLocals =
                        extendBindingLocalTypes signatures currentLocals valueType binding

                    checkComprehensionClauses nextLocals nextState comprehension rest
                | JoinClause(binding, source, condition) ->
                    let current =
                        current
                        |> noteValueDemandingNameUse document source
                        |> fun next -> checkExpression projectionSummaries document signatures currentLocals next source

                    let valueType =
                        inferQuerySourceInfo currentLocals source |> Option.map (fun sourceInfo -> sourceInfo.Query.ItemType)

                    let declaredQuantity =
                        inferQuerySourceInfo currentLocals source
                        |> Option.map (fun sourceInfo -> sourceInfo.Query.ItemQuantity)

                    let capturedRegions =
                        inferQuerySourceInfo currentLocals source
                        |> Option.map (fun sourceInfo -> sourceInfo.Query.CaptureSet)
                        |> Option.defaultValue Set.empty

                    let nextState =
                        addPatternBindings
                            document
                            binding
                            declaredQuantity
                            None
                            None
                            capturedRegions
                            []
                            (declaredQuantity |> Option.exists ResourceQuantity.requiresUse)
                            None
                            None
                            current

                    let nextLocals =
                        extendBindingLocalTypes signatures currentLocals valueType binding

                    let conditionedState =
                        nextState
                        |> noteValueDemandingNameUse document condition
                        |> fun next -> checkExpression projectionSummaries document signatures nextLocals next condition
                        |> addDropDiagnostic "join" condition

                    checkComprehensionClauses nextLocals conditionedState comprehension rest
                | IfClause condition ->
                    let current =
                        current
                        |> noteValueDemandingNameUse document condition
                        |> fun next -> checkExpression projectionSummaries document signatures currentLocals next condition
                        |> addDropDiagnostic "if" condition

                    checkComprehensionClauses currentLocals current comprehension rest
                | GroupByClause(key, aggregations, intoName) ->
                    let current =
                        current
                        |> checkNonConsumingRowExpression "group by" key currentLocals
                        |> addDropDiagnostic "group by" key

                    let current =
                        aggregations
                        |> List.fold (fun state field ->
                            state
                            |> noteValueDemandingNameUse document field.Value
                            |> fun next -> checkExpression projectionSummaries document signatures currentLocals next field.Value) current

                    let nextLocals =
                        Map.add intoName (TypeSignatures.TypeVariable $"__kappa_group_{intoName}") Map.empty

                    checkComprehensionClauses nextLocals current comprehension rest
                | OrderByClause(_, key) ->
                    let current = checkNonConsumingRowExpression "order by" key currentLocals current
                    checkComprehensionClauses currentLocals current comprehension rest
                | DistinctClause ->
                    let distinctKey =
                        match comprehension.Yield with
                        | YieldValue value -> value
                        | YieldKeyValue(key, _) -> key

                    let current = addDropDiagnostic "distinct" distinctKey current
                    checkComprehensionClauses currentLocals current comprehension rest
                | DistinctByClause key ->
                    let current =
                        current
                        |> checkNonConsumingRowExpression "distinct by" key currentLocals
                        |> addDropDiagnostic "distinct by" key

                    checkComprehensionClauses currentLocals current comprehension rest
                | SkipClause count ->
                    let current =
                        current
                        |> noteValueDemandingNameUse document count
                        |> fun next -> checkExpression projectionSummaries document signatures currentLocals next count
                        |> addDropDiagnostic "skip" count

                    checkComprehensionClauses currentLocals current comprehension rest
                | TakeClause count ->
                    let current =
                        current
                        |> noteValueDemandingNameUse document count
                        |> fun next -> checkExpression projectionSummaries document signatures currentLocals next count
                        |> addDropDiagnostic "take" count

                    checkComprehensionClauses currentLocals current comprehension rest
                | LeftJoinClause(binding, source, condition, intoName) ->
                    let current =
                        current
                        |> noteValueDemandingNameUse document source
                        |> fun next -> checkExpression projectionSummaries document signatures currentLocals next source
                        |> addLeftJoinCaptureDiagnostic source condition

                    let sourceInfo = inferQuerySourceInfo currentLocals source
                    let joinLocalTypes =
                        extendBindingLocalTypes
                            signatures
                            currentLocals
                            (sourceInfo |> Option.map (fun info -> info.Query.ItemType))
                            binding

                    let current =
                        withScope "left_join_on" (fun joinState ->
                            let joinQuantity =
                                sourceInfo
                                |> Option.map (fun info -> info.Query.ItemQuantity)
                                |> Option.orElseWith (fun () -> binding.Quantity |> Option.map ResourceQuantity.ofSurface)

                            let joinState =
                                addPatternBindings
                                    document
                                    binding
                                    joinQuantity
                                    None
                                    None
                                    Set.empty
                                    []
                                    (joinQuantity |> Option.exists ResourceQuantity.requiresUse)
                                    None
                                    None
                                    joinState

                            joinState
                            |> noteValueDemandingNameUse document condition
                            |> fun next -> checkExpression projectionSummaries document signatures joinLocalTypes next condition
                            |> checkScopeLinearDrops document) current

                    let intoType =
                        sourceInfo
                        |> Option.map (fun info -> buildQueryTypeExpr info.Query)
                        |> Option.defaultValue (TypeSignatures.TypeVariable $"__kappa_query_{intoName}")

                    let nextState =
                        addBinding
                            LocalBinding
                            intoName
                            None
                            None
                            Set.empty
                            []
                            false
                            None
                            None
                            (argumentLocation document condition)
                            current

                    let nextLocals =
                        Map.add intoName intoType currentLocals

                    checkComprehensionClauses nextLocals nextState comprehension rest

        let aliasKey segments = SyntaxFacts.moduleNameToText segments

        let copyNestedEffectLabelAliases sourcePrefix targetPrefix (aliases: Map<string, string>) =
            let sourceKey = aliasKey sourcePrefix
            let sourceNestedPrefix = sourceKey + "."

            aliases
            |> Map.toList
            |> List.filter (fun (key, _) -> key.StartsWith(sourceNestedPrefix, StringComparison.Ordinal))
            |> List.fold (fun current (key, effectName) ->
                let suffix = key.Substring(sourceNestedPrefix.Length)
                Map.add (aliasKey targetPrefix + "." + suffix) effectName current) aliases

        let rec collectEffectLabelAliases
            (prefix: string list)
            (expression: SurfaceExpression)
            (aliases: Map<string, string>)
            =
            let addDirect effectName current =
                let current = Map.add (aliasKey prefix) effectName current

                match expression with
                | Name sourcePrefix ->
                    copyNestedEffectLabelAliases sourcePrefix prefix current
                | _ ->
                    current

            match expression with
            | Name [ effectName ]
            | KindQualifiedName(EffectLabelKind, [ effectName ])
                when tryFindScopedEffectDeclaration effectName |> Option.isSome ->
                addDirect effectName aliases
            | Name nameSegments when not (List.isEmpty nameSegments) ->
                let qualifiedName = SyntaxFacts.moduleNameToText nameSegments

                if tryFindScopedEffectDeclaration qualifiedName |> Option.isSome then
                    addDirect qualifiedName aliases
                else
                    aliases
            | RecordLiteral fields ->
                fields
                |> List.fold (fun current field ->
                    collectEffectLabelAliases (prefix @ [ field.Name ]) field.Value current) aliases
            | Seal(value, _) ->
                collectEffectLabelAliases prefix value aliases
            | _ ->
                aliases

        let rec rewriteEffectLabelAliasUses (aliases: Map<string, string>) current =
            let rewrite = rewriteEffectLabelAliasUses aliases

            match current with
            | Name nameSegments when not (List.isEmpty nameSegments) ->
                let rewritten =
                    [ List.length nameSegments .. -1 .. 1 ]
                    |> List.tryPick (fun prefixLength ->
                        let prefix = nameSegments |> List.take prefixLength
                        let suffix = nameSegments |> List.skip prefixLength

                        aliases
                        |> Map.tryFind (aliasKey prefix)
                        |> Option.map (fun effectName ->
                            if List.isEmpty suffix then
                                KindQualifiedName(EffectLabelKind, [ effectName ])
                            else
                                Name(effectName :: suffix)))

                rewritten |> Option.defaultValue current
            | Name _ ->
                current
            | SyntaxQuote inner ->
                SyntaxQuote(rewrite inner)
            | SyntaxSplice inner ->
                SyntaxSplice(rewrite inner)
            | TopLevelSyntaxSplice inner ->
                TopLevelSyntaxSplice(rewrite inner)
            | CodeQuote inner ->
                CodeQuote(rewrite inner)
            | CodeSplice inner ->
                CodeSplice(rewrite inner)
            | Handle(isDeep, label, body, returnClause, operationClauses) ->
                let rewriteClause (clause: SurfaceEffectHandlerClause) =
                    { clause with
                        Body = rewrite clause.Body }

                Handle(
                    isDeep,
                    rewrite label,
                    rewrite body,
                    rewriteClause returnClause,
                    operationClauses |> List.map rewriteClause
                )
            | LocalLet(binding, value, body) ->
                let shadowedAliases =
                    collectPatternNames binding.Pattern
                    |> List.fold (fun current name ->
                        current
                        |> Map.remove name
                        |> Map.filter (fun key _ ->
                            not (key.StartsWith(name + ".", StringComparison.Ordinal))))
                        aliases

                LocalLet(binding, rewrite value, rewriteEffectLabelAliasUses shadowedAliases body)
            | LocalSignature(declaration, body) ->
                let shadowedAliases =
                    aliases
                    |> Map.remove declaration.Name
                    |> Map.filter (fun key _ ->
                        not (key.StartsWith(declaration.Name + ".", StringComparison.Ordinal)))

                LocalSignature(declaration, rewriteEffectLabelAliasUses shadowedAliases body)
            | LocalTypeAlias(declaration, body) ->
                LocalTypeAlias(declaration, rewrite body)
            | LocalScopedEffect(declaration, body) ->
                let shadowedAliases =
                    aliases
                    |> Map.remove declaration.Name
                    |> Map.filter (fun key _ ->
                        not (key.StartsWith(declaration.Name + ".", StringComparison.Ordinal)))

                LocalScopedEffect(declaration, rewriteEffectLabelAliasUses shadowedAliases body)
            | Lambda(parameters, body) ->
                let shadowedAliases =
                    parameters
                    |> List.fold (fun current parameter ->
                        current
                        |> Map.remove parameter.Name
                        |> Map.filter (fun key _ ->
                            not (key.StartsWith(parameter.Name + ".", StringComparison.Ordinal))))
                        aliases

                Lambda(parameters, rewriteEffectLabelAliasUses shadowedAliases body)
            | IfThenElse(condition, whenTrue, whenFalse) ->
                IfThenElse(rewrite condition, rewrite whenTrue, rewrite whenFalse)
            | Match(scrutinee, cases) ->
                Match(
                    rewrite scrutinee,
                    cases
                    |> List.map (fun caseClause ->
                        let shadowedAliases =
                            collectPatternNames caseClause.Pattern
                            |> List.fold (fun current name ->
                                current
                                |> Map.remove name
                                |> Map.filter (fun key _ ->
                                    not (key.StartsWith(name + ".", StringComparison.Ordinal))))
                                aliases

                        { caseClause with
                            Guard = caseClause.Guard |> Option.map rewrite
                            Body = rewriteEffectLabelAliasUses shadowedAliases caseClause.Body })
                )
            | RecordLiteral fields ->
                RecordLiteral(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | Seal(value, ascriptionTokens) ->
                Seal(rewrite value, ascriptionTokens)
            | RecordUpdate(receiver, fields) ->
                RecordUpdate(rewrite receiver, fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | MemberAccess(receiver, segments, arguments) ->
                MemberAccess(rewrite receiver, segments, arguments |> List.map rewrite)
            | SafeNavigation(receiver, navigation) ->
                SafeNavigation(rewrite receiver, { navigation with Arguments = navigation.Arguments |> List.map rewrite })
            | TagTest(receiver, constructorName) ->
                TagTest(rewrite receiver, constructorName)
            | Do statements ->
                let rec rewriteDoStatement statement =
                    match statement with
                    | DoLet(binding, value) -> DoLet(binding, rewrite value)
                    | DoLetQuestion(binding, value, failure) ->
                        let rewrittenFailure =
                            failure
                            |> Option.map (fun block ->
                                { block with
                                    Body = block.Body |> List.map rewriteDoStatement })

                        DoLetQuestion(binding, rewrite value, rewrittenFailure)
                    | DoBind(binding, value) -> DoBind(binding, rewrite value)
                    | DoUsing(binding, value) -> DoUsing(binding, rewrite value)
                    | DoVar(name, value) -> DoVar(name, rewrite value)
                    | DoAssign(name, value) -> DoAssign(name, rewrite value)
                    | DoDefer value -> DoDefer(rewrite value)
                    | DoExpression value -> DoExpression(rewrite value)
                    | DoReturn value -> DoReturn(rewrite value)
                    | DoIf(condition, whenTrue, whenFalse) ->
                        DoIf(rewrite condition, whenTrue |> List.map rewriteDoStatement, whenFalse |> List.map rewriteDoStatement)
                    | DoWhile(condition, body) ->
                        DoWhile(rewrite condition, body |> List.map rewriteDoStatement)

                Do(statements |> List.map rewriteDoStatement)
            | MonadicSplice inner ->
                MonadicSplice(rewrite inner)
            | ExplicitImplicitArgument inner ->
                ExplicitImplicitArgument(rewrite inner)
            | NamedApplicationBlock fields ->
                NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | InoutArgument inner ->
                InoutArgument(rewrite inner)
            | Unary(operatorName, inner) ->
                Unary(operatorName, rewrite inner)
            | Apply(callee, arguments) ->
                Apply(rewrite callee, arguments |> List.map rewrite)
            | Binary(left, operatorName, right) ->
                Binary(rewrite left, operatorName, rewrite right)
            | Elvis(left, right) ->
                Elvis(rewrite left, rewrite right)
            | Comprehension comprehension ->
                Comprehension { comprehension with Lowered = rewrite comprehension.Lowered }
            | PrefixedString(prefix, parts) ->
                PrefixedString(
                    prefix,
                    parts
                    |> List.map (function
                        | StringText text -> StringText text
                        | StringInterpolation(inner, format) -> StringInterpolation(rewrite inner, format))
                )
            | Literal _
            | NumericLiteral _
            | KindQualifiedName _ ->
                current

        let tryResolveHandledEffectName expression =
            match expression with
            | Name [ effectName ]
            | KindQualifiedName(EffectLabelKind, [ effectName ]) ->
                tryFindScopedEffectDeclaration effectName |> Option.map (fun _ -> effectName)
            | Name nameSegments when not (List.isEmpty nameSegments) ->
                let qualifiedName = SyntaxFacts.moduleNameToText nameSegments
                tryFindScopedEffectDeclaration qualifiedName |> Option.map (fun _ -> qualifiedName)
            | _ ->
                None

        match expression with
        | CodeQuote inner ->
            state
            |> checkEscape document expression
            |> fun next -> checkExpression projectionSummaries document signatures localTypes next inner
        | SyntaxQuote inner ->
            expressionNames inner
            |> Seq.distinct
            |> Seq.fold (fun current name -> addNonConsumingDemand document name current) state
        | SyntaxSplice _ ->
            state
        | TopLevelSyntaxSplice inner ->
            match tryDirectSyntaxSpliceBody expression with
            | Some quotedBody -> checkExpression projectionSummaries document signatures localTypes state quotedBody
            | None -> checkExpression projectionSummaries document signatures localTypes state inner
        | CodeSplice inner ->
            checkExpression projectionSummaries document signatures localTypes state inner
        | Handle(_, label, body, returnClause, operationClauses) ->
            let bindHandlerClauseNames quantity argumentNames resumptionName current =
                let current =
                    argumentNames
                    |> List.fold (fun state name -> addBinding LocalBinding name None None Set.empty [] false None None None state) current

                match resumptionName with
                | Some name ->
                    addBinding LocalBinding name quantity None Set.empty [] false None None None current
                | None ->
                    current

            let checkHandlerClause quantity argumentNames resumptionName bodyExpression current =
                withScope
                    "handler_clause"
                    (fun scopedState ->
                        scopedState
                        |> bindHandlerClauseNames quantity argumentNames resumptionName
                        |> checkExpression projectionSummaries document signatures localTypes <| bodyExpression)
                    current

            let nextState =
                state
                |> checkExpression projectionSummaries document signatures localTypes <| label
                |> checkExpression projectionSummaries document signatures localTypes <| body

            let returnArgumentNames =
                returnClause.ArgumentTokens
                |> List.choose tryParseHandlerArgumentName

            let nextState =
                checkHandlerClause None returnArgumentNames None returnClause.Body nextState

            let handledEffectName = tryResolveHandledEffectName label

            operationClauses
            |> List.fold
                (fun clauseState clause ->
                    let argumentNames =
                        clause.ArgumentTokens
                        |> List.choose tryParseHandlerArgumentName

                    let resumptionQuantity =
                        handledEffectName
                        |> Option.bind (fun effectName ->
                            tryFindScopedEffectDeclaration effectName
                            |> Option.bind (fun declaration ->
                                declaration.Operations
                                |> List.tryFind (fun operation -> String.Equals(operation.Name, clause.OperationName, StringComparison.Ordinal))
                                |> Option.map (fun operation ->
                                    operation.ResumptionQuantity
                                    |> Option.defaultValue QuantityOne
                                    |> ResourceQuantity.ofSurface)))

                    checkHandlerClause resumptionQuantity argumentNames clause.ResumptionName clause.Body clauseState)
                nextState
        | Literal _
        | NumericLiteral _
        | KindQualifiedName _
        | Name [ _ ] ->
            state
        | LocalSignature(_, body) ->
            checkExpression projectionSummaries document signatures localTypes state body
        | LocalTypeAlias(_, body) ->
            checkExpression projectionSummaries document signatures localTypes state body
        | LocalScopedEffect(declaration, body) ->
            withScopedEffectDeclaration declaration (fun () ->
                checkExpression projectionSummaries document signatures localTypes state body)
        | Name(root :: path) ->
            validatePlaceAccess
                document
                (makePlace root path)
                state
        | Name [] ->
            state
        | LocalLet(binding, value, body) ->
            let delayedBody = tryDelayedExpressionBody value
            let movedLinearBinding =
                if patternCanDischargeMovedValue binding.Pattern then
                    tryMovedLinearBinding value state
                else
                    None
            let captured =
                match value, delayedBody with
                | Lambda _, _
                | _, Some _ -> capturedRegions state value
                | _ -> Set.empty

            let capturedBindings =
                match value, delayedBody with
                | Lambda _, _
                | _, Some _ -> capturedBindings state value
                | _ -> []

            let bindingNames = collectPatternNames binding.Pattern

            let projectionDescriptorAlias =
                match binding.Pattern with
                | NamePattern aliasName ->
                    tryProjectionDescriptorAlias projectionSummaries value
                    |> Option.map (fun aliasInfo -> aliasName, aliasInfo)
                | _ ->
                    None

            let bodyForChecking =
                projectionDescriptorAlias
                |> Option.map (fun (aliasName, aliasInfo) ->
                        rewriteProjectionDescriptorApplications (Map.ofList [ aliasName, aliasInfo ]) body)
                |> Option.defaultValue body

            let bodyForChecking =
                match binding.Pattern with
                | NamePattern aliasName ->
                    collectEffectLabelAliases [ aliasName ] value Map.empty
                    |> fun aliases -> rewriteEffectLabelAliasUses aliases bodyForChecking
                | _ ->
                    bodyForChecking

            let projectionDescriptorAliasUse =
                projectionDescriptorAlias
                |> Option.map (fun (aliasName, _) -> aliasName, countProjectionDescriptorAliasUses aliasName body)

            let shadowedLinearBindings =
                bindingNames
                |> List.choose (fun name -> tryFindBinding name state)
                |> List.distinctBy (fun shadowedBinding -> shadowedBinding.Id)
                |> List.filter isExactOneBinding

            let stateAfterValue =
                match value, delayedBody with
                | Lambda _, _ -> checkLambdaParameterEscape document value state
                | _, Some _ -> state
                | _ -> checkExpression projectionSummaries document signatures localTypes state value

            let nextLocalTypes =
                tryInferConservativeExpressionType signatures localTypes value
                |> Option.orElseWith (fun () -> Some(inferConservativeFallbackBindingType value))
                |> fun valueType -> extendBindingLocalTypes signatures localTypes valueType binding

            let state =
                checkShadowedLinearBindings document shadowedLinearBindings state stateAfterValue

            let state =
                capturedBindings
                |> List.fold (fun current binding ->
                    addEvent OwnershipUseKind.Capture (findUseLocation document binding.Name 1) binding current) state

            let state =
                capturedBindings
                |> List.fold (fun current binding ->
                    transferCapturedBindingIntoClosure document binding current) state

            let allowedRegions = liveRegionIds state
            let scopeLabel =
                match bindingNames with
                | first :: _ -> $"let_{first}"
                | [] -> "let_pattern"

            withScope scopeLabel (fun scopedState ->
                let declaredQuantity =
                    binding.Quantity
                    |> Option.map ResourceQuantity.ofSurface
                    |> Option.orElseWith (fun () ->
                        match value, delayedBody with
                        | Lambda _, _
                        | _, Some _ -> inferLambdaBindingQuantity capturedBindings
                        | _ -> None)
                    |> Option.orElseWith (fun () ->
                        movedLinearBinding
                        |> Option.bind (fun (sourceBinding, _) -> sourceBinding.DeclaredQuantity))

                let movedLinearBinding =
                    if quantityBorrows declaredQuantity then
                        None
                    else
                        movedLinearBinding

                let checkDrop = declaredQuantity |> Option.exists ResourceQuantity.requiresUse

                let localLambda =
                    match value, delayedBody, bindingNames with
                    | Lambda(parameters, lambdaBody), _, [ _ ] ->
                        Some
                            { Identity = bindingNames |> List.tryHead
                              Parameters = parameters
                              Body = lambdaBody
                              CapturedBindings = capturedBindings }
                    | _, Some body, [ _ ] ->
                        Some
                            { Identity = bindingNames |> List.tryHead
                              Parameters = []
                              Body = body
                              CapturedBindings = capturedBindings }
                    | _ ->
                        None

                let closureFactId, scopedState =
                    match value, delayedBody with
                    | Lambda _, _
                    | _, Some _ ->
                        let closureName =
                            match bindingNames with
                            | [ name ] -> Some name
                            | _ -> None

                        let closureId, nextState = addClosureFact closureName capturedBindings captured scopedState
                        Some closureId, nextState
                    | _ ->
                        None, scopedState

                let scopedState =
                    movedLinearBinding
                    |> Option.map (fun (sourceBinding, sourcePath) ->
                        scopedState
                        |> addEventAtPlace
                            OwnershipUseKind.Move
                            (findBindingUseLocation document sourceBinding 1)
                            sourceBinding.Name
                            sourcePath
                            sourceBinding
                        |> consumeBindingAtPlace
                            document
                            (makePlace sourceBinding.Name sourcePath))
                    |> Option.defaultValue scopedState

                let scopedState =
                    let borrowRegion, nextState =
                        let introductionOrigin =
                            bindPatternIntroductionOrigin document binding
                            |> Option.orElseWith (fun () -> argumentLocation document value)

                        introduceBorrowRegionForQuantity $"{currentScopeId scopedState}.let" declaredQuantity introductionOrigin scopedState

                    let sourcePlace, sourcePlaces, nextState =
                        if quantityBorrows declaredQuantity then
                            prepareBorrowPatternSource projectionSummaries "let" value nextState
                        else
                            None, [], nextState

                    let nextState =
                        sourcePlaces
                        |> List.fold (fun current place -> validatePlaceAccess document place current) nextState

                    let capturedBindingOrigins =
                        capturedBindings
                        |> List.choose (fun capturedBinding -> capturedBinding.Origin)

                    let nextState =
                        addPatternBindings
                            document
                            binding
                            declaredQuantity
                            borrowRegion
                            sourcePlace
                            captured
                            capturedBindingOrigins
                            checkDrop
                            closureFactId
                            localLambda
                            nextState

                    if quantityBorrows declaredQuantity then
                        addBorrowLocksForPattern sourcePlaces binding.Pattern (argumentLocation document value) nextState
                    else
                        nextState

                scopedState
                |> fun current ->
                    projectionDescriptorAliasUse
                    |> Option.map (fun (aliasName, useCount) -> noteProjectionDescriptorAliasUses document aliasName useCount current)
                    |> Option.defaultValue current
                |> noteValueDemandingNameUse document bodyForChecking
                |> fun current -> checkExpression projectionSummaries document signatures nextLocalTypes current bodyForChecking
                |> checkResultEscapeAtBoundary allowedRegions document bodyForChecking
                |> checkScopeLinearDrops document) state
        | Lambda _ ->
            checkLambdaParameterEscape document expression state
        | IfThenElse(condition, whenTrue, whenFalse) ->
            let state =
                state
                |> noteValueDemandingNameUse document condition
                |> fun current -> checkExpression projectionSummaries document signatures localTypes current condition
            let left = checkExpressionInScope "if_then" projectionSummaries document signatures localTypes state whenTrue
            let right = checkExpressionInScope "if_else" projectionSummaries document signatures localTypes state whenFalse
            mergeBranchState left right
        | Match(scrutinee, cases) ->
            let state =
                let supportsPatternOwnership =
                    cases |> List.forall (fun caseClause -> matchPatternOwnershipSupported scrutinee caseClause.Pattern)

                let checkedState =
                    match scrutinee with
                    | Name [ scrutineeName ] ->
                        let current =
                            match tryFindBinding scrutineeName state with
                            | Some binding when binding.DeclaredQuantity = Some ResourceQuantity.zero ->
                                addDiagnostic
                                    erasedRuntimeUseCode
                                    "A match scrutinee cannot use an erased quantity-0 value at runtime."
                                    (argumentLocation document scrutinee)
                                    []
                                    document
                                    state
                            | _ ->
                                state

                        if supportsPatternOwnership then
                            checkExpression projectionSummaries document signatures localTypes current scrutinee
                        else
                            current
                            |> noteValueDemandingNameUse document scrutinee
                            |> fun next -> checkExpression projectionSummaries document signatures localTypes next scrutinee
                    | _ ->
                        state
                        |> noteValueDemandingNameUse document scrutinee
                        |> fun current -> checkExpression projectionSummaries document signatures localTypes current scrutinee

                if supportsPatternOwnership then
                    checkedState
                else
                    addDeferredFact OwnershipDeferredFact.MatchPatternResourceChecking checkedState

            match cases with
            | [] -> state
            | first :: rest ->
                let checkCase index (caseClause: SurfaceMatchCase) =
                    let allowedRegions = liveRegionIds state
                    let caseLocalTypes =
                        extendPatternLocalTypes
                            signatures
                            localTypes
                            (tryInferConservativeExpressionType signatures localTypes scrutinee)
                            caseClause.Pattern

                    withScope $"match_case{index}" (fun scopedState ->
                        let scopedState =
                            match scrutinee with
                            | Name [ scrutineeName ] when matchPatternOwnershipSupported scrutinee caseClause.Pattern ->
                                bindMatchPatternNames document scrutineeName caseClause.Pattern scopedState
                            | _ ->
                                scopedState

                        let scopedState =
                            match scrutinee, caseClause.Pattern with
                            | Name [ scrutineeName ], AnonymousRecordPattern(fields, _) ->
                                match tryFindBinding scrutineeName state with
                                | Some binding ->
                                    let mentioned =
                                        fields
                                        |> List.map (fun field -> field.Name)
                                        |> Set.ofList

                                    binding.RecordFieldQuantities
                                    |> Map.toList
                                    |> List.filter (fun (fieldName, quantity) ->
                                        not (Set.contains fieldName mentioned)
                                        && ResourceQuantity.requiresUse quantity)
                                    |> List.fold (fun current (fieldName, _) ->
                                        addDiagnostic
                                            linearDropCode
                                            $"Record pattern omits linear field '{fieldName}'."
                                            (argumentLocation document scrutinee)
                                            []
                                            document
                                            current) scopedState
                                | None ->
                                    scopedState
                            | _ ->
                                scopedState

                        let scopedState =
                            match caseClause.Guard with
                            | Some guard -> checkExpression projectionSummaries document signatures caseLocalTypes scopedState guard
                            | None -> scopedState

                        checkExpression projectionSummaries document signatures caseLocalTypes scopedState caseClause.Body
                        |> checkResultEscapeAtBoundary allowedRegions document caseClause.Body
                        |> checkScopeLinearDrops document) state

                let firstState = checkCase 0 first

                rest
                |> List.mapi (fun index caseClause ->
                    index + 1, caseClause)
                |> List.fold (fun current (index, caseClause) ->
                    mergeBranchState current (checkCase index caseClause)) firstState
        | RecordLiteral fields ->
            fields
            |> List.fold (fun current field ->
                current
                |> noteValueDemandingNameUse document field.Value
                |> fun next -> checkExpression projectionSummaries document signatures localTypes next field.Value) state
        | Seal(value, _) ->
            state
            |> noteValueDemandingNameUse document value
            |> fun current -> checkExpression projectionSummaries document signatures localTypes current value
        | RecordUpdate(receiver, fields) ->
            let state = checkExpression projectionSummaries document signatures localTypes state receiver

            let state =
                fields
                |> List.fold (fun current field ->
                    current
                    |> noteValueDemandingNameUse document field.Value
                    |> fun next -> checkExpression projectionSummaries document signatures localTypes next field.Value) state

            match tryExpressionPlace receiver with
            | Some receiverPlace ->
                let restoredPaths =
                    fields
                    |> List.map (fun field -> receiverPlace.Path @ [ field.Name ])

                let state =
                    match tryFindBinding receiverPlace.Root state with
                    | Some binding ->
                        let updatedFieldNames =
                            fields
                            |> List.map (fun field -> field.Name)
                            |> Set.ofList

                        let unrepairedPaths =
                            binding.ConsumedPaths
                            |> List.filter (fun consumedPath ->
                                restoredPaths
                                |> List.exists (fun restoredPath -> pathHasPrefix restoredPath consumedPath)
                                |> not)

                        let state =
                            if List.isEmpty unrepairedPaths then
                                state
                            else
                                let firstUnrepairedPath =
                                    unrepairedPaths
                                    |> List.head
                                    |> fun path -> String.concat "." (receiverPlace.Root :: path)

                                addDiagnostic
                                    linearOveruseCode
                                    $"Record update on '{receiverPlace.Root}' must explicitly repair previously consumed path '{firstUnrepairedPath}'."
                                    (argumentLocation document receiver)
                                    []
                                    document
                                    state

                        let state =
                            if not (List.isEmpty receiverPlace.Path) || Map.isEmpty binding.RecordFieldDependencies then
                                state
                            else
                                let missingRepairField =
                                    binding.RecordFieldDependencies
                                    |> Map.toList
                                    |> List.tryPick (fun (fieldName, dependencies) ->
                                        if Set.contains fieldName updatedFieldNames then
                                            None
                                        elif Set.isEmpty (Set.intersect dependencies updatedFieldNames) then
                                            None
                                        else
                                            Some fieldName)

                                match missingRepairField with
                                | Some fieldName ->
                                    addDiagnostic
                                        DiagnosticCode.TypeEqualityMismatch
                                        $"Record update on '{receiverPlace.Root}' changes dependent field inputs but does not repair field '{fieldName}'."
                                        (argumentLocation document receiver)
                                        []
                                        document
                                        state
                                | None ->
                                    state

                        state
                    | None ->
                        state

                match tryFindBindingId receiverPlace.Root state with
                | Some bindingId ->
                    updateBinding bindingId (restoreConsumedPaths restoredPaths) state
                | None ->
                    state
            | None ->
                state
        | MemberAccess(receiver, segments, arguments) ->
            let state =
                state
                |> noteValueDemandingNameUse document receiver
                |> fun current -> checkExpression projectionSummaries document signatures localTypes current receiver

            let state =
                match receiver, segments with
                | Name [ receiverName ], [ memberName ] ->
                    document.Syntax.Declarations
                    |> List.tryPick (function
                        | LetDeclaration definition when definition.Name = Some memberName ->
                            definition.Parameters
                            |> List.tryFind (fun parameter -> parameter.IsReceiver && parameter.Quantity = Some QuantityOne)
                        | _ ->
                            None)
                    |> Option.map (fun _ -> consumeBinding document receiverName state)
                    |> Option.defaultValue state
                | _ ->
                    state

            arguments
            |> List.fold (fun current argument ->
                current
                |> noteValueDemandingNameUse document argument
                |> fun next -> checkExpression projectionSummaries document signatures localTypes next argument) state
        | SafeNavigation(receiver, navigation) ->
            let state =
                state
                |> noteValueDemandingNameUse document receiver
                |> fun current -> checkExpression projectionSummaries document signatures localTypes current receiver

            navigation.Arguments
            |> List.fold (fun current argument ->
                current
                |> noteValueDemandingNameUse document argument
                |> fun next -> checkExpression projectionSummaries document signatures localTypes next argument) state
        | TagTest(receiver, _) ->
            state
            |> noteValueDemandingNameUse document receiver
            |> fun current -> checkExpression projectionSummaries document signatures localTypes current receiver
        | Do statements ->
            checkDoStatementsInScope "do" projectionSummaries document signatures localTypes state statements
        | MonadicSplice inner
        | ExplicitImplicitArgument inner
        | InoutArgument inner
        | Unary(_, inner) ->
            state
            |> noteValueDemandingNameUse document inner
            |> fun current -> checkExpression projectionSummaries document signatures localTypes current inner
        | NamedApplicationBlock fields ->
            fields
            |> List.fold (fun current field ->
                current
                |> noteValueDemandingNameUse document field.Value
                |> fun next -> checkExpression projectionSummaries document signatures localTypes next field.Value) state
        | Binary(left, _, right) ->
            state
            |> noteValueDemandingNameUse document left
            |> fun current -> checkExpression projectionSummaries document signatures localTypes current left
            |> noteValueDemandingNameUse document right
            |> fun current -> checkExpression projectionSummaries document signatures localTypes current right
        | Elvis(left, right) ->
            state
            |> noteValueDemandingNameUse document left
            |> fun current -> checkExpression projectionSummaries document signatures localTypes current left
            |> noteValueDemandingNameUse document right
            |> fun current -> checkExpression projectionSummaries document signatures localTypes current right
        | Comprehension comprehension ->
            withScope "query" (fun scopedState ->
                checkComprehensionClauses localTypes scopedState comprehension comprehension.Clauses
                |> checkScopeLinearDrops document) state
        | PrefixedString(_, parts) ->
            parts
            |> List.fold (fun current part ->
                match part with
                | StringText _ -> current
                | StringInterpolation(inner, _) ->
                    current
                    |> noteValueDemandingNameUse document inner
                    |> fun next -> checkExpression projectionSummaries document signatures localTypes next inner) state
        | Apply(Name [ calleeName ], [ view; Lambda(parameters, body) ])
            when String.Equals(calleeName, "withBorrowView", StringComparison.Ordinal) ->
            let state = checkExpression projectionSummaries document signatures localTypes state view
            checkImmediateLambdaDemand projectionSummaries document signatures localTypes state parameters body
        | Apply(Name [ calleeName ], [ argument ]) when String.Equals(calleeName, "captureBorrow", StringComparison.Ordinal) ->
            let state = checkExpression projectionSummaries document signatures localTypes state argument

            match tryBorrowablePlacesWithEnv Map.empty projectionSummaries state argument with
            | Some places ->
                places
                |> List.fold (fun current place ->
                    current
                    |> validatePlaceAccess document place
                    |> addNonConsumingDemand document place.Root) state
            | None ->
                state
        | Apply(Name [ calleeName ], [ argument ]) when String.Equals(calleeName, "force", StringComparison.Ordinal) ->
            let state = checkExpression projectionSummaries document signatures localTypes state argument

            match argument with
            | Name [ name ] ->
                match tryFindBinding name state with
                | Some binding ->
                    let state =
                        match binding.DeclaredQuantity with
                        | Some quantity when ResourceQuantity.requiresUse quantity ->
                            consumeBinding document name state
                        | Some quantity when ResourceQuantity.isBorrow quantity ->
                            addNamedEvent OwnershipUseKind.Borrow (findUseLocation document name 1) name state
                        | _ ->
                            state

                    match binding.LocalLambda with
                    | Some lambdaValue -> checkLocalLambdaInvocation projectionSummaries document signatures localTypes state lambdaValue []
                    | None -> state
                | None ->
                    state
            | _ ->
                state
        | Apply(Name [ calleeName ], [ argument ]) when String.Equals(calleeName, "pure", StringComparison.Ordinal) ->
            let carriesBorrow = expressionResultMayCarryBorrow state argument

            if carriesBorrow then
                state
                |> checkExpression projectionSummaries document signatures localTypes <| argument
                |> checkEscape document argument
            else
                let state =
                    match argument with
                    | Name [ name ] -> consumeBinding document name state
                    | _ -> checkExpression projectionSummaries document signatures localTypes state argument

                state
        | Apply(Name [ calleeName ], [ body ]) when String.Equals(calleeName, "fork", StringComparison.Ordinal) ->
            let captured = capturedRegions state body
            let state =
                if Set.isEmpty captured then
                    state
                else
                    addDiagnostic
                        borrowEscapeCode
                        "A forked child computation cannot capture a borrowed region from the parent fiber."
                        (argumentLocation document body)
                        []
                        document
                        state

            checkExpression projectionSummaries document signatures localTypes state body
        | Apply(Name [ calleeName ], arguments) when Map.containsKey calleeName projectionSummaries ->
            match Map.tryFind calleeName projectionSummaries with
            | None ->
                state
            | Some projectionSummary when List.length projectionSummary.Binders = List.length arguments ->
                let isAccessorProjection = accessorCapabilities projectionSummary.Body |> Option.isSome

                let state =
                    match accessorCapabilities projectionSummary.Body with
                    | Some capabilities when not (Set.contains "get" capabilities) ->
                        addProjectionCapabilityDiagnostic document "get" expression state
                    | _ ->
                        state

                let state =
                    (state, List.zip projectionSummary.Binders arguments)
                    ||> List.fold (fun current (binder, argument) ->
                        match binder with
                        | ProjectionPlaceBinder _ ->
                            match tryBorrowablePlacesWithEnv Map.empty projectionSummaries current argument with
                            | Some places when isAccessorProjection ->
                                places
                                |> List.fold (fun next place ->
                                    next
                                    |> validatePlaceAccess document place
                                    |> addNonConsumingDemand document place.Root) current
                            | Some _ ->
                                current
                            | None ->
                                current
                                |> addProjectionPlaceDiagnostic document argument
                                |> noteValueDemandingNameUse document argument
                                |> fun next -> checkExpression projectionSummaries document signatures localTypes next argument
                        | ProjectionValueBinder _ ->
                            current
                            |> noteValueDemandingNameUse document argument
                            |> fun next -> checkExpression projectionSummaries document signatures localTypes next argument)

                if isAccessorProjection then
                    state
                else
                    match tryBorrowablePlacesWithEnv Map.empty projectionSummaries state expression with
                    | Some places ->
                        places
                        |> List.fold (fun current place ->
                            current
                            |> validatePlaceAccess document place
                            |> addNonConsumingDemand document place.Root) state
                    | None ->
                        state
                        |> addProjectionPlaceDiagnostic document expression
            | Some _ ->
                arguments
                |> List.fold (fun current argument ->
                    current
                    |> noteValueDemandingNameUse document argument
                    |> fun next -> checkExpression projectionSummaries document signatures localTypes next argument) state
        | Apply(Name [ receiverName; memberName ], arguments)
            when tryBuildReceiverMethodArgumentsForDocument document (Name [ receiverName ]) memberName arguments |> Option.isSome ->
            let receiverArguments =
                tryBuildReceiverMethodArgumentsForDocument document (Name [ receiverName ]) memberName arguments
                |> Option.defaultValue arguments

            checkExpression projectionSummaries document signatures localTypes state (Apply(Name [ memberName ], receiverArguments))
        | Apply(callee, arguments) ->
            let state =
                match tryFindMultiShotScopedOperation expression with
                | Some(effectName, operation) when not (backendSupportsMultishotEffects (currentBackendProfile ())) ->
                    addDiagnostic
                        DiagnosticCode.MultishotEffectUnsupportedBackend
                        $"Backend profile '{BackendProfile.toPortableName (currentBackendProfile ())}' does not provide capability 'rt-multishot-effects' required by multi-shot operation '{effectName}.{operation.Name}' at this invocation site."
                        (argumentLocation document expression)
                        []
                        document
                        state
                | _ ->
                    state

            let state = checkExpression projectionSummaries document signatures localTypes state callee
            let state =
                match callee with
                | Name(root :: path) when not (List.isEmpty path) && fieldPathRequiresUse state (makePlace root path) ->
                    consumeBindingAtPlace document (makePlace root path) state
                | _ ->
                    state

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
                            { Identity = None
                              Parameters = parameters
                              Body = body
                              CapturedBindings = capturedBindings state callee }
                    | _ ->
                        None

            let state =
                match calleeBinding, localLambda with
                | None, Some lambdaValue ->
                    lambdaValue.CapturedBindings
                    |> List.fold (fun current binding ->
                    addEvent OwnershipUseKind.Capture (findUseLocation document binding.Name 1) binding current) state
                    |> fun current ->
                        lambdaValue.CapturedBindings
                        |> List.fold (fun next binding ->
                            transferCapturedBindingIntoClosure document binding next) current
                | _ ->
                    state

            let state =
                match calleeBinding with
                | Some binding when binding.DeclaredQuantity |> Option.exists concreteQuantityCountsUse ->
                    consumeBinding document binding.Name state
                | Some binding when quantityBorrows binding.DeclaredQuantity ->
                    addNamedEvent OwnershipUseKind.Borrow (findUseLocation document binding.Name 1) binding.Name state
                | _ ->
                    state

            let parameterNames, parameterQuantities, parameterTypes, parameterInout =
                match localLambda with
                | Some lambdaValue ->
                    lambdaValue.Parameters |> List.map (fun parameter -> Some parameter.Name),
                    lambdaValue.Parameters |> List.map parameterQuantity,
                    (lambdaValue.Parameters |> List.map (fun parameter -> parameter.TypeTokens |> Option.bind TypeSignatures.parseType)),
                    (lambdaValue.Parameters |> List.map (fun parameter -> parameter.IsInout))
                | None ->
                    match callee with
                    | Name [ name ] ->
                        match Map.tryFind name localTypes with
                        | Some localType ->
                            let quantities, parameterTypes = functionParameterInfoFromType localType
                            [], quantities, parameterTypes, List.replicate parameterTypes.Length false
                        | None ->
                            calleeName
                            |> Option.bind (fun resolvedName -> Map.tryFind resolvedName signatures)
                            |> Option.map (fun signature ->
                                signature.ParameterNames,
                                signature.ParameterQuantities,
                                (signature.ParameterTypeTokens |> List.map (Option.bind TypeSignatures.parseType)),
                                signature.ParameterInout)
                            |> Option.defaultValue ([], [], [], [])
                    | _ ->
                        calleeName
                        |> Option.bind (fun name -> Map.tryFind name signatures)
                        |> Option.map (fun signature ->
                            signature.ParameterNames,
                            signature.ParameterQuantities,
                            (signature.ParameterTypeTokens |> List.map (Option.bind TypeSignatures.parseType)),
                            signature.ParameterInout)
                        |> Option.defaultValue ([], [], [], [])

            let actualArguments =
                let splitTrailingNamedArgumentBlock (callArguments: SurfaceExpression list) =
                    let indexedNamedBlocks =
                        callArguments
                        |> List.mapi (fun index argument ->
                            match argument with
                            | NamedApplicationBlock fields -> Some(index, fields)
                            | _ -> None)
                        |> List.choose id

                    match indexedNamedBlocks with
                    | [ index, fields ] when index = List.length callArguments - 1 ->
                        Some(callArguments |> List.take index, fields)
                    | _ ->
                        None

                match splitTrailingNamedArgumentBlock arguments with
                | Some(positionalArguments, namedFields) ->
                    let namedParameterIndices =
                        parameterNames
                        |> List.mapi (fun index name -> index, name)
                        |> List.skip positionalArguments.Length
                        |> List.choose (fun (index, name) -> name |> Option.map (fun parameterName -> parameterName, index))
                        |> Map.ofList

                    let positionalActuals =
                        positionalArguments
                        |> List.mapi (fun sourceIndex argument -> sourceIndex, Some sourceIndex, argument)

                    let namedActuals =
                        namedFields
                        |> List.mapi (fun fieldIndex field ->
                            positionalArguments.Length + fieldIndex,
                            Map.tryFind field.Name namedParameterIndices,
                            field.Value)

                    positionalActuals @ namedActuals
                | None ->
                    arguments
                    |> List.mapi (fun sourceIndex argument -> sourceIndex, Some sourceIndex, argument)

            let state =
                let inoutPlaces =
                    actualArguments
                    |> List.choose (fun (sourceIndex, formalIndex, argument) ->
                        let parameterIndex = formalIndex |> Option.defaultValue sourceIndex

                        let expectsInout =
                            parameterInout
                            |> List.tryItem parameterIndex
                            |> Option.defaultValue false

                        match expectsInout, argument with
                        | true, InoutArgument inner ->
                            tryInoutArgumentPlaces projectionSummaries state inner
                            |> Option.map (fun places -> sourceIndex, argument, places)
                        | _ ->
                            None)

                let placePairs =
                    [
                        for leftIndex = 0 to List.length inoutPlaces - 1 do
                            for rightIndex = leftIndex + 1 to List.length inoutPlaces - 1 do
                                yield inoutPlaces[leftIndex], inoutPlaces[rightIndex]
                    ]

                placePairs
                |> List.fold (fun current ((_, _, leftPlaces), (_, rightArgument, rightPlaces)) ->
                    let overlaps =
                        leftPlaces
                        |> List.exists (fun leftPlace ->
                            rightPlaces
                            |> List.exists (fun rightPlace ->
                                String.Equals(leftPlace.Root, rightPlace.Root, StringComparison.Ordinal)
                                && pathsOverlap leftPlace.Path rightPlace.Path))

                    if overlaps then
                        addDiagnostic
                            borrowOverlapCode
                            "Inout arguments must have disjoint place footprints."
                            (argumentLocation document rightArgument)
                            []
                            document
                            current
                    else
                        current) state

            let state =
                let demandedQuantityAt index =
                    parameterQuantities
                    |> List.tryItem index
                    |> demandedParameterQuantity

                let borrowArguments =
                    actualArguments
                    |> List.choose (fun (sourceIndex, formalIndex, argument) ->
                        let parameterIndex = formalIndex |> Option.defaultValue sourceIndex

                        match demandedQuantityAt parameterIndex with
                        | Some demandQuantity when ResourceQuantity.isBorrow demandQuantity ->
                            tryBorrowablePlacesWithEnv Map.empty projectionSummaries state argument
                            |> Option.map (fun places -> sourceIndex, argument, places)
                        | _ ->
                            None)

                let consumingArgumentPlaces sourceIndex formalIndex argument =
                    let parameterIndex = formalIndex |> Option.defaultValue sourceIndex

                    match demandedQuantityAt parameterIndex with
                    | Some demandQuantity when concreteQuantityCountsUse demandQuantity ->
                        match argument with
                        | Name(root :: path) ->
                            Some [ makePlace root path ]
                        | InoutArgument inner ->
                            tryExpressionPlace inner |> Option.map List.singleton
                        | _ ->
                            tryBorrowablePlacesWithEnv Map.empty projectionSummaries state argument
                    | _ ->
                        match argument with
                        | Apply(callee, [ Name(root :: path) ]) ->
                            tryCalleeName callee
                            |> Option.bind (fun name -> Map.tryFind name signatures)
                            |> Option.bind (fun signature ->
                                signature.ParameterQuantities
                                |> List.tryHead
                                |> Option.flatten)
                            |> Option.filter concreteQuantityCountsUse
                            |> Option.map (fun _ -> [ makePlace root path ])
                        | _ ->
                            None

                actualArguments
                |> List.fold (fun current (consumeSourceIndex, consumeFormalIndex, consumeArgument) ->
                    match consumingArgumentPlaces consumeSourceIndex consumeFormalIndex consumeArgument with
                    | Some consumedPlaces ->
                        borrowArguments
                        |> List.filter (fun (borrowIndex, _, _) -> borrowIndex < consumeSourceIndex)
                        |> List.fold (fun next (_, borrowArgument, borrowedPlaces) ->
                            let overlaps =
                                borrowedPlaces
                                |> List.exists (fun borrowedPlace ->
                                    consumedPlaces
                                    |> List.exists (fun consumedPlace ->
                                        String.Equals(borrowedPlace.Root, consumedPlace.Root, StringComparison.Ordinal)
                                        && pathsOverlap borrowedPlace.Path consumedPlace.Path))

                            if overlaps then
                                addDiagnostic
                                    borrowOverlapCode
                                    "A temporary borrow introduced earlier in this application spine overlaps a later consuming argument."
                                    (argumentLocation document consumeArgument)
                                    [
                                        match argumentLocation document borrowArgument with
                                        | Some location ->
                                            { Message = "Earlier borrowed argument in the same application spine."
                                              Location = location }
                                        | None -> ()
                                    ]
                                    document
                                    next
                            else
                                next) current
                    | None ->
                        current) state

            let state =
                (state, actualArguments)
                ||> List.fold (fun current (sourceIndex, formalIndex, argument) ->
                    let parameterIndex = formalIndex |> Option.defaultValue sourceIndex

                    let quantity =
                        parameterQuantities
                        |> List.tryItem parameterIndex

                    let demandQuantity = demandedParameterQuantity quantity

                    let demandedParameterType =
                        parameterTypes
                        |> List.tryItem parameterIndex
                        |> Option.flatten

                    let expectsInout =
                        parameterInout
                        |> List.tryItem parameterIndex
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

                    let current =
                        match demandQuantity, argument with
                        | Some demandQuantity, Name [ name ]
                            when ResourceQuantity.isInterval demandQuantity
                                 && not (calleeName |> Option.exists (fun callee -> String.Equals(callee, "withBorrowView", StringComparison.Ordinal))) ->
                            match tryFindBinding name current with
                            | Some binding when binding.DeclaredQuantity |> Option.exists ResourceQuantity.isBorrow ->
                                current
                            | Some binding when binding.LocalLambda.IsSome || binding.ClosureFactId.IsSome ->
                                current
                            | Some binding ->
                                let capability =
                                    binding.DeclaredQuantity
                                    |> Option.defaultValue ResourceQuantity.omega

                                if ResourceQuantity.satisfies capability demandQuantity then
                                    current
                                else
                                    addDiagnostic
                                        linearOveruseCode
                                        $"An argument available at quantity '{ResourceQuantity.toSurfaceText capability}' cannot satisfy parameter demand '{ResourceQuantity.toSurfaceText demandQuantity}'."
                                        (argumentLocation document argument)
                                        []
                                        document
                                        current
                            | None ->
                                current
                        | _ ->
                            current

                    let current =
                        match demandQuantity, inferredClosureQuantity current argument with
                        | Some demandQuantity, Some capability
                            when not (calleeName |> Option.exists (fun callee -> String.Equals(callee, "withBorrowView", StringComparison.Ordinal)))
                                 && not (ResourceQuantity.satisfies capability demandQuantity) ->
                            let current = transferImmediateClosureArgumentCaptures document argument current

                            addDiagnostic
                                linearOveruseCode
                                $"An argument usable at quantity '{ResourceQuantity.toSurfaceText capability}' cannot satisfy parameter demand '{ResourceQuantity.toSurfaceText demandQuantity}'."
                                (argumentLocation document argument)
                                []
                                document
                                current
                        | _ ->
                            current

                    let current =
                        match demandedParameterType,
                              tryInferConservativeExpressionType signatures localTypes argument with
                        | Some demandedType, Some actualType
                            when shouldCheckApplicationTypeCompatibility argument demandedType actualType
                                 && not (applicationBoundaryTypesCompatible demandedType actualType) ->
                            addDiagnostic
                                DiagnosticCode.TypeEqualityMismatch
                                $"Argument type '{TypeSignatures.toText actualType}' does not match demanded parameter type '{TypeSignatures.toText demandedType}'."
                                (argumentLocation document argument)
                                []
                                document
                                current
                        | _ ->
                            current

                    let next =
                        match demandQuantity, argument with
                        | Some demandQuantity, Lambda(parameters, body) when lowerBoundIsPositive (Some demandQuantity) ->
                            checkImmediateLambdaDemand projectionSummaries document signatures localTypes current parameters body
                        | Some demandQuantity, Name(root :: path) when concreteQuantityCountsUse demandQuantity ->
                            consumeBindingAtPlace document (makePlace root path) current
                        | Some demandQuantity, _ when concreteQuantityCountsUse demandQuantity
                                                     && isProjectionCall projectionSummaries argument ->
                            match argument with
                            | Apply(Name [ projectionName ], projectionArguments) ->
                                match Map.tryFind projectionName projectionSummaries with
                                | Some projectionSummary ->
                                    match accessorCapabilities projectionSummary.Body with
                                    | Some capabilities ->
                                        let current =
                                            if Set.contains "sink" capabilities then
                                                current
                                            else
                                                addProjectionCapabilityDiagnostic document "sink" argument current

                                        match projectionPlaceArgumentPlaces projectionSummaries current projectionSummary projectionArguments with
                                        | Some places ->
                                            places
                                            |> List.fold (fun next place -> consumeBindingAtPlace document place next) current
                                        | None ->
                                            current
                                            |> addProjectionPlaceDiagnostic document argument
                                            |> fun next -> checkExpression projectionSummaries document signatures localTypes next argument
                                    | None ->
                                        match tryBorrowablePlacesWithEnv Map.empty projectionSummaries current argument with
                                        | Some places ->
                                            places
                                            |> List.fold (fun next place -> consumeBindingAtPlace document place next) current
                                        | None ->
                                            current
                                            |> addProjectionPlaceDiagnostic document argument
                                            |> fun next -> checkExpression projectionSummaries document signatures localTypes next argument
                                | None ->
                                    checkExpression projectionSummaries document signatures localTypes current argument
                            | _ ->
                                checkExpression projectionSummaries document signatures localTypes current argument
                        | Some demandQuantity, InoutArgument inner when concreteQuantityCountsUse demandQuantity ->
                            match inner with
                            | Apply(Name [ projectionName ], projectionArguments) when expectsInout && Map.containsKey projectionName projectionSummaries ->
                                let projectionSummary = projectionSummaries[projectionName]
                                let current =
                                    match accessorCapabilities projectionSummary.Body with
                                    | Some capabilities when not (Set.contains "open" capabilities) ->
                                        addProjectionCapabilityDiagnostic document "open" inner current
                                    | _ ->
                                        current

                                let places =
                                    match accessorCapabilities projectionSummary.Body with
                                    | Some _ -> projectionPlaceArgumentPlaces projectionSummaries current projectionSummary projectionArguments
                                    | None -> tryBorrowablePlacesWithEnv Map.empty projectionSummaries current inner

                                match places with
                                | Some places ->
                                    places
                                    |> List.fold (fun next place ->
                                        let next = consumeBindingAtPlace document place next

                                        match tryFindBindingId place.Root next with
                                        | Some bindingId -> updateBinding bindingId (restoreConsumedPlace place) next
                                        | None -> next) current
                                | None ->
                                    current
                                    |> addProjectionPlaceDiagnostic document inner
                                    |> fun next -> checkExpression projectionSummaries document signatures localTypes next inner
                            | _ ->
                                match tryExpressionPlace inner with
                                | Some place ->
                                    let current = consumeBindingAtPlace document place current

                                    if expectsInout then
                                        match tryFindBindingId place.Root current with
                                        | Some bindingId ->
                                            updateBinding bindingId (restoreConsumedPlace place) current
                                        | None ->
                                            current
                                    else
                                        current
                                | None ->
                                    let current =
                                        if expectsInout then
                                            addProjectionPlaceDiagnostic document inner current
                                        else
                                            current

                                    checkExpression projectionSummaries document signatures localTypes current inner
                        | Some demandQuantity, Name [ name ] when ResourceQuantity.isBorrow demandQuantity ->
                            current
                            |> addNonConsumingDemand document name
                            |> fun current -> checkExpression projectionSummaries document signatures localTypes current argument
                        | Some demandQuantity, _ when ResourceQuantity.isBorrow demandQuantity ->
                            let current =
                                match argument with
                                | Apply(Name [ projectionName ], _) ->
                                    match Map.tryFind projectionName projectionSummaries |> Option.bind (fun summary -> accessorCapabilities summary.Body) with
                                    | Some capabilities when not (Set.contains "get" capabilities) ->
                                        addProjectionCapabilityDiagnostic document "get" argument current
                                    | _ ->
                                        current
                                | _ ->
                                    current

                            let current = checkExpression projectionSummaries document signatures localTypes current argument

                            match tryBorrowablePlacesWithEnv Map.empty projectionSummaries current argument with
                            | Some places ->
                                places
                                |> List.fold (fun state place -> addNonConsumingDemand document place.Root state) current
                            | None ->
                                current
                        | _ ->
                            let current =
                                match argument with
                                | Lambda _ when not (Set.isEmpty (capturedRegions current argument)) ->
                                    checkEscape document argument current
                                | Name [ name ] ->
                                    match tryFindBinding name current with
                                    | Some binding when (binding.LocalLambda.IsSome || binding.ClosureFactId.IsSome)
                                                        && not (Set.isEmpty (bindingCapturedRegions binding)) ->
                                        checkEscape document argument current
                                    | _ ->
                                        current
                                | _ ->
                                    current

                            let current =
                                if tryDelayedExpressionBody argument |> Option.isSome then
                                    checkEscape document argument current
                                else
                                    current

                            current
                            |> noteValueDemandingNameUse document argument
                            |> fun next -> checkExpression projectionSummaries document signatures localTypes next argument

                    next)

            match localLambda with
            | Some lambdaValue ->
                checkLocalLambdaInvocation projectionSummaries document signatures localTypes state lambdaValue arguments
            | None ->
                state

    and private checkExpressionInScope
        scopeLabel
        projectionSummaries
        (document: ParsedDocument)
        (signatures: Map<string, FunctionSignature>)
        (localTypes: Map<string, TypeSignatures.TypeExpr>)
        state
        expression
        =
        let allowedRegions = liveRegionIds state

        withScope scopeLabel (fun scopedState ->
            scopedState
            |> noteValueDemandingNameUse document expression
            |> fun current -> checkExpression projectionSummaries document signatures localTypes current expression
            |> checkResultEscapeAtBoundary allowedRegions document expression) state

    and private checkDoStatements
        projectionSummaries
        (document: ParsedDocument)
        (signatures: Map<string, FunctionSignature>)
        (localTypes: Map<string, TypeSignatures.TypeExpr>)
        state
        statements
        =
        let rec loop localTypes current remaining =
            match remaining with
            | [] -> current
            | DoLet(binding, expression) :: rest ->
                let delayedBody = tryDelayedExpressionBody expression
                let movedLinearBinding =
                    if patternCanDischargeMovedValue binding.Pattern then
                        tryMovedLinearBinding expression current
                    else
                        None

                let captured =
                    match expression, delayedBody with
                    | Lambda _, _
                    | _, Some _ -> capturedRegions current expression
                    | _ -> Set.empty

                let capturedBindings =
                    match expression, delayedBody with
                    | Lambda _, _
                    | _, Some _ -> capturedBindings current expression
                    | _ -> []

                let bindingNames = collectPatternNames binding.Pattern

                let shadowedLinearBindings =
                    bindingNames
                    |> List.choose (fun name -> tryFindBinding name current)
                    |> List.distinctBy (fun shadowedBinding -> shadowedBinding.Id)
                    |> List.filter isExactOneBinding

                let currentAfterValue =
                    match expression, delayedBody with
                    | Lambda _, _ -> checkLambdaParameterEscape document expression current
                    | _, Some _ -> current
                    | _ -> checkExpression projectionSummaries document signatures localTypes current expression

                let current =
                    checkShadowedLinearBindings document shadowedLinearBindings current currentAfterValue

                let current =
                    capturedBindings
                    |> List.fold (fun state binding ->
                        addEvent OwnershipUseKind.Capture (findUseLocation document binding.Name 1) binding state) current

                let current =
                    capturedBindings
                    |> List.fold (fun state binding ->
                        transferCapturedBindingIntoClosure document binding state) current

                let closureName =
                    match expression, delayedBody, collectPatternNames binding.Pattern with
                    | Lambda _, _, [ name ]
                    | _, Some _, [ name ] -> Some name
                    | _ -> None

                let localLambda =
                    match expression, delayedBody, collectPatternNames binding.Pattern with
                    | Lambda(parameters, body), _, [ _ ] ->
                        Some
                            { Identity = collectPatternNames binding.Pattern |> List.tryHead
                              Parameters = parameters
                              Body = body
                              CapturedBindings = capturedBindings }
                    | _, Some body, [ _ ] ->
                        Some
                            { Identity = collectPatternNames binding.Pattern |> List.tryHead
                              Parameters = []
                              Body = body
                              CapturedBindings = capturedBindings }
                    | _ ->
                        None

                let closureFactId, current =
                    match expression, delayedBody with
                    | Lambda _, _
                    | _, Some _ ->
                        let closureId, current = addClosureFact closureName capturedBindings captured current
                        Some closureId, current
                    | _ -> None, current

                let declaredQuantity =
                    binding.Quantity
                    |> Option.map ResourceQuantity.ofSurface
                    |> Option.orElseWith (fun () ->
                        match expression, delayedBody with
                        | Lambda _, _
                        | _, Some _ -> inferLambdaBindingQuantity capturedBindings
                        | _ -> None)
                    |> Option.orElseWith (fun () ->
                        movedLinearBinding
                        |> Option.bind (fun (sourceBinding, _) -> sourceBinding.DeclaredQuantity))

                let movedLinearBinding =
                    if quantityBorrows declaredQuantity then
                        None
                    else
                        movedLinearBinding

                let current =
                    movedLinearBinding
                    |> Option.map (fun (sourceBinding, sourcePath) ->
                        current
                        |> addEventAtPlace
                            OwnershipUseKind.Move
                            (findBindingUseLocation document sourceBinding 1)
                            sourceBinding.Name
                            sourcePath
                            sourceBinding
                        |> consumeBindingAtPlace
                            document
                            (makePlace sourceBinding.Name sourcePath))
                    |> Option.defaultValue current

                let checkDrop = declaredQuantity |> Option.exists ResourceQuantity.requiresUse
                let capturedBindingOrigins =
                    capturedBindings
                    |> List.choose (fun binding -> binding.Origin)

                let borrowRegion, current =
                    let introductionOrigin =
                        bindPatternIntroductionOrigin document binding
                        |> Option.orElseWith (fun () -> argumentLocation document expression)

                    introduceBorrowRegionForQuantity $"{currentScopeId current}.let" declaredQuantity introductionOrigin current

                let sourcePlace, sourcePlaces, current =
                    if quantityBorrows declaredQuantity then
                        prepareBorrowPatternSource projectionSummaries "do_let" expression current
                    else
                        None, [], current

                let current =
                    sourcePlaces
                    |> List.fold (fun nextState place -> validatePlaceAccess document place nextState) current

                let current =
                    addPatternBindings
                        document
                        binding
                        declaredQuantity
                        borrowRegion
                        sourcePlace
                        captured
                        capturedBindingOrigins
                        checkDrop
                        closureFactId
                        localLambda
                        current

                let current =
                    if quantityBorrows declaredQuantity then
                        addBorrowLocksForPattern sourcePlaces binding.Pattern (argumentLocation document expression) current
                    else
                        current

                let nextLocalTypes =
                    tryInferConservativeExpressionType signatures localTypes expression
                    |> Option.orElseWith (fun () -> Some(inferConservativeFallbackBindingType expression))
                    |> fun valueType -> extendBindingLocalTypes signatures localTypes valueType binding

                let projectionDescriptorAlias =
                    match binding.Pattern with
                    | NamePattern aliasName ->
                        tryProjectionDescriptorAlias projectionSummaries expression
                        |> Option.map (fun aliasInfo -> aliasName, aliasInfo)
                    | _ ->
                        None

                let restForChecking =
                    projectionDescriptorAlias
                    |> Option.bind (fun (aliasName, aliasInfo) ->
                        match rewriteProjectionDescriptorApplications (Map.ofList [ aliasName, aliasInfo ]) (Do rest) with
                        | Do rewrittenRest -> Some rewrittenRest
                        | _ -> None)
                    |> Option.defaultValue rest

                let current =
                    projectionDescriptorAlias
                    |> Option.map (fun (aliasName, _) ->
                        noteProjectionDescriptorAliasUses document aliasName (countProjectionDescriptorAliasUsesInStatements aliasName rest) current)
                    |> Option.defaultValue current

                loop nextLocalTypes current restForChecking
            | DoLetQuestion(binding, expression, failure) :: rest ->
                let current =
                    current
                    |> noteValueDemandingNameUse document expression
                    |> fun next -> checkExpression projectionSummaries document signatures localTypes next expression

                let current =
                    match failure with
                    | Some _ ->
                        current
                    | None when letQuestionPlainFailureDropsPositiveResidue document binding.Pattern ->
                        addDiagnostic
                            linearDropCode
                            "Plain let? would discard a refutation residue carrying a positive lower-bound obligation; use an explicit else arm."
                            (bindPatternIntroductionOrigin document binding |> Option.orElseWith (fun () -> argumentLocation document expression))
                            []
                            document
                            current
                    | None ->
                        current

                let successQuantity =
                    binding.Quantity
                    |> Option.map ResourceQuantity.ofSurface
                    |> Option.orElseWith (fun () -> tryConstructorPatternQuantity document binding.Pattern)

                let successLocalTypes =
                    tryInferConservativeExpressionType signatures localTypes expression
                    |> Option.orElseWith (fun () -> Some(inferConservativeFallbackBindingType expression))
                    |> fun valueType -> extendBindingLocalTypes signatures localTypes valueType binding

                let current =
                    let borrowRegion, nextState =
                        let introductionOrigin =
                            bindPatternIntroductionOrigin document binding
                            |> Option.orElseWith (fun () -> argumentLocation document expression)

                        introduceBorrowRegionForQuantity $"{currentScopeId current}.letq" successQuantity introductionOrigin current

                    addPatternBindings
                        document
                        binding
                        successQuantity
                        borrowRegion
                        None
                        Set.empty
                        []
                        (successQuantity |> Option.exists ResourceQuantity.requiresUse)
                        None
                        None
                        nextState

                let current =
                    match failure with
                    | None ->
                        current
                    | Some failure ->
                        let diagnosticCount = current.Diagnostics.Length
                        let eventCount = current.Events.Length
                        let borrowRegionCount = current.BorrowRegions.Length
                        let closureCount = current.Closures.Length
                        let failureQuantity =
                            failure.ResiduePattern.Quantity
                            |> Option.map ResourceQuantity.ofSurface
                            |> Option.orElseWith (fun () -> tryConstructorPatternQuantity document failure.ResiduePattern.Pattern)

                        let failureState =
                            withScope "letq_else" (fun scopedState ->
                                let borrowRegion, scopedState =
                                    introduceBorrowRegionForQuantity
                                        $"{currentScopeId scopedState}.letq_else"
                                        failureQuantity
                                        (bindPatternIntroductionOrigin document failure.ResiduePattern)
                                        scopedState

                                let scopedState =
                                    addPatternBindings
                                        document
                                        failure.ResiduePattern
                                        failureQuantity
                                        borrowRegion
                                        None
                                        Set.empty
                                        []
                                        (failureQuantity |> Option.exists ResourceQuantity.requiresUse)
                                        None
                                        None
                                        scopedState

                                checkDoStatements
                                    projectionSummaries
                                    document
                                    signatures
                                    (extendBindingLocalTypes signatures localTypes None failure.ResiduePattern)
                                    scopedState
                                    failure.Body
                                |> checkAllActiveLinearDrops document) current

                        { current with
                            Diagnostics = current.Diagnostics @ (failureState.Diagnostics |> List.skip diagnosticCount)
                            Events = current.Events @ (failureState.Events |> List.skip eventCount)
                            BorrowRegions = current.BorrowRegions @ (failureState.BorrowRegions |> List.skip borrowRegionCount)
                            Closures = current.Closures @ (failureState.Closures |> List.skip closureCount)
                            DeferredFacts = (current.DeferredFacts @ failureState.DeferredFacts) |> List.distinct |> List.sort }

                loop successLocalTypes current rest
            | DoBind(binding, expression) :: rest ->
                let current =
                    match tryFindMultiShotScopedOperation expression with
                    | Some(effectName, operation) ->
                        let riskyBindings, linearNames, borrowedNames = continuationCaptureHazards current rest

                        if List.isEmpty riskyBindings then
                            current
                        else
                            let linearNamesText = String.concat ", " linearNames
                            let borrowedNamesText = String.concat ", " borrowedNames

                            let linearSummary =
                                match linearNames with
                                | [] -> None
                                | _ -> Some($"linear bindings {linearNamesText}")

                            let borrowedSummary =
                                match borrowedNames with
                                | [] -> None
                                | _ -> Some($"borrowed bindings {borrowedNamesText}")

                            let capturedSummary =
                                [ linearSummary; borrowedSummary ]
                                |> List.choose id
                                |> String.concat " and "

                            addDiagnostic
                                DiagnosticCode.QttContinuationCapture
                                $"Multi-shot operation '{effectName}.{operation.Name}' cannot capture {capturedSummary} in its continuation."
                                (argumentLocation document expression)
                                []
                                document
                                current
                    | None ->
                        current

                let current = checkExpression projectionSummaries document signatures localTypes current expression
                let declaredQuantity = binding.Quantity |> Option.map ResourceQuantity.ofSurface
                let checkDrop = declaredQuantity |> Option.exists ResourceQuantity.requiresUse

                let borrowRegion, current =
                    let introductionOrigin =
                        bindPatternIntroductionOrigin document binding
                        |> Option.orElseWith (fun () -> argumentLocation document expression)

                    introduceBorrowRegionForQuantity $"{currentScopeId current}.bind" declaredQuantity introductionOrigin current

                let sourcePlace, sourcePlaces, current =
                    if quantityBorrows declaredQuantity then
                        prepareBorrowPatternSource projectionSummaries "do_bind" expression current
                    else
                        None, [], current

                let current =
                    sourcePlaces
                    |> List.fold (fun nextState place -> validatePlaceAccess document place nextState) current

                let current =
                    addPatternBindings
                        document
                        binding
                        declaredQuantity
                        borrowRegion
                        sourcePlace
                        Set.empty
                        []
                        checkDrop
                        None
                        None
                        current

                let current =
                    if quantityBorrows declaredQuantity then
                        addBorrowLocksForPattern sourcePlaces binding.Pattern (argumentLocation document expression) current
                    else
                        current

                let nextLocalTypes =
                    tryInferConservativeExpressionType signatures localTypes expression
                    |> Option.map unwrapBindPayloadType
                    |> Option.orElseWith (fun () -> Some(inferConservativeFallbackBindingType expression))
                    |> fun valueType -> extendBindingLocalTypes signatures localTypes valueType binding

                loop nextLocalTypes current rest
            | DoUsing(binding, expression) :: rest ->
                let current = checkExpression projectionSummaries document signatures localTypes current expression
                let surfaceOrigin =
                    bindPatternIntroductionOrigin document binding
                    |> Option.orElseWith (fun () -> argumentLocation document expression)

                let region, current =
                    introduceBorrowRegion "using" None surfaceOrigin current

                let usingScopeId = $"{currentScopeId current}.using{current.NextUsingScopeId}"
                let hiddenOwnedBinding = $"{usingScopeId}.owned"
                let releaseObligationId = $"{usingScopeId}.release"

                let current =
                    { current with
                        NextUsingScopeId = current.NextUsingScopeId + 1 }

                let current =
                    addBinding
                        UsingOwnedBinding
                        hiddenOwnedBinding
                        (Some ResourceQuantity.one)
                        None
                        Set.empty
                        []
                        false
                        None
                        None
                        (argumentLocation document expression)
                        current

                let current =
                    match tryFindBindingId hiddenOwnedBinding current with
                    | Some hiddenOwnedBindingId ->
                        current
                        |> addUsingObligation releaseObligationId usingScopeId hiddenOwnedBindingId hiddenOwnedBinding region.Id surfaceOrigin
                        |> addUsingScopeFact usingScopeId surfaceOrigin hiddenOwnedBinding releaseObligationId region
                    | None ->
                        addUsingScopeFact usingScopeId surfaceOrigin hiddenOwnedBinding releaseObligationId region current

                let current =
                    collectPatternBindings binding.Pattern
                    |> List.fold (fun state (name, path) ->
                        addBindingWithPlace
                            PatternBinding
                            (makePlace hiddenOwnedBinding path)
                            name
                            (Some(ResourceQuantity.Borrow None))
                            (Some region)
                            Set.empty
                            []
                            false
                            None
                            None
                            (bindPatternNameLocation document binding name)
                            state) current

                let nextLocalTypes =
                    tryInferConservativeExpressionType signatures localTypes expression
                    |> Option.map unwrapIoType
                    |> Option.orElseWith (fun () -> Some(inferConservativeFallbackBindingType expression))
                    |> fun valueType -> extendBindingLocalTypes signatures localTypes valueType binding

                loop nextLocalTypes current rest
            | DoDefer expression :: rest ->
                let rec containsEscapingAbruptControl expression =
                    match expression with
                    | Do statements ->
                        statements
                        |> List.exists (function
                            | DoReturn _ -> true
                            | DoLet(_, inner)
                            | DoBind(_, inner)
                            | DoVar(_, inner)
                            | DoAssign(_, inner)
                            | DoUsing(_, inner)
                            | DoDefer inner
                            | DoExpression inner -> containsEscapingAbruptControl inner
                            | DoLetQuestion(_, inner, failure) ->
                                containsEscapingAbruptControl inner
                                || (failure
                                    |> Option.exists (fun failure ->
                                        failure.Body
                                        |> List.exists (fun statement -> containsEscapingAbruptControl (Do [ statement ]))))
                            | DoIf(condition, whenTrue, whenFalse) ->
                                containsEscapingAbruptControl condition
                                || (whenTrue |> List.exists (fun statement -> containsEscapingAbruptControl (Do [ statement ])))
                                || (whenFalse |> List.exists (fun statement -> containsEscapingAbruptControl (Do [ statement ])))
                            | DoWhile(condition, body) ->
                                containsEscapingAbruptControl condition
                                || (body |> List.exists (fun statement -> containsEscapingAbruptControl (Do [ statement ]))))
                    | LocalLet(_, value, body) ->
                        containsEscapingAbruptControl value || containsEscapingAbruptControl body
                    | LocalSignature(_, body) ->
                        containsEscapingAbruptControl body
                    | LocalTypeAlias(_, body) ->
                        containsEscapingAbruptControl body
                    | LocalScopedEffect(_, body) ->
                        containsEscapingAbruptControl body
                    | Handle(_, label, body, returnClause, operationClauses) ->
                        containsEscapingAbruptControl label
                        || containsEscapingAbruptControl body
                        || containsEscapingAbruptControl returnClause.Body
                        || (operationClauses |> List.exists (fun clause -> containsEscapingAbruptControl clause.Body))
                    | IfThenElse(condition, whenTrue, whenFalse) ->
                        containsEscapingAbruptControl condition
                        || containsEscapingAbruptControl whenTrue
                        || containsEscapingAbruptControl whenFalse
                    | Match(scrutinee, cases) ->
                        containsEscapingAbruptControl scrutinee
                        || (cases |> List.exists (fun caseClause -> containsEscapingAbruptControl caseClause.Body))
                    | RecordLiteral fields ->
                        fields |> List.exists (fun field -> containsEscapingAbruptControl field.Value)
                    | Seal(value, _) ->
                        containsEscapingAbruptControl value
                    | RecordUpdate(receiver, fields) ->
                        containsEscapingAbruptControl receiver
                        || (fields |> List.exists (fun field -> containsEscapingAbruptControl field.Value))
                    | MemberAccess(receiver, _, arguments) ->
                        containsEscapingAbruptControl receiver || (arguments |> List.exists containsEscapingAbruptControl)
                    | SafeNavigation(receiver, navigation) ->
                        containsEscapingAbruptControl receiver
                        || (navigation.Arguments |> List.exists containsEscapingAbruptControl)
                    | TagTest(receiver, _) ->
                        containsEscapingAbruptControl receiver
                    | SyntaxQuote _
                    | SyntaxSplice _ ->
                        false
                    | TopLevelSyntaxSplice inner ->
                        match tryDirectSyntaxSpliceBody expression with
                        | Some quotedBody -> containsEscapingAbruptControl quotedBody
                        | None -> containsEscapingAbruptControl inner
                    | CodeQuote inner
                    | CodeSplice inner ->
                        containsEscapingAbruptControl inner
                    | MonadicSplice inner
                    | ExplicitImplicitArgument inner
                    | InoutArgument inner
                    | Unary(_, inner) ->
                        containsEscapingAbruptControl inner
                    | Apply(callee, arguments) ->
                        containsEscapingAbruptControl callee || (arguments |> List.exists containsEscapingAbruptControl)
                    | NamedApplicationBlock fields ->
                        fields |> List.exists (fun field -> containsEscapingAbruptControl field.Value)
                    | Binary(left, _, right)
                    | Elvis(left, right) ->
                        containsEscapingAbruptControl left || containsEscapingAbruptControl right
                    | Comprehension comprehension ->
                        containsEscapingAbruptControl comprehension.Lowered
                    | PrefixedString(_, parts) ->
                        parts
                        |> List.exists (function
                            | StringInterpolation(inner, _) -> containsEscapingAbruptControl inner
                            | StringText _ -> false)
                    | Literal _
                    | NumericLiteral _
                    | Name _
                    | KindQualifiedName _
                    | Lambda _ ->
                        false

                let current =
                    if containsEscapingAbruptControl expression then
                        addDiagnostic
                            DiagnosticCode.ControlFlowInvalidEscape
                            "A deferred action must not contain return, break, or continue targeting an outer scope."
                            (argumentLocation document expression)
                            []
                            document
                            current
                    else
                        current

                loop localTypes current rest
            | DoVar(name, expression) :: rest ->
                let current = checkExpression projectionSummaries document signatures localTypes current expression
                let current = addBinding LocalBinding name None None Set.empty [] false None None (findBinderLocation document name) current
                let nextLocalTypes =
                    match tryInferConservativeExpressionType signatures localTypes expression with
                    | Some valueType -> Map.add name (refType valueType) localTypes
                    | None -> localTypes
                loop nextLocalTypes current rest
            | DoAssign(_, expression) :: rest ->
                let current = checkExpression projectionSummaries document signatures localTypes current expression
                loop localTypes current rest
            | DoExpression expression :: rest ->
                let current =
                    current
                    |> noteValueDemandingNameUse document expression
                    |> fun next -> checkExpression projectionSummaries document signatures localTypes next expression

                loop localTypes current rest
            | DoIf(condition, whenTrue, whenFalse) :: rest ->
                let current =
                    current
                    |> noteValueDemandingNameUse document condition
                    |> fun next -> checkExpression projectionSummaries document signatures localTypes next condition

                let trueState = checkDoStatementsInScope "if_then" projectionSummaries document signatures localTypes current whenTrue

                let falseState =
                    if List.isEmpty whenFalse then
                        current
                    else
                        checkDoStatementsInScope "if_else" projectionSummaries document signatures localTypes current whenFalse

                loop localTypes (mergeBranchState trueState falseState) rest
            | DoReturn expression :: _ ->
                current
                |> noteValueDemandingNameUse document expression
                |> fun next -> checkExpression projectionSummaries document signatures localTypes next expression
                |> checkResultEscapeAtBoundary Set.empty document expression
                |> checkAllActiveLinearDrops document
            | DoWhile(condition, body) :: rest ->
                let current =
                    checkExpression projectionSummaries document signatures localTypes current condition
                    |> addDeferredFact OwnershipDeferredFact.WhileResourceFixedPoint

                let bodyState = checkDoStatementsInScope "while" projectionSummaries document signatures localTypes current body
                let current = mergeBranchState current bodyState
                loop localTypes current rest

        let checkedState = loop localTypes state statements

        checkScopeLinearDrops document checkedState

    and private checkDoStatementsInScope
        scopeLabel
        projectionSummaries
        (document: ParsedDocument)
        (signatures: Map<string, FunctionSignature>)
        (localTypes: Map<string, TypeSignatures.TypeExpr>)
        state
        statements
        =
        let allowedRegions = liveRegionIds state
        let expression =
            match List.tryLast statements with
            | Some(DoExpression result) -> Some result
            | _ -> None

        withScope scopeLabel (fun scopedState ->
            let checkedState = checkDoStatements projectionSummaries document signatures localTypes scopedState statements

            match expression with
            | Some result ->
                checkResultEscapeAtBoundary allowedRegions document result checkedState
            | None ->
                checkedState) state

    let private tryFindDefinitionSignature
        (signatures: Map<string, FunctionSignature>)
        (document: ParsedDocument)
        (definition: LetDefinition)
        =
        definition.Name
        |> Option.bind (fun name ->
            let qualifiedName =
                document.ModuleName
                |> Option.map (fun moduleName -> $"{SyntaxFacts.moduleNameToText moduleName}.{name}")

            qualifiedName
            |> Option.bind (fun qualified -> Map.tryFind qualified signatures)
            |> Option.orElseWith (fun () -> Map.tryFind name signatures))

    let private addParameterBinding
        aliasMap
        quantityAliasMap
        typeAliasMap
        (document: ParsedDocument)
        (signature: FunctionSignature option)
        parameterIndex
        (parameter: Parameter)
        state
        =
        let signatureQuantity =
            signature
            |> Option.bind (fun functionSignature ->
                functionSignature.ParameterQuantities
                |> List.tryItem parameterIndex)
            |> Option.flatten

        let signatureInout =
            signature
            |> Option.bind (fun functionSignature ->
                functionSignature.ParameterInout
                |> List.tryItem parameterIndex)
            |> Option.defaultValue false

        let signatureTypeTokens =
            signature
            |> Option.bind (fun functionSignature ->
                functionSignature.ParameterTypeTokens
                |> List.tryItem parameterIndex)
            |> Option.flatten

        let quantity =
            if parameter.IsInout || signatureInout then
                Some ResourceQuantity.one
            else
                signatureQuantity
                |> Option.orElseWith (fun () -> parameter.Quantity |> Option.map ResourceQuantity.ofSurface)

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
            | Some region -> addBorrowRegionFact (findBinderLocation document parameter.Name) region state
            | None -> state

        let recordFieldDependencies =
            parameter.TypeTokens
            |> Option.orElse signatureTypeTokens
            |> Option.map (tryResolveRecordFieldDependencies aliasMap)
            |> Option.defaultValue Map.empty

        let recordFieldQuantities =
            parameter.TypeTokens
            |> Option.orElse signatureTypeTokens
            |> Option.map (tryResolveRecordFieldQuantities quantityAliasMap)
            |> Option.defaultValue Map.empty

        let capturedRegions =
            parameter.TypeTokens
            |> Option.orElse signatureTypeTokens
            |> Option.bind TypeSignatures.parseType
            |> Option.map (captureSetFromTypeWithAliases typeAliasMap)
            |> Option.defaultValue Set.empty

        let state =
            addBinding ParameterBinding parameter.Name quantity region capturedRegions [] checkDrop None None (findBinderLocation document parameter.Name) state

        match tryFindBindingId parameter.Name state with
        | Some bindingId ->
            updateBinding bindingId (fun binding ->
                { binding with
                    RecordFieldDependencies = recordFieldDependencies
                    RecordFieldQuantities = recordFieldQuantities }) state
        | None ->
            state

    let private checkDefinition
        aliasMap
        quantityAliasMap
        typeAliasMap
        projectionSummaries
        (signatures: Map<string, FunctionSignature>)
        (document: ParsedDocument)
        scopeId
        (definition: LetDefinition)
        =
        if definition.IsPattern then
            emptyState scopeId
        else
        match definition.Body with
        | Some body ->
            let signature = tryFindDefinitionSignature signatures document definition
            let declaredReturnType = definitionReturnTypeTokens document definition |> Option.bind TypeSignatures.parseType
            let allowedReturnRegions =
                declaredReturnType
                |> Option.map (captureSetFromTypeWithAliases typeAliasMap)
                |> Option.defaultValue Set.empty

            let localTypes =
                definition.Parameters
                |> List.mapi (fun parameterIndex parameter ->
                    let signatureTypeTokens =
                        signature
                        |> Option.bind (fun functionSignature ->
                            functionSignature.ParameterTypeTokens
                            |> List.tryItem parameterIndex)
                        |> Option.flatten

                    parameter.Name,
                    (parameter.TypeTokens
                     |> Option.orElse signatureTypeTokens
                     |> Option.bind TypeSignatures.parseType))
                |> List.choose (fun (name, inferredType) ->
                    inferredType |> Option.map (fun parsedType -> name, parsedType))
                |> Map.ofList

            let initialState =
                definition.Parameters
                |> List.mapi (fun index parameter -> index, parameter)
                |> List.fold
                    (fun state (parameterIndex, parameter) ->
                        addParameterBinding aliasMap quantityAliasMap typeAliasMap document signature parameterIndex parameter state)
                    (emptyState scopeId)
                |> checkInoutThreadedFieldRequirements document signature definition

            match body with
            | Lambda _ -> checkEscapingLambdaAgainst allowedReturnRegions document body initialState
            | _ ->
                let captureTypeMismatch =
                    match declaredReturnType, tryInferConservativeExpressionType signatures localTypes body with
                    | Some expectedType, Some actualType ->
                        (typeContainsCaptureAnnotation expectedType || typeContainsCaptureAnnotation actualType)
                        && not (TypeSignatures.definitionallyEqual actualType expectedType)
                    | _ ->
                        false

                let checkedState =
                    initialState
                    |> noteValueDemandingNameUse document body
                    |> fun current -> checkExpression projectionSummaries document signatures localTypes current body
                    |> fun current ->
                        if captureTypeMismatch then
                            current
                        else
                            checkResultEscapeAtBoundary allowedReturnRegions document body current
                    |> checkScopeLinearDrops document

                if captureTypeMismatch then
                    addDiagnostic
                        DiagnosticCode.TypeEqualityMismatch
                        "The definition body does not match the declared result type."
                        (argumentLocation document body)
                        []
                        document
                        checkedState
                else
                    checkedState
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

    let private checkDocument
        aliasMap
        quantityAliasMap
        typeAliasMap
        projectionSummaries
        signatures
        localEffectDeclarationsByModule
        exportedEffectDeclarationsByModule
        (document: ParsedDocument)
        =
        let visibleTopLevelEffects =
            visibleTopLevelEffectDeclarations localEffectDeclarationsByModule exportedEffectDeclarationsByModule document

        let states =
            document.Syntax.Declarations
            |> List.mapi (fun index declaration ->
                match declaration with
                | LetDeclaration definition ->
                    withScopedEffectDeclarations visibleTopLevelEffects (fun () ->
                        checkDefinition
                            aliasMap
                            quantityAliasMap
                            typeAliasMap
                            projectionSummaries
                            signatures
                            document
                            (definitionScopeId document index definition)
                            definition)
                | _ ->
                    emptyState $"{document.Source.FilePath}.decl{index}")

        let diagnostics =
            states
            |> List.collect (fun state -> state.Diagnostics)
            |> List.distinctBy (fun diagnostic ->
                diagnostic.Code,
                diagnostic.Severity,
                diagnostic.Message,
                (diagnostic.Location
                 |> Option.map (fun location -> location.FilePath, location.Span.Start, location.Span.Length)),
                (diagnostic.RelatedLocations
                 |> List.map (fun related ->
                     related.Message, related.Location.FilePath, related.Location.Span.Start, related.Location.Span.Length)))

        diagnostics, factsForDocument document states

    let checkDocumentsWithFactsForBackend configuredBackendProfile (documents: ParsedDocument list) =
        withBackendProfile configuredBackendProfile (fun () ->
            let signatures = collectSignatures documents
            let aliasMap = collectRecordTypeAliases documents
            let quantityAliasMap = collectRecordTypeQuantityAliases documents
            let typeAliasMap = collectTypeAliases documents
            let projectionSummaries = collectProjectionSummaries documents
            let localEffectDeclarationsByModule, exportedEffectDeclarationsByModule = buildEffectDeclarationIndices documents

            let checkedDocuments =
                documents
                |> List.map (fun document ->
                    let diagnostics, facts =
                        checkDocument
                            aliasMap
                            quantityAliasMap
                            typeAliasMap
                            projectionSummaries
                            signatures
                            localEffectDeclarationsByModule
                            exportedEffectDeclarationsByModule
                            document
                    document.Source.FilePath, diagnostics, facts)

            { Diagnostics =
                checkedDocuments
                |> List.collect (fun (_, diagnostics, _) -> diagnostics)
              OwnershipFactsByFile =
                checkedDocuments
                |> List.map (fun (filePath, _, facts) -> filePath, facts)
                |> Map.ofList })

    let checkDocumentsWithFacts (documents: ParsedDocument list) =
        checkDocumentsWithFactsForBackend BackendProfile.Interpreter documents

    let checkDocumentsForBackend configuredBackendProfile documents =
        (checkDocumentsWithFactsForBackend configuredBackendProfile documents).Diagnostics

    let checkDocuments documents =
        (checkDocumentsWithFacts documents).Diagnostics
