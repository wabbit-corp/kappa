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

    type private ProjectionSummary =
        { Name: string
          Binders: ProjectionBinder list
          Body: SurfaceProjectionBody option }

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
                  IntroducedBorrowLocks = [] }
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

            let borrowLocks =
                scope.IntroducedBorrowLocks
                |> List.fold (fun current lockId -> Map.remove lockId current) state.BorrowLocks

            { state with
                ActiveScopes = rest
                ActiveBindingIds = activeBindingIds
                BorrowLocks = borrowLocks }

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
        | None when binding.UseMaximum > 0 || not (List.isEmpty binding.ConsumedPaths) -> "consumed"
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

    let private addEventAtPlace useKind origin placeRoot placePath (binding: ResourceBinding) (state: CheckState) =
        let event: OwnershipUseFact =
            { UseId = $"{currentScopeId state}.u{state.NextEventId}"
              UseKindName = useKind
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

    let private collectPatternBindings (pattern: SurfacePattern) =
        let rec loop path current =
            seq {
                match current with
                | NamePattern name ->
                    yield name, path
                | ConstructorPattern(_, arguments) ->
                    for argument in arguments do
                        yield! loop path argument
                | AnonymousRecordPattern fields ->
                    for field in fields do
                        yield! loop (path @ [ field.Name ]) field.Pattern
                | WildcardPattern
                | LiteralPattern _ -> ()
            }

        loop [] pattern |> Seq.toList

    let private collectPatternNames (pattern: SurfacePattern) =
        collectPatternBindings pattern |> List.map fst

    let rec private expressionNames (expression: SurfaceExpression) =
        seq {
            match expression with
            | Literal _ -> ()
            | Name [ name ] -> yield name
            | Name _ -> ()
            | LocalLet(binding, value, body) ->
                yield! expressionNames value

                let boundNames =
                    collectPatternNames binding.Pattern
                    |> Set.ofList

                for name in expressionNames body do
                    if not (Set.contains name boundNames) then
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

    let private addBindingWithPlace place name quantity borrowRegion capturedRegions capturedBindingOrigins checkDrop closureFactId localLambda origin (state: CheckState) =
        let bindingId = $"{currentScopeId state}.b{state.NextBindingId}"

        let binding: ResourceBinding =
            { Id = bindingId
              Name = name
              DeclaredQuantity = quantity
              Place = place
              ConsumedPaths = []
              RecordFieldDependencies = Map.empty
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

    let private addBinding name quantity borrowRegion capturedRegions capturedBindingOrigins checkDrop closureFactId localLambda origin state =
        addBindingWithPlace
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
                place
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
                    |> Option.exists ResourceQuantity.isExactOne)
                |> Option.map (fun binding -> binding, place.Path))

    let private consumeBindingAtPlace (document: ParsedDocument) (place: ResourcePlace) (state: CheckState) =
        match tryFindBinding place.Root state with
        | None -> state
        | Some binding ->
            let placeText = String.concat "." (place.Root :: place.Path)
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
                    addDiagnostic
                        linearOveruseCode
                        $"Linear resource '{place.Root}' cannot be consumed as a whole after one of its field paths has already been consumed."
                        consumeOrigin
                        []
                        document
                        state
                elif not (List.isEmpty place.Path) && binding.UseMaximum > 0 then
                    addDiagnostic
                        linearOveruseCode
                        $"Field path '{placeText}' cannot be consumed after its root resource has already been consumed."
                        consumeOrigin
                        []
                        document
                        state
                elif not (List.isEmpty place.Path)
                     && (binding.ConsumedPaths |> List.exists (fun consumedPath -> pathsOverlap consumedPath place.Path)) then
                    addDiagnostic
                        linearOveruseCode
                        $"Field path '{placeText}' has already been consumed."
                        consumeOrigin
                        []
                        document
                        state
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

                    addDiagnostic
                        linearOveruseCode
                        $"Linear resource '{place.Root}' is consumed more than once."
                        consumeOrigin
                        relatedLocations
                        document
                        state
                else
                    state

            let state = addEventAtPlace "consume" consumeOrigin place.Root place.Path binding state

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

            { state with
                Bindings = Map.add binding.Id updated state.Bindings }

    let private consumeBinding (document: ParsedDocument) name (state: CheckState) =
        consumeBindingAtPlace document (ResourcePlace.root name) state

    let private validatePlaceAccess (document: ParsedDocument) (place: ResourcePlace) (state: CheckState) =
        match tryFindBinding place.Root state with
        | _ when tryFindBorrowOverlap place state |> Option.isSome ->
            let borrowLock = tryFindBorrowOverlap place state |> Option.get
            let placeText = String.concat "." (place.Root :: place.Path)

            addDiagnostic
                borrowOverlapCode
                $"Place '{placeText}' overlaps an active borrowed footprint."
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

    let private bindMatchPatternNames (document: ParsedDocument) scrutineeName pattern state =
        match tryFindBinding scrutineeName state with
        | None ->
            state
        | Some scrutineeBinding ->
            let patternBindings = collectPatternBindings pattern

            if List.isEmpty patternBindings then
                state
            else
                let state =
                    match scrutineeBinding.DeclaredQuantity with
                    | Some quantity when ResourceQuantity.isExactOne quantity ->
                        consumeBinding document scrutineeName state
                    | _ ->
                        state

                let checkDrop =
                    scrutineeBinding.DeclaredQuantity
                    |> Option.exists ResourceQuantity.requiresUse

                patternBindings
                |> List.fold (fun current (name, path) ->
                    addBindingWithPlace
                        (makePlace scrutineeBinding.Place.Root (scrutineeBinding.Place.Path @ path))
                        name
                        scrutineeBinding.DeclaredQuantity
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
        | ConstructorPattern(_, arguments) ->
            arguments |> List.sumBy patternBoundNameCount
        | AnonymousRecordPattern fields ->
            fields |> List.sumBy (fun field -> patternBoundNameCount field.Pattern)

    let private matchPatternOwnershipSupported scrutinee pattern =
        match patternBoundNameCount pattern with
        | 0 ->
            true
        | 1 ->
            match scrutinee with
            | Name [ _ ] -> true
            | _ -> false
        | _ ->
            false

    let private mergeBindingSnapshots (leftBinding: ResourceBinding) (rightBinding: ResourceBinding) =
        { leftBinding with
            ConsumedPaths = distinctPaths (leftBinding.ConsumedPaths @ rightBinding.ConsumedPaths)
            RecordFieldDependencies =
                if Map.isEmpty leftBinding.RecordFieldDependencies then
                    rightBinding.RecordFieldDependencies
                else
                    leftBinding.RecordFieldDependencies
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
        addBinding hiddenName (Some ResourceQuantity.one) None Set.empty [] false None None None state

    let private prepareBorrowPatternSource projectionSummaries scopeLabel expression state =
        match tryBorrowablePlacesWithEnv Map.empty projectionSummaries state expression with
        | Some [ place ] ->
            Some place, [ place ], state
        | Some places ->
            None, places, state
        | None ->
            let place, state = introduceHiddenBorrowRoot scopeLabel state
            Some place, [ place ], state

    let rec private expressionMayCarryEscapingBorrow expression =
        match expression with
        | Name [ _ ]
        | Lambda _ ->
            true
        | LocalLet(_, _, body) ->
            expressionMayCarryEscapingBorrow body
        | IfThenElse(_, whenTrue, whenFalse) ->
            expressionMayCarryEscapingBorrow whenTrue || expressionMayCarryEscapingBorrow whenFalse
        | Match(_, cases) ->
            cases |> List.exists (fun caseClause -> expressionMayCarryEscapingBorrow caseClause.Body)
        | Do statements ->
            match List.tryLast statements with
            | Some(DoExpression result) -> expressionMayCarryEscapingBorrow result
            | _ -> false
        | MonadicSplice inner
        | InoutArgument inner
        | Unary(_, inner) ->
            expressionMayCarryEscapingBorrow inner
        | Literal _
        | Name _
        | RecordUpdate _
        | Apply _
        | Binary _
        | PrefixedString _ ->
            false

    let rec private checkResultEscapeAtBoundary allowedRegions (document: ParsedDocument) expression state =
        match expression with
        | Name [ _ ]
        | Lambda _ ->
            checkEscapeAgainstAllowed allowedRegions document expression state
        | LocalLet(_, _, body) ->
            checkResultEscapeAtBoundary allowedRegions document body state
        | IfThenElse(_, whenTrue, whenFalse) ->
            state
            |> checkResultEscapeAtBoundary allowedRegions document whenTrue
            |> checkResultEscapeAtBoundary allowedRegions document whenFalse
        | Match(_, cases) ->
            (state, cases)
            ||> List.fold (fun current caseClause ->
                checkResultEscapeAtBoundary allowedRegions document caseClause.Body current)
        | Do statements ->
            match List.tryLast statements with
            | Some(DoExpression result) ->
                checkResultEscapeAtBoundary allowedRegions document result state
            | _ ->
                state
        | MonadicSplice inner
        | InoutArgument inner
        | Unary(_, inner) ->
            checkResultEscapeAtBoundary allowedRegions document inner state
        | Literal _
        | Name _
        | Apply _
        | Binary _
        | RecordUpdate _
        | PrefixedString _ ->
            state

    let private checkEscapingLambda (document: ParsedDocument) expression state =
        match expression with
        | Lambda _ ->
            let captured = capturedRegions state expression
            let capturedBindingList = capturedBindings state expression

            let state =
                capturedBindingList
                |> List.fold (fun current binding ->
                    addEvent "capture" (findUseLocation document binding.Name 1) binding current) state

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

            if Set.isEmpty captured then
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

    let private checkLambdaParameterEscape (document: ParsedDocument) expression state =
        let addLambdaParameterBinding (parameter: Parameter) current =
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

            let current =
                match region with
                | Some region -> addBorrowRegionFact region current
                | None -> current

            addBinding parameter.Name quantity region Set.empty [] checkDrop None None (findBinderLocation document parameter.Name) current

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
        projectionSummaries
        (document: ParsedDocument)
        (signatures: Map<string, FunctionSignature>)
        state
        (lambdaValue: LocalLambda)
        arguments
        =
        if List.length arguments <> List.length lambdaValue.Parameters then
            state
        else
            let allowedRegions = liveRegionIds state

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

                checkExpression projectionSummaries document signatures scopedState lambdaValue.Body
                |> checkResultEscapeAtBoundary allowedRegions document lambdaValue.Body
                |> checkScopeLinearDrops document) state

    and private checkExpression projectionSummaries (document: ParsedDocument) (signatures: Map<string, FunctionSignature>) state expression =
        match expression with
        | Literal _
        | Name [ _ ] ->
            state
        | Name(root :: path) ->
            validatePlaceAccess
                document
                (makePlace root path)
                state
        | Name [] ->
            state
        | LocalLet(binding, value, body) ->
            let movedLinearBinding = tryMovedLinearBinding value state
            let captured =
                match value with
                | Lambda _ -> capturedRegions state value
                | _ -> Set.empty

            let capturedBindings =
                match value with
                | Lambda _ -> capturedBindings state value
                | _ -> []

            let state =
                match value with
                | Lambda _ -> checkLambdaParameterEscape document value state
                | _ -> checkExpression projectionSummaries document signatures state value

            let state =
                capturedBindings
                |> List.fold (fun current binding ->
                    addEvent "capture" (findUseLocation document binding.Name 1) binding current) state

            let allowedRegions = liveRegionIds state
            let bindingNames = collectPatternNames binding.Pattern
            let scopeLabel =
                match bindingNames with
                | first :: _ -> $"let_{first}"
                | [] -> "let_pattern"

            withScope scopeLabel (fun scopedState ->
                let declaredQuantity =
                    binding.Quantity
                    |> Option.map ResourceQuantity.ofSurface
                    |> Option.orElseWith (fun () ->
                        match value with
                        | Lambda _ -> inferLambdaBindingQuantity capturedBindings
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
                    match value, bindingNames with
                    | Lambda(parameters, lambdaBody), [ _ ] ->
                        Some
                            { Parameters = parameters
                              Body = lambdaBody }
                    | _ ->
                        None

                let closureFactId, scopedState =
                    match value with
                    | Lambda _ ->
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
                            "move"
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
                        introduceBorrowRegionForQuantity $"{currentScopeId scopedState}.let" declaredQuantity scopedState

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

                checkExpression projectionSummaries document signatures scopedState body
                |> checkResultEscapeAtBoundary allowedRegions document body
                |> checkScopeLinearDrops document) state
        | Lambda _ ->
            checkLambdaParameterEscape document expression state
        | IfThenElse(condition, whenTrue, whenFalse) ->
            let state = checkExpression projectionSummaries document signatures state condition
            let left = checkExpressionInScope "if_then" projectionSummaries document signatures state whenTrue
            let right = checkExpressionInScope "if_else" projectionSummaries document signatures state whenFalse
            mergeBranchState left right
        | Match(scrutinee, cases) ->
            let state =
                let checkedState = checkExpression projectionSummaries document signatures state scrutinee

                if cases |> List.forall (fun caseClause -> matchPatternOwnershipSupported scrutinee caseClause.Pattern) then
                    checkedState
                else
                    addDeferredFact "match-pattern-resource-checking" checkedState

            match cases with
            | [] -> state
            | first :: rest ->
                let checkCase index (caseClause: SurfaceMatchCase) =
                    let allowedRegions = liveRegionIds state

                    withScope $"match_case{index}" (fun scopedState ->
                        let scopedState =
                            match scrutinee with
                            | Name [ scrutineeName ] when matchPatternOwnershipSupported scrutinee caseClause.Pattern ->
                                bindMatchPatternNames document scrutineeName caseClause.Pattern scopedState
                            | _ ->
                                scopedState

                        let scopedState =
                            match caseClause.Guard with
                            | Some guard -> checkExpression projectionSummaries document signatures scopedState guard
                            | None -> scopedState

                        checkExpression projectionSummaries document signatures scopedState caseClause.Body
                        |> checkResultEscapeAtBoundary allowedRegions document caseClause.Body
                        |> checkScopeLinearDrops document) state

                let firstState = checkCase 0 first

                rest
                |> List.mapi (fun index caseClause ->
                    index + 1, caseClause)
                |> List.fold (fun current (index, caseClause) ->
                    mergeBranchState current (checkCase index caseClause)) firstState
        | RecordUpdate(receiver, fields) ->
            let state = checkExpression projectionSummaries document signatures state receiver

            let state =
                fields
                |> List.fold (fun current field ->
                    checkExpression projectionSummaries document signatures current field.Value) state

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
        | Do statements ->
            checkDoStatementsInScope "do" projectionSummaries document signatures state statements
        | MonadicSplice inner
        | InoutArgument inner
        | Unary(_, inner) ->
            checkExpression projectionSummaries document signatures state inner
        | Binary(left, _, right) ->
            state
            |> fun state -> checkExpression projectionSummaries document signatures state left
            |> fun state -> checkExpression projectionSummaries document signatures state right
        | PrefixedString(_, parts) ->
            parts
            |> List.fold (fun current part ->
                match part with
                | StringText _ -> current
                | StringInterpolation inner -> checkExpression projectionSummaries document signatures current inner) state
        | Apply(callee, arguments) ->
            let state = checkExpression projectionSummaries document signatures state callee
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

                    let current =
                        match quantity, inferredClosureQuantity current argument with
                        | Some(Some demandQuantity), Some capability
                            when not (ResourceQuantity.satisfies capability demandQuantity) ->
                            addDiagnostic
                                linearOveruseCode
                                $"An argument usable at quantity '{ResourceQuantity.toSurfaceText capability}' cannot satisfy parameter demand '{ResourceQuantity.toSurfaceText demandQuantity}'."
                                (argumentLocation document argument)
                                []
                                document
                                current
                        | _ ->
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
                            |> fun current -> checkExpression projectionSummaries document signatures current argument
                        | Some quantity, _ when quantityBorrows quantity ->
                            checkExpression projectionSummaries document signatures current argument
                        | _ ->
                            let current =
                                if expressionMayCarryEscapingBorrow argument then
                                    checkEscape document argument current
                                else
                                    current

                            checkExpression projectionSummaries document signatures current argument

                    next, index + 1)
                |> fst

            match localLambda with
            | Some lambdaValue ->
                checkLocalLambdaInvocation projectionSummaries document signatures state lambdaValue arguments
            | None ->
                state

    and private checkExpressionInScope scopeLabel projectionSummaries (document: ParsedDocument) (signatures: Map<string, FunctionSignature>) state expression =
        let allowedRegions = liveRegionIds state

        withScope scopeLabel (fun scopedState ->
            checkExpression projectionSummaries document signatures scopedState expression
            |> checkResultEscapeAtBoundary allowedRegions document expression) state

    and private checkDoStatements projectionSummaries (document: ParsedDocument) (signatures: Map<string, FunctionSignature>) state statements =
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
                    | Lambda _ -> checkLambdaParameterEscape document expression current
                    | _ -> checkExpression projectionSummaries document signatures current expression

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
                            "move"
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
                    introduceBorrowRegionForQuantity $"{currentScopeId current}.let" declaredQuantity current

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

                loop current rest
            | DoBind(binding, expression) :: rest ->
                let current = checkExpression projectionSummaries document signatures current expression
                let declaredQuantity = binding.Quantity |> Option.map ResourceQuantity.ofSurface
                let checkDrop = declaredQuantity |> Option.exists ResourceQuantity.requiresUse

                let borrowRegion, current =
                    introduceBorrowRegionForQuantity $"{currentScopeId current}.bind" declaredQuantity current

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

                loop current rest
            | DoUsing(pattern, expression) :: rest ->
                let current = checkExpression projectionSummaries document signatures current expression

                let region, current =
                    introduceBorrowRegion "using" None current

                let usingScopeId = $"{currentScopeId current}.using{current.NextUsingScopeId}"
                let hiddenOwnedBinding = $"{usingScopeId}.owned"

                let current =
                    { current with
                        NextUsingScopeId = current.NextUsingScopeId + 1 }
                    |> addUsingScopeFact usingScopeId hiddenOwnedBinding region

                let current =
                    collectPatternBindings pattern
                    |> List.fold (fun state (name, path) ->
                        addBindingWithPlace
                            (makePlace hiddenOwnedBinding path)
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
                let current = checkExpression projectionSummaries document signatures current expression
                let current = addBinding name None None Set.empty [] false None None (findBinderLocation document name) current
                loop current rest
            | DoAssign(_, expression) :: rest ->
                let current = checkExpression projectionSummaries document signatures current expression
                loop current rest
            | DoExpression expression :: rest ->
                let current = checkExpression projectionSummaries document signatures current expression
                loop current rest
            | DoWhile(condition, body) :: rest ->
                let current =
                    checkExpression projectionSummaries document signatures current condition
                    |> addDeferredFact "while-resource-fixed-point"

                let bodyState = checkDoStatementsInScope "while" projectionSummaries document signatures current body
                let current = mergeBranchState current bodyState
                loop current rest

        let checkedState = loop state statements

        checkScopeLinearDrops document checkedState

    and private checkDoStatementsInScope scopeLabel projectionSummaries (document: ParsedDocument) (signatures: Map<string, FunctionSignature>) state statements =
        let allowedRegions = liveRegionIds state
        let expression =
            match List.tryLast statements with
            | Some(DoExpression result) -> Some result
            | _ -> None

        withScope scopeLabel (fun scopedState ->
            let checkedState = checkDoStatements projectionSummaries document signatures scopedState statements

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
            | Some region -> addBorrowRegionFact region state
            | None -> state

        let recordFieldDependencies =
            parameter.TypeTokens
            |> Option.orElse signatureTypeTokens
            |> Option.map (tryResolveRecordFieldDependencies aliasMap)
            |> Option.defaultValue Map.empty

        let state =
            addBinding parameter.Name quantity region Set.empty [] checkDrop None None (findBinderLocation document parameter.Name) state

        match tryFindBindingId parameter.Name state with
        | Some bindingId ->
            updateBinding bindingId (fun binding ->
                { binding with
                    RecordFieldDependencies = recordFieldDependencies }) state
        | None ->
            state

    let private checkDefinition
        aliasMap
        projectionSummaries
        (signatures: Map<string, FunctionSignature>)
        (document: ParsedDocument)
        scopeId
        (definition: LetDefinition)
        =
        match definition.Body with
        | Some body ->
            let signature = tryFindDefinitionSignature signatures document definition

            let initialState =
                definition.Parameters
                |> List.mapi (fun index parameter -> index, parameter)
                |> List.fold
                    (fun state (parameterIndex, parameter) ->
                        addParameterBinding aliasMap document signature parameterIndex parameter state)
                    (emptyState scopeId)

            match body with
            | Lambda _ -> checkEscapingLambda document body initialState
            | _ ->
                checkExpression projectionSummaries document signatures initialState body
                |> checkResultEscapeAtBoundary Set.empty document body
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

    let private checkDocument aliasMap projectionSummaries signatures (document: ParsedDocument) =
        let states =
            document.Syntax.Declarations
            |> List.mapi (fun index declaration ->
                match declaration with
                | LetDeclaration definition ->
                    checkDefinition aliasMap projectionSummaries signatures document (definitionScopeId document index definition) definition
                | _ ->
                    emptyState $"{document.Source.FilePath}.decl{index}")

        let diagnostics =
            states |> List.collect (fun state -> state.Diagnostics)

        diagnostics, factsForDocument document states

    let checkDocumentsWithFacts (documents: ParsedDocument list) =
        let signatures = collectSignatures documents
        let aliasMap = collectRecordTypeAliases documents
        let projectionSummaries = collectProjectionSummaries documents

        let checkedDocuments =
            documents
            |> List.map (fun document ->
                let diagnostics, facts = checkDocument aliasMap projectionSummaries signatures document
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
