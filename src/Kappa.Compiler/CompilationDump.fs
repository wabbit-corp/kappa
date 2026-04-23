namespace Kappa.Compiler

open System
open System.Text.Json

// Serializes checkpoints, traces, and target artifacts into JSON and S-expression dumps.
module CompilationDump =
    open CompilationCommon
    open CompilationFrontend

    type DumpSourceDocument =
        { FilePath: string
          ModuleIdentity: string option
          Text: string
          LineCount: int }

    type DumpToken =
        { Kind: string
          Text: string }

    type DumpDiagnostic =
        { Code: string
          Stage: string option
          Phase: string option
          Severity: string
          Message: string
          FilePath: string option
          StartLine: int option
          StartColumn: int option
          EndLine: int option
          EndColumn: int option
          RelatedOrigins: DumpRelatedOrigin list }

    and DumpRelatedOrigin =
        { Message: string
          FilePath: string
          StartLine: int
          StartColumn: int
          EndLine: int
          EndColumn: int }

    type DumpOwnershipBinding =
        { Id: string
          Name: string
          Kind: string
          DeclaredQuantity: string option
          InferredDemand: string
          State: string
          PlaceRoot: string
          PlacePath: string list
          BorrowRegionId: string option }

    type DumpOwnershipUse =
        { Id: string
          UseKind: string
          TargetBindingId: string option
          TargetName: string
          PlaceRoot: string
          PlacePath: string list }

    type DumpOwnershipBorrowRegion =
        { Id: string
          ExplicitName: string option
          OwnerScope: string }

    type DumpOwnershipUsingScope =
        { Id: string
          HiddenOwnedBinding: string
          SharedRegionId: string
          HiddenReleaseObligation: string }

    type DumpOwnershipClosure =
        { Id: string
          Name: string option
          CaptureBindingIds: string list
          CaptureNames: string list
          RegionEnvironment: string list
          EscapeStatus: string }

    type DumpOwnershipFacts =
        { Bindings: DumpOwnershipBinding list
          Uses: DumpOwnershipUse list
          BorrowRegions: DumpOwnershipBorrowRegion list
          UsingScopes: DumpOwnershipUsingScope list
          Closures: DumpOwnershipClosure list
          Deferred: string list
          Diagnostics: string list }

    type DumpDeclaration =
        { Kind: string
          Name: string option
          Visibility: string option
          IsOpaque: bool
          Summary: string
          TypeText: string option
          BodyText: string option
          Constructors: string list
          Members: string list }

    type DumpDocument =
        { FilePath: string
          ModuleHeader: string option
          InferredModuleName: string option
          ModuleIdentity: string option
          ResolvedPhases: string list
          ModuleAttributes: string list
          Imports: string list
          Tokens: DumpToken list
          Declarations: DumpDeclaration list
          Diagnostics: DumpDiagnostic list
          Ownership: DumpOwnershipFacts option }

    type DumpCoreModule =
        { Name: string
          SourceFile: string
          Imports: string list
          Ownership: DumpOwnershipFacts option
          Declarations: DumpDeclaration list }

    type DumpRuntimeBinding =
        { Name: string
          Parameters: string list
          Body: string
          Intrinsic: bool }

    type DumpRuntimeConstructor =
        { Name: string
          Arity: int
          TypeName: string }

    type DumpRuntimeModule =
        { Name: string
          SourceFile: string
          Exports: string list
          IntrinsicTerms: string list
          Constructors: DumpRuntimeConstructor list
          Bindings: DumpRuntimeBinding list }

    type DumpBackendParameter =
        { Name: string
          Representation: string }

    type DumpBackendCapture =
        { Name: string
          Representation: string }

    type DumpProvenance =
        { FilePath: string
          ModuleName: string
          DeclarationName: string option
          IntroductionKind: string }

    type DumpBackendNode =
        { Id: string
          Kind: string
          Summary: string
          Representation: string option
          Provenance: DumpProvenance option }

    type DumpBackendEdge =
        { Id: string
          SourceId: string
          TargetId: string
          Role: string }

    type DumpBackendGraph =
        { RootNodeId: string option
          Nodes: DumpBackendNode list
          Edges: DumpBackendEdge list }

    type DumpBackendFunction =
        { Id: string
          Name: string
          Parameters: DumpBackendParameter list
          CallingConvention: string
          ReturnRepresentation: string option
          EnvironmentLayout: string option
          Intrinsic: bool
          Exported: bool
          EntryPoint: bool
          ControlForm: string
          Body: string
          BodyGraph: DumpBackendGraph option
          Provenance: DumpProvenance }

    type DumpBackendConstructorLayout =
        { Id: string
          Name: string
          Tag: int
          FieldRepresentations: string list
          Provenance: DumpProvenance }

    type DumpBackendDataLayout =
        { Id: string
          TypeName: string
          RepresentationClass: string
          TagEncoding: string
          Constructors: DumpBackendConstructorLayout list
          Provenance: DumpProvenance }

    type DumpBackendEnvironmentLayout =
        { Id: string
          Name: string
          Slots: DumpBackendCapture list }

    type DumpBackendModule =
        { Id: string
          Name: string
          SourceFile: string
          Imports: string list
          Exports: string list
          EntryPoints: string list
          EntryPointIds: string list
          IntrinsicTerms: string list
          DataLayouts: DumpBackendDataLayout list
          EnvironmentLayouts: DumpBackendEnvironmentLayout list
          Functions: DumpBackendFunction list }

    let private dumpDiagnostic (diagnostic: Diagnostic) =
        let dumpRelatedOrigin (related: DiagnosticRelatedLocation) =
            { Message = related.Message
              FilePath = related.Location.FilePath
              StartLine = related.Location.Start.Line
              StartColumn = related.Location.Start.Column
              EndLine = related.Location.End.Line
              EndColumn = related.Location.End.Column }

        { Code = diagnostic.Code
          Stage = diagnostic.Stage
          Phase = diagnostic.Phase
          Severity = severityText diagnostic.Severity
          Message = diagnostic.Message
          FilePath = diagnostic.Location |> Option.map (fun location -> location.FilePath)
          StartLine = diagnostic.Location |> Option.map (fun location -> location.Start.Line)
          StartColumn = diagnostic.Location |> Option.map (fun location -> location.Start.Column)
          EndLine = diagnostic.Location |> Option.map (fun location -> location.End.Line)
          EndColumn = diagnostic.Location |> Option.map (fun location -> location.End.Column)
          RelatedOrigins = diagnostic.RelatedLocations |> List.map dumpRelatedOrigin }

    let private dumpDeclaration (declaration: TopLevelDeclaration) =
        { Kind = declarationKindText declaration
          Name = declarationName declaration
          Visibility = declarationVisibility declaration
          IsOpaque = declarationIsOpaque declaration
          Summary = declarationSummary declaration
          TypeText = declarationTypeText declaration
          BodyText = declarationBodyText declaration
          Constructors = declarationConstructors declaration
          Members = declarationMembers declaration }

    let private dumpToken (token: Token) =
        { Kind = tokenKindText token.Kind
          Text = token.Text }

    let private dumpProvenance (provenance: KCoreOrigin) =
        { FilePath = provenance.FilePath
          ModuleName = provenance.ModuleName
          DeclarationName = provenance.DeclarationName
          IntroductionKind = provenance.IntroductionKind }

    let private dumpSourceDocument (document: ParsedDocument) =
        { FilePath = document.Source.FilePath
          ModuleIdentity = document.ModuleName |> Option.map SyntaxFacts.moduleNameToText
          Text = document.Source.Content
          LineCount = document.Source.LineCount }

    let private dumpOwnership (facts: OwnershipFactSet) =
        { Bindings =
            facts.OwnershipBindings
            |> List.map (fun binding ->
                { Id = binding.BindingId
                  Name = binding.BindingName
                  Kind = binding.BindingKind
                  DeclaredQuantity = binding.BindingDeclaredQuantity
                  InferredDemand = binding.BindingInferredDemand
                  State = binding.BindingState
                  PlaceRoot = binding.BindingPlaceRoot
                  PlacePath = binding.BindingPlacePath
                  BorrowRegionId = binding.BindingBorrowRegionId })
          Uses =
            facts.OwnershipUses
            |> List.map (fun useFact ->
                { Id = useFact.UseId
                  UseKind = useFact.UseKindName
                  TargetBindingId = useFact.UseTargetBindingId
                  TargetName = useFact.UseTargetName
                  PlaceRoot = useFact.UsePlaceRoot
                  PlacePath = useFact.UsePlacePath })
          BorrowRegions =
            facts.OwnershipBorrowRegions
            |> List.map (fun region ->
                { Id = region.BorrowRegionId
                  ExplicitName = region.BorrowRegionExplicitName
                  OwnerScope = region.BorrowRegionOwnerScope })
          UsingScopes =
            facts.OwnershipUsingScopes
            |> List.map (fun usingScope ->
                { Id = usingScope.UsingScopeId
                  HiddenOwnedBinding = usingScope.UsingScopeHiddenOwnedBinding
                  SharedRegionId = usingScope.UsingScopeSharedRegionId
                  HiddenReleaseObligation = usingScope.UsingScopeHiddenReleaseObligation })
          Closures =
            facts.OwnershipClosures
            |> List.map (fun closure ->
                { Id = closure.ClosureId
                  Name = closure.ClosureName
                  CaptureBindingIds = closure.ClosureCaptureBindingIds
                  CaptureNames = closure.ClosureCaptureNames
                  RegionEnvironment = closure.ClosureRegionEnvironment
                  EscapeStatus = closure.ClosureEscapeStatus })
          Deferred = facts.OwnershipDeferred
          Diagnostics = facts.OwnershipDiagnostics }

    let private dumpFrontendDocument (document: KFrontIRModule) =
        { FilePath = document.FilePath
          ModuleHeader = document.ModuleHeader |> Option.map SyntaxFacts.moduleNameToText
          InferredModuleName = document.InferredModuleName |> Option.map SyntaxFacts.moduleNameToText
          ModuleIdentity = document.ModuleIdentity |> Option.map SyntaxFacts.moduleNameToText
          ResolvedPhases = document.ResolvedPhases |> Set.toList |> List.map KFrontIRPhase.phaseName
          ModuleAttributes = document.ModuleAttributes
          Imports = document.Imports |> List.map importSpecText
          Tokens = document.Tokens |> List.map dumpToken
          Declarations = document.Declarations |> List.map dumpDeclaration
          Diagnostics = document.Diagnostics |> List.map dumpDiagnostic
          Ownership = document.Ownership |> Option.map dumpOwnership }

    let private dumpKCoreDeclaration (declaration: KCoreDeclaration) =
        match declaration.Binding, declaration.Source with
        | Some binding, LetDeclaration _ ->
            let visibility = binding.Visibility |> visibilityText
            let visibilityPrefix = defaultArg visibility ""
            let opaquePrefix = if binding.IsOpaque then "opaque " else ""
            let bindingName = defaultArg binding.Name "<pattern>"

            let parameterText =
                binding.Parameters
                |> List.map (fun parameter -> parameter.Name)
                |> String.concat " "

            let signatureText =
                if String.IsNullOrWhiteSpace(parameterText) then
                    bindingName
                else
                    $"{bindingName} {parameterText}"

            let bodyText =
                match binding.BodyText with
                | Some text when not (String.IsNullOrWhiteSpace text) ->
                    Some text
                | _ ->
                    binding.Body |> Option.map IrText.kcoreExpressionText

            let summaryBodyText = bodyText |> Option.defaultValue "<missing>"

            { Kind = declarationKindText declaration.Source
              Name = binding.Name
              Visibility = visibility
              IsOpaque = binding.IsOpaque
              Summary = $"{visibilityPrefix} {opaquePrefix}let {signatureText} = {summaryBodyText}".Trim()
              TypeText = binding.ReturnTypeText
              BodyText = bodyText
              Constructors = []
              Members = [] }
        | _ ->
            dumpDeclaration declaration.Source

    let private dumpCoreModule (moduleDump: KCoreModule) =
        { Name = moduleDump.Name
          SourceFile = moduleDump.SourceFile
          Imports = moduleDump.Imports |> List.map importSpecText
          Ownership = moduleDump.Ownership |> Option.map dumpOwnership
          Declarations = moduleDump.Declarations |> List.map dumpKCoreDeclaration }

    let private dumpRuntimeModule (moduleDump: KRuntimeModule) =
        let constructors =
            moduleDump.Constructors
            |> List.map (fun constructor ->
                { Name = constructor.Name
                  Arity = constructor.Arity
                  TypeName = constructor.TypeName })

        let bindings =
            moduleDump.Bindings
            |> List.map (fun binding ->
                { Name = binding.Name
                  Parameters = binding.Parameters |> List.map (fun parameter -> parameter.Name)
                  Body =
                    binding.Body
                    |> Option.map IrText.runtimeExpressionText
                    |> Option.defaultValue "<intrinsic>"
                  Intrinsic = binding.Intrinsic })

        { Name = moduleDump.Name
          SourceFile = moduleDump.SourceFile
          Exports = moduleDump.Exports
          IntrinsicTerms = moduleDump.IntrinsicTerms
          Constructors = constructors
          Bindings = bindings }

    let private backendModuleId (moduleDump: KBackendModule) =
        $"backend-module:{moduleDump.Name}"

    let private backendFunctionId (moduleDump: KBackendModule) (binding: KBackendFunction) =
        $"{backendModuleId moduleDump}/function:{binding.Name}"

    let private backendEnvironmentLayoutId (moduleDump: KBackendModule) (layout: KBackendEnvironmentLayout) =
        $"{backendModuleId moduleDump}/environment:{layout.Name}"

    let private backendDataLayoutId (moduleDump: KBackendModule) (layout: KBackendDataLayout) =
        $"{backendModuleId moduleDump}/data:{layout.TypeName}"

    let private backendConstructorLayoutId
        (moduleDump: KBackendModule)
        (layout: KBackendDataLayout)
        (constructor: KBackendConstructorLayout)
        =
        $"{backendDataLayoutId moduleDump layout}/constructor:{constructor.Name}"

    let private backendResolvedNameRepresentation =
        function
        | BackendLocalName(_, representation)
        | BackendGlobalBindingName(_, _, representation)
        | BackendIntrinsicName(_, _, representation) ->
            representation |> Option.map IrText.backendRepresentationText
        | BackendConstructorName(_, _, _, _, _, representation) ->
            Some(IrText.backendRepresentationText representation)

    let private buildBackendBodyGraph (functionId: string) (provenance: DumpProvenance) (body: KBackendExpression option) =
        match body with
        | None ->
            None
        | Some rootExpression ->
            let nodes = ResizeArray<DumpBackendNode>()
            let edges = ResizeArray<DumpBackendEdge>()
            let mutable nextNodeId = 0
            let mutable nextEdgeId = 0

            let addNode kind summary representation =
                let nodeId = $"{functionId}/node:{nextNodeId}"
                nextNodeId <- nextNodeId + 1

                nodes.Add(
                    { Id = nodeId
                      Kind = kind
                      Summary = summary
                      Representation = representation
                      Provenance = Some provenance }
                )

                nodeId

            let addEdge sourceId targetId role =
                let edgeId = $"{functionId}/edge:{nextEdgeId}"
                nextEdgeId <- nextEdgeId + 1

                edges.Add(
                    { Id = edgeId
                      SourceId = sourceId
                      TargetId = targetId
                      Role = role }
                )

            let rec buildExpression expression =
                match expression with
                | BackendLiteral(_, representation) ->
                    addNode "literal" (IrText.backendExpressionText expression) (Some(IrText.backendRepresentationText representation))
                | BackendName resolvedName ->
                    addNode "name" (IrText.backendExpressionText expression) (backendResolvedNameRepresentation resolvedName)
                | BackendClosure(parameters, captures, environmentLayout, body, convention, representation) ->
                    let summary =
                        let parameterNames = parameters |> List.map (fun parameter -> parameter.Name) |> String.concat ", "
                        let captureNames = captures |> List.map (fun capture -> capture.Name) |> String.concat ", "
                        $"closure params=[{parameterNames}] captures=[{captureNames}] env={environmentLayout} {IrText.backendCallingConventionText convention}"

                    let nodeId =
                        addNode "closure" summary (Some(IrText.backendRepresentationText representation))

                    let bodyId = buildExpression body
                    addEdge nodeId bodyId "body"
                    nodeId
                | BackendIfThenElse(condition, whenTrue, whenFalse, resultRepresentation) ->
                    let nodeId =
                        addNode
                            "if"
                            (IrText.backendExpressionText expression)
                            (Some(IrText.backendRepresentationText resultRepresentation))

                    let conditionId = buildExpression condition
                    let whenTrueId = buildExpression whenTrue
                    let whenFalseId = buildExpression whenFalse
                    addEdge nodeId conditionId "condition"
                    addEdge nodeId whenTrueId "then"
                    addEdge nodeId whenFalseId "else"
                    nodeId
                | BackendMatch(scrutinee, cases, resultRepresentation) ->
                    let nodeId =
                        addNode
                            "match"
                            (IrText.backendExpressionText expression)
                            (Some(IrText.backendRepresentationText resultRepresentation))

                    let scrutineeId = buildExpression scrutinee
                    addEdge nodeId scrutineeId "scrutinee"

                    for index, caseClause in cases |> List.indexed do
                        let caseId =
                            addNode "match-case" (IrText.backendPatternText caseClause.Pattern) None

                        match caseClause.Guard with
                        | Some guard ->
                            let guardId = buildExpression guard
                            addEdge caseId guardId "guard"
                        | None ->
                            ()

                        let bodyId = buildExpression caseClause.Body
                        addEdge nodeId caseId $"case:{index}"
                        addEdge caseId bodyId "body"

                    nodeId
                | BackendExecute(inner, resultRepresentation) ->
                    let nodeId =
                        addNode
                            "execute"
                            (IrText.backendExpressionText expression)
                            (Some(IrText.backendRepresentationText resultRepresentation))

                    let innerId = buildExpression inner
                    addEdge nodeId innerId "expression"
                    nodeId
                | BackendLet(binding, value, innerBody, resultRepresentation) ->
                    let summary =
                        $"let {binding.Name}:{IrText.backendRepresentationText binding.Representation}"

                    let nodeId =
                        addNode
                            "let"
                            summary
                            (Some(IrText.backendRepresentationText resultRepresentation))

                    let valueId = buildExpression value
                    let bodyId = buildExpression innerBody
                    addEdge nodeId valueId "value"
                    addEdge nodeId bodyId "body"
                    nodeId
                | BackendSequence(first, second, resultRepresentation) ->
                    let nodeId =
                        addNode
                            "sequence"
                            (IrText.backendExpressionText expression)
                            (Some(IrText.backendRepresentationText resultRepresentation))

                    let firstId = buildExpression first
                    let secondId = buildExpression second
                    addEdge nodeId firstId "first"
                    addEdge nodeId secondId "second"
                    nodeId
                | BackendWhile(condition, innerBody) ->
                    let nodeId = addNode "while" (IrText.backendExpressionText expression) None
                    let conditionId = buildExpression condition
                    let bodyId = buildExpression innerBody
                    addEdge nodeId conditionId "condition"
                    addEdge nodeId bodyId "body"
                    nodeId
                | BackendCall(callee, arguments, convention, resultRepresentation) ->
                    let nodeId =
                        addNode
                            "call"
                            (IrText.backendCallingConventionText convention)
                            (Some(IrText.backendRepresentationText resultRepresentation))

                    let calleeId = buildExpression callee
                    addEdge nodeId calleeId "callee"

                    for index, argument in arguments |> List.indexed do
                        let argumentId = buildExpression argument
                        addEdge nodeId argumentId $"argument:{index}"

                    nodeId
                | BackendDictionaryValue(moduleName, traitName, instanceKey, representation) ->
                    addNode
                        "dictionary"
                        $"dictionary {moduleName}.{traitName}.{instanceKey}"
                        (Some(IrText.backendRepresentationText representation))
                | BackendTraitCall(traitName, memberName, dictionary, arguments, resultRepresentation) ->
                    let nodeId =
                        addNode
                            "trait-call"
                            $"{traitName}.{memberName}"
                            (Some(IrText.backendRepresentationText resultRepresentation))

                    let dictionaryId = buildExpression dictionary
                    addEdge nodeId dictionaryId "dictionary"

                    for index, argument in arguments |> List.indexed do
                        let argumentId = buildExpression argument
                        addEdge nodeId argumentId $"argument:{index}"

                    nodeId
                | BackendConstructData(moduleName, typeName, constructorName, tag, fields, representation) ->
                    let nodeId =
                        addNode
                            "construct-data"
                            $"{moduleName}.{typeName}.{constructorName}@{tag}"
                            (Some(IrText.backendRepresentationText representation))

                    for index, field in fields |> List.indexed do
                        let fieldId = buildExpression field
                        addEdge nodeId fieldId $"field:{index}"

                    nodeId
                | BackendPrefixedString(prefix, parts, resultRepresentation) ->
                    let nodeId =
                        addNode
                            "prefixed-string"
                            $"{prefix}:{IrText.backendExpressionText expression}"
                            (Some(IrText.backendRepresentationText resultRepresentation))

                    for index, part in parts |> List.indexed do
                        match part with
                        | BackendStringText text ->
                            let partId = addNode "string-text" text (Some(IrText.backendRepresentationText resultRepresentation))
                            addEdge nodeId partId $"part:{index}"
                        | BackendStringInterpolation inner ->
                            let partId = addNode "string-interpolation" (IrText.backendExpressionText inner) None
                            let innerId = buildExpression inner
                            addEdge nodeId partId $"part:{index}"
                            addEdge partId innerId "expression"

                    nodeId

            let rootNodeId = buildExpression rootExpression

            Some
                { RootNodeId = Some rootNodeId
                  Nodes = List.ofSeq nodes
                  Edges = List.ofSeq edges }

    let private dumpBackendModule (moduleDump: KBackendModule) =
        let functions =
            moduleDump.Functions
            |> List.map (fun binding ->
                let functionId = backendFunctionId moduleDump binding
                let provenance = dumpProvenance binding.Provenance

                { Id = functionId
                  Name = binding.Name
                  Parameters =
                    binding.Parameters
                    |> List.map (fun parameter ->
                        { Name = parameter.Name
                          Representation = IrText.backendRepresentationText parameter.Representation })
                  CallingConvention = IrText.backendCallingConventionText binding.CallingConvention
                  ReturnRepresentation =
                    binding.ReturnRepresentation
                    |> Option.map IrText.backendRepresentationText
                  EnvironmentLayout = binding.EnvironmentLayout
                  Intrinsic = binding.Intrinsic
                  Exported = binding.Exported
                  EntryPoint = binding.EntryPoint
                  ControlForm =
                    match binding.ControlForm with
                    | StructuredExpression -> "structured-expression"
                  Body =
                    binding.Body
                    |> Option.map IrText.backendExpressionText
                    |> Option.defaultValue "<intrinsic>"
                  BodyGraph = buildBackendBodyGraph functionId provenance binding.Body
                  Provenance = provenance })

        let dataLayouts =
            moduleDump.DataLayouts
            |> List.map (fun layout ->
                { Id = backendDataLayoutId moduleDump layout
                  TypeName = layout.TypeName
                  RepresentationClass = layout.RepresentationClass
                  TagEncoding = layout.TagEncoding
                  Constructors =
                    layout.Constructors
                    |> List.map (fun constructor ->
                        { Id = backendConstructorLayoutId moduleDump layout constructor
                          Name = constructor.Name
                          Tag = constructor.Tag
                          FieldRepresentations =
                            constructor.FieldRepresentations
                            |> List.map IrText.backendRepresentationText
                          Provenance = dumpProvenance constructor.Provenance })
                  Provenance = dumpProvenance layout.Provenance })

        let environmentLayouts =
            moduleDump.EnvironmentLayouts
            |> List.map (fun layout ->
                { Id = backendEnvironmentLayoutId moduleDump layout
                  Name = layout.Name
                  Slots =
                    layout.Slots
                    |> List.map (fun slot ->
                        { Name = slot.Name
                          Representation = IrText.backendRepresentationText slot.Representation }) })

        let functionIdByName =
            functions
            |> List.map (fun binding -> binding.Name, binding.Id)
            |> Map.ofList

        { Id = backendModuleId moduleDump
          Name = moduleDump.Name
          SourceFile = moduleDump.SourceFile
          Imports = moduleDump.Imports |> List.map importSpecText
          Exports = moduleDump.Exports
          EntryPoints = moduleDump.EntryPoints
          EntryPointIds =
            moduleDump.EntryPoints
            |> List.choose (fun entryPoint -> functionIdByName |> Map.tryFind entryPoint)
          IntrinsicTerms = moduleDump.IntrinsicTerms
          DataLayouts = dataLayouts
          EnvironmentLayouts = environmentLayouts
          Functions = functions }

    let private jsonOptions =
        JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

    let private serializeJson value =
        JsonSerializer.Serialize(value, jsonOptions)

    let private sexprEscape (value: string) =
        value
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\r", "\\r")
            .Replace("\n", "\\n")
            .Replace("\t", "\\t")

    let private sexprString value =
        $"\"{sexprEscape value}\""

    let private sexprAtom name value =
        $"({name} {value})"

    let private sexprStringAtom name value =
        sexprAtom name (sexprString value)

    let private sexprOptionalStringAtom name value =
        match value with
        | Some actual -> sexprStringAtom name actual
        | None -> sexprAtom name "nil"

    let private sexprStringList name values =
        let items =
            values
            |> List.map sexprString
            |> String.concat " "

        if String.IsNullOrWhiteSpace(items) then
            $"({name})"
        else
            $"({name} {items})"

    let private renderDumpRelatedOriginSexpr (related: DumpRelatedOrigin) =
        [
            sexprStringAtom "message" related.Message
            sexprStringAtom "file" related.FilePath
            sexprAtom "start-line" (string related.StartLine)
            sexprAtom "start-column" (string related.StartColumn)
            sexprAtom "end-line" (string related.EndLine)
            sexprAtom "end-column" (string related.EndColumn)
        ]
        |> String.concat " "
        |> fun body -> $"(related-origin {body})"

    let private renderDumpDiagnosticSexpr (diagnostic: DumpDiagnostic) =
        let relatedOrigins =
            diagnostic.RelatedOrigins
            |> List.map renderDumpRelatedOriginSexpr
            |> String.concat " "

        [
            sexprStringAtom "code" diagnostic.Code
            sexprOptionalStringAtom "stage" diagnostic.Stage
            sexprOptionalStringAtom "phase" diagnostic.Phase
            sexprStringAtom "severity" diagnostic.Severity
            sexprStringAtom "message" diagnostic.Message
            sexprOptionalStringAtom "file" diagnostic.FilePath
            diagnostic.StartLine |> Option.map (fun value -> sexprAtom "start-line" (string value)) |> Option.defaultValue (sexprAtom "start-line" "nil")
            diagnostic.StartColumn |> Option.map (fun value -> sexprAtom "start-column" (string value)) |> Option.defaultValue (sexprAtom "start-column" "nil")
            diagnostic.EndLine |> Option.map (fun value -> sexprAtom "end-line" (string value)) |> Option.defaultValue (sexprAtom "end-line" "nil")
            diagnostic.EndColumn |> Option.map (fun value -> sexprAtom "end-column" (string value)) |> Option.defaultValue (sexprAtom "end-column" "nil")
            if String.IsNullOrWhiteSpace(relatedOrigins) then "(related-origins)" else $"(related-origins {relatedOrigins})"
        ]
        |> String.concat " "
        |> fun body -> $"(diagnostic {body})"

    let private renderDumpDeclarationSexpr (declaration: DumpDeclaration) =
        [
            sexprStringAtom "kind" declaration.Kind
            sexprOptionalStringAtom "name" declaration.Name
            sexprOptionalStringAtom "visibility" declaration.Visibility
            sexprAtom "opaque" (if declaration.IsOpaque then "true" else "false")
            sexprStringAtom "summary" declaration.Summary
            declaration.TypeText |> Option.map (sexprStringAtom "type") |> Option.defaultValue (sexprAtom "type" "nil")
            declaration.BodyText |> Option.map (sexprStringAtom "body") |> Option.defaultValue (sexprAtom "body" "nil")
            sexprStringList "constructors" declaration.Constructors
            sexprStringList "members" declaration.Members
        ]
        |> String.concat " "
        |> fun body -> $"(declaration {body})"

    let private renderDumpTokenSexpr (token: DumpToken) =
        let kindAtom = sexprStringAtom "kind" token.Kind
        let textAtom = sexprStringAtom "text" token.Text
        $"(token {kindAtom} {textAtom})"

    let private renderOwnershipBindingSexpr (binding: DumpOwnershipBinding) =
        [
            sexprStringAtom "id" binding.Id
            sexprStringAtom "name" binding.Name
            sexprStringAtom "kind" binding.Kind
            sexprOptionalStringAtom "declared-quantity" binding.DeclaredQuantity
            sexprStringAtom "inferred-demand" binding.InferredDemand
            sexprStringAtom "state" binding.State
            sexprStringAtom "place-root" binding.PlaceRoot
            sexprStringList "place-path" binding.PlacePath
            sexprOptionalStringAtom "borrow-region-id" binding.BorrowRegionId
        ]
        |> String.concat " "
        |> fun body -> $"(binding {body})"

    let private renderOwnershipUseSexpr (useFact: DumpOwnershipUse) =
        [
            sexprStringAtom "id" useFact.Id
            sexprStringAtom "use-kind" useFact.UseKind
            sexprOptionalStringAtom "target-binding-id" useFact.TargetBindingId
            sexprStringAtom "target-name" useFact.TargetName
            sexprStringAtom "place-root" useFact.PlaceRoot
            sexprStringList "place-path" useFact.PlacePath
        ]
        |> String.concat " "
        |> fun body -> $"(use {body})"

    let private renderOwnershipBorrowRegionSexpr (region: DumpOwnershipBorrowRegion) =
        [
            sexprStringAtom "id" region.Id
            sexprOptionalStringAtom "explicit-name" region.ExplicitName
            sexprStringAtom "owner-scope" region.OwnerScope
        ]
        |> String.concat " "
        |> fun body -> $"(borrow-region {body})"

    let private renderOwnershipUsingScopeSexpr (usingScope: DumpOwnershipUsingScope) =
        [
            sexprStringAtom "id" usingScope.Id
            sexprStringAtom "hidden-owned-binding" usingScope.HiddenOwnedBinding
            sexprStringAtom "shared-region-id" usingScope.SharedRegionId
            sexprStringAtom "hidden-release-obligation" usingScope.HiddenReleaseObligation
        ]
        |> String.concat " "
        |> fun body -> $"(using-scope {body})"

    let private renderOwnershipClosureSexpr (closure: DumpOwnershipClosure) =
        [
            sexprStringAtom "id" closure.Id
            sexprOptionalStringAtom "name" closure.Name
            sexprStringList "capture-binding-ids" closure.CaptureBindingIds
            sexprStringList "capture-names" closure.CaptureNames
            sexprStringList "region-environment" closure.RegionEnvironment
            sexprStringAtom "escape-status" closure.EscapeStatus
        ]
        |> String.concat " "
        |> fun body -> $"(closure {body})"

    let private renderOwnershipSexpr (ownership: DumpOwnershipFacts option) =
        match ownership with
        | None ->
            "(ownership (status \"unknown\"))"
        | Some facts ->
            let bindings =
                facts.Bindings
                |> List.map renderOwnershipBindingSexpr
                |> String.concat " "

            let uses =
                facts.Uses
                |> List.map renderOwnershipUseSexpr
                |> String.concat " "

            let borrowRegions =
                facts.BorrowRegions
                |> List.map renderOwnershipBorrowRegionSexpr
                |> String.concat " "

            let usingScopes =
                facts.UsingScopes
                |> List.map renderOwnershipUsingScopeSexpr
                |> String.concat " "

            let closures =
                facts.Closures
                |> List.map renderOwnershipClosureSexpr
                |> String.concat " "

            [
                if String.IsNullOrWhiteSpace(bindings) then "(bindings)" else $"(bindings {bindings})"
                if String.IsNullOrWhiteSpace(uses) then "(uses)" else $"(uses {uses})"
                if String.IsNullOrWhiteSpace(borrowRegions) then "(borrow-regions)" else $"(borrow-regions {borrowRegions})"
                if String.IsNullOrWhiteSpace(usingScopes) then "(using-scopes)" else $"(using-scopes {usingScopes})"
                if String.IsNullOrWhiteSpace(closures) then "(closures)" else $"(closures {closures})"
                sexprStringList "deferred" facts.Deferred
                sexprStringList "diagnostics" facts.Diagnostics
            ]
            |> String.concat " "
            |> fun body -> $"(ownership {body})"

    let private renderDumpDocumentSexpr (document: DumpDocument) =
        [
            sexprStringAtom "file" document.FilePath
            sexprOptionalStringAtom "module-header" document.ModuleHeader
            sexprOptionalStringAtom "inferred-module-name" document.InferredModuleName
            sexprOptionalStringAtom "module-identity" document.ModuleIdentity
            sexprStringList "resolved-phases" document.ResolvedPhases
            sexprStringList "module-attributes" document.ModuleAttributes
            sexprStringList "imports" document.Imports
            let tokenBody =
                document.Tokens
                |> List.map renderDumpTokenSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(tokenBody) then "(tokens)" else $"(tokens {tokenBody})"
            let declarationBody =
                document.Declarations
                |> List.map renderDumpDeclarationSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(declarationBody) then "(declarations)" else $"(declarations {declarationBody})"
            let diagnosticsBody =
                document.Diagnostics
                |> List.map renderDumpDiagnosticSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(diagnosticsBody) then "(diagnostics)" else $"(diagnostics {diagnosticsBody})"
            renderOwnershipSexpr document.Ownership
        ]
        |> String.concat " "
        |> fun body -> $"(document {body})"

    let private renderDumpSourceDocumentSexpr (document: DumpSourceDocument) =
        [
            sexprStringAtom "file" document.FilePath
            sexprOptionalStringAtom "module-identity" document.ModuleIdentity
            sexprAtom "line-count" (string document.LineCount)
            sexprStringAtom "text" document.Text
        ]
        |> String.concat " "
        |> fun body -> $"(document {body})"

    let private renderDumpCoreModuleSexpr (moduleDump: DumpCoreModule) =
        [
            sexprStringAtom "name" moduleDump.Name
            sexprStringAtom "source-file" moduleDump.SourceFile
            sexprStringList "imports" moduleDump.Imports
            renderOwnershipSexpr moduleDump.Ownership
            let declarationBody =
                moduleDump.Declarations
                |> List.map renderDumpDeclarationSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(declarationBody) then "(declarations)" else $"(declarations {declarationBody})"
        ]
        |> String.concat " "
        |> fun body -> $"(module {body})"

    let private renderDumpRuntimeBindingSexpr (binding: DumpRuntimeBinding) =
        [
            sexprStringAtom "name" binding.Name
            sexprStringList "parameters" binding.Parameters
            sexprStringAtom "body" binding.Body
            sexprAtom "intrinsic" (if binding.Intrinsic then "true" else "false")
        ]
        |> String.concat " "
        |> fun body -> $"(binding {body})"

    let private renderDumpRuntimeConstructorSexpr (constructor: DumpRuntimeConstructor) =
        [
            sexprStringAtom "name" constructor.Name
            sexprAtom "arity" (string constructor.Arity)
            sexprStringAtom "type-name" constructor.TypeName
        ]
        |> String.concat " "
        |> fun body -> $"(constructor {body})"

    let private renderDumpRuntimeModuleSexpr (moduleDump: DumpRuntimeModule) =
        [
            sexprStringAtom "name" moduleDump.Name
            sexprStringAtom "source-file" moduleDump.SourceFile
            sexprStringList "exports" moduleDump.Exports
            sexprStringList "intrinsic-terms" moduleDump.IntrinsicTerms
            let constructorBody =
                moduleDump.Constructors
                |> List.map renderDumpRuntimeConstructorSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(constructorBody) then "(constructors)" else $"(constructors {constructorBody})"
            let bindingBody =
                moduleDump.Bindings
                |> List.map renderDumpRuntimeBindingSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(bindingBody) then "(bindings)" else $"(bindings {bindingBody})"
        ]
        |> String.concat " "
        |> fun body -> $"(module {body})"

    let private renderDumpBackendParameterSexpr (parameter: DumpBackendParameter) =
        [
            sexprStringAtom "name" parameter.Name
            sexprStringAtom "representation" parameter.Representation
        ]
        |> String.concat " "
        |> fun body -> $"(parameter {body})"

    let private renderDumpBackendCaptureSexpr (capture: DumpBackendCapture) =
        [
            sexprStringAtom "name" capture.Name
            sexprStringAtom "representation" capture.Representation
        ]
        |> String.concat " "
        |> fun body -> $"(capture {body})"

    let private renderDumpProvenanceSexpr (provenance: DumpProvenance) =
        [
            sexprStringAtom "file" provenance.FilePath
            sexprStringAtom "module-name" provenance.ModuleName
            sexprOptionalStringAtom "declaration-name" provenance.DeclarationName
            sexprStringAtom "introduction-kind" provenance.IntroductionKind
        ]
        |> String.concat " "
        |> fun body -> $"(provenance {body})"

    let private renderDumpBackendNodeSexpr (node: DumpBackendNode) =
        [
            sexprStringAtom "id" node.Id
            sexprStringAtom "kind" node.Kind
            sexprStringAtom "summary" node.Summary
            node.Representation
            |> Option.map (sexprStringAtom "representation")
            |> Option.defaultValue (sexprAtom "representation" "nil")
            node.Provenance
            |> Option.map renderDumpProvenanceSexpr
            |> Option.defaultValue "(provenance nil)"
        ]
        |> String.concat " "
        |> fun body -> $"(node {body})"

    let private renderDumpBackendEdgeSexpr (edge: DumpBackendEdge) =
        [
            sexprStringAtom "id" edge.Id
            sexprStringAtom "source-id" edge.SourceId
            sexprStringAtom "target-id" edge.TargetId
            sexprStringAtom "role" edge.Role
        ]
        |> String.concat " "
        |> fun body -> $"(edge {body})"

    let private renderDumpBackendGraphSexpr (graph: DumpBackendGraph option) =
        match graph with
        | None ->
            "(body-graph (root-node-id nil) (nodes) (edges))"
        | Some graph ->
            let nodeBody =
                graph.Nodes
                |> List.map renderDumpBackendNodeSexpr
                |> String.concat " "

            let edgeBody =
                graph.Edges
                |> List.map renderDumpBackendEdgeSexpr
                |> String.concat " "

            [
                graph.RootNodeId
                |> Option.map (sexprStringAtom "root-node-id")
                |> Option.defaultValue (sexprAtom "root-node-id" "nil")
                if String.IsNullOrWhiteSpace(nodeBody) then "(nodes)" else $"(nodes {nodeBody})"
                if String.IsNullOrWhiteSpace(edgeBody) then "(edges)" else $"(edges {edgeBody})"
            ]
            |> String.concat " "
            |> fun body -> $"(body-graph {body})"

    let private renderDumpBackendFunctionSexpr (binding: DumpBackendFunction) =
        [
            sexprStringAtom "name" binding.Name
            sexprStringAtom "id" binding.Id
            let parameterBody =
                binding.Parameters
                |> List.map renderDumpBackendParameterSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(parameterBody) then "(parameters)" else $"(parameters {parameterBody})"
            sexprStringAtom "calling-convention" binding.CallingConvention
            binding.ReturnRepresentation
            |> Option.map (sexprStringAtom "return-representation")
            |> Option.defaultValue (sexprAtom "return-representation" "nil")
            binding.EnvironmentLayout
            |> Option.map (sexprStringAtom "environment-layout")
            |> Option.defaultValue (sexprAtom "environment-layout" "nil")
            sexprAtom "intrinsic" (if binding.Intrinsic then "true" else "false")
            sexprAtom "exported" (if binding.Exported then "true" else "false")
            sexprAtom "entry-point" (if binding.EntryPoint then "true" else "false")
            sexprStringAtom "control-form" binding.ControlForm
            sexprStringAtom "body" binding.Body
            renderDumpBackendGraphSexpr binding.BodyGraph
            renderDumpProvenanceSexpr binding.Provenance
        ]
        |> String.concat " "
        |> fun body -> $"(function {body})"

    let private renderDumpBackendConstructorLayoutSexpr (constructor: DumpBackendConstructorLayout) =
        [
            sexprStringAtom "id" constructor.Id
            sexprStringAtom "name" constructor.Name
            sexprAtom "tag" (string constructor.Tag)
            sexprStringList "field-representations" constructor.FieldRepresentations
            renderDumpProvenanceSexpr constructor.Provenance
        ]
        |> String.concat " "
        |> fun body -> $"(constructor {body})"

    let private renderDumpBackendDataLayoutSexpr (layout: DumpBackendDataLayout) =
        [
            sexprStringAtom "id" layout.Id
            sexprStringAtom "type-name" layout.TypeName
            sexprStringAtom "representation-class" layout.RepresentationClass
            sexprStringAtom "tag-encoding" layout.TagEncoding
            let constructorBody =
                layout.Constructors
                |> List.map renderDumpBackendConstructorLayoutSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(constructorBody) then "(constructors)" else $"(constructors {constructorBody})"
            renderDumpProvenanceSexpr layout.Provenance
        ]
        |> String.concat " "
        |> fun body -> $"(data-layout {body})"

    let private renderDumpBackendEnvironmentLayoutSexpr (layout: DumpBackendEnvironmentLayout) =
        [
            sexprStringAtom "id" layout.Id
            sexprStringAtom "name" layout.Name
            let slotBody =
                layout.Slots
                |> List.map renderDumpBackendCaptureSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(slotBody) then "(slots)" else $"(slots {slotBody})"
        ]
        |> String.concat " "
        |> fun body -> $"(environment-layout {body})"

    let private renderDumpBackendModuleSexpr (moduleDump: DumpBackendModule) =
        [
            sexprStringAtom "name" moduleDump.Name
            sexprStringAtom "id" moduleDump.Id
            sexprStringAtom "source-file" moduleDump.SourceFile
            sexprStringList "imports" moduleDump.Imports
            sexprStringList "exports" moduleDump.Exports
            sexprStringList "entry-points" moduleDump.EntryPoints
            sexprStringList "entry-point-ids" moduleDump.EntryPointIds
            sexprStringList "intrinsic-terms" moduleDump.IntrinsicTerms
            let dataLayoutBody =
                moduleDump.DataLayouts
                |> List.map renderDumpBackendDataLayoutSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(dataLayoutBody) then "(data-layouts)" else $"(data-layouts {dataLayoutBody})"
            let environmentLayoutBody =
                moduleDump.EnvironmentLayouts
                |> List.map renderDumpBackendEnvironmentLayoutSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(environmentLayoutBody) then "(environment-layouts)" else $"(environment-layouts {environmentLayoutBody})"
            let functionBody =
                moduleDump.Functions
                |> List.map renderDumpBackendFunctionSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(functionBody) then "(functions)" else $"(functions {functionBody})"
        ]
        |> String.concat " "
        |> fun body -> $"(module {body})"

    let private buildConfigurationJson workspace =
        {| identity = workspace.BuildConfigurationIdentity
           packageMode = workspace.PackageMode
           backendProfile = workspace.BackendProfile
           deploymentMode = workspace.DeploymentMode
           backendIntrinsicSet = workspace.BackendIntrinsicIdentity
           elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms |}

    let private dumpFormatText =
        function
        | StageDumpFormat.Json -> "json"
        | StageDumpFormat.SExpression -> "sexpr"

    let private frontendSnapshot workspace phase =
        workspace.FrontendSnapshots
        |> Map.tryFind phase
        |> Option.defaultValue
            { Modules = workspace.KFrontIR
              Diagnostics = workspace.Diagnostics }

    let private renderCheckpointContractSexpr workspace checkpoint =
        let contract =
            CompilationCheckpoints.checkpointContractFor workspace checkpoint
            |> Option.defaultValue
                { Name = checkpoint
                  CheckpointKind = ImplementationDefinedCheckpoint
                  InputCheckpoint = None
                  RequiredBySpec = false
                  ProfileSpecific = false }

        let inputAtom =
            match contract.InputCheckpoint with
            | Some inputCheckpoint -> sexprStringAtom "input-checkpoint" inputCheckpoint
            | None -> sexprAtom "input-checkpoint" "none"

        [
            sexprStringAtom "name" contract.Name
            sexprStringAtom "kind" (CheckpointKind.toPortableName contract.CheckpointKind)
            inputAtom
            sexprAtom "required-by-spec" (if contract.RequiredBySpec then "true" else "false")
            sexprAtom "profile-specific" (if contract.ProfileSpecific then "true" else "false")
        ]
        |> String.concat " "
        |> fun body -> $"(checkpoint-contract {body})"

    let private metadataJson workspace checkpoint format =
        {| schemaVersion = "1"
           dumpFormat = dumpFormatText format
           languageVersion = languageVersion
           compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
           checkpoint = checkpoint
           compilationRoot = workspace.SourceRoot
           backendProfile = workspace.BackendProfile
           backendIntrinsicSet = workspace.BackendIntrinsicIdentity
           elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
           buildConfiguration = buildConfigurationJson workspace
           checkpointContract = CompilationCheckpoints.checkpointContractJson workspace checkpoint |}

    let private metadataSexpr workspace checkpoint format =
        let compilerAtom =
            let idAtom = sexprStringAtom "id" compilerImplementationId
            let versionAtom = sexprStringAtom "version" compilerImplementationVersion
            $"(compiler {idAtom} {versionAtom})"

        let buildConfigurationAtom =
            let identityAtom = sexprStringAtom "identity" workspace.BuildConfigurationIdentity
            let packageModeAtom = sexprAtom "package-mode" (if workspace.PackageMode then "true" else "false")
            let backendProfileAtom = sexprStringAtom "backend-profile" workspace.BackendProfile
            let deploymentModeAtom = sexprStringAtom "deployment-mode" workspace.DeploymentMode
            let backendIntrinsicSetAtom = sexprStringAtom "backend-intrinsic-set" workspace.BackendIntrinsicIdentity
            let elaborationAvailableAtom =
                sexprStringList "elaboration-available-intrinsic-terms" workspace.ElaborationAvailableIntrinsicTerms

            $"(build-configuration {identityAtom} {packageModeAtom} {backendProfileAtom} {deploymentModeAtom} {backendIntrinsicSetAtom} {elaborationAvailableAtom})"

        [
            sexprStringAtom "schema-version" "1"
            sexprStringAtom "dump-format" (dumpFormatText format)
            sexprStringAtom "language-version" languageVersion
            compilerAtom
            sexprStringAtom "checkpoint" checkpoint
            sexprStringAtom "compilation-root" workspace.SourceRoot
            sexprStringAtom "backend-profile" workspace.BackendProfile
            sexprStringAtom "backend-intrinsic-set" workspace.BackendIntrinsicIdentity
            sexprStringList "elaboration-available-intrinsic-terms" workspace.ElaborationAvailableIntrinsicTerms
            buildConfigurationAtom
            renderCheckpointContractSexpr workspace checkpoint
        ]
        |> String.concat " "

    let renderTargetCheckpointJson
        (workspace: WorkspaceCompilation)
        (checkpoint: string)
        (translationUnit: NativeTranslationUnit)
        =
        serializeJson
            {| schemaVersion = "1"
               dumpFormat = dumpFormatText StageDumpFormat.Json
               languageVersion = languageVersion
               compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
               checkpoint = checkpoint
               compilationRoot = workspace.SourceRoot
               backendProfile = workspace.BackendProfile
               backendIntrinsicSet = workspace.BackendIntrinsicIdentity
               elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
               buildConfiguration = buildConfigurationJson workspace
               checkpointContract = CompilationCheckpoints.checkpointContractJson workspace checkpoint
               artifactKind = translationUnit.ArtifactKind
               inputCheckpoint = translationUnit.InputCheckpoint
               translationUnitName = translationUnit.TranslationUnitName
               entrySymbols = translationUnit.EntrySymbols
               functionSymbols = translationUnit.FunctionSymbols
               sourceText = translationUnit.SourceText
               diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}

    let renderTargetCheckpointSexpr
        (workspace: WorkspaceCompilation)
        (checkpoint: string)
        (translationUnit: NativeTranslationUnit)
        =
        let translationUnitAtom =
            [
                sexprStringAtom "artifact-kind" translationUnit.ArtifactKind
                sexprStringAtom "input-checkpoint" translationUnit.InputCheckpoint
                sexprStringAtom "translation-unit-name" translationUnit.TranslationUnitName
                sexprStringList "entry-symbols" translationUnit.EntrySymbols
                sexprStringList "function-symbols" translationUnit.FunctionSymbols
                sexprStringAtom "source-text" translationUnit.SourceText
            ]
            |> String.concat " "
            |> fun body -> $"(translation-unit {body})"

        let diagnosticsAtom =
            workspace.Diagnostics
            |> List.map dumpDiagnostic
            |> List.map renderDumpDiagnosticSexpr
            |> String.concat " "
            |> fun body -> if String.IsNullOrWhiteSpace(body) then "(diagnostics)" else $"(diagnostics {body})"

        $"(stage-dump {metadataSexpr workspace checkpoint StageDumpFormat.SExpression} {translationUnitAtom} {diagnosticsAtom})"

    let dumpStageJson (workspace: WorkspaceCompilation) checkpoint =
        match checkpoint with
        | "surface-source" ->
            let documents =
                workspace.Documents
                |> List.sortBy (fun document -> document.Source.FilePath)
                |> List.map dumpSourceDocument

            serializeJson
                {| schemaVersion = "1"
                   dumpFormat = dumpFormatText StageDumpFormat.Json
                   languageVersion = languageVersion
                   compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
                   checkpoint = checkpoint
                   compilationRoot = workspace.SourceRoot
                   backendProfile = workspace.BackendProfile
                   backendIntrinsicSet = workspace.BackendIntrinsicIdentity
                   elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
                   buildConfiguration = buildConfigurationJson workspace
                   checkpointContract = CompilationCheckpoints.checkpointContractJson workspace checkpoint
                   documents = documents
                   diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}
        | "KCore" ->
            let modules =
                workspace.KCore
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpCoreModule

            serializeJson
                {| schemaVersion = "1"
                   dumpFormat = dumpFormatText StageDumpFormat.Json
                   languageVersion = languageVersion
                   compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
                   checkpoint = checkpoint
                   compilationRoot = workspace.SourceRoot
                   backendProfile = workspace.BackendProfile
                   backendIntrinsicSet = workspace.BackendIntrinsicIdentity
                   elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
                   buildConfiguration = buildConfigurationJson workspace
                   checkpointContract = CompilationCheckpoints.checkpointContractJson workspace checkpoint
                   modules = modules
                   diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}
        | "KRuntimeIR" ->
            let modules =
                workspace.KRuntimeIR
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpRuntimeModule

            serializeJson
                {| schemaVersion = "1"
                   dumpFormat = dumpFormatText StageDumpFormat.Json
                   languageVersion = languageVersion
                   compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
                   checkpoint = checkpoint
                   compilationRoot = workspace.SourceRoot
                   backendProfile = workspace.BackendProfile
                   backendIntrinsicSet = workspace.BackendIntrinsicIdentity
                   elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
                   buildConfiguration = buildConfigurationJson workspace
                   checkpointContract = CompilationCheckpoints.checkpointContractJson workspace checkpoint
                   modules = modules
                   diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}
        | "KBackendIR" ->
            let modules =
                workspace.KBackendIR
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpBackendModule

            serializeJson
                {| schemaVersion = "1"
                   dumpFormat = dumpFormatText StageDumpFormat.Json
                   languageVersion = languageVersion
                   compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
                   checkpoint = checkpoint
                   compilationRoot = workspace.SourceRoot
                   backendProfile = workspace.BackendProfile
                   backendIntrinsicSet = workspace.BackendIntrinsicIdentity
                   elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
                   buildConfiguration = buildConfigurationJson workspace
                   checkpointContract = CompilationCheckpoints.checkpointContractJson workspace checkpoint
                   modules = modules
                   diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}
        | _ ->
            match CheckpointVerification.tryParseCheckpoint checkpoint with
            | Some(Some phase) ->
                let snapshot = frontendSnapshot workspace phase

                let documents =
                    snapshot.Modules
                    |> List.sortBy (fun document -> document.FilePath)
                    |> List.map dumpFrontendDocument

                serializeJson
                    {| schemaVersion = "1"
                       dumpFormat = dumpFormatText StageDumpFormat.Json
                       languageVersion = languageVersion
                       compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
                       checkpoint = checkpoint
                       phase = KFrontIRPhase.phaseName phase
                       compilationRoot = workspace.SourceRoot
                       backendProfile = workspace.BackendProfile
                       backendIntrinsicSet = workspace.BackendIntrinsicIdentity
                       elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
                       buildConfiguration = buildConfigurationJson workspace
                       checkpointContract = CompilationCheckpoints.checkpointContractJson workspace checkpoint
                       documents = documents
                       diagnostics = snapshot.Diagnostics |> List.map dumpDiagnostic |}
            | _ ->
                invalidOp $"Unknown checkpoint '{checkpoint}'."

    let dumpStageSexpr (workspace: WorkspaceCompilation) checkpoint =
        match checkpoint with
        | "surface-source" ->
            let documents =
                workspace.Documents
                |> List.sortBy (fun document -> document.Source.FilePath)
                |> List.map dumpSourceDocument
                |> List.map renderDumpSourceDocumentSexpr
                |> String.concat " "

            let diagnostics =
                workspace.Diagnostics
                |> List.map dumpDiagnostic
                |> List.map renderDumpDiagnosticSexpr
                |> String.concat " "

            let documentsAtom =
                if String.IsNullOrWhiteSpace(documents) then "(documents)" else $"(documents {documents})"

            let diagnosticsAtom =
                if String.IsNullOrWhiteSpace(diagnostics) then "(diagnostics)" else $"(diagnostics {diagnostics})"

            $"(stage-dump {metadataSexpr workspace checkpoint StageDumpFormat.SExpression} {documentsAtom} {diagnosticsAtom})"
        | "KCore" ->
            let modules =
                workspace.KCore
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpCoreModule
                |> List.map renderDumpCoreModuleSexpr
                |> String.concat " "

            let diagnostics =
                workspace.Diagnostics
                |> List.map dumpDiagnostic
                |> List.map renderDumpDiagnosticSexpr
                |> String.concat " "

            let modulesAtom =
                if String.IsNullOrWhiteSpace(modules) then "(modules)" else $"(modules {modules})"

            let diagnosticsAtom =
                if String.IsNullOrWhiteSpace(diagnostics) then "(diagnostics)" else $"(diagnostics {diagnostics})"

            $"(stage-dump {metadataSexpr workspace checkpoint StageDumpFormat.SExpression} {modulesAtom} {diagnosticsAtom})"
        | "KRuntimeIR" ->
            let modules =
                workspace.KRuntimeIR
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpRuntimeModule
                |> List.map renderDumpRuntimeModuleSexpr
                |> String.concat " "

            let diagnostics =
                workspace.Diagnostics
                |> List.map dumpDiagnostic
                |> List.map renderDumpDiagnosticSexpr
                |> String.concat " "

            let modulesAtom =
                if String.IsNullOrWhiteSpace(modules) then "(modules)" else $"(modules {modules})"

            let diagnosticsAtom =
                if String.IsNullOrWhiteSpace(diagnostics) then "(diagnostics)" else $"(diagnostics {diagnostics})"

            $"(stage-dump {metadataSexpr workspace checkpoint StageDumpFormat.SExpression} {modulesAtom} {diagnosticsAtom})"
        | "KBackendIR" ->
            let modules =
                workspace.KBackendIR
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpBackendModule
                |> List.map renderDumpBackendModuleSexpr
                |> String.concat " "

            let diagnostics =
                workspace.Diagnostics
                |> List.map dumpDiagnostic
                |> List.map renderDumpDiagnosticSexpr
                |> String.concat " "

            let modulesAtom =
                if String.IsNullOrWhiteSpace(modules) then "(modules)" else $"(modules {modules})"

            let diagnosticsAtom =
                if String.IsNullOrWhiteSpace(diagnostics) then "(diagnostics)" else $"(diagnostics {diagnostics})"

            $"(stage-dump {metadataSexpr workspace checkpoint StageDumpFormat.SExpression} {modulesAtom} {diagnosticsAtom})"
        | _ ->
            match CheckpointVerification.tryParseCheckpoint checkpoint with
            | Some(Some phase) ->
                let snapshot = frontendSnapshot workspace phase

                let documents =
                    snapshot.Modules
                    |> List.sortBy (fun document -> document.FilePath)
                    |> List.map dumpFrontendDocument
                    |> List.map renderDumpDocumentSexpr
                    |> String.concat " "

                let diagnostics =
                    snapshot.Diagnostics
                    |> List.map dumpDiagnostic
                    |> List.map renderDumpDiagnosticSexpr
                    |> String.concat " "

                let phaseAtom = sexprStringAtom "phase" (KFrontIRPhase.phaseName phase)

                let documentsAtom =
                    if String.IsNullOrWhiteSpace(documents) then "(documents)" else $"(documents {documents})"

                let diagnosticsAtom =
                    if String.IsNullOrWhiteSpace(diagnostics) then "(diagnostics)" else $"(diagnostics {diagnostics})"

                $"(stage-dump {metadataSexpr workspace checkpoint StageDumpFormat.SExpression} {phaseAtom} {documentsAtom} {diagnosticsAtom})"
            | _ ->
                invalidOp $"Unknown checkpoint '{checkpoint}'."
