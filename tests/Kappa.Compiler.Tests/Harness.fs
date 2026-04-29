// Provides the public test harness facade and assertion interpreter used by compiler tests.
module Harness

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open HarnessFixtureModel
open HarnessFixtures
open HarnessWorkspace
open Kappa.Compiler
open Xunit

let createSource = HarnessWorkspace.createSource
let lexAndParse = HarnessWorkspace.lexAndParse
let compileInMemoryWorkspace = HarnessWorkspace.compileInMemoryWorkspace
let compileInMemoryWorkspaceWithBackend = HarnessWorkspace.compileInMemoryWorkspaceWithBackend
let compileInMemoryWorkspaceWithUnsafeConsume = HarnessWorkspace.compileInMemoryWorkspaceWithUnsafeConsume
let compileInMemoryWorkspaceWithPackageMode = HarnessWorkspace.compileInMemoryWorkspaceWithPackageMode
let evaluateInMemoryBinding = HarnessWorkspace.evaluateInMemoryBinding
let discoverKpFixtureCases = HarnessFixtures.discoverKpFixtureCases

type InMemoryFileSystem = HarnessWorkspace.InMemoryFileSystem
type KpFixtureMode = HarnessFixtures.KpFixtureMode
type KpFixtureDirectiveSource = HarnessFixtures.KpFixtureDirectiveSource
type KpFixtureRelation = HarnessFixtures.KpFixtureRelation
type KpFixtureConfiguration = HarnessFixtures.KpFixtureConfiguration
type KpFixtureAssertion = HarnessFixtures.KpFixtureAssertion
type KpFixtureCase = HarnessFixtures.KpFixtureCase

module KpFixtureConfiguration =
    let defaultValue = HarnessFixtures.KpFixtureConfiguration.defaultValue

let private rootedFilePath = HarnessSupport.rootedFilePath

let private rootedSiblingPath (filePath: string) (relativePath: string) =
    let directory = Path.GetDirectoryName(filePath)
    Path.Combine(directory, relativePath) |> Path.GetFullPath

type ProcessResult = HarnessSupport.ProcessResult
type LoadedManagedAssembly = HarnessSupport.LoadedManagedAssembly

let createScratchDirectory = HarnessSupport.createScratchDirectory
let currentRid = HarnessSupport.currentRid
let writeWorkspaceFiles = HarnessSupport.writeWorkspaceFiles
let executablePath = HarnessSupport.executablePath
let runProcess = HarnessSupport.runProcess
let runProcessWithEnvironment = HarnessSupport.runProcessWithEnvironment
let runProcessWithInput = HarnessSupport.runProcessWithInput
let runProcessWithEnvironmentAndInput = HarnessSupport.runProcessWithEnvironmentAndInput
let runBuiltCli = HarnessSupport.runBuiltCli
let runBuiltCliWithEnvironment = HarnessSupport.runBuiltCliWithEnvironment
let repoZigBootstrapCommand = HarnessSupport.repoZigBootstrapCommand
let ensureRepoZigExecutablePath = HarnessSupport.ensureRepoZigExecutablePath
let loadManagedAssembly = HarnessSupport.loadManagedAssembly

let private isLayoutOrSentinelToken (token: Token) =
    match token.Kind with
    | Newline
    | Indent
    | Dedent
    | EndOfFile -> true
    | _ -> false

let private tokenTexts (tokens: Token list) =
    tokens
    |> List.filter (isLayoutOrSentinelToken >> not)
    |> List.map (fun token -> token.Text)

let private formatDiagnostics = HarnessExecution.formatDiagnostics
let private countDiagnosticsBySeverity = HarnessExecution.countDiagnosticsBySeverity
let private normalizeLineEndings = HarnessExecution.normalizeLineEndings
let private normalizeExecutionOutput = HarnessExecution.normalizeExecutionOutput
let private executeBindingWithCapturedOutput = HarnessExecution.executeBindingWithCapturedOutput

let private tokenizeAssertionTypeText (expectedTypeText: string) =
    let source = createSource "__fixture_assertion_type__.kp" expectedTypeText
    let lexed = Lexer.tokenize source

    Assert.Empty(lexed.Diagnostics)
    lexed.Tokens

type private FixtureTypeInventory =
    { Terms: Set<string>
      Types: Set<string>
      Traits: Set<string> }

type private DeclaredTypeInfo =
    { Document: ParsedDocument
      Tokens: Token list }

let private emptyFixtureTypeInventory =
    { Terms = Set.empty
      Types = Set.empty
      Traits = Set.empty }

let private fixtureDocumentPrivateByDefault (document: ParsedDocument) =
    document.Syntax.ModuleAttributes
    |> List.exists (fun attributeName -> String.Equals(attributeName, "PrivateByDefault", StringComparison.Ordinal))

let private isFixtureExportedVisibility privateByDefault visibility =
    match visibility with
    | Some Visibility.Public -> true
    | Some Visibility.Private -> false
    | None -> not privateByDefault

let private buildFixtureTypeInventories (workspace: WorkspaceCompilation) =
    let addDocument includePrivate inventory (document: ParsedDocument) =
        let privateByDefault = fixtureDocumentPrivateByDefault document

        let includeVisibility visibility =
            includePrivate || isFixtureExportedVisibility privateByDefault visibility

        document.Syntax.Declarations
        |> List.fold (fun state declaration ->
            match declaration with
            | SignatureDeclaration signature when includeVisibility signature.Visibility ->
                { state with
                    Terms = Set.add signature.Name state.Terms }
            | LetDeclaration definition when includeVisibility definition.Visibility ->
                match definition.Name with
                | Some name ->
                    { state with
                        Terms = Set.add name state.Terms }
                | None ->
                    state
            | ProjectionDeclarationNode projection when includeVisibility projection.Visibility ->
                { state with
                    Terms = Set.add projection.Name state.Terms }
            | DataDeclarationNode dataDeclaration when includeVisibility dataDeclaration.Visibility ->
                { state with
                    Types = Set.add dataDeclaration.Name state.Types }
            | TypeAliasNode typeAlias when includeVisibility typeAlias.Visibility ->
                { state with
                    Types = Set.add typeAlias.Name state.Types }
            | TraitDeclarationNode traitDeclaration when includeVisibility traitDeclaration.Visibility ->
                { state with
                    Traits = Set.add traitDeclaration.Name state.Traits }
            | _ ->
                state) inventory

    let groupedDocuments =
        workspace.Documents
        |> List.choose (fun document ->
            document.ModuleName
            |> Option.map (fun moduleName -> SyntaxFacts.moduleNameToText moduleName, document))
        |> List.groupBy fst

    let localInventories =
        groupedDocuments
        |> List.map (fun (moduleName, entries) ->
            let inventory =
                entries
                |> List.map snd
                |> List.fold (addDocument true) emptyFixtureTypeInventory

            moduleName, inventory)
        |> Map.ofList

    let exportedInventories =
        groupedDocuments
        |> List.map (fun (moduleName, entries) ->
            let inventory =
                entries
                |> List.map snd
                |> List.fold (addDocument false) emptyFixtureTypeInventory

            moduleName, inventory)
        |> Map.ofList

    let exportedInventories =
        ([ "std.unicode"; "std.bytes"; "std.hash"; "std.testing" ]
         |> List.fold (fun state moduleName ->
            let inventory =
                { Terms = Stdlib.standardModuleTermNames moduleName
                  Types = Stdlib.standardModuleTypeNames moduleName
                  Traits = Stdlib.standardModuleTraitNames moduleName }

            state
            |> Map.change moduleName (function
                | Some existing ->
                    Some
                        { Terms = Set.union existing.Terms inventory.Terms
                          Types = Set.union existing.Types inventory.Types
                          Traits = Set.union existing.Traits inventory.Traits }
                | None ->
                    Some inventory)) exportedInventories)

    localInventories, exportedInventories

let private addResolutionCandidate name moduleSegments (candidates: Map<string, string list list>) =
    let existing = candidates |> Map.tryFind name |> Option.defaultValue []

    if existing |> List.exists ((=) moduleSegments) then
        candidates
    else
        Map.add name (moduleSegments :: existing) candidates

let private resolveUniqueCandidates (candidates: Map<string, string list list>) =
    candidates
    |> Map.toList
    |> List.choose (fun (name, moduleCandidates) ->
        match moduleCandidates |> List.distinct with
        | [ uniqueModule ] -> Some(name, uniqueModule)
        | _ -> None)
    |> Map.ofList

let private fixtureImportedNamesForSelection importNamespace selection (inventory: FixtureTypeInventory) =
    let inInventory name =
        match importNamespace with
        | ImportNamespace.Term -> Set.contains name inventory.Terms
        | ImportNamespace.Type -> Set.contains name inventory.Types
        | ImportNamespace.Trait -> Set.contains name inventory.Traits
        | _ -> false

    let isExcluded name (item: ExceptItem) =
        String.Equals(item.Name, name, StringComparison.Ordinal)
        && (item.Namespace.IsNone || item.Namespace = Some importNamespace)

    match selection with
    | QualifiedOnly ->
        Set.empty
    | All ->
        match importNamespace with
        | ImportNamespace.Term -> inventory.Terms
        | ImportNamespace.Type -> inventory.Types
        | ImportNamespace.Trait -> inventory.Traits
        | _ -> Set.empty
    | AllExcept excludedItems ->
        match importNamespace with
        | ImportNamespace.Term -> inventory.Terms |> Set.filter (fun name -> not (excludedItems |> List.exists (isExcluded name)))
        | ImportNamespace.Type -> inventory.Types |> Set.filter (fun name -> not (excludedItems |> List.exists (isExcluded name)))
        | ImportNamespace.Trait -> inventory.Traits |> Set.filter (fun name -> not (excludedItems |> List.exists (isExcluded name)))
        | _ -> Set.empty
    | Items items ->
        items
        |> List.choose (fun item ->
            let namespaceMatches =
                match item.Namespace with
                | Some itemNamespace -> itemNamespace = importNamespace
                | None -> inInventory item.Name

            if namespaceMatches && inInventory item.Name then
                Some(item.Alias |> Option.defaultValue item.Name)
            else
                None)
        |> Set.ofList

let private collectFixtureImportSpecs (document: ParsedDocument) =
    Stdlib.implicitImportsFor document.ModuleName
    @ (document.Syntax.Declarations
       |> List.collect (function
           | ImportDeclaration (_, specs) -> specs
           | _ -> []))

let private fixtureInScopeTypeResolutions (workspace: WorkspaceCompilation) (document: ParsedDocument) =
    let localInventories, exportedInventories = buildFixtureTypeInventories workspace

    let localTypeResolutions, localTraitResolutions =
        match document.ModuleName with
        | Some moduleName ->
            let moduleNameText = SyntaxFacts.moduleNameToText moduleName
            let localInventory = localInventories |> Map.tryFind moduleNameText |> Option.defaultValue emptyFixtureTypeInventory

            let types =
                localInventory.Types
                |> Seq.map (fun name -> name, moduleName @ [ name ])
                |> Map.ofSeq

            let traits =
                localInventory.Traits
                |> Seq.map (fun name -> name, SyntaxFacts.moduleNameToText (moduleName @ [ name ]))
                |> Map.ofSeq

            types, traits
        | None ->
            Map.empty, Map.empty

    let importedTypeCandidates, importedTraitCandidates =
        collectFixtureImportSpecs document
        |> List.fold (fun (typeCandidates, traitCandidates) spec ->
            match spec.Source with
            | Dotted moduleName ->
                let moduleNameText = SyntaxFacts.moduleNameToText moduleName
                let inventory = exportedInventories |> Map.tryFind moduleNameText |> Option.defaultValue emptyFixtureTypeInventory

                let typeCandidates' =
                    fixtureImportedNamesForSelection ImportNamespace.Type spec.Selection inventory
                    |> Seq.fold (fun state name ->
                        if localTypeResolutions.ContainsKey(name) then
                            state
                        else
                            addResolutionCandidate name moduleName state) typeCandidates

                let traitCandidates' =
                    fixtureImportedNamesForSelection ImportNamespace.Trait spec.Selection inventory
                    |> Seq.fold (fun state name ->
                        if localTraitResolutions.ContainsKey(name) then
                            state
                        else
                            addResolutionCandidate name moduleName state) traitCandidates

                typeCandidates', traitCandidates'
            | Url _ ->
                typeCandidates, traitCandidates) (Map.empty, Map.empty)

    let importedTypeResolutions =
        importedTypeCandidates
        |> resolveUniqueCandidates
        |> Map.toList
        |> List.map (fun (name, moduleName) -> name, moduleName @ [ name ])
        |> Map.ofList

    let importedTraitResolutions =
        importedTraitCandidates
        |> resolveUniqueCandidates
        |> Map.toList
        |> List.map (fun (name, moduleName) -> name, SyntaxFacts.moduleNameToText (moduleName @ [ name ]))
        |> Map.ofList

    let typeResolutions =
        importedTypeResolutions
        |> Map.fold (fun (state: Map<string, string list>) name resolvedName ->
            if Map.containsKey name state then
                state
            else
                Map.add name resolvedName state) localTypeResolutions

    let traitResolutions =
        importedTraitResolutions
        |> Map.fold (fun (state: Map<string, string>) name resolvedName ->
            if Map.containsKey name state then
                state
            else
                Map.add name resolvedName state) localTraitResolutions

    typeResolutions, traitResolutions

let private fixtureTypeNameResolutions (workspace: WorkspaceCompilation) (document: ParsedDocument) =
    let typeResolutions, traitResolutions = fixtureInScopeTypeResolutions workspace document

    let addResolution name qualifiedName (state: Map<string, string>) =
        match state |> Map.tryFind name with
        | None ->
            Map.add name qualifiedName state
        | Some existing when String.Equals(existing, qualifiedName, StringComparison.Ordinal) ->
            state
        | Some _ ->
            Map.remove name state

    let withTypes =
        typeResolutions
        |> Map.toList
        |> List.fold (fun state (name, qualifiedName) ->
            addResolution name (SyntaxFacts.moduleNameToText qualifiedName) state) Map.empty

    traitResolutions
    |> Map.toList
    |> List.fold (fun state (name, qualifiedName) -> addResolution name qualifiedName state) withTypes

let private normalizeFixtureTypeTokensWithResolutions (nameResolutions: Map<string, string>) (tokens: Token list) =
    let significantTokens =
        tokens |> List.filter (isLayoutOrSentinelToken >> not) |> List.toArray

    let rec loop index normalized =
        if index >= significantTokens.Length then
            List.rev normalized
        else
            let token = significantTokens[index]

            if Token.isName token then
                let segments = ResizeArray<string>()
                segments.Add(SyntaxFacts.trimIdentifierQuotes token.Text)

                let mutable nextIndex = index + 1
                let mutable keepReading = true

                while keepReading && nextIndex + 1 < significantTokens.Length do
                    match significantTokens[nextIndex].Kind, significantTokens[nextIndex + 1] with
                    | Dot, nextToken when Token.isName nextToken ->
                        segments.Add(SyntaxFacts.trimIdentifierQuotes nextToken.Text)
                        nextIndex <- nextIndex + 2
                    | _ ->
                        keepReading <- false

                let collapsedName = segments |> Seq.toList |> SyntaxFacts.moduleNameToText

                let normalizedName =
                    let segmentsList = segments |> Seq.toList

                    match segmentsList with
                    | [ simpleName ] ->
                        nameResolutions
                        |> Map.tryFind simpleName
                        |> Option.defaultValue (
                            if simpleName.StartsWith("std.prelude.", StringComparison.Ordinal) then
                                simpleName.Substring("std.prelude.".Length)
                            else
                                collapsedName
                        )
                    | [ "std"; "prelude"; shortName ] ->
                        shortName
                    | _ ->
                        if collapsedName.StartsWith("std.prelude.", StringComparison.Ordinal) then
                            collapsedName.Substring("std.prelude.".Length)
                        else
                            collapsedName

                let normalizedName =
                    if normalizedName.StartsWith("std.prelude.", StringComparison.Ordinal) then
                        normalizedName.Substring("std.prelude.".Length)
                    else
                        normalizedName

                loop nextIndex (normalizedName :: normalized)
            else
                loop (index + 1) (token.Text :: normalized)

    loop 0 []

let private normalizeFixtureTypeTokens (workspace: WorkspaceCompilation) (document: ParsedDocument) (tokens: Token list) =
    normalizeFixtureTypeTokensWithResolutions (fixtureTypeNameResolutions workspace document) tokens

let private resolveFixtureTypeExpr
    (typeResolutions: Map<string, string list>)
    (typeExpr: TypeSignatures.TypeExpr)
    =
    let rec loop current =
        match current with
        | TypeSignatures.TypeLevelLiteral _ ->
            current
        | TypeSignatures.TypeUniverse None ->
            current
        | TypeSignatures.TypeUniverse(Some universeExpr) ->
            TypeSignatures.TypeUniverse(Some(loop universeExpr))
        | TypeSignatures.TypeIntrinsic _ ->
            current
        | TypeSignatures.TypeApply(callee, arguments) ->
            TypeSignatures.TypeApply(loop callee, arguments |> List.map loop)
        | TypeSignatures.TypeLambda(parameterName, parameterSort, body) ->
            TypeSignatures.TypeLambda(parameterName, loop parameterSort, loop body)
        | TypeSignatures.TypeDelay inner ->
            TypeSignatures.TypeDelay(loop inner)
        | TypeSignatures.TypeMemo inner ->
            TypeSignatures.TypeMemo(loop inner)
        | TypeSignatures.TypeForce inner ->
            TypeSignatures.TypeForce(loop inner)
        | TypeSignatures.TypeProject(target, fieldName) ->
            TypeSignatures.TypeProject(loop target, fieldName)
        | TypeSignatures.TypeVariable _ ->
            current
        | TypeSignatures.TypeName([ name ], arguments) ->
            let resolvedName = typeResolutions |> Map.tryFind name |> Option.defaultValue [ name ]
            TypeSignatures.TypeName(resolvedName, arguments |> List.map loop)
        | TypeSignatures.TypeName(name, arguments) ->
            TypeSignatures.TypeName(name, arguments |> List.map loop)
        | TypeSignatures.TypeArrow(quantity, parameterType, resultType) ->
            TypeSignatures.TypeArrow(quantity, loop parameterType, loop resultType)
        | TypeSignatures.TypeEquality(left, right) ->
            TypeSignatures.TypeEquality(loop left, loop right)
        | TypeSignatures.TypeCapture(inner, captures) ->
            TypeSignatures.TypeCapture(loop inner, captures)
        | TypeSignatures.TypeEffectRow(entries, tail) ->
            TypeSignatures.TypeEffectRow(
                entries
                |> List.map (fun entry ->
                    { Label = loop entry.Label
                      Effect = loop entry.Effect }),
                tail |> Option.map loop
            )
        | TypeSignatures.TypeRecord fields ->
            TypeSignatures.TypeRecord(
                fields
                |> List.map (fun field ->
                    { field with
                        Type = loop field.Type })
            )
        | TypeSignatures.TypeUnion members ->
            TypeSignatures.TypeUnion(members |> List.map loop)

    loop typeExpr

let private resolveFixtureTypeScheme
    (workspace: WorkspaceCompilation)
    (document: ParsedDocument)
    (scheme: TypeSignatures.TypeScheme)
    =
    let typeResolutions, traitResolutions = fixtureInScopeTypeResolutions workspace document

    { scheme with
        Constraints =
            scheme.Constraints
            |> List.map (fun constraintInfo ->
                { constraintInfo with
                    TraitName =
                        traitResolutions
                        |> Map.tryFind constraintInfo.TraitName
                        |> Option.defaultValue constraintInfo.TraitName
                    Arguments =
                        constraintInfo.Arguments
                        |> List.map (resolveFixtureTypeExpr typeResolutions) })
        Body = resolveFixtureTypeExpr typeResolutions scheme.Body }

let private normalizeFixtureText (value: string) =
    value.Trim().ToLowerInvariant()

let private isPortableFixtureTraceStep (step: PipelineTraceStep) =
    let stepName = step.StepName

    not (
        stepName.Contains(Stdlib.BundledPreludeVirtualPath, StringComparison.Ordinal)
        || stepName.Contains(Stdlib.PreludeModuleText, StringComparison.Ordinal)
    )

let private declarationKindText declaration =
    match declaration with
    | ImportDeclaration (true, _) -> "export"
    | ImportDeclaration (false, _) -> "import"
    | FixityDeclarationNode _ -> "fixity"
    | ExpectDeclarationNode _ -> "expect"
    | SignatureDeclaration _ -> "signature"
    | LetDeclaration _ -> "let"
    | ProjectionDeclarationNode _ -> "projection"
    | DataDeclarationNode _ -> "data"
    | TypeAliasNode _ -> "type"
    | EffectDeclarationNode _ -> "effect"
    | TraitDeclarationNode _ -> "trait"
    | InstanceDeclarationNode _ -> "instance"
    | UnknownDeclaration _ -> "unknown"

let private visibilityText visibility =
    match visibility with
    | Some Visibility.Public -> "public"
    | Some Visibility.Private -> "private"
    | None -> ""

let private moduleSpecifierText moduleSpecifier =
    match moduleSpecifier with
    | Dotted segments ->
        SyntaxFacts.moduleNameToText segments
    | Url url ->
        $"\"{url.OriginalText}\""

let private importItemModifierText modifier =
    match modifier with
    | Unhide -> "unhide"
    | Clarify -> "clarify"

let private importNamespaceText importNamespace =
    match importNamespace with
    | ImportNamespace.Term -> "term"
    | ImportNamespace.Type -> "type"
    | ImportNamespace.Trait -> "trait"
    | ImportNamespace.Constructor -> "ctor"

let private importItemText (item: ImportItem) =
    let parts =
        (item.Modifiers |> List.map importItemModifierText)
        @ [ item.Namespace |> Option.map importNamespaceText |> Option.defaultValue ""
            item.Name ]

    let baseText =
        parts
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> String.concat " "

    let baseText =
        if item.IncludeConstructors then
            $"{baseText}(..)"
        else
            baseText

    match item.Alias with
    | Some alias -> $"{baseText} as {alias}"
    | None -> baseText

let private exceptItemText (item: ExceptItem) =
    match item.Namespace with
    | Some importNamespace -> $"{importNamespaceText importNamespace} {item.Name}"
    | None -> item.Name

let private importSpecText (spec: ImportSpec) =
    let sourceText = moduleSpecifierText spec.Source

    match spec.Alias, spec.Selection with
    | Some alias, QualifiedOnly ->
        $"{sourceText} as {alias}"
    | None, QualifiedOnly ->
        sourceText
    | None, Items items ->
        let itemText =
            items
            |> List.map importItemText
            |> String.concat " + "

        $"{sourceText}.({itemText})"
    | None, All ->
        $"{sourceText}.*"
    | None, AllExcept items ->
        let nameText = items |> List.map exceptItemText |> String.concat " + "
        $"{sourceText}.* except ({nameText})"
    | Some alias, _ ->
        $"{sourceText} as {alias}"

let private declarationDescriptorText declaration =
    let joinParts parts =
        parts
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> String.concat " "

    match declaration with
    | ImportDeclaration (true, specs) ->
        joinParts [ "export"; (specs |> List.map importSpecText |> String.concat " | ") ]
    | ImportDeclaration (false, specs) ->
        joinParts [ "import"; (specs |> List.map importSpecText |> String.concat " | ") ]
    | FixityDeclarationNode declaration ->
        let fixityText =
            match declaration.Fixity with
            | Infix _ -> "infix"
            | Prefix -> "prefix"
            | Postfix -> "postfix"

        joinParts [ "fixity"; fixityText; declaration.OperatorName ]
    | ExpectDeclarationNode declaration ->
        match declaration with
        | ExpectTypeDeclaration declaration -> joinParts [ "expect"; "type"; declaration.Name ]
        | ExpectTraitDeclaration declaration -> joinParts [ "expect"; "trait"; declaration.Name ]
        | ExpectTermDeclaration declaration -> joinParts [ "expect"; "term"; declaration.Name ]
    | SignatureDeclaration declaration ->
        joinParts [ visibilityText declaration.Visibility; (if declaration.IsOpaque then "opaque" else ""); "signature"; declaration.Name ]
    | LetDeclaration declaration ->
        joinParts [ visibilityText declaration.Visibility; (if declaration.IsOpaque then "opaque" else ""); "let"; defaultArg declaration.Name "<anonymous>" ]
    | ProjectionDeclarationNode declaration ->
        joinParts [ visibilityText declaration.Visibility; "projection"; declaration.Name ]
    | DataDeclarationNode declaration ->
        joinParts [ visibilityText declaration.Visibility; (if declaration.IsOpaque then "opaque" else ""); "data"; declaration.Name ]
    | TypeAliasNode declaration ->
        joinParts [ visibilityText declaration.Visibility; (if declaration.IsOpaque then "opaque" else ""); "type"; declaration.Name ]
    | EffectDeclarationNode declaration ->
        joinParts [ visibilityText declaration.Visibility; "effect"; declaration.Name ]
    | TraitDeclarationNode declaration ->
        joinParts [ visibilityText declaration.Visibility; "trait"; declaration.Name ]
    | InstanceDeclarationNode declaration ->
        joinParts [ "instance"; declaration.TraitName ]
    | UnknownDeclaration _ ->
        "unknown"

let private tokenKindText tokenKind =
    match tokenKind with
    | Identifier -> "Identifier"
    | IntegerLiteral -> "IntegerLiteral"
    | FloatLiteral -> "FloatLiteral"
    | StringLiteral -> "StringLiteral"
    | InterpolatedStringStart -> "InterpolatedStringStart"
    | StringTextSegment -> "StringTextSegment"
    | InterpolationStart -> "InterpolationStart"
    | InterpolationEnd -> "InterpolationEnd"
    | InterpolatedStringEnd -> "InterpolatedStringEnd"
    | CharacterLiteral -> "CharacterLiteral"
    | Keyword keyword -> $"Keyword.{Keyword.toText keyword}"
    | Operator -> "Operator"
    | AtSign -> "AtSign"
    | Backslash -> "Backslash"
    | Colon -> "Colon"
    | Comma -> "Comma"
    | Dot -> "Dot"
    | Equals -> "Equals"
    | Arrow -> "Arrow"
    | LeftParen -> "LeftParen"
    | RightParen -> "RightParen"
    | LeftBracket -> "LeftBracket"
    | RightBracket -> "RightBracket"
    | LeftEffectRow -> "LeftEffectRow"
    | RightEffectRow -> "RightEffectRow"
    | LeftBrace -> "LeftBrace"
    | RightBrace -> "RightBrace"
    | LeftSetBrace -> "LeftSetBrace"
    | RightSetBrace -> "RightSetBrace"
    | Underscore -> "Underscore"
    | Newline -> "Newline"
    | Indent -> "Indent"
    | Dedent -> "Dedent"
    | EndOfFile -> "EndOfFile"
    | BadToken -> "BadToken"

let private tryFindDocumentForFilePath (workspace: WorkspaceCompilation) (filePath: string) =
    let normalizedPath = Path.GetFullPath(filePath)

    workspace.Documents
    |> List.tryFind (fun document ->
        String.Equals(document.Source.FilePath, normalizedPath, StringComparison.OrdinalIgnoreCase))

let private lexFixtureFile (filePath: string) =
    let source = createSource filePath (File.ReadAllText(filePath))
    Lexer.tokenize source

let private tryFindDataDeclaration typeName (document: ParsedDocument) =
    document.Syntax.Declarations
    |> List.tryPick (function
        | DataDeclarationNode declaration when String.Equals(declaration.Name, typeName, StringComparison.Ordinal) ->
            Some declaration
        | _ ->
            None)

let private tryFindTraitDeclaration traitName (document: ParsedDocument) =
    document.Syntax.Declarations
    |> List.tryPick (function
        | TraitDeclarationNode declaration when String.Equals(declaration.Name, traitName, StringComparison.Ordinal) ->
            Some declaration
        | _ ->
            None)

let private tryFindLetDefinition bindingName (document: ParsedDocument) =
    document.Syntax.Declarations
    |> List.tryPick (function
        | LetDeclaration declaration when declaration.Name = Some bindingName ->
            Some declaration
        | _ ->
            None)

let rec private fixturePatternText pattern =
    match pattern with
    | WildcardPattern -> "_"
    | NamePattern name -> name
    | AsPattern(name, inner) -> $"{name}@{fixturePatternText inner}"
    | TypedPattern(inner, typeTokens) -> $"({fixturePatternText inner} : {tokenTexts typeTokens})"
    | LiteralPattern(LiteralValue.Integer value) -> string value
    | LiteralPattern(LiteralValue.Float value) -> string value
    | LiteralPattern(LiteralValue.String value) -> $"\"{value}\""
    | LiteralPattern(LiteralValue.Character value) -> $"'{value}'"
    | LiteralPattern(LiteralValue.Grapheme value) -> $"g'{value}'"
    | LiteralPattern(LiteralValue.Byte value) -> $"b'\\x{int value:X2}'"
    | LiteralPattern LiteralValue.Unit -> "()"
    | ConstructorPattern(name, arguments) ->
        let nameText = String.concat "." name

        match arguments with
        | [] -> nameText
        | _ ->
            let argumentText = arguments |> List.map fixturePatternText |> String.concat " "
            $"{nameText} {argumentText}"
    | NamedConstructorPattern(name, fields) ->
        let nameText = String.concat "." name
        let fieldText =
            fields
            |> List.map (fun field ->
                let prefix = if field.IsImplicit then "@" else ""
                $"{prefix}{field.Name} = {fixturePatternText field.Pattern}")
            |> String.concat ", "

        $"{nameText} {{ {fieldText} }}"
    | TuplePattern elements ->
        let suffix = if List.length elements = 1 then "," else ""
        let elementText = elements |> List.map fixturePatternText |> String.concat ", "
        $"({elementText}{suffix})"
    | VariantPattern(BoundVariantPattern(name, None)) -> $"(| {name} |)"
    | VariantPattern(BoundVariantPattern(name, Some typeTokens)) -> $"(| {name} : {tokenTexts typeTokens} |)"
    | VariantPattern(WildcardVariantPattern None) -> "(| _ |)"
    | VariantPattern(WildcardVariantPattern(Some typeTokens)) -> $"(| _ : {tokenTexts typeTokens} |)"
    | VariantPattern(RestVariantPattern name) -> $"(| ..{name} |)"
    | OrPattern alternatives ->
        alternatives
        |> List.map fixturePatternText
        |> String.concat " | "
    | AnonymousRecordPattern(fields, rest) ->
        let fieldText =
            fields
            |> List.map (fun field ->
                let prefix = if field.IsImplicit then "@" else ""
                $"{prefix}{field.Name} = {fixturePatternText field.Pattern}")
            |> String.concat ", "

        let restText =
            match rest with
            | Some DiscardRecordPatternRest -> ".."
            | Some(BindRecordPatternRest name) -> $"..{name}"
            | None -> ""

        [ if not (String.IsNullOrEmpty fieldText) then fieldText
          if not (String.IsNullOrEmpty restText) then restText ]
        |> String.concat ", "
        |> fun parts -> $"({parts})"

let private fixtureBindPatternText (binding: SurfaceBindPattern) =
    let quantityText =
        binding.Quantity
        |> Option.map (fun quantity -> Quantity.toSurfaceText quantity + " ")
        |> Option.defaultValue ""

    quantityText + fixturePatternText binding.Pattern

let private doItemDescriptor statement =
    match statement with
    | DoLet(binding, _) -> $"let {fixtureBindPatternText binding}"
    | DoLetQuestion(binding, _, _) -> $"let? {fixtureBindPatternText binding}"
    | DoBind(binding, _) -> $"<- {fixtureBindPatternText binding}"
    | DoUsing(binding, _) -> $"using {fixturePatternText binding.Pattern}"
    | DoDefer _ -> "defer"
    | DoVar(name, _) -> $"var {name}"
    | DoAssign(name, _) -> $"assign {name}"
    | DoIf _ -> "if"
    | DoWhile _ -> "while"
    | DoReturn _ -> "return"
    | DoExpression _ -> "expression"

let private qualifyFixtureBindingTarget (workspace: WorkspaceCompilation) (filePath: string) (target: string) =
    if target.Contains(".", StringComparison.Ordinal) then
        target
    else
        tryFindDocumentForFilePath workspace filePath
        |> Option.bind (fun document ->
            document.ModuleName
            |> Option.map (fun moduleName ->
                $"{SyntaxFacts.moduleNameToText moduleName}.{target}"))
        |> Option.defaultValue target

let private tryFindDeclaredTypeInDocument bindingName (document: ParsedDocument) =
    let signatureTokens =
        document.Syntax.Declarations
        |> List.tryPick (function
            | SignatureDeclaration signature when String.Equals(signature.Name, bindingName, StringComparison.Ordinal) ->
                Some signature.TypeTokens
            | _ ->
                None)

    match signatureTokens with
    | Some tokens ->
        Some
            { Document = document
              Tokens = tokens }
    | None ->
        document.Syntax.Declarations
        |> List.tryPick (function
            | LetDeclaration definition when definition.Name = Some bindingName ->
                definition.ReturnTypeTokens
                |> Option.map (fun tokens ->
                    { Document = document
                      Tokens = tokens })
            | _ ->
                None)

let private tryFindStandardModuleDeclaredType bindingName moduleName currentDocument =
    Stdlib.tryStandardModuleTermTypeText moduleName bindingName
    |> Option.map (fun typeText ->
        { Document = currentDocument
          Tokens =
            tokenizeAssertionTypeText typeText
            |> List.filter (fun token -> token.Kind <> EndOfFile) })

let private tryFindImportedDeclaredTypeInDocument (workspace: WorkspaceCompilation) bindingName (document: ParsedDocument) =
    let _, exportedInventories = buildFixtureTypeInventories workspace

    let importedTermCandidates =
        collectFixtureImportSpecs document
        |> List.fold (fun candidates spec ->
            match spec.Source with
            | Dotted moduleName ->
                let moduleNameText = SyntaxFacts.moduleNameToText moduleName
                let inventory = exportedInventories |> Map.tryFind moduleNameText |> Option.defaultValue emptyFixtureTypeInventory

                fixtureImportedNamesForSelection ImportNamespace.Term spec.Selection inventory
                |> Seq.fold (fun state name -> addResolutionCandidate name moduleName state) candidates
            | Url _ ->
                candidates) Map.empty

    let importedTermResolutions = resolveUniqueCandidates importedTermCandidates

    importedTermResolutions
    |> Map.tryFind bindingName
    |> Option.bind (fun moduleName ->
        let moduleNameText = SyntaxFacts.moduleNameToText moduleName

        tryFindStandardModuleDeclaredType bindingName moduleNameText document
        |> Option.orElseWith (fun () ->
            workspace.Documents
            |> List.tryPick (fun candidate ->
                match candidate.ModuleName with
                | Some candidateModuleName when String.Equals(SyntaxFacts.moduleNameToText candidateModuleName, moduleNameText, StringComparison.Ordinal) ->
                    tryFindDeclaredTypeInDocument bindingName candidate
                | _ ->
                    None)))

let private tryFindDeclaredType (workspace: WorkspaceCompilation) (currentFilePath: string option) (target: string) =
    let segments =
        target.Split('.', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    match segments with
    | [] ->
        invalidArg (nameof target) "Fixture assertion target cannot be empty."
    | [ bindingName ] ->
        let currentDocument =
            currentFilePath
            |> Option.bind (fun filePath ->
                workspace.Documents
                |> List.tryFind (fun document ->
                    String.Equals(document.Source.FilePath, Path.GetFullPath(filePath), StringComparison.OrdinalIgnoreCase)))

        match currentDocument |> Option.bind (tryFindDeclaredTypeInDocument bindingName) with
        | Some info ->
            Some info
        | None ->
            match currentDocument |> Option.bind (tryFindImportedDeclaredTypeInDocument workspace bindingName) with
            | Some info ->
                Some info
            | None ->
                let matches =
                    workspace.Documents
                    |> List.choose (fun document ->
                        tryFindDeclaredTypeInDocument bindingName document
                        |> Option.map (fun info ->
                            let moduleName =
                                document.ModuleName
                                |> Option.map SyntaxFacts.moduleNameToText
                                |> Option.defaultValue "<unknown>"

                            moduleName, info))

                match matches with
                | [] ->
                    None
                | _ when currentFilePath.IsSome ->
                    let preferredModuleName =
                        currentDocument
                        |> Option.bind (fun document -> document.ModuleName |> Option.map SyntaxFacts.moduleNameToText)

                    match preferredModuleName with
                    | Some moduleName ->
                        matches
                        |> List.tryPick (fun (candidateModuleName, info) ->
                            if String.Equals(candidateModuleName, moduleName, StringComparison.Ordinal) then
                                Some info
                            else
                                None)
                        |> Option.orElseWith (fun () ->
                            match matches with
                            | [ _, info ] ->
                                Some info
                            | multiple ->
                                let owners = multiple |> List.map fst |> String.concat ", "
                                invalidOp $"assertType target '{target}' is ambiguous across modules: {owners}.")
                    | None ->
                        match matches with
                        | [ _, info ] ->
                            Some info
                        | multiple ->
                            let owners = multiple |> List.map fst |> String.concat ", "
                            invalidOp $"assertType target '{target}' is ambiguous across modules: {owners}."
                | [ _, info ] ->
                    Some info
                | multiple ->
                    let owners = multiple |> List.map fst |> String.concat ", "
                    invalidOp $"assertType target '{target}' is ambiguous across modules: {owners}."
    | _ ->
        let moduleName = segments |> List.take (segments.Length - 1) |> SyntaxFacts.moduleNameToText
        let bindingName = List.last segments

        workspace.Documents
        |> List.tryPick (fun document ->
            match document.ModuleName with
            | Some candidate when String.Equals(SyntaxFacts.moduleNameToText candidate, moduleName, StringComparison.Ordinal) ->
                tryFindDeclaredTypeInDocument bindingName document
            | _ ->
                None)

let private requireFixtureDocument (workspace: WorkspaceCompilation) (filePath: string) lineNumber =
    match tryFindDocumentForFilePath workspace filePath with
    | Some document ->
        document
    | None ->
        failwithf "Could not find parsed document for '%s' (%s:%d)." filePath filePath lineNumber

let private requireFixtureDocumentByRelativePath
    (workspace: WorkspaceCompilation)
    (suiteRoot: string)
    (relativePath: string)
    lineNumber
    =
    let fullPath = rootedFilePath suiteRoot relativePath |> Path.GetFullPath

    match tryFindDocumentForFilePath workspace fullPath with
    | Some document ->
        document
    | None ->
        failwithf "Could not find parsed document for '%s' (%s:%d)." relativePath fullPath lineNumber

let private compileFixtureWorkspace = HarnessExecution.compileFixtureWorkspace
let private executeFixtureRun = HarnessExecution.executeFixtureRun

let private compareFixtureRelation relation actual expected =
    match relation with
    | KpFixtureRelation.Equal -> actual = expected
    | KpFixtureRelation.NotEqual -> actual <> expected
    | KpFixtureRelation.LessThan -> actual < expected
    | KpFixtureRelation.LessThanOrEqual -> actual <= expected
    | KpFixtureRelation.GreaterThan -> actual > expected
    | KpFixtureRelation.GreaterThanOrEqual -> actual >= expected

let private evaluateFixtureBinding (workspace: WorkspaceCompilation) (filePath: string) (target: string) =
    let bindingTarget = qualifyFixtureBindingTarget workspace filePath target
    bindingTarget, Interpreter.evaluateBinding workspace bindingTarget

let private executeFixtureBinding (workspace: WorkspaceCompilation) (filePath: string) (target: string) =
    let bindingTarget = qualifyFixtureBindingTarget workspace filePath target
    let result, output = executeBindingWithCapturedOutput workspace bindingTarget
    bindingTarget, result, output

let runKpFixtureCase (fixtureCase: KpFixtureCase) =
    Assert.NotEmpty(fixtureCase.SourceFiles)
    Assert.NotEmpty(fixtureCase.Assertions)

    let workspace = compileFixtureWorkspace fixtureCase

    let expectsDiagnostics =
        fixtureCase.Assertions
        |> List.exists (function
            | AssertErrorCount(expectedCount, _, _) when expectedCount > 0 -> true
            | AssertWarningCount(expectedCount, _, _) when expectedCount > 0 -> true
            | AssertDiagnostic _
            | AssertDiagnosticNext _
            | AssertDiagnosticCodes _ -> true
            | AssertDiagnosticAt _ -> true
            | AssertDiagnosticMatch _ -> true
            | _ -> false)

    if not expectsDiagnostics then
        Assert.False(
            workspace.HasErrors,
            $"Fixture '{fixtureCase.Name}' failed to compile:{Environment.NewLine}{formatDiagnostics workspace.Diagnostics}"
        )

    let runResult = lazy (executeFixtureRun fixtureCase workspace)
    let mutable nextDiagnosticIndex = 0

    let requireRunMode assertionName =
        if fixtureCase.Configuration.Mode <> KpFixtureMode.Run then
            invalidOp $"'{assertionName}' is valid only for fixtures running in mode run."

    for assertion in fixtureCase.Assertions do
        match assertion with
        | AssertNoErrors _ ->
            Assert.Equal(0, countDiagnosticsBySeverity DiagnosticSeverity.Error workspace.Diagnostics)
        | AssertNoWarnings _ ->
            Assert.Equal(0, countDiagnosticsBySeverity DiagnosticSeverity.Warning workspace.Diagnostics)
        | AssertErrorCount(expectedCount, _, _) ->
            Assert.Equal(expectedCount, countDiagnosticsBySeverity DiagnosticSeverity.Error workspace.Diagnostics)
        | AssertWarningCount(expectedCount, _, _) ->
            Assert.Equal(expectedCount, countDiagnosticsBySeverity DiagnosticSeverity.Warning workspace.Diagnostics)
        | AssertDiagnostic(severity, code, _, lineNumber) ->
            Assert.True(
                workspace.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = severity && diagnostic.Code = code),
                $"Could not find diagnostic {severity} {DiagnosticCode.toIdentifier code} ({fixtureCase.Name}:{lineNumber}). Actual diagnostics:{Environment.NewLine}{formatDiagnostics workspace.Diagnostics}"
            )
        | AssertDiagnosticNext(severity, code, _, lineNumber) ->
            let remainingDiagnostics =
                workspace.Diagnostics
                |> List.skip nextDiagnosticIndex

            match remainingDiagnostics with
            | diagnostic :: _ ->
                Assert.True(
                    diagnostic.Severity = severity && diagnostic.Code = code,
                    $"Expected next diagnostic {severity} {DiagnosticCode.toIdentifier code} but found {diagnostic.Severity} {DiagnosticCode.toIdentifier diagnostic.Code} ({fixtureCase.Name}:{lineNumber}). Actual diagnostics:{Environment.NewLine}{formatDiagnostics workspace.Diagnostics}"
                )

                nextDiagnosticIndex <- nextDiagnosticIndex + 1
            | [] ->
                Assert.True(
                    false,
                    $"Expected next diagnostic {severity} {DiagnosticCode.toIdentifier code}, but there were no remaining diagnostics ({fixtureCase.Name}:{lineNumber})."
                )
        | AssertDiagnosticCodes(expectedCodes, _, _) ->
            let actual = workspace.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)
            Assert.Equal<DiagnosticCode list>(expectedCodes, actual)
        | AssertDiagnosticAt(relativePath, severity, code, expectedLine, expectedColumn, _, lineNumber) ->
            let expectedPath = rootedFilePath fixtureCase.Root relativePath |> Path.GetFullPath

            let matches diagnostic =
                match diagnostic.Location with
                | Some location ->
                    diagnostic.Severity = severity
                    && diagnostic.Code = code
                    && String.Equals(Path.GetFullPath(location.FilePath), expectedPath, StringComparison.OrdinalIgnoreCase)
                    && location.Start.Line = expectedLine
                    && (expectedColumn |> Option.forall (fun column -> location.Start.Column = column))
                | None ->
                    false

            let expectedColumnText =
                expectedColumn
                |> Option.map (fun column -> $":{column}")
                |> Option.defaultValue ""

            let codeText = DiagnosticCode.toIdentifier code

            Assert.True(
                workspace.Diagnostics |> List.exists matches,
                $"Could not find diagnostic {severity} {codeText} at {relativePath}:{expectedLine}{expectedColumnText} ({fixtureCase.Name}:{lineNumber}). Actual diagnostics:{Environment.NewLine}{formatDiagnostics workspace.Diagnostics}"
            )
        | AssertDiagnosticMatch(pattern, _, lineNumber) ->
            let regex =
                try
                    Regex(pattern, RegexOptions.ECMAScript)
                with :? ArgumentException as ex ->
                    invalidOp $"Invalid assertDiagnosticMatch regex '{pattern}' ({fixtureCase.Name}:{lineNumber}): {ex.Message}"

            Assert.True(
                workspace.Diagnostics
                |> List.exists (fun item -> regex.IsMatch(item.Message)),
                $"Could not find diagnostic matching /{pattern}/ ({fixtureCase.Name}:{lineNumber}). Actual diagnostics:{Environment.NewLine}{formatDiagnostics workspace.Diagnostics}"
            )
        | AssertDiagnosticExplainExists(code, _, lineNumber) ->
            let explanation = DiagnosticCode.tryGetExplanation code

            Assert.True(
                explanation |> Option.exists (String.IsNullOrWhiteSpace >> not),
                $"Expected diagnostic explanation for {DiagnosticCode.toIdentifier code} ({fixtureCase.Name}:{lineNumber})."
            )
        | AssertType(target, expectedTypeText, filePath, lineNumber) ->
            let expectedTokens = tokenizeAssertionTypeText expectedTypeText

            match tryFindDeclaredType workspace (Some filePath) target with
            | Some actualInfo ->
                match
                    TypeSignatures.parseScheme actualInfo.Tokens,
                    (match tryFindDocumentForFilePath workspace filePath with
                     | Some expectedDocument -> TypeSignatures.parseScheme expectedTokens |> Option.map (fun parsed -> parsed, expectedDocument)
                     | None -> None)
                with
                | Some actualScheme, Some(expectedScheme, expectedDocument) ->
                    let resolvedActual = resolveFixtureTypeScheme workspace actualInfo.Document actualScheme
                    let resolvedExpected = resolveFixtureTypeScheme workspace expectedDocument expectedScheme

                    Assert.True(
                        TypeSignatures.schemeDefinitionallyEqual resolvedExpected resolvedActual,
                        $"Declared type for '{target}' does not definitionally equal '{expectedTypeText}'. Expected: {TypeSignatures.toText resolvedExpected.Body}. Actual: {TypeSignatures.toText resolvedActual.Body}."
                    )
                | _ ->
                    let normalizedActual =
                        normalizeFixtureTypeTokens workspace actualInfo.Document actualInfo.Tokens

                    let normalizedExpected =
                        match tryFindDocumentForFilePath workspace filePath with
                        | Some expectedDocument ->
                            normalizeFixtureTypeTokens workspace expectedDocument expectedTokens
                        | None ->
                            normalizeFixtureTypeTokensWithResolutions Map.empty expectedTokens

                    Assert.Equal<string list>(normalizedExpected, normalizedActual)
            | None ->
                failwithf "Could not find a declared top-level type for '%s' (%s:%d)." target filePath lineNumber
        | AssertFileDeclarationKinds(relativePath, expectedKinds, _, lineNumber) ->
            let document = requireFixtureDocumentByRelativePath workspace fixtureCase.Root relativePath lineNumber

            let expected =
                expectedKinds |> List.map normalizeFixtureText

            let actual =
                document.Syntax.Declarations
                |> List.map declarationKindText
                |> List.map normalizeFixtureText

            Assert.Equal<string list>(expected, actual)
        | AssertEval(target, expectedValueText, filePath, lineNumber) ->
            let bindingTarget, evaluation = evaluateFixtureBinding workspace filePath target

            match evaluation with
            | Result.Ok value ->
                Assert.Equal(expectedValueText, RuntimeValue.format value)
            | Result.Error issue ->
                failwithf
                    "Expected '%s' to evaluate to '%s', but evaluation failed with '%s' (%s:%d)."
                    bindingTarget
                    expectedValueText
                    issue.Message
                    filePath
                    lineNumber
        | AssertEvalErrorContains(target, expectedText, filePath, lineNumber) ->
            let bindingTarget, evaluation = evaluateFixtureBinding workspace filePath target

            match evaluation with
            | Result.Ok value ->
                failwithf
                    "Expected '%s' evaluation to fail with a message containing '%s', but got '%s' (%s:%d)."
                    bindingTarget
                    expectedText
                    (RuntimeValue.format value)
                    filePath
                    lineNumber
            | Result.Error issue ->
                Assert.Contains(expectedText, issue.Message, StringComparison.OrdinalIgnoreCase)
        | AssertExecute(target, expectedValueText, filePath, lineNumber) ->
            let bindingTarget, result, _ = executeFixtureBinding workspace filePath target

            match result with
            | Result.Ok value ->
                Assert.Equal(expectedValueText, RuntimeValue.format value)
            | Result.Error issue ->
                failwithf
                    "Expected '%s' execution to produce '%s', but execution failed with '%s' (%s:%d)."
                    bindingTarget
                    expectedValueText
                    issue.Message
                    filePath
                    lineNumber
        | AssertRunStdout(target, expectedOutputText, filePath, lineNumber) ->
            let bindingTarget, result, actualOutput = executeFixtureBinding workspace filePath target

            match result with
            | Result.Ok _ ->
                Assert.Equal(normalizeExecutionOutput expectedOutputText, actualOutput)
            | Result.Error issue ->
                failwithf
                    "Expected '%s' to run and write '%s', but execution failed with '%s' (%s:%d)."
                    bindingTarget
                    expectedOutputText
                    issue.Message
                    filePath
                    lineNumber
        | AssertStdout(expectedOutputText, _, _) ->
            requireRunMode "assertStdout"

            Assert.Equal(
                normalizeLineEndings expectedOutputText,
                runResult.Value.StandardOutput
            )
        | AssertStdoutFile(relativePath, filePath, _) ->
            requireRunMode "assertStdoutFile"

            let expectedOutputText =
                rootedSiblingPath filePath relativePath
                |> File.ReadAllText
                |> normalizeLineEndings

            Assert.Equal(expectedOutputText, runResult.Value.StandardOutput)
        | AssertStdoutContains(expectedOutputText, _, _) ->
            requireRunMode "assertStdoutContains"

            Assert.Contains(
                normalizeLineEndings expectedOutputText,
                runResult.Value.StandardOutput,
                StringComparison.Ordinal
            )
        | AssertStderrContains(expectedOutputText, _, _) ->
            requireRunMode "assertStderrContains"

            Assert.Contains(
                normalizeLineEndings expectedOutputText,
                runResult.Value.StandardError,
                StringComparison.Ordinal
            )
        | AssertExitCode(expectedCode, _, _) ->
            requireRunMode "assertExitCode"
            Assert.Equal(expectedCode, runResult.Value.ExitCode)
        | AssertTraceCount(eventName, subjectName, relation, expectedCount, filePath, lineNumber) ->
            let actualCount =
                Compilation.pipelineTrace workspace
                |> List.filter isPortableFixtureTraceStep
                |> List.filter (fun step ->
                    String.Equals(PipelineTraceEvent.toPortableName step.Event, eventName, StringComparison.Ordinal)
                    && String.Equals(PipelineTraceSubject.toPortableName step.Subject, subjectName, StringComparison.Ordinal))
                |> List.length

            Assert.True(
                compareFixtureRelation relation actualCount expectedCount,
                $"Trace count assertion failed for ({eventName}, {subjectName}) in '{fixtureCase.Name}' ({filePath}:{lineNumber}). Actual count: {actualCount}."
            )
        | AssertModule(expectedModuleText, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            let actualModuleText =
                document.ModuleName
                |> Option.map SyntaxFacts.moduleNameToText
                |> Option.defaultValue "<none>"

            Assert.Equal(expectedModuleText, actualModuleText)
        | AssertModuleAttributes(expectedAttributes, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            let expected =
                expectedAttributes
                |> List.map normalizeFixtureText

            let actual =
                document.Syntax.ModuleAttributes
                |> List.map normalizeFixtureText

            Assert.Equal<string list>(expected, actual)
        | AssertDeclarationKinds(expectedKinds, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            let expected =
                expectedKinds |> List.map normalizeFixtureText

            let actual =
                document.Syntax.Declarations
                |> List.map declarationKindText
                |> List.map normalizeFixtureText

            Assert.Equal<string list>(expected, actual)
        | AssertDeclarationDescriptors(expectedDescriptors, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            let expected =
                expectedDescriptors |> List.map normalizeFixtureText

            let actual =
                document.Syntax.Declarations
                |> List.map declarationDescriptorText
                |> List.map normalizeFixtureText

            Assert.Equal<string list>(expected, actual)
        | AssertParameterQuantities(bindingName, expectedQuantities, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            match tryFindLetDefinition bindingName document with
            | Some definition ->
                let expected =
                    expectedQuantities |> List.map normalizeFixtureText

                let actual =
                    definition.Parameters
                    |> List.map (fun parameter ->
                        parameter.Quantity
                        |> Option.map Quantity.toSurfaceText
                        |> Option.defaultValue "<default>")
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find let declaration '%s' (%s:%d)." bindingName filePath lineNumber
        | AssertInoutParameters(bindingName, expectedNames, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            match tryFindLetDefinition bindingName document with
            | Some definition ->
                let expected =
                    expectedNames |> List.map normalizeFixtureText

                let actual =
                    definition.Parameters
                    |> List.filter (fun parameter -> parameter.IsInout)
                    |> List.map (fun parameter -> parameter.Name)
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find let declaration '%s' (%s:%d)." bindingName filePath lineNumber
        | AssertDoItemDescriptors(bindingName, expectedDescriptors, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            match tryFindLetDefinition bindingName document with
            | Some { Body = Some(Do statements) } ->
                let expected =
                    expectedDescriptors |> List.map normalizeFixtureText

                let actual =
                    statements
                    |> List.map doItemDescriptor
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | Some _ ->
                failwithf "Let declaration '%s' does not have a do body (%s:%d)." bindingName filePath lineNumber
            | None ->
                failwithf "Could not find let declaration '%s' (%s:%d)." bindingName filePath lineNumber
        | AssertDataConstructors(typeName, expectedConstructors, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            match tryFindDataDeclaration typeName document with
            | Some declaration ->
                let expected =
                    expectedConstructors |> List.map normalizeFixtureText

                let actual =
                    declaration.Constructors
                    |> List.map (fun constructor -> constructor.Name)
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find data declaration '%s' (%s:%d)." typeName filePath lineNumber
        | AssertTraitMembers(traitName, expectedMembers, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            match tryFindTraitDeclaration traitName document with
            | Some declaration ->
                let expected =
                    expectedMembers |> List.map normalizeFixtureText

                let actual =
                    declaration.Members
                    |> List.choose (fun memberDeclaration -> memberDeclaration.Name)
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find trait declaration '%s' (%s:%d)." traitName filePath lineNumber
        | AssertContainsTokenKinds(expectedKinds, filePath, lineNumber) ->
            let lexed = lexFixtureFile filePath

            Assert.Empty(lexed.Diagnostics)

            let actualKinds =
                lexed.Tokens
                |> List.map (fun token -> tokenKindText token.Kind)
                |> List.map normalizeFixtureText

            for expectedKind in expectedKinds do
                let normalizedExpectedKind = normalizeFixtureText expectedKind

                Assert.Contains(
                    normalizedExpectedKind,
                    actualKinds
                )
        | AssertContainsTokenTexts(expectedTexts, filePath, lineNumber) ->
            let lexed = lexFixtureFile filePath

            Assert.Empty(lexed.Diagnostics)

            let actualTexts =
                lexed.Tokens
                |> tokenTexts
                |> List.map normalizeFixtureText

            for expectedText in expectedTexts do
                let normalizedExpectedText = normalizeFixtureText expectedText

                Assert.Contains(
                    normalizedExpectedText,
                    actualTexts
                )
