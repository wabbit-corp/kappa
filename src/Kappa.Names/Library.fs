namespace Kappa.Compiler

open System

[<StructuralEquality; StructuralComparison>]
type ModuleIdentity = private ModuleIdentity of string list

module ModuleIdentity =
    let ofSegments segments =
        match segments with
        | [] ->
            invalidArg (nameof segments) "Module identities must contain at least one segment."
        | _ ->
            ModuleIdentity segments

    let ofOptionalSegments segments =
        segments |> Option.map ofSegments

    let ofDottedTextUnchecked (text: string) =
        if String.IsNullOrWhiteSpace(text) then
            invalidArg (nameof text) "Module identity text must not be blank."

        text.Split('.', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> ofSegments

    let segments (ModuleIdentity segments) = segments

    let text identity =
        identity |> segments |> String.concat "."

    let asciiCaseFoldKey identity =
        identity |> segments |> List.map (fun segment -> segment.ToLowerInvariant())

[<StructuralEquality; StructuralComparison>]
type TypeIdentity = private TypeIdentity of ModuleIdentity * string list * string

module TypeIdentity =
    let create moduleIdentity scopePath name =
        if String.IsNullOrWhiteSpace(name) then
            invalidArg (nameof name) "Type identities must have a non-blank name."

        TypeIdentity(moduleIdentity, scopePath, name)

    let topLevel moduleIdentity name = create moduleIdentity [] name

    let ofDottedTextUnchecked moduleText name =
        topLevel (ModuleIdentity.ofDottedTextUnchecked moduleText) name

    let moduleIdentity (TypeIdentity(moduleIdentity, _, _)) = moduleIdentity
    let scopePath (TypeIdentity(_, scopePath, _)) = scopePath
    let name (TypeIdentity(_, _, name)) = name

    let hasTopLevelName expectedModuleIdentity expectedName identity =
        expectedModuleIdentity = moduleIdentity identity
        && List.isEmpty (scopePath identity)
        && String.Equals(name identity, expectedName, StringComparison.Ordinal)

[<StructuralEquality; StructuralComparison>]
type DeclarationKind =
    | TermDeclaration
    | ConstructorDeclaration
    | TypeAliasDeclaration
    | TypeFacetDeclaration
    | TraitDeclaration
    | TraitInstanceDeclaration
    | ProjectionDeclaration
    | EffectDeclaration
    | ModuleDeclaration

[<StructuralEquality; StructuralComparison>]
type SemanticObjectKind =
    | TypeObject
    | TraitObject
    | EffectLabelObject
    | ModuleObject
    | ProjectionObject

[<StructuralEquality; StructuralComparison>]
type DeclarationIdentity = private DeclarationIdentity of ModuleIdentity * string list * string * DeclarationKind

module DeclarationIdentity =
    let create moduleIdentity scopePath name kind =
        if String.IsNullOrWhiteSpace(name) then
            invalidArg (nameof name) "Declaration identities must have a non-blank name."

        DeclarationIdentity(moduleIdentity, scopePath, name, kind)

    let topLevel moduleIdentity name kind = create moduleIdentity [] name kind
    let moduleIdentity (DeclarationIdentity(moduleIdentity, _, _, _)) = moduleIdentity
    let scopePath (DeclarationIdentity(_, scopePath, _, _)) = scopePath
    let name (DeclarationIdentity(_, _, name, _)) = name
    let kind (DeclarationIdentity(_, _, _, kind)) = kind

    let canonicalText identity =
        ((moduleIdentity identity |> ModuleIdentity.segments) @ scopePath identity @ [ name identity ])
        |> String.concat "."

[<StructuralEquality; StructuralComparison>]
type SemanticObjectIdentity = private SemanticObjectIdentity of DeclarationIdentity * SemanticObjectKind

module SemanticObjectIdentity =
    let create declarationIdentity kind =
        SemanticObjectIdentity(declarationIdentity, kind)

    let declarationIdentity (SemanticObjectIdentity(declarationIdentity, _)) = declarationIdentity
    let kind (SemanticObjectIdentity(_, kind)) = kind

module NamesModel =
    [<Struct>]
    type LocalId =
        | LocalId of uint64

    [<Struct>]
    type ScopeId =
        | ScopeId of uint64

    [<Struct>]
    type BindingGroupId =
        | BindingGroupId of uint64

    [<Struct>]
    type OriginId =
        | OriginId of uint64

    [<Struct>]
    type ImportEnvId =
        | ImportEnvId of uint64

    [<StructuralEquality; StructuralComparison>]
    type SemanticObjectId = private SemanticObjectId of string

    module SemanticObjectId =
        let create value =
            if String.IsNullOrWhiteSpace(value) then
                invalidArg (nameof value) "Semantic object identities must not be blank."

            SemanticObjectId value

        let text (SemanticObjectId value) = value

    [<StructuralEquality; StructuralComparison>]
    type Spelling =
        | Ident of string
        | BacktickIdent of string
        | Operator of string

    module Spelling =
        let text spelling =
            match spelling with
            | Ident value
            | BacktickIdent value
            | Operator value -> value

    [<StructuralEquality; StructuralComparison>]
    type DeclKind =
        | Term
        | Type
        | Trait
        | Ctor
        | Module
        | EffectLabel

    [<StructuralEquality; StructuralComparison>]
    type SpecialFacetKind =
        | ReifiedStaticObject of primary: DeclKind
        | ProjectionSelector
        | AccessorBundle

    [<StructuralEquality; StructuralComparison>]
    type FacetKind =
        | Declaration of DeclKind
        | Special of SpecialFacetKind

    [<StructuralEquality; StructuralComparison>]
    type PatternEligibility =
        | NotPatternHead
        | ActivePatternHead

    [<StructuralEquality; StructuralComparison>]
    type BindingVisibility =
        | Public
        | Private

    [<StructuralEquality; StructuralComparison>]
    type BindingTransparency =
        | Transparent
        | Opaque

    [<StructuralEquality; StructuralComparison>]
    type NameTarget =
        | LocalTarget of LocalId
        | SemanticTarget of SemanticObjectId

    [<StructuralEquality; StructuralComparison>]
    type BindingGroupKind =
        | OrdinaryGroup
        | SameSpellingDataFamily of typeTarget: NameTarget * ctorTarget: NameTarget
        | ImportAliasGroup of importId: Guid

    type BindingFacet =
        { FacetKind: FacetKind
          Target: NameTarget
          Pattern: PatternEligibility
          ReifiedOf: NameTarget option
          PairedWith: NameTarget option
          Visibility: BindingVisibility
          Transparency: BindingTransparency
          Origin: OriginId }

    type BindingGroup =
        { Id: BindingGroupId
          Spelling: Spelling
          Scope: ScopeId
          Kind: BindingGroupKind
          Facets: BindingFacet list
          Origin: OriginId }
