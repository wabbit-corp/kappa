namespace Kappa.Compiler

open System
open TypeSignatures

// Compiler-known lowering and trait-helper semantics for ordinary std.prelude surface.
// This is distinct from true compiler/runtime intrinsic ABI contracts.
module internal KnownPreludeSemantics =
    type RuntimeSpecialCallSpec =
        { RuntimeArity: int
          ReturnsBoolean: bool }

    type BuiltinPreludeTraitMemberLowering =
        | ForwardToBuiltinBinaryOperator of string
        | ForwardToIntrinsicTerm of string

    type BuiltinPreludeTraitInstanceSpec =
        { Trait: TraitReference
          HeadTypeIdentity: TypeIdentity
          MemberLowerings: (string * BuiltinPreludeTraitMemberLowering) list }
        member this.TraitIdentity =
            this.Trait
            |> TraitReference.identity
            |> Option.defaultWith (fun () -> invalidOp "Builtin prelude trait specs must carry semantic identity.")

        member this.TraitName =
            this.Trait
            |> TraitReference.localName
            |> ReferenceName.text

    let preludeModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Prelude
    let hashModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Hash

    let private preludeTraitIdentity name =
        DeclarationIdentity.topLevel preludeModuleIdentity name TraitDeclaration

    let private preludeTraitReference name =
        TraitReference.unqualified name
        |> TraitReference.attachIdentity (preludeTraitIdentity name)

    let private preludeTypeIdentity name =
        TypeIdentity.topLevel preludeModuleIdentity name

    let private hashTypeIdentity name =
        TypeIdentity.topLevel hashModuleIdentity name

    [<Literal>]
    let BuiltinPreludeShowHelperName = "__kappa_builtin_show"

    [<Literal>]
    let BuiltinPreludeCompareHelperName = "__kappa_builtin_compare"

    let IsTraitReference = preludeTraitReference CompilerKnownSymbols.KnownTypeNames.IsTrait
    let IsPropTraitReference = preludeTraitReference CompilerKnownSymbols.KnownTypeNames.IsProp
    let EqTraitReference = preludeTraitReference "Eq"
    let OrdTraitReference = preludeTraitReference "Ord"
    let ShowTraitReference = preludeTraitReference "Show"
    let RangeableTraitReference = preludeTraitReference "Rangeable"
    let LacksRecTraitReference = preludeTraitReference "LacksRec"

    let resolvedTraitMatches expectedTraitReference actualTraitReference =
        match TraitReference.identity expectedTraitReference, TraitReference.identity actualTraitReference with
        | Some expectedIdentity, Some actualIdentity -> expectedIdentity = actualIdentity
        | _ -> false

    let traitNameMatches expectedTraitReference nameSegments =
        let localName =
            expectedTraitReference
            |> TraitReference.localName
            |> ReferenceName.text

        nameSegments = [ localName ] || nameSegments = (CompilerKnownSymbols.KnownModules.Prelude @ [ localName ])

    let knownPreludeTraitConstraint traitReference arguments =
        { Trait = traitReference
          Arguments = arguments }

    let binaryOperatorNames =
        Set.ofList [ "+"; "-"; "*"; "/"; "=="; "!="; "<"; "<="; ">"; ">="; "&&"; "||"; "is" ]

    let runtimeSpecialBinaryOperatorNames =
        Set.ofList [ "+"; "-"; "*"; "/"; "=="; "!="; "&&"; "||"; "is" ]

    let private booleanRuntimeSpecialBinaryOperatorNames =
        Set.ofList [ "=="; "!="; "&&"; "||"; "is" ]

    let shortCircuitBinaryOperatorNames =
        Set.ofList [ "&&"; "||" ]

    let eagerBuiltinBinaryOperatorNames =
        Set.difference binaryOperatorNames shortCircuitBinaryOperatorNames

    let private eqBuiltinPreludeHeadTypeIdentities =
        [ preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Unit
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Bool
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Byte
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Bytes
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.UnicodeScalar
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Grapheme
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.String
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Int
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Nat
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Integer
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Double
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Real
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Ordering
          hashTypeIdentity CompilerKnownSymbols.KnownTypeNames.HashCode ]

    let private ordBuiltinPreludeHeadTypeIdentities =
        [ preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Bool
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Byte
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Bytes
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.UnicodeScalar
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.String
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Int
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Nat
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Integer
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Double
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Real
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Ordering
          hashTypeIdentity CompilerKnownSymbols.KnownTypeNames.HashCode ]

    let private showBuiltinPreludeHeadTypeIdentities =
        [ preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Unit
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Bool
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Byte
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Bytes
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.UnicodeScalar
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Grapheme
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.String
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Int
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Nat
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Integer
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Double
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Real
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Ordering ]

    let private rangeableBuiltinPreludeHeadTypeIdentities =
        [ preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Nat
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Int
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.Integer
          preludeTypeIdentity CompilerKnownSymbols.KnownTypeNames.UnicodeScalar ]

    let private builtinPreludeTraitInstanceSpecsValue =
        lazy
            ([ for headTypeIdentity in eqBuiltinPreludeHeadTypeIdentities do
                   { Trait = EqTraitReference
                     HeadTypeIdentity = headTypeIdentity
                     MemberLowerings = [ "==", ForwardToBuiltinBinaryOperator "==" ] }
               for headTypeIdentity in ordBuiltinPreludeHeadTypeIdentities do
                   { Trait = OrdTraitReference
                     HeadTypeIdentity = headTypeIdentity
                     MemberLowerings = [ "compare", ForwardToIntrinsicTerm BuiltinPreludeCompareHelperName ] }
               for headTypeIdentity in showBuiltinPreludeHeadTypeIdentities do
                   { Trait = ShowTraitReference
                     HeadTypeIdentity = headTypeIdentity
                     MemberLowerings = [ "show", ForwardToIntrinsicTerm BuiltinPreludeShowHelperName ] } ]
             |> List.sortBy (fun spec ->
                spec.TraitName,
                 ((TypeIdentity.moduleIdentity spec.HeadTypeIdentity |> ModuleIdentity.segments) @ [ TypeIdentity.name spec.HeadTypeIdentity ])
                 |> String.concat "."))

    let builtinPreludeTraitInstanceSpecs () =
        builtinPreludeTraitInstanceSpecsValue.Value

    let private tryCanonicalBuiltinPreludeHeadTypeText
        (canonicalIdentities: TypeIdentity list)
        (normalizedTypeExpr: TypeExpr)
        =
        match normalizedTypeExpr with
        | TypeName(typeReference, []) ->
            let nameSegments = typeReference |> TypeReference.segments

            let matchesTopLevelNameSegments canonicalIdentity =
                let bareName = [ TypeIdentity.name canonicalIdentity ]
                let qualifiedName =
                    (TypeIdentity.moduleIdentity canonicalIdentity |> ModuleIdentity.segments) @ bareName

                nameSegments = bareName || nameSegments = qualifiedName

            canonicalIdentities
            |> List.tryFind matchesTopLevelNameSegments
            |> Option.map TypeIdentity.name
        | _ ->
            None

    let tryCanonicalBuiltinPreludeEqHeadTypeText normalizedTypeExpr =
        tryCanonicalBuiltinPreludeHeadTypeText eqBuiltinPreludeHeadTypeIdentities normalizedTypeExpr

    let tryCanonicalBuiltinPreludeOrdHeadTypeText normalizedTypeExpr =
        tryCanonicalBuiltinPreludeHeadTypeText ordBuiltinPreludeHeadTypeIdentities normalizedTypeExpr

    let tryCanonicalBuiltinPreludeShowHeadTypeText normalizedTypeExpr =
        tryCanonicalBuiltinPreludeHeadTypeText showBuiltinPreludeHeadTypeIdentities normalizedTypeExpr

    let tryCanonicalBuiltinPreludeRangeableHeadTypeText normalizedTypeExpr =
        tryCanonicalBuiltinPreludeHeadTypeText rangeableBuiltinPreludeHeadTypeIdentities normalizedTypeExpr

    let hiddenRuntimeHelperTermNames =
        builtinPreludeTraitInstanceSpecs ()
        |> List.collect (fun spec ->
            spec.MemberLowerings
            |> List.choose (fun (_, lowering) ->
                match lowering with
                | ForwardToIntrinsicTerm intrinsicName -> Some intrinsicName
                | ForwardToBuiltinBinaryOperator _ -> None))
        |> Set.ofList

    let private elaborationAvailablePreludeTermNames =
        Set.ofList
            [ "f"
              "re"
              "b"
              "type"
              "failElab"
              "failElabWith"
              "warnElab"
              "warnElabWith" ]

    let elaborationAvailableTermNames bundledPreludeExpectTermNames =
        Set.intersect bundledPreludeExpectTermNames elaborationAvailablePreludeTermNames

    let isBuiltinBinaryOperator name =
        binaryOperatorNames.Contains name

    let isShortCircuitBinaryOperator name =
        shortCircuitBinaryOperatorNames.Contains name

    let isEagerBuiltinBinaryOperator name =
        eagerBuiltinBinaryOperatorNames.Contains name

    let tryRuntimeSpecialBinaryOperatorSpec name =
        if runtimeSpecialBinaryOperatorNames.Contains name then
            Some
                { RuntimeArity = 2
                  ReturnsBoolean = booleanRuntimeSpecialBinaryOperatorNames.Contains name }
        else
            None
