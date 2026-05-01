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
        { TraitName: string
          HeadTypeIdentity: TypeIdentity
          MemberLowerings: (string * BuiltinPreludeTraitMemberLowering) list }

    let preludeModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Prelude
    let hashModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Hash

    let private preludeTypeIdentity name =
        TypeIdentity.topLevel preludeModuleIdentity name

    let private hashTypeIdentity name =
        TypeIdentity.topLevel hashModuleIdentity name

    [<Literal>]
    let BuiltinPreludeShowHelperName = "__kappa_builtin_show"

    [<Literal>]
    let BuiltinPreludeCompareHelperName = "__kappa_builtin_compare"

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
                   { TraitName = "Eq"
                     HeadTypeIdentity = headTypeIdentity
                     MemberLowerings = [ "==", ForwardToBuiltinBinaryOperator "==" ] }
               for headTypeIdentity in ordBuiltinPreludeHeadTypeIdentities do
                   { TraitName = "Ord"
                     HeadTypeIdentity = headTypeIdentity
                     MemberLowerings = [ "compare", ForwardToIntrinsicTerm BuiltinPreludeCompareHelperName ] }
               for headTypeIdentity in showBuiltinPreludeHeadTypeIdentities do
                   { TraitName = "Show"
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
        | TypeName(nameSegments, []) ->
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
