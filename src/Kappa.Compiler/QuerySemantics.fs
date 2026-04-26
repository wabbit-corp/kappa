namespace Kappa.Compiler

open Kappa.Compiler.ResourceModel
open Kappa.Compiler.TypeSignatures

// Shared parsing and compatibility rules for the spec-level QueryCore surface.
module internal QuerySemantics =
    type QueryUseMode =
        | Reusable
        | OneShot

    type QueryCardinality =
        | QZero
        | QOne
        | QZeroOrOne
        | QOneOrMore
        | QZeroOrMore

    type QueryModeInfo =
        { Use: QueryUseMode
          Card: QueryCardinality }

    type QueryTypeInfo =
        { Mode: QueryModeInfo
          ItemQuantity: ResourceQuantity
          ItemType: TypeExpr
          CaptureSet: Set<string> }

    type QuerySourceInfo =
        { Query: QueryTypeInfo
          SourceDemand: ResourceQuantity }

    let reusableZeroOrMoreMode =
        { Use = Reusable
          Card = QZeroOrMore }

    let oneShotZeroOrMoreMode =
        { Use = OneShot
          Card = QZeroOrMore }

    let private lastSegment nameSegments =
        nameSegments |> List.tryLast

    let private classifyUseName nameSegments =
        match lastSegment nameSegments with
        | Some "Reusable" -> Some Reusable
        | Some "OneShot" -> Some OneShot
        | _ -> None

    let private classifyCardName nameSegments =
        match lastSegment nameSegments with
        | Some "QZero" -> Some QZero
        | Some "QOne" -> Some QOne
        | Some "QZeroOrOne" -> Some QZeroOrOne
        | Some "QOneOrMore" -> Some QOneOrMore
        | Some "QZeroOrMore" -> Some QZeroOrMore
        | _ -> None

    let rec private stripCaptures typeExpr captureSet =
        match typeExpr with
        | TypeCapture(inner, captures) ->
            stripCaptures inner (Set.union captureSet (captures |> Set.ofList))
        | _ ->
            typeExpr, captureSet

    let tryParseQuantityExpr typeExpr =
        match typeExpr with
        | TypeName(nameSegments, []) ->
            match lastSegment nameSegments with
            | Some "0" -> Some ResourceQuantity.zero
            | Some "1" -> Some ResourceQuantity.one
            | Some "ω"
            | Some "omega" -> Some ResourceQuantity.omega
            | Some "<=1" -> Some ResourceQuantity.atMostOne
            | Some ">=1" -> Some ResourceQuantity.atLeastOne
            | Some "&" -> Some(ResourceQuantity.Borrow None)
            | Some text when text.StartsWith("&[") && text.EndsWith("]") && text.Length > 3 ->
                text.Substring(2, text.Length - 3)
                |> Some
                |> ResourceQuantity.Borrow
                |> Some
            | _ -> None
        | _ ->
            None

    let private classifyUseExpr (typeExpr: TypeExpr) =
        match typeExpr with
        | TypeName(nameSegments, []) -> classifyUseName nameSegments
        | _ -> None

    let private classifyCardExpr (typeExpr: TypeExpr) =
        match typeExpr with
        | TypeName(nameSegments, []) -> classifyCardName nameSegments
        | _ -> None

    let tryParseModeExpr typeExpr =
        match typeExpr with
        | TypeName(nameSegments, [ useExpr; cardExpr ]) ->
            match classifyUseName nameSegments, classifyCardName nameSegments with
            | Some useMode, Some card ->
                Some { Use = useMode; Card = card }
            | _ ->
                match lastSegment nameSegments, classifyUseExpr useExpr, classifyCardExpr cardExpr with
                | Some "QueryMode", Some useMode, Some card ->
                    Some { Use = useMode; Card = card }
                | _ ->
                    None
        | _ ->
            None

    let tryParseQueryType normalize typeExpr =
        let normalizedType, captures =
            typeExpr
            |> normalize
            |> fun current -> stripCaptures current Set.empty

        let build mode itemQuantity itemType =
            Some
                { Mode = mode
                  ItemQuantity = itemQuantity
                  ItemType = itemType
                  CaptureSet = captures }

        match normalizedType with
        | TypeName(nameSegments, [ itemType ]) ->
            match lastSegment nameSegments with
            | Some "Query" ->
                build reusableZeroOrMoreMode ResourceQuantity.omega itemType
            | Some "OnceQuery" ->
                build oneShotZeroOrMoreMode ResourceQuantity.omega itemType
            | Some "OptionalQuery" ->
                build
                    { Use = Reusable
                      Card = QZeroOrOne }
                    ResourceQuantity.omega
                    itemType
            | Some "NonEmptyQuery" ->
                build
                    { Use = Reusable
                      Card = QOneOrMore }
                    ResourceQuantity.omega
                    itemType
            | Some "SingletonQuery" ->
                build
                    { Use = Reusable
                      Card = QOne }
                    ResourceQuantity.omega
                    itemType
            | _ ->
                None
        | TypeName(nameSegments, [ modeExpr; itemQuantityExpr; itemType ]) when lastSegment nameSegments = Some "QueryCore" ->
            match tryParseModeExpr modeExpr, tryParseQuantityExpr itemQuantityExpr with
            | Some mode, Some itemQuantity ->
                build mode itemQuantity itemType
            | _ ->
                None
        | _ ->
            None

    let tryInferBuiltinQuerySource normalize typeExpr =
        let build queryType sourceDemand =
            Some
                { Query = queryType
                  SourceDemand = sourceDemand }

        match tryParseQueryType normalize typeExpr with
        | Some queryType ->
            let sourceDemand =
                match queryType.Mode.Use with
                | Reusable -> ResourceQuantity.omega
                | OneShot -> ResourceQuantity.one

            build queryType sourceDemand
        | None ->
            match normalize typeExpr with
            | TypeName(nameSegments, [ itemType ]) ->
                match lastSegment nameSegments with
                | Some "List"
                | Some "Array"
                | Some "Set" ->
                    build
                        { Mode = reusableZeroOrMoreMode
                          ItemQuantity = ResourceQuantity.omega
                          ItemType = itemType
                          CaptureSet = Set.empty }
                        ResourceQuantity.omega
                | Some "Option" ->
                    build
                        { Mode =
                            { Use = Reusable
                              Card = QZeroOrOne }
                          ItemQuantity = ResourceQuantity.omega
                          ItemType = itemType
                          CaptureSet = Set.empty }
                        ResourceQuantity.omega
                | _ ->
                    None
            | _ ->
                None

    let composeUse left right =
        match left, right with
        | Reusable, Reusable -> Reusable
        | _ -> OneShot

    let multiplyCard left right =
        match left, right with
        | QZero, _
        | _, QZero -> QZero
        | QOne, other
        | other, QOne -> other
        | QZeroOrOne, QZeroOrOne -> QZeroOrOne
        | QZeroOrOne, QOneOrMore
        | QOneOrMore, QZeroOrOne
        | QZeroOrOne, QZeroOrMore
        | QZeroOrMore, QZeroOrOne
        | QOneOrMore, QZeroOrMore
        | QZeroOrMore, QOneOrMore
        | QZeroOrMore, QZeroOrMore -> QZeroOrMore
        | QOneOrMore, QOneOrMore -> QOneOrMore

    let filterCard current =
        match current with
        | QZero -> QZero
        | QOne
        | QZeroOrOne -> QZeroOrOne
        | QOneOrMore
        | QZeroOrMore -> QZeroOrMore

    let mayDiscardRows current =
        match current with
        | QZero
        | QZeroOrOne
        | QZeroOrMore -> true
        | QOne
        | QOneOrMore -> false

    let mayDuplicateRows current =
        match current with
        | QOneOrMore
        | QZeroOrMore -> true
        | QZero
        | QOne
        | QZeroOrOne -> false

    let canCheckUse inferred expected =
        match inferred, expected with
        | Reusable, Reusable
        | Reusable, OneShot
        | OneShot, OneShot -> true
        | OneShot, Reusable -> false

    let private cardBounds current =
        match current with
        | QZero -> 0, Some 0
        | QOne -> 1, Some 1
        | QZeroOrOne -> 0, Some 1
        | QOneOrMore -> 1, None
        | QZeroOrMore -> 0, None

    let canCheckCard inferred expected =
        let inferredMin, inferredMax = cardBounds inferred
        let expectedMin, expectedMax = cardBounds expected

        let maxWithin =
            match inferredMax, expectedMax with
            | Some actualMax, Some expectedMax -> actualMax <= expectedMax
            | Some _, None -> true
            | None, Some _ -> false
            | None, None -> true

        expectedMin <= inferredMin && maxWithin

    let canCheckItemQuantity inferred expected =
        ResourceQuantity.satisfies inferred expected
