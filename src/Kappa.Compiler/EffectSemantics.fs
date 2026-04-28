namespace Kappa.Compiler

open System
open System.Security.Cryptography
open System.Text

type EffectSemanticOperation =
    { OperationId: string
      Name: string
      ResumptionQuantity: Quantity option
      SignatureTokens: Token list
      ParameterArity: int }

type EffectSemanticDeclaration =
    { VisibleName: string
      InterfaceId: string
      LabelId: string
      HeaderTokens: Token list
      Operations: EffectSemanticOperation list }

module internal EffectSemantics =
    let private stableId (prefix: string) (seed: string) =
        let bytes: byte array = Encoding.UTF8.GetBytes(seed)
        let hash: byte array = SHA256.HashData(bytes)
        let suffix = Convert.ToHexString(hash.AsSpan(0, 16)).ToLowerInvariant()

        $"{prefix}:{suffix}"

    let private topLevelSeed moduleName declarationName =
        $"top|{moduleName}|{declarationName}"

    let private localSeed identitySeed declarationName =
        $"local|{identitySeed}|{declarationName}"

    let private effectOperationId interfaceId operationName =
        stableId "effect-op" $"{interfaceId}|{operationName}"

    let private effectInterfaceId seed =
        stableId "effect-interface" seed

    let private effectLabelId seed =
        stableId "effect-label" seed

    let private effectOperationParameterArity signatureTokens =
        signatureTokens
        |> TypeSignatures.parseType
        |> Option.map TypeSignatures.functionParts
        |> Option.map (fun (parameterTypes, _) -> List.length parameterTypes)
        |> Option.defaultValue 0

    let ensureTopLevel moduleName (declaration: EffectDeclaration) =
        let seed = topLevelSeed moduleName declaration.Name
        let interfaceId = declaration.EffectInterfaceId |> Option.defaultValue (effectInterfaceId seed)
        let labelId = declaration.EffectLabelId |> Option.defaultValue (effectLabelId seed)

        let operations =
            declaration.Operations
            |> List.map (fun operation ->
                { operation with
                    OperationId =
                        operation.OperationId
                        |> Option.orElseWith (fun () -> Some(effectOperationId interfaceId operation.Name)) })

        { declaration with
            EffectInterfaceId = Some interfaceId
            EffectLabelId = Some labelId
            Operations = operations }

    let ensureLocal identitySeed (declaration: EffectDeclaration) =
        let seed = localSeed identitySeed declaration.Name
        let interfaceId = declaration.EffectInterfaceId |> Option.defaultValue (effectInterfaceId seed)
        let labelId = declaration.EffectLabelId |> Option.defaultValue (effectLabelId seed)

        let operations =
            declaration.Operations
            |> List.map (fun operation ->
                { operation with
                    OperationId =
                        operation.OperationId
                        |> Option.orElseWith (fun () -> Some(effectOperationId interfaceId operation.Name)) })

        { declaration with
            EffectInterfaceId = Some interfaceId
            EffectLabelId = Some labelId
            Operations = operations }

    let toSemantic (declaration: EffectDeclaration) =
        let interfaceId =
            declaration.EffectInterfaceId
            |> Option.defaultWith (fun () ->
                invalidOp $"Effect declaration '{declaration.Name}' is missing an interface identity.")

        let labelId =
            declaration.EffectLabelId
            |> Option.defaultWith (fun () ->
                invalidOp $"Effect declaration '{declaration.Name}' is missing a label identity.")

        { VisibleName = declaration.Name
          InterfaceId = interfaceId
          LabelId = labelId
          HeaderTokens = declaration.HeaderTokens
          Operations =
            declaration.Operations
            |> List.map (fun operation ->
                { OperationId =
                    operation.OperationId
                    |> Option.defaultWith (fun () ->
                        invalidOp $"Effect operation '{declaration.Name}.{operation.Name}' is missing an operation identity.")
                  Name = operation.Name
                  ResumptionQuantity = operation.ResumptionQuantity
                  SignatureTokens = operation.SignatureTokens
                  ParameterArity = effectOperationParameterArity operation.SignatureTokens }) }

    let importAs localName (declaration: EffectDeclaration) =
        { declaration with Name = localName }

    let tryFindOperation operationName (declaration: EffectSemanticDeclaration) =
        declaration.Operations
        |> List.tryFind (fun operation -> String.Equals(operation.Name, operationName, StringComparison.Ordinal))

    let defaultResumptionQuantity quantity =
        quantity |> Option.defaultValue QuantityOne

    let quantityPermitsMultipleResumptions quantity =
        match defaultResumptionQuantity quantity with
        | QuantityOmega
        | QuantityAtLeastOne
        | QuantityVariable _ ->
            true
        | QuantityZero
        | QuantityOne
        | QuantityBorrow _
        | QuantityAtMostOne ->
            false
