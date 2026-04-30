namespace Kappa.Compiler

open System
open System.Security.Cryptography
open System.Text

// Lowers KCore into the implementation-defined KRuntimeIR checkpoint.
module internal KRuntimeLowering =
    let private stableSyntheticSuffix (value: string) =
        let bytes = Encoding.UTF8.GetBytes(value)
        let hash = SHA256.HashData(bytes)

        hash
        |> Seq.take 10
        |> Seq.map (fun value -> value.ToString("x2"))
        |> String.concat ""

    let private syntheticRuntimeRecordTypeName recordKey =
        "__kappa_record_" + stableSyntheticSuffix recordKey

    let private tokensText (tokens: Token list) =
        tokens
        |> List.map (fun token -> token.Text)
        |> String.concat " "

    let private tryParseTypeText text =
        let source = SourceText.From("__runtime_type__.kp", text)
        let lexResult = Lexer.tokenize source

        if not (List.isEmpty lexResult.Diagnostics) then
            None
        else
            TypeSignatures.parseType lexResult.Tokens

    let private isIntrinsicCompileTimeTraitName nameSegments =
        match nameSegments with
        | [ "IsProp" ]
        | [ "IsTrait" ]
        | [ "std"; "prelude"; "IsProp" ]
        | [ "std"; "prelude"; "IsTrait" ] ->
            true
        | _ ->
            false

    let rec private eraseRuntimeTypeExpr typeExpr =
        match typeExpr with
        | TypeSignatures.TypeLevelLiteral _ ->
            typeExpr
        | TypeSignatures.TypeUniverse _
        | TypeSignatures.TypeIntrinsic _ ->
            TypeSignatures.TypeName([ "Unit" ], [])
        | TypeSignatures.TypeApply(callee, arguments) ->
            TypeSignatures.TypeApply(eraseRuntimeTypeExpr callee, arguments |> List.map eraseRuntimeTypeExpr)
        | TypeSignatures.TypeLambda(parameterName, parameterSort, body) ->
            TypeSignatures.TypeLambda(parameterName, eraseRuntimeTypeExpr parameterSort, eraseRuntimeTypeExpr body)
        | TypeSignatures.TypeDelay inner ->
            TypeSignatures.TypeDelay(eraseRuntimeTypeExpr inner)
        | TypeSignatures.TypeMemo inner ->
            TypeSignatures.TypeMemo(eraseRuntimeTypeExpr inner)
        | TypeSignatures.TypeForce inner ->
            TypeSignatures.TypeForce(eraseRuntimeTypeExpr inner)
        | TypeSignatures.TypeProject(target, fieldName) ->
            TypeSignatures.TypeProject(eraseRuntimeTypeExpr target, fieldName)
        | TypeSignatures.TypeVariable name ->
            TypeSignatures.TypeVariable name
        | TypeSignatures.TypeName(nameSegments, arguments) ->
            match nameSegments with
            | [ "Type" ]
            | [ "Universe" ]
            | [ "Constraint" ]
            | [ "Quantity" ]
            | [ "Region" ]
            | [ "RecRow" ]
            | [ "VarRow" ]
            | [ "EffRow" ]
            | [ "Label" ]
            | [ "EffLabel" ]
            | [ "Syntax" ]
            | [ "Code" ]
            | [ "std"; "prelude"; "Syntax" ]
            | [ "std"; "prelude"; "Code" ] ->
                TypeSignatures.TypeName([ "Unit" ], [])
            | _ when isIntrinsicCompileTimeTraitName nameSegments ->
                TypeSignatures.TypeName([ "Unit" ], [])
            | _ ->
                TypeSignatures.TypeName(nameSegments, arguments |> List.map eraseRuntimeTypeExpr)
        | TypeSignatures.TypeArrow(_, parameterType, resultType) ->
            TypeSignatures.TypeArrow(QuantityOmega, eraseRuntimeTypeExpr parameterType, eraseRuntimeTypeExpr resultType)
        | TypeSignatures.TypeEquality(left, _) ->
            eraseRuntimeTypeExpr left
        | TypeSignatures.TypeCapture(inner, _) ->
            eraseRuntimeTypeExpr inner
        | TypeSignatures.TypeEffectRow(entries, tail) ->
            TypeSignatures.TypeEffectRow(
                entries
                |> List.map (fun entry ->
                    { Label = eraseRuntimeTypeExpr entry.Label
                      Effect = eraseRuntimeTypeExpr entry.Effect }),
                tail |> Option.map eraseRuntimeTypeExpr
            )
        | TypeSignatures.TypeRecord fields ->
            fields
            |> List.choose (fun field ->
                if field.Quantity = QuantityZero then
                    None
                else
                    Some
                        { field with
                            Quantity = QuantityOmega
                            Type = eraseRuntimeTypeExpr field.Type })
            |> TypeSignatures.TypeRecord
        | TypeSignatures.TypeUnion members ->
            TypeSignatures.TypeUnion(members |> List.map eraseRuntimeTypeExpr)

    let private runtimeValueTypeExpr typeExpr =
        match eraseRuntimeTypeExpr typeExpr with
        | TypeSignatures.TypeName([ "IO" ], [ inner ]) -> inner
        | TypeSignatures.TypeName([ "IO" ], [ _; inner ]) -> inner
        | TypeSignatures.TypeName([ "UIO" ], [ inner ]) -> inner
        | other -> other

    let private eraseRuntimeTypeText (text: string) =
        match tryParseTypeText text with
        | Some parsed ->
            parsed |> eraseRuntimeTypeExpr |> TypeSignatures.toText
        | None ->
            text

    let private runtimeValueTypeText (text: string) =
        match tryParseTypeText text with
        | Some parsed ->
            parsed |> runtimeValueTypeExpr |> TypeSignatures.toText
        | None ->
            text

    let private normalizeTypeTokens (tokens: Token list) =
        tokens
        |> TypeSignatures.parseType
        |> Option.map (eraseRuntimeTypeExpr >> TypeSignatures.toText)
        |> Option.defaultValue (tokensText tokens)

    let private significantTokens (tokens: Token list) =
        tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)

    let private splitLeadingTypeArgumentTokens argumentCount tokens =
        let tokens = significantTokens tokens

        let takeAtom remaining =
            match remaining with
            | [] ->
                [], []
            | first :: _ when first.Kind = LeftParen ->
                let tokenArray = remaining |> List.toArray
                let mutable depth = 0
                let mutable index = 0
                let mutable keepReading = true

                while keepReading && index < tokenArray.Length do
                    match tokenArray[index].Kind with
                    | LeftParen ->
                        depth <- depth + 1
                    | RightParen ->
                        depth <- depth - 1

                        if depth = 0 then
                            keepReading <- false
                    | _ ->
                        ()

                    index <- index + 1

                List.ofArray tokenArray[0 .. index - 1], List.ofArray tokenArray[index ..]
            | first :: rest ->
                [ first ], rest

        let rec loop remaining count groups =
            if count <= 1 then
                List.rev (remaining :: groups)
            else
                let group, rest = takeAtom remaining
                loop rest (count - 1) (group :: groups)

        loop tokens (max 1 argumentCount) []

    let private lowerRuntimeParameter (parameter: KCoreParameter) : KRuntimeParameter =
        { Name = parameter.Name
          TypeText = parameter.TypeText |> Option.map eraseRuntimeTypeText }

    let private isCompileTimeParameterType parameterType =
        match parameterType with
        | TypeSignatures.TypeUniverse _
        | TypeSignatures.TypeIntrinsic TypeSignatures.UniverseClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.ConstraintClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.QuantityClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.RegionClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.RecRowClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.VarRowClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.EffRowClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.LabelClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.EffLabelClassifier
        | TypeSignatures.TypeName([ "Type" ], [])
        | TypeSignatures.TypeName([ "Constraint" ], [])
        | TypeSignatures.TypeName([ "Quantity" ], [])
        | TypeSignatures.TypeName([ "Region" ], [])
        | TypeSignatures.TypeName([ "RecRow" ], [])
        | TypeSignatures.TypeName([ "VarRow" ], [])
        | TypeSignatures.TypeName([ "EffRow" ], [])
        | TypeSignatures.TypeName([ "Label" ], [])
        | TypeSignatures.TypeName([ "EffLabel" ], [])
        | TypeSignatures.TypeName([ "Syntax" ], _)
        | TypeSignatures.TypeName([ "Code" ], _)
        | TypeSignatures.TypeName([ "std"; "prelude"; "Syntax" ], _)
        | TypeSignatures.TypeName([ "std"; "prelude"; "Code" ], _) ->
            true
        | TypeSignatures.TypeName(nameSegments, _) when isIntrinsicCompileTimeTraitName nameSegments ->
            true
        | _ ->
            false

    let private isCompileTimeKCoreParameter (parameter: KCoreParameter) =
        match parameter.Quantity with
        | Some QuantityZero ->
            true
        | _ ->
            parameter.Type |> Option.exists isCompileTimeParameterType

    let private filterRuntimeParameters (parameters: KCoreParameter list) =
        parameters |> List.filter (isCompileTimeKCoreParameter >> not)

    let rec private typeExprContainsCompileTimeOnly typeExpr =
        if isCompileTimeParameterType typeExpr then
            true
        else
            match typeExpr with
            | TypeSignatures.TypeApply(callee, arguments) ->
                typeExprContainsCompileTimeOnly callee || (arguments |> List.exists typeExprContainsCompileTimeOnly)
            | TypeSignatures.TypeLambda(_, parameterSort, body) ->
                typeExprContainsCompileTimeOnly parameterSort || typeExprContainsCompileTimeOnly body
            | TypeSignatures.TypeDelay inner
            | TypeSignatures.TypeMemo inner
            | TypeSignatures.TypeForce inner ->
                typeExprContainsCompileTimeOnly inner
            | TypeSignatures.TypeProject(target, _) ->
                typeExprContainsCompileTimeOnly target
            | TypeSignatures.TypeName(_, arguments) ->
                arguments |> List.exists typeExprContainsCompileTimeOnly
            | TypeSignatures.TypeArrow(_, parameterType, resultType) ->
                typeExprContainsCompileTimeOnly parameterType || typeExprContainsCompileTimeOnly resultType
            | TypeSignatures.TypeEquality(left, right) ->
                typeExprContainsCompileTimeOnly left || typeExprContainsCompileTimeOnly right
            | TypeSignatures.TypeCapture(inner, _) ->
                typeExprContainsCompileTimeOnly inner
            | TypeSignatures.TypeEffectRow(entries, tail) ->
                entries
                |> List.exists (fun entry ->
                    typeExprContainsCompileTimeOnly entry.Label
                    || typeExprContainsCompileTimeOnly entry.Effect)
                || tail |> Option.exists typeExprContainsCompileTimeOnly
            | TypeSignatures.TypeRecord fields ->
                fields |> List.exists (fun field -> typeExprContainsCompileTimeOnly field.Type)
            | TypeSignatures.TypeUnion members ->
                members |> List.exists typeExprContainsCompileTimeOnly
            | TypeSignatures.TypeLevelLiteral _
            | TypeSignatures.TypeUniverse _
            | TypeSignatures.TypeIntrinsic _
            | TypeSignatures.TypeVariable _ ->
                false

    let rec private typeExprReferencesCompileTimeTrait
        (compileTimeTraitNames: Set<string>)
        typeExpr
        =
        let referencesChildren =
            match typeExpr with
            | TypeSignatures.TypeApply(callee, arguments) ->
                typeExprReferencesCompileTimeTrait compileTimeTraitNames callee
                || (arguments |> List.exists (typeExprReferencesCompileTimeTrait compileTimeTraitNames))
            | TypeSignatures.TypeLambda(_, parameterSort, body) ->
                typeExprReferencesCompileTimeTrait compileTimeTraitNames parameterSort
                || typeExprReferencesCompileTimeTrait compileTimeTraitNames body
            | TypeSignatures.TypeDelay inner
            | TypeSignatures.TypeMemo inner
            | TypeSignatures.TypeForce inner ->
                typeExprReferencesCompileTimeTrait compileTimeTraitNames inner
            | TypeSignatures.TypeProject(target, _) ->
                typeExprReferencesCompileTimeTrait compileTimeTraitNames target
            | TypeSignatures.TypeArrow(_, parameterType, resultType) ->
                typeExprReferencesCompileTimeTrait compileTimeTraitNames parameterType
                || typeExprReferencesCompileTimeTrait compileTimeTraitNames resultType
            | TypeSignatures.TypeEquality(left, right) ->
                typeExprReferencesCompileTimeTrait compileTimeTraitNames left
                || typeExprReferencesCompileTimeTrait compileTimeTraitNames right
            | TypeSignatures.TypeCapture(inner, _) ->
                typeExprReferencesCompileTimeTrait compileTimeTraitNames inner
            | TypeSignatures.TypeEffectRow(entries, tail) ->
                entries
                |> List.exists (fun entry ->
                    typeExprReferencesCompileTimeTrait compileTimeTraitNames entry.Label
                    || typeExprReferencesCompileTimeTrait compileTimeTraitNames entry.Effect)
                || tail |> Option.exists (typeExprReferencesCompileTimeTrait compileTimeTraitNames)
            | TypeSignatures.TypeRecord fields ->
                fields |> List.exists (fun field -> typeExprReferencesCompileTimeTrait compileTimeTraitNames field.Type)
            | TypeSignatures.TypeUnion members ->
                members |> List.exists (typeExprReferencesCompileTimeTrait compileTimeTraitNames)
            | TypeSignatures.TypeName(_, arguments) ->
                arguments |> List.exists (typeExprReferencesCompileTimeTrait compileTimeTraitNames)
            | TypeSignatures.TypeLevelLiteral _
            | TypeSignatures.TypeUniverse _
            | TypeSignatures.TypeIntrinsic _
            | TypeSignatures.TypeVariable _ ->
                false

        let referencesSelf =
            match typeExpr with
            | TypeSignatures.TypeName(([ "Dict" ] | [ "std"; "prelude"; "Dict" ]), [ constraintType ]) ->
                match constraintType with
                | TypeSignatures.TypeName(traitNameSegments, _) ->
                    compileTimeTraitNames.Contains(List.last traitNameSegments)
                | _ ->
                    false
            | TypeSignatures.TypeName([ dictionaryTypeName ], _) ->
                compileTimeTraitNames
                |> Seq.exists (fun traitName ->
                    String.Equals(dictionaryTypeName, TraitRuntime.dictionaryTypeName traitName, StringComparison.Ordinal))
            | _ ->
                false

        referencesSelf || referencesChildren

    let private schemeRequiresRuntimeErasure
        (compileTimeTraitNames: Set<string>)
        (scheme: TypeSignatures.TypeScheme)
        =
        let touches typeExpr =
            typeExprContainsCompileTimeOnly typeExpr
            || typeExprReferencesCompileTimeTrait compileTimeTraitNames typeExpr

        let parameterTypes, resultType = TypeSignatures.schemeParts scheme

        touches resultType
        || (parameterTypes |> List.exists touches)
        || (scheme.Constraints
            |> List.exists (fun constraintInfo ->
                Set.contains constraintInfo.TraitName compileTimeTraitNames
                || (constraintInfo.Arguments |> List.exists touches)))

    let private tryParseTraitMemberScheme (memberDeclaration: TraitMember) =
        let rawTokens = significantTokens memberDeclaration.Tokens
        let tokens =
            match rawTokens with
            | { Kind = IntegerLiteral; Text = "0" } :: rest ->
                rest
            | { Kind = IntegerLiteral; Text = "1" } :: rest ->
                rest
            | { Kind = Operator; Text = "<=" } :: { Kind = IntegerLiteral; Text = "1" } :: rest ->
                rest
            | { Kind = Operator; Text = ">=" } :: { Kind = IntegerLiteral; Text = "1" } :: rest ->
                rest
            | { Kind = Operator; Text = "&" } :: { Kind = LeftBracket } :: regionToken :: { Kind = RightBracket } :: rest
                when Token.isName regionToken ->
                rest
            | { Kind = Operator; Text = "&" } :: rest ->
                rest
            | head :: rest
                when Token.isName head
                     && (String.Equals(SyntaxFacts.trimIdentifierQuotes head.Text, "\u03c9", StringComparison.Ordinal)
                         || String.Equals(SyntaxFacts.trimIdentifierQuotes head.Text, "omega", StringComparison.Ordinal)) ->
                rest
            | _ ->
                rawTokens

        match tokens with
        | head :: colon :: rest when Token.isName head && colon.Kind = Colon ->
            TypeSignatures.parseScheme rest
        | { Kind = LeftParen } :: { Kind = Operator } :: { Kind = RightParen } :: colon :: rest when colon.Kind = Colon ->
            TypeSignatures.parseScheme rest
        | _ ->
            None

    let private synthesizeBuiltinPreludeTraitArtifacts (coreModule: KCoreModule) : KRuntimeBinding list * KRuntimeTraitInstance list =
        if not (String.Equals(coreModule.Name, Stdlib.PreludeModuleText, StringComparison.Ordinal)) then
            [], []
        else
            let traitDeclarationsByName =
                coreModule.Declarations
                |> List.choose (fun declaration ->
                    match declaration.Source with
                    | TraitDeclarationNode traitDeclaration -> Some(traitDeclaration.Name, traitDeclaration)
                    | _ -> None)
                |> Map.ofList

            let makeProvenance declarationName introductionKind =
                { FilePath = coreModule.SourceFile
                  ModuleName = coreModule.Name
                  DeclarationName = Some declarationName
                  IntroductionKind = introductionKind }

            let parseHeadType headTypeText =
                match tryParseTypeText headTypeText with
                | Some parsed -> parsed
                | None -> invalidOp $"Could not parse synthesized builtin prelude head type '{headTypeText}'."

            let buildMemberBinding
                (traitDeclaration: TraitDeclaration)
                (spec: IntrinsicCatalog.BuiltinPreludeTraitInstanceSpec)
                instanceKey
                (memberName, lowering)
                : string * KRuntimeBinding
                =
                let traitTypeParameters =
                    TypeSignatures.collectLeadingTypeParameters traitDeclaration.HeaderTokens

                let headTypeExprs =
                    [ spec.HeadTypeText ] |> List.map parseHeadType

                let substitution =
                    if List.length traitTypeParameters = List.length headTypeExprs then
                        List.zip traitTypeParameters headTypeExprs |> Map.ofList
                    else
                        invalidOp
                            $"Builtin prelude instance synthesis expected trait '{traitDeclaration.Name}' to have {headTypeExprs.Length} type parameters."

                let memberDeclaration =
                    traitDeclaration.Members
                    |> List.tryFind (fun declaration -> declaration.Name = Some memberName)
                    |> Option.defaultWith (fun () ->
                        invalidOp
                            $"Builtin prelude instance synthesis could not find trait member '{traitDeclaration.Name}.{memberName}' in std.prelude.")

                let memberScheme =
                    tryParseTraitMemberScheme memberDeclaration
                    |> Option.defaultWith (fun () ->
                        invalidOp
                            $"Builtin prelude instance synthesis could not parse trait member scheme for '{traitDeclaration.Name}.{memberName}'.")
                    |> TypeSignatures.applySchemeSubstitution substitution

                let parameterTypes, resultType = TypeSignatures.schemeParts memberScheme
                let runtimeParameterTypes = parameterTypes |> List.filter (isCompileTimeParameterType >> not)

                let valueParameterNames =
                    runtimeParameterTypes |> List.mapi (fun index _ -> $"arg{index}")

                let valueParameters : KRuntimeParameter list =
                    runtimeParameterTypes
                    |> List.zip valueParameterNames
                    |> List.map (fun (parameterName, parameterType) ->
                        { Name = parameterName
                          TypeText = Some(parameterType |> eraseRuntimeTypeExpr |> TypeSignatures.toText) })

                let selfDictionaryType =
                    TypeSignatures.TypeName([ spec.TraitName ], headTypeExprs) |> TypeSignatures.toText

                let body =
                    match lowering, valueParameterNames with
                    | IntrinsicCatalog.ForwardToBuiltinBinaryOperator operatorName, [ leftName; rightName ] ->
                        KRuntimeBinary(KRuntimeName [ leftName ], operatorName, KRuntimeName [ rightName ])
                    | IntrinsicCatalog.ForwardToIntrinsicTerm intrinsicName, _ ->
                        KRuntimeApply(
                            KRuntimeName [ intrinsicName ],
                            valueParameterNames |> List.map (fun parameterName -> KRuntimeName [ parameterName ])
                        )
                    | IntrinsicCatalog.ForwardToBuiltinBinaryOperator operatorName, _ ->
                        invalidOp
                            $"Builtin prelude member '{traitDeclaration.Name}.{memberName}' expected two runtime arguments for operator '{operatorName}'."

                let bindingName = TraitRuntime.instanceMemberBindingName spec.TraitName instanceKey memberName

                (memberName,
                 { Name = bindingName
                   Parameters =
                     { Name = "__kappa_self_dict"
                       TypeText = Some selfDictionaryType }
                     :: valueParameters
                   ReturnTypeText = Some(resultType |> runtimeValueTypeExpr |> TypeSignatures.toText)
                   ExternalBinding = None
                   Body = Some body
                   Intrinsic = false
                   Provenance = makeProvenance bindingName "builtin-prelude-instance-member" })

            let synthesizedArtifacts : (KRuntimeBinding list * KRuntimeTraitInstance) list =
                IntrinsicCatalog.builtinPreludeTraitInstanceSpecs ()
                |> List.map (fun spec ->
                    let traitDeclaration =
                        traitDeclarationsByName
                        |> Map.tryFind spec.TraitName
                        |> Option.defaultWith (fun () ->
                            invalidOp $"Builtin prelude instance synthesis could not find trait '{spec.TraitName}' in std.prelude.")

                    let instanceKey = $"builtin-prelude:{spec.TraitName}:{spec.HeadTypeText}"

                    let memberBindingNames, memberBindingDeclarations =
                        spec.MemberLowerings
                        |> List.map (buildMemberBinding traitDeclaration spec instanceKey)
                        |> List.unzip

                    let dictionaryHeadTypeText =
                        TypeSignatures.TypeName([ spec.TraitName ], [ parseHeadType spec.HeadTypeText ]) |> TypeSignatures.toText

                    let dictionaryBindingName =
                        TraitRuntime.instanceDictionaryBindingName spec.TraitName instanceKey

                    let dictionaryBinding : KRuntimeBinding =
                        { Name = dictionaryBindingName
                          Parameters = []
                          ReturnTypeText = Some dictionaryHeadTypeText
                          ExternalBinding = None
                          Body = Some(KRuntimeDictionaryValue(coreModule.Name, spec.TraitName, instanceKey, []))
                          Intrinsic = false
                          Provenance = makeProvenance dictionaryBindingName "builtin-prelude-instance-dictionary" }

                    let traitInstance : KRuntimeTraitInstance =
                        { TraitName = spec.TraitName
                          InstanceKey = instanceKey
                          HeadTypeTexts = [ spec.HeadTypeText ]
                          MemberBindings =
                            List.zip
                                memberBindingNames
                                (memberBindingDeclarations |> List.map (fun binding -> binding.Name)) }

                    (memberBindingDeclarations @ [ dictionaryBinding ], traitInstance))

            synthesizedArtifacts
            |> List.fold
                (fun ((bindings: KRuntimeBinding list), (instances: KRuntimeTraitInstance list)) (nextBindings, nextInstance) ->
                    (bindings @ nextBindings, instances @ [ nextInstance ]))
                (([] : KRuntimeBinding list), ([] : KRuntimeTraitInstance list))

    let private tryAlignKeepMaskToArguments (keepMask: bool list) (arguments: KCoreArgument list) =
        let argumentCount = List.length arguments
        let maskCount = List.length keepMask

        if maskCount = argumentCount then
            Some keepMask
        elif maskCount > argumentCount then
            let omittedCount = maskCount - argumentCount
            let omittedPrefix = keepMask |> List.take omittedCount

            if omittedPrefix |> List.forall not then
                Some(keepMask |> List.skip omittedCount)
            else
                None
        else
            None

    let private filterRuntimeArguments (keepMask: bool list) (arguments: KCoreArgument list) =
        match tryAlignKeepMaskToArguments keepMask arguments with
        | Some alignedKeepMask ->
            List.zip alignedKeepMask arguments
            |> List.choose (fun (keep, argument) -> if keep then Some argument else None)
        | None ->
            arguments

    let private isCompileTimeOnlyRuntimeArgument (argument: KCoreArgument) =
        let rec isCompileTimeOnlyExpression expression =
            match expression with
            | KCoreDictionaryValue(_, traitName, _, captures) ->
                List.isEmpty captures && isIntrinsicCompileTimeTraitName [ traitName ]
            | KCoreName [ name ] ->
                name.StartsWith("__kappa_dict_IsTrait_", StringComparison.Ordinal)
                || name.StartsWith("__kappa_dict_IsProp_", StringComparison.Ordinal)
            | _ ->
                false

        isCompileTimeOnlyExpression argument.Expression

    let private filterProvablyCompileTimeOnlyArguments (keepMask: bool list) (arguments: KCoreArgument list) =
        match tryAlignKeepMaskToArguments keepMask arguments with
        | Some alignedKeepMask ->
            let zipped = List.zip alignedKeepMask arguments

            let canFilter =
                zipped
                |> List.forall (fun (keep, argument) -> keep || isCompileTimeOnlyRuntimeArgument argument)

            if canFilter then
                zipped
                |> List.choose (fun (keep, argument) -> if keep then Some argument else None)
            else
                arguments
        | None ->
            arguments

    let private isBuiltinPreludeInstanceKey (traitName: string) (instanceKey: string) =
        instanceKey.StartsWith($"builtin-prelude:{traitName}:", StringComparison.Ordinal)

    let private isBuiltinPreludeDictionary (moduleName: string) (traitName: string) (instanceKey: string) captures =
        String.Equals(moduleName, Stdlib.PreludeModuleText, StringComparison.Ordinal)
        && isBuiltinPreludeInstanceKey traitName instanceKey
        && List.isEmpty captures

    let private runtimeNameEndsWith (expectedName: string) (segments: string list) =
        match List.tryLast segments with
        | Some actualName -> String.Equals(actualName, expectedName, StringComparison.Ordinal)
        | None -> false

    let private specializeBuiltinPreludeRuntimeExpression expression =
        let rec rewrite expression =
            let rebuilt =
                match expression with
                | KRuntimeLiteral _
                | KRuntimeName _
                | KRuntimeEffectLabel _ ->
                    expression
                | KRuntimeEffectOperation(label, operationId, operationName) ->
                    KRuntimeEffectOperation(rewrite label, operationId, operationName)
                | KRuntimeClosure(parameters, body) ->
                    KRuntimeClosure(parameters, rewrite body)
                | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                    KRuntimeIfThenElse(rewrite condition, rewrite whenTrue, rewrite whenFalse)
                | KRuntimeHandle(isDeep, label, body, returnClause, operationClauses) ->
                    let rewriteClause (clause: KRuntimeEffectHandlerClause) =
                        { clause with Body = rewrite clause.Body }

                    KRuntimeHandle(
                        isDeep,
                        rewrite label,
                        rewrite body,
                        rewriteClause returnClause,
                        operationClauses |> List.map rewriteClause
                    )
                | KRuntimeMatch(scrutinee, cases) ->
                    KRuntimeMatch(
                        rewrite scrutinee,
                        cases
                        |> List.map (fun caseClause ->
                            { caseClause with
                                Guard = caseClause.Guard |> Option.map rewrite
                                Body = rewrite caseClause.Body })
                    )
                | KRuntimeExecute inner ->
                    KRuntimeExecute(rewrite inner)
                | KRuntimeLet(bindingName, value, body) ->
                    KRuntimeLet(bindingName, rewrite value, rewrite body)
                | KRuntimeDoScope(scopeLabel, body) ->
                    KRuntimeDoScope(scopeLabel, rewrite body)
                | KRuntimeScheduleExit(scopeLabel, action, body) ->
                    let rewrittenAction =
                        match action with
                        | KRuntimeDeferred deferred -> KRuntimeDeferred(rewrite deferred)
                        | KRuntimeRelease(resourceTypeText, release, resource) ->
                            KRuntimeRelease(resourceTypeText, rewrite release, rewrite resource)

                    KRuntimeScheduleExit(scopeLabel, rewrittenAction, rewrite body)
                | KRuntimeSequence(first, second) ->
                    KRuntimeSequence(rewrite first, rewrite second)
                | KRuntimeWhile(condition, body) ->
                    KRuntimeWhile(rewrite condition, rewrite body)
                | KRuntimeApply(callee, arguments) ->
                    KRuntimeApply(rewrite callee, arguments |> List.map rewrite)
                | KRuntimeDictionaryValue(moduleName, traitName, instanceKey, captures) ->
                    KRuntimeDictionaryValue(moduleName, traitName, instanceKey, captures |> List.map rewrite)
                | KRuntimeTraitCall(traitName, memberName, dictionary, arguments) ->
                    KRuntimeTraitCall(traitName, memberName, rewrite dictionary, arguments |> List.map rewrite)
                | KRuntimeUnary(operatorName, operand) ->
                    KRuntimeUnary(operatorName, rewrite operand)
                | KRuntimeBinary(left, operatorName, right) ->
                    KRuntimeBinary(rewrite left, operatorName, rewrite right)
                | KRuntimePrefixedString(prefix, parts) ->
                    KRuntimePrefixedString(
                        prefix,
                        parts
                        |> List.map (function
                            | KRuntimeStringText _ as part -> part
                            | KRuntimeStringInterpolation(inner, format) ->
                                KRuntimeStringInterpolation(rewrite inner, format))
                    )

            match rebuilt with
            | KRuntimeApply(
                KRuntimeName calleeSegments,
                [ KRuntimeDictionaryValue(moduleName, "Eq", instanceKey, captures)
                  left
                  right ]
              )
                when runtimeNameEndsWith "/=" calleeSegments
                     && isBuiltinPreludeDictionary moduleName "Eq" instanceKey captures ->
                KRuntimeBinary(left, "!=", right)
            | KRuntimeApply(
                KRuntimeName calleeSegments,
                [ KRuntimeDictionaryValue(moduleName, "Ord", instanceKey, captures)
                  left
                  right ]
              )
                when ((runtimeNameEndsWith "<" calleeSegments)
                      || (runtimeNameEndsWith "<=" calleeSegments)
                      || (runtimeNameEndsWith ">" calleeSegments)
                      || (runtimeNameEndsWith ">=" calleeSegments))
                     && isBuiltinPreludeDictionary moduleName "Ord" instanceKey captures ->
                let operatorName = List.last calleeSegments
                KRuntimeBinary(left, operatorName, right)
            | KRuntimeTraitCall(
                "Eq",
                "==",
                KRuntimeDictionaryValue(moduleName, "Eq", instanceKey, captures),
                [ left; right ]
              )
                when isBuiltinPreludeDictionary moduleName "Eq" instanceKey captures ->
                KRuntimeBinary(left, "==", right)
            | other ->
                other

        rewrite expression

    let rec private lowerKRuntimePattern pattern =
        match pattern with
        | KCoreWildcardPattern ->
            KRuntimeWildcardPattern
        | KCoreNamePattern name ->
            KRuntimeNamePattern name
        | KCoreLiteralPattern literal ->
            KRuntimeLiteralPattern literal
        | KCoreConstructorPattern(name, arguments) ->
            KRuntimeConstructorPattern(name, arguments |> List.map lowerKRuntimePattern)
        | KCoreOrPattern alternatives ->
            KRuntimeOrPattern(alternatives |> List.map lowerKRuntimePattern)

    let rec private lowerKRuntimeExitAction runtimeParameterMasks action =
        match action with
        | KCoreDeferred expression ->
            KRuntimeDeferred(lowerKRuntimeExpression runtimeParameterMasks expression)
        | KCoreRelease(resourceTypeText, release, resource) ->
            KRuntimeRelease(
                resourceTypeText,
                lowerKRuntimeExpression runtimeParameterMasks release,
                lowerKRuntimeExpression runtimeParameterMasks resource
            )

    and private lowerKRuntimeHandlerArgument argument =
        match argument with
        | KCoreEffectUnitArgument -> KRuntimeEffectUnitArgument
        | KCoreEffectWildcardArgument -> KRuntimeEffectWildcardArgument
        | KCoreEffectNameArgument name -> KRuntimeEffectNameArgument name

    and private lowerKRuntimeHandlerClause runtimeParameterMasks (clause: KCoreEffectHandlerClause) =
        ({ OperationName = clause.OperationName
           Arguments = clause.Arguments |> List.map lowerKRuntimeHandlerArgument
           ResumptionName = clause.ResumptionName
           Body = lowerKRuntimeExpression runtimeParameterMasks clause.Body }
         : KRuntimeEffectHandlerClause)

    and private lowerKRuntimeExpression runtimeParameterMasks expression =
        let rec tryDecodeInterpolatedFragments expression =
            let rec decodeList current =
                match current with
                | KCoreName [ "Nil" ] ->
                    Some []
                | KCoreAppSpine(
                    KCoreName [ "Cons" ],
                    [ { Expression = head }
                      { Expression = tail } ]
                  ) ->
                    decodeFragment head
                    |> Option.bind (fun decodedHead ->
                        decodeList tail
                        |> Option.map (fun decodedTail -> decodedHead :: decodedTail))
                | _ ->
                    None

            and decodeFragment fragment =
                match fragment with
                | KCoreAppSpine(KCoreName [ "Lit" ], [ { Expression = KCoreLiteral(LiteralValue.String text) } ]) ->
                    Some(KRuntimeStringText text)
                | KCoreAppSpine(
                    KCoreName [ "Interp" ],
                    [ _
                      { Expression = KCoreSyntaxQuote inner } ]
                  ) ->
                    Some(KRuntimeStringInterpolation(lowerKRuntimeExpression runtimeParameterMasks inner, None))
                | KCoreAppSpine(
                    KCoreName [ "InterpFmt" ],
                    [ _
                      { Expression = KCoreSyntaxQuote inner }
                      { Expression = KCoreLiteral(LiteralValue.String formatText) } ]
                  ) ->
                    Some(KRuntimeStringInterpolation(lowerKRuntimeExpression runtimeParameterMasks inner, Some formatText))
                | _ ->
                    None

            decodeList expression

        match expression with
        | KCoreLiteral literal ->
            KRuntimeLiteral literal
        | KCoreStaticObject _ ->
            KRuntimeLiteral LiteralValue.Unit
        | KCoreEffectLabel(labelName, interfaceId, labelId, operations) ->
            KRuntimeEffectLabel(
                labelName,
                interfaceId,
                labelId,
                operations
                |> List.map (fun operation ->
                    { KRuntimeEffectOperation.OperationId = operation.OperationId
                      Name = operation.Name
                      ResumptionQuantity = operation.ResumptionQuantity
                      ParameterArity = operation.ParameterArity })
            )
        | KCoreEffectOperation(label, operationId, operationName) ->
            KRuntimeEffectOperation(lowerKRuntimeExpression runtimeParameterMasks label, operationId, operationName)
        | KCoreName segments ->
            KRuntimeName segments
        | KCoreTopLevelSyntaxSplice inner ->
            match inner with
            | KCoreTraitCall(
                "InterpolatedMacro",
                "buildInterpolated",
                KCoreName [ prefix ],
                [ fragments ]
              ) ->
                match tryDecodeInterpolatedFragments fragments with
                | Some parts ->
                    KRuntimePrefixedString(prefix, parts)
                | None ->
                    lowerKRuntimeExpression runtimeParameterMasks inner
            | _ ->
                lowerKRuntimeExpression runtimeParameterMasks inner
        | KCoreSyntaxQuote inner
        | KCoreSyntaxSplice inner
        | KCoreCodeQuote inner
        | KCoreCodeSplice inner ->
            lowerKRuntimeExpression runtimeParameterMasks inner
        | KCoreLambda(parameters, body) ->
            KRuntimeClosure(
                filterRuntimeParameters parameters |> List.map (fun parameter -> parameter.Name),
                lowerKRuntimeExpression runtimeParameterMasks body
            )
        | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
            KRuntimeIfThenElse(
                lowerKRuntimeExpression runtimeParameterMasks condition,
                lowerKRuntimeExpression runtimeParameterMasks whenTrue,
                lowerKRuntimeExpression runtimeParameterMasks whenFalse
            )
        | KCoreHandle(isDeep, label, body, returnClause, operationClauses) ->
            KRuntimeHandle(
                isDeep,
                lowerKRuntimeExpression runtimeParameterMasks label,
                lowerKRuntimeExpression runtimeParameterMasks body,
                lowerKRuntimeHandlerClause runtimeParameterMasks returnClause,
                operationClauses |> List.map (lowerKRuntimeHandlerClause runtimeParameterMasks)
            )
        | KCoreMatch(scrutinee, cases) ->
            KRuntimeMatch(
                lowerKRuntimeExpression runtimeParameterMasks scrutinee,
                cases
                |> List.map (fun caseClause ->
                    { Pattern = lowerKRuntimePattern caseClause.Pattern
                      Guard = caseClause.Guard |> Option.map (lowerKRuntimeExpression runtimeParameterMasks)
                      Body = lowerKRuntimeExpression runtimeParameterMasks caseClause.Body })
            )
        | KCoreExecute inner ->
            KRuntimeExecute(lowerKRuntimeExpression runtimeParameterMasks inner)
        | KCoreLet(_, KCoreStaticObject _, body) ->
            lowerKRuntimeExpression runtimeParameterMasks body
        | KCoreLet(bindingName, value, body) ->
            KRuntimeLet(
                bindingName,
                lowerKRuntimeExpression runtimeParameterMasks value,
                lowerKRuntimeExpression runtimeParameterMasks body
            )
        | KCoreDoScope(scopeLabel, body) ->
            KRuntimeDoScope(scopeLabel, lowerKRuntimeExpression runtimeParameterMasks body)
        | KCoreScheduleExit(scopeLabel, action, body) ->
            KRuntimeScheduleExit(
                scopeLabel,
                lowerKRuntimeExitAction runtimeParameterMasks action,
                lowerKRuntimeExpression runtimeParameterMasks body
            )
        | KCoreSequence(first, second) ->
            KRuntimeSequence(
                lowerKRuntimeExpression runtimeParameterMasks first,
                lowerKRuntimeExpression runtimeParameterMasks second
            )
        | KCoreWhile(condition, body) ->
            KRuntimeWhile(
                lowerKRuntimeExpression runtimeParameterMasks condition,
                lowerKRuntimeExpression runtimeParameterMasks body
            )
        | KCoreAppSpine(callee, arguments) ->
            let runtimeArguments =
                match callee with
                | KCoreName [ calleeName ] ->
                    runtimeParameterMasks
                    |> Map.tryFind calleeName
                    |> Option.map (fun keepMask -> filterProvablyCompileTimeOnlyArguments keepMask arguments)
                    |> Option.defaultValue arguments
                | _ ->
                    arguments

            KRuntimeApply(
                lowerKRuntimeExpression runtimeParameterMasks callee,
                runtimeArguments
                |> List.map (fun argument -> lowerKRuntimeExpression runtimeParameterMasks argument.Expression)
            )
        | KCoreDictionaryValue(moduleName, traitName, instanceKey, captures) ->
            KRuntimeDictionaryValue(
                moduleName,
                traitName,
                instanceKey,
                captures |> List.map (lowerKRuntimeExpression runtimeParameterMasks)
            )
        | KCoreTraitCall(traitName, memberName, dictionary, arguments) ->
            KRuntimeTraitCall(
                traitName,
                memberName,
                lowerKRuntimeExpression runtimeParameterMasks dictionary,
                arguments |> List.map (lowerKRuntimeExpression runtimeParameterMasks)
            )
        | KCoreUnary(operatorName, operand) ->
            KRuntimeUnary(operatorName, lowerKRuntimeExpression runtimeParameterMasks operand)
        | KCoreBinary(left, operatorName, right) ->
            KRuntimeBinary(
                lowerKRuntimeExpression runtimeParameterMasks left,
                operatorName,
                lowerKRuntimeExpression runtimeParameterMasks right
            )
        | KCorePrefixedString(prefix, parts) ->
            KRuntimePrefixedString(
                prefix,
                parts
                |> List.map (function
                    | KCoreStringText text -> KRuntimeStringText text
                    | KCoreStringInterpolation inner ->
                        KRuntimeStringInterpolation(lowerKRuntimeExpression runtimeParameterMasks inner, None))
            )

    let private isPrivateByDefaultAttributes attributes =
        attributes
        |> List.exists (fun attributeName -> String.Equals(attributeName, ModuleAttribute.PrivateByDefault, StringComparison.Ordinal))

    let private isExportedVisibility moduleAttributes visibility =
        match visibility with
        | Some Visibility.Public -> true
        | Some Visibility.Private -> false
        | None -> not (isPrivateByDefaultAttributes moduleAttributes)

    let private isExportedKCoreBinding (moduleDump: KCoreModule) (binding: KCoreBinding) =
        isExportedVisibility moduleDump.ModuleAttributes binding.Visibility

    let private isExportedDataDeclaration (moduleDump: KCoreModule) (declaration: DataDeclaration) =
        not declaration.IsOpaque
        && isExportedVisibility moduleDump.ModuleAttributes declaration.Visibility

    let private isCompileTimeOnlyConstructorParameterTokens parameterTokens =
        match parameterTokens |> significantTokens with
        | { Kind = AtSign } :: _ ->
            true
        | { Kind = IntegerLiteral; Text = "0" } :: _ ->
            true
        | _ ->
            false

    let private isCompileTimeOnlyTypeExpr typeExpr =
        match typeExpr with
        | TypeSignatures.TypeUniverse _
        | TypeSignatures.TypeIntrinsic _ ->
            true
        | TypeSignatures.TypeName(nameSegments, _) ->
            match nameSegments with
            | [ "Type" ]
            | [ "Universe" ]
            | [ "Constraint" ]
            | [ "Quantity" ]
            | [ "Region" ]
            | [ "RecRow" ]
            | [ "VarRow" ]
            | [ "EffRow" ]
            | [ "Label" ]
            | [ "EffLabel" ]
            | [ "Syntax" ]
            | [ "Code" ]
            | [ "std"; "prelude"; "Syntax" ]
            | [ "std"; "prelude"; "Code" ] ->
                true
            | _ when isIntrinsicCompileTimeTraitName nameSegments ->
                true
            | _ ->
                false
        | _ ->
            false

    let private runtimeConstructorParameterTypeTokens parameterTokens =
        let typeTokens =
            match parameterTokens |> List.tryFindIndex (fun token -> token.Kind = Colon) with
            | Some colonIndex when colonIndex + 1 < parameterTokens.Length ->
                parameterTokens |> List.skip (colonIndex + 1)
            | _ ->
                parameterTokens

        typeTokens

    let private tryConstructorRuntimeSignatureFieldTypes (constructor: DataConstructor) =
        let significant = constructor.Tokens |> significantTokens

        match significant with
        | _ :: { Kind = Colon } :: typeTokens ->
            TypeSignatures.parseScheme typeTokens
            |> Option.map (fun scheme ->
                let parameterTypes, _ = TypeSignatures.schemeParts scheme

                parameterTypes
                |> List.filter (isCompileTimeParameterType >> not)
                |> List.map eraseRuntimeTypeExpr)
        | _ ->
            None

    let private runtimeConstructorParameterTokenGroups (constructor: DataConstructor) =
        TypeSignatures.constructorParameterTokenGroups constructor
        |> List.filter (fun parameterTokens ->
            not (isCompileTimeOnlyConstructorParameterTokens parameterTokens)
            && (runtimeConstructorParameterTypeTokens parameterTokens
                |> TypeSignatures.parseType
                |> Option.map isCompileTimeOnlyTypeExpr
                |> Option.defaultValue false
                |> not))

    let private runtimeConstructorParameters (constructor: DataConstructor) =
        match constructor.Parameters with
        | Some parameters ->
            parameters
            |> List.filter (fun parameter ->
                not parameter.ParameterIsImplicit
                && parameter.ParameterQuantity <> Some QuantityZero
                && (parameter.ParameterTypeTokens
                    |> TypeSignatures.parseType
                    |> Option.map isCompileTimeOnlyTypeExpr
                    |> Option.defaultValue false
                    |> not))
            |> List.map Choice1Of2
        | None ->
            runtimeConstructorParameterTokenGroups constructor |> List.map Choice2Of2

    let private constructorFieldNames (constructor: DataConstructor) =
        match tryConstructorRuntimeSignatureFieldTypes constructor with
        | Some fieldTypes ->
            List.replicate fieldTypes.Length None
        | None ->
            runtimeConstructorParameters constructor
            |> List.map (function
                | Choice1Of2 parameter ->
                    parameter.ParameterName
                | Choice2Of2 groupTokens ->
                    match groupTokens |> List.tryFindIndex (fun token -> token.Kind = Colon) with
                    | Some colonIndex when colonIndex > 0 ->
                        match groupTokens |> List.take colonIndex with
                        | [ nameToken ] when Token.isName nameToken ->
                            Some(SyntaxFacts.trimIdentifierQuotes nameToken.Text)
                        | _ ->
                            None
                    | _ ->
                        None)

    let private constructorFieldTypeTexts (constructor: DataConstructor) =
        match tryConstructorRuntimeSignatureFieldTypes constructor with
        | Some fieldTypes ->
            fieldTypes |> List.map TypeSignatures.toText
        | None ->
            runtimeConstructorParameters constructor
            |> List.map (function
                | Choice1Of2 parameter ->
                    normalizeTypeTokens parameter.ParameterTypeTokens |> eraseRuntimeTypeText
                | Choice2Of2 groupTokens ->
                    runtimeConstructorParameterTypeTokens groupTokens |> normalizeTypeTokens |> eraseRuntimeTypeText)

    let private isCompileTimeOnlyBindingBody body =
        match body with
        | Some(KCoreStaticObject _) -> true
        | _ -> false

    let private tryEtaExpandClosureBinding
        returnTypeText
        body
        : (KRuntimeParameter list * string option * KRuntimeExpression option) option =
        let rec flattenArrows typeExpr =
            match typeExpr with
            | TypeSignatures.TypeArrow(_, parameterType, resultType) ->
                let parameterTypes, finalResult = flattenArrows resultType
                parameterType :: parameterTypes, finalResult
            | other ->
                [], other

        match returnTypeText, body with
        | Some returnTypeText, Some(KRuntimeClosure(parameterNames, closureBody)) ->
            tryParseTypeText returnTypeText
            |> Option.bind (fun returnType ->
                let parameterTypes, resultType = flattenArrows returnType

                if List.length parameterNames = List.length parameterTypes then
                    let etaParameters : KRuntimeParameter list =
                        parameterNames
                        |> List.zip parameterTypes
                        |> List.map (fun (parameterType, parameterName) ->
                            ({ Name = parameterName
                               TypeText = Some(TypeSignatures.toText parameterType |> eraseRuntimeTypeText) }
                             : KRuntimeParameter))

                    Some(
                        etaParameters,
                        Some(TypeSignatures.toText resultType |> runtimeValueTypeText),
                        Some closureBody
                    )
                else
                    None)
        | _ ->
            None

    let private intrinsicRuntimeSignature (declaration: ExpectedTermDeclaration) =
        declaration.TypeTokens
        |> TypeSignatures.parseScheme
        |> Option.map (fun scheme ->
            let parameterTypes, resultType = TypeSignatures.schemeParts scheme

            let parameters: KRuntimeParameter list =
                parameterTypes
                |> List.filter (isCompileTimeParameterType >> not)
                |> List.mapi (fun index parameterType ->
                    { Name = $"arg{index}"
                      TypeText = Some(TypeSignatures.toText parameterType |> eraseRuntimeTypeText) })

            parameters, Some(runtimeValueTypeExpr resultType |> TypeSignatures.toText))

    let buildSharedRuntimeParameterMasks (coreModules: KCoreModule list) : Map<string, bool list> =
        coreModules
        |> List.collect (fun coreModule ->
            coreModule.Declarations
            |> List.choose (fun declaration ->
                declaration.Binding
                |> Option.bind (fun binding ->
                    binding.Name
                    |> Option.map (fun bindingName ->
                        bindingName,
                        (binding.Parameters |> List.map (isCompileTimeKCoreParameter >> not))))))
        |> List.groupBy fst
        |> List.choose (fun (bindingName, entries) ->
            let candidateMasks = entries |> List.map snd |> List.distinct

            match candidateMasks with
            | [ keepMask ] -> Some(bindingName, keepMask)
            | _ -> None)
        |> Map.ofList

    let lowerKRuntimeModule (sharedRuntimeParameterMasks: Map<string, bool list>) (coreModule: KCoreModule) : KRuntimeModule =
        let compileTimeOnlyTraitNames =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Source with
                | TraitDeclarationNode traitDeclaration ->
                    let isCompileTimeOnlyTrait =
                        traitDeclaration.Members
                        |> List.choose tryParseTraitMemberScheme
                        |> function
                            | [] -> false
                            | memberSchemes ->
                                memberSchemes
                                |> List.forall (schemeRequiresRuntimeErasure Set.empty)

                    if isCompileTimeOnlyTrait then
                        Some traitDeclaration.Name
                    else
                        None
                | _ ->
                    None)
            |> Set.ofList

        let visibleTraitArities = coreModule.VisibleTraitTypeParameterCounts

        let runtimeParameterMasks =
            let localRuntimeParameterMasks =
                coreModule.Declarations
                |> List.choose (fun declaration ->
                    declaration.Binding
                    |> Option.bind (fun binding ->
                        binding.Name
                        |> Option.map (fun bindingName ->
                            bindingName,
                            (binding.Parameters |> List.map (isCompileTimeKCoreParameter >> not)))))
                |> Map.ofList

            localRuntimeParameterMasks
            |> Map.fold (fun current bindingName keepMask -> Map.add bindingName keepMask current) sharedRuntimeParameterMasks

        let builtinPreludeBindings, builtinPreludeTraitInstances =
            synthesizeBuiltinPreludeTraitArtifacts coreModule

        let termBindings : KRuntimeBinding list =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Binding with
                | Some binding
                    when binding.Name.IsSome
                         && not (isCompileTimeOnlyBindingBody binding.Body)
                         && not (
                             (binding.Parameters |> List.exists (fun parameter ->
                                 parameter.Type
                                 |> Option.map (typeExprReferencesCompileTimeTrait compileTimeOnlyTraitNames)
                                 |> Option.defaultValue false))
                             || (binding.ReturnType
                                 |> Option.map (fun returnType ->
                                     typeExprContainsCompileTimeOnly returnType
                                     || typeExprReferencesCompileTimeTrait compileTimeOnlyTraitNames returnType)
                                 |> Option.defaultValue false)
                             || (binding.Name
                                 |> Option.bind TraitRuntime.tryParseDispatchBindingName
                                 |> Option.exists (fun (traitName, _) -> Set.contains traitName compileTimeOnlyTraitNames))) ->
                    let loweredBody =
                        binding.Body
                        |> Option.map (lowerKRuntimeExpression runtimeParameterMasks >> specializeBuiltinPreludeRuntimeExpression)
                    let loweredParameters = binding.Parameters |> filterRuntimeParameters |> List.map lowerRuntimeParameter
                    let loweredReturnType = binding.ReturnTypeText |> Option.map runtimeValueTypeText
                    let parameters, returnType, body =
                        match loweredParameters with
                        | [] ->
                            tryEtaExpandClosureBinding binding.ReturnTypeText loweredBody
                            |> Option.defaultValue (loweredParameters, loweredReturnType, loweredBody)
                        | _ ->
                            loweredParameters, loweredReturnType, loweredBody

                    Some
                        ({ Name = binding.Name.Value
                           Parameters = parameters
                           ReturnTypeText = returnType
                           ExternalBinding = None
                           Body = body
                           Intrinsic = false
                           Provenance = binding.Provenance }
                         : KRuntimeBinding)
                | _ ->
                    None)
            |> List.append builtinPreludeBindings

        let intrinsicBindings : KRuntimeBinding list =
            let intrinsicSignatures =
                coreModule.Declarations
                |> List.choose (fun declaration ->
                    match declaration.Source with
                    | ExpectDeclarationNode (ExpectTermDeclaration termDeclaration) ->
                        TypeSignatures.parseScheme termDeclaration.TypeTokens
                        |> Option.bind (fun scheme ->
                            if schemeRequiresRuntimeErasure compileTimeOnlyTraitNames scheme then
                                None
                            else
                                intrinsicRuntimeSignature termDeclaration
                                |> Option.map (fun signature -> termDeclaration.Name, signature))
                    | _ ->
                        None)
                |> Map.ofList

            coreModule.IntrinsicTerms
            |> List.filter intrinsicSignatures.ContainsKey
            |> List.map (fun name ->
                let parameters, returnTypeText =
                    intrinsicSignatures[name]

                { Name = name
                  Parameters = parameters
                  ReturnTypeText = returnTypeText
                  ExternalBinding = None
                  Body = None
                  Intrinsic = true
                  Provenance =
                    { FilePath = coreModule.SourceFile
                      ModuleName = coreModule.Name
                      DeclarationName = Some name
                      IntroductionKind = "intrinsic" } })

        let dataTypes : KRuntimeDataType list =
            let sourceDataTypes : KRuntimeDataType list =
                coreModule.Declarations
                |> List.choose (fun declaration ->
                    match declaration.Source with
                    | DataDeclarationNode dataDeclaration ->
                        let constructors : KRuntimeConstructor list =
                            dataDeclaration.Constructors
                            |> List.map (fun constructor ->
                                let fieldTypeTexts = constructorFieldTypeTexts constructor

                                { Name = constructor.Name
                                  Arity = fieldTypeTexts.Length
                                  TypeName = dataDeclaration.Name
                                  FieldNames = constructorFieldNames constructor
                                  FieldTypeTexts = fieldTypeTexts
                                  Provenance =
                                    { declaration.Provenance with
                                        DeclarationName = Some constructor.Name
                                        IntroductionKind = "constructor" } })

                        Some
                            { Name = dataDeclaration.Name
                              TypeParameters = TypeSignatures.collectLeadingTypeParameters dataDeclaration.HeaderTokens
                              Constructors = constructors
                              ExternalRuntimeTypeName = None }
                    | _ ->
                        None)

            let syntheticDataTypes : KRuntimeDataType list =
                coreModule.SyntheticDataTypes
                |> List.map (fun dataType ->
                    let constructor : KRuntimeConstructor =
                        { Name = dataType.ConstructorName
                          Arity = dataType.FieldNames.Length
                          TypeName = dataType.Name
                          FieldNames = dataType.FieldNames |> List.map Some
                          FieldTypeTexts = dataType.FieldTypeTexts |> List.map eraseRuntimeTypeText
                          Provenance = dataType.Provenance }

                    { Name = dataType.Name
                      TypeParameters = dataType.TypeParameters
                      Constructors = [ constructor ]
                      ExternalRuntimeTypeName = None })

            sourceDataTypes @ syntheticDataTypes

        let traits : KRuntimeTrait list =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Source with
                | TraitDeclarationNode traitDeclaration when not (Set.contains traitDeclaration.Name compileTimeOnlyTraitNames) ->
                    Some
                        { Name = traitDeclaration.Name
                          TypeParameterCount =
                            TypeSignatures.collectLeadingTypeParameters traitDeclaration.HeaderTokens
                            |> List.length }
                | _ ->
                    None)

        let traitInstances : KRuntimeTraitInstance list =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Source with
                | InstanceDeclarationNode instanceDeclaration when not (Set.contains instanceDeclaration.TraitName compileTimeOnlyTraitNames) ->
                    let instanceKey = TraitRuntime.instanceKeyFromTokens instanceDeclaration.HeaderTokens
                    let argumentCount =
                        visibleTraitArities
                        |> Map.tryFind instanceDeclaration.TraitName
                        |> Option.defaultValue 1

                    let headTypeTexts =
                        instanceDeclaration.HeaderTokens
                        |> splitLeadingTypeArgumentTokens argumentCount
                        |> List.map normalizeTypeTokens

                    let memberBindings =
                        instanceDeclaration.Members
                        |> List.choose (fun memberDeclaration ->
                            memberDeclaration.Name
                            |> Option.map (fun memberName ->
                                memberName,
                                TraitRuntime.instanceMemberBindingName
                                    instanceDeclaration.TraitName
                                    instanceKey
                                    memberName))

                    Some
                        ({ TraitName = instanceDeclaration.TraitName
                           InstanceKey = instanceKey
                           HeadTypeTexts = headTypeTexts
                           MemberBindings = memberBindings }
                         : KRuntimeTraitInstance)
                | _ ->
                    None)
            |> List.append builtinPreludeTraitInstances

        let dataTypes, termBindings, traitInstances =
            let knownTypeNames =
                ref (dataTypes |> List.map (fun dataType -> dataType.Name) |> Set.ofList)

            let runtimeRecordTypes = ref Map.empty<string, KRuntimeDataType>

            let syntheticRecordOrigin typeName =
                { FilePath = coreModule.SourceFile
                  ModuleName = coreModule.Name
                  DeclarationName = Some typeName
                  IntroductionKind = "runtime-record" }

            let tryVisibleTraitRuntimeName nameSegments =
                let qualifiedName = SyntaxFacts.moduleNameToText nameSegments

                match visibleTraitArities |> Map.tryFind qualifiedName with
                | Some arity ->
                    Some(qualifiedName, arity)
                | None ->
                    match nameSegments with
                    | [ traitName ] ->
                        visibleTraitArities |> Map.tryFind traitName |> Option.map (fun arity -> traitName, arity)
                    | _ ->
                        None

            let rec rewriteRuntimeTypeExpr typeExpr =
                match typeExpr with
                | TypeSignatures.TypeName(name, arguments) ->
                    let rewrittenArguments = arguments |> List.map rewriteRuntimeTypeExpr

                    match tryVisibleTraitRuntimeName name with
                    | Some(traitName, arity) when rewrittenArguments.Length = arity ->
                        TypeSignatures.TypeName([ TraitRuntime.dictionaryTypeName traitName ], rewrittenArguments)
                    | _ ->
                        TypeSignatures.TypeName(name, rewrittenArguments)
                | TypeSignatures.TypeVariable _
                | TypeSignatures.TypeLevelLiteral _
                | TypeSignatures.TypeIntrinsic _ ->
                    typeExpr
                | TypeSignatures.TypeUniverse level ->
                    TypeSignatures.TypeUniverse(level |> Option.map rewriteRuntimeTypeExpr)
                | TypeSignatures.TypeApply(callee, arguments) ->
                    TypeSignatures.TypeApply(rewriteRuntimeTypeExpr callee, arguments |> List.map rewriteRuntimeTypeExpr)
                | TypeSignatures.TypeLambda(parameterName, parameterSort, body) ->
                    TypeSignatures.TypeLambda(parameterName, rewriteRuntimeTypeExpr parameterSort, rewriteRuntimeTypeExpr body)
                | TypeSignatures.TypeDelay inner ->
                    TypeSignatures.TypeDelay(rewriteRuntimeTypeExpr inner)
                | TypeSignatures.TypeMemo inner ->
                    TypeSignatures.TypeMemo(rewriteRuntimeTypeExpr inner)
                | TypeSignatures.TypeForce inner ->
                    TypeSignatures.TypeForce(rewriteRuntimeTypeExpr inner)
                | TypeSignatures.TypeProject(target, fieldName) ->
                    TypeSignatures.TypeProject(rewriteRuntimeTypeExpr target, fieldName)
                | TypeSignatures.TypeArrow(quantity, parameterType, returnType) ->
                    TypeSignatures.TypeArrow(quantity, rewriteRuntimeTypeExpr parameterType, rewriteRuntimeTypeExpr returnType)
                | TypeSignatures.TypeEquality(left, right) ->
                    TypeSignatures.TypeEquality(rewriteRuntimeTypeExpr left, rewriteRuntimeTypeExpr right)
                | TypeSignatures.TypeCapture(inner, captures) ->
                    TypeSignatures.TypeCapture(rewriteRuntimeTypeExpr inner, captures)
                | TypeSignatures.TypeEffectRow(entries, tail) ->
                    TypeSignatures.TypeEffectRow(
                        entries
                        |> List.map (fun entry ->
                            { entry with
                                Label = rewriteRuntimeTypeExpr entry.Label
                                Effect = rewriteRuntimeTypeExpr entry.Effect }),
                        tail |> Option.map rewriteRuntimeTypeExpr
                    )
                | TypeSignatures.TypeRecord fields ->
                    let rewrittenFields =
                        fields
                        |> List.map (fun field ->
                            { field with
                                Type = rewriteRuntimeTypeExpr field.Type })

                    let recordExpr = TypeSignatures.TypeRecord rewrittenFields
                    let recordKey = TypeSignatures.toText recordExpr
                    let typeName = syntheticRuntimeRecordTypeName recordKey
                    let typeParameters = TypeSignatures.collectFreeTypeVariables recordExpr

                    if not (Set.contains typeName knownTypeNames.Value) && not (Map.containsKey typeName runtimeRecordTypes.Value) then
                        let constructor =
                            { Name = typeName
                              Arity = rewrittenFields.Length
                              TypeName = typeName
                              FieldNames = rewrittenFields |> List.map (fun field -> Some field.Name)
                              FieldTypeTexts = rewrittenFields |> List.map (fun field -> TypeSignatures.toText field.Type)
                              Provenance = syntheticRecordOrigin typeName }

                        runtimeRecordTypes.Value <-
                            runtimeRecordTypes.Value
                            |> Map.add
                                typeName
                                { Name = typeName
                                  TypeParameters = typeParameters
                                  Constructors = [ constructor ]
                                  ExternalRuntimeTypeName = None }

                        knownTypeNames.Value <- Set.add typeName knownTypeNames.Value

                    TypeSignatures.TypeName([ typeName ], typeParameters |> List.map TypeSignatures.TypeVariable)
                | TypeSignatures.TypeUnion members ->
                    TypeSignatures.TypeUnion(members |> List.map rewriteRuntimeTypeExpr)

            let rewriteRuntimeTypeText text =
                match tryParseTypeText text with
                | Some parsed ->
                    parsed
                    |> rewriteRuntimeTypeExpr
                    |> TypeSignatures.toText
                | None ->
                    text

            let rewrittenDataTypes =
                dataTypes
                |> List.map (fun dataType ->
                    { dataType with
                        Constructors =
                            dataType.Constructors
                            |> List.map (fun constructor ->
                                { constructor with
                                    FieldTypeTexts = constructor.FieldTypeTexts |> List.map rewriteRuntimeTypeText }) })

            let rewrittenBindings =
                termBindings
                |> List.map (fun binding ->
                    { binding with
                        Parameters =
                            binding.Parameters
                            |> List.map (fun parameter ->
                                { parameter with
                                    TypeText = parameter.TypeText |> Option.map rewriteRuntimeTypeText })
                        ReturnTypeText = binding.ReturnTypeText |> Option.map rewriteRuntimeTypeText })

            let rewrittenTraitInstances =
                traitInstances
                |> List.map (fun instanceDeclaration ->
                    { instanceDeclaration with
                        HeadTypeTexts = instanceDeclaration.HeadTypeTexts |> List.map rewriteRuntimeTypeText })

            rewrittenDataTypes @ (runtimeRecordTypes.Value |> Map.toList |> List.map snd), rewrittenBindings, rewrittenTraitInstances

        let constructors : KRuntimeConstructor list =
            dataTypes
            |> List.collect (fun dataType -> dataType.Constructors)

        let exports =
            coreModule.Declarations
            |> List.collect (fun declaration ->
                let bindingExports =
                    match declaration.Binding with
                    | Some binding when binding.Name.IsSome && isExportedKCoreBinding coreModule binding ->
                        [ binding.Name.Value ]
                    | _ ->
                        []

                let constructorExports =
                    match declaration.Source with
                    | DataDeclarationNode dataDeclaration when isExportedDataDeclaration coreModule dataDeclaration ->
                        dataDeclaration.Constructors |> List.map (fun constructor -> constructor.Name)
                    | _ ->
                        []

                bindingExports @ constructorExports)
            |> List.append coreModule.IntrinsicTerms
            |> List.distinct
            |> List.sort

        ({ Name = coreModule.Name
           SourceFile = coreModule.SourceFile
           Imports = coreModule.Imports
           Exports = exports
           IntrinsicTerms = coreModule.IntrinsicTerms
           DataTypes = dataTypes
           Traits = traits
           TraitInstances = traitInstances
           Constructors = constructors
           Bindings = termBindings @ intrinsicBindings }: KRuntimeModule)
