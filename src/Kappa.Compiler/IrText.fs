namespace Kappa.Compiler

open System

// Renders compiler IR modules into human-readable text for dumps and debugging.
module internal IrText =
    let rec kcorePatternText pattern =
        match pattern with
        | KCoreWildcardPattern -> "_"
        | KCoreNamePattern name -> name
        | KCoreLiteralPattern(LiteralValue.Integer value) -> string value
        | KCoreLiteralPattern(LiteralValue.Float value) -> string value
        | KCoreLiteralPattern(LiteralValue.String value) -> $"\"{value}\""
        | KCoreLiteralPattern(LiteralValue.Character value) -> $"'{value}'"
        | KCoreLiteralPattern LiteralValue.Unit -> "()"
        | KCoreConstructorPattern(name, arguments) ->
            let nameText = String.concat "." name

            match arguments with
            | [] ->
                nameText
            | _ ->
                let argumentText = arguments |> List.map kcorePatternText |> String.concat " "
                $"({nameText} {argumentText})"
        | KCoreOrPattern alternatives ->
            alternatives
            |> List.map kcorePatternText
            |> String.concat " | "

    let rec kcoreExitActionText action =
        match action with
        | KCoreDeferred expression ->
            $"(deferred {kcoreExpressionText expression})"
        | KCoreRelease(resourceTypeText, release, resource) ->
            let typeText = resourceTypeText |> Option.defaultValue "unknown"
            $"(release (type {typeText}) (handler {kcoreExpressionText release}) (resource {kcoreExpressionText resource}))"

    and kcoreExpressionText expression =
        match expression with
        | KCoreLiteral(LiteralValue.Integer value) -> string value
        | KCoreLiteral(LiteralValue.Float value) -> string value
        | KCoreLiteral(LiteralValue.String value) -> $"\"{value}\""
        | KCoreLiteral(LiteralValue.Character value) -> $"'{value}'"
        | KCoreLiteral LiteralValue.Unit -> "()"
        | KCoreName segments -> String.concat "." segments
        | KCoreStaticObject staticObject ->
            let kindText =
                match staticObject.ObjectKind with
                | KCoreTypeObject -> "type-object"
                | KCoreTraitObject -> "trait-object"
                | KCoreEffectLabelObject -> "effect-label-object"
                | KCoreModuleObject -> "module-object"

            let nameText = String.concat "." staticObject.Name
            let typeText = staticObject.TypeText |> Option.defaultValue "_"
            $"({kindText} {nameText} : {typeText})"
        | KCoreLambda(parameters, body) ->
            let names = parameters |> List.map (fun parameter -> parameter.Name) |> String.concat " "
            $"(lambda ({names}) {kcoreExpressionText body})"
        | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
            $"(if {kcoreExpressionText condition} {kcoreExpressionText whenTrue} {kcoreExpressionText whenFalse})"
        | KCoreMatch(scrutinee, cases) ->
            let caseText =
                cases
                |> List.map (fun caseClause ->
                    let guardText =
                        caseClause.Guard
                        |> Option.map (fun guard -> $" if {kcoreExpressionText guard}")
                        |> Option.defaultValue ""

                    $"(case {kcorePatternText caseClause.Pattern}{guardText} {kcoreExpressionText caseClause.Body})")
                |> String.concat " "

            $"(match {kcoreExpressionText scrutinee} {caseText})"
        | KCoreExecute expression ->
            $"(execute {kcoreExpressionText expression})"
        | KCoreLet(bindingName, value, body) ->
            $"(let {bindingName} {kcoreExpressionText value} {kcoreExpressionText body})"
        | KCoreDoScope(scopeLabel, body) ->
            $"(do-scope {scopeLabel} {kcoreExpressionText body})"
        | KCoreScheduleExit(scopeLabel, action, body) ->
            $"(schedule-exit {scopeLabel} {kcoreExitActionText action} {kcoreExpressionText body})"
        | KCoreSequence(first, second) ->
            $"(seq {kcoreExpressionText first} {kcoreExpressionText second})"
        | KCoreWhile(condition, body) ->
            $"(while {kcoreExpressionText condition} {kcoreExpressionText body})"
        | KCoreApply(callee, arguments) ->
            let argumentText =
                arguments
                |> List.map kcoreExpressionText
                |> String.concat " "

            if String.IsNullOrWhiteSpace(argumentText) then
                $"(apply {kcoreExpressionText callee})"
            else
                $"(apply {kcoreExpressionText callee} {argumentText})"
        | KCoreDictionaryValue(moduleName, traitName, instanceKey) ->
            $"(dictionary {moduleName} {traitName} {instanceKey})"
        | KCoreTraitCall(traitName, memberName, dictionary, arguments) ->
            let argumentText =
                arguments
                |> List.map kcoreExpressionText
                |> String.concat " "

            if String.IsNullOrWhiteSpace(argumentText) then
                $"(trait-call {traitName}.{memberName} {kcoreExpressionText dictionary})"
            else
                $"(trait-call {traitName}.{memberName} {kcoreExpressionText dictionary} {argumentText})"
        | KCoreUnary(operatorName, operand) ->
            $"({operatorName} {kcoreExpressionText operand})"
        | KCoreBinary(left, operatorName, right) ->
            $"({operatorName} {kcoreExpressionText left} {kcoreExpressionText right})"
        | KCorePrefixedString(prefix, parts) ->
            let partText =
                parts
                |> List.map (function
                    | KCoreStringText text -> $"text:{text}"
                    | KCoreStringInterpolation inner -> $"interp:{kcoreExpressionText inner}")
                |> String.concat " | "

            $"({prefix}-string {partText})"

    let rec runtimePatternText pattern =
        match pattern with
        | KRuntimeWildcardPattern -> "_"
        | KRuntimeNamePattern name -> name
        | KRuntimeLiteralPattern(LiteralValue.Integer value) -> string value
        | KRuntimeLiteralPattern(LiteralValue.Float value) -> string value
        | KRuntimeLiteralPattern(LiteralValue.String value) -> $"\"{value}\""
        | KRuntimeLiteralPattern(LiteralValue.Character value) -> $"'{value}'"
        | KRuntimeLiteralPattern LiteralValue.Unit -> "()"
        | KRuntimeConstructorPattern(name, arguments) ->
            let nameText = String.concat "." name

            match arguments with
            | [] ->
                nameText
            | _ ->
                let argumentText = arguments |> List.map runtimePatternText |> String.concat " "
                $"({nameText} {argumentText})"
        | KRuntimeOrPattern alternatives ->
            alternatives
            |> List.map runtimePatternText
            |> String.concat " | "

    let rec runtimeExitActionText action =
        match action with
        | KRuntimeDeferred expression ->
            $"(deferred {runtimeExpressionText expression})"
        | KRuntimeRelease(resourceTypeText, release, resource) ->
            let typeText = resourceTypeText |> Option.defaultValue "unknown"
            $"(release (type {typeText}) (handler {runtimeExpressionText release}) (resource {runtimeExpressionText resource}))"

    and runtimeExpressionText expression =
        match expression with
        | KRuntimeLiteral(LiteralValue.Integer value) -> string value
        | KRuntimeLiteral(LiteralValue.Float value) -> string value
        | KRuntimeLiteral(LiteralValue.String value) -> $"\"{value}\""
        | KRuntimeLiteral(LiteralValue.Character value) -> $"'{value}'"
        | KRuntimeLiteral LiteralValue.Unit -> "()"
        | KRuntimeName segments -> String.concat "." segments
        | KRuntimeClosure(parameters, body) ->
            let names = String.concat " " parameters
            $"(closure ({names}) {runtimeExpressionText body})"
        | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
            $"(if {runtimeExpressionText condition} {runtimeExpressionText whenTrue} {runtimeExpressionText whenFalse})"
        | KRuntimeMatch(scrutinee, cases) ->
            let caseText =
                cases
                |> List.map (fun caseClause ->
                    let guardText =
                        caseClause.Guard
                        |> Option.map (fun guard -> $" if {runtimeExpressionText guard}")
                        |> Option.defaultValue ""

                    $"(case {runtimePatternText caseClause.Pattern}{guardText} {runtimeExpressionText caseClause.Body})")
                |> String.concat " "

            $"(match {runtimeExpressionText scrutinee} {caseText})"
        | KRuntimeExecute expression ->
            $"(execute {runtimeExpressionText expression})"
        | KRuntimeLet(bindingName, value, body) ->
            $"(let {bindingName} {runtimeExpressionText value} {runtimeExpressionText body})"
        | KRuntimeDoScope(scopeLabel, body) ->
            $"(do-scope {scopeLabel} {runtimeExpressionText body})"
        | KRuntimeScheduleExit(scopeLabel, action, body) ->
            $"(schedule-exit {scopeLabel} {runtimeExitActionText action} {runtimeExpressionText body})"
        | KRuntimeSequence(first, second) ->
            $"(seq {runtimeExpressionText first} {runtimeExpressionText second})"
        | KRuntimeWhile(condition, body) ->
            $"(while {runtimeExpressionText condition} {runtimeExpressionText body})"
        | KRuntimeApply(callee, arguments) ->
            let argumentText =
                arguments
                |> List.map runtimeExpressionText
                |> String.concat " "

            if String.IsNullOrWhiteSpace(argumentText) then
                $"(apply {runtimeExpressionText callee})"
            else
                $"(apply {runtimeExpressionText callee} {argumentText})"
        | KRuntimeDictionaryValue(moduleName, traitName, instanceKey) ->
            $"(dictionary {moduleName} {traitName} {instanceKey})"
        | KRuntimeTraitCall(traitName, memberName, dictionary, arguments) ->
            let argumentText =
                arguments
                |> List.map runtimeExpressionText
                |> String.concat " "

            if String.IsNullOrWhiteSpace(argumentText) then
                $"(trait-call {traitName}.{memberName} {runtimeExpressionText dictionary})"
            else
                $"(trait-call {traitName}.{memberName} {runtimeExpressionText dictionary} {argumentText})"
        | KRuntimeUnary(operatorName, operand) ->
            $"({operatorName} {runtimeExpressionText operand})"
        | KRuntimeBinary(left, operatorName, right) ->
            $"({operatorName} {runtimeExpressionText left} {runtimeExpressionText right})"
        | KRuntimePrefixedString(prefix, parts) ->
            let partText =
                parts
                |> List.map (function
                    | KRuntimeStringText text -> $"text:{text}"
                    | KRuntimeStringInterpolation inner -> $"interp:{runtimeExpressionText inner}")
                |> String.concat " | "

            $"({prefix}-string {partText})"

    let rec backendRepresentationText representation =
        match representation with
        | BackendRepInt64 -> "int64"
        | BackendRepFloat64 -> "float64"
        | BackendRepBoolean -> "bool"
        | BackendRepString -> "string"
        | BackendRepChar -> "char"
        | BackendRepUnit -> "unit"
        | BackendRepRef elementRepresentation -> $"ref:{backendRepresentationText elementRepresentation}"
        | BackendRepDictionary traitName -> $"dictionary:{traitName}"
        | BackendRepTaggedData(moduleName, typeName) -> $"tagged-data:{moduleName}.{typeName}"
        | BackendRepClosure environmentLayout -> $"closure:{environmentLayout}"
        | BackendRepIOAction -> "io-action"
        | BackendRepOpaque(Some label) -> $"opaque:{label}"
        | BackendRepOpaque None -> "opaque"

    let backendCallingConventionText (convention: KBackendCallingConvention) =
        let parameterText =
            convention.ParameterRepresentations
            |> List.map backendRepresentationText
            |> String.concat ", "

        let resultText =
            convention.ResultRepresentation
            |> Option.map backendRepresentationText
            |> Option.defaultValue "void"

        let dictionaryText =
            if List.isEmpty convention.RetainedDictionaryParameters then
                ""
            else
                let names = convention.RetainedDictionaryParameters |> String.concat ", "
                $" dict=[{names}]"

        $"arity={convention.RuntimeArity} params=[{parameterText}] result={resultText}{dictionaryText}"

    let private backendResolvedNameText resolvedName =
        match resolvedName with
        | BackendLocalName(name, representation) ->
            match representation with
            | Some rep -> $"{name}:{backendRepresentationText rep}"
            | None -> name
        | BackendGlobalBindingName(moduleName, bindingName, representation) ->
            match representation with
            | Some rep -> $"{moduleName}.{bindingName}:{backendRepresentationText rep}"
            | None -> $"{moduleName}.{bindingName}"
        | BackendIntrinsicName(moduleName, bindingName, representation) ->
            match representation with
            | Some rep -> $"intrinsic {moduleName}.{bindingName}:{backendRepresentationText rep}"
            | None -> $"intrinsic {moduleName}.{bindingName}"
        | BackendConstructorName(moduleName, typeName, constructorName, tag, arity, representation) ->
            $"ctor {moduleName}.{typeName}.{constructorName}@{tag}/{arity}:{backendRepresentationText representation}"

    let rec backendPatternText pattern =
        match pattern with
        | BackendWildcardPattern -> "_"
        | BackendBindPattern binding -> $"{binding.Name}:{backendRepresentationText binding.Representation}"
        | BackendLiteralPattern(LiteralValue.Integer value, representation) ->
            $"{value}:{backendRepresentationText representation}"
        | BackendLiteralPattern(LiteralValue.Float value, representation) ->
            $"{value}:{backendRepresentationText representation}"
        | BackendLiteralPattern(LiteralValue.String value, representation) ->
            $"\"{value}\":{backendRepresentationText representation}"
        | BackendLiteralPattern(LiteralValue.Character value, representation) ->
            $"'{value}':{backendRepresentationText representation}"
        | BackendLiteralPattern(LiteralValue.Unit, representation) ->
            $"():{backendRepresentationText representation}"
        | BackendConstructorPattern(moduleName, typeName, constructorName, tag, fieldPatterns) ->
            let fields =
                fieldPatterns
                |> List.map backendPatternText
                |> String.concat " "

            if String.IsNullOrWhiteSpace(fields) then
                $"({moduleName}.{typeName}.{constructorName}@{tag})"
            else
                $"({moduleName}.{typeName}.{constructorName}@{tag} {fields})"

    let rec backendExpressionText expression =
        match expression with
        | BackendLiteral(LiteralValue.Integer value, representation) ->
            $"{value}:{backendRepresentationText representation}"
        | BackendLiteral(LiteralValue.Float value, representation) ->
            $"{value}:{backendRepresentationText representation}"
        | BackendLiteral(LiteralValue.String value, representation) ->
            $"\"{value}\":{backendRepresentationText representation}"
        | BackendLiteral(LiteralValue.Character value, representation) ->
            $"'{value}':{backendRepresentationText representation}"
        | BackendLiteral(LiteralValue.Unit, representation) ->
            $"():{backendRepresentationText representation}"
        | BackendName resolvedName ->
            backendResolvedNameText resolvedName
        | BackendClosure(parameters, captures, environmentLayout, body, convention, representation) ->
            let parameterText =
                parameters
                |> List.map (fun parameter -> $"{parameter.Name}:{backendRepresentationText parameter.Representation}")
                |> String.concat " "

            let captureText =
                captures
                |> List.map (fun capture -> $"{capture.Name}:{backendRepresentationText capture.Representation}")
                |> String.concat " "

            $"(closure env={environmentLayout} rep={backendRepresentationText representation} conv=[{backendCallingConventionText convention}] params=({parameterText}) captures=({captureText}) {backendExpressionText body})"
        | BackendIfThenElse(condition, whenTrue, whenFalse, resultRepresentation) ->
            $"(if rep={backendRepresentationText resultRepresentation} {backendExpressionText condition} {backendExpressionText whenTrue} {backendExpressionText whenFalse})"
        | BackendMatch(scrutinee, cases, resultRepresentation) ->
            let caseText =
                cases
                |> List.map (fun caseClause ->
                    let guardText =
                        caseClause.Guard
                        |> Option.map (fun guard -> $" if {backendExpressionText guard}")
                        |> Option.defaultValue ""

                    $"(case {backendPatternText caseClause.Pattern}{guardText} {backendExpressionText caseClause.Body})")
                |> String.concat " "

            $"(match rep={backendRepresentationText resultRepresentation} {backendExpressionText scrutinee} {caseText})"
        | BackendExecute(expression, resultRepresentation) ->
            $"(execute rep={backendRepresentationText resultRepresentation} {backendExpressionText expression})"
        | BackendLet(binding, value, body, resultRepresentation) ->
            $"(let {binding.Name}:{backendRepresentationText binding.Representation} {backendExpressionText value} {backendExpressionText body} rep={backendRepresentationText resultRepresentation})"
        | BackendSequence(first, second, resultRepresentation) ->
            $"(seq rep={backendRepresentationText resultRepresentation} {backendExpressionText first} {backendExpressionText second})"
        | BackendWhile(condition, body) ->
            $"(while {backendExpressionText condition} {backendExpressionText body})"
        | BackendCall(callee, arguments, convention, resultRepresentation) ->
            let argumentText =
                arguments
                |> List.map backendExpressionText
                |> String.concat " "

            $"(call rep={backendRepresentationText resultRepresentation} conv=[{backendCallingConventionText convention}] {backendExpressionText callee} {argumentText})"
        | BackendDictionaryValue(moduleName, traitName, instanceKey, representation) ->
            $"(dictionary rep={backendRepresentationText representation} {moduleName} {traitName} {instanceKey})"
        | BackendTraitCall(traitName, memberName, dictionary, arguments, resultRepresentation) ->
            let argumentText =
                arguments
                |> List.map backendExpressionText
                |> String.concat " "

            if String.IsNullOrWhiteSpace(argumentText) then
                $"(trait-call rep={backendRepresentationText resultRepresentation} {traitName}.{memberName} {backendExpressionText dictionary})"
            else
                $"(trait-call rep={backendRepresentationText resultRepresentation} {traitName}.{memberName} {backendExpressionText dictionary} {argumentText})"
        | BackendConstructData(moduleName, typeName, constructorName, tag, fields, representation) ->
            let fieldText =
                fields
                |> List.map backendExpressionText
                |> String.concat " "

            $"(construct {moduleName}.{typeName}.{constructorName}@{tag} rep={backendRepresentationText representation} {fieldText})"
        | BackendPrefixedString(prefix, parts, resultRepresentation) ->
            let partText =
                parts
                |> List.map (function
                    | BackendStringText text -> $"text:{text}"
                    | BackendStringInterpolation inner -> $"interp:{backendExpressionText inner}")
                |> String.concat " | "

            $"({prefix}-string rep={backendRepresentationText resultRepresentation} {partText})"
