// Covers the first M4 slice: effect operations, one-shot deep handlers, and multishot capture safety.
module MilestoneFourTests

open System
open System.IO
open Kappa.Compiler
open Harness
open Xunit

let private diagnosticsText diagnostics =
    diagnostics
    |> List.map (fun diagnostic -> $"{DiagnosticCode.toIdentifier diagnostic.Code}: {diagnostic.Message}")
    |> String.concat Environment.NewLine

let rec private containsDeepCoreHandle expression =
    match expression with
    | KCoreHandle(true, _, _, _, _) ->
        true
    | KCoreHandle(_, label, body, returnClause, operationClauses) ->
        containsDeepCoreHandle label
        || containsDeepCoreHandle body
        || containsDeepCoreHandle returnClause.Body
        || (operationClauses |> List.exists (fun clause -> containsDeepCoreHandle clause.Body))
    | KCoreLambda(_, body)
    | KCoreExecute body
    | KCoreDoScope(_, body)
    | KCoreUnary(_, body)
    | KCoreSyntaxQuote body
    | KCoreSyntaxSplice body
    | KCoreTopLevelSyntaxSplice body
    | KCoreCodeQuote body
    | KCoreCodeSplice body ->
        containsDeepCoreHandle body
    | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
        containsDeepCoreHandle condition || containsDeepCoreHandle whenTrue || containsDeepCoreHandle whenFalse
    | KCoreLet(_, value, body)
    | KCoreSequence(value, body) ->
        containsDeepCoreHandle value || containsDeepCoreHandle body
    | KCoreScheduleExit(_, KCoreDeferred deferred, body) ->
        containsDeepCoreHandle deferred || containsDeepCoreHandle body
    | KCoreScheduleExit(_, KCoreRelease(_, release, resource), body) ->
        containsDeepCoreHandle release || containsDeepCoreHandle resource || containsDeepCoreHandle body
    | KCoreWhile(condition, body)
    | KCoreBinary(condition, _, body) ->
        containsDeepCoreHandle condition || containsDeepCoreHandle body
    | KCoreAppSpine(callee, arguments) ->
        containsDeepCoreHandle callee
        || (arguments |> List.exists (fun argument -> containsDeepCoreHandle argument.Expression))
    | KCoreTraitCall(_, _, dictionary, arguments) ->
        containsDeepCoreHandle dictionary || (arguments |> List.exists containsDeepCoreHandle)
    | KCoreMatch(scrutinee, cases) ->
        containsDeepCoreHandle scrutinee
        || (cases
            |> List.exists (fun caseClause ->
                caseClause.Guard |> Option.exists containsDeepCoreHandle
                || containsDeepCoreHandle caseClause.Body))
    | KCorePrefixedString(_, parts) ->
        parts
        |> List.exists (function
            | KCoreStringText _ -> false
            | KCoreStringInterpolation inner -> containsDeepCoreHandle inner)
    | KCoreEffectOperation(label, _) ->
        containsDeepCoreHandle label
    | KCoreLiteral _
    | KCoreName _
    | KCoreStaticObject _
    | KCoreDictionaryValue _
    | KCoreEffectLabel _ ->
        false

let rec private containsDeepRuntimeHandle expression =
    match expression with
    | KRuntimeHandle(true, _, _, _, _) ->
        true
    | KRuntimeHandle(_, label, body, returnClause, operationClauses) ->
        containsDeepRuntimeHandle label
        || containsDeepRuntimeHandle body
        || containsDeepRuntimeHandle returnClause.Body
        || (operationClauses |> List.exists (fun clause -> containsDeepRuntimeHandle clause.Body))
    | KRuntimeClosure(_, body)
    | KRuntimeExecute body
    | KRuntimeDoScope(_, body)
    | KRuntimeUnary(_, body) ->
        containsDeepRuntimeHandle body
    | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
        containsDeepRuntimeHandle condition || containsDeepRuntimeHandle whenTrue || containsDeepRuntimeHandle whenFalse
    | KRuntimeLet(_, value, body)
    | KRuntimeSequence(value, body) ->
        containsDeepRuntimeHandle value || containsDeepRuntimeHandle body
    | KRuntimeScheduleExit(_, KRuntimeDeferred deferred, body) ->
        containsDeepRuntimeHandle deferred || containsDeepRuntimeHandle body
    | KRuntimeScheduleExit(_, KRuntimeRelease(_, release, resource), body) ->
        containsDeepRuntimeHandle release || containsDeepRuntimeHandle resource || containsDeepRuntimeHandle body
    | KRuntimeWhile(condition, body)
    | KRuntimeBinary(condition, _, body) ->
        containsDeepRuntimeHandle condition || containsDeepRuntimeHandle body
    | KRuntimeApply(callee, arguments) ->
        containsDeepRuntimeHandle callee || (arguments |> List.exists containsDeepRuntimeHandle)
    | KRuntimeTraitCall(_, _, dictionary, arguments) ->
        containsDeepRuntimeHandle dictionary || (arguments |> List.exists containsDeepRuntimeHandle)
    | KRuntimeMatch(scrutinee, cases) ->
        containsDeepRuntimeHandle scrutinee
        || (cases
            |> List.exists (fun caseClause ->
                caseClause.Guard |> Option.exists containsDeepRuntimeHandle
                || containsDeepRuntimeHandle caseClause.Body))
    | KRuntimePrefixedString(_, parts) ->
        parts
        |> List.exists (function
            | KRuntimeStringText _ -> false
            | KRuntimeStringInterpolation(inner, _) -> containsDeepRuntimeHandle inner)
    | KRuntimeEffectOperation(label, _) ->
        containsDeepRuntimeHandle label
    | KRuntimeLiteral _
    | KRuntimeName _
    | KRuntimeDictionaryValue _
    | KRuntimeEffectLabel _ ->
        false

let rec private duplicateFirstResumptionUse expression =
    let recurse current = duplicateFirstResumptionUse current

    match expression with
    | KRuntimeHandle(isDeep, label, body, returnClause, operationClauses) ->
        let rewrittenClauses =
            operationClauses
            |> List.mapi (fun index clause ->
                if index = 0 then
                    match clause.ResumptionName with
                    | Some resumptionName ->
                        let firstResume =
                            KRuntimeApply(
                                KRuntimeName [ resumptionName ],
                                [ KRuntimeName [ "True" ] ]
                            )

                        let secondResume =
                            KRuntimeApply(
                                KRuntimeName [ resumptionName ],
                                [ KRuntimeName [ "False" ] ]
                            )

                        { clause with
                            Body =
                                KRuntimeSequence(
                                    KRuntimeExecute firstResume,
                                    secondResume
                                ) }
                    | None ->
                        clause
                else
                    clause)

        KRuntimeHandle(
            isDeep,
            recurse label,
            recurse body,
            { returnClause with Body = recurse returnClause.Body },
            rewrittenClauses
        )
    | KRuntimeClosure(parameters, body) ->
        KRuntimeClosure(parameters, recurse body)
    | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
        KRuntimeIfThenElse(recurse condition, recurse whenTrue, recurse whenFalse)
    | KRuntimeLet(bindingName, value, body) ->
        KRuntimeLet(bindingName, recurse value, recurse body)
    | KRuntimeDoScope(scopeLabel, body) ->
        KRuntimeDoScope(scopeLabel, recurse body)
    | KRuntimeScheduleExit(scopeLabel, action, body) ->
        let rewrittenAction =
            match action with
            | KRuntimeDeferred deferred -> KRuntimeDeferred(recurse deferred)
            | KRuntimeRelease(resourceTypeText, release, resource) ->
                KRuntimeRelease(resourceTypeText, recurse release, recurse resource)

        KRuntimeScheduleExit(scopeLabel, rewrittenAction, recurse body)
    | KRuntimeSequence(firstExpression, secondExpression) ->
        KRuntimeSequence(recurse firstExpression, recurse secondExpression)
    | KRuntimeWhile(condition, body) ->
        KRuntimeWhile(recurse condition, recurse body)
    | KRuntimeApply(callee, arguments) ->
        KRuntimeApply(recurse callee, arguments |> List.map recurse)
    | KRuntimeTraitCall(traitName, memberName, dictionary, arguments) ->
        KRuntimeTraitCall(traitName, memberName, recurse dictionary, arguments |> List.map recurse)
    | KRuntimeMatch(scrutinee, cases) ->
        KRuntimeMatch(
            recurse scrutinee,
            cases
            |> List.map (fun caseClause ->
                { caseClause with
                    Guard = caseClause.Guard |> Option.map recurse
                    Body = recurse caseClause.Body })
        )
    | KRuntimeExecute inner ->
        KRuntimeExecute(recurse inner)
    | KRuntimeUnary(operatorName, inner) ->
        KRuntimeUnary(operatorName, recurse inner)
    | KRuntimeBinary(left, operatorName, right) ->
        KRuntimeBinary(recurse left, operatorName, recurse right)
    | KRuntimePrefixedString(prefix, parts) ->
        KRuntimePrefixedString(
            prefix,
            parts
            |> List.map (function
                | KRuntimeStringText text -> KRuntimeStringText text
                | KRuntimeStringInterpolation(inner, format) -> KRuntimeStringInterpolation(recurse inner, format))
        )
    | KRuntimeLiteral _
    | KRuntimeName _
    | KRuntimeEffectLabel _
    | KRuntimeEffectOperation _
    | KRuntimeDictionaryValue _ ->
        expression

let rec private containsRecursiveShallowCoreDriver expression =
    let recurse current = containsRecursiveShallowCoreDriver current

    match expression with
    | KCoreLet(
        goName,
        KCoreLambda([ parameter ], KCoreHandle(false, _, KCoreName [ compName ], _, _)),
        KCoreAppSpine(KCoreName [ invokedName ], [ argument ])
      )
        when String.Equals(goName, invokedName, StringComparison.Ordinal)
             && String.Equals(parameter.Name, compName, StringComparison.Ordinal) ->
        true || recurse argument.Expression
    | KCoreHandle(_, label, body, returnClause, operationClauses) ->
        recurse label
        || recurse body
        || recurse returnClause.Body
        || (operationClauses |> List.exists (fun clause -> recurse clause.Body))
    | KCoreLambda(_, body)
    | KCoreExecute body
    | KCoreDoScope(_, body)
    | KCoreUnary(_, body)
    | KCoreSyntaxQuote body
    | KCoreSyntaxSplice body
    | KCoreTopLevelSyntaxSplice body
    | KCoreCodeQuote body
    | KCoreCodeSplice body ->
        recurse body
    | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
        recurse condition || recurse whenTrue || recurse whenFalse
    | KCoreLet(_, value, body)
    | KCoreSequence(value, body) ->
        recurse value || recurse body
    | KCoreScheduleExit(_, KCoreDeferred deferred, body) ->
        recurse deferred || recurse body
    | KCoreScheduleExit(_, KCoreRelease(_, release, resource), body) ->
        recurse release || recurse resource || recurse body
    | KCoreWhile(condition, body)
    | KCoreBinary(condition, _, body) ->
        recurse condition || recurse body
    | KCoreAppSpine(callee, arguments) ->
        recurse callee || (arguments |> List.exists (fun argument -> recurse argument.Expression))
    | KCoreTraitCall(_, _, dictionary, arguments) ->
        recurse dictionary || (arguments |> List.exists recurse)
    | KCoreMatch(scrutinee, cases) ->
        recurse scrutinee
        || (cases
            |> List.exists (fun caseClause ->
                caseClause.Guard |> Option.exists recurse || recurse caseClause.Body))
    | KCorePrefixedString(_, parts) ->
        parts
        |> List.exists (function
            | KCoreStringText _ -> false
            | KCoreStringInterpolation inner -> recurse inner)
    | KCoreEffectOperation(label, _) ->
        recurse label
    | KCoreLiteral _
    | KCoreName _
    | KCoreStaticObject _
    | KCoreDictionaryValue _
    | KCoreEffectLabel _ ->
        false

[<Fact>]
let ``interpreter executes a one shot deep handler over a scoped effect`` () =
    let mainSource =
        [
            "module main"
            ""
            "result : Int"
            "let result : Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            deep handle Ask comp with"
            "                case return x -> pure x"
            "                case ask () k -> k True"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-deep-state-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("1", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected deep handler evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``interpreter executes a one shot deep handler over a parameterized scoped effect`` () =
    let mainSource =
        [
            "module main"
            ""
            "result : Int"
            "let result : Int ="
            "    block"
            "        scoped effect State (s : Type) ="
            "            get : Unit -> s"
            ""
            "        let comp : Eff <[State : State Int]> Int ="
            "            do"
            "                let n <- State.get ()"
            "                n"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            deep handle State comp with"
            "                case return x -> pure x"
            "                case get () k -> k 0"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-parameterized-local-effect-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("0", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected parameterized local effect evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``interpreter rejects repeated use of a consumed one shot resumption`` () =
    let mainSource =
        [
            "module main"
            ""
            "result : Int"
            "let result : Int ="
            "    block"
            "        scoped effect Ask ="
            "            1 ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            deep handle Ask comp with"
            "                case return x -> pure x"
            "                case ask () k -> k True"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-runtime-oneshot-failsafe-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected source workspace to compile cleanly, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    let malformedWorkspace =
        { workspace with
            KRuntimeIR =
                workspace.KRuntimeIR
                |> List.map (fun runtimeModule ->
                    if runtimeModule.Name = "main" then
                        { runtimeModule with
                            Bindings =
                                runtimeModule.Bindings
                                |> List.map (fun binding ->
                                    if binding.Name = "result" then
                                        { binding with Body = binding.Body |> Option.map duplicateFirstResumptionUse }
                                    else
                                        binding) }
                    else
                        runtimeModule) }

    match Interpreter.evaluateBinding malformedWorkspace "main.result" with
    | Result.Ok value ->
        failwithf "Expected duplicated one-shot resumption to fail at runtime, but interpreter returned %s" (RuntimeValue.format value)
    | Result.Error issue ->
        Assert.Contains("one-shot resumption", issue.Message, StringComparison.Ordinal)

[<Fact>]
let ``deep handlers elaborate to a recursive shallow driver before KCore and KRuntimeIR`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "handled : Eff <[ ]> Int"
            "let handled : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        deep handle Ask comp with"
            "            case return x -> pure x"
            "            case ask () k -> k True"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-deep-driver-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    let coreModule =
        workspace.KCore
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let coreBinding =
        coreModule.Declarations
        |> List.pick (fun declaration ->
            match declaration.Binding with
            | Some binding when binding.Name = Some "handled" -> Some binding
            | _ -> None)

    match coreBinding.Body with
    | Some body ->
        Assert.False(containsDeepCoreHandle body, "Expected KCore lowering to remove direct deep-handle nodes.")
        Assert.True(containsRecursiveShallowCoreDriver body, "Expected KCore lowering to synthesize a recursive shallow-handler driver.")
    | None ->
        failwith "Expected handled binding to have a KCore body."

    let runtimeModule =
        workspace.KRuntimeIR
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let runtimeBinding =
        runtimeModule.Bindings
        |> List.find (fun binding -> binding.Name = "handled")

    match runtimeBinding.Body with
    | Some body ->
        Assert.False(containsDeepRuntimeHandle body, "Expected KRuntimeIR lowering to remove direct deep-handle nodes.")
    | None ->
        failwith "Expected handled binding to have a KRuntimeIR body."

[<Fact>]
let ``shallow handler resumptions remain in the unhandled effect carrier`` () =
    let mainSource =
        [
            "module main"
            ""
            "result : Int"
            "let result : Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let rehandle : (1 k : ((1 x : Bool) -> Eff <[Ask : Ask]> Int)) -> Eff <[ ]> Int ="
            "            \\(1 k : ((1 x : Bool) -> Eff <[Ask : Ask]> Int)) ->"
            "                handle Ask (k True) with"
            "                    case return y -> pure y"
            "                    case ask () k2 -> k2 False"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            handle Ask comp with"
            "                case return y -> pure y"
            "                case ask () k -> rehandle k"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-shallow-rehandle-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("1", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected shallow handler rehandle evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``handlers reject missing operation clauses for the handled effect`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect State ="
            "            get : Unit -> Int"
            "            put : Int -> Unit"
            ""
            "        let comp : Eff <[State : State]> Int ="
            "            do"
            "                let n <- State.get ()"
            "                State.put (n + 1)"
            "                n"
            ""
            "        handle State comp with"
            "            case return x -> pure x"
            "            case get () k -> k 0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-missing-handler-clause-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected handlers with missing operation clauses to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerClauseMissing)

[<Fact>]
let ``handlers reject a missing return clause`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        handle Ask comp with"
            "            case ask () k -> k True"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-missing-return-clause-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected handlers without a return clause to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerClauseMissing)

[<Fact>]
let ``handlers reject duplicate return clauses`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        handle Ask comp with"
            "            case return x -> pure x"
            "            case return y -> pure y"
            "            case ask () k -> k True"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-duplicate-return-clause-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected handlers with duplicate return clauses to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerClauseDuplicate)

[<Fact>]
let ``handler return clauses must bind exactly one payload`` () =
    let noPayloadSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        handle Ask comp with"
            "            case return -> pure 0"
            "            case ask () k -> k True"
        ]
        |> String.concat "\n"

    let extraPayloadSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        handle Ask comp with"
            "            case return x y -> pure x"
            "            case ask () k -> k True"
        ]
        |> String.concat "\n"

    let noPayloadWorkspace =
        compileInMemoryWorkspace
            "memory-m4-return-clause-arity-zero-root"
            [ "main.kp", noPayloadSource ]

    let extraPayloadWorkspace =
        compileInMemoryWorkspace
            "memory-m4-return-clause-arity-two-root"
            [ "main.kp", extraPayloadSource ]

    Assert.True(noPayloadWorkspace.HasErrors, "Expected return clauses without a payload binder to be rejected.")
    Assert.True(extraPayloadWorkspace.HasErrors, "Expected return clauses with multiple payload binders to be rejected.")
    Assert.Contains(noPayloadWorkspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerClauseArityMismatch)
    Assert.Contains(extraPayloadWorkspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerClauseArityMismatch)

[<Fact>]
let ``handlers reject unexpected and duplicate operation clauses`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        handle Ask comp with"
            "            case return x -> pure x"
            "            case ask () k -> k True"
            "            case ask () k -> k False"
            "            case nope () k -> k 0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-unexpected-handler-clause-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected handlers with duplicate and unexpected clauses to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerClauseDuplicate)
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerClauseUnexpected)

[<Fact>]
let ``handlers reject operation clauses with mismatched parameter arity`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Pairing ="
            "            pair : Int -> Bool -> String"
            ""
            "        let comp : Eff <[Pairing : Pairing]> Int ="
            "            do"
            "                let _ <- Pairing.pair 1 True"
            "                0"
            ""
            "        handle Pairing comp with"
            "            case return x -> pure x"
            "            case pair x k -> pure 0"
            "            case pair x y z k -> pure 0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-handler-arity-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected handler clauses with wrong parameter arity to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerClauseArityMismatch)

[<Fact>]
let ``handler operation clauses require an explicit resumption binder`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        handle Ask comp with"
            "            case return x -> pure x"
            "            case ask () -> pure 0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-handler-missing-resumption-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected handler clauses without a resumption binder to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.ParseError)

[<Fact>]
let ``top level effect declarations reject borrowed resumption quantities`` () =
    let mainSource =
        [
            "module main"
            ""
            "effect Ask ="
            "    & ask : Unit -> Bool"
            ""
            "result : Int"
            "let result = 0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-borrowed-top-level-resumption-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected borrowed effect resumption quantities to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.EffectResumptionQuantityBorrowed)

[<Fact>]
let ``local scoped effects reject borrowed resumption quantities`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "result : Int"
            "let result : Int ="
            "    block"
            "        scoped effect Ask ="
            "            &[r] ask : Unit -> Bool"
            ""
            "        0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-borrowed-local-resumption-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected local scoped effects with borrowed resumption quantities to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.EffectResumptionQuantityBorrowed)

[<Fact>]
let ``shallow handler clauses must agree on a single target carrier`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        handle Ask comp with"
            "            case return x -> pure x"
            "            case ask () k -> 0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-shallow-handler-carrier-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected mismatched shallow handler clause carriers to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``deep handler clauses must agree on a single target carrier`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Int"
            "let bad : Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        deep handle Ask comp with"
            "            case return x -> x"
            "            case ask () k -> k True"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-deep-handler-carrier-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected mismatched deep handler clause carriers to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``handlers reject computations whose closed effect row lacks the handled label`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[ ]> Int = pure 0"
            ""
            "        handle Ask comp with"
            "            case return x -> pure x"
            "            case ask () k -> k True"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-handler-row-mismatch-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected handlers to reject computations whose closed effect row lacks the handled label.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerEffectRowMismatch)

[<Fact>]
let ``handlers reject computations whose handled row entry names a different effect interface`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        scoped effect Other ="
            "            other : Unit -> Int"
            ""
            "        let comp : Eff <[Ask : Other]> Int = pure 0"
            ""
            "        handle Ask comp with"
            "            case return x -> pure x"
            "            case ask () k -> k True"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-handler-row-interface-mismatch-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected handlers to reject rows whose handled label is paired with the wrong effect interface.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerEffectRowMismatch)

[<Fact>]
let ``deep handlers can eliminate one label from a closed multi effect row`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "ok : Eff <[Other : Other]> Int"
            "let ok : Eff <[Other : Other]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        scoped effect Other ="
            "            other : Unit -> Int"
            ""
            "        let comp : Eff <[Ask : Ask, Other : Other]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        deep handle Ask comp with"
            "            case return x -> pure x"
            "            case ask () k -> k True"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-handler-row-remainder-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected deep handler over a closed multi-effect row to typecheck, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

[<Fact>]
let ``deep handlers accept a lexically rebound effect label`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "ok : Int"
            "let ok : Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let l = Ask"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            deep handle l comp with"
            "                case return x -> pure x"
            "                case ask () k -> k True"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-handler-label-alias-root"
            "main.ok"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected deep handler with a rebound effect label to typecheck, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("1", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected rebound-label deep handler evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``effect operation selection accepts a lexically rebound effect label`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "ok : Int"
            "let ok : Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let l = Ask"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- l.ask ()"
            "                if b then 1 else 0"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            deep handle Ask comp with"
            "                case return x -> pure x"
            "                case ask () k -> k True"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-op-label-alias-root"
            "main.ok"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected aliased effect operation selection to typecheck, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("1", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected aliased effect operation selection to succeed, got %s" issue.Message

[<Fact>]
let ``deep handlers accept an effect label carried in a record field`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "ok : Int"
            "let ok : Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let pkg = (label = Ask)"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            deep handle pkg.label comp with"
            "                case return x -> pure x"
            "                case ask () k -> k True"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-handler-record-label-root"
            "main.ok"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected record-carried effect label handling to typecheck, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("1", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected record-carried effect label handling to succeed, got %s" issue.Message

[<Fact>]
let ``effect operation selection accepts an effect label carried in a record field`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "ok : Int"
            "let ok : Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let pkg = (label = Ask)"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- pkg.label.ask ()"
            "                if b then 1 else 0"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            deep handle Ask comp with"
            "                case return x -> pure x"
            "                case ask () k -> k True"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-op-record-label-root"
            "main.ok"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected record-carried effect operation selection to typecheck, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("1", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected record-carried effect operation selection to succeed, got %s" issue.Message

[<Fact>]
let ``multishot scoped effect invocation rejects captured linear suffix at the operation site`` () =
    let fixturePath =
        Path.Combine(
            __SOURCE_DIRECTORY__,
            "Fixtures",
            "borrow_qtt.100_interactions.handler_reject_multishot_resumption_captures_linear_suffix",
            "main.kp"
        )

    let mainSource = File.ReadAllText fixturePath

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-multishot-linear-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected the multishot capture rule to reject the operation site.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.QttContinuationCapture)
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.MultishotEffectUnsupportedBackend)

[<Fact>]
let ``multishot aliased effect labels still trigger capture and backend capability checks`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : (1 x : Int) -> Int"
            "let bad (1 x : Int) : Int ="
            "    block"
            "        scoped effect Choice ="
            "            ω choose : Unit -> Bool"
            ""
            "        let c = Choice"
            ""
            "        let comp : Eff <[Choice : Choice]> Int ="
            "            do"
            "                let b <- c.choose ()"
            "                if b then x else x"
            ""
            "        let handled ="
            "            deep handle Choice comp with"
            "                case return y -> pure y"
            "                case choose _ k ->"
            "                    do"
            "                        let a <- k True"
            "                        let _ <- k False"
            "                        pure a"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-multishot-alias-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected aliased multishot invocations to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.QttContinuationCapture)
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.MultishotEffectUnsupportedBackend)

[<Fact>]
let ``backends without multishot capability reject direct multishot invocations`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "result : Eff <[Choice : Choice]> Bool"
            "let result ="
            "    block"
            "        scoped effect Choice ="
            "            ω choose : Unit -> Bool"
            ""
            "        Choice.choose ()"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-m4-multishot-capability-root"
            "dotnet-il"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected backends without rt-multishot-effects to reject direct multishot invocation.")

    let diagnostic =
        workspace.Diagnostics
        |> List.tryFind (fun current -> current.Code = DiagnosticCode.MultishotEffectUnsupportedBackend)

    Assert.True(diagnostic.IsSome, "Expected a multishot backend-capability diagnostic.")
    Assert.Contains("Choice.choose", diagnostic.Value.Message)
    Assert.Contains("dotnet-il", diagnostic.Value.Message)

[<Fact>]
let ``backends without multishot capability reject exported declarations that may invoke multishot effects`` () =
    let mainSource =
        [
            "module main"
            ""
            "result : Eff <[Choice : Choice]> Bool"
            "let result ="
            "    block"
            "        scoped effect Choice ="
            "            ω choose : Unit -> Bool"
            ""
            "        Choice.choose ()"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-m4-multishot-export-root"
            "zig"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected exported multishot definitions to be rejected without backend capability.")

    let exportedDiagnostic =
        workspace.Diagnostics
        |> List.tryFind (fun diagnostic ->
            diagnostic.Code = DiagnosticCode.MultishotEffectUnsupportedBackend
            && diagnostic.Message.Contains("exported declaration 'result'", StringComparison.Ordinal))

    Assert.True(exportedDiagnostic.IsSome, "Expected an exported-declaration multishot backend-capability diagnostic.")
    Assert.Contains("Choice.choose", exportedDiagnostic.Value.Message)
    Assert.Contains("zig", exportedDiagnostic.Value.Message)

[<Fact>]
let ``shallow handler resumptions do not collapse into the handled carrier`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let acceptHandled : (1 k : ((1 x : Bool) -> Eff <[ ]> Int)) -> Eff <[ ]> Int ="
            "            \\(1 k : ((1 x : Bool) -> Eff <[ ]> Int)) -> k True"
            ""
            "        handle Ask comp with"
            "            case return y -> pure y"
            "            case ask () k ->"
            "                acceptHandled k"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-shallow-carrier-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected shallow handler resumptions with the handled carrier to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``one shot resumptions cannot be resumed twice`` () =
    let fixturePath =
        Path.Combine(
            __SOURCE_DIRECTORY__,
            "Fixtures",
            "borrow_qtt.100_interactions.handler_reject_oneshot_resumption_overuse",
            "main.kp"
        )

    let mainSource = File.ReadAllText fixturePath

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-oneshot-overuse-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected one-shot resumption overuse to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.QttLinearOveruse)

[<Fact>]
let ``shallow handlers type resumptions against the remainder row`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "probe : Int"
            "let probe : Int ="
            "    block"
            "        scoped effect Ask ="
            "            1 ask : Unit -> Bool"
            ""
            "        let handleAsk : forall (r : EffRow). Eff <[Ask : Ask | r]> Int -> Eff r Int ="
            "            \\(comp : Eff <[Ask : Ask | r]> Int) ->"
            "                handle Ask comp with"
            "                    case return y -> pure y"
            "                    case ask () k -> k True"
            ""
            "        0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-remainder-row-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected shallow handler remainder-row typing to succeed, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

[<Fact>]
let ``top level effect declarations introduce in-scope effect labels`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "effect Ask ="
            "    1 ask : Unit -> Bool"
            ""
            "ok : Int"
            "let ok : Int ="
            "    block"
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            deep handle Ask comp with"
            "                case return x -> pure x"
            "                case ask () k -> k True"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-top-level-effect-label-root"
            "main.ok"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected top-level effect labels to be in scope, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("1", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected top-level effect label evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``qualified imported effect labels remain usable across modules`` () =
    let libSource =
        [
            "module lib"
            ""
            "effect Ask ="
            "    1 ask : Unit -> Bool"
        ]
        |> String.concat "\n"

    let mainSource =
        [
            "@PrivateByDefault module main"
            "import lib"
            ""
            "ok : Int"
            "let ok : Int ="
            "    block"
            "        let comp : Eff <[lib.Ask : lib.Ask]> Int ="
            "            do"
            "                let b <- lib.Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            deep handle lib.Ask comp with"
            "                case return x -> pure x"
            "                case ask () k -> k True"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-imported-effect-label-root"
            "main.ok"
            [ "lib.kp", libSource
              "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected qualified imported effect labels to resolve, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("1", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected imported effect label evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``handlers reject open rows whose declared remainder does not account for the handled label`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "probe : Int"
            "let probe : Int ="
            "    block"
            "        scoped effect Ask ="
            "            1 ask : Unit -> Bool"
            ""
            "        scoped effect Other ="
            "            other : Unit -> Int"
            ""
            "        let badHandle : forall (r : EffRow). Eff <[Other : Other | r]> Int -> Eff r Int ="
            "            \\(comp : Eff <[Other : Other | r]> Int) ->"
            "                handle Ask comp with"
            "                    case return y -> pure y"
            "                    case ask () k -> k True"
            ""
            "        0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-open-row-mismatch-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected handler typing to reject open rows whose declared remainder cannot account for the handled label.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerEffectRowMismatch || diagnostic.Code = DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``handlers can split a handled label out of a normalized tail alias`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "effect Ask ="
            "    1 ask : Unit -> Bool"
            ""
            "effect Other ="
            "    other : Unit -> Int"
            ""
            "type AskTail (r : EffRow) = <[Ask : Ask | r]>"
            ""
            "probe : Int"
            "let probe : Int ="
            "    block"
            "        let handleAsk : forall (r : EffRow). Eff <[Other : Other | AskTail r]> Int -> Eff <[Other : Other | r]> Int ="
            "            \\(comp : Eff <[Other : Other | AskTail r]> Int) ->"
            "                handle Ask comp with"
            "                    case return y -> pure y"
            "                    case ask () k -> k True"
            ""
            "        0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-open-row-tail-alias-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected handler typing to split handled labels from normalized tail aliases, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

[<Fact>]
let ``deep handlers can split a handled label out of a normalized tail alias`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "effect Ask ="
            "    1 ask : Unit -> Bool"
            ""
            "effect Other ="
            "    other : Unit -> Int"
            ""
            "type AskTail (r : EffRow) = <[Ask : Ask | r]>"
            ""
            "probe : Int"
            "let probe : Int ="
            "    block"
            "        let handleAsk : forall (r : EffRow). Eff <[Other : Other | AskTail r]> Int -> Eff <[Other : Other | r]> Int ="
            "            \\(comp : Eff <[Other : Other | AskTail r]> Int) ->"
            "                deep handle Ask comp with"
            "                    case return y -> pure y"
            "                    case ask () k -> k True"
            ""
            "        0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-deep-open-row-tail-alias-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected deep handler typing to split handled labels from normalized tail aliases, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

[<Fact>]
let ``handlers reject normalized tail aliases whose handled label names a different interface`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "effect Ask ="
            "    1 ask : Unit -> Bool"
            ""
            "effect Other ="
            "    other : Unit -> Int"
            ""
            "type WrongAskTail (r : EffRow) = <[Ask : Other | r]>"
            ""
            "probe : Int"
            "let probe : Int ="
            "    block"
            "        let badHandle : forall (r : EffRow). Eff <[Other : Other | WrongAskTail r]> Int -> Eff <[Other : Other | r]> Int ="
            "            \\(comp : Eff <[Other : Other | WrongAskTail r]> Int) ->"
            "                handle Ask comp with"
            "                    case return y -> pure y"
            "                    case ask () k -> k True"
            ""
            "        0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-open-row-tail-alias-mismatch-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected normalized tail aliases with mismatched handled interfaces to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerEffectRowMismatch)

[<Fact>]
let ``handlers can split a handled label out of a local normalized tail alias`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "probe : Int"
            "let probe : Int ="
            "    block"
            "        scoped effect Ask ="
            "            1 ask : Unit -> Bool"
            ""
            "        scoped effect Other ="
            "            other : Unit -> Int"
            ""
            "        type AskTail (r : EffRow) = <[Ask : Ask | r]>"
            ""
            "        let handleAsk : forall (r : EffRow). Eff <[Other : Other | AskTail r]> Int -> Eff <[Other : Other | r]> Int ="
            "            \\(comp : Eff <[Other : Other | AskTail r]> Int) ->"
            "                handle Ask comp with"
            "                    case return y -> pure y"
            "                    case ask () k -> k True"
            ""
            "        0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-local-open-row-tail-alias-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected handlers to split handled labels from local normalized tail aliases, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

[<Fact>]
let ``deep handlers can split a handled label out of a local normalized tail alias`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "probe : Int"
            "let probe : Int ="
            "    block"
            "        scoped effect Ask ="
            "            1 ask : Unit -> Bool"
            ""
            "        scoped effect Other ="
            "            other : Unit -> Int"
            ""
            "        type AskTail (r : EffRow) = <[Ask : Ask | r]>"
            ""
            "        let handleAsk : forall (r : EffRow). Eff <[Other : Other | AskTail r]> Int -> Eff <[Other : Other | r]> Int ="
            "            \\(comp : Eff <[Other : Other | AskTail r]> Int) ->"
            "                deep handle Ask comp with"
            "                    case return y -> pure y"
            "                    case ask () k -> k True"
            ""
            "        0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-local-deep-open-row-tail-alias-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected deep handlers to split handled labels from local normalized tail aliases, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

[<Fact>]
let ``deep handlers reject local normalized tail aliases whose handled label names a different interface`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "probe : Int"
            "let probe : Int ="
            "    block"
            "        scoped effect Ask ="
            "            1 ask : Unit -> Bool"
            ""
            "        scoped effect Other ="
            "            other : Unit -> Int"
            ""
            "        type WrongAskTail (r : EffRow) = <[Ask : Other | r]>"
            ""
            "        let badHandle : forall (r : EffRow). Eff <[Other : Other | WrongAskTail r]> Int -> Eff <[Other : Other | r]> Int ="
            "            \\(comp : Eff <[Other : Other | WrongAskTail r]> Int) ->"
            "                deep handle Ask comp with"
            "                    case return y -> pure y"
            "                    case ask () k -> k True"
            ""
            "        0"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-local-deep-open-row-tail-alias-mismatch-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected deep handlers to reject local normalized tail aliases with mismatched handled interfaces.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.HandlerEffectRowMismatch)
