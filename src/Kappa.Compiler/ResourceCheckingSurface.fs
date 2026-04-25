namespace Kappa.Compiler

open System

// Walks surface syntax into the lightweight forms consumed by resource checking.
module internal ResourceCheckingSurface =
    let collectPatternNames (pattern: SurfacePattern) =
        let rec loop current =
            seq {
                match current with
                | NamePattern name -> yield name
                | ConstructorPattern(_, arguments) ->
                    for argument in arguments do
                        yield! loop argument
                | OrPattern alternatives ->
                    for alternative in alternatives do
                        yield! loop alternative
                | AnonymousRecordPattern fields ->
                    for field in fields do
                        yield! loop field.Pattern
                | WildcardPattern
                | LiteralPattern _ -> ()
            }

        loop pattern |> Seq.toList

    let rec expressionNames (expression: SurfaceExpression) =
        seq {
            match expression with
            | Literal _ -> ()
            | KindQualifiedName _ -> ()
            | Name(root :: _) -> yield root
            | Name [] -> ()
            | LocalLet(binding, value, body) ->
                yield! expressionNames value

                let boundNames =
                    collectPatternNames binding.Pattern
                    |> Set.ofList

                for name in expressionNames body do
                    if not (Set.contains name boundNames) then
                        yield name
            | LocalScopedEffect(_, body) ->
                yield! expressionNames body
            | Lambda(parameters, body) ->
                let parameterNames =
                    parameters
                    |> List.map (fun parameter -> parameter.Name)
                    |> Set.ofList

                for name in expressionNames body do
                    if not (Set.contains name parameterNames) then
                        yield name
            | IfThenElse(condition, whenTrue, whenFalse) ->
                yield! expressionNames condition
                yield! expressionNames whenTrue
                yield! expressionNames whenFalse
            | Match(scrutinee, cases) ->
                yield! expressionNames scrutinee

                for caseClause in cases do
                    match caseClause.Guard with
                    | Some guard -> yield! expressionNames guard
                    | None -> ()

                    yield! expressionNames caseClause.Body
            | RecordLiteral fields ->
                for field in fields do
                    yield! expressionNames field.Value
            | Seal(value, _) ->
                yield! expressionNames value
            | RecordUpdate(receiver, fields) ->
                yield! expressionNames receiver

                for field in fields do
                    yield! expressionNames field.Value
            | MemberAccess(receiver, _, arguments) ->
                yield! expressionNames receiver

                for argument in arguments do
                    yield! expressionNames argument
            | SafeNavigation(receiver, navigation) ->
                yield! expressionNames receiver

                for argument in navigation.Arguments do
                    yield! expressionNames argument
            | TagTest(receiver, _) ->
                yield! expressionNames receiver
            | Do statements ->
                for statement in statements do
                    yield! doStatementNames statement
            | MonadicSplice inner
            | ExplicitImplicitArgument inner
            | InoutArgument inner
            | Unary(_, inner) ->
                yield! expressionNames inner
            | NamedApplicationBlock fields ->
                for field in fields do
                    yield! expressionNames field.Value
            | Apply(callee, arguments) ->
                yield! expressionNames callee

                for argument in arguments do
                    yield! expressionNames argument
            | Binary(left, _, right) ->
                yield! expressionNames left
                yield! expressionNames right
            | Elvis(left, right) ->
                yield! expressionNames left
                yield! expressionNames right
            | PrefixedString(_, parts) ->
                for part in parts do
                    match part with
                    | StringText _ -> ()
                    | StringInterpolation inner -> yield! expressionNames inner
        }

    and private doStatementNames (statement: SurfaceDoStatement) =
        seq {
            match statement with
            | DoLet(_, expression)
            | DoBind(_, expression)
            | DoVar(_, expression)
            | DoAssign(_, expression)
            | DoUsing(_, expression)
            | DoReturn expression
            | DoExpression expression ->
                yield! expressionNames expression
            | DoLetQuestion(_, expression, failure) ->
                yield! expressionNames expression

                match failure with
                | Some failure ->
                    for statement in failure.Body do
                        yield! doStatementNames statement
                | None -> ()
            | DoIf(condition, whenTrue, whenFalse) ->
                yield! expressionNames condition

                for statement in whenTrue do
                    yield! doStatementNames statement

                for statement in whenFalse do
                    yield! doStatementNames statement
            | DoWhile(condition, body) ->
                yield! expressionNames condition

                for statement in body do
                    yield! doStatementNames statement
        }

    let tryCalleeName (expression: SurfaceExpression) =
        match expression with
        | Name [ name ] -> Some name
        | _ -> None
