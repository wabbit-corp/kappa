namespace Kappa.Compiler

open System

// Walks surface syntax into the lightweight forms consumed by resource checking.
module internal ResourceCheckingSurface =
    let collectPatternNames (pattern: SurfacePattern) =
        let rec loop current =
            seq {
                match current with
                | NamePattern name -> yield name
                | AsPattern(name, inner) ->
                    yield name
                    yield! loop inner
                | TypedPattern(inner, _) ->
                    yield! loop inner
                | ConstructorPattern(_, arguments) ->
                    for argument in arguments do
                        yield! loop argument
                | NamedConstructorPattern(_, fields) ->
                    for field in fields do
                        yield! loop field.Pattern
                | TuplePattern elements ->
                    for element in elements do
                        yield! loop element
                | VariantPattern(BoundVariantPattern(name, _))
                | VariantPattern(RestVariantPattern name) ->
                    yield name
                | VariantPattern(WildcardVariantPattern _) ->
                    ()
                | OrPattern alternatives ->
                    for alternative in alternatives do
                        yield! loop alternative
                | AnonymousRecordPattern(fields, rest) ->
                    for field in fields do
                        yield! loop field.Pattern
                    match rest with
                    | Some(BindRecordPatternRest name) -> yield name
                    | _ -> ()
                | WildcardPattern
                | LiteralPattern _ -> ()
            }

        loop pattern |> Seq.toList

    let rec expressionNames (expression: SurfaceExpression) =
        let expressionNamesInComprehension (comprehension: SurfaceComprehension) =
            let rec loop shadowed clauses =
                seq {
                    match clauses with
                    | [] ->
                        for name in expressionNames comprehension.Yield do
                            if not (Set.contains name shadowed) then
                                yield name
                    | clause :: rest ->
                        match clause with
                        | ForClause(_, _, binding, source) ->
                            for name in expressionNames source do
                                if not (Set.contains name shadowed) then
                                    yield name

                            let nextShadowed =
                                shadowed
                                |> Set.union (collectPatternNames binding.Pattern |> Set.ofList)

                            yield! loop nextShadowed rest
                        | LetClause(_, binding, value) ->
                            for name in expressionNames value do
                                if not (Set.contains name shadowed) then
                                    yield name

                            let nextShadowed =
                                shadowed
                                |> Set.union (collectPatternNames binding.Pattern |> Set.ofList)

                            yield! loop nextShadowed rest
                        | IfClause condition ->
                            for name in expressionNames condition do
                                if not (Set.contains name shadowed) then
                                    yield name
                            yield! loop shadowed rest
                        | OrderByClause key ->
                            for name in expressionNames key do
                                if not (Set.contains name shadowed) then
                                    yield name
                            yield! loop shadowed rest
                        | DistinctClause ->
                            yield! loop shadowed rest
                        | DistinctByClause key ->
                            for name in expressionNames key do
                                if not (Set.contains name shadowed) then
                                    yield name
                            yield! loop shadowed rest
                        | SkipClause count ->
                            for name in expressionNames count do
                                if not (Set.contains name shadowed) then
                                    yield name
                            yield! loop shadowed rest
                        | TakeClause count ->
                            for name in expressionNames count do
                                if not (Set.contains name shadowed) then
                                    yield name
                            yield! loop shadowed rest
                        | LeftJoinClause(binding, source, condition, intoName) ->
                            for name in expressionNames source do
                                if not (Set.contains name shadowed) then
                                    yield name

                            let joinShadowed =
                                shadowed
                                |> Set.union (collectPatternNames binding.Pattern |> Set.ofList)

                            for name in expressionNames condition do
                                if not (Set.contains name joinShadowed) then
                                    yield name

                            yield! loop (Set.add intoName shadowed) rest
                }

            loop Set.empty comprehension.Clauses

        seq {
            match expression with
            | Literal _ -> ()
            | NumericLiteral _ -> ()
            | KindQualifiedName _ -> ()
            | Name(root :: _) -> yield root
            | Name [] -> ()
            | SyntaxQuote inner
            | SyntaxSplice inner
            | TopLevelSyntaxSplice inner
            | CodeQuote inner
            | CodeSplice inner ->
                yield! expressionNames inner
            | LocalLet(binding, value, body) ->
                yield! expressionNames value

                let boundNames =
                    collectPatternNames binding.Pattern
                    |> Set.ofList

                for name in expressionNames body do
                    if not (Set.contains name boundNames) then
                        yield name
            | LocalSignature(declaration, body) ->
                for name in expressionNames body do
                    if not (String.Equals(name, declaration.Name, StringComparison.Ordinal)) then
                        yield name
            | LocalTypeAlias(_, body) ->
                yield! expressionNames body
            | LocalScopedEffect(_, body) ->
                yield! expressionNames body
            | Handle(_, label, body, returnClause, operationClauses) ->
                yield! expressionNames label
                yield! expressionNames body
                yield! expressionNames returnClause.Body

                for clause in operationClauses do
                    yield! expressionNames clause.Body
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
            | Comprehension comprehension ->
                yield! expressionNamesInComprehension comprehension
            | PrefixedString(_, parts) ->
                for part in parts do
                    match part with
                    | StringText _ -> ()
                    | StringInterpolation(inner, _) -> yield! expressionNames inner
        }

    and private doStatementNames (statement: SurfaceDoStatement) =
        seq {
            match statement with
            | DoLet(_, expression)
            | DoBind(_, expression)
            | DoVar(_, expression)
            | DoAssign(_, expression)
            | DoUsing(_, expression)
            | DoDefer expression
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
        | TopLevelSyntaxSplice(Name [ name ]) -> Some name
        | _ -> None
