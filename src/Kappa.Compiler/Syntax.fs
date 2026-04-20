namespace Kappa.Compiler

open System
open System.Text

type Keyword =
    | As
    | Asc
    | AssertTotal
    | Break
    | By
    | Case
    | Clarify
    | Continue
    | Ctor
    | Data
    | Desc
    | Derive
    | Distinct
    | Do
    | Elif
    | Else
    | Except
    | Expect
    | Export
    | Finally
    | For
    | Forall
    | Group
    | If
    | Import
    | Impossible
    | In
    | Infix
    | Instance
    | Is
    | Join
    | Left
    | Let
    | Match
    | Module
    | Opaque
    | Order
    | Postfix
    | Prefix
    | Private
    | Public
    | Return
    | Right
    | Skip
    | Take
    | Term
    | Then
    | Top
    | Trait
    | Try
    | Type
    | Unhide
    | Using
    | Var
    | While
    | Yield

module Keyword =
    let private entries =
        [
            "as", As
            "asc", Asc
            "assertTotal", AssertTotal
            "break", Break
            "by", By
            "case", Case
            "clarify", Clarify
            "continue", Continue
            "ctor", Ctor
            "data", Data
            "desc", Desc
            "derive", Derive
            "distinct", Distinct
            "do", Do
            "elif", Elif
            "else", Else
            "except", Except
            "expect", Expect
            "export", Export
            "finally", Finally
            "for", For
            "forall", Forall
            "group", Group
            "if", If
            "import", Import
            "impossible", Impossible
            "in", In
            "infix", Infix
            "instance", Instance
            "is", Is
            "join", Join
            "left", Left
            "let", Let
            "match", Match
            "module", Module
            "opaque", Opaque
            "order", Order
            "postfix", Postfix
            "prefix", Prefix
            "private", Private
            "public", Public
            "return", Return
            "right", Right
            "skip", Skip
            "take", Take
            "term", Term
            "then", Then
            "top", Top
            "trait", Trait
            "try", Try
            "type", Type
            "unhide", Unhide
            "using", Using
            "var", Var
            "while", While
            "yield", Yield
        ]
        |> dict

    let tryParse (text: string) =
        match entries.TryGetValue(text) with
        | true, keyword -> Some keyword
        | _ -> None

    let toText keyword =
        entries
        |> Seq.find (fun pair -> pair.Value = keyword)
        |> fun pair -> pair.Key

type TokenKind =
    | Identifier
    | IntegerLiteral
    | FloatLiteral
    | StringLiteral
    | InterpolatedStringStart
    | StringTextSegment
    | InterpolationStart
    | InterpolationEnd
    | InterpolatedStringEnd
    | CharacterLiteral
    | Keyword of Keyword
    | Operator
    | AtSign
    | Backslash
    | Colon
    | Comma
    | Dot
    | Equals
    | Arrow
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | LeftBrace
    | RightBrace
    | LeftSetBrace
    | RightSetBrace
    | Underscore
    | Newline
    | Indent
    | Dedent
    | EndOfFile
    | BadToken

type Token =
    { Kind: TokenKind
      Text: string
      Span: TextSpan }

type Visibility =
    | Public
    | Private

type ModuleSpecifier =
    | Dotted of string list
    | Url of string

type ImportNamespace =
    | Term
    | Type
    | Trait
    | Constructor

type ImportItemModifier =
    | Unhide
    | Clarify

type ImportItem =
    { Modifiers: ImportItemModifier list
      Namespace: ImportNamespace option
      Name: string }

type ImportSelection =
    | QualifiedOnly
    | Items of ImportItem list
    | All
    | AllExcept of string list

type ImportSpec =
    { Source: ModuleSpecifier
      Alias: string option
      Selection: ImportSelection }

type LiteralValue =
    | Integer of int64
    | Float of double
    | String of string
    | Character of char
    | Unit

type Parameter =
    { Name: string
      TypeTokens: Token list option }

type FixityAssociativity =
    | NonAssociative
    | LeftAssociative
    | RightAssociative

type FixityKind =
    | Infix of FixityAssociativity
    | Prefix
    | Postfix

type FixityDeclaration =
    { Fixity: FixityKind
      Precedence: int
      OperatorName: string }

type CoreExpression =
    | Literal of LiteralValue
    | Name of string list
    | Lambda of Parameter list * CoreExpression
    | IfThenElse of CoreExpression * CoreExpression * CoreExpression
    | Match of CoreExpression * MatchCase list
    | Do of DoStatement list
    | MonadicSplice of CoreExpression
    | Apply of CoreExpression * CoreExpression list
    | Unary of operatorName: string * CoreExpression
    | Binary of CoreExpression * operatorName: string * CoreExpression
    | PrefixedString of prefix: string * parts: InterpolatedStringPart list

and InterpolatedStringPart =
    | StringText of string
    | StringInterpolation of CoreExpression

and CorePattern =
    | WildcardPattern
    | NamePattern of string
    | LiteralPattern of LiteralValue
    | ConstructorPattern of string list * CorePattern list

and MatchCase =
    { Pattern: CorePattern
      Body: CoreExpression }

and DoStatement =
    | DoLet of string * CoreExpression
    | DoBind of string * CoreExpression
    | DoVar of string * CoreExpression
    | DoAssign of string * CoreExpression
    | DoWhile of condition: CoreExpression * body: DoStatement list
    | DoExpression of CoreExpression

type BindingSignature =
    { Visibility: Visibility option
      IsOpaque: bool
      Name: string
      TypeTokens: Token list }

type ExpectedTypeDeclaration =
    { Name: string
      HeaderTokens: Token list
      Span: TextSpan }

type ExpectedTraitDeclaration =
    { Name: string
      HeaderTokens: Token list
      Span: TextSpan }

type ExpectedTermDeclaration =
    { Name: string
      TypeTokens: Token list
      Span: TextSpan }

type ExpectDeclaration =
    | ExpectTypeDeclaration of ExpectedTypeDeclaration
    | ExpectTraitDeclaration of ExpectedTraitDeclaration
    | ExpectTermDeclaration of ExpectedTermDeclaration

type LetDefinition =
    { Visibility: Visibility option
      IsOpaque: bool
      Name: string option
      Parameters: Parameter list
      HeaderTokens: Token list
      ReturnTypeTokens: Token list option
      BodyTokens: Token list
      Body: CoreExpression option }

type DataConstructor =
    { Name: string
      Tokens: Token list
      Arity: int }

type DataDeclaration =
    { Visibility: Visibility option
      IsOpaque: bool
      Name: string
      HeaderTokens: Token list
      Constructors: DataConstructor list }

type TypeAlias =
    { Visibility: Visibility option
      IsOpaque: bool
      Name: string
      HeaderTokens: Token list
      BodyTokens: Token list option }

type TraitMember =
    { Name: string option
      Tokens: Token list }

type TraitDeclaration =
    { Visibility: Visibility option
      Name: string
      HeaderTokens: Token list
      Members: TraitMember list }

type InstanceDeclaration =
    { TraitName: string
      HeaderTokens: Token list
      Members: LetDefinition list }

type TopLevelDeclaration =
    | ImportDeclaration of isExport: bool * specs: ImportSpec list
    | FixityDeclarationNode of FixityDeclaration
    | ExpectDeclarationNode of ExpectDeclaration
    | SignatureDeclaration of BindingSignature
    | LetDeclaration of LetDefinition
    | DataDeclarationNode of DataDeclaration
    | TypeAliasNode of TypeAlias
    | TraitDeclarationNode of TraitDeclaration
    | InstanceDeclarationNode of InstanceDeclaration
    | UnknownDeclaration of Token list

type CompilationUnit =
    { ModuleAttributes: string list
      ModuleHeader: string list option
      Declarations: TopLevelDeclaration list
      Tokens: Token list }

module Token =
    let isKeyword keyword token =
        match token.Kind with
        | Keyword value when value = keyword -> true
        | _ -> false

    let isName token =
        match token.Kind with
        | Identifier
        | Keyword _ -> true
        | _ -> false

module SyntaxFacts =
    let isIdentifierStart character = Char.IsLetter(character) || character = '_'

    let isIdentifierPart character =
        Char.IsLetterOrDigit(character) || character = '_'

    let isOperatorCharacter character =
        "!$%&*+-./:<=>?@^|~".Contains(string character)

    let trimIdentifierQuotes (value: string) =
        if value.Length >= 2 && value[0] = '`' && value[value.Length - 1] = '`' then
            value.Substring(1, value.Length - 2)
        else
            value

    let trimStringQuotes (value: string) =
        if value.Length >= 2 && value[0] = '"' && value[value.Length - 1] = '"' then
            value.Substring(1, value.Length - 2)
        else
            value

    let tryUnescapeStringContent (value: string) =
        let builder = StringBuilder()
        let mutable index = 0
        let mutable error = None

        let appendCodePoint (codePointText: string) =
            match Int32.TryParse(codePointText, Globalization.NumberStyles.HexNumber, null) with
            | true, value when value > int Char.MaxValue || Char.IsSurrogate(char value) ->
                error <- Some $"Invalid Unicode escape '\\u{codePointText}'."
            | true, value ->
                builder.Append(char value) |> ignore
            | _ ->
                error <- Some $"Invalid Unicode escape '\\u{codePointText}'."

        while index < value.Length && error.IsNone do
            if value[index] <> '\\' then
                builder.Append(value[index]) |> ignore
                index <- index + 1
            elif index + 1 >= value.Length then
                error <- Some "Unterminated escape sequence."
            else
                match value[index + 1] with
                | '\\' ->
                    builder.Append('\\') |> ignore
                    index <- index + 2
                | '"' ->
                    builder.Append('"') |> ignore
                    index <- index + 2
                | '\'' ->
                    builder.Append('\'') |> ignore
                    index <- index + 2
                | '$' ->
                    builder.Append('$') |> ignore
                    index <- index + 2
                | 'n' ->
                    builder.Append('\n') |> ignore
                    index <- index + 2
                | 't' ->
                    builder.Append('\t') |> ignore
                    index <- index + 2
                | 'r' ->
                    builder.Append('\r') |> ignore
                    index <- index + 2
                | 'b' ->
                    builder.Append('\b') |> ignore
                    index <- index + 2
                | 'u' when index + 5 < value.Length ->
                    appendCodePoint (value.Substring(index + 2, 4))
                    index <- index + 6
                | other ->
                    error <- Some $"Unknown escape sequence '\\{other}'."

        match error with
        | Some message -> Result.Error message
        | None -> Result.Ok(builder.ToString())

    let tryDecodeStringLiteral (value: string) =
        trimStringQuotes value
        |> tryUnescapeStringContent

    let tryDecodeCharacterLiteral (value: string) =
        if value.Length >= 2 && value[0] = '\'' && value[value.Length - 1] = '\'' then
            match tryUnescapeStringContent (value.Substring(1, value.Length - 2)) with
            | Result.Ok text when text.Length = 1 ->
                Result.Ok text[0]
            | Result.Ok _ ->
                Result.Error "Character literals must decode to exactly one character."
            | Result.Error message ->
                Result.Error message
        else
            Result.Error "Invalid character literal."

    let moduleNameToText (segments: string seq) = String.Join(".", segments)

type FixityEntry =
    { Infix: (FixityAssociativity * int) option
      Prefix: int option
      Postfix: int option }

type FixityTable = Map<string, FixityEntry>

module FixityTable =
    let empty : FixityTable = Map.empty

    let private emptyEntry =
        { Infix = None
          Prefix = None
          Postfix = None }

    let add (declaration: FixityDeclaration) (table: FixityTable) =
        let existing =
            table
            |> Map.tryFind declaration.OperatorName
            |> Option.defaultValue emptyEntry

        let updated =
            match declaration.Fixity with
            | Infix associativity ->
                { existing with
                    Infix = Some(associativity, declaration.Precedence) }
            | Prefix ->
                { existing with
                    Prefix = Some declaration.Precedence }
            | Postfix ->
                { existing with
                    Postfix = Some declaration.Precedence }

        table.Add(declaration.OperatorName, updated)

    let tryFindInfix operatorName (table: FixityTable) =
        table
        |> Map.tryFind operatorName
        |> Option.bind (fun entry -> entry.Infix)

    let tryFindPrefix operatorName (table: FixityTable) =
        table
        |> Map.tryFind operatorName
        |> Option.bind (fun entry -> entry.Prefix)

    let tryFindPostfix operatorName (table: FixityTable) =
        table
        |> Map.tryFind operatorName
        |> Option.bind (fun entry -> entry.Postfix)

    let defaultPrelude =
        empty
        |> add
            { Fixity = Infix LeftAssociative
              Precedence = 30
              OperatorName = "||" }
        |> add
            { Fixity = Infix LeftAssociative
              Precedence = 40
              OperatorName = "&&" }
        |> add
            { Fixity = Infix NonAssociative
              Precedence = 50
              OperatorName = "==" }
        |> add
            { Fixity = Infix NonAssociative
              Precedence = 50
              OperatorName = "!=" }
        |> add
            { Fixity = Infix NonAssociative
              Precedence = 50
              OperatorName = "<" }
        |> add
            { Fixity = Infix NonAssociative
              Precedence = 50
              OperatorName = "<=" }
        |> add
            { Fixity = Infix NonAssociative
              Precedence = 50
              OperatorName = ">" }
        |> add
            { Fixity = Infix NonAssociative
              Precedence = 50
              OperatorName = ">=" }
        |> add
            { Fixity = Infix RightAssociative
              Precedence = 40
              OperatorName = "::" }
        |> add
            { Fixity = Infix LeftAssociative
              Precedence = 60
              OperatorName = "+" }
        |> add
            { Fixity = Infix LeftAssociative
              Precedence = 60
              OperatorName = "-" }
        |> add
            { Fixity = Infix LeftAssociative
              Precedence = 70
              OperatorName = "*" }
        |> add
            { Fixity = Infix LeftAssociative
              Precedence = 70
              OperatorName = "/" }
        |> add
            { Fixity = Prefix
              Precedence = 80
              OperatorName = "-" }
