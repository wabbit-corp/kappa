namespace Kappa.Compiler

type NumericLiteralParseError =
    | InvalidNumericLiteral of tokenText: string

type UrlModuleSpecifierParseError =
    | MissingBaseUrl
    | EmptyPin
    | MissingSha256Digest
    | InvalidSha256Digest of pinText: string
    | EmptyRefPin
    | UnsupportedPin of pinText: string

type StringLiteralDecodeError =
    | UnknownEscapeSequence of escapeText: string
    | InvalidUnicodeEscape of escapeText: string
    | UnterminatedEscapeSequence
    | UnterminatedUnicodeEscapeSequence
    | InvalidMultilineClosingDelimiterIndentation
    | MultilineContentIndentationMismatch
    | InvalidRawMultilineStringLiteral
    | InvalidRawStringLiteral
