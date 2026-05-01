namespace Kappa.Compiler

type PrefixedIntegerBase =
    | BinaryBase
    | OctalBase
    | HexadecimalBase

type NumericTokenScanError =
    | MalformedPrefixedIntegerLiteral of integerBase: PrefixedIntegerBase

type NumericLiteralParseError =
    | InvalidNumericLiteral of tokenText: string

type StringLiteralDecodeError =
    | UnknownEscapeSequence of escapeText: string
    | InvalidUnicodeEscape of escapeText: string
    | UnterminatedEscapeSequence
    | UnterminatedUnicodeEscapeSequence
    | InvalidMultilineClosingDelimiterIndentation
    | MultilineContentIndentationMismatch
    | InvalidRawMultilineStringLiteral
    | InvalidRawStringLiteral

type UnicodeScalarLiteralDecodeError =
    | UnicodeScalarInvalidLiteralForm
    | UnicodeScalarTextInvalid of StringLiteralDecodeError
    | UnicodeScalarMustDecodeToExactlyOneScalar

type GraphemeLiteralDecodeError =
    | GraphemeInvalidLiteralForm
    | GraphemeTextInvalid of StringLiteralDecodeError
    | GraphemeMustDecodeToExactlyOneExtendedCluster

type ByteLiteralDecodeError =
    | ByteInvalidLiteralForm
    | ByteInvalidEscape of escapeText: string
    | ByteInvalidUnicodeEscape of escapeText: string
    | ByteUnknownEscapeSequence of escapeText: string
    | ByteUnterminatedEscapeSequence
    | ByteUnterminatedUnicodeEscapeSequence
    | ByteMustDecodeToExactlyOneByte

type UrlModuleSpecifierParseError =
    | MissingBaseUrl
    | EmptyPin
    | MissingSha256Digest
    | InvalidSha256Digest of pinText: string
    | EmptyRefPin
    | UnsupportedPin of pinText: string
