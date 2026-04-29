namespace Kappa.Compiler

open System
open System.Globalization
open System.Buffers.Binary
open System.Text

// Centralizes Unicode scalar, grapheme, UTF-8, normalization, and hashing helpers.
module UnicodeText =
    type NormalizationFormName =
        | NFC
        | NFD
        | NFKC
        | NFKD

    let private utf8Strict = UTF8Encoding(false, true)

    let decodeUtf8Strict (bytes: byte array) =
        try
            Result.Ok(utf8Strict.GetString(bytes))
        with :? DecoderFallbackException as ex ->
            Result.Error ex.Message

    let encodeUtf8 (text: string) =
        Encoding.UTF8.GetBytes(text)

    let normalize form (text: string) =
        match form with
        | NFC -> text.Normalize(NormalizationForm.FormC)
        | NFD -> text.Normalize(NormalizationForm.FormD)
        | NFKC -> text.Normalize(NormalizationForm.FormKC)
        | NFKD -> text.Normalize(NormalizationForm.FormKD)

    let isNormalized form (text: string) =
        let targetForm =
            match form with
            | NFC -> NormalizationForm.FormC
            | NFD -> NormalizationForm.FormD
            | NFKC -> NormalizationForm.FormKC
            | NFKD -> NormalizationForm.FormKD

        text.IsNormalized(targetForm)

    let canonicalEquivalent left right =
        normalize NFC left = normalize NFC right

    let scalarCount (text: string) =
        text.EnumerateRunes() |> Seq.length

    let scalarValues (text: string) =
        text.EnumerateRunes() |> Seq.toList

    let compareScalarSequences (left: string) (right: string) =
        let rec loop (leftRunes: Rune list) (rightRunes: Rune list) =
            match leftRunes, rightRunes with
            | [], [] ->
                0
            | [], _ :: _ ->
                -1
            | _ :: _, [] ->
                1
            | leftRune :: remainingLeft, rightRune :: remainingRight ->
                let comparison = compare leftRune.Value rightRune.Value

                if comparison <> 0 then
                    comparison
                else
                    loop remainingLeft remainingRight

        loop (scalarValues left) (scalarValues right)

    let trySingleScalar (text: string) =
        let runes = scalarValues text

        match runes with
        | [ rune ] -> Some rune
        | _ -> None

    let scalarToString (rune: Rune) =
        rune.ToString()

    let tryScalarFromValue (value: int64) =
        if value < 0L || value > int64 0x10FFFF then
            None
        else
            let codePoint = int value

            if codePoint >= 0xD800 && codePoint <= 0xDFFF then
                None
            else
                Rune.TryCreate(codePoint)
                |> function
                    | true, rune -> Some rune
                    | _ -> None

    let scalarValue (rune: Rune) =
        int64 rune.Value

    let graphemeBoundaries (text: string) =
        StringInfo.ParseCombiningCharacters(text) |> Array.toList

    let graphemeCount (text: string) =
        if String.IsNullOrEmpty(text) then
            0
        else
            graphemeBoundaries text |> List.length

    let graphemes (text: string) =
        let starts = graphemeBoundaries text

        starts
        |> List.mapi (fun index start ->
            let nextStart =
                starts
                |> List.tryItem (index + 1)
                |> Option.defaultValue text.Length

            text.Substring(start, nextStart - start))

    let trySingleGrapheme (text: string) =
        match graphemes text with
        | [ grapheme ] when not (String.IsNullOrEmpty(grapheme)) -> Some grapheme
        | _ -> None

    let byteLength (text: string) =
        encodeUtf8 text |> Array.length |> int64

    let trySingleByte (text: string) =
        trySingleScalar text
        |> Option.bind (fun rune ->
            if rune.Value >= 0 && rune.Value <= 0xFF then
                Some(byte rune.Value)
            else
                None)

    let words (text: string) =
        text.Split([| ' '; '\t'; '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    let sentences (text: string) =
        text.Split([| '.'; '!'; '?' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun sentence -> sentence.Trim())
        |> Array.filter (String.IsNullOrWhiteSpace >> not)
        |> Array.toList

    let private fnvOffset = 1469598103934665603UL
    let private fnvPrime = 1099511628211UL

    let initHashState (seed: int64) =
        uint64 seed ^^^ fnvOffset

    let updateHashStateWithBytes (state: uint64) (bytes: byte array) =
        let mutable nextState = state

        for value in bytes do
            nextState <- nextState ^^^ uint64 value
            nextState <- nextState * fnvPrime

        nextState

    let finishHashState (state: uint64) =
        int64 state

    let hashBytesWithSeed (seed: int64) (bytes: byte array) =
        initHashState seed
        |> fun state -> updateHashStateWithBytes state bytes
        |> finishHashState

    let int64ToLittleEndianBytes (value: int64) =
        let bytes = Array.zeroCreate<byte> 8
        BinaryPrimitives.WriteInt64LittleEndian(bytes, value)
        bytes

    let uint64ToLittleEndianBytes (value: uint64) =
        let bytes = Array.zeroCreate<byte> 8
        BinaryPrimitives.WriteUInt64LittleEndian(bytes, value)
        bytes

    let doubleToLittleEndianBytes (value: double) =
        value
        |> BitConverter.DoubleToUInt64Bits
        |> uint64ToLittleEndianBytes
