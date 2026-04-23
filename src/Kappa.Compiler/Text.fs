namespace Kappa.Compiler

open System

// Shared source-text, span, and location primitives used by lexing, parsing, and diagnostics.
[<Struct>]
type TextSpan =
    { Start: int
      Length: int }

    member this.End = this.Start + this.Length

    static member FromBounds(startIndex: int, endIndex: int) =
        { Start = startIndex
          Length = max 0 (endIndex - startIndex) }

[<Struct>]
type LinePosition =
    { Line: int
      Column: int }

[<Struct>]
type SourceLocation =
    { FilePath: string
      Span: TextSpan
      Start: LinePosition
      End: LinePosition }

type SourceText private (filePath: string, content: string, lineStarts: int array) =
    member _.FilePath = filePath
    member _.Content = content
    member _.Length = content.Length
    member _.LineCount = lineStarts.Length
    member _.LineStarts = lineStarts

    member _.GetLineText(lineIndex: int) =
        if lineIndex < 0 || lineIndex >= lineStarts.Length then
            invalidArg (nameof lineIndex) "Line index is out of range."

        let startIndex = lineStarts[lineIndex]

        let mutable endIndex =
            if lineIndex + 1 < lineStarts.Length then
                lineStarts[lineIndex + 1]
            else
                content.Length

        if endIndex > startIndex && content[endIndex - 1] = '\n' then
            endIndex <- endIndex - 1

        if endIndex > startIndex && content[endIndex - 1] = '\r' then
            endIndex <- endIndex - 1

        content.Substring(startIndex, endIndex - startIndex)

    member _.Slice(span: TextSpan) =
        let safeStart = Math.Clamp(span.Start, 0, content.Length)
        let safeEnd = Math.Clamp(span.End, safeStart, content.Length)
        content.Substring(safeStart, safeEnd - safeStart)

    member _.GetLinePosition(index: int) =
        let bounded = Math.Clamp(index, 0, content.Length)
        let mutable low = 0
        let mutable high = lineStarts.Length - 1
        let mutable result = 0

        while low <= high do
            let middle = low + ((high - low) / 2)

            if lineStarts[middle] <= bounded then
                result <- middle
                low <- middle + 1
            else
                high <- middle - 1

        { Line = result + 1
          Column = bounded - lineStarts[result] + 1 }

    member this.GetLocation(span: TextSpan) =
        let safeStart = Math.Clamp(span.Start, 0, content.Length)
        let safeEnd = Math.Clamp(span.End, safeStart, content.Length)
        let safeSpan = TextSpan.FromBounds(safeStart, safeEnd)

        { FilePath = filePath
          Span = safeSpan
          Start = this.GetLinePosition(safeSpan.Start)
          End = this.GetLinePosition(safeSpan.End) }

    static member From(filePath: string, content: string) =
        let starts = ResizeArray<int>()
        starts.Add(0)

        for index in 0 .. content.Length - 1 do
            if content[index] = '\n' then
                starts.Add(index + 1)

        SourceText(filePath, content, starts.ToArray())
