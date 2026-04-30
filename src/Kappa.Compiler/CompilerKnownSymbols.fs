namespace Kappa.Compiler

// Canonical compiler-known symbolic names and paths. Semantic phases should import these rather than hardcoding raw strings.
module CompilerKnownSymbols =
    module KnownModules =
        let Prelude = [ "std"; "prelude" ]
        let Hash = [ "std"; "hash" ]
        let Unicode = [ "std"; "unicode" ]
        let Bytes = [ "std"; "bytes" ]

    module KnownTypeNames =
        [<Literal>]
        let Unit = "Unit"

        [<Literal>]
        let Bool = "Bool"

        [<Literal>]
        let Byte = "Byte"

        [<Literal>]
        let Bytes = "Bytes"

        [<Literal>]
        let Char = "Char"

        [<Literal>]
        let Double = "Double"

        [<Literal>]
        let Dict = "Dict"

        [<Literal>]
        let Float = "Float"

        [<Literal>]
        let Grapheme = "Grapheme"

        [<Literal>]
        let HashCode = "HashCode"

        [<Literal>]
        let Int = "Int"

        [<Literal>]
        let Integer = "Integer"

        [<Literal>]
        let IO = "IO"

        [<Literal>]
        let Nat = "Nat"

        [<Literal>]
        let Option = "Option"

        [<Literal>]
        let Ordering = "Ordering"

        [<Literal>]
        let Real = "Real"

        [<Literal>]
        let Ref = "Ref"

        [<Literal>]
        let String = "String"

        [<Literal>]
        let UIO = "UIO"

        [<Literal>]
        let UnicodeScalar = "UnicodeScalar"

        [<Literal>]
        let Universe = "Universe"

        [<Literal>]
        let IsProp = "IsProp"

        [<Literal>]
        let IsTrait = "IsTrait"

        [<Literal>]
        let Syntax = "Syntax"

        [<Literal>]
        let Code = "Code"

        [<Literal>]
        let Constraint = "Constraint"

        [<Literal>]
        let Quantity = "Quantity"

        [<Literal>]
        let Region = "Region"

        [<Literal>]
        let RecRow = "RecRow"

        [<Literal>]
        let VarRow = "VarRow"

        [<Literal>]
        let EffRow = "EffRow"

        [<Literal>]
        let Label = "Label"

        [<Literal>]
        let EffLabel = "EffLabel"

        [<Literal>]
        let Need = "Need"

        [<Literal>]
        let Thunk = "Thunk"

    module KnownTypePaths =
        let bare name = [ name ]
        let prelude name = KnownModules.Prelude @ [ name ]
        let hash name = KnownModules.Hash @ [ name ]
        let unicode name = KnownModules.Unicode @ [ name ]
        let bytes name = KnownModules.Bytes @ [ name ]

        let isBare expectedName segments =
            segments = bare expectedName

        let isPrelude expectedName segments =
            segments = prelude expectedName

        let isBareOrPrelude expectedName segments =
            isBare expectedName segments || isPrelude expectedName segments
