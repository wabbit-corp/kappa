namespace Kappa.Compiler

// Canonical compiler-known symbolic names and paths. Semantic phases should import these rather than hardcoding raw strings.
module CompilerKnownSymbols =
    module KnownModules =
        let Prelude = [ "std"; "prelude" ]
        let Hash = [ "std"; "hash" ]
        let Unicode = [ "std"; "unicode" ]
        let Bytes = [ "std"; "bytes" ]
        let Gradual = [ "std"; "gradual" ]
        let Ffi = [ "std"; "ffi" ]
        let FfiC = [ "std"; "ffi"; "c" ]
        let Bridge = [ "std"; "bridge" ]
        let Atomic = [ "std"; "atomic" ]
        let Supervisor = [ "std"; "supervisor" ]
        let Testing = [ "std"; "testing" ]

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
        let BorrowView = "BorrowView"

        [<Literal>]
        let Char = "Char"

        [<Literal>]
        let ClosedCode = "ClosedCode"

        [<Literal>]
        let ComprehensionPlan = "ComprehensionPlan"

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
        let Elab = "Elab"

        [<Literal>]
        let ElabGoal = "ElabGoal"

        [<Literal>]
        let Eff = "Eff"

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
        let Projector = "Projector"

        [<Literal>]
        let Query = "Query"

        [<Literal>]
        let QueryCard = "QueryCard"

        [<Literal>]
        let QueryCore = "QueryCore"

        [<Literal>]
        let QueryMode = "QueryMode"

        [<Literal>]
        let QueryUse = "QueryUse"

        [<Literal>]
        let RawComprehension = "RawComprehension"

        [<Literal>]
        let Res = "Res"

        [<Literal>]
        let Set = "Set"

        [<Literal>]
        let SingletonQuery = "SingletonQuery"

        [<Literal>]
        let SyntaxFragment = "SyntaxFragment"

        [<Literal>]
        let SyntaxOrigin = "SyntaxOrigin"

        [<Literal>]
        let Thunk = "Thunk"

        [<Literal>]
        let Void = "Void"

        [<Literal>]
        let List = "List"

        [<Literal>]
        let Array = "Array"

        [<Literal>]
        let Map = "Map"

        [<Literal>]
        let Match = "Match"

        [<Literal>]
        let NonEmptyQuery = "NonEmptyQuery"

        [<Literal>]
        let OnceQuery = "OnceQuery"

        [<Literal>]
        let OptionalQuery = "OptionalQuery"

        [<Literal>]
        let QueryModeReusable = "Reusable"

        [<Literal>]
        let QueryModeOneShot = "OneShot"

        [<Literal>]
        let QueryCardZero = "QZero"

        [<Literal>]
        let QueryCardOne = "QOne"

        [<Literal>]
        let QueryCardZeroOrOne = "QZeroOrOne"

        [<Literal>]
        let QueryCardOneOrMore = "QOneOrMore"

        [<Literal>]
        let QueryCardZeroOrMore = "QZeroOrMore"

        [<Literal>]
        let ShapeAdtKind = "ShapeAdtKind"

        [<Literal>]
        let ShapeVisibility = "ShapeVisibility"

        [<Literal>]
        let ShapeErrorKind = "ShapeErrorKind"

        [<Literal>]
        let ShapeError = "ShapeError"

        [<Literal>]
        let ShapeParameter = "ShapeParameter"

        [<Literal>]
        let ShapeField = "ShapeField"

        [<Literal>]
        let ShapeConstructor = "ShapeConstructor"

        [<Literal>]
        let AdtShape = "AdtShape"

        [<Literal>]
        let RecordShape = "RecordShape"

        [<Literal>]
        let BoundField = "BoundField"

        [<Literal>]
        let BoundFieldPair = "BoundFieldPair"

        [<Literal>]
        let FieldArgument = "FieldArgument"

        [<Literal>]
        let FieldConstraint = "FieldConstraint"

        [<Literal>]
        let InterpolatedMacro = "InterpolatedMacro"

        [<Literal>]
        let Zipper = "Zipper"

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

        let hasLastSegment expectedName segments =
            match List.tryLast segments with
            | Some actualName -> System.String.Equals(actualName, expectedName, System.StringComparison.Ordinal)
            | None -> false
