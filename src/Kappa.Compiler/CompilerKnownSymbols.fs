namespace Kappa.Compiler

// Canonical compiler-known symbolic names and paths. Semantic phases should import these rather than hardcoding raw strings.
module CompilerKnownSymbols =
    type KnownType =
        | UniverseType
        | ConstraintType
        | QuantityType
        | RegionType
        | RecRowType
        | VarRowType
        | EffRowType
        | LabelType
        | EffLabelType
        | UnitType
        | BoolType
        | ByteType
        | BytesType
        | BorrowViewType
        | CharType
        | CodeType
        | ClosedCodeType
        | ComprehensionPlanType
        | DictType
        | DoubleType
        | ElabType
        | ElabGoalType
        | EffType
        | FloatType
        | GraphemeType
        | IntType
        | IntegerType
        | InterpolatedMacroType
        | IOType
        | IsPropType
        | IsTraitType
        | ListType
        | MatchType
        | NatType
        | NeedType
        | NonEmptyQueryType
        | OnceQueryType
        | OptionType
        | OptionalQueryType
        | ProjectorType
        | QueryType
        | QueryCoreType
        | QueryModeType
        | RealType
        | RefType
        | ResType
        | SetType
        | SingletonQueryType
        | StringType
        | SyntaxType
        | SyntaxFragmentType
        | SyntaxOriginType
        | ThunkType
        | UIOType
        | UnicodeScalarType
        | VoidType
        | ZipperType

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

    module KnownTypes =
        let private bareOnly knownName =
            [ KnownTypePaths.bare knownName ], KnownTypePaths.bare knownName

        let private bareOrPrelude knownName =
            [ KnownTypePaths.bare knownName
              KnownTypePaths.prelude knownName ],
            KnownTypePaths.bare knownName

        let acceptedPaths knownType =
            match knownType with
            | UniverseType -> fst (bareOnly KnownTypeNames.Universe)
            | ConstraintType -> fst (bareOnly KnownTypeNames.Constraint)
            | QuantityType -> fst (bareOnly KnownTypeNames.Quantity)
            | RegionType -> fst (bareOnly KnownTypeNames.Region)
            | RecRowType -> fst (bareOnly KnownTypeNames.RecRow)
            | VarRowType -> fst (bareOnly KnownTypeNames.VarRow)
            | EffRowType -> fst (bareOnly KnownTypeNames.EffRow)
            | LabelType -> fst (bareOnly KnownTypeNames.Label)
            | EffLabelType -> fst (bareOnly KnownTypeNames.EffLabel)
            | UnitType -> fst (bareOrPrelude KnownTypeNames.Unit)
            | BoolType -> fst (bareOrPrelude KnownTypeNames.Bool)
            | ByteType -> fst (bareOrPrelude KnownTypeNames.Byte)
            | BytesType -> fst (bareOrPrelude KnownTypeNames.Bytes)
            | BorrowViewType -> fst (bareOrPrelude KnownTypeNames.BorrowView)
            | CharType -> fst (bareOrPrelude KnownTypeNames.Char)
            | CodeType -> fst (bareOrPrelude KnownTypeNames.Code)
            | ClosedCodeType -> fst (bareOrPrelude KnownTypeNames.ClosedCode)
            | ComprehensionPlanType -> fst (bareOrPrelude KnownTypeNames.ComprehensionPlan)
            | DictType -> fst (bareOrPrelude KnownTypeNames.Dict)
            | DoubleType -> fst (bareOrPrelude KnownTypeNames.Double)
            | ElabType -> fst (bareOrPrelude KnownTypeNames.Elab)
            | ElabGoalType -> fst (bareOrPrelude KnownTypeNames.ElabGoal)
            | EffType -> fst (bareOnly KnownTypeNames.Eff)
            | FloatType -> fst (bareOrPrelude KnownTypeNames.Float)
            | GraphemeType -> fst (bareOrPrelude KnownTypeNames.Grapheme)
            | IntType -> fst (bareOrPrelude KnownTypeNames.Int)
            | IntegerType -> fst (bareOrPrelude KnownTypeNames.Integer)
            | InterpolatedMacroType -> fst (bareOrPrelude KnownTypeNames.InterpolatedMacro)
            | IOType -> fst (bareOrPrelude KnownTypeNames.IO)
            | IsPropType -> fst (bareOrPrelude KnownTypeNames.IsProp)
            | IsTraitType -> fst (bareOrPrelude KnownTypeNames.IsTrait)
            | ListType -> fst (bareOrPrelude KnownTypeNames.List)
            | MatchType -> fst (bareOnly KnownTypeNames.Match)
            | NatType -> fst (bareOrPrelude KnownTypeNames.Nat)
            | NeedType -> fst (bareOrPrelude KnownTypeNames.Need)
            | NonEmptyQueryType -> fst (bareOnly KnownTypeNames.NonEmptyQuery)
            | OnceQueryType -> fst (bareOnly KnownTypeNames.OnceQuery)
            | OptionType -> fst (bareOrPrelude KnownTypeNames.Option)
            | OptionalQueryType -> fst (bareOnly KnownTypeNames.OptionalQuery)
            | ProjectorType -> fst (bareOrPrelude KnownTypeNames.Projector)
            | QueryType -> fst (bareOnly KnownTypeNames.Query)
            | QueryCoreType -> fst (bareOnly KnownTypeNames.QueryCore)
            | QueryModeType -> fst (bareOnly KnownTypeNames.QueryMode)
            | RealType -> fst (bareOrPrelude KnownTypeNames.Real)
            | RefType -> fst (bareOrPrelude KnownTypeNames.Ref)
            | ResType -> fst (bareOnly KnownTypeNames.Res)
            | SetType -> fst (bareOnly KnownTypeNames.Set)
            | SingletonQueryType -> fst (bareOnly KnownTypeNames.SingletonQuery)
            | StringType -> fst (bareOrPrelude KnownTypeNames.String)
            | SyntaxType -> fst (bareOrPrelude KnownTypeNames.Syntax)
            | SyntaxFragmentType -> fst (bareOrPrelude KnownTypeNames.SyntaxFragment)
            | SyntaxOriginType -> fst (bareOrPrelude KnownTypeNames.SyntaxOrigin)
            | ThunkType -> fst (bareOrPrelude KnownTypeNames.Thunk)
            | UIOType -> fst (bareOrPrelude KnownTypeNames.UIO)
            | UnicodeScalarType -> fst (bareOrPrelude KnownTypeNames.UnicodeScalar)
            | VoidType -> fst (bareOnly KnownTypeNames.Void)
            | ZipperType -> fst (bareOnly KnownTypeNames.Zipper)

        let canonicalPath knownType =
            match knownType with
            | UniverseType -> snd (bareOnly KnownTypeNames.Universe)
            | ConstraintType -> snd (bareOnly KnownTypeNames.Constraint)
            | QuantityType -> snd (bareOnly KnownTypeNames.Quantity)
            | RegionType -> snd (bareOnly KnownTypeNames.Region)
            | RecRowType -> snd (bareOnly KnownTypeNames.RecRow)
            | VarRowType -> snd (bareOnly KnownTypeNames.VarRow)
            | EffRowType -> snd (bareOnly KnownTypeNames.EffRow)
            | LabelType -> snd (bareOnly KnownTypeNames.Label)
            | EffLabelType -> snd (bareOnly KnownTypeNames.EffLabel)
            | UnitType -> snd (bareOrPrelude KnownTypeNames.Unit)
            | BoolType -> snd (bareOrPrelude KnownTypeNames.Bool)
            | ByteType -> snd (bareOrPrelude KnownTypeNames.Byte)
            | BytesType -> snd (bareOrPrelude KnownTypeNames.Bytes)
            | BorrowViewType -> snd (bareOrPrelude KnownTypeNames.BorrowView)
            | CharType -> snd (bareOrPrelude KnownTypeNames.Char)
            | CodeType -> snd (bareOrPrelude KnownTypeNames.Code)
            | ClosedCodeType -> snd (bareOrPrelude KnownTypeNames.ClosedCode)
            | ComprehensionPlanType -> snd (bareOrPrelude KnownTypeNames.ComprehensionPlan)
            | DictType -> snd (bareOrPrelude KnownTypeNames.Dict)
            | DoubleType -> snd (bareOrPrelude KnownTypeNames.Double)
            | ElabType -> snd (bareOrPrelude KnownTypeNames.Elab)
            | ElabGoalType -> snd (bareOrPrelude KnownTypeNames.ElabGoal)
            | EffType -> snd (bareOnly KnownTypeNames.Eff)
            | FloatType -> snd (bareOrPrelude KnownTypeNames.Float)
            | GraphemeType -> snd (bareOrPrelude KnownTypeNames.Grapheme)
            | IntType -> snd (bareOrPrelude KnownTypeNames.Int)
            | IntegerType -> snd (bareOrPrelude KnownTypeNames.Integer)
            | InterpolatedMacroType -> snd (bareOrPrelude KnownTypeNames.InterpolatedMacro)
            | IOType -> snd (bareOrPrelude KnownTypeNames.IO)
            | IsPropType -> snd (bareOrPrelude KnownTypeNames.IsProp)
            | IsTraitType -> snd (bareOrPrelude KnownTypeNames.IsTrait)
            | ListType -> snd (bareOrPrelude KnownTypeNames.List)
            | MatchType -> snd (bareOnly KnownTypeNames.Match)
            | NatType -> snd (bareOrPrelude KnownTypeNames.Nat)
            | NeedType -> snd (bareOrPrelude KnownTypeNames.Need)
            | NonEmptyQueryType -> snd (bareOnly KnownTypeNames.NonEmptyQuery)
            | OnceQueryType -> snd (bareOnly KnownTypeNames.OnceQuery)
            | OptionType -> snd (bareOrPrelude KnownTypeNames.Option)
            | OptionalQueryType -> snd (bareOnly KnownTypeNames.OptionalQuery)
            | ProjectorType -> snd (bareOrPrelude KnownTypeNames.Projector)
            | QueryType -> snd (bareOnly KnownTypeNames.Query)
            | QueryCoreType -> snd (bareOnly KnownTypeNames.QueryCore)
            | QueryModeType -> snd (bareOnly KnownTypeNames.QueryMode)
            | RealType -> snd (bareOrPrelude KnownTypeNames.Real)
            | RefType -> snd (bareOrPrelude KnownTypeNames.Ref)
            | ResType -> snd (bareOnly KnownTypeNames.Res)
            | SetType -> snd (bareOnly KnownTypeNames.Set)
            | SingletonQueryType -> snd (bareOnly KnownTypeNames.SingletonQuery)
            | StringType -> snd (bareOrPrelude KnownTypeNames.String)
            | SyntaxType -> snd (bareOrPrelude KnownTypeNames.Syntax)
            | SyntaxFragmentType -> snd (bareOrPrelude KnownTypeNames.SyntaxFragment)
            | SyntaxOriginType -> snd (bareOrPrelude KnownTypeNames.SyntaxOrigin)
            | ThunkType -> snd (bareOrPrelude KnownTypeNames.Thunk)
            | UIOType -> snd (bareOrPrelude KnownTypeNames.UIO)
            | UnicodeScalarType -> snd (bareOrPrelude KnownTypeNames.UnicodeScalar)
            | VoidType -> snd (bareOnly KnownTypeNames.Void)
            | ZipperType -> snd (bareOnly KnownTypeNames.Zipper)

        let matchesName knownType nameSegments =
            acceptedPaths knownType |> List.exists ((=) nameSegments)

        let tryClassifyName nameSegments =
            [ UniverseType
              ConstraintType
              QuantityType
              RegionType
              RecRowType
              VarRowType
              EffRowType
              LabelType
              EffLabelType
              UnitType
              BoolType
              ByteType
              BytesType
              BorrowViewType
              CharType
              CodeType
              ClosedCodeType
              ComprehensionPlanType
              DictType
              DoubleType
              ElabType
              ElabGoalType
              EffType
              FloatType
              GraphemeType
              IntType
              IntegerType
              InterpolatedMacroType
              IOType
              IsPropType
              IsTraitType
              ListType
              MatchType
              NatType
              NeedType
              NonEmptyQueryType
              OnceQueryType
              OptionType
              OptionalQueryType
              ProjectorType
              QueryType
              QueryCoreType
              QueryModeType
              RealType
              RefType
              ResType
              SetType
              SingletonQueryType
              StringType
              SyntaxType
              SyntaxFragmentType
              SyntaxOriginType
              ThunkType
              UIOType
              UnicodeScalarType
              VoidType
              ZipperType ]
            |> List.tryFind (fun knownType -> matchesName knownType nameSegments)
