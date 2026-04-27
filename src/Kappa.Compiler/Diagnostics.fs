namespace Kappa.Compiler

type DiagnosticSeverity =
    | Info
    | Warning
    | Error

type DiagnosticCode =
    | SourceInfo
    | SourceWarning
    | LexicalError
    | ParseError
    | FrontendValidation
    | CheckpointVerification
    | TargetCheckpoint
    | ExpectAmbiguous
    | ExpectUnsatisfied
    | ImportCycle
    | ImportAmbiguous
    | ImportItemNotFound
    | UrlImportUnpinnedInPackageMode
    | UrlImportRefPinRequiresLock
    | HostModuleReservedRoot
    | HostModuleUnsupportedBackend
    | MultishotEffectUnsupportedBackend
    | ModuleNameUnresolved
    | ModulePathMismatch
    | StaticObjectUnresolved
    | ActivePatternInvalid
    | ProjectionDefinitionUnsupported
    | DuplicateDeclaration
    | DuplicatePatternBinder
    | TraitInstanceAmbiguous
    | TraitSupertraitUnsatisfied
    | HandlerEffectRowMismatch
    | HandlerClauseMissing
    | HandlerClauseDuplicate
    | HandlerClauseArityMismatch
    | HandlerClauseUnexpected
    | EffectResumptionQuantityBorrowed
    | NameUnresolved
    | RecursiveTypeAlias
    | MalformedConstructorDeclaration
    | RecursionRequiresSignature
    | SignatureUnsatisfied
    | OrPatternBinderMismatch
    | SafeNavigationAmbiguous
    | SafeNavigationReceiverNotOption
    | ElvisReceiverNotOption
    | RecordDuplicateField
    | RecordDependencyCycle
    | RecordDependencyInvalid
    | RecordProjectionMissingField
    | RecordPatchInvalidItem
    | RecordPatchDuplicatePath
    | RecordPatchPrefixConflict
    | RecordPatchUnknownPath
    | RowExtensionDuplicateLabel
    | RowExtensionExistingField
    | RowExtensionMissingLacksConstraint
    | ApplicationNonCallable
    | SealDirectLiteralForSignature
    | SealOpenRecordAscription
    | SealOpaqueUnfolding
    | TypeEqualityMismatch
    | NumericLiteralOutOfRange
    | UnicodeInvalidScalarLiteral
    | UnicodeInvalidGraphemeLiteral
    | UnicodeInvalidByteLiteral
    | UnicodeInvalidUtf8
    | UnicodeBidiControl
    | UnicodeConfusableIdentifier
    | UnicodeNonNormalizedSourceText
    | UnexpectedIndentation
    | UnterminatedBacktickIdentifier
    | UnterminatedStringLiteral
    | UnterminatedCharacterLiteral
    | UnterminatedBlockComment
    | QttLinearDrop
    | QttLinearOveruse
    | QttBorrowConsume
    | QttBorrowOverlap
    | QttBorrowEscape
    | QttContinuationCapture
    | QttErasedRuntimeUse
    | QttUsingExplicitQuantity
    | QttInoutMarkerRequired
    | QttInoutMarkerUnexpected
    | QttInoutThreadedFieldMissing
    | ControlFlowInvalidEscape

module DiagnosticCode =
    let toIdentifier code =
        match code with
        | SourceInfo -> "I_SOURCE"
        | SourceWarning -> "W_SOURCE"
        | LexicalError -> "E_LEXICAL"
        | ParseError -> "E_PARSE"
        | FrontendValidation -> "E_FRONTEND_VALIDATION"
        | CheckpointVerification -> "E_CHECKPOINT_VERIFICATION"
        | TargetCheckpoint -> "E_TARGET_CHECKPOINT"
        | ExpectAmbiguous -> "E_EXPECT_AMBIGUOUS"
        | ExpectUnsatisfied -> "E_EXPECT_UNSATISFIED"
        | ImportCycle -> "E_IMPORT_CYCLE"
        | ImportAmbiguous -> "E_IMPORT_AMBIGUOUS"
        | ImportItemNotFound -> "E_IMPORT_ITEM_NOT_FOUND"
        | UrlImportUnpinnedInPackageMode -> "E_URL_IMPORT_UNPINNED_IN_PACKAGE_MODE"
        | UrlImportRefPinRequiresLock -> "E_URL_IMPORT_REF_PIN_REQUIRES_LOCK"
        | HostModuleReservedRoot -> "E_HOST_MODULE_RESERVED_ROOT"
        | HostModuleUnsupportedBackend -> "E_HOST_MODULE_UNSUPPORTED_BACKEND"
        | MultishotEffectUnsupportedBackend -> "E_MULTISHOT_EFFECT_UNSUPPORTED_BACKEND"
        | ModuleNameUnresolved -> "E_MODULE_NAME_UNRESOLVED"
        | ModulePathMismatch -> "E_MODULE_PATH_MISMATCH"
        | StaticObjectUnresolved -> "E_STATIC_OBJECT_UNRESOLVED"
        | ActivePatternInvalid -> "E_ACTIVE_PATTERN_INVALID"
        | ProjectionDefinitionUnsupported -> "E_PROJECTION_DEFINITION_UNSUPPORTED"
        | DuplicateDeclaration -> "E_DUPLICATE_DECLARATION"
        | DuplicatePatternBinder -> "E_DUPLICATE_PATTERN_BINDER"
        | TraitInstanceAmbiguous -> "E_TRAIT_INSTANCE_AMBIGUOUS"
        | TraitSupertraitUnsatisfied -> "E_TRAIT_SUPERTRAIT_UNSATISFIED"
        | HandlerEffectRowMismatch -> "E_HANDLER_EFFECT_ROW_MISMATCH"
        | HandlerClauseMissing -> "E_HANDLER_CLAUSE_MISSING"
        | HandlerClauseDuplicate -> "E_HANDLER_CLAUSE_DUPLICATE"
        | HandlerClauseArityMismatch -> "E_HANDLER_CLAUSE_ARITY_MISMATCH"
        | HandlerClauseUnexpected -> "E_HANDLER_CLAUSE_UNEXPECTED"
        | EffectResumptionQuantityBorrowed -> "E_EFFECT_RESUMPTION_QUANTITY_BORROWED"
        | NameUnresolved -> "E_NAME_UNRESOLVED"
        | RecursiveTypeAlias -> "E_RECURSIVE_TYPE_ALIAS"
        | MalformedConstructorDeclaration -> "E_MALFORMED_CONSTRUCTOR_DECLARATION"
        | RecursionRequiresSignature -> "E_RECURSION_REQUIRES_SIGNATURE"
        | SignatureUnsatisfied -> "E_SIGNATURE_UNSATISFIED"
        | OrPatternBinderMismatch -> "E_OR_PATTERN_BINDER_MISMATCH"
        | SafeNavigationAmbiguous -> "E_SAFE_NAVIGATION_AMBIGUOUS"
        | SafeNavigationReceiverNotOption -> "E_SAFE_NAVIGATION_RECEIVER_NOT_OPTION"
        | ElvisReceiverNotOption -> "E_ELVIS_RECEIVER_NOT_OPTION"
        | RecordDuplicateField -> "E_RECORD_DUPLICATE_FIELD"
        | RecordDependencyCycle -> "E_RECORD_DEPENDENCY_CYCLE"
        | RecordDependencyInvalid -> "E_RECORD_DEPENDENCY_INVALID"
        | RecordProjectionMissingField -> "E_RECORD_PROJECTION_MISSING_FIELD"
        | RecordPatchInvalidItem -> "E_RECORD_PATCH_INVALID_ITEM"
        | RecordPatchDuplicatePath -> "E_RECORD_PATCH_DUPLICATE_PATH"
        | RecordPatchPrefixConflict -> "E_RECORD_PATCH_PREFIX_CONFLICT"
        | RecordPatchUnknownPath -> "E_RECORD_PATCH_UNKNOWN_PATH"
        | RowExtensionDuplicateLabel -> "E_ROW_EXTENSION_DUPLICATE_LABEL"
        | RowExtensionExistingField -> "E_ROW_EXTENSION_EXISTING_FIELD"
        | RowExtensionMissingLacksConstraint -> "E_ROW_EXTENSION_MISSING_LACKS_CONSTRAINT"
        | ApplicationNonCallable -> "E_APPLICATION_NONCALLABLE"
        | SealDirectLiteralForSignature -> "E_SEAL_DIRECT_LITERAL_FOR_SIGNATURE"
        | SealOpenRecordAscription -> "E_SEAL_OPEN_RECORD_ASCRIPTION"
        | SealOpaqueUnfolding -> "E_SEAL_OPAQUE_UNFOLDING"
        | TypeEqualityMismatch -> "E_TYPE_EQUALITY_MISMATCH"
        | NumericLiteralOutOfRange -> "E_NUMERIC_LITERAL_OUT_OF_RANGE"
        | UnicodeInvalidScalarLiteral -> "E_UNICODE_INVALID_SCALAR_LITERAL"
        | UnicodeInvalidGraphemeLiteral -> "E_UNICODE_INVALID_GRAPHEME_LITERAL"
        | UnicodeInvalidByteLiteral -> "E_UNICODE_INVALID_BYTE_LITERAL"
        | UnicodeInvalidUtf8 -> "E_UNICODE_INVALID_UTF8"
        | UnicodeBidiControl -> "W_UNICODE_BIDI_CONTROL"
        | UnicodeConfusableIdentifier -> "W_UNICODE_CONFUSABLE_IDENTIFIER"
        | UnicodeNonNormalizedSourceText -> "W_UNICODE_NON_NORMALIZED_SOURCE_TEXT"
        | UnexpectedIndentation -> "E_UNEXPECTED_INDENTATION"
        | UnterminatedBacktickIdentifier -> "E_UNTERMINATED_BACKTICK_IDENTIFIER"
        | UnterminatedStringLiteral -> "E_UNTERMINATED_STRING_LITERAL"
        | UnterminatedCharacterLiteral -> "E_UNTERMINATED_CHARACTER_LITERAL"
        | UnterminatedBlockComment -> "E_UNTERMINATED_BLOCK_COMMENT"
        | QttLinearDrop -> "E_QTT_LINEAR_DROP"
        | QttLinearOveruse -> "E_QTT_LINEAR_OVERUSE"
        | QttBorrowConsume -> "E_QTT_BORROW_CONSUME"
        | QttBorrowOverlap -> "E_QTT_BORROW_OVERLAP"
        | QttBorrowEscape -> "E_QTT_BORROW_ESCAPE"
        | QttContinuationCapture -> "E_QTT_CONTINUATION_CAPTURE"
        | QttErasedRuntimeUse -> "E_QTT_ERASED_RUNTIME_USE"
        | QttUsingExplicitQuantity -> "E_QTT_USING_EXPLICIT_QUANTITY"
        | QttInoutMarkerRequired -> "E_QTT_INOUT_MARKER_REQUIRED"
        | QttInoutMarkerUnexpected -> "E_QTT_INOUT_MARKER_UNEXPECTED"
        | QttInoutThreadedFieldMissing -> "E_QTT_INOUT_THREADED_FIELD_MISSING"
        | ControlFlowInvalidEscape -> "E_CONTROL_FLOW_INVALID_ESCAPE"

    let tryParseIdentifier (identifier: string) =
        match identifier.Trim() with
        | "I_SOURCE" -> Some SourceInfo
        | "W_SOURCE" -> Some SourceWarning
        | "E_LEXICAL" -> Some LexicalError
        | "E_PARSE" -> Some ParseError
        | "E_FRONTEND_VALIDATION" -> Some FrontendValidation
        | "E_CHECKPOINT_VERIFICATION" -> Some CheckpointVerification
        | "E_TARGET_CHECKPOINT" -> Some TargetCheckpoint
        | "E_EXPECT_AMBIGUOUS" -> Some ExpectAmbiguous
        | "E_EXPECT_UNSATISFIED" -> Some ExpectUnsatisfied
        | "E_IMPORT_CYCLE" -> Some ImportCycle
        | "E_IMPORT_AMBIGUOUS" -> Some ImportAmbiguous
        | "E_IMPORT_ITEM_NOT_FOUND" -> Some ImportItemNotFound
        | "E_URL_IMPORT_UNPINNED_IN_PACKAGE_MODE" -> Some UrlImportUnpinnedInPackageMode
        | "E_URL_IMPORT_REF_PIN_REQUIRES_LOCK" -> Some UrlImportRefPinRequiresLock
        | "E_HOST_MODULE_RESERVED_ROOT" -> Some HostModuleReservedRoot
        | "E_HOST_MODULE_UNSUPPORTED_BACKEND" -> Some HostModuleUnsupportedBackend
        | "E_MULTISHOT_EFFECT_UNSUPPORTED_BACKEND" -> Some MultishotEffectUnsupportedBackend
        | "E_MODULE_NAME_UNRESOLVED" -> Some ModuleNameUnresolved
        | "E_MODULE_PATH_MISMATCH" -> Some ModulePathMismatch
        | "E_STATIC_OBJECT_UNRESOLVED" -> Some StaticObjectUnresolved
        | "E_ACTIVE_PATTERN_INVALID" -> Some ActivePatternInvalid
        | "E_PROJECTION_DEFINITION_UNSUPPORTED" -> Some ProjectionDefinitionUnsupported
        | "E_DUPLICATE_DECLARATION" -> Some DuplicateDeclaration
        | "E_DUPLICATE_PATTERN_BINDER" -> Some DuplicatePatternBinder
        | "E_TRAIT_INSTANCE_AMBIGUOUS" -> Some TraitInstanceAmbiguous
        | "E_TRAIT_SUPERTRAIT_UNSATISFIED" -> Some TraitSupertraitUnsatisfied
        | "E_HANDLER_EFFECT_ROW_MISMATCH" -> Some HandlerEffectRowMismatch
        | "E_HANDLER_CLAUSE_MISSING" -> Some HandlerClauseMissing
        | "E_HANDLER_CLAUSE_DUPLICATE" -> Some HandlerClauseDuplicate
        | "E_HANDLER_CLAUSE_ARITY_MISMATCH" -> Some HandlerClauseArityMismatch
        | "E_HANDLER_CLAUSE_UNEXPECTED" -> Some HandlerClauseUnexpected
        | "E_EFFECT_RESUMPTION_QUANTITY_BORROWED" -> Some EffectResumptionQuantityBorrowed
        | "E_NAME_UNRESOLVED" -> Some NameUnresolved
        | "E_RECURSIVE_TYPE_ALIAS" -> Some RecursiveTypeAlias
        | "E_MALFORMED_CONSTRUCTOR_DECLARATION" -> Some MalformedConstructorDeclaration
        | "E_RECURSION_REQUIRES_SIGNATURE" -> Some RecursionRequiresSignature
        | "E_SIGNATURE_UNSATISFIED" -> Some SignatureUnsatisfied
        | "E_OR_PATTERN_BINDER_MISMATCH" -> Some OrPatternBinderMismatch
        | "E_SAFE_NAVIGATION_AMBIGUOUS" -> Some SafeNavigationAmbiguous
        | "E_SAFE_NAVIGATION_RECEIVER_NOT_OPTION" -> Some SafeNavigationReceiverNotOption
        | "E_ELVIS_RECEIVER_NOT_OPTION" -> Some ElvisReceiverNotOption
        | "E_RECORD_DUPLICATE_FIELD" -> Some RecordDuplicateField
        | "E_RECORD_DEPENDENCY_CYCLE" -> Some RecordDependencyCycle
        | "E_RECORD_DEPENDENCY_INVALID" -> Some RecordDependencyInvalid
        | "E_RECORD_PROJECTION_MISSING_FIELD" -> Some RecordProjectionMissingField
        | "E_RECORD_PATCH_INVALID_ITEM" -> Some RecordPatchInvalidItem
        | "E_RECORD_PATCH_DUPLICATE_PATH" -> Some RecordPatchDuplicatePath
        | "E_RECORD_PATCH_PREFIX_CONFLICT" -> Some RecordPatchPrefixConflict
        | "E_RECORD_PATCH_UNKNOWN_PATH" -> Some RecordPatchUnknownPath
        | "E_ROW_EXTENSION_DUPLICATE_LABEL" -> Some RowExtensionDuplicateLabel
        | "E_ROW_EXTENSION_EXISTING_FIELD" -> Some RowExtensionExistingField
        | "E_ROW_EXTENSION_MISSING_LACKS_CONSTRAINT" -> Some RowExtensionMissingLacksConstraint
        | "E_APPLICATION_NONCALLABLE" -> Some ApplicationNonCallable
        | "E_SEAL_DIRECT_LITERAL_FOR_SIGNATURE" -> Some SealDirectLiteralForSignature
        | "E_SEAL_OPEN_RECORD_ASCRIPTION" -> Some SealOpenRecordAscription
        | "E_SEAL_OPAQUE_UNFOLDING" -> Some SealOpaqueUnfolding
        | "E_TYPE_EQUALITY_MISMATCH" -> Some TypeEqualityMismatch
        | "E_TYPE_MISMATCH" -> Some TypeEqualityMismatch
        | "E_NUMERIC_LITERAL_OUT_OF_RANGE" -> Some NumericLiteralOutOfRange
        | "E_UNSOLVED_IMPLICIT" -> Some TypeEqualityMismatch
        | "E_UNICODE_INVALID_SCALAR_LITERAL" -> Some UnicodeInvalidScalarLiteral
        | "E_UNICODE_INVALID_GRAPHEME_LITERAL" -> Some UnicodeInvalidGraphemeLiteral
        | "E_UNICODE_INVALID_BYTE_LITERAL" -> Some UnicodeInvalidByteLiteral
        | "E_UNICODE_INVALID_UTF8" -> Some UnicodeInvalidUtf8
        | "W_UNICODE_BIDI_CONTROL" -> Some UnicodeBidiControl
        | "W_UNICODE_CONFUSABLE_IDENTIFIER" -> Some UnicodeConfusableIdentifier
        | "W_UNICODE_NON_NORMALIZED_SOURCE_TEXT" -> Some UnicodeNonNormalizedSourceText
        | "E_UNEXPECTED_INDENTATION" -> Some UnexpectedIndentation
        | "E_UNTERMINATED_BACKTICK_IDENTIFIER" -> Some UnterminatedBacktickIdentifier
        | "E_UNTERMINATED_STRING_LITERAL" -> Some UnterminatedStringLiteral
        | "E_UNTERMINATED_CHARACTER_LITERAL" -> Some UnterminatedCharacterLiteral
        | "E_UNTERMINATED_BLOCK_COMMENT" -> Some UnterminatedBlockComment
        | "E_QTT_LINEAR_DROP" -> Some QttLinearDrop
        | "E_QTT_LINEAR_OVERUSE" -> Some QttLinearOveruse
        | "E_QTT_BORROW_CONSUME" -> Some QttBorrowConsume
        | "E_QTT_BORROW_OVERLAP" -> Some QttBorrowOverlap
        | "E_QTT_BORROW_ESCAPE" -> Some QttBorrowEscape
        | "E_QTT_CONTINUATION_CAPTURE" -> Some QttContinuationCapture
        | "E_QTT_ERASED_RUNTIME_USE" -> Some QttErasedRuntimeUse
        | "E_QTT_USING_EXPLICIT_QUANTITY" -> Some QttUsingExplicitQuantity
        | "E_QTT_INOUT_MARKER_REQUIRED" -> Some QttInoutMarkerRequired
        | "E_QTT_INOUT_MARKER_UNEXPECTED" -> Some QttInoutMarkerUnexpected
        | "E_QTT_INOUT_THREADED_FIELD_MISSING" -> Some QttInoutThreadedFieldMissing
        | "E_CONTROL_FLOW_INVALID_ESCAPE" -> Some ControlFlowInvalidEscape
        | _ -> None

    let tryGetExplanation code =
        match code with
        | LexicalError ->
            Some "The source text contains a token or character sequence that is not valid Kappa syntax."
        | ParseError ->
            Some "The token stream is well-formed lexically, but it does not match the grammar expected at that source position."
        | NameUnresolved ->
            Some "A referenced name is not in lexical, module, or imported scope."
        | TypeEqualityMismatch ->
            Some "Two types that must agree after normalization do not definitionally equal one another."
        | NumericLiteralOutOfRange ->
            Some "A numeric literal cannot be represented at the target numeric type required by the surrounding typing context."
        | QttContinuationCapture ->
            Some "A multi-shot continuation cannot capture live linear or borrowed resources across the operation site."
        | MultishotEffectUnsupportedBackend ->
            Some "The selected backend profile does not advertise rt-multishot-effects, so reachable multi-shot effect invocations and exported declarations that may invoke them must be rejected."
        | HandlerEffectRowMismatch ->
            Some "A handler can eliminate only an Eff computation whose handled label appears in the handled effect row."
        | HandlerClauseMissing ->
            Some "A handler must define exactly one return clause and exactly one operation clause for every operation declared by the handled effect interface."
        | HandlerClauseDuplicate ->
            Some "A handler must not define more than one return clause or more than one clause for the same effect operation."
        | HandlerClauseArityMismatch ->
            Some "A handler return clause must bind exactly one payload, and each handler operation clause must bind exactly the parameters declared by the handled effect operation."
        | HandlerClauseUnexpected ->
            Some "A handler clause names an operation that is not declared by the handled effect interface."
        | EffectResumptionQuantityBorrowed ->
            Some "Resumption quantities describe the handler-bound resumption value itself. Borrowed resumption values and borrowed resumption payloads are not part of the language."
        | UnicodeInvalidScalarLiteral ->
            Some "A Unicode scalar literal must decode to exactly one valid Unicode scalar value and must not contain surrogate or out-of-range code points."
        | UnicodeInvalidGraphemeLiteral ->
            Some "A grapheme literal must decode to exactly one extended grapheme cluster with valid Unicode scalar content."
        | UnicodeInvalidByteLiteral ->
            Some "A byte literal must decode to exactly one byte value in the inclusive range 0x00 through 0xFF."
        | UnicodeInvalidUtf8 ->
            Some "Kappa source files are UTF-8 text. Ill-formed UTF-8 input is rejected before lexing."
        | UnicodeBidiControl ->
            Some "Bidirectional control characters can obscure visual source order and should be reviewed or removed."
        | UnicodeConfusableIdentifier ->
            Some "An identifier uses Unicode characters that are visually confusable with characters from other scripts."
        | UnicodeNonNormalizedSourceText ->
            Some "The source contains Unicode text that is not in the implementation's preferred normalization form."
        | UnterminatedStringLiteral ->
            Some "A string literal reached the end of its line or file before a matching closing delimiter."
        | UnterminatedCharacterLiteral ->
            Some "A character-like literal reached the end of its line or file before a matching closing delimiter."
        | _ ->
            None

type DiagnosticRelatedLocation =
    { Message: string
      Location: SourceLocation }

type Diagnostic =
    { Severity: DiagnosticSeverity
      Code: DiagnosticCode
      Stage: string option
      Phase: string option
      Message: string
      Location: SourceLocation option
      RelatedLocations: DiagnosticRelatedLocation list }

type DiagnosticBag() =
    let items = ResizeArray<Diagnostic>()

    member _.Add(
        severity: DiagnosticSeverity,
        code: DiagnosticCode,
        message: string,
        ?location: SourceLocation,
        ?relatedLocations: DiagnosticRelatedLocation seq,
        ?stage: string,
        ?phase: string
    ) =
        items.Add(
            { Severity = severity
              Code = code
              Stage = stage
              Phase = phase
              Message = message
              Location = location
              RelatedLocations = relatedLocations |> Option.map Seq.toList |> Option.defaultValue [] }
        )

    member this.AddInfo(code: DiagnosticCode, message: string, ?location: SourceLocation, ?stage: string, ?phase: string) =
        this.Add(Info, code, message, ?location = location, ?stage = stage, ?phase = phase)

    member this.AddWarning(code: DiagnosticCode, message: string, ?location: SourceLocation, ?stage: string, ?phase: string) =
        this.Add(Warning, code, message, ?location = location, ?stage = stage, ?phase = phase)

    member this.AddError(code: DiagnosticCode, message: string, ?location: SourceLocation, ?stage: string, ?phase: string) =
        this.Add(Error, code, message, ?location = location, ?stage = stage, ?phase = phase)

    member _.AddRange(diagnostics: Diagnostic seq) =
        diagnostics |> Seq.iter items.Add

    member _.Items = List.ofSeq items
    member _.HasErrors = items |> Seq.exists (fun diagnostic -> diagnostic.Severity = Error)
