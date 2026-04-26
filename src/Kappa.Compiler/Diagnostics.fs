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
    | ModuleNameUnresolved
    | ModulePathMismatch
    | StaticObjectUnresolved
    | ActivePatternInvalid
    | ProjectionDefinitionUnsupported
    | DuplicateDeclaration
    | DuplicatePatternBinder
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
        | ModuleNameUnresolved -> "E_MODULE_NAME_UNRESOLVED"
        | ModulePathMismatch -> "E_MODULE_PATH_MISMATCH"
        | StaticObjectUnresolved -> "E_STATIC_OBJECT_UNRESOLVED"
        | ActivePatternInvalid -> "E_ACTIVE_PATTERN_INVALID"
        | ProjectionDefinitionUnsupported -> "E_PROJECTION_DEFINITION_UNSUPPORTED"
        | DuplicateDeclaration -> "E_DUPLICATE_DECLARATION"
        | DuplicatePatternBinder -> "E_DUPLICATE_PATTERN_BINDER"
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
        | "E_MODULE_NAME_UNRESOLVED" -> Some ModuleNameUnresolved
        | "E_MODULE_PATH_MISMATCH" -> Some ModulePathMismatch
        | "E_STATIC_OBJECT_UNRESOLVED" -> Some StaticObjectUnresolved
        | "E_ACTIVE_PATTERN_INVALID" -> Some ActivePatternInvalid
        | "E_PROJECTION_DEFINITION_UNSUPPORTED" -> Some ProjectionDefinitionUnsupported
        | "E_DUPLICATE_DECLARATION" -> Some DuplicateDeclaration
        | "E_DUPLICATE_PATTERN_BINDER" -> Some DuplicatePatternBinder
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
        | "E_NUMERIC_LITERAL_OUT_OF_RANGE" -> Some NumericLiteralOutOfRange
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
        | "E_QTT_ERASED_RUNTIME_USE" -> Some QttErasedRuntimeUse
        | "E_QTT_USING_EXPLICIT_QUANTITY" -> Some QttUsingExplicitQuantity
        | "E_QTT_INOUT_MARKER_REQUIRED" -> Some QttInoutMarkerRequired
        | "E_QTT_INOUT_MARKER_UNEXPECTED" -> Some QttInoutMarkerUnexpected
        | "E_QTT_INOUT_THREADED_FIELD_MISSING" -> Some QttInoutThreadedFieldMissing
        | "E_CONTROL_FLOW_INVALID_ESCAPE" -> Some ControlFlowInvalidEscape
        | _ -> None

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
