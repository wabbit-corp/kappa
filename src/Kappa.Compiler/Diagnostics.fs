namespace Kappa.Compiler

type DiagnosticSeverity =
    | Info
    | Warning
    | Error

type DiagnosticCode =
    | SourceInfo
    | SourceWarning
    | TabCharacterNotPermitted
    | UnrecognizedCharacter
    | MalformedNumericLiteral
    | UnterminatedStringInterpolation
    | ExpectedSyntaxToken
    | ExpectedClosingDelimiter
    | ExpectedIndentedBlock
    | InvalidTypeSyntax
    | UnsupportedSyntax
    | UnexpectedTrailingSyntax
    | ModifierNotApplicable
    | ElaborationFailed
    | ImportUnhideRequiresBuildSetting
    | ImportClarifyRequiresBuildSetting
    | ImportItemModifierReexportForbidden
    | AssertTerminatesRequiresModuleAttribute
    | AssertReducibleRequiresModuleAttribute
    | CheckpointVerification
    | TargetCheckpoint
    | ExpectAmbiguous
    | ExpectUnsatisfied
    | ImportCycle
    | ImportAmbiguous
    | ImportItemNotFound
    | UrlImportUnpinnedInPackageMode
    | UrlImportRefPinRequiresLock
    | UrlImportUnsupported
    | HostModuleReservedRoot
    | HostModuleUnsupportedBackend
    | EffectRuntimeUnsupportedBackend
    | MultishotEffectUnsupportedBackend
    | ModuleAttributeUnknown
    | ModuleHeaderMisplaced
    | ModuleHeaderExpectedAfterAttributes
    | ModuleNameUnresolved
    | ModulePathMismatch
    | ModuleCaseFoldCollision
    | StaticObjectUnresolved
    | PatternHeadNotConstructorOrActivePattern
    | ActivePatternLinearityViolation
    | ActivePatternMatchResultNotAllowedInPlainLetQuestion
    | ActivePatternMissingScrutineeBinder
    | ActivePatternMonadicResult
    | ProjectionCapabilityRequired
    | ProjectionUpdateTargetUnsupported
    | ProjectionRootInvalid
    | ProjectionDescriptorRootMissing
    | ProjectionRootsPackMismatch
    | ProjectionDescriptorRootsLiteralRequired
    | ProjectionDescriptorValueExpected
    | ProjectionMissingPlaceBinder
    | ProjectionYieldInvalid
    | ProjectionExpandedAccessorPlaceBinderMismatch
    | ProjectionAccessorClauseDuplicate
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
    | DerivingShapeNotData
    | DerivingShapeNotClosedRecord
    | DerivingShapeOpaqueRepresentation
    | DerivingShapeUnsupportedType
    | DerivingShapeBadConstructorArguments
    | DerivingShapeBadRecordArguments
    | DerivingShapeMissingRuntimeFieldInstance
    | DerivingShapeDeclarationEffect
    | NameAmbiguous
    | NameUnresolved
    | RecursiveTypeAlias
    | MalformedConstructorDeclaration
    | RecursionRequiresSignature
    | SignatureUnsatisfied
    | OrPatternBinderMismatch
    | OrPatternBinderTypeMismatch
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
        | TabCharacterNotPermitted -> "E_TAB_CHARACTER_NOT_PERMITTED"
        | UnrecognizedCharacter -> "E_UNRECOGNIZED_CHARACTER"
        | MalformedNumericLiteral -> "E_MALFORMED_NUMERIC_LITERAL"
        | UnterminatedStringInterpolation -> "E_UNTERMINATED_STRING_INTERPOLATION"
        | ExpectedSyntaxToken -> "E_EXPECTED_SYNTAX_TOKEN"
        | ExpectedClosingDelimiter -> "E_EXPECTED_CLOSING_DELIMITER"
        | ExpectedIndentedBlock -> "E_EXPECTED_INDENTED_BLOCK"
        | InvalidTypeSyntax -> "E_INVALID_TYPE_SYNTAX"
        | UnsupportedSyntax -> "E_UNSUPPORTED_SYNTAX"
        | UnexpectedTrailingSyntax -> "E_UNEXPECTED_TRAILING_SYNTAX"
        | ModifierNotApplicable -> "E_MODIFIER_NOT_APPLICABLE"
        | ElaborationFailed -> "E_ELABORATION_FAILED"
        | ImportUnhideRequiresBuildSetting -> "E_IMPORT_UNHIDE_REQUIRES_BUILD_SETTING"
        | ImportClarifyRequiresBuildSetting -> "E_IMPORT_CLARIFY_REQUIRES_BUILD_SETTING"
        | ImportItemModifierReexportForbidden -> "E_IMPORT_ITEM_MODIFIER_REEXPORT_FORBIDDEN"
        | AssertTerminatesRequiresModuleAttribute -> "E_ASSERT_TERMINATES_REQUIRES_MODULE_ATTRIBUTE"
        | AssertReducibleRequiresModuleAttribute -> "E_ASSERT_REDUCIBLE_REQUIRES_MODULE_ATTRIBUTE"
        | CheckpointVerification -> "E_CHECKPOINT_VERIFICATION"
        | TargetCheckpoint -> "E_TARGET_CHECKPOINT"
        | ExpectAmbiguous -> "E_EXPECT_AMBIGUOUS"
        | ExpectUnsatisfied -> "E_EXPECT_UNSATISFIED"
        | ImportCycle -> "E_IMPORT_CYCLE"
        | ImportAmbiguous -> "E_IMPORT_AMBIGUOUS"
        | ImportItemNotFound -> "E_IMPORT_ITEM_NOT_FOUND"
        | UrlImportUnpinnedInPackageMode -> "E_URL_IMPORT_UNPINNED_IN_PACKAGE_MODE"
        | UrlImportRefPinRequiresLock -> "E_URL_IMPORT_REF_PIN_REQUIRES_LOCK"
        | UrlImportUnsupported -> "E_URL_IMPORT_UNSUPPORTED"
        | HostModuleReservedRoot -> "E_HOST_MODULE_RESERVED_ROOT"
        | HostModuleUnsupportedBackend -> "E_HOST_MODULE_UNSUPPORTED_BACKEND"
        | EffectRuntimeUnsupportedBackend -> "E_EFFECT_RUNTIME_UNSUPPORTED_BACKEND"
        | MultishotEffectUnsupportedBackend -> "E_MULTISHOT_EFFECT_UNSUPPORTED_BACKEND"
        | ModuleAttributeUnknown -> "E_MODULE_ATTRIBUTE_UNKNOWN"
        | ModuleHeaderMisplaced -> "E_MODULE_HEADER_MISPLACED"
        | ModuleHeaderExpectedAfterAttributes -> "E_MODULE_HEADER_EXPECTED_AFTER_ATTRIBUTES"
        | ModuleNameUnresolved -> "E_MODULE_NAME_UNRESOLVED"
        | ModulePathMismatch -> "E_MODULE_PATH_MISMATCH"
        | ModuleCaseFoldCollision -> "E_MODULE_CASE_FOLD_COLLISION"
        | StaticObjectUnresolved -> "E_STATIC_OBJECT_UNRESOLVED"
        | PatternHeadNotConstructorOrActivePattern -> "E_PATTERN_HEAD_NOT_CONSTRUCTOR_OR_ACTIVE_PATTERN"
        | ActivePatternLinearityViolation -> "E_ACTIVE_PATTERN_LINEARITY_VIOLATION"
        | ActivePatternMatchResultNotAllowedInPlainLetQuestion -> "E_ACTIVE_PATTERN_MATCH_RESULT_NOT_ALLOWED_IN_PLAIN_LET_QUESTION"
        | ActivePatternMissingScrutineeBinder -> "E_ACTIVE_PATTERN_MISSING_SCRUTINEE_BINDER"
        | ActivePatternMonadicResult -> "E_ACTIVE_PATTERN_MONADIC_RESULT"
        | ProjectionCapabilityRequired -> "E_PROJECTION_CAPABILITY_REQUIRED"
        | ProjectionUpdateTargetUnsupported -> "E_PROJECTION_UPDATE_TARGET_UNSUPPORTED"
        | ProjectionRootInvalid -> "E_PROJECTION_ROOT_INVALID"
        | ProjectionDescriptorRootMissing -> "E_PROJECTION_DESCRIPTOR_ROOT_MISSING"
        | ProjectionRootsPackMismatch -> "E_PROJECTION_ROOTS_PACK_MISMATCH"
        | ProjectionDescriptorRootsLiteralRequired -> "E_PROJECTION_DESCRIPTOR_ROOTS_LITERAL_REQUIRED"
        | ProjectionDescriptorValueExpected -> "E_PROJECTION_DESCRIPTOR_VALUE_EXPECTED"
        | ProjectionMissingPlaceBinder -> "E_PROJECTION_MISSING_PLACE_BINDER"
        | ProjectionYieldInvalid -> "E_PROJECTION_YIELD_INVALID"
        | ProjectionExpandedAccessorPlaceBinderMismatch -> "E_PROJECTION_EXPANDED_ACCESSOR_PLACE_BINDER_MISMATCH"
        | ProjectionAccessorClauseDuplicate -> "E_PROJECTION_ACCESSOR_CLAUSE_DUPLICATE"
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
        | DerivingShapeNotData -> "KAPPA_DERIVING_SHAPE_NOT_DATA"
        | DerivingShapeNotClosedRecord -> "KAPPA_DERIVING_SHAPE_NOT_CLOSED_RECORD"
        | DerivingShapeOpaqueRepresentation -> "KAPPA_DERIVING_SHAPE_OPAQUE_REPRESENTATION"
        | DerivingShapeUnsupportedType -> "KAPPA_DERIVING_SHAPE_UNSUPPORTED_TYPE"
        | DerivingShapeBadConstructorArguments -> "KAPPA_DERIVING_SHAPE_BAD_CONSTRUCTOR_ARGUMENTS"
        | DerivingShapeBadRecordArguments -> "KAPPA_DERIVING_SHAPE_BAD_RECORD_ARGUMENTS"
        | DerivingShapeMissingRuntimeFieldInstance -> "KAPPA_DERIVING_SHAPE_MISSING_RUNTIME_FIELD_INSTANCE"
        | DerivingShapeDeclarationEffect -> "KAPPA_DERIVING_SHAPE_DECLARATION_EFFECT"
        | NameAmbiguous -> "E_NAME_AMBIGUOUS"
        | NameUnresolved -> "E_NAME_UNRESOLVED"
        | RecursiveTypeAlias -> "E_RECURSIVE_TYPE_ALIAS"
        | MalformedConstructorDeclaration -> "E_MALFORMED_CONSTRUCTOR_DECLARATION"
        | RecursionRequiresSignature -> "E_RECURSION_REQUIRES_SIGNATURE"
        | SignatureUnsatisfied -> "E_SIGNATURE_UNSATISFIED"
        | OrPatternBinderMismatch -> "E_OR_PATTERN_BINDER_MISMATCH"
        | OrPatternBinderTypeMismatch -> "E_OR_PATTERN_BINDER_TYPE_MISMATCH"
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
        | "E_TAB_CHARACTER_NOT_PERMITTED" -> Some TabCharacterNotPermitted
        | "E_UNRECOGNIZED_CHARACTER" -> Some UnrecognizedCharacter
        | "E_MALFORMED_NUMERIC_LITERAL" -> Some MalformedNumericLiteral
        | "E_UNTERMINATED_STRING_INTERPOLATION" -> Some UnterminatedStringInterpolation
        | "E_EXPECTED_SYNTAX_TOKEN" -> Some ExpectedSyntaxToken
        | "E_EXPECTED_CLOSING_DELIMITER" -> Some ExpectedClosingDelimiter
        | "E_EXPECTED_INDENTED_BLOCK" -> Some ExpectedIndentedBlock
        | "E_INVALID_TYPE_SYNTAX" -> Some InvalidTypeSyntax
        | "E_UNSUPPORTED_SYNTAX" -> Some UnsupportedSyntax
        | "E_UNEXPECTED_TRAILING_SYNTAX" -> Some UnexpectedTrailingSyntax
        | "E_MODIFIER_NOT_APPLICABLE" -> Some ModifierNotApplicable
        | "E_ELABORATION_FAILED" -> Some ElaborationFailed
        | "E_IMPORT_UNHIDE_REQUIRES_BUILD_SETTING" -> Some ImportUnhideRequiresBuildSetting
        | "E_IMPORT_CLARIFY_REQUIRES_BUILD_SETTING" -> Some ImportClarifyRequiresBuildSetting
        | "E_IMPORT_ITEM_MODIFIER_REEXPORT_FORBIDDEN" -> Some ImportItemModifierReexportForbidden
        | "E_ASSERT_TERMINATES_REQUIRES_MODULE_ATTRIBUTE" -> Some AssertTerminatesRequiresModuleAttribute
        | "E_ASSERT_REDUCIBLE_REQUIRES_MODULE_ATTRIBUTE" -> Some AssertReducibleRequiresModuleAttribute
        | "E_CHECKPOINT_VERIFICATION" -> Some CheckpointVerification
        | "E_TARGET_CHECKPOINT" -> Some TargetCheckpoint
        | "E_EXPECT_AMBIGUOUS" -> Some ExpectAmbiguous
        | "E_EXPECT_UNSATISFIED" -> Some ExpectUnsatisfied
        | "E_IMPORT_CYCLE" -> Some ImportCycle
        | "E_IMPORT_AMBIGUOUS" -> Some ImportAmbiguous
        | "E_IMPORT_ITEM_NOT_FOUND" -> Some ImportItemNotFound
        | "E_URL_IMPORT_UNPINNED_IN_PACKAGE_MODE" -> Some UrlImportUnpinnedInPackageMode
        | "E_URL_IMPORT_REF_PIN_REQUIRES_LOCK" -> Some UrlImportRefPinRequiresLock
        | "E_URL_IMPORT_UNSUPPORTED" -> Some UrlImportUnsupported
        | "E_HOST_MODULE_RESERVED_ROOT" -> Some HostModuleReservedRoot
        | "E_HOST_MODULE_UNSUPPORTED_BACKEND" -> Some HostModuleUnsupportedBackend
        | "E_EFFECT_RUNTIME_UNSUPPORTED_BACKEND" -> Some EffectRuntimeUnsupportedBackend
        | "E_MULTISHOT_EFFECT_UNSUPPORTED_BACKEND" -> Some MultishotEffectUnsupportedBackend
        | "E_MODULE_ATTRIBUTE_UNKNOWN" -> Some ModuleAttributeUnknown
        | "E_MODULE_HEADER_MISPLACED" -> Some ModuleHeaderMisplaced
        | "E_MODULE_HEADER_EXPECTED_AFTER_ATTRIBUTES" -> Some ModuleHeaderExpectedAfterAttributes
        | "E_MODULE_NAME_UNRESOLVED" -> Some ModuleNameUnresolved
        | "E_MODULE_PATH_MISMATCH" -> Some ModulePathMismatch
        | "E_MODULE_CASE_FOLD_COLLISION" -> Some ModuleCaseFoldCollision
        | "E_STATIC_OBJECT_UNRESOLVED" -> Some StaticObjectUnresolved
        | "E_PATTERN_HEAD_NOT_CONSTRUCTOR_OR_ACTIVE_PATTERN" -> Some PatternHeadNotConstructorOrActivePattern
        | "E_ACTIVE_PATTERN_LINEARITY_VIOLATION" -> Some ActivePatternLinearityViolation
        | "E_ACTIVE_PATTERN_MATCH_RESULT_NOT_ALLOWED_IN_PLAIN_LET_QUESTION" -> Some ActivePatternMatchResultNotAllowedInPlainLetQuestion
        | "E_ACTIVE_PATTERN_MISSING_SCRUTINEE_BINDER" -> Some ActivePatternMissingScrutineeBinder
        | "E_ACTIVE_PATTERN_MONADIC_RESULT" -> Some ActivePatternMonadicResult
        | "E_PROJECTION_CAPABILITY_REQUIRED" -> Some ProjectionCapabilityRequired
        | "E_PROJECTION_UPDATE_TARGET_UNSUPPORTED" -> Some ProjectionUpdateTargetUnsupported
        | "E_PROJECTION_ROOT_INVALID" -> Some ProjectionRootInvalid
        | "E_PROJECTION_DESCRIPTOR_ROOT_MISSING" -> Some ProjectionDescriptorRootMissing
        | "E_PROJECTION_ROOTS_PACK_MISMATCH" -> Some ProjectionRootsPackMismatch
        | "E_PROJECTION_DESCRIPTOR_ROOTS_LITERAL_REQUIRED" -> Some ProjectionDescriptorRootsLiteralRequired
        | "E_PROJECTION_DESCRIPTOR_VALUE_EXPECTED" -> Some ProjectionDescriptorValueExpected
        | "E_PROJECTION_MISSING_PLACE_BINDER" -> Some ProjectionMissingPlaceBinder
        | "E_PROJECTION_YIELD_INVALID" -> Some ProjectionYieldInvalid
        | "E_PROJECTION_EXPANDED_ACCESSOR_PLACE_BINDER_MISMATCH" -> Some ProjectionExpandedAccessorPlaceBinderMismatch
        | "E_PROJECTION_ACCESSOR_CLAUSE_DUPLICATE" -> Some ProjectionAccessorClauseDuplicate
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
        | "KAPPA_DERIVING_SHAPE_NOT_DATA" -> Some DerivingShapeNotData
        | "KAPPA_DERIVING_SHAPE_NOT_CLOSED_RECORD" -> Some DerivingShapeNotClosedRecord
        | "KAPPA_DERIVING_SHAPE_OPAQUE_REPRESENTATION" -> Some DerivingShapeOpaqueRepresentation
        | "KAPPA_DERIVING_SHAPE_UNSUPPORTED_TYPE" -> Some DerivingShapeUnsupportedType
        | "KAPPA_DERIVING_SHAPE_BAD_CONSTRUCTOR_ARGUMENTS" -> Some DerivingShapeBadConstructorArguments
        | "KAPPA_DERIVING_SHAPE_BAD_RECORD_ARGUMENTS" -> Some DerivingShapeBadRecordArguments
        | "KAPPA_DERIVING_SHAPE_MISSING_RUNTIME_FIELD_INSTANCE" -> Some DerivingShapeMissingRuntimeFieldInstance
        | "KAPPA_DERIVING_SHAPE_DECLARATION_EFFECT" -> Some DerivingShapeDeclarationEffect
        | "E_NAME_AMBIGUOUS" -> Some NameAmbiguous
        | "E_NAME_UNRESOLVED" -> Some NameUnresolved
        | "E_RECURSIVE_TYPE_ALIAS" -> Some RecursiveTypeAlias
        | "E_MALFORMED_CONSTRUCTOR_DECLARATION" -> Some MalformedConstructorDeclaration
        | "E_RECURSION_REQUIRES_SIGNATURE" -> Some RecursionRequiresSignature
        | "E_SIGNATURE_UNSATISFIED" -> Some SignatureUnsatisfied
        | "E_OR_PATTERN_BINDER_MISMATCH" -> Some OrPatternBinderMismatch
        | "E_OR_PATTERN_BINDER_TYPE_MISMATCH" -> Some OrPatternBinderTypeMismatch
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
        | TabCharacterNotPermitted ->
            Some "Tab characters are not permitted at this lexical position. Kappa source text must use the implementation's accepted space-based layout rules."
        | UnrecognizedCharacter ->
            Some "The source text contains a character that does not begin any valid Kappa token at this lexical position."
        | MalformedNumericLiteral ->
            Some "A numeric literal prefix, digit sequence, separator, or suffix is lexically malformed before typing is considered."
        | UnterminatedStringInterpolation ->
            Some "A string interpolation opened an embedded expression but reached the end of the containing source span before a matching closing brace."
        | ExpectedSyntaxToken ->
            Some "The parser reached a position where the grammar required a specific token or syntactic form that was not present."
        | ExpectedClosingDelimiter ->
            Some "An opening syntactic delimiter was not closed by the matching closing delimiter required by the grammar."
        | ExpectedIndentedBlock ->
            Some "The grammar required an indented block or a matching dedent boundary at this source position."
        | InvalidTypeSyntax ->
            Some "A region that is grammatically reserved for type syntax does not contain a well-formed type expression."
        | UnsupportedSyntax ->
            Some "The written token sequence is syntactically recognizable but not an admitted Kappa surface form at this position."
        | UnexpectedTrailingSyntax ->
            Some "A syntactic form parsed successfully, but extra tokens remained where the grammar requires the form to end."
        | ModifierNotApplicable ->
            Some "A source modifier or assertion was attached to a declaration kind that does not admit that modifier."
        | ElaborationFailed ->
            Some "Elaboration-time evaluation rejected the program or macro expansion without a more specific declared diagnostic code."
        | ImportUnhideRequiresBuildSetting ->
            Some "The import item uses the unhide escape hatch, but the current build configuration does not enable allow_unhiding for this compilation mode."
        | ImportClarifyRequiresBuildSetting ->
            Some "The import item uses the clarify escape hatch, but the current build configuration does not enable allow_clarify for this compilation mode."
        | ImportItemModifierReexportForbidden ->
            Some "Imports that use unhide or clarify are local escape hatches and must not be re-exported into another module's public surface."
        | AssertTerminatesRequiresModuleAttribute ->
            Some "assertTerminates and assertTotal are admitted only when the declaring module explicitly enables allow_assert_terminates."
        | AssertReducibleRequiresModuleAttribute ->
            Some "assertReducible is admitted only when the declaring module explicitly enables allow_assert_reducible."
        | ImportCycle ->
            Some "Imports must form an acyclic module dependency graph. Each module in the reported cycle depends, directly or through fragments, on the next module in the cycle."
        | ImportAmbiguous ->
            Some "This import syntax can resolve to more than one meaning. Rewrite it with an explicit module-only form or an explicit item-selection form so resolution is unique."
        | ImportItemNotFound ->
            Some "An explicitly selected or excluded import item does not exist in the referenced module's export surface."
        | ModuleNameUnresolved ->
            Some "A referenced module name does not resolve to any module available in the current compilation unit or bundled standard-module inventory."
        | NameAmbiguous ->
            Some "A referenced name resolves to multiple admissible declarations in the nearest applicable binding group, so compilation must reject the use until it is disambiguated."
        | NameUnresolved ->
            Some "A referenced name is not in lexical, module, or imported scope."
        | OrPatternBinderTypeMismatch ->
            Some "Each corresponding binder across an or-pattern's alternatives must have definitionally equal types after refinement and normalization."
        | TypeEqualityMismatch ->
            Some "Two types that must agree after normalization do not definitionally equal one another."
        | NumericLiteralOutOfRange ->
            Some "A numeric literal cannot be represented at the target numeric type required by the surrounding typing context."
        | QttContinuationCapture ->
            Some "A multi-shot continuation cannot capture live linear or borrowed resources across the operation site."
        | EffectRuntimeUnsupportedBackend ->
            Some "The selected backend profile does not implement first-class effect runtime constructs such as effect labels, effect operations, or handlers. Programs that retain such constructs at target lowering must be rejected on that backend."
        | MultishotEffectUnsupportedBackend ->
            Some "The selected backend profile does not advertise rt-multishot-effects, so reachable multi-shot effect invocations and exported declarations that may invoke them must be rejected."
        | ModuleAttributeUnknown ->
            Some "Only documented module attributes are accepted. Unknown module attributes are compile-time errors."
        | ModuleHeaderMisplaced ->
            Some "A source file may contain at most one module header, and if present it must appear before any non-comment, non-whitespace top-level token other than leading module attributes."
        | ModuleHeaderExpectedAfterAttributes ->
            Some "Top-level @Ident module attributes are only valid as the leading prefix of a module header."
        | ModuleCaseFoldCollision ->
            Some "Two or more source files in the same compilation unit produce module names that are equal after lowercase-ASCII case folding but differ in case. The collision must be rejected and all participating files identified."
        | PatternHeadNotConstructorOrActivePattern ->
            Some "A pattern head must resolve to a constructor or an active pattern, not to an ordinary term binding."
        | ActivePatternLinearityViolation ->
            Some "A refutable active-pattern use must not consume its scrutinee linearly unless the language form explicitly accounts for the residual case."
        | ActivePatternMatchResultNotAllowedInPlainLetQuestion ->
            Some "Plain let? destructuring admits Option-style active patterns, but not Match-returning active patterns that carry explicit miss residue."
        | ActivePatternMissingScrutineeBinder ->
            Some "An active pattern declaration must bind at least one explicit scrutinee parameter."
        | ActivePatternMonadicResult ->
            Some "An active pattern declaration result type must be a supported pattern result shape, not a general monadic computation."
        | ProjectionCapabilityRequired ->
            Some "This projection or accessor use requires a declared projection capability such as get, set, sink, or inout at the use site."
        | ProjectionUpdateTargetUnsupported ->
            Some "Projection-section update syntax can target only a selector projection or an accessor projection that provides update capability."
        | ProjectionRootInvalid ->
            Some "Projection roots must be stable places or selector-computed places whose roots are themselves stable places."
        | ProjectionDescriptorRootMissing ->
            Some "A projector descriptor application must still supply at least one root argument after elaboration."
        | ProjectionRootsPackMismatch ->
            Some "A projector descriptor roots pack must provide exactly the declared projector root fields, with matching names and no omissions."
        | ProjectionDescriptorRootsLiteralRequired ->
            Some "A multi-root projector descriptor application must receive its roots through an explicit record-literal roots pack."
        | ProjectionDescriptorValueExpected ->
            Some "This position expects a projector descriptor value, but the expression has already been fully applied and therefore denotes the projected focus instead."
        | ProjectionMissingPlaceBinder ->
            Some "A projection declaration must introduce at least one place binder."
        | ProjectionYieldInvalid ->
            Some "A selector projection yield must denote a stable place rooted in one of the projection's declared place binders."
        | ProjectionExpandedAccessorPlaceBinderMismatch ->
            Some "An expanded accessor projection declaration must bind exactly one place parameter."
        | ProjectionAccessorClauseDuplicate ->
            Some "An expanded accessor projection must not declare the same accessor clause kind more than once."
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
        | DerivingShapeNotData ->
            Some "The requested derivation-shape ADT inspection target does not elaborate to a visible data representation at the splice site."
        | DerivingShapeNotClosedRecord ->
            Some "The requested derivation-shape record inspection target does not elaborate to a visible closed record type at the splice site."
        | DerivingShapeOpaqueRepresentation ->
            Some "A data or record representation exists, but ordinary visibility or opacity rules do not make it inspectable at this elaboration site."
        | DerivingShapeUnsupportedType ->
            Some "The requested derivation-shape target is well-typed but outside the implementation's currently supported Phase 0 reflection forms."
        | DerivingShapeBadConstructorArguments ->
            Some "The supplied derivation-shape constructor arguments do not match the target constructor's required fields."
        | DerivingShapeBadRecordArguments ->
            Some "The supplied derivation-shape record arguments do not match the target record's required fields."
        | DerivingShapeMissingRuntimeFieldInstance ->
            Some "A required runtime-relevant field constraint could not be solved by ordinary implicit resolution at the splice site."
        | DerivingShapeDeclarationEffect ->
            Some "Phase 0 derivation-shape helpers must not manufacture or publish declarations as an elaboration-time side effect."
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
