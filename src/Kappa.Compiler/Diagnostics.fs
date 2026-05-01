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

type DiagnosticPayloadValue =
    | DiagnosticPayloadText of string
    | DiagnosticPayloadTextList of string list

type DiagnosticPayloadField =
    { Name: string
      Value: DiagnosticPayloadValue }

type DiagnosticPayload =
    { Kind: string
      Fields: DiagnosticPayloadField list }

type DiagnosticDescriptor =
    { Code: DiagnosticCode
      Family: string option
      Message: string
      Payload: DiagnosticPayload
      Explanation: string option }

type SimpleDiagnosticKind =
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
    | ModulePathMismatch
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
    | NameUnresolved
    | ModuleNameUnresolved
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

type SimpleDiagnosticEvidence =
    { Kind: SimpleDiagnosticKind
      Detail: string }

type CodeDetailEvidence =
    { Code: DiagnosticCode
      Detail: string }

type MacroExpansionDiagnosticEvidence =
    | FailElabMessage of message: string
    | WarnElabMessage of message: string
    | FailElabWithCode of code: DiagnosticCode * message: string
    | WarnElabWithCode of code: DiagnosticCode * message: string
    | SyntaxValueEscapesCapturedBindings of capturedNames: string list

type NumericLiteralOutOfRangeEvidence =
    { LiteralText: string }

type NameUnresolvedReferenceKind =
    | OrdinaryNameReference
    | ModuleQualifierReference

type NameUnresolvedEvidence =
    { Spelling: string
      ReferenceKind: NameUnresolvedReferenceKind
      AdmissibleKinds: string list
      SearchedScopes: string list
      GeneratedSyntax: bool }

type NameAmbiguousEvidence =
    { Spelling: string
      CandidateDescriptions: string list }

type DuplicatePatternBinderEvidence =
    { BinderName: string }

type DuplicateDeclarationEvidence =
    { Detail: string
      ModuleName: string option }

type ImportCycleEvidence =
    { ModulePath: string list }

type ModuleCaseFoldCollisionEvidence =
    { CollidingPaths: string list }

type InvalidUtf8SourceEvidence =
    { Detail: string }

type ModuleNameUnresolvedEvidence =
    { FilePath: string }

type HostModuleReservedRootEvidence =
    { ModuleName: string }

type ModulePathMismatchEvidence =
    { DeclaredModuleName: string
      InferredModuleName: string }

type UrlImportUnsupportedEvidence =
    { SpecifierText: string }

type HostModuleUnsupportedBackendEvidence =
    { BackendProfile: string
      HostBindingRoot: string }

type ImportAmbiguousEvidence =
    { FullModuleName: string
      ParentModuleName: string
      ItemName: string }

type ImportModuleNameUnresolvedEvidence =
    | BareImportTargetNotFound of fullModuleName: string * parentModuleName: string * itemName: string
    | ImportedModuleNotFound of importedModuleName: string

type ImportBuildSettingRequirement =
    | AllowUnhiding
    | AllowClarify

type ImportModifierBuildSettingEvidence =
    { ItemText: string
      RequiredSetting: ImportBuildSettingRequirement }

type ImportItemModifierReexportForbiddenEvidence =
    { ItemText: string }

type ImportItemLookupContext =
    | IncludedImportItem
    | ExcludedImportItem

type ImportItemNotFoundEvidence =
    { ItemName: string
      ImportedModuleName: string
      LookupContext: ImportItemLookupContext }

type ExpectDeclarationKind =
    | ExpectedTypeDeclaration
    | ExpectedTraitDeclaration
    | ExpectedTermDeclaration

type ExpectSatisfactionEvidence =
    { ExpectedKind: ExpectDeclarationKind
      ExpectedName: string }

type SignatureUnsatisfiedEvidence =
    { SignatureName: string
      SignatureTypeText: string }

type UrlImportPackageModeEvidence =
    | UrlImportUnpinnedInPackageMode of specifierText: string
    | UrlImportRefPinRequiresLock of specifierText: string

type ExpectedActualMismatchKind =
    | ExpressionTypeMismatch
    | ApplicationArgumentMismatch
    | FunctionResultMismatch
    | RecordFieldMismatch
    | RecordUpdateFieldMismatch

type ExpectedActualMismatchReason =
    | DependentEqualityTransportRequired
    | FunctionQuantityIncompatible
    | UnionCompatibilityFailed
    | SuspensionCompatibilityFailed
    | GeneralExpectedActualMismatch

type TypeEqualityMismatchEvidence =
    | ReflRequiresDefinitionallyEqualSides
    | ExpectedActualTypeMismatch of
        context: string
        * expectedText: string
        * actualText: string
        * mismatchKind: ExpectedActualMismatchKind
        * reason: ExpectedActualMismatchReason

type TypecheckingDiagnosticEvidence =
    | ProjectionCapabilityRequiredAtSite of capability: string
    | ProjectionRootRequiresStablePlace
    | RecordUpdateDependentFieldRequiresRepair of receiverRoot: string * fieldName: string
    | MultishotEffectUnsupportedBackendAtInvocation of backendProfile: string * effectVisibleName: string * operationName: string
    | ApplicationArgumentTypeMismatchAtSite of actualTypeText: string * demandedTypeText: string
    | ContinuationCaptureForbidden of effectVisibleName: string * operationName: string * capturedSummary: string
    | DeferredActionContainsAbruptOuterEscape
    | DefinitionBodyResultTypeMismatch

type BackendRuntimeEffectUseEvidence =
    | BackendEffectLabelUse of labelName: string
    | BackendEffectOperationUse of operationName: string
    | BackendEffectHandlerUse of isDeep: bool

type BackendLoweringErrorEvidence =
    | EmptyRuntimeName
    | AmbiguousRuntimeName of name: string
    | UnresolvedRuntimeName of name: string
    | UnresolvedModuleQualifier of qualifierText: string
    | EmptyRuntimeConstructorName
    | AmbiguousRuntimeConstructorName of constructorName: string
    | UnresolvedRuntimeConstructorName of constructorName: string
    | RuntimeCallTargetArityMismatch of moduleName: string * bindingName: string * expectedArity: int * actualArity: int
    | RuntimeCallTargetCouldNotBeLowered of moduleName: string * bindingName: string
    | OrPatternAlternativesBindDifferentNames
    | OrPatternBinderRepresentationMismatch of binderName: string
    | ConstructorPatternArityMismatch of patternText: string * expectedArity: int * actualArity: int
    | PatternDoesNotResolveToConstructor of patternText: string
    | RuntimeIntrinsicArityMismatch of intrinsicName: string * expectedArity: int * actualArity: int
    | RuntimeTagTestRequiresConstructorName

type BackendDiagnosticEvidence =
    | EffectRuntimeUnsupportedBackend of backendProfile: string * declarationName: string * effectUse: BackendRuntimeEffectUseEvidence
    | KBackendLoweringFailed of moduleName: string * error: BackendLoweringErrorEvidence

type TargetCheckpointDiagnosticEvidence =
    | TargetCheckpointWorkspaceHasDiagnostics of checkpoint: string * diagnosticCount: int * diagnosticCodes: string list
    | TargetCheckpointMalformedInput of checkpoint: string * inputCheckpoint: string * diagnosticCount: int * diagnosticCodes: string list
    | TargetCheckpointEmitterFailure of checkpoint: string * backendProfile: string * detail: string
    | UnknownTargetCheckpoint of checkpoint: string

type CheckpointVerificationEvidence =
    | DuplicateFileIdentity of checkpoint: string * filePath: string
    | NonEmptyLineTableRequired of checkpoint: string * filePath: string
    | EndOfFileTokenRequired of checkpoint: string * filePath: string
    | ResolvedPhaseSetMismatch of checkpoint: string * filePath: string * expectedPhases: string list * actualPhases: string list
    | OwnershipFactsExposedBeforeBodyResolve of checkpoint: string * filePath: string
    | DuplicateModuleIdentity of checkpoint: string * moduleName: string
    | MissingImportedRuntimeModule of checkpoint: string * moduleName: string * importedModuleName: string
    | DuplicateRuntimeBindingIdentity of checkpoint: string * moduleName: string * bindingName: string
    | DuplicateRuntimeConstructorIdentity of checkpoint: string * moduleName: string * constructorName: string
    | UnsupportedRuntimeIntrinsicTerm of checkpoint: string * moduleName: string * intrinsicName: string * backendProfile: string
    | IntrinsicBindingMustOmitBody of checkpoint: string * moduleName: string * bindingName: string
    | IntrinsicBindingMustBeListedInModuleIntrinsicTerms of checkpoint: string * moduleName: string * bindingName: string
    | UnsupportedRuntimeIntrinsicBinding of checkpoint: string * moduleName: string * bindingName: string * backendProfile: string
    | RuntimeBindingMustHaveBody of checkpoint: string * moduleName: string * bindingName: string
    | DuplicateRuntimePatternBinder of checkpoint: string * bindingLabel: string * binderName: string
    | DuplicateRuntimeClosureParameter of checkpoint: string * bindingLabel: string * parameterName: string
    | DeepRuntimeHandlerMustBeDesugared of checkpoint: string * bindingLabel: string
    | MissingBackendModuleForRuntimeModule of checkpoint: string * runtimeModuleName: string
    | MissingImportedBackendModule of checkpoint: string * moduleName: string * importedModuleName: string
    | DuplicateBackendFunctionIdentity of checkpoint: string * moduleName: string * functionName: string
    | DuplicateBackendDataLayoutIdentity of checkpoint: string * moduleName: string * typeName: string
    | DuplicateBackendEnvironmentLayoutIdentity of checkpoint: string * moduleName: string * layoutName: string
    | UnsupportedBackendIntrinsicTerm of checkpoint: string * moduleName: string * intrinsicName: string * backendProfile: string
    | ListedBackendEntryPointMustExistAsFunction of checkpoint: string * moduleName: string * entryPointName: string
    | ListedBackendEntryPointMustBeMarked of checkpoint: string * bindingLabel: string
    | BackendFunctionCallingConventionArityMismatch of checkpoint: string * bindingLabel: string
    | BackendFunctionParameterRepresentationsMismatch of checkpoint: string * bindingLabel: string
    | BackendFunctionEnvironmentLayoutMissing of checkpoint: string * bindingLabel: string * layoutName: string * moduleName: string
    | BackendEntryPointMustBeExported of checkpoint: string * bindingLabel: string
    | BackendMarkedEntryPointMustBeListed of checkpoint: string * bindingLabel: string
    | IntrinsicBackendFunctionMustOmitBody of checkpoint: string * bindingLabel: string
    | IntrinsicBackendFunctionMustBeListedInModuleIntrinsicTerms of checkpoint: string * bindingLabel: string
    | UnsupportedBackendIntrinsicFunction of checkpoint: string * moduleName: string * bindingName: string * backendProfile: string
    | BackendFunctionMustHaveBody of checkpoint: string * bindingLabel: string
    | PreErasureParameterTypeMetadataLeak of checkpoint: string * bindingLabel: string * parameterName: string * typeText: string
    | PreErasureReturnTypeMetadataLeak of checkpoint: string * bindingLabel: string * typeText: string
    | PreErasureConstructorFieldMetadataLeak of checkpoint: string * fieldLabel: string * typeText: string
    | PreErasureInstanceHeadMetadataLeak of checkpoint: string * instanceHeadLabel: string * typeText: string
    | DuplicateBackendPatternBinder of checkpoint: string * bindingLabel: string * binderName: string
    | BackendOrPatternAlternativesMustBindSameNames of checkpoint: string * bindingLabel: string
    | BackendOrPatternBinderRepresentationMismatch of checkpoint: string * bindingLabel: string * binderName: string
    | BackendPatternConstructorMissingFromModuleGraph of checkpoint: string * bindingLabel: string * constructorText: string
    | BackendResolvedRuntimeNameMissing of checkpoint: string * bindingLabel: string * resolvedNameText: string
    | DuplicateBackendClosureParameter of checkpoint: string * bindingLabel: string * parameterName: string
    | DuplicateBackendClosureCapture of checkpoint: string * bindingLabel: string * captureName: string
    | BackendClosureEnvironmentLayoutMissing of checkpoint: string * bindingLabel: string * layoutName: string * moduleName: string
    | BackendClosureCallingConventionArityMismatch of checkpoint: string * bindingLabel: string
    | BackendClosureParameterRepresentationsMismatch of checkpoint: string * bindingLabel: string
    | BackendClosureRepresentationLayoutMismatch of checkpoint: string * bindingLabel: string * layoutName: string
    | BackendClosureRepresentationMustBeClosure of checkpoint: string * bindingLabel: string
    | BackendCallConventionArityMismatch of checkpoint: string * bindingLabel: string
    | BackendCallParameterRepresentationCountMismatch of checkpoint: string * bindingLabel: string
    | BackendConstructedDataMissingLayout of checkpoint: string * bindingLabel: string * constructorText: string

type SurfaceRecordContext =
    | RecordTypeTelescope
    | RecordLiteralFields

type SurfaceRecordDiagnosticEvidence =
    | RecordFieldDeclaredMoreThanOnce of fieldName: string
    | RecordFieldDependsOnUnknownField of fieldName: string * referencedField: string
    | RecordDependenciesMustBeAcyclic of context: SurfaceRecordContext
    | RecordPatchPathDeclaredMoreThanOnce of pathText: string
    | RecordPatchPathStrictPrefixConflict of prefixPath: string * fullPath: string
    | RowExtensionLabelDeclaredMoreThanOnce of labelName: string
    | RowExtensionLabelAlreadyExists of labelName: string
    | RowExtensionLabelMissingLacksConstraint of labelName: string * rowName: string
    | RecordPatchUnknownField of fieldName: string
    | RecordPatchContinuesThroughNonRecordField of fieldName: string
    | ProjectionSectionUpdateTargetUnsupported

type SurfaceElaborationDiagnosticEvidence =
    | ProjectionDefinitionRequiresPlaceBinder
    | SelectorProjectionYieldInvalid of reason: ProjectionYieldInvalidReason
    | ExpandedAccessorProjectionRequiresExactlyOnePlaceBinder
    | ProjectionAccessorClauseDeclaredMoreThanOnce of clauseKind: string
    | UnknownModuleAttribute of attributeName: string * supportedAttributes: string list
    | AssertTerminatesRequiresAllowAttribute of bindingName: string
    | AssertReducibleRequiresAllowAttribute of bindingName: string
    | RecursiveTypeAliasDependsOnItself of aliasName: string
    | TopLevelRecursiveBindingRequiresPrecedingSignature of bindingName: string
    | TrivialRecursiveCycleMustBeRejected of bindingName: string
    | ConstructorDeclarationStartsWithDeclarationKeyword of dataTypeName: string * constructorName: string
    | ConstructorExposesRuntimeFieldMetadataOfType of constructorName: string
    | ConstructorDeclarationMalformedInDataType of dataTypeName: string * constructorName: string
    | RecordProjectionFieldMissing of fieldName: string
    | SealProjectionWouldExposeOpaqueDependentMember of rootName: string * memberName: string
    | RecordLiteralCannotBeCheckedDirectlyAgainstSignatureType
    | SealAscriptionMustBeClosedRecordType
    | ProjectionDescriptorRootsMustBeStablePlaces
    | ProjectionDescriptorApplicationRequiresAtLeastOneRoot
    | ProjectionDescriptorRootsPackFieldsMustMatchBindersExactly
    | MultiRootProjectionDescriptorRequiresRecordLiteralRootsPack
    | FullyAppliedProjectionProducesProjectedFocusNotDescriptorValue
    | MemberCallRequiresExactlyOneReceiverBinder of memberName: string
    | MemberCallMissingPrecedingExplicitArguments of memberName: string
    | MemberCallMissingExplicitArguments of memberName: string
    | ApplicationArgumentsDoNotMatchCalleeParameters
    | NamedApplicationArgumentsDoNotMatchCalleeParameterMetadata
    | ImplicitApplicationArgumentUnresolvedOrMismatched
    | NamedApplicationRequiresPreservedParameterMetadata
    | ImplicitApplicationArgumentAmbiguous
    | QuantityZeroImplicitCannotSatisfyRuntimeParameter
    | ApplicationExpressionNotCallable of calleeTypeText: string
    | OverloadedTraitMemberUnresolvedForArgumentTypes of memberName: string * traitOwners: string list * argumentTypes: string list
    | MemberAccessNotWellFormedForReceiverType of memberName: string * receiverTypeText: string
    | SafeNavigationRequiresKnownResultType of memberText: string
    | SafeNavigationReceiverMustBeOption
    | ElvisReceiverMustBeOption
    | QueryCarrierCannotDiscardMapMetadata
    | QueryCarrierCannotDiscardSetMetadata
    | QueryCarrierUseMismatch of expectedUse: string * inferredUse: string
    | QueryCarrierCardinalityMismatch of inferredCardinality: string * expectedCardinality: string
    | QueryCarrierItemQuantityMismatch of inferredQuantity: string * expectedQuantity: string
    | ComprehensionSinkItemTypeMismatch of expectedItemTypeText: string * actualItemTypeText: string
    | ArithmeticOperandResolvesToCallableBinding of operandLabel: string * bindingName: string
    | ArithmeticOperandsMustBeNumeric of leftTypeText: string * rightTypeText: string
    | ShortCircuitOperandMustBeSuspendedBool of operandLabel: string * operandTypeText: string
    | ObjectPhaseValueCannotBePassedDirectlyToElaboration of name: string
    | QuoteSpliceRequiresSyntax of actualTypeText: string
    | QuoteSpliceRequiresSyntaxNotElabSyntax
    | TopLevelSpliceRequiresSyntaxOrElabSyntax of actualTypeText: string
    | DoBindRequiresBindableCarrier of sourceTypeText: string
    | HandlerClausesMustReturnCarrierResultType of actualTypeText: string
    | HandlerEffectRowMissingLabel of effectName: string * effectRowText: string
    | HandlerEffectRowNeedsTailRefinement of effectName: string * effectRowText: string
    | HandlerHandledExpressionMustBeEff of handledTypeText: string
    | HandlerMissingOperationClause of effectName: string * operationName: string
    | HandlerDuplicateOperationClause of effectName: string * operationName: string
    | HandlerUnexpectedOperationClause of effectName: string * operationName: string
    | HandlerOperationClauseArityMismatch of operationName: string * actualArgumentCount: int * expectedArgumentCount: int
    | HandlerRelevantResumptionUnused of operationName: string * resumptionName: string
    | ConstructorDefaultReferencesUnavailableBinder of parameterName: string * referencedName: string
    | ConstructorDefaultTypeMismatch of parameterName: string * actualTypeText: string * expectedTypeText: string
    | ConstructorDefaultCouldNotBeChecked of parameterName: string
    | BorrowedEffectResumptionQuantityUnsupported of effectOperationName: string * quantityText: string
    | ParameterNumericContextTypeMismatch of parameterName: string * requiredTypeText: string * declaredTypeText: string
    | ConstructorFieldProjectionRequiresUniqueRefinement of projectionText: string
    | ListConsDefinitionBodyRequiresListResultType of declaredResultTypeText: string
    | StaticObjectOrTypeValueCannotSatisfyDefinitionBody
    | MacroExpandedSyntaxOverusesLinearBinder of binderName: string
    | DeclaredTypeInferredTypeMismatch of context: string * declaredTypeText: string * inferredTypeText: string
    | OrPatternBinderTypesDisagree of binderName: string * firstTypeText: string * alternativeTypeText: string
    | InstanceMemberReturnTypeMismatch of traitName: string * memberName: string * expectedReturnTypeText: string * declaredReturnTypeText: string
    | MultishotEffectUnsupportedBackendForExportedDeclaration of backendProfile: string * effectVisibleName: string * operationName: string * bindingName: string
    | RefutableGeneratorRequiresForQuestion
    | LeftJoinBinderNotInScopeAfterClause of bindingName: string * intoName: string
    | TraitConstraintUnresolved of constraintText: string
    | ImplicitTraitConstraintUnresolved of constraintText: string
    | TraitConstraintAmbiguous of constraintText: string * candidateTexts: string list
    | TraitSupertraitEvidenceUnsatisfied of traitName: string * requiredConstraintText: string
    | SyntaxCarrierEscapesCapturedBindings of escapedNames: string list
    | PrefixedStringPrefixMustResolveToInterpolatedMacro of prefix: string
    | ProjectedFieldTypeDoesNotMatchDeclaredResultType
    | ConstructorTermCannotSatisfyTypeValuedDefinitionBody
    | ElaborationFailureFromFailElabWith of errorCode: string * elaborationMessage: string
    | StaticConstructorRequiresPreservedStaticObjectIdentity of memberName: string
    | StaticObjectUnresolvedByKind of kindSelector: string * nameText: string
    | PatternHeadResolvedToOrdinaryTerm of headName: string
    | ActivePatternLinearlyConsumesScrutineeInRefutableContext of patternName: string * context: string
    | MatchReturningActivePatternNotPermittedInPlainLetQuestion of patternName: string
    | ActivePatternDeclarationRequiresExplicitScrutineeBinder
    | ActivePatternDeclarationResultMustNotBeMonadic

and ProjectionYieldInvalidReason =
    | ProjectionYieldMustDenoteStablePlaceNotAccessorBundle
    | ProjectionYieldMustDenoteStablePlace
    | ProjectionYieldMustBeRootedInPlaceBinder

type CorePatternParsingEvidence =
    | UnsupportedParameterBinderSyntax
    | ExpectedParameterBinder
    | ExpectedParameterName
    | ExpectedPatternName
    | OrPatternAlternativesMustBindSameNames
    | NumericLiteralSuffixesNotPermittedInPatterns of tokenText: string
    | NumericLiteralNotRepresentableInPatterns of tokenText: string
    | ExpectedLiteralPattern
    | ExpectedPatternCloseParenthesis
    | ExpectedRecordPatternFieldLabel
    | ExpectedRecordPatternField
    | ExpectedPatternCloseBrace
    | ExpectedNamedConstructorPatternFieldLabel
    | ExpectedNamedConstructorPatternField
    | ExpectedPattern
    | NamedConstructorPatternsCannotTakePositionalSubpatterns
    | OnlyConstructorPatternsMayTakeArguments
    | UnexpectedTrailingPatternTokens
    | InvalidStringLiteralPattern of StringLiteralDecodeError
    | InvalidNumericLiteralPattern of NumericLiteralParseError

type CoreExpressionParsingEvidence =
    | UnterminatedParameterBinder
    | ExpectedLambdaParameterOrArrow
    | LambdaMustDeclareAtLeastOneParameter
    | ExpectedDoBlockDedent
    | ExpectedCaseClauseArrow
    | ExpectedIndentedExpressionDedent
    | ExplicitBracesAfterLayoutIntroducedBlockForbidden
    | UnexpectedIndentationInIndentedCaseBody
    | MatchExpressionMustDeclareAtLeastOneCase
    | ExpectedMatchCasesDedent
    | ExpectedWhileBodyDedent
    | ExpectedLetQuestionFailureArrow
    | ExpectedLetQuestionEquals
    | ExpectedDoIfThen
    | ExpectedExpressionAfterDefer
    | ExpectedUsingBindingPattern
    | ExpectedUsingBindingArrow
    | UnexpectedIndentedExpressionContinuationInDoBinding
    | ExpectedDoBindingAssignmentOrBind
    | ExpectedWhileDo
    | DoBlockMustContainAtLeastOneStatement
    | ExpectedLambdaBodyArrow
    | PureBlockMustContainExpression of CorePureBlockContext
    | ExpectedPureBlockFinalExpression of CorePureBlockContext
    | InvalidStringLiteralExpression of StringLiteralDecodeError
    | InvalidNumericLiteralExpression of NumericLiteralParseError
    | InvalidStringTextSegment of StringLiteralDecodeError
    | ExpectedNamedGroupAggregation
    | ExpectedGroupAggregationEquals
    | ExpectedComprehensionGeneratorIn
    | ExpectedConflictCombineUsingQualifiedName
    | ExpectedConflictClauseForm
    | ExpectedComprehensionLetEquals
    | ExpectedComprehensionJoinClause
    | ExpectedGroupByIntoName
    | ExpectedGroupAggregationBlockClose
    | ExpectedGroupAggregationBlock
    | ExpectedLeftJoinIntoName
    | ExpectedLeftJoinClause
    | UnsupportedComprehensionClause
    | QueryPagingRequiresOrderedPipeline of operationKind: string
    | ComprehensionMustEndWithYieldClause
    | ExpectedHandlerClauseArrow
    | ExpectedHandlerResumptionBinder
    | ExpectedHandlerClauseHead
    | ExpectedHandlerClauseStartingWithCase
    | ExpectedHandlerClause
    | ExpectedHandlerWith
    | ExpectedImplicitParameterAfterAt of CoreHeaderContext
    | UnterminatedImplicitParameterBinder of CoreHeaderContext
    | UnsupportedImplicitParameterSyntax of CoreHeaderContext
    | UnterminatedParameterBinderInHeader of CoreHeaderContext
    | UnsupportedHeaderSyntax of CoreHeaderContext
    | ExpectedCoreKeyword of keywordText: string
    | ExpectedQualifiedNameSegment
    | ExpectedNameAfterSelector of selectorDescription: string
    | ExpectedConstructorNameAfterIs
    | ExpectedRecordUpdateClose
    | ExpectedExplicitMemberProjectionName
    | ExpectedSafeNavigationMemberAccess
    | ConstructorTagTestsCannotBeChained
    | UnexpectedTrailingExpressionTokens
    | ExpectedNamedApplicationFieldLabel
    | ExpectedNamedApplicationField
    | ExpectedRecordFieldLabel
    | ExpectedRecordField
    | ExpectedSealAs
    | ExpectedSealValue
    | ExpectedProjectionBodyHead
    | ExpectedProjectionMatchCaseBlock
    | ExpectedProjectionMatchCaseClause
    | ExpectedProjectionCaseClauseArrow
    | ExpectedLocalLetIn
    | ExpectedLocalLetEquals
    | ExpectedExpressionCloseParenthesis
    | ExpectedExpression
    | ExpectedInterpolationEndBeforeStringResumes
    | ExpectedInterpolatedStringContent
    | UnterminatedInterpolatedString
    | ExpectedIfThen
    | ExpectedIfElse
    | DuplicateHandlerReturnClause
    | MissingHandlerReturnClause
    | HandlerReturnClauseArityMismatch of argumentCount: int
    | RecordPatchExtensionMustBeTopLevelLabel
    | ExpectedRecordPatchPath
    | ExpectedRecordPatchItem
    | ExpectedProjectionThen
    | ExpectedProjectionElse
    | ExpectedProjectionAccessorClause
    | ExpectedProjectionAccessorClauseArrow
    | ExpectedProjectionSetAccessor
    | ProjectionSetAccessorRequiresTypedParameter
    | ProjectionSetAccessorUsesOrdinaryParameter
    | ExpectedProjectionPlaceBinder
    | ProjectionParametersMustNotUseInout
    | ExpectedProjectionResultTypeColon
    | UnterminatedProjectionBinder
    | UnsupportedProjectionBinderSyntax
    | ExpectedProjectionSetAccessorParameter
    | ExpectedListExpressionClose
    | ExpectedSetExpressionClose
    | ExpectedMapExpressionClose
    | ExpectedNamedApplicationBlockClose
    | ExpectedSyntaxQuoteClose
    | ExpectedSyntaxSpliceClose
    | ExpectedCodeQuoteClose

and CoreHeaderContext =
    | TopLevelFunctionHeader
    | LocalFunctionHeader

and CorePureBlockContext =
    | BlockExpressionBody
    | LambdaPureBody

type ParserNameExpectationRole =
    | BindingName
    | DataTypeName
    | ModuleName
    | ModuleNameSegment
    | ModuleAttributeName
    | NameInList
    | NameInSignatureDeclaration
    | ProjectionName
    | TypeAliasName
    | TraitNameAfterExpectTrait
    | TypeNameAfterExpectType
    | TermNameAfterExpectTerm
    | EffectName
    | ImportedName
    | ExcludedImportName
    | AliasAfterAs

type ParserListContext =
    | NameList
    | ExclusionList
    | ImportItemList

type ParserDeclarationSubject =
    | ParserEffectDeclarationSubject
    | ParserInstanceDeclarationSubject
    | ParserInstanceMemberDeclarationSubject
    | ParserProjectionDeclarationSubject
    | ParserNamedDeclarationSubject of keywordText: string

type ParserDeclarationBlockDelimiter =
    | ParserRightParenthesisDelimiter
    | ParserRightBraceDelimiter
    | ParserRightBracketDelimiter
    | ParserRightSetBraceDelimiter
    | ParserDedentDelimiter

type ParserModifierTarget =
    | ProjectionDeclarationTarget
    | DataDeclarationTarget
    | EffectDeclarationTarget
    | TraitDeclarationTarget
    | SignatureDeclarationTarget
    | InstanceDeclarationTarget
    | TermDeclarationsOnly

type ParserSyntaxEvidence =
    | ExpectedKeyword of keywordText: string
    | ExpectedName of ParserNameExpectationRole
    | ExpectedColonInExpectTermDeclaration
    | ExpectedColonInSignatureDeclaration
    | ExpectedListStart of ParserListContext
    | ExpectedListClose of ParserListContext
    | ExpectedListComma of ParserListContext
    | DuplicateImportItemModifier of modifierKeyword: string
    | ExpectedImportItemAfterDot
    | CtorAllCannotBeCombinedWithAlias
    | MultipleTotalityAssertionsOnSameDeclaration
    | ExpectedExpectDeclarationKind
    | ExpectedFixityDeclaration
    | ExpectedNumericPrecedenceInFixityDeclaration
    | ExpectedValidIntegerPrecedenceInFixityDeclaration
    | ExpectedFixityOperatorLeftParenthesis
    | ExpectedFixityOperatorToken
    | ExpectedFixityOperatorRightParenthesis
    | ExpectedTopLevelDeclaration
    | ExpectedTraitHeadAfterTrait
    | ExpectedTraitMemberSignatureOrDefaultDefinition
    | ExpectedTypeAliasBody
    | ExpectedValidTypeAliasBody
    | ExpectedSignatureType
    | ExpectedValidSignatureType
    | ExpectedConstructorName
    | UnsupportedConstructorParameterSyntax
    | ExpectedEffectOperationSignatureShape
    | ExpectedEffectOperationSignature
    | ExpectedEqualsInDeclaration of ParserDeclarationSubject
    | ExpectedIndentedBlock
    | ExpectedDeclarationBlockClosure of ParserDeclarationBlockDelimiter
    | ExpectedValidInstanceHeadAfterInstance
    | ExpectedInstanceMemberDefinitionStartingWithLet
    | UnexpectedIndentationAtTopLevel
    | ModuleHeaderExpectedAfterTopLevelAttributes
    | ModuleHeaderMisplacedAfterTopLevelItems
    | TotalityAssertionNotApplicableTo of ParserModifierTarget
    | OpacityModifierNotApplicableToProjectionDeclaration
    | OpacityAndTotalityNotApplicableToEffectDeclaration
    | VisibilityOpacityAndTotalityNotApplicableToInstanceDeclaration
    | InvalidStringLiteral of StringLiteralDecodeError
    | InvalidUrlModuleSpecifier of specifierText: string * UrlModuleSpecifierParseError

type UnicodeScalarLiteralEvidence = UnicodeScalarLiteralDecodeError

type UnicodeGraphemeLiteralEvidence = GraphemeLiteralDecodeError

type UnicodeByteLiteralEvidence = ByteLiteralDecodeError

type LexerDiagnosticEvidence =
    | TabCharacterNotPermitted of inIndentation: bool
    | UnexpectedIndentation of indent: int
    | UnrecognizedCharacter of character: string
    | MalformedNumericLiteral of NumericTokenScanError
    | UnterminatedStringLiteral of isPrefixed: bool
    | UnterminatedCharacterLiteral
    | UnterminatedBacktickIdentifier
    | UnterminatedStringInterpolation
    | UnterminatedBlockComment

type QttCardinalityEffect =
    | Discard
    | Duplicate
    | DuplicateOrDiscard
    | Reorder

type QttDemandSatisfactionMode =
    | Available
    | Usable

type QttLinearDropEvidence =
    | ShadowedBindingMustConsumePreviousValue of shadowedBindingName: string
    | BindingNotConsumedOnEveryPath of bindingName: string
    | ClauseMayDiscardRowBinding of clauseLabel: string * bindingName: string
    | DropClauseCardinalityMayAffectRowBinding of
        clauseLabel: string * bindingName: string * effect: QttCardinalityEffect * cardinalityText: string
    | RecordPatternOmittedField of fieldName: string
    | PlainLetQuestionDiscardRefutationResidue

type QttLinearOveruseEvidence =
    | WholeResourceConsumedAfterFieldPath of resourceName: string
    | WholeResourceUsedAfterFieldPath of resourceName: string
    | FieldPathConsumedAfterRoot of placeText: string
    | FieldPathAlreadyConsumed of placeText: string
    | ResourceConsumedMoreThanOnce of resourceName: string
    | ClauseMustNotConsumeRowBinding of clauseLabel: string * bindingName: string
    | OveruseClauseCardinalityMayAffectRowBinding of
        clauseLabel: string * bindingName: string * effect: QttCardinalityEffect * cardinalityText: string
    | LeftJoinCapturesLinearOuterRowBinding of bindingName: string
    | RecordUpdateRequiresRepair of receiverRoot: string * firstUnrepairedPath: string
    | QuantityCannotSatisfyParameterDemand of
        capabilityText: string * demandText: string * mode: QttDemandSatisfactionMode

type QttBorrowConsumeEvidence =
    | BorrowedResourceCannotBeConsumed of resourceName: string

type QttBorrowOverlapEvidence =
    | PlaceOverlapsActiveBorrowedFootprint of placeText: string
    | InoutArgumentsRequireDisjointFootprints
    | TemporaryBorrowOverlapsLaterConsumingArgument

type QttBorrowEscapeEvidence =
    | ValueCapturesBorrowedRegion
    | LambdaCapturesBorrowedRegion
    | ForkedChildCapturesBorrowedRegion

type QttErasedRuntimeUseEvidence =
    | QuantityZeroBindingUsedAtRuntime of bindingName: string
    | RuntimeClosureCapturesErasedBinding of bindingName: string
    | MatchScrutineeUsesErasedValue

type QttUsingExplicitQuantityEvidence =
    | UsingBindsBorrowedPattern

type QttInoutMarkerRequiredEvidence =
    | InoutMarkerRequired

type QttInoutMarkerUnexpectedEvidence =
    | InoutMarkerUnexpected

type QttInoutThreadedFieldMissingEvidence =
    | InoutThreadedFieldMissing of parameterName: string

type DiagnosticFact =
    | SimpleDiagnostic of SimpleDiagnosticEvidence
    | CodeDetailDiagnostic of CodeDetailEvidence
    | NameUnresolvedDiagnostic of NameUnresolvedEvidence
    | NameAmbiguousDiagnostic of NameAmbiguousEvidence
    | DuplicatePatternBinderDiagnostic of DuplicatePatternBinderEvidence
    | DuplicateDeclarationDiagnostic of DuplicateDeclarationEvidence
    | ImportCycleDiagnostic of ImportCycleEvidence
    | ModuleCaseFoldCollisionDiagnostic of ModuleCaseFoldCollisionEvidence
    | InvalidUtf8SourceDiagnostic of InvalidUtf8SourceEvidence
    | MacroExpansionDiagnostic of MacroExpansionDiagnosticEvidence
    | NumericLiteralOutOfRangeDiagnostic of NumericLiteralOutOfRangeEvidence
    | ModuleNameUnresolvedDiagnostic of ModuleNameUnresolvedEvidence
    | HostModuleReservedRootDiagnostic of HostModuleReservedRootEvidence
    | ModulePathMismatchDiagnostic of ModulePathMismatchEvidence
    | UrlImportUnsupportedDiagnostic of UrlImportUnsupportedEvidence
    | HostModuleUnsupportedBackendDiagnostic of HostModuleUnsupportedBackendEvidence
    | ImportAmbiguousDiagnostic of ImportAmbiguousEvidence
    | ImportModuleNameUnresolvedDiagnostic of ImportModuleNameUnresolvedEvidence
    | ImportModifierBuildSettingDiagnostic of ImportModifierBuildSettingEvidence
    | ImportItemModifierReexportForbiddenDiagnostic of ImportItemModifierReexportForbiddenEvidence
    | ImportItemNotFoundDiagnostic of ImportItemNotFoundEvidence
    | ExpectUnsatisfiedDiagnostic of ExpectSatisfactionEvidence
    | ExpectAmbiguousDiagnostic of ExpectSatisfactionEvidence
    | SignatureUnsatisfiedDiagnostic of SignatureUnsatisfiedEvidence
    | UrlImportPackageModeDiagnostic of UrlImportPackageModeEvidence
    | TypeEqualityMismatchDiagnostic of TypeEqualityMismatchEvidence
    | TypecheckingDiagnostic of TypecheckingDiagnosticEvidence
    | BackendDiagnostic of BackendDiagnosticEvidence
    | TargetCheckpointDiagnostic of TargetCheckpointDiagnosticEvidence
    | CheckpointVerificationDiagnostic of CheckpointVerificationEvidence
    | SurfaceRecordDiagnostic of SurfaceRecordDiagnosticEvidence
    | SurfaceElaborationDiagnostic of SurfaceElaborationDiagnosticEvidence
    | ParserSyntaxDiagnostic of ParserSyntaxEvidence
    | CorePatternParsingDiagnostic of CorePatternParsingEvidence
    | CoreExpressionParsingDiagnostic of CoreExpressionParsingEvidence
    | UnicodeScalarLiteralDiagnostic of UnicodeScalarLiteralEvidence
    | UnicodeGraphemeLiteralDiagnostic of UnicodeGraphemeLiteralEvidence
    | UnicodeByteLiteralDiagnostic of UnicodeByteLiteralEvidence
    | LexerDiagnostic of LexerDiagnosticEvidence
    | QttLinearDropDiagnostic of QttLinearDropEvidence
    | QttLinearOveruseDiagnostic of QttLinearOveruseEvidence
    | QttBorrowConsumeDiagnostic of QttBorrowConsumeEvidence
    | QttBorrowOverlapDiagnostic of QttBorrowOverlapEvidence
    | QttBorrowEscapeDiagnostic of QttBorrowEscapeEvidence
    | QttErasedRuntimeUseDiagnostic of QttErasedRuntimeUseEvidence
    | QttUsingExplicitQuantityDiagnostic of QttUsingExplicitQuantityEvidence
    | QttInoutMarkerRequiredDiagnostic of QttInoutMarkerRequiredEvidence
    | QttInoutMarkerUnexpectedDiagnostic of QttInoutMarkerUnexpectedEvidence
    | QttInoutThreadedFieldMissingDiagnostic of QttInoutThreadedFieldMissingEvidence

module DiagnosticFact =
    let private field name value =
        { Name = name
          Value = value }

    let private payload kind fields =
        { Kind = kind
          Fields = fields }

    let private defaultFamily code =
        match code with
        | DiagnosticCode.NameUnresolved
        | DiagnosticCode.ModuleNameUnresolved -> Some "kappa.name.unresolved"
        | DiagnosticCode.NameAmbiguous -> Some "kappa.name.ambiguous"
        | DiagnosticCode.ImportAmbiguous -> Some "kappa.import.ambiguous-dotted"
        | _ -> None

    let private descriptor code family message payload =
        { Code = code
          Family = family |> Option.orElseWith (fun () -> defaultFamily code)
          Message = message
          Payload = payload
          Explanation = DiagnosticCode.tryGetExplanation code }

    let private simpleCode kind =
        match kind with
        | SimpleDiagnosticKind.SourceInfo -> DiagnosticCode.SourceInfo
        | SimpleDiagnosticKind.SourceWarning -> DiagnosticCode.SourceWarning
        | SimpleDiagnosticKind.TabCharacterNotPermitted -> DiagnosticCode.TabCharacterNotPermitted
        | SimpleDiagnosticKind.UnrecognizedCharacter -> DiagnosticCode.UnrecognizedCharacter
        | SimpleDiagnosticKind.MalformedNumericLiteral -> DiagnosticCode.MalformedNumericLiteral
        | SimpleDiagnosticKind.UnterminatedStringInterpolation -> DiagnosticCode.UnterminatedStringInterpolation
        | SimpleDiagnosticKind.ExpectedSyntaxToken -> DiagnosticCode.ExpectedSyntaxToken
        | SimpleDiagnosticKind.ExpectedClosingDelimiter -> DiagnosticCode.ExpectedClosingDelimiter
        | SimpleDiagnosticKind.ExpectedIndentedBlock -> DiagnosticCode.ExpectedIndentedBlock
        | SimpleDiagnosticKind.InvalidTypeSyntax -> DiagnosticCode.InvalidTypeSyntax
        | SimpleDiagnosticKind.UnsupportedSyntax -> DiagnosticCode.UnsupportedSyntax
        | SimpleDiagnosticKind.UnexpectedTrailingSyntax -> DiagnosticCode.UnexpectedTrailingSyntax
        | SimpleDiagnosticKind.ModifierNotApplicable -> DiagnosticCode.ModifierNotApplicable
        | SimpleDiagnosticKind.ElaborationFailed -> DiagnosticCode.ElaborationFailed
        | SimpleDiagnosticKind.ImportUnhideRequiresBuildSetting -> DiagnosticCode.ImportUnhideRequiresBuildSetting
        | SimpleDiagnosticKind.ImportClarifyRequiresBuildSetting -> DiagnosticCode.ImportClarifyRequiresBuildSetting
        | SimpleDiagnosticKind.ImportItemModifierReexportForbidden -> DiagnosticCode.ImportItemModifierReexportForbidden
        | SimpleDiagnosticKind.AssertTerminatesRequiresModuleAttribute -> DiagnosticCode.AssertTerminatesRequiresModuleAttribute
        | SimpleDiagnosticKind.AssertReducibleRequiresModuleAttribute -> DiagnosticCode.AssertReducibleRequiresModuleAttribute
        | SimpleDiagnosticKind.CheckpointVerification -> DiagnosticCode.CheckpointVerification
        | SimpleDiagnosticKind.TargetCheckpoint -> DiagnosticCode.TargetCheckpoint
        | SimpleDiagnosticKind.ExpectAmbiguous -> DiagnosticCode.ExpectAmbiguous
        | SimpleDiagnosticKind.ExpectUnsatisfied -> DiagnosticCode.ExpectUnsatisfied
        | SimpleDiagnosticKind.ImportAmbiguous -> DiagnosticCode.ImportAmbiguous
        | SimpleDiagnosticKind.ImportItemNotFound -> DiagnosticCode.ImportItemNotFound
        | SimpleDiagnosticKind.UrlImportUnpinnedInPackageMode -> DiagnosticCode.UrlImportUnpinnedInPackageMode
        | SimpleDiagnosticKind.UrlImportRefPinRequiresLock -> DiagnosticCode.UrlImportRefPinRequiresLock
        | SimpleDiagnosticKind.UrlImportUnsupported -> DiagnosticCode.UrlImportUnsupported
        | SimpleDiagnosticKind.HostModuleReservedRoot -> DiagnosticCode.HostModuleReservedRoot
        | SimpleDiagnosticKind.HostModuleUnsupportedBackend -> DiagnosticCode.HostModuleUnsupportedBackend
        | SimpleDiagnosticKind.EffectRuntimeUnsupportedBackend -> DiagnosticCode.EffectRuntimeUnsupportedBackend
        | SimpleDiagnosticKind.MultishotEffectUnsupportedBackend -> DiagnosticCode.MultishotEffectUnsupportedBackend
        | SimpleDiagnosticKind.ModuleAttributeUnknown -> DiagnosticCode.ModuleAttributeUnknown
        | SimpleDiagnosticKind.ModuleHeaderMisplaced -> DiagnosticCode.ModuleHeaderMisplaced
        | SimpleDiagnosticKind.ModuleHeaderExpectedAfterAttributes -> DiagnosticCode.ModuleHeaderExpectedAfterAttributes
        | SimpleDiagnosticKind.ModulePathMismatch -> DiagnosticCode.ModulePathMismatch
        | SimpleDiagnosticKind.StaticObjectUnresolved -> DiagnosticCode.StaticObjectUnresolved
        | SimpleDiagnosticKind.PatternHeadNotConstructorOrActivePattern -> DiagnosticCode.PatternHeadNotConstructorOrActivePattern
        | SimpleDiagnosticKind.ActivePatternLinearityViolation -> DiagnosticCode.ActivePatternLinearityViolation
        | SimpleDiagnosticKind.ActivePatternMatchResultNotAllowedInPlainLetQuestion -> DiagnosticCode.ActivePatternMatchResultNotAllowedInPlainLetQuestion
        | SimpleDiagnosticKind.ActivePatternMissingScrutineeBinder -> DiagnosticCode.ActivePatternMissingScrutineeBinder
        | SimpleDiagnosticKind.ActivePatternMonadicResult -> DiagnosticCode.ActivePatternMonadicResult
        | SimpleDiagnosticKind.ProjectionCapabilityRequired -> DiagnosticCode.ProjectionCapabilityRequired
        | SimpleDiagnosticKind.ProjectionUpdateTargetUnsupported -> DiagnosticCode.ProjectionUpdateTargetUnsupported
        | SimpleDiagnosticKind.ProjectionRootInvalid -> DiagnosticCode.ProjectionRootInvalid
        | SimpleDiagnosticKind.ProjectionDescriptorRootMissing -> DiagnosticCode.ProjectionDescriptorRootMissing
        | SimpleDiagnosticKind.ProjectionRootsPackMismatch -> DiagnosticCode.ProjectionRootsPackMismatch
        | SimpleDiagnosticKind.ProjectionDescriptorRootsLiteralRequired -> DiagnosticCode.ProjectionDescriptorRootsLiteralRequired
        | SimpleDiagnosticKind.ProjectionDescriptorValueExpected -> DiagnosticCode.ProjectionDescriptorValueExpected
        | SimpleDiagnosticKind.ProjectionMissingPlaceBinder -> DiagnosticCode.ProjectionMissingPlaceBinder
        | SimpleDiagnosticKind.ProjectionYieldInvalid -> DiagnosticCode.ProjectionYieldInvalid
        | SimpleDiagnosticKind.ProjectionExpandedAccessorPlaceBinderMismatch -> DiagnosticCode.ProjectionExpandedAccessorPlaceBinderMismatch
        | SimpleDiagnosticKind.ProjectionAccessorClauseDuplicate -> DiagnosticCode.ProjectionAccessorClauseDuplicate
        | SimpleDiagnosticKind.TraitInstanceAmbiguous -> DiagnosticCode.TraitInstanceAmbiguous
        | SimpleDiagnosticKind.TraitSupertraitUnsatisfied -> DiagnosticCode.TraitSupertraitUnsatisfied
        | SimpleDiagnosticKind.HandlerEffectRowMismatch -> DiagnosticCode.HandlerEffectRowMismatch
        | SimpleDiagnosticKind.HandlerClauseMissing -> DiagnosticCode.HandlerClauseMissing
        | SimpleDiagnosticKind.HandlerClauseDuplicate -> DiagnosticCode.HandlerClauseDuplicate
        | SimpleDiagnosticKind.HandlerClauseArityMismatch -> DiagnosticCode.HandlerClauseArityMismatch
        | SimpleDiagnosticKind.HandlerClauseUnexpected -> DiagnosticCode.HandlerClauseUnexpected
        | SimpleDiagnosticKind.EffectResumptionQuantityBorrowed -> DiagnosticCode.EffectResumptionQuantityBorrowed
        | SimpleDiagnosticKind.DerivingShapeNotData -> DiagnosticCode.DerivingShapeNotData
        | SimpleDiagnosticKind.DerivingShapeNotClosedRecord -> DiagnosticCode.DerivingShapeNotClosedRecord
        | SimpleDiagnosticKind.DerivingShapeOpaqueRepresentation -> DiagnosticCode.DerivingShapeOpaqueRepresentation
        | SimpleDiagnosticKind.DerivingShapeUnsupportedType -> DiagnosticCode.DerivingShapeUnsupportedType
        | SimpleDiagnosticKind.DerivingShapeBadConstructorArguments -> DiagnosticCode.DerivingShapeBadConstructorArguments
        | SimpleDiagnosticKind.DerivingShapeBadRecordArguments -> DiagnosticCode.DerivingShapeBadRecordArguments
        | SimpleDiagnosticKind.DerivingShapeMissingRuntimeFieldInstance -> DiagnosticCode.DerivingShapeMissingRuntimeFieldInstance
        | SimpleDiagnosticKind.DerivingShapeDeclarationEffect -> DiagnosticCode.DerivingShapeDeclarationEffect
        | SimpleDiagnosticKind.NameUnresolved -> DiagnosticCode.NameUnresolved
        | SimpleDiagnosticKind.ModuleNameUnresolved -> DiagnosticCode.ModuleNameUnresolved
        | SimpleDiagnosticKind.RecursiveTypeAlias -> DiagnosticCode.RecursiveTypeAlias
        | SimpleDiagnosticKind.MalformedConstructorDeclaration -> DiagnosticCode.MalformedConstructorDeclaration
        | SimpleDiagnosticKind.RecursionRequiresSignature -> DiagnosticCode.RecursionRequiresSignature
        | SimpleDiagnosticKind.SignatureUnsatisfied -> DiagnosticCode.SignatureUnsatisfied
        | SimpleDiagnosticKind.OrPatternBinderMismatch -> DiagnosticCode.OrPatternBinderMismatch
        | SimpleDiagnosticKind.OrPatternBinderTypeMismatch -> DiagnosticCode.OrPatternBinderTypeMismatch
        | SimpleDiagnosticKind.SafeNavigationAmbiguous -> DiagnosticCode.SafeNavigationAmbiguous
        | SimpleDiagnosticKind.SafeNavigationReceiverNotOption -> DiagnosticCode.SafeNavigationReceiverNotOption
        | SimpleDiagnosticKind.ElvisReceiverNotOption -> DiagnosticCode.ElvisReceiverNotOption
        | SimpleDiagnosticKind.RecordDuplicateField -> DiagnosticCode.RecordDuplicateField
        | SimpleDiagnosticKind.RecordDependencyCycle -> DiagnosticCode.RecordDependencyCycle
        | SimpleDiagnosticKind.RecordDependencyInvalid -> DiagnosticCode.RecordDependencyInvalid
        | SimpleDiagnosticKind.RecordProjectionMissingField -> DiagnosticCode.RecordProjectionMissingField
        | SimpleDiagnosticKind.RecordPatchInvalidItem -> DiagnosticCode.RecordPatchInvalidItem
        | SimpleDiagnosticKind.RecordPatchDuplicatePath -> DiagnosticCode.RecordPatchDuplicatePath
        | SimpleDiagnosticKind.RecordPatchPrefixConflict -> DiagnosticCode.RecordPatchPrefixConflict
        | SimpleDiagnosticKind.RecordPatchUnknownPath -> DiagnosticCode.RecordPatchUnknownPath
        | SimpleDiagnosticKind.RowExtensionDuplicateLabel -> DiagnosticCode.RowExtensionDuplicateLabel
        | SimpleDiagnosticKind.RowExtensionExistingField -> DiagnosticCode.RowExtensionExistingField
        | SimpleDiagnosticKind.RowExtensionMissingLacksConstraint -> DiagnosticCode.RowExtensionMissingLacksConstraint
        | SimpleDiagnosticKind.ApplicationNonCallable -> DiagnosticCode.ApplicationNonCallable
        | SimpleDiagnosticKind.SealDirectLiteralForSignature -> DiagnosticCode.SealDirectLiteralForSignature
        | SimpleDiagnosticKind.SealOpenRecordAscription -> DiagnosticCode.SealOpenRecordAscription
        | SimpleDiagnosticKind.SealOpaqueUnfolding -> DiagnosticCode.SealOpaqueUnfolding
        | SimpleDiagnosticKind.TypeEqualityMismatch -> DiagnosticCode.TypeEqualityMismatch
        | SimpleDiagnosticKind.NumericLiteralOutOfRange -> DiagnosticCode.NumericLiteralOutOfRange
        | SimpleDiagnosticKind.UnicodeInvalidScalarLiteral -> DiagnosticCode.UnicodeInvalidScalarLiteral
        | SimpleDiagnosticKind.UnicodeInvalidGraphemeLiteral -> DiagnosticCode.UnicodeInvalidGraphemeLiteral
        | SimpleDiagnosticKind.UnicodeInvalidByteLiteral -> DiagnosticCode.UnicodeInvalidByteLiteral
        | SimpleDiagnosticKind.UnicodeInvalidUtf8 -> DiagnosticCode.UnicodeInvalidUtf8
        | SimpleDiagnosticKind.UnicodeBidiControl -> DiagnosticCode.UnicodeBidiControl
        | SimpleDiagnosticKind.UnicodeConfusableIdentifier -> DiagnosticCode.UnicodeConfusableIdentifier
        | SimpleDiagnosticKind.UnicodeNonNormalizedSourceText -> DiagnosticCode.UnicodeNonNormalizedSourceText
        | SimpleDiagnosticKind.UnexpectedIndentation -> DiagnosticCode.UnexpectedIndentation
        | SimpleDiagnosticKind.UnterminatedBacktickIdentifier -> DiagnosticCode.UnterminatedBacktickIdentifier
        | SimpleDiagnosticKind.UnterminatedStringLiteral -> DiagnosticCode.UnterminatedStringLiteral
        | SimpleDiagnosticKind.UnterminatedCharacterLiteral -> DiagnosticCode.UnterminatedCharacterLiteral
        | SimpleDiagnosticKind.UnterminatedBlockComment -> DiagnosticCode.UnterminatedBlockComment
        | SimpleDiagnosticKind.QttLinearDrop -> DiagnosticCode.QttLinearDrop
        | SimpleDiagnosticKind.QttLinearOveruse -> DiagnosticCode.QttLinearOveruse
        | SimpleDiagnosticKind.QttBorrowConsume -> DiagnosticCode.QttBorrowConsume
        | SimpleDiagnosticKind.QttBorrowOverlap -> DiagnosticCode.QttBorrowOverlap
        | SimpleDiagnosticKind.QttBorrowEscape -> DiagnosticCode.QttBorrowEscape
        | SimpleDiagnosticKind.QttContinuationCapture -> DiagnosticCode.QttContinuationCapture
        | SimpleDiagnosticKind.QttErasedRuntimeUse -> DiagnosticCode.QttErasedRuntimeUse
        | SimpleDiagnosticKind.QttUsingExplicitQuantity -> DiagnosticCode.QttUsingExplicitQuantity
        | SimpleDiagnosticKind.QttInoutMarkerRequired -> DiagnosticCode.QttInoutMarkerRequired
        | SimpleDiagnosticKind.QttInoutMarkerUnexpected -> DiagnosticCode.QttInoutMarkerUnexpected
        | SimpleDiagnosticKind.QttInoutThreadedFieldMissing -> DiagnosticCode.QttInoutThreadedFieldMissing
        | SimpleDiagnosticKind.ControlFlowInvalidEscape -> DiagnosticCode.ControlFlowInvalidEscape

    let private simplePayloadKind kind =
        simpleCode kind
        |> DiagnosticCode.toIdentifier
        |> fun identifier ->
            let trimmed =
                if identifier.Length > 2 && identifier[1] = '_' then
                    identifier.Substring(2)
                else
                    identifier

            trimmed.ToLowerInvariant().Replace('_', '-')

    let simple kind detail =
        SimpleDiagnostic
            { Kind = kind
              Detail = detail }

    let codeDetail code detail =
        CodeDetailDiagnostic
            { Code = code
              Detail = detail }

    let macroExpansion evidence =
        MacroExpansionDiagnostic evidence

    let numericLiteralOutOfRange literalText =
        NumericLiteralOutOfRangeDiagnostic { LiteralText = literalText }

    let nameUnresolved spelling =
        NameUnresolvedDiagnostic
            { Spelling = spelling
              ReferenceKind = OrdinaryNameReference
              AdmissibleKinds = []
              SearchedScopes = []
              GeneratedSyntax = false }

    let moduleQualifierUnresolved spelling =
        NameUnresolvedDiagnostic
            { Spelling = spelling
              ReferenceKind = ModuleQualifierReference
              AdmissibleKinds = []
              SearchedScopes = []
              GeneratedSyntax = false }

    let nameAmbiguous spelling candidateDescriptions =
        NameAmbiguousDiagnostic
            { Spelling = spelling
              CandidateDescriptions = candidateDescriptions }

    let duplicatePatternBinder binderName =
        DuplicatePatternBinderDiagnostic { BinderName = binderName }

    let duplicateDeclaration detail moduleName =
        DuplicateDeclarationDiagnostic
            { Detail = detail
              ModuleName = moduleName }

    let importCycle modulePath =
        ImportCycleDiagnostic { ModulePath = modulePath }

    let moduleCaseFoldCollision collidingPaths =
        ModuleCaseFoldCollisionDiagnostic { CollidingPaths = collidingPaths }

    let invalidUtf8Source detail =
        InvalidUtf8SourceDiagnostic { Detail = detail }

    let moduleNameUnresolved filePath =
        ModuleNameUnresolvedDiagnostic { FilePath = filePath }

    let hostModuleReservedRoot moduleName =
        HostModuleReservedRootDiagnostic { ModuleName = moduleName }

    let modulePathMismatch declaredModuleName inferredModuleName =
        ModulePathMismatchDiagnostic
            { DeclaredModuleName = declaredModuleName
              InferredModuleName = inferredModuleName }

    let urlImportUnsupported specifierText =
        UrlImportUnsupportedDiagnostic { SpecifierText = specifierText }

    let hostModuleUnsupportedBackend backendProfile hostBindingRoot =
        HostModuleUnsupportedBackendDiagnostic
            { BackendProfile = backendProfile
              HostBindingRoot = hostBindingRoot }

    let importAmbiguous fullModuleName parentModuleName itemName =
        ImportAmbiguousDiagnostic
            { FullModuleName = fullModuleName
              ParentModuleName = parentModuleName
              ItemName = itemName }

    let bareImportTargetNotFound fullModuleName parentModuleName itemName =
        ImportModuleNameUnresolvedDiagnostic(BareImportTargetNotFound(fullModuleName, parentModuleName, itemName))

    let importedModuleNotFound importedModuleName =
        ImportModuleNameUnresolvedDiagnostic(ImportedModuleNotFound importedModuleName)

    let importModifierRequiresBuildSetting itemText requiredSetting =
        ImportModifierBuildSettingDiagnostic
            { ItemText = itemText
              RequiredSetting = requiredSetting }

    let importItemModifierReexportForbidden itemText =
        ImportItemModifierReexportForbiddenDiagnostic { ItemText = itemText }

    let importItemNotFound itemName importedModuleName lookupContext =
        ImportItemNotFoundDiagnostic
            { ItemName = itemName
              ImportedModuleName = importedModuleName
              LookupContext = lookupContext }

    let expectUnsatisfied expectedKind expectedName =
        ExpectUnsatisfiedDiagnostic
            { ExpectedKind = expectedKind
              ExpectedName = expectedName }

    let expectAmbiguous expectedKind expectedName =
        ExpectAmbiguousDiagnostic
            { ExpectedKind = expectedKind
              ExpectedName = expectedName }

    let signatureUnsatisfied signatureName signatureTypeText =
        SignatureUnsatisfiedDiagnostic
            { SignatureName = signatureName
              SignatureTypeText = signatureTypeText }

    let urlImportUnpinnedInPackageMode specifierText =
        UrlImportPackageModeDiagnostic(UrlImportUnpinnedInPackageMode specifierText)

    let urlImportRefPinRequiresLock specifierText =
        UrlImportPackageModeDiagnostic(UrlImportRefPinRequiresLock specifierText)

    let reflRequiresDefinitionallyEqualSides =
        TypeEqualityMismatchDiagnostic ReflRequiresDefinitionallyEqualSides

    let expectedActualTypeMismatch context expectedText actualText mismatchKind reason =
        TypeEqualityMismatchDiagnostic(
            TypeEqualityMismatchEvidence.ExpectedActualTypeMismatch(
                context,
                expectedText,
                actualText,
                mismatchKind,
                reason
            )
        )

    let typechecking evidence =
        TypecheckingDiagnostic evidence

    let backend evidence =
        BackendDiagnostic evidence

    let targetCheckpoint evidence =
        TargetCheckpointDiagnostic evidence

    let checkpointVerification evidence =
        CheckpointVerificationDiagnostic evidence

    let surfaceRecord evidence =
        SurfaceRecordDiagnostic evidence

    let surfaceElaboration evidence =
        SurfaceElaborationDiagnostic evidence

    let parserSyntax evidence =
        ParserSyntaxDiagnostic evidence

    let corePatternParsing evidence =
        CorePatternParsingDiagnostic evidence

    let private expectedActualMismatchKindText mismatchKind =
        match mismatchKind with
        | ExpressionTypeMismatch -> "expression-type-mismatch"
        | ApplicationArgumentMismatch -> "application-argument-mismatch"
        | FunctionResultMismatch -> "function-result-mismatch"
        | RecordFieldMismatch -> "record-field-mismatch"
        | RecordUpdateFieldMismatch -> "record-update-field-mismatch"

    let private expectedActualMismatchReasonText reason =
        match reason with
        | DependentEqualityTransportRequired -> "dependent-equality-transport-required"
        | FunctionQuantityIncompatible -> "function-quantity-incompatible"
        | UnionCompatibilityFailed -> "union-compatibility-failed"
        | SuspensionCompatibilityFailed -> "suspension-compatibility-failed"
        | GeneralExpectedActualMismatch -> "general-expected-actual-mismatch"

    let private expectedActualMismatchFamily mismatchKind reason =
        match reason, mismatchKind with
        | DependentEqualityTransportRequired, _ -> Some "kappa.type.transport-failed"
        | _, ApplicationArgumentMismatch -> Some "kappa.application.argument-mismatch"
        | _ -> Some "kappa.type.mismatch"

    let private expectedActualMismatchMessage context expectedText actualText reason =
        match reason with
        | DependentEqualityTransportRequired ->
            $"{context} requires equality transport evidence for dependent expected type '{expectedText}', but the argument has type '{actualText}'."
        | FunctionQuantityIncompatible ->
            $"{context} has incompatible function quantity: expected '{expectedText}' but found '{actualText}'."
        | UnionCompatibilityFailed ->
            $"{context} does not match the expected union type '{expectedText}'; no valid union injection or widening applies from '{actualText}'."
        | SuspensionCompatibilityFailed ->
            $"{context} does not match the expected suspension type '{expectedText}' from expression type '{actualText}'."
        | GeneralExpectedActualMismatch ->
            $"{context} type mismatch: expected '{expectedText}' but found '{actualText}'."

    let coreExpressionParsing evidence =
        CoreExpressionParsingDiagnostic evidence

    let unicodeInvalidScalarLiteral evidence =
        UnicodeScalarLiteralDiagnostic evidence

    let unicodeInvalidGraphemeLiteral evidence =
        UnicodeGraphemeLiteralDiagnostic evidence

    let unicodeInvalidByteLiteral evidence =
        UnicodeByteLiteralDiagnostic evidence

    let lexer evidence =
        LexerDiagnostic evidence

    let qttLinearDrop evidence = QttLinearDropDiagnostic evidence
    let qttLinearOveruse evidence = QttLinearOveruseDiagnostic evidence
    let qttBorrowConsume evidence = QttBorrowConsumeDiagnostic evidence
    let qttBorrowOverlap evidence = QttBorrowOverlapDiagnostic evidence
    let qttBorrowEscape evidence = QttBorrowEscapeDiagnostic evidence
    let qttErasedRuntimeUse evidence = QttErasedRuntimeUseDiagnostic evidence
    let qttUsingExplicitQuantity evidence = QttUsingExplicitQuantityDiagnostic evidence
    let qttInoutMarkerRequired evidence = QttInoutMarkerRequiredDiagnostic evidence
    let qttInoutMarkerUnexpected evidence = QttInoutMarkerUnexpectedDiagnostic evidence
    let qttInoutThreadedFieldMissing evidence = QttInoutThreadedFieldMissingDiagnostic evidence

    let private qttCardinalityEffectText effect =
        match effect with
        | Discard -> "discard"
        | Duplicate -> "duplicate"
        | DuplicateOrDiscard -> "duplicate or discard"
        | Reorder -> "reorder"

    let private importBuildSettingText setting =
        match setting with
        | AllowUnhiding -> "allow_unhiding"
        | AllowClarify -> "allow_clarify"

    let private corePureBlockContextPayloadText context =
        match context with
        | BlockExpressionBody -> "block-expression"
        | LambdaPureBody -> "lambda-pure-body"

    let private expectDeclarationKindText kind =
        match kind with
        | ExpectedTypeDeclaration -> "type"
        | ExpectedTraitDeclaration -> "trait"
        | ExpectedTermDeclaration -> "term"

    let private parserNameRoleText role =
        match role with
        | BindingName -> "binding name"
        | DataTypeName -> "data type name"
        | ModuleName -> "module name"
        | ModuleNameSegment -> "module name segment"
        | ModuleAttributeName -> "module attribute name"
        | NameInList -> "name in the list"
        | NameInSignatureDeclaration -> "name in the signature declaration"
        | ProjectionName -> "projection name"
        | TypeAliasName -> "type alias name"
        | TraitNameAfterExpectTrait -> "trait name after 'expect trait'"
        | TypeNameAfterExpectType -> "type name after 'expect type'"
        | TermNameAfterExpectTerm -> "term name after 'expect term'"
        | EffectName -> "effect name"
        | ImportedName -> "imported name"
        | ExcludedImportName -> "excluded import name"
        | AliasAfterAs -> "alias after 'as'"

    let private parserListContextText context =
        match context with
        | NameList -> "list"
        | ExclusionList -> "exclusion list"
        | ImportItemList -> "import item list"

    let private parserDeclarationSubjectText subject =
        match subject with
        | ParserEffectDeclarationSubject -> "effect declaration"
        | ParserInstanceDeclarationSubject -> "instance declaration"
        | ParserInstanceMemberDeclarationSubject -> "instance member declaration"
        | ParserProjectionDeclarationSubject -> "projection declaration"
        | ParserNamedDeclarationSubject keywordText -> $"{keywordText} declaration"

    let private parserDeclarationBlockDelimiterText delimiter =
        match delimiter with
        | ParserRightParenthesisDelimiter -> "')'"
        | ParserRightBraceDelimiter -> "'}'"
        | ParserRightBracketDelimiter -> "']'"
        | ParserRightSetBraceDelimiter -> "'>}'"
        | ParserDedentDelimiter -> "the declaration block to dedent"

    let private parserModifierTargetText target =
        match target with
        | ProjectionDeclarationTarget -> "projection declarations"
        | DataDeclarationTarget -> "data declarations"
        | EffectDeclarationTarget -> "effect declarations"
        | TraitDeclarationTarget -> "trait declarations"
        | SignatureDeclarationTarget -> "signature declarations"
        | InstanceDeclarationTarget -> "instance declarations"
        | TermDeclarationsOnly -> "term declarations"

    let private coreHeaderContextText context =
        match context with
        | TopLevelFunctionHeader -> "function header"
        | LocalFunctionHeader -> "local function header"

    let private coreHeaderContextPayloadText context =
        match context with
        | TopLevelFunctionHeader -> "function-header"
        | LocalFunctionHeader -> "local-function-header"

    let private stringLiteralDecodeErrorTag error =
        match error with
        | UnknownEscapeSequence _ -> "unknown-escape-sequence"
        | InvalidUnicodeEscape _ -> "invalid-unicode-escape"
        | UnterminatedEscapeSequence -> "unterminated-escape-sequence"
        | UnterminatedUnicodeEscapeSequence -> "unterminated-unicode-escape-sequence"
        | InvalidMultilineClosingDelimiterIndentation -> "invalid-multiline-closing-delimiter-indentation"
        | MultilineContentIndentationMismatch -> "multiline-content-indentation-mismatch"
        | InvalidRawMultilineStringLiteral -> "invalid-raw-multiline-string-literal"
        | InvalidRawStringLiteral -> "invalid-raw-string-literal"

    let private stringLiteralDecodeErrorMessage error =
        match error with
        | UnknownEscapeSequence escapeText -> $"unknown escape sequence '{escapeText}'"
        | InvalidUnicodeEscape escapeText -> $"invalid Unicode escape '{escapeText}'"
        | UnterminatedEscapeSequence -> "unterminated escape sequence"
        | UnterminatedUnicodeEscapeSequence -> "unterminated Unicode escape sequence"
        | InvalidMultilineClosingDelimiterIndentation -> "invalid multiline string closing delimiter indentation"
        | MultilineContentIndentationMismatch -> "a multiline string content line does not match the closing delimiter indentation"
        | InvalidRawMultilineStringLiteral -> "invalid raw multiline string literal"
        | InvalidRawStringLiteral -> "invalid raw string literal"

    let private stringLiteralDecodeErrorFields error =
        let baseFields =
            [ field "string-literal-error" (DiagnosticPayloadText(stringLiteralDecodeErrorTag error)) ]

        match error with
        | UnknownEscapeSequence escapeText
        | InvalidUnicodeEscape escapeText ->
            baseFields @ [ field "escape-text" (DiagnosticPayloadText escapeText) ]
        | _ ->
            baseFields

    let private urlModuleSpecifierParseErrorTag error =
        match error with
        | MissingBaseUrl -> "missing-base-url"
        | EmptyPin -> "empty-pin"
        | MissingSha256Digest -> "missing-sha256-digest"
        | InvalidSha256Digest _ -> "invalid-sha256-digest"
        | EmptyRefPin -> "empty-ref-pin"
        | UnsupportedPin _ -> "unsupported-pin"

    let private urlModuleSpecifierParseErrorMessage error =
        match error with
        | MissingBaseUrl -> "the base URL is empty"
        | EmptyPin -> "the URL pin is empty"
        | MissingSha256Digest -> "sha256 pins must include a hexadecimal digest"
        | InvalidSha256Digest _ -> "sha256 pins must use a hexadecimal digest"
        | EmptyRefPin -> "ref pins must include a non-empty reference"
        | UnsupportedPin _ -> "the pin must use 'sha256:<hex>' or 'ref:<text>'"

    let private urlModuleSpecifierParseErrorFields error =
        let baseFields =
            [ field "url-parse-error" (DiagnosticPayloadText(urlModuleSpecifierParseErrorTag error)) ]

        match error with
        | InvalidSha256Digest pinText
        | UnsupportedPin pinText ->
            baseFields @ [ field "pin-text" (DiagnosticPayloadText pinText) ]
        | _ ->
            baseFields

    let private numericLiteralParseErrorTag error =
        match error with
        | InvalidNumericLiteral _ -> "invalid-numeric-literal"

    let private prefixedIntegerBaseText integerBase =
        match integerBase with
        | BinaryBase -> "binary"
        | OctalBase -> "octal"
        | HexadecimalBase -> "hexadecimal"

    let private surfaceRecordContextText context =
        match context with
        | RecordTypeTelescope -> "record type"
        | RecordLiteralFields -> "record literal"

    let private surfaceRecordContextPayloadText context =
        match context with
        | RecordTypeTelescope -> "record-type-telescope"
        | RecordLiteralFields -> "record-literal-fields"

    let private numericLiteralParseErrorText error =
        match error with
        | InvalidNumericLiteral tokenText -> tokenText

    let private unicodeScalarLiteralEvidenceTag evidence =
        match evidence with
        | UnicodeScalarInvalidLiteralForm -> "invalid-literal-form"
        | UnicodeScalarTextInvalid error -> stringLiteralDecodeErrorTag error
        | UnicodeScalarMustDecodeToExactlyOneScalar -> "must-decode-to-exactly-one-scalar"

    let private unicodeGraphemeLiteralEvidenceTag evidence =
        match evidence with
        | GraphemeInvalidLiteralForm -> "invalid-literal-form"
        | GraphemeTextInvalid error -> stringLiteralDecodeErrorTag error
        | GraphemeMustDecodeToExactlyOneExtendedCluster -> "must-decode-to-exactly-one-extended-cluster"

    let private unicodeByteLiteralEvidenceTag evidence =
        match evidence with
        | ByteInvalidLiteralForm -> "invalid-literal-form"
        | ByteInvalidEscape _ -> "invalid-byte-escape"
        | ByteInvalidUnicodeEscape _ -> "invalid-unicode-escape"
        | ByteUnknownEscapeSequence _ -> "unknown-escape-sequence"
        | ByteUnterminatedEscapeSequence -> "unterminated-escape-sequence"
        | ByteUnterminatedUnicodeEscapeSequence -> "unterminated-unicode-escape-sequence"
        | ByteMustDecodeToExactlyOneByte -> "must-decode-to-exactly-one-byte"

    let describe fact =
        match fact with
        | SimpleDiagnostic evidence ->
            let code = simpleCode evidence.Kind

            descriptor
                code
                None
                evidence.Detail
                (payload
                    (simplePayloadKind evidence.Kind)
                    [ field "detail" (DiagnosticPayloadText evidence.Detail) ])
        | CodeDetailDiagnostic evidence ->
            descriptor
                evidence.Code
                None
                evidence.Detail
                (payload
                    (let identifier = DiagnosticCode.toIdentifier evidence.Code
                     let trimmed =
                         if identifier.Length > 2 && identifier[1] = '_' then
                             identifier.Substring(2)
                         else
                             identifier

                     trimmed.ToLowerInvariant().Replace('_', '-'))
                    [ field "detail" (DiagnosticPayloadText evidence.Detail) ])
        | MacroExpansionDiagnostic evidence ->
            let descriptorFor code reason message =
                descriptor
                    code
                    None
                    message
                    (payload
                        "macro-expansion-diagnostic"
                        [ field "reason" (DiagnosticPayloadText reason)
                          field "message" (DiagnosticPayloadText message) ])

            match evidence with
            | FailElabMessage message ->
                descriptorFor DiagnosticCode.ElaborationFailed "fail-elab" message
            | WarnElabMessage message ->
                descriptorFor DiagnosticCode.SourceWarning "warn-elab" message
            | SyntaxValueEscapesCapturedBindings capturedNames ->
                descriptor
                    DiagnosticCode.ElaborationFailed
                    None
                    "Syntax value escapes the scope where one or more captured local binders are available."
                    (payload
                        "macro-expansion-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "syntax-value-escapes-captured-bindings")
                          field "captured-names" (DiagnosticPayloadTextList capturedNames) ])
            | FailElabWithCode(code, message) ->
                descriptor
                    code
                    None
                    message
                    (payload
                        "macro-expansion-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "fail-elab-with")
                          field "message" (DiagnosticPayloadText message)
                          field "diagnostic-code" (DiagnosticPayloadText(DiagnosticCode.toIdentifier code)) ])
            | WarnElabWithCode(code, message) ->
                descriptor
                    code
                    None
                    message
                    (payload
                        "macro-expansion-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "warn-elab-with")
                          field "message" (DiagnosticPayloadText message)
                          field "diagnostic-code" (DiagnosticPayloadText(DiagnosticCode.toIdentifier code)) ])
        | NumericLiteralOutOfRangeDiagnostic evidence ->
            descriptor
                DiagnosticCode.NumericLiteralOutOfRange
                None
                $"Numeric literal '{evidence.LiteralText}' is outside the currently supported runtime range for its target type."
                (payload
                    "numeric-literal-out-of-range"
                    [ field "literal-text" (DiagnosticPayloadText evidence.LiteralText) ])
        | NameUnresolvedDiagnostic evidence ->
            let referenceKindText, message =
                match evidence.ReferenceKind with
                | OrdinaryNameReference -> "ordinary-name", $"Name '{evidence.Spelling}' is not in scope."
                | ModuleQualifierReference -> "module-qualifier", $"Module qualifier '{evidence.Spelling}' is not in scope."

            descriptor
                DiagnosticCode.NameUnresolved
                (Some "kappa.name.unresolved")
                message
                (payload
                    "name-unresolved"
                    [ field "spelling" (DiagnosticPayloadText evidence.Spelling)
                      field "reference-kind" (DiagnosticPayloadText referenceKindText)
                      field "admissible-kinds" (DiagnosticPayloadTextList evidence.AdmissibleKinds)
                      field "searched-scopes" (DiagnosticPayloadTextList evidence.SearchedScopes)
                      field "generated-syntax" (DiagnosticPayloadText(string evidence.GeneratedSyntax)) ])
        | NameAmbiguousDiagnostic evidence ->
            let message =
                match evidence.CandidateDescriptions with
                | [] ->
                    $"Name '{evidence.Spelling}' is ambiguous."
                | candidates ->
                    let candidateText = String.concat ", " candidates
                    $"Name '{evidence.Spelling}' is ambiguous between {candidateText}. Use an explicit import item, module alias, or qualification."

            descriptor
                DiagnosticCode.NameAmbiguous
                (Some "kappa.name.ambiguous")
                message
                (payload
                    "name-ambiguous"
                    [ field "spelling" (DiagnosticPayloadText evidence.Spelling)
                      field "candidates" (DiagnosticPayloadTextList evidence.CandidateDescriptions) ])
        | DuplicatePatternBinderDiagnostic evidence ->
            descriptor
                DiagnosticCode.DuplicatePatternBinder
                None
                $"Pattern binder '{evidence.BinderName}' is bound more than once in the same pattern."
                (payload "duplicate-pattern-binder" [ field "binder" (DiagnosticPayloadText evidence.BinderName) ])
        | DuplicateDeclarationDiagnostic evidence ->
            let message =
                match evidence.ModuleName with
                | Some moduleName -> $"{evidence.Detail} in module '{moduleName}'."
                | None -> evidence.Detail

            descriptor
                DiagnosticCode.DuplicateDeclaration
                None
                message
                (payload
                    "duplicate-declaration"
                    [ field "detail" (DiagnosticPayloadText evidence.Detail)
                      field "module" (DiagnosticPayloadText(evidence.ModuleName |> Option.defaultValue "")) ])
        | ImportCycleDiagnostic evidence ->
            let cycleText = String.concat " -> " evidence.ModulePath

            descriptor
                DiagnosticCode.ImportCycle
                None
                $"Import cycle detected: {cycleText}."
                (payload "import-cycle" [ field "modules" (DiagnosticPayloadTextList evidence.ModulePath) ])
        | ModuleCaseFoldCollisionDiagnostic evidence ->
            let collidingPathsText = String.concat ", " evidence.CollidingPaths

            descriptor
                DiagnosticCode.ModuleCaseFoldCollision
                None
                $"Module names that differ only by case after ASCII case-folding are not permitted in one compilation unit: {collidingPathsText}."
                (payload
                    "module-case-fold-collision"
                    [ field "paths" (DiagnosticPayloadTextList evidence.CollidingPaths) ])
        | InvalidUtf8SourceDiagnostic evidence ->
            descriptor
                DiagnosticCode.UnicodeInvalidUtf8
                None
                $"Source file is not valid UTF-8: {evidence.Detail}"
                (payload "invalid-utf8-source" [ field "detail" (DiagnosticPayloadText evidence.Detail) ])
        | ModuleNameUnresolvedDiagnostic evidence ->
            descriptor
                DiagnosticCode.ModuleNameUnresolved
                (Some "kappa.name.unresolved")
                $"Could not derive a Kappa module name from '{evidence.FilePath}'. Source files must live under the source root, end in the exact '.kp' suffix, and every directory, basename, and fragment segment must match [A-Za-z_][A-Za-z0-9_]*."
                (payload "module-name-unresolved" [ field "file-path" (DiagnosticPayloadText evidence.FilePath) ])
        | HostModuleReservedRootDiagnostic evidence ->
            descriptor
                DiagnosticCode.HostModuleReservedRoot
                None
                $"Source-defined module '{evidence.ModuleName}' uses a reserved host binding root. Host binding modules are supplied from host metadata, not user-written Kappa source."
                (payload
                    "host-module-reserved-root"
                    [ field "module-name" (DiagnosticPayloadText evidence.ModuleName) ])
        | ModulePathMismatchDiagnostic evidence ->
            descriptor
                DiagnosticCode.ModulePathMismatch
                None
                $"Module header '{evidence.DeclaredModuleName}' does not match the path-derived module name '{evidence.InferredModuleName}'."
                (payload
                    "module-path-mismatch"
                    [ field "declared-module-name" (DiagnosticPayloadText evidence.DeclaredModuleName)
                      field "inferred-module-name" (DiagnosticPayloadText evidence.InferredModuleName) ])
        | UrlImportUnsupportedDiagnostic evidence ->
            descriptor
                DiagnosticCode.UrlImportUnsupported
                None
                $"URL module specifier '{evidence.SpecifierText}' is not supported by this toolchain. URL imports and exports require fetch/cache/lock resolution, which is not implemented."
                (payload
                    "url-import-unsupported"
                    [ field "specifier-text" (DiagnosticPayloadText evidence.SpecifierText) ])
        | HostModuleUnsupportedBackendDiagnostic evidence ->
            descriptor
                DiagnosticCode.HostModuleUnsupportedBackend
                None
                $"Backend profile '{evidence.BackendProfile}' does not provide host binding root '{evidence.HostBindingRoot}'."
                (payload
                    "host-module-unsupported-backend"
                    [ field "backend-profile" (DiagnosticPayloadText evidence.BackendProfile)
                      field "host-binding-root" (DiagnosticPayloadText evidence.HostBindingRoot) ])
        | ImportAmbiguousDiagnostic evidence ->
            descriptor
                DiagnosticCode.ImportAmbiguous
                (Some "kappa.import.ambiguous-dotted")
                $"Bare dotted import/export '{evidence.FullModuleName}' is ambiguous between module '{evidence.FullModuleName}' and item '{evidence.ItemName}' from module '{evidence.ParentModuleName}'. Use an explicit module-only form or '(...)' singleton syntax."
                (payload
                    "import-ambiguous"
                    [ field "full-module-name" (DiagnosticPayloadText evidence.FullModuleName)
                      field "parent-module-name" (DiagnosticPayloadText evidence.ParentModuleName)
                      field "item-name" (DiagnosticPayloadText evidence.ItemName) ])
        | ImportModuleNameUnresolvedDiagnostic evidence ->
            match evidence with
            | BareImportTargetNotFound(fullModuleName, parentModuleName, itemName) ->
                descriptor
                    DiagnosticCode.ModuleNameUnresolved
                    (Some "kappa.name.unresolved")
                    $"Neither module '{fullModuleName}' nor item '{itemName}' from module '{parentModuleName}' was found."
                    (payload
                        "import-module-name-unresolved"
                        [ field "reason" (DiagnosticPayloadText "bare-import-target-not-found")
                          field "full-module-name" (DiagnosticPayloadText fullModuleName)
                          field "parent-module-name" (DiagnosticPayloadText parentModuleName)
                          field "item-name" (DiagnosticPayloadText itemName) ])
            | ImportedModuleNotFound importedModuleName ->
                descriptor
                    DiagnosticCode.ModuleNameUnresolved
                    (Some "kappa.name.unresolved")
                    $"Imported module '{importedModuleName}' was not found."
                    (payload
                        "import-module-name-unresolved"
                        [ field "reason" (DiagnosticPayloadText "imported-module-not-found")
                          field "imported-module-name" (DiagnosticPayloadText importedModuleName) ])
        | ImportModifierBuildSettingDiagnostic evidence ->
            let buildSetting = importBuildSettingText evidence.RequiredSetting

            let code =
                match evidence.RequiredSetting with
                | AllowUnhiding -> DiagnosticCode.ImportUnhideRequiresBuildSetting
                | AllowClarify -> DiagnosticCode.ImportClarifyRequiresBuildSetting

            descriptor
                code
                None
                $"Import item '{evidence.ItemText}' requires build setting '{buildSetting}', which is disabled in package mode."
                (payload
                    "import-modifier-build-setting"
                    [ field "item-text" (DiagnosticPayloadText evidence.ItemText)
                      field "required-setting" (DiagnosticPayloadText buildSetting) ])
        | ImportItemModifierReexportForbiddenDiagnostic evidence ->
            descriptor
                DiagnosticCode.ImportItemModifierReexportForbidden
                None
                $"Import item '{evidence.ItemText}' uses unhide/clarify and must not be re-exported."
                (payload
                    "import-item-modifier-reexport-forbidden"
                    [ field "item-text" (DiagnosticPayloadText evidence.ItemText) ])
        | ImportItemNotFoundDiagnostic evidence ->
            let messagePrefix =
                match evidence.LookupContext with
                | IncludedImportItem -> "Import item"
                | ExcludedImportItem -> "Excluded import item"

            let lookupContext =
                match evidence.LookupContext with
                | IncludedImportItem -> "included-import-item"
                | ExcludedImportItem -> "excluded-import-item"

            descriptor
                DiagnosticCode.ImportItemNotFound
                None
                $"{messagePrefix} '{evidence.ItemName}' was not found in module '{evidence.ImportedModuleName}'."
                (payload
                    "import-item-not-found"
                    [ field "item-name" (DiagnosticPayloadText evidence.ItemName)
                      field "imported-module-name" (DiagnosticPayloadText evidence.ImportedModuleName)
                      field "lookup-context" (DiagnosticPayloadText lookupContext) ])
        | ExpectUnsatisfiedDiagnostic evidence ->
            let expectedKind = expectDeclarationKindText evidence.ExpectedKind

            descriptor
                DiagnosticCode.ExpectUnsatisfied
                None
                $"Unsatisfied expect declaration for {expectedKind} '{evidence.ExpectedName}'."
                (payload
                    "expect-unsatisfied"
                    [ field "expected-kind" (DiagnosticPayloadText expectedKind)
                      field "expected-name" (DiagnosticPayloadText evidence.ExpectedName) ])
        | ExpectAmbiguousDiagnostic evidence ->
            let expectedKind = expectDeclarationKindText evidence.ExpectedKind

            descriptor
                DiagnosticCode.ExpectAmbiguous
                None
                $"Multiple satisfactions were found for expected {expectedKind} '{evidence.ExpectedName}'."
                (payload
                    "expect-ambiguous"
                    [ field "expected-kind" (DiagnosticPayloadText expectedKind)
                      field "expected-name" (DiagnosticPayloadText evidence.ExpectedName) ])
        | SignatureUnsatisfiedDiagnostic evidence ->
            descriptor
                DiagnosticCode.SignatureUnsatisfied
                None
                $"Top-level signature '{evidence.SignatureName} : {evidence.SignatureTypeText}' has no matching definition in the same source file. Define it with 'let {evidence.SignatureName} = ...' or declare it as 'expect term {evidence.SignatureName} : {evidence.SignatureTypeText}'."
                (payload
                    "signature-unsatisfied"
                    [ field "signature-name" (DiagnosticPayloadText evidence.SignatureName)
                      field "signature-type-text" (DiagnosticPayloadText evidence.SignatureTypeText) ])
        | UrlImportPackageModeDiagnostic evidence ->
            match evidence with
            | UrlImportUnpinnedInPackageMode specifierText ->
                descriptor
                    DiagnosticCode.UrlImportUnpinnedInPackageMode
                    None
                    $"URL import '{specifierText}' is unpinned. Package mode requires pinned URL imports."
                    (payload
                        "url-import-package-mode"
                        [ field "reason" (DiagnosticPayloadText "url-import-unpinned-in-package-mode")
                          field "specifier-text" (DiagnosticPayloadText specifierText) ])
            | UrlImportRefPinRequiresLock specifierText ->
                descriptor
                    DiagnosticCode.UrlImportRefPinRequiresLock
                    None
                    $"URL import '{specifierText}' uses a ref pin, but this toolchain has no recorded immutable resolution for it in package mode."
                    (payload
                        "url-import-package-mode"
                        [ field "reason" (DiagnosticPayloadText "url-import-ref-pin-requires-lock")
                          field "specifier-text" (DiagnosticPayloadText specifierText) ])
        | TypeEqualityMismatchDiagnostic evidence ->
            match evidence with
            | ReflRequiresDefinitionallyEqualSides ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "The proof term 'refl' requires both sides of the equality type to be definitionally equal. Function binder quantities are part of type identity."
                    (payload
                        "type-equality-mismatch"
                        [ field "reason" (DiagnosticPayloadText "refl-requires-definitionally-equal-sides") ])
            | ExpectedActualTypeMismatch(context, expectedText, actualText, mismatchKind, reason) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    (expectedActualMismatchFamily mismatchKind reason)
                    (expectedActualMismatchMessage context expectedText actualText reason)
                    (payload
                        "expected-actual"
                        [ field "reason" (DiagnosticPayloadText (expectedActualMismatchReasonText reason))
                          field "context" (DiagnosticPayloadText context)
                          field "expected" (DiagnosticPayloadText expectedText)
                          field "actual" (DiagnosticPayloadText actualText)
                          field "mismatch-kind" (DiagnosticPayloadText (expectedActualMismatchKindText mismatchKind)) ])
        | TypecheckingDiagnostic evidence ->
            match evidence with
            | ProjectionCapabilityRequiredAtSite capability ->
                descriptor
                    DiagnosticCode.ProjectionCapabilityRequired
                    None
                    $"Projection/accessor use requires the '{capability}' capability at this site."
                    (payload
                        "typechecking-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "projection-capability-required-at-site")
                          field "capability" (DiagnosticPayloadText capability) ])
            | ProjectionRootRequiresStablePlace ->
                descriptor
                    DiagnosticCode.ProjectionRootInvalid
                    None
                    "Projection place arguments must be stable places or computed selector places with stable roots."
                    (payload
                        "typechecking-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "projection-root-requires-stable-place") ])
            | RecordUpdateDependentFieldRequiresRepair(receiverRoot, fieldName) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Record update on '{receiverRoot}' changes dependent field inputs but does not repair field '{fieldName}'."
                    (payload
                        "typechecking-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "record-update-dependent-field-requires-repair")
                          field "receiver-root" (DiagnosticPayloadText receiverRoot)
                          field "field-name" (DiagnosticPayloadText fieldName) ])
            | MultishotEffectUnsupportedBackendAtInvocation(backendProfile, effectVisibleName, operationName) ->
                descriptor
                    DiagnosticCode.MultishotEffectUnsupportedBackend
                    None
                    $"Backend profile '{backendProfile}' does not provide capability 'rt-multishot-effects' required by multi-shot operation '{effectVisibleName}.{operationName}' at this invocation site."
                    (payload
                        "typechecking-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "multishot-effect-unsupported-backend-at-invocation")
                          field "backend-profile" (DiagnosticPayloadText backendProfile)
                          field "effect-visible-name" (DiagnosticPayloadText effectVisibleName)
                          field "operation-name" (DiagnosticPayloadText operationName) ])
            | ApplicationArgumentTypeMismatchAtSite(actualTypeText, demandedTypeText) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Argument type '{actualTypeText}' does not match demanded parameter type '{demandedTypeText}'."
                    (payload
                        "typechecking-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "application-argument-type-mismatch-at-site")
                          field "actual-type-text" (DiagnosticPayloadText actualTypeText)
                          field "demanded-type-text" (DiagnosticPayloadText demandedTypeText) ])
            | ContinuationCaptureForbidden(effectVisibleName, operationName, capturedSummary) ->
                descriptor
                    DiagnosticCode.QttContinuationCapture
                    None
                    $"Multi-shot operation '{effectVisibleName}.{operationName}' cannot capture {capturedSummary} in its continuation."
                    (payload
                        "typechecking-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "continuation-capture-forbidden")
                          field "effect-visible-name" (DiagnosticPayloadText effectVisibleName)
                          field "operation-name" (DiagnosticPayloadText operationName)
                          field "captured-summary" (DiagnosticPayloadText capturedSummary) ])
            | DeferredActionContainsAbruptOuterEscape ->
                descriptor
                    DiagnosticCode.ControlFlowInvalidEscape
                    None
                    "A deferred action must not contain return, break, or continue targeting an outer scope."
                    (payload
                        "typechecking-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "deferred-action-contains-abrupt-outer-escape") ])
            | DefinitionBodyResultTypeMismatch ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "The definition body does not match the declared result type."
                    (payload
                        "typechecking-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "definition-body-result-type-mismatch") ])
        | BackendDiagnostic evidence ->
            let backendEffectUseFields effectUse =
                match effectUse with
                | BackendEffectLabelUse labelName ->
                    "effect-label", "effect label", [ field "effect-use-name" (DiagnosticPayloadText labelName) ]
                | BackendEffectOperationUse operationName ->
                    "effect-operation", "effect operation", [ field "effect-use-name" (DiagnosticPayloadText operationName) ]
                | BackendEffectHandlerUse true ->
                    "deep-handler", "deep effect handler", []
                | BackendEffectHandlerUse false ->
                    "handler", "effect handler", []

            let backendLoweringErrorDescriptor moduleName error =
                match error with
                | EmptyRuntimeName ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-empty-runtime-name",
                    "Runtime name resolution cannot lower an empty runtime name.",
                    []
                | AmbiguousRuntimeName name ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-ambiguous-runtime-name",
                    $"Runtime name '{name}' resolves ambiguously during KBackendIR lowering.",
                    [ field "runtime-name" (DiagnosticPayloadText name) ]
                | UnresolvedRuntimeName name ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-unresolved-runtime-name",
                    $"Runtime name '{name}' could not be resolved during KBackendIR lowering.",
                    [ field "runtime-name" (DiagnosticPayloadText name) ]
                | UnresolvedModuleQualifier qualifierText ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-unresolved-module-qualifier",
                    $"Module qualifier '{qualifierText}' could not be resolved during KBackendIR lowering.",
                    [ field "qualifier-text" (DiagnosticPayloadText qualifierText) ]
                | EmptyRuntimeConstructorName ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-empty-runtime-constructor-name",
                    "Runtime constructor resolution cannot lower an empty constructor name.",
                    []
                | AmbiguousRuntimeConstructorName constructorName ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-ambiguous-runtime-constructor-name",
                    $"Runtime constructor '{constructorName}' resolves ambiguously during KBackendIR lowering.",
                    [ field "constructor-name" (DiagnosticPayloadText constructorName) ]
                | UnresolvedRuntimeConstructorName constructorName ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-unresolved-runtime-constructor-name",
                    $"Runtime constructor '{constructorName}' could not be resolved during KBackendIR lowering.",
                    [ field "constructor-name" (DiagnosticPayloadText constructorName) ]
                | RuntimeCallTargetArityMismatch(targetModuleName, bindingName, expectedArity, actualArity) ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-runtime-call-target-arity-mismatch",
                    $"Runtime call target '{targetModuleName}.{bindingName}' expects {expectedArity} argument(s) but received {actualArity}.",
                    [ field "target-module-name" (DiagnosticPayloadText targetModuleName)
                      field "binding-name" (DiagnosticPayloadText bindingName)
                      field "expected-arity" (DiagnosticPayloadText(string expectedArity))
                      field "actual-arity" (DiagnosticPayloadText(string actualArity)) ]
                | RuntimeCallTargetCouldNotBeLowered(targetModuleName, bindingName) ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-runtime-call-target-could-not-be-lowered",
                    $"Runtime call target '{targetModuleName}.{bindingName}' could not be lowered to KBackendIR.",
                    [ field "target-module-name" (DiagnosticPayloadText targetModuleName)
                      field "binding-name" (DiagnosticPayloadText bindingName) ]
                | OrPatternAlternativesBindDifferentNames ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-or-pattern-alternatives-bind-different-names",
                    "Backend lowering requires each or-pattern alternative to bind the same names.",
                    []
                | OrPatternBinderRepresentationMismatch binderName ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-or-pattern-binder-representation-mismatch",
                    $"Backend lowering requires binder '{binderName}' to keep the same runtime representation across every or-pattern alternative.",
                    [ field "binder-name" (DiagnosticPayloadText binderName) ]
                | ConstructorPatternArityMismatch(patternText, expectedArity, actualArity) ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-constructor-pattern-arity-mismatch",
                    $"Pattern '{patternText}' expected {expectedArity} constructor argument(s), but received {actualArity}.",
                    [ field "pattern-text" (DiagnosticPayloadText patternText)
                      field "expected-arity" (DiagnosticPayloadText(string expectedArity))
                      field "actual-arity" (DiagnosticPayloadText(string actualArity)) ]
                | PatternDoesNotResolveToConstructor patternText ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-pattern-does-not-resolve-to-constructor",
                    $"Pattern '{patternText}' does not resolve to a constructor during KBackendIR lowering.",
                    [ field "pattern-text" (DiagnosticPayloadText patternText) ]
                | RuntimeIntrinsicArityMismatch(intrinsicName, expectedArity, actualArity) ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-runtime-intrinsic-arity-mismatch",
                    $"Runtime intrinsic '{intrinsicName}' expects {expectedArity} argument(s) but received {actualArity}.",
                    [ field "intrinsic-name" (DiagnosticPayloadText intrinsicName)
                      field "expected-arity" (DiagnosticPayloadText(string expectedArity))
                      field "actual-arity" (DiagnosticPayloadText(string actualArity)) ]
                | RuntimeTagTestRequiresConstructorName ->
                    DiagnosticCode.CheckpointVerification,
                    "backend-lowering-runtime-tag-test-requires-constructor-name",
                    "Runtime operator 'is' expects a constructor name on the right-hand side.",
                    []

            match evidence with
            | EffectRuntimeUnsupportedBackend(backendProfile, declarationName, effectUse) ->
                let effectUseKind, effectUseText, useFields = backendEffectUseFields effectUse

                descriptor
                    DiagnosticCode.EffectRuntimeUnsupportedBackend
                    None
                    $"Backend profile '{backendProfile}' does not implement {effectUseText} in declaration '{declarationName}'. This backend must reject unsupported effect runtime constructs before target lowering."
                    (payload
                        "backend-diagnostic"
                        ([ field "reason" (DiagnosticPayloadText "effect-runtime-unsupported-backend")
                           field "backend-profile" (DiagnosticPayloadText backendProfile)
                           field "declaration-name" (DiagnosticPayloadText declarationName)
                           field "effect-use-kind" (DiagnosticPayloadText effectUseKind) ]
                         @ useFields))
            | KBackendLoweringFailed(moduleName, error) ->
                let code, reason, message, fields = backendLoweringErrorDescriptor moduleName error

                descriptor
                    code
                    None
                    message
                    (payload
                        "backend-diagnostic"
                        ([ field "reason" (DiagnosticPayloadText reason)
                           field "module-name" (DiagnosticPayloadText moduleName) ]
                         @ fields))
        | TargetCheckpointDiagnostic evidence ->
            match evidence with
            | TargetCheckpointWorkspaceHasDiagnostics(checkpoint, diagnosticCount, diagnosticCodes) ->
                descriptor
                    DiagnosticCode.TargetCheckpoint
                    None
                    $"Cannot emit target checkpoint '{checkpoint}' for a workspace with diagnostics."
                    (payload
                        "target-checkpoint-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "target-checkpoint-workspace-has-diagnostics")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "diagnostic-count" (DiagnosticPayloadText(string diagnosticCount))
                          field "diagnostic-codes" (DiagnosticPayloadTextList diagnosticCodes) ])
            | TargetCheckpointMalformedInput(checkpoint, inputCheckpoint, diagnosticCount, diagnosticCodes) ->
                descriptor
                    DiagnosticCode.TargetCheckpoint
                    None
                    $"Cannot emit target checkpoint '{checkpoint}' from malformed {inputCheckpoint}."
                    (payload
                        "target-checkpoint-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "target-checkpoint-malformed-input")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "input-checkpoint" (DiagnosticPayloadText inputCheckpoint)
                          field "diagnostic-count" (DiagnosticPayloadText(string diagnosticCount))
                          field "diagnostic-codes" (DiagnosticPayloadTextList diagnosticCodes) ])
            | TargetCheckpointEmitterFailure(checkpoint, backendProfile, detail) ->
                descriptor
                    DiagnosticCode.TargetCheckpoint
                    None
                    $"Target checkpoint '{checkpoint}' failed during backend emission for profile '{backendProfile}': {detail}"
                    (payload
                        "target-checkpoint-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "target-checkpoint-emitter-failure")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "backend-profile" (DiagnosticPayloadText backendProfile)
                          field "detail" (DiagnosticPayloadText detail) ])
            | UnknownTargetCheckpoint checkpoint ->
                descriptor
                    DiagnosticCode.TargetCheckpoint
                    None
                    $"Unknown checkpoint '{checkpoint}'."
                    (payload
                        "target-checkpoint-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "unknown-target-checkpoint")
                          field "checkpoint" (DiagnosticPayloadText checkpoint) ])
        | CheckpointVerificationDiagnostic evidence ->
            match evidence with
            | DuplicateFileIdentity(checkpoint, filePath) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires unique file identities, but '{filePath}' appeared more than once."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-file-identity")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "file-path" (DiagnosticPayloadText filePath) ])
            | NonEmptyLineTableRequired(checkpoint, filePath) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires non-empty line tables for '{filePath}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "non-empty-line-table-required")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "file-path" (DiagnosticPayloadText filePath) ])
            | EndOfFileTokenRequired(checkpoint, filePath) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires an EOF token for '{filePath}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "end-of-file-token-required")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "file-path" (DiagnosticPayloadText filePath) ])
            | ResolvedPhaseSetMismatch(checkpoint, filePath, expectedPhases, actualPhases) ->
                let expectedText = String.concat ", " expectedPhases
                let actualText = String.concat ", " actualPhases

                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires '{filePath}' to expose resolved phases [{expectedText}], but found [{actualText}]."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "resolved-phase-set-mismatch")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "file-path" (DiagnosticPayloadText filePath)
                          field "expected-phases" (DiagnosticPayloadTextList expectedPhases)
                          field "actual-phases" (DiagnosticPayloadTextList actualPhases) ])
            | OwnershipFactsExposedBeforeBodyResolve(checkpoint, filePath) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' must not expose BODY_RESOLVE ownership facts for '{filePath}' before BODY_RESOLVE."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "ownership-facts-exposed-before-body-resolve")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "file-path" (DiagnosticPayloadText filePath) ])
            | DuplicateModuleIdentity(checkpoint, moduleName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires unique module identities, but '{moduleName}' appeared more than once."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-module-identity")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName) ])
            | MissingImportedRuntimeModule(checkpoint, moduleName, importedModuleName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires imported runtime module '{importedModuleName}' to be present for module '{moduleName}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "missing-imported-runtime-module")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "imported-module-name" (DiagnosticPayloadText importedModuleName) ])
            | DuplicateRuntimeBindingIdentity(checkpoint, moduleName, bindingName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires unique binding identities within module '{moduleName}', but '{bindingName}' was duplicated."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-runtime-binding-identity")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "binding-name" (DiagnosticPayloadText bindingName) ])
            | DuplicateRuntimeConstructorIdentity(checkpoint, moduleName, constructorName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires unique constructor identities within module '{moduleName}', but '{constructorName}' was duplicated."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-runtime-constructor-identity")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "constructor-name" (DiagnosticPayloadText constructorName) ])
            | UnsupportedRuntimeIntrinsicTerm(checkpoint, moduleName, intrinsicName, backendProfile) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires intrinsic term '{intrinsicName}' in module '{moduleName}' to be provided by backend profile '{backendProfile}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "unsupported-runtime-intrinsic-term")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "intrinsic-name" (DiagnosticPayloadText intrinsicName)
                          field "backend-profile" (DiagnosticPayloadText backendProfile) ])
            | IntrinsicBindingMustOmitBody(checkpoint, moduleName, bindingName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires intrinsic binding '{moduleName}.{bindingName}' to omit a body."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "intrinsic-binding-must-omit-body")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "binding-name" (DiagnosticPayloadText bindingName) ])
            | IntrinsicBindingMustBeListedInModuleIntrinsicTerms(checkpoint, moduleName, bindingName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires intrinsic binding '{moduleName}.{bindingName}' to be listed in module intrinsic terms."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "intrinsic-binding-must-be-listed-in-module-intrinsic-terms")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "binding-name" (DiagnosticPayloadText bindingName) ])
            | UnsupportedRuntimeIntrinsicBinding(checkpoint, moduleName, bindingName, backendProfile) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires intrinsic term '{bindingName}' in module '{moduleName}' to be provided by backend profile '{backendProfile}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "unsupported-runtime-intrinsic-binding")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "binding-name" (DiagnosticPayloadText bindingName)
                          field "backend-profile" (DiagnosticPayloadText backendProfile) ])
            | RuntimeBindingMustHaveBody(checkpoint, moduleName, bindingName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires runtime binding '{moduleName}.{bindingName}' to have a body."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "runtime-binding-must-have-body")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "binding-name" (DiagnosticPayloadText bindingName) ])
            | DuplicateRuntimePatternBinder(checkpoint, bindingLabel, binderName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires unique pattern binder names within '{bindingLabel}', but '{binderName}' was duplicated."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-runtime-pattern-binder")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "binder-name" (DiagnosticPayloadText binderName) ])
            | DuplicateRuntimeClosureParameter(checkpoint, bindingLabel, parameterName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires closures in '{bindingLabel}' to have unique parameter names, but '{parameterName}' was duplicated."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-runtime-closure-parameter")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "parameter-name" (DiagnosticPayloadText parameterName) ])
            | DeepRuntimeHandlerMustBeDesugared(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires direct deep-handle runtime nodes in '{bindingLabel}' to be desugared into the recursive shallow-handler driver before KRuntimeIR verification."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "deep-runtime-handler-must-be-desugared")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | MissingBackendModuleForRuntimeModule(checkpoint, runtimeModuleName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires a backend module for runtime module '{runtimeModuleName}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "missing-backend-module-for-runtime-module")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "runtime-module-name" (DiagnosticPayloadText runtimeModuleName) ])
            | MissingImportedBackendModule(checkpoint, moduleName, importedModuleName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires imported backend module '{importedModuleName}' to be present for module '{moduleName}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "missing-imported-backend-module")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "imported-module-name" (DiagnosticPayloadText importedModuleName) ])
            | DuplicateBackendFunctionIdentity(checkpoint, moduleName, functionName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires unique function identities within module '{moduleName}', but '{functionName}' was duplicated."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-backend-function-identity")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "function-name" (DiagnosticPayloadText functionName) ])
            | DuplicateBackendDataLayoutIdentity(checkpoint, moduleName, typeName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires unique data-layout identities within module '{moduleName}', but '{typeName}' was duplicated."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-backend-data-layout-identity")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "type-name" (DiagnosticPayloadText typeName) ])
            | DuplicateBackendEnvironmentLayoutIdentity(checkpoint, moduleName, layoutName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires unique environment-layout identities within module '{moduleName}', but '{layoutName}' was duplicated."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-backend-environment-layout-identity")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "layout-name" (DiagnosticPayloadText layoutName) ])
            | UnsupportedBackendIntrinsicTerm(checkpoint, moduleName, intrinsicName, backendProfile) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires intrinsic term '{intrinsicName}' in module '{moduleName}' to be provided by backend profile '{backendProfile}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "unsupported-backend-intrinsic-term")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "intrinsic-name" (DiagnosticPayloadText intrinsicName)
                          field "backend-profile" (DiagnosticPayloadText backendProfile) ])
            | ListedBackendEntryPointMustExistAsFunction(checkpoint, moduleName, entryPointName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires listed entry point '{moduleName}.{entryPointName}' to be present as a function."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "listed-backend-entry-point-must-exist-as-function")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "entry-point-name" (DiagnosticPayloadText entryPointName) ])
            | ListedBackendEntryPointMustBeMarked(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires listed entry point '{bindingLabel}' to be marked as an entry point."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "listed-backend-entry-point-must-be-marked")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | BackendFunctionCallingConventionArityMismatch(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires function '{bindingLabel}' to have a calling convention arity matching its parameter count."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-function-calling-convention-arity-mismatch")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | BackendFunctionParameterRepresentationsMismatch(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires function '{bindingLabel}' to have calling convention parameter representations that match its parameters."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-function-parameter-representations-mismatch")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | BackendFunctionEnvironmentLayoutMissing(checkpoint, bindingLabel, layoutName, moduleName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires function '{bindingLabel}' to reference an environment layout present in module '{moduleName}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-function-environment-layout-missing")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "layout-name" (DiagnosticPayloadText layoutName)
                          field "module-name" (DiagnosticPayloadText moduleName) ])
            | BackendEntryPointMustBeExported(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires entry point '{bindingLabel}' to be exported."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-entry-point-must-be-exported")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | BackendMarkedEntryPointMustBeListed(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires function '{bindingLabel}' marked as an entry point to be listed in module entry points."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-marked-entry-point-must-be-listed")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | IntrinsicBackendFunctionMustOmitBody(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires intrinsic function '{bindingLabel}' to omit a body."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "intrinsic-backend-function-must-omit-body")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | IntrinsicBackendFunctionMustBeListedInModuleIntrinsicTerms(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires intrinsic function '{bindingLabel}' to be listed in module intrinsic terms."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "intrinsic-backend-function-must-be-listed-in-module-intrinsic-terms")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | UnsupportedBackendIntrinsicFunction(checkpoint, moduleName, bindingName, backendProfile) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires intrinsic term '{bindingName}' in module '{moduleName}' to be provided by backend profile '{backendProfile}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "unsupported-backend-intrinsic-function")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "module-name" (DiagnosticPayloadText moduleName)
                          field "binding-name" (DiagnosticPayloadText bindingName)
                          field "backend-profile" (DiagnosticPayloadText backendProfile) ])
            | BackendFunctionMustHaveBody(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires function '{bindingLabel}' to have a body."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-function-must-have-body")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | PreErasureParameterTypeMetadataLeak(checkpoint, bindingLabel, parameterName, typeText) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires pre-erasure runtime metadata to be removed before backend lowering, but parameter '{bindingLabel}.{parameterName}' still exposes '{typeText}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "pre-erasure-parameter-type-metadata-leak")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "parameter-name" (DiagnosticPayloadText parameterName)
                          field "type-text" (DiagnosticPayloadText typeText) ])
            | PreErasureReturnTypeMetadataLeak(checkpoint, bindingLabel, typeText) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires pre-erasure runtime metadata to be removed before backend lowering, but return type of '{bindingLabel}' still exposes '{typeText}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "pre-erasure-return-type-metadata-leak")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "type-text" (DiagnosticPayloadText typeText) ])
            | PreErasureConstructorFieldMetadataLeak(checkpoint, fieldLabel, typeText) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires pre-erasure runtime metadata to be removed before backend lowering, but constructor field '{fieldLabel}' still exposes '{typeText}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "pre-erasure-constructor-field-metadata-leak")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "field-label" (DiagnosticPayloadText fieldLabel)
                          field "type-text" (DiagnosticPayloadText typeText) ])
            | PreErasureInstanceHeadMetadataLeak(checkpoint, instanceHeadLabel, typeText) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires pre-erasure runtime metadata to be removed before backend lowering, but instance head '{instanceHeadLabel}' still exposes '{typeText}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "pre-erasure-instance-head-metadata-leak")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "instance-head-label" (DiagnosticPayloadText instanceHeadLabel)
                          field "type-text" (DiagnosticPayloadText typeText) ])
            | DuplicateBackendPatternBinder(checkpoint, bindingLabel, binderName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires unique pattern binder names within '{bindingLabel}', but '{binderName}' was duplicated."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-backend-pattern-binder")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "binder-name" (DiagnosticPayloadText binderName) ])
            | BackendOrPatternAlternativesMustBindSameNames(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires each or-pattern alternative within '{bindingLabel}' to bind the same names."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-or-pattern-alternatives-must-bind-same-names")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | BackendOrPatternBinderRepresentationMismatch(checkpoint, bindingLabel, binderName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires binder '{binderName}' within '{bindingLabel}' to keep the same backend representation across every or-pattern alternative."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-or-pattern-binder-representation-mismatch")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "binder-name" (DiagnosticPayloadText binderName) ])
            | BackendPatternConstructorMissingFromModuleGraph(checkpoint, bindingLabel, constructorText) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires resolved constructor patterns, but '{constructorText}' in '{bindingLabel}' is not present in the backend module graph."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-pattern-constructor-missing-from-module-graph")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "constructor-text" (DiagnosticPayloadText constructorText) ])
            | BackendResolvedRuntimeNameMissing(checkpoint, bindingLabel, resolvedNameText) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires resolved runtime names, but '{resolvedNameText}' in '{bindingLabel}' is not present in the backend module graph."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-resolved-runtime-name-missing")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "resolved-name-text" (DiagnosticPayloadText resolvedNameText) ])
            | DuplicateBackendClosureParameter(checkpoint, bindingLabel, parameterName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires closures in '{bindingLabel}' to have unique parameter names, but '{parameterName}' was duplicated."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-backend-closure-parameter")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "parameter-name" (DiagnosticPayloadText parameterName) ])
            | DuplicateBackendClosureCapture(checkpoint, bindingLabel, captureName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires closures in '{bindingLabel}' to have unique capture names, but '{captureName}' was duplicated."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "duplicate-backend-closure-capture")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "capture-name" (DiagnosticPayloadText captureName) ])
            | BackendClosureEnvironmentLayoutMissing(checkpoint, bindingLabel, layoutName, moduleName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires closure environment layout '{layoutName}' in '{bindingLabel}' to be present in module '{moduleName}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-closure-environment-layout-missing")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "layout-name" (DiagnosticPayloadText layoutName)
                          field "module-name" (DiagnosticPayloadText moduleName) ])
            | BackendClosureCallingConventionArityMismatch(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires closure '{bindingLabel}' to have a calling convention arity matching its parameter count."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-closure-calling-convention-arity-mismatch")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | BackendClosureParameterRepresentationsMismatch(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires closure '{bindingLabel}' to have calling convention parameter representations that match its parameters."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-closure-parameter-representations-mismatch")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | BackendClosureRepresentationLayoutMismatch(checkpoint, bindingLabel, layoutName) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires closure '{bindingLabel}' to have a representation that references environment layout '{layoutName}'."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-closure-representation-layout-mismatch")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "layout-name" (DiagnosticPayloadText layoutName) ])
            | BackendClosureRepresentationMustBeClosure(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires closure '{bindingLabel}' to use a closure representation."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-closure-representation-must-be-closure")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | BackendCallConventionArityMismatch(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires calls in '{bindingLabel}' to have an argument count matching the calling convention arity."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-call-convention-arity-mismatch")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | BackendCallParameterRepresentationCountMismatch(checkpoint, bindingLabel) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires calls in '{bindingLabel}' to have a parameter representation for each argument."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-call-parameter-representation-count-mismatch")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel) ])
            | BackendConstructedDataMissingLayout(checkpoint, bindingLabel, constructorText) ->
                descriptor
                    DiagnosticCode.CheckpointVerification
                    None
                    $"Checkpoint '{checkpoint}' requires constructed data '{constructorText}' in '{bindingLabel}' to match a backend data layout."
                    (payload
                        "checkpoint-verification-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "backend-constructed-data-missing-layout")
                          field "checkpoint" (DiagnosticPayloadText checkpoint)
                          field "binding-label" (DiagnosticPayloadText bindingLabel)
                          field "constructor-text" (DiagnosticPayloadText constructorText) ])
        | SurfaceRecordDiagnostic evidence ->
            match evidence with
            | RecordFieldDeclaredMoreThanOnce fieldName ->
                descriptor
                    DiagnosticCode.RecordDuplicateField
                    None
                    $"Record field '{fieldName}' is declared more than once."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "record-field-declared-more-than-once")
                          field "field-name" (DiagnosticPayloadText fieldName) ])
            | RecordFieldDependsOnUnknownField(fieldName, referencedField) ->
                descriptor
                    DiagnosticCode.RecordDependencyInvalid
                    None
                    $"Record field '{fieldName}' depends on field '{referencedField}', which is not in the explicit record telescope."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "record-field-depends-on-unknown-field")
                          field "field-name" (DiagnosticPayloadText fieldName)
                          field "referenced-field" (DiagnosticPayloadText referencedField) ])
            | RecordDependenciesMustBeAcyclic context ->
                descriptor
                    DiagnosticCode.RecordDependencyCycle
                    None
                    $"{surfaceRecordContextText context} field dependencies must be acyclic."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "record-dependencies-must-be-acyclic")
                          field "record-context" (DiagnosticPayloadText(surfaceRecordContextPayloadText context)) ])
            | RecordPatchPathDeclaredMoreThanOnce pathText ->
                descriptor
                    DiagnosticCode.RecordPatchDuplicatePath
                    None
                    $"Record patch updates path '{pathText}' more than once."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "record-patch-path-declared-more-than-once")
                          field "path-text" (DiagnosticPayloadText pathText) ])
            | RecordPatchPathStrictPrefixConflict(prefixPath, fullPath) ->
                descriptor
                    DiagnosticCode.RecordPatchPrefixConflict
                    None
                    $"Record patch path '{prefixPath}' is a strict prefix of '{fullPath}'."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "record-patch-path-strict-prefix-conflict")
                          field "prefix-path" (DiagnosticPayloadText prefixPath)
                          field "full-path" (DiagnosticPayloadText fullPath) ])
            | RowExtensionLabelDeclaredMoreThanOnce labelName ->
                descriptor
                    DiagnosticCode.RowExtensionDuplicateLabel
                    None
                    $"Row extension label '{labelName}' appears more than once."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "row-extension-label-declared-more-than-once")
                          field "label-name" (DiagnosticPayloadText labelName) ])
            | RowExtensionLabelAlreadyExists labelName ->
                descriptor
                    DiagnosticCode.RowExtensionExistingField
                    None
                    $"Row extension label '{labelName}' already exists in the receiver record."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "row-extension-label-already-exists")
                          field "label-name" (DiagnosticPayloadText labelName) ])
            | RowExtensionLabelMissingLacksConstraint(labelName, rowName) ->
                descriptor
                    DiagnosticCode.RowExtensionMissingLacksConstraint
                    None
                    $"Row extension label '{labelName}' for row '{rowName}' requires a matching LacksRec constraint."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "row-extension-label-missing-lacks-constraint")
                          field "label-name" (DiagnosticPayloadText labelName)
                          field "row-name" (DiagnosticPayloadText rowName) ])
            | RecordPatchUnknownField fieldName ->
                descriptor
                    DiagnosticCode.RecordPatchUnknownPath
                    None
                    $"Record patch path contains unknown field '{fieldName}'."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "record-patch-unknown-field")
                          field "field-name" (DiagnosticPayloadText fieldName) ])
            | RecordPatchContinuesThroughNonRecordField fieldName ->
                descriptor
                    DiagnosticCode.RecordPatchUnknownPath
                    None
                    $"Record patch path continues through non-record field '{fieldName}'."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "record-patch-continues-through-non-record-field")
                          field "field-name" (DiagnosticPayloadText fieldName) ])
            | ProjectionSectionUpdateTargetUnsupported ->
                descriptor
                    DiagnosticCode.ProjectionUpdateTargetUnsupported
                    None
                    "Projection-section update requires an accessor/setter or selector projection."
                    (payload
                        "surface-record-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "projection-section-update-target-unsupported") ])
        | SurfaceElaborationDiagnostic evidence ->
            match evidence with
            | ProjectionDefinitionRequiresPlaceBinder ->
                descriptor
                    DiagnosticCode.ProjectionMissingPlaceBinder
                    None
                    "A projection definition must declare at least one place binder."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "projection-definition-requires-place-binder") ])
            | SelectorProjectionYieldInvalid reason ->
                let reasonText, message =
                    match reason with
                    | ProjectionYieldMustDenoteStablePlaceNotAccessorBundle ->
                        "selector-projection-yield-must-denote-stable-place-not-accessor-bundle",
                        "A selector projection yield must denote a stable place, not an accessor bundle."
                    | ProjectionYieldMustDenoteStablePlace ->
                        "selector-projection-yield-must-denote-stable-place",
                        "A selector projection yield must denote a stable place."
                    | ProjectionYieldMustBeRootedInPlaceBinder ->
                        "selector-projection-yield-must-be-rooted-in-place-binder",
                        "A selector projection yield must denote a stable place rooted in a place binder."

                descriptor
                    DiagnosticCode.ProjectionYieldInvalid
                    None
                    message
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "category" (DiagnosticPayloadText "projection-yield-invalid")
                          field "reason" (DiagnosticPayloadText reasonText) ])
            | ExpandedAccessorProjectionRequiresExactlyOnePlaceBinder ->
                descriptor
                    DiagnosticCode.ProjectionExpandedAccessorPlaceBinderMismatch
                    None
                    "An expanded accessor projection must declare exactly one place binder."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "expanded-accessor-projection-requires-exactly-one-place-binder") ])
            | ProjectionAccessorClauseDeclaredMoreThanOnce clauseKind ->
                descriptor
                    DiagnosticCode.ProjectionAccessorClauseDuplicate
                    None
                    $"Projection accessor clause '{clauseKind}' is declared more than once."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "projection-accessor-clause-declared-more-than-once")
                          field "clause-kind" (DiagnosticPayloadText clauseKind) ])
            | UnknownModuleAttribute(attributeName, supportedAttributes) ->
                let supportedAttributeSpellings = supportedAttributes |> List.map (fun name -> "@" + name)
                let supportedText = String.concat ", " supportedAttributeSpellings

                descriptor
                    DiagnosticCode.ModuleAttributeUnknown
                    None
                    $"Unknown module attribute '@{attributeName}'. Supported module attributes in this implementation are: {supportedText}."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "unknown-module-attribute")
                          field "attribute-name" (DiagnosticPayloadText attributeName)
                          field "supported-attributes" (DiagnosticPayloadTextList supportedAttributeSpellings) ])
            | AssertTerminatesRequiresAllowAttribute bindingName ->
                descriptor
                    DiagnosticCode.AssertTerminatesRequiresModuleAttribute
                    None
                    $"Declaration '{bindingName}' uses assertTerminates/assertTotal without enabling module attribute 'allow_assert_terminates'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "assert-terminates-requires-allow-attribute")
                          field "binding-name" (DiagnosticPayloadText bindingName) ])
            | AssertReducibleRequiresAllowAttribute bindingName ->
                descriptor
                    DiagnosticCode.AssertReducibleRequiresModuleAttribute
                    None
                    $"Declaration '{bindingName}' uses assertReducible without enabling module attribute 'allow_assert_reducible'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "assert-reducible-requires-allow-attribute")
                          field "binding-name" (DiagnosticPayloadText bindingName) ])
            | RecursiveTypeAliasDependsOnItself aliasName ->
                descriptor
                    DiagnosticCode.RecursiveTypeAlias
                    None
                    $"Type alias '{aliasName}' recursively depends on itself."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "recursive-type-alias-depends-on-itself")
                          field "alias-name" (DiagnosticPayloadText aliasName) ])
            | TopLevelRecursiveBindingRequiresPrecedingSignature bindingName ->
                descriptor
                    DiagnosticCode.RecursionRequiresSignature
                    None
                    $"Top-level binding '{bindingName}' is recursive but has no preceding signature declaration."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "top-level-recursive-binding-requires-preceding-signature")
                          field "binding-name" (DiagnosticPayloadText bindingName) ])
            | TrivialRecursiveCycleMustBeRejected bindingName ->
                descriptor
                    DiagnosticCode.RecursionRequiresSignature
                    None
                    $"Recursive cycle for binding '{bindingName}' is not total and must be rejected."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "trivial-recursive-cycle-must-be-rejected")
                          field "binding-name" (DiagnosticPayloadText bindingName) ])
            | ConstructorDeclarationStartsWithDeclarationKeyword(dataTypeName, constructorName) ->
                descriptor
                    DiagnosticCode.MalformedConstructorDeclaration
                    None
                    $"Constructor declaration in data type '{dataTypeName}' starts with declaration keyword '{constructorName}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "constructor-declaration-starts-with-declaration-keyword")
                          field "data-type-name" (DiagnosticPayloadText dataTypeName)
                          field "constructor-name" (DiagnosticPayloadText constructorName) ])
            | ConstructorExposesRuntimeFieldMetadataOfType constructorName ->
                descriptor
                    DiagnosticCode.MalformedConstructorDeclaration
                    None
                    $"Constructor '{constructorName}' exposes runtime field metadata of type 'Type'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "constructor-exposes-runtime-field-metadata-of-type")
                          field "constructor-name" (DiagnosticPayloadText constructorName) ])
            | ConstructorDeclarationMalformedInDataType(dataTypeName, constructorName) ->
                descriptor
                    DiagnosticCode.MalformedConstructorDeclaration
                    None
                    $"Constructor declaration '{constructorName}' in data type '{dataTypeName}' is malformed."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "constructor-declaration-malformed-in-data-type")
                          field "data-type-name" (DiagnosticPayloadText dataTypeName)
                          field "constructor-name" (DiagnosticPayloadText constructorName) ])
            | RecordProjectionFieldMissing fieldName ->
                descriptor
                    DiagnosticCode.RecordProjectionMissingField
                    None
                    $"Record type has no field named '{fieldName}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "record-projection-field-missing")
                          field "field-name" (DiagnosticPayloadText fieldName) ])
            | SealProjectionWouldExposeOpaqueDependentMember(rootName, memberName) ->
                descriptor
                    DiagnosticCode.SealOpaqueUnfolding
                    None
                    $"Projection '{rootName}.{memberName}' would expose a member whose type depends on an opaque package member."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "seal-projection-would-expose-opaque-dependent-member")
                          field "root-name" (DiagnosticPayloadText rootName)
                          field "member-name" (DiagnosticPayloadText memberName) ])
            | RecordLiteralCannotBeCheckedDirectlyAgainstSignatureType ->
                descriptor
                    DiagnosticCode.SealDirectLiteralForSignature
                    None
                    "A record literal cannot be checked directly against a signature type; use 'seal ... as ...'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "record-literal-cannot-be-checked-directly-against-signature-type") ])
            | SealAscriptionMustBeClosedRecordType ->
                descriptor
                    DiagnosticCode.SealOpenRecordAscription
                    None
                    "The ascribed type of 'seal' must be a closed record type."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "seal-ascription-must-be-closed-record-type") ])
            | ProjectionDescriptorRootsMustBeStablePlaces ->
                descriptor
                    DiagnosticCode.ProjectionRootInvalid
                    None
                    "Projector descriptor roots must be stable places or selector-computed places."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "projection-descriptor-roots-must-be-stable-places") ])
            | ProjectionDescriptorApplicationRequiresAtLeastOneRoot ->
                descriptor
                    DiagnosticCode.ProjectionDescriptorRootMissing
                    None
                    "A projector descriptor application must still have at least one root."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "projection-descriptor-application-requires-at-least-one-root") ])
            | ProjectionDescriptorRootsPackFieldsMustMatchBindersExactly ->
                descriptor
                    DiagnosticCode.ProjectionRootsPackMismatch
                    None
                    "Projector descriptor roots pack fields must match the projector root binders exactly."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "projection-descriptor-roots-pack-fields-must-match-binders-exactly") ])
            | MultiRootProjectionDescriptorRequiresRecordLiteralRootsPack ->
                descriptor
                    DiagnosticCode.ProjectionDescriptorRootsLiteralRequired
                    None
                    "Multi-root projector descriptors must be applied to a record literal roots pack."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "multi-root-projection-descriptor-requires-record-literal-roots-pack") ])
            | FullyAppliedProjectionProducesProjectedFocusNotDescriptorValue ->
                descriptor
                    DiagnosticCode.ProjectionDescriptorValueExpected
                    None
                    "A fully applied projection produces the projected focus, not a projector descriptor value."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "fully-applied-projection-produces-projected-focus-not-descriptor-value") ])
            | MemberCallRequiresExactlyOneReceiverBinder memberName ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Member-call sugar for '{memberName}' requires exactly one receiver binder."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "member-call-requires-exactly-one-receiver-binder")
                          field "member-name" (DiagnosticPayloadText memberName) ])
            | MemberCallMissingPrecedingExplicitArguments memberName ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Member-call sugar for '{memberName}' is missing preceding explicit argument(s)."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "member-call-missing-preceding-explicit-arguments")
                          field "member-name" (DiagnosticPayloadText memberName) ])
            | MemberCallMissingExplicitArguments memberName ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Member-call sugar for '{memberName}' is missing explicit argument(s)."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "member-call-missing-explicit-arguments")
                          field "member-name" (DiagnosticPayloadText memberName) ])
            | ApplicationArgumentsDoNotMatchCalleeParameters ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "Application argument types do not match the callee parameters."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "application-arguments-do-not-match-callee-parameters") ])
            | NamedApplicationArgumentsDoNotMatchCalleeParameterMetadata ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "Named application arguments do not match the callee parameter metadata."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "named-application-arguments-do-not-match-callee-parameter-metadata") ])
            | ImplicitApplicationArgumentUnresolvedOrMismatched ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "Implicit application argument could not be resolved or does not match the implicit parameter."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "implicit-application-argument-unresolved-or-mismatched") ])
            | NamedApplicationRequiresPreservedParameterMetadata ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "Named application requires a callee with preserved parameter metadata."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "named-application-requires-preserved-parameter-metadata") ])
            | ImplicitApplicationArgumentAmbiguous ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "Implicit application argument is ambiguous in the nearest lexical implicit scope."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "implicit-application-argument-ambiguous") ])
            | QuantityZeroImplicitCannotSatisfyRuntimeParameter ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "A quantity-0 local implicit value cannot satisfy a runtime implicit parameter."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "quantity-zero-implicit-cannot-satisfy-runtime-parameter") ])
            | ApplicationExpressionNotCallable calleeTypeText ->
                descriptor
                    DiagnosticCode.ApplicationNonCallable
                    None
                    $"Expression of type '{calleeTypeText}' is not callable."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "application-expression-not-callable")
                          field "callee-type-text" (DiagnosticPayloadText calleeTypeText) ])
            | OverloadedTraitMemberUnresolvedForArgumentTypes(memberName, traitOwners, argumentTypes) ->
                let traitOwnerText = String.concat ", " traitOwners
                let argumentTypeText = String.concat ", " argumentTypes

                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Overloaded trait member '{memberName}' from [{traitOwnerText}] could not be resolved for argument types '{argumentTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "overloaded-trait-member-unresolved-for-argument-types")
                          field "member-name" (DiagnosticPayloadText memberName)
                          field "trait-owners" (DiagnosticPayloadTextList traitOwners)
                          field "argument-types" (DiagnosticPayloadTextList argumentTypes) ])
            | MemberAccessNotWellFormedForReceiverType(memberName, receiverTypeText) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Member access '.{memberName}' is not well-formed for receiver type '{receiverTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "member-access-not-well-formed-for-receiver-type")
                          field "member-name" (DiagnosticPayloadText memberName)
                          field "receiver-type-text" (DiagnosticPayloadText receiverTypeText) ])
            | SafeNavigationRequiresKnownResultType memberText ->
                descriptor
                    DiagnosticCode.SafeNavigationAmbiguous
                    None
                    $"Safe-navigation `?.` requires knowing the result type of '{memberText}' to decide whether to wrap or flatten."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "safe-navigation-requires-known-result-type")
                          field "member-text" (DiagnosticPayloadText memberText) ])
            | SafeNavigationReceiverMustBeOption ->
                descriptor
                    DiagnosticCode.SafeNavigationReceiverNotOption
                    None
                    "Safe-navigation `?.` requires its receiver to have type `Option T` for some `T`."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "safe-navigation-receiver-must-be-option") ])
            | ElvisReceiverMustBeOption ->
                descriptor
                    DiagnosticCode.ElvisReceiverNotOption
                    None
                    "Elvis `?:` requires its left-hand side to have type `Option T` for some `T`."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "elvis-receiver-must-be-option") ])
            | QueryCarrierCannotDiscardMapMetadata ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "Query carrier is ill-formed for a map comprehension; Query cannot silently discard map metadata."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "query-carrier-cannot-discard-map-metadata") ])
            | QueryCarrierCannotDiscardSetMetadata ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "Query carrier is ill-formed for a set comprehension; Query cannot silently discard set metadata."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "query-carrier-cannot-discard-set-metadata") ])
            | QueryCarrierUseMismatch(expectedUse, inferredUse) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Query carrier expects a {expectedUse} query, but the comprehension infers {inferredUse}; use OnceQuery or an explicitly one-shot QueryCore carrier."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "query-carrier-use-mismatch")
                          field "expected-use" (DiagnosticPayloadText expectedUse)
                          field "inferred-use" (DiagnosticPayloadText inferredUse) ])
            | QueryCarrierCardinalityMismatch(inferredCardinality, expectedCardinality) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Query cardinality mismatch: inferred {inferredCardinality} cannot be checked as {expectedCardinality}."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "query-carrier-cardinality-mismatch")
                          field "inferred-cardinality" (DiagnosticPayloadText inferredCardinality)
                          field "expected-cardinality" (DiagnosticPayloadText expectedCardinality) ])
            | QueryCarrierItemQuantityMismatch(inferredQuantity, expectedQuantity) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Query item quantity mismatch: inferred item quantity '{inferredQuantity}' cannot be checked as '{expectedQuantity}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "query-carrier-item-quantity-mismatch")
                          field "inferred-quantity" (DiagnosticPayloadText inferredQuantity)
                          field "expected-quantity" (DiagnosticPayloadText expectedQuantity) ])
            | ComprehensionSinkItemTypeMismatch(expectedItemTypeText, actualItemTypeText) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Selected comprehension sink expects Item '{expectedItemTypeText}', but the yielded item type is '{actualItemTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "comprehension-sink-item-type-mismatch")
                          field "expected-item-type-text" (DiagnosticPayloadText expectedItemTypeText)
                          field "actual-item-type-text" (DiagnosticPayloadText actualItemTypeText) ])
            | ArithmeticOperandResolvesToCallableBinding(operandLabel, bindingName) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Arithmetic operator {operandLabel} must be a numeric value, but '{bindingName}' resolves to a callable binding."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "arithmetic-operand-resolves-to-callable-binding")
                          field "operand-label" (DiagnosticPayloadText operandLabel)
                          field "binding-name" (DiagnosticPayloadText bindingName) ])
            | ArithmeticOperandsMustBeNumeric(leftTypeText, rightTypeText) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Arithmetic operator operands must both be numeric, but found '{leftTypeText}' and '{rightTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "arithmetic-operands-must-be-numeric")
                          field "left-type-text" (DiagnosticPayloadText leftTypeText)
                          field "right-type-text" (DiagnosticPayloadText rightTypeText) ])
            | ShortCircuitOperandMustBeSuspendedBool(operandLabel, operandTypeText) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Short-circuit operator {operandLabel} must match the expected suspended Bool operand type, but found '{operandTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "short-circuit-operand-must-be-suspended-bool")
                          field "operand-label" (DiagnosticPayloadText operandLabel)
                          field "operand-type-text" (DiagnosticPayloadText operandTypeText) ])
            | ObjectPhaseValueCannotBePassedDirectlyToElaboration name ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Object-phase/runtime value '{name}' cannot be passed directly to elaboration. Quote it as Syntax or rebind it inside Elab."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "object-phase-value-cannot-be-passed-directly-to-elaboration")
                          field "name" (DiagnosticPayloadText name) ])
            | QuoteSpliceRequiresSyntax actualTypeText ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Quote splice `${{...}}` requires `Syntax t`, but found '{actualTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "quote-splice-requires-syntax")
                          field "actual-type-text" (DiagnosticPayloadText actualTypeText) ])
            | QuoteSpliceRequiresSyntaxNotElabSyntax ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "Quote splice `${...}` requires `Syntax t`; `Elab (Syntax t)` is not run implicitly inside a quote."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "quote-splice-requires-syntax-not-elab-syntax") ])
            | TopLevelSpliceRequiresSyntaxOrElabSyntax actualTypeText ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Top-level splice `$(...)` requires `Syntax t` or `Elab (Syntax t)`, but found '{actualTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "top-level-splice-requires-syntax-or-elab-syntax")
                          field "actual-type-text" (DiagnosticPayloadText actualTypeText) ])
            | DoBindRequiresBindableCarrier sourceTypeText ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"A '<-' do binding requires a bindable carrier value, but found '{sourceTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "do-bind-requires-bindable-carrier")
                          field "source-type-text" (DiagnosticPayloadText sourceTypeText) ])
            | HandlerClausesMustReturnCarrierResultType actualTypeText ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Handler clauses must return a carrier-applied result type 'm b', but inferred '{actualTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "handler-clauses-must-return-carrier-result-type")
                          field "actual-type-text" (DiagnosticPayloadText actualTypeText) ])
            | HandlerEffectRowMissingLabel(effectName, effectRowText) ->
                descriptor
                    DiagnosticCode.HandlerEffectRowMismatch
                    None
                    $"Handler for '{effectName}' expects the handled computation to carry label '{effectName}' in its effect row, but found '{effectRowText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "handler-effect-row-missing-label")
                          field "effect-name" (DiagnosticPayloadText effectName)
                          field "effect-row-text" (DiagnosticPayloadText effectRowText) ])
            | HandlerEffectRowNeedsTailRefinement(effectName, effectRowText) ->
                descriptor
                    DiagnosticCode.HandlerEffectRowMismatch
                    None
                    $"Handler for '{effectName}' cannot split label '{effectName}' out of open effect row '{effectRowText}' without explicit row refinement. Mention '{effectName}' explicitly in the handled computation type."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "handler-effect-row-needs-tail-refinement")
                          field "effect-name" (DiagnosticPayloadText effectName)
                          field "effect-row-text" (DiagnosticPayloadText effectRowText) ])
            | HandlerHandledExpressionMustBeEff handledTypeText ->
                descriptor
                    DiagnosticCode.HandlerEffectRowMismatch
                    None
                    $"Handler expects an Eff computation, but the handled expression has type '{handledTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "handler-handled-expression-must-be-eff")
                          field "handled-type-text" (DiagnosticPayloadText handledTypeText) ])
            | HandlerMissingOperationClause(effectName, operationName) ->
                descriptor
                    DiagnosticCode.HandlerClauseMissing
                    None
                    $"Handler for '{effectName}' is missing a clause for operation '{operationName}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "handler-missing-operation-clause")
                          field "effect-name" (DiagnosticPayloadText effectName)
                          field "operation-name" (DiagnosticPayloadText operationName) ])
            | HandlerDuplicateOperationClause(effectName, operationName) ->
                descriptor
                    DiagnosticCode.HandlerClauseDuplicate
                    None
                    $"Handler for '{effectName}' defines more than one clause for operation '{operationName}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "handler-duplicate-operation-clause")
                          field "effect-name" (DiagnosticPayloadText effectName)
                          field "operation-name" (DiagnosticPayloadText operationName) ])
            | HandlerUnexpectedOperationClause(effectName, operationName) ->
                descriptor
                    DiagnosticCode.HandlerClauseUnexpected
                    None
                    $"Handler for '{effectName}' defines a clause for undeclared operation '{operationName}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "handler-unexpected-operation-clause")
                          field "effect-name" (DiagnosticPayloadText effectName)
                          field "operation-name" (DiagnosticPayloadText operationName) ])
            | HandlerOperationClauseArityMismatch(operationName, actualArgumentCount, expectedArgumentCount) ->
                descriptor
                    DiagnosticCode.HandlerClauseArityMismatch
                    None
                    $"Handler clause for operation '{operationName}' binds {actualArgumentCount} argument(s), but the effect declaration requires {expectedArgumentCount}."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "handler-operation-clause-arity-mismatch")
                          field "operation-name" (DiagnosticPayloadText operationName)
                          field "actual-argument-count" (DiagnosticPayloadText(string actualArgumentCount))
                          field "expected-argument-count" (DiagnosticPayloadText(string expectedArgumentCount)) ])
            | HandlerRelevantResumptionUnused(operationName, resumptionName) ->
                descriptor
                    DiagnosticCode.QttLinearDrop
                    None
                    $"Handler clause for operation '{operationName}' must use relevant resumption '{resumptionName}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "handler-relevant-resumption-unused")
                          field "operation-name" (DiagnosticPayloadText operationName)
                          field "resumption-name" (DiagnosticPayloadText resumptionName) ])
            | ConstructorDefaultReferencesUnavailableBinder(parameterName, referencedName) ->
                descriptor
                    DiagnosticCode.NameUnresolved
                    None
                    $"Default expression for constructor parameter '{parameterName}' cannot reference '{referencedName}' before it is bound."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "constructor-default-references-unavailable-binder")
                          field "parameter-name" (DiagnosticPayloadText parameterName)
                          field "referenced-name" (DiagnosticPayloadText referencedName) ])
            | ConstructorDefaultTypeMismatch(parameterName, actualTypeText, expectedTypeText) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Default expression for constructor parameter '{parameterName}' has type '{actualTypeText}', but '{expectedTypeText}' was expected."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "constructor-default-type-mismatch")
                          field "parameter-name" (DiagnosticPayloadText parameterName)
                          field "actual-type-text" (DiagnosticPayloadText actualTypeText)
                          field "expected-type-text" (DiagnosticPayloadText expectedTypeText) ])
            | ConstructorDefaultCouldNotBeChecked parameterName ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Default expression for constructor parameter '{parameterName}' could not be checked against its declared type."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "constructor-default-could-not-be-checked")
                          field "parameter-name" (DiagnosticPayloadText parameterName) ])
            | BorrowedEffectResumptionQuantityUnsupported(effectOperationName, quantityText) ->
                descriptor
                    DiagnosticCode.EffectResumptionQuantityBorrowed
                    None
                    $"Effect operation '{effectOperationName}' declares borrowed resumption quantity '{quantityText}'. Resumption quantities quantify the handler-bound resumption value itself, and borrowed resumptions are not part of the language."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "borrowed-effect-resumption-quantity-unsupported")
                          field "effect-operation-name" (DiagnosticPayloadText effectOperationName)
                          field "quantity-text" (DiagnosticPayloadText quantityText) ])
            | ParameterNumericContextTypeMismatch(parameterName, requiredTypeText, declaredTypeText) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Parameter '{parameterName}' is used in a numeric context that requires '{requiredTypeText}', but its declared type is '{declaredTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "parameter-numeric-context-type-mismatch")
                          field "parameter-name" (DiagnosticPayloadText parameterName)
                          field "required-type-text" (DiagnosticPayloadText requiredTypeText)
                          field "declared-type-text" (DiagnosticPayloadText declaredTypeText) ])
            | ConstructorFieldProjectionRequiresUniqueRefinement projectionText ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Constructor-field projection '{projectionText}' requires a unique constructor refinement in this branch."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "constructor-field-projection-requires-unique-refinement")
                          field "projection-text" (DiagnosticPayloadText projectionText) ])
            | ListConsDefinitionBodyRequiresListResultType declaredResultTypeText ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Definition body uses list-cons syntax and therefore must have a List result type, but the declared result type is '{declaredResultTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "list-cons-definition-body-requires-list-result-type")
                          field "declared-result-type-text" (DiagnosticPayloadText declaredResultTypeText) ])
            | StaticObjectOrTypeValueCannotSatisfyDefinitionBody ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "A type or static object value cannot satisfy this definition body."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "static-object-or-type-value-cannot-satisfy-definition-body") ])
            | MacroExpandedSyntaxOverusesLinearBinder binderName ->
                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"Macro-expanded syntax uses linear binder '{binderName}' more than once."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "macro-expanded-syntax-overuses-linear-binder")
                          field "binder-name" (DiagnosticPayloadText binderName) ])
            | DeclaredTypeInferredTypeMismatch(context, declaredTypeText, inferredTypeText) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"{context}: declared '{declaredTypeText}' but inferred '{inferredTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "declared-type-inferred-type-mismatch")
                          field "context" (DiagnosticPayloadText context)
                          field "declared-type-text" (DiagnosticPayloadText declaredTypeText)
                          field "inferred-type-text" (DiagnosticPayloadText inferredTypeText) ])
            | OrPatternBinderTypesDisagree(binderName, firstTypeText, alternativeTypeText) ->
                descriptor
                    DiagnosticCode.OrPatternBinderTypeMismatch
                    None
                    $"Binder '{binderName}' has type '{firstTypeText}' in one or-pattern alternative but '{alternativeTypeText}' in another."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "or-pattern-binder-types-disagree")
                          field "binder-name" (DiagnosticPayloadText binderName)
                          field "first-type-text" (DiagnosticPayloadText firstTypeText)
                          field "alternative-type-text" (DiagnosticPayloadText alternativeTypeText) ])
            | InstanceMemberReturnTypeMismatch(traitName, memberName, expectedReturnTypeText, declaredReturnTypeText) ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Instance member '{memberName}' for trait '{traitName}' must return '{expectedReturnTypeText}', but declares '{declaredReturnTypeText}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "instance-member-return-type-mismatch")
                          field "trait-name" (DiagnosticPayloadText traitName)
                          field "member-name" (DiagnosticPayloadText memberName)
                          field "expected-return-type-text" (DiagnosticPayloadText expectedReturnTypeText)
                          field "declared-return-type-text" (DiagnosticPayloadText declaredReturnTypeText) ])
            | MultishotEffectUnsupportedBackendForExportedDeclaration(backendProfile, effectVisibleName, operationName, bindingName) ->
                descriptor
                    DiagnosticCode.MultishotEffectUnsupportedBackend
                    None
                    $"Backend profile '{backendProfile}' does not provide capability 'rt-multishot-effects' required by multi-shot operation '{effectVisibleName}.{operationName}' from exported declaration '{bindingName}'."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "multishot-effect-unsupported-backend-for-exported-declaration")
                          field "backend-profile" (DiagnosticPayloadText backendProfile)
                          field "effect-visible-name" (DiagnosticPayloadText effectVisibleName)
                          field "operation-name" (DiagnosticPayloadText operationName)
                          field "binding-name" (DiagnosticPayloadText bindingName) ])
            | RefutableGeneratorRequiresForQuestion ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "Refutable generator patterns must use 'for?'; plain 'for' requires an irrefutable pattern."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "refutable-generator-requires-for-question") ])
            | LeftJoinBinderNotInScopeAfterClause(bindingName, intoName) ->
                descriptor
                    DiagnosticCode.NameUnresolved
                    None
                    $"Left-join binder '{bindingName}' is not in scope after the clause; only '{intoName}' remains available."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "left-join-binder-not-in-scope-after-clause")
                          field "binding-name" (DiagnosticPayloadText bindingName)
                          field "into-name" (DiagnosticPayloadText intoName) ])
            | TraitConstraintUnresolved constraintText ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Trait constraint '{constraintText}' could not be resolved."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "trait-constraint-unresolved")
                          field "constraint-text" (DiagnosticPayloadText constraintText) ])
            | ImplicitTraitConstraintUnresolved constraintText ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Implicit trait constraint '{constraintText}' could not be resolved."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "implicit-trait-constraint-unresolved")
                          field "constraint-text" (DiagnosticPayloadText constraintText) ])
            | TraitConstraintAmbiguous(constraintText, candidateTexts) ->
                let candidateSummary = String.concat ", " candidateTexts
                descriptor
                    DiagnosticCode.TraitInstanceAmbiguous
                    None
                    $"Multiple instance candidates survive for constraint '{constraintText}': {candidateSummary}."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "trait-constraint-ambiguous")
                          field "constraint-text" (DiagnosticPayloadText constraintText)
                          field "candidate-texts" (DiagnosticPayloadTextList candidateTexts) ])
            | TraitSupertraitEvidenceUnsatisfied(traitName, requiredConstraintText) ->
                descriptor
                    DiagnosticCode.TraitSupertraitUnsatisfied
                    None
                    $"Instance for trait '{traitName}' requires declared supertrait '{requiredConstraintText}', but that evidence is not satisfied by the instance premises or visible instances."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "trait-supertrait-evidence-unsatisfied")
                          field "trait-name" (DiagnosticPayloadText traitName)
                          field "required-constraint-text" (DiagnosticPayloadText requiredConstraintText) ])
            | SyntaxCarrierEscapesCapturedBindings escapedNames ->
                let escapedText = String.concat ", " escapedNames

                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Syntax value captures binder(s) '{escapedText}' that do not remain in scope at the binding site; captured locals and borrows cannot escape through Syntax."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "syntax-carrier-escapes-captured-bindings")
                          field "escaped-names" (DiagnosticPayloadTextList escapedNames) ])
            | PrefixedStringPrefixMustResolveToInterpolatedMacro prefix ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    $"Prefixed string prefix '{prefix}' must resolve to a term of type 'Elab (InterpolatedMacro t)' for some 't', or to an elaboration-time trait evidence value."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "prefixed-string-prefix-must-resolve-to-interpolated-macro")
                          field "prefix" (DiagnosticPayloadText prefix) ])
            | ProjectedFieldTypeDoesNotMatchDeclaredResultType ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "Projected field type does not match the declared result type."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "projected-field-type-does-not-match-declared-result-type") ])
            | ConstructorTermCannotSatisfyTypeValuedDefinitionBody ->
                descriptor
                    DiagnosticCode.TypeEqualityMismatch
                    None
                    "A data constructor term cannot satisfy a Type-valued definition body; use an explicit kind-qualified type object when the type facet is intended."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "constructor-term-cannot-satisfy-type-valued-definition-body") ])
            | ElaborationFailureFromFailElabWith(errorCode, elaborationMessage) ->
                descriptor
                    DiagnosticCode.ElaborationFailed
                    None
                    $"{errorCode}: {elaborationMessage}"
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "elaboration-failure-from-fail-elab-with")
                          field "error-code" (DiagnosticPayloadText errorCode)
                          field "elaboration-message" (DiagnosticPayloadText elaborationMessage) ])
            | StaticConstructorRequiresPreservedStaticObjectIdentity memberName ->
                descriptor
                    DiagnosticCode.StaticObjectUnresolved
                    None
                    $"Static constructor '{memberName}' requires a receiver with preserved static-object identity."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "static-constructor-requires-preserved-static-object-identity")
                          field "member-name" (DiagnosticPayloadText memberName) ])
            | StaticObjectUnresolvedByKind(kindSelector, nameText) ->
                descriptor
                    DiagnosticCode.StaticObjectUnresolved
                    None
                    $"No {kindSelector} static object named '{nameText}' is visible."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "static-object-unresolved-by-kind")
                          field "kind-selector" (DiagnosticPayloadText kindSelector)
                          field "name-text" (DiagnosticPayloadText nameText) ])
            | PatternHeadResolvedToOrdinaryTerm headName ->
                descriptor
                    DiagnosticCode.PatternHeadNotConstructorOrActivePattern
                    None
                    $"Pattern head '{headName}' resolves to an ordinary term, not a constructor or active pattern."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "pattern-head-resolved-to-ordinary-term")
                          field "head-name" (DiagnosticPayloadText headName) ])
            | ActivePatternLinearlyConsumesScrutineeInRefutableContext(patternName, context) ->
                descriptor
                    DiagnosticCode.ActivePatternLinearityViolation
                    None
                    $"Option-returning active pattern '{patternName}' consumes its scrutinee linearly in a refutable {context}."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "active-pattern-linearly-consumes-scrutinee-in-refutable-context")
                          field "pattern-name" (DiagnosticPayloadText patternName)
                          field "context" (DiagnosticPayloadText context) ])
            | MatchReturningActivePatternNotPermittedInPlainLetQuestion patternName ->
                descriptor
                    DiagnosticCode.ActivePatternMatchResultNotAllowedInPlainLetQuestion
                    None
                    $"Match-returning active pattern '{patternName}' is not permitted in plain let? destructuring."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "match-returning-active-pattern-not-permitted-in-plain-let-question")
                          field "pattern-name" (DiagnosticPayloadText patternName) ])
            | ActivePatternDeclarationRequiresExplicitScrutineeBinder ->
                descriptor
                    DiagnosticCode.ActivePatternMissingScrutineeBinder
                    None
                    "An active pattern declaration must have at least one explicit binder for the scrutinee."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "active-pattern-declaration-requires-explicit-scrutinee-binder") ])
            | ActivePatternDeclarationResultMustNotBeMonadic ->
                descriptor
                    DiagnosticCode.ActivePatternMonadicResult
                    None
                    "An active pattern declaration result type must not be monadic."
                    (payload
                        "surface-elaboration-diagnostic"
                        [ field "reason" (DiagnosticPayloadText "active-pattern-declaration-result-must-not-be-monadic") ])
        | ParserSyntaxDiagnostic evidence ->
            match evidence with
            | ExpectedKeyword keyword ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Expected '{keyword}'."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-keyword")
                          field "keyword" (DiagnosticPayloadText keyword) ])
            | ExpectedName role ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Expected a {parserNameRoleText role}."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-name")
                          field "name-role" (DiagnosticPayloadText(parserNameRoleText role)) ])
            | InvalidStringLiteral error ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"String literal text is invalid: {stringLiteralDecodeErrorMessage error}."
                    (payload
                        "parser-syntax"
                        ([ field "reason" (DiagnosticPayloadText "invalid-string-literal") ]
                         @ stringLiteralDecodeErrorFields error))
            | InvalidUrlModuleSpecifier(specifierText, error) ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"URL module specifier '{specifierText}' is invalid: {urlModuleSpecifierParseErrorMessage error}."
                    (payload
                        "parser-syntax"
                        ([ field "reason" (DiagnosticPayloadText "invalid-url-module-specifier")
                           field "specifier-text" (DiagnosticPayloadText specifierText) ]
                         @ urlModuleSpecifierParseErrorFields error))
            | ExpectedColonInExpectTermDeclaration ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected ':' in the expect term declaration."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-colon-in-expect-term-declaration") ])
            | ExpectedColonInSignatureDeclaration ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected ':' in the signature declaration."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-colon-in-signature-declaration") ])
            | ExpectedListStart context ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Expected '(' to start the {parserListContextText context}."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-list-start")
                          field "list-context" (DiagnosticPayloadText(parserListContextText context)) ])
            | ExpectedListClose context ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Expected ')' to close the {parserListContextText context}."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-list-close")
                          field "list-context" (DiagnosticPayloadText(parserListContextText context)) ])
            | ExpectedListComma context ->
                let message =
                    match context with
                    | NameList -> "Expected ',' between list items."
                    | ExclusionList -> "Expected ',' between excluded import items."
                    | ImportItemList -> "Expected ',' between import items."

                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    message
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-list-comma")
                          field "list-context" (DiagnosticPayloadText(parserListContextText context)) ])
            | DuplicateImportItemModifier modifierKeyword ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Duplicate '{modifierKeyword}' modifier in import item."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "duplicate-import-item-modifier")
                          field "modifier-keyword" (DiagnosticPayloadText modifierKeyword) ])
            | ExpectedImportItemAfterDot ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected '*', '(...)', or a singleton item after '.'."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-import-item-after-dot") ])
            | CtorAllCannotBeCombinedWithAlias ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "ctorAll may not be combined with an alias."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "ctor-all-cannot-be-combined-with-alias") ])
            | MultipleTotalityAssertionsOnSameDeclaration ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Multiple totality assertions cannot be applied to the same declaration."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "multiple-totality-assertions-on-same-declaration") ])
            | ExpectedExpectDeclarationKind ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected 'type', 'trait', or 'term' after 'expect'."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-expect-declaration-kind") ])
            | ExpectedFixityDeclaration ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a fixity declaration."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-fixity-declaration") ])
            | ExpectedNumericPrecedenceInFixityDeclaration ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a numeric precedence in the fixity declaration."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-numeric-precedence-in-fixity-declaration") ])
            | ExpectedValidIntegerPrecedenceInFixityDeclaration ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a valid integer precedence in the fixity declaration."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-valid-integer-precedence-in-fixity-declaration") ])
            | ExpectedFixityOperatorLeftParenthesis ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected '(' before the operator token in the fixity declaration."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-fixity-operator-left-parenthesis") ])
            | ExpectedFixityOperatorToken ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected an operator token in the fixity declaration."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-fixity-operator-token") ])
            | ExpectedFixityOperatorRightParenthesis ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected ')' after the operator token in the fixity declaration."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-fixity-operator-right-parenthesis") ])
            | ExpectedTopLevelDeclaration ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a top-level declaration."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-top-level-declaration") ])
            | ExpectedTraitHeadAfterTrait ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a trait head after 'trait'."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-trait-head-after-trait") ])
            | ExpectedTraitMemberSignatureOrDefaultDefinition ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a trait member signature or a default member definition."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-trait-member-signature-or-default-definition") ])
            | ExpectedTypeAliasBody ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a type alias body."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-type-alias-body") ])
            | ExpectedValidTypeAliasBody ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a valid type alias body."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-valid-type-alias-body") ])
            | ExpectedSignatureType ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a signature type."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-signature-type") ])
            | ExpectedValidSignatureType ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a valid signature type."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-valid-signature-type") ])
            | ExpectedConstructorName ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a constructor name."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-constructor-name") ])
            | UnsupportedConstructorParameterSyntax ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Unsupported constructor parameter syntax."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "unsupported-constructor-parameter-syntax") ])
            | ExpectedEffectOperationSignatureShape ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected an effect operation signature of the form 'op : ...'."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-effect-operation-signature-shape") ])
            | ExpectedEffectOperationSignature ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected an effect operation signature."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-effect-operation-signature") ])
            | ExpectedEqualsInDeclaration subject ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Expected '=' in the {parserDeclarationSubjectText subject}."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-equals-in-declaration")
                          field "declaration-subject" (DiagnosticPayloadText(parserDeclarationSubjectText subject)) ])
            | ExpectedIndentedBlock ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected an indented block."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-indented-block") ])
            | ExpectedDeclarationBlockClosure delimiter ->
                let message =
                    match delimiter with
                    | ParserDedentDelimiter -> "Expected the declaration block to dedent."
                    | _ -> $"Expected {parserDeclarationBlockDelimiterText delimiter} to close the declaration block item."

                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    message
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-declaration-block-closure")
                          field "delimiter" (DiagnosticPayloadText(parserDeclarationBlockDelimiterText delimiter)) ])
            | ExpectedValidInstanceHeadAfterInstance ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a valid instance head after 'instance'."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "expected-valid-instance-head-after-instance") ])
            | ExpectedInstanceMemberDefinitionStartingWithLet ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected an instance member definition starting with 'let'."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "expected-instance-member-definition-starting-with-let") ])
            | UnexpectedIndentationAtTopLevel ->
                descriptor
                    DiagnosticCode.UnexpectedIndentation
                    None
                    "Unexpected indentation."
                    (payload "parser-syntax" [ field "reason" (DiagnosticPayloadText "unexpected-indentation-at-top-level") ])
            | ModuleHeaderExpectedAfterTopLevelAttributes ->
                descriptor
                    DiagnosticCode.ModuleHeaderExpectedAfterAttributes
                    None
                    "Top-level module attributes must be followed immediately by a module header."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "module-header-expected-after-top-level-attributes") ])
            | ModuleHeaderMisplacedAfterTopLevelItems ->
                descriptor
                    DiagnosticCode.ModuleHeaderMisplaced
                    None
                    "A module header may appear only once and must be the first top-level item in the file after any leading module attributes."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "module-header-misplaced-after-top-level-items") ])
            | TotalityAssertionNotApplicableTo target ->
                let targetText = parserModifierTargetText target

                let message =
                    match target with
                    | TermDeclarationsOnly -> "Totality assertions currently apply only to term declarations."
                    | _ -> $"Totality assertions do not apply to {targetText}."

                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    message
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "totality-assertion-not-applicable-to")
                          field "target" (DiagnosticPayloadText targetText) ])
            | OpacityModifierNotApplicableToProjectionDeclaration ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Opacity modifiers do not apply to projection declarations."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "opacity-modifier-not-applicable-to-projection-declaration") ])
            | OpacityAndTotalityNotApplicableToEffectDeclaration ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Opacity and totality assertions do not apply to effect declarations."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "opacity-and-totality-not-applicable-to-effect-declaration") ])
            | VisibilityOpacityAndTotalityNotApplicableToInstanceDeclaration ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Visibility, opacity, and totality modifiers do not apply to instance declarations."
                    (payload
                        "parser-syntax"
                        [ field "reason" (DiagnosticPayloadText "visibility-opacity-and-totality-not-applicable-to-instance-declaration") ])
        | CorePatternParsingDiagnostic evidence ->
            match evidence with
            | UnsupportedParameterBinderSyntax ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Unsupported parameter binder syntax."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "unsupported-parameter-binder-syntax") ])
            | ExpectedParameterBinder ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a parameter binder."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-parameter-binder") ])
            | ExpectedParameterName ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a parameter name."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-parameter-name") ])
            | ExpectedPatternName ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a pattern name."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-pattern-name") ])
            | OrPatternAlternativesMustBindSameNames ->
                descriptor
                    DiagnosticCode.OrPatternBinderMismatch
                    None
                    "Each or-pattern alternative must bind the same set of names."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "or-pattern-alternatives-must-bind-same-names") ])
            | NumericLiteralSuffixesNotPermittedInPatterns tokenText ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Numeric literal suffixes are not permitted in patterns: '{tokenText}'."
                    (payload
                        "core-pattern-parsing"
                        [ field "reason" (DiagnosticPayloadText "numeric-literal-suffixes-not-permitted-in-patterns")
                          field "token-text" (DiagnosticPayloadText tokenText) ])
            | NumericLiteralNotRepresentableInPatterns tokenText ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Numeric literal '{tokenText}' is not representable in patterns."
                    (payload
                        "core-pattern-parsing"
                        [ field "reason" (DiagnosticPayloadText "numeric-literal-not-representable-in-patterns")
                          field "token-text" (DiagnosticPayloadText tokenText) ])
            | ExpectedLiteralPattern ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a literal pattern."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-literal-pattern") ])
            | ExpectedPatternCloseParenthesis ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected ')' to close the pattern."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-pattern-close-parenthesis") ])
            | ExpectedRecordPatternFieldLabel ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a record pattern field label."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-record-pattern-field-label") ])
            | ExpectedRecordPatternField ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a record pattern field of the form 'name = pattern'."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-record-pattern-field") ])
            | ExpectedPatternCloseBrace ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected '}' to close the pattern."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-pattern-close-brace") ])
            | ExpectedNamedConstructorPatternFieldLabel ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a named constructor pattern field label."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-named-constructor-pattern-field-label") ])
            | ExpectedNamedConstructorPatternField ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a named constructor pattern field."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-named-constructor-pattern-field") ])
            | ExpectedPattern ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a pattern."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "expected-pattern") ])
            | NamedConstructorPatternsCannotTakePositionalSubpatterns ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Named constructor patterns cannot take positional subpatterns."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "named-constructor-patterns-cannot-take-positional-subpatterns") ])
            | OnlyConstructorPatternsMayTakeArguments ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Only constructor patterns may take arguments."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "only-constructor-patterns-may-take-arguments") ])
            | UnexpectedTrailingPatternTokens ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Unexpected tokens at the end of the pattern."
                    (payload "core-pattern-parsing" [ field "reason" (DiagnosticPayloadText "unexpected-trailing-pattern-tokens") ])
            | InvalidStringLiteralPattern error ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"String literal pattern text is invalid: {stringLiteralDecodeErrorMessage error}."
                    (payload
                        "core-pattern-parsing"
                        ([ field "reason" (DiagnosticPayloadText "invalid-string-literal-pattern") ]
                         @ stringLiteralDecodeErrorFields error))
            | InvalidNumericLiteralPattern error ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Numeric literal pattern text is invalid: '{numericLiteralParseErrorText error}'."
                    (payload
                        "core-pattern-parsing"
                        [ field "reason" (DiagnosticPayloadText "invalid-numeric-literal-pattern")
                          field "numeric-literal-error" (DiagnosticPayloadText(numericLiteralParseErrorTag error))
                          field "token-text" (DiagnosticPayloadText(numericLiteralParseErrorText error)) ])
        | CoreExpressionParsingDiagnostic evidence ->
            match evidence with
            | UnterminatedParameterBinder ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Unterminated parameter binder."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "unterminated-parameter-binder") ])
            | ExpectedLambdaParameterOrArrow ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a lambda parameter or '->'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-lambda-parameter-or-arrow") ])
            | LambdaMustDeclareAtLeastOneParameter ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "A lambda must declare at least one parameter."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "lambda-must-declare-at-least-one-parameter") ])
            | ExpectedDoBlockDedent ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected the do block to dedent."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-do-block-dedent") ])
            | ExpectedCaseClauseArrow ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '->' in the case clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-case-clause-arrow") ])
            | ExpectedIndentedExpressionDedent ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected the indented expression to dedent."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-indented-expression-dedent") ])
            | ExplicitBracesAfterLayoutIntroducedBlockForbidden ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Explicit braces are not permitted after a layout-introduced block."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "explicit-braces-after-layout-introduced-block-forbidden") ])
            | UnexpectedIndentationInIndentedCaseBody ->
                descriptor DiagnosticCode.UnexpectedIndentation None "Unexpected indentation."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "unexpected-indentation-in-indented-case-body") ])
            | MatchExpressionMustDeclareAtLeastOneCase ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "A match expression must declare at least one case."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "match-expression-must-declare-at-least-one-case") ])
            | ExpectedMatchCasesDedent ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected the match cases to dedent."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-match-cases-dedent") ])
            | ExpectedWhileBodyDedent ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected the while body to dedent."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-while-body-dedent") ])
            | ExpectedLetQuestionFailureArrow ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '->' in the let? failure arm."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-let-question-failure-arrow") ])
            | ExpectedLetQuestionEquals ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '=' in the let? binding."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-let-question-equals") ])
            | ExpectedDoIfThen ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'then' in the do if statement."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-do-if-then") ])
            | ExpectedExpressionAfterDefer ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected an expression after 'defer'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-expression-after-defer") ])
            | ExpectedUsingBindingPattern ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a pattern in the using binding."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-using-binding-pattern") ])
            | ExpectedUsingBindingArrow ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '<-' in the using binding."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-using-binding-arrow") ])
            | UnexpectedIndentedExpressionContinuationInDoBinding ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Unexpected indented expression continuation in the do binding."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "unexpected-indented-expression-continuation-in-do-binding") ])
            | ExpectedDoBindingAssignmentOrBind ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '=' or '<-' in the do binding."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-do-binding-assignment-or-bind") ])
            | ExpectedWhileDo ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'do' in the while statement."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-while-do") ])
            | DoBlockMustContainAtLeastOneStatement ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "A do block must contain at least one statement."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "do-block-must-contain-at-least-one-statement") ])
            | ExpectedLambdaBodyArrow ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '->' after the lambda parameters."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-lambda-body-arrow") ])
            | PureBlockMustContainExpression context ->
                let message =
                    match context with
                    | BlockExpressionBody -> "A pure block must contain at least one expression."
                    | LambdaPureBody -> "A lambda pure block must contain at least one expression."

                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    message
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "pure-block-must-contain-expression")
                          field "block-context" (DiagnosticPayloadText(corePureBlockContextPayloadText context)) ])
            | ExpectedPureBlockFinalExpression context ->
                let message =
                    match context with
                    | BlockExpressionBody -> "Expected a sequence of local declarations followed by a final expression in the block."
                    | LambdaPureBody -> "Expected a sequence of local declarations followed by a final expression in the lambda body."

                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    message
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "expected-pure-block-final-expression")
                          field "block-context" (DiagnosticPayloadText(corePureBlockContextPayloadText context)) ])
            | InvalidStringLiteralExpression error ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"String literal text is invalid: {stringLiteralDecodeErrorMessage error}."
                    (payload
                        "core-expression-parsing"
                        ([ field "reason" (DiagnosticPayloadText "invalid-string-literal-expression") ]
                         @ stringLiteralDecodeErrorFields error))
            | InvalidNumericLiteralExpression error ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Numeric literal text is invalid: '{numericLiteralParseErrorText error}'."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "invalid-numeric-literal-expression")
                          field "numeric-literal-error" (DiagnosticPayloadText(numericLiteralParseErrorTag error))
                          field "token-text" (DiagnosticPayloadText(numericLiteralParseErrorText error)) ])
            | InvalidStringTextSegment error ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"String text segment is invalid: {stringLiteralDecodeErrorMessage error}."
                    (payload
                        "core-expression-parsing"
                        ([ field "reason" (DiagnosticPayloadText "invalid-string-text-segment") ]
                         @ stringLiteralDecodeErrorFields error))
            | ExpectedNamedGroupAggregation ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a named aggregation of the form 'name = expr' in the group clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-named-group-aggregation") ])
            | ExpectedGroupAggregationEquals ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '=' in the group aggregation."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-group-aggregation-equals") ])
            | ExpectedComprehensionGeneratorIn ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'in' in the comprehension generator."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-comprehension-generator-in") ])
            | ExpectedConflictCombineUsingQualifiedName ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a qualified name after 'on conflict combine using'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-conflict-combine-using-qualified-name") ])
            | ExpectedConflictClauseForm ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'on conflict keep last', 'on conflict keep first', 'on conflict combine using <name>', or 'on conflict combine with <expr>'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-conflict-clause-form") ])
            | ExpectedComprehensionLetEquals ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '=' in the comprehension let clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-comprehension-let-equals") ])
            | ExpectedComprehensionJoinClause ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'join <pat> in <source> on <condition>' in the comprehension clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-comprehension-join-clause") ])
            | ExpectedGroupByIntoName ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'into <name>' after the group aggregation block."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-group-by-into-name") ])
            | ExpectedGroupAggregationBlockClose ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '}' to close the group aggregation block."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-group-aggregation-block-close") ])
            | ExpectedGroupAggregationBlock ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '{ ... }' after 'group by <expr>'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-group-aggregation-block") ])
            | ExpectedLeftJoinIntoName ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a single binder name after 'into' in the left-join clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-left-join-into-name") ])
            | ExpectedLeftJoinClause ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'left join <pat> in <source> on <condition> into <name>' in the comprehension clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-left-join-clause") ])
            | UnsupportedComprehensionClause ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Unsupported comprehension clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "unsupported-comprehension-clause") ])
            | QueryPagingRequiresOrderedPipeline operationKind ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "skip and take require an ordered query pipeline; the current pipeline is unordered."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "query-paging-requires-ordered-pipeline")
                          field "operation-kind" (DiagnosticPayloadText operationKind) ])
            | ComprehensionMustEndWithYieldClause ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "A comprehension must end with a yield clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "comprehension-must-end-with-yield-clause") ])
            | ExpectedHandlerClauseArrow ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '->' in the handler clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-handler-clause-arrow") ])
            | ExpectedHandlerResumptionBinder ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a resumption binder in the handler clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-handler-resumption-binder") ])
            | ExpectedHandlerClauseHead ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a handler clause head after 'case'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-handler-clause-head") ])
            | ExpectedHandlerClauseStartingWithCase ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a handler clause starting with 'case'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-handler-clause-starting-with-case") ])
            | ExpectedHandlerClause ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a handler clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-handler-clause") ])
            | ExpectedHandlerWith ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'with' in the handler expression."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-handler-with") ])
            | ExpectedImplicitParameterAfterAt context ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Expected an implicit parameter after '@' in the {coreHeaderContextText context}."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "expected-implicit-parameter-after-at")
                          field "header-context" (DiagnosticPayloadText(coreHeaderContextPayloadText context)) ])
            | UnterminatedImplicitParameterBinder context ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Unterminated implicit parameter binder in the {coreHeaderContextText context}."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "unterminated-implicit-parameter-binder")
                          field "header-context" (DiagnosticPayloadText(coreHeaderContextPayloadText context)) ])
            | UnsupportedImplicitParameterSyntax context ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Unsupported implicit parameter syntax in the {coreHeaderContextText context}."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "unsupported-implicit-parameter-syntax")
                          field "header-context" (DiagnosticPayloadText(coreHeaderContextPayloadText context)) ])
            | UnterminatedParameterBinderInHeader context ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Unterminated parameter binder in the {coreHeaderContextText context}."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "unterminated-parameter-binder-in-header")
                          field "header-context" (DiagnosticPayloadText(coreHeaderContextPayloadText context)) ])
            | UnsupportedHeaderSyntax context ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    $"Unsupported {coreHeaderContextText context} syntax."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "unsupported-header-syntax")
                          field "header-context" (DiagnosticPayloadText(coreHeaderContextPayloadText context)) ])
            | ExpectedCoreKeyword keywordText ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None $"Expected '{keywordText}'."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "expected-core-keyword")
                          field "keyword-text" (DiagnosticPayloadText keywordText) ])
            | ExpectedQualifiedNameSegment ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a name."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-qualified-name-segment") ])
            | ExpectedNameAfterSelector selectorDescription ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None $"Expected a name after '{selectorDescription}'."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "expected-name-after-selector")
                          field "selector-description" (DiagnosticPayloadText selectorDescription) ])
            | ExpectedConstructorNameAfterIs ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a constructor name after 'is'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-constructor-name-after-is") ])
            | ExpectedRecordUpdateClose ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '}' to close the record update."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-record-update-close") ])
            | ExpectedExplicitMemberProjectionName ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected an operator or member name inside explicit member projection."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-explicit-member-projection-name") ])
            | ExpectedSafeNavigationMemberAccess ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a member access after '?.'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-safe-navigation-member-access") ])
            | ConstructorTagTestsCannotBeChained ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Constructor tag tests cannot be chained without parentheses."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "constructor-tag-tests-cannot-be-chained") ])
            | UnexpectedTrailingExpressionTokens ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Unexpected tokens at the end of the expression."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "unexpected-trailing-expression-tokens") ])
            | ExpectedNamedApplicationFieldLabel ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a named application field label."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-named-application-field-label") ])
            | ExpectedNamedApplicationField ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a named application field of the form 'name = expr' or a punned field name."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-named-application-field") ])
            | ExpectedRecordFieldLabel ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a record field label."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-record-field-label") ])
            | ExpectedRecordField ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a record field of the form 'name = expr' or a punned field name."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-record-field") ])
            | ExpectedSealAs ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'as' in the seal expression."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-seal-as") ])
            | ExpectedSealValue ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a value to seal."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-seal-value") ])
            | ExpectedProjectionBodyHead ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'yield', 'if', or 'match' in the projection body."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-projection-body-head") ])
            | ExpectedProjectionMatchCaseBlock ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected an indented case block in the projection match body."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-projection-match-case-block") ])
            | ExpectedProjectionMatchCaseClause ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected a 'case' clause in the projection match body."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-projection-match-case-clause") ])
            | ExpectedProjectionCaseClauseArrow ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '->' in the projection case clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-projection-case-clause-arrow") ])
            | ExpectedLocalLetIn ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'in' after the local let binding."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-local-let-in") ])
            | ExpectedLocalLetEquals ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '=' in the local let binding."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-local-let-equals") ])
            | ExpectedExpressionCloseParenthesis ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected ')' to close the expression."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-expression-close-parenthesis") ])
            | ExpectedExpression ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected an expression."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-expression") ])
            | ExpectedInterpolationEndBeforeStringResumes ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected the interpolation to end before the string resumes."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "expected-interpolation-end-before-string-resumes") ])
            | ExpectedInterpolatedStringContent ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected interpolated string content."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-interpolated-string-content") ])
            | UnterminatedInterpolatedString ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Unterminated interpolated string."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "unterminated-interpolated-string") ])
            | ExpectedIfThen ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'then' in the if expression."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-if-then") ])
            | ExpectedIfElse ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected 'else' in the if expression."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-if-else") ])
            | DuplicateHandlerReturnClause ->
                descriptor DiagnosticCode.HandlerClauseDuplicate None "A handler must not define more than one return clause."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "duplicate-handler-return-clause") ])
            | MissingHandlerReturnClause ->
                descriptor DiagnosticCode.HandlerClauseMissing None "A handler must define exactly one return clause of the form 'case return x -> ...'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "missing-handler-return-clause") ])
            | HandlerReturnClauseArityMismatch argumentCount ->
                descriptor
                    DiagnosticCode.HandlerClauseArityMismatch
                    None
                    $"A handler return clause must bind exactly one payload argument, but binds {argumentCount}."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "handler-return-clause-arity-mismatch")
                          field "argument-count" (DiagnosticPayloadText(string argumentCount)) ])
            | RecordPatchExtensionMustBeTopLevelLabel ->
                descriptor
                    DiagnosticCode.RecordPatchInvalidItem
                    None
                    "Row-extension fields must be top-level labels of the form 'name := expr'."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "record-patch-extension-must-be-top-level-label") ])
            | ExpectedRecordPatchPath ->
                descriptor
                    DiagnosticCode.RecordPatchInvalidItem
                    None
                    "Expected a record patch path before '=' or ':='."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-record-patch-path") ])
            | ExpectedRecordPatchItem ->
                descriptor
                    DiagnosticCode.RecordPatchInvalidItem
                    None
                    "Expected a record patch item of the form 'path = expr' or 'name := expr'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-record-patch-item") ])
            | ExpectedProjectionThen ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected 'then' in the projection body."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-projection-then") ])
            | ExpectedProjectionElse ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected 'else' in the projection body."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-projection-else") ])
            | ExpectedProjectionAccessorClause ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a projection accessor clause."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "expected-projection-accessor-clause") ])
            | ExpectedProjectionAccessorClauseArrow ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected '->' in the projection accessor clause."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "expected-projection-accessor-clause-arrow") ])
            | ExpectedProjectionSetAccessor ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a projection set accessor."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-projection-set-accessor") ])
            | ProjectionSetAccessorRequiresTypedParameter ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Projection set accessors require a typed parameter."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "projection-set-accessor-requires-typed-parameter") ])
            | ProjectionSetAccessorUsesOrdinaryParameter ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Projection set accessors use an ordinary '(name : Type)' parameter."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "projection-set-accessor-uses-ordinary-parameter") ])
            | ExpectedProjectionPlaceBinder ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a projection place binder of the form '(place name : Type)'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-projection-place-binder") ])
            | ProjectionParametersMustNotUseInout ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Projection parameters must not use 'inout'."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "projection-parameters-must-not-use-inout") ])
            | ExpectedProjectionResultTypeColon ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected ':' before the projection result type."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-projection-result-type-colon") ])
            | UnterminatedProjectionBinder ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Unterminated projection binder."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "unterminated-projection-binder") ])
            | UnsupportedProjectionBinderSyntax ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Unsupported projection binder syntax."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "unsupported-projection-binder-syntax") ])
            | ExpectedProjectionSetAccessorParameter ->
                descriptor
                    DiagnosticCode.ExpectedSyntaxToken
                    None
                    "Expected a projection set accessor parameter of the form '(name : Type)'."
                    (payload
                        "core-expression-parsing"
                        [ field "reason" (DiagnosticPayloadText "expected-projection-set-accessor-parameter") ])
            | ExpectedListExpressionClose ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected ']' to close the list expression."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-list-expression-close") ])
            | ExpectedSetExpressionClose ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '|}' to close the set expression."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-set-expression-close") ])
            | ExpectedMapExpressionClose ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '}' to close the map expression."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-map-expression-close") ])
            | ExpectedNamedApplicationBlockClose ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '}' to close the named application block."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-named-application-block-close") ])
            | ExpectedSyntaxQuoteClose ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '}' to close the syntax quote."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-syntax-quote-close") ])
            | ExpectedSyntaxSpliceClose ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '}' to close the syntax splice."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-syntax-splice-close") ])
            | ExpectedCodeQuoteClose ->
                descriptor DiagnosticCode.ExpectedSyntaxToken None "Expected '>.' to close the code quote."
                    (payload "core-expression-parsing" [ field "reason" (DiagnosticPayloadText "expected-code-quote-close") ])
        | UnicodeScalarLiteralDiagnostic evidence ->
            match evidence with
            | UnicodeScalarInvalidLiteralForm ->
                descriptor
                    DiagnosticCode.UnicodeInvalidScalarLiteral
                    None
                    "Unicode scalar literal is not a valid single-quoted literal."
                    (payload
                        "unicode-invalid-scalar-literal"
                        [ field "reason" (DiagnosticPayloadText "invalid-literal-form")
                          field "unicode-scalar-error" (DiagnosticPayloadText(unicodeScalarLiteralEvidenceTag evidence)) ])
            | UnicodeScalarTextInvalid error ->
                descriptor
                    DiagnosticCode.UnicodeInvalidScalarLiteral
                    None
                    $"Unicode scalar literal text is invalid: {stringLiteralDecodeErrorMessage error}."
                    (payload
                        "unicode-invalid-scalar-literal"
                        ([ field "reason" (DiagnosticPayloadText "unicode-scalar-text-invalid")
                           field "unicode-scalar-error" (DiagnosticPayloadText(unicodeScalarLiteralEvidenceTag evidence)) ]
                         @ stringLiteralDecodeErrorFields error))
            | UnicodeScalarMustDecodeToExactlyOneScalar ->
                descriptor
                    DiagnosticCode.UnicodeInvalidScalarLiteral
                    None
                    "Unicode scalar literal must decode to exactly one valid Unicode scalar value."
                    (payload
                        "unicode-invalid-scalar-literal"
                        [ field "reason" (DiagnosticPayloadText "must-decode-to-exactly-one-scalar")
                          field "unicode-scalar-error" (DiagnosticPayloadText(unicodeScalarLiteralEvidenceTag evidence)) ])
        | UnicodeGraphemeLiteralDiagnostic evidence ->
            match evidence with
            | GraphemeInvalidLiteralForm ->
                descriptor
                    DiagnosticCode.UnicodeInvalidGraphemeLiteral
                    None
                    "Grapheme literal is not a valid prefixed single-quoted literal."
                    (payload
                        "unicode-invalid-grapheme-literal"
                        [ field "reason" (DiagnosticPayloadText "invalid-literal-form")
                          field "unicode-grapheme-error" (DiagnosticPayloadText(unicodeGraphemeLiteralEvidenceTag evidence)) ])
            | GraphemeTextInvalid error ->
                descriptor
                    DiagnosticCode.UnicodeInvalidGraphemeLiteral
                    None
                    $"Grapheme literal text is invalid: {stringLiteralDecodeErrorMessage error}."
                    (payload
                        "unicode-invalid-grapheme-literal"
                        ([ field "reason" (DiagnosticPayloadText "grapheme-text-invalid")
                           field "unicode-grapheme-error" (DiagnosticPayloadText(unicodeGraphemeLiteralEvidenceTag evidence)) ]
                         @ stringLiteralDecodeErrorFields error))
            | GraphemeMustDecodeToExactlyOneExtendedCluster ->
                descriptor
                    DiagnosticCode.UnicodeInvalidGraphemeLiteral
                    None
                    "Grapheme literal must decode to exactly one extended grapheme cluster."
                    (payload
                        "unicode-invalid-grapheme-literal"
                        [ field "reason" (DiagnosticPayloadText "must-decode-to-exactly-one-extended-cluster")
                          field "unicode-grapheme-error" (DiagnosticPayloadText(unicodeGraphemeLiteralEvidenceTag evidence)) ])
        | UnicodeByteLiteralDiagnostic evidence ->
            let bytePayloadFields =
                [ field "byte-literal-error" (DiagnosticPayloadText(unicodeByteLiteralEvidenceTag evidence)) ]

            match evidence with
            | ByteInvalidLiteralForm ->
                descriptor
                    DiagnosticCode.UnicodeInvalidByteLiteral
                    None
                    "Byte literal is not a valid prefixed single-quoted literal."
                    (payload
                        "unicode-invalid-byte-literal"
                        ([ field "reason" (DiagnosticPayloadText "invalid-literal-form") ] @ bytePayloadFields))
            | ByteInvalidEscape escapeText ->
                descriptor
                    DiagnosticCode.UnicodeInvalidByteLiteral
                    None
                    $"Byte literal text is invalid: invalid byte escape '{escapeText}'."
                    (payload
                        "unicode-invalid-byte-literal"
                        ([ field "reason" (DiagnosticPayloadText "byte-text-invalid")
                           field "escape-text" (DiagnosticPayloadText escapeText) ]
                         @ bytePayloadFields))
            | ByteInvalidUnicodeEscape escapeText ->
                descriptor
                    DiagnosticCode.UnicodeInvalidByteLiteral
                    None
                    $"Byte literal text is invalid: invalid Unicode escape '{escapeText}'."
                    (payload
                        "unicode-invalid-byte-literal"
                        ([ field "reason" (DiagnosticPayloadText "byte-text-invalid")
                           field "escape-text" (DiagnosticPayloadText escapeText) ]
                         @ bytePayloadFields))
            | ByteUnknownEscapeSequence escapeText ->
                descriptor
                    DiagnosticCode.UnicodeInvalidByteLiteral
                    None
                    $"Byte literal text is invalid: unknown escape sequence '{escapeText}'."
                    (payload
                        "unicode-invalid-byte-literal"
                        ([ field "reason" (DiagnosticPayloadText "byte-text-invalid")
                           field "escape-text" (DiagnosticPayloadText escapeText) ]
                         @ bytePayloadFields))
            | ByteUnterminatedEscapeSequence ->
                descriptor
                    DiagnosticCode.UnicodeInvalidByteLiteral
                    None
                    "Byte literal text is invalid: unterminated escape sequence."
                    (payload
                        "unicode-invalid-byte-literal"
                        ([ field "reason" (DiagnosticPayloadText "byte-text-invalid") ] @ bytePayloadFields))
            | ByteUnterminatedUnicodeEscapeSequence ->
                descriptor
                    DiagnosticCode.UnicodeInvalidByteLiteral
                    None
                    "Byte literal text is invalid: unterminated Unicode escape sequence."
                    (payload
                        "unicode-invalid-byte-literal"
                        ([ field "reason" (DiagnosticPayloadText "byte-text-invalid") ] @ bytePayloadFields))
            | ByteMustDecodeToExactlyOneByte ->
                descriptor
                    DiagnosticCode.UnicodeInvalidByteLiteral
                    None
                    "Byte literal must decode to exactly one byte value."
                    (payload
                        "unicode-invalid-byte-literal"
                        ([ field "reason" (DiagnosticPayloadText "must-decode-to-exactly-one-byte") ] @ bytePayloadFields))
        | LexerDiagnostic evidence ->
            match evidence with
            | TabCharacterNotPermitted inIndentation ->
                let message =
                    if inIndentation then
                        "Tabs are not permitted in indentation."
                    else
                        "Tabs are not permitted."

                let context =
                    if inIndentation then "indentation" else "general"

                descriptor
                    DiagnosticCode.TabCharacterNotPermitted
                    None
                    message
                    (payload
                        "tab-character-not-permitted"
                        [ field "reason" (DiagnosticPayloadText "tab-character-not-permitted")
                          field "tab-context" (DiagnosticPayloadText context) ])
            | UnexpectedIndentation indent ->
                descriptor
                    DiagnosticCode.UnexpectedIndentation
                    None
                    $"Unexpected indentation level {indent}; expected one of the previous block levels."
                    (payload
                        "unexpected-indentation"
                        [ field "reason" (DiagnosticPayloadText "unexpected-indentation")
                          field "indent" (DiagnosticPayloadText(string indent)) ])
            | UnrecognizedCharacter character ->
                descriptor
                    DiagnosticCode.UnrecognizedCharacter
                    None
                    $"Unrecognized character '{character}'."
                    (payload
                        "unrecognized-character"
                        [ field "reason" (DiagnosticPayloadText "unrecognized-character")
                          field "character" (DiagnosticPayloadText character) ])
            | MalformedNumericLiteral error ->
                match error with
                | MalformedPrefixedIntegerLiteral integerBase ->
                    let integerBaseText = prefixedIntegerBaseText integerBase

                    descriptor
                        DiagnosticCode.MalformedNumericLiteral
                        None
                        $"Malformed {integerBaseText} integer literal."
                        (payload
                            "malformed-numeric-literal"
                            [ field "reason" (DiagnosticPayloadText "malformed-prefixed-integer-literal")
                              field "integer-base" (DiagnosticPayloadText integerBaseText) ])
            | UnterminatedStringLiteral isPrefixed ->
                let message =
                    if isPrefixed then
                        "Unterminated prefixed string literal."
                    else
                        "Unterminated string literal."

                descriptor
                    DiagnosticCode.UnterminatedStringLiteral
                    None
                    message
                    (payload
                        "unterminated-string-literal"
                        [ field "reason" (DiagnosticPayloadText "unterminated-string-literal")
                          field "prefixed" (DiagnosticPayloadText(if isPrefixed then "true" else "false")) ])
            | UnterminatedCharacterLiteral ->
                descriptor
                    DiagnosticCode.UnterminatedCharacterLiteral
                    None
                    "Unterminated character literal."
                    (payload
                        "unterminated-character-literal"
                        [ field "reason" (DiagnosticPayloadText "unterminated-character-literal") ])
            | UnterminatedBacktickIdentifier ->
                descriptor
                    DiagnosticCode.UnterminatedBacktickIdentifier
                    None
                    "Unterminated backtick identifier."
                    (payload
                        "unterminated-backtick-identifier"
                        [ field "reason" (DiagnosticPayloadText "unterminated-backtick-identifier") ])
            | UnterminatedStringInterpolation ->
                descriptor
                    DiagnosticCode.UnterminatedStringInterpolation
                    None
                    "Unterminated string interpolation."
                    (payload
                        "unterminated-string-interpolation"
                        [ field "reason" (DiagnosticPayloadText "unterminated-string-interpolation") ])
            | UnterminatedBlockComment ->
                descriptor
                    DiagnosticCode.UnterminatedBlockComment
                    None
                    "Unterminated block comment."
                    (payload
                        "unterminated-block-comment"
                        [ field "reason" (DiagnosticPayloadText "unterminated-block-comment") ])
        | QttLinearDropDiagnostic evidence ->
            match evidence with
            | ShadowedBindingMustConsumePreviousValue shadowedBindingName ->
                descriptor
                    DiagnosticCode.QttLinearDrop
                    None
                    $"Shadowing binding '{shadowedBindingName}' must consume the previous linear value exactly once in the right-hand side."
                    (payload
                        "qtt-linear-drop"
                        [ field "reason" (DiagnosticPayloadText "shadowed-binding-must-consume-previous-value")
                          field "binding" (DiagnosticPayloadText shadowedBindingName) ])
            | BindingNotConsumedOnEveryPath bindingName ->
                descriptor
                    DiagnosticCode.QttLinearDrop
                    None
                    $"Linear resource '{bindingName}' is not consumed on every path."
                    (payload
                        "qtt-linear-drop"
                        [ field "reason" (DiagnosticPayloadText "binding-not-consumed-on-every-path")
                          field "binding" (DiagnosticPayloadText bindingName) ])
            | ClauseMayDiscardRowBinding(clauseLabel, bindingName) ->
                descriptor
                    DiagnosticCode.QttLinearDrop
                    None
                    $"`{clauseLabel}` may discard linear row binding '{bindingName}'."
                    (payload
                        "qtt-linear-drop"
                        [ field "reason" (DiagnosticPayloadText "clause-may-discard-row-binding")
                          field "clause" (DiagnosticPayloadText clauseLabel)
                          field "binding" (DiagnosticPayloadText bindingName) ])
            | DropClauseCardinalityMayAffectRowBinding(clauseLabel, bindingName, effect, cardinalityText) ->
                descriptor
                    DiagnosticCode.QttLinearDrop
                    None
                    $"`{clauseLabel}` may {qttCardinalityEffectText effect} linear row binding '{bindingName}' because the clause cardinality is {cardinalityText}."
                    (payload
                        "qtt-linear-drop"
                        [ field "reason" (DiagnosticPayloadText "clause-cardinality-may-affect-row-binding")
                          field "clause" (DiagnosticPayloadText clauseLabel)
                          field "binding" (DiagnosticPayloadText bindingName)
                          field "effect" (DiagnosticPayloadText(qttCardinalityEffectText effect))
                          field "cardinality" (DiagnosticPayloadText cardinalityText) ])
            | RecordPatternOmittedField fieldName ->
                descriptor
                    DiagnosticCode.QttLinearDrop
                    None
                    $"Record pattern omits linear field '{fieldName}'."
                    (payload
                        "qtt-linear-drop"
                        [ field "reason" (DiagnosticPayloadText "record-pattern-omitted-field")
                          field "field" (DiagnosticPayloadText fieldName) ])
            | PlainLetQuestionDiscardRefutationResidue ->
                descriptor
                    DiagnosticCode.QttLinearDrop
                    None
                    "Plain let? would discard a refutation residue carrying a positive lower-bound obligation; use an explicit else arm."
                    (payload
                        "qtt-linear-drop"
                        [ field "reason" (DiagnosticPayloadText "plain-let-question-discard-refutation-residue") ])
        | QttLinearOveruseDiagnostic evidence ->
            match evidence with
            | WholeResourceConsumedAfterFieldPath resourceName ->
                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"Linear resource '{resourceName}' cannot be consumed as a whole after one of its field paths has already been consumed."
                    (payload
                        "qtt-linear-overuse"
                        [ field "reason" (DiagnosticPayloadText "whole-resource-consumed-after-field-path")
                          field "resource" (DiagnosticPayloadText resourceName) ])
            | WholeResourceUsedAfterFieldPath resourceName ->
                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"Linear resource '{resourceName}' cannot be used as a whole after one of its field paths has already been consumed."
                    (payload
                        "qtt-linear-overuse"
                        [ field "reason" (DiagnosticPayloadText "whole-resource-used-after-field-path")
                          field "resource" (DiagnosticPayloadText resourceName) ])
            | FieldPathConsumedAfterRoot placeText ->
                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"Field path '{placeText}' cannot be consumed after its root resource has already been consumed."
                    (payload
                        "qtt-linear-overuse"
                        [ field "reason" (DiagnosticPayloadText "field-path-consumed-after-root")
                          field "place" (DiagnosticPayloadText placeText) ])
            | FieldPathAlreadyConsumed placeText ->
                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"Field path '{placeText}' has already been consumed."
                    (payload
                        "qtt-linear-overuse"
                        [ field "reason" (DiagnosticPayloadText "field-path-already-consumed")
                          field "place" (DiagnosticPayloadText placeText) ])
            | ResourceConsumedMoreThanOnce resourceName ->
                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"Linear resource '{resourceName}' is consumed more than once."
                    (payload
                        "qtt-linear-overuse"
                        [ field "reason" (DiagnosticPayloadText "resource-consumed-more-than-once")
                          field "resource" (DiagnosticPayloadText resourceName) ])
            | ClauseMustNotConsumeRowBinding(clauseLabel, bindingName) ->
                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"`{clauseLabel}` must not consume row binding '{bindingName}'."
                    (payload
                        "qtt-linear-overuse"
                        [ field "reason" (DiagnosticPayloadText "clause-must-not-consume-row-binding")
                          field "clause" (DiagnosticPayloadText clauseLabel)
                          field "binding" (DiagnosticPayloadText bindingName) ])
            | OveruseClauseCardinalityMayAffectRowBinding(clauseLabel, bindingName, effect, cardinalityText) ->
                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"`{clauseLabel}` may {qttCardinalityEffectText effect} linear row binding '{bindingName}' because the clause cardinality is {cardinalityText}."
                    (payload
                        "qtt-linear-overuse"
                        [ field "reason" (DiagnosticPayloadText "clause-cardinality-may-affect-row-binding")
                          field "clause" (DiagnosticPayloadText clauseLabel)
                          field "binding" (DiagnosticPayloadText bindingName)
                          field "effect" (DiagnosticPayloadText(qttCardinalityEffectText effect))
                          field "cardinality" (DiagnosticPayloadText cardinalityText) ])
            | LeftJoinCapturesLinearOuterRowBinding bindingName ->
                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"`left join ... into` would capture linear outer row binding '{bindingName}' for delayed query use."
                    (payload
                        "qtt-linear-overuse"
                        [ field "reason" (DiagnosticPayloadText "left-join-captures-linear-outer-row-binding")
                          field "binding" (DiagnosticPayloadText bindingName) ])
            | RecordUpdateRequiresRepair(receiverRoot, firstUnrepairedPath) ->
                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"Record update on '{receiverRoot}' must explicitly repair previously consumed path '{firstUnrepairedPath}'."
                    (payload
                        "qtt-linear-overuse"
                        [ field "reason" (DiagnosticPayloadText "record-update-requires-repair")
                          field "receiver-root" (DiagnosticPayloadText receiverRoot)
                          field "path" (DiagnosticPayloadText firstUnrepairedPath) ])
            | QuantityCannotSatisfyParameterDemand(capabilityText, demandText, mode) ->
                let capabilityVerb =
                    match mode with
                    | Available -> "available"
                    | Usable -> "usable"

                descriptor
                    DiagnosticCode.QttLinearOveruse
                    None
                    $"An argument {capabilityVerb} at quantity '{capabilityText}' cannot satisfy parameter demand '{demandText}'."
                    (payload
                        "qtt-linear-overuse"
                        [ field "reason" (DiagnosticPayloadText "quantity-cannot-satisfy-parameter-demand")
                          field "mode" (DiagnosticPayloadText capabilityVerb)
                          field "capability" (DiagnosticPayloadText capabilityText)
                          field "demand" (DiagnosticPayloadText demandText) ])
        | QttBorrowConsumeDiagnostic evidence ->
            match evidence with
            | BorrowedResourceCannotBeConsumed resourceName ->
                descriptor
                    DiagnosticCode.QttBorrowConsume
                    None
                    $"Borrowed resource '{resourceName}' cannot be consumed."
                    (payload
                        "qtt-borrow-consume"
                        [ field "reason" (DiagnosticPayloadText "borrowed-resource-cannot-be-consumed")
                          field "resource" (DiagnosticPayloadText resourceName) ])
        | QttBorrowOverlapDiagnostic evidence ->
            match evidence with
            | PlaceOverlapsActiveBorrowedFootprint placeText ->
                descriptor
                    DiagnosticCode.QttBorrowOverlap
                    None
                    $"Place '{placeText}' overlaps an active borrowed footprint."
                    (payload
                        "qtt-borrow-overlap"
                        [ field "reason" (DiagnosticPayloadText "place-overlaps-active-borrowed-footprint")
                          field "place" (DiagnosticPayloadText placeText) ])
            | InoutArgumentsRequireDisjointFootprints ->
                descriptor
                    DiagnosticCode.QttBorrowOverlap
                    None
                    "Inout arguments must have disjoint place footprints."
                    (payload
                        "qtt-borrow-overlap"
                        [ field "reason" (DiagnosticPayloadText "inout-arguments-require-disjoint-footprints") ])
            | TemporaryBorrowOverlapsLaterConsumingArgument ->
                descriptor
                    DiagnosticCode.QttBorrowOverlap
                    None
                    "A temporary borrow introduced earlier in this application spine overlaps a later consuming argument."
                    (payload
                        "qtt-borrow-overlap"
                        [ field "reason" (DiagnosticPayloadText "temporary-borrow-overlaps-later-consuming-argument") ])
        | QttBorrowEscapeDiagnostic evidence ->
            match evidence with
            | ValueCapturesBorrowedRegion ->
                descriptor
                    DiagnosticCode.QttBorrowEscape
                    None
                    "A value that captures a borrowed region cannot escape its protected scope."
                    (payload "qtt-borrow-escape" [ field "reason" (DiagnosticPayloadText "value-captures-borrowed-region") ])
            | LambdaCapturesBorrowedRegion ->
                descriptor
                    DiagnosticCode.QttBorrowEscape
                    None
                    "A lambda that captures a borrowed region cannot escape its protected scope."
                    (payload "qtt-borrow-escape" [ field "reason" (DiagnosticPayloadText "lambda-captures-borrowed-region") ])
            | ForkedChildCapturesBorrowedRegion ->
                descriptor
                    DiagnosticCode.QttBorrowEscape
                    None
                    "A forked child computation cannot capture a borrowed region from the parent fiber."
                    (payload
                        "qtt-borrow-escape"
                        [ field "reason" (DiagnosticPayloadText "forked-child-captures-borrowed-region") ])
        | QttErasedRuntimeUseDiagnostic evidence ->
            match evidence with
            | QuantityZeroBindingUsedAtRuntime bindingName ->
                descriptor
                    DiagnosticCode.QttErasedRuntimeUse
                    None
                    $"Quantity-0 binding '{bindingName}' cannot be used at runtime."
                    (payload
                        "qtt-erased-runtime-use"
                        [ field "reason" (DiagnosticPayloadText "quantity-zero-binding-used-at-runtime")
                          field "binding" (DiagnosticPayloadText bindingName) ])
            | RuntimeClosureCapturesErasedBinding bindingName ->
                descriptor
                    DiagnosticCode.QttErasedRuntimeUse
                    None
                    $"A runtime closure cannot capture erased quantity-0 binding '{bindingName}'."
                    (payload
                        "qtt-erased-runtime-use"
                        [ field "reason" (DiagnosticPayloadText "runtime-closure-captures-erased-binding")
                          field "binding" (DiagnosticPayloadText bindingName) ])
            | MatchScrutineeUsesErasedValue ->
                descriptor
                    DiagnosticCode.QttErasedRuntimeUse
                    None
                    "A match scrutinee cannot use an erased quantity-0 value at runtime."
                    (payload
                        "qtt-erased-runtime-use"
                        [ field "reason" (DiagnosticPayloadText "match-scrutinee-uses-erased-value") ])
        | QttUsingExplicitQuantityDiagnostic UsingBindsBorrowedPattern ->
            descriptor
                DiagnosticCode.QttUsingExplicitQuantity
                None
                "'using' binds its pattern at borrowed quantity '&'; explicit quantity markers are not permitted."
                (payload
                    "qtt-using-explicit-quantity"
                    [ field "reason" (DiagnosticPayloadText "using-binds-borrowed-pattern") ])
        | QttInoutMarkerRequiredDiagnostic InoutMarkerRequired ->
            descriptor
                DiagnosticCode.QttInoutMarkerRequired
                None
                "An argument supplied to an 'inout' parameter must be marked with '~'."
                (payload
                    "qtt-inout-marker-required"
                    [ field "reason" (DiagnosticPayloadText "inout-marker-required") ])
        | QttInoutMarkerUnexpectedDiagnostic InoutMarkerUnexpected ->
            descriptor
                DiagnosticCode.QttInoutMarkerUnexpected
                None
                "The '~' marker can only be used for an 'inout' parameter."
                (payload
                    "qtt-inout-marker-unexpected"
                    [ field "reason" (DiagnosticPayloadText "inout-marker-unexpected") ])
        | QttInoutThreadedFieldMissingDiagnostic(InoutThreadedFieldMissing parameterName) ->
            descriptor
                DiagnosticCode.QttInoutThreadedFieldMissing
                None
                $"An 'inout' parameter '{parameterName}' requires the result type to contain a quantity-1 field named '{parameterName}' after peeling any enclosing monad."
                (payload
                    "qtt-inout-threaded-field-missing"
                    [ field "reason" (DiagnosticPayloadText "inout-threaded-field-missing")
                      field "parameter-name" (DiagnosticPayloadText parameterName) ])

type Diagnostic =
    { Severity: DiagnosticSeverity
      Fact: DiagnosticFact
      Code: DiagnosticCode
      Stage: string option
      Phase: string option
      Message: string
      Location: SourceLocation option
      RelatedLocations: DiagnosticRelatedLocation list
      Family: string option
      Payload: DiagnosticPayload
      Explanation: string option }

module Diagnostics =
    let create severity fact location relatedLocations stage phase =
        let descriptor = DiagnosticFact.describe fact

        { Severity = severity
          Fact = fact
          Code = descriptor.Code
          Stage = stage
          Phase = phase
          Message = descriptor.Message
          Location = location
          RelatedLocations = relatedLocations
          Family = descriptor.Family
          Payload = descriptor.Payload
          Explanation = descriptor.Explanation }

    let errorFact stage phase location relatedLocations fact =
        create Error fact location relatedLocations (Some stage) phase

type DiagnosticBag() =
    let items = ResizeArray<Diagnostic>()

    member _.Add(
        severity: DiagnosticSeverity,
        fact: DiagnosticFact,
        ?location: SourceLocation,
        ?relatedLocations: DiagnosticRelatedLocation seq,
        ?stage: string,
        ?phase: string
    ) =
        items.Add(
            Diagnostics.create
                severity
                fact
                location
                (relatedLocations |> Option.map Seq.toList |> Option.defaultValue [])
                stage
                phase
        )

    member this.AddInfo(fact: DiagnosticFact, ?location: SourceLocation, ?stage: string, ?phase: string) =
        this.Add(Info, fact, ?location = location, ?stage = stage, ?phase = phase)

    member this.AddWarning(fact: DiagnosticFact, ?location: SourceLocation, ?stage: string, ?phase: string) =
        this.Add(Warning, fact, ?location = location, ?stage = stage, ?phase = phase)

    member this.AddError(fact: DiagnosticFact, ?location: SourceLocation, ?stage: string, ?phase: string) =
        this.Add(Error, fact, ?location = location, ?stage = stage, ?phase = phase)

    member _.AddRange(diagnostics: Diagnostic seq) =
        diagnostics |> Seq.iter items.Add

    member _.Items = List.ofSeq items
    member _.HasErrors = items |> Seq.exists (fun diagnostic -> diagnostic.Severity = Error)
