namespace Kappa.Compiler

// Exposes the standardized zig backend facade for translation-unit and artifact emission.
module ZigCcBackend =
    let emitTranslationUnit workspace =
        ZigCcBackendArtifact.emitTranslationUnit workspace

    let emitArtifact workspace entryPoint outputDirectory =
        ZigCcBackendArtifact.emitArtifact workspace entryPoint outputDirectory
