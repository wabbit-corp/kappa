namespace Kappa.Compiler

module ZigCcBackend =
    let emitTranslationUnit workspace =
        ZigCcBackendArtifact.emitTranslationUnit workspace

    let emitArtifact workspace entryPoint outputDirectory =
        ZigCcBackendArtifact.emitArtifact workspace entryPoint outputDirectory
