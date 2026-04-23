namespace Kappa.Compiler

// Exposes the IL emitter facade used by managed backend code paths.
module IlDotNetBackend =
    let emittedModuleTypeName moduleName =
        IlDotNetBackendModel.emittedModuleTypeName moduleName

    let emittedMethodName bindingName =
        IlDotNetBackendModel.emittedMethodName bindingName

    let emitClrAssemblyArtifact modules outputDirectory =
        IlDotNetBackendEmit.emitClrAssemblyArtifact modules outputDirectory

    let emitAssemblyArtifact workspace outputDirectory =
        IlDotNetBackendEmit.emitAssemblyArtifact workspace outputDirectory
