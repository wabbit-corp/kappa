namespace Kappa.Compiler

module IlDotNetBackend =
    let emittedModuleTypeName moduleName =
        IlDotNetBackendModel.emittedModuleTypeName moduleName

    let emittedMethodName bindingName =
        IlDotNetBackendModel.emittedMethodName bindingName

    let emitClrAssemblyArtifact modules outputDirectory =
        IlDotNetBackendEmit.emitClrAssemblyArtifact modules outputDirectory

    let emitAssemblyArtifact workspace outputDirectory =
        IlDotNetBackendEmit.emitAssemblyArtifact workspace outputDirectory
