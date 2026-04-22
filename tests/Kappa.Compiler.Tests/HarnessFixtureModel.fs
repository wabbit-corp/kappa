module HarnessFixtureModel

open Kappa.Compiler

type KpFixtureMode =
    | Analyze
    | Check
    | Compile
    | Run

type KpFixtureDirectiveSource =
    | KpSourceFile
    | SuiteDirectiveFile

type KpFixtureRelation =
    | Equal
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual

type KpFixtureConfiguration =
    { Mode: KpFixtureMode
      PackageMode: bool
      BackendProfile: string
      EntryPoint: string option
      RunArgs: string list
      StdinFile: string option
      DumpFormat: StageDumpFormat }

module KpFixtureConfiguration =
    let defaultValue =
        { Mode = KpFixtureMode.Check
          PackageMode = true
          BackendProfile = "interpreter"
          EntryPoint = None
          RunArgs = []
          StdinFile = None
          DumpFormat = StageDumpFormat.Json }

type KpFixtureAssertion =
    | AssertNoErrors of filePath: string * lineNumber: int
    | AssertNoWarnings of filePath: string * lineNumber: int
    | AssertErrorCount of expectedCount: int * filePath: string * lineNumber: int
    | AssertWarningCount of expectedCount: int * filePath: string * lineNumber: int
    | AssertDiagnosticCodes of expectedCodes: string list * filePath: string * lineNumber: int
    | AssertDiagnosticAt of relativePath: string * severity: DiagnosticSeverity * code: string * expectedLine: int * expectedColumn: int option * filePath: string * lineNumber: int
    | AssertDiagnosticMatch of regexPattern: string * filePath: string * lineNumber: int
    | AssertType of target: string * expectedTypeText: string * filePath: string * lineNumber: int
    | AssertFileDeclarationKinds of relativePath: string * expectedKinds: string list * filePath: string * lineNumber: int
    | AssertEval of target: string * expectedValueText: string * filePath: string * lineNumber: int
    | AssertEvalErrorContains of target: string * expectedText: string * filePath: string * lineNumber: int
    | AssertExecute of target: string * expectedValueText: string * filePath: string * lineNumber: int
    | AssertRunStdout of target: string * expectedOutputText: string * filePath: string * lineNumber: int
    | AssertStdout of expectedOutputText: string * filePath: string * lineNumber: int
    | AssertStdoutContains of expectedOutputText: string * filePath: string * lineNumber: int
    | AssertStderrContains of expectedOutputText: string * filePath: string * lineNumber: int
    | AssertExitCode of expectedCode: int * filePath: string * lineNumber: int
    | AssertTraceCount of eventName: string * subjectName: string * relation: KpFixtureRelation * expectedCount: int * filePath: string * lineNumber: int
    | AssertModule of expectedModuleText: string * filePath: string * lineNumber: int
    | AssertModuleAttributes of expectedAttributes: string list * filePath: string * lineNumber: int
    | AssertDeclarationKinds of expectedKinds: string list * filePath: string * lineNumber: int
    | AssertDeclarationDescriptors of expectedDescriptors: string list * filePath: string * lineNumber: int
    | AssertParameterQuantities of bindingName: string * expectedQuantities: string list * filePath: string * lineNumber: int
    | AssertInoutParameters of bindingName: string * expectedNames: string list * filePath: string * lineNumber: int
    | AssertDoItemDescriptors of bindingName: string * expectedDescriptors: string list * filePath: string * lineNumber: int
    | AssertDataConstructors of typeName: string * expectedConstructors: string list * filePath: string * lineNumber: int
    | AssertTraitMembers of traitName: string * expectedMembers: string list * filePath: string * lineNumber: int
    | AssertContainsTokenKinds of expectedKinds: string list * filePath: string * lineNumber: int
    | AssertContainsTokenTexts of expectedTexts: string list * filePath: string * lineNumber: int

type KpFixtureCase =
    { Name: string
      Root: string
      SourceFiles: string list
      Configuration: KpFixtureConfiguration
      Assertions: KpFixtureAssertion list }

    override this.ToString() = this.Name
