// Defines the shared fixture configuration, assertion, and discovered-case model.
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
    | IncrementalDirectiveFile

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
      AllowUnsafeConsume: bool
      RequiredCapabilities: Set<string>
      RequiredBackendProfiles: Set<string>
      RequiredPackageModes: Set<bool>
      EntryPoint: string option
      RunArgs: string list
      StdinFile: string option
      DumpFormat: StageDumpFormat }

module KpFixtureConfiguration =
    let defaultValue =
        { Mode = KpFixtureMode.Check
          PackageMode = true
          BackendProfile = "interpreter"
          AllowUnsafeConsume = false
          RequiredCapabilities = Set.empty
          RequiredBackendProfiles = Set.empty
          RequiredPackageModes = Set.empty
          EntryPoint = None
          RunArgs = []
          StdinFile = None
          DumpFormat = StageDumpFormat.Json }

type KpFixtureAssertion =
    | AssertNoErrors of filePath: string * lineNumber: int
    | AssertNoWarnings of filePath: string * lineNumber: int
    | AssertErrorCount of expectedCount: int * filePath: string * lineNumber: int
    | AssertWarningCount of expectedCount: int * filePath: string * lineNumber: int
    | AssertDiagnostic of severity: DiagnosticSeverity * code: DiagnosticCode * filePath: string * lineNumber: int
    | AssertDiagnosticNext of severity: DiagnosticSeverity * code: DiagnosticCode * filePath: string * lineNumber: int
    | AssertDiagnosticCodes of expectedCodes: DiagnosticCode list * filePath: string * lineNumber: int
    | AssertDiagnosticAt of
        relativePath: string *
        severity: DiagnosticSeverity *
        code: DiagnosticCode *
        expectedStartLine: int *
        expectedStartColumn: int option *
        expectedEndLine: int option *
        expectedEndColumn: int option *
        filePath: string *
        lineNumber: int
    | AssertDiagnosticMatch of regexPattern: string * filePath: string * lineNumber: int
    | AssertDiagnosticExplainExists of code: DiagnosticCode * filePath: string * lineNumber: int
    | AssertType of target: string * expectedTypeText: string * filePath: string * lineNumber: int
    | AssertFileDeclarationKinds of relativePath: string * expectedKinds: string list * filePath: string * lineNumber: int
    | AssertStageDump of checkpoint: string * relativePath: string * filePath: string * lineNumber: int
    | AssertEval of target: string * expectedValueText: string * filePath: string * lineNumber: int
    | AssertEvalErrorContains of target: string * expectedText: string * filePath: string * lineNumber: int
    | AssertExecute of target: string * expectedValueText: string * filePath: string * lineNumber: int
    | AssertRunStdout of target: string * expectedOutputText: string * filePath: string * lineNumber: int
    | AssertStdout of expectedOutputText: string * filePath: string * lineNumber: int
    | AssertStdoutFile of relativePath: string * filePath: string * lineNumber: int
    | AssertStdoutContains of expectedOutputText: string * filePath: string * lineNumber: int
    | AssertStderrContains of expectedOutputText: string * filePath: string * lineNumber: int
    | AssertStderrFile of relativePath: string * filePath: string * lineNumber: int
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

type KpIncrementalFixtureAssertion =
    | AssertStepNoErrors of stepIndex: int * filePath: string * lineNumber: int
    | AssertStepErrorCount of stepIndex: int * expectedCount: int * filePath: string * lineNumber: int
    | AssertStepWarningCount of stepIndex: int * expectedCount: int * filePath: string * lineNumber: int
    | AssertStepTraceCount of stepIndex: int * eventName: string * subjectName: string * relation: KpFixtureRelation * expectedCount: int * filePath: string * lineNumber: int

type KpFixtureCase =
    { Name: string
      Root: string
      SourceFiles: string list
      Configuration: KpFixtureConfiguration
      Assertions: KpFixtureAssertion list }

    override this.ToString() = this.Name

type KpIncrementalFixtureCase =
    { Name: string
      Root: string
      RequiredCapabilities: Set<string>
      RequiredBackendProfiles: Set<string>
      RequiredPackageModes: Set<bool>
      Steps: KpFixtureCase list
      Assertions: KpIncrementalFixtureAssertion list }

    override this.ToString() = this.Name
