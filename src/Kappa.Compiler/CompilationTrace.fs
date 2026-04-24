namespace Kappa.Compiler

// Builds the observable pipeline trace from executed stages and verification results.
module internal CompilationTrace =
    type VerificationSummary =
        { Frontend: bool
          KCore: bool
          KRuntimeIR: bool
          KBackendIR: bool
          Targets: Map<string, bool> }

    let private traceStep eventName subject stepName inputCheckpoint outputCheckpoint changedRepresentation verificationAttempted verificationSucceeded =
        { Event = eventName
          Subject = subject
          StepName = stepName
          InputCheckpoint = inputCheckpoint
          OutputCheckpoint = outputCheckpoint
          ChangedRepresentation = changedRepresentation
          VerificationAttempted = verificationAttempted
          VerificationSucceeded = verificationSucceeded }

    let buildPipelineTrace
        (documents: KFrontIRModule list)
        (targetCheckpoints: string list)
        (verification: VerificationSummary)
        =
        let frontendCheckpoint = KFrontIRPhase.checkpointName CHECKERS

        let phaseTransitions =
            KFrontIRPhase.all
            |> List.pairwise

        let documentSteps =
            documents
            |> List.sortBy (fun document -> document.FilePath)
            |> List.collect (fun document ->
                let label =
                    document.ModuleIdentity
                    |> Option.map SyntaxFacts.moduleNameToText
                    |> Option.defaultValue document.FilePath

                let parseSteps =
                    [
                        traceStep
                            PipelineTraceEvent.Parse
                            PipelineTraceSubject.File
                            $"parse {document.FilePath}"
                            "surface-source"
                            "surface-source"
                            false
                            false
                            None
                        traceStep
                            PipelineTraceEvent.BuildKFrontIR
                            PipelineTraceSubject.File
                            $"build KFrontIR for {document.FilePath}"
                            "surface-source"
                            (KFrontIRPhase.checkpointName RAW)
                            true
                            false
                            None
                    ]

                let phaseSteps =
                    phaseTransitions
                    |> List.map (fun (fromPhase, toPhase) ->
                        traceStep
                            PipelineTraceEvent.AdvancePhase
                            PipelineTraceSubject.Module
                            $"advance {label} to {KFrontIRPhase.phaseName toPhase}"
                            (KFrontIRPhase.checkpointName fromPhase)
                            (KFrontIRPhase.checkpointName toPhase)
                            true
                            false
                            None)

                let verifyAndLowerSteps =
                    [
                        traceStep
                            PipelineTraceEvent.Verify
                            PipelineTraceSubject.Module
                            $"verify {label} at {frontendCheckpoint}"
                            frontendCheckpoint
                            frontendCheckpoint
                            false
                            true
                            (Some verification.Frontend)
                        traceStep
                            PipelineTraceEvent.LowerKCore
                            PipelineTraceSubject.Module
                            $"lower {label} to KCore"
                            (KFrontIRPhase.checkpointName CORE_LOWERING)
                            "KCore"
                            true
                            false
                            None
                        traceStep
                            PipelineTraceEvent.Verify
                            PipelineTraceSubject.KCoreUnit
                            $"verify {label} at KCore"
                            "KCore"
                            "KCore"
                            false
                            true
                            (Some verification.KCore)
                        traceStep
                            PipelineTraceEvent.LowerKRuntimeIR
                            PipelineTraceSubject.KCoreUnit
                            $"lower {label} to KRuntimeIR"
                            "KCore"
                            "KRuntimeIR"
                            true
                            false
                            None
                        traceStep
                            PipelineTraceEvent.Verify
                            PipelineTraceSubject.KRuntimeIRUnit
                            $"verify {label} at KRuntimeIR"
                            "KRuntimeIR"
                            "KRuntimeIR"
                            false
                            true
                            (Some verification.KRuntimeIR)
                        traceStep
                            PipelineTraceEvent.LowerKBackendIR
                            PipelineTraceSubject.Module
                            $"lower {label} to KBackendIR"
                            "KRuntimeIR"
                            "KBackendIR"
                            true
                            false
                            None
                        traceStep
                            PipelineTraceEvent.Verify
                            PipelineTraceSubject.KBackendIRUnit
                            $"verify {label} at KBackendIR"
                            "KBackendIR"
                            "KBackendIR"
                            false
                            true
                            (Some verification.KBackendIR)
                    ]

                parseSteps @ phaseSteps @ verifyAndLowerSteps)

        let targetSteps =
            targetCheckpoints
            |> List.collect (fun checkpoint ->
                let verified =
                    verification.Targets
                    |> Map.tryFind checkpoint
                    |> Option.defaultValue false

                [
                    traceStep
                        PipelineTraceEvent.LowerTarget
                        PipelineTraceSubject.TargetUnit
                        $"lower KBackendIR to {checkpoint}"
                        "KBackendIR"
                        checkpoint
                        true
                        false
                        None
                    traceStep
                        PipelineTraceEvent.Verify
                        PipelineTraceSubject.TargetUnit
                        $"verify target at {checkpoint}"
                        checkpoint
                        checkpoint
                        false
                        true
                        (Some verified)
                ])

        documentSteps @ targetSteps
