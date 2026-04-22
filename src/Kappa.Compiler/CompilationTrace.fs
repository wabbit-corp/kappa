namespace Kappa.Compiler

module internal CompilationTrace =
    let private traceStep eventName subject stepName inputCheckpoint outputCheckpoint changedRepresentation verificationAttempted verificationSucceeded =
        { Event = eventName
          Subject = subject
          StepName = stepName
          InputCheckpoint = inputCheckpoint
          OutputCheckpoint = outputCheckpoint
          ChangedRepresentation = changedRepresentation
          VerificationAttempted = verificationAttempted
          VerificationSucceeded = verificationSucceeded }

    let buildPipelineTrace (workspace: WorkspaceCompilation) =
        let documents =
            workspace.KFrontIR
            |> List.sortBy (fun document -> document.FilePath)

        let frontendCheckpoint = KFrontIRPhase.checkpointName CHECKERS
        let frontendVerified = CheckpointVerification.verifyCheckpoint workspace frontendCheckpoint |> List.isEmpty
        let coreVerified = CheckpointVerification.verifyCheckpoint workspace "KCore" |> List.isEmpty
        let runtimeVerified = CheckpointVerification.verifyCheckpoint workspace "KRuntimeIR" |> List.isEmpty
        let backendVerified = CheckpointVerification.verifyCheckpoint workspace "KBackendIR" |> List.isEmpty

        let phaseTransitions =
            KFrontIRPhase.all
            |> List.pairwise

        let documentSteps =
            documents
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

                let verifySteps =
                    [
                        traceStep
                            PipelineTraceEvent.Verify
                            PipelineTraceSubject.Module
                            $"verify {label} at {frontendCheckpoint}"
                            frontendCheckpoint
                            frontendCheckpoint
                            false
                            true
                            (Some frontendVerified)
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
                            (Some coreVerified)
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
                            (Some runtimeVerified)
                        traceStep
                            PipelineTraceEvent.LowerKBackendIR
                            PipelineTraceSubject.KRuntimeIRUnit
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
                            (Some backendVerified)
                    ]

                parseSteps @ phaseSteps @ verifySteps)

        let targetSteps =
            CompilationCheckpoints.targetCheckpointNames workspace
            |> List.collect (fun checkpoint ->
                let targetVerified =
                    CompilationCheckpoints.verifyTargetCheckpoint workspace checkpoint
                    |> List.isEmpty

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
                        (Some targetVerified)
                ])

        documentSteps @ targetSteps
