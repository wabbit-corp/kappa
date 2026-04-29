namespace Kappa.Compiler

open System.Collections.Generic

// Records the observable pipeline trace from the steps the compiler actually executed.
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

    let private moduleLabel moduleIdentity filePath =
        moduleIdentity
        |> Option.map SyntaxFacts.moduleNameToText
        |> Option.defaultValue filePath

    type Recorder() =
        let steps = ResizeArray<PipelineTraceStep>()

        member _.RecordStep
            eventName
            subject
            stepName
            inputCheckpoint
            outputCheckpoint
            changedRepresentation
            verificationAttempted
            verificationSucceeded
            =
            steps.Add(
                traceStep
                    eventName
                    subject
                    stepName
                    inputCheckpoint
                    outputCheckpoint
                    changedRepresentation
                    verificationAttempted
                    verificationSucceeded
            )

        member this.RecordParse(filePath: string) =
            this.RecordStep
                PipelineTraceEvent.Parse
                PipelineTraceSubject.File
                $"parse {filePath}"
                "surface-source"
                "surface-source"
                false
                false
                None

        member this.RecordBuildKFrontIR(filePath: string) =
            this.RecordStep
                PipelineTraceEvent.BuildKFrontIR
                PipelineTraceSubject.File
                $"build KFrontIR for {filePath}"
                "surface-source"
                (KFrontIRPhase.checkpointName RAW)
                true
                false
                None

        member this.RecordAdvancePhase(fromPhase: KFrontIRPhase, toPhase: KFrontIRPhase, moduleIdentity, filePath) =
            let label = moduleLabel moduleIdentity filePath

            this.RecordStep
                PipelineTraceEvent.AdvancePhase
                PipelineTraceSubject.Module
                $"advance {label} to {KFrontIRPhase.phaseName toPhase}"
                (KFrontIRPhase.checkpointName fromPhase)
                (KFrontIRPhase.checkpointName toPhase)
                true
                false
                None

        member this.RecordLowerKCore(moduleIdentity, filePath) =
            let label = moduleLabel moduleIdentity filePath

            this.RecordStep
                PipelineTraceEvent.LowerKCore
                PipelineTraceSubject.Module
                $"lower {label} to KCore"
                (KFrontIRPhase.checkpointName CORE_LOWERING)
                "KCore"
                true
                false
                None

        member this.RecordLowerKRuntimeIR(moduleName: string) =
            this.RecordStep
                PipelineTraceEvent.LowerKRuntimeIR
                PipelineTraceSubject.KCoreUnit
                $"lower {moduleName} to KRuntimeIR"
                "KCore"
                "KRuntimeIR"
                true
                false
                None

        member this.RecordLowerKBackendIR(moduleName: string) =
            this.RecordStep
                PipelineTraceEvent.LowerKBackendIR
                PipelineTraceSubject.Module
                $"lower {moduleName} to KBackendIR"
                "KRuntimeIR"
                "KBackendIR"
                true
                false
                None

        member this.RecordLowerTarget(checkpoint: string, inputCheckpoint: string) =
            this.RecordStep
                PipelineTraceEvent.LowerTarget
                PipelineTraceSubject.TargetUnit
                $"lower {inputCheckpoint} to {checkpoint}"
                inputCheckpoint
                checkpoint
                true
                false
                None

        member this.RecordVerification(subject, stepName: string, checkpoint: string, succeeded: bool) =
            this.RecordStep PipelineTraceEvent.Verify subject stepName checkpoint checkpoint false true (Some succeeded)

        member _.Finish() = steps |> Seq.toList
