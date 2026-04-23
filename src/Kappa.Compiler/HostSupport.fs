namespace Kappa.Compiler

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices

// Provides host/process/path utilities shared by the CLI, backends, and tests.
module HostSupport =
    type ProcessResult =
        { ExitCode: int
          StandardOutput: string
          StandardError: string }

    let currentRid () =
        let suffix =
            match RuntimeInformation.ProcessArchitecture with
            | Architecture.X64 -> "x64"
            | Architecture.Arm64 -> "arm64"
            | Architecture.X86 -> "x86"
            | architecture -> invalidOp $"Unsupported architecture '{architecture}'."

        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            $"win-{suffix}"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
            $"linux-{suffix}"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
            $"osx-{suffix}"
        else
            invalidOp "Unsupported operating system."

    let executablePath (directory: string) (baseName: string) =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            Path.Combine(directory, $"{baseName}.exe")
        else
            Path.Combine(directory, baseName)

    let private runProcessCore
        (workingDirectory: string)
        (fileName: string)
        (arguments: string)
        (environmentVariables: (string * string) list)
        (standardInputText: string option)
        =
        let startInfo = ProcessStartInfo()
        startInfo.WorkingDirectory <- workingDirectory
        startInfo.FileName <- fileName
        startInfo.Arguments <- arguments
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        startInfo.RedirectStandardInput <- standardInputText.IsSome

        for name, value in environmentVariables do
            startInfo.Environment[name] <- value

        use child = new Process()
        child.StartInfo <- startInfo

        if not (child.Start()) then
            invalidOp $"Failed to start process '{fileName}'."

        match standardInputText with
        | Some inputText ->
            child.StandardInput.Write(inputText)
            child.StandardInput.Close()
        | None ->
            ()

        let standardOutput = child.StandardOutput.ReadToEnd()
        let standardError = child.StandardError.ReadToEnd()
        child.WaitForExit()

        { ExitCode = child.ExitCode
          StandardOutput = standardOutput.Replace("\r\n", "\n")
          StandardError = standardError.Replace("\r\n", "\n") }

    let runProcess (workingDirectory: string) (fileName: string) (arguments: string) =
        runProcessCore workingDirectory fileName arguments [] None

    let runProcessWithEnvironment
        (workingDirectory: string)
        (fileName: string)
        (arguments: string)
        (environmentVariables: (string * string) list)
        =
        runProcessCore workingDirectory fileName arguments environmentVariables None

    let runProcessWithInput
        (workingDirectory: string)
        (fileName: string)
        (arguments: string)
        (standardInputText: string option)
        =
        runProcessCore workingDirectory fileName arguments [] standardInputText

    let runProcessWithEnvironmentAndInput
        (workingDirectory: string)
        (fileName: string)
        (arguments: string)
        (environmentVariables: (string * string) list)
        (standardInputText: string option)
        =
        runProcessCore workingDirectory fileName arguments environmentVariables standardInputText
