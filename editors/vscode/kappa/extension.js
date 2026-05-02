"use strict";

const fs = require("fs");
const path = require("path");
const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

let client = null;
let outputChannel = null;
let interpretationChannel = null;

function getConfiguration() {
    return vscode.workspace.getConfiguration("kappa");
}

function defaultServerProjectPath(context) {
    return path.resolve(context.extensionPath, "..", "..", "..", "src", "Kappa.LSP", "Kappa.LSP.fsproj");
}

function resolveServerProjectPath(context) {
    const configuredPath = getConfiguration().get("languageServer.projectPath", "").trim();
    const projectPath = configuredPath || defaultServerProjectPath(context);

    if (!path.isAbsolute(projectPath)) {
        throw new Error(`Kappa server project path must be absolute: ${projectPath}`);
    }

    if (!fs.existsSync(projectPath)) {
        throw new Error(`Kappa server project not found: ${projectPath}`);
    }

    return projectPath;
}

function resolveServerDllPath() {
    const configuredPath = getConfiguration().get("languageServer.serverDllPath", "").trim();

    if (!configuredPath) {
        return "";
    }

    if (!path.isAbsolute(configuredPath)) {
        throw new Error(`Kappa server DLL path must be absolute: ${configuredPath}`);
    }

    if (!fs.existsSync(configuredPath)) {
        throw new Error(`Kappa server DLL not found: ${configuredPath}`);
    }

    return configuredPath;
}

function createServerOptions(context) {
    const dotnetCommand = getConfiguration().get("languageServer.dotnetCommand", "dotnet").trim() || "dotnet";
    const serverDllPath = resolveServerDllPath();

    if (serverDllPath) {
        return {
            command: dotnetCommand,
            args: ["exec", serverDllPath, "--stdio"],
            options: {
                cwd: path.dirname(serverDllPath)
            }
        };
    }

    const projectPath = resolveServerProjectPath(context);
    return {
        command: dotnetCommand,
        args: ["run", "--project", projectPath, "--", "--stdio"],
        options: {
            cwd: path.dirname(projectPath)
        }
    };
}

async function startClient(context) {
    if (outputChannel === null) {
        outputChannel = vscode.window.createOutputChannel("Kappa Language Server");
        context.subscriptions.push(outputChannel);
    }

    if (interpretationChannel === null) {
        interpretationChannel = vscode.window.createOutputChannel("Kappa Interpretation");
        context.subscriptions.push(interpretationChannel);
    }

    const serverOptions = createServerOptions(context);
    const clientOptions = {
        documentSelector: [{ scheme: "file", language: "kappa" }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher("**/*.kp")
        },
        outputChannel
    };

    client = new LanguageClient("kappa", "Kappa Language Server", serverOptions, clientOptions);
    const startPromise = client.start();
    context.subscriptions.push(startPromise);
    await startPromise;
}

async function stopClient() {
    if (client !== null) {
        const current = client;
        client = null;
        await current.stop();
    }
}

async function restartClient(context) {
    await stopClient();
    await startClient(context);
}

async function interpretFunctionAtCursor() {
    if (client === null) {
        vscode.window.showErrorMessage("Kappa language server is not running.");
        return;
    }

    const editor = vscode.window.activeTextEditor;

    if (!editor || editor.document.languageId !== "kappa") {
        vscode.window.showErrorMessage("Open a Kappa source file before interpreting.");
        return;
    }

    const result = await client.sendRequest("kappa/interpretAtPosition", {
        textDocument: {
            uri: editor.document.uri.toString()
        },
        position: {
            line: editor.selection.active.line,
            character: editor.selection.active.character
        }
    });

    if (!result || result.success !== true) {
        const message = result && result.error ? result.error : "Kappa interpretation failed.";
        vscode.window.showErrorMessage(message);
        return;
    }

    interpretationChannel.clear();
    interpretationChannel.appendLine(result.binding || "<unknown>");

    if (result.mode) {
        interpretationChannel.appendLine(`mode: ${result.mode}`);
    }

    if (result.output) {
        interpretationChannel.appendLine("");
        interpretationChannel.append(result.output);

        if (!result.output.endsWith("\n")) {
            interpretationChannel.appendLine("");
        }
    }

    if (result.value) {
        interpretationChannel.appendLine("");
        interpretationChannel.appendLine(result.value);
    }

    interpretationChannel.show(true);
}

async function activate(context) {
    context.subscriptions.push(
        vscode.commands.registerCommand("kappa.restartLanguageServer", async () => {
            try {
                await restartClient(context);
            } catch (error) {
                const message = error instanceof Error ? error.message : String(error);
                vscode.window.showErrorMessage(`Failed to restart Kappa language server: ${message}`);
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand("kappa.interpretFunctionAtCursor", interpretFunctionAtCursor)
    );

    try {
        await startClient(context);
    } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        vscode.window.showErrorMessage(`Failed to start Kappa language server: ${message}`);
        throw error;
    }
}

async function deactivate() {
    await stopClient();
}

module.exports = {
    activate,
    deactivate
};
