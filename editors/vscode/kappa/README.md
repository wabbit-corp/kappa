# Kappa VS Code Extension

This extension starts `Kappa.LSP` over stdio and enables diagnostics plus basic syntax highlighting for `.kp` files.

Usage:

1. From the repository root, run `powershell -ExecutionPolicy Bypass -File scripts/install-vscode-extension.ps1`.
2. Restart VS Code if it was already running.
3. Open a workspace containing `.kp` files.

For extension development, open the `editors/vscode/kappa` folder in VS Code and run the `Run Kappa Extension` launch configuration from `.vscode/launch.json`.

Use `Kappa: Interpret Function at Cursor` from the command palette to evaluate the top-level binding under the cursor. Pure values are normalized and displayed. `IO`/`UIO` values are executed to completion and any printed output is captured in the `Kappa Interpretation` output channel.

By default the extension launches:

```text
dotnet run --project <repo>/src/Kappa.LSP/Kappa.LSP.fsproj -- --stdio
```

The install script packages the extension into `artifacts/vscode/kappa-vscode.vsix`, installs it with the local `code` CLI, and configures `kappa.languageServer.projectPath` to this checkout's `src/Kappa.LSP/Kappa.LSP.fsproj`.
It also builds `src/Kappa.LSP/bin/Debug/net10.0/Kappa.LSP.dll` and configures `kappa.languageServer.serverDllPath`, so the installed extension can start the server without rebuilding the compiler on activation.
