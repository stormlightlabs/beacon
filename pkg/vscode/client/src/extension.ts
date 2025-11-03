import * as path from "path";
import { ExtensionContext, workspace } from "vscode";

import { Executable, LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const serverPath = getServerPath(context);
  const runExecutable: Executable = { command: serverPath, options: { cwd: getWorkspaceRoot(context) } };
  const debugExecutable: Executable = {
    command: serverPath,
    options: { cwd: getWorkspaceRoot(context), env: { ...process.env, RUST_LOG: "beacon_lsp=debug" } },
  };
  const serverOptions: ServerOptions = { run: runExecutable, debug: debugExecutable };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "python" }],
    synchronize: { fileEvents: workspace.createFileSystemWatcher("**/*.py") },
  };

  client = new LanguageClient("beacon-lsp", "Beacon Language Server", serverOptions, clientOptions);

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

/**
 * Get the path to the Beacon LSP server binary
 */
function getWorkspaceRoot(context: ExtensionContext): string {
  const wsRoot = path.resolve(context.extensionPath, "..", "..");
  return wsRoot;
}

function getServerPath(context: ExtensionContext): string {
  const wsRoot = getWorkspaceRoot(context);
  const dbgBin = path.join(wsRoot, "target", "debug", "beacon-lsp");
  // const releaseBin = path.join(wsRoot, "target", "release", "beacon-lsp");
  //
  // const fs = require("fs");
  // if (fs.existsSync(releaseBin)) {
  //   return releaseBin;
  // }
  return dbgBin;
}
