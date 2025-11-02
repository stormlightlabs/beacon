import * as path from "path";
import { ExtensionContext, workspace } from "vscode";

import { Executable, LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const serverPath = getServerPath(context);
  const serverOptions: ServerOptions = {
    run: { command: serverPath } as Executable,
    debug: { command: serverPath, options: { env: { ...process.env, RUST_LOG: "beacon_lsp=debug" } } } as Executable,
  };

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
function getServerPath(context: ExtensionContext): string {
  const wsRoot = path.resolve(context.extensionPath, "..", "..");
  const dbgBin = path.join(wsRoot, "target", "debug", "beacon-lsp");
  // const releaseBin = path.join(wsRoot, "target", "release", "beacon-lsp");
  //
  // const fs = require("fs");
  // if (fs.existsSync(releaseBin)) {
  //   return releaseBin;
  // }
  return dbgBin;
}
