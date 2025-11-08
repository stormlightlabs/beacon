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
    synchronize: { fileEvents: workspace.createFileSystemWatcher("**/*.py"), configurationSection: "beacon" },
    initializationOptions: getBeaconConfig(),
  };

  client = new LanguageClient("beacon-lsp", "Beacon Language Server", serverOptions, clientOptions);

  context.subscriptions.push(workspace.onDidChangeConfiguration((event) => {
    if (event.affectsConfiguration("beacon")) {
      client.sendNotification("workspace/didChangeConfiguration", { settings: { beacon: getBeaconConfig() } });
    }
  }));

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

/**
 * Get Beacon configuration from workspace settings
 *
 * Reads VSCode configuration and converts it to the format expected by the LSP server.
 */
function getBeaconConfig() {
  const config = workspace.getConfiguration("beacon");

  return {
    mode: config.get("typeChecking.mode", "balanced"),
    pythonVersion: config.get("python.version", "3.12"),
    maxAnyDepth: config.get("advanced.maxAnyDepth", 3),
    incremental: config.get("advanced.incremental", true),
    workspaceAnalysis: config.get("advanced.workspaceAnalysis", true),
    enableCaching: config.get("advanced.enableCaching", true),
    cacheSize: config.get("advanced.cacheSize", 100),
    unresolvedImportSeverity: config.get("diagnostics.unresolvedImports", "warning"),
    circularImportSeverity: config.get("diagnostics.circularImports", "warning"),
    inlayHints: {
      enable: config.get("inlayHints.enable", true),
      variableTypes: config.get("inlayHints.variableTypes", true),
      functionReturnTypes: config.get("inlayHints.functionReturnTypes", true),
      parameterNames: config.get("inlayHints.parameterNames", false),
    },
    interpreterPath: config.get("python.interpreterPath", ""),
  };
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
