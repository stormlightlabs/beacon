import * as cp from "child_process";
import * as path from "path";
import {
  commands,
  Diagnostic,
  DiagnosticSeverity,
  ExtensionContext,
  languages,
  Position,
  Range,
  StatusBarAlignment,
  StatusBarItem,
  window,
  workspace,
} from "vscode";

import { Executable, LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient;
let statusBarItem: StatusBarItem | undefined;
const lintDiagnostics = languages.createDiagnosticCollection("beacon-cli");

export function activate(context: ExtensionContext) {
  statusBarItem = window.createStatusBarItem(StatusBarAlignment.Right, 100);
  statusBarItem.text = "$(sync~spin) Beacon: Loading...";
  statusBarItem.tooltip = "Starting Beacon language server";
  statusBarItem.show();
  context.subscriptions.push(statusBarItem);

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

  context.subscriptions.push(lintDiagnostics);
  context.subscriptions.push(commands.registerCommand("beacon.runLint", async () => {
    await runLintCommand(context);
  }));

  client.start().then(() => {
    if (statusBarItem) {
      statusBarItem.text = "$(lightbulb-sparkle) Beacon Ready";
      statusBarItem.tooltip = "Beacon Python language server initialized";
    }
  }, (error) => {
    if (statusBarItem) {
      statusBarItem.text = "$(error-small) Beacon failed";
      statusBarItem.tooltip = `Beacon language server failed to start: ${
        error instanceof Error ? error.message : String(error)
      }`;
    }
    window.showErrorMessage(
      `Beacon language server failed to start: ${error instanceof Error ? error.message : String(error)}`,
    );
  });
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
    typeChecking: { mode: config.get("typeChecking.mode", "balanced") },
    pythonVersion: config.get("python.version", "3.12"),
    stubPaths: config.get("python.stubPaths", ["stubs"]),
    sourceRoots: config.get("workspace.sourceRoots", []),
    excludePatterns: config.get("workspace.excludePatterns", []),
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
    formatting: {
      enabled: config.get("formatting.enabled", true),
      lineLength: config.get("formatting.lineLength", 88),
      indentSize: config.get("formatting.indentSize", 4),
      quoteStyle: config.get("formatting.quoteStyle", "double"),
      trailingCommas: config.get("formatting.trailingCommas", "multiline"),
      maxBlankLines: config.get("formatting.maxBlankLines", 2),
      importSorting: config.get("formatting.importSorting", "pep8"),
      compatibilityMode: config.get("formatting.compatibilityMode", "black"),
      useTabs: config.get("formatting.useTabs", false),
      normalizeDocstringQuotes: config.get("formatting.normalizeDocstringQuotes", true),
      spacesAroundOperators: config.get("formatting.spacesAroundOperators", true),
      blankLineBeforeClass: config.get("formatting.blankLineBeforeClass", true),
      blankLineBeforeFunction: config.get("formatting.blankLineBeforeFunction", true),
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

async function runLintCommand(context: ExtensionContext) {
  const editor = window.activeTextEditor;
  if (!editor) {
    window.showInformationMessage("No active Python file to lint.");
    return;
  }

  const document = editor.document;
  if (document.languageId !== "python") {
    window.showInformationMessage("Beacon CLI lint only supports Python files.");
    return;
  }

  if (document.isDirty) {
    await document.save();
  }

  const cliPath = getCliPath(context);
  const args = ["lint", document.uri.fsPath, "--format", "json"];

  try {
    const result = await execFileAsync(cliPath, args, { cwd: getWorkspaceRoot(context) });
    const diagnostics = parseLintDiagnostics(result.stdout, document.uri.fsPath);
    lintDiagnostics.set(document.uri, diagnostics);
    window.setStatusBarMessage(`Beacon lint reported ${diagnostics.length} issue(s).`, 4000);
  } catch (error) {
    lintDiagnostics.delete(document.uri);
    window.showErrorMessage(`Beacon lint failed: ${error instanceof Error ? error.message : String(error)}`);
  }
}

function parseLintDiagnostics(output: string, filePath: string): Diagnostic[] {
  const diagnostics: Diagnostic[] = [];
  const trimmed = output.trim();
  if (!trimmed) {
    return diagnostics;
  }

  type LintResult = { file?: string; line: number; col: number; end_col?: number; rule: string; message: string };
  let parsed: LintResult[];
  try {
    parsed = JSON.parse(trimmed) as LintResult[];
  } catch (error) {
    window.showErrorMessage(`Failed to parse beacon lint output: ${error}`);
    return diagnostics;
  }

  for (const entry of parsed) {
    if (entry.file && entry.file !== filePath) {
      continue;
    }

    const start = new Position(Math.max(entry.line - 1, 0), Math.max(entry.col - 1, 0));
    const spanEnd = entry.end_col && entry.end_col > entry.col ? entry.end_col - 1 : Math.max(entry.col, 1);
    const end = new Position(Math.max(entry.line - 1, 0), Math.max(spanEnd, start.character + 1));
    const diagnostic = new Diagnostic(
      new Range(start, end),
      `[${entry.rule}] ${entry.message}`,
      DiagnosticSeverity.Warning,
    );
    diagnostics.push(diagnostic);
  }

  return diagnostics;
}

function execFileAsync(command: string, args: string[], options: cp.ExecFileOptions) {
  return new Promise<{ stdout: string; stderr: string }>((resolve, reject) => {
    cp.execFile(command, args, options, (error, stdout, stderr) => {
      if (error) {
        if (stderr) {
          reject(typeof stderr === "string" ? stderr : stderr.toString("utf8"));
          return;
        }
        reject(error);
        return;
      }
      const normalizedStdout = typeof stdout === "string" ? stdout : stdout.toString("utf8");
      const normalizedStderr = typeof stderr === "string" ? stderr : stderr.toString("utf8");
      resolve({ stdout: normalizedStdout, stderr: normalizedStderr });
    });
  });
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

function getCliPath(context: ExtensionContext): string {
  const wsRoot = getWorkspaceRoot(context);
  return path.join(wsRoot, "target", "debug", "beacon-cli");
}
