# VS Code Extension

The Beacon VS Code extension (`pkg/vscode/`) pairs the Rust language server with the VSCode UI.
It activates automatically for Python files and forwards editor requests to the Beacon LSP binary.

## Feature Highlights

- On-type diagnostics for syntax and type errors
- Hover tooltips with type information
- Go to definition & find references
- Document and workspace symbols
- Semantic tokens for enhanced highlighting
- Identifier completions and inlay hints
- (Scaffolded) code actions for quick fixes

These capabilities mirror the features exposed by the Rust server in `crates/server`.

## Repository Layout

```sh
pkg/vscode/
├── client/                 # TypeScript client that binds to VS Code APIs
│   ├── src/extension.ts    # Extension entry point; starts the LSP client
│   └── src/test/           # End-to-end tests using the VS Code test runner
├── package.json            # Extension manifest (activation, contributions)
├── tsconfig.json           # TypeScript project references
├── eslint.config.js        # Lint configuration
└── dprint.json             # Formatting config for client sources
```

The client launches the Beacon server binary from `target/debug/beacon-lsp` (or `target/release/beacon-lsp` if present). Ensure one of these binaries exists before activating the extension.

## Prerequisites

- **Rust toolchain** (stable) with `cargo` available in `PATH`
- **Node.js 18+** (aligned with current VS Code requirements)
- **pnpm** for dependency management
    Install globally with `npm install -g pnpm`
- **VS Code** ≥ 1.100 (see `package.json` `engines` field)
- _(Optional)_ [`vsce`](https://code.visualstudio.com/api/working-with-extensions/publishing-extension) or `ovsx` for packaging/publishing

## Installing Dependencies

From the repository root:

```bash
pnpm install
```

This installs dependencies for all packages, including the VS Code extension.

## Building The Extension Client

The extension compiles TypeScript into `client/out/`:

```bash
pnpm --filter beacon-lsp compile
```

For iterative development, run:

```bash
pnpm --filter beacon-lsp watch
```

This keeps the TypeScript project in watch mode so recompiles happen automatically after you edit client files.

## Building The Beacon LSP Server

The client resolves the server binary relative to the repository root:

```sh
target/debug/beacon-lsp    (default)
target/release/beacon-lsp  (used if available)
```

Build the server before launching the extension:

```bash
cargo build -p beacon-lsp              # debug binary
# or
cargo build -p beacon-lsp --release    # release binary
```

## Running In VS Code

1. Open `pkg/vscode` in VS Code.
2. Select the **Run and Debug** panel and choose the **Beacon LSP** launch configuration (provided in `.vscode/launch.json`).
3. Press **F5** to start the Extension Development Host.
4. In the new window, open a Python file (the repository’s `samples/` directory is a good starting point).

The launch configuration compiles the TypeScript client and relies on the previously built Rust binary.
In debug mode, `RUST_LOG=beacon_lsp=debug` is set automatically so server logs appear in the “Beacon LSP” output channel.

## Configuration

The extension provides extensive configuration options accessible through VS Code settings.
All settings are under the `beacon.*` namespace and can be configured per-workspace or globally.

### Type Checking

| Setting                    | Type     | Default      | Description                                                      |
| -------------------------- | -------- | ------------ | ---------------------------------------------------------------- |
| `beacon.typeChecking.mode` | `string` | `"balanced"` | Type checking strictness: `"strict"`, `"balanced"`, or `"loose"` |

### Inlay Hints

| Setting                                 | Type      | Default | Description                                         |
| --------------------------------------- | --------- | ------- | --------------------------------------------------- |
| `beacon.inlayHints.enable`              | `boolean` | `true`  | Enable inlay hints for type information             |
| `beacon.inlayHints.variableTypes`       | `boolean` | `true`  | Show inlay hints for inferred variable types        |
| `beacon.inlayHints.functionReturnTypes` | `boolean` | `true`  | Show inlay hints for inferred function return types |
| `beacon.inlayHints.parameterNames`      | `boolean` | `false` | Show inlay hints for parameter names in calls       |

### Python Settings

| Setting                         | Type       | Default     | Description                                                            |
| ------------------------------- | ---------- | ----------- | ---------------------------------------------------------------------- |
| `beacon.python.version`         | `string`   | `"3.12"`    | Target Python version: `"3.9"`, `"3.10"`, `"3.11"`, `"3.12"`, `"3.13"` |
| `beacon.python.interpreterPath` | `string`   | `""`        | Path to Python interpreter for runtime introspection                   |
| `beacon.python.stubPaths`       | `string[]` | `["stubs"]` | Additional paths to search for .pyi stub files                         |

### Workspace Settings

| Setting                            | Type       | Default | Description                                                        |
| ---------------------------------- | ---------- | ------- | ------------------------------------------------------------------ |
| `beacon.workspace.sourceRoots`     | `string[]` | `[]`    | Source roots for module resolution (in addition to workspace root) |
| `beacon.workspace.excludePatterns` | `string[]` | `[]`    | Patterns to exclude from workspace scanning (e.g., venv/, .venv/)  |

### Diagnostics

| Setting                                | Type     | Default     | Description                                                       |
| -------------------------------------- | -------- | ----------- | ----------------------------------------------------------------- |
| `beacon.diagnostics.unresolvedImports` | `string` | `"warning"` | Severity for unresolved imports: `"error"`, `"warning"`, `"info"` |
| `beacon.diagnostics.circularImports`   | `string` | `"warning"` | Severity for circular imports: `"error"`, `"warning"`, `"info"`   |

### Advanced

| Setting                             | Type      | Default | Description                                              |
| ----------------------------------- | --------- | ------- | -------------------------------------------------------- |
| `beacon.advanced.maxAnyDepth`       | `number`  | `3`     | Maximum depth for Any type propagation (0-10)            |
| `beacon.advanced.incremental`       | `boolean` | `true`  | Enable incremental type checking                         |
| `beacon.advanced.workspaceAnalysis` | `boolean` | `true`  | Enable workspace-wide analysis                           |
| `beacon.advanced.enableCaching`     | `boolean` | `true`  | Enable caching of parse trees and type inference results |
| `beacon.advanced.cacheSize`         | `number`  | `100`   | Maximum number of documents to cache (0-1000)            |

### Debugging

| Setting               | Type     | Default | Description                                             |
| --------------------- | -------- | ------- | ------------------------------------------------------- |
| `beacon.trace.server` | `string` | `"off"` | JSON-RPC tracing: `"off"`, `"messages"`, or `"verbose"` |

Enable `messages` or `verbose` while debugging protocol issues; traces are written to the "Beacon LSP" output channel.

### Example Configuration

Add these settings to your `.vscode/settings.json`:

```json
{
  "beacon.typeChecking.mode": "strict",
  "beacon.python.version": "3.12",
  "beacon.python.stubPaths": ["stubs", "typings"],
  "beacon.workspace.sourceRoots": ["src", "lib"],
  "beacon.workspace.excludePatterns": [
      "**/venv/**",
      "**/.venv/**",
      "**/build/**"
  ],
  "beacon.inlayHints.enable": true,
  "beacon.inlayHints.variableTypes": true,
  "beacon.inlayHints.functionReturnTypes": true,
  "beacon.diagnostics.unresolvedImports": "error",
  "beacon.diagnostics.circularImports": "warning"
}
```

### Configuration Precedence

Beacon merges configuration from multiple sources:

1. **Default values** - Built-in defaults
2. **TOML file** - `beacon.toml` or `pyproject.toml` in workspace root
3. **VS Code settings** - User/workspace settings (highest precedence)

See [Configuration](../configuration.md) for details on TOML configuration files.

## Packaging & Publishing

1. Ensure the client is built (`pnpm --filter beacon-lsp compile`) and the server release binary exists (`cargo build -p beacon-lsp --release`).
2. From `pkg/vscode`, run `vsce package` (or `ovsx package`) to produce a `.vsix`.
3. Publish the package with `vsce publish` or `ovsx publish` once authenticated.

The generated `.vsix` expects the server binary to be shipped alongside the extension or obtainable on the user’s machine.
Adjust `extension.ts` if you plan to bundle the binary differently.
