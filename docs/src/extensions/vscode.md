# VS Code Extension

The Beacon VS Code extension (`pkg/vscode/`) pairs the Rust language server with the VS Code UI. It activates automatically for Python files and forwards editor requests to the Beacon LSP binary.

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

The launch configuration compiles the TypeScript client and relies on the previously built Rust binary. In debug mode, `RUST_LOG=beacon_lsp=debug` is set automatically so server logs appear in the “Beacon LSP” output channel.

## Configuration

The extension contributes a single user/workspace setting:

| Setting               | Values                           | Description                                                      |
| --------------------- | -------------------------------- | ---------------------------------------------------------------- |
| `beacon.trace.server` | `off` \| `messages` \| `verbose` | Controls JSON-RPC tracing between VS Code and the Beacon server. |

Enable `messages` or `verbose` while debugging protocol issues; traces are written to the “Beacon LSP” output channel.

## Packaging & Publishing

1. Ensure the client is built (`pnpm --filter beacon-lsp compile`) and the server release binary exists (`cargo build -p beacon-lsp --release`).
2. From `pkg/vscode`, run `vsce package` (or `ovsx package`) to produce a `.vsix`.
3. Publish the package with `vsce publish` or `ovsx publish` once authenticated.

The generated `.vsix` expects the server binary to be shipped alongside the extension or obtainable on the user’s machine. Adjust `extension.ts` if you plan to bundle the binary differently.

## Troubleshooting

- **Extension activates but features are missing**: confirm the `beacon-lsp` binary exists in `target/debug` or `target/release`.
- **No diagnostics shown**: check the “Beacon LSP” output channel for JSON-RPC logs and run the server manually (`cargo run -p beacon-lsp`) to ensure it starts cleanly.
- **TypeScript errors**: rerun `pnpm --filter beacon-lsp compile` and ensure dependencies are installed.
- **Protocol tracing**: set `beacon.trace.server` to `verbose` and inspect the output to verify requests/responses.

With the extension compiled and the Rust server built, you can iterate quickly—edit the TypeScript client, rebuild with `pnpm watch`, and reload the Extension Development Host (`Developer: Reload Window`) to pick up changes.
