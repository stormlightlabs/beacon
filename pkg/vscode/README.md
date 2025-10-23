# Beacon LSP Extension

VSCode extension for Beacon Language Server - Hindley-Milner type inference for Python.

## Features

This extension provides advanced type inference and analysis for Python files using the Beacon LSP server.
Features include:

- Diagnostics (syntax and semantic errors)
- Hover information (type information)
- Go to definition
- Document symbols (outline view)
- Semantic tokens (enhanced syntax highlighting)
- Completions
- Find references
- Inlay hints
- Code actions

## Structure

```sh
.
├── client                 # VSCode Extension Client
│   ├── src
│   │   ├── test           # E2E tests
│   │   └── extension.ts   # Extension entry point
├── package.json           # Extension manifest
└── .vscode
    ├── launch.json        # Debug configurations
    └── tasks.json         # Build tasks
```

The LSP server is implemented in Rust and located at `../../crates/server`.

## Development Setup

1. Install dependencies (from project root):

   ```bash
   pnpm install
   ```

2. Build the extension and Rust LSP server:

   ```bash
   pnpm --filter beacon-lsp compile
   cargo build --package=beacon-lsp
   ```

3. Open VSCode in this folder:

   ```bash
   code .
   ```

4. Press F5 to launch the extension in debug mode

5. In the Extension Development Host window, open a Python file from the `samples/` directory

## Running the Extension

Open the project root (not `pkg/vscode/`) in VSCode, then press F5.

The launch configuration will:

- Build the Rust LSP server (debug mode)
- Compile the TypeScript client
- Open a new VSCode window with the extension loaded
- Open the `samples/` directory with Python test files

**Note:** Launch configurations are in `../../.vscode/launch.json` at the project root.
