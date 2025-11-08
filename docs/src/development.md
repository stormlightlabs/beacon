# Development Quick Start

## Installation

Build from source:

```sh
cargo build --release
```

The CLI will be available at `target/release/beacon`.

Install system-wide:

```sh
cargo install --path crates/cli
```

This installs the `beacon` binary to `~/.cargo/bin`.

## Type Checking

Check Python files for type errors using Hindley-Milner inference:

```sh
# Check a file
beacon typecheck example.py

# Check with JSON output for CI
beacon typecheck --format json example.py

# Check from stdin
cat example.py | beacon typecheck
```

## Language Server

Install beacon-lsp system-wide:

```sh
cargo install --path crates/server
```

This installs the `beacon-lsp` binary to `~/.cargo/bin`, making it available in your PATH.

Start the LSP server for editor integration:

```sh
beacon-lsp
```

Or use the CLI:

```sh
beacon lsp
```

For debugging, start with file logging:

```sh
beacon lsp --log-file /tmp/beacon.log
```

## Debug Tools

Debug builds include additional tools for inspecting the type system:

```sh
# Build in debug mode
cargo build

# View tree-sitter CST
target/debug/beacon debug tree example.py

# Show AST with inferred types
target/debug/beacon debug ast example.py

# Display generated constraints
target/debug/beacon debug constraints example.py

# Show unification results
target/debug/beacon debug unify example.py
```

Note: Debug commands are only available in debug builds (compiled with `cargo build`), not in release builds.

Full documentation: [CLI Tools](./cli/overview.md)

## Editor Extensions

Beacon supports VS Code, Zed, and Neovim through the Language Server Protocol.

See [Editor Extensions Documentation](./extensions/overview.md) for setup instructions.

**Quick Links**:

- [VS Code](./extensions/vscode.md)
- [Zed](./extensions/zed.md)
- [Neovim](./extensions/overview.md#neovim-integration)

## Project Structure

```sh
.
├─ crates/
│  ├─ cli/              # `beacon-cli` entry point with clap
│  ├─ server/           # `beacon-lsp` LSP server (tower-lsp or raw) using lsp-types
│  ├─ core/             # `beacon-core` type definitions, solver, unifier
│  ├─ constraints/      # `beacon-constraint` constraint generation
│  └─ parser/           # `beacon-parser` tree-sitter Python adapter
└── pkg/                # Editor extensions & plugins
```
