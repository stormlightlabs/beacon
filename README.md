# Beacon

[![codecov](https://codecov.io/gh/stormlightlabs/beacon/branch/main/graph/badge.svg)](https://codecov.io/gh/stormlightlabs/beacon)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
![Status - CI/CD](https://github.com/stormlightlabs/beacon/actions/workflows/ci.yml/badge.svg)

Beacon is a Rust implementation of the language server protocol & a hindley-milner type system for Python, inspired by languages like F# & OCaml and the work of [Astral](https://astral.sh) & [BASED](https://docs.basedpyright.com/latest/) pyright.

## Features

Beacon provides advanced type inference and static analysis for Python:

- Hindley-Milner type inference with generics and constraints
- Type narrowing through pattern matching and control flow
- Real-time diagnostics for syntax, semantic, and type errors
- Code intelligence: hover, completions, navigation, symbol search
- Code actions and refactoring: quick fixes, renaming, protocol implementations
- Semantic highlighting and inlay hints
- Code formatting and static analysis

[Full documentation](https://stormlightlabs.github.io/beacon/)

## Editor Support

Beacon works with any LSP-compatible editor:

- [VS Code / VSCodium](./pkg/vscode/README.md)
- [Zed](./pkg/zed/README.md)
- Neovim (via standard LSP client)
- Helix (via standard LSP client)

## Usage Modes

### Stdio (Default)

For editor integration, Beacon uses stdio mode by default:

```bash
# Invoke the lsp binary directly or use the cli
beacon-lsp # or beacon lsp
```

### TCP

For debugging, testing, or remote connections, use TCP mode:

```bash
beacon lsp --tcp --host 127.0.0.1 --port 9350
```

## Quick Start

See the [Development Quick Start Guide](./docs/src/development.md) for more detailed installation, usage, and editor setup instructions

### Build & Run from Source

Install the prerequisites listed in [CONTRIBUTING.md](./CONTRIBUTING.md#prerequisites), then clone the repo and bootstrap:

```sh
git clone https://github.com/stormlightlabs/beacon
pnpm install && cargo fetch
```

Use cargo or the `just` recipes to build (`just build`), then run directly

```sh
# CLI
cargo run -p beacon-cli -- typecheck samples/basic.py
# Language server (stdio mode)
cargo run -p beacon-lsp -- --stdio
```

---

Made with ⚡️ by Stormlight Labs.

Stormlight Labs is just me, [Owais](https://github.com/desertthunder). Sponsor my work [here on github](https://github.com/sponsors/desertthunder)
