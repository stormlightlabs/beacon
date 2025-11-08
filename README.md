# Beacon

[![codecov](https://codecov.io/gh/stormlightlabs/beacon/branch/main/graph/badge.svg)](https://codecov.io/gh/stormlightlabs/beacon)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
![Status - CI/CD](https://github.com/stormlightlabs/beacon/actions/workflows/ci.yml/badge.svg)

Beacon is a Rust implementation of the language server protocol & a hindley-milner type system for Python, inspired by languages like F# & OCaml and the work of [Astral](https://astral.sh).

## Project

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

## Quick Start

### Installation

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

### Type Checking

Check Python files for type errors using Hindley-Milner inference:

```sh
# Check a file
beacon typecheck example.py

# Check with JSON output for CI
beacon typecheck --format json example.py

# Check from stdin
cat example.py | beacon typecheck
```

### Language Server

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

### Debug Tools

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

Full documentation: [CLI Tools](./docs/src/cli/overview.md)

## Completions & Static Analysis

### Completions

The `CompletionProvider` looks at the latest document snapshot (text, AST, and symbol tables) to suggest in-scope names, imports, and attribute members the moment you type or hit the `"."` trigger.

### Feedback

Beacon's static analyzer (in `crates/server/src/analysis`) combines [Hindley-Milner](#why-hm-for-python) style inference with control/data-flow passes.

**Inlay hints** display inline type information throughout your code:

- Type hints for assignments without explicit annotations
- Return type hints for functions without return type annotations
- Parameter name hints in function calls

### Shared insight

Both systems reuse the same analyzer output, so upcoming features like type-filtered completions or richer hover details piggyback on the existing substitution data instead of re-walking files.

## Development

### Editor Extensions

Beacon supports VS Code, Zed, and Neovim through the Language Server Protocol.

See [Editor Extensions Documentation](./docs/src/extensions/overview.md) for setup instructions.

**Quick Links**:

- [VS Code](./docs/src/extensions/vscode.md)
- [Zed](./docs/src/extensions/zed.md)
- [Neovim](./docs/src/extensions/overview.md#neovim-integration)

## Goals

Deliver an **incremental, performant, pragmatic** Hindley-Milner (HM) type inference and checking engine for Python that integrates with modern editor tooling via the Language Server Protocol (LSP).
The system should support Python’s dynamic features thoughtfully, interoperate with `typing` hints, and scale to multi-file projects.

### Why HM for Python?

HM type systems provide principled inference (no annotations required), compositional reasoning, strong guarantees, & fast unification-based algorithms (Algorithm W family).

#### Challenges

- Pervasive dynamism (monkey-patching, `__getattr__`, metaclasses, duck typing, runtime reflection),
- Nominal & structural patterns mixed
- Subtyping-ish expectations (`None`, unions, protocols)
- First-class classes & functions
- Decorators
- Generators
- Async
- Pattern matching (PEP 634).

### Design

**HM core + pragmatic extensions**, with a **gradual boundary** to accommodate Python idioms and annotations:

HM for expressions and local bindings.

Controlled **subtyping-like** features via **union/optionals** and **protocols/structural constraints**.

**Annotation-aware**: treat PEP 484/PEP 695 types as constraints and hints.

**Soundness modes**: "strict", "balanced", "loose" (affecting treatment of `Any`, unknown attributes, dynamic imports).

```text
               ┌──────────────────────────────────────────────────┐
               │                  LSP Frontend                    │
               │ (tower-lsp or custom) using lsp-types for models │
               └───────────────▲───────────────────────▲──────────┘
                               │                       │
                     Requests / Notifications    Diagnostics, hovers
                               │                       │
┌──────────────────────────────┼───────────────────────┼────────────────────────────┐
│                          Language Server Core                                     │
│  ┌───────────────────────┐  ┌──────────────────────┐  ┌────────────────────────┐  │
│  │   Document Manager    │  │    Project Graph     │  │     Incremental Index  │  │
│  │ (text, versions, TS   │  │ (imports, deps,      │  │ (symbols, stubs,       │  │
│  │  parse trees)         │  │  module cache)       │  │  types, caches)        │  │
│  └──────────▲────────────┘  └──────────▲───────────┘  └──────────▲─────────────┘  │
│             │                           │                          │              │
│     ┌───────┴────────┐        ┌─────────┴──────────┐      ┌────────┴────────────┐ │
│     │  Tree-sitter   │        │  Constraint Gen    │      │   Solver / Types    │ │
│     │  Parser (Py)   │        │ (walk TS AST,      │      │ (unification,       │ │
│     │  + lossless    │        │  produce HM +      │      │  polymorphism,      │ │
│     │  syntax facts) │        │  extensions)       │      │  row/structural)    │ │
│     └───────▲────────┘        └─────────▲──────────┘      └────────▲────────────┘ │
│             │                           │                          │              │
│             └─────── Source -> AST  ────┴── Constraints ───────────┘              │
└───────────────────────────────────────────────────────────────────────────────────┘
```

---

Made with ⚡️ by Stormlight Labs.

Stormlight Labs is just me, [Owais](https://github.com/desertthunder). Support my work on [Ko-fi](https://ko-fi.com/desertthunder).

[![Brainmade](https://brainmade.org/88x31-dark.png)](https://brainmade.org)
