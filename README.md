# Beacon

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
│
└── pkg/                # Editor extensions & plugins
```

## Goals

Deliver an **incremental, performant, pragmatic** Hindley–Milner (HM) type inference and checking engine for Python that integrates with modern editor tooling via the Language Server Protocol (LSP).
The system should support Python’s dynamic features thoughtfully, interoperate with `typing` hints, and scale to multi-file projects.

### Why HM for Python?

- HM type systems provide principled inference (no annotations required), compositional reasoning, strong guarantees, & fast unification-based algorithms (Algorithm W family).
- Challenges (related to type checking a dynamic language like Python)
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

- HM for expressions and local bindings.
- Controlled **subtyping-like** features via **union/optionals** and **protocols/structural constraints**.
- **Annotation-aware**: treat PEP 484/PEP 695 types as constraints and hints.
- **Soundness modes**: "strict", "balanced", "loose" (affecting treatment of `Any`, unknown attributes, dynamic imports).

## High-Level Architecture

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
│             └──────── Source → AST  ────┴── Constraints ───────────┘              │
└───────────────────────────────────────────────────────────────────────────────────┘
```

## Local Development & Debugging

### VSCode Extension Setup

The VSCode extension allows you to run and debug the Beacon LSP server locally.

#### Prerequisites

- Rust toolchain (cargo, rustc)
- Node.js and npm
- VSCode

#### Initial Setup

1. Install dependencies:

   ```bash
   pnpm install
   ```

2. Build the Rust LSP server:

   ```bash
   cargo build --package beacon-lsp
   ```

3. Compile the TypeScript client:

   ```bash
   pnpm --filter beacon-lsp compile
   ```

#### Running the Extension

1. Open the project root in VSCode:

   ```bash
   code .
   ```

2. Press F5 or select "Launch Beacon Extension" from the Run and Debug panel

3. This will:
   - Build the Rust LSP server in debug mode
   - Compile the TypeScript client
   - Open a new Extension Development Host window
   - Load the `samples/` directory with Python test files

4. Open a Python file (`.py`) to activate the extension

#### Development Workflow

**Watch Mode:**

- Use "Launch Extension (watch mode)" configuration
- TypeScript changes will automatically recompile
- Reload the extension window (Cmd+R / Ctrl+R) to see TypeScript changes
- For Rust changes, rebuild with `cargo build --package beacon-lsp` and reload

**Manual Build:**

- Build Rust server: `cargo build --package beacon-lsp`
- Build TypeScript: `pnpm --filter beacon-lsp compile`

**Tasks Available:**

- `cargo build (debug)` - Build Rust server in debug mode
- `cargo build (release)` - Build Rust server in release mode (faster)
- `pnpm: compile` - Compile TypeScript client
- `pnpm: watch` - Watch and recompile TypeScript changes
- `build extension` - Build both Rust server and TypeScript client

#### Debugging

**Rust Server Logs:**

- Server logs are written to stderr
- View in Output panel → "Beacon Language Server"
- Set `RUST_LOG=beacon_lsp=debug` for verbose logging (configured in launch.json)

**TypeScript Client Debugging:**

- Set breakpoints in `client/src/extension.ts`
- Use Debug Console in the main VSCode window

**LSP Communication:**

- Enable trace in VSCode settings: `"beacon.trace.server": "verbose"`
- View LSP messages in Output panel → "Beacon Language Server"

#### Testing Changes

1. Make changes to Rust server code
2. Rebuild: `cargo build --package beacon-lsp`
3. Reload Extension Development Host window (Cmd+R / Ctrl+R)
4. Test with Python files in `samples/`

#### Troubleshooting

**Extension doesn't activate:**

- Check that you're opening a `.py` file
- Check Output panel for errors

**Server not found:**

- Verify binary exists: `ls target/debug/beacon-lsp`
- Rebuild: `cargo build --package beacon-lsp`

**No diagnostics/features:**

- Check server logs in Output panel
- Enable verbose logging: `"beacon.trace.server": "verbose"`
- Check for Rust compilation errors
