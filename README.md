# Beacon

Beacon is a Rust implementation of the language server protocol & a hindley-milner type system for Python, inspired by languages like F# & OCaml and the work of [Astral](https://astral.sh).

## Project

```sh
crates/
  cli/              # `beacon-cli` entry point with clap
  server/           # `beacon-lsp` LSP server (tower-lsp or raw) using lsp-types
  core/             # `beacon-core` type definitions, solver, unifier
  constraints/      # `beacon-constraint` constraint generation
  parser/           # `beacon-parser` tree-sitter Python adapter
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
