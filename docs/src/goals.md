# Goals

Deliver an **incremental, performant, pragmatic** Hindley-Milner (HM) type inference and checking engine for Python that integrates with modern editor tooling via the Language Server Protocol (LSP).
The system should support Python’s dynamic features thoughtfully, interoperate with `typing` hints, and scale to multi-file projects.

## Why HM for Python?

HM type systems provide principled inference (no annotations required), compositional reasoning, strong guarantees, & fast unification-based algorithms (Algorithm W family).

### Challenges

- Pervasive dynamism (monkey-patching, `__getattr__`, metaclasses, duck typing, runtime reflection),
- Nominal & structural patterns mixed
- Subtyping-ish expectations (`None`, unions, protocols)
- First-class classes & functions
- Decorators
- Generators
- Async
- Pattern matching (PEP 634).

## Design

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

## LSP Implementation Goals

Beacon's LSP focuses on delivering a fast, editor-friendly surface for the Beacon analyzer without overcommitting to unfinished infrastructure. The current goals fall into five themes.

### Primary Goals

**Immediate feedback**: run parsing and type analysis on every edit so diagnostics stay in sync with the buffer.

**Core navigation**: support hover, go-to-definition, references, and symbol search for rapid code exploration.

**Authoring assistance**: provide completions, document symbols, inlay hints, and semantic tokens to guide editing.

**Refactoring primitives**: offer reliable rename support and lay the groundwork for richer code actions.

**Modular design**: isolate feature logic behind provider traits so contributors can evolve features independently.

### Out-of-Scope (For Now)

- **Full workspace indexing**: we limit operations to open documents until indexing and cache management mature.
- **Formatting and linting**: formatting endpoints and lint integrations are planned but not part of the initial release.
- **Editor-specific UX**: we stick to LSP-standard capabilities instead of bespoke VS Code UI components.
