# Beacon Roadmap

Beacon's v1 goal is a maintainable Hindley-Milner type checker, language
server, and static checker for Python.

The v1 contract lives in [specs/v1.md](./specs/v1.md). Detailed behavior
specs cover [typing breadth](./specs/typing-breadth.md),
[narrowing](./specs/narrowing.md), [imports and stubs](./specs/imports.md),
and [dynamic Python](./specs/dynamic.md).

Near-term execution lives in [TODO.md](./TODO.md). QA workflows and e2e
validation live in [QA.md](./QA.md).

## Current Position

Beacon already has a parser, core type model, constraint solver, analyzer,
workspace index, CLI, formatter, LSP providers, editor packages, stdlib stub
embedding from the `typeshed` submodule, and broad tests.

V1 work should define the supported behavior, make it executable as tests, and
keep CLI and LSP results aligned.

## V1 Definition

Beacon v1 is viable when it can be used on a real Python workspace as a
dependable checker and editor backend:

- Local HM inference is useful without annotation-heavy code.
- Python annotations, `.pyi` stubs, protocols, decorators, generics,
  overloads, `Any`, unions, optionals, and narrowing feed one analysis model.
- Diagnostics have stable codes, severities, spans, suppressions, and
  mode-specific behavior.
- CLI, LSP, and JSON output agree on diagnostics for the same files.
- Workspace analysis handles packages, relative imports, stubs, re-exports,
  `__all__`, star imports, cycles, and cross-file symbol references.
- Dynamic Python has explicit rules for concrete inference, conservative
  diagnostics, or `Any`/unknown fallback.
- Editor features are tested against the same workspace behavior as the CLI.
- Performance is measured by repeatable checks after semantics stabilize.
- Docs state supported behavior and known limits directly.

## Product Boundary

V1 scope:

- Parser and symbol resolution for modern Python used by the checker.
- HM checking with Python-specific extensions.
- Workspace-aware type, import, data-flow, lint, and formatting diagnostics.
- Config from `beacon.toml`, `[tool.beacon]`, editor settings, and file
  directives.
- CLI `typecheck`, `analyze`, `lint`, `format`, and supported debug commands.
- LSP diagnostics, hover, completion, definition, references, rename, symbols,
  semantic tokens, inlay hints, signature help, folding, formatting, and code
  actions.
- Safe autofixes and code transformations where edits are mechanical and
  tested.
- VS Code and Zed integration.

V1 can document limits. Unsupported behavior must still produce intentional,
tested behavior instead of crashes, placeholder output, or frontend drift.

## Contract Test Bed

Milestone 1 should build a shared test bed, not a special product subpackage.
Use a representative workspace fixture rooted under one test directory, with
small modules that look like a normal Python project.

Suggested layout:

```text
fixtures/workspace/
  beacon.toml
  pyproject.toml
  stubs/
  app/
    __init__.py
    models.py
    protocols.py
    services.py
    async_jobs.py
    patterns.py
    dynamic.py
    broken.py
    subpkg/
      __init__.py
      reexports.py
```

The fixture should be shared by parser, analyzer, CLI, and LSP tests through
helpers rather than copied into each crate. Each crate can still add focused
fixtures for local edge cases.

Existing `samples/` should be backmerged into this workspace where they cover
supported behavior. After that, `samples/` should contain only clearly labeled
manual or non-contract examples, or disappear entirely.

Every contract assertion should prefer structured data over text-only checks:

- diagnostic code, severity, message fragment, span, and optional tags;
- inferred symbol type or hover text where the type is part of the contract;
- target URI/range for definition, references, rename, and workspace symbols;
- exact edit payloads for formatting, autofix, and code actions;
- stable JSON snapshots for CLI output intended for CI.

## Test Bed Coverage

The contract workspace should cover these v1 behaviors.

### Syntax And Symbols

- Functions, classes, decorators, comprehensions, lambdas, walrus expressions,
  pattern matching, async syntax, and context managers.
- Scope behavior for locals, globals, nonlocals, class bodies,
  comprehensions, imports, f-strings, and annotations.
- Python version gates for syntax and stdlib availability.

### Type Semantics

- Local inference, calls, overloads, generics, protocols, data models, async,
  and generator behavior. See [typing breadth](./specs/typing-breadth.md).
- Union/optional simplification, guard behavior, joins, reachability, and
  pattern matching. See [narrowing](./specs/narrowing.md).

### Diagnostics

- Syntax, unresolved names, imports, private imports, broken re-exports,
  circular imports, type mismatches, bad calls, attribute errors, variance,
  protocols, unsafe `Any`, and invalid casts where supported.
- Data-flow diagnostics for use-before-definition, unreachable code, unused
  symbols, and constant-branch evaluation.
- Lint diagnostics and suppressions, including unknown or overly broad
  suppression comments.
- Config diagnostics for invalid settings, invalid paths, and precedence
  between config sources.
- Mode differences for strict, balanced, and relaxed checking.

### Workspace And Stubs

- Package roots, source roots, imports, exports, stubs, and invalidation.
  See [imports and stubs](./specs/imports.md).

### CLI

- `typecheck --format json` has a golden snapshot.
- `typecheck`, `analyze`, and `lint` match LSP diagnostics for the same
  workspace.
- Human, compact, and JSON output preserve stable codes, severities, spans,
  paths, and exit behavior.
- Placeholder v1-path output is implemented, hidden, or outside the contract.

### LSP

- Open, edit, save, close, rename, and delete file flows.
- Push and request diagnostics, including clearing and version safety.
- Hover, completion, auto-import, signature help, definition, type definition,
  references, rename, document symbols, workspace symbols, highlights,
  semantic tokens, folding, inlay hints, formatting, and code actions.
- Workspace updates after import, re-export, config, and stub changes.

### Unsupported Behavior

Dynamic Python should be tested explicitly. Each case should infer a concrete
type, emit a stable diagnostic, or cross an explicit `Any`/unknown boundary.
See [dynamic Python](./specs/dynamic.md).

## Milestones

### v0.6: Contract Freeze

Freeze the v1 behavior list, diagnostic categories, config shape, fixture
layout, and CLI/LSP parity expectations. Build the shared contract workspace
and convert milestone 1 into executable tests.

### v0.7: Checker Stabilization

Close semantic holes exposed by the contract tests. Prioritize calls,
generics, protocols, narrowing, dataclasses, async/generator behavior, and
actionable diagnostics.

### v0.8: Workspace, Stubs, And Editor Actions

Make imports, stubs, invalidation, re-exports, snippets, auto-imports,
autofixes, and code transformations reliable across CLI and LSP.

### v0.9: Product Beta

Run full QA on CLI, VS Code, Zed, generic LSP clients, multi-root workspaces,
telemetry, formatter, and autofix behavior. Remove placeholder v1-path
commands and publish limitations.

### v1.0: Stable Contract

No new feature work. Fix release blockers, freeze docs, tag, and publish
editor packages.
