# Beacon Roadmap

Beacon's v1 goal is a maintainable, powerful Hindley-Milner type checker, language server, and static checker for Python.

Near-term execution lives in [TODO.md](./TODO.md). Testing, e2e coverage, and refactor validation live in [QA.md](./QA.md).

## Current Position

Beacon already has a parser, core type model, constraint solver, analyzer, workspace index, CLI, formatter, LSP providers, editor packages, stdlib stub embedding from the `typeshed` git submodule, and broad tests.

V1 work should define the supported contract, harden the checker, and route CLI and LSP behavior through the same analysis.

## V1 Definition

Beacon v1 is viable when it can be used on a real Python package as a dependable checker and editor backend:

- Local HM inference is useful without requiring annotation-heavy code.
- Python annotations, `.pyi` stubs, protocols, decorators, generics, overloads, `Any`, unions, optionals, and control-flow narrowing feed the same constraint model.
- Diagnostics have stable codes, source spans, suppression behavior, mode-specific severity, and matching CLI/LSP output.
- Workspace analysis handles packages, relative imports, stubs, re-exports, `__all__`, star imports, cycles, and cross-file symbol references with documented rules.
- Dynamic Python behavior has explicit semantics for metaprogramming, monkey-patching, custom import hooks, and runtime reflection.
- CLI and LSP agree on diagnostics, config, and workspace behavior.
- Snippets, auto-imports, autofixes, telemetry, multi-root workspaces, and formatting are part of the product surface.
- Performance is good enough for normal package development and measured by repeatable benchmarks.
- Docs state supported behavior and known limitations directly.

## V1 Product Boundary

V1 scope:

- Parser and symbol resolution for modern Python syntax used by the checker.
- HM type checking with Python-specific extensions.
- Workspace-aware static checks and import diagnostics.
- Dynamic Python support with documented fallback behavior.
- LSP diagnostics, hover, completion, goto definition, references, rename, symbols, semantic tokens, inlay hints, snippets, auto-imports, formatting, and code actions.
- CLI `typecheck`, `analyze`, `lint`, `format`, and debug commands that are reproducible outside the editor.
- Lint autofixes and formatter behavior that are safe enough for editor use.
- Telemetry and release logging with privacy controls.
- Multi-root workspace support.
- VS Code and Zed integration.

V1 can still document limits. A dynamic Python feature can be supported by precise inference, a conservative diagnostic, or an explicit unknown/`Any` boundary. The behavior must be deliberate and tested.

## Workstreams

### 1. Checker Semantics

Make the HM engine reliable for the supported Python subset.

- Value restriction and generalization for Python expressions.
- Type variable constraints, bounds, variance, and generic class behavior.
- Callable, overload, method receiver, default argument, and keyword argument constraints.
- Union/optional simplification, joins, narrowing, TypeGuard, and TypeIs.
- Protocol and structural conformance, including generic and inherited protocols.
- Async, generator, coroutine, context manager, and dataclass behavior.
- Error messages that explain the real constraint failure.

### 2. Stubs And Imports

Make stubs part of the analysis contract.

- Deterministic `typeshed` submodule bootstrap.
- Reliable embedded stdlib stub bundle and version reporting.
- Clear precedence between workspace source, workspace stubs, custom `stubPaths`, bundled stdlib, and site-packages.
- Tests for `.pyi` overrides, partial stubs, re-exports, and stale/missing stubs.

### 3. One Analysis Pipeline

Eliminate frontend drift.

- Shared diagnostic path for CLI and LSP.
- Shared config loading for `beacon.toml`, `pyproject.toml`, editor settings, and file mode directives.
- Stable diagnostic JSON shape for CI.
- Clear boundaries between syntax, name, type, data-flow, lint, taint, import, and formatting diagnostics.

### 4. Workspace Reliability

Make cross-file behavior predictable before optimizing it.

- Package and namespace package module naming.
- Relative import and re-export resolution.
- Symbol-level invalidation for changed exports/imports.
- Workspace-wide references, rename, diagnostics, and dead export analysis.
- Graceful handling of cycles and partial information.

### 5. LSP And CLI Productization

Keep only what can be validated.

- Version-safe diagnostics publishing and clearing.
- Hover/completion/navigation/refactoring backed by current symbol/type data.
- Snippet completion and auto-import behavior backed by workspace symbols and stubs.
- Autofix actions for diagnostics where the edit is mechanical and covered by tests.
- CLI commands with no placeholder output in the v1 path.
- Privacy-safe release logging with developer-grade debug logging behind flags.
- Manual editor QA for VS Code, Zed, and at least one generic LSP client.

### 6. Formatting And Lint Fixes

Make formatter and lint fixes editor-safe.

- Full-document, range, on-type, and save formatting behavior.
- Formatter config validation and compatibility notes.
- Autofixes for unused imports, redundant pass, simple import cleanup, and other mechanical rules.
- Edit safety tests that prove unrelated code is preserved.
- CLI and LSP parity for format/check/fix behavior.

### 7. Multi-Root, Telemetry, And Dynamic Python

Cover the remaining v1 product surface.

- Multi-root workspace indexing, config, diagnostics, and symbol search.
- Telemetry events for analysis timing, cache behavior, crashes, and feature errors.
- User controls for telemetry and log level.
- Conservative handling for monkey-patching, metaclasses, `__getattr__`, custom import hooks, and reflection.
- Tests that define when Beacon infers a type, emits a diagnostic, or falls back to `Any`/unknown.

### 8. Scale And Release Hardening

Measure after semantics stabilize.

- Budgets for single-file checks, cold workspace indexing, incremental reanalysis, and dependency fan-out.
- Benchmarks for normal package, large synthetic workspace, and rapid edit scenarios.
- Memory profiles for stub loading, workspace indexes, caches, and solver hot paths.
- Fuzzing or stress tests for parser/analyzer crash safety.

## Release Path

### v0.6: Contract Freeze

Freeze the v1-supported behavior list, diagnostic categories, config shape, fixture conventions, and CLI/LSP parity expectations.

### v0.7: Checker Stabilization

Close semantic holes in the HM core and constraint solver. Add fixtures before implementation changes.

### v0.8: Workspace, Stubs, And Editor Actions

Make imports, stubs, workspace diagnostics, invalidation, re-exports, snippets, auto-imports, and autofixes reliable across CLI and LSP.

### v0.9: Product Beta

Run full QA on CLI, VS Code, Zed, generic LSP clients, multi-root workspaces, telemetry, and formatter/autofix behavior. Remove placeholder v1-path commands. Publish limitations.

### v1.0: Stable Contract

No new feature work. Fix release blockers, freeze docs, tag, and publish editor packages.
