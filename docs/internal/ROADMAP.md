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

### 2. Core Type API Architecture

Keep core narrow, stable, and dependency-light.

- Split type model, constructors, normalization, subtyping, display, type schemes, and overloads into smaller modules.
- Split unification by responsibility: variables, applications and variance, unions, records, tuples, forall, and error reporting.
- Provide shared helpers for builtin names, literal/base-type conversion, type decomposition, `Any` detection, type-variable detection, and diagnostic formatting.
- Keep annotation parsing, class metadata, protocol metadata, and overload resolution as stable APIs used by constraints, analyzer, server, and CLI.
- Decide whether any constraint data belongs in core without adding parser, analyzer, or server dependencies.
- Replace broad exports with an intentional public surface after downstream crates migrate.

### 3. Parser And Symbol Architecture

Keep syntax and symbol behavior stable while making parser work easier to isolate.

- Split AST models from tree-sitter CST conversion.
- Split CST conversion by syntax family: definitions, statements, expressions, literals, imports, comprehensions, and patterns.
- Share node, span, body, and text extraction helpers instead of repeating tree-sitter cursor loops.
- Keep literal parsing, import parsing, decorator extraction, docstring extraction, and pattern parsing covered by focused tests.
- Split symbol resolution into symbol model, scope table, definition pass, reference pass, annotation references, f-string references, and query helpers.
- Preserve spans for diagnostics, hover, goto, references, rename, and semantic tokens.

### 4. Constraint Solver Architecture

Keep solver behavior stable while making checker work easier to review.

- Split the main solve loop from equality, call, attribute, protocol, pattern, join, and error-recovery handling.
- Move constraint models, spans, type error wrappers, type guard metadata, and control-flow tracking out of `lib.rs`.
- Define the `Constraint::Narrowing` contract: solver output, analyzer-only state, or replacement by more concrete constraints.
- Deduplicate pattern and class compatibility rules across narrowing, validation, and exhaustiveness.
- Keep protocol variance, callable compatibility, generator/coroutine compatibility, and attribute lookup covered by characterization tests.
- Decide whether constraint data stays crate-local or moves into `beacon-core`.

### 5. Stubs And Imports

Make stubs part of the analysis contract.

- Deterministic `typeshed` submodule bootstrap.
- Reliable embedded stdlib stub bundle and version reporting.
- Clear precedence between workspace source, workspace stubs, custom `stubPaths`, bundled stdlib, and site-packages.
- Tests for `.pyi` overrides, partial stubs, re-exports, and stale/missing stubs.

### 6. Analyzer Architecture

Make the analyzer crate easier to extend without changing behavior.

- Split CFG graph data, builders, call graph resolution, and workspace wrappers.
- Split the walker around Python constructs and analysis phases.
- Share AST traversal helpers across data-flow, taint, lint, CFG, and type walking.
- Keep BEA lint codes, diagnostic spans, and suppression behavior stable while rule code moves.
- Separate stub loading concerns: cache, TypeVars, annotations, class registry, and method extraction.
- Replace production `todo!()` paths with implemented behavior or explicit diagnostics.
- Reduce public exports so CLI and LSP depend on the analyzer contract instead of internal modules.

### 7. One Analysis Pipeline

Eliminate frontend drift.

- Shared diagnostic path for CLI and LSP.
- Shared config loading for `beacon.toml`, `pyproject.toml`, editor settings, and file mode directives.
- Stable diagnostic JSON shape for CI.
- Clear boundaries between syntax, name, type, data-flow, lint, taint, import, and formatting diagnostics.

### 8. Workspace Reliability

Make cross-file behavior predictable before optimizing it.

- Package and namespace package module naming.
- Relative import and re-export resolution.
- Symbol-level invalidation for changed exports/imports.
- Workspace-wide references, rename, diagnostics, and dead export analysis.
- Graceful handling of cycles and partial information.

### 9. LSP And CLI Productization

Keep only what can be validated.

- Version-safe diagnostics publishing and clearing.
- Hover/completion/navigation/refactoring backed by current symbol/type data.
- Snippet completion and auto-import behavior backed by workspace symbols and stubs.
- Autofix actions for diagnostics where the edit is mechanical and covered by tests.
- CLI commands with no placeholder output in the v1 path.
- Privacy-safe release logging with developer-grade debug logging behind flags.
- Manual editor QA for VS Code, Zed, and at least one generic LSP client.

### 10. Formatting And Lint Fixes

Make formatter and lint fixes editor-safe.

- Full-document, range, on-type, and save formatting behavior.
- Formatter config validation and compatibility notes.
- Autofixes for unused imports, redundant pass, simple import cleanup, and other mechanical rules.
- Edit safety tests that prove unrelated code is preserved.
- CLI and LSP parity for format/check/fix behavior.

### 11. Multi-Root, Telemetry, And Dynamic Python

Cover the remaining v1 product surface.

- Multi-root workspace indexing, config, diagnostics, and symbol search.
- Telemetry events for analysis timing, cache behavior, crashes, and feature errors.
- User controls for telemetry and log level.
- Conservative handling for monkey-patching, metaclasses, `__getattr__`, custom import hooks, and reflection.
- Tests that define when Beacon infers a type, emits a diagnostic, or falls back to `Any`/unknown.

### 12. Scale And Release Hardening

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
