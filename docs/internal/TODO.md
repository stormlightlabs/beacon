# Beacon TODO

## Milestone 0: Define The Contract

- [ ] Document submodule setup in public contributor docs:
  - `git submodule update --init --recursive`
  - expected `typeshed/stubs` layout
  - what "Bundled 7 typeshed stubs" means at build time
- [x] Treat ignored doctests as intentional examples unless a future change makes one worth promoting to a runnable test.
- [ ] Decide which commands form the v1 release gate.
- [ ] Decide which fixtures define the v1 supported Python subset.
- [ ] Decide the v1 diagnostic/config compatibility policy.

## Milestone 1: Write The V1 Contract As Tests

- [ ] Create one shared multi-file "v1 package" fixture used by parser, analyzer, CLI, and LSP tests.
- [ ] Add expected diagnostics as structured assertions: code, severity, message fragment, span.
- [ ] Add golden JSON output for `beacon typecheck --format json`.
- [ ] Add LSP fixture coverage for open, edit, save, close, rename, and delete.
- [ ] Add config fixtures for `beacon.toml`, `pyproject.toml`, editor settings, and file mode directives.
- [ ] Add tests for unsupported behavior that assert graceful fallback.

## Milestone 2: Unify Diagnostics Across Frontends

- [ ] Confirm CLI `typecheck`, `analyze`, and `lint` use the same diagnostic provider as LSP wherever possible.
- [ ] Remove, implement, or hide placeholder v1-path CLI output:
  - unification trace
  - CFG visualization
  - inferred type display
- [ ] Verify suppressions apply consistently across type, lint, data-flow, import, and formatting diagnostics.
- [ ] Freeze the v1 diagnostic JSON shape.
- [ ] Ensure strict/balanced/relaxed modes change severity predictably.

## Milestone 3: Checker Reliability Pass

- [ ] Audit value restriction/generalization behavior and add regression fixtures.
- [ ] Add focused call-constraint tests for positional, keyword, defaulted, variadic, and bound receiver calls.
- [ ] Add generic class variance inference fixtures.
- [ ] Expand protocol tests for inheritance, generic protocols, bounds, and missing methods.
- [ ] Expand TypeGuard/TypeIs narrowing beyond simple first-argument cases.
- [ ] Add async/generator/coroutine/context-manager fixtures that assert inferred types.
- [ ] Review unification errors for actionable messages and source spans.

## Milestone 4: Stubs And Workspace Reliability

- [ ] Add setup validation for absent or partially initialized `typeshed`.
- [ ] Report embedded stub version/hash/count in a debug or version command.
- [ ] Test precedence between workspace `.py`, workspace `.pyi`, custom `stubPaths`, bundled stdlib, and site-packages.
- [ ] Test package roots, namespace packages, relative imports, star imports, private imports, and re-export chains in one fixture style.
- [ ] Verify dependency invalidation when an imported file, exported symbol, or stub changes.
- [ ] Add workspace-wide reference/rename tests that include imports and re-exports.

## Milestone 5: LSP And Editor Hardening

- [ ] Add version-safe diagnostics publishing tests.
- [ ] Verify hover, completion, goto definition, references, rename, symbols, semantic tokens, and inlay hints against the v1 fixture.
- [ ] Add snippet completion tests using scope, imports, and inferred types.
- [ ] Add auto-import tests using workspace symbols, stubs, and re-exports.
- [ ] Add refactoring edit tests for extract function, extract variable, inline function, move symbol, change signature, and rename.
- [ ] Add autofix tests for mechanical lint/import edits.
- [ ] Confirm release logging omits source code, absolute paths, and symbol names by default.
- [ ] Run manual QA in VS Code, Zed, and one generic LSP client.

## Milestone 6: Dynamic Python And Product Surface

- [ ] Define supported fallback behavior for monkey-patching, metaclasses, `__getattr__`, custom import hooks, and reflection.
- [ ] Add tests that assert when dynamic behavior resolves to a concrete type, emits a diagnostic, or falls back to `Any`/unknown.
- [ ] Add multi-root workspace tests for config, diagnostics, symbols, and imports.
- [ ] Add telemetry event tests for timing, cache behavior, crashes, and feature errors.
- [ ] Add formatter config validation and editor formatting QA cases.

## Milestone 7: Performance Gates

- [ ] Define budgets for single-file checks, cold workspace indexing, incremental reanalysis, dependency fan-out, and memory.
- [ ] Add benchmarks for those budgets.
- [ ] Add a synthetic large workspace fixture.
- [ ] Profile `crates/server/src/workspace.rs`, `crates/server/src/cache.rs`, solver hot paths, and stub loading.
- [ ] Decide which caches are process-only and which can be persisted.
