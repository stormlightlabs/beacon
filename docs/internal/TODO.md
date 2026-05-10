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

## Milestone 1: Refactor Core Type APIs

Keep core dependency-light while making it the stable API that parser, constraints, analyzer, server, and CLI can build on. Core is well tested and clippy-clean, but downstream crates are duplicating type formatting, type decomposition, builtin mapping, and compatibility helpers.

- [ ] Split `types.rs` into type model, constructors, application/decomposition helpers, normalization, subtyping, diagnostics/display formatting, type schemes, and overloads.
- [ ] Split `unify.rs` into solve dispatch, type-variable binding, application variance, union handling, record handling, tuple/forall handling, and unification errors.
- [ ] Move `TypeVarGen` out of `lib.rs` into a focused type-variable module.
- [ ] Replace broad glob re-exports from `lib.rs` with an intentional public surface, keeping compatibility re-exports until downstream crates migrate.
- [ ] Add shared helpers for builtin type names, literal-to-base conversion, `Any`/type-variable detection, app decomposition, class/protocol app decomposition, and diagnostic type formatting.
- [ ] Decide which constraint data can move into `beacon-core` without adding parser, analyzer, or server dependencies.
- [ ] Keep `AnnotationParser` as the shared annotation parser, but split its lexer/parser internals if further typing work expands it.
- [ ] Keep class metadata, protocol metadata, and overload resolution APIs covered by downstream characterization tests.
- [ ] Make `cargo clippy -p beacon-core --all-targets -- -D warnings` stay clean.

## Milestone 2: Refactor The Parser

Keep parsed AST and symbol behavior stable while separating parser responsibilities. The parser test suite is broad and clippy-clean, but `lib.rs` and `resolve.rs` are large enough that syntax, symbol, and span work will be hard to review in place.

- [ ] Move AST models, operators, patterns, parameters, imports, match cases, and helper methods out of `lib.rs` into focused modules.
- [ ] Split tree-sitter CST conversion by syntax family: definitions, statements, expressions, literals, imports, comprehensions, and pattern matching.
- [ ] Extract shared node/span helpers for tree-sitter child lookup, text extraction, body extraction, list delimiters, and position conversion.
- [ ] Move Python literal parsing into a small module with integer, float, string, and prefix tests.
- [ ] Split `resolve.rs` into symbol model, scope table, definition pass, reference pass, annotation reference scanning, f-string reference scanning, and unused/shadowed queries.
- [ ] Keep symbol reference spans stable, especially decorators, annotations, imports, comprehensions, `with` aliases, and pattern bindings.
- [ ] Decide whether docstring and RST parsing stay in `beacon-parser` or move behind a documented parser API boundary.
- [ ] Make `cargo clippy -p beacon-parser --all-targets -- -D warnings` stay clean.

## Milestone 3: Refactor The Analyzer

Keep behavior stable while reducing the analyzer crate's internal coupling. The current tests are useful, but several modules carry too many responsibilities for v1 work to stay predictable.

- [ ] Split `cfg.rs` into graph data structures, CFG building, call graph construction, call resolution, and workspace/module wrappers.
- [ ] Split `walker` by Python construct or analysis phase so type, import, class, function, call, match, and control-flow behavior are easier to test in isolation.
- [ ] Replace the production `todo!()` path in `walker/visitors.rs` with implemented behavior or an explicit diagnostic.
- [ ] Extract shared AST utilities for statement flattening, hoisted definitions, target names, use/def collection, call extraction, and source positions.
- [ ] Split `linter.rs` into a small dispatcher plus rule groups. Keep BEA codes, messages, suppressions, and spans stable.
- [ ] Split `loader.rs` into stub cache, TypeVar extraction, annotation conversion, and class/method registry loading.
- [ ] Remove string-based parsing of class bases in the stub loader where AST data can carry the same information.
- [ ] Reconcile `const_eval::ConstValue` and `data_flow::ConstantValue`, or document why both representations must exist.
- [ ] Narrow public exports from `crates/analyzer/src/lib.rs` so server and CLI code depend on stable analyzer APIs instead of internals.
- [ ] Make `cargo clippy -p beacon-analyzer --all-targets -- -D warnings` pass.

## Milestone 4: Refactor The Constraint Solver

Keep solver behavior stable while moving the main pieces into smaller modules. The crate has strong tests, but `solver.rs` and `lib.rs` carry enough policy that v1 checker work will be hard to review in place.

- [ ] Split `solver.rs` into solve loop, equality/unification handling, call handling, attribute handling, protocol/variance checks, generator compatibility, pattern constraints, and error recovery.
- [ ] Move `Constraint`, `ConstraintSet`, `ConstraintResult`, `Span`, `TypeErrorInfo`, and type guard metadata into focused model modules.
- [ ] Move `ControlFlowContext`, `TypeSetTracker`, and narrowing scope tracking out of `lib.rs`.
- [ ] Decide whether `Constraint::Narrowing` should affect solver output or stay as analyzer-side state, then update tests to assert that contract.
- [ ] Deduplicate pattern/class compatibility helpers across `pattern.rs`, `pattern_validation.rs`, `exhaustiveness.rs`, and `lib.rs`.
- [ ] Keep protocol inheritance, variance, callable compatibility, generator/coroutine compatibility, and attribute lookup behavior covered by characterization tests before moving code.
- [ ] Replace the `ConstraintSet` TODO with a concrete decision: keep crate-local constraint types or move shared forms into `beacon-core`.
- [ ] Make `cargo clippy -p beacon-constraint --all-targets -- -D warnings` pass.

## Milestone 5: Write The V1 Contract As Tests

- [ ] Create one shared multi-file "v1 package" fixture used by parser, analyzer, CLI, and LSP tests.
- [ ] Add expected diagnostics as structured assertions: code, severity, message fragment, span.
- [ ] Add golden JSON output for `beacon typecheck --format json`.
- [ ] Add LSP fixture coverage for open, edit, save, close, rename, and delete.
- [ ] Add config fixtures for `beacon.toml`, `pyproject.toml`, editor settings, and file mode directives.
- [ ] Add tests for unsupported behavior that assert graceful fallback.

## Milestone 6: Unify Diagnostics Across Frontends

- [ ] Confirm CLI `typecheck`, `analyze`, and `lint` use the same diagnostic provider as LSP wherever possible.
- [ ] Remove, implement, or hide placeholder v1-path CLI output:
  - unification trace
  - CFG visualization
  - inferred type display
- [ ] Verify suppressions apply consistently across type, lint, data-flow, import, and formatting diagnostics.
- [ ] Freeze the v1 diagnostic JSON shape.
- [ ] Ensure strict/balanced/relaxed modes change severity predictably.

## Milestone 7: Checker Reliability Pass

- [ ] Audit value restriction/generalization behavior and add regression fixtures.
- [ ] Add focused call-constraint tests for positional, keyword, defaulted, variadic, and bound receiver calls.
- [ ] Add generic class variance inference fixtures.
- [ ] Expand protocol tests for inheritance, generic protocols, bounds, and missing methods.
- [ ] Expand TypeGuard/TypeIs narrowing beyond simple first-argument cases.
- [ ] Add async/generator/coroutine/context-manager fixtures that assert inferred types.
- [ ] Review unification errors for actionable messages and source spans.

## Milestone 8: Stubs And Workspace Reliability

- [ ] Add setup validation for absent or partially initialized `typeshed`.
- [ ] Report embedded stub version/hash/count in a debug or version command.
- [ ] Test precedence between workspace `.py`, workspace `.pyi`, custom `stubPaths`, bundled stdlib, and site-packages.
- [ ] Test package roots, namespace packages, relative imports, star imports, private imports, and re-export chains in one fixture style.
- [ ] Verify dependency invalidation when an imported file, exported symbol, or stub changes.
- [ ] Add workspace-wide reference/rename tests that include imports and re-exports.

## Milestone 9: LSP And Editor Hardening

- [ ] Add version-safe diagnostics publishing tests.
- [ ] Verify hover, completion, goto definition, references, rename, symbols, semantic tokens, and inlay hints against the v1 fixture.
- [ ] Add snippet completion tests using scope, imports, and inferred types.
- [ ] Add auto-import tests using workspace symbols, stubs, and re-exports.
- [ ] Add refactoring edit tests for extract function, extract variable, inline function, move symbol, change signature, and rename.
- [ ] Add autofix tests for mechanical lint/import edits.
- [ ] Confirm release logging omits source code, absolute paths, and symbol names by default.
- [ ] Run manual QA in VS Code, Zed, and one generic LSP client.

## Milestone 10: Dynamic Python And Product Surface

- [ ] Define supported fallback behavior for monkey-patching, metaclasses, `__getattr__`, custom import hooks, and reflection.
- [ ] Add tests that assert when dynamic behavior resolves to a concrete type, emits a diagnostic, or falls back to `Any`/unknown.
- [ ] Add multi-root workspace tests for config, diagnostics, symbols, and imports.
- [ ] Add telemetry event tests for timing, cache behavior, crashes, and feature errors.
- [ ] Add formatter config validation and editor formatting QA cases.

## Milestone 11: Performance Gates

- [ ] Define budgets for single-file checks, cold workspace indexing, incremental reanalysis, dependency fan-out, and memory.
- [ ] Add benchmarks for those budgets.
- [ ] Add a synthetic large workspace fixture.
- [ ] Profile `crates/server/src/workspace.rs`, `crates/server/src/cache.rs`, solver hot paths, and stub loading.
- [ ] Decide which caches are process-only and which can be persisted.
