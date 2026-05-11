# Beacon TODO

## Milestone 0: Define The Contract

- [x] Document submodule setup in public contributor docs:
  - `git submodule update --init --recursive`
  - expected `typeshed/stubs` layout
  - what the embedded stdlib module list means at build time
- [x] Treat ignored doctests as intentional examples unless a future change makes one worth promoting to a runnable test.
- [ ] Decide which commands form the v1 release gate.
- [ ] Decide which fixtures define the v1 supported Python subset.
- [ ] Decide the v1 diagnostic/config compatibility policy.

## Milestone 1: Refactor Core Type APIs

Keep core dependency-light while making it the stable API that parser, constraints, analyzer, server, and CLI can build on. Core is well tested and clippy-clean, but downstream crates are duplicating type formatting, type decomposition, builtin mapping, and compatibility helpers.

- [x] Split `types.rs` into type model, constructors, application/decomposition helpers, normalization, subtyping, diagnostics/display formatting, type schemes, and overloads.
- [x] Split `unify.rs` into solve dispatch, type-variable binding, application variance, union handling, record handling, tuple/forall handling, and unification errors.
- [x] Move `TypeVarGen` out of `lib.rs` into a focused type-variable module.
- [x] Replace broad glob re-exports from `lib.rs` with an intentional public surface, keeping compatibility re-exports until downstream crates migrate.
- [x] Add shared helpers for builtin type names, literal-to-base conversion, `Any`/type-variable detection, app decomposition, class/protocol app decomposition, and diagnostic type formatting.
- [x] Decide which constraint data can move into `beacon-core` without adding parser, analyzer, or server dependencies:
  - keep solver-specific `Constraint`, `ConstraintSet`, `ConstraintResult`, `Span`, `TypeErrorInfo`, narrowing metadata, and control-flow state in `beacon-constraint` for now
  - move only reusable type-model helpers into `beacon-core` until a shared diagnostic model is designed
- [x] Keep `AnnotationParser` as the shared annotation parser, but split its lexer/parser internals if further typing work expands it.
- [x] Keep class metadata, protocol metadata, and overload resolution APIs covered by downstream characterization tests.
- [x] Make `cargo clippy -p beacon-core --all-targets -- -D warnings` stay clean.

## Milestone 2: Refactor The Parser

Keep parsed AST and symbol behavior stable while separating parser responsibilities. The parser test suite is broad and clippy-clean, but `lib.rs` and `resolve.rs` are large enough that syntax, symbol, and span work will be hard to review in place.

- [x] Move AST models, operators, patterns, parameters, imports, match cases, and helper methods out of `lib.rs` into focused modules.
- [x] Split tree-sitter CST conversion by syntax family: definitions, statements, expressions, literals, imports, comprehensions, and pattern matching.
- [x] Extract shared node/span helpers for tree-sitter child lookup, text extraction, body extraction, list delimiters, and position conversion.
- [x] Move Python literal parsing into a small module with integer, float, string, and prefix tests.
- [x] Split `resolve.rs` into symbol model, scope table, definition pass, reference pass, annotation reference scanning, f-string reference scanning, and unused/shadowed queries.
- [x] Keep symbol reference spans stable, especially decorators, annotations, imports, comprehensions, `with` aliases, and pattern bindings.
- [x] Decide whether docstring and RST parsing stay in `beacon-parser` or move behind a documented parser API boundary:
  - decision: keep docstring and RST parsing in `beacon-parser` as the documented parser-facing API for source text metadata
  - preserve the existing public exports from `lib.rs` so analyzer, server, and CLI code do not depend on implementation modules
- [x] Make `cargo clippy -p beacon-parser --all-targets -- -D warnings` stay clean.

## Milestone 3: Refactor The Analyzer

Keep behavior stable while reducing the analyzer crate's internal coupling. The current tests are useful, but several modules carry too many responsibilities for v1 work to stay predictable.

- [x] Split `cfg.rs` into graph data structures, CFG building, call graph construction, call resolution, and workspace/module wrappers.
- [x] Split `walker` by Python construct or analysis phase so type, import, class, function, call, match, and control-flow behavior are easier to test in isolation.
- [x] Replace the production `todo!()` path in `walker/visitors.rs` with implemented behavior or an explicit diagnostic.
- [x] Move the walker-specific AST helpers that were already repeated locally into `walker/ast_utils.rs`.
- [x] Split `linter.rs` into a small dispatcher plus rule groups. Keep BEA codes, messages, suppressions, and spans stable.
- [x] Split `loader.rs` into stub cache, TypeVar extraction, annotation conversion, and class/method registry loading.
- [x] Remove string-based parsing of class bases in the stub loader where AST data can carry the same information.
- [x] Reconcile `const_eval::ConstValue` and `data_flow::ConstantValue`, or document why both representations must exist.
- [x] Narrow public exports from `crates/analyzer/src/lib.rs` so server and CLI code depend on stable analyzer APIs instead of internals.
- [x] Make `cargo clippy -p beacon-analyzer --all-targets -- -D warnings` pass.

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

## Milestone 5: Deduplicate Shared Analysis Mechanics

The parser and analyzer refactors split large files into smaller modules, but several passes still duplicate AST traversal, source-position conversion, name extraction, and type/annotation handling. Consolidate the mechanics that are genuinely shared while preserving pass-specific semantics.

Current duplication audit:

- AST traversal shape is repeated across analyzer data flow, linter traversal, CFG call extraction, taint analysis, server workspace indexing, inlay hints, semantic tokens, and diagnostics. The pass-specific decisions are intentional; the mechanics of walking child statements, child expressions, and nested body blocks are not.
- Source span and byte-offset conversion is repeated across parser resolve, analyzer CFG/data-flow, server analysis, diagnostics, and constraint error mapping. Different callers may need different fallbacks, but the core line/column-to-byte and node-position logic should not be copied.
- Target and binding extraction is split across parser AST helpers, analyzer taint, analyzer loader, server diagnostics, and workspace symbol indexing. Pass-specific filtering is intentional; extracting binding names from the same AST shapes should be shared.
- Annotation and type parsing has overlapping paths in core `AnnotationParser`, analyzer loader, analyzer walker utilities, server workspace parsing, and diagnostics. Stub-specific TypeVar context is legitimate, but generic annotation parsing and conversion should have one shared path.
- Pattern and class compatibility logic overlaps across constraint pattern validation, exhaustiveness, solver logic, and analyzer pattern handling. Constraint-specific error reporting is intentional; shape compatibility and binding extraction should be shared.
- Test code repeatedly hand-builds verbose `AstNode` values. Local test setup is fine, but common node shapes should use builders or fixtures.

- [ ] Create shared AST traversal helpers that expose child statements, child expressions, and body blocks without forcing every analysis pass to duplicate large `match AstNode` walks.
- [ ] Move source location helpers to a single public parser/core-facing API for line/column spans, byte offsets, and node positions; migrate analyzer, constraints, and server callers.
- [ ] Consolidate target/binding extraction into parser AST helpers that cover identifiers, tuple/list destructuring, starred targets, attributes, subscripts, imports, `with` aliases, comprehensions, and pattern bindings.
- [ ] Replace string-based comprehension target handling with AST-backed binding extraction where parser data can carry the original structure.
- [ ] Reuse one annotation/type conversion path for stub loading and analysis-time annotations, with tests for TypeVars, bounds, constraints, unions, generics, callables, and qualified names.
- [ ] Introduce test AST builders or fixture helpers for common node shapes so unit tests do not repeatedly hand-build verbose `AstNode` values.
- [ ] Add duplication regression checks to the v1 gate once the shared helpers land, either through a lightweight local script or a documented manual audit if no Rust-aware tool is adopted.
- [ ] Keep all moved helpers covered by behavior-preserving tests before deleting pass-local copies.

## Milestone 6: Write The V1 Contract As Tests

- [ ] Create one shared multi-file "v1 package" fixture used by parser, analyzer, CLI, and LSP tests.
- [ ] Add expected diagnostics as structured assertions: code, severity, message fragment, span.
- [ ] Add golden JSON output for `beacon typecheck --format json`.
- [ ] Add LSP fixture coverage for open, edit, save, close, rename, and delete.
- [ ] Add config fixtures for `beacon.toml`, `pyproject.toml`, editor settings, and file mode directives.
- [ ] Add tests for unsupported behavior that assert graceful fallback.

## Milestone 7: Unify Diagnostics Across Frontends

- [ ] Confirm CLI `typecheck`, `analyze`, and `lint` use the same diagnostic provider as LSP wherever possible.
- [ ] Remove, implement, or hide placeholder v1-path CLI output:
  - unification trace
  - CFG visualization
  - inferred type display
- [ ] Verify suppressions apply consistently across type, lint, data-flow, import, and formatting diagnostics.
- [ ] Freeze the v1 diagnostic JSON shape.
- [ ] Ensure strict/balanced/relaxed modes change severity predictably.

## Milestone 8: Checker Reliability Pass

- [ ] Audit value restriction/generalization behavior and add regression fixtures.
- [ ] Add focused call-constraint tests for positional, keyword, defaulted, variadic, and bound receiver calls.
- [ ] Add generic class variance inference fixtures.
- [ ] Expand protocol tests for inheritance, generic protocols, bounds, and missing methods.
- [ ] Expand TypeGuard/TypeIs narrowing beyond simple first-argument cases.
- [ ] Add async/generator/coroutine/context-manager fixtures that assert inferred types.
- [ ] Review unification errors for actionable messages and source spans.

## Milestone 9: Stubs And Workspace Reliability

- [ ] Add setup validation for absent or partially initialized `typeshed`.
- [ ] Report embedded stub version/hash/count in a debug or version command.
- [ ] Test precedence between workspace `.py`, workspace `.pyi`, custom `stubPaths`, bundled stdlib, and site-packages.
- [ ] Test package roots, namespace packages, relative imports, star imports, private imports, and re-export chains in one fixture style.
- [ ] Verify dependency invalidation when an imported file, exported symbol, or stub changes.
- [ ] Add workspace-wide reference/rename tests that include imports and re-exports.

## Milestone 10: LSP And Editor Hardening

- [ ] Add version-safe diagnostics publishing tests.
- [ ] Verify hover, completion, goto definition, references, rename, symbols, semantic tokens, and inlay hints against the v1 fixture.
- [ ] Add snippet completion tests using scope, imports, and inferred types.
- [ ] Add auto-import tests using workspace symbols, stubs, and re-exports.
- [ ] Add refactoring edit tests for extract function, extract variable, inline function, move symbol, change signature, and rename.
- [ ] Add autofix tests for mechanical lint/import edits.
- [ ] Confirm release logging omits source code, absolute paths, and symbol names by default.
- [ ] Run manual QA in VS Code, Zed, and one generic LSP client.

## Milestone 11: Dynamic Python And Product Surface

- [ ] Define supported fallback behavior for monkey-patching, metaclasses, `__getattr__`, custom import hooks, and reflection.
- [ ] Add tests that assert when dynamic behavior resolves to a concrete type, emits a diagnostic, or falls back to `Any`/unknown.
- [ ] Add multi-root workspace tests for config, diagnostics, symbols, and imports.
- [ ] Add telemetry event tests for timing, cache behavior, crashes, and feature errors.
- [ ] Add formatter config validation and editor formatting QA cases.

## Milestone 12: Performance Gates

- [ ] Define budgets for single-file checks, cold workspace indexing, incremental reanalysis, dependency fan-out, and memory.
- [ ] Add benchmarks for those budgets.
- [ ] Add a synthetic large workspace fixture.
- [ ] Profile `crates/server/src/workspace.rs`, `crates/server/src/cache.rs`, solver hot paths, and stub loading.
- [ ] Decide which caches are process-only and which can be persisted.
