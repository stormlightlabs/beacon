# Beacon TODO

## Milestone 1: Workspace Fixture And Harness

- [x] Create one shared top-level `fixtures/workspace/` fixture used by parser,
      analyzer, CLI, and LSP tests.
  - include a normal package, subpackage, config files, custom stubs, and
    intentionally broken modules
  - avoid a special "v1 package" concept; this fixture is the representative
    workspace fixture
- [x] Backmerge former sample programs into `fixtures/workspace/`.
  - preserve useful sample scenarios as modules, config cases, or expected
    diagnostic cases in the workspace fixture
  - remove the old sample directory so it does not become a parallel fixture
    surface
- [x] Add shared fixture helpers so crates can load `fixtures/workspace/`
      without copying files.
- [x] Add structured assertion helpers for diagnostics, inferred types, LSP
      ranges, and edit payloads.
- [x] Add parser, analyzer, CLI, and LSP smoke tests proving the shared
      fixture loads consistently.

## Milestone 2: Diagnostics, Config, And CLI Parity

- [x] Confirm CLI `typecheck`, `analyze`, and `lint` use the same diagnostic
      provider as LSP wherever possible.
- [x] Add expected diagnostics as structured assertions: code, severity,
      message fragment, span, source file, and optional tags.
- [x] Add golden JSON output for `beacon typecheck --format json`.
- [x] Freeze the v1 diagnostic JSON shape.
- [x] Verify suppressions apply consistently across type, lint, data-flow,
      import, and formatting diagnostics.
- [x] Add config assertions for `beacon.toml`, `pyproject.toml`, editor
      settings, and file mode directives.
- [x] Ensure strict/balanced/relaxed modes change severity predictably.
- [x] Implement placeholder v1-path CLI output:
  - unification trace
  - CFG visualization
  - inferred type display

## Milestone 3: Typing Breadth

- [x] Implement the coverage in [Typing Surface](../src/lsp/typing_surface.md).
- [x] Audit value restriction/generalization behavior and add regression
      fixtures.
- [x] Add focused call-constraint tests for positional, keyword, defaulted,
      keyword-only, variadic, and bound receiver calls.
- [x] Add overload selection, overlap, and implementation compatibility tests.
- [x] Add generic function and class fixtures for TypeVar bounds,
      constraints, variance, defaults, ParamSpec, and TypeVarTuple.
- [x] Add protocol tests for inheritance, generic protocols, callback
      protocols, bounds, and missing methods.
- [x] Add dataclass, dataclass transform, enum, TypedDict, property,
      decorator, `Final`, `ClassVar`, and override fixtures.
- [x] Add async/generator/coroutine/context-manager fixtures that assert
      inferred types.
- [x] Review unification errors for actionable messages and source spans.

## Milestone 4: Narrowing And Data Flow

- [x] Implement the coverage in [Type Narrowing](../src/lsp/type_narrowing.md).
- [x] Expand TypeGuard/TypeIs narrowing beyond simple first-argument cases.
- [x] Add guard fixtures for `isinstance`, `issubclass`, `type`, `None`,
      truthiness, membership, `hasattr`, `callable`, and assertions.
- [x] Add pattern matching fixtures for literal, singleton, sequence, mapping,
      class, OR, AS, star, guard, and exhaustiveness behavior.
- [x] Add branch, loop, try/finally, and match join tests that assert inferred
      types before guards, inside branches, and after joins.
- [x] Verify use-before-definition, unreachable code, unused symbol, and
      constant-branch diagnostics against the shared fixture.
- [x] Define how `Any` and unknown values interact with narrowing.

## Milestone 5: Imports, Stubs, And Workspace Reliability

- [x] Implement the coverage in
      [Imports and Stubs](../src/lsp/imports_and_stubs.md).
- [x] Add setup validation for absent or partially initialized `typeshed`.
- [x] Report embedded stub version/hash/count in a debug or version command.
- [x] Test precedence between workspace `.py`, workspace `.pyi`, custom
      `stubPaths`, bundled stdlib, and site-packages.
- [x] Test package roots, namespace packages, relative imports, star imports,
      private imports, and re-export chains in one fixture style.
- [x] Verify dependency invalidation when an imported file, exported symbol,
      `__all__`, config, or stub changes.
- [x] Add workspace-wide reference/rename tests that include imports and
      re-exports.
- [x] Add multi-root workspace tests for config, diagnostics, symbols, and
      imports after single-root behavior is stable.

## Milestone 6: Dynamic Python

- [x] Implement the coverage in
      [Dynamic Python Diagnostics](../src/lsp/diagnostics/dynamic.md).
- [x] Define supported fallback behavior for monkey-patching, metaclasses,
      `__getattr__`, dynamic imports, custom import hooks, and reflection.
- [x] Add tests that assert when dynamic behavior resolves to a concrete type,
      emits a diagnostic, or falls back to `Any`/unknown.
- [x] Verify strict/balanced/relaxed mode behavior for unsafe dynamic
      boundaries.
- [x] Confirm fallback types do not erase unrelated precise types.

## Milestone 7: LSP And Editor Product Surface

- [x] Add LSP fixture coverage for open, edit, save, close, rename, and
      delete.
- [x] Add version-safe diagnostics publishing and clearing tests.
- [x] Verify hover, completion, goto definition, references, rename, symbols,
      semantic tokens, and inlay hints against the v1 fixture.
- [x] Add snippet completion tests using scope, imports, and inferred types.
- [x] Add auto-import tests using workspace symbols, stubs, and re-exports.
- [x] Add refactoring edit tests for extract function, extract variable,
      inline function, move symbol, change signature, and rename.
- [x] Add autofix tests for mechanical lint/import edits.
- [x] Add telemetry event tests for timing, cache behavior, crashes, and
      feature errors.
- [x] Confirm release logging omits source code, absolute paths, and symbol
      names by default.

### QA

- [x] Add formatter config validation and editor formatting QA cases.
- [ ] Run manual QA in VS Code, Zed, and one generic LSP client.

## Milestone 8: Performance Gates

- [ ] Define budgets for single-file checks, cold workspace indexing,
      incremental reanalysis, dependency fan-out, and memory.
- [ ] Add benchmarks for those budgets.
- [ ] Add a synthetic large workspace fixture.
- [ ] Profile `crates/server/src/workspace.rs`,
      `crates/server/src/cache.rs`, solver hot paths, and stub loading.
- [ ] Decide which caches are process-only and which can be persisted.

## Parking Lot

- [ ] Fix `with ... as` target typing.
  - Today `visit_with` binds the target to the `__enter__`/`__aenter__`
    method type instead of the result of calling it.
  - Emit a `Call` constraint for the enter method and bind the optional target
    to that return type. Add CLI/analyzer regressions for sync and async
    context managers.
- [ ] Stop nested definitions from leaking into outer scopes.
  - `TypeEnvironment::populate_from_ast` descends into function and class
    bodies using the same environment, so nested names can be visible at module
    scope.
  - Pre-populate only declarations that belong to the current scope, or make
    pre-population scope-aware.
    Add a regression test where a nested function call at module scope is undefined.
- [ ] Correct binary operator typing.
  - `visit_ops` treats most binary operators as same-type operands returning the
    left operand type, so `1 / 2` is inferred as `int`.
  - Split operator lowering by operator family. Start with `/`, `+`, and
    comparisons, then decide how far to model dunder methods.
- [ ] Validate required positional-only parameters with `**kwargs`.
  - `merge_and_validate_param_metadata` can miss a required positional-only
    argument when a keyword with the same name is accepted into `**kwargs`.
  - Check required positional-only parameters separately from keywordable
    parameters and add call-shape tests for `/` plus `**kwargs`.
- [ ] Preserve useful attribute constraints on fresh type variables.
  - `solve_attribute_constraint` currently does nothing for `Type::Var(_)`.
  - Bind the variable to a record/row-like type or defer `HasAttr` until the
    variable resolves. Keep the current low-noise behavior for unknown values.
- [ ] Decide how unsupported annotations should surface.
  - `parse_annotation_or_any` turns parse failures into `Any`, which can hide
    unsupported syntax such as qualified annotations if no earlier layer strips
    the qualifier.
  - Report an annotation diagnostic so users can distinguish gradual `Any`
    from parser loss.
- [ ] Model operator dunder methods.
  - Arithmetic and comparison do not lower to `__add__`, `__radd__`,
    `__truediv__`, `__lt__`, and related methods.
  - Add a conservative dunder-based path for user-defined numeric classes,
    string/list concatenation, numeric widening, and comparison overloads.
