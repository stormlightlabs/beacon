# Beacon Roadmap

This document tracks the major milestones required to deliver a robust Hindley-Milner type system for Python together with an incremental LSP server.

## Parser & Tree-sitter

**Goal:** Provide a lossless parser pipeline that feeds incremental AST updates into constraint generation.
**Tasks:**

- [x] Bind `tree-sitter-python` and expose the node kinds required for analysis.
- [x] Implement scope building for modules, classes, functions, and blocks, including import alias tracking.
- [x] Capture type annotations (parameters, return types, variable annotations) in AST.
- [x] Build CST-to-AST adapters for `with` statements, comprehensions, walrus operator, pattern matching, and decorators.
- [ ] Support incremental edits by invalidating AST slices via Tree-sitter edit sessions (deferred).
**Implementation Notes:**
- Preserve trivia for diagnostics, ranges, and code actions.
- Recognize typing syntax (`list[int]`, `Callable`, `Literal`, etc.) during AST lowering.
- Type annotations are captured and exposed via semantic tokens for syntax highlighting.
**Acceptance:**
- [x] Parser captures type annotations from function parameters, return types, and variable declarations.
- [x] AST records scopes and symbol tables for downstream passes.
- [x] Semantic tokens provide accurate highlighting for type annotations.
- [x] Parser handles with, comprehensions, walrus operator, pattern matching, and decorators.
- [ ] Parser updates respond to document edits without full reparse (deferred).

## Records & Protocols

**Goal:** Support structural typing through row-polymorphic records and protocol entailment.
**Tasks:**

- [x] Model attribute reads/writes with `HasAttr`-style constraints.
- [x] Encode builtin protocols (`Sized`, iterator protocols) and allow manual authoring.
- [ ] Support object construction via `__init__` and row extension.
**Implementation Notes:**
- Reuse index metadata for attribute lookup.
- Provide gradual fallbacks when protocol information is missing.
**Acceptance:**
- [x] Protocol system implemented with Iterator, Iterable, Sized, Sequence, Mapping, ContextManager, Callable.
- [x] Protocol mismatches surface diagnostics with error codes.
- [ ] Attribute access infers precise types for locals, globals, and instance members (partial - HasAttr constraint exists but not fully utilized).

## Typing Interop

**Goal:** Ingest Python typing annotations while balancing strictness modes.
**Tasks:**

- [x] Parse `Optional`, `Union`, `TypeVar`, `Generic`, and `Protocol` annotations.
- [x] Implement PEP 561 stub discovery (manual paths, stub packages, inline stubs).
- [x] Parse `.pyi` stub files and extract type signatures.
- [ ] Integrate stub lookups into module resolution and constraint generation.
- [ ] Respect strict, balanced, and loose modes when annotations disagree with inference.
**Implementation Notes:**
- Treat annotations as bounds, not absolute truths, unless running in strict mode.
- Stub discovery follows PEP 561 resolution order (manual → stub packages → inline → typeshed).
- StubFile structure stores exports, re-exports, **all** declarations, and partial markers.
- StubCache uses simple HashMap caching; LRU eviction deferred for future optimization.
**Acceptance:**
- [x] AnnotationParser handles TypeVar, Generic, Protocol with tests passing.
- [x] Workspace discovers and indexes stub files following PEP 561.
- [x] parse_stub_file() extracts function, class, and variable type signatures.
- [x] Workspace provides get_stub_type() and get_stub_exports() for on-demand stub queries.
- [ ] Module resolution queries stubs before falling back to inference.
- [ ] Mode selection changes diagnostic severity without breaking inference.

## Collections & Iteration

**Goal:** Deliver first-class support for Python collection protocols.
**Tasks:**

- [x] Implement iterator/iterable constraints for `for` loops and comprehensions (list, set, dict, generator).
- [ ] Implement context manager protocol constraints for `with` statements.
- [ ] Model subscripting and slicing for lists, tuples, dicts, sets, and custom containers.
- [ ] Provide inlay hints for inferred collection element types.
**Implementation Notes:**
- Normalize builtin signatures from stubs to avoid reimplementing shapes manually.
- Share logic with completion and hover features.
**Acceptance:**
- [x] For loops and comprehensions correctly infer element types from iterables (e.g., `for x in [1,2,3]` infers `x: int`).
- [x] Protocol constraint system extracts element types from `list[T]`, `set[T]`, `dict[K,V]`, `tuple[T]`.
- [ ] Collection-heavy examples show inferred types in inlay hints.
- [ ] Indexing and slicing produce correct constraint shapes.

## Classes & Overloads

**Goal:** Model class semantics, overload resolution, and decorator effects.
**Tasks:**

- [ ] Resolve overloads by choosing the most specific callable signature.
- [ ] Handle `@property`, `@staticmethod`, `@classmethod`, and dataclass helpers.
- [ ] Support metaclass-aware class construction and module type exports.
**Implementation Notes:**
- Use decorator stubs to rewrite function types before registration.
- Ensure overload fallbacks degrade to inferred principal types safely.
**Acceptance:**
- Overloaded functions pick consistent signatures for each call site.
- Decorator application updates hover and diagnostics output.

## Async & Generators

**Goal:** Handle async/await and generator semantics end-to-end.
**Tasks:**

- [ ] Infer coroutine return types and `await` result types.
- [ ] Model generators, `yield`, and `yield from` with proper element and send types.
- [ ] Wire async constructs into diagnostics and inlay hints.
**Implementation Notes:**
- Reuse iterable protocol machinery where possible.
- Consider gradual fallback when async functions return `Any`.
**Acceptance:**
- Async and generator code infers types consistent with Python typing rules.
- IDE features display results without panics or missing data.

### Proper Generator Type Modeling

Generator expressions and functions are approximated as iterables/lists using existing type infrastructure.
This works for basic iteration but loses important generator-specific semantics.

#### Requirements for Generator[YieldType, SendType, ReturnType]

1. Type System Extensions
   - Add `Generator` type constructor to `beacon_core::TypeCtor` with arity 3
   - Add `AsyncGenerator` type constructor for async generators (arity 2)
   - Add `Coroutine` type constructor for async/await (arity 3)

2. Constraint Generation Changes
   - Detect `yield` statements in function bodies to infer generator functions
   - Track yield expression types separately from return types
   - Model `send()` method parameter types for bidirectional communication
   - Handle `yield from` delegation with proper type propagation

3. Protocol Integration
   - Implement `Iterator[T]` protocol (provides `__iter__` and `__next__`)
   - Implement `Generator[Y, S, R]` protocol (extends Iterator with `send`, `throw`, `close`)
   - Handle covariance/contravariance: yield type is covariant, send type is contravariant

4. Semantic Analysis
   - Distinguish between generator functions (contain `yield`) and regular functions
   - Infer return type as `Generator[Y, None, R]` for simple generators
   - Track `StopIteration` value propagation for return statements
   - Handle `yield from` as generator delegation

5. Type Checking
   - Verify `send()` arguments match declared send type
   - Check that yielded values match yield type
   - Ensure return values match return type (propagated via StopIteration)

#### Tasks

- [ ] Continue using iterable approximations (current state)
- [ ] Add Generator type constructor and basic yield detection
- [ ] Full bidirectional typing with send/throw support
- [ ] AsyncGenerator and Coroutine types for async/await

#### Testing Strategy

- Simple generators: `def count() -> Generator[int, None, None]: yield 1`
- Bidirectional: `def echo() -> Generator[int, int, None]: sent = yield value`
- Generator delegation: `yield from other_gen()`
- Async variants: `async def fetch() -> AsyncGenerator[Data, None]`

## Pattern Matching

**Goal:** Support PEP 634 structural pattern matching with useful diagnostics.
**Tasks:**

- [ ] Generate constraints for mapping, sequence, class, and OR patterns.
- [ ] Track exhaustiveness and unreachable pattern diagnostics.
- [ ] Provide quick fixes for missing cases.
**Implementation Notes:**
- Reuse record and union infrastructure for pattern coverage.
- Keep diagnostics resilient when inference degrades to `Any`.
**Acceptance:**
- Pattern matching examples infer types and flag missing branches.
- Exhaustiveness warnings include actionable suggestions.

## Incrementality & Caching

**Goal:** Deliver fast feedback through incremental constraint slicing and caching.
**Tasks:**

- [ ] Slice constraints by strongly connected components.
- [ ] Cache analysis results on disk and reuse across sessions.
- [ ] Parallelize independent slices for large projects.
- [ ] Implement LRU eviction for StubCache (deferred - currently uses simple HashMap).
**Implementation Notes:**
- Use Tree-sitter byte ranges to limit recomputation.
- Track module dependency graphs in the index.
- StubCache currently uses on-demand loading without eviction; suitable for most workspaces.
**Acceptance:**
- Editing a single file recomputes only affected slices.
- Warm runs complete significantly faster than cold runs.

## Developer Ergonomics

**Goal:** Provide meaningful diagnostics, code actions, and configuration switches.
**Tasks:**

- [ ] Expand diagnostics with "did you mean" suggestions and annotation fixes.
- [ ] Implement code actions for inserting annotations and adjusting Optional usage.
- [ ] Surface configuration options (`mode`, `pythonVersion`, `stubPaths`, `decoratorStubs`) in the CLI and LSP.
**Implementation Notes:**
- Share diagnostic formatting between CLI and LSP outputs.
- Keep config parsing centralized to avoid diverging defaults.
**Acceptance:**
- Diagnostics include actionable fixes in both CLI and editor.
- Changing config toggles behavior without restarting the server.

## Performance

**Goal:** Ensure the server scales to large repositories with predictable resource usage.
**Tasks:**

- [ ] Build benchmarks for cold vs warm runs and large module graphs.
- [ ] Profile memory and CPU hotspots; introduce caching or arenas as needed.
- [ ] Stress test with fuzzing and random AST generation.
**Implementation Notes:**
- Integrate benchmarks into CI gating where feasible.
- Track regressions with historical benchmark baselines.
**Acceptance:**
- Benchmark suite runs automatically and flags regressions.
- Memory usage remains stable under large project loads.

## Testing

**Goal:** Maintain confidence with layered automated testing.
**Tasks:**

- [ ] Add golden tests for inference outputs and diagnostics.
- [ ] Write property tests for unifier idempotence and substitution application.
- [ ] Build a corpus harness using real-world snippets compared against MyPy or Pyright.
- [ ] Integrate targeted fuzzing for parser and solver components.
**Implementation Notes:**
- Reuse `crates/test_support` fixtures for both CLI and server tests.
- Snapshot outputs should stay stable across minor refactors.
**Acceptance:**
- Test suites cover happy paths and edge cases called out in the roadmap.
- CI runs property tests and fuzzers regularly without flakiness.

## Risks & Mitigations

**Goal:** Track high-impact risks and mitigation strategies.
**Tasks:**

- [ ] Identify dynamic Python features that could erode soundness; gate them behind modes.
- [ ] Catalog decorator behaviors and allow user-supplied stubs.
- [ ] Monitor performance cliffs with telemetry from benchmarks.
**Implementation Notes:**
- Emit diagnostics when inference widens to `Any`.
- Encourage community contributions for missing protocol or decorator coverage.

## Deliverables

- [x] Tree-sitter parser with CST-to-AST lowering (incremental updates deferred).
- [x] Core type system and solver with comprehensive tests.
- [x] Constraint generator covering core Python constructs.
- [x] Name resolution and symbol indexing for variables, imports, and decorators.
- [x] Row-polymorphic records and protocol entailment (protocols: Iterator, Iterable, Sized, Sequence, Mapping, ContextManager, Callable).
- [ ] LSP server features: diagnostics (partial), hover (partial), inlay hints (pending), code actions (pending).
- [x] Typing interop: TypeVar/Generic/Protocol parsing (partial - stub integration pending).
- [x] PEP 561 stub discovery and parsing (partial - constraint generation integration pending).
- [ ] Benchmarks, corpus tests, and fuzzing harness.
- [ ] Contributor documentation for modes, constraints, and contribution workflow.
