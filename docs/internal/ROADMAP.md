# Beacon Roadmap

This document tracks the major milestones required to deliver a robust Hindley-Milner type system for Python together with an incremental LSP server. It mirrors the actionable style of TODO.md so contributors can claim work quickly.

## Parser & Tree-sitter

**Goal:** Provide a lossless parser pipeline that feeds incremental AST updates into constraint generation.
**Tasks:**
    - [ ] Bind `tree-sitter-python` and expose the node kinds required for analysis.
    - [ ] Build CST-to-AST adapters that normalize `with`, comprehensions, walrus operator, pattern matching, and decorators.
    - [ ] Implement scope building for modules, classes, functions, and blocks, including import alias tracking.
    - [ ] Support incremental edits by invalidating AST slices via Tree-sitter edit sessions.
**Implementation Notes:**
    - Preserve trivia for diagnostics, ranges, and code actions.
    - Recognize typing syntax (`list[int]`, `Callable`, `Literal`, etc.) during AST lowering.
**Acceptance:**
    - Parser updates respond to document edits without full reparse.
    - AST records scopes, decorators, and symbol tables for downstream passes.

## Core Type System

**Goal:** Implement a Hindley-Milner core with row-polymorphic records and a robust substitution system.
**Tasks:**
    - [ ] Define type variables, monotypes, polytypes, and higher-kinded constructors.
    - [ ] Build substitution composition and occurs-checking unification.
    - [ ] Support row-polymorphic records and structural constraints.
    - [ ] Model `Any`, top, and bottom types with gradual semantics.
**Implementation Notes:**
    - Generalize only at non-expansive bindings (value restriction).
    - Ensure substitutions apply recursively and idempotently.
**Acceptance:**
    - Types, kinding, substitution, and unifier modules compile with thorough unit tests.
    - Row-polymorphic records and unions round-trip through the core representation.

## Constraint Generation

**Goal:** Generate constraints for Python expressions that feed the solver with Algorithm W semantics.
**Tasks:**
    - [ ] Cover variables, literals, functions, calls, attributes, indexing, comprehensions, and destructuring.
    - [ ] Represent constraints in an intermediate form consumable by the solver.
    - [ ] Integrate generator output with parser AST nodes and symbol tables.
    - [ ] Emit diagnostics hooks for mismatches and quick fixes.
**Implementation Notes:**
    - Track scope information for let-generalization.
    - Use iterator protocols for comprehensions and loops.
**Acceptance:**
    - Constraint generation handles the core Python expression set with tests.
    - Failures produce actionable diagnostics tied to source ranges.

## Records & Protocols

**Goal:** Support structural typing through row-polymorphic records and protocol entailment.
**Tasks:**
    - [ ] Model attribute reads/writes with `HasAttr`-style constraints.
    - [ ] Encode builtin protocols (`Sized`, iterator protocols) and allow manual authoring.
    - [ ] Support object construction via `__init__` and row extension.
**Implementation Notes:**
    - Reuse index metadata for attribute lookup.
    - Provide gradual fallbacks when protocol information is missing.
**Acceptance:**
    - Attribute access infers precise types for locals, globals, and instance members.
    - Protocol mismatches surface diagnostics with suggested fixes.

## Typing Interop

**Goal:** Ingest Python typing annotations while balancing strictness modes.
**Tasks:**
    - [ ] Parse `Optional`, `Union`, `TypeVar`, `Generic`, and `Protocol` annotations.
    - [ ] Respect strict, balanced, and loose modes when annotations disagree with inference.
    - [ ] Allow `.pyi` stub ingestion and module cache population.
**Implementation Notes:**
    - Treat annotations as bounds, not absolute truths, unless running in strict mode.
    - Cache stub results in the index for reuse between analyses.
**Acceptance:**
    - Mode selection changes diagnostic severity without breaking inference.
    - Stub ingestion populates the index and unlocks protocol resolution.

## Collections & Iteration

**Goal:** Deliver first-class support for Python collection protocols.
**Tasks:**
    - [ ] Implement iterator/iterable constraints for `for`, comprehensions, and context managers.
    - [ ] Model subscripting and slicing for lists, tuples, dicts, sets, and custom containers.
    - [ ] Provide inlay hints for inferred collection element types.
**Implementation Notes:**
    - Normalize builtin signatures from stubs to avoid reimplementing shapes manually.
    - Share logic with completion and hover features.
**Acceptance:**
    - Collection-heavy examples infer element types and show them in hints.
    - Indexing and slicing produce correct constraint shapes.

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
**Implementation Notes:**
    - Leverage Tree-sitter byte ranges to limit recomputation.
    - Track module dependency graphs in the index.
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

## Hardening & Performance

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

## Testing & Quality

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

- [ ] Tree-sitter parser with CST-to-AST lowering and incremental updates.
- [ ] Core type system and solver with comprehensive tests.
- [ ] Constraint generator covering core Python constructs.
- [ ] Name resolution and symbol indexing for variables, imports, and decorators.
- [ ] Row-polymorphic records and protocol entailment.
- [ ] LSP server features: diagnostics, hover, inlay hints, code actions.
- [ ] Typing interop with `.pyi` ingestion and mode configuration.
- [ ] Benchmarks, corpus tests, and fuzzing harness.
- [ ] Contributor documentation for modes, constraints, and contribution workflow.
