# Beacon Roadmap

Strategic milestones for delivering a Hindley-Milner type system and LSP server for Python. See TODO.md for implementation details.

## Foundation (Complete)

- [x] Tree-sitter parser with CST-to-AST lowering
- [x] Core type system and solver
- [x] Constraint generator for Python constructs
- [x] Name resolution and symbol indexing
- [x] Row-polymorphic records and protocol entailment
- [x] Class metadata tracking and attribute resolution
- [x] Object construction via `__init__`
- [x] TypeVar/Generic/Protocol parsing
- [x] PEP 561 stub discovery and parsing

## Core Type System Stability (Critical - Blocks LSP Features)

**Why:** Fix blocking bugs and integrate existing infrastructure before building LSP features.

### Class Construction Bug Fix

**Status:** Fixed in commit [current]

- [x] Fix class construction with multiple methods (UnificationError on classes with `__init__` + other methods)
    - **Solution:** Skip re-processing FunctionDef nodes in ClassDef body visitor to prevent double type variable generation
    - **Test:** `test_class_construction_multiple_methods` now passing
- [ ] Infer bound method types for `f = obj.method` patterns
- [ ] Enable builtin type attribute access (str, list, dict methods)

**Known Limitations:** No `__getattr__`/`__getattribute__` support, no inheritance modeling

### Stub System Integration

**Status:** Complete - Fully operational with concurrent access

- [x] Wire stub lookups into constraint generation
    - **Implementation:** ImportFrom handler queries stub cache directly using read locks
    - **Architecture:** Extracted `StubCache` into separate `Arc<RwLock<StubCache>>` shared between workspace and analyzer
- [x] Query stubs before falling back to inference
    - **Implementation:** Read locks on stub cache for concurrent, non-blocking access
    - **Fallback:** Uses type variables when stub unavailable or lock fails
- [x] Pre-load builtin stubs during workspace initialization
    - **Implementation:** `builtins.pyi` loaded on workspace init with core types (str, int, float, bool, list, dict, tuple)
- [ ] Respect type checking modes (strict/balanced/loose)

**Note:** Stub lookup uses read locks for concurrent access during constraint generation, eliminating previous deadlock issues

### Collection Protocol Completion

**Status:** Complete - Core protocols implemented

- [x] Context manager protocol (`__enter__`, `__exit__`)
    - **Implementation:** With statements generate HasAttr constraints for both methods
    - **Feature:** Target variable in `as` clause receives type from `__enter__` return
- [x] Subscripting and slicing (`__getitem__`)
    - **Implementation:** Subscript expressions generate HasAttr constraint for `__getitem__`
    - **Feature:** Result type inferred from `__getitem__` signature
- [x] Builtin stub support for core collections
    - **Implementation:** list, dict, tuple, set types with proper `__getitem__` signatures
- [ ] Inlay hints for collection element types (deferred to LSP features phase)

## Type System Extensions

**Requires:** Core type system stability complete

### Advanced Protocols & Records

- [ ] User-defined protocol checking (custom iterators, protocols)
- [ ] Row extension for class inheritance
- [ ] Structural record types for duck typing
- [ ] Protocol intersection and union types

### Overloads & Decorators

- [ ] Overload resolution for multiple signatures
- [ ] `@property`, `@staticmethod`, `@classmethod` support
- [ ] Class-level attributes and class methods
- [ ] Metaclass-aware construction

## LSP Features

**Requires:** Core type system stability (class bug fix, stub integration)

### Completions

- [ ] Attribute completions after `.` using type inference
- [ ] Import completions from workspace
- [ ] Filtering, ranking, and fuzzy matching
- [ ] Cross-file symbol completions

### Hover & Diagnostics

- [ ] Display inferred types with docstrings and signatures
- [ ] "Did you mean" suggestions and annotation fixes
- [ ] Code actions for refactoring (insert annotations, adjust Optional)

### Inlay Hints

- [ ] Variable type hints
- [ ] Parameter hints in function calls
- [ ] Return type hints
- [ ] Configurable verbosity

## Advanced Type Features (Optional)

### Pattern Matching (PEP 634)

- [ ] Pattern constraints for mapping, sequence, class, and OR patterns
- [ ] Exhaustiveness checking and unreachable pattern detection
- [ ] Quick fixes for missing cases

### Async & Generators

**Note:** Currently using iterable approximations (sufficient for basic iteration)

- [ ] `Generator[Y, S, R]`, `AsyncGenerator[Y, S]`, `Coroutine[Y, S, R]` type constructors
- [ ] Yield detection to distinguish generator functions
- [ ] Coroutine return types and `await` result types
- [ ] Bidirectional generator `send()` support
- [ ] `yield from` delegation

## Infrastructure (Can Proceed Independently)

### Static Analysis & Linting

- [ ] Linter rule engine (see TODO.md for BEA rule codes)
- [ ] Symbol reference tracking for unused detection
- [ ] Suppression support (`# type: ignore`, `# noqa:`)
- [ ] Per-rule configuration and severity

### Incrementality & Caching

- [ ] Constraint slicing by strongly connected components
- [ ] Disk cache for analysis results
- [ ] Parallelization for large projects
- [ ] StubCache LRU eviction (currently simple HashMap)

### Configuration & Ergonomics

- [ ] Configuration options (`mode`, `pythonVersion`, `stubPaths`, `decoratorStubs`)
- [ ] Config hot-reload without restart
- [ ] Contributor documentation

### Performance & Testing

- [ ] Benchmarks for cold/warm runs and large projects
- [ ] Memory and CPU profiling
- [ ] Golden tests for inference outputs
- [ ] Property tests for unifier correctness
- [ ] Corpus harness (compare against MyPy/Pyright)
- [ ] Fuzzing for parser and solver

## Mitigated Risks

- **Attribute access soundness:** HasAttr constraints prevent invalid access (HM007)
- **Constructor safety:** `__init__` validates argument types
- **Protocol satisfaction:** Protocol constraints detect interface mismatches (HM006)
- **Graceful degradation:** Type variables and `Any` allow flexible inference

## Parking Lot

**Future Protocol Enhancements:**

- Additional builtin protocols (Sized, Container, Callable, etc.)
- User-defined Protocol classes from typing.Protocol
- Protocol intersection and union types
- Variance in protocol method signatures
