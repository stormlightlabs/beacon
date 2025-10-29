# Beacon Roadmap

Strategic milestones for delivering a Hindley-Milner type system and LSP server for Python. See TODO.md for implementation details.

## Type System Extensions

**Requires:** Core type system stability

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

**Requires:** Core type system stability

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
- [ ] Inlay hints for collection element types

## Advanced Type Features

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

- [ ] Type checking modes (strict/balanced/loose)

**Future Protocol Enhancements:**

- Additional builtin protocols (Sized, Container, Callable, etc.)
- User-defined Protocol classes from typing.Protocol
- Protocol intersection and union types
- Variance in protocol method signatures
