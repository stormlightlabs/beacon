# Beacon Roadmap

Strategic milestones for delivering a Hindley-Milner type system and LSP server for Python. See TODO.md for implementation details.

## Type System Extensions

## LSP Features

### Hover & Diagnostics

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
    - Now includes inheritance chain checking via `lookup_attribute_with_inheritance`
- **Constructor safety:** `__init__` and `__new__` validate argument types
    - `__new__` checked before `__init__` for metaclass-aware construction
- **Protocol satisfaction:** Protocol constraints detect interface mismatches (HM006)
    - Extended to user-defined protocols via structural checking
- **Graceful degradation:** Type variables and `Any` allow flexible inference
- **Decorator typing:** @property, @staticmethod, @classmethod properly transform method signatures
- **Inheritance support:** Row polymorphism enables structural subtyping with method overrides
- **Protocol composition:** Intersection and union types enable complex protocol requirements
    - `Type::Intersection` with normalization (flatten, deduplicate, sort)
    - Proper subtyping semantics for composed protocols
- **Overload resolution:** Full Python `@overload` semantics with first-match-wins strategy
    - Method name tracking in `BoundMethod` type for runtime resolution
    - Automatic self-parameter handling in overload matching
    - MethodType enum supporting both single and overloaded methods
- **Variance checking:** Contravariant parameters, covariant return types in protocols
    - Full signature compatibility validation via `Type::is_subtype_of()`

## Parking Lot

- [ ] Type checking modes (strict/balanced/loose)
