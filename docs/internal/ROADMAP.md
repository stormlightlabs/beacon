# Beacon Roadmap

Strategic milestones for delivering a Hindley-Milner type system and LSP server for Python. See TODO.md for implementation details.

## Type System Extensions

## LSP Features

### Hover & Diagnostics

- [ ] "Did you mean" suggestions and annotation fixes
- [ ] Code actions for refactoring (insert annotations, adjust Optional)

## Advanced Type Features

### Pattern Matching (PEP 634)

See [TODO](./TODO.md)

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
- [ ] Type checking modes (strict/balanced/loose)
- [ ] Configurable verbosity for inlay hints

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

### Incremental Re-analysis

- [ ] Implement proper node-to-scope mapping (currently includes all nodes)
- [ ] Track scope dependencies for transitive invalidation
- [ ] Selective re-analysis: only re-analyze changed scopes (requires refactoring walker)
- [ ] Add scope boundary detection in position_map filtering

### Core Type System

- [ ] Add more comprehensive stdlib stubs (os, sys, pathlib, etc.)
- [ ] Implement type checking mode awareness (strict/balanced/loose)
- [ ] Auto-generate stubs from Python runtime introspection
