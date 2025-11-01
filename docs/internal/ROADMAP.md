# Beacon Roadmap

Strategic milestones for delivering a Hindley-Milner type system and LSP server for Python.

## LSP Features

### Hover & Diagnostics

- [ ] "Did you mean" suggestions and annotation fixes
- [ ] Code actions for refactoring (insert annotations, adjust Optional)

## Advanced Type Features

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
