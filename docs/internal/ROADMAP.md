# Beacon Roadmap

Strategic milestones for delivering a Hindley-Milner type system and LSP server for Python.

## LSP Features

### Hover & Diagnostics

- [ ] "Did you mean" suggestions and annotation fixes
- [ ] Code actions for refactoring (insert annotations, adjust Optional)

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

### LSP

- [x] Folding / code-regions (`textDocument/foldingRange`): Provide collapse/expand ranges for the editor to display code folding
- [ ] Go to definition (`textDocument/definition`): The client asks for the location(s) where the symbol at a cursor position is defined; the server responds with URI/position(s).
- [ ] Find references (`textDocument/references`): Find all occurrences of the symbol (in code/workspace) and return a list of reference locations.

---

- [ ] Signature help (`textDocument/signatureHelp`): While typing a function call, the server provides parameter lists, overloads, and hints
- [ ] Inlay hints / parameter names (`textDocument/inlayHint`): Provide inline hints (e.g., parameter names, inferred types) at specific positions

---

- Code actions / quick-fixes (`textDocument/codeAction`): Given a range or diagnostic, the server returns actionable fixes (insert import, refactor, etc.)
    - Auto-import / import management (`textDocument/codeAction`): The server suggests or inserts missing imports or optimizes them via code actions. (Not always a named method in the base spec.)
- Rename symbol (`textDocument/rename`): Request to rename a symbol; server returns workspace edits to update symbol and its usages
