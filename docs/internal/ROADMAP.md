# Beacon Roadmap

Strategic milestones for delivering a Hindley-Milner type system and LSP server for Python.

## LSP Features

- [x] Signature help (`textDocument/signatureHelp`) - function calls provide parameter lists, overloads, and hints
- [ ] Code actions / quick-fixes (`textDocument/codeAction`) - given a range or diagnostic,
      the server returns actionable fixes (insert import, refactor, etc.)
    - [ ] Auto-import / import management (`textDocument/codeAction`)
- [ ] Formatting (`textDocument/formatting`, `textDocument/rangeFormatting`):
      Format the entire document or a selected range according to style rules (black rules).
- [ ] Workspace/folder support (`workspace/didChangeWorkspaceFolders`):
      Allow the server to handle multiple folders/projects in a single workspace.

### Hover & Diagnostics

- [ ] Diagnostics (errors/warnings) (`textDocument/publishDiagnostics`) (notification):
      The server sends diagnostics (syntax errors, type errors, warnings) as they are found.
- [ ] "Did you mean" suggestions and annotation fixes
- [x] Code actions for refactoring (insert annotations, adjust Optional)

## Infrastructure

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

- [x] Configuration options (`mode`, `pythonVersion`, `stubPaths`, `decoratorStubs`)
- [x] Config hot-reload without restart
- [x] Type checking modes (strict/balanced/loose)
- [ ] Formatting options
- [x] Contributor documentation
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
- [ ] Pull stubs from typeshed repository
