# Beacon LSP Implementation Plan

Implementation details and mod-specific tasks.

## LSP Features

**Files:** `crates/server/src/features/diagnostics.rs`, `crates/server/src/features/code_actions.rs`

### Quick Fixes

- [ ] Move pattern before subsuming pattern (PM002)

### Configuration

- [x] Make interpreter path configurable (via `beacon.python.interpreterPath`)
- [x] Configuration file support (beacon.toml, pyproject.toml)
- [x] LSP configuration via workspace settings
- [x] Config hot-reload support
- [x] Diagnostic severity configuration
- [ ] Inlay hints configuration implementation
- [ ] Integration tests for configuration flow (config loading and hot-reload)
- [ ] Formatting options

### Static Analysis & Linting

**Files:** `crates/server/src/analysis/linter.rs`, `crates/server/src/analysis/rules/mod.rs`, `crates/server/src/features/diagnostics.rs`

See [Linter Rules](#linter-rules) section below for BEA code implementation status.

### Infrastructure

- [ ] Suppression support (`# type: ignore`, `# noqa:`)
- [ ] Rule configuration (per-rule enable/disable, severity)
- [ ] Symbol table integration for unused detection

### Incremental Re-analysis

- [ ] Benchmark incremental performance (target: <100ms)

### Cross-File Diagnostics

**Files:** `crates/server/src/workspace.rs`, `crates/server/src/features/diagnostics.rs`

- [ ] Inconsistent symbol exports (`__all__` mismatches)
- [ ] Conflicting stub definitions

## Linter Rules

Implementation status for BEA diagnostic codes. See `docs/src/lsp/lint_rules.md` for full rule documentation.

### Needs Symbol Table Integration

- [ ] BEA022: UnusedIndirectAssignment (blocked: requires Global/Nonlocal AST nodes)

### Needs Expression Evaluation

- [ ] BEA023: ForwardAnnotationSyntaxError - Needs Python expression parser for annotation validation
- [ ] BEA024: MultiValueRepeatedKeyLiteral - Requires dict key evaluation
- [ ] BEA029: RedundantPass - Requires separate pass

### Other

- [ ] BEA004: YieldOutsideFunction - Track yield/yield-from in visit_node
- [ ] BEA014: TStringMissingPlaceholders - Template string support
- [ ] BEA025: PercentFormatInvalidFormat - Validate % format syntax

### Improve Caching Granularity

Current limitations from `analysis/mod.rs`:

- Scope cache implementation is basic (extracts all nodes, no real filtering)
- No proper node-to-scope mapping
- No dependency tracking between scopes
- Cache invalidation is wholesale per document

Tasks:

- [ ] Implement proper node-to-scope mapping during constraint generation
- [ ] Track inter-scope dependencies (e.g., function calls, imports)
- [ ] Selective invalidation: only reanalyze changed scopes and dependents
- [ ] Benchmark incremental analysis with large files (target: <50ms for single-scope change)
