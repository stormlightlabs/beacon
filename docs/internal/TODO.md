# Beacon LSP Implementation Plan

Implementation details and mod-specific tasks.

## Core Type System Stability

### Future Work

- [ ] Add more comprehensive stdlib stubs (os, sys, pathlib, etc.)
- [ ] Implement type checking mode awareness (strict/balanced/loose)
- [ ] Auto-generate stubs from Python runtime introspection
- [ ] Protocol intersection/union types (Protocol1 & Protocol2)
- [ ] Overload resolution (@overload decorator)
- [ ] Method signature compatibility in protocol checking

---

- [ ] Integration test: `with open('file') as f:` infers file type (requires _IO stub integration)
- [ ] Integration test: `lst[0]` where `lst: list[int]` infers `int` (requires full test harness)
- [ ] Integration test: Class inheritance with method override
- [ ] Integration test: User-defined protocol satisfaction

## LSP Features

**Dependencies:** Part 1 complete (class bug fix, stub integration)

### Completions

**Files:** `crates/server/src/features/completion.rs`

**Part 1:**

- [ ] Parse backwards from cursor to understand context
- [ ] Attribute completions after `.` using type inference
- [ ] Import completions from workspace symbols
- [ ] Test with user-defined and builtin types

**Part 2:**

- [ ] Prefix matching against typed text
- [ ] Relevance ranking (scope proximity, usage frequency)
- [ ] Cross-file symbol completions
- [ ] Fuzzy matching support

### Hover Improvements

**Files:** `crates/server/src/features/hover.rs`, `crates/server/src/introspection.rs` (new)

- [ ] Display inferred types with docstrings
- [ ] Show signature help
- [ ] Module/package documentation from introspection
- [ ] Make interpreter path configurable

### Inlay Hints

**Files:** `crates/server/src/features/inlay_hints.rs` (new)

- [ ] Variable type hints for local variables
- [ ] Parameter hints in function calls
- [ ] Return type hints for functions
- [ ] Configuration options (on/off, verbosity)

## Infrastructure (Independent - Can Proceed in Parallel)

### Static Analysis & Linting

**Dependencies:** None

**Files:** `crates/server/src/analysis/linter.rs`, `crates/server/src/analysis/rules/mod.rs`, `crates/server/src/features/diagnostics.rs`

See [Linter Rules](#linter-rules) section below for BEA code implementation status.

**Infrastructure:**

- [ ] Suppression support (`# type: ignore`, `# noqa:`)
- [ ] Rule configuration (per-rule enable/disable, severity)
- [ ] Symbol table integration for unused detection

### Incremental Re-analysis

**Files:** `crates/server/src/analysis/cache.rs` (new), `crates/server/src/analysis/mod.rs`

- [ ] Cache CFGs, symbol tables, diagnostics per module
- [ ] Recompute only changed scopes
- [ ] Maintain last-known diagnostic set
- [ ] Benchmark incremental performance (target: <100ms)

### Cross-File Diagnostics

**Files:** `crates/server/src/workspace/graph.rs`, `crates/server/src/features/diagnostics.rs`

- [ ] Circular imports detection
- [ ] Missing modules detection
- [ ] Inconsistent symbol exports (`__all__` mismatches)
- [ ] Conflicting stub definitions

## Linter Rules

Implementation status for BEA diagnostic codes. See `docs/src/lsp/lint_rules.md` for full rule documentation.

### Needs Parser Support

- [ ] BEA012: AssertTuple - Requires Assert AST node
- [ ] BEA023: ForwardAnnotationSyntaxError - Needs Python expression parser for annotation validation
- [ ] BEA009: TwoStarredExpressions - Needs starred expression support in assignment targets
- [ ] BEA010: TooManyExpressionsInStarredAssignment - Validate unpacking arity

### Needs Symbol Table Integration

- [ ] BEA015: UnusedImport
- [ ] BEA017: UnusedAnnotation
- [ ] BEA018: RedefinedWhileUnused
- [ ] BEA022: UnusedIndirectAssignment

### Needs Expression Evaluation

- [ ] BEA024: MultiValueRepeatedKeyLiteral - Requires dict key evaluation (linter.rs:340)
- [ ] BEA029: RedundantPass - Requires separate pass (linter.rs:266)

### Other

- [ ] BEA004: YieldOutsideFunction - Track yield/yield-from in visit_node
- [ ] BEA014: TStringMissingPlaceholders - Template string support
- [ ] BEA025: PercentFormatInvalidFormat - Validate % format syntax
