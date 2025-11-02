# Beacon LSP Implementation Plan

Implementation details and mod-specific tasks.

## Refactor Constraint Generation

**Goal**: Fix None type unification and reduce false positives in type checking

**Files**: `crates/server/src/analysis/mod.rs`, `crates/constraints/src/lib.rs`, `crates/server/src/analysis/walker.rs`

### Tasks

- [x] Add context tracking (void vs value context) to constraint generator
- [x] Analyze all function return paths during constraint generation
- [x] Detect mixed return patterns (some paths return value, some return None)
- [x] Integrate return path analysis into FunctionDef handler
- [x] Infer `Optional[T]` return type for functions with mixed return paths
- [x] Infer `None` return type for functions with only implicit returns
- [ ] Skip Equal constraints for expression statements in void contexts
- [ ] Handle `if __name__ == "__main__"` idiom specially (always void context)

## Integration Test Cases

- [ ] `with open('file') as f:` infers file type (requires _IO stub integration)
- [ ] `lst[0]` where `lst: list[int]` infers `int` (requires full test harness)
- [ ] Class inheritance with method override

### Pattern Matching

- [ ] Sequence patterns `[x, y, *rest]`
- [ ] Mapping patterns `{"key": value}`
- [ ] Class patterns `Point(x, y)`
- [ ] OR patterns with consistent bindings `case 1 | 2 | 3:`
- [ ] AS patterns `case [x, *rest] as full:`
- [ ] Guard expressions `case x if x > 0:`
- [ ] Nested patterns

## Pattern Matching

### LSP Features

**Files:** `crates/server/src/features/diagnostics.rs`, `crates/server/src/features/code_actions.rs`

#### Diagnostics

- [ ] HM010: Pattern type mismatch (e.g., matching int pattern against str)
- [ ] HM013: Invalid pattern structure (wrong number of elements, etc.)

#### Quick Fixes

- [ ] Add missing case for non-exhaustive matches (PM001)
- [ ] Remove unreachable pattern (PM002)
- [ ] Move pattern before subsuming pattern (PM002)

## Infrastructure

### Configuration

- [ ] Make interpreter path configurable
- [ ] on/off, verbosity for inlay hints

### Static Analysis & Linting

**Files:** `crates/server/src/analysis/linter.rs`, `crates/server/src/analysis/rules/mod.rs`, `crates/server/src/features/diagnostics.rs`

See [Linter Rules](#linter-rules) section below for BEA code implementation status.

**Infrastructure:**

- [ ] Suppression support (`# type: ignore`, `# noqa:`)
- [ ] Rule configuration (per-rule enable/disable, severity)
- [ ] Symbol table integration for unused detection

### Incremental Re-analysis

- [ ] Benchmark incremental performance (target: <100ms)

### Cross-File Diagnostics

**Files:** `crates/server/src/workspace.rs`, `crates/server/src/features/diagnostics.rs`

- [ ] Inconsistent symbol exports (`__all__` mismatches)

---

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
