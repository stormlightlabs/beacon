# Beacon LSP Implementation Plan

Implementation details and mod-specific tasks.

## Async & Generators

### Type System Foundation & AST Extensions (COMPLETED)

**Files:** `crates/core/src/types.rs`, `crates/core/src/annotation_parser.rs`, `crates/parser/src/lib.rs`, `stubs/builtins.pyi`

### Yield Detection & Basic Generator Inference (COMPLETED)

**Files:** `crates/server/src/analysis/walker.rs`

### Bidirectional Send & Advanced Features (COMPLETED)

**Files:** `crates/server/src/analysis/walker.rs`, `crates/server/src/analysis/type_env.rs`, `crates/core/src/types.rs`

### Async Protocols & Full Support

**Files:** `crates/core/src/protocols.rs`, `crates/parser/src/lib.rs`

- [ ] Extend `ProtocolChecker` to handle async iteration protocols
- [ ] Add constraint generation for `async for` and `async with` statements
- [ ] Add comprehensive integration tests for async/await and generators

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
