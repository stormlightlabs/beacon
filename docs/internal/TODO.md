# Beacon LSP Implementation Plan

Implementation details and mod-specific tasks.

## Type System

### Class Features - Decorators & Fields

- [x] Add class-level annotation extraction in extract_class_metadata
- [x] Implement @dataclass special handling
- [x] Add enum class support
- [x] Fix decorator constraint generation

**Impact**: Clears 20+ false positives (CacheEntry/bool, enum attributes, dataclass fields)

### Stdlib Integration

**Goal**: Complete method signatures for builtin types

**Tasks**:

- [ ] Expand builtins.pyi stub
    - str methods: splitlines, strip, replace, format, etc.
    - list methods: append, extend, pop, etc.
    - dict methods: get, items, keys, values, etc.
- [ ] Add pathlib.pyi stub
    - Path class with **init**, read_text, write_text, exists, etc.
    - PurePath methods
- [ ] Add typing.pyi improvements
    - Generator, Iterator protocol methods
    - Ensure TypeVar, Generic properly defined
- [ ] Fix stub loading to ensure all modules loaded
    - Verify builtins always loaded
    - Add logging for missing stubs

**Impact**: Clears 10+ false positives (splitlines not found, Path.write_text, etc.)

### Type Inference

**Goal**: Fix complex type inference issues

**Tasks**:

- [x] Filter module/class docstrings from constraint generation
- [x] Add return type inference from context
- [x] Generator/comprehension type construction
- [ ] Type parameter instantiation for generic types
    - When looking up methods on `list[T]`, dict[K,V]`, etc., instantiate type parameters
    - Example: `list[int].__getitem__` should return `int`, not `_T`
    - Requires: Type parameter tracking and substitution in method signatures
    - **Note**: Large feature requiring changes to type system and class registry

**Impact**: Docstring fix clears ANY001 false positives on module/class/function docstrings

### Flow-Sensitive Type Narrowing

**Location**: `crates/server/src/analysis/walker.rs`, `crates/constraints/src/lib.rs`

**Goal**: Implement type narrowing after None checks and type guards to support patterns like:

```python
calc = maybe_calc()  # calc: Calculator | None
if calc is not None:
    calc.add(1, 2)  # calc narrowed to Calculator here
```

**Tasks**:

- [ ] Design control flow tracking system in constraint generator
- [ ] Implement None-check narrowing
- [ ] Add Narrowing constraint type
- [ ] Extend to isinstance() checks
- [ ] Handle truthiness narrowing
- [ ] Add tests for flow-sensitive narrowing

### Polish & Validation

**Goal**: Zero false positives on test files

**Tasks**:

- [ ] Run full diagnostic pass on all sample files
    - Verify bugs.json is empty
    - Check for new edge cases
- [ ] Add missing linter suppressions
    - Implement `# type: ignore` support
    - Handle intentional test cases (unused variables, etc.)
- [ ] Improve error messages
    - Show clearer messages for decorator issues
    - Better formatting for union type mismatches
    - Add "did you mean?" suggestions
- [ ] Update TODO.md with remaining enhancements
    - Flow-sensitive type narrowing (separate from false positive fixes)
    - Pattern matching support
    - Performance optimizations
- [ ] Documentation update
    - Document known limitations
    - Add troubleshooting guide for type errors

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
