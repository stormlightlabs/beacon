# Beacon LSP Implementation Plan

## Overview

1. Get basic LSP infrastructure working first
2. Layer on the full Hindley-Milner type inference system

### Implementation Order

#### Completions Part 1: Attribute & Import completions

- Attribute completions after `.` (uses type inference)
- Import completions from workspace
- **Files:** `crates/server/src/features/completion.rs`

#### Completions Part 2: Filtering, ranking, cross-file

- Prefix matching, relevance ranking
- Cross-file symbol completions
- **Files:** `crates/server/src/features/completion.rs`

#### Advanced flow-sensitive narrowing with full CFG integration

- Create `TypeFlowAnalyzer` for tracking types across CFG blocks
- Implement forward dataflow analysis with join point merging
- Fixpoint iteration for loop-carried narrowing
- **Files:** `crates/server/src/analysis/type_flow.rs` (new), integrate with `mod.rs`

#### Module/Package Documentation on Hover

- Introspect installed packages
- Show module-level docs
- **Files:** `crates/server/src/features/hover.rs`, `crates/server/src/introspection.rs` (new)

#### Static Analysis Part 1: Linter & Quality Checks

- See [section](#static-analysis)
- Implement rule engine for basic diagnostics - documented in [lint rules](../src/lsp/lint_rules.md)
- Unused imports, variables, undefined names
- **Files:** `crates/server/src/analysis/linter.rs` (new), `crates/server/src/analysis/rules/mod.rs`, `crates/server/src/features/diagnostics.rs`

#### Static Analysis Part 2: Incremental Re-analysis

- Cache CFGs, symbol tables, diagnostics per module
- Recompute only changed scopes
- **Files:** `crates/server/src/analysis/cache.rs` (new), `crates/server/src/analysis/mod.rs`

#### Cross-File Diagnostics

- Circular imports, missing modules
- Dependency graph validation
- **Files:** `crates/server/src/workspace/graph.rs`, `crates/server/src/features/diagnostics.rs`

### Parking Lot (Future Phases)

- Annotation coverage checks
- Advanced caching strategies (persistent cache)
- Snippet completions
- Document formatting
- Inheritance awareness

## Completions

**Goal:** Provide context-aware autocomplete suggestions
**Tasks:**

### Part 1

- [ ] Attribute completions (after `.`)
    - Use type inference to get type of left-hand expression
    - Query type's attributes/methods
    - Handle built-in types (str, list, dict methods)
- [ ] Import completions
    - Suggest available modules from workspace
    - Complete import statement structure

### Part 2

- [ ] Filtering and ranking
    - Prefix matching against typed text
    - Rank by relevance (scope proximity, usage frequency)
- [ ] Cross-file symbol completions
**Implementation Notes:**
- Currently returns hardcoded builtins only
- Need to parse backwards from cursor to understand context
- Type inference integration critical for attribute completions
- Start with local scope, then built-ins, then keywords
- Consider fuzzy matching for better UX
**Files to Modify:**
- `crates/server/src/features/completion.rs`
**Acceptance:**
- Suggests visible symbols at cursor position
- Provides accurate attribute completions after `.`
- Filters suggestions based on what's typed
- Includes relevant keywords for context

## Static Analysis

**Goal:** Perform whole-workspace static analysis for code correctness, style, and type-hint consistency (semantic diagnostics).
**Tasks:**

### Part 1 - Linter & Quality Checks

**Partially Implemented / Need Enhancement:**

- [ ] BEA012: AssertTuple
    - **TODO:** Requires Assert AST node to be added to parser
    - **TODO:** Detect tuple literals in assert statements
- [ ] BEA023: ForwardAnnotationSyntaxError
    - **TODO:** Use proper Python expression parser for annotation validation
    - Current implementation only checks bracket matching

**Not Yet Implemented:**

- [ ] BEA004: YieldOutsideFunction
    - Need to track yield/yield-from in visit_node
- [ ] BEA009: TwoStarredExpressions
    - Need to parse starred expressions in assignment targets
- [ ] BEA010: TooManyExpressionsInStarredAssignment
    - Need to validate unpacking arity
- [ ] BEA014: TStringMissingPlaceholders
    - Template string support needed
- [ ] BEA015: UnusedImport
    - Infrastructure in place, needs integration with symbol reference tracking
- [ ] BEA017: UnusedAnnotation
    - Check annotated variables without references
- [ ] BEA018: RedefinedWhileUnused
    - Track variable reassignments before first use
- [ ] BEA022: UnusedIndirectAssignment
    - Check global/nonlocal declarations without reassignment
- [ ] BEA024: MultiValueRepeatedKeyLiteral
    - **TODO:** Requires expression evaluation for dict keys (line 340 in linter.rs)
- [ ] BEA025: PercentFormatInvalidFormat
    - Validate % format string syntax
- [ ] BEA029: RedundantPass
    - **TODO:** Requires separate pass to know if blocks have other content (line 266 in linter.rs)

**Infrastructure Improvements Needed:**

- [ ] Suppression support (`# type: ignore`, `# noqa:`)
    - Parse inline comments during analysis
    - Filter diagnostics based on suppressions
- [ ] Rule configuration
    - Per-rule enable/disable via config
    - Severity customization
- [ ] Symbol table integration
    - Currently `_symbol_table` field is unused
    - For BEA015, BEA017, BEA018, BEA022

**Limitations:**

1. Parser does not have Assert AST node - affects BEA012
2. Parser represents `from x import *` as empty names array (handled)
3. No support for starred expressions in assignment targets - affects BEA009, BEA010

### Part 2

- [ ] Incremental Re-analysis
    - Cache CFGs, symbol tables, and diagnostics per module
    - Recompute only changed scopes or dependent modules
    - Maintain last-known diagnostic set for live feedback
- [ ] Cross-File Diagnostics
    - Use workspace dependency graph to check:
        - Circular imports
        - Missing modules
        - Inconsistent symbol exports (`__all__` mismatches)
        - Conflicting stub definitions
- [ ] Integration Tests
    - Validate accuracy of error positions and messages
    - Verify cross-module invalidation
    - Benchmark incremental re-analysis performance

**Acceptance:**

- Detects and reports **exhaustive categories** of errors and warnings
- Handles **control-flow-aware** analysis (flow narrowing, dead-code detection).
- Works incrementally — updating diagnostics within 100 ms of file edits.
- Fully integrated with your existing workspace module index and dependency graph.

## Module/Package Documentation on Hover

- Make interpreter path configurable via LSP settings
- Support module-level imports (`import os`)
- Async introspection (non-blocking hover)
- Cross-file import resolution

## Document Formatting

Basic formatting support for user convenience

- Should conform to PEP8 (similar to black)
- Users will likely use other tools (black, ruff)
- Low priority - focus on analysis first

## Parking Lot

- Add inheritance awareness (`__init__` in base classes)
- Partial workspace analysis (analyze subset)
- Advanced caching strategies (persistent cache across sessions)
- Snippet completions (function templates, etc.)
- LSP resolve for expensive completion details
- Annotation coverage checks (detect missing/partial type annotations)
    - Warn when functions lack type hints or have incomplete annotations
    - Configurable per TypeCheckingMode (strict, balanced, minimal)
    - Compare inferred types with declared annotations
- Advanced flow-sensitive narrowing with full CFG integration
    - Create `TypeFlowAnalyzer` for tracking types across CFG blocks
    - Implement forward dataflow analysis with join point merging
    - Implement fixpoint iteration for loop-carried narrowing
    - Add CFG edge metadata for guard conditions
    - Integrate TypeFlowAnalyzer with main Analyzer
    - Handle nested conditions and complex control flow patterns
    - Track type narrowing across loop iterations
    - Support exception handler narrowing
