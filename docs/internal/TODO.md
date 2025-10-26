# Beacon LSP Implementation Plan

## Overview

1. Get basic LSP infrastructure working first
2. Layer on the full Hindley-Milner type inference system

### Implementation Order

#### Static Analysis Part 1: Linter & Quality Checks

- Implement rule engine for basic diagnostics
- Unused imports, variables, undefined names
- **Files:** `crates/server/src/analysis/linter.rs` (new), `crates/server/src/analysis/rules/mod.rs`, `crates/server/src/features/diagnostics.rs`

#### Hover Documentation

- Show inferred types at cursor position (uses type_map from #1)
- Display docstrings for symbols
- Basic type signature formatting
- **Files:** `crates/server/src/features/hover.rs`

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

### Part 1

- [ ] Linter & Quality Checks
    - Add rule engine for common static rules:
        - Unused imports, variables, parameters
        - Undefined variables
        - Duplicate definitions
        - Unreachable code
        - Redundant `pass` or empty `except`
    - Provide suppressions (`# type: ignore`, `# noqa`, etc.)
    - Group diagnostics by category (error, warning, hint)

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

## Hover Documentation

- Advanced RST rendering (directives, cross-references)
- NumPy/Google docstring style parsing
- Dynamic docstring generation from type hints
- Inline documentation for built-in types

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

Advanced features and optimizations for future consideration.

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
