# Beacon LSP Implementation Plan

This document outlines the plan for implementing a minimal, working, testable LSP server for Beacon.

1. Get basic LSP infrastructure working first
2. Layer on the full Hindley-Milner type inference system.

## Completions

**Goal:** Provide context-aware autocomplete suggestions
**Tasks:**

### Part 1

- [ ] Implement context detection
    - Determine if cursor is in expression, import statement, or attribute access
    - Identify partial identifier being typed
- [ ] Symbol table completions
    - Extract all symbols visible in current scope
    - Include variables, functions, classes from symbol table
    - Walk up scope chain for nested contexts
- [ ] Attribute completions (after `.`)
    - Use type inference to get type of left-hand expression
    - Query type's attributes/methods
    - Handle built-in types (str, list, dict methods)
- [ ] Keyword completions
    - Context-aware keywords (def, class, if, for, etc.)
    - Statement vs expression context

### Part 2

- [ ] Import completions
    - Suggest available modules from workspace/stubs
    - Complete import statement structure
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

- [x] Symbol Table & Scope Analysis
    - Build per-module symbol tables with local, nonlocal, and global scope resolution
    - Track variable assignments and usages across control-flow paths
    - Record symbol kinds (function, class, import, variable, parameter, etc.)
    - Detect shadowed names and unused variables
- [x] Control Flow & Data Flow Analysis
    - Construct per-function control-flow graphs (CFG)
    - Perform definite-assignment checks (use before assignment)
    - Detect unreachable code and inconsistent return paths
    - Implement simple constant propagation for “always true/false” conditionals
    - Support `try/except/finally` and context managers in CFGs

### Part 2

- [ ] Type Consistency & Type Hint Checking
    - Parse and resolve PEP 484/585 type hints and annotations
    - Validate annotated types against inferred/observed values
    - Warn on incompatible assignments, mismatched call arguments, or bad returns
    - Detect missing or partially specified annotations
- [ ] Flow-Sensitive Narrowing
    - Implement local flow narrowing (e.g., `if isinstance(x, int): …` → x: int)
    - Track `None` checks (`if x is not None`)
    - Infer type unions for conditional branches

### Part 3

- [ ] Diagnostics Integration
    - Emit LSP `textDocument/publishDiagnostics` notifications
    - Include severity, code, and message
    - Support quick fixes for common issues (e.g., remove unused import)
- [ ] Cross-File Diagnostics
    - Use workspace dependency graph to check:
        - Circular imports
        - Missing modules
        - Inconsistent symbol exports (`__all__` mismatches)
        - Conflicting stub definitions

### Part 4

- [ ] Linter & Quality Checks
    - Add rule engine for common static rules:
        - Unused imports, variables, parameters
        - Undefined variables
        - Duplicate definitions
        - Unreachable code
        - Redundant `pass` or empty `except`
    - Provide suppressions (`# type: ignore`, `# noqa`, etc.)
    - Group diagnostics by category (error, warning, hint)
- [ ] Incremental Re-analysis
    - Cache CFGs, symbol tables, and diagnostics per module
    - Recompute only changed scopes or dependent modules
    - Maintain last-known diagnostic set for live feedback
- [ ] Integration Tests
    - Validate accuracy of error positions and messages
    - Verify cross-module invalidation
    - Benchmark incremental re-analysis performance

**Acceptance:**

- Detects and reports **exhaustive categories** of errors and warnings
- Handles **control-flow-aware** analysis (flow narrowing, dead-code detection).
- Works incrementally — updating diagnostics within 100 ms of file edits.
- Fully integrated with your existing workspace module index and dependency graph.

## Stub file support

- Discover .pyi stub files in configured paths
- Parse stub files for type signatures
- Prefer stubs over source when available
- Cache parsed stubs (LRU eviction)

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

## Code actions (insert annotations, fix types)

## Protocol/structural type checking

## Document formatting

- Should conform to PEP8 (similar to black)
    - Users will likely use other tools

## Improve Error Messaging

- Write "did you mean?" suggestions

## Parking Lot

- Add inheritance awareness (`__init__` in base classes)
- Partial workspace analysis (analyze subset)
- Advanced caching strategies (persistent cache across sessions)
- Snippet completions (function templates, etc.)
- LSP resolve for expensive completion details
- Integrate static analysis with `.pyi` stubs and third-party type definitions
