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

### Part 2

- [ ] Attribute completions (after `.`)
    - Use type inference to get type of left-hand expression
    - Query type's attributes/methods
    - Handle built-in types (str, list, dict methods)
- [ ] Import completions
    - Suggest available modules from workspace/stubs
    - Complete import statement structure
- [ ] Keyword completions
    - Context-aware keywords (def, class, if, for, etc.)
    - Statement vs expression context

### Part 3

- [ ] Filtering and ranking
    - Prefix matching against typed text
    - Rank by relevance (scope proximity, usage frequency)
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
**Deferred:**
- Cross-file symbol completions (requires workspace analysis)
- Snippet completions (function templates, etc.)
- LSP resolve for expensive completion details

## Static Analysis

**Goal:** Perform whole-workspace static analysis for code correctness, style, and type-hint consistency (semantic diagnostics).
**Tasks:**

- [ ] Symbol Table & Scope Analysis
    - Build per-module symbol tables with local, nonlocal, and global scope resolution
    - Track variable assignments and usages across control-flow paths
    - Record symbol kinds (function, class, import, variable, parameter, etc.)
    - Detect shadowed names and unused variables
- [ ] Control Flow & Data Flow Analysis
    - Construct per-function control-flow graphs (CFG)
    - Perform definite-assignment checks (use before assignment)
    - Detect unreachable code and inconsistent return paths
    - Implement simple constant propagation for “always true/false” conditionals
    - Support `try/except/finally` and context managers in CFGs
- [ ] Type Consistency & Type Hint Checking
    - Parse and resolve PEP 484/585 type hints and annotations
    - Validate annotated types against inferred/observed values
    - Warn on incompatible assignments, mismatched call arguments, or bad returns
    - Detect missing or partially specified annotations
    - Integrate with `.pyi` stubs and third-party type definitions (defer)
- [ ] Flow-Sensitive Narrowing
    - Implement local flow narrowing (e.g., `if isinstance(x, int): …` → x: int)
    - Track `None` checks (`if x is not None`)
    - Infer type unions for conditional branches

### Part 2

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

### Part 3

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

## Workspace-wide Analysis

**Goal:** Coordinate multi-file type checking and navigation
**Tasks:**

### Part 1

- [ ] File discovery and indexing
    - Scan workspace root for .py files
    - Watch file system for changes (create/delete/modify)
    - Maintain index of workspace Python files
- [ ] Module resolution
    - Convert file paths to module names (src/foo/bar.py → foo.bar)
    - Reverse: resolve import statements to file paths
    - Handle package `__init__.py` files
    - Support configurable source roots
- [ ] Dependency graph
    - Parse import statements from each file
    - Build directed graph of module dependencies
    - Detect strongly connected components (circular imports)
    - Topological sort for analysis order

- [ ] Stub file support (defer)
    - Discover .pyi stub files in configured paths
    - Parse stub files for type signatures
    - Prefer stubs over source when available
    - Cache parsed stubs (LRU eviction)

### Part 2

- [ ] Incremental invalidation
    - When file changes, invalidate dependents
    - Re-analyze affected modules in dependency order
    - Avoid re-analyzing unchanged modules
- [ ] Cross-file operations
    - Goto-definition across files (follow imports)
    - Find references across workspace
    - Workspace symbols (fuzzy search all symbols)
- [ ] Add integration tests

**Implementation Notes:**

- Current scaffold in `crates/server/src/workspace.rs`
- File watching: use `notify` crate or LSP file watching
- Dependency graph: track import edges, detect SCCs with Tarjan's algorithm
- Memory management: don't load entire workspace into RAM, use LRU caches
- Parallelization: analyze independent modules in parallel
- Error handling: graceful degradation when files have syntax errors
**Files to Modify:**
- `crates/server/src/workspace.rs`
- `crates/server/src/document.rs` (integrate with workspace)
- `crates/server/src/features/goto_definition.rs` (cross-file support)
- `crates/server/src/features/references.rs` (cross-file support)
**Acceptance:**
- Discovers all Python files in workspace
- Resolves import statements to source files
- Detects and reports circular dependencies
- Incremental re-analysis when files change
- Goto-definition works across files
- Find references works across workspace
- Handles workspaces with 1000+ files efficiently
**Deferred:**
- Distributed/remote workspaces
- Partial workspace analysis (analyze subset)
- Advanced caching strategies (persistent cache across sessions)

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
