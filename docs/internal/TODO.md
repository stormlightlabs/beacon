# Beacon LSP Implementation Plan

This document outlines the plan for implementing a minimal, working, testable LSP server for Beacon.

1. Get basic LSP infrastructure working first
2. Layer on the full Hindley-Milner type inference system.

## Completions

**Goal:** Provide context-aware autocomplete suggestions
**Tasks:**
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
    - [ ] Import completions
        - Suggest available modules from workspace/stubs
        - Complete import statement structure
    - [ ] Keyword completions
        - Context-aware keywords (def, class, if, for, etc.)
        - Statement vs expression context
    - [ ] Filtering and ranking
        - Prefix matching against typed text
        - Rank by relevance (scope proximity, usage frequency)
    - [ ] Add comprehensive tests
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

## Workspace-wide Analysis

**Goal:** Coordinate multi-file type checking and navigation
**Tasks:**
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
    - [ ] Stub file support
        - Discover .pyi stub files in configured paths
        - Parse stub files for type signatures
        - Prefer stubs over source when available
        - Cache parsed stubs (LRU eviction)
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
