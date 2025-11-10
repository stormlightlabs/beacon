# Analysis Refactoring Plan

## Overview

Refactor the analysis code from `crates/server/src/analysis/` to a shared `beacon-analyzer` crate, enabling CLI access to static analysis while maintaining LSP functionality.

## Current State

### Code in `crates/server/src/analysis/`

**General-purpose analysis (should move):**

- `cfg.rs` - Control Flow Graph construction
- `data_flow.rs` - Data flow analysis (use-before-def, unreachable code, unused variables)
- `linter.rs` - Static linting (PyFlakes-style rules)
- `rules/mod.rs` - Linting rule definitions and diagnostics
- `pattern.rs` - Pattern matching analysis (match exhaustiveness, reachability)
- `type_env.rs` - Type environment for constraint generation
- `walker.rs` - AST walking and constraint generation
- `loader.rs` - Stub file loading into class registry

**LSP-specific (should stay):**

- `mod.rs` - Main `Analyzer` orchestrator (uses DocumentManager, CacheManager, Workspace)

## Target Architecture

### beacon-analyzer Crate

Core static analysis library with no LSP dependencies.

**Public API:**

```rs
pub struct StaticAnalyzer
  - new(config: AnalyzerConfig) -> Self
  - analyze_source(source: &str, filename: &str) -> AnalysisResult
  - analyze_ast(ast: &AstNode, symbol_table: &SymbolTable) -> AnalysisResult

pub struct AnalysisResult
  - type_map: FxHashMap<usize, Type>
  - position_map: FxHashMap<(usize, usize), usize>
  - type_errors: Vec<TypeErrorInfo>
  - cfg_results: Option<CfgAnalysisResult>
  - data_flow_results: Option<DataFlowResult>
  - lint_diagnostics: Vec<DiagnosticMessage>

pub struct CfgAnalysisResult
  - cfgs: FxHashMap<String, ControlFlowGraph>  // function name -> CFG
  - reachable_blocks: FxHashMap<String, Vec<BlockId>>

pub mod cfg { ... }
pub mod data_flow { ... }
pub mod linter { ... }
pub mod rules { ... }
pub mod pattern { ... }
```

**Dependencies:**

- `beacon-core` - Type system
- `beacon-parser` - AST and symbol table
- `beacon-constraint` - Constraint generation and solving
- `rustc-hash` - Hash maps
- `url` (optional, for file URIs)

### beacon-lsp Crate

LSP server with document management and caching.

**Adapter Pattern:**

- Keep existing `Analyzer` in `mod.rs`
- Use `beacon-analyzer::StaticAnalyzer` internally
- Add LSP-specific features: caching, incremental analysis, workspace integration

**Structure:**

```sh
crates/server/src/analysis/
  mod.rs          - Analyzer orchestrator (LSP-specific)
  adapter.rs      - Bridge between LSP and beacon-analyzer (NEW)
```

### CLI Integration

Add `analyze` subcommand with granular targets.

**Commands:**

```bash
# Analyze entire package (directory with __init__.py)
beacon analyze package ./src/myapp

# Analyze entire project (workspace with multiple packages)
beacon analyze project .

# Analyze single file
beacon analyze file ./src/myapp/core.py

# Analyze specific function in file
beacon analyze function ./src/myapp/core.py:process_data

# Analyze specific class in file
beacon analyze class ./src/myapp/models.py:User
```

**Output Options:**

- `--format json|human|compact`
- `--show-cfg` - Include CFG visualization
- `--show-types` - Show inferred types
- `--lint-only` - Only run linter
- `--dataflow-only` - Only run data flow analysis

## Migration Steps

### Step 1: Create beacon-analyzer Foundation

1. Update `crates/analyzer/Cargo.toml`:
   - Add dependencies: beacon-core, beacon-parser, beacon-constraint, rustc-hash

2. Create module structure in `crates/analyzer/src/`:
   - `lib.rs` - Public API and StaticAnalyzer
   - `cfg.rs` - Move from server
   - `data_flow.rs` - Move from server
   - `linter.rs` - Move from server
   - `rules.rs` - Move from server/analysis/rules/mod.rs
   - `pattern.rs` - Move from server
   - `type_env.rs` - Move from server
   - `walker.rs` - Move from server
   - `loader.rs` - Move from server

3. Update imports and remove LSP-specific dependencies:
   - Remove `crate::cache::*` references
   - Remove `crate::document::DocumentManager` references
   - Remove `crate::workspace::*` references (keep only types needed for stubs)
   - Make all modules work with plain AST/SymbolTable inputs

### Step 2: Create LSP Adapter

1. Keep `crates/server/src/analysis/mod.rs` as is
2. Create `crates/server/src/analysis/adapter.rs`:
   - Bridge between DocumentManager and StaticAnalyzer
   - Handle caching strategy
   - Convert between LSP-specific and analyzer-specific types
3. Update `Analyzer` to use `beacon-analyzer::StaticAnalyzer` internally

### Step 3: Update beacon-lsp Dependencies

1. Add `beacon-analyzer` to `crates/server/Cargo.toml`
2. Update `crates/server/src/analysis/mod.rs`:
   - Import from `beacon_analyzer::*`
   - Remove direct references to moved modules
   - Keep DocumentManager, CacheManager, Workspace integration

### Step 4: Add CLI Commands

1. Update `crates/cli/src/main.rs`:
   - Add `Analyze` subcommand with variants (Package, Project, File, Function, Class)
   - Add output formatting options
   - Add `Lint` subcommand

2. Implement analyzer commands:
   - Use `beacon-analyzer` directly (no DocumentManager needed)
   - Parse source with `beacon-parser`
   - Run analysis with `StaticAnalyzer`
   - Format output based on `--format` flag

3. Add helper for function/class targeting:
   - Parse `file:name` syntax
   - Extract specific function/class from AST
   - Run focused analysis

### Step 5: Split E2E Tests

Based on `docs/internal/e2e_analysis_tests.md`:

**Tests for beacon-analyzer:**

- CFG construction and reachability
- Data flow analysis diagnostics
- Linter rule checks
- Pattern matching exhaustiveness

**Tests for beacon-lsp:**

- Incremental analysis with caching
- Document update handling
- Workspace-aware analysis
- LSP diagnostic conversion

Create:

- `crates/analyzer/tests/analysis_core_e2e.rs` - Core analysis tests
- Keep `crates/server/tests/analysis_e2e.rs` - LSP integration tests (if exists)

## Implementation Order

### Phase 1: Foundation (this PR)

1. Create beacon-analyzer crate structure
2. Move general-purpose modules
3. Add StaticAnalyzer public API
4. Update beacon-lsp to use beacon-analyzer
5. Ensure existing tests pass

### Phase 2: CLI Integration

1. Add `beacon analyze` commands
2. Implement output formatters
3. Add CLI tests

### Phase 3: Test Organization

1. Split e2e tests
2. Add analyzer-specific tests
3. Add CLI integration tests

## Success Criteria

- All existing LSP functionality works unchanged
- `beacon-analyzer` can be used standalone (no LSP dependencies)
- CLI can run analysis without starting LSP server
- Test coverage maintained/improved
- Clear separation between LSP-specific and general analysis code

## File Movement Summary

### Move to `crates/analyzer/src/`

```sh
crates/server/src/analysis/cfg.rs          -> crates/analyzer/src/cfg.rs
crates/server/src/analysis/data_flow.rs    -> crates/analyzer/src/data_flow.rs
crates/server/src/analysis/linter.rs       -> crates/analyzer/src/linter.rs
crates/server/src/analysis/pattern.rs      -> crates/analyzer/src/pattern.rs
crates/server/src/analysis/type_env.rs     -> crates/analyzer/src/type_env.rs
crates/server/src/analysis/walker.rs       -> crates/analyzer/src/walker.rs
crates/server/src/analysis/loader.rs       -> crates/analyzer/src/loader.rs
crates/server/src/analysis/rules/mod.rs    -> crates/analyzer/src/rules.rs
```

### Keep in `crates/server/src/analysis/`

```sh
crates/server/src/analysis/mod.rs          - Analyzer orchestrator
```

### New files

```sh
crates/analyzer/src/lib.rs                 - Public API
crates/server/src/analysis/adapter.rs      - LSP adapter (optional)
```

## Workspace Dependency Handling

The `loader.rs` module references `crate::workspace::StubCache`. Options:

1. **Extract StubCache to beacon-core** - Make it available to analyzer
2. **Pass as trait** - Define `StubProvider` trait in analyzer
3. **Keep in LSP, pass data** - Analyzer receives pre-loaded stub data

Recommendation: Option 2 (trait-based) for flexibility.
