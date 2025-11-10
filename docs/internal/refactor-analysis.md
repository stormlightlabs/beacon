# Analysis Refactoring Plan

## Overview

Refactor the analysis code from `crates/server/src/analysis/` to a shared `beacon-analyzer` crate, enabling CLI access to static analysis while maintaining LSP functionality.

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

## Add CLI Commands

1. Update `crates/cli/src/main.rs`:
   - Add `Analyze` subcommand with variants (Package, Project, File, Function, Class)
   - Add output formatting options
   - Add `Lint` subcommand (doesn't fix, just prints)

2. Implement analyzer commands:
   - Use `beacon-analyzer` directly (no DocumentManager needed)
   - Parse source with `beacon-parser`
   - Run analysis with `StaticAnalyzer`
   - Format output based on `--format` flag

3. Add helper for function/class targeting:
   - Parse `file:name` syntax
   - Extract specific function/class from AST
   - Run focused analysis

## Split E2E Tests

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

- `crates/analyzer/tests/analyzer_tests.rs` - Core analysis tests
- Keep `crates/server/tests/static_analysis_tests.rs` - LSP integration tests (if exists)
