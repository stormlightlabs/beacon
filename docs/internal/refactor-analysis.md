# Analysis Refactoring Plan

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
