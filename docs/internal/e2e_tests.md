# End-to-End Test Suite

The integration plan mirrors the formatter regression style: inline fixtures, direct
assertions, and cargo tests committed to the repo. We'll tackle suites in the
following order so each layer builds on the previous one.

## Parser Harness

- Implement `parser_e2e.rs` per `docs/internal/e2e_parser_tests.md`
- Seed fixtures for advanced syntax, typing-heavy code, and pattern matching

## Configuration Flow Coverage

- Use the parser harness to exercise config load + hot-reload scenarios
- Ensure `beacon.toml` / workspace settings round-trips are covered
- Test LSP configuration integration

## Static Analysis E2E

- Add `crates/analyzer/tests/analyzer_tests.rs` & `crates/server/tests/static_analysis_tests.rs`
  as described in this document
- Focus on CFG/data-flow diagnostics over multi-file fixtures

## Hindley-Milner Solver

- Follow `docs/internal/e2e_hm_tests.md` to assert on inferred types and solver diagnostics
- Test variance checking and protocol-based structural typing

## Linter

- Build `linter_tests.rs` according to this document, verifying lint messages and autofixes
- Test suppression support and rule configuration

## Completion Flow Validation

- After linter coverage, add integration tests for snippet/completion pipelines
- Test context-aware filtering and LSP completion integration

## Formatting Integration

- Test `textDocument/formatting` and `textDocument/rangeFormatting` handlers
- Verify format-on-save and format-on-type integration
- Test configuration hot-reload for formatter settings

## Tasks

### Parser E2E

- [ ] Implement parser e2e harness (`parser_e2e.rs`)
- [ ] Add fixtures for advanced syntax (walrus operator, match statements, async comprehensions)
- [ ] Add fixtures for typing-heavy code (Protocols, TypedDict, Generic variance)
- [ ] Add fixtures for pattern matching (structural patterns, guards, bindings)
- [ ] Investigate parser output for "is not" expressions (`test_is_not_literal` currently skipped)
- [ ] Populate missing fixture directories

### Configuration Integration

- [ ] Test config loading from `beacon.toml`
- [ ] Test config loading from `pyproject.toml`
- [ ] Test workspace settings override
- [ ] Test config hot-reload via `did_change_configuration`
- [ ] Test diagnostic severity configuration
- [ ] Test inlay hints configuration
- [ ] Test formatter configuration integration

### Static Analysis E2E

- [ ] Create helper `assert_diagnostics` util to compare `(rule_id, message, span)` tuples
- [ ] Populate missing fixture directories for integration tests
- [ ] Add automated dot-graph comparison for CFG visualization
- [ ] Test cross-file diagnostics (symbol exports, stub conflicts)

### HM Type Checker

- [ ] Fixtures for row polymorphism and record types
- [ ] Bivariant variance for special cases
- [ ] Variance checking for Protocol definitions
- [ ] Variance inference for generic classes without explicit annotations
- [ ] Generator/AsyncGenerator/Coroutine integration test fixtures (see module notes for more context)
- [ ] Symbol table integration for `get_symbol_type` to look up types by identifier name (stub exists, tests use pattern matching instead)

### Linter Integration

- [ ] Test suppression support (`# type: ignore`, `# noqa:`)
- [ ] Test per-rule configuration (enable/disable, severity)
- [ ] Implement comprehensive autofix test scaffolding with verification
- [ ] Test symbol table integration for unused detection
- [ ] Fix skipped test: `test_nested_functions_break_continue` (linter scope reset for nested functions)

### Snippet Engine Integration

- [ ] Test snippet completion provider registration
- [ ] Test `textDocument/completion` handler for snippets
- [ ] Test snippet placeholder navigation (tab stops, nested placeholders)
- [ ] Test context-aware snippet filtering (scope detection)
- [ ] Test snippet variable resolution (`$TM_FILENAME`, `$LINE_COMMENT`, etc.)
- [ ] Test multi-cursor snippet expansion
- [ ] Test user-defined snippet loading (workspace and global)
- [ ] Test snippet priority system (user > workspace > built-in)
- [ ] Test snippet auto-import insertion
- [ ] Validate all built-in snippets are syntactically correct

### Formatting Integration

- [ ] Test `textDocument/formatting` handler
- [ ] Test `textDocument/rangeFormatting` handler
- [ ] Test `textDocument/willSaveWaitUntil` for format-on-save
- [ ] Test `textDocument/onTypeFormatting` for automatic formatting
- [ ] Test formatter config hot-reload
- [ ] Test `# beacon: disable` comments to skip formatting for specific regions
- [ ] Test compatibility with Black/autopep8 outputs
- [ ] Test error handling (malformed code, incomplete parsing)
- [ ] Test incremental formatting (cache unchanged regions)

### LSP Features Integration

- [ ] Test signature help (`textDocument/signatureHelp`)
- [ ] Test code actions / quick-fixes (`textDocument/codeAction`)
- [ ] Test auto-import / import management
- [ ] Test workspace/folder support (`workspace/didChangeWorkspaceFolders`)
- [ ] Test diagnostics publishing (`textDocument/publishDiagnostics`)
- [ ] Test "did you mean" suggestions
