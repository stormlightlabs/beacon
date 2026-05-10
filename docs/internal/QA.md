# Beacon QA And E2E Plan

Use this file for QA workflows, e2e test planning, and validation of analysis/refactoring work.

## Baseline

Run from the repository root:

```sh
git submodule update --init --recursive
just fmt
just lint
just test
mdbook build docs
```

Expanded Rust equivalents:

```sh
cargo fmt --all --check
cargo clippy --workspace --all-targets --all-features -- -D warnings
cargo test --workspace
```

Editor package checks:

```sh
pnpm install
pnpm --filter beacon-lsp compile
pnpm --filter beacon-lsp lint
```

## Setup Checks

Before trusting test results:

- `git submodule status typeshed` should show a checked-out commit.
- `typeshed/stubs` should exist.
- Generated embedded stubs should report a non-zero count.
- `pnpm install` should have been run before editor package checks.
- Debug-only CLI commands should be tested with a debug build.

Useful probes:

```sh
git submodule status typeshed
ls typeshed/stubs
cargo test -p beacon-analyzer embedded_stubs
cargo test -p beacon-analyzer loader::tests::test_stub_loading_typeshed_builtins
cargo run -p beacon-cli -- version
```

## Focused Commands

Parser:

```sh
just test-parser
cargo test -p beacon-parser
```

Core type system and constraints:

```sh
cargo test -p beacon-core
cargo test -p beacon-constraint
```

Analyzer:

```sh
cargo test -p beacon-analyzer
cargo test -p beacon-analyzer data_flow
cargo test -p beacon-analyzer linter
cargo test -p beacon-analyzer workspace
```

Language server:

```sh
cargo test -p beacon-lsp
cargo test -p beacon-lsp diagnostics
cargo test -p beacon-lsp formatting
cargo test -p beacon-lsp refactoring
```

CLI:

```sh
just test-cli
cargo test -p beacon-cli
```

## CLI Smoke Tests

Use samples and a temporary workspace to verify frontend behavior without an editor:

```sh
cargo run -p beacon-cli -- check samples/basic.py
cargo run -p beacon-cli -- resolve samples/basic.py --verbose
cargo run -p beacon-cli -- typecheck samples/basic.py
cargo run -p beacon-cli -- typecheck --format json samples/basic.py
cargo run -p beacon-cli -- lint samples
cargo run -p beacon-cli -- analyze project samples
cargo run -p beacon-cli -- format --check samples
```

Debug build probes:

```sh
cargo run -p beacon-cli -- debug tree samples/basic.py
cargo run -p beacon-cli -- debug ast samples/basic.py
cargo run -p beacon-cli -- debug constraints samples/basic.py
cargo run -p beacon-cli -- debug diagnostics samples
```

Commands that print placeholder `TODO` output stay outside the v1 contract until implemented or hidden.

## E2E Test Plan

The e2e suite should prove Beacon's v1 contract across parser, analyzer, CLI, and LSP. Prefer inline fixtures and direct assertions committed to the repo.

### Fixture Strategy

Create one shared "v1 package" fixture with:

- Package root and at least one subpackage.
- `__init__.py` re-exports and `__all__`.
- Relative and absolute imports.
- A `.pyi` stub overriding or supplementing a `.py` file.
- A custom stub path case.
- A circular import.
- A star import.
- A private symbol import.
- A dataclass, protocol, generic class, overload, async function, generator, context manager, and pattern match.
- One symbol exercised by hover, goto, references, rename, typecheck, and diagnostics.

Expected assertions should compare diagnostic code, severity, message fragment, and span.

### Parser And Symbol Resolution

- Advanced syntax: walrus, pattern matching, async, comprehensions, decorators, generic annotations.
- Type-heavy syntax: Protocol, TypedDict, TypeVar bounds/constraints, ParamSpec-style syntax where supported.
- Symbol scopes: locals, globals, nonlocals, comprehensions, class scopes, imports, and re-exports.

### HM And Constraint Solver

- Inferred local types, polymorphic functions, generic classes, and row/record-like structural behavior.
- Type variable bounds, constraints, and variance.
- Callable, overload, receiver, positional, keyword, defaulted, variadic, async, generator, and coroutine behavior.
- Protocol structural conformance and negative cases.
- Union/optional simplification, narrowing, TypeGuard, TypeIs, and pattern constraints.

### Analyzer Diagnostics

- CFG construction and reachability.
- Use-before-def, unused variable, unreachable code, constant propagation.
- Linter rules and suppression behavior.
- Import/export diagnostics, private imports, star imports, missing symbols, and re-export chains.
- Taint analysis only where behavior is intentionally in the v1 contract.

### LSP Protocol

- Open, change, save, close, delete, and rename file flows.
- Diagnostics publish and clear with version safety.
- Hover, completion, goto definition, references, rename, document symbols, workspace symbols, semantic tokens, folding, inlay hints, formatting, and code actions.
- Refactoring edits for extract function, extract variable, inline function, move symbol, and change signature.

### CLI Contract

- `typecheck`, `analyze`, and `lint` produce the same diagnostics as LSP for the same workspace.
- JSON output is stable enough for snapshots and CI.
- Debug commands either produce real data or are clearly outside v1.

## Refactor Validation

Use this checklist before and after any refactor touching parser, core types, constraints, analyzer, workspace, diagnostics, CLI, LSP, formatter, editor packages, or shared test fixtures:

- Identify the owner boundary: parser, core, constraints, analyzer, server, CLI, or editor package.
- Add or update characterization tests before changing behavior.
- Confirm CLI and LSP still route through the same analysis path where intended.
- Verify config loading and suppressions still apply.
- Run focused tests for the touched crate, then `cargo test --workspace`.
- For workspace changes, test invalidation and dependency fan-out.
- For checker changes, compare substitutions, type errors, narrowing behavior, protocol behavior, pattern behavior, and attribute/call behavior.
- For diagnostics, compare code, severity, message, and span.
- For refactoring features, inspect generated edits for import changes and unrelated rewrites.
- For public API changes, confirm downstream crates compile and dependency direction stays intentional.
- For stub or import changes, verify behavior with the initialized `typeshed` submodule.

## Manual LSP QA

Run the server:

```sh
cargo run -p beacon-cli -- lsp --stdio
```

For TCP smoke testing:

```sh
cargo run -p beacon-cli -- lsp --tcp --host 127.0.0.1 --port 9350
```

In VS Code, Zed, or another LSP client, verify:

- Open/edit/save/close a Python file without crashes.
- Diagnostics appear, update after edits, and clear when fixed.
- Parse, type, lint, data-flow, import, and pattern diagnostics have correct spans.
- Hover shows inferred types and builtin/stub documentation where available.
- Completion works for imports, member access, calls, names, and keywords.
- Go to definition and references work inside one file and across files.
- Rename updates only the intended scope or workspace symbol set.
- Document symbols, workspace symbols, document highlight, folding, semantic tokens, and inlay hints are stable.
- Formatting works for full document, range formatting, on-type formatting, and save formatting.
- Code actions/refactorings produce valid edits and preserve unrelated code.
- Config changes update behavior without restarting when hot-reload is expected.

## Diagnostic Matrix

Every v1 diagnostic class needs at least one automated test and one LSP smoke case:

- Syntax/parser errors.
- Name resolution: undefined names, shadowing.
- Imports: unresolved module, missing module, circular import, invalid/private import, star import, re-export.
- HM/type system: mismatch, occurs check, generic arity, protocol failure, attribute missing, argument count/type, keyword errors, variance.
- Annotation/mode: missing annotations, annotation mismatches, implicit `Any`, strict/balanced/relaxed severity differences.
- Pattern matching: non-exhaustive, unreachable, type mismatch, structure mismatch.
- Data-flow: use before definition, unreachable code, unused variable.
- Type safety: unsafe `Any`.
- Dunder/magic method placement.
- Linter BEA rules and suppression behavior.

## Performance QA

Track these budgets once semantics are stable:

- Single-file parse/check latency.
- Cold workspace indexing time.
- Incremental reanalysis time for one edited file.
- Reanalysis fan-out after an exported symbol changes.
- Memory after opening a large workspace.
- Cache hit rate during rapid edits.

Suggested commands:

```sh
cargo bench -p beacon-core
cargo bench -p beacon-lsp
cargo bench -p beacon-cli
```

Performance changes count only when diagnostics remain stable for the same fixture.

## Release QA

Before a prerelease:

- `just fmt`, `just lint`, and `just test` pass from a clean checkout with submodules initialized.
- VS Code and Zed packages compile and lint.
- `mdbook build docs` passes.
- JSON diagnostic output is stable.
- All ignored tests are justified here or removed.
- Release logging defaults are privacy-safe.
- Known limitations appear in public docs and internal notes.
