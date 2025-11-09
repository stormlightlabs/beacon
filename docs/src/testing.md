# Contributor Testing Guide

The repository contains several layers of tests that keep the formatter, language server,
and static analysis features aligned. Run the suites below before submitting formatter or LSP changes.

## Formatter

- `cargo test --package beacon-lsp --test formatting_regression_tests`
  End-to-end regression coverage for real-world Python snippets. Recent additions include:
    - `test_typevar_assignments` for covariant bounds and keyword spacing.
    - `test_walrus_operator_patterns` and `test_generators_and_yield` for modern syntax.
    - `test_data_science_method_calls` and
    `test_complex_lambda_and_functional` for keyword-heavy method chains and nested lambda expressions.
- `cargo test --package beacon-lsp --test formatting_tests`
  Unit tests exercising individual formatting rules (whitespace, imports, doc strings, range formatting, etc.).
- `cargo test --package beacon-lsp --test lsp_formatting_integration_tests`
  Validates document/range formatting via the LSP pipeline.

When debugging a regression, you can run an individual test (for example `cargo test --package beacon-lsp --test formatting_regression_tests test_type_annotations_basic`) to inspect the formatter output.

## Language Server & Analysis

- `cargo test --package beacon-lsp`
  Runs all LSP providers, static analysis (CFG, data-flow, lint rules), and supporting infrastructure.
  This is the canonical smoke test before opening a pull request.
- `cargo test --workspace`
  Optional full sweep that executes parser, core utilities, and the CLI in addition to the language server crate.

## Guidelines

1. Prefer targeted regression tests for every formatting or analysis bug fix.
2. Keep tests deterministic: avoid timing assumptions or filesystem-global state.
3. If a test case documents a known gap, annotate it with `#[ignore]` and file an issue so it can be tracked explicitly.
4. Mention the exact command you executed when reporting failures; most suites now emit additional context to help diagnose spacing or tokenization issues.

Running the formatter suites plus `cargo test --package beacon-lsp` provides confidence that contributor changes respect the documented behavior across the entire toolchain.
