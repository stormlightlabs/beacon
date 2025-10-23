# Testing Strategy

Beacon’s LSP crate includes both unit tests and async integration tests to ensure feature behaviour remains stable as the analyzer evolves.

## Provider Unit Tests

Each feature module embeds targeted tests that construct in-memory documents via `DocumentManager::new()`.

Common scenarios include rename edits across nested scopes, workspace symbol searches, and diagnostic generation for simple errors.

Because providers operate on real ASTs and symbol tables, these tests exercise production logic without needing a running language server.

## Backend Integration Tests

Async tests spin up an in-process `tower_lsp::LspService<Backend>` to simulate client interactions.

They call methods like `initialize`, `did_open`, `did_change`, `hover`, and `completion`, asserting that responses match expectations and no panics occur.

This pattern verifies protocol wiring, capability registration, and shared state management without external tooling.

## Command-line Checks

`cargo check` and `cargo check --tests` are run frequently for quick feedback.

`cargo fmt --check` enforces formatting consistency across Rust code.

Documentation changes are validated with `mdbook build docs` to catch broken links or syntax errors.
