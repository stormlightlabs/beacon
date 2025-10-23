# Architecture Overview

The language server lives in `crates/server` and centres on the `Backend` type, which implements `tower_lsp::LanguageServer`. The architecture is deliberately modular so feature work and analyzer development can proceed in parallel.

## Core Components

- **Backend**: receives every LSP request/notification and routes it to feature providers. It owns the shared state required by multiple features.
- **Client (`tower_lsp::Client`)**: handles outbound communication, including diagnostics, logs, and custom notifications.
- **DocumentManager**: thread-safe cache of open documents. Each `Document` stores:
    - Source text (`ropey::Rope` for cheap edits).
    - Tree-sitter parse tree.
    - Beacon AST.
    - Symbol table produced by the name resolver.
- **Analyzer**: the Beacon type checker wrapped in an `Arc<RwLock<_>>` because many features need mutable access to its caches.
- **Workspace**: tracks the workspace root URI and will later manage module resolution and indexing.
- **Features**: a simple struct that instantiates each provider with shared dependencies and exposes them to the backend.

## Concurrency Model

`tower_lsp::LspService` drives the backend on the Tokio runtime.

Read-heavy operations borrow documents or analyzer state immutably; diagnostics and rename take write locks to update caches.

Documents store text in a `ropey::Rope`, so incremental edits only touch the modified spans.

## Error Handling

Feature methods typically return `Option<T>`: `None` means the feature has no answer for the request rather than hard-failing.

When unrecoverable errors occur (e.g., document not found), providers log via the client instead of crashing the server process.

## Extensibility

Adding a new LSP method involves creating a provider (or extending an existing one) and exposing it through the `Features` struct.

Because providers depend only on `DocumentManager` and optionally the analyzer, they are easy to test in isolation.

This architecture keeps protocol plumbing concentrated in the backend while feature logic stays modular and testable.
