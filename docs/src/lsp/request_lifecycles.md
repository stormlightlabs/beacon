# Request Lifecycles

This section traces how the server handles key LSP interactions from start to finish.

## Initialization

1. **`initialize` request**
   - Captures the workspace root (`root_uri`) from the client.
   - Builds `ServerCapabilities`, advertising supported features: incremental sync, hover, completion, definitions, references, highlights, code actions, inlay hints, semantic tokens (full & range), document/workspace symbols, rename, and workspace symbol resolve.
   - Returns `InitializeResult` with optional `ServerInfo`.
2. **`initialized` notification**
   - Currently logs an info message. Future work will kick off workspace scanning or indexing.

## Text Synchronization & Diagnostics

`didOpen` → store the document, parse it, and call `publish_diagnostics`.

`didChange` → apply edits, reparse, invalidate analyzer caches, then re-run diagnostics.

`didSave` → trigger diagnostics again; behaviour matches the change handler.

`didClose` → remove the document and publish empty diagnostics to clear markers.

`publish_diagnostics` collects issues via `DiagnosticProvider`, tagging them with the current document version to avoid race conditions.

## Hover, Completion, and Navigation

`hover` → query `HoverProvider`, which reads the AST and analyzer to produce `Hover` content.

`completion` → call `CompletionProvider`, returning a `CompletionResponse` (list or completion list).

`gotoDefinition`, `typeDefinition`, `references`, `documentHighlight` → use symbol table lookups to answer navigation requests.

These operations are pure reads when possible, avoiding locks beyond short-lived document snapshots.

## Symbols

`documentSymbol` → returns either `DocumentSymbol` trees or `SymbolInformation` lists.

`workspace/symbol` → aggregates symbols from every open document, performing case-insensitive matching.

`workspaceSymbol/resolve` → currently a no-op passthrough

## Semantic Tokens & Inlay Hints

`textDocument/semanticTokens/full` and `/range` → run the semantic tokens provider to emit delta-encoded token sequences for supported types/modifiers.

`textDocument/inlayHint` → acquire a write lock on the analyzer and compute inline hints for the requested range.

## Refactoring

`textDocument/rename` → validate the new identifier, locate the target symbol, collect edits (AST traversal + Tree-sitter identifiers), deduplicate, and return a `WorkspaceEdit`.

`textDocument/codeAction` → placeholder; currently returns an empty list until specific actions are implemented.

## Shutdown

`shutdown` returns `Ok(())`, signalling graceful teardown.

`exit` follows to terminate the process. We do not persist state yet, so shutdown is effectively stateless.
