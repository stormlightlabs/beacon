# Document Pipeline

The document pipeline keeps Beacon’s view of each open file synchronized with the editor. `DocumentManager` orchestrates the lifecycle and ensures every feature works from the same parse tree, AST, and symbol table.

## Lifecycle Events

1. **Open** (`textDocument/didOpen`)
   - Create a `Document` with the initial text, version, and URI.
   - Parse immediately via `LspParser` to populate the parse tree, AST, and symbol table.
   - Insert the document into the manager’s map.
2. **Change** (`textDocument/didChange`)
   - Apply full or incremental edits to the document’s rope.
   - Re-run the parser to refresh derived data.
   - Invalidate analyzer caches so diagnostics and semantic queries recompute with fresh information.
3. **Save** (`textDocument/didSave`)
   - Trigger diagnostics for the new persisted content. Behaviour matches the change handler today.
4. **Close** (`textDocument/didClose`)
   - Remove the document and send an empty diagnostics array to clear markers in the editor.

## Data Stored per Document

**Text**: stored as a `ropey::Rope` for efficient splicing.

**Parse tree**: Tree-sitter syntax tree produced by the parser.

**AST**: Beacon’s simplified abstract syntax tree used by features and the analyzer.

**Symbol table**: scope-aware mapping created during name resolution.

**Version**: latest client-supplied document version, echoed back when publishing diagnostics.

## Access Patterns

`get_document`: exposes an immutable snapshot to consumers like hover or completion.

`get_document_mut`: allows controlled mutation when necessary (rare in practice).

`all_documents`: lists URIs so workspace-level features can iterate through open files.

By centralizing parsing and symbol management, the pipeline guarantees consistent snapshots across diagnostics, navigation, and refactoring features.
