# Feature Providers

Each capability exposed by the language server lives in its own provider under `crates/server/src/features`. Providers share the `DocumentManager` and, when needed, the analyzer. This modular design keeps logic focused and testable.

## Diagnostics

`DiagnosticProvider` aggregates:

- Parse errors emitted by the parser.
- Unbound variable checks.
- Type errors and warnings from the analyzer.
- Additional semantic warnings (e.g., annotation mismatches).

Results are published with document versions to prevent stale diagnostics in the editor.

## Hover

`HoverProvider` returns context-sensitive information for the symbol under the cursor—typically inferred types or documentation snippets. It reads the current AST and analyzer output to assemble `Hover` responses.

## Completion

`CompletionProvider` uses symbol tables to surface in-scope identifiers. Trigger characters (currently `"."`) allow editors to request completions proactively.

## Navigation

`GotoDefinitionProvider` locates definitions using symbol table lookups.

`ReferencesProvider` returns all occurrences of a symbol across open documents.

`DocumentHighlightProvider` highlights occurrences within a single file.

## Symbols

`DocumentSymbolsProvider` walks the AST to produce hierarchical outlines (classes, functions, variables).

`WorkspaceSymbolsProvider` scans all open documents, performing case-insensitive matching. It falls back to sensible defaults when nested symbols are missing from the symbol table.

## Semantic Enhancements

`SemanticTokensProvider` projects syntax nodes into semantic token types and modifiers, enabling advanced highlighting.

`InlayHintsProvider` emits type annotations or other inline hints derived from the analyzer.

## Refactoring

`RenameProvider` validates proposed identifiers, gathers edits via both AST traversal and Tree-sitter scans, deduplicates overlapping ranges, and returns a `WorkspaceEdit`. Future refinements will integrate deeper analyzer data for cross-file renames.

## Code Actions

`CodeActionsProvider` is scaffolded for quick fixes. It currently returns empty results until specific code actions are implemented.

Adding new features typically means introducing a provider that consumes `DocumentManager`, optionally the analyzer, and wiring it through the `Features` struct so the backend can route requests.
