# Feature Providers

Each capability exposed by the language server lives in its own provider under `crates/server/src/features`.
Providers share the `DocumentManager` and, when needed, the analyzer.

## Diagnostics

`DiagnosticProvider` aggregates:

- Parse errors emitted by the parser.
- Unbound variable checks.
- Type errors and warnings from the analyzer.
- Additional semantic warnings (e.g., annotation mismatches).

Results are published with document versions to prevent stale diagnostics in the editor.

## Hover

`HoverProvider` returns context-sensitive information for the symbol under the cursor—typically inferred types or documentation snippets.
It reads the current AST and analyzer output to assemble `Hover` responses.

The hover system integrates with the builtin documentation and dunder metadata modules to provide rich information for Python's standard types and magic methods.

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

`RenameProvider` validates proposed identifiers, gathers edits via both AST traversal and Tree-sitter scans, deduplicates overlapping ranges, and returns a `WorkspaceEdit`.

## Code Actions

`CodeActionsProvider` provides quick fixes for common issues:

- Removing unused variables and imports
- Wrapping types with `Optional` for None-related type errors
- Automatically adding `from typing import Optional` when needed
- Insert type annotations from inference (coming soon!)
- Add missing imports for undefined symbols (coming soon!)
- Implement missing protocol methods (coming soon!)
- Extract to function/method refactorings (coming soon!)
- Inline variable refactorings (coming soon!)

## Support Modules

The features system includes specialized support modules:

`builtin_docs` provides embedded documentation for Python built-in types (str, int, list, dict, etc.).
Documentation is loaded from JSON at compile time and includes descriptions, common methods, and links to official Python documentation.

`dunders` supplies metadata and documentation for Python's magic methods (`__init__`, `__str__`, etc.) and builtin variables (`__name__`, `__file__`, etc.).

Adding new features typically means introducing a provider that consumes `DocumentManager`, optionally the analyzer, and wiring it through the `Features` struct so the backend can route requests.
