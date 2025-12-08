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

`GotoDefinitionProvider` locates definitions using symbol table lookups and supports cross-file navigation for all symbol types.
The provider resolves imports across the workspace, including wildcard imports, to navigate from usage sites to definitions in other modules.

`GotoTypeDefinitionProvider` navigates from a variable to its type definition, supporting both local and cross-file type resolution.

`GotoImplementationProvider` finds all implementations of abstract methods or protocols across the workspace.

`ReferencesProvider` returns all occurrences of a symbol across open documents.

`DocumentHighlightProvider` highlights all occurrences of a symbol within a single file when the cursor is positioned on it.
The provider walks the AST to identify and classify occurrences:

- Variables: marked as READ or WRITE based on context (assignments are WRITE, usage is READ)
- Function names: highlighted in both definitions and call sites
- Function parameters: highlighted in both the parameter list and within the function body
- Class members: highlighted across the class definition

## Symbols

`DocumentSymbolsProvider` walks the AST to produce hierarchical outlines (classes, functions, variables).

`WorkspaceSymbolsProvider` scans all open documents, performing case-insensitive matching with fuzzy search scoring.
It falls back to sensible defaults when nested symbols are missing from the symbol table.
The provider supports lazy symbol resolution for LSP clients that request location details on-demand.

## Semantic Enhancements

`SemanticTokensProvider` projects syntax nodes into semantic token types and modifiers, enabling advanced highlighting.

`InlayHintsProvider` emits type annotations or other inline hints derived from the analyzer.

## Refactoring

The refactoring system provides multi-file code transformations with precise type inference and module resolution.

`RenameProvider` validates proposed identifiers, gathers edits across all workspace files, deduplicates overlapping ranges, and returns a `WorkspaceEdit`.
Cross-file rename support ensures consistent symbol updates throughout the codebase.

`ExtractFunctionProvider` extracts selected code into a new function, handling:

- Variable capture analysis to determine parameters and return values
- Control flow preservation for early returns and breaks
- Type inference for function signatures
- Precise import management for extracted code

`ExtractVariableProvider` extracts complex expressions into named variables with inferred type annotations.

`InlineFunctionProvider` replaces function calls with the function body, handling:

- Control flow transformations (return statements, conditional logic)
- Expression contexts and side effect analysis
- Variable scoping and name shadowing
- Multiple call site inlining

`ChangeSignatureProvider` modifies function signatures and updates all call sites:

- Add, remove, or reorder parameters
- Update parameter types and default values
- Propagate changes across all references
- Maintain type safety throughout the refactoring

`MoveSymbolProvider` relocates functions, classes, or variables to different modules:

- Updates imports in all affected files
- Preserves type information across module boundaries
- Handles circular import prevention
- Maintains workspace consistency

## Code Actions

`CodeActionsProvider` provides quick fixes and refactoring actions:

**Quick Fixes:**

- Removing unused variables and imports
- Wrapping types with `Optional` for None-related type errors
- Automatically adding `from typing import Optional` when needed
- Adding missing pattern cases in match statements
- Removing unreachable pattern cases
- Implementing missing protocol methods for built-in protocols (Iterable, Iterator, Sized, Callable, Sequence, Mapping)

**Refactorings:**

- Inserting type annotations from inferred types on variable assignments
- Extract to function with automatic parameter inference
- Extract to variable with type annotations
- Inline function calls with control flow handling
- Change function signatures across all call sites
- Move symbols between modules
- Add missing imports for undefined symbols (coming soon!)

## Support Modules

The features system includes specialized support modules:

`builtin_docs` provides embedded documentation for Python built-in types (str, int, list, dict, etc.).
Documentation is loaded from JSON at compile time and includes descriptions, common methods, and links to official Python documentation.

`dunders` supplies metadata and documentation for Python's magic methods (`__init__`, `__str__`, etc.) and builtin variables (`__name__`, `__file__`, etc.).

Adding new features typically means introducing a provider that consumes `DocumentManager`, optionally the analyzer, and wiring it through the `Features` struct so the backend can route requests.
