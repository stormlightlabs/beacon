# Current Limitations

The Beacon language server already covers core workflows but still has notable constraints. Understanding these limitations helps set expectations for contributors and users.

## Open-Document Focus

Navigation features (goto definition, type definition, implementation) and refactoring operations (rename, extract, inline, move) now support cross-file operations.

However, some features still focus on open documents. Comprehensive workspace-wide analysis for all features is ongoing.

## Analyzer Coupling

Rename and references rely on a mix of AST traversal and simple heuristics; deep semantic queries across modules are not yet available.

Analyzer caches are invalidated wholesale after edits. Incremental typing work is on the roadmap but not implemented.

## Performance

Tree-sitter reparses the entire document per change. While acceptable for small files, large modules may benefit from incremental parsing.

Workspace symbol searches iterate synchronously over all open documents, which can lag in large sessions.

## Feature Gaps

Code actions support quick fixes and several refactorings (extract function/variable, inline function, change signature, move symbol).
Additional refactorings like automatic import suggestions remain unimplemented.

Formatting endpoints (`textDocument/formatting`, etc.) are unimplemented.

Configuration (`Config`) is still a stub and does not honor user settings.

## Tooling Ergonomics

Error messages from the analyzer can be terse; improving diagnostics and logs is part of future work.

There is no persistence of analysis results across sessions, so large projects require recomputation on startup.
