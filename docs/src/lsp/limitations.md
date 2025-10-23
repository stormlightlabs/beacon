# Current Limitations

The Beacon language server already covers core workflows but still has notable constraints. Understanding these limitations helps set expectations for contributors and users.

## Open-Document Focus

Most features only inspect documents currently open in the editor.

Closed files are invisible until workspace indexing is implemented, so cross-project references or renames may miss targets.

## Analyzer Coupling

Rename and references rely on a mix of AST traversal and simple heuristics; deep semantic queries across modules are not yet available.

Analyzer caches are invalidated wholesale after edits. Incremental typing work is on the roadmap but not implemented.

## Performance

Tree-sitter reparses the entire document per change. While acceptable for small files, large modules may benefit from incremental parsing.

Workspace symbol searches iterate synchronously over all open documents, which can lag in large sessions.

## Feature Gaps

Code actions return placeholders; no concrete quick fixes ship yet.

Formatting endpoints (`textDocument/formatting`, etc.) are unimplemented.

Configuration (`Config`) is still a stub and does not honour user settings.

## Tooling Ergonomics

Error messages from the analyzer can be terse; improving diagnostics and logs is part of future work.

There is no persistence of analysis results across sessions, so large projects require recomputation on startup.
