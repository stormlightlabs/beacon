# Workspace Services

While most features operate on individual documents, Beacon’s language server already supports several cross-file capabilities and is laying groundwork for broader workspace awareness.

## Workspace Symbols

Iterates over URIs retrieved from `DocumentManager::all_documents`.

For each document, fetches the AST and symbol table, then performs case-insensitive matching against the query string.

Returns `SymbolInformation` with ranges, optional container names, and deprecation tags (`SymbolTag::DEPRECATED` where applicable).

Falls back to reasonable defaults when nested symbols (e.g., class methods) are missing from the symbol table.

## Document Symbols

Provides structured outlines per file, organising classes, functions, assignments, and nested items.

Editors use the resulting tree to populate outline panes, breadcrumbs, or navigation search.

## Workspace State

- The `Workspace` struct records the `root_uri` supplied during initialization.
- Future enhancements will:
    - Crawl the filesystem to discover modules outside the current editor session.
    - Populate caches for unopened files, enabling cross-file references and renames.
    - Track configuration and environment settings at the workspace level.

## Notifications and Logging

The backend emits `window/logMessage` notifications for status updates and `window/showMessage` for user-facing alerts.

Diagnostics are republished after changes so editors update their inline markers and problems panels.

## Long-Term Plans

Implement persistent symbol indexing keyed by the workspace root.

Add background tasks that refresh indexes when files change on disk.

Support multi-root workspaces and remote filesystems where applicable.

Although the current implementation focuses on open buffers, the architecture is designed to scale to full-project workflows as these enhancements land.
