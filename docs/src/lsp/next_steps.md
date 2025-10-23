# Next Steps

The following projects are planned to evolve Beacon’s language server from a solid MVP into a full-featured development companion.

## Analyzer Integration

Tighten the connection between the LSP and analyzer so rename, references, and completions can operate across modules.

Cache analyzer results to avoid repeated full reanalysis after every edit.

Surface richer hover information (e.g., inferred types with provenance, docstrings).

## Workspace Indexing

Build a background indexer that scans the workspace root, populating symbol data for unopened files.

Add file watchers to refresh indexes when on-disk files change outside the editor.

Support multi-root workspaces and remote development scenarios.

## Tooling Enhancements

Implement formatting (`textDocument/formatting`, `rangeFormatting`) and integrate with Beacon’s formatting rules.

Deliver concrete code actions (e.g., quick fixes for undefined variables, import suggestions).

Extend semantic tokens with modifier support (documentation, deprecated symbols) and align with editor theming.

## Performance & Reliability

Adopt Tree-sitter’s incremental parsing to reduce reparse costs for large files.

Improve logging and telemetry so users can diagnose performance issues or protocol errors.

Harden handling of unexpected client input, ensuring the server degrades gracefully.

## Documentation & Ecosystem

Publish editor-specific setup guides (VS Code, Neovim, Helix, Zed) alongside troubleshooting tips.

Automate documentation deployment (see `deploy-docs` workflow) and version docs with releases.

Encourage community extensions by documenting provider APIs and expected invariants.
