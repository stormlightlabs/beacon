# Goals And Scope

Beacon’s LSP focuses on delivering a fast, editor-friendly surface for the Beacon analyzer without overcommitting to unfinished infrastructure. The current goals fall into five themes.

## Primary Goals

**Immediate feedback**: run parsing and type analysis on every edit so diagnostics stay in sync with the buffer.

**Core navigation**: support hover, go-to-definition, references, and symbol search for rapid code exploration.

**Authoring assistance**: provide completions, document symbols, inlay hints, and semantic tokens to guide editing.

**Refactoring primitives**: offer reliable rename support and lay the groundwork for richer code actions.

**Modular design**: isolate feature logic behind provider traits so contributors can evolve features independently.

## Out-of-Scope (For Now)

- **Full workspace indexing**: we limit operations to open documents until indexing and cache management mature.
- **Formatting and linting**: formatting endpoints and lint integrations are planned but not part of the initial release.
- **Editor-specific UX**: we stick to LSP-standard capabilities instead of bespoke VS Code UI components.
- **Heavy configuration**: configuration parsing is minimal; user options will be respected in a future milestone.
