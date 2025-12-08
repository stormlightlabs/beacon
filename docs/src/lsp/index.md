# Beacon Language Server

Beacon's Language Server Protocol (LSP) implementation bridges the Rust-based analyzer with editors such as Zed, VSCode/VSCodium, Neovim, and Helix. This chapter documents the system from high-level goals to feature-by-feature behaviour.

## LSP Capabilities Quick Reference

Beacon implements the following LSP features:

- Diagnostics: Real-time syntax, semantic, and type error reporting
- Hover: Context-sensitive type information and documentation
- Completion: Symbol table-based completions
- Navigation: Go to definition, type definition, implementation (cross-file), find references, document highlights
- Symbols: Document outline and workspace fuzzy search
- Semantic tokens and inlay hints
- Refactoring: Rename (cross-file), extract function/variable, inline function, change signature, move symbol, code actions, quick fixes

See [Feature Providers](./feature_providers.md) for detailed implementation.

## Documentation Overview

Use the sidebar to jump into any topic, or start with the sections below:

- [Goals And Scope](./goals.md) - what the server delivers today and what is intentionally out of scope.
- [Architecture Overview](./architecture.md) - how shared state, concurrency, and feature wiring are structured.
- [Document Pipeline](./document_pipeline.md) - how file contents become parse trees, ASTs, and symbol tables.
- [Caching](./caching.md) - multi-layer cache architecture and invalidation strategies for fast incremental updates.
- [Feature Providers](./feature_providers.md) - the capabilities exposed via LSP requests and notifications.
- [Request Lifecycles](./request_lifecycles.md) - end-to-end flows for initialization, diagnostics, completions, and more.
- [Workspace Services](./workspace_services.md) - cross-file features and emerging workspace indexing plans.
- [Testing Strategy](./testing.md) - automated coverage for providers and backend flows.
- [Current Limitations](./limitations.md) - known gaps and trade-offs in the current implementation.
- [Next Steps](./next_steps.md) - near-term improvements on the roadmap.

If you are new to the Language Server Protocol itself, read the primer in [Learn → Language Server Protocol](../learn/lsp_overview.md) before diving into these implementation details.
