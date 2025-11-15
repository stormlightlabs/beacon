# Beacon Language Server

Beacon’s Language Server Protocol (LSP) implementation bridges the Rust-based analyzer with editors such as Zed, VSCode/VSCodium, Neovim, and Helix. This chapter documents the system from high-level goals to feature-by-feature behaviour.

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
