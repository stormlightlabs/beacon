# Language Server Protocol

## Why LSP Exists

Before LSP, editor integrations for language tooling (completion, diagnostics, refactors) were bespoke. Every compiler or analyzer needed plug-ins for VS Code, Vim, IntelliJ, Sublime, etc., and each editor duplicated work to support many languages. This matrix of per-language, per-editor plug-ins slowed innovation and made advanced tooling inaccessible outside first-party IDEs.

The Language Server Protocol—initiated by Microsoft for VS Code and now standardized by the Open Source community—solves this coupling. It defines a JSON-RPC protocol so a single language server can speak to any compliant editor. Editors implement the client half once and gain tooling support for every language that implements the server half.

## Problems It Solves

- **Shared investment**: Language teams implement the protocol once instead of maintaining multiple editor-specific plug-ins.
- **Editor freedom**: Developers choose tools without sacrificing language-aware features.
- **Feature parity**: Diagnostics, go-to-definition, workspace symbols, rename, and more behave consistently across environments.
- **Incremental updates**: The protocol is designed for streaming updates as the user types, enabling responsive experiences.

## How LSP Works

1. **Transport**: Client and server communicate over stdin/stdout pipes, TCP, or WebSockets. Messages use JSON-RPC 2.0 framed with `Content-Length` headers.
2. **Initialization**: Client sends `initialize` with capabilities and workspace metadata. Server responds with supported features (`ServerCapabilities`). A follow-up `initialized` notification signals readiness.
3. **Document Synchronization**: The client streams document lifecycle notifications (`didOpen`, `didChange`, `didSave`, `didClose`) so the server maintains up-to-date views of open files.
4. **Feature Requests**: Once documents are synchronized, the client issues requests such as:
   - `textDocument/completion` for completion items.
   - `textDocument/hover` for inline info.
   - `textDocument/definition` and `textDocument/references` for navigation.
   - `textDocument/documentSymbol` and `workspace/symbol` for structure searches.
   - `textDocument/codeAction`, `textDocument/rename`, `textDocument/semanticTokens`, and more.
5. **Responses and Notifications**: Servers send responses with payloads defined in the protocol. They can also push diagnostics (`textDocument/publishDiagnostics`) or log messages asynchronously.
6. **Shutdown**: Clients request graceful shutdown via `shutdown` followed by `exit`.

The protocol evolves through versioned specifications (currently 3.x). Beacon targets the subset required for an ergonomic Python workflow, while keeping the implementation modular so new methods can be added as needed.
