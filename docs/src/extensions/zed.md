# Zed Extension

The Beacon Zed extension (`pkg/zed/`) connects Zed's built-in Python language
support to the `beacon-lsp` language server. It provides Beacon diagnostics and
LSP features while relying on Zed's existing Python grammar and language metadata.

## Feature Highlights

- On-type diagnostics for syntax, semantic, and type errors
- Hover tooltips with type information
- Go to definition and find references
- Document and workspace symbols
- Semantic tokens and inlay hints when enabled in Zed
- Identifier completions and code actions

These capabilities mirror the features exposed by the Rust server in
`crates/server`.

## Repository Layout

```sh
pkg/zed/
├── src/
│   └── lib.rs       # Extension implementation
├── Cargo.toml       # Rust project manifest
├── extension.toml   # Zed extension metadata
├── LICENSE          # Extension publishing license
└── README.md        # Installation instructions
```

The extension compiles to WebAssembly (`wasm32-wasip1`) and launches the
`beacon-lsp` binary through the Language Server Protocol.

## Manifest Notes

`extension.toml` follows the current Zed language-server manifest shape:

```toml
[language_servers.beacon-lsp]
name = "Beacon LSP"
languages = ["Python"]
```

Beacon does not ship a Python Tree-sitter grammar. Zed already provides Python
language metadata, so the extension only registers the `beacon-lsp` language
server for the `Python` language name.

## Prerequisites

- Rust installed via `rustup`
- `wasm32-wasip1` target (`rustup target add wasm32-wasip1`)
- `beacon-lsp` installed and available in `PATH`
- Zed editor

Install the language server from the repository root:

```sh
cargo install --path crates/server
```

Verify that Zed will be able to find it:

```sh
which beacon-lsp
```

## Building the Extension

```sh
cd pkg/zed
cargo build --target wasm32-wasip1 --release
```

The compiled extension is written to:

```text
target/wasm32-wasip1/release/beacon_zed.wasm
```

## Installing a Dev Extension

Zed no longer requires manually symlinking local extensions. For local testing:

1. Build the extension.
2. Open Zed's Extensions page.
3. Choose **Install Dev Extension** or run `zed: install dev extension` from the
   command palette.
4. Select the `pkg/zed` directory.
5. Open a Python file to activate Beacon.

If a published Beacon extension is installed later, Zed will show that it is
"Overridden by dev extension" until the dev extension is removed.

## Debugging

Use `zed: open log` to inspect `Zed.log`. For extension `println!` or `dbg!`
output and more verbose startup logs, launch Zed from a terminal:

```sh
zed --foreground
```

The extension starts `beacon-lsp` with `RUST_LOG=info`. To increase server-side
logging while testing, update `get_env_for_language_server` in
`pkg/zed/src/lib.rs` and rebuild the extension.

## Publishing Checklist

Zed publishes extensions through PRs to
[`zed-industries/extensions`](https://github.com/zed-industries/extensions).
Before publishing or updating Beacon:

- Keep `extension.toml`'s `version` in sync with the extension registry entry.
- Use the extension ID `beacon`; Zed's publishing rules disallow `zed` or
  `extension` in extension IDs and names.
- Keep a valid accepted license at `pkg/zed/LICENSE` because this extension lives
  in a subdirectory of the repository.
- Do not bundle `beacon-lsp` into the extension. Zed requires language-server
  extensions to find or download external servers instead.
- Test the extension locally with **Install Dev Extension** before opening the
  publishing PR.

## Resources

- [Zed Extension Development](https://zed.dev/docs/extensions/developing-extensions)
- [Zed Language Extensions](https://zed.dev/docs/extensions/languages)
- [Zed Extension API](https://docs.rs/zed_extension_api/latest/zed_extension_api/)
- [Beacon Configuration](../configuration.md)
