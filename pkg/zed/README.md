# Beacon Zed Extension

Python language-server extension for Zed that runs `beacon-lsp`.

## Features

- Hindley-Milner type inference with automatic generalization
- Real-time diagnostics for syntax, semantic, and type errors
- Hover, completions, go-to-definition, and find references
- Code actions, symbol navigation, semantic tokens, and inlay hints

[Full feature documentation](https://stormlightlabs.github.io/beacon/)

## Install `beacon-lsp`

The extension expects `beacon-lsp` to be available in Zed's `PATH`:

```sh
# From the repository root
cargo install --path crates/server
```

Verify the binary is discoverable:

```sh
which beacon-lsp
```

## Build the Extension

Rust must be installed through `rustup` for Zed dev extensions.

```sh
rustup target add wasm32-wasip1
cd pkg/zed
cargo build --target wasm32-wasip1 --release
```

The extension wasm is written to:

```text
target/wasm32-wasip1/release/beacon_zed.wasm
```

## Install as a Dev Extension

1. Open Zed's Extensions page.
2. Click **Install Dev Extension** or run `zed: install dev extension`.
3. Select this `pkg/zed` directory.
4. Open a Python file.

For troubleshooting, run `zed: open log`. To see extension stdout/stderr, launch
Zed from a terminal with `zed --foreground`.

## Manifest

Beacon registers only a language server for Zed's built-in Python language:

```toml
[language_servers.beacon-lsp]
name = "Beacon LSP"
languages = ["Python"]
```

It does not ship a Python grammar or bundle the `beacon-lsp` binary.

## References

- [Developing Extensions](https://zed.dev/docs/extensions/developing-extensions)
- [Language Extensions](https://zed.dev/docs/extensions/languages)
- [Extension API](https://docs.rs/zed_extension_api/latest/zed_extension_api/)
