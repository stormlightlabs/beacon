# Beacon Zed Extension

Python language server extension for Zed that provides Hindley-Milner type inference.

## Installation

### Install beacon-lsp

The extension requires `beacon-lsp` to be available in your PATH:

```sh
# From the project root
cargo install --path crates/server
```

This installs `beacon-lsp` to `~/.cargo/bin`.

### Build and Install Extension

```sh
# Build the extension
cargo build --target wasm32-wasip1 --release

# The extension wasm is at:
# target/wasm32-wasip1/release/beacon_zed.wasm
```

Install the extension following [Zed's extension installation guide](https://zed.dev/docs/extensions/installing-extensions).

## Development

See Zed's docs on [developing extensions](https://zed.dev/docs/extensions/developing-extensions):

- [Language Extension](https://zed.dev/docs/extensions/languages)
- [Capabilities](https://zed.dev/docs/extensions/capabilities)
- [API](https://docs.rs/zed_extension_api/latest/zed_extension_api/)
