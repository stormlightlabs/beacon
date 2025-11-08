# Zed Extension

The Beacon Zed extension (`pkg/zed/`) integrates the Beacon language server with Zed editor.
It activates automatically for Python files and provides Hindley-Milner type checking alongside standard LSP features.

## Feature Highlights

- On-type diagnostics for syntax and type errors
- Hover tooltips with type information
- Go to definition & find references
- Document and workspace symbols
- Semantic tokens for enhanced highlighting
- Identifier completions and inlay hints
- Code actions for quick fixes and refactoring

These capabilities mirror the features exposed by the Rust server in `crates/server`.

## Repository Layout

```sh
pkg/zed/
├── src/
│   └── lib.rs       # Extension implementation
├── Cargo.toml       # Rust project manifest
├── extension.toml   # Zed extension metadata
└── README.md        # Installation instructions
```

The extension is compiled to WebAssembly (wasm32-wasip1) and communicates with the beacon-lsp binary via the Language Server Protocol.

## Prerequisites

- **Rust toolchain** (stable) with `cargo` available in `PATH`
- **wasm32-wasip1 target** for Rust (install with `rustup target add wasm32-wasip1`)
- **beacon-lsp binary** installed and available in `PATH`
- **Zed editor** installed

## Installing beacon-lsp

The extension requires `beacon-lsp` to be available in your system PATH:

```bash
# From the repository root
cargo install --path crates/server
```

This installs the `beacon-lsp` binary to `~/.cargo/bin`. Ensure `~/.cargo/bin` is in your PATH.

Verify installation:

```bash
which beacon-lsp
# Should output: /Users/<username>/.cargo/bin/beacon-lsp
```

## Building The Extension

The extension must be compiled to WebAssembly:

```bash
cd pkg/zed
cargo build --target wasm32-wasip1 --release
```

The compiled extension will be at:

```text
target/wasm32-wasip1/release/beacon_zed.wasm
```

## Installing The Extension

### Development Installation

For local development and testing:

1. Build the extension (see above)
2. Create a symlink to the extension directory in Zed's extensions folder:

    ```sh
    # macOS
    mkdir -p ~/.config/zed/extensions
    ln -s /path/to/beacon/pkg/zed ~/.config/zed/extensions/beacon
    ```

3. Restart Zed or reload the window
4. Open a Python file to activate the extension

### Distribution Installation

To distribute the extension, package it following [Zed's extension installation guide](https://zed.dev/docs/extensions/installing-extensions).

The extension expects `beacon-lsp` to be available in the user's PATH. Users should install it via:

```bash
cargo install beacon-lsp
```

## Extension Implementation

The extension implements the `zed::Extension` trait with the following key components:

### Language Server Command

Returns the command to launch beacon-lsp:

```rust
fn language_server_command(
    &mut self, _: &zed::LanguageServerId, worktree: &zed::Worktree) -> zed::Result<zed::Command> {
    let command = worktree
        .which("beacon-lsp")
        .ok_or_else(|| "beacon-lsp not found in PATH")?;

    Ok(zed::Command {
        command,
        args: vec![],
        env: vec![("RUST_LOG".to_string(), "info".to_string())],
    })
}
```

### Environment Variables

The extension sets `RUST_LOG=info` to configure logging. Logs are written to stderr and can be viewed in Zed's log panel.

### Arguments

beacon-lsp doesn't require command-line arguments as it communicates via stdin/stdout.

## Configuration

See [Configuration](../configuration.md)for details.

## Development Workflow

### Making Changes

1. Edit the extension source in `pkg/zed/src/lib.rs`
2. Rebuild the extension:

    ```bash
    cargo build --target wasm32-wasip1 --release
    ```

3. Restart Zed to load the updated extension

### Debugging

Enable detailed logging:

```bash
RUST_LOG=beacon_lsp=debug zed
```

Or set the environment variable in your shell before launching Zed. Logs appear in:

- **macOS**: `~/Library/Logs/Zed/Zed.log`
- **Linux**: `~/.local/share/zed/logs/Zed.log`

### Testing Changes

1. Build the language server with your changes:

    ```bash
    cargo build -p beacon-lsp
    cargo install --path crates/server
    ```

2. Rebuild the extension if needed
3. Open a Python project in Zed
4. Test LSP features:
    - Hover over variables to see type information
    - Use Cmd+Click (macOS) or Ctrl+Click (Linux) for go-to-definition
    - Check the Problems panel for diagnostics
    - Trigger completions with Ctrl+Space

## Comparison with VS Code Extension

| Feature         | Zed                 | VS Code               |
| --------------- | ------------------- | --------------------- |
| Installation    | Manual build + PATH | Marketplace (planned) |
| Configuration   | TOML files          | VS Code settings UI   |
| Debugging       | Log files           | Output panel          |
| Language Server | Shared (beacon-lsp) | Shared (beacon-lsp)   |
| Features        | Full LSP support    | Full LSP support      |
| Platform        | macOS, Linux        | macOS, Linux, Windows |

Both extensions use the same beacon-lsp server, so feature parity is guaranteed.

## Resources

- [Zed Extension Development](https://zed.dev/docs/extensions/developing-extensions)
- [Zed Language Extensions](https://zed.dev/docs/extensions/languages)
- [Zed Extension API](https://docs.rs/zed_extension_api/latest/zed_extension_api/)
- [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
- [Beacon Configuration](../configuration.md)
