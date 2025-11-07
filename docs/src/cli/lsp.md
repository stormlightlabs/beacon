# Language Server

The `lsp` command starts the Beacon Language Server Protocol server for editor integration.

## Usage

```sh
beacon-cli lsp [OPTIONS]
```

## Options

- `--tcp <PORT>` - Use TCP on the specified port (TODO: not yet implemented)
- `--log-file <PATH>` - Write logs to the specified file

## Communication Modes

### stdio (Default)

The default mode uses standard input/output for LSP communication. This is the standard mode for editor integration:

```sh
beacon-cli lsp
```

Editors spawn the LSP server and communicate via pipes. This is automatically configured by editor plugins.

### TCP Mode (TODO)

TCP mode allows remote LSP connections and easier debugging:

```sh
beacon-cli lsp --tcp 9257
```

## Logging

### stderr (Default)

By default, logs are written to stderr:

```sh
beacon-cli lsp 2> lsp.log
```

### File Logging

Use the `--log-file` option to write logs to a specific file:

```sh
beacon-cli lsp --log-file /tmp/beacon-lsp.log
```

The log file is created if it doesn't exist and appended to if it does.

## Environment Variables

Control log level via the `RUST_LOG` environment variable:

```sh
# Info level (default)
RUST_LOG=info beacon-cli lsp

# Debug level for verbose logging
RUST_LOG=debug beacon-cli lsp

# Trace level for very verbose logging
RUST_LOG=trace beacon-cli lsp
```

## Editor Integration

### VS Code

The Beacon VS Code extension automatically spawns the LSP server. No manual configuration needed.

### Neovim

Configure nvim-lspconfig:

```lua
require'lspconfig'.beacon.setup{
  cmd = { "beacon-cli", "lsp" },
  filetypes = { "python" },
  root_dir = function(fname)
    return vim.fn.getcwd()
  end,
}
```

### Emacs (lsp-mode)

Add to your configuration:

```elisp
(add-to-list 'lsp-language-id-configuration '(python-mode . "python"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("beacon-cli" "lsp"))
                  :major-modes '(python-mode)
                  :server-id 'beacon))
```

## LSP Features

The Beacon LSP server provides:

- Full type inference (Hindley-Milner)
- Hover information with inferred types
- Go to definition
- Find references
- Document/workspace symbols
- Semantic tokens
- Inlay hints (type annotations)
- Code actions
- Diagnostics (type errors)
- Auto-completion

See the [LSP documentation](../lsp/) for detailed feature descriptions.
