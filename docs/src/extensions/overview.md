# Editor Extensions

Beacon provides language server integration for multiple editors through the Language Server Protocol (LSP).
All extensions communicate with the same `beacon-lsp` server, ensuring feature parity across editors.

## Supported Editors

### VS Code

Full-featured extension with settings UI and marketplace distribution (planned).

[VS Code Extension Documentation](./vscode.md)

### Zed

Native WebAssembly extension with TOML-based configuration.

[Zed Extension Documentation](./zed.md)

### Neovim

Native LSP client integration using built-in LSP support.

[Neovim Setup](#neovim-integration) (see below)

### Other LSP-Compatible Editors

Beacon works with any editor supporting the Language Server Protocol. See [Manual Setup](#manual-setup) for configuration.

## Installation

### Prerequisites

All editors require `beacon-lsp` to be installed and available in your PATH:

```bash
# Install from source
cargo install --path crates/server

# Verify installation
which beacon-lsp
```

Ensure `~/.cargo/bin` is in your PATH:

```bash
# Add to ~/.zshrc or ~/.bashrc
export PATH="$HOME/.cargo/bin:$PATH"
```

## Neovim Integration

Neovim has built-in LSP support starting from version 0.5.0. Beacon integrates seamlessly with Neovim's native LSP client.

### Requirements

- Neovim ≥ 0.8.0 (recommended 0.10.0+)
- `beacon-lsp` installed and in PATH
- `nvim-lspconfig` plugin (optional but recommended)

### Setup with nvim-lspconfig

Using [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig):

```lua
-- ~/.config/nvim/lua/plugins/lsp.lua or init.lua

local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Register beacon-lsp if not already registered
if not configs.beacon then
  configs.beacon = {
    default_config = {
      cmd = { 'beacon-lsp' },
      filetypes = { 'python' },
      root_dir = function(fname)
        return lspconfig.util.root_pattern(
          'beacon.toml',
          'pyproject.toml',
          '.git'
        )(fname) or lspconfig.util.path.dirname(fname)
      end,
      settings = {},
      init_options = {
        typeChecking = {
          mode = 'balanced', -- 'strict', 'balanced', or 'loose'
        },
        python = {
          version = '3.12',
          stubPaths = { 'stubs', 'typings' },
        },
        workspace = {
          sourceRoots = {},
          excludePatterns = { '**/venv/**', '**/.venv/**' },
        },
        inlayHints = {
          enable = true,
          variableTypes = true,
          functionReturnTypes = true,
          parameterNames = false,
        },
        diagnostics = {
          unresolvedImports = 'warning',
          circularImports = 'warning',
        },
        advanced = {
          incremental = true,
          workspaceAnalysis = true,
          enableCaching = true,
          cacheSize = 100,
        },
      },
    },
  }
end

-- Setup beacon-lsp
lspconfig.beacon.setup({
  on_attach = function(client, bufnr)
    -- Enable completion
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Keybindings
    local opts = { noremap = true, silent = true, buffer = bufnr }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set('n', '<space>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, opts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<space>f', function()
      vim.lsp.buf.format({ async = true })
    end, opts)

    -- Enable inlay hints (Neovim 0.10+)
    if client.server_capabilities.inlayHintProvider then
      vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
    end
  end,
  capabilities = require('cmp_nvim_lsp').default_capabilities(),
})
```

### Manual Setup (Without nvim-lspconfig)

For minimal configuration without plugins:

```lua
-- ~/.config/nvim/init.lua

vim.api.nvim_create_autocmd('FileType', {
  pattern = 'python',
  callback = function()
    vim.lsp.start({
      name = 'beacon-lsp',
      cmd = { 'beacon-lsp' },
      root_dir = vim.fs.dirname(
        vim.fs.find({ 'beacon.toml', 'pyproject.toml', '.git' }, {
          upward = true,
        })[1]
      ),
      settings = {
        typeChecking = { mode = 'balanced' },
        python = { version = '3.12' },
        inlayHints = { enable = true },
      },
    })
  end,
})

-- Keybindings
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local opts = { buffer = args.buf }
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, opts)
  end,
})
```

### LazyVim Setup

For [LazyVim](https://www.lazyvim.org/) users:

```lua
-- ~/.config/nvim/lua/plugins/beacon.lua

return {
  {
    'neovim/nvim-lspconfig',
    opts = {
      servers = {
        beacon = {
          cmd = { 'beacon-lsp' },
          filetypes = { 'python' },
          root_dir = function(fname)
            local util = require('lspconfig.util')
            return util.root_pattern('beacon.toml', 'pyproject.toml', '.git')(fname)
          end,
          settings = {
            typeChecking = { mode = 'balanced' },
            python = { version = '3.12' },
            inlayHints = {
              enable = true,
              variableTypes = true,
              functionReturnTypes = true,
            },
          },
        },
      },
    },
  },
}
```

### Kickstart.nvim Setup

For [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim) users:

```lua
-- Add to your init.lua servers table

local servers = {
  -- ... other servers
  beacon = {
    cmd = { 'beacon-lsp' },
    filetypes = { 'python' },
    settings = {
      typeChecking = { mode = 'balanced' },
      python = { version = '3.12' },
    },
  },
}
```

### Completion Support

Beacon works with popular completion plugins:

#### nvim-cmp

```lua
-- ~/.config/nvim/lua/plugins/completion.lua

local cmp = require('cmp')
local lspconfig = require('lspconfig')

lspconfig.beacon.setup({
  capabilities = require('cmp_nvim_lsp').default_capabilities(),
})

cmp.setup({
  sources = {
    { name = 'nvim_lsp' },
    { name = 'buffer' },
    { name = 'path' },
  },
})
```

#### coq_nvim

```lua
local coq = require('coq')
lspconfig.beacon.setup(coq.lsp_ensure_capabilities())
```

### Diagnostics Configuration

Customize diagnostic display:

```lua
-- Configure diagnostics display
vim.diagnostic.config({
  virtual_text = {
    prefix = '●',
    source = 'beacon',
  },
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
})

-- Custom diagnostic signs
local signs = { Error = '✘', Warn = '⚠', Hint = '󰌶', Info = 'ℹ' }
for type, icon in pairs(signs) do
  local hl = 'DiagnosticSign' .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end
```

### Inlay Hints

Enable inlay hints (Neovim 0.10+):

```lua
-- Enable inlay hints globally
vim.lsp.inlay_hint.enable(true)

-- Toggle inlay hints with a keybinding
vim.keymap.set('n', '<leader>th', function()
  vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
end, { desc = 'Toggle Inlay Hints' })
```

### Workspace Configuration

Override settings per project using `beacon.toml` in your project root.

See [Configuration Documentation](../configuration.md) for complete details on all available options and TOML structure.

## Manual Setup

For editors not listed above, configure your LSP client to:

1. **Command**: `beacon-lsp`
2. **File Types**: `python`
3. **Root Patterns**: `beacon.toml`, `pyproject.toml`, `.git`
4. **Communication**: stdio (stdin/stdout)

### Example Configuration

```json
{
    "command": "beacon-lsp",
    "filetypes": ["python"],
    "rootPatterns": ["beacon.toml", "pyproject.toml", ".git"],
    "settings": {
        "typeChecking": { "mode": "balanced" },
        "python": { "version": "3.12" }
    }
}
```

## Feature Comparison

| Feature            | VS Code | Zed | Neovim    | Other |
| ------------------ | ------- | --- | --------- | ----- |
| Diagnostics        | ✓       | ✓   | ✓         | ✓     |
| Hover              | ✓       | ✓   | ✓         | ✓     |
| Completions        | ✓       | ✓   | ✓         | ✓     |
| Go to Definition   | ✓       | ✓   | ✓         | ✓     |
| Find References    | ✓       | ✓   | ✓         | ✓     |
| Document Symbols   | ✓       | ✓   | ✓         | ✓     |
| Workspace Symbols  | ✓       | ✓   | ✓         | ✓     |
| Semantic Tokens    | ✓       | ✓   | ✓         | ✓     |
| Inlay Hints        | ✓       | ✓   | ✓ (0.10+) | ✓     |
| Code Actions       | ✓       | ✓   | ✓         | ✓     |
| Rename             | ✓       | ✓   | ✓         | ✓     |
| Folding Ranges     | ✓       | ✓   | ✓         | ✓     |
| Document Highlight | ✓       | ✓   | ✓         | ✓     |
| Signature Help     | ✓       | ✓   | ✓         | ✓     |
| Settings UI        | ✓       | -   | -         | -     |
| Marketplace        | Planned | -   | -         | -     |

All editors share the same language server, ensuring consistent behavior and feature parity.

## Configuration

See [Configuration Documentation](../configuration.md) for complete details.

## Resources

- [Language Server Protocol Specification](https://microsoft.github.io/language-server-protocol/)
- [Beacon Configuration](../configuration.md)
- [beacon-lsp Architecture](../lsp/architecture.md)
- [Neovim LSP Documentation](https://neovim.io/doc/user/lsp.html)
- [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)
