# Beacon

Beacon is a Rust implementation of the language server protocol & a hindley-milner type
system for Python, inspired by languages like F# & OCaml and the work of Astral.

## Project

```sh
crates/
  cli/              # `beacon-cli` entry point with clap
  server/           # `beacon-lsp` LSP server (tower-lsp or raw) using lsp-types
  core/             # `beacon-core` type definitions, solver, unifier
  constraints/      # `beacon-constraint` constraint generation
  parser/           # `beacon-parser` tree-sitter Python adapter
```
