# CLI Overview

The Beacon CLI provides command-line tools for parsing, type checking, and analyzing Python code using Hindley-Milner type inference.

## Available Commands

### Core Commands

- `parse` - Parse Python files and display the AST
- `highlight` - Syntax highlighting with optional colors
- `check` - Validate Python syntax for parse errors
- `resolve` - Analyze name resolution and display symbol tables
- `format` - Run the Beacon formatter without starting the language server

### Type Checking

- `typecheck` - Perform Hindley-Milner type inference and report type errors

### Language Server

- `lsp` - Start the Beacon Language Server Protocol server

### Debug Tools (Debug Builds Only)

- `debug tree` - Display tree-sitter CST structure
- `debug ast` - Show AST with inferred types
- `debug constraints` - Display generated type constraints
- `debug unify` - Show unification trace

## Installation

Build from source:

```sh
cargo build --release
```

The binary will be available at `target/release/beacon-cli`.

## Basic Usage

All commands accept either a file path or read from stdin:

```sh
# From file
beacon-cli typecheck example.py

# From stdin
cat example.py | beacon-cli typecheck
```

## Getting Help

For detailed help on any command:

```sh
beacon-cli help <command>
```

For the complete list of options:

```sh
beacon-cli --help
```
