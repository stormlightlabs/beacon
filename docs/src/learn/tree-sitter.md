# Tree-sitter

This document contains notes I've compiled based on learnings about [tree-sitter](https://github.com/tree-sitter/tree-sitter).

Tree-sitter is both a **parser-generator tool** and an **incremental parsing library**[^1].
It’s optimized for embedding in editors and tooling (rather than being only a compiler backend parser).
It supports many languages, with language-specific grammars[^2].

From the official site:

> Tree-sitter is a parser generator tool and an incremental parsing library.
> It can build a concrete syntax tree for a source file and efficiently update the syntax tree as the source file is edited.

## What problems it solves

Here are its key value-propositions and the issues it addresses:

### Better than regex/highlight hacks

Traditional editors often use regular expressions or ad-hoc syntax rules for things like syntax highlighting, folding, code navigation.
These approaches tend to fail with complex nested constructs or incomplete code (common in live editing).
Tree-sitter uses a proper parse tree (Concrete Syntax Tree) rather than purely regex heuristics, giving more accurate structure.

### Incremental parsing / live editing

In an editor context, users are typing and modifying files constantly. Re-parsing the entire file on every keystroke is expensive and slow.
Tree-sitter supports **incremental parsing**, meaning it updates only the changed portion of the tree rather than rebuilding everything.
This means edits are reflected quickly and the tree remains coherent, which enables features like structured selection, live syntax highlighting, etc.

### Unified API / language-agnostic tooling

Because each language has a Tree-sitter grammar, you can build tooling (highlighting, navigation, refactoring) in a language-agnostic way: query the tree, capture nodes of interest, etc.
This reduces duplication of effort: editor vendors don’t have to write custom parsing logic per language to support advanced features.

### Error-tolerant parsing for editing

Since code is often incomplete/invalid in the middle of editing, a robust parser needs to recover gracefully.
Tree-sitter is designed to continue to provide a usable tree under such conditions so editors can rely on the tree structure even when the file is only partially valid.

### Enables richer editor tooling

Because you have a full tree, you can support advanced features: structural selection (e.g., select "function" or "if block"), code folding by AST node, refactorings, cross-language injections (e.g., embedded languages).
For example, using queries you can capture specific nodes in the tree and apply tooling logic.

## Internals

### Grammar / Parser Generation

For each language you want support for, you write a **grammar file**, typically `grammar.js` (or some variant) describing the language’s syntax in a DSL provided by Tree-sitter.
Example: You describe rules like `sum: ...`, `product: ...`, define precedence, associativity (via helpers like `prec.left`, `prec.right`).
You then run the Tree-sitter CLI (or build process) to **generate** a `parser.c` file (and possibly `scanner.c`) that formalizes the grammar into C code.
That generated parser becomes the actual runtime component for that language.

### Lexer/Tokenization

The generated parser includes a **lexer** (scanner) component that tokenizes the source code (turning characters into tokens).
In some languages, you may supply a custom external scanner to handle tricky lexing cases (e.g., indent-based blocks, embedded languages) via `scanner.c`.

### Parser Engine (GLR / LR)

The core algorithm is a **generalized LR (GLR)** parser. GLR means it can handle grammars with some ambiguity and still produce valid parse trees.
In simple terms, the parser uses a parse table (states × tokens) to decide shift/reduce actions.
The grammar defines precedence/associativity to resolve ambiguities.
In addition to traditional LR parsing, Tree-sitter is optimized for incremental operation (see next).

### Tree Representation & Node Structure

After parsing, you obtain a **Concrete Syntax Tree (CST)**, which is a graph of nodes representing lexical tokens and syntactic constructs.
Nodes carry the source-range (start and end positions) information.
Nodes can be named, anonymous (underscore prefix in grammar means "anonymous" so it doesn’t appear in the final tree) to keep the tree cleaner.

### Incremental Parsing

A key feature: when the source text changes (e.g., editing in an editor), Tree-sitter avoids re-parsing the whole file.
Instead it **reuses** existing subtrees for unchanged regions and **re-parses only** the changed region plus a small margin around it.

1. Editor notifies parser of changes (range of changed characters, old/new text)
2. Parser identifies which nodes’ source ranges are invalidated
3. It re-parses the minimal region and re-connects to reused nodes outside that region
4. It produces an updated tree with source ranges corrected.

### Querying & Tree Walk / API

Once you have a tree, you can run **queries** (S-expression style) to find sets of nodes matching patterns.
For example, capture all `if_statement` nodes or function declarations.
The API (C API, plus language bindings) allows you to walk nodes, inspect children, get start/end positions, text, etc[^3].
The query system is powerful: you can specify patterns, nested structures, predicates (e.g., `#eq? @attr_name "class"`).

### Embedding / Use in Editors & Tools

Tree-sitter is designed to be embedded: the parsing library is written in C, and there are bindings in many languages (Rust, JS, Python, etc.)[^2].
Editor plugins (for example nvim‑treesitter for Neovim) use Tree-sitter for syntax highlighting, structural editing, text-objects.

[^1]: <https://tree-sitter.github.io/> "Tree-sitter: Introduction"
[^2]: <https://en.wikipedia.org/wiki/Tree-sitter_%28parser_generator%29> "Tree-sitter (parser generator)"
[^3]: <https://tree-sitter.github.io/tree-sitter/using-parsers/> "Using Parsers - Tree-sitter"
