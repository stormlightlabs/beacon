# reStructuredText Parser

The `beacon-parser` crate ships with a lightweight, purpose-built reStructuredText parser implemented in `beacon_parser::rst`
It focuses on the subset of syntax needed for documentation tooling.

## Block Structure

The parser performs a simple recursive-descent scan over the input and recognises:

| Input              | Pattern                                                                                          |
| ------------------ | ------------------------------------------------------------------------------------------------ |
| Setext headings    | a text line followed by `=====` (level 1) or `-----` (level 2)                                   |
| Paragraphs         | consecutive non-blank lines that are not captured by another rule                                |
| Lists              | unordered markers `-`, `*`, `+` and ordered markers like `1.`; items are rendered inline for now |
| Fenced code blocks | triple backticks start and end verbatim sections                                                 |
| Block quotes       | lines beginning with `>` are parsed recursively as nested blocks                                 |

For field and definition entries, the parser can parse Sphinx-style `:param foo: ...` & NumPy/Google formats like `foo : int` and `foo (int): ...` to emit labelled paragraphs that keep parameter names and optional type information.

Each recognised construct is represented by a `Block` enum variant, which also drives the HTML rendering used downstream.

## Inline Features

Within paragraph-like content the parser performs a single-pass scan that emits `Inline` nodes.

Supported markup covers common docstring patterns:

| Markup                         | Member           |
| ------------------------------ | ---------------- |
| `*emphasis*`                   | `Inline::Em`     |
| `**strong**`                   | `Inline::Strong` |
| `` `code` `` and ` `literal` ` | `Inline::Code`   |
| `` `text <url>`_ ``            | `Inline::Link`   |

Unrecognised sequences fall back to `Inline::Text`, to avoid panics or lossy conversions when encountering partial markup.

## Rendering Helpers

Two helper functions expose the parsed output in convenient formats:

1. `html_of(input: &str) -> String` walks the block tree and renders HTML via each `Block`/`Inline` display implementation.
2. `markdown_of(input: &str) -> String` normalises indentation (see below), renders to HTML, and converts the result back to Markdown with `html2md`.
   This is the entry point used by the language server when sending hover contents.

## Docstring Support

Python docstrings rarely contain “pure” reStructuredText, so a few pragmatic steps make the output useful in practice:

1. Input is trimmed and dedented to remove indentation introduced by Python syntax.
2. Heading-like lines ending with `:` (for example `Args:` or `Returns:`) are promoted to level-two headings when followed by indented content.
3. Parameter tables expressed as `:param foo:`, `foo : int`, or `foo (int):` are normalised into labelled paragraphs while preserving type annotations.
4. Continuation lines indented relative to the field/definition entry are parsed recursively, so nested lists and code blocks continue to work.
