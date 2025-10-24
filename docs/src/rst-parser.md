# reStructuredText Parser

The `beacon-parser` crate ships with a lightweight, purpose-built reStructuredText parser implemented in `crates/parser/src/rst.rs`.
It focuses on the subset of the syntax we need for documentation generation and command line output, favoring predictable behavior.

## Block Structure

The parser performs a straightforward recursive-descent walk over the input, processing one logical block at a time.

It recognises:

- Setext-style headings: a line of text followed by `=====` (level 1) or `-----` (level 2).
- Paragraphs: one or more non-blank lines that are not captured by any other   block rule.
- Bullet lists: lines beginning with `-`, `*`, or `+` followed by a space.
  List items are treated as inline-only content for simplicity.
- Fenced code blocks: triple backticks on their own line open and close a block, preserving the inner text verbatim.
- Block quotes: lines prefixed with `>` become a nested document that is parsed recursively.

Each recognised block is converted into a `Block` enum variant, which also drives rendering to HTML or Markdown.

## Inlines

Within paragraph-like content the parser performs a single-pass scan that emits `Inline` nodes.

- `*emphasis*` → `Inline::Em`
- `**strong**` → `Inline::Strong`
- `` `code` `` → `Inline::Code`
- `` `text <url>`_ `` → `Inline::Link`

Inline scanning falls back to literal text whenever it encounters mismatched or unsupported markup, to avoid panics or lossy conversions.

## Rendering Helpers

Two helper functions expose the parser output in convenient formats:

1. `html_of(input: &str) -> String` renders the parsed document to HTML by walking the block tree and using `Display` implementations for both `Block` and `Inline`.
2. `markdown_of(input: &str) -> String` first renders to HTML and then converts the result back to Markdown using the `html2md` crate.
    This is essential for rendering docstrings when we receive snippets that need to be surfaced as markdown
