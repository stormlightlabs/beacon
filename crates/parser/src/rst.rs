//! Recursive descent based parser for reStructuredText
//!
//! TODO:

use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Inline {
    Text(String),
    Em(String),
    Strong(String),
    Code(String),
    Link { text: String, url: String },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Block {
    Heading { level: u8, inlines: Vec<Inline> },
    Paragraph(Vec<Inline>),
    List(Vec<Vec<Inline>>),
    CodeBlock(String),
    Quote(Vec<Block>),
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("unexpected end of input")]
    Eof,
    #[error("invalid syntax at line {line}: {msg}")]
    Invalid { line: usize, msg: String },
}

#[derive(Debug, Clone, Copy)]
struct Line<'a> {
    _num: usize,
    raw: &'a str,
}

#[derive(Debug)]
struct Lines<'a> {
    all: Vec<Line<'a>>,
    i: usize,
}

impl<'a> Lines<'a> {
    fn new(input: &'a str) -> Self {
        let all = input
            .lines()
            .enumerate()
            .map(|(i, raw)| Line { _num: i + 1, raw })
            .collect();
        Self { all, i: 0 }
    }

    fn peek(&self) -> Option<&Line<'a>> {
        self.all.get(self.i)
    }

    fn next(&mut self) -> Option<Line<'a>> {
        let l = self.all.get(self.i).cloned();
        self.i += (l.is_some()) as usize;
        l
    }

    fn backtrack(&mut self) {
        if self.i > 0 {
            self.i -= 1;
        }
    }

    fn is_eof(&self) -> bool {
        self.i >= self.all.len()
    }
}

fn is_blank(s: &str) -> bool {
    s.trim().is_empty()
}

fn is_bullet(s: &str) -> bool {
    let t = s.trim_start();
    t.starts_with("- ") || t.starts_with("* ") || t.starts_with("+ ")
}

fn strip_bullet(s: &str) -> Option<&str> {
    let t = s.trim_start();
    for p in ["- ", "* ", "+ "] {
        if let Some(rest) = t.strip_prefix(p) {
            return Some(rest);
        }
    }
    None
}

fn underline_level(s: &str) -> Option<u8> {
    let t = s.trim();
    if !t.is_empty() && t.chars().all(|c| c == '=') {
        Some(1)
    } else if !t.is_empty() && t.chars().all(|c| c == '-') {
        Some(2)
    } else {
        None
    }
}

/// Inline recognizer that runs a linear scan, greedy where safe.
/// Handles **strong**, *em*, `code`, and `text <url>`_ references.
fn parse_inlines(text: &str) -> Vec<Inline> {
    let mut out = Vec::new();
    let mut buf = String::new();
    let bytes = text.as_bytes();
    let mut i = 0;

    let flush_text = |buf: &mut String, out: &mut Vec<Inline>| {
        if !buf.is_empty() {
            out.push(Inline::Text(std::mem::take(buf)));
        }
    };

    while i < text.len() {
        if bytes[i] == b'*' && i + 1 < text.len() && bytes[i + 1] == b'*' {
            if let Some(end) = text[i + 2..].find("**") {
                let inner = &text[i + 2..i + 2 + end];
                if !inner.is_empty() {
                    flush_text(&mut buf, &mut out);
                    out.push(Inline::Strong(inner.to_string()));
                    i += 2 + end + 2;
                    continue;
                }
            }
        }

        if bytes[i] == b'*' {
            if let Some(end) = text[i + 1..].find('*') {
                let inner = &text[i + 1..i + 1 + end];
                if !inner.is_empty() {
                    flush_text(&mut buf, &mut out);
                    out.push(Inline::Em(inner.to_string()));
                    i += 1 + end + 1;
                    continue;
                }
            }
        }

        if bytes[i] == b'`' {
            if let Some(end) = text[i + 1..].find('`') {
                let closing_tick = i + 1 + end;
                let after_tick = closing_tick + 1;
                if after_tick < text.len() && bytes[after_tick] == b'_' {
                    let inner = &text[i + 1..closing_tick];
                    if let (Some(l), Some(r)) = (inner.find('<'), inner.rfind('>')) {
                        if r > l {
                            let label = inner[..l].trim();
                            let url = inner[l + 1..r].trim();
                            if !label.is_empty() && !url.is_empty() {
                                flush_text(&mut buf, &mut out);
                                out.push(Inline::Link { text: label.to_string(), url: url.to_string() });
                                i = after_tick + 1;
                                continue;
                            }
                        }
                    }
                }

                flush_text(&mut buf, &mut out);
                let inner = &text[i + 1..closing_tick];
                out.push(Inline::Code(inner.to_string()));
                i = closing_tick + 1;
                continue;
            }
        }

        let ch = text[i..].chars().next().unwrap();
        buf.push(ch);
        i += ch.len_utf8();
    }

    if !buf.is_empty() {
        out.push(Inline::Text(buf));
    }
    out
}

pub fn parse(input: &str) -> Result<Vec<Block>, ParseError> {
    let mut ls = Lines::new(input);
    let mut blocks = Vec::new();

    while !ls.is_eof() {
        while let Some(l) = ls.peek() {
            if is_blank(l.raw) {
                ls.next();
            } else {
                break;
            }
        }
        if ls.is_eof() {
            break;
        }

        if let Some(l) = ls.peek() {
            if l.raw.trim() == "```" {
                ls.next();
                let mut buf = String::new();
                while let Some(inner) = ls.next() {
                    if inner.raw.trim() == "```" {
                        break;
                    }
                    buf.push_str(inner.raw);
                    buf.push('\n');
                }
                blocks.push(Block::CodeBlock(buf));
                continue;
            }
        }

        if let Some(l) = ls.peek() {
            if l.raw.trim_start().starts_with('>') {
                let mut quote = String::new();
                while let Some(q) = ls.peek() {
                    let t = q.raw.trim_start();
                    if t.starts_with('>') {
                        ls.next();
                        quote.push_str(t.trim_start_matches("> ").trim_start_matches('>'));
                        quote.push('\n');
                    } else {
                        break;
                    }
                }
                let inner = parse(&quote)?;
                blocks.push(Block::Quote(inner));
                continue;
            }
        }

        if let Some(l) = ls.peek() {
            if is_bullet(l.raw) {
                let mut items: Vec<Vec<Inline>> = Vec::new();
                while let Some(it) = ls.peek() {
                    if !is_bullet(it.raw) {
                        break;
                    }
                    let line = ls.next().unwrap();
                    let content = strip_bullet(line.raw).unwrap().trim_end();
                    items.push(parse_inlines(content));
                }
                blocks.push(Block::List(items));
                continue;
            }
        }

        if let Some(title) = ls.next() {
            if let Some(ul) = ls.peek() {
                if let Some(level) = underline_level(ul.raw) {
                    ls.next(); // consume underline
                    let inlines = parse_inlines(title.raw.trim());
                    blocks.push(Block::Heading { level, inlines });
                    continue;
                }
            }
            ls.backtrack();
        }

        let mut buf = String::new();
        while let Some(l) = ls.peek() {
            if is_blank(l.raw) || is_bullet(l.raw) || l.raw.trim() == "```" || l.raw.trim_start().starts_with('>') {
                break;
            }
            buf.push_str(ls.next().unwrap().raw);
            buf.push('\n');
        }
        let text = buf.trim_end();
        if !text.is_empty() {
            blocks.push(Block::Paragraph(parse_inlines(text)));
        }
    }

    Ok(blocks)
}

impl fmt::Display for Inline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Inline::Text(t) => write!(f, "{t}"),
            Inline::Em(t) => write!(f, "<em>{}</em>", t),
            Inline::Strong(t) => write!(f, "<strong>{}</strong>", t),
            Inline::Code(t) => write!(f, "<code>{}</code>", html_escape(t)),
            Inline::Link { text, url } => write!(f, "<a href=\"{}\">{}</a>", url, text),
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Block::Heading { level, inlines } => {
                let tag = match level {
                    1 => "h1",
                    2 => "h2",
                    _ => "h2",
                };
                write!(f, "<{}>{}</{}>", tag, join_inlines(inlines), tag)
            }
            Block::Paragraph(inl) => write!(f, "<p>{}</p>", join_inlines(inl)),
            Block::List(items) => {
                write!(f, "<ul>")?;
                for it in items {
                    write!(f, "<li>{}</li>", join_inlines(it))?;
                }
                write!(f, "</ul>")
            }
            Block::CodeBlock(code) => write!(f, "<pre><code>{}</code></pre>", html_escape(code)),
            Block::Quote(children) => {
                write!(f, "<blockquote>")?;
                for b in children {
                    write!(f, "{b}")?;
                }
                write!(f, "</blockquote>")
            }
        }
    }
}

fn join_inlines(v: &[Inline]) -> String {
    v.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("")
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;")
}

fn html_of(input: &str) -> String {
    parse(input)
        .unwrap()
        .into_iter()
        .map(|b| b.to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn markdown_of(input: &str) -> String {
    let html = &html_of(input);
    html2md::parse_html(html)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_test_basic_doc() {
        let doc = r#"
Title
=====

A paragraph with *emphasis*, **strong**, and `code`.
"#;

        let html = html_of(doc);
        assert!(html.contains("<h1>Title</h1>"));
        assert!(html.contains("<em>emphasis</em>"));
        assert!(html.contains("<strong>strong</strong>"));
        assert!(html.contains("<code>code</code>"));
    }

    #[test]
    fn parses_setext_headings() {
        let doc = "Heading 1\n=========\n\nHeading 2\n---------";
        let ast = parse(doc).unwrap();

        assert_eq!(ast.len(), 2);
        match &ast[0] {
            Block::Heading { level, inlines } => {
                assert_eq!(*level, 1);
                assert_eq!(inlines[0], Inline::Text("Heading 1".into()));
            }
            _ => panic!("expected heading"),
        }

        match &ast[1] {
            Block::Heading { level, inlines } => {
                assert_eq!(*level, 2);
                assert_eq!(inlines[0], Inline::Text("Heading 2".into()));
            }
            _ => panic!("expected heading"),
        }
    }

    #[test]
    fn parses_paragraphs() {
        let doc = "This is a paragraph.\n\nAnother paragraph.";
        let ast = parse(doc).unwrap();

        assert_eq!(ast.len(), 2);
        assert!(matches!(ast[0], Block::Paragraph(_)));
        assert!(matches!(ast[1], Block::Paragraph(_)));
    }

    #[test]
    fn parses_unordered_list() {
        let doc = "- One\n- Two\n- Three";
        let ast = parse(doc).unwrap();

        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Block::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0][0], Inline::Text("One".into()));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn parses_code_fence() {
        let doc = "```\nline1\nline2\n```";
        let ast = parse(doc).unwrap();

        assert_eq!(ast.len(), 1);
        match &ast[0] {
            Block::CodeBlock(code) => {
                assert!(code.contains("line1"));
                assert!(code.contains("line2"));
            }
            _ => panic!("expected code block"),
        }
    }

    #[test]
    fn parses_quote_block() {
        let doc = "> quoted line\n> continues\n\nregular paragraph";
        let ast = parse(doc).unwrap();

        assert_eq!(ast.len(), 2);
        match &ast[0] {
            Block::Quote(inner) => {
                assert_eq!(inner.len(), 1);
                assert!(matches!(&inner[0], Block::Paragraph(_)));
            }
            _ => panic!("expected quote block"),
        }
    }

    #[test]
    fn parses_emphasis_and_strong() {
        let line = "A *word* and a **strong** one";
        let inl = parse_inlines(line);
        let html = join_inlines(&inl);
        assert!(html.contains("<em>word</em>"));
        assert!(html.contains("<strong>strong</strong>"));
    }

    #[test]
    fn parses_inline_code() {
        let line = "Inline `code` works";
        let html = join_inlines(&parse_inlines(line));
        assert!(html.contains("<code>code</code>"));
    }

    #[test]
    fn parses_inline_link() {
        let line = "`example <https://example.com>`_";
        let html = join_inlines(&parse_inlines(line));
        assert!(html.contains("<a href=\"https://example.com\">example</a>"));
    }

    #[test]
    fn inline_link_requires_reference_suffix() {
        let line = "`example <https://example.com>`";
        let inl = parse_inlines(line);
        assert_eq!(inl, vec![Inline::Code("example <https://example.com>".into())]);
    }

    #[test]
    fn inline_link_mixed_with_text() {
        let line = "Read `docs <https://example.com>`_ now.";
        let inl = parse_inlines(line);
        assert_eq!(
            inl,
            vec![
                Inline::Text("Read ".into()),
                Inline::Link { text: "docs".into(), url: "https://example.com".into() },
                Inline::Text(" now.".into())
            ]
        );
    }

    #[test]
    fn parses_mixed_inline_styles() {
        let line = "**bold** *em* `code` and `link <x>`_";
        let html = join_inlines(&parse_inlines(line));
        assert!(html.contains("<strong>bold</strong>"));
        assert!(html.contains("<em>em</em>"));
        assert!(html.contains("<code>code</code>"));
        assert!(html.contains("<a href=\"x\">link</a>"));
    }

    #[test]
    fn unmatched_markup_falls_back_to_text() {
        let line = "An *unfinished emphasis";
        let inl = parse_inlines(line);
        assert_eq!(inl, vec![Inline::Text("An *unfinished emphasis".into())]);
    }

    #[test]
    fn html_of_renders_expected_html() {
        let doc = "Heading\n=======\n\nBody text.";
        let rendered = html_of(doc);
        assert_eq!(rendered.trim(), "<h1>Heading</h1>\n<p>Body text.</p>");
    }

    #[test]
    fn markdown_of_round_trips_to_markdown() {
        let doc = "Heading\n=======\n\n- Item 1\n- Item 2";
        let markdown = markdown_of(doc);
        let normalized = markdown.trim();
        assert_eq!(normalized, "Heading\n==========\n\n* Item 1\n* Item 2");
    }

    #[test]
    fn parses_multiple_blocks_correctly() {
        let doc = r#"
Title
=====

- One
- Two
- Three

> quote

````

code

```
"#;
        let html = html_of(doc);
        assert!(html.contains("<h1>Title</h1>"));
        assert!(html.contains("<ul>"));
        assert!(html.contains("<blockquote>"));
        assert!(html.contains("<pre><code>"));
    }

    #[test]
    fn ignores_blank_lines() {
        let doc = "\n\nParagraph\n\n\nAnother\n";
        let ast = parse(doc).unwrap();
        assert_eq!(ast.len(), 2);
    }

    #[test]
    fn html_escape_works() {
        let code = "<script>alert('x')</script>";
        let esc = html_escape(code);
        assert!(esc.contains("&lt;"));
        assert!(esc.contains("&gt;"));
        assert!(!esc.contains("<script>"));
    }
}
