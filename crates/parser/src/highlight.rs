use owo_colors::{OwoColorize, Style};
use tree_sitter::Node;

/// Syntax highlighter for Python code using tree-sitter
pub struct PythonHighlighter {
    enable_colors: bool,
}

/// Color scheme for different syntax elements
#[derive(Debug, Clone)]
pub struct ColorScheme {
    pub keyword: Style,
    pub function: Style,
    pub class: Style,
    pub string: Style,
    pub number: Style,
    pub comment: Style,
    pub identifier: Style,
    pub operator: Style,
    pub punctuation: Style,
    pub type_annotation: Style,
    pub builtin: Style,
}

impl Default for ColorScheme {
    fn default() -> Self {
        Self {
            keyword: Style::new().bright_magenta().bold(),
            function: Style::new().bright_blue(),
            class: Style::new().bright_yellow().bold(),
            string: Style::new().bright_green(),
            number: Style::new().bright_cyan(),
            comment: Style::new().bright_black().italic(),
            identifier: Style::new().white(),
            operator: Style::new().bright_white(),
            punctuation: Style::new().bright_black(),
            type_annotation: Style::new().bright_cyan(),
            builtin: Style::new().bright_magenta(),
        }
    }
}

impl PythonHighlighter {
    pub fn new(enable_colors: bool) -> Self {
        Self { enable_colors }
    }

    /// Highlight Python source code using tree-sitter
    ///
    /// Collects all highlight ranges, sorts by starting position, then applies highlights
    pub fn highlight(&self, source: &str, tree: &tree_sitter::Tree) -> String {
        if !self.enable_colors {
            return source.to_string();
        }

        let color_scheme = ColorScheme::default();
        let root_node = tree.root_node();

        let mut highlights = Vec::new();
        self.collect_highlights(root_node, source, &color_scheme, &mut highlights);

        highlights.sort_by_key(|h| h.start);

        self.apply_highlights(source, highlights)
    }

    fn collect_highlights(
        &self, node: Node, source: &str, color_scheme: &ColorScheme, highlights: &mut Vec<Highlight>,
    ) {
        let start_byte = node.start_byte();
        let end_byte = node.end_byte();
        let style = self.get_node_style(node, source, color_scheme);

        if let Some(style) = style {
            highlights.push(Highlight {
                start: start_byte,
                end: end_byte,
                style,
                priority: self.get_node_priority(node.kind()),
            });
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_highlights(child, source, color_scheme, highlights);
        }
    }

    fn get_node_style(&self, node: Node, source: &str, color_scheme: &ColorScheme) -> Option<Style> {
        match node.kind() {
            "def" | "class" | "if" | "elif" | "else" | "for" | "while" | "try" | "except" | "finally" | "with"
            | "as" | "import" | "from" | "return" | "yield" | "break" | "continue" | "pass" | "del" | "global"
            | "nonlocal" | "lambda" | "and" | "or" | "not" | "in" | "is" | "async" | "await" => {
                Some(color_scheme.keyword)
            }
            "function_definition" => match node.child_by_field_name("name") {
                Some(_named_node) => return Some(color_scheme.function),
                None => None,
            },
            "class_definition" => match node.child_by_field_name("name") {
                Some(_named_node) => return Some(color_scheme.class),
                None => None,
            },
            "string" | "string_content" => Some(color_scheme.string),
            "integer" | "float" | "true" | "false" => Some(color_scheme.number),
            "none" => Some(color_scheme.keyword),
            "comment" => Some(color_scheme.comment),
            "=" | "+" | "-" | "*" | "/" | "//" | "%" | "**" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+=" | "-="
            | "*=" | "/=" | "//=" | "%=" | "**=" | "&" | "|" | "^" | "~" | "<<" | ">>" | "&=" | "|=" | "^=" | "<<="
            | ">>=" => Some(color_scheme.operator),
            "(" | ")" | "[" | "]" | "{" | "}" | "," | ":" | ";" | "." => Some(color_scheme.punctuation),
            "identifier" => {
                let text = node.utf8_text(source.as_bytes()).ok()?;
                match text {
                    "print" | "len" | "range" | "enumerate" | "zip" | "map" | "filter" | "sum" | "min" | "max"
                    | "abs" | "round" | "int" | "float" | "str" | "bool" | "list" | "dict" | "set" | "tuple"
                    | "type" | "isinstance" | "hasattr" | "getattr" | "setattr" | "delattr" | "open" | "input" => {
                        Some(color_scheme.builtin)
                    }
                    "self" | "cls" => Some(color_scheme.keyword),
                    _ => Some(color_scheme.identifier),
                }
            }
            _ => None,
        }
    }

    fn get_node_priority(&self, node_kind: &str) -> u8 {
        match node_kind {
            "def" | "class" | "if" | "else" | "return" => 10,
            "string" | "integer" | "float" => 9,
            "identifier" => 1,
            _ => 5,
        }
    }

    fn apply_highlights(&self, source: &str, mut highlights: Vec<Highlight>) -> String {
        if highlights.is_empty() {
            return source.to_string();
        }

        highlights.sort_by(|a, b| a.start.cmp(&b.start).then(b.priority.cmp(&a.priority)));

        let mut filtered_highlights = Vec::new();
        let mut last_end = 0;

        for highlight in highlights {
            if highlight.start >= last_end {
                last_end = highlight.end;
                filtered_highlights.push(highlight);
            }
        }

        filtered_highlights.sort_by(|a, b| b.start.cmp(&a.start));

        let mut result = source.to_string();
        for highlight in filtered_highlights {
            if highlight.start < result.len() && highlight.end <= result.len() {
                let text = &result[highlight.start..highlight.end];
                let styled_text = text.style(highlight.style).to_string();
                result.replace_range(highlight.start..highlight.end, &styled_text);
            }
        }

        result
    }
}

#[derive(Debug, Clone)]
struct Highlight {
    start: usize,
    end: usize,
    style: Style,
    priority: u8,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PythonParser;

    #[test]
    fn test_basic_highlighting() {
        let mut parser = PythonParser::new().unwrap();
        let source = "def hello(name):\n    return f'Hello {name}'";

        let parsed = parser.parse(source).unwrap();
        let highlighter = PythonHighlighter::new(true);
        let highlighted = highlighter.highlight(source, &parsed.tree);

        // The highlighted version should be different from the original
        // (contains ANSI color codes)
        assert_ne!(highlighted, source);
        assert!(highlighted.len() > source.len());
    }

    #[test]
    fn test_no_colors() {
        let mut parser = PythonParser::new().unwrap();
        let source = "def hello(): pass";

        let parsed = parser.parse(source).unwrap();
        let highlighter = PythonHighlighter::new(false);
        let highlighted = highlighter.highlight(source, &parsed.tree);

        // Should be identical when colors are disabled
        assert_eq!(highlighted, source);
    }

    #[test]
    fn test_string_highlighting() {
        let mut parser = PythonParser::new().unwrap();
        let source = "x = 'hello world'";

        let parsed = parser.parse(source).unwrap();
        let highlighter = PythonHighlighter::new(true);
        let highlighted = highlighter.highlight(source, &parsed.tree);

        // Should contain color codes for the string
        assert_ne!(highlighted, source);
        assert!(highlighted.contains("hello world"));
    }
}
