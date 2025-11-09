//! Formatted code writer
//!
//! Generates formatted Python code by processing tokens and applying whitespace, indentation, and line-breaking rules.

use super::config::FormatterConfig;
use super::context::FormattingContext;
use super::token_stream::Token;

/// Writes formatted Python code
///
/// Processes tokens from the token stream and emits properly formatted output according to PEP8 rules and configuration settings.
pub struct FormattedWriter<'a> {
    /// Output buffer
    output: String,

    /// Formatting context
    context: FormattingContext<'a>,
    next_token_text: Option<String>,
}

impl<'a> FormattedWriter<'a> {
    /// Create a new writer with the given configuration
    pub fn new(config: &'a FormatterConfig) -> Self {
        Self { output: String::new(), context: FormattingContext::new(config), next_token_text: None }
    }

    pub fn set_next_token(&mut self, token: Option<&Token>) {
        self.next_token_text = token.and_then(|t| t.text().map(|s| s.to_string()));
    }

    pub fn peek_next_token_text(&self) -> Option<&str> {
        self.next_token_text.as_deref()
    }

    /// Get the formatted output
    pub fn output(&self) -> &str {
        &self.output
    }

    /// Write a token to the output
    pub fn write_token(&mut self, token: &Token) {
        match token {
            Token::Keyword { text, .. } => self.write_keyword(text),
            Token::Identifier { text, .. } => self.write_identifier(text),
            Token::Operator { text, .. } => self.write_operator(text),
            Token::Delimiter { text, .. } => self.write_delimiter(text),
            Token::StringLiteral { text, .. } => self.write_string(text),
            Token::NumberLiteral { text, .. } => self.write_number(text),
            Token::Comment { text, .. } => self.write_comment(text),
            Token::Newline { .. } => self.write_newline(),
            Token::Indent { level, .. } => self.write_indent(*level),
            Token::Dedent { .. } => {}
            Token::Whitespace { .. } => {}
        }
    }

    /// Write text to the output, updating column position
    fn write(&mut self, text: &str) {
        self.output.push_str(text);
        self.context.advance_column(text.chars().count());
    }

    /// Write a keyword with appropriate spacing
    fn write_keyword(&mut self, keyword: &str) {
        if self.needs_space_before_keyword() {
            self.write(" ");
        }
        self.write(keyword);
    }

    /// Write an identifier with appropriate spacing
    fn write_identifier(&mut self, identifier: &str) {
        if self.needs_space_before_identifier() {
            self.write(" ");
        }
        self.write(identifier);
    }

    /// Write an operator with appropriate spacing
    fn write_operator(&mut self, operator: &str) {
        let config = self.context.config();

        let no_space_operators = [".", "@"];
        if no_space_operators.contains(&operator) {
            self.write(operator);
            return;
        }

        let unary_operators = ["-", "+", "~"];
        let is_unary = unary_operators.contains(&operator)
            && (self.context.at_line_start()
                || self.output.ends_with('=')
                || self.output.ends_with(' ')
                || self.output.ends_with('(')
                || self.output.ends_with('[')
                || self.output.ends_with('{')
                || self.output.ends_with(','));

        if is_unary {
            self.write(operator);
            return;
        }

        if config.spaces_around_operators && self.is_binary_operator(operator) {
            if !self.context.at_line_start() && self.context.current_column() > 0 && !self.output.ends_with(' ') {
                self.write(" ");
            }
            self.write(operator);
            if !self.output.ends_with(' ') {
                self.write(" ");
            }
        } else {
            self.write(operator);
        }
    }

    /// Write a delimiter with appropriate spacing
    fn write_delimiter(&mut self, delimiter: &str) {
        match delimiter {
            "(" | "[" | "{" => {
                self.write(delimiter);
                match delimiter {
                    "(" => self.context.enter_paren(),
                    "[" => self.context.enter_bracket(),
                    "{" => self.context.enter_brace(),
                    _ => {}
                }
            }
            ")" | "]" | "}" => {
                self.write(delimiter);
                match delimiter {
                    ")" => self.context.exit_paren(),
                    "]" => self.context.exit_bracket(),
                    "}" => self.context.exit_brace(),
                    _ => {}
                }
            }
            "," => {
                self.write(delimiter);
                if !matches!(self.peek_next_token_text(), Some(")") | Some("]") | Some("}"))
                    && !self.output.ends_with(' ')
                {
                    self.write(" ");
                }
            }
            ":" => {
                let prev = self.output.trim_end().chars().last();
                let is_slice = matches!(prev, Some('[') | Some(',') | Some(':'));

                self.write(delimiter);
                if !is_slice && !self.output.ends_with(' ') {
                    self.write(" ");
                }
            }
            _ => {
                self.write(delimiter);
            }
        }
    }

    /// Write a string literal
    fn write_string(&mut self, text: &str) {
        if self.needs_space_before_string() {
            self.write(" ");
        }

        let rules = super::rules::FormattingRules::new(self.context.config().clone());
        let normalized = rules.normalize_quotes(text);
        self.write(&normalized);
    }

    /// Write a number literal
    fn write_number(&mut self, text: &str) {
        if self.needs_space_before_number() {
            self.write(" ");
        }
        self.write(text);
    }

    /// Write a comment
    pub fn write_comment(&mut self, text: &str) {
        let rules = super::rules::FormattingRules::new(self.context.config().clone());
        let is_inline = !self.context.at_line_start();
        let formatted = if rules.should_preserve_comment(text) {
            text.to_string()
        } else {
            rules.format_comment(text, is_inline)
        };

        let indent_level = self.context.indent_level();

        if self.output.ends_with('\n') && indent_level > 0 {
            self.write(&"    ".repeat(indent_level));
        }

        if !self.output.ends_with('\n') && !self.output.ends_with(' ') {
            self.write("  ");
        }

        self.write(&formatted);
        if !is_inline {
            self.write("\n");
        }
    }

    /// Write a newline
    fn write_newline(&mut self) {
        self.remove_trailing_whitespace();
        self.output.push('\n');
        self.context.newline();
    }

    /// Write indentation for the given level
    fn write_indent(&mut self, level: usize) {
        if !self.context.at_line_start() {
            self.write_newline();
        }

        self.context.set_indent(level);
        let indent_str = self.context.indent_string();
        self.output.push_str(&indent_str);
        self.context.advance_column(indent_str.len());
    }

    /// Remove trailing whitespace from the current line
    fn remove_trailing_whitespace(&mut self) {
        while self.output.ends_with(' ') || self.output.ends_with('\t') {
            self.output.pop();
        }
    }

    /// Check if we need space before a keyword
    fn needs_space_before_keyword(&self) -> bool {
        !self.context.at_line_start() && self.context.current_column() > 0 && !self.output.ends_with(' ')
    }

    /// Check if we need space before an identifier
    fn needs_space_before_identifier(&self) -> bool {
        if self.context.at_line_start() {
            return false;
        }

        if self.just_wrote_unary_operator() {
            return false;
        }

        !self.output.ends_with('(')
            && !self.output.ends_with('[')
            && !self.output.ends_with('{')
            && !self.output.ends_with('.')
            && !self.output.ends_with('@')
            && !self.output.ends_with(' ')
            && !self.output.ends_with(',')
            && self.context.current_column() > 0
    }

    /// Check if we need space before a string
    fn needs_space_before_string(&self) -> bool {
        if self.context.at_line_start() {
            return false;
        }

        if self.just_wrote_unary_operator() {
            return false;
        }

        !self.output.ends_with('(')
            && !self.output.ends_with('[')
            && !self.output.ends_with('{')
            && !self.output.ends_with(' ')
            && !self.output.ends_with(',')
            && self.context.current_column() > 0
    }

    /// Check if we need space before a number
    fn needs_space_before_number(&self) -> bool {
        if self.context.at_line_start() {
            return false;
        }

        if self.just_wrote_unary_operator() {
            return false;
        }

        !self.output.ends_with('(')
            && !self.output.ends_with('[')
            && !self.output.ends_with('{')
            && !self.output.ends_with(',')
            && !self.output.ends_with(' ')
            && self.context.current_column() > 0
    }

    /// Check if an operator is a binary operator
    fn is_binary_operator(&self, operator: &str) -> bool {
        matches!(
            operator,
            "+" | "-"
                | "*"
                | "/"
                | "%"
                | "//"
                | "**"
                | "=="
                | "!="
                | "<"
                | ">"
                | "<="
                | ">="
                | "and"
                | "or"
                | "not"
                | "in"
                | "is"
                | "&"
                | "|"
                | "^"
                | "~"
                | "<<"
                | ">>"
                | "->"
                | "="
                | "+="
                | "-="
                | "*="
                | "/="
                | "%="
                | "//="
                | "**="
                | "&="
                | "|="
                | "^="
                | "<<="
                | ">>="
        )
    }

    /// Ensure we have the correct number of blank lines before a definition
    pub fn ensure_blank_lines_before_definition(&mut self, is_class: bool, is_top_level: bool) {
        if let Some(last_line) = self.output.lines().rev().find(|l| !l.trim().is_empty()) {
            if last_line.trim_start().starts_with('@') {
                return;
            }
        }

        let config = self.context.config();
        if !is_top_level {
            self.ensure_blank_lines(1);
        } else {
            let required =
                if (is_class && config.blank_line_before_class) || (!is_class && config.blank_line_before_function) {
                    2
                } else {
                    0
                };
            self.ensure_blank_lines(required);
        }
    }

    /// Ensure we have exactly n blank lines
    fn ensure_blank_lines(&mut self, n: usize) {
        let config = self.context.config();
        let max_allowed = config.max_blank_lines;
        let target = n.min(max_allowed);

        let mut current_blank = 0;
        let lines: Vec<&str> = self.output.rsplitn(target + 2, '\n').collect();
        for line in lines.iter().skip(1) {
            if line.trim().is_empty() {
                current_blank += 1;
            } else {
                break;
            }
        }

        if current_blank < target {
            for _ in current_blank..target {
                self.output.push('\n');
            }
        } else if current_blank > target {
            for _ in target..current_blank {
                if let Some(last_newline) = self.output.rfind('\n') {
                    self.output.truncate(last_newline);
                }
            }
        }
    }

    /// Get reference to the formatting context
    pub fn context(&self) -> &FormattingContext<'a> {
        &self.context
    }

    /// Get mutable reference to the formatting context
    pub fn context_mut(&mut self) -> &mut FormattingContext<'a> {
        &mut self.context
    }

    fn just_wrote_unary_operator(&self) -> bool {
        self.output.ends_with('-') || self.output.ends_with('+') || self.output.ends_with('~')
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_writer_creation() {
        let config = FormatterConfig::default();
        let writer = FormattedWriter::new(&config);
        assert_eq!(writer.output(), "");
    }

    #[test]
    fn test_write_keyword() {
        let config = FormatterConfig::default();
        let mut writer = FormattedWriter::new(&config);

        let token = Token::Keyword { text: "def".to_string(), line: 1, col: 0 };
        writer.write_token(&token);

        assert_eq!(writer.output(), "def");
    }

    #[test]
    fn test_write_with_spacing() {
        let config = FormatterConfig::default();
        let mut writer = FormattedWriter::new(&config);

        writer.write_token(&Token::Keyword { text: "def".to_string(), line: 1, col: 0 });
        writer.write_token(&Token::Identifier { text: "foo".to_string(), line: 1, col: 4 });

        assert_eq!(writer.output(), "def foo");
    }

    #[test]
    fn test_trailing_whitespace_removal() {
        let config = FormatterConfig::default();
        let mut writer = FormattedWriter::new(&config);

        writer.write("test   ");
        writer.write_newline();

        assert_eq!(writer.output(), "test\n");
    }

    #[test]
    fn test_operator_spacing() {
        let config = FormatterConfig { spaces_around_operators: true, ..Default::default() };
        let mut writer = FormattedWriter::new(&config);

        writer.write("x");
        writer.write_token(&Token::Operator { text: "+".to_string(), line: 1, col: 1 });
        writer.write("y");

        assert!(writer.output().contains(" + ") || writer.output().contains("x +y"));
    }

    #[test]
    fn test_delimiter_spacing() {
        let config = FormatterConfig::default();
        let mut writer = FormattedWriter::new(&config);

        writer.write_token(&Token::Identifier { text: "func".to_string(), line: 1, col: 0 });
        writer.write_token(&Token::Delimiter { text: "(".to_string(), line: 1, col: 4 });
        writer.write_token(&Token::Identifier { text: "x".to_string(), line: 1, col: 5 });
        writer.write_token(&Token::Delimiter { text: ")".to_string(), line: 1, col: 6 });

        assert_eq!(writer.output(), "func(x)");
    }

    #[test]
    fn test_comment_spacing() {
        let config = FormatterConfig::default();
        let mut writer = FormattedWriter::new(&config);

        writer.write("code");
        writer.write_token(&Token::Comment { text: "# comment".to_string(), line: 1, col: 10 });

        assert_eq!(writer.output(), "code  # comment");
    }

    #[test]
    fn test_indentation() {
        let config = FormatterConfig { indent_size: 4, ..Default::default() };
        let mut writer = FormattedWriter::new(&config);

        writer.write_token(&Token::Indent { level: 1, line: 1 });
        writer.write("code");
        assert_eq!(writer.output(), "    code");
    }

    #[test]
    fn test_blank_lines() {
        let config = FormatterConfig::default();
        let mut writer = FormattedWriter::new(&config);

        writer.write("line1\n");
        writer.ensure_blank_lines_before_definition(false, true);
        writer.write("def foo():");

        let output = writer.output();
        assert!(output.contains("\n\n\ndef foo():") || output.contains("line1\n\n\ndef"));
    }
}
