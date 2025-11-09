//! Formatting rules implementation
//!
//! Implements PEP8 formatting rules for whitespace, indentation,
//! line length, and line breaking.

use super::config::FormatterConfig;
use super::context::FormattingContext;
use super::token_stream::Token;
use unicode_width::UnicodeWidthStr;

/// Line breaking decision
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakDecision {
    /// Must break here
    MustBreak,
    /// Can break here if needed
    CanBreak,
    /// Should not break here
    NoBreak,
}

/// Rules for formatting Python code
pub struct FormattingRules {
    config: FormatterConfig,
}

impl FormattingRules {
    /// Create new formatting rules with the given configuration
    pub fn new(config: FormatterConfig) -> Self {
        Self { config }
    }

    /// Calculate the display width of text (handles multi-byte Unicode)
    ///
    /// Uses Unicode width calculation to properly handle characters
    /// that occupy more than one column (e.g., emoji, CJK characters).
    pub fn text_width(&self, text: &str) -> usize {
        UnicodeWidthStr::width(text)
    }

    /// Check if a line would exceed the configured line length
    pub fn would_exceed_line_length(&self, current_col: usize, additional_text: &str) -> bool {
        current_col + self.text_width(additional_text) > self.config.line_length
    }

    /// Determine if we should break before a token
    pub fn should_break_before(&self, token: &Token, context: &FormattingContext) -> BreakDecision {
        if context.in_string() || context.in_comment() {
            return BreakDecision::NoBreak;
        }

        match token {
            Token::Operator { text, .. } => {
                if self.is_breakable_operator(text) {
                    if context.is_nested() { BreakDecision::CanBreak } else { BreakDecision::NoBreak }
                } else {
                    BreakDecision::NoBreak
                }
            }

            Token::Delimiter { text, .. } if text == "," => BreakDecision::CanBreak,

            Token::Delimiter { text, .. } if matches!(text.as_str(), "(" | "[" | "{") => {
                if context.is_nested() && context.nesting_depth() > 1 {
                    BreakDecision::CanBreak
                } else {
                    BreakDecision::NoBreak
                }
            }

            _ => BreakDecision::NoBreak,
        }
    }

    /// Determine the indentation level for a wrapped line
    pub fn wrapped_line_indent(&self, context: &FormattingContext, base_indent: usize) -> usize {
        if context.is_nested() { base_indent + 1 } else { base_indent }
    }

    /// Check if an operator is suitable for line breaking
    fn is_breakable_operator(&self, operator: &str) -> bool {
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
                | "&"
                | "|"
                | "^"
                | "<<"
                | ">>"
        )
    }

    /// Calculate the preferred break point in a sequence of tokens
    ///
    /// Returns the index of the token before which we should break,
    /// or None if no suitable break point is found.
    pub fn find_best_break_point(
        &self, tokens: &[Token], context: &FormattingContext, max_width: usize,
    ) -> Option<usize> {
        let mut current_width = context.current_column();
        let mut best_break = None;
        let mut best_break_priority = 0;

        for (idx, token) in tokens.iter().enumerate() {
            let token_width = if let Some(text) = token.text() { self.text_width(text) } else { 0 };

            current_width += token_width;

            if current_width > max_width {
                return best_break.or(Some(idx));
            }

            let (can_break, priority) = self.break_priority(token);
            if can_break && priority > best_break_priority {
                best_break = Some(idx);
                best_break_priority = priority;
            }
        }

        None
    }

    /// Get the priority of breaking at this token
    fn break_priority(&self, token: &Token) -> (bool, u8) {
        match token {
            Token::Delimiter { text, .. } if text == "," => (true, 10),
            Token::Operator { text, .. } if self.is_breakable_operator(text) => (true, 5),
            Token::Delimiter { text, .. } if matches!(text.as_str(), "(" | "[" | "{") => (true, 3),
            _ => (false, 0),
        }
    }

    /// Check if we should preserve a user-inserted line break
    pub fn should_preserve_line_break(&self, context: &FormattingContext) -> bool {
        context.current_column() < self.config.line_length
    }

    /// Calculate indentation for function/method parameters
    pub fn parameter_indentation(&self, context: &FormattingContext, is_hanging: bool) -> usize {
        if is_hanging { context.current_column() } else { context.indent_level() + 1 }
    }

    /// Calculate indentation for collection literals
    pub fn collection_indentation(&self, context: &FormattingContext, is_hanging: bool) -> usize {
        if is_hanging { context.current_column() } else { context.indent_level() + 1 }
    }

    /// Determine wrapping strategy for a function call
    pub fn function_call_wrapping_strategy(&self, args_width: usize, context: &FormattingContext) -> WrappingStrategy {
        let available = context.remaining_line_space();

        if args_width <= available {
            WrappingStrategy::Horizontal
        } else if args_width > self.config.line_length / 2 {
            WrappingStrategy::Vertical
        } else {
            WrappingStrategy::Mixed
        }
    }

    /// Determine wrapping strategy for a collection literal
    pub fn collection_wrapping_strategy(&self, elements_width: usize, context: &FormattingContext) -> WrappingStrategy {
        let available = context.remaining_line_space();
        if elements_width <= available { WrappingStrategy::Horizontal } else { WrappingStrategy::Vertical }
    }

    /// Get the configuration
    pub fn config(&self) -> &FormatterConfig {
        &self.config
    }
}

/// Strategy for wrapping multi-line constructs
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WrappingStrategy {
    /// All on one line
    Horizontal,
    /// One element per line
    Vertical,
    /// Multiple elements per line, break at comma boundaries
    Mixed,
}

/// Blank line rules
impl FormattingRules {
    /// Get required number of blank lines before a class definition
    pub fn blank_lines_before_class(&self, is_top_level: bool) -> usize {
        if is_top_level && self.config.blank_line_before_class {
            2
        } else if !is_top_level {
            1
        } else {
            0
        }
    }

    /// Get required number of blank lines before a function definition
    pub fn blank_lines_before_function(&self, is_top_level: bool, is_method: bool) -> usize {
        if is_top_level && self.config.blank_line_before_function {
            2
        } else if is_method {
            1
        } else {
            0
        }
    }

    /// Check if we've exceeded the maximum consecutive blank lines
    pub fn exceeds_max_blank_lines(&self, count: usize) -> bool {
        count > self.config.max_blank_lines
    }
}

/// String formatting rules
impl FormattingRules {
    /// Normalize string quotes according to configuration
    ///
    /// Converts string quotes to the configured style (single, double, or preserve).
    /// Handles edge cases like strings containing quotes and f-strings.
    pub fn normalize_quotes(&self, text: &str) -> String {
        use super::config::QuoteStyle;

        if self.config.quote_style == QuoteStyle::Preserve {
            return text.to_string();
        }

        let is_triple_quoted = text.starts_with("\"\"\"")
            || text.starts_with("'''")
            || text.starts_with("r\"\"\"")
            || text.starts_with("r'''")
            || text.starts_with("f\"\"\"")
            || text.starts_with("f'''");

        if is_triple_quoted {
            return self.normalize_triple_quoted_string(text);
        }

        let (prefix, rest) = self.extract_string_prefix(text);

        let current_quote = if rest.starts_with('\'') {
            '\''
        } else if rest.starts_with('"') {
            '"'
        } else {
            return text.to_string();
        };

        let target_quote = match self.config.quote_style {
            QuoteStyle::Single => '\'',
            QuoteStyle::Double => '"',
            QuoteStyle::Preserve => return text.to_string(),
        };

        if current_quote == target_quote {
            return text.to_string();
        }

        let content = &rest[1..rest.len() - 1];

        if content.contains(target_quote) && !content.contains(current_quote) {
            return text.to_string();
        }

        format!("{prefix}{target_quote}{content}{target_quote}")
    }

    /// Extract string prefix (r, f, rf, etc.)
    fn extract_string_prefix<'a>(&self, text: &'a str) -> (&'a str, &'a str) {
        let prefixes = ["rf", "fr", "rb", "br", "r", "f", "b", "u"];

        for prefix in &prefixes {
            if text.to_lowercase().starts_with(prefix) {
                return (&text[..prefix.len()], &text[prefix.len()..]);
            }
        }

        ("", text)
    }

    /// Normalize triple-quoted strings (docstrings)
    fn normalize_triple_quoted_string(&self, text: &str) -> String {
        use super::config::QuoteStyle;

        if !self.config.normalize_docstring_quotes {
            return text.to_string();
        }

        let (prefix, rest) = self.extract_string_prefix(text);

        let current_triple = if rest.starts_with("'''") {
            "'''"
        } else if rest.starts_with("\"\"\"") {
            "\"\"\""
        } else {
            return text.to_string();
        };

        let target_triple = match self.config.quote_style {
            QuoteStyle::Single => "'''",
            QuoteStyle::Double => "\"\"\"",
            QuoteStyle::Preserve => return text.to_string(),
        };

        if current_triple == target_triple {
            return text.to_string();
        }

        let content = &rest[3..rest.len() - 3];

        if content.contains(target_triple) {
            return text.to_string();
        }

        format!("{prefix}{target_triple}{content}{target_triple}")
    }

    /// Check if a string is a docstring
    ///
    /// A docstring is the first statement in a module, function, class, or method.
    pub fn is_docstring(&self, text: &str) -> bool {
        text.starts_with("\"\"\"") || text.starts_with("'''")
    }

    /// Format indentation for a triple-quoted string
    ///
    /// Ensures consistent indentation inside multi-line docstrings.
    pub fn format_docstring_indentation(&self, text: &str, base_indent: usize) -> String {
        let lines: Vec<&str> = text.lines().collect();

        if lines.len() <= 1 {
            return text.to_string();
        }

        let indent = " ".repeat(base_indent);
        let mut result = String::new();

        result.push_str(lines[0]);
        result.push('\n');

        for line in &lines[1..] {
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                result.push_str(&indent);
                result.push_str(trimmed);
            }
            result.push('\n');
        }

        result
    }
}

/// Structural formatting rules
impl FormattingRules {
    /// Determine if a trailing comma should be added to a multi-line structure
    pub fn should_add_trailing_comma(&self, is_multiline: bool, context: &FormattingContext) -> bool {
        use super::config::TrailingCommas;

        match self.config.trailing_commas {
            TrailingCommas::Always => true,
            TrailingCommas::Multiline => is_multiline && context.is_nested(),
            TrailingCommas::Never => false,
        }
    }

    /// Format decorator spacing
    ///
    /// Ensures each decorator is on its own line with proper indentation.
    pub fn format_decorator(&self, decorator_text: &str) -> String {
        let trimmed = decorator_text.trim();
        if !trimmed.starts_with('@') { format!("@{trimmed}") } else { trimmed.to_string() }
    }

    /// Calculate spacing around type annotations
    ///
    /// Determines the appropriate spacing for type annotations (e.g., x: int, -> str).
    pub fn type_annotation_spacing(&self) -> (usize, usize) {
        (0, 1)
    }

    /// Determine if a lambda should be wrapped to multiple lines
    pub fn should_wrap_lambda(&self, lambda_width: usize, context: &FormattingContext) -> bool {
        context.current_column() + lambda_width > self.config.line_length
    }

    /// Format dictionary key-value alignment
    ///
    /// Returns the preferred indentation for dictionary values in multi-line dicts.
    pub fn dict_value_indent(&self, context: &FormattingContext, key_width: usize) -> usize {
        if context.is_nested() { context.indent_level() + 1 } else { key_width + 2 }
    }

    /// Determine wrapping strategy for comprehensions
    pub fn comprehension_wrapping_strategy(&self, total_width: usize, context: &FormattingContext) -> WrappingStrategy {
        let available = context.remaining_line_space();
        if total_width <= available { WrappingStrategy::Horizontal } else { WrappingStrategy::Vertical }
    }

    /// Get required blank lines before a class definition
    pub fn blank_lines_for_class(&self, context: &FormattingContext) -> usize {
        let is_top_level = context.indent_level() == 0;
        self.blank_lines_before_class(is_top_level)
    }

    /// Get required blank lines before a function definition
    pub fn blank_lines_for_function(&self, context: &FormattingContext, is_method: bool) -> usize {
        let is_top_level = context.indent_level() == 0;
        self.blank_lines_before_function(is_top_level, is_method)
    }
}

/// Comment formatting rules
impl FormattingRules {
    /// Format a comment with appropriate spacing
    ///
    /// Ensures comments have proper spacing and alignment.
    pub fn format_comment(&self, comment: &str, _is_inline: bool) -> String {
        let trimmed = comment.trim();

        if !trimmed.starts_with('#') {
            return comment.to_string();
        }

        let content = trimmed[1..].trim_start();
        if content.is_empty() { "#".to_string() } else { format!("# {content}") }
    }

    /// Check if a comment should be preserved as-is
    ///
    /// Some special comments (like type: ignore, noqa) should be preserved exactly.
    pub fn should_preserve_comment(&self, comment: &str) -> bool {
        let trimmed = comment.trim();

        trimmed.contains("type: ignore")
            || trimmed.contains("noqa")
            || trimmed.contains("pylint:")
            || trimmed.contains("mypy:")
            || trimmed.contains("flake8:")
            || trimmed.starts_with("# fmt:")
            || trimmed.starts_with("# black:")
    }

    /// Determine if a block comment should have blank lines around it
    pub fn needs_blank_lines_around_comment(&self, comment: &str, context: &FormattingContext) -> bool {
        context.indent_level() == 0 && comment.lines().count() > 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::formatting::config::{QuoteStyle, TrailingCommas};

    #[test]
    fn test_text_width_ascii() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        assert_eq!(rules.text_width("hello"), 5);
        assert_eq!(rules.text_width(""), 0);
    }

    #[test]
    fn test_text_width_unicode() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        assert!(rules.text_width("你好") >= 2);
    }

    #[test]
    fn test_line_length_check() {
        let config = FormatterConfig { line_length: 80, ..Default::default() };
        let rules = FormattingRules::new(config);
        assert!(!rules.would_exceed_line_length(70, "hello"));
        assert!(rules.would_exceed_line_length(70, "this is a very long string"));
    }

    #[test]
    fn test_break_before_operator() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config.clone());
        let mut context = FormattingContext::new(&config);
        let token = Token::Operator { text: "+".to_string(), line: 1, col: 10 };

        assert_eq!(rules.should_break_before(&token, &context), BreakDecision::NoBreak);

        context.enter_paren();
        assert_eq!(rules.should_break_before(&token, &context), BreakDecision::CanBreak);
    }

    #[test]
    fn test_break_priority() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        let comma = Token::Delimiter { text: ",".to_string(), line: 1, col: 10 };
        let (can_break, priority) = rules.break_priority(&comma);
        assert!(can_break);
        assert_eq!(priority, 10);

        let plus = Token::Operator { text: "+".to_string(), line: 1, col: 10 };
        let (can_break, priority) = rules.break_priority(&plus);
        assert!(can_break);
        assert_eq!(priority, 5);
    }

    #[test]
    fn test_blank_lines_before_class() {
        let config = FormatterConfig { blank_line_before_class: true, ..Default::default() };
        let rules = FormattingRules::new(config);
        assert_eq!(rules.blank_lines_before_class(true), 2);
        assert_eq!(rules.blank_lines_before_class(false), 1);
    }

    #[test]
    fn test_blank_lines_before_function() {
        let config = FormatterConfig { blank_line_before_function: true, ..Default::default() };
        let rules = FormattingRules::new(config);
        assert_eq!(rules.blank_lines_before_function(true, false), 2);
        assert_eq!(rules.blank_lines_before_function(false, true), 1);
        assert_eq!(rules.blank_lines_before_function(false, false), 0);
    }

    #[test]
    fn test_max_blank_lines() {
        let config = FormatterConfig { max_blank_lines: 2, ..Default::default() };
        let rules = FormattingRules::new(config);
        assert!(!rules.exceeds_max_blank_lines(1));
        assert!(!rules.exceeds_max_blank_lines(2));
        assert!(rules.exceeds_max_blank_lines(3));
    }

    #[test]
    fn test_wrapping_strategy_horizontal() {
        let config = FormatterConfig { line_length: 80, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let mut context = FormattingContext::new(&config);

        context.advance_column(10);
        let strategy = rules.function_call_wrapping_strategy(20, &context);

        assert_eq!(strategy, WrappingStrategy::Horizontal);
    }

    #[test]
    fn test_wrapping_strategy_vertical() {
        let config = FormatterConfig { line_length: 80, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let mut context = FormattingContext::new(&config);

        context.advance_column(10);
        let strategy = rules.function_call_wrapping_strategy(100, &context);

        assert_eq!(strategy, WrappingStrategy::Vertical);
    }

    #[test]
    fn test_find_best_break_point() {
        let config = FormatterConfig { line_length: 80, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let context = FormattingContext::new(&config);

        let tokens = vec![
            Token::Identifier { text: "x".to_string(), line: 1, col: 0 },
            Token::Operator { text: "+".to_string(), line: 1, col: 2 },
            Token::Identifier { text: "y".to_string(), line: 1, col: 4 },
            Token::Delimiter { text: ",".to_string(), line: 1, col: 5 },
            Token::Identifier { text: "z".to_string(), line: 1, col: 7 },
        ];

        let break_point = rules.find_best_break_point(&tokens, &context, 80);
        assert!(break_point.is_none() || break_point == Some(4));
    }

    #[test]
    fn test_normalize_quotes_double_to_single() {
        let config = FormatterConfig { quote_style: QuoteStyle::Single, ..Default::default() };
        let rules = FormattingRules::new(config);
        assert_eq!(rules.normalize_quotes(r#""hello""#), "'hello'");
        assert_eq!(rules.normalize_quotes(r#""world""#), "'world'");
    }

    #[test]
    fn test_normalize_quotes_single_to_double() {
        let config = FormatterConfig { quote_style: QuoteStyle::Double, ..Default::default() };
        let rules = FormattingRules::new(config);
        assert_eq!(rules.normalize_quotes("'hello'"), r#""hello""#);
        assert_eq!(rules.normalize_quotes("'world'"), r#""world""#);
    }

    #[test]
    fn test_normalize_quotes_preserve() {
        let config = FormatterConfig { quote_style: QuoteStyle::Preserve, ..Default::default() };
        let rules = FormattingRules::new(config);
        assert_eq!(rules.normalize_quotes(r#""hello""#), r#""hello""#);
        assert_eq!(rules.normalize_quotes("'world'"), "'world'");
    }

    #[test]
    fn test_normalize_quotes_with_prefix() {
        let config = FormatterConfig { quote_style: QuoteStyle::Double, ..Default::default() };
        let rules = FormattingRules::new(config);
        assert_eq!(rules.normalize_quotes("r'hello'"), r#"r"hello""#);
        assert_eq!(rules.normalize_quotes("f'world'"), r#"f"world""#);
        assert_eq!(rules.normalize_quotes("rf'test'"), r#"rf"test""#);
    }

    #[test]
    fn test_normalize_quotes_avoid_escaping() {
        let config = FormatterConfig { quote_style: QuoteStyle::Double, ..Default::default() };
        let rules = FormattingRules::new(config);
        assert_eq!(rules.normalize_quotes(r#"'He said "hello"'"#), r#"'He said "hello"'"#);
    }

    #[test]
    fn test_normalize_triple_quoted_string() {
        let config = FormatterConfig { quote_style: QuoteStyle::Double, ..Default::default() };
        let rules = FormattingRules::new(config);
        assert_eq!(rules.normalize_quotes("'''docstring'''"), r#""""docstring""""#);
    }

    #[test]
    fn test_normalize_triple_quoted_preserve_if_contains_target() {
        let config = FormatterConfig { quote_style: QuoteStyle::Double, ..Default::default() };
        let rules = FormattingRules::new(config);
        let input = r#"'''He said """hello""" to me'''"#;
        assert_eq!(rules.normalize_quotes(input), input);
    }

    #[test]
    fn test_is_docstring() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        assert!(rules.is_docstring(r#""""This is a docstring""""#));
        assert!(rules.is_docstring("'''This is also a docstring'''"));
        assert!(!rules.is_docstring(r#""Regular string""#));
        assert!(!rules.is_docstring("'Regular string'"));
    }

    #[test]
    fn test_format_docstring_indentation() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        let docstring = r#""""
This is a docstring
    with mixed indentation
        and multiple levels
""""#;

        let formatted = rules.format_docstring_indentation(docstring, 4);
        assert!(formatted.contains("    This is a docstring"));
        assert!(formatted.contains("    with mixed indentation"));
    }

    #[test]
    fn test_extract_string_prefix() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        assert_eq!(rules.extract_string_prefix(r#"r"hello""#), ("r", r#""hello""#));
        assert_eq!(rules.extract_string_prefix(r#"f"world""#), ("f", r#""world""#));
        assert_eq!(rules.extract_string_prefix(r#"rf"test""#), ("rf", r#""test""#));
        assert_eq!(rules.extract_string_prefix(r#"FR"test""#), ("FR", r#""test""#));
        assert_eq!(rules.extract_string_prefix(r#""plain""#), ("", r#""plain""#));
    }

    #[test]
    fn test_format_comment_basic() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        assert_eq!(rules.format_comment("#comment", false), "# comment");
        assert_eq!(rules.format_comment("#  comment", false), "# comment");
        assert_eq!(rules.format_comment("# comment ", false), "# comment");
    }

    #[test]
    fn test_format_comment_empty() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        assert_eq!(rules.format_comment("#", false), "#");
        assert_eq!(rules.format_comment("#  ", false), "#");
    }

    #[test]
    fn test_should_preserve_comment() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        assert!(rules.should_preserve_comment("# type: ignore"));
        assert!(rules.should_preserve_comment("# noqa: E501"));
        assert!(rules.should_preserve_comment("# pylint: disable=line-too-long"));
        assert!(rules.should_preserve_comment("# mypy: ignore"));
        assert!(rules.should_preserve_comment("# fmt: off"));
        assert!(!rules.should_preserve_comment("# regular comment"));
    }

    #[test]
    fn test_needs_blank_lines_around_comment() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config.clone());
        let context = FormattingContext::new(&config);
        let block_comment = "# This is a block comment\n# that spans multiple lines";
        assert!(rules.needs_blank_lines_around_comment(block_comment, &context));

        let inline_comment = "# single line";
        assert!(!rules.needs_blank_lines_around_comment(inline_comment, &context));
    }

    #[test]
    fn test_trailing_comma_always() {
        let config = FormatterConfig { trailing_commas: TrailingCommas::Always, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let context = FormattingContext::new(&config);
        assert!(rules.should_add_trailing_comma(true, &context));
        assert!(rules.should_add_trailing_comma(false, &context));
    }

    #[test]
    fn test_trailing_comma_multiline() {
        let config = FormatterConfig { trailing_commas: TrailingCommas::Multiline, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let mut context = FormattingContext::new(&config);
        context.enter_paren();
        assert!(rules.should_add_trailing_comma(true, &context));
        assert!(!rules.should_add_trailing_comma(false, &context));
    }

    #[test]
    fn test_trailing_comma_never() {
        let config = FormatterConfig { trailing_commas: TrailingCommas::Never, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let context = FormattingContext::new(&config);
        assert!(!rules.should_add_trailing_comma(true, &context));
        assert!(!rules.should_add_trailing_comma(false, &context));
    }

    #[test]
    fn test_format_decorator() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        assert_eq!(rules.format_decorator("@property"), "@property");
        assert_eq!(rules.format_decorator("property"), "@property");
        assert_eq!(rules.format_decorator("  @decorator  "), "@decorator");
    }

    #[test]
    fn test_type_annotation_spacing() {
        let config = FormatterConfig::default();
        let rules = FormattingRules::new(config);
        let (before, after) = rules.type_annotation_spacing();
        assert_eq!(before, 0);
        assert_eq!(after, 1);
    }

    #[test]
    fn test_should_wrap_lambda() {
        let config = FormatterConfig { line_length: 80, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let mut context = FormattingContext::new(&config);
        context.advance_column(70);
        assert!(!rules.should_wrap_lambda(5, &context));
        assert!(rules.should_wrap_lambda(20, &context));
    }

    #[test]
    fn test_dict_value_indent() {
        let config = FormatterConfig { indent_size: 4, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let mut context = FormattingContext::new(&config);
        context.enter_brace();
        let indent = rules.dict_value_indent(&context, 10);
        assert_eq!(indent, 1);
    }

    #[test]
    fn test_comprehension_wrapping_horizontal() {
        let config = FormatterConfig { line_length: 80, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let mut context = FormattingContext::new(&config);
        context.advance_column(10);
        let strategy = rules.comprehension_wrapping_strategy(30, &context);
        assert_eq!(strategy, WrappingStrategy::Horizontal);
    }

    #[test]
    fn test_comprehension_wrapping_vertical() {
        let config = FormatterConfig { line_length: 80, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let mut context = FormattingContext::new(&config);
        context.advance_column(10);
        let strategy = rules.comprehension_wrapping_strategy(100, &context);
        assert_eq!(strategy, WrappingStrategy::Vertical);
    }

    #[test]
    fn test_blank_lines_for_class() {
        let config = FormatterConfig { blank_line_before_class: true, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let context = FormattingContext::new(&config);
        assert_eq!(rules.blank_lines_for_class(&context), 2);
    }

    #[test]
    fn test_blank_lines_for_function() {
        let config = FormatterConfig { blank_line_before_function: true, ..Default::default() };
        let rules = FormattingRules::new(config.clone());
        let context = FormattingContext::new(&config);
        assert_eq!(rules.blank_lines_for_function(&context, false), 2);
        assert_eq!(rules.blank_lines_for_function(&context, true), 2);
    }
}
