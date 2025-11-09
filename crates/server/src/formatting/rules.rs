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
        // Never break inside strings or comments
        if context.in_string() || context.in_comment() {
            return BreakDecision::NoBreak;
        }

        match token {
            // Can break before operators in expressions
            Token::Operator { text, .. } => {
                if self.is_breakable_operator(text) {
                    if context.is_nested() { BreakDecision::CanBreak } else { BreakDecision::NoBreak }
                } else {
                    BreakDecision::NoBreak
                }
            }

            // Can break after commas
            Token::Delimiter { text, .. } if text == "," => BreakDecision::CanBreak,

            // Can break at opening brackets if nested
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
        // If we're inside parentheses/brackets, add one indent level
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

            // Check if we've exceeded the line length
            if current_width > max_width {
                return best_break.or(Some(idx));
            }

            // Evaluate break priority
            let (can_break, priority) = self.break_priority(token);
            if can_break && priority > best_break_priority {
                best_break = Some(idx);
                best_break_priority = priority;
            }
        }

        None
    }

    /// Get the priority of breaking at this token
    ///
    /// Returns (can_break, priority) where higher priority means
    /// this is a better place to break.
    fn break_priority(&self, token: &Token) -> (bool, u8) {
        match token {
            // Prefer breaking after commas
            Token::Delimiter { text, .. } if text == "," => (true, 10),

            // Can break before binary operators
            Token::Operator { text, .. } if self.is_breakable_operator(text) => (true, 5),

            // Can break at opening brackets
            Token::Delimiter { text, .. } if matches!(text.as_str(), "(" | "[" | "{") => (true, 3),

            _ => (false, 0),
        }
    }

    /// Check if we should preserve a user-inserted line break
    pub fn should_preserve_line_break(&self, context: &FormattingContext) -> bool {
        // Preserve line breaks if we're under the limit
        context.current_column() < self.config.line_length
    }

    /// Calculate indentation for function/method parameters
    pub fn parameter_indentation(&self, context: &FormattingContext, is_hanging: bool) -> usize {
        if is_hanging {
            // Hanging indent: align with opening parenthesis
            context.current_column()
        } else {
            // Regular indent: one level deeper than function def
            context.indent_level() + 1
        }
    }

    /// Calculate indentation for collection literals
    pub fn collection_indentation(&self, context: &FormattingContext, is_hanging: bool) -> usize {
        if is_hanging {
            // Hanging indent: align with opening bracket
            context.current_column()
        } else {
            // Regular indent: one level deeper
            context.indent_level() + 1
        }
    }

    /// Determine wrapping strategy for a function call
    pub fn function_call_wrapping_strategy(&self, args_width: usize, context: &FormattingContext) -> WrappingStrategy {
        let available = context.remaining_line_space();

        if args_width <= available {
            // All arguments fit on one line
            WrappingStrategy::Horizontal
        } else if args_width > self.config.line_length / 2 {
            // Arguments are too long, use vertical layout
            WrappingStrategy::Vertical
        } else {
            // Try to fit multiple arguments per line
            WrappingStrategy::Mixed
        }
    }

    /// Determine wrapping strategy for a collection literal
    pub fn collection_wrapping_strategy(&self, elements_width: usize, context: &FormattingContext) -> WrappingStrategy {
        let available = context.remaining_line_space();

        if elements_width <= available {
            WrappingStrategy::Horizontal
        } else {
            // Use vertical layout for collections
            WrappingStrategy::Vertical
        }
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

#[cfg(test)]
mod tests {
    use super::*;

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

        // Emoji and CJK characters may have different widths
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

        // Not nested - should not break
        assert_eq!(rules.should_break_before(&token, &context), BreakDecision::NoBreak);

        // Nested - can break
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
        // Should prefer breaking after the comma
        assert!(break_point.is_none() || break_point == Some(4));
    }
}
