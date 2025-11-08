//! Formatting context tracking
//!
//! Tracks the current state during formatting, including indentation level,
//! line length, parenthesis depth, and other contextual information.

use super::config::FormatterConfig;

/// Context information during formatting
///
/// Tracks mutable state as the formatter processes tokens:
/// - Current indentation level
/// - Current line length
/// - Nesting depth (parentheses, brackets, braces)
/// - Whether we're inside a string, comment, etc.
#[derive(Debug, Clone)]
pub struct FormattingContext {
    /// Current indentation level (number of indent units)
    indent_level: usize,

    /// Current column position on the line
    current_column: usize,

    /// Current line number
    current_line: usize,

    /// Depth of nested parentheses
    paren_depth: usize,

    /// Depth of nested brackets
    bracket_depth: usize,

    /// Depth of nested braces
    brace_depth: usize,

    /// Whether we're inside a string literal
    in_string: bool,

    /// Whether we're inside a comment
    in_comment: bool,

    /// Whether the current line is blank
    is_blank_line: bool,

    /// Number of consecutive blank lines
    consecutive_blank_lines: usize,

    /// Whether we're at the start of a line
    at_line_start: bool,

    /// Configuration reference
    config: FormatterConfig,
}

impl FormattingContext {
    /// Create a new formatting context with the given configuration
    pub fn new(config: FormatterConfig) -> Self {
        Self {
            indent_level: 0,
            current_column: 0,
            current_line: 1,
            paren_depth: 0,
            bracket_depth: 0,
            brace_depth: 0,
            in_string: false,
            in_comment: false,
            is_blank_line: true,
            consecutive_blank_lines: 0,
            at_line_start: true,
            config,
        }
    }

    /// Get the current indentation level
    pub fn indent_level(&self) -> usize {
        self.indent_level
    }

    /// Increase indentation level
    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    /// Decrease indentation level
    pub fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    /// Set indentation level explicitly
    pub fn set_indent(&mut self, level: usize) {
        self.indent_level = level;
    }

    /// Get the current column position
    pub fn current_column(&self) -> usize {
        self.current_column
    }

    /// Add to the current column position
    pub fn advance_column(&mut self, count: usize) {
        self.current_column += count;
        if count > 0 {
            self.at_line_start = false;
            self.is_blank_line = false;
        }
    }

    /// Get the current line number
    pub fn current_line(&self) -> usize {
        self.current_line
    }

    /// Move to a new line
    pub fn newline(&mut self) {
        self.current_line += 1;
        self.current_column = 0;
        self.at_line_start = true;

        if self.is_blank_line {
            self.consecutive_blank_lines += 1;
        } else {
            self.consecutive_blank_lines = 0;
        }

        self.is_blank_line = true;
    }

    /// Check if we're at the start of a line
    pub fn at_line_start(&self) -> bool {
        self.at_line_start
    }

    /// Get the number of consecutive blank lines
    pub fn consecutive_blank_lines(&self) -> usize {
        self.consecutive_blank_lines
    }

    /// Reset consecutive blank line counter
    pub fn reset_blank_lines(&mut self) {
        self.consecutive_blank_lines = 0;
    }

    /// Check if adding text would exceed line length
    pub fn would_exceed_line_length(&self, text_len: usize) -> bool {
        self.current_column + text_len > self.config.line_length
    }

    /// Get the remaining space on the current line
    pub fn remaining_line_space(&self) -> usize {
        self.config.line_length.saturating_sub(self.current_column)
    }

    /// Get total nesting depth (parens + brackets + braces)
    pub fn nesting_depth(&self) -> usize {
        self.paren_depth + self.bracket_depth + self.brace_depth
    }

    /// Check if we're inside any nesting construct
    pub fn is_nested(&self) -> bool {
        self.nesting_depth() > 0
    }

    /// Enter a parenthesis
    pub fn enter_paren(&mut self) {
        self.paren_depth += 1;
    }

    /// Exit a parenthesis
    pub fn exit_paren(&mut self) {
        if self.paren_depth > 0 {
            self.paren_depth -= 1;
        }
    }

    /// Get parenthesis depth
    pub fn paren_depth(&self) -> usize {
        self.paren_depth
    }

    /// Enter a bracket
    pub fn enter_bracket(&mut self) {
        self.bracket_depth += 1;
    }

    /// Exit a bracket
    pub fn exit_bracket(&mut self) {
        if self.bracket_depth > 0 {
            self.bracket_depth -= 1;
        }
    }

    /// Get bracket depth
    pub fn bracket_depth(&self) -> usize {
        self.bracket_depth
    }

    /// Enter a brace
    pub fn enter_brace(&mut self) {
        self.brace_depth += 1;
    }

    /// Exit a brace
    pub fn exit_brace(&mut self) {
        if self.brace_depth > 0 {
            self.brace_depth -= 1;
        }
    }

    /// Get brace depth
    pub fn brace_depth(&self) -> usize {
        self.brace_depth
    }

    /// Enter a string literal
    pub fn enter_string(&mut self) {
        self.in_string = true;
    }

    /// Exit a string literal
    pub fn exit_string(&mut self) {
        self.in_string = false;
    }

    /// Check if we're inside a string
    pub fn in_string(&self) -> bool {
        self.in_string
    }

    /// Enter a comment
    pub fn enter_comment(&mut self) {
        self.in_comment = true;
    }

    /// Exit a comment
    pub fn exit_comment(&mut self) {
        self.in_comment = false;
    }

    /// Check if we're inside a comment
    pub fn in_comment(&self) -> bool {
        self.in_comment
    }

    /// Get the indent string for the current level
    pub fn indent_string(&self) -> String {
        self.config.indent_string().repeat(self.indent_level)
    }

    /// Calculate the column position after writing indent
    pub fn indent_column(&self) -> usize {
        self.indent_string().len()
    }

    /// Get a reference to the configuration
    pub fn config(&self) -> &FormatterConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_creation() {
        let config = FormatterConfig::default();
        let ctx = FormattingContext::new(config);

        assert_eq!(ctx.indent_level(), 0);
        assert_eq!(ctx.current_column(), 0);
        assert_eq!(ctx.current_line(), 1);
        assert!(ctx.at_line_start());
    }

    #[test]
    fn test_indentation() {
        let config = FormatterConfig::default();
        let mut ctx = FormattingContext::new(config);

        ctx.indent();
        assert_eq!(ctx.indent_level(), 1);

        ctx.indent();
        assert_eq!(ctx.indent_level(), 2);

        ctx.dedent();
        assert_eq!(ctx.indent_level(), 1);

        ctx.dedent();
        assert_eq!(ctx.indent_level(), 0);

        ctx.dedent();
        assert_eq!(ctx.indent_level(), 0);
    }

    #[test]
    fn test_column_tracking() {
        let config = FormatterConfig::default();
        let mut ctx = FormattingContext::new(config);

        assert_eq!(ctx.current_column(), 0);
        assert!(ctx.at_line_start());

        ctx.advance_column(5);
        assert_eq!(ctx.current_column(), 5);
        assert!(!ctx.at_line_start());

        ctx.newline();
        assert_eq!(ctx.current_column(), 0);
        assert!(ctx.at_line_start());
    }

    #[test]
    fn test_line_length_check() {
        let config = FormatterConfig { line_length: 80, ..Default::default() };
        let mut ctx = FormattingContext::new(config);

        ctx.advance_column(70);
        assert!(!ctx.would_exceed_line_length(5));
        assert!(ctx.would_exceed_line_length(15));
        assert_eq!(ctx.remaining_line_space(), 10);
    }

    #[test]
    fn test_nesting_depth() {
        let config = FormatterConfig::default();
        let mut ctx = FormattingContext::new(config);

        assert_eq!(ctx.nesting_depth(), 0);
        assert!(!ctx.is_nested());

        ctx.enter_paren();
        assert_eq!(ctx.paren_depth(), 1);
        assert_eq!(ctx.nesting_depth(), 1);
        assert!(ctx.is_nested());

        ctx.enter_bracket();
        assert_eq!(ctx.bracket_depth(), 1);
        assert_eq!(ctx.nesting_depth(), 2);

        ctx.exit_paren();
        assert_eq!(ctx.nesting_depth(), 1);

        ctx.exit_bracket();
        assert_eq!(ctx.nesting_depth(), 0);
        assert!(!ctx.is_nested());
    }

    #[test]
    fn test_string_and_comment_tracking() {
        let config = FormatterConfig::default();
        let mut ctx = FormattingContext::new(config);

        assert!(!ctx.in_string());
        assert!(!ctx.in_comment());

        ctx.enter_string();
        assert!(ctx.in_string());

        ctx.exit_string();
        assert!(!ctx.in_string());

        ctx.enter_comment();
        assert!(ctx.in_comment());

        ctx.exit_comment();
        assert!(!ctx.in_comment());
    }

    #[test]
    fn test_blank_line_tracking() {
        let config = FormatterConfig::default();
        let mut ctx = FormattingContext::new(config);

        assert_eq!(ctx.consecutive_blank_lines(), 0);

        ctx.newline();
        assert_eq!(ctx.consecutive_blank_lines(), 1);

        ctx.newline();
        assert_eq!(ctx.consecutive_blank_lines(), 2);

        ctx.advance_column(5);
        ctx.newline();
        assert_eq!(ctx.consecutive_blank_lines(), 0);
    }

    #[test]
    fn test_indent_string() {
        let config = FormatterConfig { indent_size: 4, ..Default::default() };
        let mut ctx = FormattingContext::new(config);

        assert_eq!(ctx.indent_string(), "");

        ctx.indent();
        assert_eq!(ctx.indent_string(), "    ");

        ctx.indent();
        assert_eq!(ctx.indent_string(), "        ");
    }
}
