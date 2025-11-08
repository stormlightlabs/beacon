//! Formatter state machine
//!
//! Tracks the current formatting state and transitions between different
//! formatting modes (e.g., statement mode, expression mode, string mode).

use super::context::FormattingContext;
use super::token_stream::Token;

/// Formatting state representing the current mode of the formatter
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatterState {
    /// At the start of a new line, before any content
    LineStart,

    /// Processing a statement (assignment, import, etc.)
    Statement,

    /// Processing an expression
    Expression,

    /// Inside a function/class definition
    Definition,

    /// Inside a parameter list
    Parameters,

    /// Inside a function call arguments
    Arguments,

    /// Inside a list/tuple/set literal
    Collection,

    /// Inside a dictionary literal
    Dictionary,

    /// Inside a string literal
    String,

    /// Inside a comment
    Comment,

    /// Processing an import statement
    Import,

    /// Inside a with statement context
    WithContext,

    /// Inside a decorator
    Decorator,

    /// End of line, after content
    LineEnd,
}

/// State machine for tracking formatting context
///
/// Manages transitions between different formatting states based on
/// the tokens being processed. Helps determine appropriate formatting
/// rules for each context.
#[derive(Debug)]
pub struct StateMachine {
    /// Current state
    current_state: FormatterState,

    /// Previous state (for potential backtracking)
    previous_state: Option<FormatterState>,

    /// State stack for nested constructs
    state_stack: Vec<FormatterState>,
}

impl Default for StateMachine {
    fn default() -> Self {
        Self { current_state: FormatterState::LineStart, previous_state: None, state_stack: Vec::new() }
    }
}

impl StateMachine {
    /// Create a new state machine starting at LineStart
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the current state
    pub fn current_state(&self) -> FormatterState {
        self.current_state
    }

    /// Get the previous state, if any
    pub fn previous_state(&self) -> Option<FormatterState> {
        self.previous_state
    }

    /// Push the current state onto the stack and transition to a new state
    /// when entering nested constructs like function calls or collections.
    pub fn push_state(&mut self, new_state: FormatterState) {
        self.state_stack.push(self.current_state);
        self.transition(new_state);
    }

    /// Pop the previous state from the stack and transition to it when exiting nested constructs.
    pub fn pop_state(&mut self) -> Option<FormatterState> {
        if let Some(state) = self.state_stack.pop() {
            self.transition(state);
            Some(state)
        } else {
            None
        }
    }

    /// Transition to a new state
    fn transition(&mut self, new_state: FormatterState) {
        self.previous_state = Some(self.current_state);
        self.current_state = new_state;
    }

    /// Process a token and update state accordingly
    pub fn process_token(&mut self, token: &Token, context: &FormattingContext) -> FormatterState {
        match token {
            Token::Keyword { text, .. } => self.handle_keyword(text, context),
            Token::Delimiter { text, .. } => self.handle_delimiter(text, context),
            Token::Newline { .. } => self.handle_newline(),
            Token::Indent { .. } => self.handle_indent(),
            Token::StringLiteral { .. } => self.handle_string(),
            Token::Comment { .. } => self.handle_comment(),
            Token::Operator { text, .. } if text == "@" => self.handle_decorator(),
            _ => self.current_state,
        }
    }

    /// Handle keyword tokens
    fn handle_keyword(&mut self, keyword: &str, _context: &FormattingContext) -> FormatterState {
        match keyword {
            "def" | "class" | "async" => {
                self.transition(FormatterState::Definition);
            }
            "import" | "from" => {
                self.transition(FormatterState::Import);
            }
            "with" => {
                self.transition(FormatterState::WithContext);
            }
            _ => {
                if self.current_state == FormatterState::LineStart {
                    self.transition(FormatterState::Statement);
                }
            }
        }
        self.current_state
    }

    /// Handle delimiter tokens
    fn handle_delimiter(&mut self, delimiter: &str, _context: &FormattingContext) -> FormatterState {
        match delimiter {
            "(" => {
                let new_state = match self.current_state {
                    FormatterState::Definition => FormatterState::Parameters,
                    _ => FormatterState::Arguments,
                };
                self.push_state(new_state);
            }
            ")" => {
                self.pop_state();
            }
            "[" => {
                self.push_state(FormatterState::Collection);
            }
            "]" => {
                self.pop_state();
            }
            "{" => {
                self.push_state(FormatterState::Dictionary);
            }
            "}" => {
                self.pop_state();
            }
            ":" => {
                if self.current_state == FormatterState::Definition || self.current_state == FormatterState::WithContext
                {
                    self.transition(FormatterState::Statement);
                }
            }
            _ => {}
        }
        self.current_state
    }

    /// Handle newline tokens
    fn handle_newline(&mut self) -> FormatterState {
        self.transition(FormatterState::LineEnd);
        self.transition(FormatterState::LineStart);
        self.current_state
    }

    /// Handle indent tokens
    fn handle_indent(&mut self) -> FormatterState {
        self.current_state
    }

    /// Handle string literal tokens
    fn handle_string(&mut self) -> FormatterState {
        if self.current_state == FormatterState::LineStart {
            self.transition(FormatterState::Expression);
        }
        self.current_state
    }

    /// Handle comment tokens
    fn handle_comment(&mut self) -> FormatterState {
        self.push_state(FormatterState::Comment);
        self.current_state
    }

    /// Handle decorator (@) tokens
    fn handle_decorator(&mut self) -> FormatterState {
        self.transition(FormatterState::Decorator);
        self.current_state
    }

    /// Check if we're in a nested context
    pub fn is_nested(&self) -> bool {
        !self.state_stack.is_empty()
    }

    /// Get the depth of nesting
    pub fn nesting_depth(&self) -> usize {
        self.state_stack.len()
    }

    /// Reset the state machine to initial state
    pub fn reset(&mut self) {
        self.current_state = FormatterState::LineStart;
        self.previous_state = None;
        self.state_stack.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::formatting::config::FormatterConfig;

    #[test]
    fn test_state_machine_creation() {
        let sm = StateMachine::new();
        assert_eq!(sm.current_state(), FormatterState::LineStart);
        assert_eq!(sm.previous_state(), None);
    }

    #[test]
    fn test_state_transitions() {
        let mut sm = StateMachine::new();
        let config = FormatterConfig::default();
        let context = FormattingContext::new(config);

        let token = Token::Keyword { text: "def".to_string(), line: 1, col: 0 };

        sm.process_token(&token, &context);
        assert_eq!(sm.current_state(), FormatterState::Definition);
        assert_eq!(sm.previous_state(), Some(FormatterState::LineStart));
    }

    #[test]
    fn test_nested_states() {
        let mut sm = StateMachine::new();
        let config = FormatterConfig::default();
        let context = FormattingContext::new(config);

        assert!(!sm.is_nested());
        assert_eq!(sm.nesting_depth(), 0);

        let open_paren = Token::Delimiter { text: "(".to_string(), line: 1, col: 4 };
        sm.process_token(&open_paren, &context);

        assert!(sm.is_nested());
        assert_eq!(sm.nesting_depth(), 1);
        assert_eq!(sm.current_state(), FormatterState::Arguments);

        let close_paren = Token::Delimiter { text: ")".to_string(), line: 1, col: 10 };
        sm.process_token(&close_paren, &context);

        assert!(!sm.is_nested());
        assert_eq!(sm.nesting_depth(), 0);
    }

    #[test]
    fn test_newline_transitions() {
        let mut sm = StateMachine::new();
        let config = FormatterConfig::default();
        let context = FormattingContext::new(config);

        sm.transition(FormatterState::Statement);

        let newline = Token::Newline { line: 1 };
        sm.process_token(&newline, &context);

        assert_eq!(sm.current_state(), FormatterState::LineStart);
    }

    #[test]
    fn test_import_state() {
        let mut sm = StateMachine::new();
        let config = FormatterConfig::default();
        let context = FormattingContext::new(config);

        let import_token = Token::Keyword { text: "import".to_string(), line: 1, col: 0 };

        sm.process_token(&import_token, &context);
        assert_eq!(sm.current_state(), FormatterState::Import);
    }

    #[test]
    fn test_reset() {
        let mut sm = StateMachine::new();

        sm.push_state(FormatterState::Expression);
        sm.push_state(FormatterState::Arguments);

        assert!(sm.is_nested());

        sm.reset();

        assert_eq!(sm.current_state(), FormatterState::LineStart);
        assert!(!sm.is_nested());
        assert_eq!(sm.previous_state(), None);
    }
}
