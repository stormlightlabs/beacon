//! Token stream generation from AST
//!
//! Converts a parsed AST into a stream of formatting tokens.
//! Tokens represent syntactic elements with their associated metadata
//! (position, whitespace requirements, etc.).

use beacon_parser::AstNode;

/// Token type representing syntactic elements for formatting
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// Keyword (if, def, class, etc.)
    Keyword { text: String, line: usize, col: usize },
    /// Identifier (variable name, function name, etc.)
    Identifier { text: String, line: usize, col: usize },
    /// Operator (+, -, *, /, etc.)
    Operator { text: String, line: usize, col: usize },
    /// Delimiter (parenthesis, bracket, brace, comma, colon)
    Delimiter { text: String, line: usize, col: usize },
    /// String literal
    StringLiteral {
        text: String,
        line: usize,
        col: usize,
        quote_char: char,
    },
    /// Number literal
    NumberLiteral { text: String, line: usize, col: usize },
    /// Comment
    Comment { text: String, line: usize, col: usize },
    /// Newline
    Newline { line: usize },
    /// Indentation change
    Indent { level: usize, line: usize },
    /// Dedent
    Dedent { level: usize, line: usize },
    /// Whitespace (used for preserving specific spacing)
    Whitespace { count: usize, line: usize, col: usize },
}

impl Token {
    /// Get the line number of this token
    pub fn line(&self) -> usize {
        match self {
            Token::Keyword { line, .. }
            | Token::Identifier { line, .. }
            | Token::Operator { line, .. }
            | Token::Delimiter { line, .. }
            | Token::StringLiteral { line, .. }
            | Token::NumberLiteral { line, .. }
            | Token::Comment { line, .. }
            | Token::Newline { line }
            | Token::Indent { line, .. }
            | Token::Dedent { line, .. }
            | Token::Whitespace { line, .. } => *line,
        }
    }

    /// Get the column number of this token, if applicable
    pub fn col(&self) -> Option<usize> {
        match self {
            Token::Keyword { col, .. }
            | Token::Identifier { col, .. }
            | Token::Operator { col, .. }
            | Token::Delimiter { col, .. }
            | Token::StringLiteral { col, .. }
            | Token::NumberLiteral { col, .. }
            | Token::Comment { col, .. }
            | Token::Whitespace { col, .. } => Some(*col),
            Token::Newline { .. } | Token::Indent { .. } | Token::Dedent { .. } => None,
        }
    }

    /// Get the text content of this token, if applicable
    pub fn text(&self) -> Option<&str> {
        match self {
            Token::Keyword { text, .. }
            | Token::Identifier { text, .. }
            | Token::Operator { text, .. }
            | Token::Delimiter { text, .. }
            | Token::StringLiteral { text, .. }
            | Token::NumberLiteral { text, .. }
            | Token::Comment { text, .. } => Some(text),
            Token::Newline { .. } | Token::Indent { .. } | Token::Dedent { .. } | Token::Whitespace { .. } => None,
        }
    }
}

/// Iterator over tokens generated from an AST
pub struct TokenStream {
    tokens: Vec<Token>,
    position: usize,
}

impl TokenStream {
    /// Create a new token stream from an AST node
    ///
    /// Traverses the AST and generates a sequence of formatting tokens.
    pub fn from_ast(node: &AstNode) -> Self {
        let mut generator = TokenGenerator::new();
        generator.visit_node(node);
        Self { tokens: generator.tokens, position: 0 }
    }

    /// Create an empty token stream
    pub fn empty() -> Self {
        Self { tokens: Vec::new(), position: 0 }
    }

    /// Peek at the next token without consuming it
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    /// Peek ahead at the token at offset from current position
    pub fn peek_ahead(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.position + offset)
    }

    /// Check if we're at the end of the stream
    pub fn is_empty(&self) -> bool {
        self.position >= self.tokens.len()
    }

    /// Get the current position in the stream
    pub fn position(&self) -> usize {
        self.position
    }

    /// Reset the stream to the beginning
    pub fn reset(&mut self) {
        self.position = 0;
    }

    /// Get all remaining tokens as a slice
    pub fn remaining(&self) -> &[Token] {
        &self.tokens[self.position..]
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position < self.tokens.len() {
            let token = self.tokens[self.position].clone();
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }
}

/// Generates tokens from an AST
struct TokenGenerator {
    tokens: Vec<Token>,
    current_indent: usize,
}

impl TokenGenerator {
    fn new() -> Self {
        Self { tokens: Vec::new(), current_indent: 0 }
    }

    /// Visit an AST node and generate tokens
    fn visit_node(&mut self, node: &AstNode) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.visit_node(stmt);
                }
            }
            AstNode::FunctionDef { name, args, body, return_type, decorators, is_async, line, col, .. } => {
                for decorator in decorators {
                    self.tokens
                        .push(Token::Operator { text: "@".to_string(), line: *line, col: *col });
                    self.tokens
                        .push(Token::Identifier { text: decorator.clone(), line: *line, col: col + 1 });
                    self.tokens.push(Token::Newline { line: *line });
                }

                if *is_async {
                    self.tokens
                        .push(Token::Keyword { text: "async".to_string(), line: *line, col: *col });
                    self.tokens
                        .push(Token::Whitespace { count: 1, line: *line, col: col + 5 });
                }
                self.tokens
                    .push(Token::Keyword { text: "def".to_string(), line: *line, col: *col });
                self.tokens
                    .push(Token::Whitespace { count: 1, line: *line, col: col + 3 });
                self.tokens
                    .push(Token::Identifier { text: name.clone(), line: *line, col: col + 4 });

                self.tokens
                    .push(Token::Delimiter { text: "(".to_string(), line: *line, col: *col });
                for (i, param) in args.iter().enumerate() {
                    self.tokens
                        .push(Token::Identifier { text: param.name.clone(), line: param.line, col: param.col });
                    if i < args.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: param.line, col: param.col });
                    }
                }
                self.tokens
                    .push(Token::Delimiter { text: ")".to_string(), line: *line, col: *col });

                if let Some(ret_type) = return_type {
                    self.tokens
                        .push(Token::Operator { text: "->".to_string(), line: *line, col: *col });
                    self.tokens
                        .push(Token::Identifier { text: ret_type.clone(), line: *line, col: *col });
                }

                self.tokens
                    .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Newline { line: *line });

                self.current_indent += 1;
                for stmt in body {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: *line });
                    self.visit_node(stmt);
                }
                self.current_indent -= 1;
            }
            AstNode::Return { value, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "return".to_string(), line: *line, col: *col });
                if let Some(val) = value {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(val);
                }
                self.tokens.push(Token::Newline { line: *line });
            }
            // TODO: Implement token generation for other node types
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::{AstNode, Parameter};

    #[test]
    fn test_empty_token_stream() {
        let stream = TokenStream::empty();
        assert!(stream.is_empty());
    }

    #[test]
    fn test_token_stream_iteration() {
        let mut stream = TokenStream::empty();
        stream.tokens = vec![
            Token::Keyword { text: "def".to_string(), line: 1, col: 0 },
            Token::Identifier { text: "foo".to_string(), line: 1, col: 4 },
        ];
        stream.position = 0;

        assert!(!stream.is_empty());
        let first = stream.next();
        assert!(first.is_some());
        assert_eq!(first.unwrap().text(), Some("def"));
    }

    #[test]
    fn test_token_line_and_col() {
        let token = Token::Identifier { text: "test".to_string(), line: 5, col: 10 };
        assert_eq!(token.line(), 5);
        assert_eq!(token.col(), Some(10));
    }

    #[test]
    fn test_function_def_tokens() {
        let node = AstNode::FunctionDef {
            name: "test_func".to_string(),
            args: vec![Parameter {
                name: "x".to_string(),
                line: 1,
                col: 14,
                end_line: 1,
                end_col: 15,
                type_annotation: None,
                default_value: None,
            }],
            body: vec![AstNode::Return { value: None, line: 2, col: 4, end_line: 2, end_col: 10 }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 0,
            end_line: 2,
            end_col: 10,
        };

        let stream = TokenStream::from_ast(&node);
        assert!(!stream.is_empty());

        let tokens: Vec<Token> = stream.collect();
        assert!(!tokens.is_empty());
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "def"))
        );
    }
}
