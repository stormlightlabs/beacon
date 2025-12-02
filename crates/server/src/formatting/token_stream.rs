//! Token stream generation from AST
//!
//! Converts a parsed AST into a stream of formatting tokens.
//! Tokens represent syntactic elements with their associated metadata (position, whitespace requirements, etc.).

use super::import::{ImportCategory, categorize_import};
use beacon_parser::{AstNode, LiteralValue, Pattern};

fn normalize_type_annotation(annotation: &str) -> String {
    if !annotation.contains(',') {
        return annotation.to_string();
    }

    let mut result = String::with_capacity(annotation.len());
    let mut chars = annotation.chars().peekable();
    let mut in_string = false;
    let mut current_quote = '\0';

    while let Some(ch) = chars.next() {
        match ch {
            '\'' | '"' => {
                if in_string {
                    result.push(ch);
                    if ch == current_quote {
                        in_string = false;
                    }
                } else {
                    in_string = true;
                    current_quote = ch;
                    result.push(ch);
                }
            }
            '\\' if in_string => {
                result.push(ch);
                if let Some(next) = chars.next() {
                    result.push(next);
                }
            }
            ',' if !in_string => {
                result.push(',');
                while matches!(chars.peek(), Some(' ')) {
                    chars.next();
                }

                match chars.peek() {
                    Some(')') | Some(']') | Some('}') | Some(',') => {}
                    _ => result.push(' '),
                }
            }
            _ => result.push(ch),
        }
    }

    result
}

/// Token type representing syntactic elements for formatting
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// Keyword (if, def, class, etc.)
    Keyword { text: String, line: usize, col: usize },
    /// Identifier (variable name, function name, etc.)
    Identifier { text: String, line: usize, col: usize },
    /// Operator (+, -, *, /, etc.)
    Operator { text: String, line: usize, col: usize },
    /// Keyword/default argument equal sign (no spaces)
    KeywordEqual { line: usize, col: usize },
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
    Comment {
        text: String,
        line: usize,
        col: usize,
        indent_hint: Option<usize>,
    },
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
            | Token::KeywordEqual { line, .. }
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
            | Token::KeywordEqual { col, .. }
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
            Token::KeywordEqual { .. } => Some("="),
            Token::Newline { .. } | Token::Indent { .. } | Token::Dedent { .. } | Token::Whitespace { .. } => None,
        }
    }
}

/// Iterator over tokens generated from an AST
pub struct TokenStream {
    tokens: Vec<Token>,
    position: usize,
}

/// Parsed comment metadata used for reinserting comments into the token stream
#[derive(Debug, Clone)]
pub struct Comment {
    /// 1-based line number of the comment
    pub line: usize,
    /// 1-based column where the comment starts
    pub col: usize,
    /// Raw comment text (e.g. "# comment")
    pub text: String,
    /// Indent level hint inferred from tree-sitter block nesting
    pub indent_level: usize,
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

    /// Create a token stream from an AST node and merge in comments collected from the source.
    pub fn from_ast_with_comments(node: &AstNode, comments: &[Comment]) -> Self {
        let mut generator = TokenGenerator::new();
        generator.visit_node(node);
        let tokens = if comments.is_empty() { generator.tokens } else { merge_comments(generator.tokens, comments) };

        Self { tokens, position: 0 }
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

fn merge_comments(mut tokens: Vec<Token>, comments: &[Comment]) -> Vec<Token> {
    if comments.is_empty() {
        return tokens;
    }

    let mut merged = Vec::with_capacity(tokens.len() + comments.len());
    let mut comment_iter = comments.iter().peekable();

    for token in tokens.drain(..) {
        while let Some(comment) = comment_iter.peek() {
            if comment_precedes_token(comment, &token) {
                merged.push(Token::Comment {
                    text: comment.text.clone(),
                    line: comment.line,
                    col: comment.col,
                    indent_hint: Some(comment.indent_level),
                });
                comment_iter.next();
            } else {
                break;
            }
        }

        merged.push(token);
    }

    for comment in comment_iter {
        merged.push(Token::Comment {
            text: comment.text.clone(),
            line: comment.line,
            col: comment.col,
            indent_hint: Some(comment.indent_level),
        });
    }

    merged
}

fn comment_precedes_token(comment: &Comment, token: &Token) -> bool {
    if comment.line < token.line() {
        return true;
    }

    if comment.line == token.line() {
        return comment.col <= token_column(token);
    }

    false
}

fn token_column(token: &Token) -> usize {
    token.col().unwrap_or(usize::MAX)
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

    /// Visit a pattern and generate tokens
    ///
    /// Converts Pattern nodes from match statements into formatting tokens,
    /// handling all pattern types including values, sequences, mappings, classes,
    /// as-patterns, and or-patterns.
    fn visit_pattern(&mut self, pattern: &Pattern, line: usize, col: usize) {
        match pattern {
            Pattern::MatchValue(node) => {
                self.visit_node(node);
            }
            Pattern::MatchSequence(patterns) => {
                self.tokens.push(Token::Delimiter { text: "[".to_string(), line, col });
                for (i, pat) in patterns.iter().enumerate() {
                    self.visit_pattern(pat, line, col);
                    if i < patterns.len() - 1 {
                        self.tokens.push(Token::Delimiter { text: ",".to_string(), line, col });
                        self.tokens.push(Token::Whitespace { count: 1, line, col });
                    }
                }
                self.tokens.push(Token::Delimiter { text: "]".to_string(), line, col });
            }
            Pattern::MatchMapping { keys, patterns } => {
                self.tokens.push(Token::Delimiter { text: "{".to_string(), line, col });
                for (i, (key, pat)) in keys.iter().zip(patterns.iter()).enumerate() {
                    self.visit_node(key);
                    self.tokens.push(Token::Delimiter { text: ":".to_string(), line, col });
                    self.tokens.push(Token::Whitespace { count: 1, line, col });
                    self.visit_pattern(pat, line, col);
                    if i < keys.len() - 1 {
                        self.tokens.push(Token::Delimiter { text: ",".to_string(), line, col });
                        self.tokens.push(Token::Whitespace { count: 1, line, col });
                    }
                }
                self.tokens.push(Token::Delimiter { text: "}".to_string(), line, col });
            }
            Pattern::MatchClass { cls, patterns } => {
                self.tokens.push(Token::Identifier { text: cls.clone(), line, col });
                self.tokens.push(Token::Delimiter { text: "(".to_string(), line, col });
                for (i, pat) in patterns.iter().enumerate() {
                    self.visit_pattern(pat, line, col);
                    if i < patterns.len() - 1 {
                        self.tokens.push(Token::Delimiter { text: ",".to_string(), line, col });
                        self.tokens.push(Token::Whitespace { count: 1, line, col });
                    }
                }
                self.tokens.push(Token::Delimiter { text: ")".to_string(), line, col });
            }
            Pattern::MatchAs { pattern, name } => {
                if let Some(pat) = pattern {
                    self.visit_pattern(pat, line, col);
                    self.tokens.push(Token::Whitespace { count: 1, line, col });
                    self.tokens.push(Token::Keyword { text: "as".to_string(), line, col });
                    self.tokens.push(Token::Whitespace { count: 1, line, col })
                }
                if let Some(n) = name {
                    self.tokens.push(Token::Identifier { text: n.clone(), line, col })
                } else if pattern.is_none() {
                    self.tokens.push(Token::Identifier { text: "_".to_string(), line, col })
                }
            }
            Pattern::MatchOr(patterns) => {
                for (i, pat) in patterns.iter().enumerate() {
                    self.visit_pattern(pat, line, col);
                    if i < patterns.len() - 1 {
                        self.tokens.push(Token::Whitespace { count: 1, line, col });
                        self.tokens.push(Token::Operator { text: "|".to_string(), line, col });
                        self.tokens.push(Token::Whitespace { count: 1, line, col });
                    }
                }
            }
        }
    }

    /// Visit an AST node and generate tokens
    fn visit_node(&mut self, node: &AstNode) {
        match node {
            AstNode::Module { body, .. } => {
                let mut first_stmt = true;
                let mut start_index = 0;
                if let Some(first) = body.first() {
                    if Self::is_string_literal(first) {
                        self.emit_docstring(first, true);
                        start_index = 1;
                        first_stmt = false;
                    }
                }

                let mut last_import_category: Option<ImportCategory> = None;
                let mut previous_was_import = false;
                let mut previous_line: Option<usize> = None;

                for stmt in &body[start_index..] {
                    let current_line = Self::node_start_line(stmt);

                    if let Some(category) = Self::import_category(stmt) {
                        if let Some(prev) = last_import_category {
                            if prev != category {
                                self.tokens.push(Token::Newline { line: current_line });
                            }
                        }
                        last_import_category = Some(category);
                    } else {
                        last_import_category = None;
                    }

                    if previous_was_import
                        && !matches!(stmt, AstNode::Import { .. } | AstNode::ImportFrom { .. })
                        && !Self::is_top_level_definition(stmt)
                    {
                        self.tokens.push(Token::Newline { line: current_line });
                    }

                    if !first_stmt && Self::is_top_level_definition(stmt) {
                        self.tokens.push(Token::Newline { line: current_line });
                        self.tokens.push(Token::Newline { line: current_line });
                    }

                    if !first_stmt && !Self::is_top_level_definition(stmt) {
                        let is_import = matches!(stmt, AstNode::Import { .. } | AstNode::ImportFrom { .. });
                        if !(previous_was_import && is_import) {
                            if let Some(prev_line) = previous_line {
                                if current_line > prev_line + 1 {
                                    self.tokens.push(Token::Newline { line: current_line });
                                }
                            }
                        }
                    }

                    self.visit_node(stmt);
                    first_stmt = false;
                    previous_was_import = matches!(stmt, AstNode::Import { .. } | AstNode::ImportFrom { .. });
                    previous_line = Some(current_line);
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

                if !decorators.is_empty() {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: *line });
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
                    if param.name.starts_with("**") {
                        self.tokens
                            .push(Token::Operator { text: "**".to_string(), line: param.line, col: param.col });
                        self.tokens.push(Token::Identifier {
                            text: param.name[2..].to_string(),
                            line: param.line,
                            col: param.col + 2,
                        });
                    } else if param.name.starts_with('*') {
                        self.tokens
                            .push(Token::Operator { text: "*".to_string(), line: param.line, col: param.col });
                        self.tokens.push(Token::Identifier {
                            text: param.name[1..].to_string(),
                            line: param.line,
                            col: param.col + 1,
                        });
                    } else {
                        self.tokens.push(Token::Identifier {
                            text: param.name.clone(),
                            line: param.line,
                            col: param.col,
                        });
                    }

                    if let Some(annotation) = &param.type_annotation {
                        self.tokens
                            .push(Token::Delimiter { text: ":".to_string(), line: param.line, col: param.col });
                        self.tokens
                            .push(Token::Whitespace { count: 1, line: param.line, col: param.col });
                        let normalized = normalize_type_annotation(annotation);
                        self.tokens
                            .push(Token::Identifier { text: normalized, line: param.line, col: param.col });
                    }
                    if let Some(default) = &param.default_value {
                        self.tokens
                            .push(Token::Whitespace { count: 1, line: param.line, col: param.col });
                        self.tokens
                            .push(Token::Operator { text: "=".to_string(), line: param.line, col: param.col });
                        self.tokens
                            .push(Token::Whitespace { count: 1, line: param.line, col: param.col });
                        self.visit_node(default);
                    }
                    if i < args.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: param.line, col: param.col });
                        self.tokens
                            .push(Token::Whitespace { count: 1, line: param.line, col: param.col });
                    }
                }
                self.tokens
                    .push(Token::Delimiter { text: ")".to_string(), line: *line, col: *col });

                if let Some(ret_type) = return_type {
                    self.tokens
                        .push(Token::Operator { text: "->".to_string(), line: *line, col: *col });
                    let normalized = normalize_type_annotation(ret_type);
                    self.tokens
                        .push(Token::Identifier { text: normalized, line: *line, col: *col });
                }

                self.tokens
                    .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Newline { line: *line });

                self.current_indent += 1;
                let mut previous_was_docstring = false;
                for (idx, stmt) in body.iter().enumerate() {
                    let stmt_line = match Self::node_start_line(stmt) {
                        0 => *line,
                        other => other,
                    };
                    if idx > 0 && matches!(stmt, AstNode::FunctionDef { .. }) && !previous_was_docstring {
                        let line = Self::node_start_line(stmt);
                        self.tokens.push(Token::Newline { line });
                        self.tokens.push(Token::Newline { line });
                    }

                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: stmt_line });
                    if idx == 0 && Self::is_string_literal(stmt) {
                        self.emit_docstring(stmt, true);
                        previous_was_docstring = true;
                    } else {
                        self.visit_node(stmt);
                        previous_was_docstring = false;
                    }
                }
                self.current_indent -= 1;
                self.tokens
                    .push(Token::Dedent { level: self.current_indent, line: *line });
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
            AstNode::ClassDef { name, bases, body, decorators, line, col, .. } => {
                for decorator in decorators {
                    self.tokens
                        .push(Token::Operator { text: "@".to_string(), line: *line, col: *col });
                    self.tokens
                        .push(Token::Identifier { text: decorator.clone(), line: *line, col: col + 1 });
                    self.tokens.push(Token::Newline { line: *line });
                }

                if !decorators.is_empty() {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: *line });
                }

                self.tokens
                    .push(Token::Keyword { text: "class".to_string(), line: *line, col: *col });
                self.tokens
                    .push(Token::Whitespace { count: 1, line: *line, col: col + 5 });
                self.tokens
                    .push(Token::Identifier { text: name.clone(), line: *line, col: col + 6 });

                if !bases.is_empty() {
                    self.tokens
                        .push(Token::Delimiter { text: "(".to_string(), line: *line, col: *col });
                    for (i, base) in bases.iter().enumerate() {
                        self.tokens
                            .push(Token::Identifier { text: base.clone(), line: *line, col: *col });
                        if i < bases.len() - 1 {
                            self.tokens
                                .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                            self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        }
                    }
                    self.tokens
                        .push(Token::Delimiter { text: ")".to_string(), line: *line, col: *col });
                }

                self.tokens
                    .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Newline { line: *line });

                self.current_indent += 1;
                let mut last_kind = None;
                for (idx, stmt) in body.iter().enumerate() {
                    let stmt_line = match Self::node_start_line(stmt) {
                        0 => *line,
                        other => other,
                    };
                    let kind = Self::class_item_kind(stmt);
                    if matches!(kind, ClassItemKind::Method) {
                        if let Some(ClassItemKind::Docstring | ClassItemKind::Attribute | ClassItemKind::Method) =
                            last_kind
                        {
                            let line = Self::node_start_line(stmt);
                            self.tokens.push(Token::Newline { line });
                        }
                    }

                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: stmt_line });
                    if idx == 0 && matches!(kind, ClassItemKind::Docstring) {
                        self.emit_docstring(stmt, true);
                    } else {
                        self.visit_node(stmt);
                    }
                    last_kind = Some(kind);
                }
                self.current_indent -= 1;
                self.tokens
                    .push(Token::Dedent { level: self.current_indent, line: *line });
            }
            AstNode::Assignment { target, value, line, col, .. } => {
                self.visit_node(target);
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.tokens
                    .push(Token::Operator { text: "=".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(value);
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::AnnotatedAssignment { target, type_annotation, value, line, col, .. } => {
                self.visit_node(target);
                self.tokens
                    .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                let normalized = normalize_type_annotation(type_annotation);
                self.tokens
                    .push(Token::Identifier { text: normalized, line: *line, col: *col });
                if let Some(val) = value {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Operator { text: "=".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(val);
                }
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::Identifier { name, line, col, .. } => {
                self.tokens
                    .push(Token::Identifier { text: name.clone(), line: *line, col: *col });
            }
            AstNode::Literal { value, line, col, .. } => match value {
                LiteralValue::String { value: s, prefix } => {
                    let quote = if s.contains('\n') { "\"\"\"" } else { "\"" };
                    let text = format!("{prefix}{quote}{s}{quote}");
                    self.tokens
                        .push(Token::StringLiteral { text, line: *line, col: *col, quote_char: '"' });
                }
                LiteralValue::Integer(i) => {
                    self.tokens
                        .push(Token::NumberLiteral { text: i.to_string(), line: *line, col: *col });
                }
                LiteralValue::Float(f) => {
                    self.tokens
                        .push(Token::NumberLiteral { text: f.to_string(), line: *line, col: *col });
                }
                LiteralValue::Boolean(b) => {
                    let text = if *b { "True" } else { "False" };
                    self.tokens
                        .push(Token::Keyword { text: text.to_string(), line: *line, col: *col });
                }
                LiteralValue::None => {
                    self.tokens
                        .push(Token::Keyword { text: "None".to_string(), line: *line, col: *col });
                }
            },
            AstNode::Call { function, args, keywords, line, col, .. } => {
                self.visit_node(function);
                self.tokens
                    .push(Token::Delimiter { text: "(".to_string(), line: *line, col: *col });

                for (i, arg) in args.iter().enumerate() {
                    self.visit_node(arg);
                    if i < args.len() - 1 || !keywords.is_empty() {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    }
                }

                for (i, (key, val)) in keywords.iter().enumerate() {
                    self.tokens
                        .push(Token::Identifier { text: key.clone(), line: *line, col: *col });
                    self.tokens.push(Token::KeywordEqual { line: *line, col: *col });
                    self.visit_node(val);
                    if i < keywords.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    }
                }

                self.tokens
                    .push(Token::Delimiter { text: ")".to_string(), line: *line, col: *col });
            }
            AstNode::BinaryOp { left, op, right, line, col, .. } => {
                self.visit_node(left);
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });

                use beacon_parser::BinaryOperator;
                let op_text = match op {
                    BinaryOperator::Add => "+",
                    BinaryOperator::Sub => "-",
                    BinaryOperator::Mult => "*",
                    BinaryOperator::Div => "/",
                    BinaryOperator::FloorDiv => "//",
                    BinaryOperator::Mod => "%",
                    BinaryOperator::Pow => "**",
                    BinaryOperator::BitOr => "|",
                    BinaryOperator::BitXor => "^",
                    BinaryOperator::BitAnd => "&",
                    BinaryOperator::LeftShift => "<<",
                    BinaryOperator::RightShift => ">>",
                    BinaryOperator::MatMult => "@",
                    BinaryOperator::And => "and",
                    BinaryOperator::Or => "or",
                };
                self.tokens
                    .push(Token::Operator { text: op_text.to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(right);
            }
            AstNode::UnaryOp { op, operand, line, col, .. } => {
                use beacon_parser::UnaryOperator;
                let op_text = match op {
                    UnaryOperator::Not => "not",
                    UnaryOperator::Plus => "+",
                    UnaryOperator::Minus => "-",
                    UnaryOperator::Invert => "~",
                };
                self.tokens
                    .push(Token::Operator { text: op_text.to_string(), line: *line, col: *col });
                if matches!(op, beacon_parser::UnaryOperator::Not) {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                }
                self.visit_node(operand);
            }
            AstNode::Compare { left, ops, comparators, line, col, .. } => {
                self.visit_node(left);

                use beacon_parser::CompareOperator;
                for (i, op) in ops.iter().enumerate() {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    let op_text = match op {
                        CompareOperator::Eq => "==",
                        CompareOperator::NotEq => "!=",
                        CompareOperator::Lt => "<",
                        CompareOperator::LtE => "<=",
                        CompareOperator::Gt => ">",
                        CompareOperator::GtE => ">=",
                        CompareOperator::Is => "is",
                        CompareOperator::IsNot => "is not",
                        CompareOperator::In => "in",
                        CompareOperator::NotIn => "not in",
                    };
                    self.tokens
                        .push(Token::Operator { text: op_text.to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    if let Some(comparator) = comparators.get(i) {
                        self.visit_node(comparator);
                    }
                }
            }
            AstNode::If { test, body, elif_parts, else_body, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "if".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(test);
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
                self.tokens
                    .push(Token::Dedent { level: self.current_indent, line: *line });

                for (elif_test, elif_body) in elif_parts {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: *line });
                    self.tokens
                        .push(Token::Keyword { text: "elif".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(elif_test);
                    self.tokens
                        .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Newline { line: *line });

                    self.current_indent += 1;
                    for stmt in elif_body {
                        self.tokens
                            .push(Token::Indent { level: self.current_indent, line: *line });
                        self.visit_node(stmt);
                    }
                    self.current_indent -= 1;
                    self.tokens
                        .push(Token::Dedent { level: self.current_indent, line: *line });
                }

                if let Some(else_stmts) = else_body {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: *line });
                    self.tokens
                        .push(Token::Keyword { text: "else".to_string(), line: *line, col: *col });
                    self.tokens
                        .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Newline { line: *line });

                    self.current_indent += 1;
                    for stmt in else_stmts {
                        self.tokens
                            .push(Token::Indent { level: self.current_indent, line: *line });
                        self.visit_node(stmt);
                    }
                    self.current_indent -= 1;
                    self.tokens
                        .push(Token::Dedent { level: self.current_indent, line: *line });
                }
            }
            AstNode::For { target, iter, body, else_body, is_async, line, col, .. } => {
                if *is_async {
                    self.tokens
                        .push(Token::Keyword { text: "async".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                }
                self.tokens
                    .push(Token::Keyword { text: "for".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.tokens
                    .push(Token::Identifier { text: target.target_to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.tokens
                    .push(Token::Keyword { text: "in".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(iter);
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
                self.tokens
                    .push(Token::Dedent { level: self.current_indent, line: *line });

                if let Some(else_stmts) = else_body {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: *line });
                    self.tokens
                        .push(Token::Keyword { text: "else".to_string(), line: *line, col: *col });
                    self.tokens
                        .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Newline { line: *line });

                    self.current_indent += 1;
                    for stmt in else_stmts {
                        self.tokens
                            .push(Token::Indent { level: self.current_indent, line: *line });
                        self.visit_node(stmt);
                    }
                    self.current_indent -= 1;
                    self.tokens
                        .push(Token::Dedent { level: self.current_indent, line: *line });
                }
            }
            AstNode::While { test, body, else_body, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "while".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(test);
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
                self.tokens
                    .push(Token::Dedent { level: self.current_indent, line: *line });

                if let Some(else_stmts) = else_body {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: *line });
                    self.tokens
                        .push(Token::Keyword { text: "else".to_string(), line: *line, col: *col });
                    self.tokens
                        .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Newline { line: *line });

                    self.current_indent += 1;
                    for stmt in else_stmts {
                        self.tokens
                            .push(Token::Indent { level: self.current_indent, line: *line });
                        self.visit_node(stmt);
                    }
                    self.current_indent -= 1;
                    self.tokens
                        .push(Token::Dedent { level: self.current_indent, line: *line });
                }
            }
            AstNode::Import { module, alias, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "import".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.tokens
                    .push(Token::Identifier { text: module.clone(), line: *line, col: *col });
                if let Some(alias_name) = alias {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Keyword { text: "as".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Identifier { text: alias_name.clone(), line: *line, col: *col });
                }
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::ImportFrom { module, names, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "from".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.tokens
                    .push(Token::Identifier { text: module.clone(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.tokens
                    .push(Token::Keyword { text: "import".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });

                for (i, name) in names.iter().enumerate() {
                    self.tokens
                        .push(Token::Identifier { text: name.clone(), line: *line, col: *col });
                    if i < names.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    }
                }
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::Attribute { object, attribute, line, col, .. } => {
                self.visit_node(object);
                self.tokens
                    .push(Token::Delimiter { text: ".".to_string(), line: *line, col: *col });
                self.tokens
                    .push(Token::Identifier { text: attribute.clone(), line: *line, col: *col });
            }
            AstNode::Pass { line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "pass".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::Break { line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "break".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::Continue { line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "continue".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::Global { names, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "global".to_string(), line: *line, col: *col });
                for (i, name) in names.iter().enumerate() {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Identifier { text: name.clone(), line: *line, col: *col });
                    if i < names.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                    }
                }
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::Nonlocal { names, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "nonlocal".to_string(), line: *line, col: *col });
                for (i, name) in names.iter().enumerate() {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Identifier { text: name.clone(), line: *line, col: *col });
                    if i < names.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                    }
                }
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::Raise { exc, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "raise".to_string(), line: *line, col: *col });
                if let Some(exception) = exc {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(exception);
                }
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::Tuple { elements, is_parenthesized, line, col, .. } => {
                let wrap = *is_parenthesized;
                if wrap {
                    self.tokens
                        .push(Token::Delimiter { text: "(".to_string(), line: *line, col: *col });
                }
                for (i, elem) in elements.iter().enumerate() {
                    self.visit_node(elem);
                    if i < elements.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    } else if elements.len() == 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                    }
                }
                if wrap {
                    self.tokens
                        .push(Token::Delimiter { text: ")".to_string(), line: *line, col: *col });
                }
            }
            AstNode::List { elements, line, col, .. } => {
                self.tokens
                    .push(Token::Delimiter { text: "[".to_string(), line: *line, col: *col });
                for (i, elem) in elements.iter().enumerate() {
                    self.visit_node(elem);
                    if i < elements.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    }
                }
                self.tokens
                    .push(Token::Delimiter { text: "]".to_string(), line: *line, col: *col });
            }
            AstNode::Dict { keys, values, line, col, .. } => {
                self.tokens
                    .push(Token::Delimiter { text: "{".to_string(), line: *line, col: *col });
                for (i, (key, value)) in keys.iter().zip(values.iter()).enumerate() {
                    self.visit_node(key);
                    self.tokens
                        .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(value);
                    if i < keys.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    }
                }
                self.tokens
                    .push(Token::Delimiter { text: "}".to_string(), line: *line, col: *col });
            }
            AstNode::Set { elements, line, col, .. } => {
                self.tokens
                    .push(Token::Delimiter { text: "{".to_string(), line: *line, col: *col });
                for (i, elem) in elements.iter().enumerate() {
                    self.visit_node(elem);
                    if i < elements.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    }
                }
                self.tokens
                    .push(Token::Delimiter { text: "}".to_string(), line: *line, col: *col });
            }
            AstNode::Subscript { value, slice, line, col, .. } => {
                self.visit_node(value);
                self.tokens
                    .push(Token::Delimiter { text: "[".to_string(), line: *line, col: *col });
                self.visit_node(slice);
                self.tokens
                    .push(Token::Delimiter { text: "]".to_string(), line: *line, col: *col });
            }
            AstNode::Try { body, handlers, else_body, finally_body, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "try".to_string(), line: *line, col: *col });
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
                self.tokens
                    .push(Token::Dedent { level: self.current_indent, line: *line });

                for handler in handlers {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: handler.line });
                    self.tokens.push(Token::Keyword {
                        text: "except".to_string(),
                        line: handler.line,
                        col: handler.col,
                    });
                    if let Some(exc_type) = &handler.exception_type {
                        self.tokens
                            .push(Token::Whitespace { count: 1, line: handler.line, col: handler.col });
                        self.tokens.push(Token::Identifier {
                            text: exc_type.clone(),
                            line: handler.line,
                            col: handler.col,
                        });
                        if let Some(name) = &handler.name {
                            self.tokens
                                .push(Token::Whitespace { count: 1, line: handler.line, col: handler.col });
                            self.tokens.push(Token::Keyword {
                                text: "as".to_string(),
                                line: handler.line,
                                col: handler.col,
                            });
                            self.tokens
                                .push(Token::Whitespace { count: 1, line: handler.line, col: handler.col });
                            self.tokens.push(Token::Identifier {
                                text: name.clone(),
                                line: handler.line,
                                col: handler.col,
                            });
                        }
                    }
                    self.tokens
                        .push(Token::Delimiter { text: ":".to_string(), line: handler.line, col: handler.col });
                    self.tokens.push(Token::Newline { line: handler.line });

                    self.current_indent += 1;
                    for stmt in &handler.body {
                        self.tokens
                            .push(Token::Indent { level: self.current_indent, line: handler.line });
                        self.visit_node(stmt);
                    }
                    self.current_indent -= 1;
                    self.tokens
                        .push(Token::Dedent { level: self.current_indent, line: *line });
                }

                if let Some(else_stmts) = else_body {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: *line });
                    self.tokens
                        .push(Token::Keyword { text: "else".to_string(), line: *line, col: *col });
                    self.tokens
                        .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Newline { line: *line });

                    self.current_indent += 1;
                    for stmt in else_stmts {
                        self.tokens
                            .push(Token::Indent { level: self.current_indent, line: *line });
                        self.visit_node(stmt);
                    }
                    self.current_indent -= 1;
                    self.tokens
                        .push(Token::Dedent { level: self.current_indent, line: *line });
                }

                if let Some(finally_stmts) = finally_body {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: *line });
                    self.tokens
                        .push(Token::Keyword { text: "finally".to_string(), line: *line, col: *col });
                    self.tokens
                        .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Newline { line: *line });

                    self.current_indent += 1;
                    for stmt in finally_stmts {
                        self.tokens
                            .push(Token::Indent { level: self.current_indent, line: *line });
                        self.visit_node(stmt);
                    }
                    self.current_indent -= 1;
                    self.tokens
                        .push(Token::Dedent { level: self.current_indent, line: *line });
                }
            }
            AstNode::With { items, body, is_async, line, col, .. } => {
                if *is_async {
                    self.tokens
                        .push(Token::Keyword { text: "async".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                }
                self.tokens
                    .push(Token::Keyword { text: "with".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });

                for (i, item) in items.iter().enumerate() {
                    self.visit_node(&item.context_expr);
                    if let Some(var) = &item.optional_vars {
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.tokens
                            .push(Token::Keyword { text: "as".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.tokens
                            .push(Token::Identifier { text: var.clone(), line: *line, col: *col });
                    }
                    if i < items.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    }
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
                self.tokens
                    .push(Token::Dedent { level: self.current_indent, line: *line });
            }
            AstNode::Lambda { args, body, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "lambda".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });

                for (i, param) in args.iter().enumerate() {
                    if param.name.starts_with("**") {
                        self.tokens
                            .push(Token::Operator { text: "**".to_string(), line: param.line, col: param.col });
                        self.tokens.push(Token::Identifier {
                            text: param.name[2..].to_string(),
                            line: param.line,
                            col: param.col + 2,
                        });
                    } else if param.name.starts_with('*') {
                        self.tokens
                            .push(Token::Operator { text: "*".to_string(), line: param.line, col: param.col });
                        self.tokens.push(Token::Identifier {
                            text: param.name[1..].to_string(),
                            line: param.line,
                            col: param.col + 1,
                        });
                    } else {
                        self.tokens.push(Token::Identifier {
                            text: param.name.clone(),
                            line: param.line,
                            col: param.col,
                        });
                    }

                    if i < args.len() - 1 {
                        self.tokens
                            .push(Token::Delimiter { text: ",".to_string(), line: param.line, col: param.col });
                        self.tokens
                            .push(Token::Whitespace { count: 1, line: param.line, col: param.col });
                    }
                }

                self.tokens
                    .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(body);
            }
            AstNode::ListComp { element, generators, line, col, .. } => {
                self.tokens
                    .push(Token::Delimiter { text: "[".to_string(), line: *line, col: *col });
                self.visit_node(element);

                for comp in generators {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Keyword { text: "for".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Identifier { text: comp.target.clone(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Keyword { text: "in".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(&comp.iter);

                    for cond in &comp.ifs {
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.tokens
                            .push(Token::Keyword { text: "if".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.visit_node(cond);
                    }
                }

                self.tokens
                    .push(Token::Delimiter { text: "]".to_string(), line: *line, col: *col });
            }
            AstNode::DictComp { key, value, generators, line, col, .. } => {
                self.tokens
                    .push(Token::Delimiter { text: "{".to_string(), line: *line, col: *col });
                self.visit_node(key);
                self.tokens
                    .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(value);

                for comp in generators {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Keyword { text: "for".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Identifier { text: comp.target.clone(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Keyword { text: "in".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(&comp.iter);

                    for cond in &comp.ifs {
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.tokens
                            .push(Token::Keyword { text: "if".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.visit_node(cond);
                    }
                }

                self.tokens
                    .push(Token::Delimiter { text: "}".to_string(), line: *line, col: *col });
            }
            AstNode::SetComp { element, generators, line, col, .. } => {
                self.tokens
                    .push(Token::Delimiter { text: "{".to_string(), line: *line, col: *col });
                self.visit_node(element);

                for comp in generators {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Keyword { text: "for".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Identifier { text: comp.target.clone(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Keyword { text: "in".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(&comp.iter);

                    for cond in &comp.ifs {
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.tokens
                            .push(Token::Keyword { text: "if".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.visit_node(cond);
                    }
                }

                self.tokens
                    .push(Token::Delimiter { text: "}".to_string(), line: *line, col: *col });
            }
            AstNode::GeneratorExp { element, generators, line, col, .. } => {
                self.tokens
                    .push(Token::Delimiter { text: "(".to_string(), line: *line, col: *col });
                self.visit_node(element);

                for g in generators {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Keyword { text: "for".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Identifier { text: g.target.clone(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.tokens
                        .push(Token::Keyword { text: "in".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(&g.iter);

                    for cond in &g.ifs {
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.tokens
                            .push(Token::Keyword { text: "if".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.visit_node(cond);
                    }
                }

                self.tokens
                    .push(Token::Delimiter { text: ")".to_string(), line: *line, col: *col });
            }
            AstNode::NamedExpr { target, value, line, col, .. } => {
                self.tokens
                    .push(Token::Identifier { text: target.clone(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.tokens
                    .push(Token::Operator { text: ":=".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(value);
            }
            AstNode::Yield { value, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "yield".to_string(), line: *line, col: *col });
                if let Some(val) = value {
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(val);
                }
            }
            AstNode::YieldFrom { value, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "yield".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.tokens
                    .push(Token::Keyword { text: "from".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(value);
            }
            AstNode::Await { value, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "await".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(value);
            }
            AstNode::Assert { test, msg, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "assert".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(test);
                if let Some(message) = msg {
                    self.tokens
                        .push(Token::Delimiter { text: ",".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                    self.visit_node(message);
                }
                self.tokens.push(Token::Newline { line: *line });
            }
            AstNode::Starred { value, line, col, .. } => {
                self.tokens
                    .push(Token::Operator { text: "*".to_string(), line: *line, col: *col });
                self.visit_node(value);
            }
            AstNode::ParenthesizedExpression { expression, line, col, .. } => {
                self.tokens
                    .push(Token::Delimiter { text: "(".to_string(), line: *line, col: *col });
                self.visit_node(expression);
                self.tokens
                    .push(Token::Delimiter { text: ")".to_string(), line: *line, col: *col });
            }
            AstNode::Match { subject, cases, line, col, .. } => {
                self.tokens
                    .push(Token::Keyword { text: "match".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                self.visit_node(subject);
                self.tokens
                    .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                self.tokens.push(Token::Newline { line: *line });

                self.current_indent += 1;
                for case in cases {
                    self.tokens
                        .push(Token::Indent { level: self.current_indent, line: *line });
                    self.tokens
                        .push(Token::Keyword { text: "case".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });

                    self.visit_pattern(&case.pattern, *line, *col);

                    if let Some(guard) = &case.guard {
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.tokens
                            .push(Token::Keyword { text: "if".to_string(), line: *line, col: *col });
                        self.tokens.push(Token::Whitespace { count: 1, line: *line, col: *col });
                        self.visit_node(guard);
                    }
                    self.tokens
                        .push(Token::Delimiter { text: ":".to_string(), line: *line, col: *col });
                    self.tokens.push(Token::Newline { line: *line });

                    self.current_indent += 1;
                    for stmt in &case.body {
                        self.tokens
                            .push(Token::Indent { level: self.current_indent, line: *line });
                        self.visit_node(stmt);
                    }
                    self.current_indent -= 1;
                    self.tokens
                        .push(Token::Dedent { level: self.current_indent, line: *line });
                }
                self.current_indent -= 1;
                self.tokens
                    .push(Token::Dedent { level: self.current_indent, line: *line });
            }
        }
    }

    fn emit_docstring(&mut self, node: &AstNode, add_blank_line: bool) {
        if let AstNode::Literal { value: LiteralValue::String { value, prefix }, line, .. } = node {
            let text = format!("{prefix}\"\"\"{value}\"\"\"");
            self.tokens
                .push(Token::StringLiteral { text, line: *line, col: 0, quote_char: '"' });
            self.tokens.push(Token::Newline { line: *line });
            if add_blank_line {
                self.tokens.push(Token::Newline { line: *line });
            }
        }
    }

    fn is_string_literal(node: &AstNode) -> bool {
        matches!(node, AstNode::Literal { value: LiteralValue::String { .. }, .. })
    }

    fn import_category(node: &AstNode) -> Option<ImportCategory> {
        match node {
            AstNode::Import { module, .. } | AstNode::ImportFrom { module, .. } => Some(categorize_import(module)),
            _ => None,
        }
    }

    fn class_item_kind(node: &AstNode) -> ClassItemKind {
        match node {
            AstNode::Literal { value: LiteralValue::String { .. }, .. } => ClassItemKind::Docstring,
            AstNode::Assignment { .. } | AstNode::AnnotatedAssignment { .. } => ClassItemKind::Attribute,
            AstNode::FunctionDef { .. } => ClassItemKind::Method,
            _ => ClassItemKind::Other,
        }
    }

    fn is_top_level_definition(node: &AstNode) -> bool {
        matches!(node, AstNode::FunctionDef { .. } | AstNode::ClassDef { .. })
    }

    fn node_start_line(node: &AstNode) -> usize {
        match node {
            AstNode::FunctionDef { line, .. }
            | AstNode::ClassDef { line, .. }
            | AstNode::Assignment { line, .. }
            | AstNode::AnnotatedAssignment { line, .. }
            | AstNode::Return { line, .. }
            | AstNode::Import { line, .. }
            | AstNode::ImportFrom { line, .. }
            | AstNode::If { line, .. }
            | AstNode::For { line, .. }
            | AstNode::While { line, .. }
            | AstNode::Try { line, .. }
            | AstNode::With { line, .. }
            | AstNode::Literal { line, .. }
            | AstNode::Identifier { line, .. }
            | AstNode::Call { line, .. }
            | AstNode::BinaryOp { line, .. }
            | AstNode::UnaryOp { line, .. }
            | AstNode::Compare { line, .. }
            | AstNode::Lambda { line, .. }
            | AstNode::Subscript { line, .. }
            | AstNode::Pass { line, .. }
            | AstNode::Break { line, .. }
            | AstNode::Continue { line, .. }
            | AstNode::Raise { line, .. }
            | AstNode::Tuple { line, .. }
            | AstNode::List { line, .. }
            | AstNode::Dict { line, .. }
            | AstNode::Set { line, .. }
            | AstNode::Yield { line, .. }
            | AstNode::YieldFrom { line, .. }
            | AstNode::Await { line, .. }
            | AstNode::Assert { line, .. }
            | AstNode::Starred { line, .. }
            | AstNode::ParenthesizedExpression { line, .. }
            | AstNode::ListComp { line, .. }
            | AstNode::DictComp { line, .. }
            | AstNode::SetComp { line, .. }
            | AstNode::GeneratorExp { line, .. }
            | AstNode::NamedExpr { line, .. }
            | AstNode::Match { line, .. } => *line,
            _ => 0,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ClassItemKind {
    Docstring,
    Attribute,
    Method,
    Other,
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::{AstNode, ExceptHandler, MatchCase, Parameter};

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

    #[test]
    fn test_class_def_tokens() {
        let node = AstNode::ClassDef {
            name: "MyClass".to_string(),
            bases: vec!["BaseClass".to_string()],
            metaclass: None,
            body: vec![AstNode::Pass { line: 2, col: 4, end_line: 2, end_col: 8 }],
            docstring: None,
            decorators: vec![],
            line: 1,
            col: 0,
            end_line: 2,
            end_col: 8,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "class"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Identifier { text, .. } if text == "MyClass"))
        );
    }

    #[test]
    fn test_assignment_tokens() {
        let node = AstNode::Assignment {
            target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 0, end_line: 1, end_col: 1 }),
            value: Box::new(AstNode::Literal {
                value: beacon_parser::LiteralValue::Integer(42),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 6,
            }),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 6,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Operator { text, .. } if text == "="))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Identifier { text, .. } if text == "x"))
        );
    }

    #[test]
    fn test_binary_op_tokens() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: beacon_parser::LiteralValue::Integer(1),
                line: 1,
                col: 0,
                end_line: 1,
                end_col: 1,
            }),
            op: beacon_parser::BinaryOperator::Add,
            right: Box::new(AstNode::Literal {
                value: beacon_parser::LiteralValue::Integer(2),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 5,
            }),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 5,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Operator { text, .. } if text == "+"))
        );
    }

    #[test]
    fn test_if_statement_tokens() {
        let node = AstNode::If {
            test: Box::new(AstNode::Literal {
                value: beacon_parser::LiteralValue::Boolean(true),
                line: 1,
                col: 3,
                end_line: 1,
                end_col: 7,
            }),
            body: vec![AstNode::Pass { line: 2, col: 4, end_line: 2, end_col: 8 }],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            col: 0,
            end_line: 2,
            end_col: 8,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "if"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "pass"))
        );
    }

    #[test]
    fn test_for_loop_tokens() {
        let node = AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "range".to_string(),
                    line: 1,
                    col: 10,
                    end_line: 1,
                    end_col: 15,
                }),
                args: vec![AstNode::Literal {
                    value: beacon_parser::LiteralValue::Integer(10),
                    line: 1,
                    col: 16,
                    end_line: 1,
                    end_col: 18,
                }],
                keywords: vec![],
                line: 1,
                col: 11,
                end_line: 1,
                end_col: 19,
            }),
            body: vec![AstNode::Pass { line: 2, col: 4, end_line: 2, end_col: 8 }],
            else_body: None,
            is_async: false,
            line: 1,
            col: 0,
            end_line: 2,
            end_col: 8,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "for"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "in"))
        );
    }

    #[test]
    fn test_import_tokens() {
        let node = AstNode::Import {
            module: "os".to_string(),
            alias: None,
            extra_modules: Vec::new(),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 9,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "import"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Identifier { text, .. } if text == "os"))
        );
    }

    #[test]
    fn test_list_tokens() {
        let node = AstNode::List {
            elements: vec![
                AstNode::Literal {
                    value: beacon_parser::LiteralValue::Integer(1),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
                AstNode::Literal {
                    value: beacon_parser::LiteralValue::Integer(2),
                    line: 1,
                    col: 4,
                    end_line: 1,
                    end_col: 5,
                },
            ],
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 6,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == "["))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == "]"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == ","))
        );
    }

    #[test]
    fn test_dict_tokens() {
        let node = AstNode::Dict {
            keys: vec![AstNode::Literal {
                value: beacon_parser::LiteralValue::String { value: "key".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 6,
            }],
            values: vec![AstNode::Literal {
                value: beacon_parser::LiteralValue::Integer(42),
                line: 1,
                col: 8,
                end_line: 1,
                end_col: 10,
            }],
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 11,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == "{"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == "}"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == ":"))
        );
    }

    #[test]
    fn test_try_except_tokens() {
        let node = AstNode::Try {
            body: vec![AstNode::Pass { line: 2, col: 4, end_line: 2, end_col: 8 }],
            handlers: vec![ExceptHandler {
                exception_type: Some("Exception".to_string()),
                name: None,
                body: vec![AstNode::Pass { line: 4, col: 4, end_line: 4, end_col: 8 }],
                line: 3,
                col: 0,
                end_line: 4,
                end_col: 8,
            }],
            else_body: None,
            finally_body: None,
            line: 1,
            col: 0,
            end_line: 4,
            end_col: 8,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "try"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "except"))
        );
    }

    #[test]
    fn test_lambda_tokens() {
        let node = AstNode::Lambda {
            args: vec![Parameter {
                name: "x".to_string(),
                line: 1,
                col: 7,
                end_line: 1,
                end_col: 8,
                type_annotation: None,
                default_value: None,
            }],
            body: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 10, end_line: 1, end_col: 11 }),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 11,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "lambda"))
        );
    }

    #[test]
    fn test_yield_tokens() {
        let node = AstNode::Yield {
            value: Some(Box::new(AstNode::Literal {
                value: beacon_parser::LiteralValue::Integer(42),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 8,
            })),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 8,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "yield"))
        );
    }

    #[test]
    fn test_await_tokens() {
        let node = AstNode::Await {
            value: Box::new(AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "async_func".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 11,
                }),
                args: vec![],
                keywords: vec![],
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 18,
            }),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 18,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "await"))
        );
    }

    #[test]
    fn test_assert_tokens() {
        let node = AstNode::Assert {
            test: Box::new(AstNode::Literal {
                value: beacon_parser::LiteralValue::Boolean(true),
                line: 1,
                col: 7,
                end_line: 1,
                end_col: 11,
            }),
            msg: None,
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 11,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "assert"))
        );
    }

    #[test]
    fn test_break_continue_tokens() {
        let break_node = AstNode::Break { line: 1, col: 0, end_line: 1, end_col: 5 };
        let continue_node = AstNode::Continue { line: 2, col: 0, end_line: 2, end_col: 8 };

        let stream1 = TokenStream::from_ast(&break_node);
        let tokens1: Vec<Token> = stream1.collect();
        assert!(
            tokens1
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "break"))
        );

        let stream2 = TokenStream::from_ast(&continue_node);
        let tokens2: Vec<Token> = stream2.collect();
        assert!(
            tokens2
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "continue"))
        );
    }

    #[test]
    fn test_match_with_value_pattern() {
        let node = AstNode::Match {
            subject: Box::new(AstNode::Identifier {
                name: "value".to_string(),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 11,
            }),
            cases: vec![MatchCase {
                pattern: Pattern::MatchValue(AstNode::Literal {
                    value: beacon_parser::LiteralValue::Integer(42),
                    line: 2,
                    col: 9,
                    end_line: 2,
                    end_col: 11,
                }),
                guard: None,
                body: vec![AstNode::Pass { line: 3, col: 8, end_line: 3, end_col: 12 }],
                line: 2,
                col: 5,
                end_line: 3,
                end_col: 12,
            }],
            line: 1,
            col: 0,
            end_line: 3,
            end_col: 12,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "match"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "case"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::NumberLiteral { text, .. } if text == "42"))
        );
    }

    #[test]
    fn test_match_with_sequence_pattern() {
        let node = AstNode::Match {
            subject: Box::new(AstNode::Identifier {
                name: "data".to_string(),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 10,
            }),
            cases: vec![MatchCase {
                pattern: Pattern::MatchSequence(vec![
                    Pattern::MatchValue(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(1),
                        line: 2,
                        col: 10,
                        end_line: 2,
                        end_col: 11,
                    }),
                    Pattern::MatchValue(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(2),
                        line: 2,
                        col: 13,
                        end_line: 2,
                        end_col: 14,
                    }),
                ]),
                guard: None,
                body: vec![AstNode::Pass { line: 3, col: 8, end_line: 3, end_col: 12 }],
                line: 2,
                col: 5,
                end_line: 3,
                end_col: 12,
            }],
            line: 1,
            col: 0,
            end_line: 3,
            end_col: 12,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == "["))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == "]"))
        );
    }

    #[test]
    fn test_match_with_mapping_pattern() {
        let node = AstNode::Match {
            subject: Box::new(AstNode::Identifier {
                name: "data".to_string(),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 10,
            }),
            cases: vec![MatchCase {
                pattern: Pattern::MatchMapping {
                    keys: vec![AstNode::Literal {
                        value: beacon_parser::LiteralValue::String { value: "name".to_string(), prefix: String::new() },
                        line: 2,
                        col: 10,
                        end_line: 2,
                        end_col: 16,
                    }],
                    patterns: vec![Pattern::MatchAs { pattern: None, name: Some("n".to_string()) }],
                },
                guard: None,
                body: vec![AstNode::Pass { line: 3, col: 8, end_line: 3, end_col: 12 }],
                line: 2,
                col: 5,
                end_line: 3,
                end_col: 12,
            }],
            line: 1,
            col: 0,
            end_line: 3,
            end_col: 12,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == "{"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == "}"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Identifier { text, .. } if text == "n"))
        );
    }

    #[test]
    fn test_match_with_class_pattern() {
        let node = AstNode::Match {
            subject: Box::new(AstNode::Identifier {
                name: "obj".to_string(),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 9,
            }),
            cases: vec![MatchCase {
                pattern: Pattern::MatchClass {
                    cls: "Point".to_string(),
                    patterns: vec![
                        Pattern::MatchValue(AstNode::Literal {
                            value: beacon_parser::LiteralValue::Integer(0),
                            line: 2,
                            col: 16,
                            end_line: 2,
                            end_col: 17,
                        }),
                        Pattern::MatchValue(AstNode::Literal {
                            value: beacon_parser::LiteralValue::Integer(0),
                            line: 2,
                            col: 19,
                            end_line: 2,
                            end_col: 20,
                        }),
                    ],
                },
                guard: None,
                body: vec![AstNode::Pass { line: 3, col: 8, end_line: 3, end_col: 12 }],
                line: 2,
                col: 5,
                end_line: 3,
                end_col: 12,
            }],
            line: 1,
            col: 0,
            end_line: 3,
            end_col: 12,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Identifier { text, .. } if text == "Point"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == "("))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Delimiter { text, .. } if text == ")"))
        );
    }

    #[test]
    fn test_match_with_as_pattern() {
        let node = AstNode::Match {
            subject: Box::new(AstNode::Identifier {
                name: "value".to_string(),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 11,
            }),
            cases: vec![MatchCase {
                pattern: Pattern::MatchAs {
                    pattern: Some(Box::new(Pattern::MatchValue(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(42),
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 11,
                    }))),
                    name: Some("x".to_string()),
                },
                guard: None,
                body: vec![AstNode::Pass { line: 3, col: 8, end_line: 3, end_col: 12 }],
                line: 2,
                col: 5,
                end_line: 3,
                end_col: 12,
            }],
            line: 1,
            col: 0,
            end_line: 3,
            end_col: 12,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "as"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Identifier { text, .. } if text == "x"))
        );
    }

    #[test]
    fn test_match_with_wildcard_pattern() {
        let node = AstNode::Match {
            subject: Box::new(AstNode::Identifier {
                name: "value".to_string(),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 11,
            }),
            cases: vec![MatchCase {
                pattern: Pattern::MatchAs { pattern: None, name: None },
                guard: None,
                body: vec![AstNode::Pass { line: 3, col: 8, end_line: 3, end_col: 12 }],
                line: 2,
                col: 5,
                end_line: 3,
                end_col: 12,
            }],
            line: 1,
            col: 0,
            end_line: 3,
            end_col: 12,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Identifier { text, .. } if text == "_"))
        );
    }

    #[test]
    fn test_match_with_or_pattern() {
        let node = AstNode::Match {
            subject: Box::new(AstNode::Identifier {
                name: "value".to_string(),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 11,
            }),
            cases: vec![MatchCase {
                pattern: Pattern::MatchOr(vec![
                    Pattern::MatchValue(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(1),
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 10,
                    }),
                    Pattern::MatchValue(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(2),
                        line: 2,
                        col: 13,
                        end_line: 2,
                        end_col: 14,
                    }),
                ]),
                guard: None,
                body: vec![AstNode::Pass { line: 3, col: 8, end_line: 3, end_col: 12 }],
                line: 2,
                col: 5,
                end_line: 3,
                end_col: 12,
            }],
            line: 1,
            col: 0,
            end_line: 3,
            end_col: 12,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Operator { text, .. } if text == "|"))
        );
    }

    #[test]
    fn test_raise_tokens() {
        let node = AstNode::Raise {
            exc: Some(Box::new(AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "ValueError".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 11,
                }),
                args: vec![],
                keywords: vec![],
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 18,
            })),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 18,
        };

        let stream = TokenStream::from_ast(&node);
        let tokens: Vec<Token> = stream.collect();
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Keyword { text, .. } if text == "raise"))
        );
    }
}
