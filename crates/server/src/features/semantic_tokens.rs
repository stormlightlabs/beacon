//! Semantic tokens provider
//!
//! Provides semantic highlighting based on symbol types and roles.
//!
//! Semantic tokens are delta-encoded positions that map identifiers to their semantic meaning.
//! Each token is represented by 5 integers: deltaLine, deltaStartChar, length, tokenType, tokenModifiers.
//! The encoding is relative to the previous token for efficiency.

use crate::document::DocumentManager;
use crate::utils;
use beacon_parser::{AstNode, ScopeId, SymbolKind, SymbolTable};
use lsp_types::{
    Position, Range, SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensParams,
    SemanticTokensRangeParams, SemanticTokensRangeResult, SemanticTokensResult,
};
use url::Url;

/// Raw token data before delta encoding
#[derive(Debug, Clone)]
struct RawToken {
    /// Line number (0-indexed)
    line: u32,
    /// Character position (0-indexed, UTF-16)
    character: u32,
    /// Token length in UTF-16 code units
    length: u32,
    /// Token type index
    token_type: u32,
    /// Token modifiers bitfield
    modifiers: u32,
}

pub struct SemanticTokensProvider {
    documents: DocumentManager,
}

pub const SUPPORTED_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::TYPE,
    SemanticTokenType::CLASS,
    SemanticTokenType::ENUM,
    SemanticTokenType::INTERFACE,
    SemanticTokenType::STRUCT,
    SemanticTokenType::TYPE_PARAMETER,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::METHOD,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::OPERATOR,
];

pub const SUPPORTED_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION,
    SemanticTokenModifier::DEFINITION,
    SemanticTokenModifier::READONLY,
    SemanticTokenModifier::STATIC,
    SemanticTokenModifier::DEPRECATED,
    SemanticTokenModifier::ABSTRACT,
    SemanticTokenModifier::ASYNC,
    SemanticTokenModifier::MODIFICATION,
];

impl SemanticTokensProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Provide semantic tokens for an entire document
    pub fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Option<SemanticTokensResult> {
        let uri = params.text_document.uri;
        let tokens = self.generate_tokens(&uri)?;

        Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        }))
    }

    /// Provide semantic tokens for a range
    pub fn semantic_tokens_range(&self, params: SemanticTokensRangeParams) -> Option<SemanticTokensRangeResult> {
        let uri = params.text_document.uri;
        let range = params.range;

        self.documents.get_document(&uri, |doc| {
            let ast = doc.ast()?;
            let symbol_table = doc.symbol_table()?;
            let tree = doc.tree()?;
            let text = doc.text();

            let mut raw_tokens = Vec::new();

            self.collect_tokens_from_node(ast, symbol_table, symbol_table.root_scope, &text, &mut raw_tokens);
            self.collect_keyword_tokens(tree.root_node(), &text, &mut raw_tokens);

            let filtered_tokens: Vec<_> = raw_tokens
                .into_iter()
                .filter(|t| Self::position_in_range(Position { line: t.line, character: t.character }, range))
                .collect();

            let mut sorted_tokens = filtered_tokens;
            sorted_tokens.sort_by_key(|t| (t.line, t.character));

            Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: Self::delta_encode(sorted_tokens),
            }))
        })?
    }

    /// Check if a position is within a range
    fn position_in_range(pos: Position, range: Range) -> bool {
        if pos.line < range.start.line || pos.line > range.end.line {
            return false;
        }
        if pos.line == range.start.line && pos.character < range.start.character {
            return false;
        }
        if pos.line == range.end.line && pos.character > range.end.character {
            return false;
        }
        true
    }

    /// Generate semantic tokens for a document
    fn generate_tokens(&self, uri: &Url) -> Option<Vec<SemanticToken>> {
        self.documents.get_document(uri, |doc| {
            let ast = doc.ast()?;
            let symbol_table = doc.symbol_table()?;
            let tree = doc.tree()?;
            let text = doc.text();

            let mut raw_tokens = Vec::new();

            self.collect_tokens_from_node(ast, symbol_table, symbol_table.root_scope, &text, &mut raw_tokens);
            self.collect_keyword_tokens(tree.root_node(), &text, &mut raw_tokens);

            raw_tokens.sort_by_key(|t| (t.line, t.character));

            Some(Self::delta_encode(raw_tokens))
        })?
    }

    /// Walk AST and collect token data
    fn collect_tokens_from_node(
        &self, node: &AstNode, symbol_table: &SymbolTable, current_scope: ScopeId, text: &str,
        raw_tokens: &mut Vec<RawToken>,
    ) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.collect_tokens_from_node(stmt, symbol_table, current_scope, text, raw_tokens);
                }
            }

            AstNode::FunctionDef { name, args, body, return_type, decorators, line, col, .. } => {
                let decorator_token_type = self.get_token_type_index(&SymbolKind::Function);
                for decorator in decorators {
                    let decorator_line = line.saturating_sub(decorators.len());
                    Self::add_token(decorator, decorator_line, 1, decorator_token_type, 0, text, raw_tokens);
                }

                let token_type = self.get_token_type_index(&SymbolKind::Function);
                let modifiers = self.get_definition_modifier();
                Self::add_token(name, *line, *col, token_type, modifiers, text, raw_tokens);

                let func_scope = if let Some(first_param) = args.first() {
                    let line = (first_param.line as u32).saturating_sub(1);
                    let character = (first_param.col as u32).saturating_sub(1);
                    let pos = Position::new(line, character);
                    let byte_offset = utils::position_to_byte_offset(text, pos);
                    symbol_table.find_scope_at_position(byte_offset)
                } else if let Some(first_stmt) = body.first() {
                    let (stmt_line, stmt_col) = Self::get_node_position(first_stmt);
                    let line = (stmt_line as u32).saturating_sub(1);
                    let character = (stmt_col as u32).saturating_sub(1);
                    let pos = Position::new(line, character);
                    let byte_offset = utils::position_to_byte_offset(text, pos);
                    symbol_table.find_scope_at_position(byte_offset)
                } else {
                    let line = (*line as u32).saturating_sub(1);
                    let character = (*col as u32) + name.len() as u32 + 2;
                    let pos = Position::new(line, character);
                    let byte_offset = utils::position_to_byte_offset(text, pos);
                    symbol_table.find_scope_at_position(byte_offset)
                };

                let param_token_type = self.get_token_type_index(&SymbolKind::Parameter);
                let param_modifiers = self.get_definition_modifier();
                let type_token_type = SUPPORTED_TYPES
                    .iter()
                    .position(|t| *t == SemanticTokenType::TYPE)
                    .unwrap_or(0) as u32;

                for param in args {
                    Self::add_token(
                        &param.name,
                        param.line,
                        param.col,
                        param_token_type,
                        param_modifiers,
                        text,
                        raw_tokens,
                    );

                    if let Some(type_ann) = &param.type_annotation {
                        if let Some(type_col) =
                            Self::find_type_annotation_position(text, param.line, param.col, &param.name, type_ann)
                        {
                            Self::add_token(type_ann, param.line, type_col, type_token_type, 0, text, raw_tokens);
                        }
                    }

                    if let Some(default) = &param.default_value {
                        self.collect_tokens_from_node(default, symbol_table, current_scope, text, raw_tokens);
                    }
                }

                if let Some(ret_type) = return_type {
                    if let Some(ret_col) = Self::find_return_type_position(text, *line, ret_type) {
                        Self::add_token(ret_type, *line, ret_col, type_token_type, 0, text, raw_tokens);
                    }
                }

                for stmt in body {
                    self.collect_tokens_from_node(stmt, symbol_table, func_scope, text, raw_tokens);
                }
            }
            AstNode::ClassDef { name, body, decorators, line, col, .. } => {
                let decorator_token_type = self.get_token_type_index(&SymbolKind::Function);
                for decorator in decorators {
                    let decorator_line = line.saturating_sub(decorators.len());
                    Self::add_token(decorator, decorator_line, 1, decorator_token_type, 0, text, raw_tokens);
                }

                let token_type = self.get_token_type_index(&SymbolKind::Class);
                let modifiers = self.get_definition_modifier();
                Self::add_token(name, *line, *col, token_type, modifiers, text, raw_tokens);

                let class_scope = if let Some(first_stmt) = body.first() {
                    let (stmt_line, stmt_col) = Self::get_node_position(first_stmt);
                    let line = (stmt_line as u32).saturating_sub(1);
                    let character = (stmt_col as u32).saturating_sub(1);
                    let pos = Position::new(line, character);
                    let byte_offset = utils::position_to_byte_offset(text, pos);
                    symbol_table.find_scope_at_position(byte_offset)
                } else {
                    let line = (*line as u32).saturating_sub(1);
                    let character = (*col as u32) + name.len() as u32 + 2;
                    let pos = Position::new(line, character);
                    let byte_offset = utils::position_to_byte_offset(text, pos);
                    symbol_table.find_scope_at_position(byte_offset)
                };

                for stmt in body {
                    self.collect_tokens_from_node(stmt, symbol_table, class_scope, text, raw_tokens);
                }
            }
            AstNode::Assignment { target, value, line, col } => {
                let token_type = self.get_token_type_index(&SymbolKind::Variable);
                let modifiers = self.get_definition_modifier();
                Self::add_token(target, *line, *col, token_type, modifiers, text, raw_tokens);
                self.collect_tokens_from_node(value, symbol_table, current_scope, text, raw_tokens);
            }

            AstNode::AnnotatedAssignment { target, type_annotation, value, line, col } => {
                let token_type = self.get_token_type_index(&SymbolKind::Variable);
                let modifiers = self.get_definition_modifier();
                Self::add_token(target, *line, *col, token_type, modifiers, text, raw_tokens);

                let type_token_type = SUPPORTED_TYPES
                    .iter()
                    .position(|t| *t == SemanticTokenType::TYPE)
                    .unwrap_or(0) as u32;

                if let Some(type_col) = Self::find_type_annotation_position(text, *line, *col, target, type_annotation)
                {
                    Self::add_token(type_annotation, *line, type_col, type_token_type, 0, text, raw_tokens);
                }

                if let Some(val) = value {
                    self.collect_tokens_from_node(val, symbol_table, current_scope, text, raw_tokens);
                }
            }

            AstNode::Call { function, args, line, col } => {
                if function.contains('.') {
                    let parts: Vec<&str> = function.split('.').collect();
                    let mut current_col = *col;

                    for (i, part) in parts.iter().enumerate() {
                        if i == 0 {
                            if let Some(symbol) = symbol_table.lookup_symbol(part, current_scope) {
                                let token_type = self.get_token_type_index(&symbol.kind);
                                Self::add_token(part, *line, current_col, token_type, 0, text, raw_tokens);
                            }
                        } else {
                            let token_type = SUPPORTED_TYPES
                                .iter()
                                .position(|t| *t == SemanticTokenType::PROPERTY)
                                .unwrap_or(0) as u32;
                            Self::add_token(part, *line, current_col, token_type, 0, text, raw_tokens);
                        }
                        current_col += part.len() + 1;
                    }
                } else if let Some(symbol) = symbol_table.lookup_symbol(function, current_scope) {
                    let token_type = self.get_token_type_index(&symbol.kind);
                    let modifiers = 0;
                    Self::add_token(function, *line, *col, token_type, modifiers, text, raw_tokens);
                }

                for arg in args {
                    self.collect_tokens_from_node(arg, symbol_table, current_scope, text, raw_tokens);
                }
            }

            AstNode::Identifier { name, line, col } => {
                if let Some(symbol) = symbol_table.lookup_symbol(name, current_scope) {
                    let token_type = self.get_token_type_index(&symbol.kind);
                    let modifiers = 0;
                    Self::add_token(name, *line, *col, token_type, modifiers, text, raw_tokens);
                }
            }

            AstNode::Literal { value, line, col } => {
                use beacon_parser::LiteralValue;
                let (token_type, length) = match value {
                    LiteralValue::String { value: s, .. } => {
                        let idx = SUPPORTED_TYPES
                            .iter()
                            .position(|t| *t == SemanticTokenType::STRING)
                            .unwrap_or(0) as u32;
                        (idx, (s.len() + 2) as u32)
                    }
                    LiteralValue::Integer(i) => {
                        let idx = SUPPORTED_TYPES
                            .iter()
                            .position(|t| *t == SemanticTokenType::NUMBER)
                            .unwrap_or(0) as u32;
                        (idx, i.to_string().len() as u32)
                    }
                    LiteralValue::Float(f) => {
                        let idx = SUPPORTED_TYPES
                            .iter()
                            .position(|t| *t == SemanticTokenType::NUMBER)
                            .unwrap_or(0) as u32;
                        (idx, f.to_string().len() as u32)
                    }
                    LiteralValue::Boolean(_) | LiteralValue::None => {
                        let idx = SUPPORTED_TYPES
                            .iter()
                            .position(|t| *t == SemanticTokenType::KEYWORD)
                            .unwrap_or(0) as u32;
                        let len = match value {
                            LiteralValue::Boolean(true) => 4,
                            LiteralValue::Boolean(false) => 5,
                            LiteralValue::None => 4,
                            _ => 0,
                        };
                        (idx, len)
                    }
                };

                raw_tokens.push(RawToken {
                    line: (*line as u32).saturating_sub(1),
                    character: (*col as u32).saturating_sub(1),
                    length,
                    token_type,
                    modifiers: 0,
                });
            }

            AstNode::Return { value: Some(val), .. } => {
                self.collect_tokens_from_node(val, symbol_table, current_scope, text, raw_tokens);
            }

            AstNode::Import { module, alias, line, col } => {
                let name = alias.as_ref().unwrap_or(module);
                let token_type = self.get_token_type_index(&SymbolKind::Import);
                let modifiers = self.get_definition_modifier();
                Self::add_token(name, *line, *col, token_type, modifiers, text, raw_tokens);
            }

            AstNode::ImportFrom { names, line, col, .. } => {
                let token_type = self.get_token_type_index(&SymbolKind::Import);
                let modifiers = self.get_definition_modifier();
                for name in names {
                    Self::add_token(name, *line, *col, token_type, modifiers, text, raw_tokens);
                }
            }

            AstNode::Attribute { object, attribute, line, col } => {
                self.collect_tokens_from_node(object, symbol_table, current_scope, text, raw_tokens);

                let token_type = SUPPORTED_TYPES
                    .iter()
                    .position(|t| *t == SemanticTokenType::PROPERTY)
                    .unwrap_or(0) as u32;
                Self::add_token(attribute, *line, *col, token_type, 0, text, raw_tokens);
            }
            _ => {}
        }
    }

    /// Walk tree-sitter CST and collect keyword tokens
    fn collect_keyword_tokens(&self, node: tree_sitter::Node, _text: &str, raw_tokens: &mut Vec<RawToken>) {
        if self.is_keyword_node(node.kind()) {
            let start_position = node.start_position();
            let line = start_position.row as u32;
            let character = start_position.column as u32;
            let length = (node.end_byte() - node.start_byte()) as u32;

            let keyword_type = SUPPORTED_TYPES
                .iter()
                .position(|t| *t == SemanticTokenType::KEYWORD)
                .unwrap_or(0) as u32;

            raw_tokens.push(RawToken { line, character, length, token_type: keyword_type, modifiers: 0 });
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_keyword_tokens(child, _text, raw_tokens);
        }
    }

    /// Check if a node kind represents a Python keyword
    fn is_keyword_node(&self, kind: &str) -> bool {
        matches!(
            kind,
            "def"
                | "class"
                | "if"
                | "elif"
                | "else"
                | "for"
                | "while"
                | "try"
                | "except"
                | "finally"
                | "with"
                | "as"
                | "import"
                | "from"
                | "return"
                | "yield"
                | "break"
                | "continue"
                | "pass"
                | "del"
                | "global"
                | "nonlocal"
                | "lambda"
                | "and"
                | "or"
                | "not"
                | "in"
                | "is"
                | "async"
                | "await"
                | "raise"
                | "assert"
                | "match"
                | "case"
        )
    }

    /// Add a token for an identifier
    fn add_token(
        name: &str, line: usize, col: usize, token_type: u32, modifiers: u32, text: &str,
        raw_tokens: &mut Vec<RawToken>,
    ) {
        let lsp_line = (line as u32).saturating_sub(1);
        let lsp_col = (col as u32).saturating_sub(1);
        let length = name.encode_utf16().count() as u32;
        let mut final_line = lsp_line;
        let mut final_col = lsp_col;

        if !name.is_empty() && !text.is_empty() {
            let approx_position = Position::new(lsp_line, lsp_col);
            let approx_offset = utils::position_to_byte_offset(text, approx_position);

            if approx_offset <= text.len() {
                if let Some(relative) = text[approx_offset..].find(name) {
                    let byte_offset = approx_offset + relative;
                    let position = utils::byte_offset_to_position(text, byte_offset);
                    final_line = position.line;
                    final_col = position.character;
                } else if line > 0 {
                    if let Some(line_text) = text.lines().nth(line - 1) {
                        if let Some(pos) = line_text.find(name) {
                            final_line = lsp_line;
                            final_col = pos as u32;
                        }
                    }
                }
            }
        }

        raw_tokens.push(RawToken { line: final_line, character: final_col, length, token_type, modifiers });
    }

    /// Extract line and column from any AST node
    fn get_node_position(node: &AstNode) -> (usize, usize) {
        match node {
            AstNode::Module { .. } => (1, 1),
            AstNode::Yield { line, col, .. }
            | AstNode::YieldFrom { line, col, .. }
            | AstNode::Await { line, col, .. }
            | AstNode::Tuple { line, col, .. }
            | AstNode::List { line, col, .. }
            | AstNode::Dict { line, col, .. }
            | AstNode::Set { line, col, .. }
            | AstNode::FunctionDef { line, col, .. }
            | AstNode::ClassDef { line, col, .. }
            | AstNode::Assignment { line, col, .. }
            | AstNode::AnnotatedAssignment { line, col, .. }
            | AstNode::Call { line, col, .. }
            | AstNode::Identifier { line, col, .. }
            | AstNode::Literal { line, col, .. }
            | AstNode::Return { line, col, .. }
            | AstNode::Import { line, col, .. }
            | AstNode::ImportFrom { line, col, .. }
            | AstNode::Attribute { line, col, .. }
            | AstNode::If { line, col, .. }
            | AstNode::For { line, col, .. }
            | AstNode::While { line, col, .. }
            | AstNode::Try { line, col, .. }
            | AstNode::With { line, col, .. }
            | AstNode::ListComp { line, col, .. }
            | AstNode::DictComp { line, col, .. }
            | AstNode::SetComp { line, col, .. }
            | AstNode::GeneratorExp { line, col, .. }
            | AstNode::NamedExpr { line, col, .. }
            | AstNode::BinaryOp { line, col, .. }
            | AstNode::UnaryOp { line, col, .. }
            | AstNode::Compare { line, col, .. }
            | AstNode::Lambda { line, col, .. }
            | AstNode::Subscript { line, col, .. }
            | AstNode::Match { line, col, .. }
            | AstNode::Pass { line, col }
            | AstNode::Break { line, col }
            | AstNode::Continue { line, col }
            | AstNode::Raise { line, col, .. } => (*line, *col),
        }
    }

    /// Find the actual column position of a type annotation in source text
    /// Searches for the type annotation after the identifier (e.g., after "name" in "name: int")
    fn find_type_annotation_position(
        text: &str, line: usize, identifier_col: usize, identifier_name: &str, type_annotation: &str,
    ) -> Option<usize> {
        let lines: Vec<&str> = text.lines().collect();
        if line == 0 || line > lines.len() {
            return None;
        }

        let line_text = lines[line - 1];
        let start_pos = identifier_col.saturating_sub(1) + identifier_name.len();

        if start_pos >= line_text.len() {
            return None;
        }

        if let Some(colon_pos) = line_text[start_pos..].find(':') {
            let after_colon = start_pos + colon_pos + 1;
            if after_colon < line_text.len() {
                let remaining = &line_text[after_colon..];
                if let Some(type_start) = remaining.find(|c: char| !c.is_whitespace()) {
                    let type_pos = after_colon + type_start;
                    if line_text[type_pos..].starts_with(type_annotation) {
                        return Some(type_pos + 1); // +1 for 1-indexed column
                    }
                }
            }
        }

        None
    }

    /// Find the column position of a return type annotation by searching for "-> <type>" pattern on the line
    fn find_return_type_position(text: &str, line: usize, type_annotation: &str) -> Option<usize> {
        let lines: Vec<&str> = text.lines().collect();
        if line == 0 || line > lines.len() {
            return None;
        }

        let line_text = lines[line - 1];

        if let Some(arrow_pos) = line_text.find("->") {
            let after_arrow = arrow_pos + 2;
            if after_arrow < line_text.len() {
                let remaining = &line_text[after_arrow..];
                if let Some(type_start) = remaining.find(|c: char| !c.is_whitespace()) {
                    let type_pos = after_arrow + type_start;
                    if line_text[type_pos..].starts_with(type_annotation) {
                        return Some(type_pos + 1); // +1 for 1-indexed column
                    }
                }
            }
        }

        None
    }

    /// Map symbol kind to semantic token type index
    fn get_token_type_index(&self, symbol_kind: &SymbolKind) -> u32 {
        let token_type = match symbol_kind {
            SymbolKind::Function => SemanticTokenType::FUNCTION,
            SymbolKind::Class => SemanticTokenType::CLASS,
            SymbolKind::Parameter => SemanticTokenType::PARAMETER,
            SymbolKind::Variable => SemanticTokenType::VARIABLE,
            SymbolKind::Import => SemanticTokenType::NAMESPACE,
            SymbolKind::MagicMethod => SemanticTokenType::METHOD,
            SymbolKind::BuiltinVar => SemanticTokenType::VARIABLE,
        };

        SUPPORTED_TYPES.iter().position(|t| *t == token_type).unwrap_or(0) as u32
    }

    /// Get modifier bitfield for definition/declaration
    fn get_definition_modifier(&self) -> u32 {
        let def_idx = SUPPORTED_MODIFIERS
            .iter()
            .position(|m| *m == SemanticTokenModifier::DEFINITION)
            .unwrap_or(0);

        1 << def_idx
    }

    /// Delta encode raw tokens into LSP semantic tokens format
    fn delta_encode(raw_tokens: Vec<RawToken>) -> Vec<SemanticToken> {
        let mut result = Vec::new();
        let mut prev_line = 0;
        let mut prev_char = 0;

        for token in raw_tokens {
            let delta_line = token.line - prev_line;
            let delta_start = if delta_line == 0 { token.character - prev_char } else { token.character };

            result.push(SemanticToken {
                delta_line,
                delta_start,
                length: token.length,
                token_type: token.token_type,
                token_modifiers_bitset: token.modifiers,
            });

            prev_line = token.line;
            prev_char = token.character;
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    fn decode_tokens(tokens: &[SemanticToken]) -> Vec<(u32, u32, u32, u32)> {
        let mut result = Vec::with_capacity(tokens.len());
        let mut line = 0;
        let mut character = 0;

        for token in tokens {
            line += token.delta_line;
            if token.delta_line == 0 {
                character += token.delta_start;
            } else {
                character = token.delta_start;
            }

            result.push((line, character, token.length, token.token_type));
        }

        result
    }

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _ = SemanticTokensProvider::new(documents);
    }

    #[test]
    fn test_supported_types_count() {
        assert_eq!(SUPPORTED_TYPES.len(), 18);
    }

    #[test]
    fn test_supported_modifiers_count() {
        assert_eq!(SUPPORTED_MODIFIERS.len(), 8);
    }

    #[test]
    fn test_symbol_to_token_type_mapping() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents);

        let func_idx = provider.get_token_type_index(&SymbolKind::Function);
        let class_idx = provider.get_token_type_index(&SymbolKind::Class);
        let param_idx = provider.get_token_type_index(&SymbolKind::Parameter);
        let var_idx = provider.get_token_type_index(&SymbolKind::Variable);
        let import_idx = provider.get_token_type_index(&SymbolKind::Import);

        assert_ne!(func_idx, class_idx);
        assert_ne!(func_idx, param_idx);
        assert_ne!(func_idx, var_idx);

        assert_eq!(SUPPORTED_TYPES[func_idx as usize], SemanticTokenType::FUNCTION);
        assert_eq!(SUPPORTED_TYPES[class_idx as usize], SemanticTokenType::CLASS);
        assert_eq!(SUPPORTED_TYPES[param_idx as usize], SemanticTokenType::PARAMETER);
        assert_eq!(SUPPORTED_TYPES[var_idx as usize], SemanticTokenType::VARIABLE);
        assert_eq!(SUPPORTED_TYPES[import_idx as usize], SemanticTokenType::NAMESPACE);
    }

    #[test]
    fn test_simple_variable_assignment() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();

        documents.open_document(uri.clone(), 1, "x = 42".to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();
        assert!(!tokens.is_empty(), "Expected tokens for variable and literal");
    }

    #[test]
    fn test_function_definition() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def hello():
    pass"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        assert!(!tokens.is_empty(), "Expected token for function name");

        let func_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::FUNCTION)
            .unwrap() as u32;

        let func_token = tokens.iter().find(|t| t.token_type == func_type_idx);
        assert!(func_token.is_some(), "Expected FUNCTION token");
        assert_ne!(
            func_token.unwrap().token_modifiers_bitset,
            0,
            "Expected DEFINITION modifier"
        );
    }

    #[test]
    fn test_class_definition() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"class MyClass:
    pass"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        assert!(!tokens.is_empty(), "Expected token for class name");

        let class_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::CLASS)
            .unwrap() as u32;

        let class_token = tokens.iter().find(|t| t.token_type == class_type_idx);
        assert!(class_token.is_some(), "Expected CLASS token");
    }

    #[test]
    fn test_function_call() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def greet():
    pass

greet()"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        assert!(tokens.len() >= 2, "Expected tokens for function def and call");

        let func_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::FUNCTION)
            .unwrap() as u32;

        let func_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == func_type_idx).collect();

        assert!(func_tokens.len() >= 2, "Expected at least 2 FUNCTION tokens");
        assert_ne!(
            func_tokens[0].token_modifiers_bitset, 0,
            "Definition should have modifier"
        );
        assert_eq!(
            func_tokens[1].token_modifiers_bitset, 0,
            "Usage should not have modifier"
        );
    }

    #[test]
    fn test_nested_scopes() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"global_var = 1

def func():
    local_var = 2
    x = global_var"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        assert!(tokens.len() >= 5, "Expected multiple tokens for nested scopes");
    }

    #[test]
    fn test_string_literals() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"message = "hello world""#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        assert!(tokens.len() >= 2, "Expected tokens for variable and string");

        let string_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::STRING)
            .unwrap() as u32;

        let string_token = tokens.iter().find(|t| t.token_type == string_type_idx);
        assert!(string_token.is_some(), "Expected STRING token");
    }

    #[test]
    fn test_number_literals() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42\ny = 3.14";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();
        let number_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::NUMBER)
            .unwrap() as u32;

        let number_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == number_type_idx).collect();
        assert!(number_tokens.len() >= 2, "Expected 2 number tokens");
    }

    #[test]
    fn test_empty_file() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();

        documents.open_document(uri.clone(), 1, "".to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();
        assert_eq!(tokens.len(), 0, "Empty file should have no tokens");
    }

    #[test]
    fn test_delta_encoding() {
        let raw_tokens = vec![
            RawToken { line: 0, character: 0, length: 1, token_type: 0, modifiers: 0 },
            RawToken { line: 0, character: 4, length: 2, token_type: 1, modifiers: 0 },
            RawToken { line: 1, character: 0, length: 3, token_type: 0, modifiers: 0 },
        ];

        let encoded = SemanticTokensProvider::delta_encode(raw_tokens);

        assert_eq!(encoded.len(), 3);

        assert_eq!(encoded[0].delta_line, 0);
        assert_eq!(encoded[0].delta_start, 0);

        assert_eq!(encoded[1].delta_line, 0);
        assert_eq!(encoded[1].delta_start, 4);

        assert_eq!(encoded[2].delta_line, 1);
        assert_eq!(encoded[2].delta_start, 0);
    }

    #[test]
    fn test_position_in_range() {
        let range = Range { start: Position { line: 1, character: 5 }, end: Position { line: 3, character: 10 } };

        assert!(!SemanticTokensProvider::position_in_range(
            Position { line: 0, character: 0 },
            range
        ));

        assert!(SemanticTokensProvider::position_in_range(
            Position { line: 1, character: 5 },
            range
        ));

        assert!(SemanticTokensProvider::position_in_range(
            Position { line: 2, character: 0 },
            range
        ));

        assert!(SemanticTokensProvider::position_in_range(
            Position { line: 3, character: 10 },
            range
        ));

        assert!(!SemanticTokensProvider::position_in_range(
            Position { line: 4, character: 0 },
            range
        ));
    }

    #[test]
    fn test_semantic_tokens_full() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def add(a, b):
    return a + b

result = add(1, 2)"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = SemanticTokensParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.semantic_tokens_full(params);
        assert!(result.is_some());

        match result.unwrap() {
            SemanticTokensResult::Tokens(tokens) => {
                assert!(
                    !tokens.data.is_empty(),
                    "Expected tokens for function definition and call"
                );
            }
            _ => panic!("Expected Tokens result"),
        }
    }

    #[test]
    fn test_complex_python_code() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"class Calculator:
    def add(self, x, y):
        result = x + y
        return result

calc = Calculator()
answer = calc.add(5, 3)"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        assert!(tokens.len() > 5, "Expected many tokens for complex code");

        let class_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::CLASS)
            .unwrap() as u32;
        let func_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::FUNCTION)
            .unwrap() as u32;
        let has_class = tokens.iter().any(|t| t.token_type == class_type_idx);
        let has_function = tokens.iter().any(|t| t.token_type == func_type_idx);

        assert!(has_class, "Expected CLASS token");
        assert!(has_function, "Expected FUNCTION token");
    }

    #[test]
    fn test_import_tokens() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"import os
import sys as system
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();
        let import_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::NAMESPACE)
            .unwrap() as u32;

        let import_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == import_type_idx).collect();
        assert!(import_tokens.len() >= 2, "Expected at least 2 import tokens");

        assert_ne!(
            import_tokens[0].token_modifiers_bitset, 0,
            "Expected DEFINITION modifier"
        );
    }

    #[test]
    fn test_import_from_tokens() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "from math import sqrt, pi";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();
        let import_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::NAMESPACE)
            .unwrap() as u32;

        let import_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == import_type_idx).collect();
        assert!(import_tokens.len() >= 2, "Expected tokens for sqrt and pi");
    }

    #[test]
    fn test_attribute_tokens() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();

        let source = r#"import os
x = os.path
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();
        let property_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .unwrap() as u32;

        let property_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == property_type_idx).collect();
        assert!(!property_tokens.is_empty(), "Expected PROPERTY token for attribute");
    }

    #[test]
    fn test_nested_attribute_tokens() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();

        let source = r#"import os
result = os.path.join("a", "b")
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        let property_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .unwrap() as u32;

        let property_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == property_type_idx).collect();
        assert!(
            property_tokens.len() >= 2,
            "Expected PROPERTY tokens for nested attributes"
        );
    }

    #[test]
    fn test_import_usage_tokens() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();

        let source = r#"import os
from math import sqrt

x = os
y = sqrt(16)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        assert!(tokens.len() >= 6, "Expected tokens for imports, variables, and usages");
    }

    #[test]
    fn test_combined_imports_and_attributes() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();

        let source = r#"import os
import sys as system
from math import sqrt, pi

path = os.path.join("home", "user")
pi_value = pi
sys_info = system.version
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        assert!(
            tokens.len() > 10,
            "Expected many tokens for complex import/attribute code"
        );

        let import_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::NAMESPACE)
            .unwrap() as u32;
        let import_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == import_type_idx).collect();
        assert!(import_tokens.len() >= 4, "Expected tokens for os, system, sqrt, pi");

        let property_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .unwrap() as u32;
        let property_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == property_type_idx).collect();
        assert!(property_tokens.len() >= 3, "Expected tokens for path, join, version");
    }

    #[test]
    fn test_def_keyword_vs_function_name() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "def greet():\n    pass";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        let keyword_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::KEYWORD)
            .unwrap() as u32;
        let function_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::FUNCTION)
            .unwrap() as u32;

        let def_token = tokens.iter().find(|t| t.delta_line == 0 && t.delta_start == 0);
        assert!(def_token.is_some(), "Expected token for 'def' keyword");
        assert_eq!(
            def_token.unwrap().token_type,
            keyword_type_idx,
            "Expected 'def' to have KEYWORD token type"
        );

        let greet_token = tokens.iter().find(|t| t.token_type == function_type_idx);
        assert!(greet_token.is_some(), "Expected token for 'greet' function name");
        assert_eq!(
            greet_token.unwrap().token_type,
            function_type_idx,
            "Expected 'greet' to have FUNCTION token type"
        );

        assert_ne!(
            keyword_type_idx, function_type_idx,
            "'def' and 'greet' must have different token types"
        );
    }

    #[test]
    fn test_class_keyword_vs_class_name() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "class Calculator:\n    pass";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        let keyword_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::KEYWORD)
            .unwrap() as u32;
        let class_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::CLASS)
            .unwrap() as u32;

        let class_keyword_token = tokens.iter().find(|t| t.delta_line == 0 && t.delta_start == 0);
        assert!(class_keyword_token.is_some(), "Expected token for 'class' keyword");
        assert_eq!(
            class_keyword_token.unwrap().token_type,
            keyword_type_idx,
            "Expected 'class' to have KEYWORD token type"
        );

        let calculator_token = tokens.iter().find(|t| t.token_type == class_type_idx);
        assert!(calculator_token.is_some(), "Expected token for 'Calculator' class name");
        assert_eq!(
            calculator_token.unwrap().token_type,
            class_type_idx,
            "Expected 'Calculator' to have CLASS token type"
        );

        assert_ne!(
            keyword_type_idx, class_type_idx,
            "'class' and 'Calculator' must have different token types"
        );
    }

    #[test]
    fn test_import_keywords_vs_module_names() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "import os\nfrom math import sqrt";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        let keyword_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::KEYWORD)
            .unwrap() as u32;
        let namespace_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::NAMESPACE)
            .unwrap() as u32;

        let keyword_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == keyword_type_idx).collect();
        assert!(
            keyword_tokens.len() >= 3,
            "Expected at least 3 keyword tokens (import, from, import)"
        );

        let namespace_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == namespace_type_idx).collect();
        assert!(
            namespace_tokens.len() >= 2,
            "Expected at least 2 namespace tokens (os, sqrt)"
        );

        assert_ne!(
            keyword_type_idx, namespace_type_idx,
            "Keywords and module names must have different token types"
        );
    }

    #[test]
    fn test_import_module_token_position() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "import os\n";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();
        let decoded = decode_tokens(&tokens);

        let namespace_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::NAMESPACE)
            .unwrap() as u32;

        let module_token = tokens
            .iter()
            .zip(decoded.iter())
            .find(|(token, _)| token.token_type == namespace_type_idx)
            .map(|(_, decoded)| decoded)
            .expect("Expected namespace token for module name");

        assert_eq!(module_token.0, 0, "Module should be on first line");
        assert_eq!(module_token.1, 7, "Module token should start at character 7");
        assert_eq!(module_token.2, 2, "Module token length should be 2");
    }

    #[test]
    fn test_multiple_keywords_in_statement() {
        let documents = DocumentManager::new().unwrap();
        let provider = SemanticTokensProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "if x in range(10):\n    pass";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let tokens = provider.generate_tokens(&uri).unwrap();

        let keyword_type_idx = SUPPORTED_TYPES
            .iter()
            .position(|t| *t == SemanticTokenType::KEYWORD)
            .unwrap() as u32;

        let keyword_tokens: Vec<_> = tokens.iter().filter(|t| t.token_type == keyword_type_idx).collect();
        assert!(
            keyword_tokens.len() >= 3,
            "Expected at least 3 keyword tokens (if, in, pass)"
        );
    }
}
