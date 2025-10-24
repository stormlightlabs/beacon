//! Semantic tokens provider
//!
//! Provides semantic highlighting based on symbol types and roles.
//!
//! Semantic tokens are delta-encoded positions that map identifiers to their semantic meaning.
//! Each token is represented by 5 integers: deltaLine, deltaStartChar, length, tokenType, tokenModifiers.
//! The encoding is relative to the previous token for efficiency.

use crate::document::DocumentManager;
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
    ///
    /// TODO: Implement token generation from AST and symbol table
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
            let text = doc.text();

            let mut raw_tokens = Vec::new();
            self.collect_tokens_from_node(ast, symbol_table, symbol_table.root_scope, &text, &mut raw_tokens);

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
            let text = doc.text();

            let mut raw_tokens = Vec::new();
            self.collect_tokens_from_node(ast, symbol_table, symbol_table.root_scope, &text, &mut raw_tokens);

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
                    self.add_token(decorator, decorator_line, 1, decorator_token_type, 0, text, raw_tokens);
                }

                let token_type = self.get_token_type_index(&SymbolKind::Function);
                let modifiers = self.get_definition_modifier();
                self.add_token(name, *line, *col, token_type, modifiers, text, raw_tokens);

                let func_scope = self.find_function_scope(symbol_table, current_scope);
                let param_token_type = self.get_token_type_index(&SymbolKind::Parameter);
                let param_modifiers = self.get_definition_modifier();
                let type_token_type = SUPPORTED_TYPES
                    .iter()
                    .position(|t| *t == SemanticTokenType::TYPE)
                    .unwrap_or(0) as u32;

                for param in args {
                    self.add_token(
                        &param.name,
                        param.line,
                        param.col,
                        param_token_type,
                        param_modifiers,
                        text,
                        raw_tokens,
                    );

                    if let Some(type_ann) = &param.type_annotation {
                        let type_col = param.col + param.name.len() + 2; // +2 for ": "
                        self.add_token(type_ann, param.line, type_col, type_token_type, 0, text, raw_tokens);
                    }
                }

                if let Some(ret_type) = return_type {
                    let return_type_col = *col + name.len() + 10; // Approximate
                    self.add_token(ret_type, *line, return_type_col, type_token_type, 0, text, raw_tokens);
                }

                for stmt in body {
                    self.collect_tokens_from_node(stmt, symbol_table, func_scope, text, raw_tokens);
                }
            }
            AstNode::ClassDef { name, body, decorators, line, col, .. } => {
                let decorator_token_type = self.get_token_type_index(&SymbolKind::Function);
                for decorator in decorators {
                    let decorator_line = line.saturating_sub(decorators.len());
                    self.add_token(decorator, decorator_line, 1, decorator_token_type, 0, text, raw_tokens);
                }

                let token_type = self.get_token_type_index(&SymbolKind::Class);
                let modifiers = self.get_definition_modifier();
                self.add_token(name, *line, *col, token_type, modifiers, text, raw_tokens);

                let class_scope = self.find_class_scope(symbol_table, current_scope);

                for stmt in body {
                    self.collect_tokens_from_node(stmt, symbol_table, class_scope, text, raw_tokens);
                }
            }
            AstNode::Assignment { target, value, line, col } => {
                let token_type = self.get_token_type_index(&SymbolKind::Variable);
                let modifiers = self.get_definition_modifier();
                self.add_token(target, *line, *col, token_type, modifiers, text, raw_tokens);
                self.collect_tokens_from_node(value, symbol_table, current_scope, text, raw_tokens);
            }

            AstNode::AnnotatedAssignment { target, type_annotation, value, line, col } => {
                let token_type = self.get_token_type_index(&SymbolKind::Variable);
                let modifiers = self.get_definition_modifier();
                self.add_token(target, *line, *col, token_type, modifiers, text, raw_tokens);

                let type_token_type = SUPPORTED_TYPES
                    .iter()
                    .position(|t| *t == SemanticTokenType::TYPE)
                    .unwrap_or(0) as u32;
                let type_col = *col + target.len() + 2; // +2 for ": "
                self.add_token(type_annotation, *line, type_col, type_token_type, 0, text, raw_tokens);

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
                                self.add_token(part, *line, current_col, token_type, 0, text, raw_tokens);
                            }
                        } else {
                            let token_type = SUPPORTED_TYPES
                                .iter()
                                .position(|t| *t == SemanticTokenType::PROPERTY)
                                .unwrap_or(0) as u32;
                            self.add_token(part, *line, current_col, token_type, 0, text, raw_tokens);
                        }
                        current_col += part.len() + 1;
                    }
                } else {
                    if let Some(symbol) = symbol_table.lookup_symbol(function, current_scope) {
                        let token_type = self.get_token_type_index(&symbol.kind);
                        let modifiers = 0;
                        self.add_token(function, *line, *col, token_type, modifiers, text, raw_tokens);
                    }
                }

                for arg in args {
                    self.collect_tokens_from_node(arg, symbol_table, current_scope, text, raw_tokens);
                }
            }

            AstNode::Identifier { name, line, col } => {
                if let Some(symbol) = symbol_table.lookup_symbol(name, current_scope) {
                    let token_type = self.get_token_type_index(&symbol.kind);
                    let modifiers = 0;
                    self.add_token(name, *line, *col, token_type, modifiers, text, raw_tokens);
                }
            }

            AstNode::Literal { value, line, col } => {
                use beacon_parser::LiteralValue;
                let (token_type, length) = match value {
                    LiteralValue::String(s) => {
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

            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    self.collect_tokens_from_node(val, symbol_table, current_scope, text, raw_tokens);
                }
            }

            AstNode::Import { module, alias, line, col } => {
                let name = alias.as_ref().unwrap_or(module);
                let token_type = self.get_token_type_index(&SymbolKind::Import);
                let modifiers = self.get_definition_modifier();
                self.add_token(name, *line, *col, token_type, modifiers, text, raw_tokens);
            }

            AstNode::ImportFrom { names, line, col, .. } => {
                let token_type = self.get_token_type_index(&SymbolKind::Import);
                let modifiers = self.get_definition_modifier();
                for name in names {
                    self.add_token(name, *line, *col, token_type, modifiers, text, raw_tokens);
                }
            }

            AstNode::Attribute { object, attribute, line, col } => {
                self.collect_tokens_from_node(object, symbol_table, current_scope, text, raw_tokens);

                let token_type = SUPPORTED_TYPES
                    .iter()
                    .position(|t| *t == SemanticTokenType::PROPERTY)
                    .unwrap_or(0) as u32;
                self.add_token(attribute, *line, *col, token_type, 0, text, raw_tokens);
            }
            _ => {}
        }
    }

    /// Add a token for an identifier
    fn add_token(
        &self, name: &str, line: usize, col: usize, token_type: u32, modifiers: u32, _text: &str,
        raw_tokens: &mut Vec<RawToken>,
    ) {
        let lsp_line = (line as u32).saturating_sub(1);
        let lsp_col = (col as u32).saturating_sub(1);
        let length = name.encode_utf16().count() as u32;

        raw_tokens.push(RawToken { line: lsp_line, character: lsp_col, length, token_type, modifiers });
    }

    /// Find the function scope (child of current scope with ScopeKind::Function)
    fn find_function_scope(&self, symbol_table: &SymbolTable, parent_scope: ScopeId) -> ScopeId {
        match symbol_table.scopes.get(&parent_scope) {
            Some(parent) => parent
                .children
                .iter()
                .find_map(|&child_id| {
                    symbol_table
                        .scopes
                        .get(&child_id)
                        .filter(|child| child.kind == beacon_parser::ScopeKind::Function)
                        .map(|_| child_id)
                })
                .unwrap_or(parent_scope),
            None => parent_scope,
        }
    }

    /// Find the class scope (child of current scope with ScopeKind::Class)
    fn find_class_scope(&self, symbol_table: &SymbolTable, parent_scope: ScopeId) -> ScopeId {
        match symbol_table.scopes.get(&parent_scope) {
            Some(parent) => parent
                .children
                .iter()
                .find_map(|&child_id| {
                    symbol_table
                        .scopes
                        .get(&child_id)
                        .filter(|child| child.kind == beacon_parser::ScopeKind::Class)
                        .map(|_| child_id)
                })
                .unwrap_or(parent_scope),
            None => parent_scope,
        }
    }

    /// Map symbol kind to semantic token type index
    fn get_token_type_index(&self, symbol_kind: &SymbolKind) -> u32 {
        let token_type = match symbol_kind {
            SymbolKind::Function => SemanticTokenType::FUNCTION,
            SymbolKind::Class => SemanticTokenType::CLASS,
            SymbolKind::Parameter => SemanticTokenType::PARAMETER,
            SymbolKind::Variable => SemanticTokenType::VARIABLE,
            SymbolKind::Import => SemanticTokenType::NAMESPACE,
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

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _ = SemanticTokensProvider::new(documents);
    }

    #[test]
    fn test_supported_types_count() {
        assert!(!SUPPORTED_TYPES.is_empty());
        assert_eq!(SUPPORTED_TYPES.len(), 18);
    }

    #[test]
    fn test_supported_modifiers_count() {
        assert!(!SUPPORTED_MODIFIERS.is_empty());
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

        assert_eq!(tokens[0].token_type, func_type_idx, "Expected FUNCTION token type");
        assert_ne!(tokens[0].token_modifiers_bitset, 0, "Expected DEFINITION modifier");
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

        assert_eq!(tokens[0].token_type, class_type_idx, "Expected CLASS token type");
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

        assert_eq!(tokens[0].token_type, func_type_idx);
        assert_eq!(tokens[1].token_type, func_type_idx);
        assert_ne!(tokens[0].token_modifiers_bitset, 0, "Definition should have modifier");
        assert_eq!(tokens[1].token_modifiers_bitset, 0, "Usage should not have modifier");
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

        // Should have token for attribute 'path'
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
}
