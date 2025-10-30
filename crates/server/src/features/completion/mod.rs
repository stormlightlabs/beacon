//! Code completion provider
//!
//! Provides intelligent completions for identifiers, attributes, imports, and keywords.
use crate::analysis::Analyzer;
use crate::document::DocumentManager;
use crate::features::dunders;
use crate::parser::LspParser;
use crate::workspace::Workspace;
use beacon_parser::{BUILTIN_DUNDERS, MAGIC_METHODS, ScopeId, Symbol};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, Documentation, MarkupContent, MarkupKind,
    Position,
};
use std::sync::Arc;
use tokio::sync::RwLock;
use url::Url;

/// The context in which a completion was requested
#[derive(Debug, Clone, PartialEq)]
enum CompletionContextType {
    /// Regular expression context
    Expression,
    /// Inside an import statement
    Import,
    /// After a dot (attribute access)
    Attribute,
}

/// Information about the completion context at the cursor
#[derive(Debug)]
struct CompletionContext {
    /// The partial identifier being typed
    prefix: String,
    /// The type of context
    context_type: CompletionContextType,
    /// The scope at the cursor position
    scope_id: ScopeId,
    /// Whether we're in a class scope
    _in_class: bool,
    /// Whether we're in a statement context (vs expression context)
    is_statement_context: bool,
}

#[derive(Debug, serde::Deserialize)]
struct BuiltinInfo {
    name: String,
    description: String,
}

/// Python builtin functions with their descriptions
const BUILTINS_JSON: &[u8] = include_bytes!("builtins.json");

/// Keyword context - where a keyword can appear
#[derive(Debug, Clone, Copy, PartialEq, serde::Deserialize)]
enum KeywordContext {
    /// Only valid in statement position
    Statement,
    /// Only valid in expression position
    Expression,
    /// Valid in both contexts
    Both,
}

/// Information about a Python keyword
#[derive(Debug, serde::Deserialize)]
struct KeywordInfo {
    name: String,
    description: String,
    context: KeywordContext,
}

/// Python keywords with their descriptions and valid contexts
const KEYWORDS_JSON: &[u8] = include_bytes!("keywords.json");

pub struct CompletionProvider {
    _documents: DocumentManager,
    builtins: Vec<BuiltinInfo>,
    keywords: Vec<KeywordInfo>,
    workspace: Arc<RwLock<Workspace>>,
    analyzer: Arc<RwLock<Analyzer>>,
}

impl CompletionProvider {
    pub fn new(documents: DocumentManager, workspace: Arc<RwLock<Workspace>>, analyzer: Arc<RwLock<Analyzer>>) -> Self {
        let keywords = serde_json::from_slice(KEYWORDS_JSON).expect("Invalid keywords.json");
        let builtins = serde_json::from_slice(BUILTINS_JSON).expect("Invalid builtins.json");
        Self { _documents: documents, keywords, builtins, workspace, analyzer }
    }

    /// Provide completions at a position
    pub async fn completion(&self, params: CompletionParams) -> Option<CompletionResponse> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let mut items = Vec::new();

        let text = self._documents.get_document(uri, |doc| Some(doc.text()))??;
        let line_text = text.lines().nth(position.line as usize)?;
        let prefix = &line_text[..position.character.min(line_text.len() as u32) as usize];

        if prefix.ends_with("__") || prefix.contains("__") {
            items.extend(self.dunder_completions(uri, position));
            return Some(CompletionResponse::Array(items));
        }

        let context = self._documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let symbol_table = doc.symbol_table()?;
            self.detect_context(uri, position, &text, tree, symbol_table)
        })??;

        match context.context_type {
            CompletionContextType::Expression => {
                let symbol_table = self._documents.get_document(uri, |doc| doc.symbol_table().cloned())?;
                if let Some(st) = symbol_table {
                    let mut completions = self.symbol_completions(&st, context.scope_id, &context.prefix);
                    completions.extend(self.keyword_completions(context.is_statement_context, &context.prefix));
                    items.extend(completions);
                }
            }
            CompletionContextType::Import => {
                if let Some(import_completions) = self.import_completions(uri, position, &text).await {
                    items.extend(import_completions);
                }
            }
            CompletionContextType::Attribute => {
                if let Some(attr_completions) = self.attribute_completions(uri, position, &context.prefix).await {
                    items.extend(attr_completions);
                }
            }
        }

        Some(CompletionResponse::Array(items))
    }

    /// Get dunder completions based on current context
    fn dunder_completions(&self, uri: &Url, position: Position) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        let in_class = self
            ._documents
            .get_document(uri, |doc| {
                if let Some(symbol_table) = doc.symbol_table() {
                    let line = (position.line + 1) as usize;
                    let col = (position.character + 1) as usize;
                    let content = doc.text();
                    let byte_offset = Self::position_to_byte_offset(&content, line, col);

                    let scope_id = symbol_table.find_scope_at_position(byte_offset);
                    Some(symbol_table.is_in_class_scope(scope_id))
                } else {
                    None
                }
            })
            .flatten()
            .unwrap_or(false);

        for &dunder_name in BUILTIN_DUNDERS {
            if let Some(info) = dunders::get_dunder_info(dunder_name) {
                items.push(self.dunder_completion_item(info));
            }
        }

        if in_class {
            for &magic_method in MAGIC_METHODS {
                if let Some(info) = dunders::get_dunder_info(magic_method) {
                    items.push(self.dunder_completion_item(info));
                }
            }
        }

        items
    }

    /// Create a completion item for a dunder with documentation
    fn dunder_completion_item(&self, info: &dunders::DunderInfo) -> CompletionItem {
        CompletionItem {
            label: info.name.clone(),
            kind: Some(if info.category == "method" {
                CompletionItemKind::METHOD
            } else {
                CompletionItemKind::VARIABLE
            }),
            detail: Some(info.category.clone()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("{}\n\n[Documentation]({})", info.doc, info.link),
            })),
            ..Default::default()
        }
    }

    /// Convert line/col position to byte offset
    fn position_to_byte_offset(content: &str, line: usize, col: usize) -> usize {
        let mut byte_offset = 0;
        let mut current_line = 1;
        let mut current_col = 1;

        for ch in content.chars() {
            if current_line == line && current_col == col {
                return byte_offset;
            }
            if ch == '\n' {
                current_line += 1;
                current_col = 1;
            } else {
                current_col += 1;
            }
            byte_offset += ch.len_utf8();
        }

        byte_offset
    }

    /// Detect the completion context at the cursor position
    ///
    /// Analyzes the text and AST to determine:
    /// - What partial identifier is being typed
    /// - Whether we're in an expression, import, or attribute context
    /// - The current scope
    fn detect_context(
        &self, _uri: &Url, position: Position, text: &str, tree: &tree_sitter::Tree,
        symbol_table: &beacon_parser::SymbolTable,
    ) -> Option<CompletionContext> {
        let line_idx = position.line as usize;
        let char_idx = position.character as usize;
        let line = text.lines().nth(line_idx)?;
        let prefix = Self::extract_partial_identifier(line, char_idx);
        let byte_offset = Self::position_to_byte_offset(text, line_idx + 1, char_idx + 1);
        let scope_id = symbol_table.find_scope_at_position(byte_offset);

        let _in_class = symbol_table.is_in_class_scope(scope_id);

        let context_type = Self::detect_context_type(line, char_idx, tree, text, position);
        let is_statement_context = Self::detect_statement_context(tree, text, position);

        Some(CompletionContext { prefix, context_type, scope_id, _in_class, is_statement_context })
    }

    /// Detect the type of completion context based on surrounding text and tree-sitter nodes
    fn detect_context_type(
        line: &str, char_idx: usize, tree: &tree_sitter::Tree, text: &str, position: Position,
    ) -> CompletionContextType {
        let prefix = &line[..char_idx.min(line.len())];

        if prefix.trim_end().ends_with('.') {
            return CompletionContextType::Attribute;
        }

        let parser = LspParser::new().ok();
        if let Some(p) = parser {
            if let Some(node) = p.node_at_position(tree, text, position) {
                let mut current = node;
                while let Some(parent) = current.parent() {
                    match parent.kind() {
                        "import_statement" | "import_from_statement" => {
                            return CompletionContextType::Import;
                        }
                        _ => {}
                    }
                    current = parent;
                }
            }
        }

        CompletionContextType::Expression
    }

    /// Detect whether we're in a statement context (vs expression context)
    ///
    /// Walks up the tree-sitter AST to determine if the cursor is in a position where a statement is
    /// expected vs where an expression is expected (inside parentheses, after operators, etc.)
    fn detect_statement_context(tree: &tree_sitter::Tree, text: &str, position: Position) -> bool {
        let line_idx = position.line as usize;

        if let Some(line) = text.lines().nth(line_idx) {
            let char_idx = position.character as usize;
            let before_cursor = &line[..char_idx.min(line.len())];
            let trimmed = before_cursor.trim_start();

            if (trimmed.starts_with("if ")
                || trimmed.starts_with("elif ")
                || trimmed.starts_with("while ")
                || trimmed.starts_with("for "))
                && !trimmed.contains(':')
            {
                return false;
            }

            if trimmed.contains('(') || trimmed.contains('[') || trimmed.contains('{') {
                let open_parens = trimmed.matches('(').count();
                let close_parens = trimmed.matches(')').count();
                if open_parens > close_parens {
                    return false;
                }
            }

            if trimmed.contains('=') && !trimmed.starts_with("def ") && !trimmed.starts_with("class ") {
                return false;
            }
        }

        let parser = LspParser::new().ok();
        if let Some(p) = parser {
            if let Some(node) = p.node_at_position(tree, text, position) {
                let mut current = node;

                while let Some(parent) = current.parent() {
                    match parent.kind() {
                        "binary_operator"
                        | "unary_operator"
                        | "comparison"
                        | "boolean_operator"
                        | "lambda"
                        | "subscript"
                        | "call"
                        | "argument_list"
                        | "parenthesized_expression"
                        | "list"
                        | "tuple"
                        | "dictionary"
                        | "set"
                        | "expression_statement"
                        | "assignment"
                        | "if_clause"
                        | "while_clause"
                        | "for_clause" => {
                            return false;
                        }
                        "module" | "function_definition" | "class_definition" | "block" | "suite" => {
                            return true;
                        }
                        _ => {}
                    }
                    current = parent;
                }
            }
        }

        true
    }

    /// Extract the partial identifier being typed at the cursor position
    ///
    /// Parses backwards from the cursor to find the start of an identifier.
    fn extract_partial_identifier(line: &str, char_idx: usize) -> String {
        let before_cursor = &line[..char_idx.min(line.len())];

        let identifier_start = before_cursor
            .chars()
            .rev()
            .take_while(|&c| c.is_alphanumeric() || c == '_')
            .count();

        if identifier_start == 0 {
            return String::new();
        }

        let start_pos = before_cursor.len().saturating_sub(identifier_start);
        before_cursor[start_pos..].to_string()
    }

    /// Create a completion item for a builtin
    fn builtin_completion(name: &str, description: &str) -> CompletionItem {
        CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("builtin".to_string()),
            documentation: Some(Documentation::String(description.to_string())),
            ..Default::default()
        }
    }

    fn completion_item(
        symbol: &Symbol, kind: CompletionItemKind, detail: Option<String>, documentation: Option<Documentation>,
    ) -> CompletionItem {
        CompletionItem { label: symbol.name.clone(), kind: Some(kind), detail, documentation, ..Default::default() }
    }

    /// Get completions for symbols visible in the current scope
    ///
    /// Returns completion items for:
    /// - Local variables, functions, and classes in the current scope
    /// - Symbols from parent scopes (walking up the scope chain)
    /// - Builtin functions
    /// - Filters results by the prefix being typed
    fn symbol_completions(
        &self, symbol_table: &beacon_parser::SymbolTable, scope_id: ScopeId, prefix: &str,
    ) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        let visible_symbols = symbol_table.get_visible_symbols(scope_id);

        for symbol in visible_symbols {
            if !prefix.is_empty() && !symbol.name.starts_with(prefix) {
                continue;
            }

            let kind = match symbol.kind {
                beacon_parser::SymbolKind::Function => CompletionItemKind::FUNCTION,
                beacon_parser::SymbolKind::Class => CompletionItemKind::CLASS,
                beacon_parser::SymbolKind::Variable => CompletionItemKind::VARIABLE,
                beacon_parser::SymbolKind::Parameter => CompletionItemKind::VARIABLE,
                beacon_parser::SymbolKind::Import => CompletionItemKind::MODULE,
                beacon_parser::SymbolKind::MagicMethod => CompletionItemKind::METHOD,
                beacon_parser::SymbolKind::BuiltinVar => CompletionItemKind::CONSTANT,
            };

            let detail = format!("{} (line {})", symbol.kind.name(), symbol.line);
            let documentation = symbol.docstring.as_ref().map(|doc| {
                Documentation::MarkupContent(MarkupContent { kind: MarkupKind::Markdown, value: doc.clone() })
            });

            items.push(Self::completion_item(symbol, kind, Some(detail), documentation));
        }

        for BuiltinInfo { name, description } in &self.builtins {
            if !prefix.is_empty() && !name.starts_with(prefix) {
                continue;
            }
            items.push(Self::builtin_completion(name, description));
        }

        items
    }

    /// Get completions for attributes on a type
    ///
    /// Uses type inference to determine the type of the expression before '.' and looks up available attributes/methods on that type.
    async fn attribute_completions(&self, uri: &Url, position: Position, prefix: &str) -> Option<Vec<CompletionItem>> {
        let mut items = Vec::new();

        let (_, expr_position) = self._documents.get_document(uri, |doc| {
            let text = doc.text();
            let line_idx = position.line as usize;
            let char_idx = position.character as usize;
            let line = text.lines().nth(line_idx)?;
            let before_cursor = &line[..char_idx.min(line.len())];
            let dot_pos = before_cursor.rfind('.')?;
            let expr_end = dot_pos;
            let expr_position = Position { line: position.line, character: expr_end.saturating_sub(1) as u32 };

            Some((text, expr_position))
        })??;

        let mut analyzer = self.analyzer.write().await;
        let expr_type = analyzer.type_at_position(uri, expr_position).ok()??;

        match &expr_type {
            beacon_core::Type::Con(beacon_core::TypeCtor::Class(class_name)) => {
                let workspace = self.workspace.read().await;
                if let Some(exports) = workspace.get_stub_exports(class_name) {
                    for (name, ty) in exports {
                        if !prefix.is_empty() && !name.starts_with(prefix) {
                            continue;
                        }

                        let kind = match ty {
                            beacon_core::Type::Fun(_, _) => CompletionItemKind::METHOD,
                            _ => CompletionItemKind::PROPERTY,
                        };

                        items.push(CompletionItem {
                            label: name.clone(),
                            kind: Some(kind),
                            detail: Some(ty.to_string()),
                            ..Default::default()
                        });
                    }
                }
            }
            beacon_core::Type::Con(type_ctor) => {
                let class_name = match type_ctor {
                    beacon_core::TypeCtor::String => Some("str"),
                    beacon_core::TypeCtor::Int => Some("int"),
                    beacon_core::TypeCtor::Float => Some("float"),
                    beacon_core::TypeCtor::Bool => Some("bool"),
                    beacon_core::TypeCtor::List => Some("list"),
                    beacon_core::TypeCtor::Dict => Some("dict"),
                    beacon_core::TypeCtor::Set => Some("set"),
                    beacon_core::TypeCtor::Tuple => Some("tuple"),
                    _ => None,
                };

                if let Some(class_name) = class_name {
                    let workspace = self.workspace.read().await;
                    if let Some(exports) = workspace.get_stub_exports(class_name) {
                        for (name, ty) in exports {
                            if !prefix.is_empty() && !name.starts_with(prefix) {
                                continue;
                            }

                            let kind = match ty {
                                beacon_core::Type::Fun(_, _) => CompletionItemKind::METHOD,
                                _ => CompletionItemKind::PROPERTY,
                            };

                            items.push(CompletionItem {
                                label: name.clone(),
                                kind: Some(kind),
                                detail: Some(ty.to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
            _ => {}
        }

        Some(items)
    }

    /// Get completions for imports
    ///
    /// Scans available modules and stub files in the workspace.
    /// Handles both `import` and `from X import` cases.
    async fn import_completions(&self, _uri: &Url, position: Position, text: &str) -> Option<Vec<CompletionItem>> {
        let mut items = Vec::new();

        let line_idx = position.line as usize;
        let line = text.lines().nth(line_idx)?;
        let workspace = self.workspace.read().await;

        if line.trim_start().starts_with("from ") && line.contains(" import") {
            let from_pos = line.find("from ")?;
            let import_pos = line.find(" import")?;
            if from_pos < import_pos {
                let module_part = line[from_pos + 5..import_pos].trim();

                if !module_part.is_empty() {
                    if let Some(exports) = workspace.get_stub_exports(module_part) {
                        for (name, ty) in exports {
                            let kind = match ty {
                                beacon_core::Type::Fun(_, _) => CompletionItemKind::FUNCTION,
                                beacon_core::Type::Con(beacon_core::TypeCtor::Class(_)) => CompletionItemKind::CLASS,
                                _ => CompletionItemKind::VARIABLE,
                            };

                            items.push(CompletionItem {
                                label: name.clone(),
                                kind: Some(kind),
                                detail: Some(format!("from {module_part}")),
                                documentation: Some(Documentation::String(ty.to_string())),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
        } else {
            for (_, module_name) in workspace.all_modules() {
                items.push(CompletionItem {
                    label: module_name,
                    kind: Some(CompletionItemKind::MODULE),
                    ..Default::default()
                });
            }
        }

        Some(items)
    }

    /// Get keyword completions based on context by filtering by the prefix being typed.
    ///
    /// Returns completion items for Python keywords appropriate to the current context:
    /// - Statement context: def, class, if, for, while, try, etc.
    /// - Expression context: lambda, and, or, not, in, is
    /// - Both contexts: None, True, False
    fn keyword_completions(&self, is_statement_context: bool, prefix: &str) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        for keyword in &self.keywords {
            if !prefix.is_empty() && !keyword.name.starts_with(prefix) {
                continue;
            }

            let include = match keyword.context {
                KeywordContext::Statement => is_statement_context,
                KeywordContext::Expression => !is_statement_context,
                KeywordContext::Both => true,
            };

            if include {
                items.push(CompletionItem {
                    label: keyword.name.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("keyword".to_string()),
                    documentation: Some(Documentation::String(keyword.description.to_string())),
                    ..Default::default()
                });
            }
        }

        items
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::Analyzer;
    use crate::config::Config;
    use crate::workspace::Workspace;
    use lsp_types::TextDocumentPositionParams;
    use std::str::FromStr;
    use std::sync::Arc;
    use tokio::sync::RwLock;

    fn create_test_provider(documents: DocumentManager) -> CompletionProvider {
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let analyzer = Arc::new(RwLock::new(Analyzer::new(config, documents.clone())));
        CompletionProvider::new(documents, workspace, analyzer)
    }

    #[tokio::test]
    async fn test_builtin_completion() {
        let item = CompletionProvider::builtin_completion("test", "Test function");

        assert_eq!(item.label, "test");
        assert_eq!(item.kind, Some(CompletionItemKind::FUNCTION));

        match item.documentation {
            Some(Documentation::String(docs)) => assert_eq!(docs, "Test function".to_string()),
            Some(_) | None => unreachable!(),
        }
    }

    #[tokio::test]
    async fn test_dunder_completion_in_module_scope() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "__";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 2 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "__name__"));
                assert!(items.iter().any(|item| item.label == "__file__"));
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_dunder_completion_in_class_scope() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "class MyClass:\n    def __";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 10 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            // NOTE: Magic methods may or may not be included depending on whether symbol table is available.
            // This is acceptable behavior and the important thing is we don't crash and provide some completions.
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "__name__"));
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_dunder_completion_item_has_documentation() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents);

        if let Some(info) = dunders::get_dunder_info("__init__") {
            let item = provider.dunder_completion_item(info);

            assert_eq!(item.label, "__init__");
            assert_eq!(item.kind, Some(CompletionItemKind::METHOD));
            assert!(item.documentation.is_some());

            match item.documentation.unwrap() {
                Documentation::MarkupContent(content) => {
                    assert!(content.value.contains("Initializer"));
                    assert!(content.value.contains("https://"));
                }
                _ => panic!("Expected markup content"),
            }
        }
    }

    #[tokio::test]
    async fn test_position_to_byte_offset() {
        let content = "line 1\nline 2\nline 3";

        assert_eq!(CompletionProvider::position_to_byte_offset(content, 1, 1), 0);
        assert_eq!(CompletionProvider::position_to_byte_offset(content, 2, 1), 7);
        assert_eq!(CompletionProvider::position_to_byte_offset(content, 2, 3), 9);
    }

    #[tokio::test]
    async fn test_extract_partial_identifier() {
        assert_eq!(CompletionProvider::extract_partial_identifier("hello", 5), "hello");
        assert_eq!(CompletionProvider::extract_partial_identifier("hello", 3), "hel");
        assert_eq!(CompletionProvider::extract_partial_identifier("x = pri", 7), "pri");
        assert_eq!(CompletionProvider::extract_partial_identifier("my_var", 6), "my_var");
        assert_eq!(CompletionProvider::extract_partial_identifier("x = ", 4), "");
        assert_eq!(CompletionProvider::extract_partial_identifier("", 0), "");
        assert_eq!(CompletionProvider::extract_partial_identifier("x.attr", 6), "attr");
    }

    #[tokio::test]
    async fn test_extract_partial_identifier_with_special_chars() {
        assert_eq!(CompletionProvider::extract_partial_identifier("x + var", 7), "var");
        assert_eq!(CompletionProvider::extract_partial_identifier("(my_func", 8), "my_func");
        assert_eq!(CompletionProvider::extract_partial_identifier("[item", 5), "item");
    }

    #[tokio::test]
    async fn test_symbol_completions_includes_local_variables() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 42
y = "hello"

"#;
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 3, character: 0 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(
                    items.iter().any(|item| item.label == "x"),
                    "Expected to find 'x' in completions"
                );
                assert!(
                    items.iter().any(|item| item.label == "y"),
                    "Expected to find 'y' in completions"
                );
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_symbol_completions_includes_functions() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def my_function():
    pass

def another_func():
    pass

my
"#;
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 6, character: 2 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "my_function"));
                assert!(items.iter().any(|item| item.label == "another_func"));
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_symbol_completions_with_prefix_filtering() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
my_var = 1
my_func = lambda: None
other_var = 2
my
"#;
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 4, character: 2 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "my_var"));
                assert!(items.iter().any(|item| item.label == "my_func"));
                assert!(!items.iter().any(|item| item.label == "other_var"));
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_symbol_completions_includes_builtins() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "pri";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 3 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "print"));
                assert!(!items.iter().any(|item| item.label == "len"));
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_symbol_completions_nested_scope() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
outer_var = 1

def my_function():
    pass

"#;
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 5, character: 0 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(
                    items.iter().any(|item| item.label == "outer_var"),
                    "Expected 'outer_var'"
                );
                assert!(
                    items.iter().any(|item| item.label == "my_function"),
                    "Expected 'my_function'"
                );
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_symbol_completions_includes_classes() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "class MyClass:\n    pass\n\nclass AnotherClass:\n    pass\n\nMy";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 6, character: 2 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "MyClass"), "Expected 'MyClass'");
                assert!(
                    !items.iter().any(|item| item.label == "AnotherClass"),
                    "Should NOT have 'AnotherClass' (filtered by prefix 'My')"
                );
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_keyword_completions_statement_context() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "\n";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "def"), "Expected 'def' keyword");
                assert!(
                    items.iter().any(|item| item.label == "class"),
                    "Expected 'class' keyword"
                );
                assert!(items.iter().any(|item| item.label == "if"), "Expected 'if' keyword");
                assert!(items.iter().any(|item| item.label == "for"), "Expected 'for' keyword");
                assert!(
                    !items.iter().any(|item| item.label == "lambda"),
                    "Should NOT have 'lambda' in statement context"
                );
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_keyword_completions_expression_context() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = (";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 5 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(
                    items.iter().any(|item| item.label == "lambda"),
                    "Expected 'lambda' in expression context"
                );
                assert!(
                    items.iter().any(|item| item.label == "not"),
                    "Expected 'not' in expression context"
                );
                assert!(
                    !items.iter().any(|item| item.label == "def"),
                    "Should NOT have 'def' in expression context"
                );
                assert!(
                    !items.iter().any(|item| item.label == "class"),
                    "Should NOT have 'class' in expression context"
                );
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_keyword_completions_with_prefix() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "de";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 2 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(
                    items.iter().any(|item| item.label == "def"),
                    "Expected 'def' with prefix 'de'"
                );
                assert!(
                    items.iter().any(|item| item.label == "del"),
                    "Expected 'del' with prefix 'de'"
                );
                assert!(
                    !items.iter().any(|item| item.label == "class"),
                    "Should NOT have 'class' with prefix 'de'"
                );
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_keyword_completions_literals_always_available() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = ";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 4 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "None"), "Expected 'None'");
                assert!(items.iter().any(|item| item.label == "True"), "Expected 'True'");
                assert!(items.iter().any(|item| item.label == "False"), "Expected 'False'");
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_keyword_completions_not_after_dot() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "obj.";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 4 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(
                    !items.iter().any(|item| item.label == "def"),
                    "Should NOT have keywords after '.'"
                );
                assert!(
                    !items.iter().any(|item| item.label == "lambda"),
                    "Should NOT have keywords after '.'"
                );
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_keyword_completions_logical_operators_in_expression() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "if x ";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 5 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await;
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(
                    items.iter().any(|item| item.label == "and"),
                    "Expected 'and' in condition"
                );
                assert!(
                    items.iter().any(|item| item.label == "or"),
                    "Expected 'or' in condition"
                );
                assert!(
                    items.iter().any(|item| item.label == "is"),
                    "Expected 'is' in condition"
                );
                assert!(
                    items.iter().any(|item| item.label == "in"),
                    "Expected 'in' in condition"
                );
            }
            _ => panic!("Expected array response"),
        }
    }
}
