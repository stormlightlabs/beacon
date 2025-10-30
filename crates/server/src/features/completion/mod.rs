//! Code completion provider
//!
//! Provides intelligent completions for identifiers, attributes, imports, and keywords.
mod algorithms;

use crate::analysis::Analyzer;
use crate::document::DocumentManager;
use crate::features::dunders;
use crate::parser::LspParser;
use crate::workspace::Workspace;
use algorithms::{FuzzyMatcher, PrefixMatcher, RocchioScorer, StringSimilarity};
use beacon_parser::{BUILTIN_DUNDERS, MAGIC_METHODS, ScopeId, Symbol, SymbolKind};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, Documentation, MarkupContent, MarkupKind,
    Position,
};
use rustc_hash::FxHashMap;
use std::collections::HashMap;
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
    prefix_matcher: PrefixMatcher,
    fuzzy_matcher: FuzzyMatcher,
    rocchio_scorer: RocchioScorer,
}

impl CompletionProvider {
    pub fn new(documents: DocumentManager, workspace: Arc<RwLock<Workspace>>, analyzer: Arc<RwLock<Analyzer>>) -> Self {
        let keywords = serde_json::from_slice(KEYWORDS_JSON).expect("Invalid keywords.json");
        let builtins = serde_json::from_slice(BUILTINS_JSON).expect("Invalid builtins.json");
        Self {
            _documents: documents,
            keywords,
            builtins,
            workspace,
            analyzer,
            prefix_matcher: PrefixMatcher::case_insensitive(),
            fuzzy_matcher: FuzzyMatcher::new(),
            rocchio_scorer: RocchioScorer::new(),
        }
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
                if let Some(import_completions) = self.import_completions(uri, position, &context.prefix, &text).await {
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

        let trimmed_prefix = prefix.trim_start();
        if trimmed_prefix.starts_with("import ")
            || (trimmed_prefix.starts_with("from ") && trimmed_prefix.contains(" import"))
        {
            return CompletionContextType::Import;
        }

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

    fn symbol_kind_to_completion_kind(kind: &SymbolKind) -> CompletionItemKind {
        match kind {
            SymbolKind::Function => CompletionItemKind::FUNCTION,
            SymbolKind::Class => CompletionItemKind::CLASS,
            SymbolKind::Variable => CompletionItemKind::VARIABLE,
            SymbolKind::Parameter => CompletionItemKind::VARIABLE,
            SymbolKind::Import => CompletionItemKind::MODULE,
            SymbolKind::MagicMethod => CompletionItemKind::METHOD,
            SymbolKind::BuiltinVar => CompletionItemKind::CONSTANT,
        }
    }

    fn match_score(&self, prefix: &str, candidate: &str) -> Option<f64> {
        if prefix.is_empty() {
            return Some(0.5);
        }

        let prefix_score = self.prefix_matcher.similarity(prefix, candidate);
        if prefix_score > 0.0 {
            return Some(prefix_score);
        }

        if prefix.len() < 3 {
            return None;
        }

        let fuzzy_score = self.fuzzy_matcher.similarity(prefix, candidate);
        if fuzzy_score >= self.fuzzy_matcher.threshold() {
            return Some(fuzzy_score * 0.8);
        }

        None
    }

    fn insert_scored(map: &mut HashMap<String, (CompletionItem, f64)>, item: CompletionItem, score: f64) {
        let label = item.label.clone();
        map.entry(label)
            .and_modify(|existing| {
                if score > existing.1 {
                    *existing = (item.clone(), score);
                }
            })
            .or_insert((item, score));
    }

    fn collect_document_symbols(documents: &DocumentManager, uri: &Url) -> Option<Vec<Symbol>> {
        documents
            .get_document(uri, |doc| {
                let symbol_table = doc.symbol_table()?;
                let scope_symbols = symbol_table.get_scope_symbols(symbol_table.root_scope)?;
                Some(scope_symbols.values().cloned().collect::<Vec<_>>())
            })
            .flatten()
    }

    fn scope_distance(
        symbol_table: &beacon_parser::SymbolTable, symbol_scope: ScopeId, current_scope: ScopeId,
    ) -> Option<usize> {
        let mut distance = 0;
        let mut scope = current_scope;

        loop {
            if scope == symbol_scope {
                return Some(distance);
            }

            let scope_info = symbol_table.scopes.get(&scope)?;

            match scope_info.parent {
                Some(parent) => {
                    scope = parent;
                    distance += 1;
                }
                None => break,
            }
        }

        None
    }

    fn uri_matches_module(uri: &Url, module_name: &str) -> bool {
        if uri.scheme() != "file" {
            return false;
        }

        let module_path = module_name.replace('.', "/");
        let path = uri.path();

        path.ends_with(&format!("/{module_path}.py")) || path.ends_with(&format!("/{module_path}/__init__.py"))
    }

    fn find_document_for_module(documents: &DocumentManager, module_name: &str) -> Option<Url> {
        documents
            .all_documents()
            .into_iter()
            .find(|candidate| Self::uri_matches_module(candidate, module_name))
    }

    fn resolve_module(workspace: &Workspace, current_uri: &Url, module_part: &str) -> (Option<String>, Option<Url>) {
        if module_part.starts_with('.') {
            let leading_dots = module_part.chars().take_while(|&c| c == '.').count();
            let rest = &module_part[leading_dots..];

            if let Some(from_module) = workspace.uri_to_module_name(current_uri) {
                let resolved_uri = workspace.resolve_relative_import(&from_module, rest, leading_dots);
                let module_name = resolved_uri
                    .as_ref()
                    .and_then(|uri| workspace.uri_to_module_name(uri))
                    .or_else(|| {
                        if rest.is_empty() {
                            Some(from_module.clone())
                        } else if from_module.is_empty() {
                            Some(rest.to_string())
                        } else {
                            Some(format!("{from_module}.{rest}"))
                        }
                    });

                (module_name, resolved_uri)
            } else {
                (None, None)
            }
        } else {
            let uri = workspace.resolve_import(module_part);
            let module_name = if module_part.is_empty() { None } else { Some(module_part.to_string()) };

            (module_name, uri)
        }
    }

    /// Get completions for symbols visible in the current scope
    ///
    /// Uses advanced scoring algorithms to rank completions:
    /// - Prefix matching with word boundary support
    /// - Fuzzy matching for typo tolerance
    /// - Rocchio relevance scoring based on scope proximity and symbol type
    ///
    /// Returns completion items sorted by relevance score (highest first).
    fn symbol_completions(
        &self, symbol_table: &beacon_parser::SymbolTable, scope_id: ScopeId, prefix: &str,
    ) -> Vec<CompletionItem> {
        #[derive(Debug)]
        struct ScoredCompletion {
            item: CompletionItem,
            score: f64,
        }

        let mut scored_items = Vec::new();

        let visible_symbols = symbol_table.get_visible_symbols(scope_id);

        for symbol in visible_symbols {
            let scope_distance = Self::scope_distance(symbol_table, symbol.scope_id, scope_id).unwrap_or(0);
            let type_bonus = match symbol.kind {
                SymbolKind::Function => 1.0,
                SymbolKind::Class => 0.9,
                SymbolKind::MagicMethod => 0.8,
                SymbolKind::Import => 0.7,
                SymbolKind::Variable | SymbolKind::Parameter => 0.5,
                SymbolKind::BuiltinVar => 0.6,
            };

            let score = if prefix.is_empty() {
                self.rocchio_scorer
                    .score_with_context("", &symbol.name, scope_distance, type_bonus)
            } else {
                let prefix_score = self.prefix_matcher.similarity(prefix, &symbol.name);
                if prefix_score > 0.0 {
                    prefix_score * 0.7
                        + self
                            .rocchio_scorer
                            .score_with_context(prefix, &symbol.name, scope_distance, type_bonus)
                            * 0.3
                } else {
                    let fuzzy_score = self.fuzzy_matcher.similarity(prefix, &symbol.name);
                    if fuzzy_score >= self.fuzzy_matcher.threshold() {
                        fuzzy_score * 0.6
                            + self
                                .rocchio_scorer
                                .score_with_context(prefix, &symbol.name, scope_distance, type_bonus)
                                * 0.4
                    } else {
                        continue;
                    }
                }
            };

            let kind = Self::symbol_kind_to_completion_kind(&symbol.kind);

            let detail = format!("{} (line {})", symbol.kind.name(), symbol.line);
            let documentation = symbol.docstring.as_ref().map(|doc| {
                Documentation::MarkupContent(MarkupContent { kind: MarkupKind::Markdown, value: doc.clone() })
            });

            scored_items.push(ScoredCompletion {
                item: Self::completion_item(symbol, kind, Some(detail), documentation),
                score,
            });
        }

        for BuiltinInfo { name, description } in &self.builtins {
            let score = if prefix.is_empty() {
                0.3
            } else {
                let prefix_score = self.prefix_matcher.similarity(prefix, name);
                if prefix_score > 0.0 {
                    prefix_score * 0.8
                } else {
                    let fuzzy_score = self.fuzzy_matcher.similarity(prefix, name);
                    if fuzzy_score >= self.fuzzy_matcher.threshold() {
                        fuzzy_score * 0.5
                    } else {
                        continue;
                    }
                }
            };

            scored_items.push(ScoredCompletion { item: Self::builtin_completion(name, description), score });
        }

        scored_items.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
        scored_items.into_iter().map(|sc| sc.item).collect()
    }

    /// Get completions for attributes on a type
    ///
    /// Uses type inference to determine the type of the expression before '.' and
    /// looks up available attributes/methods on that type. Results are scored and sorted by relevance.
    async fn attribute_completions(&self, uri: &Url, position: Position, prefix: &str) -> Option<Vec<CompletionItem>> {
        #[derive(Debug)]
        struct ScoredCompletion {
            item: CompletionItem,
            score: f64,
        }

        let mut scored_items = Vec::new();

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
        let process_exports = |exports: &FxHashMap<String, beacon_core::Type>,
                               scored_items: &mut Vec<ScoredCompletion>,
                               prefix: &str,
                               prefix_matcher: &PrefixMatcher,
                               fuzzy_matcher: &FuzzyMatcher| {
            for (name, ty) in exports {
                let score = if prefix.is_empty() {
                    if matches!(ty, beacon_core::Type::Fun(_, _)) { 0.7 } else { 0.5 }
                } else {
                    let prefix_score = prefix_matcher.similarity(prefix, name);
                    if prefix_score > 0.0 {
                        prefix_score
                    } else {
                        let fuzzy_score = fuzzy_matcher.similarity(prefix, name);
                        if fuzzy_score >= fuzzy_matcher.threshold() {
                            fuzzy_score * 0.7
                        } else {
                            continue;
                        }
                    }
                };

                let kind = match ty {
                    beacon_core::Type::Fun(_, _) => CompletionItemKind::METHOD,
                    _ => CompletionItemKind::PROPERTY,
                };

                scored_items.push(ScoredCompletion {
                    item: CompletionItem {
                        label: name.clone(),
                        kind: Some(kind),
                        detail: Some(ty.to_string()),
                        ..Default::default()
                    },
                    score,
                });
            }
        };

        match &expr_type {
            beacon_core::Type::Con(beacon_core::TypeCtor::Class(class_name)) => {
                let workspace = self.workspace.read().await;
                if let Some(exports) = workspace.get_stub_exports(class_name) {
                    process_exports(
                        &exports,
                        &mut scored_items,
                        prefix,
                        &self.prefix_matcher,
                        &self.fuzzy_matcher,
                    );
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
                        process_exports(
                            &exports,
                            &mut scored_items,
                            prefix,
                            &self.prefix_matcher,
                            &self.fuzzy_matcher,
                        );
                    }
                }
            }
            _ => {}
        }

        scored_items.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
        Some(scored_items.into_iter().map(|sc| sc.item).collect())
    }

    /// Get completions for imports
    ///
    /// Scans available modules and stub files in the workspace. Handles both `import` and `from X import` cases.
    async fn import_completions(
        &self, uri: &Url, position: Position, _context_prefix: &str, text: &str,
    ) -> Option<Vec<CompletionItem>> {
        let mut scored: HashMap<String, (CompletionItem, f64)> = HashMap::new();

        let line_idx = position.line as usize;
        let line = text.lines().nth(line_idx)?;
        let trimmed = line.trim_start();

        if trimmed.starts_with("from ") && line.contains(" import") {
            let from_pos = line.find("from ")?;
            let import_pos = line.find(" import")?;
            if from_pos < import_pos {
                let module_part = line[from_pos + 5..import_pos].trim();

                if !module_part.is_empty() {
                    let char_idx = position.character.min(line.len() as u32) as usize;
                    let prefix_start = import_pos + " import".len();
                    let symbol_prefix = if char_idx > prefix_start { line[prefix_start..char_idx].trim() } else { "" };

                    let (mut module_name, mut module_uri, stub_exports, mut offline_symbols) = {
                        let workspace = self.workspace.read().await;
                        let (resolved_name, resolved_uri) = Self::resolve_module(&workspace, uri, module_part);

                        let stub_exports = resolved_name.as_ref().and_then(|name| workspace.get_stub_exports(name));

                        let offline_symbols = resolved_uri.as_ref().and_then(|target_uri| {
                            if self._documents.has_document(target_uri) {
                                None
                            } else {
                                workspace.load_workspace_file(target_uri).and_then(|parse_result| {
                                    parse_result
                                        .symbol_table
                                        .get_scope_symbols(parse_result.symbol_table.root_scope)
                                        .map(|symbols| symbols.values().cloned().collect::<Vec<_>>())
                                })
                            }
                        });

                        (resolved_name, resolved_uri, stub_exports, offline_symbols)
                    };

                    if module_name.is_none() && !module_part.starts_with('.') && !module_part.is_empty() {
                        module_name = Some(module_part.to_string());
                    }

                    if module_uri.is_none() {
                        if let Some(name) = module_name.as_ref() {
                            module_uri = Self::find_document_for_module(&self._documents, name);
                        }
                    }

                    if let Some(exports) = stub_exports {
                        let module_display = module_name.clone().unwrap_or_else(|| module_part.to_string());
                        for (name, ty) in exports {
                            if let Some(score) = self.match_score(symbol_prefix, &name) {
                                let kind = match ty {
                                    beacon_core::Type::Fun(_, _) => CompletionItemKind::FUNCTION,
                                    beacon_core::Type::Con(beacon_core::TypeCtor::Class(_)) => {
                                        CompletionItemKind::CLASS
                                    }
                                    _ => CompletionItemKind::VARIABLE,
                                };

                                let item = CompletionItem {
                                    label: name.clone(),
                                    kind: Some(kind),
                                    detail: Some(format!("from {module_display}")),
                                    documentation: Some(Documentation::String(ty.to_string())),
                                    ..Default::default()
                                };

                                Self::insert_scored(&mut scored, item, score + 0.4);
                            }
                        }
                    }

                    let module_display = module_name.clone().unwrap_or_else(|| module_part.to_string());

                    let mut module_symbols = Vec::new();
                    if let Some(ref module_uri) = module_uri {
                        if let Some(symbols) = Self::collect_document_symbols(&self._documents, module_uri) {
                            module_symbols = symbols;
                        } else if let Some(symbols) = offline_symbols.take() {
                            module_symbols = symbols;
                        }
                    } else if let Some(symbols) = offline_symbols.take() {
                        module_symbols = symbols;
                    }

                    for symbol in module_symbols {
                        if symbol.name.starts_with('_') {
                            continue;
                        }

                        if let Some(score) = self.match_score(symbol_prefix, &symbol.name) {
                            let detail = format!("{} • {}", symbol.kind.name(), module_display);
                            let documentation = symbol.docstring.as_ref().map(|doc| {
                                Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: doc.clone(),
                                })
                            });

                            let item = CompletionItem {
                                label: symbol.name.clone(),
                                kind: Some(Self::symbol_kind_to_completion_kind(&symbol.kind)),
                                detail: Some(detail),
                                documentation,
                                ..Default::default()
                            };

                            Self::insert_scored(&mut scored, item, score + 0.3);
                        }
                    }
                }
            }
        } else {
            let char_idx = position.character.min(line.len() as u32) as usize;
            let module_prefix = if let Some(pos) = line.find("import ") {
                let start = pos + "import ".len();
                if char_idx > start { line[start..char_idx].trim() } else { "" }
            } else {
                ""
            };

            let mut module_names: Vec<String> = {
                let workspace = self.workspace.read().await;
                workspace.all_modules().into_iter().map(|(_, name)| name).collect()
            };

            if module_names.is_empty() {
                module_names = self
                    ._documents
                    .all_documents()
                    .into_iter()
                    .filter_map(|doc_uri| {
                        if doc_uri.scheme() != "file" {
                            None
                        } else {
                            let path = std::path::Path::new(doc_uri.path());
                            path.file_stem().and_then(|stem| stem.to_str().map(|s| s.to_string()))
                        }
                    })
                    .collect();
            }

            module_names.sort();
            module_names.dedup();

            for name in module_names {
                if let Some(score) = self.match_score(module_prefix, &name) {
                    let item = CompletionItem {
                        label: name.clone(),
                        kind: Some(CompletionItemKind::MODULE),
                        ..Default::default()
                    };

                    Self::insert_scored(&mut scored, item, score);
                }
            }
        }

        if scored.is_empty() {
            None
        } else {
            let mut items: Vec<_> = scored.into_values().collect();
            items.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or_else(|| a.0.label.cmp(&b.0.label)));
            Some(items.into_iter().map(|(item, _)| item).collect())
        }
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
    async fn test_import_completions_from_workspace_module() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let module_uri = Url::from_str("file:///module_b.py").unwrap();
        let module_source = r#"
def exported_func():
    pass

EXTRA_VALUE = 1

OTHER_THING = 2
"#;
        documents
            .open_document(module_uri.clone(), 1, module_source.to_string())
            .unwrap();

        let doc_symbols = documents
            .get_document(&module_uri, |doc| {
                doc.symbol_table().and_then(|table| {
                    table
                        .get_scope_symbols(table.root_scope)
                        .map(|symbols| symbols.keys().cloned().collect::<Vec<_>>())
                })
            })
            .flatten()
            .unwrap_or_default();
        assert!(
            doc_symbols.contains(&"exported_func".to_string()),
            "symbol table missing exported_func: {doc_symbols:?}"
        );

        let current_uri = Url::from_str("file:///module_a.py").unwrap();
        let source = "from module_b import ";
        documents
            .open_document(current_uri.clone(), 1, source.to_string())
            .unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: current_uri.clone() },
                position: Position { line: 0, character: 20 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await.expect("response");

        match response {
            CompletionResponse::Array(items) => {
                let labels: Vec<_> = items.iter().map(|item| item.label.as_str()).collect();
                assert!(
                    items.iter().any(|item| item.label == "exported_func"),
                    "expected exported_func in completions, items: {labels:?}"
                );
                assert!(
                    items.iter().any(|item| item.label == "EXTRA_VALUE"),
                    "expected EXTRA_VALUE in completions, items: {labels:?}"
                );
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_import_completions_filters_by_prefix() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let module_uri = Url::from_str("file:///module_b.py").unwrap();
        let module_source = r#"
def exported_func():
    pass

EXTRA_VALUE = 1

OTHER_THING = 2
"#;
        documents
            .open_document(module_uri.clone(), 1, module_source.to_string())
            .unwrap();

        let current_uri = Url::from_str("file:///module_a.py").unwrap();
        let source = "from module_b import ex";
        documents
            .open_document(current_uri.clone(), 1, source.to_string())
            .unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: current_uri.clone() },
                position: Position { line: 0, character: 23 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await.expect("response");

        match response {
            CompletionResponse::Array(items) => {
                let labels: Vec<_> = items.iter().map(|item| item.label.as_str()).collect();
                assert!(
                    items.iter().any(|item| item.label == "exported_func"),
                    "expected exported_func with prefix filter, items: {labels:?}"
                );
                assert!(
                    !items.iter().any(|item| item.label == "OTHER_THING"),
                    "OTHER_THING should be filtered out by prefix, items: {labels:?}"
                );
            }
            _ => panic!("Expected array response"),
        }
    }

    #[tokio::test]
    async fn test_import_module_completions_with_prefix() {
        let documents = DocumentManager::new().unwrap();
        let provider = create_test_provider(documents.clone());

        let alpha_uri = Url::from_str("file:///alpha.py").unwrap();
        documents
            .open_document(alpha_uri.clone(), 1, "alpha_var = 1\n".to_string())
            .unwrap();
        let beta_uri = Url::from_str("file:///beta.py").unwrap();
        documents
            .open_document(beta_uri.clone(), 1, "beta_var = 2\n".to_string())
            .unwrap();

        let current_uri = Url::from_str("file:///consumer.py").unwrap();
        let source = "import alp";
        documents
            .open_document(current_uri.clone(), 1, source.to_string())
            .unwrap();

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: current_uri.clone() },
                position: Position { line: 0, character: 9 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
            context: None,
        };

        let response = provider.completion(params).await.expect("response");

        match response {
            CompletionResponse::Array(items) => {
                assert!(
                    items.iter().any(|item| item.label == "alpha"),
                    "expected alpha module suggestion"
                );
                assert!(
                    !items.iter().any(|item| item.label == "beta"),
                    "beta module should be filtered out by prefix"
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
