//! Workspace symbols provider
//!
//! Searches for symbols across all documents in the workspace and supports fuzzy matching and returns symbol locations for quick navigation.

use crate::document::DocumentManager;
use crate::workspace::Workspace;

use beacon_parser::{AstNode, SymbolKind, SymbolTable};
use lsp_types::{
    Location, Position, Range, SymbolInformation, SymbolKind as LspSymbolKind, SymbolTag, Url, WorkspaceSymbol,
    WorkspaceSymbolParams,
};
use std::sync::Arc;
use tokio::sync::RwLock;

/// Workspace symbols provider
///
/// Implements workspace-wide symbol search with fuzzy matching for quick navigation to any symbol in the workspace.
pub struct WorkspaceSymbolsProvider {
    documents: DocumentManager,
    workspace: Arc<RwLock<Workspace>>,
}

impl WorkspaceSymbolsProvider {
    pub fn new(documents: DocumentManager, workspace: Arc<RwLock<Workspace>>) -> Self {
        Self { documents, workspace }
    }

    /// Search for symbols across the workspace
    ///
    /// Returns symbols matching the query from all workspace files using fuzzy matching.
    pub fn workspace_symbol(&self, params: WorkspaceSymbolParams) -> Option<Vec<SymbolInformation>> {
        let query = params.query.to_lowercase();
        let mut results = Vec::new();

        for uri in self.documents.all_documents() {
            self.documents.get_document(&uri, |doc| {
                let symbol_table = doc.symbol_table()?;
                let ast = doc.ast()?;

                Self::collect_matching_symbols(&uri, ast, symbol_table, &query, &mut results);
                Some(())
            });
        }

        let workspace_uris = tokio::runtime::Handle::try_current()
            .ok()
            .and_then(|_| {
                tokio::task::block_in_place(|| {
                    tokio::runtime::Handle::current().block_on(async {
                        let workspace = self.workspace.read().await;
                        Some(workspace.all_indexed_files())
                    })
                })
            })
            .unwrap_or_default();

        for uri in workspace_uris {
            if !self.documents.has_document(&uri) {
                if let Ok(_handle) = tokio::runtime::Handle::try_current() {
                    let workspace = tokio::task::block_in_place(|| {
                        tokio::runtime::Handle::current().block_on(async { self.workspace.read().await })
                    });

                    if let Some(parse_result) = workspace.load_workspace_file(&uri) {
                        Self::collect_matching_symbols(
                            &uri,
                            &parse_result.ast,
                            &parse_result.symbol_table,
                            &query,
                            &mut results,
                        );
                    }
                }
            }
        }

        if query.is_empty() || results.is_empty() {
            if results.is_empty() { None } else { Some(results) }
        } else {
            results.sort_by_cached_key(|sym| {
                let score = Self::fuzzy_match_score(&sym.name.to_lowercase(), &query);
                -(score as i32)
            });

            Some(results)
        }
    }

    /// Resolve a workspace symbol to fill in location details
    ///
    /// Takes a WorkspaceSymbol and returns it with complete location information for lazy loading in some LSP clients.
    pub fn symbol_resolve(&self, symbol: WorkspaceSymbol) -> WorkspaceSymbol {
        // TODO: Implement symbol lookup and location resolution
        symbol
    }

    /// Collect symbols matching the query from an AST
    ///
    /// Recursively walks the AST and adds matching symbols to results.
    fn collect_matching_symbols(
        uri: &Url, node: &AstNode, symbol_table: &SymbolTable, query: &str, results: &mut Vec<SymbolInformation>,
    ) {
        match node {
            AstNode::FunctionDef { name, line, col, body, .. } => {
                if query.is_empty() || name.to_lowercase().contains(query) {
                    let symbol_kind = symbol_table
                        .lookup_symbol(name, symbol_table.root_scope)
                        .map(|symbol| symbol.kind.clone())
                        .unwrap_or(SymbolKind::Function);

                    let position =
                        Position { line: (*line as u32).saturating_sub(1), character: (*col as u32).saturating_sub(1) };

                    let range = Range {
                        start: position,
                        end: Position { line: position.line, character: position.character + name.len() as u32 },
                    };

                    results.push(Self::create_symbol_information(
                        uri,
                        name.clone(),
                        &symbol_kind,
                        range,
                        false,
                    ));
                }

                for stmt in body {
                    Self::collect_matching_symbols(uri, stmt, symbol_table, query, results);
                }
            }
            AstNode::ClassDef { name, line, col, body, .. } => {
                if query.is_empty() || name.to_lowercase().contains(query) {
                    let symbol_kind = symbol_table
                        .lookup_symbol(name, symbol_table.root_scope)
                        .map(|symbol| symbol.kind.clone())
                        .unwrap_or(SymbolKind::Class);

                    let position =
                        Position { line: (*line as u32).saturating_sub(1), character: (*col as u32).saturating_sub(1) };

                    let range = Range {
                        start: position,
                        end: Position { line: position.line, character: position.character + name.len() as u32 },
                    };
                    results.push(Self::create_symbol_information(
                        uri,
                        name.clone(),
                        &symbol_kind,
                        range,
                        false,
                    ));
                }

                for stmt in body {
                    Self::collect_matching_symbols(uri, stmt, symbol_table, query, results);
                }
            }
            AstNode::Assignment { target, value, line, col, .. } => {
                let target_str = target.target_to_string();
                if query.is_empty() || target_str.to_lowercase().contains(query) {
                    let symbol_kind = symbol_table
                        .lookup_symbol(&target_str, symbol_table.root_scope)
                        .map(|symbol| symbol.kind.clone())
                        .unwrap_or(SymbolKind::Variable);

                    let position =
                        Position { line: (*line as u32).saturating_sub(1), character: (*col as u32).saturating_sub(1) };

                    let range = Range {
                        start: position,
                        end: Position { line: position.line, character: position.character + target_str.len() as u32 },
                    };

                    results.push(Self::create_symbol_information(
                        uri,
                        target_str,
                        &symbol_kind,
                        range,
                        false,
                    ));
                }

                Self::collect_matching_symbols(uri, value, symbol_table, query, results);
            }
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::collect_matching_symbols(uri, stmt, symbol_table, query, results);
                }
            }
            _ => {}
        }
    }

    /// Convert beacon parser symbol kind to LSP symbol kind
    fn convert_symbol_kind(kind: &SymbolKind) -> LspSymbolKind {
        match kind {
            SymbolKind::Function => LspSymbolKind::FUNCTION,
            SymbolKind::Class => LspSymbolKind::CLASS,
            SymbolKind::Variable => LspSymbolKind::VARIABLE,
            SymbolKind::Parameter => LspSymbolKind::VARIABLE,
            SymbolKind::Import => LspSymbolKind::MODULE,
            SymbolKind::MagicMethod => LspSymbolKind::METHOD,
            SymbolKind::BuiltinVar => LspSymbolKind::CONSTANT,
        }
    }

    /// Create a `SymbolInformation` instance for the given symbol metadata.
    fn create_symbol_information(
        uri: &Url, name: String, kind: &SymbolKind, range: Range, is_deprecated: bool,
    ) -> SymbolInformation {
        SymbolInformation {
            name,
            kind: Self::convert_symbol_kind(kind),
            tags: Self::symbol_tags(is_deprecated),
            #[allow(deprecated)]
            deprecated: if is_deprecated { Some(true) } else { None },
            location: Location { uri: uri.clone(), range },
            container_name: None,
        }
    }

    /// Convert deprecation metadata into `SymbolTag`s.
    fn symbol_tags(is_deprecated: bool) -> Option<Vec<SymbolTag>> {
        if is_deprecated { Some(vec![SymbolTag::DEPRECATED]) } else { None }
    }

    /// Compute fuzzy match score between a symbol name and query
    ///
    /// Returns a higher score for better matches.
    /// Scoring prioritizes:
    /// - Exact matches (highest)
    /// - Prefix matches
    /// - Consecutive character matches
    /// - Any subsequence matches (lowest)
    fn fuzzy_match_score(name: &str, query: &str) -> usize {
        if query.is_empty() {
            return 100;
        }

        if name == query {
            return 1000;
        }

        if name.starts_with(query) {
            return 500;
        }

        if name.contains(query) {
            return 300;
        }

        let mut query_chars = query.chars().peekable();
        let mut matched = 0;
        let mut consecutive = 0;
        let mut max_consecutive = 0;

        for ch in name.chars() {
            if Some(&ch) == query_chars.peek() {
                query_chars.next();
                matched += 1;
                consecutive += 1;
                max_consecutive = max_consecutive.max(consecutive);
            } else {
                consecutive = 0;
            }
        }

        if matched == query.len() { 100 + max_consecutive * 10 } else { 0 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use std::str::FromStr;

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let _provider = WorkspaceSymbolsProvider::new(documents, workspace);
    }

    #[test]
    fn test_workspace_symbol_empty_workspace() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents, workspace);

        let params = WorkspaceSymbolParams {
            query: "test".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_none());
    }

    #[test]
    fn test_workspace_symbol_find_function() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def hello():
    pass

def world():
    pass"#;

        documents.open_document(uri, 1, source.to_string()).unwrap();

        let params = WorkspaceSymbolParams {
            query: "hello".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_some());

        let symbols = result.unwrap();
        assert!(!symbols.is_empty());

        let hello_symbol = symbols.iter().find(|s| s.name == "hello");
        assert!(hello_symbol.is_some());
        assert_eq!(hello_symbol.unwrap().kind, LspSymbolKind::FUNCTION);
    }

    #[test]
    fn test_workspace_symbol_find_class() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"class MyClass:
    pass"#;

        documents.open_document(uri, 1, source.to_string()).unwrap();

        let params = WorkspaceSymbolParams {
            query: "MyClass".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_some());

        let symbols = result.unwrap();
        let class_symbol = symbols.iter().find(|s| s.name == "MyClass");
        assert!(class_symbol.is_some());
        assert_eq!(class_symbol.unwrap().kind, LspSymbolKind::CLASS);
    }

    #[test]
    fn test_workspace_symbol_find_variable() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "my_variable = 42";

        documents.open_document(uri, 1, source.to_string()).unwrap();

        let params = WorkspaceSymbolParams {
            query: "my_variable".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_some());

        let symbols = result.unwrap();
        let var_symbol = symbols.iter().find(|s| s.name == "my_variable");
        assert!(var_symbol.is_some());
        assert_eq!(var_symbol.unwrap().kind, LspSymbolKind::VARIABLE);
    }

    #[test]
    fn test_workspace_symbol_case_insensitive() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "def HelloWorld():\n    pass";

        documents.open_document(uri, 1, source.to_string()).unwrap();

        let params = WorkspaceSymbolParams {
            query: "helloworld".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_some());

        let symbols = result.unwrap();
        let symbol = symbols.iter().find(|s| s.name == "HelloWorld");
        assert!(symbol.is_some());
    }

    #[test]
    fn test_workspace_symbol_partial_match() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "def calculate_sum():\n    pass";

        documents.open_document(uri, 1, source.to_string()).unwrap();

        let params = WorkspaceSymbolParams {
            query: "calc".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_some());

        let symbols = result.unwrap();
        let symbol = symbols.iter().find(|s| s.name == "calculate_sum");
        assert!(symbol.is_some());
    }

    #[test]
    fn test_workspace_symbol_empty_query() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def func1():
    pass

def func2():
    pass"#;

        documents.open_document(uri, 1, source.to_string()).unwrap();

        let params = WorkspaceSymbolParams {
            query: "".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_some());

        let symbols = result.unwrap();
        assert!(symbols.len() >= 2);
    }

    #[test]
    fn test_workspace_symbol_multiple_documents() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);

        let uri1 = Url::from_str("file:///file1.py").unwrap();
        let uri2 = Url::from_str("file:///file2.py").unwrap();

        documents
            .open_document(uri1, 1, "def func1():\n    pass".to_string())
            .unwrap();
        documents
            .open_document(uri2, 1, "def func2():\n    pass".to_string())
            .unwrap();

        let params = WorkspaceSymbolParams {
            query: "func".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_some());

        let symbols = result.unwrap();
        assert!(symbols.len() >= 2);

        let has_func1 = symbols.iter().any(|s| s.name == "func1");
        let has_func2 = symbols.iter().any(|s| s.name == "func2");
        assert!(has_func1);
        assert!(has_func2);
    }

    #[test]
    fn test_workspace_symbol_no_match() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "def hello():\n    pass";

        documents.open_document(uri, 1, source.to_string()).unwrap();

        let params = WorkspaceSymbolParams {
            query: "nonexistent".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_none());
    }

    #[test]
    fn test_workspace_symbol_nested_function() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"class MyClass:
    def my_method(self):
        pass"#;

        documents.open_document(uri, 1, source.to_string()).unwrap();

        let params = WorkspaceSymbolParams {
            query: "my_method".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_some());

        let symbols = result.unwrap();
        let method_symbol = symbols.iter().find(|s| s.name == "my_method");
        assert!(method_symbol.is_some());
    }

    #[test]
    fn test_convert_symbol_kind() {
        assert_eq!(
            WorkspaceSymbolsProvider::convert_symbol_kind(&SymbolKind::Function),
            LspSymbolKind::FUNCTION
        );
        assert_eq!(
            WorkspaceSymbolsProvider::convert_symbol_kind(&SymbolKind::Class),
            LspSymbolKind::CLASS
        );
        assert_eq!(
            WorkspaceSymbolsProvider::convert_symbol_kind(&SymbolKind::Variable),
            LspSymbolKind::VARIABLE
        );
        assert_eq!(
            WorkspaceSymbolsProvider::convert_symbol_kind(&SymbolKind::Parameter),
            LspSymbolKind::VARIABLE
        );
        assert_eq!(
            WorkspaceSymbolsProvider::convert_symbol_kind(&SymbolKind::Import),
            LspSymbolKind::MODULE
        );
    }

    #[test]
    fn test_symbol_resolve() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents, workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let location = Location {
            uri,
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 5 } },
        };

        let symbol = WorkspaceSymbol {
            name: "test".to_string(),
            kind: LspSymbolKind::FUNCTION,
            tags: None,
            location: lsp_types::OneOf::Left(location),
            container_name: None,
            data: None,
        };

        let _ = provider.symbol_resolve(symbol);
        // TODO: incomplete - currently just returns the symbol unchanged
    }

    #[test]
    fn test_workspace_symbol_location_range() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "def hello():\n    pass";

        documents.open_document(uri, 1, source.to_string()).unwrap();

        let params = WorkspaceSymbolParams {
            query: "hello".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_some());

        let symbols = result.unwrap();
        let symbol = &symbols[0];

        assert_eq!(symbol.location.range.start.line, 0);
        assert_eq!(
            symbol.location.range.end.character,
            symbol.location.range.start.character + "hello".len() as u32
        );
    }

    #[test]
    fn test_collect_matching_symbols_module() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let provider = WorkspaceSymbolsProvider::new(documents.clone(), workspace);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"x = 1
y = 2
def func():
    pass"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = WorkspaceSymbolParams {
            query: "".to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.workspace_symbol(params);
        assert!(result.is_some());

        let symbols = result.unwrap();
        assert!(symbols.len() >= 3);
    }
}
