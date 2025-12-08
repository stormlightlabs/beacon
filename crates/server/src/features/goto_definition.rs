//! Go to definition provider
//!
//! Navigates to the definition of symbols (variables, functions, classes, imports).

use crate::analysis::Analyzer;
use crate::document::DocumentManager;
use crate::workspace::Workspace;
use crate::{parser, utils};
use beacon_core::{Type, TypeCtor};
use beacon_parser::SymbolKind;
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, Position};
use ropey::Rope;
use std::sync::Arc;
use tokio::sync::RwLock;
use tree_sitter::Node;
use url::Url;

pub struct GotoDefinitionProvider {
    documents: DocumentManager,
    workspace: Arc<RwLock<Workspace>>,
}

impl GotoDefinitionProvider {
    pub fn new(documents: DocumentManager, workspace: Arc<RwLock<Workspace>>) -> Self {
        Self { documents, workspace }
    }

    /// Find the definition of the symbol at a position
    ///
    /// Uses symbol table and name resolution with proper scope tracking
    pub fn goto_definition(&self, params: GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let is_import = self.documents.get_document(&uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let p = parser::LspParser::new().ok()?;
            let node = p.node_at_position(tree, &text, position)?;

            let mut current = node;
            while let Some(parent) = current.parent() {
                if matches!(parent.kind(), "import_statement" | "import_from_statement") {
                    return Some(true);
                }
                current = parent;
            }

            Some(false)
        });

        if is_import == Some(Some(true)) {
            let cross_file_location = tokio::task::block_in_place(|| {
                tokio::runtime::Handle::current()
                    .block_on(async { self.find_cross_file_definition(&uri, position).await })
            });

            return cross_file_location.map(GotoDefinitionResponse::Scalar);
        }

        if let Ok(handle) = tokio::runtime::Handle::try_current()
            && handle.runtime_flavor() == tokio::runtime::RuntimeFlavor::MultiThread {
                let cross_file_location = tokio::task::block_in_place(|| {
                    handle.block_on(async { self.find_imported_symbol_definition(&uri, position).await })
                });

                if cross_file_location.is_some() {
                    return cross_file_location.map(GotoDefinitionResponse::Scalar);
                }
            }

        if let Some(location) = self.find_definition(&uri, position) {
            return Some(GotoDefinitionResponse::Scalar(location));
        }

        None
    }

    /// Find the definition location for a symbol at a position by looking up the identifier at the
    /// given position in the symbol table and returns its definition location.
    ///
    /// Uses position-based scope resolution to find symbols in the correct lexical scope.
    fn find_definition(&self, uri: &Url, position: Position) -> Option<Location> {
        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let symbol_table = doc.symbol_table()?;
            let p = parser::LspParser::new().ok()?;
            let node = p.node_at_position(tree, &text, position)?;

            if node.kind() != "identifier" {
                None
            } else {
                let identifier_text = node.utf8_text(text.as_bytes()).ok()?;
                let byte_offset = utils::position_to_byte_offset(&text, position);
                let scope = symbol_table.find_scope_at_position(byte_offset);
                let symbol = symbol_table.lookup_symbol(identifier_text, scope)?;
                let range = self.find_identifier_range_at_definition(tree, &text, symbol, identifier_text)?;
                Some(Location { uri: uri.clone(), range })
            }
        })?
    }

    /// Find the range of an identifier at its definition location
    fn find_identifier_range_at_definition(
        &self, tree: &tree_sitter::Tree, text: &str, symbol: &beacon_parser::Symbol, identifier_name: &str,
    ) -> Option<lsp_types::Range> {
        let line_idx = symbol.line.saturating_sub(1);
        let byte_col = symbol.col.saturating_sub(1);
        let rope = Rope::from_str(text);

        if line_idx >= rope.len_lines() {
            None
        } else {
            let symbol_byte_offset = rope.line_to_byte(line_idx) + byte_col;
            let node = tree
                .root_node()
                .descendant_for_byte_range(symbol_byte_offset, symbol_byte_offset)?;

            let identifier_node = Self::find_identifier_node(text, node, identifier_name)?;
            Some(utils::tree_sitter_range_to_lsp_range(text, identifier_node.range()))
        }
    }

    /// Traverses up the tree to find a node matching `identifier_name`.
    fn find_identifier_node<'a>(text: &str, mut node: Node<'a>, identifier_name: &str) -> Option<Node<'a>> {
        loop {
            if let Some(name_node) = node.child_by_field_name("name")
                && name_node.kind() == "identifier" {
                    return Some(name_node);
                }

            if Self::node_is_identifier_with_name(node, text, identifier_name) {
                return Some(node);
            }

            if let Some(left_node) = node.child_by_field_name("left")
                && Self::node_is_identifier_with_name(left_node, text, identifier_name) {
                    return Some(left_node);
                }

            if Self::is_import_node(node)
                && let Some(found) = Self::find_in_imports(node, text, identifier_name) {
                    return Some(found);
                }

            node = node.parent()?;
        }
    }

    fn node_is_identifier_with_name(node: Node, text: &str, name: &str) -> bool {
        node.kind() == "identifier" && node.utf8_text(text.as_bytes()).map(|t| t == name).unwrap_or(false)
    }

    fn is_import_node(node: Node) -> bool {
        matches!(node.kind(), "import_statement" | "import_from_statement")
    }

    fn find_in_imports<'a>(node: Node<'a>, text: &str, identifier_name: &str) -> Option<Node<'a>> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "dotted_name" | "identifier" => {
                    if let Ok(child_text) = child.utf8_text(text.as_bytes())
                        && child_text == identifier_name {
                            return Some(child);
                        }
                }
                "aliased_import" => {
                    for field in ["alias", "name"] {
                        if let Some(alias_or_name) = child.child_by_field_name(field)
                            && Self::node_is_identifier_with_name(alias_or_name, text, identifier_name) {
                                return Some(alias_or_name);
                            }
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Find definition of an imported symbol used anywhere in the file
    ///
    /// Resolves symbols that were imported from other modules to their original definition.
    pub async fn find_imported_symbol_definition(&self, uri: &Url, position: Position) -> Option<Location> {
        let identifier = self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let p = parser::LspParser::new().ok()?;
            let node = p.node_at_position(tree, &text, position)?;

            if node.kind() != "identifier" {
                return None;
            }

            node.utf8_text(text.as_bytes()).ok().map(String::from)
        })??;

        tracing::debug!("Looking for cross-file definition of '{}' in {}", identifier, uri);

        let workspace = self.workspace.read().await;
        let symbol_imports = workspace.get_symbol_imports(uri);

        for import in symbol_imports {
            if import.symbol == "*" {
                if let Some(module_uri) = workspace.resolve_import(&import.from_module)
                    && let Some(location) = self.find_symbol_in_module(&module_uri, &identifier, &workspace) {
                        tracing::debug!(
                            "Found '{}' in star import from {} ({})",
                            identifier,
                            import.from_module,
                            module_uri
                        );
                        return Some(location);
                    }
            } else if import.symbol == identifier
                && let Some(module_uri) = workspace.resolve_import(&import.from_module)
                    && let Some(location) = self.find_symbol_in_module(&module_uri, &identifier, &workspace) {
                        tracing::debug!(
                            "Found '{}' imported from {} ({})",
                            identifier,
                            import.from_module,
                            module_uri
                        );
                        return Some(location);
                    }
        }

        tracing::debug!("No cross-file definition found for '{}' in {}", identifier, uri);
        None
    }

    /// Find a symbol definition in a specific module
    fn find_symbol_in_module(
        &self, module_uri: &Url, symbol_name: &str, workspace: &crate::workspace::Workspace,
    ) -> Option<Location> {
        if let Some(location) = self.documents.get_document(module_uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let symbol_table = doc.symbol_table()?;

            let root_scope = symbol_table.root_scope;
            let symbol = symbol_table.lookup_symbol(symbol_name, root_scope)?;

            let range = self.find_identifier_range_at_definition(tree, &text, symbol, symbol_name)?;
            Some(Location { uri: module_uri.clone(), range })
        }) {
            return location;
        }

        if let Some(parse_result) = workspace.load_workspace_file(module_uri) {
            let root_scope = parse_result.symbol_table.root_scope;
            if let Some(symbol) = parse_result.symbol_table.lookup_symbol(symbol_name, root_scope) {
                let text = parse_result.rope.to_string();
                if let Some(range) =
                    self.find_identifier_range_at_definition(&parse_result.tree, &text, symbol, symbol_name)
                {
                    return Some(Location { uri: module_uri.clone(), range });
                }
            }
        }

        None
    }

    /// Find cross-file definitions by following imports
    ///
    /// Detects import statements and resolves them to their target files.
    /// Handles both `import foo` and `from foo import bar` cases.
    pub async fn find_cross_file_definition(&self, uri: &Url, position: Position) -> Option<Location> {
        let (module_name, is_relative) = self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let p = parser::LspParser::new().ok()?;
            let node = p.node_at_position(tree, &text, position)?;

            if node.kind() != "identifier" && node.kind() != "dotted_name" {
                return None;
            }

            let mut current = node;
            while let Some(parent) = current.parent() {
                if parent.kind() == "import_statement" {
                    let name = parent
                        .child_by_field_name("name")
                        .and_then(|n| n.utf8_text(text.as_bytes()).ok())?
                        .to_string();
                    let is_rel = name.starts_with('.');
                    return Some((name, is_rel));
                } else if parent.kind() == "import_from_statement" {
                    let name = parent
                        .child_by_field_name("module_name")
                        .and_then(|n| n.utf8_text(text.as_bytes()).ok())?
                        .to_string();
                    let is_rel = name.starts_with('.');
                    return Some((name, is_rel));
                }
                current = parent;
            }

            None
        })??;

        let workspace = self.workspace.read().await;

        let target_uri = if is_relative {
            let from_module = workspace.uri_to_module_name(uri)?;
            let leading_dots = module_name.chars().take_while(|&c| c == '.').count();
            let rest = &module_name[leading_dots..];
            workspace.resolve_relative_import(&from_module, rest, leading_dots)?
        } else {
            workspace.resolve_import(&module_name)?
        };

        let range = if self.documents.has_document(&target_uri) {
            self.documents.get_document(&target_uri, |doc| {
                let tree = doc.tree()?;
                let text = doc.text();
                Some(utils::tree_sitter_range_to_lsp_range(&text, tree.root_node().range()))
            })?
        } else {
            let parse_result = workspace.load_workspace_file(&target_uri)?;
            let root_text = parse_result.rope.to_string();
            Some(utils::tree_sitter_range_to_lsp_range(
                &root_text,
                parse_result.tree.root_node().range(),
            ))
        }?;

        Some(Location { uri: target_uri, range })
    }

    /// Navigate to the type definition of the symbol at a position
    ///
    /// Uses type inference to get the type of the expression, then navigates to the definition of that type (class, protocol, etc.)
    pub fn goto_type_definition(&self, uri: &Url, position: Position, analyzer: &mut Analyzer) -> Option<Location> {
        let ty = analyzer.type_at_position(uri, position).ok()??;

        let class_name = Self::extract_class_name_from_type(&ty)?;

        tracing::debug!(
            "Looking for type definition of '{}' from position {:?} in {}",
            class_name,
            position,
            uri
        );

        if let Some(location) = self.find_class_definition(uri, &class_name) {
            return Some(location);
        }

        if let Ok(handle) = tokio::runtime::Handle::try_current() {
            if handle.runtime_flavor() == tokio::runtime::RuntimeFlavor::MultiThread {
                let workspace_future = self.find_class_in_workspace(&class_name);
                tokio::task::block_in_place(|| handle.block_on(workspace_future))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Extract class name from a Type
    fn extract_class_name_from_type(ty: &Type) -> Option<String> {
        match ty {
            Type::Con(TypeCtor::Class(name)) => Some(name.clone()),
            Type::Con(TypeCtor::Protocol(Some(name), _)) => Some(name.clone()),
            Type::App(ctor, _) => match &**ctor {
                Type::Con(TypeCtor::Class(name)) => Some(name.clone()),
                Type::Con(TypeCtor::Protocol(Some(name), _)) => Some(name.clone()),
                _ => None,
            },
            Type::BoundMethod(receiver, _, _) => match &**receiver {
                Type::Con(TypeCtor::Class(name)) => Some(name.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    /// Find class definition in the local file
    fn find_class_definition(&self, uri: &Url, class_name: &str) -> Option<Location> {
        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let symbol_table = doc.symbol_table()?;

            let symbol = symbol_table
                .scopes
                .get(&symbol_table.root_scope)?
                .symbols
                .get(class_name)?;

            if symbol.kind != SymbolKind::Class {
                return None;
            }

            let range = self.find_identifier_range_at_definition(tree, &text, symbol, class_name)?;
            Some(Location { uri: uri.clone(), range })
        })?
    }

    /// Find class definition across the workspace
    async fn find_class_in_workspace(&self, class_name: &str) -> Option<Location> {
        let workspace = self.workspace.read().await;

        for (module_uri, _module_name) in workspace.all_modules() {
            if let Some(location) = self.find_symbol_in_module(&module_uri, class_name, &workspace) {
                let is_class = self.documents.get_document(&module_uri, |doc| {
                    let symbol_table = doc.symbol_table()?;
                    symbol_table
                        .lookup_symbol(class_name, symbol_table.root_scope)
                        .map(|s| s.kind == SymbolKind::Class)
                })?;

                if is_class == Some(true) {
                    tracing::debug!(
                        "Found class '{}' definition in workspace module {}",
                        class_name,
                        module_uri
                    );
                    return Some(location);
                }
            }

            if let Some(parse_result) = workspace.load_workspace_file(&module_uri)
                && let Some(symbol) = parse_result
                    .symbol_table
                    .lookup_symbol(class_name, parse_result.symbol_table.root_scope)
                    && symbol.kind == SymbolKind::Class {
                        let text = parse_result.rope.to_string();
                        if let Some(range) =
                            self.find_identifier_range_at_definition(&parse_result.tree, &text, symbol, class_name)
                        {
                            tracing::debug!(
                                "Found class '{}' definition in workspace file {}",
                                class_name,
                                module_uri
                            );
                            return Some(Location { uri: module_uri.clone(), range });
                        }
                    }
        }

        None
    }

    /// Navigate to implementations of a protocol or abstract class
    ///
    /// Finds all classes that implement a protocol or inherit from a base class.
    /// For methods, finds overriding implementations in subclasses.
    pub fn goto_implementation(&self, uri: &Url, position: Position, analyzer: &mut Analyzer) -> Vec<Location> {
        let handle = match tokio::runtime::Handle::try_current() {
            Ok(h) => h,
            Err(_) => return Vec::new(),
        };

        let ty = match analyzer.type_at_position(uri, position) {
            Ok(Some(t)) => t,
            _ => return Vec::new(),
        };

        let class_name = match Self::extract_class_name_from_type(&ty) {
            Some(name) => name,
            None => return Vec::new(),
        };

        tracing::debug!(
            "Looking for implementations of '{}' from position {:?} in {}",
            class_name,
            position,
            uri
        );

        if handle.runtime_flavor() == tokio::runtime::RuntimeFlavor::MultiThread {
            tokio::task::block_in_place(|| handle.block_on(self.find_implementations(&class_name)))
        } else {
            Vec::new()
        }
    }

    /// Find all implementations of a class/protocol across the workspace
    async fn find_implementations(&self, base_class_name: &str) -> Vec<Location> {
        let workspace = self.workspace.read().await;
        let mut implementations = Vec::new();

        for (module_uri, _module_name) in workspace.all_modules() {
            let module_implementations = self.find_implementations_in_module(&module_uri, base_class_name, &workspace);
            implementations.extend(module_implementations);
        }

        tracing::debug!(
            "Found {} implementation(s) of '{}'",
            implementations.len(),
            base_class_name
        );

        implementations
    }

    /// Find implementations in a specific module
    fn find_implementations_in_module(
        &self, module_uri: &Url, base_class_name: &str, workspace: &Workspace,
    ) -> Vec<Location> {
        if let Some(Some(impls)) = self.documents.get_document(module_uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let symbol_table = doc.symbol_table()?;
            let ast = doc.ast()?;

            let mut implementations = Vec::new();
            Self::find_class_implementations(
                ast,
                &text,
                tree,
                symbol_table,
                base_class_name,
                module_uri,
                &mut implementations,
            );
            Some(implementations)
        }) {
            return impls;
        }

        let mut implementations = Vec::new();
        if let Some(parse_result) = workspace.load_workspace_file(module_uri) {
            let text = parse_result.rope.to_string();
            Self::find_class_implementations(
                &parse_result.ast,
                &text,
                &parse_result.tree,
                &parse_result.symbol_table,
                base_class_name,
                module_uri,
                &mut implementations,
            );
        }

        implementations
    }

    /// Find classes that inherit from or implement the base class
    fn find_class_implementations(
        ast: &beacon_parser::AstNode, text: &str, tree: &tree_sitter::Tree, symbol_table: &beacon_parser::SymbolTable,
        base_class_name: &str, module_uri: &Url, implementations: &mut Vec<Location>,
    ) {
        use beacon_parser::AstNode;

        match ast {
            AstNode::ClassDef { name, bases, body, .. } => {
                for base_name in bases {
                    if base_name == base_class_name
                        && let Some(symbol) = symbol_table.lookup_symbol(name, symbol_table.root_scope)
                            && symbol.kind == SymbolKind::Class {
                                let line_idx = symbol.line.saturating_sub(1);
                                let byte_col = symbol.col.saturating_sub(1);
                                let rope = Rope::from_str(text);

                                if line_idx < rope.len_lines() {
                                    let symbol_byte_offset = rope.line_to_byte(line_idx) + byte_col;
                                    if let Some(node) = tree
                                        .root_node()
                                        .descendant_for_byte_range(symbol_byte_offset, symbol_byte_offset)
                                        && let Some(identifier_node) =
                                            GotoDefinitionProvider::find_identifier_node(text, node, name)
                                        {
                                            let range =
                                                utils::tree_sitter_range_to_lsp_range(text, identifier_node.range());
                                            implementations.push(Location { uri: module_uri.clone(), range });
                                            tracing::debug!(
                                                "Found implementation: {} extends {} in {}",
                                                name,
                                                base_class_name,
                                                module_uri
                                            );
                                        }
                                }
                            }
                }

                for child in body {
                    Self::find_class_implementations(
                        child,
                        text,
                        tree,
                        symbol_table,
                        base_class_name,
                        module_uri,
                        implementations,
                    );
                }
            }
            AstNode::Module { body, .. } => {
                for child in body {
                    Self::find_class_implementations(
                        child,
                        text,
                        tree,
                        symbol_table,
                        base_class_name,
                        module_uri,
                        implementations,
                    );
                }
            }
            AstNode::FunctionDef { body, .. } => {
                for child in body {
                    Self::find_class_implementations(
                        child,
                        text,
                        tree,
                        symbol_table,
                        base_class_name,
                        module_uri,
                        implementations,
                    );
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis;
    use crate::config::Config;
    use std::str::FromStr;

    fn create_test_provider() -> (GotoDefinitionProvider, Url) {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = GotoDefinitionProvider::new(documents.clone(), workspace);
        let uri = Url::from_str("file:///test.py").unwrap();
        (provider, uri)
    }

    fn open_test_document(documents: &DocumentManager, uri: &Url, source: &str) {
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();
    }

    #[test]
    fn test_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let _provider = GotoDefinitionProvider::new(documents, workspace);
    }

    #[test]
    fn test_goto_function_definition() {
        let (provider, uri) = create_test_provider();
        let source = r#"def hello():
    pass

hello()"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 3, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
                assert_eq!(location.range.start.character, 4);
                assert_eq!(location.range.end.line, 0);
                assert_eq!(location.range.end.character, 9);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_variable_definition() {
        let (provider, uri) = create_test_provider();
        let source = r#"x = 42
y = x + 10"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 4 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
                assert_eq!(location.range.start.character, 0);
                assert_eq!(location.range.end.character, 1);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_class_definition() {
        let (provider, uri) = create_test_provider();
        let source = r#"class MyClass:
    pass

obj = MyClass()"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 3, character: 6 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
                assert_eq!(location.range.start.character, 6);
                assert_eq!(location.range.end.character, 13);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_definition_not_identifier() {
        let (provider, uri) = create_test_provider();
        let source = "x = 42";
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 2 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_none());
    }

    #[test]
    fn test_goto_definition_undefined_symbol() {
        let (provider, uri) = create_test_provider();
        let source = "x = undefined_var";
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                position: Position { line: 0, character: 4 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_none());
    }

    #[test]
    fn test_goto_definition_multi_line_function() {
        let (provider, uri) = create_test_provider();
        let source = r#"def long_function_name(
    param1,
    param2
):
    pass

long_function_name(1, 2)"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 6, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
                assert_eq!(location.range.start.character, 4);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_import_definition() {
        let (provider, uri) = create_test_provider();
        let source = r#"import os
x = os"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 4 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_definition_with_utf8_chars() {
        let (provider, uri) = create_test_provider();
        let source = "café = '☕'\nx = café";
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 1, character: 4 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 0);
                assert_eq!(location.range.start.character, 0);
                assert_eq!(location.range.end.character, 4); // "café" in UTF-16 is 4 code units
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_definition_local_variable() {
        let (provider, uri) = create_test_provider();
        let source = r#"def foo():
    test_data = [1, 2, 3]
    result = test_data
    return result"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 2, character: 13 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 1);
                assert_eq!(location.range.start.character, 4);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_definition_nested_function_variable() {
        let (provider, uri) = create_test_provider();
        let source = r#"def outer():
    x = 10
    def inner():
        y = x + 5
        return y
    return inner()"#;
        open_test_document(&provider.documents, &uri, source);

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 4, character: 15 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some());

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, uri);
                assert_eq!(location.range.start.line, 3);
                assert_eq!(location.range.start.character, 8);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[test]
    fn test_goto_definition_document_not_found() {
        let (provider, _) = create_test_provider();
        let nonexistent_uri = Url::from_str("file:///nonexistent.py").unwrap();

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: nonexistent_uri },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_none());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_cross_file_goto_definition_function() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = GotoDefinitionProvider::new(documents.clone(), workspace.clone());
        let module_a_uri = Url::from_str("file:///module_a.py").unwrap();
        let module_a_source = r#"def helper_function():
    return 42
"#;
        documents
            .open_document(module_a_uri.clone(), 1, module_a_source.to_string())
            .unwrap();

        let module_b_uri = Url::from_str("file:///module_b.py").unwrap();
        let module_b_source = r#"from module_a import helper_function

result = helper_function()
"#;
        documents
            .open_document(module_b_uri.clone(), 1, module_b_source.to_string())
            .unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(
                module_a_uri.clone(),
                "module_a".to_string(),
                std::path::PathBuf::from("/"),
            );
            ws.add_test_module(
                module_b_uri.clone(),
                "module_b".to_string(),
                std::path::PathBuf::from("/"),
            );
            ws.build_test_dependency_graph();
        }

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: module_b_uri },
                position: Position::new(2, 9),
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some(), "Should find cross-file definition");

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, module_a_uri);
                assert_eq!(location.range.start.line, 0);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_cross_file_goto_definition_class() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = GotoDefinitionProvider::new(documents.clone(), workspace.clone());
        let models_uri = Url::from_str("file:///models.py").unwrap();
        let models_source = r#"class User:
    def __init__(self, name):
        self.name = name
"#;
        documents
            .open_document(models_uri.clone(), 1, models_source.to_string())
            .unwrap();

        let main_uri = Url::from_str("file:///main.py").unwrap();
        let main_source = r#"from models import User

user = User("Alice")
"#;
        documents
            .open_document(main_uri.clone(), 1, main_source.to_string())
            .unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(models_uri.clone(), "models".to_string(), std::path::PathBuf::from("/"));
            ws.add_test_module(main_uri.clone(), "main".to_string(), std::path::PathBuf::from("/"));
            ws.build_test_dependency_graph();
        }

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: main_uri },
                position: Position::new(2, 7),
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some(), "Should find cross-file class definition");

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, models_uri);
                assert_eq!(location.range.start.line, 0);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_cross_file_goto_definition_variable() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = GotoDefinitionProvider::new(documents.clone(), workspace.clone());
        let constants_uri = Url::from_str("file:///constants.py").unwrap();
        let constants_source = r#"MAX_SIZE = 100
DEFAULT_TIMEOUT = 30
"#;
        documents
            .open_document(constants_uri.clone(), 1, constants_source.to_string())
            .unwrap();

        let app_uri = Url::from_str("file:///app.py").unwrap();
        let app_source = r#"from constants import MAX_SIZE

buffer = [0] * MAX_SIZE
"#;
        documents
            .open_document(app_uri.clone(), 1, app_source.to_string())
            .unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(
                constants_uri.clone(),
                "constants".to_string(),
                std::path::PathBuf::from("/"),
            );
            ws.add_test_module(app_uri.clone(), "app".to_string(), std::path::PathBuf::from("/"));
            ws.build_test_dependency_graph();
        }

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: app_uri },
                position: Position::new(2, 15),
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some(), "Should find cross-file variable definition");

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, constants_uri);
                assert_eq!(location.range.start.line, 0);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_cross_file_goto_definition_star_import() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config, documents.clone())));
        let provider = GotoDefinitionProvider::new(documents.clone(), workspace.clone());
        let utils_uri = Url::from_str("file:///utils.py").unwrap();
        let utils_source = r#"def format_string(s):
    return s.upper()

def parse_int(s):
    return int(s)
"#;
        documents
            .open_document(utils_uri.clone(), 1, utils_source.to_string())
            .unwrap();

        let main_uri = Url::from_str("file:///main.py").unwrap();
        let main_source = r#"from utils import *

result = format_string("hello")
"#;
        documents
            .open_document(main_uri.clone(), 1, main_source.to_string())
            .unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(utils_uri.clone(), "utils".to_string(), std::path::PathBuf::from("/"));
            ws.add_test_module(main_uri.clone(), "main".to_string(), std::path::PathBuf::from("/"));
            ws.build_test_dependency_graph();
        }

        let params = GotoDefinitionParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: main_uri },
                position: Position::new(2, 9),
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let result = provider.goto_definition(params);
        assert!(result.is_some(), "Should find definition via star import");

        match result.unwrap() {
            GotoDefinitionResponse::Scalar(location) => {
                assert_eq!(location.uri, utils_uri);
                assert_eq!(location.range.start.line, 0);
            }
            _ => panic!("Expected scalar location"),
        }
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_goto_type_definition() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let mut analyzer = analysis::Analyzer::with_workspace(config, documents.clone(), workspace.clone());
        let provider = GotoDefinitionProvider::new(documents.clone(), workspace.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"class MyClass:
    def __init__(self):
        self.value = 42

obj = MyClass()
result = obj
"#;
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(uri.clone(), "test".to_string(), std::path::PathBuf::from("/"));
        }

        analyzer.analyze(&uri).ok();

        let location = provider.goto_type_definition(&uri, Position::new(5, 9), &mut analyzer);
        assert!(
            location.is_some(),
            "Should find type definition for variable with class type"
        );

        let location = location.unwrap();
        assert_eq!(location.uri, uri);
        assert_eq!(location.range.start.line, 0);
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_goto_type_definition_cross_file() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let mut analyzer = analysis::Analyzer::with_workspace(config, documents.clone(), workspace.clone());
        let provider = GotoDefinitionProvider::new(documents.clone(), workspace.clone());

        let models_uri = Url::from_str("file:///models.py").unwrap();
        let models_source = r#"class User:
    def __init__(self, name):
        self.name = name
"#;
        documents
            .open_document(models_uri.clone(), 1, models_source.to_string())
            .unwrap();

        let main_uri = Url::from_str("file:///main.py").unwrap();
        let main_source = r#"from models import User

user = User("Alice")
"#;
        documents
            .open_document(main_uri.clone(), 1, main_source.to_string())
            .unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(models_uri.clone(), "models".to_string(), std::path::PathBuf::from("/"));
            ws.add_test_module(main_uri.clone(), "main".to_string(), std::path::PathBuf::from("/"));
            ws.build_test_dependency_graph();
        }

        analyzer.analyze(&models_uri).ok();
        analyzer.analyze(&main_uri).ok();

        let workspace_lock = workspace.read().await;
        drop(workspace_lock);

        let location_future = provider.find_class_in_workspace("User");
        let location = tokio::task::block_in_place(|| tokio::runtime::Handle::current().block_on(location_future));

        assert!(location.is_some(), "Should find User class in workspace");
        let location = location.unwrap();
        assert_eq!(location.uri, models_uri, "Should find User in models.py");
        assert_eq!(location.range.start.line, 0, "User class should be on line 0");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_goto_implementation() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let mut analyzer = analysis::Analyzer::with_workspace(config, documents.clone(), workspace.clone());
        let provider = GotoDefinitionProvider::new(documents.clone(), workspace.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"class Animal:
    def speak(self):
        pass

class Dog(Animal):
    def speak(self):
        return "Woof"

class Cat(Animal):
    def speak(self):
        return "Meow"

animal = Animal()
"#;
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(uri.clone(), "test".to_string(), std::path::PathBuf::from("/"));
        }

        analyzer.analyze(&uri).ok();

        let locations = provider.goto_implementation(&uri, Position::new(12, 9), &mut analyzer);
        assert_eq!(locations.len(), 2, "Should find two implementations of Animal");

        let class_names: Vec<_> = locations.iter().map(|loc| loc.range.start.line).collect();
        assert!(class_names.contains(&4), "Should find Dog at line 4");
        assert!(class_names.contains(&8), "Should find Cat at line 8");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_goto_implementation_cross_file() {
        let documents = DocumentManager::new().unwrap();
        let config = Config::default();
        let workspace = Arc::new(RwLock::new(Workspace::new(None, config.clone(), documents.clone())));
        let mut analyzer = analysis::Analyzer::with_workspace(config, documents.clone(), workspace.clone());
        let provider = GotoDefinitionProvider::new(documents.clone(), workspace.clone());

        let base_uri = Url::from_str("file:///base.py").unwrap();
        let base_source = r#"class Shape:
    def area(self):
        pass
"#;
        documents
            .open_document(base_uri.clone(), 1, base_source.to_string())
            .unwrap();

        let impl1_uri = Url::from_str("file:///circle.py").unwrap();
        let impl1_source = r#"from base import Shape

class Circle(Shape):
    def area(self):
        return 3.14 * self.radius ** 2
"#;
        documents
            .open_document(impl1_uri.clone(), 1, impl1_source.to_string())
            .unwrap();

        let impl2_uri = Url::from_str("file:///square.py").unwrap();
        let impl2_source = r#"from base import Shape

class Square(Shape):
    def area(self):
        return self.side ** 2
"#;
        documents
            .open_document(impl2_uri.clone(), 1, impl2_source.to_string())
            .unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(base_uri.clone(), "base".to_string(), std::path::PathBuf::from("/"));
            ws.add_test_module(impl1_uri.clone(), "circle".to_string(), std::path::PathBuf::from("/"));
            ws.add_test_module(impl2_uri.clone(), "square".to_string(), std::path::PathBuf::from("/"));
            ws.build_test_dependency_graph();
        }

        analyzer.analyze(&base_uri).ok();

        let locations = provider.goto_implementation(&base_uri, Position::new(0, 6), &mut analyzer);
        assert_eq!(locations.len(), 2, "Should find two implementations across files");

        let impl_uris: Vec<_> = locations.iter().map(|loc| loc.uri.clone()).collect();
        assert!(impl_uris.contains(&impl1_uri), "Should find Circle implementation");
        assert!(impl_uris.contains(&impl2_uri), "Should find Square implementation");
    }
}
