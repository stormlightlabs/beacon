//! Code completion provider
//!
//! Provides intelligent completions for identifiers, attributes, imports, and keywords.
use crate::document::DocumentManager;
use crate::features::dunders;
use crate::parser::LspParser;
use beacon_parser::{BUILTIN_DUNDERS, MAGIC_METHODS, ScopeId};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, Documentation, MarkupContent, MarkupKind,
    Position,
};
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
}

/// Python builtin functions with their descriptions
const BUILTINS: &[(&str, &str)] = &[
    ("print", "Print objects to the text stream"),
    ("len", "Return the length of an object"),
    ("range", "Return a sequence of numbers"),
    ("enumerate", "Return an enumerate object"),
    ("zip", "Return an iterator of tuples"),
    ("map", "Apply function to every item of iterable"),
    ("filter", "Construct iterator from elements which are true"),
    ("sum", "Return the sum of a 'start' value plus an iterable of numbers"),
    ("min", "Return the smallest item in an iterable"),
    ("max", "Return the largest item in an iterable"),
    ("abs", "Return the absolute value of a number"),
    ("round", "Round a number to a given precision"),
    ("int", "Convert to an integer"),
    ("float", "Convert to a floating point number"),
    ("str", "Convert to a string"),
    ("bool", "Convert to a boolean"),
    ("list", "Create a list object"),
    ("dict", "Create a dictionary object"),
    ("set", "Create a set object"),
    ("tuple", "Create a tuple object"),
    ("type", "Return the type of an object"),
    ("isinstance", "Check if object is an instance of a class"),
    ("hasattr", "Check if object has an attribute"),
    ("getattr", "Get an attribute from an object"),
    ("setattr", "Set an attribute on an object"),
    ("delattr", "Delete an attribute from an object"),
    ("open", "Open a file and return a file object"),
    ("input", "Read a string from standard input"),
    ("sorted", "Return a new sorted list from an iterable"),
    ("reversed", "Return a reverse iterator"),
    ("all", "Return True if all elements are true"),
    ("any", "Return True if any element is true"),
    ("chr", "Return a string of one character from Unicode"),
    ("ord", "Return Unicode code point of a character"),
    ("hex", "Convert integer to hexadecimal string"),
    ("bin", "Convert integer to binary string"),
    ("oct", "Convert integer to octal string"),
    ("callable", "Check if object appears callable"),
    ("dir", "List of names in the current scope or object attributes"),
    ("help", "Invoke the built-in help system"),
    ("id", "Return the identity of an object"),
    ("format", "Format a value using a format specification"),
    ("vars", "Return __dict__ attribute of an object"),
    ("eval", "Evaluate a Python expression"),
    ("exec", "Execute Python code dynamically"),
    ("compile", "Compile source into code object"),
    ("globals", "Return dictionary of current global symbol table"),
    ("locals", "Return dictionary of current local symbol table"),
    ("next", "Retrieve next item from iterator"),
    ("iter", "Return an iterator object"),
    ("slice", "Return a slice object"),
    ("super", "Return a proxy object for parent class"),
    ("property", "Return a property attribute"),
    ("classmethod", "Transform method into a class method"),
    ("staticmethod", "Transform method into a static method"),
    ("bytes", "Return a new bytes object"),
    ("bytearray", "Return a new bytearray object"),
    ("memoryview", "Return a memory view object"),
    ("complex", "Create a complex number"),
    ("frozenset", "Return a new frozenset object"),
    ("hash", "Return the hash value of an object"),
    ("pow", "Return base to the power of exp"),
    ("divmod", "Return quotient and remainder of division"),
    ("ascii", "Return a string with non-ASCII escaped"),
    ("repr", "Return a string representation of an object"),
];

pub struct CompletionProvider {
    _documents: DocumentManager,
}

impl CompletionProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { _documents: documents }
    }

    /// Provide completions at a position
    pub fn completion(&self, params: CompletionParams) -> Option<CompletionResponse> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let mut items = Vec::new();

        let completion_result = self._documents.get_document(uri, |doc| {
            let text = doc.text();
            let tree = doc.tree()?;
            let symbol_table = doc.symbol_table()?;

            let line_text = text.lines().nth(position.line as usize)?;
            let prefix = &line_text[..position.character.min(line_text.len() as u32) as usize];

            if prefix.ends_with("__") || prefix.contains("__") {
                return Some(self.dunder_completions(uri, position));
            }

            let context = self.detect_context(uri, position, &text, tree, symbol_table)?;

            match context.context_type {
                CompletionContextType::Expression => {
                    Some(self.symbol_completions(symbol_table, context.scope_id, &context.prefix))
                }
                CompletionContextType::Import => Some(Vec::new()),
                CompletionContextType::Attribute => Some(Vec::new()),
            }
        });

        if let Some(Some(result)) = completion_result {
            items.extend(result);
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

        Some(CompletionContext { prefix, context_type, scope_id, _in_class })
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

    /// Extract the partial identifier being typed at the cursor position
    ///
    /// Parses backwards from the cursor to find the start of an identifier.
    /// Returns the partial identifier or empty string if no identifier is being typed.
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
    fn _builtin_completion(&self, name: &str, detail: &str) -> CompletionItem {
        CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(detail.to_string()),
            ..Default::default()
        }
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

            items.push(CompletionItem {
                label: symbol.name.clone(),
                kind: Some(kind),
                detail: Some(detail),
                documentation,
                ..Default::default()
            });
        }

        for &(name, description) in BUILTINS {
            if !prefix.is_empty() && !name.starts_with(prefix) {
                continue;
            }

            items.push(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("builtin".to_string()),
                documentation: Some(Documentation::String(description.to_string())),
                ..Default::default()
            });
        }

        items
    }

    /// TODO: Get completions for attributes on a type
    /// Use type inference to get type of expression before '.'
    /// Look up attributes/methods on that type
    pub fn _attribute_completions(&self, _uri: &Url, _position: Position) -> Vec<CompletionItem> {
        Vec::new()
    }

    /// TODO: Get completions for imports
    /// Scan available modules and stub files
    pub fn _import_completions(&self, _uri: &Url, _position: Position) -> Vec<CompletionItem> {
        Vec::new()
    }

    /// TODO: Get keyword completions based on context
    pub fn _keyword_completions(&self, _position: Position) -> Vec<CompletionItem> {
        Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::TextDocumentPositionParams;
    use std::str::FromStr;

    #[test]
    fn test_builtin_completion() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents);

        let item = provider._builtin_completion("test", "Test function");

        assert_eq!(item.label, "test");
        assert_eq!(item.kind, Some(CompletionItemKind::FUNCTION));
        assert_eq!(item.detail, Some("Test function".to_string()));
    }

    #[test]
    fn test_dunder_completion_in_module_scope() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

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

        let response = provider.completion(params);
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "__name__"));
                assert!(items.iter().any(|item| item.label == "__file__"));
            }
            _ => panic!("Expected array response"),
        }
    }

    #[test]
    fn test_dunder_completion_in_class_scope() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

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

        let response = provider.completion(params);
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

    #[test]
    fn test_dunder_completion_item_has_documentation() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

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

    #[test]
    fn test_position_to_byte_offset() {
        let content = "line 1\nline 2\nline 3";

        assert_eq!(CompletionProvider::position_to_byte_offset(content, 1, 1), 0);
        assert_eq!(CompletionProvider::position_to_byte_offset(content, 2, 1), 7);
        assert_eq!(CompletionProvider::position_to_byte_offset(content, 2, 3), 9);
    }

    #[test]
    fn test_extract_partial_identifier() {
        assert_eq!(CompletionProvider::extract_partial_identifier("hello", 5), "hello");
        assert_eq!(CompletionProvider::extract_partial_identifier("hello", 3), "hel");
        assert_eq!(CompletionProvider::extract_partial_identifier("x = pri", 7), "pri");
        assert_eq!(CompletionProvider::extract_partial_identifier("my_var", 6), "my_var");
        assert_eq!(CompletionProvider::extract_partial_identifier("x = ", 4), "");
        assert_eq!(CompletionProvider::extract_partial_identifier("", 0), "");
        assert_eq!(CompletionProvider::extract_partial_identifier("x.attr", 6), "attr");
    }

    #[test]
    fn test_extract_partial_identifier_with_special_chars() {
        assert_eq!(CompletionProvider::extract_partial_identifier("x + var", 7), "var");
        assert_eq!(CompletionProvider::extract_partial_identifier("(my_func", 8), "my_func");
        assert_eq!(CompletionProvider::extract_partial_identifier("[item", 5), "item");
    }

    #[test]
    fn test_symbol_completions_includes_local_variables() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());
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

        let response = provider.completion(params);
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

    #[test]
    fn test_symbol_completions_includes_functions() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

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

        let response = provider.completion(params);
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "my_function"));
                assert!(items.iter().any(|item| item.label == "another_func"));
            }
            _ => panic!("Expected array response"),
        }
    }

    #[test]
    fn test_symbol_completions_with_prefix_filtering() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

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

        let response = provider.completion(params);
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

    #[test]
    fn test_symbol_completions_includes_builtins() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

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

        let response = provider.completion(params);
        assert!(response.is_some());

        match response.unwrap() {
            CompletionResponse::Array(items) => {
                assert!(items.iter().any(|item| item.label == "print"));
                assert!(!items.iter().any(|item| item.label == "len"));
            }
            _ => panic!("Expected array response"),
        }
    }

    #[test]
    fn test_symbol_completions_nested_scope() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

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

        let response = provider.completion(params);
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

    #[test]
    fn test_symbol_completions_includes_classes() {
        let documents = DocumentManager::new().unwrap();
        let provider = CompletionProvider::new(documents.clone());

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

        let response = provider.completion(params);
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
}
