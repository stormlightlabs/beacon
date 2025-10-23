//! Hover information provider
//!
//! Displays type information, signatures, and documentation when hovering over identifiers and expressions.

use crate::analysis::Analyzer;
use crate::document::DocumentManager;
use beacon_parser::{AstNode, SymbolKind};
use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position};
use url::Url;

pub struct HoverProvider {
    documents: DocumentManager,
}

impl HoverProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Provide hover information at a position
    pub fn hover(&self, params: HoverParams, analyzer: &mut Analyzer) -> Option<Hover> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        match self.hover_symbol(&uri, position) {
            Some(hover) => Some(hover),
            None => {
                let ty = analyzer.type_at_position(&uri, position).ok()??;
                let contents = HoverContents::Markup(self.format_type_hover(&ty));
                Some(Hover { contents, range: None })
            }
        }
    }

    /// Get hover information for a symbol
    fn hover_symbol(&self, uri: &Url, position: Position) -> Option<Hover> {
        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let symbol_table = doc.symbol_table()?;
            let ast = doc.ast()?;
            let parser = crate::parser::LspParser::new().ok()?;
            let node = parser.node_at_position(tree, &text, position)?;

            match node.kind() {
                "identifier" => {
                    let identifier_text = node.utf8_text(text.as_bytes()).ok()?;
                    let symbol = symbol_table.lookup_symbol(identifier_text, symbol_table.root_scope)?;
                    let content = match symbol.kind {
                        SymbolKind::Function => self.format_function_hover(identifier_text, ast, symbol.line),
                        SymbolKind::Class => self.format_class_hover(identifier_text, ast, symbol.line),
                        SymbolKind::Variable => {
                            self.format_variable_hover(identifier_text, &symbol.kind, symbol.line, symbol.col)
                        }
                        SymbolKind::Parameter => self.format_parameter_hover(identifier_text, symbol.line, symbol.col),
                        SymbolKind::Import => self.format_import_hover(identifier_text),
                    };

                    Some(Hover { contents: HoverContents::Markup(content), range: None })
                }
                _ => None,
            }
        })?
    }

    /// Format hover for a function
    fn format_function_hover(&self, name: &str, ast: &AstNode, def_line: usize) -> MarkupContent {
        let signature = self
            .extract_function_signature(ast, name)
            .unwrap_or_else(|| format!("def {}(...)", name));

        MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!(
                "```python\n{}\n```\n\n**Function** defined at line {}",
                signature, def_line
            ),
        }
    }

    /// Format hover for a class
    fn format_class_hover(&self, name: &str, _ast: &AstNode, def_line: usize) -> MarkupContent {
        MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!(
                "```python\nclass {}\n```\n\n**Class** defined at line {}",
                name, def_line
            ),
        }
    }

    /// Format hover for a variable
    fn format_variable_hover(&self, name: &str, _kind: &SymbolKind, def_line: usize, _def_col: usize) -> MarkupContent {
        MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("**Variable** `{}`\n\nDefined at line {}", name, def_line),
        }
    }

    fn format_parameter_hover(&self, name: &str, def_line: usize, _def_col: usize) -> MarkupContent {
        MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("**Parameter** `{}`\n\nDefined at line {}", name, def_line),
        }
    }

    fn format_import_hover(&self, name: &str) -> MarkupContent {
        MarkupContent { kind: MarkupKind::Markdown, value: format!("**Import** `{}`", name) }
    }

    fn extract_function_signature(&self, node: &AstNode, name: &str) -> Option<String> {
        match node {
            AstNode::Module { body } => {
                for stmt in body {
                    let sig = self.extract_function_signature(stmt, name);
                    if sig.is_some() {
                        return sig;
                    }
                }
                None
            }
            AstNode::FunctionDef { name: fn_name, args, .. } if fn_name == name => {
                Some(format!("def {}({})", name, args.join(", ")))
            }
            AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    let sig = self.extract_function_signature(stmt, name);
                    if sig.is_some() {
                        return sig;
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Format a type for display in hover
    ///
    /// Provides rich formatting with type information
    fn format_type_hover(&self, ty: &beacon_core::Type) -> MarkupContent {
        let type_str = self.format_type(ty);
        let value = format!("```python\n{}\n```\n\n**Inferred type**", type_str);
        MarkupContent { kind: MarkupKind::Markdown, value }
    }

    /// Format a type as a string using Display trait
    ///
    /// Uses the Display implementation from beacon-core which provides:
    /// - Pretty-printed type syntax (e.g., "list[int]" not "App(List, Int)")
    /// - Function signatures (e.g., "(int, str) -> bool")
    /// - Union types (e.g., "int | str")
    /// - Type variables with hints (e.g., "'alpha0")
    fn format_type(&self, ty: &beacon_core::Type) -> String {
        ty.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_core::{Type, TypeCtor};

    #[test]
    fn test_format_type_simple() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        assert_eq!(provider.format_type(&Type::Con(TypeCtor::Int)), "int");
        assert_eq!(provider.format_type(&Type::Con(TypeCtor::String)), "str");
        assert_eq!(provider.format_type(&Type::Con(TypeCtor::Bool)), "bool");
        assert_eq!(provider.format_type(&Type::Con(TypeCtor::NoneType)), "None");
    }

    #[test]
    fn test_format_type_list() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let list_int = Type::list(Type::Con(TypeCtor::Int));
        assert_eq!(provider.format_type(&list_int), "list[int]");

        let list_str = Type::list(Type::Con(TypeCtor::String));
        assert_eq!(provider.format_type(&list_str), "list[str]");
    }

    #[test]
    fn test_format_type_dict() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let dict_str_int = Type::dict(Type::Con(TypeCtor::String), Type::Con(TypeCtor::Int));
        assert_eq!(provider.format_type(&dict_str_int), "dict[str, int]");
    }

    #[test]
    fn test_format_type_function() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let fun1 = Type::fun(vec![Type::Con(TypeCtor::Int)], Type::Con(TypeCtor::String));
        assert_eq!(provider.format_type(&fun1), "int -> str");

        let fun2 = Type::fun(
            vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)],
            Type::Con(TypeCtor::Bool),
        );
        assert_eq!(provider.format_type(&fun2), "(int, str) -> bool");

        let fun3 = Type::fun(vec![], Type::Con(TypeCtor::Int));
        assert_eq!(provider.format_type(&fun3), "() -> int");
    }

    #[test]
    fn test_format_type_union() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let union = Type::union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
        let formatted = provider.format_type(&union);
        assert!(formatted.contains("int"));
        assert!(formatted.contains("str"));
        assert!(formatted.contains("|"));
    }

    #[test]
    fn test_format_type_optional() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let optional_int = Type::optional(Type::Con(TypeCtor::Int));
        let formatted = provider.format_type(&optional_int);
        assert!(formatted.contains("int"));
        assert!(formatted.contains("None"));
    }

    #[test]
    fn test_format_type_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let ty = Type::Con(TypeCtor::Int);
        let hover = provider.format_type_hover(&ty);
        assert_eq!(hover.kind, MarkupKind::Markdown);
        assert!(hover.value.contains("```python"));
        assert!(hover.value.contains("int"));
        assert!(hover.value.contains("Inferred type"));
    }

    #[test]
    fn test_format_function_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let ast = beacon_parser::AstNode::Module {
            body: vec![beacon_parser::AstNode::FunctionDef {
                name: "test_func".to_string(),
                args: vec!["x".to_string(), "y".to_string()],
                body: vec![],
                line: 1,
                col: 1,
            }],
        };
        let content = provider.format_function_hover("test_func", &ast, 1);

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("def test_func"));
        assert!(content.value.contains("Function"));
        assert!(content.value.contains("line 1"));
    }

    #[test]
    fn test_format_class_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let ast = beacon_parser::AstNode::Module { body: vec![] };
        let content = provider.format_class_hover("MyClass", &ast, 5);
        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("class MyClass"));
        assert!(content.value.contains("Class"));
        assert!(content.value.contains("line 5"));
    }

    #[test]
    fn test_format_variable_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let content = provider.format_variable_hover("my_var", &beacon_parser::SymbolKind::Variable, 10, 5);
        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("my_var"));
        assert!(content.value.contains("Variable"));
        assert!(content.value.contains("line 10"));
    }

    #[test]
    fn test_format_parameter_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let content = provider.format_parameter_hover("param", 3, 8);
        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("param"));
        assert!(content.value.contains("Parameter"));
        assert!(content.value.contains("line 3"));
    }

    #[test]
    fn test_format_import_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let content = provider.format_import_hover("os");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("os"));
        assert!(content.value.contains("Import"));
    }

    #[test]
    fn test_extract_function_signature() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let ast = beacon_parser::AstNode::Module {
            body: vec![beacon_parser::AstNode::FunctionDef {
                name: "calculate".to_string(),
                args: vec!["a".to_string(), "b".to_string()],
                body: vec![],
                line: 1,
                col: 1,
            }],
        };

        let signature = provider.extract_function_signature(&ast, "calculate");
        assert!(signature.is_some());
        assert_eq!(signature.unwrap(), "def calculate(a, b)");
    }

    #[test]
    fn test_extract_function_signature_nested_in_class() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let ast = beacon_parser::AstNode::Module {
            body: vec![beacon_parser::AstNode::ClassDef {
                name: "MyClass".to_string(),
                body: vec![beacon_parser::AstNode::FunctionDef {
                    name: "method".to_string(),
                    args: vec!["self".to_string()],
                    body: vec![],
                    line: 2,
                    col: 5,
                }],
                line: 1,
                col: 1,
            }],
        };

        let signature = provider.extract_function_signature(&ast, "method");
        assert!(signature.is_some());
        assert_eq!(signature.unwrap(), "def method(self)");
    }

    #[test]
    fn test_extract_function_signature_not_found() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let ast = beacon_parser::AstNode::Module { body: vec![] };
        let signature = provider.extract_function_signature(&ast, "nonexistent");
        assert!(signature.is_none());
    }

    #[test]
    fn test_hover_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _provider = HoverProvider::new(documents);
    }

    #[test]
    fn test_format_type_nested_complex() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let dict_type = Type::dict(Type::Con(TypeCtor::String), Type::Con(TypeCtor::Int));
        let list_dict = Type::list(dict_type);
        let formatted = provider.format_type(&list_dict);
        assert!(formatted.contains("list"));
        assert!(formatted.contains("dict"));
        assert!(formatted.contains("str"));
        assert!(formatted.contains("int"));
    }

    #[test]
    fn test_format_type_type_variables() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let tv = beacon_core::TypeVar::named(0, "T");
        let formatted = provider.format_type(&Type::Var(tv));
        assert!(formatted.contains("T"));
    }

    #[test]
    fn test_format_type_record() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let formatted = provider.format_type(&Type::Record(
            vec![
                ("x".to_string(), Type::Con(TypeCtor::Int)),
                ("y".to_string(), Type::Con(TypeCtor::String)),
            ],
            None,
        ));
        assert!(formatted.contains("x"));
        assert!(formatted.contains("y"));
    }

    #[test]
    fn test_format_type_any() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        assert_eq!(provider.format_type(&Type::Con(TypeCtor::Any)), "Any");
    }

    #[test]
    fn test_format_type_never() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        assert_eq!(provider.format_type(&Type::Con(TypeCtor::Never)), "Never");
    }

    #[test]
    fn test_extract_function_signature_empty_args() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let ast = beacon_parser::AstNode::Module {
            body: vec![beacon_parser::AstNode::FunctionDef {
                name: "no_args".to_string(),
                args: vec![],
                body: vec![],
                line: 1,
                col: 1,
            }],
        };

        let signature = provider.extract_function_signature(&ast, "no_args");
        assert!(signature.is_some());
        assert_eq!(signature.unwrap(), "def no_args()");
    }

    #[test]
    fn test_hover_with_type_at_position() {
        use std::str::FromStr;

        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = HoverParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _result = provider.hover(params, &mut analyzer);
    }

    #[test]
    fn test_format_type_forall() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let tv = beacon_core::TypeVar::new(0);
        let inner = Type::Fun(vec![Type::Var(tv.clone())], Box::new(Type::Var(tv.clone())));
        let forall = Type::ForAll(vec![tv], Box::new(inner));

        let formatted = provider.format_type(&forall);
        assert!(formatted.contains("∀") || formatted.contains("'t"));
    }

    #[test]
    fn test_format_type_app() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let app = Type::App(
            Box::new(Type::Con(TypeCtor::Class("Result".to_string()))),
            Box::new(Type::Con(TypeCtor::Int)),
        );

        let _formatted = provider.format_type(&app);
    }
}
