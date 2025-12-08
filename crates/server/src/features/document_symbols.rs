//! Document symbols provider
//!
//! Provides an outline view of the document showing all functions, classes, and variables.
//! Supports hierarchical structure with methods nested inside classes.

use crate::document::DocumentManager;

use beacon_parser::AstNode;
use lsp_types::{DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, Position, Range, SymbolKind};
use url::Url;

pub struct DocumentSymbolsProvider {
    documents: DocumentManager,
}

impl DocumentSymbolsProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    pub fn document_symbols(&self, params: DocumentSymbolParams) -> Option<DocumentSymbolResponse> {
        let uri = params.text_document.uri;
        self.extract_symbols(&uri)
    }

    fn extract_symbols(&self, uri: &Url) -> Option<DocumentSymbolResponse> {
        self.documents.get_document(uri, |doc| {
            let ast = doc.ast()?;
            let symbols = self.ast_to_symbols(ast);
            Some(DocumentSymbolResponse::Nested(symbols))
        })?
    }

    fn ast_to_symbols(&self, node: &AstNode) -> Vec<DocumentSymbol> {
        match node {
            AstNode::Module { body, .. } => {
                let mut symbols = Vec::new();
                for stmt in body {
                    if let Some(symbol) = self.node_to_symbol(stmt) {
                        symbols.push(symbol);
                    }
                }
                symbols
            }
            _ => Vec::new(),
        }
    }

    fn node_to_symbol(&self, node: &AstNode) -> Option<DocumentSymbol> {
        match node {
            AstNode::FunctionDef { name, args, body, line, col, .. } => {
                let range = self.node_range(*line, *col, body);
                let selection_range = self.identifier_range(*line, *col, name.len());

                let children: Vec<DocumentSymbol> =
                    body.iter().filter_map(|child| self.node_to_symbol(child)).collect();

                Some(DocumentSymbol {
                    name: name.clone(),
                    detail: Some(format!(
                        "({})",
                        args.iter().map(|p| p.name.as_str()).collect::<Vec<_>>().join(", ")
                    )),
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    range,
                    selection_range,
                    children: if children.is_empty() { None } else { Some(children) },
                    #[allow(deprecated)]
                    deprecated: Some(false),
                })
            }

            AstNode::ClassDef { name, body, line, col, .. } => {
                let range = self.node_range(*line, *col, body);
                let selection_range = self.identifier_range(*line, *col, name.len());
                let children: Vec<DocumentSymbol> = body
                    .iter()
                    .filter_map(|child| match child {
                        AstNode::FunctionDef { .. } => {
                            let mut symbol = self.node_to_symbol(child)?;
                            symbol.kind = SymbolKind::METHOD;
                            Some(symbol)
                        }
                        AstNode::ClassDef { .. } => self.node_to_symbol(child),
                        _ => None,
                    })
                    .collect();

                Some(DocumentSymbol {
                    name: name.clone(),
                    detail: None,
                    kind: SymbolKind::CLASS,
                    tags: None,
                    range,
                    selection_range,
                    children: if children.is_empty() { None } else { Some(children) },
                    #[allow(deprecated)]
                    deprecated: Some(false),
                })
            }

            AstNode::Assignment { target, value, line, col, .. } => {
                let target_str = target.target_to_string();
                let range = self.assignment_range(*line, *col, value);
                let selection_range = self.identifier_range(*line, *col, target_str.len());

                Some(DocumentSymbol {
                    name: target_str,
                    detail: None,
                    kind: SymbolKind::VARIABLE,
                    tags: None,
                    range,
                    selection_range,
                    children: None,
                    #[allow(deprecated)]
                    deprecated: Some(false),
                })
            }

            _ => None,
        }
    }

    /// Calculate range for a node based on its body
    fn node_range(&self, line: usize, col: usize, body: &[AstNode]) -> Range {
        let start = Position { line: (line.saturating_sub(1)) as u32, character: (col.saturating_sub(1)) as u32 };

        let end = if let Some(last_child) = body.last() {
            Self::node_end_position(last_child)
        } else {
            Position { line: start.line, character: start.character + 10 }
        };

        Range { start, end }
    }

    fn identifier_range(&self, line: usize, col: usize, name_len: usize) -> Range {
        let start = Position { line: (line.saturating_sub(1)) as u32, character: (col.saturating_sub(1)) as u32 };
        let end = Position { line: start.line, character: start.character + name_len as u32 };
        Range { start, end }
    }

    fn assignment_range(&self, line: usize, col: usize, value: &AstNode) -> Range {
        let start = Position { line: (line.saturating_sub(1)) as u32, character: (col.saturating_sub(1)) as u32 };
        let end = Self::node_end_position(value);
        Range { start, end }
    }

    fn node_end_position(node: &AstNode) -> Position {
        match node {
            AstNode::FunctionDef { body, line, col, .. } | AstNode::ClassDef { body, line, col, .. } => {
                match body.last() {
                    Some(last) => Self::node_end_position(last),
                    None => {
                        Position { line: (*line).saturating_sub(1) as u32, character: (*col).saturating_sub(1) as u32 }
                    }
                }
            }
            AstNode::Module { body, .. } => match body.last() {
                Some(last) => Self::node_end_position(last),
                None => Position { line: 0, character: 0 },
            },
            AstNode::If { body, else_body, .. } => {
                if let Some(else_stmts) = else_body
                    && let Some(last) = else_stmts.last() {
                        return Self::node_end_position(last);
                    }
                if let Some(last) = body.last() {
                    Self::node_end_position(last)
                } else {
                    Position { line: 0, character: 0 }
                }
            }
            AstNode::For { body, line, col, .. }
            | AstNode::While { body, line, col, .. }
            | AstNode::With { body, line, col, .. } => {
                if let Some(last) = body.last() {
                    Self::node_end_position(last)
                } else {
                    Position { line: (*line).saturating_sub(1) as u32, character: (*col + 10).saturating_sub(1) as u32 }
                }
            }
            AstNode::Try { body, finally_body, line, col, .. } => {
                if let Some(finally_stmts) = finally_body
                    && let Some(last) = finally_stmts.last() {
                        return Self::node_end_position(last);
                    }
                if let Some(last) = body.last() {
                    Self::node_end_position(last)
                } else {
                    Position { line: (*line).saturating_sub(1) as u32, character: (*col + 10).saturating_sub(1) as u32 }
                }
            }
            AstNode::Yield { line, col, .. }
            | AstNode::YieldFrom { line, col, .. }
            | AstNode::Await { line, col, .. }
            | AstNode::Tuple { line, col, .. }
            | AstNode::List { line, col, .. }
            | AstNode::Dict { line, col, .. }
            | AstNode::Set { line, col, .. }
            | AstNode::Literal { line, col, .. }
            | AstNode::Identifier { line, col, .. }
            | AstNode::Assignment { line, col, .. }
            | AstNode::AnnotatedAssignment { line, col, .. }
            | AstNode::Call { line, col, .. }
            | AstNode::Return { line, col, .. }
            | AstNode::Import { line, col, .. }
            | AstNode::ImportFrom { line, col, .. }
            | AstNode::Attribute { line, col, .. }
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
            | AstNode::Pass { line, col, .. }
            | AstNode::Break { line, col, .. }
            | AstNode::Continue { line, col, .. }
            | AstNode::Global { line, col, .. }
            | AstNode::Nonlocal { line, col, .. }
            | AstNode::Raise { line, col, .. }
            | AstNode::Assert { line, col, .. }
            | AstNode::Starred { line, col, .. }
            | AstNode::ParenthesizedExpression { line, col, .. } => {
                Position { line: (*line).saturating_sub(1) as u32, character: (*col + 10).saturating_sub(1) as u32 }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_document_symbols_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _provider = DocumentSymbolsProvider::new(documents);
    }

    #[test]
    fn test_extract_function_symbols() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentSymbolsProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def greet(name):
    return f"Hello {name}"

def calculate(x, y):
    return x + y
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let symbols = provider.extract_symbols(&uri);
        assert!(symbols.is_some());

        if let Some(DocumentSymbolResponse::Nested(symbols)) = symbols {
            assert_eq!(symbols.len(), 2);

            assert_eq!(symbols[0].name, "greet");
            assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
            assert!(symbols[0].detail.as_ref().unwrap().contains("name"));

            assert_eq!(symbols[1].name, "calculate");
            assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);
            assert!(symbols[1].detail.as_ref().unwrap().contains("x"));
            assert!(symbols[1].detail.as_ref().unwrap().contains("y"));
        } else {
            panic!("Expected nested document symbols");
        }
    }

    #[test]
    fn test_extract_class_symbols() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentSymbolsProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Calculator:
    def add(self, x, y):
        return x + y

    def subtract(self, x, y):
        return x - y
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let symbols = provider.extract_symbols(&uri);
        assert!(symbols.is_some());

        if let Some(DocumentSymbolResponse::Nested(symbols)) = symbols {
            assert_eq!(symbols.len(), 1);

            let class = &symbols[0];
            assert_eq!(class.name, "Calculator");
            assert_eq!(class.kind, SymbolKind::CLASS);

            let children = class.children.as_ref().unwrap();
            assert_eq!(children.len(), 2);

            assert_eq!(children[0].name, "add");
            assert_eq!(children[0].kind, SymbolKind::METHOD);

            assert_eq!(children[1].name, "subtract");
            assert_eq!(children[1].kind, SymbolKind::METHOD);
        } else {
            panic!("Expected nested document symbols");
        }
    }

    #[test]
    fn test_extract_variable_symbols() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentSymbolsProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 42
message = "Hello"
result = calculate(10, 20)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let symbols = provider.extract_symbols(&uri);
        assert!(symbols.is_some());

        if let Some(DocumentSymbolResponse::Nested(symbols)) = symbols {
            assert_eq!(symbols.len(), 3);

            assert_eq!(symbols[0].name, "x");
            assert_eq!(symbols[0].kind, SymbolKind::VARIABLE);

            assert_eq!(symbols[1].name, "message");
            assert_eq!(symbols[1].kind, SymbolKind::VARIABLE);

            assert_eq!(symbols[2].name, "result");
            assert_eq!(symbols[2].kind, SymbolKind::VARIABLE);
        } else {
            panic!("Expected nested document symbols");
        }
    }

    #[test]
    fn test_extract_mixed_symbols() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentSymbolsProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
VERSION = "1.0"

def initialize():
    pass

class Application:
    def run(self):
        pass

DEBUG = True
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let symbols = provider.extract_symbols(&uri);
        assert!(symbols.is_some());

        if let Some(DocumentSymbolResponse::Nested(symbols)) = symbols {
            assert_eq!(symbols.len(), 4);

            assert_eq!(symbols[0].name, "VERSION");
            assert_eq!(symbols[0].kind, SymbolKind::VARIABLE);

            assert_eq!(symbols[1].name, "initialize");
            assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);

            assert_eq!(symbols[2].name, "Application");
            assert_eq!(symbols[2].kind, SymbolKind::CLASS);

            assert_eq!(symbols[3].name, "DEBUG");
            assert_eq!(symbols[3].kind, SymbolKind::VARIABLE);
        } else {
            panic!("Expected nested document symbols");
        }
    }

    #[test]
    fn test_symbol_ranges() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentSymbolsProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def test():
    pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let symbols = provider.extract_symbols(&uri);
        assert!(symbols.is_some());

        if let Some(DocumentSymbolResponse::Nested(symbols)) = symbols {
            assert_eq!(symbols.len(), 1);

            let symbol = &symbols[0];
            assert!(symbol.selection_range.start.line <= symbol.range.start.line);
            assert!(symbol.selection_range.end.line <= symbol.range.end.line);
        } else {
            panic!("Expected nested document symbols");
        }
    }

    #[test]
    fn test_nested_classes() {
        let documents = DocumentManager::new().unwrap();
        let provider = DocumentSymbolsProvider::new(documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Outer:
    class Inner:
        pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let symbols = provider.extract_symbols(&uri);
        assert!(symbols.is_some());

        if let Some(DocumentSymbolResponse::Nested(symbols)) = symbols {
            assert_eq!(symbols.len(), 1);

            let outer = &symbols[0];
            assert_eq!(outer.name, "Outer");

            let children = outer.children.as_ref().unwrap();
            assert_eq!(children.len(), 1);
            assert_eq!(children[0].name, "Inner");
            assert_eq!(children[0].kind, SymbolKind::CLASS);
        } else {
            panic!("Expected nested document symbols");
        }
    }
}
