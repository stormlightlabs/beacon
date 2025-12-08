//! Signature help provider
//!
//! Displays function signature information while typing function calls, showing parameter names,
//! types, and highlighting the currently active parameter based on cursor position.

use crate::analysis::Analyzer;
use crate::document::DocumentManager;
use crate::parser::LspParser;
use crate::utils;

use beacon_core::Type;
use lsp_types::{
    ParameterInformation, ParameterLabel, Position, SignatureHelp, SignatureHelpParams, SignatureInformation,
};
use tree_sitter::Node;
use url::Url;

pub struct SignatureHelpProvider {
    documents: DocumentManager,
}

impl SignatureHelpProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    /// Provide signature help at a position
    pub fn signature_help(&self, params: SignatureHelpParams, analyzer: &mut Analyzer) -> Option<SignatureHelp> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        self.get_signature_help(&uri, position, analyzer)
    }

    /// Get signature help by finding the enclosing call expression and extracting function type
    fn get_signature_help(&self, uri: &Url, position: Position, analyzer: &mut Analyzer) -> Option<SignatureHelp> {
        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let parser = LspParser::new().ok()?;
            let node = parser.node_at_position(tree, &text, position)?;
            let symbol_table = doc.symbol_table()?;
            let (call_node, func_node) = self.find_enclosing_call(node, &text)?;

            let func_type = if func_node.kind() == "identifier" {
                let identifier_text = func_node.utf8_text(text.as_bytes()).ok()?;

                if let Some(symbol) = symbol_table.lookup_symbol(identifier_text, symbol_table.root_scope) {
                    let def_pos = Position { line: (symbol.line - 1) as u32, character: symbol.col as u32 };
                    analyzer.type_at_position(uri, def_pos).ok()??
                } else {
                    return None;
                }
            } else {
                let func_position = self.node_to_position(func_node, &text)?;
                analyzer.type_at_position(uri, func_position).ok()??
            };

            let signature_info = Self::extract_signature_info(&func_type)?;
            let active_parameter = self.determine_active_parameter(call_node, position, &text);
            Some(SignatureHelp { signatures: vec![signature_info], active_signature: Some(0), active_parameter })
        })?
    }

    /// Find the enclosing call expression by walking up the tree
    fn find_enclosing_call<'a>(&self, node: Node<'a>, _text: &str) -> Option<(Node<'a>, Node<'a>)> {
        let mut current = node;

        loop {
            match current.kind() {
                "call" => {
                    let func = current.child_by_field_name("function")?;
                    return Some((current, func));
                }
                "argument_list" => {
                    if let Some(parent) = current.parent()
                        && parent.kind() == "call"
                    {
                        let func = parent.child_by_field_name("function")?;
                        return Some((parent, func));
                    }
                }
                _ => {}
            }

            match current.parent() {
                Some(parent) => current = parent,
                None => return None,
            }
        }
    }

    /// Extract signature information from a function type
    fn extract_signature_info(func_type: &Type) -> Option<SignatureInformation> {
        match func_type {
            Type::Fun(params, ret_type) => {
                let mut label = String::from("(");
                let mut parameters = Vec::new();
                let mut param_labels = Vec::new();

                for (i, (param_name, param_type)) in params.iter().enumerate() {
                    if i > 0 {
                        label.push_str(", ");
                    }

                    let start = label.len();

                    let param_str = if param_name.is_empty()
                        || (param_name.starts_with('_') && param_name[1..].chars().all(|c| c.is_ascii_digit()))
                    {
                        format!("{param_type}")
                    } else {
                        format!("{param_name}: {param_type}")
                    };

                    label.push_str(&param_str);
                    let end = label.len();

                    param_labels.push((start as u32, end as u32));

                    parameters.push(ParameterInformation {
                        label: ParameterLabel::LabelOffsets([start as u32, end as u32]),
                        documentation: Some(lsp_types::Documentation::String(format!("Type: {param_type}"))),
                    });
                }

                label.push_str(") -> ");
                label.push_str(&ret_type.to_string());

                Some(SignatureInformation {
                    label,
                    documentation: None,
                    parameters: Some(parameters),
                    active_parameter: None,
                })
            }
            Type::BoundMethod(_, _, method_type) => Self::extract_signature_info(method_type),
            Type::ForAll(_, inner) => Self::extract_signature_info(inner),
            _ => None,
        }
    }

    /// Determine the active parameter index based on cursor position
    /// Counts the number of commas before the cursor position within the argument list
    fn determine_active_parameter(&self, call_node: Node, position: Position, text: &str) -> Option<u32> {
        let arg_list = call_node.child_by_field_name("argument")?;
        let cursor_offset = utils::position_to_byte_offset(text, position);

        let mut comma_count = 0;
        let mut cursor = arg_list.walk();

        for child in arg_list.children(&mut cursor) {
            if child.start_byte() >= cursor_offset {
                break;
            }

            if child.kind() == "," {
                comma_count += 1;
            }
        }

        Some(comma_count)
    }

    /// Convert tree-sitter Node to LSP Position
    fn node_to_position(&self, node: Node, text: &str) -> Option<Position> {
        Some(utils::tree_sitter_point_to_position(text, node.start_position()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use lsp_types::TextDocumentPositionParams;
    use std::str::FromStr;

    #[test]
    fn test_signature_help_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _ = SignatureHelpProvider::new(documents);
    }

    #[test]
    fn test_signature_help_simple_function() {
        let documents = DocumentManager::new().unwrap();
        let provider = SignatureHelpProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def greet(name: str) -> str:
    return f"Hello {name}"

x = "world"
result = greet(x)"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = SignatureHelpParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 4, character: 15 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            context: None,
        };

        let _ = provider.signature_help(params, &mut analyzer);
    }

    #[test]
    fn test_signature_help_multiple_parameters() {
        let documents = DocumentManager::new().unwrap();
        let provider = SignatureHelpProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def add(x: int, y: int) -> int:
    return x + y

a = 1
b = 2
result = add(a, b)"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = SignatureHelpParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 5, character: 16 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            context: None,
        };

        let _ = provider.signature_help(params, &mut analyzer);
    }

    #[test]
    fn test_signature_help_no_call() {
        let documents = DocumentManager::new().unwrap();
        let provider = SignatureHelpProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = SignatureHelpParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 4 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            context: None,
        };

        let result = provider.signature_help(params, &mut analyzer);
        assert!(result.is_none());
    }

    #[test]
    fn test_determine_active_parameter_first() {
        let documents = DocumentManager::new().unwrap();
        let provider = SignatureHelpProvider::new(documents.clone());
        let config = Config::default();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def func(a, b, c):
    pass

func("#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = SignatureHelpParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 3, character: 5 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            context: None,
        };

        let result = provider.signature_help(params, &mut analyzer);
        if let Some(sig_help) = result {
            assert_eq!(sig_help.active_parameter, Some(0));
        }
    }

    #[test]
    fn test_extract_signature_info_from_type() {
        let func_type = Type::fun(
            vec![
                ("x".to_string(), Type::Con(beacon_core::TypeCtor::Int)),
                ("y".to_string(), Type::Con(beacon_core::TypeCtor::String)),
            ],
            Type::Con(beacon_core::TypeCtor::Bool),
        );

        let sig_info = SignatureHelpProvider::extract_signature_info(&func_type);
        assert!(sig_info.is_some());

        let sig = sig_info.unwrap();
        assert!(sig.label.contains("x: int"));
        assert!(sig.label.contains("y: str"));
        assert!(sig.label.contains("-> bool"));
        assert_eq!(sig.parameters.as_ref().unwrap().len(), 2);
    }

    #[test]
    fn test_extract_signature_info_unnamed_params() {
        let func_type = Type::fun_unnamed(
            vec![
                Type::Con(beacon_core::TypeCtor::Int),
                Type::Con(beacon_core::TypeCtor::String),
            ],
            Type::Con(beacon_core::TypeCtor::Bool),
        );

        let sig_info = SignatureHelpProvider::extract_signature_info(&func_type);
        assert!(sig_info.is_some());

        let sig = sig_info.unwrap();
        assert!(sig.label.contains("int"));
        assert!(sig.label.contains("str"));
        assert!(sig.label.contains("-> bool"));
    }

    #[test]
    fn test_node_to_position() {
        let documents = DocumentManager::new().unwrap();
        let provider = SignatureHelpProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42\ny = 100";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();
        documents.get_document(&uri, |doc| {
            let tree = doc.tree().unwrap();
            let text = doc.text();
            let root = tree.root_node();

            let pos = provider.node_to_position(root, &text);
            assert!(pos.is_some());
            assert_eq!(pos.unwrap(), Position { line: 0, character: 0 });
        });
    }
}
