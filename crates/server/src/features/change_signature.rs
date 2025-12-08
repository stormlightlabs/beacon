//! Change Signature refactoring
//!
//! Modifies function signatures and updates all call sites across the workspace.
//! Supports adding, removing, renaming, and reordering parameters.

use super::refactoring::{EditCollector, RefactoringContext, RefactoringValidator};
use crate::utils;

use lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Parameter change description
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ParameterChange {
    /// Add a new parameter
    Add {
        name: String,
        default_value: Option<String>,
        position: usize,
    },
    /// Remove an existing parameter
    Remove { name: String },
    /// Rename a parameter
    Rename { old_name: String, new_name: String },
    /// Reorder parameters
    Reorder { new_order: Vec<String> },
}

/// Parameters for change signature refactoring
pub struct ChangeSignatureParams {
    /// The file containing the function
    pub uri: Url,
    /// Position of the function definition
    pub position: Position,
    /// List of parameter changes
    pub changes: Vec<ParameterChange>,
}

/// Change signature refactoring provider
pub struct ChangeSignatureProvider {
    context: RefactoringContext,
}

/// Information about a function's signature
struct FunctionSignature {
    name: String,
    parameters: Vec<Parameter>,
    return_annotation: Option<String>,
}

/// Information about a single parameter
#[derive(Clone, Debug)]
struct Parameter {
    name: String,
    type_annotation: Option<String>,
    default_value: Option<String>,
}

/// Information about a call site
struct CallSite {
    uri: Url,
    range: Range,
}

impl ChangeSignatureProvider {
    /// Create a new change signature provider
    pub fn new(context: RefactoringContext) -> Self {
        Self { context }
    }

    /// Execute change signature refactoring
    pub async fn execute(&self, params: ChangeSignatureParams) -> Option<WorkspaceEdit> {
        for change in &params.changes {
            if Self::validate_parameter_change(change).is_err() {
                return None;
            }
        }

        let (tree, text) = self.context.get_tree_and_text(&params.uri)?;

        let function_def = Self::find_function_definition(&tree, &text, params.position)?;
        let signature = Self::extract_function_signature(function_def, &text)?;
        let new_signature = Self::apply_signature_changes(&signature, &params.changes)?;

        let mut call_sites = Vec::new();
        for file_ctx in self.context.iter_relevant_files(&params.uri).await {
            Self::find_call_sites(
                file_ctx.tree.root_node(),
                &file_ctx.text,
                &signature.name,
                &file_ctx.uri,
                &mut call_sites,
            );
        }

        let mut collector = EditCollector::new();

        let def_edit = Self::generate_definition_edit(function_def, &text, &new_signature)?;
        collector.add_edit(params.uri.clone(), def_edit);

        for call_site in &call_sites {
            let (call_tree, call_text) = self.context.get_tree_and_text(&call_site.uri)?;
            let byte_offset = Self::position_to_byte_offset(&call_text, call_site.range.start);
            let call_node = Self::find_call_at_offset(call_tree.root_node(), byte_offset)?;

            if let Some(edit) =
                Self::generate_call_site_edit(call_node, &call_text, &signature, &new_signature, &params.changes)
            {
                collector.add_edit(call_site.uri.clone(), edit);
            }
        }

        collector.into_workspace_edit()
    }

    /// Validate a parameter change
    fn validate_parameter_change(change: &ParameterChange) -> Result<(), String> {
        match change {
            ParameterChange::Add { name, .. } => RefactoringValidator::validate_identifier(name),
            ParameterChange::Rename { new_name, .. } => RefactoringValidator::validate_identifier(new_name),
            _ => Ok(()),
        }
    }

    /// Find function definition at or near a position
    fn find_function_definition<'a>(
        tree: &'a tree_sitter::Tree, text: &str, position: Position,
    ) -> Option<tree_sitter::Node<'a>> {
        let byte_offset = Self::position_to_byte_offset(text, position);
        let root = tree.root_node();

        Self::find_function_at_offset(root, byte_offset)
    }

    /// Recursively find function definition containing the byte offset
    fn find_function_at_offset(node: tree_sitter::Node, byte_offset: usize) -> Option<tree_sitter::Node> {
        if node.kind() == "function_definition" {
            if let Some(name_node) = node.child_by_field_name("name")
                && name_node.start_byte() <= byte_offset
                && byte_offset <= name_node.end_byte()
            {
                return Some(node);
            }

            if node.start_byte() <= byte_offset && byte_offset <= node.end_byte() {
                if let Some(body) = node.child_by_field_name("body")
                    && byte_offset >= body.start_byte()
                {
                    return Self::find_function_at_offset(body, byte_offset);
                }
                return Some(node);
            }
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(func) = Self::find_function_at_offset(child, byte_offset) {
                return Some(func);
            }
        }

        None
    }

    /// Extract function signature information
    fn extract_function_signature(node: tree_sitter::Node, text: &str) -> Option<FunctionSignature> {
        if node.kind() != "function_definition" {
            return None;
        }

        let name_node = node.child_by_field_name("name")?;
        let name = name_node.utf8_text(text.as_bytes()).ok()?.to_string();

        let params_node = node.child_by_field_name("parameters")?;
        let parameters = Self::extract_parameters(params_node, text);

        let return_annotation = node
            .child_by_field_name("return_type")
            .and_then(|n| n.utf8_text(text.as_bytes()).ok())
            .map(|s| s.to_string());

        Some(FunctionSignature { name, parameters, return_annotation })
    }

    /// Extract parameters from function parameters node
    fn extract_parameters(params_node: tree_sitter::Node, text: &str) -> Vec<Parameter> {
        let mut parameters = Vec::new();
        let mut cursor = params_node.walk();

        for child in params_node.children(&mut cursor) {
            match child.kind() {
                "identifier" => {
                    if let Ok(param_name) = child.utf8_text(text.as_bytes()) {
                        parameters.push(Parameter {
                            name: param_name.to_string(),
                            type_annotation: None,
                            default_value: None,
                        });
                    }
                }
                "typed_parameter" => {
                    let name = child
                        .child_by_field_name("name")
                        .and_then(|n| n.utf8_text(text.as_bytes()).ok())
                        .map(|s| s.to_string());

                    let type_annotation = child
                        .child_by_field_name("type")
                        .and_then(|n| n.utf8_text(text.as_bytes()).ok())
                        .map(|s| s.to_string());

                    if let Some(name) = name {
                        parameters.push(Parameter { name, type_annotation, default_value: None });
                    }
                }
                "default_parameter" => {
                    let name = child
                        .child_by_field_name("name")
                        .and_then(|n| n.utf8_text(text.as_bytes()).ok())
                        .map(|s| s.to_string());

                    let default_value = child
                        .child_by_field_name("value")
                        .and_then(|n| n.utf8_text(text.as_bytes()).ok())
                        .map(|s| s.to_string());

                    let type_annotation = if let Some(name_node) = child.child_by_field_name("name")
                        && name_node.kind() == "typed_parameter"
                    {
                        name_node
                            .child_by_field_name("type")
                            .and_then(|n| n.utf8_text(text.as_bytes()).ok())
                            .map(|s| s.to_string())
                    } else {
                        None
                    };

                    if let Some(name) = name {
                        parameters.push(Parameter { name, type_annotation, default_value });
                    }
                }
                _ => {}
            }
        }

        parameters
    }

    /// Apply parameter changes to create a new signature
    fn apply_signature_changes(
        signature: &FunctionSignature, changes: &[ParameterChange],
    ) -> Option<FunctionSignature> {
        let mut new_params = signature.parameters.clone();

        for change in changes {
            match change {
                ParameterChange::Add { name, default_value, position } => {
                    let param =
                        Parameter { name: name.clone(), type_annotation: None, default_value: default_value.clone() };
                    let insert_pos = (*position).min(new_params.len());
                    new_params.insert(insert_pos, param);
                }
                ParameterChange::Remove { name } => {
                    new_params.retain(|p| p.name != *name);
                }
                ParameterChange::Rename { old_name, new_name } => {
                    for param in &mut new_params {
                        if param.name == *old_name {
                            param.name = new_name.clone();
                        }
                    }
                }
                ParameterChange::Reorder { new_order } => {
                    let mut reordered = Vec::new();
                    let param_map: HashMap<_, _> = new_params.iter().map(|p| (p.name.clone(), p.clone())).collect();

                    for name in new_order {
                        if let Some(param) = param_map.get(name) {
                            reordered.push(param.clone());
                        }
                    }

                    new_params = reordered;
                }
            }
        }

        Some(FunctionSignature {
            name: signature.name.clone(),
            parameters: new_params,
            return_annotation: signature.return_annotation.clone(),
        })
    }

    /// Generate edit for the function definition
    fn generate_definition_edit(
        function_def: tree_sitter::Node, text: &str, new_signature: &FunctionSignature,
    ) -> Option<TextEdit> {
        let params_node = function_def.child_by_field_name("parameters")?;
        let range = utils::tree_sitter_range_to_lsp_range(text, params_node.range());

        let new_params = Self::format_parameters(&new_signature.parameters);

        Some(TextEdit { range, new_text: format!("({})", new_params) })
    }

    /// Format parameters as a string
    fn format_parameters(parameters: &[Parameter]) -> String {
        parameters
            .iter()
            .map(|p| {
                let mut param_str = p.name.clone();

                if let Some(ref type_ann) = p.type_annotation {
                    param_str.push_str(": ");
                    param_str.push_str(type_ann);
                }

                if let Some(ref default) = p.default_value {
                    param_str.push_str(" = ");
                    param_str.push_str(default);
                }

                param_str
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Find all call sites for a function
    fn find_call_sites(
        node: tree_sitter::Node, text: &str, function_name: &str, uri: &Url, call_sites: &mut Vec<CallSite>,
    ) {
        if node.kind() == "call"
            && let Some(func_node) = node.child_by_field_name("function")
            && func_node.kind() == "identifier"
            && let Ok(name) = func_node.utf8_text(text.as_bytes())
            && name == function_name
        {
            call_sites
                .push(CallSite { uri: uri.clone(), range: utils::tree_sitter_range_to_lsp_range(text, node.range()) });
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            Self::find_call_sites(child, text, function_name, uri, call_sites);
        }
    }

    /// Recursively find call expression at byte offset
    fn find_call_at_offset(node: tree_sitter::Node, byte_offset: usize) -> Option<tree_sitter::Node> {
        if node.kind() == "call" && node.start_byte() <= byte_offset && byte_offset <= node.end_byte() {
            return Some(node);
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(call) = Self::find_call_at_offset(child, byte_offset) {
                return Some(call);
            }
        }

        None
    }

    /// Generate edit for a call site
    fn generate_call_site_edit(
        call_node: tree_sitter::Node, text: &str, old_signature: &FunctionSignature, new_signature: &FunctionSignature,
        changes: &[ParameterChange],
    ) -> Option<TextEdit> {
        let args_node = call_node.child_by_field_name("arguments")?;
        let old_args = Self::extract_call_arguments(args_node, text);
        let new_args = Self::transform_arguments(&old_args, old_signature, new_signature, changes)?;
        let range = utils::tree_sitter_range_to_lsp_range(text, args_node.range());
        Some(TextEdit { range, new_text: format!("({})", new_args.join(", ")) })
    }

    /// Extract arguments from a call's argument list
    fn extract_call_arguments(args_node: tree_sitter::Node, text: &str) -> Vec<String> {
        let mut arguments = Vec::new();
        let mut cursor = args_node.walk();

        for child in args_node.children(&mut cursor) {
            match child.kind() {
                "(" | ")" | "," => {}
                _ => {
                    if let Ok(arg_text) = child.utf8_text(text.as_bytes()) {
                        arguments.push(arg_text.to_string());
                    }
                }
            }
        }

        arguments
    }

    /// Transform call arguments based on signature changes
    fn transform_arguments(
        old_args: &[String], old_signature: &FunctionSignature, new_signature: &FunctionSignature,
        changes: &[ParameterChange],
    ) -> Option<Vec<String>> {
        let mut arg_map: HashMap<String, String> = HashMap::new();

        for (i, arg) in old_args.iter().enumerate() {
            if i < old_signature.parameters.len() {
                arg_map.insert(old_signature.parameters[i].name.clone(), arg.clone());
            }
        }

        for change in changes {
            if let ParameterChange::Rename { old_name, new_name } = change
                && let Some(value) = arg_map.remove(old_name)
            {
                arg_map.insert(new_name.clone(), value);
            }
        }

        let mut new_args = Vec::new();
        for param in &new_signature.parameters {
            if let Some(arg_value) = arg_map.get(&param.name) {
                new_args.push(arg_value.clone());
            } else if let Some(ref default) = param.default_value {
                new_args.push(default.clone());
            } else {
                new_args.push(format!("# TODO: provide value for {}", param.name));
            }
        }

        Some(new_args)
    }

    /// Convert LSP position to byte offset
    fn position_to_byte_offset(text: &str, position: Position) -> usize {
        let mut offset = 0;
        for (line_num, line) in text.lines().enumerate() {
            if line_num < position.line as usize {
                offset += line.len() + 1;
            } else {
                offset += position.character as usize;
                break;
            }
        }
        offset
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_parameter_change() {
        let valid_add = ParameterChange::Add {
            name: "new_param".to_string(),
            default_value: Some("None".to_string()),
            position: 0,
        };
        assert!(ChangeSignatureProvider::validate_parameter_change(&valid_add).is_ok());

        let invalid_add = ParameterChange::Add { name: "123invalid".to_string(), default_value: None, position: 0 };
        assert!(ChangeSignatureProvider::validate_parameter_change(&invalid_add).is_err());

        let valid_rename = ParameterChange::Rename { old_name: "old".to_string(), new_name: "new_name".to_string() };
        assert!(ChangeSignatureProvider::validate_parameter_change(&valid_rename).is_ok());
    }

    #[test]
    fn test_format_parameters() {
        let params = vec![
            Parameter { name: "x".to_string(), type_annotation: None, default_value: None },
            Parameter { name: "y".to_string(), type_annotation: Some("int".to_string()), default_value: None },
            Parameter {
                name: "z".to_string(),
                type_annotation: Some("str".to_string()),
                default_value: Some("'default'".to_string()),
            },
        ];

        let formatted = ChangeSignatureProvider::format_parameters(&params);
        assert_eq!(formatted, "x, y: int, z: str = 'default'");
    }

    #[test]
    fn test_apply_signature_changes_add() {
        let sig = FunctionSignature {
            name: "test".to_string(),
            parameters: vec![Parameter { name: "x".to_string(), type_annotation: None, default_value: None }],
            return_annotation: None,
        };

        let changes =
            vec![ParameterChange::Add { name: "y".to_string(), default_value: Some("0".to_string()), position: 1 }];

        let new_sig = ChangeSignatureProvider::apply_signature_changes(&sig, &changes).unwrap();
        assert_eq!(new_sig.parameters.len(), 2);
        assert_eq!(new_sig.parameters[1].name, "y");
    }

    #[test]
    fn test_apply_signature_changes_remove() {
        let sig = FunctionSignature {
            name: "test".to_string(),
            parameters: vec![
                Parameter { name: "x".to_string(), type_annotation: None, default_value: None },
                Parameter { name: "y".to_string(), type_annotation: None, default_value: None },
            ],
            return_annotation: None,
        };

        let changes = vec![ParameterChange::Remove { name: "x".to_string() }];

        let new_sig = ChangeSignatureProvider::apply_signature_changes(&sig, &changes).unwrap();
        assert_eq!(new_sig.parameters.len(), 1);
        assert_eq!(new_sig.parameters[0].name, "y");
    }

    #[test]
    fn test_apply_signature_changes_rename() {
        let sig = FunctionSignature {
            name: "test".to_string(),
            parameters: vec![Parameter { name: "old_name".to_string(), type_annotation: None, default_value: None }],
            return_annotation: None,
        };

        let changes =
            vec![ParameterChange::Rename { old_name: "old_name".to_string(), new_name: "new_name".to_string() }];

        let new_sig = ChangeSignatureProvider::apply_signature_changes(&sig, &changes).unwrap();
        assert_eq!(new_sig.parameters[0].name, "new_name");
    }
}
