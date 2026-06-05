use beacon_core::{ParseError, Result};
use tree_sitter::Node;

use super::helpers::field_text;
use crate::{AstNode, Parameter, ParameterKind, PythonParser};

fn parameter_kind_from_name(name: &str) -> ParameterKind {
    if name.starts_with("**") {
        ParameterKind::KwArgs
    } else if name.starts_with('*') {
        ParameterKind::VarArgs
    } else {
        ParameterKind::PositionalOrKeyword
    }
}

impl PythonParser {
    pub(crate) fn extract_identifier(&self, node: &Node, source: &str, field: &str) -> Result<String> {
        field_text(node, source, field)
    }

    pub(crate) fn extract_function_args(&self, node: &Node, source: &str) -> Result<Vec<Parameter>> {
        if node.kind() == "lambda_parameters" {
            return self.extract_parameter_list(node, source);
        }

        if let Some(params) = node.child_by_field_name("parameters") {
            return self.extract_parameter_list(&params, source);
        }

        Ok(Vec::new())
    }

    pub(crate) fn extract_parameter_list(&self, params: &Node, source: &str) -> Result<Vec<Parameter>> {
        let mut args: Vec<Parameter> = Vec::new();
        let mut keyword_only = false;
        let mut cursor = params.walk();
        for child in params.children(&mut cursor) {
            if child.is_extra() || matches!(child.kind(), "," | "(" | ")") {
                continue;
            }
            match child.kind() {
                "positional_separator" => {
                    for param in &mut args {
                        if matches!(param.kind, ParameterKind::PositionalOrKeyword) {
                            param.kind = ParameterKind::PositionalOnly;
                        }
                    }
                    continue;
                }
                "keyword_separator" => {
                    keyword_only = true;
                    continue;
                }
                _ => {}
            }
            if let Some(mut param) = self.extract_parameter(&child, source)? {
                if keyword_only && matches!(param.kind, ParameterKind::PositionalOrKeyword) {
                    param.kind = ParameterKind::KeywordOnly;
                }
                if matches!(param.kind, ParameterKind::VarArgs) {
                    keyword_only = true;
                }
                args.push(param);
            }
        }
        Ok(args)
    }

    pub(crate) fn extract_parameter(&self, node: &Node, source: &str) -> Result<Option<Parameter>> {
        match node.kind() {
            "identifier" => {
                let arg_name = node.utf8_text(source.as_bytes()).map_err(|_| ParseError::InvalidUtf8)?;
                let start_position = node.start_position();
                let end_position = node.end_position();
                Ok(Some(Parameter {
                    name: arg_name.to_string(),
                    line: start_position.row + 1,
                    col: start_position.column + 1,
                    end_line: end_position.row + 1,
                    end_col: end_position.column + 1,
                    type_annotation: None,
                    default_value: None,
                    kind: ParameterKind::PositionalOrKeyword,
                }))
            }
            "list_splat_pattern" | "dictionary_splat_pattern" => {
                let arg_text = node.utf8_text(source.as_bytes()).map_err(|_| ParseError::InvalidUtf8)?;
                let start_position = node.start_position();
                let end_position = node.end_position();
                Ok(Some(Parameter {
                    name: arg_text.to_string(),
                    line: start_position.row + 1,
                    col: start_position.column + 1,
                    end_line: end_position.row + 1,
                    end_col: end_position.column + 1,
                    type_annotation: None,
                    default_value: None,
                    kind: if node.kind() == "dictionary_splat_pattern" {
                        ParameterKind::KwArgs
                    } else {
                        ParameterKind::VarArgs
                    },
                }))
            }
            "typed_parameter" | "typed_default_parameter" => {
                let mut name = None;
                let mut type_annotation = None;
                let mut default_value = None;
                let mut position = node.start_position();
                let end_position = node.end_position();

                if let Some(name_node) = node.child_by_field_name("name") {
                    name = Some(
                        name_node
                            .utf8_text(source.as_bytes())
                            .map_err(|_| ParseError::InvalidUtf8)?
                            .to_string(),
                    );
                    position = name_node.start_position();
                } else {
                    let mut cursor = node.walk();
                    for child in node.children(&mut cursor) {
                        if matches!(
                            child.kind(),
                            "identifier" | "list_splat_pattern" | "dictionary_splat_pattern"
                        ) {
                            name = Some(
                                child
                                    .utf8_text(source.as_bytes())
                                    .map_err(|_| ParseError::InvalidUtf8)?
                                    .to_string(),
                            );
                            position = child.start_position();
                            break;
                        }
                    }
                }

                if let Some(type_node) = node.child_by_field_name("type") {
                    type_annotation = Some(
                        type_node
                            .utf8_text(source.as_bytes())
                            .map_err(|_| ParseError::InvalidUtf8)?
                            .to_string(),
                    );
                }

                if let Some(value_node) = node.child_by_field_name("value") {
                    default_value = Some(Box::new(self.node_to_ast(value_node, source)?));
                }

                match name {
                    Some(n) => Ok(Some(Parameter {
                        name: n.clone(),
                        line: position.row + 1,
                        col: position.column + 1,
                        end_line: end_position.row + 1,
                        end_col: end_position.column + 1,
                        type_annotation,
                        default_value,
                        kind: parameter_kind_from_name(&n),
                    })),
                    None => Ok(None),
                }
            }
            "default_parameter" => {
                let mut name = None;
                let mut default_value = None;
                let mut position = node.start_position();
                let end_position = node.end_position();

                if let Some(name_node) = node.child_by_field_name("name") {
                    name = Some(
                        name_node
                            .utf8_text(source.as_bytes())
                            .map_err(|_| ParseError::InvalidUtf8)?
                            .to_string(),
                    );
                    position = name_node.start_position();
                }

                if let Some(value_node) = node.child_by_field_name("value") {
                    default_value = Some(Box::new(self.node_to_ast(value_node, source)?));
                }

                match name {
                    Some(n) => Ok(Some(Parameter {
                        name: n,
                        line: position.row + 1,
                        col: position.column + 1,
                        end_line: end_position.row + 1,
                        end_col: end_position.column + 1,
                        type_annotation: None,
                        default_value,
                        kind: ParameterKind::PositionalOrKeyword,
                    })),
                    None => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    pub(crate) fn extract_function_body(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing function body".to_string()))?;

        let mut body = Vec::new();
        let mut cursor = body_node.walk();

        for child in body_node.children(&mut cursor) {
            if child.is_extra() {
                continue;
            }

            body.push(self.node_to_ast(child, source)?);
        }

        Ok(body)
    }

    pub(crate) fn extract_return_type(&self, node: &Node, source: &str) -> Option<String> {
        node.child_by_field_name("return_type")
            .and_then(|type_node| type_node.utf8_text(source.as_bytes()).ok())
            .map(|s| s.to_string())
    }

    pub(crate) fn extract_class_body(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing class body".to_string()))?;

        let mut body = Vec::new();
        let mut cursor = body_node.walk();

        for child in body_node.children(&mut cursor) {
            if !child.is_extra() {
                body.push(self.node_to_ast(child, source)?);
            }
        }

        Ok(body)
    }

    pub(crate) fn extract_class_bases(&self, node: &Node, source: &str) -> Vec<String> {
        let mut bases = Vec::new();

        if let Some(arg_list) = node.child_by_field_name("superclasses") {
            let mut cursor = arg_list.walk();
            for child in arg_list.children(&mut cursor) {
                if child.kind() == "keyword_argument"
                    || child.kind() == ","
                    || child.kind() == "("
                    || child.kind() == ")"
                {
                    continue;
                }
                if child.is_extra() {
                    continue;
                }
                if let Ok(base_name) = child.utf8_text(source.as_bytes()) {
                    bases.push(base_name.to_string());
                }
            }
        }

        bases
    }

    pub(crate) fn extract_class_metaclass(&self, node: &Node, source: &str) -> Option<String> {
        let arg_list = node.child_by_field_name("superclasses")?;
        let mut cursor = arg_list.walk();

        for child in arg_list.children(&mut cursor) {
            if child.kind() != "keyword_argument" {
                continue;
            }

            let name_node = child.child(0)?;
            let name = name_node.utf8_text(source.as_bytes()).ok()?;
            if name != "metaclass" {
                continue;
            }

            let value_node = child.child(2)?;
            return value_node.utf8_text(source.as_bytes()).ok().map(|s| s.to_string());
        }

        None
    }

    pub(crate) fn extract_decorator_name(&self, decorator_node: &Node, source: &str) -> Option<String> {
        let mut cursor = decorator_node.walk();
        for child in decorator_node.children(&mut cursor) {
            if child.kind() == "identifier" || child.kind() == "attribute" {
                return child.utf8_text(source.as_bytes()).ok().map(|s| s.to_string());
            }
        }
        None
    }

    pub(crate) fn extract_docstring(&self, body_node: &Node, source: &str) -> Option<String> {
        let mut cursor = body_node.walk();
        for child in body_node.children(&mut cursor) {
            if child.is_extra() {
                continue;
            }

            if child.kind() == "expression_statement" {
                let mut expr_cursor = child.walk();
                for expr_child in child.children(&mut expr_cursor) {
                    if expr_child.kind() == "string" {
                        return self.extract_string_content(&expr_child, source);
                    }
                }
            }

            break;
        }
        None
    }

    pub(crate) fn extract_string_content(&self, string_node: &Node, source: &str) -> Option<String> {
        let mut cursor = string_node.walk();
        let mut content = String::new();

        for child in string_node.children(&mut cursor) {
            if child.kind() == "string_content"
                && let Ok(text) = child.utf8_text(source.as_bytes())
            {
                content.push_str(text);
            }
        }

        if content.is_empty() { None } else { Some(content) }
    }
}
