use beacon_core::{ParseError, Result};
use tree_sitter::Node;

use super::types::InfoArgsKwargs;
use crate::{AstNode, BinaryOperator, CompareOperator, Parameter, PythonParser, UnaryOperator};

impl PythonParser {
    pub(crate) fn extract_call_function(&self, node: &Node, source: &str) -> Result<AstNode> {
        let function_node = node
            .child_by_field_name("function")
            .ok_or_else(|| ParseError::TreeSitterError("Missing call function".to_string()))?;

        self.node_to_ast(function_node, source)
    }

    pub(crate) fn _extract_call_args(&self, node: &Node, source: &str) -> Result<Vec<AstNode>> {
        let args_node = node.child_by_field_name("arguments");
        let mut args = Vec::new();

        if let Some(arguments) = args_node {
            let mut cursor = arguments.walk();
            for child in arguments.children(&mut cursor) {
                if !child.is_extra() && child.kind() != "(" && child.kind() != ")" && child.kind() != "," {
                    args.push(self.node_to_ast(child, source)?);
                }
            }
        }

        Ok(args)
    }

    pub(crate) fn extract_call_args_and_kwargs(&self, node: &Node, source: &str) -> Result<InfoArgsKwargs> {
        let args_node = node.child_by_field_name("arguments");
        let mut args = Vec::new();
        let mut keywords = Vec::new();

        if let Some(arguments) = args_node {
            let mut cursor = arguments.walk();
            for child in arguments.children(&mut cursor) {
                if !child.is_extra() && child.kind() != "(" && child.kind() != ")" && child.kind() != "," {
                    match child.kind() {
                        "keyword_argument" => {
                            if let Some(name_node) = child.child_by_field_name("name") {
                                let name = name_node
                                    .utf8_text(source.as_bytes())
                                    .map_err(|_| ParseError::InvalidUtf8)?
                                    .to_string();

                                if let Some(value_node) = child.child_by_field_name("value") {
                                    let value = self.node_to_ast(value_node, source)?;
                                    keywords.push((name, value));
                                }
                            }
                        }
                        _ => args.push(self.node_to_ast(child, source)?),
                    }
                }
            }
        }

        Ok(InfoArgsKwargs(args, keywords))
    }

    pub(crate) fn extract_attribute_info(&self, node: &Node, source: &str) -> Result<(AstNode, String)> {
        let object_node = node
            .child_by_field_name("object")
            .ok_or_else(|| ParseError::TreeSitterError("Missing object in attribute".to_string()))?;

        let attribute_node = node
            .child_by_field_name("attribute")
            .ok_or_else(|| ParseError::TreeSitterError("Missing attribute name".to_string()))?;

        let object = self.node_to_ast(object_node, source)?;

        let attribute = attribute_node
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?
            .to_string();

        Ok((object, attribute))
    }

    pub(crate) fn extract_named_expr_info(&self, node: &Node, source: &str) -> Result<(String, AstNode)> {
        let name = node
            .child_by_field_name("name")
            .ok_or_else(|| ParseError::TreeSitterError("Missing named expression target".to_string()))?
            .utf8_text(source.as_bytes())
            .map_err(|_| ParseError::InvalidUtf8)?
            .to_string();

        let value_node = node
            .child_by_field_name("value")
            .ok_or_else(|| ParseError::TreeSitterError("Missing named expression value".to_string()))?;
        let value = self.node_to_ast(value_node, source)?;

        Ok((name, value))
    }

    pub(crate) fn extract_binary_op_info(
        &self, node: &Node, source: &str,
    ) -> Result<(AstNode, BinaryOperator, AstNode)> {
        let left_node = node
            .child_by_field_name("left")
            .ok_or_else(|| ParseError::TreeSitterError("Missing binary operator left operand".to_string()))?;
        let left = self.node_to_ast(left_node, source)?;

        let right_node = node
            .child_by_field_name("right")
            .ok_or_else(|| ParseError::TreeSitterError("Missing binary operator right operand".to_string()))?;
        let right = self.node_to_ast(right_node, source)?;

        let mut cursor = node.walk();
        let op_str = node
            .children(&mut cursor)
            .find(|n| self.is_binary_operator(n.kind()))
            .and_then(|n| n.utf8_text(source.as_bytes()).ok())
            .ok_or_else(|| ParseError::TreeSitterError("Missing binary operator".to_string()))?;

        let op = self.parse_binary_operator(op_str)?;

        Ok((left, op, right))
    }

    pub(crate) fn extract_unary_op_info(&self, node: &Node, source: &str) -> Result<(UnaryOperator, AstNode)> {
        let operand_node = node
            .child_by_field_name("argument")
            .ok_or_else(|| ParseError::TreeSitterError("Missing unary operator operand".to_string()))?;
        let operand = self.node_to_ast(operand_node, source)?;

        let mut cursor = node.walk();
        let op_str = node
            .children(&mut cursor)
            .find(|n| self.is_unary_operator(n.kind()))
            .and_then(|n| n.utf8_text(source.as_bytes()).ok())
            .ok_or_else(|| ParseError::TreeSitterError("Missing unary operator".to_string()))?;

        let op = self.parse_unary_operator(op_str)?;

        Ok((op, operand))
    }

    pub(crate) fn extract_comparison_info(
        &self, node: &Node, source: &str,
    ) -> Result<(AstNode, Vec<CompareOperator>, Vec<AstNode>)> {
        let mut operands = Vec::new();
        let mut operators = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            if !child.is_extra() {
                if self.is_compare_operator(child.kind()) {
                    if let Ok(text) = child.utf8_text(source.as_bytes())
                        && let Ok(op) = self.parse_compare_operator(text)
                    {
                        operators.push(op);
                    }
                } else if child.kind() != "(" && child.kind() != ")" {
                    operands.push(self.node_to_ast(child, source)?);
                }
            }
        }

        if operands.is_empty() {
            return Err(ParseError::TreeSitterError("No operands in comparison".to_string()).into());
        }

        let left = operands.remove(0);

        Ok((left, operators, operands))
    }

    pub(crate) fn extract_boolean_op_info(
        &self, node: &Node, source: &str,
    ) -> Result<(AstNode, BinaryOperator, AstNode)> {
        let left_node = node
            .child_by_field_name("left")
            .ok_or_else(|| ParseError::TreeSitterError("Missing boolean operator left operand".to_string()))?;
        let left = self.node_to_ast(left_node, source)?;

        let right_node = node
            .child_by_field_name("right")
            .ok_or_else(|| ParseError::TreeSitterError("Missing boolean operator right operand".to_string()))?;
        let right = self.node_to_ast(right_node, source)?;

        let mut cursor = node.walk();
        let op_str = node
            .children(&mut cursor)
            .find(|n| n.kind() == "and" || n.kind() == "or")
            .and_then(|n| n.utf8_text(source.as_bytes()).ok())
            .ok_or_else(|| ParseError::TreeSitterError("Missing boolean operator".to_string()))?;

        let op = match op_str {
            "and" => BinaryOperator::And,
            "or" => BinaryOperator::Or,
            _ => return Err(ParseError::TreeSitterError(format!("Unknown boolean operator: {op_str}")).into()),
        };

        Ok((left, op, right))
    }

    pub(crate) fn extract_lambda_info(&self, node: &Node, source: &str) -> Result<(Vec<Parameter>, AstNode)> {
        let args = node
            .child_by_field_name("parameters")
            .map(|params| self.extract_function_args(&params, source))
            .transpose()?
            .unwrap_or_default();

        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| ParseError::TreeSitterError("Missing lambda body".to_string()))?;
        let body = self.node_to_ast(body_node, source)?;

        Ok((args, body))
    }

    pub(crate) fn extract_subscript_info(&self, node: &Node, source: &str) -> Result<(AstNode, AstNode)> {
        let value_node = node
            .child_by_field_name("value")
            .ok_or_else(|| ParseError::TreeSitterError("Missing subscript value".to_string()))?;
        let value = self.node_to_ast(value_node, source)?;

        let subscript_node = node
            .child_by_field_name("subscript")
            .ok_or_else(|| ParseError::TreeSitterError("Missing subscript index".to_string()))?;
        let slice = self.node_to_ast(subscript_node, source)?;

        Ok((value, slice))
    }

    pub(crate) fn is_binary_operator(&self, kind: &str) -> bool {
        matches!(
            kind,
            "+" | "-" | "*" | "/" | "//" | "%" | "**" | "@" | "&" | "|" | "^" | "<<" | ">>"
        )
    }

    pub(crate) fn is_unary_operator(&self, kind: &str) -> bool {
        matches!(kind, "not" | "~" | "+" | "-")
    }

    pub(crate) fn is_compare_operator(&self, kind: &str) -> bool {
        matches!(kind, "==" | "!=" | "<" | "<=" | ">" | ">=" | "is" | "in" | "not")
    }

    pub(crate) fn parse_binary_operator(&self, op: &str) -> Result<BinaryOperator> {
        match op {
            "+" => Ok(BinaryOperator::Add),
            "-" => Ok(BinaryOperator::Sub),
            "*" => Ok(BinaryOperator::Mult),
            "/" => Ok(BinaryOperator::Div),
            "//" => Ok(BinaryOperator::FloorDiv),
            "%" => Ok(BinaryOperator::Mod),
            "**" => Ok(BinaryOperator::Pow),
            "@" => Ok(BinaryOperator::MatMult),
            "&" => Ok(BinaryOperator::BitAnd),
            "|" => Ok(BinaryOperator::BitOr),
            "^" => Ok(BinaryOperator::BitXor),
            "<<" => Ok(BinaryOperator::LeftShift),
            ">>" => Ok(BinaryOperator::RightShift),
            _ => Err(ParseError::TreeSitterError(format!("Unknown binary operator: {op}")).into()),
        }
    }

    pub(crate) fn parse_unary_operator(&self, op: &str) -> Result<UnaryOperator> {
        match op {
            "not" => Ok(UnaryOperator::Not),
            "~" => Ok(UnaryOperator::Invert),
            "+" => Ok(UnaryOperator::Plus),
            "-" => Ok(UnaryOperator::Minus),
            _ => Err(ParseError::TreeSitterError(format!("Unknown unary operator: {op}")).into()),
        }
    }

    pub(crate) fn parse_compare_operator(&self, op: &str) -> Result<CompareOperator> {
        match op {
            "==" => Ok(CompareOperator::Eq),
            "!=" => Ok(CompareOperator::NotEq),
            "<" => Ok(CompareOperator::Lt),
            "<=" => Ok(CompareOperator::LtE),
            ">" => Ok(CompareOperator::Gt),
            ">=" => Ok(CompareOperator::GtE),
            "is" => Ok(CompareOperator::Is),
            "is not" => Ok(CompareOperator::IsNot),
            "in" => Ok(CompareOperator::In),
            "not in" => Ok(CompareOperator::NotIn),
            _ => Err(ParseError::TreeSitterError(format!("Unknown comparison operator: {op}")).into()),
        }
    }
}
