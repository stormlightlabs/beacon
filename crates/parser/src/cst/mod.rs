//! Tree-sitter CST to Beacon AST conversion.

pub(crate) mod comprehensions;
pub(crate) mod definitions;
pub(crate) mod expressions;
pub(crate) mod helpers;
pub(crate) mod imports;
pub(crate) mod literals;
pub(crate) mod patterns;
pub(crate) mod statements;
pub(crate) mod types;

use beacon_core::{ParseError, Result};
use tree_sitter::Node;

use crate::{AstNode, PythonParser};

use self::{
    helpers::node_span,
    types::{InfoArgsKwargs, InfoFor, InfoIf, InfoTry},
};

impl PythonParser {
    pub(crate) fn node_to_ast(&self, node: Node, source: &str) -> Result<AstNode> {
        let (line, col, end_line, end_col) = node_span(&node);

        match node.kind() {
            "decorated_definition" => {
                let mut decorators = Vec::new();
                let mut definition_node = None;
                let mut cursor = node.walk();

                for child in node.children(&mut cursor) {
                    match child.kind() {
                        "decorator" => {
                            if let Some(dec_name) = self.extract_decorator_name(&child, source) {
                                decorators.push(dec_name)
                            }
                        }
                        "function_definition" | "async_function_definition" | "class_definition" => {
                            definition_node = Some(child)
                        }
                        _ => {}
                    }
                }
                if let Some(def_node) = definition_node {
                    let mut ast = self.node_to_ast(def_node, source)?;

                    match &mut ast {
                        AstNode::FunctionDef { decorators: decs, .. } | AstNode::ClassDef { decorators: decs, .. } => {
                            *decs = decorators
                        }
                        _ => {}
                    }

                    return Ok(ast);
                }

                Ok(AstNode::Identifier { name: "<decorated>".to_string(), line, col, end_line, end_col })
            }
            "module" => {
                let mut body = Vec::new();
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if !child.is_extra() {
                        body.push(self.node_to_ast(child, source)?)
                    }
                }

                let docstring = self.extract_docstring(&node, source);
                Ok(AstNode::Module { body, docstring })
            }
            "function_definition" => {
                let name = self.extract_identifier(&node, source, "name")?;
                let args = self.extract_function_args(&node, source)?;
                let body = self.extract_function_body(&node, source)?;
                let return_type = self.extract_return_type(&node, source);
                let decorators = Vec::new();
                let docstring = node
                    .child_by_field_name("body")
                    .and_then(|body_node| self.extract_docstring(&body_node, source));

                let is_async = {
                    let mut cursor = node.walk();
                    node.children(&mut cursor).any(|child| child.kind() == "async")
                };

                Ok(AstNode::FunctionDef {
                    name,
                    args,
                    body,
                    docstring,
                    return_type,
                    decorators,
                    is_async,
                    line,
                    col,
                    end_line,
                    end_col,
                })
            }
            "class_definition" => {
                let name = self.extract_identifier(&node, source, "name")?;
                let bases = self.extract_class_bases(&node, source);
                let metaclass = self.extract_class_metaclass(&node, source);
                let body = self.extract_class_body(&node, source)?;
                let decorators = Vec::new();
                let docstring = node
                    .child_by_field_name("body")
                    .and_then(|body_node| self.extract_docstring(&body_node, source));

                Ok(AstNode::ClassDef {
                    name,
                    bases,
                    metaclass,
                    body,
                    docstring,
                    decorators,
                    line,
                    col,
                    end_line,
                    end_col,
                })
            }
            "expression_statement" => {
                if let Some(child) = node.named_child(0) {
                    self.node_to_ast(child, source)
                } else {
                    Ok(AstNode::Identifier { name: "<empty_expression>".to_string(), line, col, end_line, end_col })
                }
            }
            "assignment" => {
                let target = self.extract_assignment_target(&node, source)?;
                if let Some(type_node) = node.child_by_field_name("type") {
                    let type_annotation = type_node
                        .utf8_text(source.as_bytes())
                        .map_err(|_| ParseError::InvalidUtf8)?
                        .to_string();

                    let value = node
                        .child_by_field_name("right")
                        .map(|v| self.node_to_ast(v, source))
                        .transpose()?
                        .map(Box::new);

                    Ok(AstNode::AnnotatedAssignment {
                        target: Box::new(target),
                        type_annotation,
                        value,
                        line,
                        col,
                        end_line,
                        end_col,
                    })
                } else {
                    let value = self.extract_assignment_value(&node, source)?;
                    Ok(AstNode::Assignment {
                        target: Box::new(target),
                        value: Box::new(value),
                        line,
                        col,
                        end_line,
                        end_col,
                    })
                }
            }
            "augmented_assignment" => {
                let target = self.extract_assignment_target(&node, source)?;
                let right = self.extract_assignment_value(&node, source)?;
                let operator = self.extract_augmented_assignment_operator(&node, source)?;

                let augmented_value = AstNode::BinaryOp {
                    left: Box::new(target.clone()),
                    op: operator,
                    right: Box::new(right),
                    line,
                    col,
                    end_line,
                    end_col,
                };

                Ok(AstNode::Assignment {
                    target: Box::new(target),
                    value: Box::new(augmented_value),
                    line,
                    col,
                    end_line,
                    end_col,
                })
            }
            "call" => {
                let function = self.extract_call_function(&node, source)?;
                let InfoArgsKwargs(args, keywords) = self.extract_call_args_and_kwargs(&node, source)?;
                Ok(AstNode::Call { function: Box::new(function), args, keywords, line, col, end_line, end_col })
            }
            "identifier" => {
                let name = node.utf8_text(source.as_bytes()).map_err(|_| ParseError::InvalidUtf8)?;
                Ok(AstNode::Identifier { name: name.to_string(), line, col, end_line, end_col })
            }
            "string" | "integer" | "float" | "true" | "false" | "none" => {
                let value = self.extract_literal_value(&node, source)?;
                Ok(AstNode::Literal { value, line, col, end_line, end_col })
            }
            "return_statement" => {
                let value = self.extract_return_value(&node, source)?;
                Ok(AstNode::Return { value: value.map(Box::new), line, col, end_line, end_col })
            }
            "import_statement" => {
                let modules = self.extract_import_info(&node, source)?;
                let mut iter = modules.into_iter();
                let (module, alias) = iter
                    .next()
                    .ok_or_else(|| ParseError::TreeSitterError("Missing import name".to_string()))?;
                let extra_modules: Vec<_> = iter.collect();
                Ok(AstNode::Import { module, alias, extra_modules, line, col, end_line, end_col })
            }
            "import_from_statement" => {
                let (module, names) = self.extract_import_from_info(&node, source)?;
                Ok(AstNode::ImportFrom { module, names, line, col, end_line, end_col })
            }
            "future_import_statement" => {
                let names = self.extract_future_import_names(&node, source);
                Ok(AstNode::ImportFrom { module: "__future__".to_string(), names, line, col, end_line, end_col })
            }
            "attribute" => {
                let (object, attribute) = self.extract_attribute_info(&node, source)?;
                Ok(AstNode::Attribute { object: Box::new(object), attribute, line, col, end_line, end_col })
            }
            "if_statement" => {
                let InfoIf(test, body, elif_parts, else_body) = self.extract_if_info(&node, source)?;
                Ok(AstNode::If { test: Box::new(test), body, elif_parts, else_body, line, col, end_line, end_col })
            }
            "for_statement" => {
                let InfoFor(target, iter, body, else_body, is_async) = self.extract_for_info(&node, source)?;
                Ok(AstNode::For {
                    target: Box::new(target),
                    iter: Box::new(iter),
                    body,
                    else_body,
                    is_async,
                    line,
                    col,
                    end_line,
                    end_col,
                })
            }
            "while_statement" => {
                let (test, body, else_body) = self.extract_while_info(&node, source)?;
                Ok(AstNode::While { test: Box::new(test), body, else_body, line, col, end_line, end_col })
            }
            "try_statement" => {
                let InfoTry(body, handlers, else_body, finally_body) = self.extract_try_info(&node, source)?;
                Ok(AstNode::Try { body, handlers, else_body, finally_body, line, col, end_line, end_col })
            }
            "with_statement" => {
                let (items, body, is_async) = self.extract_with_info(&node, source)?;
                Ok(AstNode::With { items, body, is_async, line, col, end_line, end_col })
            }
            "list_comprehension" => {
                let (element, generators) = self.extract_list_comp_info(&node, source)?;
                Ok(AstNode::ListComp { element: Box::new(element), generators, line, col, end_line, end_col })
            }
            "dictionary_comprehension" => {
                let (key, value, generators) = self.extract_dict_comp_info(&node, source)?;
                Ok(AstNode::DictComp {
                    key: Box::new(key),
                    value: Box::new(value),
                    generators,
                    line,
                    col,
                    end_line,
                    end_col,
                })
            }
            "set_comprehension" => {
                let (element, generators) = self.extract_set_comp_info(&node, source)?;
                Ok(AstNode::SetComp { element: Box::new(element), generators, line, col, end_line, end_col })
            }
            "generator_expression" => {
                let (element, generators) = self.extract_generator_exp_info(&node, source)?;
                Ok(AstNode::GeneratorExp { element: Box::new(element), generators, line, col, end_line, end_col })
            }
            "named_expression" => {
                let (target, value) = self.extract_named_expr_info(&node, source)?;
                Ok(AstNode::NamedExpr { target, value: Box::new(value), line, col, end_line, end_col })
            }
            "binary_operator" => {
                let (left, op, right) = self.extract_binary_op_info(&node, source)?;
                Ok(
                    AstNode::BinaryOp {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                        line,
                        col,
                        end_line,
                        end_col,
                    },
                )
            }
            "unary_operator" | "not_operator" => {
                let (op, operand) = self.extract_unary_op_info(&node, source)?;
                Ok(AstNode::UnaryOp { op, operand: Box::new(operand), line, col, end_line, end_col })
            }
            "comparison_operator" => {
                let (left, ops, comparators) = self.extract_comparison_info(&node, source)?;
                Ok(AstNode::Compare { left: Box::new(left), ops, comparators, line, col, end_line, end_col })
            }
            "lambda" => {
                let (args, body) = self.extract_lambda_info(&node, source)?;
                Ok(AstNode::Lambda { args, body: Box::new(body), line, col, end_line, end_col })
            }
            "subscript" => {
                let (value, slice) = self.extract_subscript_info(&node, source)?;
                Ok(AstNode::Subscript { value: Box::new(value), slice: Box::new(slice), line, col, end_line, end_col })
            }
            "match_statement" => {
                let (subject, cases) = self.extract_match_info(&node, source)?;
                Ok(AstNode::Match { subject: Box::new(subject), cases, line, col, end_line, end_col })
            }
            "pass_statement" => Ok(AstNode::Pass { line, col, end_line, end_col }),
            "break_statement" => Ok(AstNode::Break { line, col, end_line, end_col }),
            "continue_statement" => Ok(AstNode::Continue { line, col, end_line, end_col }),
            "raise_statement" => {
                let exc = self.extract_raise_value(&node, source)?;
                Ok(AstNode::Raise { exc: exc.map(Box::new), line, col, end_line, end_col })
            }
            "global_statement" => {
                let names = self.extract_global_or_nonlocal_names(&node, source)?;
                Ok(AstNode::Global { names, line, col, end_line, end_col })
            }
            "nonlocal_statement" => {
                let names = self.extract_global_or_nonlocal_names(&node, source)?;
                Ok(AstNode::Nonlocal { names, line, col, end_line, end_col })
            }
            "yield" => {
                let value = node
                    .named_child(0)
                    .map(|child| self.node_to_ast(child, source))
                    .transpose()?;
                Ok(AstNode::Yield { value: value.map(Box::new), line, col, end_line, end_col })
            }
            "await" => {
                if let Some(child) = node.named_child(0) {
                    let value = self.node_to_ast(child, source)?;
                    Ok(AstNode::Await { value: Box::new(value), line, col, end_line, end_col })
                } else {
                    Err(ParseError::MissingNode("await value".to_string()).into())
                }
            }
            "tuple" | "expression_list" | "pattern_list" | "tuple_pattern" => {
                let elements = self.extract_tuple_elements(&node, source)?;
                let is_parenthesized = node.kind() == "tuple" || node.kind() == "tuple_pattern";
                Ok(AstNode::Tuple { elements, is_parenthesized, line, col, end_line, end_col })
            }
            "list" | "list_pattern" => {
                let elements = self.extract_list_elements(&node, source)?;
                Ok(AstNode::List { elements, line, col, end_line, end_col })
            }
            "dictionary" => {
                let (keys, values) = self.extract_dict_pairs(&node, source)?;
                Ok(AstNode::Dict { keys, values, line, col, end_line, end_col })
            }
            "set" => {
                let elements = self.extract_set_elements(&node, source)?;
                Ok(AstNode::Set { elements, line, col, end_line, end_col })
            }
            "parenthesized_expression" => match node.named_child(0) {
                Some(child) => {
                    let expression = self.node_to_ast(child, source)?;
                    Ok(AstNode::ParenthesizedExpression {
                        expression: Box::new(expression),
                        line,
                        col,
                        end_line,
                        end_col,
                    })
                }
                None => Ok(AstNode::Identifier { name: "<empty_parens>".to_string(), line, col, end_line, end_col }),
            },
            "boolean_operator" => {
                let (left, op, right) = self.extract_boolean_op_info(&node, source)?;
                Ok(
                    AstNode::BinaryOp {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                        line,
                        col,
                        end_line,
                        end_col,
                    },
                )
            }
            "assert_statement" => {
                let test = node
                    .named_child(0)
                    .ok_or_else(|| ParseError::TreeSitterError("Missing assert test".to_string()))?;
                let test_ast = self.node_to_ast(test, source)?;

                let msg = node
                    .named_child(1)
                    .map(|msg_node| self.node_to_ast(msg_node, source))
                    .transpose()?
                    .map(Box::new);

                Ok(AstNode::Assert { test: Box::new(test_ast), msg, line, col, end_line, end_col })
            }
            "starred_expression" => {
                if let Some(child) = node.named_child(0) {
                    let value = self.node_to_ast(child, source)?;
                    Ok(AstNode::Starred { value: Box::new(value), line, col, end_line, end_col })
                } else {
                    Err(ParseError::MissingNode("starred value".to_string()).into())
                }
            }
            _ => match node.utf8_text(source.as_bytes()) {
                Ok(text) => Ok(AstNode::Identifier { name: text.to_string(), line, col, end_line, end_col }),
                Err(_) => Ok(AstNode::Identifier { name: format!("<{}>]", node.kind()), line, col, end_line, end_col }),
            },
        }
    }
}
