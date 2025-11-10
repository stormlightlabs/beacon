//! Function analysis helpers
//!
//! This module provides utilities for analyzing function definitions, including
//! detecting function kinds (regular, generator, async generator, coroutine) and
//! analyzing return paths for type inference.

use beacon_parser::AstNode;

/// Represents the kind of function based on yield/await detection
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    /// Regular synchronous function
    Regular,
    /// Generator function (contains yield or yield from)
    Generator,
    /// Async generator function (async def with yield)
    AsyncGenerator,
    /// Coroutine function (async def without yield)
    Coroutine,
}

/// Detect what kind of function this is based on its body and async flag
///
/// Traverses the function body AST to detect yield, yield from, and await expressions.
/// Note: This is a simplified implementation that detects presence but doesn't validate
/// that yields/awaits are at the correct scope level (e.g., not inside nested functions).
pub fn detect_function_kind(body: &[AstNode], is_async: bool) -> FunctionKind {
    let has_yield = contains_yield(body);

    match (is_async, has_yield) {
        (true, true) => FunctionKind::AsyncGenerator,
        (true, false) => FunctionKind::Coroutine,
        (false, true) => FunctionKind::Generator,
        (false, false) => FunctionKind::Regular,
    }
}

/// Check if the AST nodes contain yield or yield from expressions in the current scope.
///
/// This correctly excludes yields that appear in nested function definitions, lambdas,
/// or comprehensions (which have their own scope).
pub fn contains_yield(nodes: &[AstNode]) -> bool {
    for node in nodes {
        if check_node_for_yield(node) {
            return true;
        }
    }
    false
}

/// Recursively check a single node for yield/yield from in the current scope
fn check_node_for_yield(node: &AstNode) -> bool {
    match node {
        AstNode::Yield { .. } | AstNode::YieldFrom { .. } => true,
        AstNode::FunctionDef { .. } | AstNode::ClassDef { .. } => false,
        AstNode::If { test, body, elif_parts, else_body, .. } => {
            check_node_for_yield(test)
                || contains_yield(body)
                || elif_parts.iter().any(|(test, body)| check_node_for_yield(test) || contains_yield(body))
                || else_body.as_ref().is_some_and(|b| contains_yield(b))
        }
        AstNode::For { iter, body, else_body, .. } => {
            check_node_for_yield(iter) || contains_yield(body) || else_body.as_ref().is_some_and(|b| contains_yield(b))
        }
        AstNode::While { test, body, else_body, .. } => {
            check_node_for_yield(test) || contains_yield(body) || else_body.as_ref().is_some_and(|b| contains_yield(b))
        }
        AstNode::Try { body, handlers, else_body, finally_body, .. } => {
            contains_yield(body)
                || handlers.iter().any(|h| contains_yield(&h.body))
                || else_body.as_ref().is_some_and(|b| contains_yield(b))
                || finally_body.as_ref().is_some_and(|b| contains_yield(b))
        }
        AstNode::With { body, .. } => contains_yield(body),
        AstNode::Match { subject, cases, .. } => {
            check_node_for_yield(subject) || cases.iter().any(|c| contains_yield(&c.body))
        }
        AstNode::ListComp { .. }
        | AstNode::DictComp { .. }
        | AstNode::SetComp { .. }
        | AstNode::GeneratorExp { .. } => false,
        AstNode::Call { args, keywords, .. } => {
            args.iter().any(check_node_for_yield) || keywords.iter().any(|(_, v)| check_node_for_yield(v))
        }
        AstNode::BinaryOp { left, right, .. } => check_node_for_yield(left) || check_node_for_yield(right),
        AstNode::UnaryOp { operand, .. } => check_node_for_yield(operand),
        AstNode::Compare { left, comparators, .. } => {
            check_node_for_yield(left) || comparators.iter().any(check_node_for_yield)
        }
        AstNode::Lambda { .. } => false,
        AstNode::Assignment { value, .. } => check_node_for_yield(value),
        AstNode::AnnotatedAssignment { value, .. } => value.as_ref().is_some_and(|v| check_node_for_yield(v)),
        AstNode::NamedExpr { value, .. } => check_node_for_yield(value),
        AstNode::Return { value, .. } => value.as_ref().is_some_and(|v| check_node_for_yield(v)),
        AstNode::Raise { exc, .. } => exc.as_ref().is_some_and(|e| check_node_for_yield(e)),
        AstNode::Attribute { object, .. } => check_node_for_yield(object),
        AstNode::Subscript { value, slice, .. } => check_node_for_yield(value) || check_node_for_yield(slice),
        AstNode::Tuple { elements, .. } | AstNode::List { elements, .. } => elements.iter().any(check_node_for_yield),
        AstNode::Dict { keys, values, .. } => {
            keys.iter().any(check_node_for_yield) || values.iter().any(check_node_for_yield)
        }
        AstNode::Set { elements, .. } => elements.iter().any(check_node_for_yield),
        AstNode::Await { value, .. } => check_node_for_yield(value),
        AstNode::Identifier { .. }
        | AstNode::Literal { .. }
        | AstNode::Pass { .. }
        | AstNode::Break { .. }
        | AstNode::Continue { .. }
        | AstNode::Import { .. }
        | AstNode::ImportFrom { .. }
        | AstNode::Module { .. }
        | AstNode::Assert { .. }
        | AstNode::Starred { .. }
        | AstNode::ParenthesizedExpression { .. } => false,
    }
}

/// Return path analysis result
///
/// Tracks what kinds of returns exist in a function to infer the correct return type.
#[derive(Debug, Clone)]
pub struct ReturnPathAnalysis {
    /// Whether function has explicit `return value` statements
    pub has_value_returns: bool,
    /// Whether function has explicit `return` (no value) statements
    pub has_none_returns: bool,
    /// Whether function can fall through (implicit return None)
    pub has_implicit_return: bool,
}

impl ReturnPathAnalysis {
    /// Determine if this function should infer Optional[T] return type
    ///
    /// A function gets Optional[T] when it has both:
    /// - Explicit returns with values, AND
    /// - Either explicit `return` without value OR implicit returns (fall through)
    pub fn should_infer_optional(&self) -> bool {
        self.has_value_returns && (self.has_none_returns || self.has_implicit_return)
    }

    /// Determine if this function should infer None return type
    ///
    /// A function gets None return type when it has NO value returns, only explicit `return` or implicit returns.
    pub fn should_infer_none(&self) -> bool {
        !self.has_value_returns && (self.has_none_returns || self.has_implicit_return)
    }
}

/// Analyze return paths in a function body
///
/// Examines all paths through the function to determine what types of returns exist.
/// This is used to infer Optional[T] for mixed returns and None for implicit returns.
pub fn analyze_return_paths(body: &[AstNode]) -> ReturnPathAnalysis {
    let mut analysis = ReturnPathAnalysis {
        has_value_returns: false,
        has_none_returns: false,
        has_implicit_return: true,
    };

    for stmt in body {
        collect_returns(stmt, &mut analysis);
    }

    analysis.has_implicit_return = !all_paths_exit(body);

    analysis
}

/// Recursively collect return statements from a node
fn collect_returns(node: &AstNode, analysis: &mut ReturnPathAnalysis) {
    match node {
        AstNode::Return { value, .. } => {
            if value.is_some() {
                analysis.has_value_returns = true;
            } else {
                analysis.has_none_returns = true;
            }
        }
        AstNode::If { body, elif_parts, else_body, .. } => {
            for stmt in body {
                collect_returns(stmt, analysis);
            }
            for (_, elif_body) in elif_parts {
                for stmt in elif_body {
                    collect_returns(stmt, analysis);
                }
            }
            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    collect_returns(stmt, analysis);
                }
            }
        }
        AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
            for stmt in body {
                collect_returns(stmt, analysis);
            }
            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    collect_returns(stmt, analysis);
                }
            }
        }
        AstNode::Try { body, handlers, else_body, finally_body, .. } => {
            for stmt in body {
                collect_returns(stmt, analysis);
            }
            for handler in handlers {
                for stmt in &handler.body {
                    collect_returns(stmt, analysis);
                }
            }
            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    collect_returns(stmt, analysis);
                }
            }
            if let Some(finally_stmts) = finally_body {
                for stmt in finally_stmts {
                    collect_returns(stmt, analysis);
                }
            }
        }
        AstNode::With { body, .. } => {
            for stmt in body {
                collect_returns(stmt, analysis);
            }
        }
        AstNode::Match { cases, .. } => {
            for case in cases {
                for stmt in &case.body {
                    collect_returns(stmt, analysis);
                }
            }
        }
        AstNode::FunctionDef { .. } | AstNode::ClassDef { .. } | AstNode::Lambda { .. } => {}
        _ => {}
    }
}

/// Check if all paths through the statements exit (return or raise)
pub fn all_paths_exit(stmts: &[AstNode]) -> bool {
    if stmts.is_empty() {
        return false;
    }

    for stmt in stmts {
        if stmt_always_exits(stmt) {
            return true;
        }
    }

    false
}

/// Check if a statement always exits (never falls through)
fn stmt_always_exits(stmt: &AstNode) -> bool {
    match stmt {
        AstNode::Return { .. } | AstNode::Raise { .. } => true,
        AstNode::If { body, elif_parts, else_body, .. } => {
            if else_body.is_none() {
                return false;
            }
            all_paths_exit(body)
                && elif_parts.iter().all(|(_, elif_body)| all_paths_exit(elif_body))
                && else_body.as_ref().is_some_and(|e| all_paths_exit(e))
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::LiteralValue;

    fn make_yield(line: usize, col: usize) -> AstNode {
        AstNode::Yield { value: None, end_line: line, line, col, end_col: col + 5 }
    }

    #[test]
    fn test_contains_yield_simple() {
        let nodes = vec![make_yield(1, 1)];
        assert!(contains_yield(&nodes));
    }

    #[test]
    fn test_contains_yield_nested_function() {
        let nodes = vec![AstNode::FunctionDef {
            name: "inner".to_string(),
            args: vec![],
            body: vec![make_yield(2, 5)],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 2,
            end_line: 2,
            col: 1,
            end_col: 1,
        }];
        assert!(!contains_yield(&nodes), "Yields in nested functions should not be detected");
    }

    #[test]
    fn test_contains_yield_in_lambda() {
        let nodes = vec![AstNode::Assignment {
            target: Box::new(AstNode::Identifier {
                name: "x".to_string(),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            value: Box::new(AstNode::Lambda {
                args: vec![],
                body: Box::new(make_yield(1, 20)),
                line: 1,
                col: 5,
                end_line: 1,
                end_col: 5,
            }),
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];
        assert!(!contains_yield(&nodes), "Yields in lambda functions should not be detected");
    }

    #[test]
    fn test_contains_yield_in_list_comp() {
        let nodes = vec![AstNode::ListComp {
            element: Box::new(make_yield(1, 10)),
            generators: vec![],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];
        assert!(!contains_yield(&nodes), "Yields in list comprehensions should not be detected");
    }

    #[test]
    fn test_contains_yield_in_generator_exp() {
        let nodes = vec![AstNode::GeneratorExp {
            element: Box::new(make_yield(1, 10)),
            generators: vec![],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];
        assert!(!contains_yield(&nodes), "Yields in generator expressions should not be detected");
    }

    #[test]
    fn test_contains_yield_in_dict_comp() {
        let nodes = vec![AstNode::DictComp {
            key: Box::new(make_yield(1, 10)),
            value: Box::new(AstNode::Identifier {
                name: "v".to_string(),
                line: 1,
                col: 15,
                end_line: 1,
                end_col: 16,
            }),
            generators: vec![],
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];
        assert!(!contains_yield(&nodes), "Yields in dict comprehensions should not be detected");
    }

    #[test]
    fn test_contains_yield_in_set_comp() {
        let nodes = vec![AstNode::SetComp {
            element: Box::new(make_yield(1, 10)),
            generators: vec![],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];
        assert!(!contains_yield(&nodes), "Yields in set comprehensions should not be detected");
    }

    #[test]
    fn test_contains_yield_in_if_statement() {
        let nodes = vec![AstNode::If {
            test: Box::new(AstNode::Identifier {
                name: "condition".to_string(),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 13,
            }),
            body: vec![make_yield(2, 5)],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];
        assert!(contains_yield(&nodes), "Yields in if statement bodies should be detected");
    }

    #[test]
    fn test_detect_function_kind_regular() {
        let body = vec![AstNode::Return {
            value: Some(Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                end_line: 1,
                col: 8,
                end_col: 8,
            })),
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];
        assert_eq!(detect_function_kind(&body, false), FunctionKind::Regular);
    }

    #[test]
    fn test_detect_function_kind_generator() {
        let body = vec![AstNode::Yield {
            value: Some(Box::new(AstNode::Literal {
                value: LiteralValue::Integer(1),
                line: 1,
                end_line: 1,
                col: 11,
                end_col: 11,
            })),
            line: 1,
            end_line: 1,
            col: 5,
            end_col: 5,
        }];
        assert_eq!(detect_function_kind(&body, false), FunctionKind::Generator);
    }

    #[test]
    fn test_detect_function_kind_async_generator() {
        let body = vec![AstNode::Yield {
            value: Some(Box::new(AstNode::Literal {
                value: LiteralValue::Integer(1),
                line: 1,
                end_line: 1,
                col: 11,
                end_col: 11,
            })),
            line: 1,
            end_line: 1,
            col: 5,
            end_col: 5,
        }];
        assert_eq!(detect_function_kind(&body, true), FunctionKind::AsyncGenerator);
    }

    #[test]
    fn test_detect_function_kind_coroutine() {
        let body = vec![AstNode::Await {
            value: Box::new(AstNode::Identifier {
                name: "something".to_string(),
                line: 1,
                col: 11,
                end_line: 1,
                end_col: 20,
            }),
            line: 1,
            end_line: 1,
            col: 5,
            end_col: 5,
        }];
        assert_eq!(detect_function_kind(&body, true), FunctionKind::Coroutine);
    }

    #[test]
    fn test_return_path_analysis_implicit_none() {
        let body = vec![AstNode::Assignment {
            target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
            value: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                col: 5,
                end_line: 1,
                end_col: 7,
            }),
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 2,
        }];

        let analysis = analyze_return_paths(&body);
        assert!(!analysis.has_value_returns, "Should have no value returns");
        assert!(!analysis.has_none_returns, "Should have no explicit None returns");
        assert!(analysis.has_implicit_return, "Should have implicit return");
        assert!(analysis.should_infer_none(), "Should infer None return type");
    }

    #[test]
    fn test_return_path_analysis_explicit_value() {
        let body = vec![AstNode::Return {
            value: Some(Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                end_line: 1,
                col: 12,
                end_col: 12,
            })),
            line: 1,
            end_line: 1,
            col: 5,
            end_col: 5,
        }];

        let analysis = analyze_return_paths(&body);
        assert!(analysis.has_value_returns, "Should have value returns");
        assert!(!analysis.has_none_returns, "Should have no explicit None returns");
        assert!(!analysis.has_implicit_return, "Should not have implicit return (always exits)");
        assert!(!analysis.should_infer_none(), "Should not infer None");
    }

    #[test]
    fn test_return_path_analysis_mixed_returns() {
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier {
                name: "condition".to_string(),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 13,
            }),
            body: vec![AstNode::Return {
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(42),
                    line: 2,
                    end_line: 2,
                    col: 16,
                    end_col: 16,
                })),
                line: 2,
                end_line: 2,
                col: 9,
                end_col: 9,
            }],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];

        let analysis = analyze_return_paths(&body);
        assert!(analysis.has_value_returns, "Should have value returns");
        assert!(!analysis.has_none_returns, "Should have no explicit None returns");
        assert!(analysis.has_implicit_return, "Should have implicit return (if might not execute)");
        assert!(analysis.should_infer_optional(), "Should infer Optional[T] for mixed returns");
    }

    #[test]
    fn test_return_path_analysis_all_paths_return() {
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier {
                name: "condition".to_string(),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 13,
            }),
            body: vec![AstNode::Return {
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 2,
                    end_line: 2,
                    col: 16,
                    end_col: 16,
                })),
                line: 2,
                end_line: 2,
                col: 9,
                end_col: 9,
            }],
            elif_parts: vec![],
            else_body: Some(vec![AstNode::Return {
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(2),
                    line: 4,
                    end_line: 4,
                    col: 16,
                    end_col: 16,
                })),
                line: 4,
                end_line: 4,
                col: 9,
                end_col: 9,
            }]),
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        let analysis = analyze_return_paths(&body);
        assert!(analysis.has_value_returns, "Should have value returns");
        assert!(!analysis.has_none_returns, "Should have no explicit None returns");
        assert!(!analysis.has_implicit_return, "Should not have implicit return (all paths exit)");
        assert!(!analysis.should_infer_optional(), "Should not infer Optional when all paths return values");
    }
}
