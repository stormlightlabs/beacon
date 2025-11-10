//! Common utility functions for the AST walker
//!
//! This module provides helper functions used across the walker implementation
//! for tasks like detecting docstrings, extracting node positions, and converting
//! type names to Type representations.

use beacon_core::{Type, TypeCtor, TypeVarGen};
use beacon_parser::{AstNode, LiteralValue};

/// Check if a statement is a docstring (string literal expression)
///
/// Docstrings are standalone string literals that appear as expression statements.
/// They are metadata and should not generate type constraints.
pub fn is_docstring(node: &AstNode) -> bool {
    matches!(node, AstNode::Literal { value: LiteralValue::String { .. }, .. })
}

/// Get the line and column position from any AstNode.
///
/// Extracts the source position from the node for error reporting and span tracking.
pub fn get_node_position(node: &AstNode) -> (usize, usize, usize, usize) {
    match node {
        AstNode::Module { .. } => (1, 1, 1, 1),
        AstNode::FunctionDef { line, col, end_line, end_col, .. }
        | AstNode::ClassDef { line, col, end_line, end_col, .. }
        | AstNode::If { line, col, end_line, end_col, .. }
        | AstNode::For { line, col, end_line, end_col, .. }
        | AstNode::While { line, col, end_line, end_col, .. }
        | AstNode::Try { line, col, end_line, end_col, .. }
        | AstNode::With { line, col, end_line, end_col, .. }
        | AstNode::Match { line, col, end_line, end_col, .. }
        | AstNode::Assignment { line, col, end_line, end_col, .. }
        | AstNode::AnnotatedAssignment { line, col, end_line, end_col, .. }
        | AstNode::Call { line, col, end_line, end_col, .. }
        | AstNode::Identifier { line, col, end_line, end_col, .. }
        | AstNode::Literal { line, col, end_line, end_col, .. }
        | AstNode::Return { line, col, end_line, end_col, .. }
        | AstNode::BinaryOp { line, col, end_line, end_col, .. }
        | AstNode::UnaryOp { line, col, end_line, end_col, .. }
        | AstNode::Compare { line, col, end_line, end_col, .. }
        | AstNode::Attribute { line, col, end_line, end_col, .. }
        | AstNode::Subscript { line, col, end_line, end_col, .. }
        | AstNode::List { line, col, end_line, end_col, .. }
        | AstNode::Tuple { line, col, end_line, end_col, .. }
        | AstNode::Set { line, col, end_line, end_col, .. }
        | AstNode::Dict { line, col, end_line, end_col, .. }
        | AstNode::ListComp { line, col, end_line, end_col, .. }
        | AstNode::DictComp { line, col, end_line, end_col, .. }
        | AstNode::SetComp { line, col, end_line, end_col, .. }
        | AstNode::GeneratorExp { line, col, end_line, end_col, .. }
        | AstNode::NamedExpr { line, col, end_line, end_col, .. }
        | AstNode::Lambda { line, col, end_line, end_col, .. }
        | AstNode::Yield { line, col, end_line, end_col, .. }
        | AstNode::YieldFrom { line, col, end_line, end_col, .. }
        | AstNode::Await { line, col, end_line, end_col, .. }
        | AstNode::Import { line, col, end_line, end_col, .. }
        | AstNode::ImportFrom { line, col, end_line, end_col, .. }
        | AstNode::Raise { line, col, end_line, end_col, .. }
        | AstNode::Pass { line, col, end_line, end_col, .. }
        | AstNode::Break { line, col, end_line, end_col, .. }
        | AstNode::Continue { line, col, end_line, end_col, .. }
        | AstNode::Assert { line, col, end_line, end_col, .. }
        | AstNode::Starred { line, col, end_line, end_col, .. }
        | AstNode::ParenthesizedExpression { line, col, end_line, end_col, .. } => (*line, *col, *end_line, *end_col),
    }
}

/// Convert a type name string to a Type
///
/// Maps common Python type names (int, str, float, bool, list, dict, set, tuple)
/// to their corresponding Type representations. Unknown types get a fresh type variable.
pub fn type_name_to_type(name: &str) -> Type {
    match name {
        "int" => Type::int(),
        "str" => Type::string(),
        "float" => Type::float(),
        "bool" => Type::bool(),
        "list" => Type::Con(TypeCtor::List),
        "dict" => Type::Con(TypeCtor::Dict),
        "set" => Type::Con(TypeCtor::Set),
        "tuple" => Type::Con(TypeCtor::Tuple),
        _ => Type::Var(TypeVarGen::new().fresh()),
    }
}

/// Detect if this is the `if __name__ == "__main__":` idiom
///
/// This Python idiom checks if the script is being run directly.
/// The body should always be treated as void context (like module-level code).
pub fn is_main_guard(test: &AstNode) -> bool {
    if let AstNode::Compare { left, ops, comparators, .. } = test {
        if ops.len() == 1 && comparators.len() == 1 {
            let is_name_left = matches!(
                left.as_ref(),
                AstNode::Identifier { name, .. } if name == "__name__"
            );
            let is_main_right = matches!(
                &comparators[0],
                AstNode::Literal { value: LiteralValue::String { value, .. }, .. } if value == "__main__"
            );

            let is_main_left = matches!(
                left.as_ref(),
                AstNode::Literal { value: LiteralValue::String { value, .. }, .. } if value == "__main__"
            );
            let is_name_right = matches!(
                &comparators[0],
                AstNode::Identifier { name, .. } if name == "__name__"
            );

            let is_eq = matches!(ops[0], beacon_parser::CompareOperator::Eq);

            is_eq && ((is_name_left && is_main_right) || (is_main_left && is_name_right))
        } else {
            false
        }
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::LiteralValue;

    #[test]
    fn test_is_docstring() {
        let docstring = AstNode::Literal {
            value: LiteralValue::String {
                value: "This is a docstring".to_string(),
                prefix: String::new(),
            },
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 20,
        };
        assert!(is_docstring(&docstring));

        let number = AstNode::Literal {
            value: LiteralValue::Integer(42),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert!(!is_docstring(&number));
    }

    #[test]
    fn test_type_name_to_type() {
        assert!(matches!(type_name_to_type("int"), Type::Con(TypeCtor::Int)));
        assert!(matches!(type_name_to_type("str"), Type::Con(TypeCtor::String)));
        assert!(matches!(type_name_to_type("float"), Type::Con(TypeCtor::Float)));
        assert!(matches!(type_name_to_type("bool"), Type::Con(TypeCtor::Bool)));
        assert!(matches!(type_name_to_type("list"), Type::Con(TypeCtor::List)));
        assert!(matches!(type_name_to_type("dict"), Type::Con(TypeCtor::Dict)));
        assert!(matches!(type_name_to_type("set"), Type::Con(TypeCtor::Set)));
        assert!(matches!(type_name_to_type("tuple"), Type::Con(TypeCtor::Tuple)));
        assert!(matches!(type_name_to_type("unknown"), Type::Var(_)));
    }

    #[test]
    fn test_is_main_guard_detection() {
        let test1 = AstNode::Compare {
            left: Box::new(AstNode::Identifier {
                name: "__name__".to_string(),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 12,
            }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::String {
                    value: "__main__".to_string(),
                    prefix: String::new(),
                },
                line: 1,
                end_line: 1,
                col: 15,
                end_col: 15,
            }],
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 4,
        };
        assert!(is_main_guard(&test1), "Should detect __name__ == \"__main__\"");

        let test2 = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String {
                    value: "__main__".to_string(),
                    prefix: String::new(),
                },
                line: 1,
                end_line: 1,
                col: 4,
                end_col: 4,
            }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Identifier {
                name: "__name__".to_string(),
                line: 1,
                col: 18,
                end_line: 1,
                end_col: 26,
            }],
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 4,
        };
        assert!(is_main_guard(&test2), "Should detect \"__main__\" == __name__");

        let test3 = AstNode::Compare {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 4, end_line: 1, end_col: 5 }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                col: 9,
                end_line: 1,
                end_col: 11,
            }],
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 4,
        };
        assert!(!is_main_guard(&test3), "Should not detect regular comparison");
    }
}
