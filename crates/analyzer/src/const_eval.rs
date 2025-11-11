use beacon_parser::{AstNode, LiteralValue};
use std::hash::{Hash, Hasher};

/// Constant value that can be evaluated from AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    None,
}

impl Eq for ConstValue {}

/// Custom hash implementation for ConstValue.
/// Follows Python's hashing rules where True == 1 and False == 0 for dict keys.
impl Hash for ConstValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ConstValue::String(s) => {
                0u8.hash(state);
                s.hash(state);
            }
            ConstValue::Integer(i) => {
                1u8.hash(state);
                i.hash(state);
            }
            ConstValue::Float(f) => {
                2u8.hash(state);
                f.to_bits().hash(state);
            }
            ConstValue::Boolean(b) => {
                1u8.hash(state);
                (*b as i64).hash(state);
            }
            ConstValue::None => {
                3u8.hash(state);
            }
        }
    }
}

/// Evaluates an AST node to a constant value if possible.
pub fn evaluate_const_expr(node: &AstNode) -> Option<ConstValue> {
    match node {
        AstNode::Literal { value, .. } => match value {
            LiteralValue::String { value, .. } => Some(ConstValue::String(value.clone())),
            LiteralValue::Integer(i) => Some(ConstValue::Integer(*i)),
            LiteralValue::Float(f) => Some(ConstValue::Float(*f)),
            LiteralValue::Boolean(b) => Some(ConstValue::Boolean(*b)),
            LiteralValue::None => Some(ConstValue::None),
        },
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use beacon_parser::BinaryOperator;
    use std::collections::HashSet;

    #[test]
    fn test_const_value_equality() {
        assert_eq!(
            ConstValue::String("test".to_string()),
            ConstValue::String("test".to_string())
        );
        assert_ne!(
            ConstValue::String("test".to_string()),
            ConstValue::String("other".to_string())
        );

        assert_eq!(ConstValue::Integer(42), ConstValue::Integer(42));
        assert_ne!(ConstValue::Integer(42), ConstValue::Integer(43));

        assert_eq!(ConstValue::Boolean(true), ConstValue::Boolean(true));
        assert_ne!(ConstValue::Boolean(true), ConstValue::Boolean(false));

        assert_eq!(ConstValue::None, ConstValue::None);

        assert_ne!(ConstValue::Integer(1), ConstValue::String("1".to_string()));
        assert_ne!(ConstValue::Boolean(true), ConstValue::String("true".to_string()));
    }

    #[test]
    fn test_const_value_hash_deduplication() {
        let mut set = HashSet::new();

        set.insert(ConstValue::String("key".to_string()));
        set.insert(ConstValue::String("key".to_string()));
        assert_eq!(set.len(), 1);

        set.clear();
        set.insert(ConstValue::Integer(42));
        set.insert(ConstValue::Integer(42));
        assert_eq!(set.len(), 1);

        set.clear();
        set.insert(ConstValue::Boolean(true));
        set.insert(ConstValue::Boolean(true));
        assert_eq!(set.len(), 1);

        set.clear();
        set.insert(ConstValue::None);
        set.insert(ConstValue::None);
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn test_python_bool_int_equivalence() {
        let mut set = HashSet::new();
        set.insert(ConstValue::Boolean(true));
        set.insert(ConstValue::Integer(1));

        assert_eq!(set.len(), 2);
        assert_ne!(ConstValue::Boolean(true), ConstValue::Integer(1));
    }

    #[test]
    fn test_evaluate_const_expr_literals() {
        let string_lit = AstNode::Literal {
            value: LiteralValue::String { value: "test".to_string(), prefix: String::new() },
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 6,
        };
        assert_eq!(
            evaluate_const_expr(&string_lit),
            Some(ConstValue::String("test".to_string()))
        );

        let int_lit = AstNode::Literal { value: LiteralValue::Integer(42), line: 1, col: 1, end_line: 1, end_col: 3 };
        assert_eq!(evaluate_const_expr(&int_lit), Some(ConstValue::Integer(42)));

        let bool_lit =
            AstNode::Literal { value: LiteralValue::Boolean(true), line: 1, col: 1, end_line: 1, end_col: 5 };
        assert_eq!(evaluate_const_expr(&bool_lit), Some(ConstValue::Boolean(true)));

        let none_lit = AstNode::Literal { value: LiteralValue::None, line: 1, col: 1, end_line: 1, end_col: 5 };
        assert_eq!(evaluate_const_expr(&none_lit), Some(ConstValue::None));
    }

    #[test]
    fn test_evaluate_const_expr_non_literals() {
        let ident = AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 };
        assert_eq!(evaluate_const_expr(&ident), None);

        let binop = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(1),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Add,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(2),
                line: 1,
                col: 5,
                end_line: 1,
                end_col: 6,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 6,
        };
        assert_eq!(evaluate_const_expr(&binop), None);
    }
}
