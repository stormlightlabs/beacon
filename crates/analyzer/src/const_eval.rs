use beacon_parser::{AstNode, BinaryOperator, LiteralValue, UnaryOperator};
use std::hash::{Hash, Hasher};

/// Constant value that can be evaluated from AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    None,
    Tuple(Vec<ConstValue>),
    List(Vec<ConstValue>),
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
            ConstValue::Tuple(elements) => {
                4u8.hash(state);
                elements.hash(state);
            }
            ConstValue::List(elements) => {
                5u8.hash(state);
                elements.hash(state);
            }
        }
    }
}

/// Evaluates an AST node to a constant value if possible.
///
/// Supports:
/// - Literals (strings, integers, floats, booleans, None)
/// - Unary operations (+x, -x, not x, ~x)
/// - Binary arithmetic operations (+, -, *, /, //, %, **)
/// - String concatenation and repetition
/// - Tuple and list literals
pub fn evaluate_const_expr(node: &AstNode) -> Option<ConstValue> {
    match node {
        AstNode::Literal { value, .. } => match value {
            LiteralValue::String { value, .. } => Some(ConstValue::String(value.clone())),
            LiteralValue::Integer(i) => Some(ConstValue::Integer(*i)),
            LiteralValue::Float(f) => Some(ConstValue::Float(*f)),
            LiteralValue::Boolean(b) => Some(ConstValue::Boolean(*b)),
            LiteralValue::None => Some(ConstValue::None),
        },
        AstNode::UnaryOp { op, operand, .. } => evaluate_unary_op(op, operand),
        AstNode::BinaryOp { left, op, right, .. } => evaluate_binary_op(left, op, right),
        AstNode::Tuple { elements, .. } => {
            let values: Option<Vec<_>> = elements.iter().map(evaluate_const_expr).collect();
            values.map(ConstValue::Tuple)
        }
        AstNode::List { elements, .. } => {
            let values: Option<Vec<_>> = elements.iter().map(evaluate_const_expr).collect();
            values.map(ConstValue::List)
        }
        _ => None,
    }
}

/// Evaluates a unary operation on a constant value.
fn evaluate_unary_op(op: &UnaryOperator, operand: &AstNode) -> Option<ConstValue> {
    let value = evaluate_const_expr(operand)?;
    match (op, value) {
        (&UnaryOperator::Plus, ConstValue::Integer(i)) => Some(ConstValue::Integer(i)),
        (&UnaryOperator::Plus, ConstValue::Float(f)) => Some(ConstValue::Float(f)),
        (&UnaryOperator::Minus, ConstValue::Integer(i)) => Some(ConstValue::Integer(-i)),
        (&UnaryOperator::Minus, ConstValue::Float(f)) => Some(ConstValue::Float(-f)),
        (&UnaryOperator::Not, ConstValue::Boolean(b)) => Some(ConstValue::Boolean(!b)),
        (&UnaryOperator::Not, ConstValue::None) => Some(ConstValue::Boolean(true)),
        (&UnaryOperator::Not, ConstValue::Integer(0)) => Some(ConstValue::Boolean(true)),
        (&UnaryOperator::Not, ConstValue::Integer(_)) => Some(ConstValue::Boolean(false)),
        (&UnaryOperator::Not, ConstValue::String(ref s)) if s.is_empty() => Some(ConstValue::Boolean(true)),
        (&UnaryOperator::Not, ConstValue::String(_)) => Some(ConstValue::Boolean(false)),
        (&UnaryOperator::Not, ConstValue::Float(0.0)) => Some(ConstValue::Boolean(true)),
        (&UnaryOperator::Not, ConstValue::Float(_)) => Some(ConstValue::Boolean(false)),
        (&UnaryOperator::Not, ConstValue::Tuple(ref elements)) if elements.is_empty() => {
            Some(ConstValue::Boolean(true))
        }
        (&UnaryOperator::Not, ConstValue::Tuple(_)) => Some(ConstValue::Boolean(false)),
        (&UnaryOperator::Not, ConstValue::List(ref elements)) if elements.is_empty() => Some(ConstValue::Boolean(true)),
        (&UnaryOperator::Not, ConstValue::List(_)) => Some(ConstValue::Boolean(false)),
        (&UnaryOperator::Invert, ConstValue::Integer(i)) => Some(ConstValue::Integer(!i)),
        _ => None,
    }
}

/// Evaluates a binary operation on two constant values.
fn evaluate_binary_op(left: &AstNode, op: &BinaryOperator, right: &AstNode) -> Option<ConstValue> {
    let left_val = evaluate_const_expr(left)?;
    let right_val = evaluate_const_expr(right)?;

    match (left_val, op, right_val) {
        (ConstValue::Integer(l), &BinaryOperator::Add, ConstValue::Integer(r)) => {
            Some(ConstValue::Integer(l.checked_add(r)?))
        }
        (ConstValue::Integer(l), &BinaryOperator::Sub, ConstValue::Integer(r)) => {
            Some(ConstValue::Integer(l.checked_sub(r)?))
        }
        (ConstValue::Integer(l), &BinaryOperator::Mult, ConstValue::Integer(r)) => {
            Some(ConstValue::Integer(l.checked_mul(r)?))
        }
        (ConstValue::Integer(l), &BinaryOperator::Div, ConstValue::Integer(r)) if r != 0 => {
            Some(ConstValue::Float(l as f64 / r as f64))
        }
        (ConstValue::Integer(l), &BinaryOperator::FloorDiv, ConstValue::Integer(r)) if r != 0 => {
            Some(ConstValue::Integer(l.checked_div(r)?))
        }
        (ConstValue::Integer(l), &BinaryOperator::Mod, ConstValue::Integer(r)) if r != 0 => {
            Some(ConstValue::Integer(l.checked_rem(r)?))
        }
        (ConstValue::Integer(l), &BinaryOperator::Pow, ConstValue::Integer(r)) if r >= 0 => {
            let exp = r as u32;
            Some(ConstValue::Integer(l.checked_pow(exp)?))
        }
        (ConstValue::Float(l), &BinaryOperator::Add, ConstValue::Float(r)) => Some(ConstValue::Float(l + r)),
        (ConstValue::Float(l), &BinaryOperator::Sub, ConstValue::Float(r)) => Some(ConstValue::Float(l - r)),
        (ConstValue::Float(l), &BinaryOperator::Mult, ConstValue::Float(r)) => Some(ConstValue::Float(l * r)),
        (ConstValue::Float(l), &BinaryOperator::Div, ConstValue::Float(r)) => Some(ConstValue::Float(l / r)),
        (ConstValue::Float(l), &BinaryOperator::FloorDiv, ConstValue::Float(r)) => {
            Some(ConstValue::Float((l / r).floor()))
        }
        (ConstValue::Float(l), &BinaryOperator::Mod, ConstValue::Float(r)) => Some(ConstValue::Float(l % r)),
        (ConstValue::Float(l), &BinaryOperator::Pow, ConstValue::Float(r)) => Some(ConstValue::Float(l.powf(r))),
        (ConstValue::Integer(l), &BinaryOperator::Add, ConstValue::Float(r)) => Some(ConstValue::Float(l as f64 + r)),
        (ConstValue::Float(l), &BinaryOperator::Add, ConstValue::Integer(r)) => Some(ConstValue::Float(l + r as f64)),
        (ConstValue::Integer(l), &BinaryOperator::Sub, ConstValue::Float(r)) => Some(ConstValue::Float(l as f64 - r)),
        (ConstValue::Float(l), &BinaryOperator::Sub, ConstValue::Integer(r)) => Some(ConstValue::Float(l - r as f64)),
        (ConstValue::Integer(l), &BinaryOperator::Mult, ConstValue::Float(r)) => Some(ConstValue::Float(l as f64 * r)),
        (ConstValue::Float(l), &BinaryOperator::Mult, ConstValue::Integer(r)) => Some(ConstValue::Float(l * r as f64)),
        (ConstValue::Integer(l), &BinaryOperator::Div, ConstValue::Float(r)) => Some(ConstValue::Float(l as f64 / r)),
        (ConstValue::Float(l), &BinaryOperator::Div, ConstValue::Integer(r)) => Some(ConstValue::Float(l / r as f64)),
        (ConstValue::Integer(l), &BinaryOperator::FloorDiv, ConstValue::Float(r)) => {
            Some(ConstValue::Float((l as f64 / r).floor()))
        }
        (ConstValue::Float(l), &BinaryOperator::FloorDiv, ConstValue::Integer(r)) => {
            Some(ConstValue::Float((l / r as f64).floor()))
        }
        (ConstValue::Integer(l), &BinaryOperator::Mod, ConstValue::Float(r)) => Some(ConstValue::Float(l as f64 % r)),
        (ConstValue::Float(l), &BinaryOperator::Mod, ConstValue::Integer(r)) => Some(ConstValue::Float(l % r as f64)),
        (ConstValue::Integer(l), &BinaryOperator::Pow, ConstValue::Float(r)) => {
            Some(ConstValue::Float((l as f64).powf(r)))
        }
        (ConstValue::Float(l), &BinaryOperator::Pow, ConstValue::Integer(r)) => {
            Some(ConstValue::Float(l.powf(r as f64)))
        }

        (ConstValue::String(l), &BinaryOperator::Add, ConstValue::String(r)) => {
            Some(ConstValue::String(format!("{l}{r}")))
        }
        (ConstValue::String(s), &BinaryOperator::Mult, ConstValue::Integer(n))
        | (ConstValue::Integer(n), &BinaryOperator::Mult, ConstValue::String(s)) => {
            if n < 0 || n > 10000 {
                None
            } else {
                Some(ConstValue::String(s.repeat(n as usize)))
            }
        }
        (ConstValue::Integer(l), &BinaryOperator::BitAnd, ConstValue::Integer(r)) => Some(ConstValue::Integer(l & r)),
        (ConstValue::Integer(l), &BinaryOperator::BitOr, ConstValue::Integer(r)) => Some(ConstValue::Integer(l | r)),
        (ConstValue::Integer(l), &BinaryOperator::BitXor, ConstValue::Integer(r)) => Some(ConstValue::Integer(l ^ r)),
        (ConstValue::Integer(l), &BinaryOperator::LeftShift, ConstValue::Integer(r)) if r >= 0 && r < 64 => {
            Some(ConstValue::Integer(l.checked_shl(r as u32)?))
        }
        (ConstValue::Integer(l), &BinaryOperator::RightShift, ConstValue::Integer(r)) if r >= 0 && r < 64 => {
            Some(ConstValue::Integer(l.checked_shr(r as u32)?))
        }
        (ConstValue::Boolean(l), &BinaryOperator::And, ConstValue::Boolean(r)) => Some(ConstValue::Boolean(l && r)),
        (ConstValue::Boolean(l), &BinaryOperator::Or, ConstValue::Boolean(r)) => Some(ConstValue::Boolean(l || r)),

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
    fn test_evaluate_const_expr_identifiers() {
        let ident = AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 };
        assert_eq!(evaluate_const_expr(&ident), None);
    }

    #[test]
    fn test_unary_plus() {
        let node = AstNode::UnaryOp {
            op: UnaryOperator::Plus,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(42)));

        let node = AstNode::UnaryOp {
            op: UnaryOperator::Plus,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::Float(2.5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Float(2.5)));
    }

    #[test]
    fn test_unary_minus() {
        let node = AstNode::UnaryOp {
            op: UnaryOperator::Minus,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(-42)));

        let node = AstNode::UnaryOp {
            op: UnaryOperator::Minus,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::Float(2.5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Float(-2.5)));
    }

    #[test]
    fn test_unary_not() {
        let node = AstNode::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::Boolean(true),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));

        let node = AstNode::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(AstNode::Literal { value: LiteralValue::None, line: 1, col: 1, end_line: 1, end_col: 2 }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(0),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));

        let node = AstNode::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: String::new(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "hello".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_unary_invert() {
        let node = AstNode::UnaryOp {
            op: UnaryOperator::Invert,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(!42)));

        let node = AstNode::UnaryOp {
            op: UnaryOperator::Invert,
            operand: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(-1),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(0)));
    }

    #[test]
    fn test_integer_addition() {
        let node = AstNode::BinaryOp {
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(3)));
    }

    #[test]
    fn test_integer_subtraction() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(10),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Sub,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(3),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(7)));
    }

    #[test]
    fn test_integer_multiplication() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(6),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Mult,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(7),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(42)));
    }

    #[test]
    fn test_integer_division() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(7),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Div,
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Float(3.5)));
    }

    #[test]
    fn test_integer_floor_division() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(7),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::FloorDiv,
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(3)));
    }

    #[test]
    fn test_integer_modulo() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(10),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Mod,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(3),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(1)));
    }

    #[test]
    fn test_integer_power() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(2),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Pow,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(10),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(1024)));
    }

    #[test]
    fn test_division_by_zero() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(10),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Div,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(0),
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
        assert_eq!(evaluate_const_expr(&node), None);
    }

    #[test]
    fn test_float_arithmetic() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Float(2.5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Add,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Float(3.5),
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
        let result = evaluate_const_expr(&node);
        assert!(matches!(result, Some(ConstValue::Float(f)) if (f - 6.0).abs() < 0.01));
    }

    #[test]
    fn test_mixed_int_float_arithmetic() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Add,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Float(2.5),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Float(7.5)));
    }

    #[test]
    fn test_string_concatenation() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "hello".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Add,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: " world".to_string(), prefix: String::new() },
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
        assert_eq!(
            evaluate_const_expr(&node),
            Some(ConstValue::String("hello world".to_string()))
        );
    }

    #[test]
    fn test_string_repetition() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "ab".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Mult,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(3),
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
        assert_eq!(
            evaluate_const_expr(&node),
            Some(ConstValue::String("ababab".to_string()))
        );

        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(3),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Mult,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "ab".to_string(), prefix: String::new() },
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
        assert_eq!(
            evaluate_const_expr(&node),
            Some(ConstValue::String("ababab".to_string()))
        );
    }

    #[test]
    fn test_string_repetition_negative() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "ab".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Mult,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(-1),
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
        assert_eq!(evaluate_const_expr(&node), None);
    }

    #[test]
    fn test_bitwise_and() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(0b1100),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::BitAnd,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(0b1010),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(0b1000)));
    }

    #[test]
    fn test_bitwise_or() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(0b1100),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::BitOr,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(0b1010),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(0b1110)));
    }

    #[test]
    fn test_bitwise_xor() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(0b1100),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::BitXor,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(0b1010),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(0b0110)));
    }

    #[test]
    fn test_bitwise_shift() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(1),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::LeftShift,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(3),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(8)));

        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(16),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::RightShift,
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(4)));
    }

    #[test]
    fn test_boolean_operations() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Boolean(true),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::And,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Boolean(false),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));

        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Boolean(true),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Or,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Boolean(false),
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
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_tuple_literal() {
        let node = AstNode::Tuple {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 4, end_line: 1, end_col: 5 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 7, end_line: 1, end_col: 8 },
            ],
            is_parenthesized: true,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 9,
        };
        assert_eq!(
            evaluate_const_expr(&node),
            Some(ConstValue::Tuple(vec![
                ConstValue::Integer(1),
                ConstValue::Integer(2),
                ConstValue::Integer(3)
            ]))
        );
    }

    #[test]
    fn test_list_literal() {
        let node = AstNode::List {
            elements: vec![
                AstNode::Literal {
                    value: LiteralValue::String { value: "a".to_string(), prefix: String::new() },
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
                AstNode::Literal {
                    value: LiteralValue::String { value: "b".to_string(), prefix: String::new() },
                    line: 1,
                    col: 4,
                    end_line: 1,
                    end_col: 5,
                },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 6,
        };
        assert_eq!(
            evaluate_const_expr(&node),
            Some(ConstValue::List(vec![
                ConstValue::String("a".to_string()),
                ConstValue::String("b".to_string())
            ]))
        );
    }

    #[test]
    fn test_empty_tuple() {
        let node =
            AstNode::Tuple { elements: vec![], is_parenthesized: true, line: 1, col: 1, end_line: 1, end_col: 2 };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Tuple(vec![])));
    }

    #[test]
    fn test_nested_tuple() {
        let inner_tuple = AstNode::Tuple {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 4, end_line: 1, end_col: 5 },
            ],
            is_parenthesized: true,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 6,
        };

        let node = AstNode::Tuple {
            elements: vec![
                inner_tuple,
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 8, end_line: 1, end_col: 9 },
            ],
            is_parenthesized: true,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 10,
        };

        assert_eq!(
            evaluate_const_expr(&node),
            Some(ConstValue::Tuple(vec![
                ConstValue::Tuple(vec![ConstValue::Integer(1), ConstValue::Integer(2)]),
                ConstValue::Integer(3)
            ]))
        );
    }

    #[test]
    fn test_overflow_handling() {
        let node = AstNode::BinaryOp {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(i64::MAX),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            op: BinaryOperator::Add,
            right: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(1),
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
        assert_eq!(evaluate_const_expr(&node), None);
    }

    #[test]
    fn test_nan_handling() {
        use std::hash::Hasher;

        let nan1 = ConstValue::Float(f64::NAN);
        let nan2 = ConstValue::Float(f64::NAN);

        assert_ne!(nan1, nan2);

        let mut hasher1 = std::collections::hash_map::DefaultHasher::new();
        let mut hasher2 = std::collections::hash_map::DefaultHasher::new();
        nan1.hash(&mut hasher1);
        nan2.hash(&mut hasher2);
        assert_eq!(hasher1.finish(), hasher2.finish());

        let mut set = HashSet::new();
        set.insert(ConstValue::Float(f64::NAN));
        set.insert(ConstValue::Float(f64::NAN));
        assert_eq!(set.len(), 2);
    }

    #[test]
    fn test_complex_expression() {
        let inner = AstNode::BinaryOp {
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

        let node = AstNode::UnaryOp {
            op: UnaryOperator::Minus,
            operand: Box::new(inner),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 6,
        };

        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Integer(-3)));
    }

    #[test]
    fn test_tuple_hash_deduplication() {
        let mut set = HashSet::new();
        let tuple1 = ConstValue::Tuple(vec![ConstValue::Integer(1), ConstValue::Integer(2)]);
        let tuple2 = ConstValue::Tuple(vec![ConstValue::Integer(1), ConstValue::Integer(2)]);

        set.insert(tuple1);
        set.insert(tuple2);
        assert_eq!(set.len(), 1);
    }
}
