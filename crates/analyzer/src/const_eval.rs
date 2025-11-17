use beacon_parser::{AstNode, BinaryOperator, CompareOperator, LiteralValue, UnaryOperator};
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
    Dict(Vec<(ConstValue, ConstValue)>),
    Set(Vec<ConstValue>),
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
            ConstValue::Dict(items) => {
                6u8.hash(state);
                items.hash(state);
            }
            ConstValue::Set(elements) => {
                7u8.hash(state);
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
/// - Comparison operations (==, !=, <, <=, >, >=, is, is not, in, not in)
/// - String concatenation and repetition
/// - Tuple, list, dict, and set literals
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
        AstNode::Compare { left, ops, comparators, .. } => evaluate_compare(left, ops, comparators),
        AstNode::Tuple { elements, .. } => {
            let values: Option<Vec<_>> = elements.iter().map(evaluate_const_expr).collect();
            values.map(ConstValue::Tuple)
        }
        AstNode::List { elements, .. } => {
            let values: Option<Vec<_>> = elements.iter().map(evaluate_const_expr).collect();
            values.map(ConstValue::List)
        }
        AstNode::Dict { keys, values, .. } => {
            let key_values: Option<Vec<_>> = keys.iter().map(evaluate_const_expr).collect();
            let value_values: Option<Vec<_>> = values.iter().map(evaluate_const_expr).collect();
            Some(ConstValue::Dict(key_values?.into_iter().zip(value_values?).collect()))
        }
        AstNode::Set { elements, .. } => {
            let values: Option<Vec<_>> = elements.iter().map(evaluate_const_expr).collect();
            values.map(|mut v| {
                let mut seen = std::collections::HashSet::new();
                v.retain(|item| seen.insert(item.clone()));
                ConstValue::Set(v)
            })
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
        (&UnaryOperator::Not, ConstValue::Dict(ref items)) if items.is_empty() => Some(ConstValue::Boolean(true)),
        (&UnaryOperator::Not, ConstValue::Dict(_)) => Some(ConstValue::Boolean(false)),
        (&UnaryOperator::Not, ConstValue::Set(ref elements)) if elements.is_empty() => Some(ConstValue::Boolean(true)),
        (&UnaryOperator::Not, ConstValue::Set(_)) => Some(ConstValue::Boolean(false)),
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

/// Evaluates a comparison expression (potentially chained like `a < b < c`).
///
/// Python allows comparison chains like `1 < 2 < 3`, which is equivalent to `1 < 2 and 2 < 3`.
/// Returns a boolean ConstValue representing the result of all comparisons.
fn evaluate_compare(left: &AstNode, ops: &[CompareOperator], comparators: &[AstNode]) -> Option<ConstValue> {
    if ops.len() != comparators.len() {
        return None;
    }

    let mut current = evaluate_const_expr(left)?;

    for (op, comparator) in ops.iter().zip(comparators.iter()) {
        let right = evaluate_const_expr(comparator)?;
        let result = evaluate_single_compare(&current, op, &right)?;

        if !result {
            return Some(ConstValue::Boolean(false));
        }

        current = right;
    }

    Some(ConstValue::Boolean(true))
}

/// Performs a single comparison between two constant values.
fn evaluate_single_compare(left: &ConstValue, op: &CompareOperator, right: &ConstValue) -> Option<bool> {
    match op {
        CompareOperator::Eq => Some(values_equal(left, right)),
        CompareOperator::NotEq => Some(!values_equal(left, right)),
        CompareOperator::Lt => compare_values(left, right).map(|ord| ord == std::cmp::Ordering::Less),
        CompareOperator::LtE => compare_values(left, right).map(|ord| ord != std::cmp::Ordering::Greater),
        CompareOperator::Gt => compare_values(left, right).map(|ord| ord == std::cmp::Ordering::Greater),
        CompareOperator::GtE => compare_values(left, right).map(|ord| ord != std::cmp::Ordering::Less),
        CompareOperator::Is => Some(values_identical(left, right)),
        CompareOperator::IsNot => Some(!values_identical(left, right)),
        CompareOperator::In => Some(value_in_container(left, right)),
        CompareOperator::NotIn => Some(!value_in_container(left, right)),
    }
}

/// Checks if two values are equal (Python `==` semantics).
fn values_equal(left: &ConstValue, right: &ConstValue) -> bool {
    match (left, right) {
        (ConstValue::Integer(l), ConstValue::Integer(r)) => l == r,
        (ConstValue::Float(l), ConstValue::Float(r)) => l == r,
        (ConstValue::String(l), ConstValue::String(r)) => l == r,
        (ConstValue::Boolean(l), ConstValue::Boolean(r)) => l == r,
        (ConstValue::None, ConstValue::None) => true,
        (ConstValue::Tuple(l), ConstValue::Tuple(r)) => l == r,
        (ConstValue::List(l), ConstValue::List(r)) => l == r,
        (ConstValue::Dict(l), ConstValue::Dict(r)) => {
            if l.len() != r.len() {
                return false;
            }
            l.iter()
                .all(|(k, v)| r.iter().any(|(rk, rv)| values_equal(k, rk) && values_equal(v, rv)))
        }
        (ConstValue::Set(l), ConstValue::Set(r)) => {
            if l.len() != r.len() {
                return false;
            }
            l.iter().all(|item| r.iter().any(|ritem| values_equal(item, ritem)))
        }
        (ConstValue::Integer(l), ConstValue::Float(r)) => (*l as f64) == *r,
        (ConstValue::Float(l), ConstValue::Integer(r)) => *l == (*r as f64),
        (ConstValue::Boolean(true), ConstValue::Integer(1)) => true,
        (ConstValue::Boolean(false), ConstValue::Integer(0)) => true,
        (ConstValue::Integer(1), ConstValue::Boolean(true)) => true,
        (ConstValue::Integer(0), ConstValue::Boolean(false)) => true,
        (ConstValue::Boolean(b), ConstValue::Float(f)) => (*b as i64 as f64) == *f,
        (ConstValue::Float(f), ConstValue::Boolean(b)) => *f == (*b as i64 as f64),
        _ => false,
    }
}

/// Checks if two values are identical (Python `is` semantics).
///
/// For constant values, `is` behaves like `==` for None, True, False.
/// For other values, we use equality as an approximation since we're dealing with constants.
fn values_identical(left: &ConstValue, right: &ConstValue) -> bool {
    match (left, right) {
        (ConstValue::None, ConstValue::None) => true,
        (ConstValue::Boolean(l), ConstValue::Boolean(r)) => l == r,
        (ConstValue::Integer(l), ConstValue::Integer(r)) if *l >= -5 && *l <= 256 && *r >= -5 && *r <= 256 => l == r,
        _ => false,
    }
}

/// Compares two values for ordering (for <, <=, >, >=).
fn compare_values(left: &ConstValue, right: &ConstValue) -> Option<std::cmp::Ordering> {
    match (left, right) {
        (ConstValue::Integer(l), ConstValue::Integer(r)) => Some(l.cmp(r)),
        (ConstValue::Float(l), ConstValue::Float(r)) => l.partial_cmp(r),
        (ConstValue::String(l), ConstValue::String(r)) => Some(l.cmp(r)),
        (ConstValue::Integer(l), ConstValue::Float(r)) => (*l as f64).partial_cmp(r),
        (ConstValue::Float(l), ConstValue::Integer(r)) => l.partial_cmp(&(*r as f64)),
        (ConstValue::Boolean(l), ConstValue::Boolean(r)) => Some(l.cmp(r)),
        (ConstValue::Boolean(l), ConstValue::Integer(r)) => Some((*l as i64).cmp(r)),
        (ConstValue::Integer(l), ConstValue::Boolean(r)) => Some(l.cmp(&(*r as i64))),
        (ConstValue::Tuple(l), ConstValue::Tuple(r)) => compare_sequences(l, r),
        (ConstValue::List(l), ConstValue::List(r)) => compare_sequences(l, r),
        _ => None,
    }
}

/// Compares two sequences lexicographically.
fn compare_sequences(left: &[ConstValue], right: &[ConstValue]) -> Option<std::cmp::Ordering> {
    for (l, r) in left.iter().zip(right.iter()) {
        match compare_values(l, r)? {
            std::cmp::Ordering::Equal => continue,
            ordering => return Some(ordering),
        }
    }
    Some(left.len().cmp(&right.len()))
}

/// Checks if a value is contained in a container (for `in` operator).
fn value_in_container(value: &ConstValue, container: &ConstValue) -> bool {
    match container {
        ConstValue::Tuple(elements) | ConstValue::List(elements) | ConstValue::Set(elements) => {
            elements.iter().any(|elem| values_equal(value, elem))
        }
        ConstValue::Dict(items) => items.iter().any(|(key, _)| values_equal(value, key)),
        ConstValue::String(s) => {
            if let ConstValue::String(needle) = value {
                s.contains(needle.as_str())
            } else {
                false
            }
        }
        _ => false,
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

    #[test]
    fn test_compare_eq_integers() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(3),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_compare_not_eq() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::NotEq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(3),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::NotEq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_compare_less_than() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(3),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(3),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_compare_less_than_or_equal() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::LtE],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(3),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::LtE],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(7),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::LtE],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_compare_greater_than() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(10),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Gt],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(3),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Gt],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_compare_greater_than_or_equal() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::GtE],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(7),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::GtE],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(3),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::GtE],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_compare_mixed_numeric() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Float(5.0),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(3),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Float(5.5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Float(10.5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Gt],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_compare_boolean_integer() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Boolean(true),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(1),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Boolean(false),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(0),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Boolean(true),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(2),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_compare_strings() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "abc".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::String { value: "abc".to_string(), prefix: String::new() },
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "abc".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::String { value: "xyz".to_string(), prefix: String::new() },
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_compare_is_operator() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal { value: LiteralValue::None, line: 1, col: 1, end_line: 1, end_col: 2 }),
            ops: vec![CompareOperator::Is],
            comparators: vec![AstNode::Literal { value: LiteralValue::None, line: 1, col: 6, end_line: 1, end_col: 7 }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Boolean(true),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Is],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Boolean(true),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Is],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(1000),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Is],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(1000),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_compare_is_not_operator() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::IsNot],
            comparators: vec![AstNode::Literal { value: LiteralValue::None, line: 1, col: 6, end_line: 1, end_col: 7 }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Boolean(true),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::IsNot],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Boolean(false),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_compare_in_operator() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(2),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::In],
            comparators: vec![AstNode::List {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::In],
            comparators: vec![AstNode::List {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "ab".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::In],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::String { value: "abcd".to_string(), prefix: String::new() },
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "xyz".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::In],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::String { value: "abcd".to_string(), prefix: String::new() },
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(2),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::In],
            comparators: vec![AstNode::Tuple {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                is_parenthesized: true,
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_compare_not_in_operator() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::NotIn],
            comparators: vec![AstNode::List {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(2),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::NotIn],
            comparators: vec![AstNode::List {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_compare_chained() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(1),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt, CompareOperator::Lt],
            comparators: vec![
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 5, end_line: 1, end_col: 6 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 9, end_line: 1, end_col: 10 },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 10,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(1),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt, CompareOperator::Lt],
            comparators: vec![
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 5, end_line: 1, end_col: 6 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 9, end_line: 1, end_col: 10 },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 10,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq, CompareOperator::NotEq],
            comparators: vec![
                AstNode::Literal { value: LiteralValue::Integer(5), line: 1, col: 5, end_line: 1, end_col: 6 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 9, end_line: 1, end_col: 10 },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 10,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_compare_tuple_equality() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Tuple {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                is_parenthesized: true,
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Tuple {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                is_parenthesized: true,
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Tuple {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                is_parenthesized: true,
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Tuple {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                is_parenthesized: true,
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_compare_tuple_ordering() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Tuple {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                is_parenthesized: true,
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt],
            comparators: vec![AstNode::Tuple {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                is_parenthesized: true,
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));

        let node = AstNode::Compare {
            left: Box::new(AstNode::Tuple {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                is_parenthesized: true,
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt],
            comparators: vec![AstNode::Tuple {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                is_parenthesized: true,
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_compare_list_ordering() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::List {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt],
            comparators: vec![AstNode::List {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_compare_type_mismatch() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "test".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Lt],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), None);

        let node = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "test".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(5),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 7,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_dict_literal_empty() {
        let node = AstNode::Dict { keys: vec![], values: vec![], line: 1, col: 1, end_line: 1, end_col: 2 };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Dict(vec![])));
    }

    #[test]
    fn test_dict_literal_simple() {
        let node = AstNode::Dict {
            keys: vec![
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
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
            ],
            values: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        let result = evaluate_const_expr(&node);
        assert_eq!(
            result,
            Some(ConstValue::Dict(vec![
                (ConstValue::String("a".to_string()), ConstValue::Integer(1)),
                (ConstValue::String("b".to_string()), ConstValue::Integer(2)),
            ]))
        );
    }

    #[test]
    fn test_dict_literal_mixed_types() {
        let node = AstNode::Dict {
            keys: vec![
                AstNode::Literal {
                    value: LiteralValue::String { value: "str_key".to_string(), prefix: String::new() },
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
                AstNode::Literal { value: LiteralValue::Integer(42), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal { value: LiteralValue::Boolean(true), line: 1, col: 1, end_line: 1, end_col: 2 },
            ],
            values: vec![
                AstNode::Literal {
                    value: LiteralValue::Float(std::f64::consts::PI),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
                AstNode::Literal {
                    value: LiteralValue::String { value: "value".to_string(), prefix: String::new() },
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
                AstNode::Literal { value: LiteralValue::None, line: 1, col: 1, end_line: 1, end_col: 2 },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        let result = evaluate_const_expr(&node);
        assert_eq!(
            result,
            Some(ConstValue::Dict(vec![
                (
                    ConstValue::String("str_key".to_string()),
                    ConstValue::Float(std::f64::consts::PI)
                ),
                (ConstValue::Integer(42), ConstValue::String("value".to_string())),
                (ConstValue::Boolean(true), ConstValue::None),
            ]))
        );
    }

    #[test]
    fn test_dict_literal_with_expressions() {
        let node = AstNode::Dict {
            keys: vec![
                AstNode::UnaryOp {
                    op: UnaryOperator::Minus,
                    operand: Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(1),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
                AstNode::BinaryOp {
                    left: Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(2),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    op: BinaryOperator::Add,
                    right: Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(3),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
            ],
            values: vec![
                AstNode::Literal {
                    value: LiteralValue::String { value: "neg".to_string(), prefix: String::new() },
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
                AstNode::Literal {
                    value: LiteralValue::String { value: "sum".to_string(), prefix: String::new() },
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        let result = evaluate_const_expr(&node);
        assert_eq!(
            result,
            Some(ConstValue::Dict(vec![
                (ConstValue::Integer(-1), ConstValue::String("neg".to_string())),
                (ConstValue::Integer(5), ConstValue::String("sum".to_string())),
            ]))
        );
    }

    #[test]
    fn test_dict_equality_order_independent() {
        let dict1 = ConstValue::Dict(vec![
            (ConstValue::String("a".to_string()), ConstValue::Integer(1)),
            (ConstValue::String("b".to_string()), ConstValue::Integer(2)),
        ]);
        let dict2 = ConstValue::Dict(vec![
            (ConstValue::String("b".to_string()), ConstValue::Integer(2)),
            (ConstValue::String("a".to_string()), ConstValue::Integer(1)),
        ]);
        assert!(values_equal(&dict1, &dict2));
        assert!(values_equal(&dict2, &dict1));
    }

    #[test]
    fn test_dict_inequality() {
        let dict1 = ConstValue::Dict(vec![(ConstValue::String("a".to_string()), ConstValue::Integer(1))]);
        let dict2 = ConstValue::Dict(vec![(ConstValue::String("a".to_string()), ConstValue::Integer(2))]);
        assert!(!values_equal(&dict1, &dict2));

        let dict3 = ConstValue::Dict(vec![(ConstValue::String("a".to_string()), ConstValue::Integer(1))]);
        let dict4 = ConstValue::Dict(vec![(ConstValue::String("b".to_string()), ConstValue::Integer(1))]);
        assert!(!values_equal(&dict3, &dict4));
    }

    #[test]
    fn test_dict_in_operator() {
        let dict = ConstValue::Dict(vec![
            (ConstValue::String("a".to_string()), ConstValue::Integer(1)),
            (ConstValue::String("b".to_string()), ConstValue::Integer(2)),
        ]);
        assert!(value_in_container(&ConstValue::String("a".to_string()), &dict));
        assert!(value_in_container(&ConstValue::String("b".to_string()), &dict));
        assert!(!value_in_container(&ConstValue::String("c".to_string()), &dict));
        assert!(!value_in_container(&ConstValue::Integer(1), &dict));
    }

    #[test]
    fn test_dict_not_operator() {
        let empty_dict = AstNode::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(AstNode::Dict { keys: vec![], values: vec![], line: 1, col: 1, end_line: 1, end_col: 2 }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&empty_dict), Some(ConstValue::Boolean(true)));

        let non_empty_dict = AstNode::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(AstNode::Dict {
                keys: vec![AstNode::Literal {
                    value: LiteralValue::String { value: "a".to_string(), prefix: String::new() },
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }],
                values: vec![AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }],
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
        assert_eq!(evaluate_const_expr(&non_empty_dict), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_set_literal_empty() {
        // NOTE: {} is an empty dict, not set. Empty set must be set() in Python
        let node = AstNode::Set { elements: vec![], line: 1, col: 1, end_line: 1, end_col: 2 };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Set(vec![])));
    }

    #[test]
    fn test_set_literal_simple() {
        let node = AstNode::Set {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 1, end_line: 1, end_col: 2 },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        let result = evaluate_const_expr(&node);
        assert_eq!(
            result,
            Some(ConstValue::Set(vec![
                ConstValue::Integer(1),
                ConstValue::Integer(2),
                ConstValue::Integer(3),
            ]))
        );
    }

    #[test]
    fn test_set_literal_deduplication() {
        let node = AstNode::Set {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };

        let result = evaluate_const_expr(&node);
        assert_eq!(
            result,
            Some(ConstValue::Set(vec![
                ConstValue::Integer(1),
                ConstValue::Integer(2),
                ConstValue::Integer(3),
            ]))
        );
    }

    #[test]
    fn test_set_literal_mixed_types() {
        let node = AstNode::Set {
            elements: vec![
                AstNode::Literal {
                    value: LiteralValue::String { value: "hello".to_string(), prefix: String::new() },
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
                AstNode::Literal { value: LiteralValue::Integer(42), line: 1, col: 1, end_line: 1, end_col: 2 },
                AstNode::Literal {
                    value: LiteralValue::Float(std::f64::consts::PI),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
                AstNode::Literal { value: LiteralValue::Boolean(true), line: 1, col: 1, end_line: 1, end_col: 2 },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        let result = evaluate_const_expr(&node);
        assert_eq!(
            result,
            Some(ConstValue::Set(vec![
                ConstValue::String("hello".to_string()),
                ConstValue::Integer(42),
                ConstValue::Float(std::f64::consts::PI),
                ConstValue::Boolean(true),
            ]))
        );
    }

    #[test]
    fn test_set_literal_with_expressions() {
        let node = AstNode::Set {
            elements: vec![
                AstNode::UnaryOp {
                    op: UnaryOperator::Minus,
                    operand: Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(5),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
                AstNode::BinaryOp {
                    left: Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(2),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    op: BinaryOperator::Mult,
                    right: Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(3),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                },
            ],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        let result = evaluate_const_expr(&node);
        assert_eq!(
            result,
            Some(ConstValue::Set(vec![ConstValue::Integer(-5), ConstValue::Integer(6),]))
        );
    }

    #[test]
    fn test_set_equality_order_independent() {
        let set1 = ConstValue::Set(vec![
            ConstValue::Integer(1),
            ConstValue::Integer(2),
            ConstValue::Integer(3),
        ]);
        let set2 = ConstValue::Set(vec![
            ConstValue::Integer(3),
            ConstValue::Integer(1),
            ConstValue::Integer(2),
        ]);
        assert!(values_equal(&set1, &set2));
        assert!(values_equal(&set2, &set1));
    }

    #[test]
    fn test_set_inequality() {
        let set1 = ConstValue::Set(vec![ConstValue::Integer(1), ConstValue::Integer(2)]);
        let set2 = ConstValue::Set(vec![ConstValue::Integer(1), ConstValue::Integer(3)]);
        assert!(!values_equal(&set1, &set2));

        let set3 = ConstValue::Set(vec![ConstValue::Integer(1)]);
        let set4 = ConstValue::Set(vec![ConstValue::Integer(1), ConstValue::Integer(2)]);
        assert!(!values_equal(&set3, &set4));
    }

    #[test]
    fn test_set_in_operator() {
        let set = ConstValue::Set(vec![
            ConstValue::Integer(1),
            ConstValue::Integer(2),
            ConstValue::Integer(3),
        ]);
        assert!(value_in_container(&ConstValue::Integer(1), &set));
        assert!(value_in_container(&ConstValue::Integer(2), &set));
        assert!(value_in_container(&ConstValue::Integer(3), &set));
        assert!(!value_in_container(&ConstValue::Integer(4), &set));
    }

    #[test]
    fn test_set_not_operator() {
        let empty_set = AstNode::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(AstNode::Set { elements: vec![], line: 1, col: 1, end_line: 1, end_col: 2 }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&empty_set), Some(ConstValue::Boolean(true)));

        let non_empty_set = AstNode::UnaryOp {
            op: UnaryOperator::Not,
            operand: Box::new(AstNode::Set {
                elements: vec![AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }],
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
        assert_eq!(evaluate_const_expr(&non_empty_set), Some(ConstValue::Boolean(false)));
    }

    #[test]
    fn test_set_comparison_in_match() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Set {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Set {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_dict_comparison_in_match() {
        let node = AstNode::Compare {
            left: Box::new(AstNode::Dict {
                keys: vec![AstNode::Literal {
                    value: LiteralValue::String { value: "a".to_string(), prefix: String::new() },
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }],
                values: vec![AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }],
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }),
            ops: vec![CompareOperator::Eq],
            comparators: vec![AstNode::Dict {
                keys: vec![AstNode::Literal {
                    value: LiteralValue::String { value: "a".to_string(), prefix: String::new() },
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }],
                values: vec![AstNode::Literal {
                    value: LiteralValue::Integer(1),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }],
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        assert_eq!(evaluate_const_expr(&node), Some(ConstValue::Boolean(true)));
    }

    #[test]
    fn test_nested_dict_and_set() {
        let node = AstNode::Dict {
            keys: vec![AstNode::Tuple {
                elements: vec![
                    AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 1, end_line: 1, end_col: 2 },
                    AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 1, end_line: 1, end_col: 2 },
                ],
                is_parenthesized: true,
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }],
            values: vec![AstNode::Literal {
                value: LiteralValue::String { value: "tuple_key".to_string(), prefix: String::new() },
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 2,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 2,
        };
        let result = evaluate_const_expr(&node);
        assert_eq!(
            result,
            Some(ConstValue::Dict(vec![(
                ConstValue::Tuple(vec![ConstValue::Integer(1), ConstValue::Integer(2)]),
                ConstValue::String("tuple_key".to_string())
            ),]))
        );
    }
}
