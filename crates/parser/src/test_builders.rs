//! Small AST builders for parser tests.

use crate::{AstNode, LiteralValue};

pub fn ident(name: &str) -> AstNode {
    AstNode::Identifier { name: name.to_string(), line: 1, col: 1, end_line: 1, end_col: 1 + name.len() }
}

pub fn int(value: i64) -> AstNode {
    AstNode::Literal { value: LiteralValue::Integer(value), line: 1, col: 1, end_line: 1, end_col: 2 }
}

pub fn assignment(name: &str, value: AstNode) -> AstNode {
    AstNode::Assignment {
        target: Box::new(ident(name)),
        value: Box::new(value),
        line: 1,
        col: 1,
        end_line: 1,
        end_col: 2,
    }
}
