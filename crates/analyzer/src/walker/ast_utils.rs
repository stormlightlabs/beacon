//! Shared AST helpers for constraint generation.
//!
//! Keep these helpers focused on AST shape and source positions so visitor
//! modules do not each grow their own extraction logic.

use crate::type_env::TypeEnvironment;

use beacon_core::{Type, TypeScheme};
use beacon_parser::AstNode;

/// Get the line and column position from any AstNode.
///
/// Extracts the source position from the node for error reporting and span tracking.
pub(super) fn get_node_position(node: &AstNode) -> (usize, usize, usize, usize) {
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
        | AstNode::Global { line, col, end_line, end_col, .. }
        | AstNode::Nonlocal { line, col, end_line, end_col, .. }
        | AstNode::Assert { line, col, end_line, end_col, .. }
        | AstNode::Starred { line, col, end_line, end_col, .. }
        | AstNode::ParenthesizedExpression { line, col, end_line, end_col, .. } => (*line, *col, *end_line, *end_col),
    }
}

pub(super) fn bind_comprehension_target(env: &mut TypeEnvironment, target: &str, ty: &Type) {
    let parts: Vec<&str> = if target.contains(',') { target.split(',').collect() } else { vec![target] };

    for part in parts {
        let name = part.trim().trim_matches(|c| matches!(c, '(' | ')' | '[' | ']'));
        if name.is_empty() {
            continue;
        }
        env.bind(name.to_string(), TypeScheme::mono(ty.clone()));
    }
}
