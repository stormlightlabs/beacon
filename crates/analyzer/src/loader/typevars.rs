use super::annotations::parse_type_annotation;

use beacon_core::{Type, TypeVar, TypeVarGen, Variance};
use beacon_parser::AstNode;
use rustc_hash::FxHashSet;
use std::collections::HashMap;

#[derive(Debug)]
pub struct StubTypeContext {
    pub(super) type_vars: FxHashSet<String>,
    var_map: HashMap<String, TypeVar>,
    var_gen: TypeVarGen,
    /// Maps TypeVar names to their variance (covariant, contravariant, invariant)
    var_variance: HashMap<String, Variance>,
    /// Maps TypeVar names to their bound types (if any)
    var_bounds: HashMap<String, Type>,
    /// Maps TypeVar names to their constraint types (if any)
    var_constraints: HashMap<String, Vec<Type>>,
}

impl StubTypeContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register_type_var(&mut self, name: &str, variance: Variance, bound: Option<Type>, constraints: Vec<Type>) {
        self.type_vars.insert(name.to_string());
        self.var_variance.insert(name.to_string(), variance);
        if let Some(bound_ty) = bound {
            self.var_bounds.insert(name.to_string(), bound_ty);
        }
        if !constraints.is_empty() {
            self.var_constraints.insert(name.to_string(), constraints);
        }
    }

    pub fn is_type_var(&self, name: &str) -> bool {
        self.type_vars.contains(name)
    }

    pub fn get_or_create_type_var(&mut self, name: &str) -> TypeVar {
        if let Some(tv) = self.var_map.get(name) {
            tv.clone()
        } else {
            let variance = self.var_variance.get(name).copied().unwrap_or(Variance::Invariant);
            let tv = TypeVar::with_variance(self.var_gen.fresh().id, Some(name.to_string()), variance);
            self.var_map.insert(name.to_string(), tv.clone());
            tv
        }
    }

    pub fn get_bound(&self, name: &str) -> Option<&Type> {
        self.var_bounds.get(name)
    }

    pub fn get_constraints(&self, name: &str) -> Option<&Vec<Type>> {
        self.var_constraints.get(name)
    }
}

impl Default for StubTypeContext {
    fn default() -> Self {
        Self {
            type_vars: FxHashSet::default(),
            var_map: HashMap::default(),
            var_gen: TypeVarGen::new(),
            var_variance: HashMap::default(),
            var_bounds: HashMap::default(),
            var_constraints: HashMap::default(),
        }
    }
}

pub fn collect_stub_type_vars(node: &AstNode, ctx: &mut StubTypeContext) {
    match node {
        AstNode::Module { body, .. } => {
            for stmt in body {
                collect_stub_type_vars(stmt, ctx);
            }
        }
        AstNode::Assignment { target, value, .. } => {
            let AstNode::Call { function, args, keywords, .. } = value.as_ref() else {
                return;
            };
            if !function
                .qualified_name()
                .is_some_and(|function_name| function_name == "TypeVar" || function_name.ends_with(".TypeVar"))
            {
                return;
            }

            let target_str = target.target_display();
            let (variance, bound, constraints) = extract_typevar_metadata_from_stub(args, keywords, ctx);
            ctx.register_type_var(&target_str, variance, bound, constraints);
        }
        AstNode::ClassDef { body, .. } => {
            for stmt in body {
                collect_stub_type_vars(stmt, ctx);
            }
        }
        _ => {}
    }
}

/// Extract variance, bounds, and constraints from TypeVar calls in stub files.
///
/// Parses:
/// - Variance: `covariant=True` and `contravariant=True` keyword arguments
/// - Bound: `bound=Animal` keyword argument
/// - Constraints: Positional arguments after the name: `TypeVar('T', int, str)`
///
/// Examples:
/// - `TypeVar('T')` → (Invariant, None, [])
/// - `TypeVar('T_Co', covariant=True)` → (Covariant, None, [])
/// - `TypeVar('T', bound=Animal)` → (Invariant, Some(Animal), [])
/// - `TypeVar('T', int, str)` → (Invariant, None, [int, str])
fn extract_typevar_metadata_from_stub(
    args: &[AstNode], keywords: &[(String, AstNode)], ctx: &mut StubTypeContext,
) -> (Variance, Option<Type>, Vec<Type>) {
    let mut covariant = false;
    let mut contravariant = false;
    let mut bound = None;

    for (key, value) in keywords {
        match key.as_str() {
            "covariant" => {
                covariant = is_true_literal(value);
            }
            "contravariant" => {
                contravariant = is_true_literal(value);
            }
            "bound" => {
                bound = parse_typevar_bound_from_stub(value, ctx);
            }
            _ => {}
        }
    }

    let variance = match (covariant, contravariant) {
        (true, false) => Variance::Covariant,
        (false, true) => Variance::Contravariant,
        _ => Variance::Invariant,
    };

    let mut constraints = Vec::new();
    if args.len() > 1 {
        for constraint_node in &args[1..] {
            if let Some(constraint_ty) = parse_typevar_constraint_from_stub(constraint_node, ctx) {
                constraints.push(constraint_ty);
            }
        }
    }

    (variance, bound, constraints)
}

/// Parse a TypeVar bound from a stub AST node
fn parse_typevar_bound_from_stub(node: &AstNode, ctx: &mut StubTypeContext) -> Option<Type> {
    match node {
        AstNode::Identifier { name, .. } => parse_type_annotation(name, ctx),
        AstNode::Subscript { value, slice, .. } => {
            let base_name = match value.as_ref() {
                AstNode::Identifier { name, .. } => name.as_str(),
                _ => return None,
            };

            let slice_str = node_to_annotation_string(slice);
            let full_annotation = format!("{base_name}[{slice_str}]");
            parse_type_annotation(&full_annotation, ctx)
        }
        _ => None,
    }
}

/// Parse a TypeVar constraint from a stub AST node
fn parse_typevar_constraint_from_stub(node: &AstNode, ctx: &mut StubTypeContext) -> Option<Type> {
    match node {
        AstNode::Identifier { name, .. } => parse_type_annotation(name, ctx),
        AstNode::Subscript { value, slice, .. } => {
            let base_name = match value.as_ref() {
                AstNode::Identifier { name, .. } => name.as_str(),
                _ => return None,
            };

            let slice_str = node_to_annotation_string(slice);
            let full_annotation = format!("{base_name}[{slice_str}]");
            parse_type_annotation(&full_annotation, ctx)
        }
        _ => None,
    }
}

/// Convert an AST node to an annotation string (for complex type expressions)
fn node_to_annotation_string(node: &AstNode) -> String {
    match node {
        AstNode::Identifier { name, .. } => name.clone(),
        AstNode::Subscript { value, slice, .. } => {
            let base = node_to_annotation_string(value);
            let slice_str = node_to_annotation_string(slice);
            format!("{base}[{slice_str}]")
        }
        AstNode::Tuple { elements, .. } => {
            let parts: Vec<String> = elements.iter().map(node_to_annotation_string).collect();
            parts.join(", ")
        }
        _ => "Any".to_string(),
    }
}

/// Check if an AST node is a True literal
fn is_true_literal(node: &AstNode) -> bool {
    matches!(
        node,
        AstNode::Literal { value: beacon_parser::LiteralValue::Boolean(true), .. }
    )
}
