use crate::{Span, TypePredicate};

use beacon_core::{Type, TypeCtor};
use rustc_hash::FxHashMap;

/// A single narrowing scope in the control-flow stack.
#[derive(Debug, Clone, Default)]
struct NarrowingScope {
    narrowed_vars: FxHashMap<String, Type>,
    #[allow(dead_code)]
    predicate: Option<TypePredicate>,
}

/// A join point where control-flow paths merge.
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct JoinPoint {
    variable: String,
    incoming_types: Vec<Type>,
    span: Span,
}

/// Tracks type elimination through successive narrowing for exhaustiveness checks.
#[derive(Debug, Clone)]
pub struct TypeSetTracker {
    variable: String,
    original_type: Type,
    eliminated_types: Vec<Type>,
}

impl TypeSetTracker {
    pub fn new(variable: String, original_type: Type) -> Self {
        Self { variable, original_type, eliminated_types: Vec::new() }
    }

    pub fn eliminate_type(&mut self, ty: Type) {
        if !self.eliminated_types.contains(&ty) {
            self.eliminated_types.push(ty);
        }
    }

    pub fn remaining_types(&self) -> Type {
        match &self.original_type {
            Type::Union(variants) => {
                let remaining: Vec<Type> = variants
                    .iter()
                    .filter(|v| !self.eliminated_types.contains(v))
                    .cloned()
                    .collect();

                if remaining.is_empty() {
                    Type::never()
                } else if remaining.len() == 1 {
                    remaining.into_iter().next().unwrap()
                } else {
                    Type::union(remaining)
                }
            }
            _ => {
                if self.eliminated_types.contains(&self.original_type) {
                    Type::never()
                } else {
                    self.original_type.clone()
                }
            }
        }
    }

    pub fn is_exhaustive(&self) -> bool {
        match &self.original_type {
            Type::Union(variants) => variants.iter().all(|v| self.eliminated_types.contains(v)),
            _ => self.eliminated_types.contains(&self.original_type),
        }
    }

    pub fn variable(&self) -> &str {
        &self.variable
    }

    pub fn original_type(&self) -> &Type {
        &self.original_type
    }

    pub fn eliminated_types(&self) -> &[Type] {
        &self.eliminated_types
    }
}

/// Control-flow context for tracking narrowing scopes and type elimination.
#[derive(Debug, Clone)]
pub struct ControlFlowContext {
    scopes: Vec<NarrowingScope>,
    pending_joins: Vec<JoinPoint>,
    type_trackers: FxHashMap<String, TypeSetTracker>,
}

impl ControlFlowContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_scope(&mut self, predicate: Option<TypePredicate>) {
        self.scopes
            .push(NarrowingScope { narrowed_vars: FxHashMap::default(), predicate });
    }

    pub fn pop_scope(&mut self) -> bool {
        if self.scopes.len() > 1 {
            self.scopes.pop();
            true
        } else {
            false
        }
    }

    pub fn narrow(&mut self, var: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.narrowed_vars.insert(var, ty);
        }
    }

    pub fn get_narrowed_type(&self, var: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.narrowed_vars.get(var) {
                return Some(ty);
            }
        }
        None
    }

    pub fn add_join(&mut self, variable: String, types: Vec<Type>, span: Span) {
        self.pending_joins
            .push(JoinPoint { variable, incoming_types: types, span });
    }

    #[allow(private_interfaces)]
    pub fn take_pending_joins(&mut self) -> Vec<JoinPoint> {
        std::mem::take(&mut self.pending_joins)
    }

    pub fn start_tracking(&mut self, variable: String, original_type: Type) {
        self.type_trackers
            .insert(variable.clone(), TypeSetTracker::new(variable, original_type));
    }

    pub fn eliminate_type(&mut self, variable: &str, eliminated_type: Type) {
        if let Some(tracker) = self.type_trackers.get_mut(variable) {
            tracker.eliminate_type(eliminated_type);
        }
    }

    pub fn get_remaining_types(&self, variable: &str) -> Option<Type> {
        self.type_trackers
            .get(variable)
            .map(|tracker| tracker.remaining_types())
    }

    pub fn apply_remaining_types(&self, variable: &str, current_type: &Type) -> Option<Type> {
        self.type_trackers.get(variable).and_then(|tracker| {
            let refined = tracker
                .eliminated_types()
                .iter()
                .fold(current_type.clone(), |acc, eliminated| {
                    acc.remove_from_union(eliminated)
                });

            if matches!(refined, Type::Con(TypeCtor::Never)) || refined == *current_type {
                None
            } else {
                Some(refined)
            }
        })
    }

    pub fn is_exhaustive(&self, variable: &str) -> bool {
        self.type_trackers
            .get(variable)
            .map(|tracker| tracker.is_exhaustive())
            .unwrap_or(false)
    }

    pub fn stop_tracking(&mut self, variable: &str) {
        self.type_trackers.remove(variable);
    }

    pub fn get_tracker(&self, variable: &str) -> Option<&TypeSetTracker> {
        self.type_trackers.get(variable)
    }
}

impl Default for ControlFlowContext {
    fn default() -> Self {
        Self {
            scopes: vec![NarrowingScope::default()],
            pending_joins: Vec::new(),
            type_trackers: FxHashMap::default(),
        }
    }
}
