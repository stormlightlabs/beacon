pub mod context;
pub mod exhaustiveness;
pub mod flow;
pub mod model;
pub mod pattern_compat;
pub mod pattern_validation;
pub mod solver;

mod pattern;
mod predicate;

pub use context::ConstraintGenContext;
pub use flow::{ControlFlowContext, TypeSetTracker};
pub use model::{Constraint, ConstraintResult, ConstraintSet, Span, TypeErrorInfo, TypeGuardInfo, TypeGuardKind};
pub use pattern_compat::{is_dict_type, is_sequence_type, type_compatible_with_class};
pub use predicate::TypePredicate;

#[cfg(test)]
use beacon_core::{Type, TypeCtor};
#[cfg(test)]
use beacon_parser::ScopeId;

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::{Pattern, ScopeKind};

    #[test]
    fn test_scope_stack_push_pop() {
        let mut ctx = ConstraintGenContext::new();
        let scope_id1 = ScopeId::from_raw(1);
        let scope_id2 = ScopeId::from_raw(2);
        assert!(ctx.scope_stack.is_empty());

        ctx.push_scope(scope_id1);
        assert_eq!(ctx.scope_stack.len(), 1);
        assert_eq!(ctx.scope_stack.last(), Some(&scope_id1));

        ctx.push_scope(scope_id2);
        assert_eq!(ctx.scope_stack.len(), 2);
        assert_eq!(ctx.scope_stack.last(), Some(&scope_id2));

        assert_eq!(ctx.pop_scope(), Some(scope_id2));
        assert_eq!(ctx.scope_stack.len(), 1);
        assert_eq!(ctx.scope_stack.last(), Some(&scope_id1));

        assert_eq!(ctx.pop_scope(), Some(scope_id1));
        assert!(ctx.scope_stack.is_empty());

        assert_eq!(ctx.pop_scope(), None);
    }

    #[test]
    fn test_node_to_scope_mapping() {
        let mut ctx = ConstraintGenContext::new();
        let scope_id = ScopeId::from_raw(1);

        let node_id1 = ctx.record_type(1, 1, Type::int());
        assert!(!ctx.node_to_scope.contains_key(&node_id1));

        ctx.push_scope(scope_id);
        let node_id2 = ctx.record_type(2, 2, Type::string());
        assert_eq!(ctx.node_to_scope.get(&node_id2), Some(&scope_id));

        let node_id3 = ctx.record_type(3, 3, Type::bool());
        assert_eq!(ctx.node_to_scope.get(&node_id3), Some(&scope_id));

        ctx.pop_scope();
        let node_id4 = ctx.record_type(4, 4, Type::float());
        assert!(!ctx.node_to_scope.contains_key(&node_id4));
    }

    #[test]
    fn test_scope_dependencies() {
        let mut ctx = ConstraintGenContext::new();
        let scope1 = ScopeId::from_raw(1);
        let scope2 = ScopeId::from_raw(2);
        let scope3 = ScopeId::from_raw(3);

        assert!(ctx.scope_dependencies.is_empty());

        ctx.add_scope_dependency(scope1, scope2);
        assert_eq!(ctx.scope_dependencies.get(&scope1).map(|s| s.len()), Some(1));
        assert!(ctx.scope_dependencies.get(&scope1).unwrap().contains(&scope2));

        ctx.add_scope_dependency(scope1, scope3);
        assert_eq!(ctx.scope_dependencies.get(&scope1).map(|s| s.len()), Some(2));
        assert!(ctx.scope_dependencies.get(&scope1).unwrap().contains(&scope2));
        assert!(ctx.scope_dependencies.get(&scope1).unwrap().contains(&scope3));

        ctx.add_scope_dependency(scope1, scope2);
        assert_eq!(ctx.scope_dependencies.get(&scope1).map(|s| s.len()), Some(2));

        assert!(!ctx.scope_dependencies.contains_key(&scope2));
    }

    #[test]
    fn test_is_sequence_type() {
        let list_type = Type::Con(TypeCtor::List);
        assert!(is_sequence_type(&list_type));

        let tuple_type = Type::Con(TypeCtor::Tuple);
        assert!(is_sequence_type(&tuple_type));

        let list_int = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::int()));
        assert!(is_sequence_type(&list_int));

        let int_type = Type::int();
        assert!(!is_sequence_type(&int_type));

        let dict_type = Type::Con(TypeCtor::Dict);
        assert!(!is_sequence_type(&dict_type));
    }

    #[test]
    fn test_is_dict_type() {
        let dict_type = Type::Con(TypeCtor::Dict);
        assert!(is_dict_type(&dict_type));

        let dict_type_app = Type::App(Box::new(Type::Con(TypeCtor::Dict)), Box::new(Type::string()));
        assert!(is_dict_type(&dict_type_app));

        let int_type = Type::int();
        assert!(!is_dict_type(&int_type));

        let list_type = Type::Con(TypeCtor::List);
        assert!(!is_dict_type(&list_type));
    }

    #[test]
    fn test_type_compatible_with_class() {
        let point_type = Type::Con(TypeCtor::Class("Point".to_string()));
        assert!(type_compatible_with_class(&point_type, "Point"));
        assert!(!type_compatible_with_class(&point_type, "Circle"));

        let int_type = Type::int();
        assert!(!type_compatible_with_class(&int_type, "Point"));
    }

    #[test]
    fn test_type_predicate_matches_pattern_apply() {
        let union_type = Type::union(vec![Type::Con(TypeCtor::List), Type::string()]);
        let pattern = Pattern::MatchSequence(vec![]);
        let predicate = TypePredicate::MatchesPattern(pattern);
        let result = predicate.apply(&union_type);
        assert_eq!(result, Type::Con(TypeCtor::List));
    }

    #[test]
    fn test_type_predicate_matches_pattern_negate() {
        let pattern = Pattern::MatchSequence(vec![]);
        let predicate = TypePredicate::MatchesPattern(pattern.clone());
        let negated = predicate.negate();
        assert!(matches!(negated, TypePredicate::Not(_)));
    }

    #[test]
    fn test_type_predicate_matches_pattern_has_no_simple_negation() {
        let pattern = Pattern::MatchSequence(vec![]);
        let predicate = TypePredicate::MatchesPattern(pattern);
        assert!(!predicate.has_simple_negation());
    }

    #[test]
    fn test_control_flow_context_narrowing_scope() {
        let mut ctx = ControlFlowContext::new();
        ctx.push_scope(Some(TypePredicate::IsNotNone));
        ctx.narrow("x".to_string(), Type::int());

        assert_eq!(ctx.get_narrowed_type("x"), Some(&Type::int()));

        assert!(ctx.pop_scope());
        assert_eq!(ctx.get_narrowed_type("x"), None);
    }

    #[test]
    fn test_control_flow_context_nested_scopes() {
        let mut ctx = ControlFlowContext::new();

        ctx.push_scope(None);
        ctx.narrow("x".to_string(), Type::int());

        ctx.push_scope(None);
        ctx.narrow("x".to_string(), Type::string());

        assert_eq!(ctx.get_narrowed_type("x"), Some(&Type::string()));

        ctx.pop_scope();
        assert_eq!(ctx.get_narrowed_type("x"), Some(&Type::int()));

        ctx.pop_scope();
        assert_eq!(ctx.get_narrowed_type("x"), None);
    }

    #[test]
    fn test_control_flow_context_cannot_pop_root_scope() {
        let mut ctx = ControlFlowContext::new();
        assert!(!ctx.pop_scope());

        ctx.narrow("x".to_string(), Type::int());
        assert_eq!(ctx.get_narrowed_type("x"), Some(&Type::int()));
    }

    #[test]
    fn test_type_set_tracker_new() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let tracker = TypeSetTracker::new("x".to_string(), union_type.clone());
        assert_eq!(tracker.variable(), "x");
        assert_eq!(tracker.original_type(), &union_type);
        assert_eq!(tracker.eliminated_types().len(), 0);
    }

    #[test]
    fn test_type_set_tracker_eliminate_type() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let mut tracker = TypeSetTracker::new("x".to_string(), union_type);

        tracker.eliminate_type(Type::none());
        assert_eq!(tracker.eliminated_types(), &[Type::none()]);

        tracker.eliminate_type(Type::int());
        assert_eq!(tracker.eliminated_types().len(), 2);
        assert!(tracker.eliminated_types().contains(&Type::none()));
        assert!(tracker.eliminated_types().contains(&Type::int()));
    }

    #[test]
    fn test_type_set_tracker_eliminate_type_deduplication() {
        let union_type = Type::union(vec![Type::int(), Type::string()]);
        let mut tracker = TypeSetTracker::new("x".to_string(), union_type);

        tracker.eliminate_type(Type::int());
        tracker.eliminate_type(Type::int());
        tracker.eliminate_type(Type::int());

        assert_eq!(tracker.eliminated_types(), &[Type::int()]);
    }

    #[test]
    fn test_type_set_tracker_remaining_types_union() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let mut tracker = TypeSetTracker::new("x".to_string(), union_type);
        let remaining = tracker.remaining_types();
        assert_eq!(remaining, Type::union(vec![Type::int(), Type::string(), Type::none()]));

        tracker.eliminate_type(Type::none());
        let remaining = tracker.remaining_types();
        assert_eq!(remaining, Type::union(vec![Type::int(), Type::string()]));

        tracker.eliminate_type(Type::int());
        let remaining = tracker.remaining_types();
        assert_eq!(remaining, Type::string());

        tracker.eliminate_type(Type::string());
        let remaining = tracker.remaining_types();
        assert_eq!(remaining, Type::never());
    }

    #[test]
    fn test_type_set_tracker_remaining_types_non_union() {
        let int_type = Type::int();
        let mut tracker = TypeSetTracker::new("x".to_string(), int_type.clone());

        assert_eq!(tracker.remaining_types(), int_type);

        tracker.eliminate_type(int_type.clone());
        assert_eq!(tracker.remaining_types(), Type::never());
    }

    #[test]
    fn test_type_set_tracker_is_exhaustive_union() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let mut tracker = TypeSetTracker::new("x".to_string(), union_type);

        assert!(!tracker.is_exhaustive());

        tracker.eliminate_type(Type::none());
        assert!(!tracker.is_exhaustive());

        tracker.eliminate_type(Type::int());
        assert!(!tracker.is_exhaustive());

        tracker.eliminate_type(Type::string());
        assert!(tracker.is_exhaustive());
    }

    #[test]
    fn test_type_set_tracker_is_exhaustive_non_union() {
        let int_type = Type::int();
        let mut tracker = TypeSetTracker::new("x".to_string(), int_type.clone());

        assert!(!tracker.is_exhaustive());

        tracker.eliminate_type(int_type);
        assert!(tracker.is_exhaustive());
    }

    #[test]
    fn test_control_flow_start_and_stop_tracking() {
        let mut ctx = ControlFlowContext::new();
        let union_type = Type::union(vec![Type::int(), Type::string()]);

        ctx.start_tracking("x".to_string(), union_type.clone());
        assert!(ctx.get_tracker("x").is_some());

        ctx.stop_tracking("x");
        assert!(ctx.get_tracker("x").is_none());
    }

    #[test]
    fn test_control_flow_eliminate_type() {
        let mut ctx = ControlFlowContext::new();
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);

        ctx.start_tracking("x".to_string(), union_type);
        ctx.eliminate_type("x", Type::none());

        let tracker = ctx.get_tracker("x").unwrap();
        assert_eq!(tracker.eliminated_types(), &[Type::none()]);
    }

    #[test]
    fn test_control_flow_get_remaining_types() {
        let mut ctx = ControlFlowContext::new();
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);

        ctx.start_tracking("x".to_string(), union_type);
        ctx.eliminate_type("x", Type::none());

        let remaining = ctx.get_remaining_types("x").unwrap();
        assert_eq!(remaining, Type::union(vec![Type::int(), Type::string()]));
    }

    #[test]
    fn test_control_flow_is_exhaustive() {
        let mut ctx = ControlFlowContext::new();
        let union_type = Type::union(vec![Type::int(), Type::string()]);

        ctx.start_tracking("x".to_string(), union_type);

        assert!(!ctx.is_exhaustive("x"));

        ctx.eliminate_type("x", Type::int());
        assert!(!ctx.is_exhaustive("x"));

        ctx.eliminate_type("x", Type::string());
        assert!(ctx.is_exhaustive("x"));
    }

    #[test]
    fn test_control_flow_exhaustiveness_successive_eliminations() {
        let mut ctx = ControlFlowContext::new();
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);

        ctx.start_tracking("x".to_string(), union_type);

        ctx.eliminate_type("x", Type::none());
        let remaining = ctx.get_remaining_types("x").unwrap();
        assert_eq!(remaining, Type::union(vec![Type::int(), Type::string()]));

        ctx.eliminate_type("x", Type::int());
        let remaining = ctx.get_remaining_types("x").unwrap();
        assert_eq!(remaining, Type::string());

        assert!(!ctx.is_exhaustive("x"));

        ctx.eliminate_type("x", Type::string());
        assert!(ctx.is_exhaustive("x"));
        assert_eq!(ctx.get_remaining_types("x").unwrap(), Type::never());
    }

    #[test]
    fn test_predicate_eliminated_types_is_not_none() {
        let union_type = Type::union(vec![Type::int(), Type::none()]);
        let predicate = TypePredicate::IsNotNone;
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated, vec![Type::none()]);
    }

    #[test]
    fn test_predicate_eliminated_types_is_none() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let predicate = TypePredicate::IsNone;
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated.len(), 2);
        assert!(eliminated.contains(&Type::int()));
        assert!(eliminated.contains(&Type::string()));
    }

    #[test]
    fn test_predicate_eliminated_types_isinstance() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let predicate = TypePredicate::IsInstance(Type::int());
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated.len(), 2);
        assert!(eliminated.contains(&Type::string()));
        assert!(eliminated.contains(&Type::none()));
    }

    #[test]
    fn test_predicate_eliminated_types_isinstance_union_target() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let target_union = Type::union(vec![Type::int(), Type::string()]);
        let predicate = TypePredicate::IsInstance(target_union);
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated, vec![Type::none()]);
    }

    #[test]
    fn test_predicate_eliminated_types_is_truthy() {
        let union_type = Type::union(vec![Type::int(), Type::none()]);
        let predicate = TypePredicate::IsTruthy;
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated, vec![Type::none()]);
    }

    #[test]
    fn test_predicate_eliminated_types_and() {
        let union_type = Type::union(vec![Type::int(), Type::none()]);
        let pred1 = TypePredicate::IsNotNone;
        let pred2 = TypePredicate::IsTruthy;
        let predicate = TypePredicate::And(Box::new(pred1), Box::new(pred2));
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated.len(), 1);
        assert!(eliminated.contains(&Type::none()));
    }

    #[test]
    fn test_predicate_eliminated_types_or() {
        let union_type = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        let pred1 = TypePredicate::IsInstance(Type::int());
        let pred2 = TypePredicate::IsInstance(Type::string());
        let predicate = TypePredicate::Or(Box::new(pred1), Box::new(pred2));
        let eliminated = predicate.eliminated_types(&union_type);
        assert_eq!(eliminated, vec![Type::none()]);
    }

    #[test]
    fn test_line_col_to_byte_offset() {
        let source = "def foo():\n    return 42\n";

        assert_eq!(beacon_parser::line_col_to_byte_offset(source, 1, 1), Some(0));

        assert_eq!(beacon_parser::line_col_to_byte_offset(source, 1, 4), Some(3));

        assert_eq!(beacon_parser::line_col_to_byte_offset(source, 2, 1), Some(11));

        assert_eq!(beacon_parser::line_col_to_byte_offset(source, 2, 5), Some(15));
    }

    #[test]
    fn test_find_scope_at_position() {
        let source = "x = 1\ndef foo():\n    return 42\n";
        let mut symbol_table = beacon_parser::SymbolTable::new();

        let func_scope = symbol_table.create_scope(ScopeKind::Function, symbol_table.root_scope, 17, source.len());

        let mut ctx = ConstraintGenContext::new();
        ctx.set_context(&symbol_table, source);

        let scope = ctx.find_scope_at_position(3, 5);
        assert_eq!(scope, Some(func_scope));

        let scope = ctx.find_scope_at_position(1, 1);
        assert_eq!(scope, Some(symbol_table.root_scope));
    }

    #[test]
    fn test_set_context_stores_references() {
        let source = "x = 1\n";
        let symbol_table = beacon_parser::SymbolTable::new();
        let mut ctx = ConstraintGenContext::new();

        assert!(ctx.symbol_table().is_none());
        assert!(ctx.source.is_none());

        ctx.set_context(&symbol_table, source);

        assert!(ctx.symbol_table().is_some());
        assert!(ctx.source.is_some());
    }
}
