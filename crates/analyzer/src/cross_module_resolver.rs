//! Cross-module type resolution for transitive type propagation
//!
//! Resolves types across module boundaries using the call graph and workspace type environment.
//! Implements iterative type propagation to handle mutually recursive functions.

use beacon_constraint::{Constraint, ConstraintSet, Span};
use beacon_core::{Result, Type, TypeScheme};

use crate::cfg::{FunctionId, WorkspaceCFG};
use crate::workspace::WorkspaceTypeEnvironment;

/// Cross-module type resolver
///
/// Propagates types across module boundaries by analyzing the call graph and resolving function types transitively.
pub struct CrossModuleTypeResolver<'a> {
    /// Workspace CFG containing the call graph
    workspace_cfg: &'a WorkspaceCFG,
    /// Workspace type environment
    workspace_env: &'a mut WorkspaceTypeEnvironment,
    /// Maximum iterations for fixed-point computation
    max_iterations: usize,
}

impl<'a> CrossModuleTypeResolver<'a> {
    pub fn new(workspace_cfg: &'a WorkspaceCFG, workspace_env: &'a mut WorkspaceTypeEnvironment) -> Self {
        Self { workspace_cfg, workspace_env, max_iterations: 10 }
    }

    /// Set maximum iterations for fixed-point computation
    pub fn with_max_iterations(mut self, max_iterations: usize) -> Self {
        self.max_iterations = max_iterations;
        self
    }

    /// Perform transitive type propagation across all modules
    ///
    /// Uses iterative constraint solving to propagate types along call graph edges until a fixed point is reached (no types change) or max iterations exceeded.
    pub fn propagate_types(&mut self) -> Result<PropagationResult> {
        let call_graph = self.workspace_cfg.call_graph();
        let sccs = call_graph.strongly_connected_components();

        let mut iteration = 0;
        let mut changed = true;
        let mut constraints_generated = 0;

        while changed && iteration < self.max_iterations {
            changed = false;
            iteration += 1;

            tracing::debug!(
                "Cross-module type propagation iteration {} of {}",
                iteration,
                self.max_iterations
            );

            for scc in sccs.iter().rev() {
                if self.propagate_scc_types(scc, &mut constraints_generated)? {
                    changed = true;
                }
            }

            if !changed {
                tracing::info!("Cross-module type propagation converged after {} iterations", iteration);
                break;
            }
        }

        if changed {
            tracing::warn!(
                "Cross-module type propagation did not converge after {} iterations",
                self.max_iterations
            );
        }

        Ok(PropagationResult { iterations: iteration, converged: !changed, constraints_generated })
    }

    /// Propagate types within a strongly connected component (SCC)
    ///
    /// Functions in an SCC are mutually recursive and must be solved together.
    fn propagate_scc_types(&mut self, scc: &[FunctionId], constraints_generated: &mut usize) -> Result<bool> {
        let call_graph = self.workspace_cfg.call_graph();
        let mut changed = false;

        for function_id in scc {
            changed |= self.propagate_function_types(function_id, call_graph, constraints_generated)?;
        }

        Ok(changed)
    }

    /// Propagate types for a single function's cross-module calls
    fn propagate_function_types(
        &mut self, function_id: &FunctionId, call_graph: &crate::cfg::CallGraph, constraints_generated: &mut usize,
    ) -> Result<bool> {
        let Some(call_sites) = call_graph.get_call_sites(function_id) else {
            return Ok(false);
        };

        let mut changed = false;

        for call_site in call_sites {
            if let Some(type_changed) = self.try_resolve_call_site(function_id, call_site)?
                && type_changed
            {
                changed = true;
                *constraints_generated += 1;
            }
        }

        Ok(changed)
    }

    /// Try to resolve a call site if it's a cross-module call
    fn try_resolve_call_site(&mut self, caller: &FunctionId, call_site: &crate::cfg::CallSite) -> Result<Option<bool>> {
        let Some(callee) = &call_site.receiver else {
            return Ok(None);
        };

        if callee.uri == caller.uri {
            return Ok(None);
        }

        self.resolve_cross_module_call(caller, callee).map(Some)
    }

    /// Resolve a cross-module function call
    ///
    /// Attempts to resolve the callee's type and propagate it to the caller.
    fn resolve_cross_module_call(&mut self, _caller: &FunctionId, callee: &FunctionId) -> Result<bool> {
        if let Some(_callee_type) = self.workspace_env.get_function_type(callee) {
            return Ok(false);
        }

        if let Some(module_info) = self.workspace_env.get_module(&callee.uri) {
            let function_type = module_info
                .lookup_symbol_type(&callee.name, callee.scope_id)
                .unwrap_or_else(Type::any);

            let type_scheme = TypeScheme::mono(function_type);
            self.workspace_env.set_function_type(callee.clone(), type_scheme);
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Generate additional constraints for cross-module calls
    ///
    /// Creates Call constraints that unify argument types with parameter types and propagate return types back to call sites.
    /// This method requires CallSite node IDs to be populated via CallSite::with_nodes().
    pub fn generate_cross_module_constraints(&self) -> Result<ConstraintSet> {
        let call_graph = self.workspace_cfg.call_graph();
        let mut constraints = Vec::new();

        for (caller_fn, call_sites) in call_graph.all_call_sites() {
            for call_site in call_sites {
                if let Some(new_constraints) = self.process_call_site(caller_fn, call_site) {
                    constraints.extend(new_constraints);
                }
            }
        }

        Ok(ConstraintSet { constraints })
    }

    /// Process a single call site and generate constraints if it's a cross-module call
    fn process_call_site(
        &self, caller_fn: &FunctionId, call_site: &crate::cfg::CallSite,
    ) -> Option<Vec<beacon_constraint::Constraint>> {
        let callee_fn = call_site.receiver.as_ref()?;
        if callee_fn.uri == caller_fn.uri {
            return None;
        }

        let callee_type_scheme = self.workspace_env.get_function_type(callee_fn)?;
        let caller_module = self.workspace_env.get_module(&caller_fn.uri)?;

        let call_node_id = call_site.call_node_id?;
        let result_node_id = call_site.result_node_id?;

        let span = Span::new(call_site.line, call_site.col);
        let type_map = &caller_module.constraint_result.1;

        let pos_args = self.collect_positional_args(&call_site.arg_node_ids, type_map, span);

        let kw_args = self.collect_keyword_args(&call_site.kwarg_node_ids, type_map, span);

        let result_type = type_map.get(&result_node_id).cloned().unwrap_or_else(Type::any);
        let call_type = type_map.get(&call_node_id).cloned().unwrap_or_else(Type::any);

        let call_constraint = Constraint::Call(call_type.clone(), pos_args, kw_args, result_type, span);
        let equal_constraint = Constraint::Equal(call_type, callee_type_scheme.ty.clone(), span);

        Some(vec![call_constraint, equal_constraint])
    }

    /// Collect positional argument types from node IDs
    fn collect_positional_args(
        &self, arg_node_ids: &[usize], type_map: &rustc_hash::FxHashMap<usize, Type>, span: beacon_constraint::Span,
    ) -> Vec<(Type, beacon_constraint::Span)> {
        arg_node_ids
            .iter()
            .filter_map(|node_id| type_map.get(node_id).map(|ty| (ty.clone(), span)))
            .collect()
    }

    /// Collect keyword argument types from node IDs
    fn collect_keyword_args(
        &self, kwarg_node_ids: &[(String, usize)], type_map: &rustc_hash::FxHashMap<usize, Type>,
        span: beacon_constraint::Span,
    ) -> Vec<(String, Type, beacon_constraint::Span)> {
        kwarg_node_ids
            .iter()
            .filter_map(|(name, node_id)| type_map.get(node_id).map(|ty| (name.clone(), ty.clone(), span)))
            .collect()
    }
}

/// Result of cross-module type propagation
#[derive(Debug, Clone)]
pub struct PropagationResult {
    /// Number of iterations performed
    pub iterations: usize,
    /// Whether the propagation converged (reached fixed point)
    pub converged: bool,
    /// Number of cross-module constraints generated
    pub constraints_generated: usize,
}

impl PropagationResult {
    /// Check if propagation was successful
    pub fn is_successful(&self) -> bool {
        self.converged
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfg::{BlockId, CallKind, CallSite};
    use crate::workspace::{ModuleTypeInfo, WorkspaceTypeEnvironment};
    use beacon_constraint::ConstraintGenContext;
    use beacon_parser::ScopeId;
    use beacon_parser::{Scope, ScopeKind, Symbol, SymbolKind, SymbolTable};
    use rustc_hash::FxHashMap;
    use std::sync::Arc;
    use url::Url;

    #[test]
    fn test_max_iterations_setting() {
        let workspace_cfg = WorkspaceCFG::new();
        let mut workspace_env = WorkspaceTypeEnvironment::new();

        let resolver = CrossModuleTypeResolver::new(&workspace_cfg, &mut workspace_env).with_max_iterations(20);

        assert_eq!(resolver.max_iterations, 20);
    }

    #[test]
    fn test_propagation_with_empty_workspace() {
        let workspace_cfg = WorkspaceCFG::new();
        let mut workspace_env = WorkspaceTypeEnvironment::new();

        let mut resolver = CrossModuleTypeResolver::new(&workspace_cfg, &mut workspace_env);
        let result = resolver.propagate_types().unwrap();

        assert!(result.converged);
        assert_eq!(result.iterations, 1);
        assert_eq!(result.constraints_generated, 0);
    }

    #[test]
    fn test_propagation_result_success() {
        let result = PropagationResult { iterations: 3, converged: true, constraints_generated: 10 };
        assert!(result.is_successful());
    }

    #[test]
    fn test_propagation_result_failure() {
        let result = PropagationResult { iterations: 10, converged: false, constraints_generated: 100 };
        assert!(!result.is_successful());
    }

    #[test]
    fn test_cross_module_call_detection() {
        let mut workspace_cfg = WorkspaceCFG::new();

        let uri_a = Url::parse("file:///module_a.py").unwrap();
        let uri_b = Url::parse("file:///module_b.py").unwrap();

        let func_a = FunctionId::new(uri_a, ScopeId::from_raw(1), "func_a".to_string());
        let func_b = FunctionId::new(uri_b, ScopeId::from_raw(1), "func_b".to_string());

        let call_site = CallSite::new(BlockId(0), 0, Some(func_b), CallKind::Direct, 10, 5);

        workspace_cfg.call_graph_mut().add_call_site(func_a.clone(), &call_site);

        let call_sites = workspace_cfg.call_graph().get_call_sites(&func_a);
        assert!(call_sites.is_some());
        assert_eq!(call_sites.unwrap().len(), 1);

        let callee = &call_sites.unwrap()[0].receiver;
        assert!(callee.is_some());
        assert_ne!(callee.as_ref().unwrap().uri, func_a.uri);
    }

    #[test]
    fn test_resolve_cross_module_call_extracts_function_type() {
        let uri_a = Url::parse("file:///module_a.py").unwrap();
        let uri_b = Url::parse("file:///module_b.py").unwrap();

        let func_a = FunctionId::new(uri_a, ScopeId::from_raw(1), "caller".to_string());
        let func_b = FunctionId::new(uri_b.clone(), ScopeId::from_raw(2), "callee".to_string());

        let mut symbol_table_b = SymbolTable::new();
        let scope_id_b = ScopeId::from_raw(2);

        let mut scope_b = Scope {
            id: scope_id_b,
            kind: ScopeKind::Function,
            parent: Some(symbol_table_b.root_scope),
            symbols: FxHashMap::default(),
            children: vec![],
            start_byte: 0,
            end_byte: 100,
        };

        scope_b.symbols.insert(
            "callee".to_string(),
            Symbol {
                name: "callee".to_string(),
                kind: SymbolKind::Function,
                line: 10,
                col: 5,
                end_col: 11,
                scope_id: scope_id_b,
                docstring: None,
                references: vec![],
            },
        );

        symbol_table_b.scopes.insert(scope_id_b, scope_b);

        let ctx_b = ConstraintGenContext::new();
        let mut type_map_b = FxHashMap::default();
        let mut position_map_b = FxHashMap::default();

        position_map_b.insert((10, 5), 100);
        type_map_b.insert(100, Type::fun(vec![("x".to_string(), Type::int())], Type::string()));

        let constraint_result_b = beacon_constraint::ConstraintResult(
            beacon_constraint::ConstraintSet { constraints: vec![] },
            type_map_b,
            position_map_b,
            FxHashMap::default(),
            rustc_hash::FxHashSet::default(),
            ctx_b.class_registry,
            FxHashMap::default(),
            FxHashMap::default(),
            ctx_b.typevar_registry,
        );

        let module_info_b = ModuleTypeInfo::new(constraint_result_b, Arc::new(symbol_table_b), "".to_string());

        let workspace_cfg = WorkspaceCFG::new();
        let mut workspace_env = WorkspaceTypeEnvironment::new();
        workspace_env.add_module(uri_b, module_info_b);

        let mut resolver = CrossModuleTypeResolver::new(&workspace_cfg, &mut workspace_env);
        let changed = resolver.resolve_cross_module_call(&func_a, &func_b).unwrap();

        assert!(changed);

        let cached_type = workspace_env.get_function_type(&func_b);
        assert!(cached_type.is_some());

        let type_scheme = cached_type.unwrap();
        match &type_scheme.ty {
            Type::Fun(params, ret) => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].0, "x");
                assert_eq!(params[0].1, Type::int());
                assert_eq!(**ret, Type::string());
            }
            _ => panic!("Expected function type, got {:?}", type_scheme.ty),
        }
    }

    #[test]
    fn test_resolve_cross_module_call_missing_module_returns_false() {
        let uri_a = Url::parse("file:///module_a.py").unwrap();
        let uri_b = Url::parse("file:///module_b.py").unwrap();

        let func_a = FunctionId::new(uri_a, ScopeId::from_raw(1), "caller".to_string());
        let func_b = FunctionId::new(uri_b, ScopeId::from_raw(2), "callee".to_string());

        let workspace_cfg = WorkspaceCFG::new();
        let mut workspace_env = WorkspaceTypeEnvironment::new();

        let mut resolver = CrossModuleTypeResolver::new(&workspace_cfg, &mut workspace_env);
        let changed = resolver.resolve_cross_module_call(&func_a, &func_b).unwrap();

        assert!(!changed);
    }

    #[test]
    fn test_resolve_cross_module_call_cached_type_returns_false() {
        use crate::workspace::WorkspaceTypeEnvironment;

        let uri_a = Url::parse("file:///module_a.py").unwrap();
        let uri_b = Url::parse("file:///module_b.py").unwrap();

        let func_a = FunctionId::new(uri_a, ScopeId::from_raw(1), "caller".to_string());
        let func_b = FunctionId::new(uri_b, ScopeId::from_raw(2), "callee".to_string());

        let workspace_cfg = WorkspaceCFG::new();
        let mut workspace_env = WorkspaceTypeEnvironment::new();

        workspace_env.set_function_type(func_b.clone(), TypeScheme::mono(Type::int()));

        let mut resolver = CrossModuleTypeResolver::new(&workspace_cfg, &mut workspace_env);
        let changed = resolver.resolve_cross_module_call(&func_a, &func_b).unwrap();

        assert!(!changed);
    }
}
