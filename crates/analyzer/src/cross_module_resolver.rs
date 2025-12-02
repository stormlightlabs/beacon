//! Cross-module type resolution for transitive type propagation
//!
//! Resolves types across module boundaries using the call graph and workspace type environment.
//! Implements iterative type propagation to handle mutually recursive functions.

use beacon_constraint::ConstraintSet;
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
            if let Some(call_sites) = call_graph.get_call_sites(function_id) {
                for call_site in call_sites {
                    if let Some(callee) = &call_site.receiver {
                        if callee.uri != function_id.uri {
                            if self.resolve_cross_module_call(function_id, callee)? {
                                changed = true;
                                *constraints_generated += 1;
                            }
                        }
                    }
                }
            }
        }

        Ok(changed)
    }

    /// Resolve a cross-module function call
    ///
    /// Attempts to resolve the callee's type and propagate it to the caller.
    ///
    /// # Current Implementation
    ///
    /// Currently caches a placeholder `Type::any()` for all cross-module functions to enable
    /// convergence of the fixed-point iteration. The actual type extraction requires:
    ///
    /// 1. **Symbol-to-Node Tracking**: Map FunctionId to AST node IDs during constraint generation
    /// 2. **Type Extraction**: Look up the function's type in `module_info.constraint_result.1`
    /// 3. **Type Propagation**: Use `generate_cross_module_constraints()` to create Call constraints
    ///
    /// # Future Work
    ///
    /// Once call sites track node IDs (via `CallSite::with_nodes()`), the generated constraints
    /// will properly unify argument and return types across module boundaries.
    fn resolve_cross_module_call(&mut self, _caller: &FunctionId, callee: &FunctionId) -> Result<bool> {
        if let Some(_callee_type) = self.workspace_env.get_function_type(callee) {
            // Type already cached, no changes occurred
            Ok(false)
        } else {
            if let Some(_module_info) = self.workspace_env.get_module(&callee.uri) {
                // FIXME: Extract actual function type from module_info.constraint_result
                // For now, cache a placeholder type to allow fixed-point iteration to converge
                let type_scheme = TypeScheme::mono(Type::any());
                self.workspace_env.set_function_type(callee.clone(), type_scheme);
                Ok(true) // Indicate that a type was newly cached
            } else {
                // Module not found in workspace, no changes
                Ok(false)
            }
        }
    }

    /// Generate additional constraints for cross-module calls
    ///
    /// Creates Call constraints that unify argument types with parameter types and propagate return types back to call sites.
    /// This method requires CallSite node IDs to be populated via CallSite::with_nodes().
    pub fn generate_cross_module_constraints(&self) -> Result<ConstraintSet> {
        use beacon_constraint::{Constraint, Span};

        let call_graph = self.workspace_cfg.call_graph();
        let mut constraints = Vec::new();

        // Iterate through all functions in the call graph
        for (caller_fn, call_sites) in call_graph.all_call_sites() {
            for call_site in call_sites {
                if let Some(callee_fn) = &call_site.receiver {
                    if callee_fn.uri != caller_fn.uri {
                        if let Some(callee_type_scheme) = self.workspace_env.get_function_type(callee_fn) {
                            if let Some(caller_module) = self.workspace_env.get_module(&caller_fn.uri) {
                                if let (Some(call_node_id), Some(result_node_id)) =
                                    (call_site.call_node_id, call_site.result_node_id)
                                {
                                    let span = Span::new(call_site.line, call_site.col);

                                    let mut pos_args = Vec::new();
                                    for arg_node_id in &call_site.arg_node_ids {
                                        if let Some(arg_type) = caller_module.constraint_result.1.get(arg_node_id) {
                                            pos_args.push((arg_type.clone(), span));
                                        }
                                    }

                                    let mut kw_args = Vec::new();
                                    for (name, kwarg_node_id) in &call_site.kwarg_node_ids {
                                        if let Some(arg_type) = caller_module.constraint_result.1.get(kwarg_node_id) {
                                            kw_args.push((name.clone(), arg_type.clone(), span));
                                        }
                                    }

                                    let result_type = caller_module
                                        .constraint_result
                                        .1
                                        .get(&result_node_id)
                                        .cloned()
                                        .unwrap_or_else(Type::any);

                                    let call_type = caller_module
                                        .constraint_result
                                        .1
                                        .get(&call_node_id)
                                        .cloned()
                                        .unwrap_or_else(Type::any);

                                    constraints.push(Constraint::Call(
                                        call_type.clone(),
                                        pos_args,
                                        kw_args,
                                        result_type,
                                        span,
                                    ));

                                    constraints.push(Constraint::Equal(call_type, callee_type_scheme.ty.clone(), span));
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(ConstraintSet { constraints })
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
    use beacon_parser::ScopeId;
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

        let func_a = FunctionId::new(uri_a.clone(), ScopeId::from_raw(1), "func_a".to_string());
        let func_b = FunctionId::new(uri_b.clone(), ScopeId::from_raw(1), "func_b".to_string());

        let call_site = CallSite::new(BlockId(0), 0, Some(func_b.clone()), CallKind::Direct, 10, 5);

        workspace_cfg.call_graph_mut().add_call_site(func_a.clone(), call_site);

        let call_sites = workspace_cfg.call_graph().get_call_sites(&func_a);
        assert!(call_sites.is_some());
        assert_eq!(call_sites.unwrap().len(), 1);

        let callee = &call_sites.unwrap()[0].receiver;
        assert!(callee.is_some());
        assert_ne!(callee.as_ref().unwrap().uri, func_a.uri);
    }
}
