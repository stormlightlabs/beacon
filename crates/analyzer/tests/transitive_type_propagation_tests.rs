//! Integration tests for transitive type propagation across module boundaries

use beacon_analyzer::{
    BlockId, CallSite, CrossModuleTypeResolver, FunctionId, ModuleTypeInfo, WorkspaceCFG, WorkspaceTypeEnvironment,
    WorkspaceTypeEnvironmentBuilder, cfg::CallKind,
};
use beacon_constraint::{ConstraintGenContext, ConstraintSet};
use beacon_parser::{ScopeId, SymbolTable};
use std::sync::Arc;
use url::Url;

fn test_uri(path: &str) -> Url {
    Url::parse(&format!("file:///{}", path)).unwrap()
}

#[test]
fn test_workspace_type_environment_basic() {
    let env = WorkspaceTypeEnvironment::new();
    assert_eq!(env.modules().len(), 0);
}

#[test]
fn test_simple_cross_module_call() {
    let uri_a = test_uri("module_a.py");
    let uri_b = test_uri("module_b.py");

    let symbol_table_a = SymbolTable::new();
    let symbol_table_b = SymbolTable::new();

    let mut workspace_cfg = WorkspaceCFG::new();

    let func_a_add = FunctionId::new(uri_a.clone(), ScopeId::from_raw(1), "add".to_string());
    let func_b_calculate = FunctionId::new(uri_b.clone(), ScopeId::from_raw(1), "calculate".to_string());

    let call_site = CallSite::new(BlockId(1), 0, Some(func_a_add.clone()), CallKind::Direct, 5, 10);
    workspace_cfg
        .call_graph_mut()
        .add_call_site(func_b_calculate.clone(), call_site);

    let ctx_a = ConstraintGenContext::new();
    let constraints_a = beacon_constraint::ConstraintResult(
        ConstraintSet { constraints: vec![] },
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_a.class_registry,
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_a.typevar_registry,
    );

    let ctx_b = ConstraintGenContext::new();
    let constraints_b = beacon_constraint::ConstraintResult(
        ConstraintSet { constraints: vec![] },
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_b.class_registry,
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_b.typevar_registry,
    );

    let mut workspace_env = WorkspaceTypeEnvironmentBuilder::new()
        .add_module(
            uri_a.clone(),
            ModuleTypeInfo::new(constraints_a, Arc::new(symbol_table_a), "".to_string()),
        )
        .add_module(
            uri_b.clone(),
            ModuleTypeInfo::new(constraints_b, Arc::new(symbol_table_b), "".to_string()),
        )
        .build();

    workspace_env.add_import(uri_b.clone(), "add".to_string(), uri_a.clone(), ScopeId::from_raw(1));

    let resolved = workspace_env.resolve_import(&uri_b, "add");
    assert!(resolved.is_some());
    assert_eq!(resolved.unwrap().0, uri_a);

    let mut resolver = CrossModuleTypeResolver::new(&workspace_cfg, &mut workspace_env);
    let result = resolver.propagate_types().expect("Type propagation failed");

    assert!(result.converged);
}

#[test]
fn test_mutual_recursion_across_modules() {
    let uri_a = test_uri("module_a.py");
    let uri_b = test_uri("module_b.py");

    let symbol_table_a = SymbolTable::new();
    let symbol_table_b = SymbolTable::new();

    let mut workspace_cfg = WorkspaceCFG::new();

    let func_a_is_even = FunctionId::new(uri_a.clone(), ScopeId::from_raw(1), "is_even".to_string());
    let func_b_is_odd = FunctionId::new(uri_b.clone(), ScopeId::from_raw(1), "is_odd".to_string());

    workspace_cfg.call_graph_mut().add_call_site(
        func_a_is_even.clone(),
        CallSite::new(BlockId(1), 0, Some(func_b_is_odd.clone()), CallKind::Direct, 5, 10),
    );

    workspace_cfg.call_graph_mut().add_call_site(
        func_b_is_odd.clone(),
        CallSite::new(BlockId(1), 0, Some(func_a_is_even.clone()), CallKind::Direct, 5, 10),
    );

    let sccs = workspace_cfg.call_graph().strongly_connected_components();
    let cycle_detected = sccs
        .iter()
        .any(|scc| scc.len() == 2 && scc.contains(&func_a_is_even) && scc.contains(&func_b_is_odd));
    assert!(cycle_detected, "SCC detection should find mutual recursion");

    let ctx_a = ConstraintGenContext::new();
    let constraints_a = beacon_constraint::ConstraintResult(
        ConstraintSet { constraints: vec![] },
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_a.class_registry,
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_a.typevar_registry,
    );

    let ctx_b = ConstraintGenContext::new();
    let constraints_b = beacon_constraint::ConstraintResult(
        ConstraintSet { constraints: vec![] },
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_b.class_registry,
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_b.typevar_registry,
    );

    let mut workspace_env = WorkspaceTypeEnvironmentBuilder::new()
        .add_module(
            uri_a.clone(),
            ModuleTypeInfo::new(constraints_a, Arc::new(symbol_table_a), "".to_string()),
        )
        .add_module(
            uri_b.clone(),
            ModuleTypeInfo::new(constraints_b, Arc::new(symbol_table_b), "".to_string()),
        )
        .build();

    workspace_env.add_import(uri_a.clone(), "is_odd".to_string(), uri_b.clone(), ScopeId::from_raw(1));
    workspace_env.add_import(
        uri_b.clone(),
        "is_even".to_string(),
        uri_a.clone(),
        ScopeId::from_raw(1),
    );

    let mut resolver = CrossModuleTypeResolver::new(&workspace_cfg, &mut workspace_env).with_max_iterations(20);

    let result = resolver.propagate_types().expect("Type propagation failed");

    assert!(
        result.converged,
        "Propagation should converge even with mutual recursion"
    );
}

#[test]
fn test_transitive_type_flow() {
    let uri_a = test_uri("module_a.py");
    let uri_b = test_uri("module_b.py");
    let uri_c = test_uri("module_c.py");

    let symbol_table_a = SymbolTable::new();
    let symbol_table_b = SymbolTable::new();
    let symbol_table_c = SymbolTable::new();

    let mut workspace_cfg = WorkspaceCFG::new();

    let func_a_get_int = FunctionId::new(uri_a.clone(), ScopeId::from_raw(1), "get_int".to_string());
    let func_b_get_number = FunctionId::new(uri_b.clone(), ScopeId::from_raw(1), "get_number".to_string());
    let func_c_compute = FunctionId::new(uri_c.clone(), ScopeId::from_raw(1), "compute".to_string());

    workspace_cfg.call_graph_mut().add_call_site(
        func_b_get_number.clone(),
        CallSite::new(BlockId(1), 0, Some(func_a_get_int.clone()), CallKind::Direct, 3, 10),
    );
    workspace_cfg.call_graph_mut().add_call_site(
        func_c_compute.clone(),
        CallSite::new(BlockId(1), 0, Some(func_b_get_number.clone()), CallKind::Direct, 3, 10),
    );

    let callees_b = workspace_cfg.call_graph().get_callees(&func_b_get_number);
    assert_eq!(callees_b.len(), 1);
    assert_eq!(callees_b[0], func_a_get_int);

    let callees_c = workspace_cfg.call_graph().get_callees(&func_c_compute);
    assert_eq!(callees_c.len(), 1);
    assert_eq!(callees_c[0], func_b_get_number);

    let ctx_a = ConstraintGenContext::new();
    let constraints_a = beacon_constraint::ConstraintResult(
        ConstraintSet { constraints: vec![] },
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_a.class_registry,
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_a.typevar_registry,
    );

    let ctx_b = ConstraintGenContext::new();
    let constraints_b = beacon_constraint::ConstraintResult(
        ConstraintSet { constraints: vec![] },
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_b.class_registry,
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_b.typevar_registry,
    );

    let ctx_c = ConstraintGenContext::new();
    let constraints_c = beacon_constraint::ConstraintResult(
        ConstraintSet { constraints: vec![] },
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_c.class_registry,
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_c.typevar_registry,
    );

    let mut workspace_env = WorkspaceTypeEnvironmentBuilder::new()
        .add_module(
            uri_a.clone(),
            ModuleTypeInfo::new(constraints_a, Arc::new(symbol_table_a), "".to_string()),
        )
        .add_module(
            uri_b.clone(),
            ModuleTypeInfo::new(constraints_b, Arc::new(symbol_table_b), "".to_string()),
        )
        .add_module(
            uri_c.clone(),
            ModuleTypeInfo::new(constraints_c, Arc::new(symbol_table_c), "".to_string()),
        )
        .build();

    workspace_env.add_import(
        uri_b.clone(),
        "get_int".to_string(),
        uri_a.clone(),
        ScopeId::from_raw(1),
    );
    workspace_env.add_import(
        uri_c.clone(),
        "get_number".to_string(),
        uri_b.clone(),
        ScopeId::from_raw(1),
    );

    let mut resolver = CrossModuleTypeResolver::new(&workspace_cfg, &mut workspace_env);
    let result = resolver.propagate_types().expect("Type propagation failed");

    assert!(result.converged);
}

#[test]
fn test_max_iterations_limit() {
    let uri_a = test_uri("module_a.py");
    let symbol_table_a = SymbolTable::new();

    let workspace_cfg = WorkspaceCFG::new();

    let ctx_a = ConstraintGenContext::new();
    let constraints_a = beacon_constraint::ConstraintResult(
        ConstraintSet { constraints: vec![] },
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_a.class_registry,
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_a.typevar_registry,
    );

    let mut workspace_env = WorkspaceTypeEnvironmentBuilder::new()
        .add_module(
            uri_a.clone(),
            ModuleTypeInfo::new(constraints_a, Arc::new(symbol_table_a), "".to_string()),
        )
        .build();

    let mut resolver = CrossModuleTypeResolver::new(&workspace_cfg, &mut workspace_env).with_max_iterations(1);

    let result = resolver.propagate_types().expect("Type propagation failed");

    assert_eq!(result.iterations, 1);
    assert!(result.converged);
}

#[test]
fn test_function_type_caching() {
    let uri_a = test_uri("module_a.py");
    let symbol_table_a = SymbolTable::new();

    let ctx_a = ConstraintGenContext::new();
    let constraints_a = beacon_constraint::ConstraintResult(
        ConstraintSet { constraints: vec![] },
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_a.class_registry,
        rustc_hash::FxHashMap::default(),
        rustc_hash::FxHashMap::default(),
        ctx_a.typevar_registry,
    );

    let mut workspace_env = WorkspaceTypeEnvironmentBuilder::new()
        .add_module(
            uri_a.clone(),
            ModuleTypeInfo::new(constraints_a, Arc::new(symbol_table_a), "".to_string()),
        )
        .build();

    let func_id = FunctionId::new(uri_a.clone(), ScopeId::from_raw(1), "identity".to_string());
    let type_scheme = beacon_core::TypeScheme::mono(beacon_core::Type::int());
    workspace_env.set_function_type(func_id.clone(), type_scheme.clone());

    let cached_type = workspace_env.get_function_type(&func_id);
    assert!(cached_type.is_some());
    assert_eq!(cached_type.unwrap().ty, beacon_core::Type::int());

    workspace_env.clear_function_types();
    assert!(workspace_env.get_function_type(&func_id).is_none());
}
