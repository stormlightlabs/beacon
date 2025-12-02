use beacon_analyzer::{
    BlockId, CallGraph, CallResolver, CallSite, CfgBuilder, ControlFlowGraph, FunctionId, ModuleCFG, ModuleCFGBuilder,
    WorkspaceCFG, cfg::CallKind,
};
use beacon_parser::{AstNode, LiteralValue, ScopeId, ScopeKind, Symbol, SymbolKind, SymbolTable};
use url::Url;

fn test_uri(path: &str) -> Url {
    Url::parse(&format!("file:///{}", path)).unwrap()
}

#[test]
fn test_function_id_creation() {
    let uri = test_uri("test.py");
    let scope_id = ScopeId::from_raw(1);
    let func_id = FunctionId::new(uri.clone(), scope_id, "test_func".to_string());
    assert_eq!(func_id.uri, uri);
    assert_eq!(func_id.scope_id, scope_id);
    assert_eq!(func_id.name, "test_func");
}

#[test]
fn test_function_id_equality() {
    let uri = test_uri("test.py");
    let scope_id = ScopeId::from_raw(1);

    let func1 = FunctionId::new(uri.clone(), scope_id, "foo".to_string());
    let func2 = FunctionId::new(uri.clone(), scope_id, "foo".to_string());
    let func3 = FunctionId::new(uri.clone(), ScopeId::from_raw(2), "bar".to_string());

    assert_eq!(func1, func2);
    assert_ne!(func1, func3);
}

#[test]
fn test_call_site_creation() {
    let block_id = BlockId(5);
    let callee = FunctionId::new(test_uri("test.py"), ScopeId::from_raw(1), "foo".to_string());

    let call_site = CallSite::new(block_id, 10, Some(callee.clone()), CallKind::Direct, 42, 15);

    assert_eq!(call_site.block_id, block_id);
    assert_eq!(call_site.stmt_index, 10);
    assert_eq!(call_site.receiver, Some(callee));
    assert_eq!(call_site.kind, CallKind::Direct);
    assert_eq!(call_site.line, 42);
    assert_eq!(call_site.col, 15);
}

#[test]
fn test_call_site_unresolved_callee() {
    let call_site = CallSite::new(BlockId(1), 5, None, CallKind::Dynamic, 10, 5);

    assert!(call_site.receiver.is_none());
    assert_eq!(call_site.kind, CallKind::Dynamic);
}

#[test]
fn test_call_graph_add_call_site() {
    let mut call_graph = CallGraph::new();

    let caller = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "caller".to_string());
    let callee = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "callee".to_string());

    let call_site = CallSite::new(BlockId(1), 0, Some(callee.clone()), CallKind::Direct, 10, 5);

    call_graph.add_call_site(caller.clone(), call_site);

    let sites = call_graph.get_call_sites(&caller).unwrap();
    assert_eq!(sites.len(), 1);
    assert_eq!(sites[0].receiver, Some(callee.clone()));

    let callers = call_graph.get_callers(&callee).unwrap();
    assert_eq!(callers.len(), 1);
    assert_eq!(callers[0], caller);
}

#[test]
fn test_call_graph_multiple_call_sites() {
    let mut call_graph = CallGraph::new();

    let caller = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "caller".to_string());
    let callee1 = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "callee1".to_string());
    let callee2 = FunctionId::new(test_uri("c.py"), ScopeId::from_raw(3), "callee2".to_string());

    let call1 = CallSite::new(BlockId(1), 0, Some(callee1.clone()), CallKind::Direct, 10, 5);
    let call2 = CallSite::new(BlockId(2), 1, Some(callee2.clone()), CallKind::Method, 15, 8);

    call_graph.add_call_site(caller.clone(), call1);
    call_graph.add_call_site(caller.clone(), call2);

    let sites = call_graph.get_call_sites(&caller).unwrap();
    assert_eq!(sites.len(), 2);

    let callees = call_graph.get_callees(&caller);
    assert_eq!(callees.len(), 2);
    assert!(callees.contains(&callee1));
    assert!(callees.contains(&callee2));
}

#[test]
fn test_call_graph_get_callees_with_unresolved() {
    let mut call_graph = CallGraph::new();

    let caller = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "caller".to_string());
    let callee = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "callee".to_string());

    call_graph.add_call_site(
        caller.clone(),
        CallSite::new(BlockId(1), 0, Some(callee.clone()), CallKind::Direct, 10, 5),
    );
    call_graph.add_call_site(
        caller.clone(),
        CallSite::new(BlockId(2), 1, None, CallKind::Dynamic, 15, 8),
    );

    let callees = call_graph.get_callees(&caller);
    assert_eq!(callees.len(), 1);
    assert_eq!(callees[0], callee);
}

#[test]
fn test_call_graph_reachable_functions_simple_chain() {
    let mut call_graph = CallGraph::new();

    let func1 = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "func1".to_string());
    let func2 = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "func2".to_string());
    let func3 = FunctionId::new(test_uri("c.py"), ScopeId::from_raw(3), "func3".to_string());

    call_graph.add_call_site(
        func1.clone(),
        CallSite::new(BlockId(1), 0, Some(func2.clone()), CallKind::Direct, 10, 5),
    );
    call_graph.add_call_site(
        func2.clone(),
        CallSite::new(BlockId(1), 0, Some(func3.clone()), CallKind::Direct, 20, 5),
    );

    let reachable = call_graph.reachable_functions(&[func1.clone()]);

    assert_eq!(reachable.len(), 3);
    assert!(reachable.contains(&func1));
    assert!(reachable.contains(&func2));
    assert!(reachable.contains(&func3));
}

#[test]
fn test_call_graph_reachable_functions_diamond() {
    let mut call_graph = CallGraph::new();

    let entry = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "entry".to_string());
    let left = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "left".to_string());
    let right = FunctionId::new(test_uri("c.py"), ScopeId::from_raw(3), "right".to_string());
    let bottom = FunctionId::new(test_uri("d.py"), ScopeId::from_raw(4), "bottom".to_string());

    call_graph.add_call_site(
        entry.clone(),
        CallSite::new(BlockId(1), 0, Some(left.clone()), CallKind::Direct, 10, 5),
    );
    call_graph.add_call_site(
        entry.clone(),
        CallSite::new(BlockId(2), 1, Some(right.clone()), CallKind::Direct, 15, 5),
    );
    call_graph.add_call_site(
        left.clone(),
        CallSite::new(BlockId(1), 0, Some(bottom.clone()), CallKind::Direct, 20, 5),
    );
    call_graph.add_call_site(
        right.clone(),
        CallSite::new(BlockId(1), 0, Some(bottom.clone()), CallKind::Direct, 25, 5),
    );

    let reachable = call_graph.reachable_functions(&[entry.clone()]);

    assert_eq!(reachable.len(), 4);
    assert!(reachable.contains(&entry));
    assert!(reachable.contains(&left));
    assert!(reachable.contains(&right));
    assert!(reachable.contains(&bottom));
}

#[test]
fn test_call_graph_reachable_functions_cycle() {
    let mut call_graph = CallGraph::new();

    let func1 = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "func1".to_string());
    let func2 = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "func2".to_string());
    let func3 = FunctionId::new(test_uri("c.py"), ScopeId::from_raw(3), "func3".to_string());

    call_graph.add_call_site(
        func1.clone(),
        CallSite::new(BlockId(1), 0, Some(func2.clone()), CallKind::Direct, 10, 5),
    );
    call_graph.add_call_site(
        func2.clone(),
        CallSite::new(BlockId(1), 0, Some(func3.clone()), CallKind::Direct, 20, 5),
    );
    call_graph.add_call_site(
        func3.clone(),
        CallSite::new(BlockId(1), 0, Some(func1.clone()), CallKind::Direct, 30, 5),
    );

    let reachable = call_graph.reachable_functions(&[func1.clone()]);

    assert_eq!(reachable.len(), 3);
    assert!(reachable.contains(&func1));
    assert!(reachable.contains(&func2));
    assert!(reachable.contains(&func3));
}

#[test]
fn test_call_graph_unreachable_functions() {
    let mut call_graph = CallGraph::new();

    let func1 = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "func1".to_string());
    let func2 = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "func2".to_string());
    let func3 = FunctionId::new(test_uri("c.py"), ScopeId::from_raw(3), "func3".to_string());

    call_graph.add_call_site(
        func1.clone(),
        CallSite::new(BlockId(1), 0, Some(func2.clone()), CallKind::Direct, 10, 5),
    );

    let reachable = call_graph.reachable_functions(&[func1.clone()]);

    assert_eq!(reachable.len(), 2);
    assert!(reachable.contains(&func1));
    assert!(reachable.contains(&func2));
    assert!(!reachable.contains(&func3));
}

#[test]
fn test_module_cfg_creation() {
    let uri = test_uri("test.py");
    let module_cfg = ModuleCFG::new(uri.clone(), "test".to_string());

    assert_eq!(module_cfg.uri, uri);
    assert_eq!(module_cfg.module_name, "test");
    assert_eq!(module_cfg.function_cfgs.len(), 0);
    assert_eq!(module_cfg.call_sites.len(), 0);
}

#[test]
fn test_module_cfg_add_function() {
    let uri = test_uri("test.py");
    let mut module_cfg = ModuleCFG::new(uri, "test".to_string());

    let cfg = ControlFlowGraph::new();
    let scope_id = ScopeId::from_raw(1);

    module_cfg.add_function_cfg(scope_id, "test_func".to_string(), cfg);

    assert_eq!(module_cfg.function_cfgs.len(), 1);
    assert!(module_cfg.function_cfgs.contains_key(&scope_id));
}

#[test]
fn test_module_cfg_add_call_site() {
    let uri = test_uri("test.py");
    let mut module_cfg = ModuleCFG::new(uri.clone(), "test".to_string());

    let callee = FunctionId::new(test_uri("other.py"), ScopeId::from_raw(2), "foo".to_string());
    let call_site = CallSite::new(BlockId(1), 0, Some(callee), CallKind::Direct, 10, 5);

    module_cfg.add_call_site(call_site);

    assert_eq!(module_cfg.call_sites.len(), 1);
}

#[test]
fn test_workspace_cfg_creation() {
    let workspace_cfg = WorkspaceCFG::new();
    assert_eq!(workspace_cfg.modules().len(), 0);
    assert_eq!(workspace_cfg.entry_point_fns().len(), 0);
}

#[test]
fn test_workspace_cfg_add_module() {
    let mut workspace_cfg = WorkspaceCFG::new();

    let uri = test_uri("test.py");
    let module_cfg = ModuleCFG::new(uri.clone(), "test".to_string());

    workspace_cfg.add_module(module_cfg);

    assert_eq!(workspace_cfg.modules().len(), 1);
    assert!(workspace_cfg.get_module(&uri).is_some());
}

#[test]
fn test_workspace_cfg_add_entry_point() {
    let mut workspace_cfg = WorkspaceCFG::new();

    let entry = FunctionId::new(test_uri("main.py"), ScopeId::from_raw(1), "main".to_string());
    workspace_cfg.add_entry_point(entry.clone());

    let entry_points = workspace_cfg.entry_points();
    assert_eq!(entry_points.len(), 1);
    assert_eq!(entry_points[0], entry);
}

#[test]
fn test_workspace_cfg_reachable_functions() {
    let mut workspace_cfg = WorkspaceCFG::new();

    let func1 = FunctionId::new(test_uri("a.py"), ScopeId::from_raw(1), "func1".to_string());
    let func2 = FunctionId::new(test_uri("b.py"), ScopeId::from_raw(2), "func2".to_string());

    workspace_cfg.add_entry_point(func1.clone());

    let call_site = CallSite::new(BlockId(1), 0, Some(func2.clone()), CallKind::Direct, 10, 5);
    workspace_cfg.call_graph_mut().add_call_site(func1.clone(), call_site);

    let reachable = workspace_cfg.reachable_functions();
    assert_eq!(reachable.len(), 2);
    assert!(reachable.contains(&func1));
    assert!(reachable.contains(&func2));
}

#[test]
fn test_workspace_cfg_cross_module_reachable_blocks() {
    let mut workspace_cfg = WorkspaceCFG::new();

    let uri1 = test_uri("a.py");
    let uri2 = test_uri("b.py");

    let func1 = FunctionId::new(uri1.clone(), ScopeId::from_raw(1), "func1".to_string());
    let func2 = FunctionId::new(uri2.clone(), ScopeId::from_raw(2), "func2".to_string());

    let mut module1 = ModuleCFG::new(uri1.clone(), "a".to_string());
    let cfg1 = ControlFlowGraph::new();
    module1.add_function_cfg(ScopeId::from_raw(1), "func1".to_string(), cfg1);

    let mut module2 = ModuleCFG::new(uri2.clone(), "b".to_string());
    let cfg2 = ControlFlowGraph::new();
    module2.add_function_cfg(ScopeId::from_raw(2), "func2".to_string(), cfg2);

    workspace_cfg.add_module(module1);
    workspace_cfg.add_module(module2);

    let call_site = CallSite::new(BlockId(1), 0, Some(func2.clone()), CallKind::Direct, 10, 5);
    workspace_cfg.call_graph_mut().add_call_site(func1.clone(), call_site);

    let reachable = workspace_cfg.cross_module_reachable_blocks(&[func1.clone()]);

    assert_eq!(reachable.len(), 2);
    assert!(reachable.contains_key(&func1));
    assert!(reachable.contains_key(&func2));
}

#[test]
fn test_call_extraction_simple_call() {
    let mut builder = CfgBuilder::new();
    let body = vec![AstNode::Call {
        function: Box::new(AstNode::Identifier { name: "foo".to_string(), line: 1, col: 1, end_line: 1, end_col: 4 }),
        args: vec![],
        keywords: vec![],
        line: 1,
        col: 1,
        end_line: 1,
        end_col: 6,
    }];

    builder.build_function(&body);
    let call_sites = builder.call_sites();

    assert_eq!(call_sites.len(), 1);
    assert_eq!(call_sites[0].1, 0);
}

#[test]
fn test_call_extraction_assignment() {
    let mut builder = CfgBuilder::new();
    let body = vec![AstNode::Assignment {
        target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
        value: Box::new(AstNode::Call {
            function: Box::new(AstNode::Identifier {
                name: "foo".to_string(),
                line: 1,
                col: 5,
                end_line: 1,
                end_col: 8,
            }),
            args: vec![],
            keywords: vec![],
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 10,
        }),
        line: 1,
        col: 1,
        end_line: 1,
        end_col: 10,
    }];

    builder.build_function(&body);
    let call_sites = builder.call_sites();

    assert_eq!(call_sites.len(), 1);
}

#[test]
fn test_call_extraction_nested_calls() {
    let mut builder = CfgBuilder::new();
    let body = vec![AstNode::Call {
        function: Box::new(AstNode::Identifier { name: "outer".to_string(), line: 1, col: 1, end_line: 1, end_col: 6 }),
        args: vec![AstNode::Call {
            function: Box::new(AstNode::Identifier {
                name: "inner".to_string(),
                line: 1,
                col: 7,
                end_line: 1,
                end_col: 12,
            }),
            args: vec![],
            keywords: vec![],
            line: 1,
            col: 7,
            end_line: 1,
            end_col: 14,
        }],
        keywords: vec![],
        line: 1,
        col: 1,
        end_line: 1,
        end_col: 15,
    }];

    builder.build_function(&body);
    let call_sites = builder.call_sites();

    assert_eq!(call_sites.len(), 2);
}

#[test]
fn test_call_extraction_return_statement() {
    let mut builder = CfgBuilder::new();
    let body = vec![AstNode::Return {
        value: Some(Box::new(AstNode::Call {
            function: Box::new(AstNode::Identifier {
                name: "foo".to_string(),
                line: 1,
                col: 8,
                end_line: 1,
                end_col: 11,
            }),
            args: vec![],
            keywords: vec![],
            line: 1,
            col: 8,
            end_line: 1,
            end_col: 13,
        })),
        line: 1,
        col: 1,
        end_line: 1,
        end_col: 13,
    }];

    builder.build_function(&body);
    let call_sites = builder.call_sites();

    assert_eq!(call_sites.len(), 1);
}

#[test]
fn test_call_extraction_method_call() {
    let mut builder = CfgBuilder::new();
    let body = vec![AstNode::Call {
        function: Box::new(AstNode::Attribute {
            object: Box::new(AstNode::Identifier { name: "obj".to_string(), line: 1, col: 1, end_line: 1, end_col: 4 }),
            attribute: "method".to_string(),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 11,
        }),
        args: vec![],
        keywords: vec![],
        line: 1,
        col: 1,
        end_line: 1,
        end_col: 13,
    }];

    builder.build_function(&body);
    let call_sites = builder.call_sites();

    assert_eq!(call_sites.len(), 1);
}

#[test]
fn test_call_extraction_in_list_comp() {
    let mut builder = CfgBuilder::new();
    let body = vec![AstNode::Assignment {
        target: Box::new(AstNode::Identifier { name: "result".to_string(), line: 1, col: 1, end_line: 1, end_col: 7 }),
        value: Box::new(AstNode::ListComp {
            element: Box::new(AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "transform".to_string(),
                    line: 1,
                    col: 11,
                    end_line: 1,
                    end_col: 20,
                }),
                args: vec![AstNode::Identifier { name: "x".to_string(), line: 1, col: 21, end_line: 1, end_col: 22 }],
                keywords: vec![],
                line: 1,
                col: 11,
                end_line: 1,
                end_col: 23,
            }),
            generators: vec![],
            line: 1,
            col: 10,
            end_line: 1,
            end_col: 40,
        }),
        line: 1,
        col: 1,
        end_line: 1,
        end_col: 40,
    }];

    builder.build_function(&body);
    let call_sites = builder.call_sites();

    assert_eq!(call_sites.len(), 1);
}

#[test]
fn test_take_call_sites() {
    let mut builder = CfgBuilder::new();
    let body = vec![AstNode::Call {
        function: Box::new(AstNode::Identifier { name: "foo".to_string(), line: 1, col: 1, end_line: 1, end_col: 4 }),
        args: vec![],
        keywords: vec![],
        line: 1,
        col: 1,
        end_line: 1,
        end_col: 6,
    }];

    builder.build_function(&body);
    let (cfg, call_sites) = builder.take_call_sites();

    assert_eq!(call_sites.len(), 1);
    assert!(!cfg.blocks.is_empty());
}

#[test]
fn test_call_resolver_direct_call() {
    let mut symbol_table = beacon_parser::SymbolTable::new();
    let root_scope = symbol_table.root_scope;

    let func_scope = symbol_table.create_scope(ScopeKind::Function, root_scope, 10, 100);
    symbol_table.add_symbol(
        root_scope,
        Symbol {
            name: "my_func".to_string(),
            kind: SymbolKind::Function,
            line: 1,
            col: 5,
            scope_id: func_scope,
            docstring: None,
            references: vec![],
        },
    );

    let uri = test_uri("test.py");
    let resolver = CallResolver::new(&symbol_table, &uri, root_scope);

    let call = AstNode::Call {
        function: Box::new(AstNode::Identifier {
            name: "my_func".to_string(),
            line: 5,
            col: 1,
            end_line: 5,
            end_col: 8,
        }),
        args: vec![],
        keywords: vec![],
        line: 5,
        col: 1,
        end_line: 5,
        end_col: 10,
    };

    let (receiver, kind) = resolver.resolve_call(&call);
    assert_eq!(kind, CallKind::Direct);
    assert!(receiver.is_some());
    let func_id = receiver.unwrap();
    assert_eq!(func_id.name, "my_func");
    assert_eq!(func_id.scope_id, func_scope);
}

#[test]
fn test_call_resolver_method_call() {
    let symbol_table = SymbolTable::new();
    let uri = test_uri("test.py");
    let resolver = CallResolver::new(&symbol_table, &uri, symbol_table.root_scope);

    let call = AstNode::Call {
        function: Box::new(AstNode::Attribute {
            object: Box::new(AstNode::Identifier { name: "obj".to_string(), line: 5, col: 1, end_line: 5, end_col: 4 }),
            attribute: "method".to_string(),
            line: 5,
            col: 1,
            end_line: 5,
            end_col: 11,
        }),
        args: vec![],
        keywords: vec![],
        line: 5,
        col: 1,
        end_line: 5,
        end_col: 13,
    };

    let (receiver, kind) = resolver.resolve_call(&call);
    assert_eq!(kind, CallKind::Method);
    assert!(receiver.is_none());
}

#[test]
fn test_call_resolver_unresolved_call() {
    let symbol_table = SymbolTable::new();
    let uri = test_uri("test.py");
    let resolver = CallResolver::new(&symbol_table, &uri, symbol_table.root_scope);

    let call = AstNode::Call {
        function: Box::new(AstNode::Identifier {
            name: "unknown_func".to_string(),
            line: 5,
            col: 1,
            end_line: 5,
            end_col: 12,
        }),
        args: vec![],
        keywords: vec![],
        line: 5,
        col: 1,
        end_line: 5,
        end_col: 14,
    };

    let (receiver, kind) = resolver.resolve_call(&call);
    assert_eq!(kind, CallKind::Direct);
    assert!(receiver.is_none());
}

#[test]
fn test_module_cfg_builder() {
    use beacon_parser::NameResolver;

    let source = r#"
def foo():
    pass

def bar():
    foo()

x = bar()
"#;

    let mut resolver = NameResolver::new(source.to_string());

    let ast = AstNode::Module {
        body: vec![
            AstNode::FunctionDef {
                name: "foo".to_string(),
                args: vec![],
                body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
                return_type: None,
                decorators: vec![],
                is_async: false,
                docstring: None,
                line: 1,
                col: 1,
                end_line: 2,
                end_col: 9,
            },
            AstNode::FunctionDef {
                name: "bar".to_string(),
                args: vec![],
                body: vec![AstNode::Call {
                    function: Box::new(AstNode::Identifier {
                        name: "foo".to_string(),
                        line: 5,
                        col: 5,
                        end_line: 5,
                        end_col: 8,
                    }),
                    args: vec![],
                    keywords: vec![],
                    line: 5,
                    col: 5,
                    end_line: 5,
                    end_col: 10,
                }],
                return_type: None,
                decorators: vec![],
                is_async: false,
                docstring: None,
                line: 4,
                col: 1,
                end_line: 5,
                end_col: 10,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 7,
                    col: 1,
                    end_line: 7,
                    end_col: 2,
                }),
                value: Box::new(AstNode::Call {
                    function: Box::new(AstNode::Identifier {
                        name: "bar".to_string(),
                        line: 7,
                        col: 5,
                        end_line: 7,
                        end_col: 8,
                    }),
                    args: vec![],
                    keywords: vec![],
                    line: 7,
                    col: 5,
                    end_line: 7,
                    end_col: 10,
                }),
                line: 7,
                col: 1,
                end_line: 7,
                end_col: 10,
            },
        ],
        docstring: None,
    };

    resolver.resolve(&ast).expect("Failed to resolve");

    let uri = test_uri("test_module.py");
    let builder = ModuleCFGBuilder::new(&resolver.symbol_table, uri.clone(), "test_module".to_string(), source);
    let module_cfg = builder.build(&ast);

    assert_eq!(module_cfg.uri, uri);
    assert_eq!(module_cfg.module_name, "test_module");

    assert!(!module_cfg.module_init_cfg.blocks.is_empty());
    assert!(!module_cfg.call_sites.is_empty());
}

#[test]
fn test_module_level_cfg_construction() {
    let mut builder = CfgBuilder::new();

    let body = vec![
        AstNode::Import {
            module: "os".to_string(),
            alias: None,
            extra_modules: vec![],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 10,
        },
        AstNode::Assignment {
            target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 2, col: 1, end_line: 2, end_col: 2 }),
            value: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 7,
            }),
            line: 2,
            col: 1,
            end_line: 2,
            end_col: 7,
        },
        AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::Pass { line: 5, col: 5, end_line: 5, end_col: 9 }],
            return_type: None,
            decorators: vec![],
            is_async: false,
            docstring: None,
            line: 4,
            col: 1,
            end_line: 5,
            end_col: 9,
        },
        AstNode::Call {
            function: Box::new(AstNode::Identifier {
                name: "print".to_string(),
                line: 7,
                col: 1,
                end_line: 7,
                end_col: 6,
            }),
            args: vec![AstNode::Identifier { name: "x".to_string(), line: 7, col: 7, end_line: 7, end_col: 8 }],
            keywords: vec![],
            line: 7,
            col: 1,
            end_line: 7,
            end_col: 9,
        },
    ];

    builder.build_module(&body);
    let cfg = builder.build();

    let total_stmts: usize = cfg.blocks.values().map(|b| b.statements.len()).sum();
    assert_eq!(total_stmts, 3);
}

#[test]
fn test_workspace_cfg_with_call_graph() {
    let source_a = r#"
def func_a():
    return 42

result = func_a()
"#;

    let mut resolver_a = beacon_parser::NameResolver::new(source_a.to_string());

    let ast_a = AstNode::Module {
        body: vec![
            AstNode::FunctionDef {
                name: "func_a".to_string(),
                args: vec![],
                body: vec![AstNode::Return {
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(42),
                        line: 2,
                        col: 12,
                        end_line: 2,
                        end_col: 14,
                    })),
                    line: 2,
                    col: 5,
                    end_line: 2,
                    end_col: 14,
                }],
                return_type: None,
                decorators: vec![],
                is_async: false,
                docstring: None,
                line: 1,
                col: 1,
                end_line: 2,
                end_col: 14,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "result".to_string(),
                    line: 4,
                    col: 1,
                    end_line: 4,
                    end_col: 7,
                }),
                value: Box::new(AstNode::Call {
                    function: Box::new(AstNode::Identifier {
                        name: "func_a".to_string(),
                        line: 4,
                        col: 10,
                        end_line: 4,
                        end_col: 16,
                    }),
                    args: vec![],
                    keywords: vec![],
                    line: 4,
                    col: 10,
                    end_line: 4,
                    end_col: 18,
                }),
                line: 4,
                col: 1,
                end_line: 4,
                end_col: 18,
            },
        ],
        docstring: None,
    };

    resolver_a.resolve(&ast_a).expect("Failed to resolve module A");

    let uri_a = test_uri("module_a.py");
    let builder_a = ModuleCFGBuilder::new(
        &resolver_a.symbol_table,
        uri_a.clone(),
        "module_a".to_string(),
        source_a,
    );
    let module_cfg_a = builder_a.build(&ast_a);

    let mut workspace_cfg = WorkspaceCFG::new();
    workspace_cfg.add_module(module_cfg_a);

    assert!(workspace_cfg.get_module(&uri_a).is_some());

    let module_a = workspace_cfg.get_module(&uri_a).unwrap();

    assert!(!module_a.call_sites.is_empty(), "Expected call sites to be captured");

    let func_a_call = module_a
        .call_sites
        .iter()
        .find(|cs| cs.receiver.as_ref().map(|r| r.name.as_str()) == Some("func_a"));

    assert!(func_a_call.is_some(), "Expected to find call to func_a");

    let call = func_a_call.unwrap();
    assert_eq!(call.kind, CallKind::Direct);
    assert!(call.receiver.is_some());

    let receiver = call.receiver.as_ref().unwrap();
    assert_eq!(receiver.name, "func_a");
    assert_eq!(receiver.uri, uri_a);
}

#[test]
fn test_scc_no_cycles() {
    let mut call_graph = CallGraph::new();

    let uri = test_uri("test.py");
    let func_a = FunctionId::new(uri.clone(), ScopeId::from_raw(1), "a".to_string());
    let func_b = FunctionId::new(uri.clone(), ScopeId::from_raw(2), "b".to_string());
    let func_c = FunctionId::new(uri.clone(), ScopeId::from_raw(3), "c".to_string());

    let call_a_to_b = CallSite::new(BlockId(1), 0, Some(func_b.clone()), CallKind::Direct, 1, 1);
    let call_b_to_c = CallSite::new(BlockId(2), 0, Some(func_c.clone()), CallKind::Direct, 2, 1);

    call_graph.add_call_site(func_a.clone(), call_a_to_b);
    call_graph.add_call_site(func_b.clone(), call_b_to_c);

    let sccs = call_graph.strongly_connected_components();
    assert_eq!(sccs.len(), 3);
    assert!(sccs.iter().all(|scc| scc.len() == 1));
    assert!(!call_graph.has_circular_dependencies());
}

#[test]
fn test_scc_simple_cycle() {
    let mut call_graph = CallGraph::new();

    let uri = test_uri("test.py");
    let func_a = FunctionId::new(uri.clone(), ScopeId::from_raw(1), "a".to_string());
    let func_b = FunctionId::new(uri.clone(), ScopeId::from_raw(2), "b".to_string());

    let call_a_to_b = CallSite::new(BlockId(1), 0, Some(func_b.clone()), CallKind::Direct, 1, 1);
    let call_b_to_a = CallSite::new(BlockId(2), 0, Some(func_a.clone()), CallKind::Direct, 2, 1);

    call_graph.add_call_site(func_a.clone(), call_a_to_b);
    call_graph.add_call_site(func_b.clone(), call_b_to_a);

    let sccs = call_graph.strongly_connected_components();
    assert_eq!(sccs.len(), 1);
    assert_eq!(sccs[0].len(), 2);
    assert!(call_graph.has_circular_dependencies());

    let circular_funcs = call_graph.circular_dependency_functions();
    assert_eq!(circular_funcs.len(), 2);
    assert!(circular_funcs.contains(&func_a));
    assert!(circular_funcs.contains(&func_b));
}

#[test]
fn test_scc_self_loop() {
    let mut call_graph = CallGraph::new();

    let uri = test_uri("test.py");
    let func_a = FunctionId::new(uri.clone(), ScopeId::from_raw(1), "a".to_string());

    let call_a_to_a = CallSite::new(BlockId(1), 0, Some(func_a.clone()), CallKind::Direct, 1, 1);

    call_graph.add_call_site(func_a.clone(), call_a_to_a);

    let sccs = call_graph.strongly_connected_components();
    assert_eq!(sccs.len(), 1);
    assert_eq!(sccs[0].len(), 1);
    assert_eq!(sccs[0][0], func_a);
    assert!(!call_graph.has_circular_dependencies());
}

#[test]
fn test_scc_complex_cycle() {
    let mut call_graph = CallGraph::new();

    let uri = test_uri("test.py");
    let func_a = FunctionId::new(uri.clone(), ScopeId::from_raw(1), "a".to_string());
    let func_b = FunctionId::new(uri.clone(), ScopeId::from_raw(2), "b".to_string());
    let func_c = FunctionId::new(uri.clone(), ScopeId::from_raw(3), "c".to_string());
    let func_d = FunctionId::new(uri.clone(), ScopeId::from_raw(4), "d".to_string());
    let func_e = FunctionId::new(uri.clone(), ScopeId::from_raw(5), "e".to_string());

    call_graph.add_call_site(
        func_a.clone(),
        CallSite::new(BlockId(1), 0, Some(func_b.clone()), CallKind::Direct, 1, 1),
    );
    call_graph.add_call_site(
        func_b.clone(),
        CallSite::new(BlockId(2), 0, Some(func_c.clone()), CallKind::Direct, 2, 1),
    );
    call_graph.add_call_site(
        func_c.clone(),
        CallSite::new(BlockId(3), 0, Some(func_a.clone()), CallKind::Direct, 3, 1),
    );
    call_graph.add_call_site(
        func_d.clone(),
        CallSite::new(BlockId(4), 0, Some(func_e.clone()), CallKind::Direct, 4, 1),
    );

    let sccs = call_graph.strongly_connected_components();
    assert_eq!(sccs.len(), 3);

    let scc_sizes: Vec<usize> = sccs.iter().map(|scc| scc.len()).collect();
    assert!(scc_sizes.contains(&3)); // A, B, C cycle
    assert!(scc_sizes.iter().filter(|&&s| s == 1).count() == 2);

    assert!(call_graph.has_circular_dependencies());

    let circular_funcs = call_graph.circular_dependency_functions();
    assert_eq!(circular_funcs.len(), 3);
    assert!(circular_funcs.contains(&func_a));
    assert!(circular_funcs.contains(&func_b));
    assert!(circular_funcs.contains(&func_c));
    assert!(!circular_funcs.contains(&func_d));
    assert!(!circular_funcs.contains(&func_e));
}

#[test]
fn test_scc_multiple_cycles() {
    let mut call_graph = CallGraph::new();

    let uri = test_uri("test.py");
    let func_a = FunctionId::new(uri.clone(), ScopeId::from_raw(1), "a".to_string());
    let func_b = FunctionId::new(uri.clone(), ScopeId::from_raw(2), "b".to_string());
    let func_c = FunctionId::new(uri.clone(), ScopeId::from_raw(3), "c".to_string());
    let func_d = FunctionId::new(uri.clone(), ScopeId::from_raw(4), "d".to_string());

    call_graph.add_call_site(
        func_a.clone(),
        CallSite::new(BlockId(1), 0, Some(func_b.clone()), CallKind::Direct, 1, 1),
    );
    call_graph.add_call_site(
        func_b.clone(),
        CallSite::new(BlockId(2), 0, Some(func_a.clone()), CallKind::Direct, 2, 1),
    );

    call_graph.add_call_site(
        func_c.clone(),
        CallSite::new(BlockId(3), 0, Some(func_d.clone()), CallKind::Direct, 3, 1),
    );
    call_graph.add_call_site(
        func_d.clone(),
        CallSite::new(BlockId(4), 0, Some(func_c.clone()), CallKind::Direct, 4, 1),
    );

    let sccs = call_graph.strongly_connected_components();

    assert_eq!(sccs.len(), 2);
    assert!(sccs.iter().all(|scc| scc.len() == 2));
    assert!(call_graph.has_circular_dependencies());

    let circular_funcs = call_graph.circular_dependency_functions();
    assert_eq!(circular_funcs.len(), 4);
}

#[test]
fn test_scc_nested_structure() {
    let mut call_graph = CallGraph::new();

    let uri = test_uri("test.py");
    let func_a = FunctionId::new(uri.clone(), ScopeId::from_raw(1), "a".to_string());
    let func_b = FunctionId::new(uri.clone(), ScopeId::from_raw(2), "b".to_string());
    let func_c = FunctionId::new(uri.clone(), ScopeId::from_raw(3), "c".to_string());
    let func_d = FunctionId::new(uri.clone(), ScopeId::from_raw(4), "d".to_string());

    call_graph.add_call_site(
        func_a.clone(),
        CallSite::new(BlockId(1), 0, Some(func_b.clone()), CallKind::Direct, 1, 1),
    );
    call_graph.add_call_site(
        func_b.clone(),
        CallSite::new(BlockId(2), 0, Some(func_c.clone()), CallKind::Direct, 2, 1),
    );
    call_graph.add_call_site(
        func_c.clone(),
        CallSite::new(BlockId(3), 0, Some(func_d.clone()), CallKind::Direct, 3, 1),
    );
    call_graph.add_call_site(
        func_d.clone(),
        CallSite::new(BlockId(4), 0, Some(func_b.clone()), CallKind::Direct, 4, 1),
    );

    let sccs = call_graph.strongly_connected_components();
    assert_eq!(sccs.len(), 2);

    let cycle_scc = sccs.iter().find(|scc| scc.len() == 3).expect("Should find cycle SCC");
    assert!(cycle_scc.contains(&func_b));
    assert!(cycle_scc.contains(&func_c));
    assert!(cycle_scc.contains(&func_d));

    assert!(call_graph.has_circular_dependencies());

    let circular_funcs = call_graph.circular_dependency_functions();
    assert_eq!(circular_funcs.len(), 3);
    assert!(!circular_funcs.contains(&func_a));
}

#[test]
fn test_scc_reachability_with_cycles() {
    let mut call_graph = CallGraph::new();

    let uri = test_uri("test.py");
    let func_a = FunctionId::new(uri.clone(), ScopeId::from_raw(1), "a".to_string());
    let func_b = FunctionId::new(uri.clone(), ScopeId::from_raw(2), "b".to_string());
    let func_c = FunctionId::new(uri.clone(), ScopeId::from_raw(3), "c".to_string());
    let func_d = FunctionId::new(uri.clone(), ScopeId::from_raw(4), "d".to_string());

    call_graph.add_call_site(
        func_a.clone(),
        CallSite::new(BlockId(1), 0, Some(func_b.clone()), CallKind::Direct, 1, 1),
    );
    call_graph.add_call_site(
        func_b.clone(),
        CallSite::new(BlockId(2), 0, Some(func_c.clone()), CallKind::Direct, 2, 1),
    );
    call_graph.add_call_site(
        func_c.clone(),
        CallSite::new(BlockId(3), 0, Some(func_b.clone()), CallKind::Direct, 3, 1),
    );
    call_graph.add_call_site(
        func_c.clone(),
        CallSite::new(BlockId(4), 0, Some(func_d.clone()), CallKind::Direct, 4, 1),
    );

    let reachable = call_graph.reachable_functions(&[func_a.clone()]);

    assert_eq!(reachable.len(), 4);
    assert!(reachable.contains(&func_a));
    assert!(reachable.contains(&func_b));
    assert!(reachable.contains(&func_c));
    assert!(reachable.contains(&func_d));
}

#[test]
fn test_scc_empty_graph() {
    let call_graph = CallGraph::new();

    let sccs = call_graph.strongly_connected_components();
    assert!(sccs.is_empty());
    assert!(!call_graph.has_circular_dependencies());

    let circular_funcs = call_graph.circular_dependency_functions();
    assert!(circular_funcs.is_empty());
}

#[test]
fn test_scc_single_function_no_calls() {
    let mut call_graph = CallGraph::new();

    let uri = test_uri("test.py");
    let func_a = FunctionId::new(uri.clone(), ScopeId::from_raw(1), "a".to_string());

    call_graph.add_call_site(
        func_a.clone(),
        CallSite::new(BlockId(1), 0, None, CallKind::Dynamic, 1, 1),
    );

    let sccs = call_graph.strongly_connected_components();

    assert_eq!(sccs.len(), 1);
    assert_eq!(sccs[0].len(), 1);
    assert_eq!(sccs[0][0], func_a);
    assert!(!call_graph.has_circular_dependencies());
}

#[test]
fn test_scc_cross_module_cycle() {
    let mut call_graph = CallGraph::new();

    let uri_a = test_uri("a.py");
    let uri_b = test_uri("b.py");

    let func_a = FunctionId::new(uri_a, ScopeId::from_raw(1), "func_a".to_string());
    let func_b = FunctionId::new(uri_b, ScopeId::from_raw(1), "func_b".to_string());

    call_graph.add_call_site(
        func_a.clone(),
        CallSite::new(BlockId(1), 0, Some(func_b.clone()), CallKind::Direct, 1, 1),
    );
    call_graph.add_call_site(
        func_b.clone(),
        CallSite::new(BlockId(1), 0, Some(func_a.clone()), CallKind::Direct, 1, 1),
    );

    let sccs = call_graph.strongly_connected_components();

    assert_eq!(sccs.len(), 1);
    assert_eq!(sccs[0].len(), 2);
    assert!(call_graph.has_circular_dependencies());

    let circular_funcs = call_graph.circular_dependency_functions();
    assert_eq!(circular_funcs.len(), 2);
    assert!(circular_funcs.contains(&func_a));
    assert!(circular_funcs.contains(&func_b));
}
