use beacon_lsp::{config::Config, document::DocumentManager, workspace::Workspace};
use url::Url;

fn file_uri(workspace_root: &str, path: &str) -> Url {
    Url::parse(&format!("{workspace_root}/{path}")).unwrap()
}

#[tokio::test]
async fn test_cross_file_call_detection() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let module_a_uri = file_uri(workspace_root.as_str(), "module_a.py");
    let module_a_content = r#"
def func_a():
    pass
"#;
    documents
        .open_document(module_a_uri.clone(), 0, module_a_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_a_uri);

    let module_b_uri = file_uri(workspace_root.as_str(), "module_b.py");
    let module_b_content = r#"
from module_a import func_a

def func_b():
    func_a()
"#;
    documents
        .open_document(module_b_uri.clone(), 0, module_b_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_b_uri);

    workspace.build_module_cfg(&module_a_uri);
    workspace.build_module_cfg(&module_b_uri);

    let workspace_cfg = workspace.workspace_cfg();
    let cfg = workspace_cfg.read().unwrap();
    let call_graph = cfg.call_graph();

    let module_a_cfg = cfg.get_module(&module_a_uri).expect("Module A CFG not found");
    let _func_a_id = module_a_cfg
        .function_ids()
        .into_iter()
        .find(|f| f.name == "func_a")
        .expect("func_a not found");

    let module_b_cfg = cfg.get_module(&module_b_uri).expect("Module B CFG not found");
    let func_b_id = module_b_cfg
        .function_ids()
        .into_iter()
        .find(|f| f.name == "func_b")
        .expect("func_b not found");

    let callees = call_graph.get_callees(&func_b_id);
    assert!(!callees.is_empty(), "func_b should call something");

    let callee = &callees[0];
    assert_eq!(callee.name, "func_a");
    assert_eq!(callee.uri, module_b_uri, "Should point to local import stub");
}

#[tokio::test]
#[ignore]
/// TODO: Implement cross module linking
async fn test_circular_dependency_cfg() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let module_a_uri = file_uri(workspace_root.as_str(), "module_a.py");
    let module_a_content = r#"
import module_b
def func_a():
    module_b.func_b()
"#;
    documents
        .open_document(module_a_uri.clone(), 0, module_a_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_a_uri);

    let module_b_uri = file_uri(workspace_root.as_str(), "module_b.py");
    let module_b_content = r#"
import module_a
def func_b():
    module_a.func_a()
"#;
    documents
        .open_document(module_b_uri.clone(), 0, module_b_content.to_string())
        .unwrap();
    workspace.update_dependencies(&module_b_uri);

    workspace.build_module_cfg(&module_a_uri);
    workspace.build_module_cfg(&module_b_uri);

    let workspace_cfg = workspace.workspace_cfg();
    let cfg = workspace_cfg.read().unwrap();
    let call_graph = cfg.call_graph();

    assert!(
        call_graph.has_circular_dependencies(),
        "Should detect circular dependencies"
    );
}

#[tokio::test]
#[ignore]
/// TODO: Implement cross-file taint analysis test
/// This requires:
/// 1. Cross-module CFG linking (as in test_circular_dependency_cfg)
/// 2. Inter-procedural data flow analysis that traverses the linked CallGraph
/// 3. Definitions of "Source" and "Sink" that can be tracked across files
async fn test_cross_file_taint_propagation() {}
