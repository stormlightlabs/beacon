use beacon_lsp::{
    config::Config,
    document::DocumentManager,
    features::{
        move_symbol::{MoveSymbolParams, MoveSymbolProvider},
        refactoring::RefactoringContext,
    },
    workspace::Workspace,
};
use lsp_types::{Position, Url};
use std::sync::Arc;
use tokio::sync::RwLock;

fn file_uri(workspace_root: &str, path: &str) -> Url {
    Url::parse(&format!("{workspace_root}/{path}")).unwrap()
}

#[tokio::test]
async fn test_move_symbol_across_files() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let source_uri = file_uri(workspace_root.as_str(), "source.py");
    let source_content = r#"
def moved_function():
    pass

def other_function():
    moved_function()
"#;
    documents
        .open_document(source_uri.clone(), 0, source_content.to_string())
        .unwrap();
    workspace.update_dependencies(&source_uri);

    let target_uri = file_uri(workspace_root.as_str(), "target.py");
    let target_content = "";
    documents
        .open_document(target_uri.clone(), 0, target_content.to_string())
        .unwrap();
    workspace.update_dependencies(&target_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let context = RefactoringContext::new(documents.clone(), workspace_arc.clone());
    let provider = MoveSymbolProvider::new(context);

    let position = Position { line: 1, character: 4 };

    let params = MoveSymbolParams { source_uri: source_uri.clone(), position, target_uri: target_uri.clone() };

    let result = provider.execute(params).await;
    assert!(result.is_some(), "Move symbol execution failed");

    let edit = result.unwrap();
    let changes = edit.changes.unwrap();

    let target_edits = changes.get(&target_uri).unwrap();
    let definition_added = target_edits
        .iter()
        .any(|e| e.new_text.contains("def moved_function():"));
    assert!(definition_added, "Target file should contain moved function definition");

    let source_edits = changes.get(&source_uri).unwrap();
    let import_added = source_edits
        .iter()
        .any(|e| e.new_text.contains("from target import moved_function"));
    assert!(import_added, "Source file should import moved function");
}

#[tokio::test]
async fn test_move_symbol_updates_dependencies() {
    let documents = DocumentManager::new().unwrap();
    let config = Config::default();
    let workspace_root = Url::parse("file:///workspace").unwrap();
    let mut workspace = Workspace::new(Some(workspace_root.clone()), config.clone(), documents.clone());

    let source_uri = file_uri(workspace_root.as_str(), "source.py");
    let source_content = r#"
def moved_function():
    pass
"#;
    documents
        .open_document(source_uri.clone(), 0, source_content.to_string())
        .unwrap();
    workspace.update_dependencies(&source_uri);

    let target_uri = file_uri(workspace_root.as_str(), "target.py");
    let target_content = "";
    documents
        .open_document(target_uri.clone(), 0, target_content.to_string())
        .unwrap();
    workspace.update_dependencies(&target_uri);

    let client_uri = file_uri(workspace_root.as_str(), "client.py");
    let client_content = r#"
from source import moved_function

def main():
    moved_function()
"#;
    documents
        .open_document(client_uri.clone(), 0, client_content.to_string())
        .unwrap();
    workspace.update_dependencies(&client_uri);

    let workspace_arc = Arc::new(RwLock::new(workspace));
    let context = RefactoringContext::new(documents.clone(), workspace_arc);
    let provider = MoveSymbolProvider::new(context);

    let position = Position { line: 1, character: 4 };

    let params = MoveSymbolParams { source_uri: source_uri.clone(), position, target_uri: target_uri.clone() };

    let result = provider.execute(params).await;
    assert!(result.is_some());

    let edit = result.unwrap();
    let changes = edit.changes.unwrap();

    if let Some(client_edits) = changes.get(&client_uri) {
        let import_updated = client_edits.iter().any(|e| e.new_text == "target");
        assert!(import_updated, "Client import module should be updated to 'target'");
    } else {
        panic!("No edits for client.py");
    }
}
