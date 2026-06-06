use beacon_lsp::analysis::Analyzer;
use beacon_lsp::config::Config;
use beacon_lsp::document::DocumentManager;
use beacon_lsp::features::CodeActionsProvider;
use lsp_types::{
    CodeActionContext, CodeActionKind, CodeActionOrCommand, CodeActionParams, Diagnostic, NumberOrString, Position,
    Range, TextDocumentIdentifier, WorkDoneProgressParams,
};
use std::sync::Arc;
use tokio::sync::RwLock;
use url::Url;

fn test_uri(name: &str) -> Url {
    Url::parse(&format!("file:///workspace/{name}")).expect("test URI should be valid")
}

fn diagnostic(code: &str, message: &str, line: u32, start: u32, end: u32) -> Diagnostic {
    Diagnostic {
        range: Range { start: Position { line, character: start }, end: Position { line, character: end } },
        severity: None,
        code: Some(NumberOrString::String(code.to_string())),
        code_description: None,
        source: Some("beacon".to_string()),
        message: message.to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

async fn actions_for(source: &str, uri: Url, diagnostic: Diagnostic) -> Vec<lsp_types::CodeAction> {
    let documents = DocumentManager::new().expect("document manager should initialize");
    documents
        .open_document(uri.clone(), 1, source)
        .expect("document should open");
    let analyzer = Arc::new(RwLock::new(Analyzer::new(Config::default(), documents.clone())));
    let provider = CodeActionsProvider::new(documents, analyzer);

    provider
        .code_actions(CodeActionParams {
            text_document: TextDocumentIdentifier { uri },
            range: diagnostic.range,
            context: CodeActionContext {
                diagnostics: vec![diagnostic],
                only: Some(vec![CodeActionKind::QUICKFIX]),
                trigger_kind: None,
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: Default::default(),
        })
        .await
        .into_iter()
        .filter_map(|action| match action {
            CodeActionOrCommand::CodeAction(action) => Some(action),
            CodeActionOrCommand::Command(_) => None,
        })
        .collect()
}

#[tokio::test(flavor = "multi_thread")]
async fn autofix_removes_unused_import_line_with_mechanical_edit() {
    let uri = test_uri("unused_import.py");
    let source = "import os\n\ndef main():\n    return 1\n";
    let actions = actions_for(
        source,
        uri.clone(),
        diagnostic("unused-import", "Unused import: os", 0, 7, 9),
    )
    .await;

    let remove = actions
        .iter()
        .find(|action| action.title == "Remove unused definition")
        .expect("unused import quick fix should be offered");
    assert_eq!(remove.kind, Some(CodeActionKind::QUICKFIX));
    assert_eq!(remove.is_preferred, Some(true));

    let edits = remove
        .edit
        .as_ref()
        .and_then(|edit| edit.changes.as_ref())
        .and_then(|changes| changes.get(&uri))
        .expect("unused import quick fix should target the current file");
    assert_eq!(edits.len(), 1);
    assert_eq!(edits[0].range.start, Position { line: 0, character: 0 });
    assert_eq!(edits[0].range.end, Position { line: 1, character: 0 });
    assert!(edits[0].new_text.is_empty());
}

#[tokio::test(flavor = "multi_thread")]
async fn autofix_wraps_none_return_type_and_adds_optional_import() {
    let uri = test_uri("optional_return.py");
    let source = "def lookup(flag: bool) -> str:\n    if flag:\n        return \"ok\"\n    return None\n";
    let actions = actions_for(
        source,
        uri.clone(),
        diagnostic("HM001", "Cannot return None from function returning str", 0, 25, 28),
    )
    .await;

    let wrap = actions
        .iter()
        .find(|action| action.title == "Wrap type with Optional")
        .expect("Optional quick fix should be offered");
    assert_eq!(wrap.kind, Some(CodeActionKind::QUICKFIX));
    assert_eq!(wrap.is_preferred, Some(true));

    let edits = wrap
        .edit
        .as_ref()
        .and_then(|edit| edit.changes.as_ref())
        .and_then(|changes| changes.get(&uri))
        .expect("Optional quick fix should target the current file");
    assert!(
        edits
            .iter()
            .any(|edit| edit.new_text == "from typing import Optional\n")
    );
    assert!(edits.iter().any(|edit| edit.new_text == " Optional[str]"));
}
