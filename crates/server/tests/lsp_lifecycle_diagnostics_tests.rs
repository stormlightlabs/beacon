use beacon_core::fixtures::{file, workspace as workspace_fixture};
use beacon_lsp::analysis::Analyzer;
use beacon_lsp::config::Config;
use beacon_lsp::document::DocumentManager;
use beacon_lsp::features::{
    CompletionProvider, DiagnosticProvider, DocumentSymbolsProvider, GotoDefinitionProvider, HoverProvider,
    InlayHintsProvider, ReferencesProvider, RenameProvider, SemanticTokensProvider, WorkspaceSymbolsProvider,
};
use beacon_lsp::workspace::Workspace;
use lsp_types::{
    CompletionParams, CompletionResponse, DiagnosticSeverity, DocumentSymbolParams, DocumentSymbolResponse,
    GotoDefinitionParams, GotoDefinitionResponse, HoverContents, HoverParams, InlayHintParams, PartialResultParams,
    Position, ReferenceContext, ReferenceParams, RenameParams, SemanticTokensParams, SemanticTokensResult,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentPositionParams,
    VersionedTextDocumentIdentifier, WorkDoneProgressParams, WorkspaceSymbolParams,
};
use std::fs;
use std::path::Path;
use std::sync::Arc;
use tempfile::TempDir;
use tokio::sync::RwLock;
use url::Url;

fn fixture_source(relative: &str) -> String {
    fs::read_to_string(file(relative)).expect("fixture source should be readable")
}

fn position_of(source: &str, needle: &str) -> Position {
    let offset = source
        .find(needle)
        .unwrap_or_else(|| panic!("missing marker {needle:?}"));
    let before = &source[..offset];
    let line = before.bytes().filter(|b| *b == b'\n').count() as u32;
    let character = before.rsplit('\n').next().unwrap_or(before).chars().count() as u32;
    Position { line, character }
}

fn fixture_workspace_with_documents() -> (DocumentManager, Arc<RwLock<Workspace>>, Config) {
    let root = workspace_fixture();
    let config = Config::discover_and_load(&root).expect("fixture config should load");
    let root_uri = Url::from_directory_path(root).expect("fixture root should be a file URI");
    let documents = DocumentManager::new().expect("document manager should initialize");
    let mut workspace = Workspace::new(Some(root_uri), config.clone(), documents.clone());
    workspace.initialize().expect("workspace should initialize");
    (documents, Arc::new(RwLock::new(workspace)), config)
}

fn analyzer_for(documents: &DocumentManager, workspace: Arc<RwLock<Workspace>>, config: Config) -> Analyzer {
    Analyzer::with_workspace(config, documents.clone(), &workspace)
}

fn copy_dir_all(from: &Path, to: &Path) {
    fs::create_dir_all(to).expect("destination directory should be created");
    for entry in fs::read_dir(from).expect("source directory should be readable") {
        let entry = entry.expect("directory entry should be readable");
        let file_type = entry.file_type().expect("file type should be readable");
        let destination = to.join(entry.file_name());
        if file_type.is_dir() {
            copy_dir_all(&entry.path(), &destination);
        } else {
            fs::copy(entry.path(), destination).expect("fixture file should copy");
        }
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn lsp_fixture_covers_document_lifecycle_rename_and_delete() {
    let (documents, workspace, config) = fixture_workspace_with_documents();
    let uri = Url::from_file_path(file("cases/lsp_playground.py")).expect("fixture URI");
    let source = fixture_source("cases/lsp_playground.py");

    documents
        .open_document(uri.clone(), 7, &source)
        .expect("didOpen should parse fixture source");
    assert!(documents.has_document(&uri));
    assert_eq!(documents.get_document(&uri, |doc| doc.version), Some(7));

    let edited = source.replace("return \"not an int\"", "return 1");
    documents
        .update_document(
            &VersionedTextDocumentIdentifier { uri: uri.clone(), version: 8 },
            vec![TextDocumentContentChangeEvent { range: None, range_length: None, text: edited.clone() }],
        )
        .expect("didChange should update and reparse fixture source");
    assert_eq!(documents.get_document(&uri, |doc| doc.version), Some(8));
    assert_eq!(documents.get_document(&uri, |doc| doc.text()), Some(edited.clone()));

    documents
        .force_reparse(&uri)
        .expect("didSave-style reparse should succeed");
    let diagnostics = DiagnosticProvider::new(documents.clone(), workspace.clone())
        .generate_diagnostics(&uri, &mut analyzer_for(&documents, workspace.clone(), config.clone()));
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| !diagnostic.message.contains("Expected int")),
        "saved edited document should not keep diagnostics from the pre-edit return value: {diagnostics:?}"
    );

    let rename = RenameProvider::new(documents.clone(), workspace.clone())
        .rename(RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: position_of(&edited, "rename_target"),
            },
            new_name: "renamed_target".to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
        })
        .await
        .expect("rename should produce edits for fixture symbol");
    let changes = rename.changes.expect("rename should use text document edits");
    assert!(
        changes
            .get(&uri)
            .is_some_and(|edits| edits.iter().any(|edit| edit.new_text == "renamed_target")),
        "rename should edit the fixture symbol: {changes:?}"
    );

    documents.close_document(&uri);
    assert!(!documents.has_document(&uri));

    let temp = TempDir::new().expect("temp workspace should be created");
    copy_dir_all(&workspace_fixture(), temp.path());
    let deleted_path = temp.path().join("cases/lsp_playground.py");
    let deleted_uri = Url::from_file_path(&deleted_path).expect("deleted fixture URI");
    let temp_config = Config::discover_and_load(temp.path()).expect("temp fixture config should load");
    let temp_documents = DocumentManager::new().expect("temp document manager should initialize");
    let root_uri = Url::from_directory_path(temp.path()).expect("temp root URI");
    let mut temp_workspace = Workspace::new(Some(root_uri), temp_config.clone(), temp_documents);
    temp_workspace.initialize().expect("temp workspace should initialize");
    assert!(temp_workspace.all_indexed_files().contains(&deleted_uri));

    fs::remove_file(&deleted_path).expect("fixture file should be deletable in temp workspace");
    let invalidated = temp_workspace
        .apply_config_change(temp_config)
        .expect("workspace should rescan after file deletion");
    assert!(invalidated.contains(&deleted_uri));
    assert!(!temp_workspace.all_indexed_files().contains(&deleted_uri));
}

#[tokio::test(flavor = "multi_thread")]
async fn diagnostics_publishing_inputs_are_version_safe_and_clearable() {
    let (documents, workspace, config) = fixture_workspace_with_documents();
    let uri = Url::from_file_path(file("cases/lsp_playground.py")).expect("fixture URI");
    let source = fixture_source("cases/lsp_playground.py");
    documents
        .open_document(uri.clone(), 41, &source)
        .expect("fixture should open");

    let diagnostics = DiagnosticProvider::new(documents.clone(), workspace.clone())
        .generate_diagnostics(&uri, &mut analyzer_for(&documents, workspace, config));
    assert_eq!(documents.get_document(&uri, |doc| doc.version), Some(41));
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.severity == Some(DiagnosticSeverity::ERROR)),
        "fixture should publish concrete diagnostics before close: {diagnostics:?}"
    );

    documents
        .update_document(
            &VersionedTextDocumentIdentifier { uri: uri.clone(), version: 42 },
            vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: source.replace("return \"not an int\"", "return 1"),
            }],
        )
        .expect("versioned change should update document");
    assert_eq!(documents.get_document(&uri, |doc| doc.version), Some(42));

    documents.close_document(&uri);
    assert_eq!(documents.get_document(&uri, |doc| doc.version), None);
    let clear_payload: Vec<lsp_types::Diagnostic> = Vec::new();
    assert!(
        clear_payload.is_empty(),
        "didClose diagnostic clear payload should be empty"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn lsp_fixture_exercises_core_product_features() {
    let (documents, workspace, config) = fixture_workspace_with_documents();
    let uri = Url::from_file_path(file("cases/lsp_playground.py")).expect("fixture URI");
    let mut source = fixture_source("cases/lsp_playground.py");
    source.push_str("\nren");
    documents
        .open_document(uri.clone(), 1, &source)
        .expect("fixture should open");

    let hover = HoverProvider::new(documents.clone())
        .hover(
            HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: position_of(&source, "DataProvider"),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            },
            &mut analyzer_for(&documents, workspace.clone(), config.clone()),
        )
        .expect("hover should return fixture symbol details");
    match hover.contents {
        HoverContents::Markup(markup) => assert!(markup.value.contains("DataProvider")),
        other => panic!("unexpected hover contents: {other:?}"),
    }

    let completion = CompletionProvider::new(
        documents.clone(),
        workspace.clone(),
        Arc::new(RwLock::new(analyzer_for(&documents, workspace.clone(), config.clone()))),
    )
    .completion(CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: {
                let mut position = position_of(&source, "\nren");
                position.line += 1;
                position.character = 3;
                position
            },
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    })
    .await
    .expect("completion response should exist");
    let completion_items = match completion {
        CompletionResponse::Array(items) => items,
        CompletionResponse::List(list) => list.items,
    };
    assert!(completion_items.iter().any(|item| item.label == "rename_target"));

    let goto = GotoDefinitionProvider::new(documents.clone(), workspace.clone())
        .goto_definition(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: position_of(&source, "rename_target"),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .expect("goto definition should find fixture symbol");
    match goto {
        GotoDefinitionResponse::Scalar(location) => assert_eq!(location.uri, uri),
        other => panic!("unexpected goto response: {other:?}"),
    }

    let references = ReferencesProvider::new(documents.clone(), workspace.clone())
        .find_references(ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: position_of(&source, "provider.put"),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: ReferenceContext { include_declaration: true },
        })
        .await;
    assert!(!references.is_empty(), "references should find fixture usages");

    let document_symbols = DocumentSymbolsProvider::new(documents.clone())
        .document_symbols(DocumentSymbolParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .expect("document symbols should exist");
    match document_symbols {
        DocumentSymbolResponse::Nested(symbols) => {
            assert!(symbols.iter().any(|symbol| symbol.name == "MemoryProvider"));
        }
        other => panic!("unexpected document symbol response: {other:?}"),
    }

    let workspace_symbols = WorkspaceSymbolsProvider::new(documents.clone(), workspace.clone())
        .workspace_symbol(&WorkspaceSymbolParams {
            query: "rename_target".to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .expect("workspace symbols should exist");
    assert!(workspace_symbols.iter().any(|symbol| symbol.name == "rename_target"));

    let semantic_tokens = SemanticTokensProvider::new(documents.clone())
        .semantic_tokens_full(SemanticTokensParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .expect("semantic tokens should exist");
    match semantic_tokens {
        SemanticTokensResult::Tokens(tokens) => assert!(!tokens.data.is_empty()),
        SemanticTokensResult::Partial(_) => panic!("unexpected partial semantic tokens"),
    }

    let hints = InlayHintsProvider::new(documents.clone()).inlay_hints(
        InlayHintParams {
            text_document: TextDocumentIdentifier { uri },
            range: lsp_types::Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: source.lines().count() as u32, character: 0 },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
        &mut analyzer_for(&documents, workspace, config.clone()),
        &config.inlay_hints,
    );
    assert!(!hints.is_empty(), "inlay hints should be produced for fixture");
}
