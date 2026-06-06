use beacon_core::fixtures::{file, workspace as workspace_fixture};
use beacon_lsp::analysis::Analyzer;
use beacon_lsp::config::Config;
use beacon_lsp::document::DocumentManager;
use beacon_lsp::features::change_signature::ChangeSignatureParams;
use beacon_lsp::features::extract_function::ExtractFunctionParams;
use beacon_lsp::features::extract_variable::ExtractVariableParams;
use beacon_lsp::features::inline_function::InlineFunctionParams;
use beacon_lsp::features::move_symbol::MoveSymbolParams;
use beacon_lsp::features::{
    ChangeSignatureProvider, CompletionProvider, ExtractFunctionProvider, ExtractVariableProvider,
    InlineFunctionProvider, MoveSymbolProvider, ParameterChange, RefactoringContext, RenameProvider,
};
use beacon_lsp::workspace::Workspace;
use lsp_types::{
    CompletionParams, CompletionResponse, InsertTextFormat, PartialResultParams, Position, Range, RenameParams,
    TextDocumentIdentifier, TextDocumentPositionParams, WorkDoneProgressParams,
};
use std::fs;
use std::sync::Arc;
use tokio::sync::RwLock;
use url::Url;

fn fixture_source(relative: &str) -> String {
    fs::read_to_string(file(relative)).expect("fixture source should be readable")
}

fn position_of(source: &str, needle: &str) -> Position {
    let offset = source
        .find(needle)
        .unwrap_or_else(|| panic!("missing marker {needle:?}"));
    position_at_offset(source, offset)
}

fn position_at_offset(source: &str, offset: usize) -> Position {
    let before = &source[..offset];
    let line = before.bytes().filter(|b| *b == b'\n').count() as u32;
    let character = before.rsplit('\n').next().unwrap_or(before).chars().count() as u32;
    Position { line, character }
}

fn range_of(source: &str, needle: &str) -> Range {
    let start = source
        .find(needle)
        .unwrap_or_else(|| panic!("missing range {needle:?}"));
    Range { start: position_at_offset(source, start), end: position_at_offset(source, start + needle.len()) }
}

fn cursor_after(source: &str, needle: &str) -> Position {
    let start = source
        .find(needle)
        .unwrap_or_else(|| panic!("missing cursor marker {needle:?}"));
    position_at_offset(source, start + needle.len())
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

fn completion_items(response: CompletionResponse) -> Vec<lsp_types::CompletionItem> {
    match response {
        CompletionResponse::Array(items) => items,
        CompletionResponse::List(list) => list.items,
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn snippet_completions_use_scope_imports_and_inferred_types() {
    let (documents, workspace, config) = fixture_workspace_with_documents();
    let uri = Url::from_file_path(file("cases/lsp_playground.py")).expect("fixture URI");
    let mut source = fixture_source("cases/lsp_playground.py");
    source.push_str("\nren");
    documents
        .open_document(uri.clone(), 1, &source)
        .expect("fixture should open");

    let provider = CompletionProvider::new(
        documents.clone(),
        workspace.clone(),
        Arc::new(RwLock::new(analyzer_for(&documents, workspace.clone(), config.clone()))),
    );

    let scoped_items = completion_items(
        provider
            .completion(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: cursor_after(&source, "\nren"),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .await
            .expect("completion response should exist"),
    );
    let rename_target = scoped_items
        .iter()
        .find(|item| item.label == "rename_target")
        .expect("function in scope should be offered");
    assert_eq!(rename_target.insert_text.as_deref(), Some("rename_target($0)"));
    assert_eq!(rename_target.insert_text_format, Some(InsertTextFormat::SNIPPET));

    let mut imported_source = fixture_source("cases/lsp_playground.py");
    imported_source.push_str("\nPa");
    documents
        .open_document(uri.clone(), 2, &imported_source)
        .expect("fixture should reopen with imported-symbol completion marker");
    let imported_items = completion_items(
        provider
            .completion(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: cursor_after(&imported_source, "\nPa"),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .await
            .expect("imported symbol completions should exist"),
    );
    assert!(imported_items.iter().any(|item| item.label == "Path"));
}

#[tokio::test(flavor = "multi_thread")]
async fn auto_import_completions_use_workspace_symbols_stubs_and_reexports() {
    let (documents, workspace, config) = fixture_workspace_with_documents();
    let api_uri = Url::from_file_path(file("imports/api.py")).expect("api fixture URI");
    documents
        .open_document(api_uri, 1, &fixture_source("imports/api.py"))
        .expect("api fixture should open so workspace symbols are available");

    let uri = Url::from_file_path(file("cases/auto_import_completion.py")).expect("auto import URI");
    let source = "make_\n";
    documents
        .open_document(uri.clone(), 1, source)
        .expect("auto import document should open");

    let provider = CompletionProvider::new(
        documents.clone(),
        workspace.clone(),
        Arc::new(RwLock::new(analyzer_for(&documents, workspace, config))),
    );
    let items = completion_items(
        provider
            .completion(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: cursor_after(source, "make_"),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .await
            .expect("auto-import completion response should exist"),
    );

    let make_api = items
        .iter()
        .find(|item| item.label == "make_api")
        .expect("workspace symbol should complete");
    let edits = make_api
        .additional_text_edits
        .as_ref()
        .expect("workspace symbol completion should include an auto-import edit");
    assert!(
        edits
            .iter()
            .any(|edit| edit.new_text == "from imports.api import make_api\n")
    );
    assert_eq!(make_api.insert_text.as_deref(), Some("make_api($0)"));
    assert_eq!(make_api.insert_text_format, Some(InsertTextFormat::SNIPPET));
}

#[tokio::test(flavor = "multi_thread")]
async fn refactoring_edits_cover_extract_inline_move_change_signature_and_rename() {
    let (documents, workspace, config) = fixture_workspace_with_documents();
    let uri = Url::from_file_path(file("cases/lsp_playground.py")).expect("fixture URI");
    let target_uri = Url::from_file_path(file("cases/refactor_target.py")).expect("target URI");
    let mut source = fixture_source("cases/lsp_playground.py");
    source.push_str(
        r#"

def inline_target(value: int) -> int:
    return value + 1

inline_result = inline_target(2)

def signature_target(name: str) -> str:
    return name.upper()

signature_result = signature_target("beacon")
"#,
    );
    documents
        .open_document(uri.clone(), 1, &source)
        .expect("fixture should open");
    documents
        .open_document(target_uri.clone(), 1, "\n")
        .expect("target refactor document should open");

    let context = RefactoringContext::new(documents.clone(), workspace.clone());

    let mut analyzer = analyzer_for(&documents, workspace.clone(), config.clone());
    let extracted_variable = ExtractVariableProvider::new(context.clone())
        .execute(
            ExtractVariableParams {
                uri: uri.clone(),
                range: range_of(&source, "\"Beacon\""),
                variable_name: "provider_name".to_string(),
                replace_all: false,
            },
            Some(&mut analyzer),
        )
        .await
        .expect("extract variable should produce edits");
    let extracted_variable_edits = extracted_variable.changes.expect("extract variable should use changes");
    assert!(
        extracted_variable_edits
            .get(&uri)
            .is_some_and(|edits| edits.iter().any(|edit| edit.new_text.contains("provider_name")))
    );

    let mut analyzer = analyzer_for(&documents, workspace.clone(), config);
    let extracted_function = ExtractFunctionProvider::new(context.clone())
        .execute(
            ExtractFunctionParams {
                uri: uri.clone(),
                range: range_of(
                    &source,
                    "content = path.read_text(encoding=\"utf-8\")\n    return content.strip()",
                ),
                function_name: "read_clean_text".to_string(),
            },
            Some(&mut analyzer),
        )
        .await
        .expect("extract function should produce edits");
    let extracted_function_edits = extracted_function.changes.expect("extract function should use changes");
    assert!(
        extracted_function_edits
            .get(&uri)
            .is_some_and(|edits| edits.iter().any(|edit| edit.new_text.contains("read_clean_text")))
    );

    let inline_edit = InlineFunctionProvider::new(context.clone())
        .execute(InlineFunctionParams {
            uri: uri.clone(),
            position: position_of(&source, "inline_target"),
            inline_all: true,
        })
        .await
        .expect("inline function should produce edits");
    let inline_edits = inline_edit.changes.expect("inline function should use changes");
    assert!(
        inline_edits
            .get(&uri)
            .is_some_and(|edits| edits.iter().any(|edit| edit.new_text.is_empty())),
        "inline-all should remove the original function definition: {inline_edits:?}"
    );

    let move_edit = MoveSymbolProvider::new(context.clone())
        .execute(MoveSymbolParams {
            source_uri: uri.clone(),
            position: position_of(&source, "rename_target"),
            target_uri: target_uri.clone(),
        })
        .await
        .expect("move symbol should produce edits");
    let move_edits = move_edit.changes.expect("move symbol should use changes");
    assert!(move_edits.contains_key(&uri));
    assert!(
        move_edits
            .get(&target_uri)
            .is_some_and(|edits| edits.iter().any(|edit| edit.new_text.contains("def rename_target")))
    );

    let signature_edit = ChangeSignatureProvider::new(context.clone())
        .execute(ChangeSignatureParams {
            uri: uri.clone(),
            position: position_of(&source, "signature_target"),
            changes: vec![ParameterChange::Add {
                name: "suffix".to_string(),
                default_value: Some("\"!\"".to_string()),
                position: 0,
            }],
        })
        .await
        .expect("change signature should produce edits");
    let signature_edits = signature_edit.changes.expect("change signature should use changes");
    assert!(
        signature_edits
            .get(&uri)
            .is_some_and(|edits| edits.iter().any(|edit| edit.new_text.contains("suffix"))),
        "change signature should add the requested parameter: {signature_edits:?}"
    );

    let rename_edit = RenameProvider::new(documents, workspace)
        .rename(RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: position_of(&source, "rename_target"),
            },
            new_name: "renamed_target".to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
        })
        .await
        .expect("rename should produce edits");
    let rename_edits = rename_edit.changes.expect("rename should use changes");
    assert!(
        rename_edits
            .get(&uri)
            .is_some_and(|edits| edits.iter().any(|edit| edit.new_text == "renamed_target"))
    );
}
