use beacon_core::fixtures::{file, workspace as workspace_fixture};
use beacon_lsp::config::Config;
use beacon_lsp::document::DocumentManager;
use beacon_lsp::features::{ReferencesProvider, RenameProvider};
use beacon_lsp::workspace::Workspace;
use lsp_types::{
    Position, ReferenceContext, ReferenceParams, RenameParams, TextDocumentContentChangeEvent, TextDocumentIdentifier,
    TextDocumentPositionParams, VersionedTextDocumentIdentifier,
};
use std::fs;
use std::sync::Arc;
use tempfile::TempDir;
use tokio::sync::RwLock;
use url::Url;

fn fixture_workspace() -> Workspace {
    let root = workspace_fixture();
    let config = Config::discover_and_load(&root).expect("fixture config should load");
    let root_uri = Url::from_directory_path(root).expect("fixture root should be a file URI");
    let documents = DocumentManager::new().expect("document manager should initialize");
    let mut workspace = Workspace::new(Some(root_uri), config, documents);
    workspace.initialize().expect("workspace should initialize");
    workspace
}

fn fixture_workspace_with_documents() -> (DocumentManager, Arc<RwLock<Workspace>>) {
    let root = workspace_fixture();
    let config = Config::discover_and_load(&root).expect("fixture config should load");
    let root_uri = Url::from_directory_path(root).expect("fixture root should be a file URI");
    let documents = DocumentManager::new().expect("document manager should initialize");
    let mut workspace = Workspace::new(Some(root_uri), config, documents.clone());
    workspace.initialize().expect("workspace should initialize");
    (documents, Arc::new(RwLock::new(workspace)))
}

#[test]
fn workspace_fixture_covers_import_module_roots_and_forms() {
    let workspace = fixture_workspace();

    for module in [
        "imports",
        "imports.api",
        "imports.local_source",
        "imports.subpkg",
        "imports.subpkg.tools",
        "namespace_root.plugin",
        "app.models",
        "typing",
    ] {
        assert!(workspace.resolve_import(module).is_some(), "{module} should resolve");
    }

    let api_uri = Url::from_file_path(file("imports/api.py")).expect("api fixture URI");
    let deps = workspace.get_dependencies(&api_uri);

    for dependency in [
        "imports/local_source.py",
        "imports/star_source.py",
        "imports/subpkg/__init__.py",
        "imports/subpkg/reexport_chain.py",
        "imports/type_only.py",
        "app/models.py",
    ] {
        let dependency_uri = Url::from_file_path(file(dependency)).expect("dependency URI");
        assert!(deps.contains(&dependency_uri), "api.py should depend on {dependency}");
    }
}

#[test]
fn workspace_fixture_covers_exports_star_imports_and_reexport_chains() {
    let workspace = fixture_workspace();

    let package_uri = Url::from_file_path(file("imports/__init__.py")).expect("package URI");
    let package_exports = workspace.resolve_star_import(&package_uri);
    assert_eq!(package_exports, vec!["PublicApi", "make_api", "exported_value"]);

    let star_uri = Url::from_file_path(file("imports/star_source.py")).expect("star fixture URI");
    let star_exports = workspace.resolve_star_import(&star_uri);
    assert!(star_exports.contains(&"STAR_VALUE".to_string()));
    assert!(star_exports.contains(&"star_func".to_string()));
    assert!(!star_exports.contains(&"HIDDEN_PUBLIC".to_string()));
    assert!(!star_exports.contains(&"_PRIVATE_VALUE".to_string()));

    let chain_uri = Url::from_file_path(file("imports/subpkg/reexport_chain.py")).expect("chain fixture URI");
    assert_eq!(workspace.resolve_star_import(&chain_uri), vec!["renamed_tool"]);
}

#[test]
fn workspace_fixture_covers_missing_and_circular_imports_gracefully() {
    let workspace = fixture_workspace();

    let broken_uri = Url::from_file_path(file("imports/broken_imports.py")).expect("broken fixture URI");
    let unresolved = workspace.unresolved_imports(&broken_uri);
    assert!(unresolved.contains(&"definitely_missing_module".to_string()));
    assert!(unresolved.contains(&".missing_sibling".to_string()));

    let symbol_imports = workspace.get_symbol_imports(&broken_uri);
    assert!(symbol_imports.iter().any(|import| import.symbol == "_private_helper"));

    let circular_a = Url::from_file_path(file("imports/circular_a.py")).expect("circular a URI");
    let circular_b = Url::from_file_path(file("imports/circular_b.py")).expect("circular b URI");
    let cycles = workspace.circular_dependencies();
    assert!(
        cycles
            .iter()
            .any(|cycle| cycle.contains(&circular_a) && cycle.contains(&circular_b)),
        "circular import pair should be reported as one SCC"
    );
}

#[test]
fn workspace_inline_pyi_takes_precedence_over_workspace_py() {
    let workspace = fixture_workspace();

    let source_uri = workspace
        .resolve_import("imports.stubbed")
        .expect("workspace .py should resolve as the module location");
    assert!(source_uri.to_file_path().unwrap().ends_with("imports/stubbed.py"));

    let stub = workspace
        .load_stub("imports.stubbed")
        .expect("inline pyi should be loaded for imports.stubbed");
    assert!(stub.path.ends_with("imports/stubbed.pyi"));
}

#[test]
fn stub_precedence_covers_custom_inline_bundled_and_site_packages() {
    let temp = TempDir::new().expect("temp workspace");
    let root = temp.path();

    fs::create_dir_all(root.join("pkg")).unwrap();
    fs::write(root.join("pkg/__init__.py"), "").unwrap();
    fs::write(root.join("pkg/mod.py"), "def value():\n    return 'source'\n").unwrap();
    fs::write(root.join("pkg/mod.pyi"), "def value() -> str: ...\n").unwrap();

    let custom_stubs = root.join("custom-stubs");
    fs::create_dir_all(custom_stubs.join("pkg")).unwrap();
    fs::write(custom_stubs.join("pkg/mod.pyi"), "def custom_value() -> int: ...\n").unwrap();

    let site_packages = root.join(".venv/lib/python3.11/site-packages");
    fs::create_dir_all(&site_packages).unwrap();
    fs::write(site_packages.join("thirdparty.pyi"), "def site_value() -> str: ...\n").unwrap();
    fs::write(
        site_packages.join("typing.pyi"),
        "def site_typing_shadow() -> None: ...\n",
    )
    .unwrap();

    let root_uri = Url::from_directory_path(root).unwrap();
    let documents = DocumentManager::new().unwrap();

    let mut with_custom = Workspace::new(
        Some(root_uri.clone()),
        Config { stub_paths: vec![custom_stubs.clone()], ..Default::default() },
        documents.clone(),
    );
    with_custom.initialize().unwrap();
    let custom = with_custom.load_stub("pkg.mod").expect("custom stub should resolve");
    assert!(custom.path.ends_with("custom-stubs/pkg/mod.pyi"));

    let mut without_custom = Workspace::new(
        Some(root_uri),
        Config { stub_paths: vec![], ..Default::default() },
        documents,
    );
    without_custom.initialize().unwrap();

    let inline = without_custom.load_stub("pkg.mod").expect("inline stub should resolve");
    assert!(inline.path.ends_with("pkg/mod.pyi"));

    let bundled = without_custom
        .load_stub("typing")
        .expect("bundled typing should resolve");
    assert!(
        bundled.content.is_some(),
        "bundled stdlib stub should beat site-packages shadow"
    );

    let site = without_custom
        .load_stub("thirdparty")
        .expect("site-packages stub should resolve for non-bundled third-party module");
    assert!(site.path.ends_with("site-packages/thirdparty.pyi"));
}

#[test]
fn dependency_invalidation_covers_imports_exports_all_config_and_stubs() {
    let mut workspace = fixture_workspace();

    let api_uri = Url::from_file_path(file("imports/api.py")).expect("api URI");
    let local_uri = Url::from_file_path(file("imports/local_source.py")).expect("local source URI");
    let star_uri = Url::from_file_path(file("imports/star_source.py")).expect("star source URI");

    for uri in [&api_uri, &local_uri, &star_uri] {
        workspace.mark_analyzed(uri, 1);
    }

    let imported_file_invalidated = workspace.invalidate_dependents(&local_uri);
    assert!(imported_file_invalidated.contains(&local_uri));
    assert!(imported_file_invalidated.contains(&api_uri));

    let new_star_source = r#"__all__ = ["STAR_VALUE"]

STAR_VALUE = 7

def star_func() -> int:
    return STAR_VALUE
"#;
    workspace
        .documents()
        .open_document(star_uri.clone(), 2, new_star_source)
        .unwrap();
    workspace.update_dependencies(&star_uri);
    assert_eq!(
        workspace.get_all_exports(&star_uri),
        Some(vec!["STAR_VALUE".to_string()])
    );

    let all_invalidated = workspace.invalidate_dependents(&star_uri);
    assert!(all_invalidated.contains(&star_uri));
    assert!(all_invalidated.contains(&api_uri));

    workspace
        .documents()
        .update_document(
            &VersionedTextDocumentIdentifier { uri: star_uri.clone(), version: 3 },
            vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "__all__ = [\"STAR_VALUE\", \"star_func\"]\n\nSTAR_VALUE = 7\n\ndef star_func() -> int:\n    return STAR_VALUE\n".to_string(),
            }],
        )
        .unwrap();
    workspace.update_dependencies(&star_uri);
    assert_eq!(
        workspace.get_all_exports(&star_uri),
        Some(vec!["STAR_VALUE".to_string(), "star_func".to_string()])
    );

    let stub_invalidated = workspace.invalidate_stub_module("typing");
    assert!(stub_invalidated.contains(&api_uri));

    let mut new_config = workspace.config().clone();
    new_config.stub_paths.push(workspace_fixture().join("stubs"));
    let config_invalidated = workspace.apply_config_change(new_config).unwrap();
    assert!(config_invalidated.contains(&api_uri));
}

#[tokio::test]
async fn workspace_references_include_imports_and_reexports() {
    let (documents, workspace) = fixture_workspace_with_documents();
    let local_uri = Url::from_file_path(file("imports/local_source.py")).expect("local source URI");
    let local_source = fs::read_to_string(file("imports/local_source.py")).expect("local source fixture");
    documents.open_document(local_uri.clone(), 1, &local_source).unwrap();

    let provider = ReferencesProvider::new(documents, workspace);
    let locations = provider
        .find_references(ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: local_uri.clone() },
                position: Position { line: 0, character: 6 },
            },
            context: ReferenceContext { include_declaration: true },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        })
        .await;

    let api_uri = Url::from_file_path(file("imports/api.py")).expect("api URI");
    let reexports_uri = Url::from_file_path(file("imports/reexports.py")).expect("reexports URI");
    assert!(locations.iter().any(|location| location.uri == local_uri));
    assert!(locations.iter().any(|location| location.uri == api_uri));
    assert!(locations.iter().any(|location| location.uri == reexports_uri));
}

#[tokio::test]
async fn workspace_rename_edits_imports_and_reexports() {
    let (documents, workspace) = fixture_workspace_with_documents();
    let local_uri = Url::from_file_path(file("imports/local_source.py")).expect("local source URI");
    let local_source = fs::read_to_string(file("imports/local_source.py")).expect("local source fixture");
    documents.open_document(local_uri.clone(), 1, &local_source).unwrap();

    let provider = RenameProvider::new(documents, workspace);
    let edit = provider
        .rename(RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: local_uri.clone() },
                position: Position { line: 0, character: 6 },
            },
            new_name: "RenamedLocalSource".to_string(),
            work_done_progress_params: Default::default(),
        })
        .await
        .expect("rename should produce workspace edit");

    let changes = edit.changes.expect("rename should use changes map");
    let api_uri = Url::from_file_path(file("imports/api.py")).expect("api URI");
    let reexports_uri = Url::from_file_path(file("imports/reexports.py")).expect("reexports URI");
    assert!(changes.contains_key(&local_uri));
    assert!(changes.contains_key(&api_uri));
    assert!(changes.contains_key(&reexports_uri));
    assert!(
        changes
            .values()
            .flatten()
            .all(|edit| edit.new_text == "RenamedLocalSource")
    );
}

#[test]
fn multi_root_workspace_resolves_config_symbols_diagnostics_and_imports() {
    let temp = TempDir::new().expect("temp workspace");
    let root = temp.path();
    let root_a = root.join("root_a");
    let root_b = root.join("root_b");
    fs::create_dir_all(root_a.join("pkg_a")).unwrap();
    fs::create_dir_all(root_b.join("pkg_b")).unwrap();

    fs::write(
        root_a.join("pkg_a/__init__.py"),
        "from .core import exported\n__all__ = [\"exported\"]\n",
    )
    .unwrap();
    fs::write(
        root_a.join("pkg_a/core.py"),
        "def exported() -> str:\n    return \"a\"\n",
    )
    .unwrap();
    fs::write(root_b.join("pkg_b/__init__.py"), "").unwrap();
    fs::write(
        root_b.join("pkg_b/consumer.py"),
        "from pkg_a import exported\n\nvalue = exported()\n",
    )
    .unwrap();

    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(
        Some(Url::from_directory_path(root).unwrap()),
        Config { source_roots: vec![root_a.clone(), root_b.clone()], stub_paths: vec![], ..Default::default() },
        documents,
    );
    workspace.initialize().unwrap();

    let pkg_a = workspace
        .resolve_import("pkg_a")
        .expect("pkg_a should resolve from root_a");
    let consumer = workspace
        .resolve_import("pkg_b.consumer")
        .expect("pkg_b.consumer should resolve from root_b");
    assert_eq!(workspace.config().source_roots, vec![root_a, root_b]);
    assert!(workspace.unresolved_imports(&consumer).is_empty());
    assert!(workspace.get_dependencies(&consumer).contains(&pkg_a));
    assert!(workspace.get_module_symbols(&consumer).contains("value"));
    assert!(workspace.all_modules().iter().any(|(_, module)| module == "pkg_a.core"));
}
