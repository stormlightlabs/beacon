use beacon_core::fixtures::{file, workspace as workspace_fixture};
use beacon_lsp::config::Config;
use beacon_lsp::document::DocumentManager;
use beacon_lsp::workspace::Workspace;
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

    let stub = workspace
        .load_stub("imports.stubbed")
        .expect("inline pyi should be loaded for imports.stubbed");
    assert!(stub.path.ends_with("imports/stubbed.pyi"));
}
