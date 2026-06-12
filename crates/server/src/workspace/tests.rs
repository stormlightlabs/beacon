use super::*;
use beacon_core::Type;
use std::{fs, io::Write};
use tempfile::{NamedTempFile, TempDir};

#[test]
fn workspace_creation() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let _ = Workspace::new(None, config, documents);
}

#[test]
fn dependency_graph_creation() {
    let _ = DependencyGraph::new();
}

#[test]
fn stub_cache_creation() {
    let _ = StubCache::new();
}

#[test]
fn path_to_module_name() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(None, config, documents);

    assert_eq!(
        workspace.path_to_module_name(Path::new("foo.py")),
        Some("foo".to_string())
    );

    assert_eq!(
        workspace.path_to_module_name(Path::new("foo/bar.py")),
        Some("foo.bar".to_string())
    );

    assert_eq!(
        workspace.path_to_module_name(Path::new("foo/__init__.py")),
        Some("foo".to_string())
    );

    assert_eq!(
        workspace.path_to_module_name(Path::new("a/b/c/d.py")),
        Some("a.b.c.d".to_string())
    );

    assert_eq!(workspace.path_to_module_name(Path::new("__init__.py")), None);
    assert_eq!(workspace.path_to_module_name(Path::new("foo.txt")), None);
}

#[test]
fn resolve_relative_import() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(None, config, documents);

    let root = PathBuf::from("/workspace");
    workspace.index.insert(ModuleInfo::new(
        Url::parse("file:///workspace/pkg/utils.py").unwrap(),
        "pkg.utils".to_string(),
        root.clone(),
        false,
    ));
    workspace.index.insert(ModuleInfo::new(
        Url::parse("file:///workspace/pkg/sub/helper.py").unwrap(),
        "pkg.sub.helper".to_string(),
        root,
        false,
    ));

    let _result = workspace.resolve_relative_import("pkg.sub.mod", "", 2);

    let result = workspace.resolve_relative_import("pkg.sub.mod", "utils", 2);
    assert!(result.is_some());
    assert_eq!(result.unwrap().path(), "/workspace/pkg/utils.py");

    let result = workspace.resolve_relative_import("pkg.sub.mod", "helper", 1);
    assert!(result.is_some());
    assert_eq!(result.unwrap().path(), "/workspace/pkg/sub/helper.py");

    let result = workspace.resolve_relative_import("pkg", "foo", 3);
    assert!(result.is_none());
}

#[test]
fn workspace_index() {
    let mut index = WorkspaceIndex::new();

    let uri1 = Url::parse("file:///test/foo.py").unwrap();
    let uri2 = Url::parse("file:///test/bar.py").unwrap();

    let info1 = ModuleInfo::new(uri1.clone(), "foo".to_string(), PathBuf::from("/test"), false);
    let info2 = ModuleInfo::new(uri2.clone(), "bar".to_string(), PathBuf::from("/test"), false);

    index.insert(info1);
    index.insert(info2);

    assert!(index._get(&uri1).is_some());
    assert_eq!(index._get(&uri1).unwrap().module_name, "foo");

    assert!(index.get_by_name("bar").is_some());
    assert_eq!(index.get_by_name("bar").unwrap().uri, uri2);
    assert!(index._contains(&uri1));

    let removed = index._remove(&uri1);
    assert!(removed.is_some());
    assert!(!index._contains(&uri1));
    assert!(index.get_by_name("foo").is_none());
}

#[test]
fn dependency_graph_add_edge() {
    let mut graph = DependencyGraph::new();

    let uri1 = Url::parse("file:///test/a.py").unwrap();
    let uri2 = Url::parse("file:///test/b.py").unwrap();
    let uri3 = Url::parse("file:///test/c.py").unwrap();

    graph.add_edge(&uri1, &uri2);
    graph.add_edge(&uri1, &uri3);

    let deps = graph.get_dependencies(&uri1).unwrap();
    assert_eq!(deps.len(), 2);
    assert!(deps.contains(&uri2));
    assert!(deps.contains(&uri3));

    let dependents_b = graph.get_dependents(&uri2).unwrap();
    assert_eq!(dependents_b.len(), 1);
    assert!(dependents_b.contains(&uri1));

    let dependents_c = graph.get_dependents(&uri3).unwrap();
    assert_eq!(dependents_c.len(), 1);
    assert!(dependents_c.contains(&uri1));
}

#[test]
fn dependency_graph_remove_edges() {
    let mut graph = DependencyGraph::new();

    let uri1 = Url::parse("file:///test/a.py").unwrap();
    let uri2 = Url::parse("file:///test/b.py").unwrap();
    let uri3 = Url::parse("file:///test/c.py").unwrap();

    graph.add_edge(&uri1, &uri2);
    graph.add_edge(&uri1, &uri3);
    graph.rm_edges(&uri1);

    assert!(graph.get_dependencies(&uri1).is_none());
    assert!(graph.get_dependents(&uri2).is_none());
    assert!(graph.get_dependents(&uri3).is_none());
}

#[test]
fn dependency_graph_circular() {
    let mut graph = DependencyGraph::new();

    let uri1 = Url::parse("file:///test/a.py").unwrap();
    let uri2 = Url::parse("file:///test/b.py").unwrap();
    let uri3 = Url::parse("file:///test/c.py").unwrap();

    graph.add_edge(&uri1, &uri2);
    graph.add_edge(&uri2, &uri3);
    graph.add_edge(&uri3, &uri1);

    let sccs = graph.compute_sccs();

    assert_eq!(sccs.len(), 1);
    assert_eq!(sccs[0].len(), 3);
    assert!(sccs[0].contains(&uri1));
    assert!(sccs[0].contains(&uri2));
    assert!(sccs[0].contains(&uri3));
}

#[test]
fn dependency_graph_no_circular() {
    let mut graph = DependencyGraph::new();

    let uri1 = Url::parse("file:///test/a.py").unwrap();
    let uri2 = Url::parse("file:///test/b.py").unwrap();
    let uri3 = Url::parse("file:///test/c.py").unwrap();

    graph.add_edge(&uri1, &uri2);
    graph.add_edge(&uri2, &uri3);

    let sccs = graph.compute_sccs();

    assert_eq!(sccs.len(), 3);
    for scc in sccs {
        assert_eq!(scc.len(), 1);
    }
}

#[test]
fn dependency_graph_analysis_order() {
    let mut graph = DependencyGraph::new();

    let uri1 = Url::parse("file:///test/a.py").unwrap();
    let uri2 = Url::parse("file:///test/b.py").unwrap();
    let uri3 = Url::parse("file:///test/c.py").unwrap();
    let uri4 = Url::parse("file:///test/d.py").unwrap();

    graph.add_edge(&uri1, &uri2);
    graph.add_edge(&uri1, &uri3);
    graph.add_edge(&uri2, &uri4);
    graph.add_edge(&uri3, &uri4);

    let order = graph.analysis_order();

    assert_eq!(order.len(), 4);

    let positions: std::collections::HashMap<_, _> = order
        .iter()
        .enumerate()
        .flat_map(|(i, group)| group.iter().map(move |uri| (uri, i)))
        .collect();

    let pos_a = positions.get(&uri1).unwrap();
    let pos_b = positions.get(&uri2).unwrap();
    let pos_c = positions.get(&uri3).unwrap();
    let pos_d = positions.get(&uri4).unwrap();

    assert!(pos_d < pos_b);
    assert!(pos_d < pos_c);
    assert!(pos_b < pos_a);
    assert!(pos_c < pos_a);
}

#[test]
fn stub_file_parsing() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(None, config, documents);
    let mut stub_file = NamedTempFile::new().unwrap();

    writeln!(
        stub_file,
        "def foo(x: int, y: str) -> bool: ...\nclass MyClass: ...\nmy_var: list[int]"
    )
    .unwrap();
    stub_file.flush().unwrap();

    let result = workspace.parse_stub_file(stub_file.path());
    assert!(result.is_ok());

    let stub = result.unwrap();
    assert!(!stub.exports.is_empty());
    assert!(stub.exports.contains_key("foo"));

    if let Some(ty) = stub.exports.get("foo") {
        assert!(matches!(ty, Type::Fun(_, _)));
    }

    assert!(stub.exports.contains_key("MyClass"));
    assert!(stub.exports.contains_key("my_var"));
}

#[test]
fn stub_resolution_order() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(None, config, documents);

    assert!(!workspace.has_stub("nonexistent_module"));
}

#[test]
fn annotation_parser_integration() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(None, config, documents);

    let ty = workspace.parse_annotation_string("list[int]");
    assert!(ty.is_some());

    let ty = workspace.parse_annotation_string("dict[str, bool]");
    assert!(ty.is_some());

    let ty = workspace.parse_annotation_string("Generic[T]");
    assert!(ty.is_some());

    let ty = workspace.parse_annotation_string("invalid[[[");
    assert!(ty.is_none());
}

#[test]
fn parse_stub_from_string() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(None, config, documents);
    let stub_content = r#"
"""Test stub file."""

class TestClass:
   def test_method(self) -> int: ...

def test_function(x: str) -> bool: ...
"#;

    let result = workspace.parse_stub_from_string("test_module", stub_content);
    assert!(result.is_ok(), "Failed to parse stub: {:?}", result.err());

    let stub = result.unwrap();
    assert_eq!(stub.module, "test_module");
    assert_eq!(stub.path, PathBuf::from("<embedded>/test_module.pyi"));
    assert!(!stub.is_partial);
    assert!(stub.exports.contains_key("TestClass"));
    assert!(stub.exports.contains_key("test_function"));
}

#[test]
fn embedded_typeshed_stubs_available() {
    let stdlib_modules = beacon_analyzer::EMBEDDED_STDLIB_MODULES;

    for module_name in stdlib_modules.iter().copied() {
        let stub = beacon_analyzer::get_embedded_stub(module_name);
        assert!(stub.is_some(), "Typeshed stub for '{module_name}' should be available");

        let stub = stub.unwrap();
        assert_eq!(stub.module, module_name);
        assert!(!stub.is_partial);
        assert!(
            stub.content.is_some(),
            "Typeshed stub for '{module_name}' should have content"
        );
    }
}

#[test]
fn load_builtin_stubs() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(None, config, documents);

    workspace.load_builtin_stubs();

    let cache = workspace.stubs.read().unwrap();
    let expected_modules = vec!["builtins", "typing", "dataclasses", "os", "enum", "pathlib"];
    for module_name in expected_modules {
        assert!(cache.contains(module_name), "Stdlib module {module_name} not loaded");
    }

    drop(cache);
    for module_name in ["builtins", "typing", "dataclasses", "os", "enum", "pathlib"] {
        assert!(
            workspace.index.get_by_name(module_name).is_some(),
            "Stdlib module {module_name} not registered in index"
        );
    }
}

#[test]
fn stdlib_stubs_have_expected_exports() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();

    let mut workspace = Workspace::new(None, config, documents);
    workspace.load_builtin_stubs();
    let cache = workspace.stubs.read().unwrap();

    let builtins = cache.get("builtins").expect("builtins not loaded");
    assert!(builtins.exports.contains_key("int"));
    assert!(builtins.exports.contains_key("str"));
    assert!(builtins.exports.contains_key("list"));
    assert!(builtins.exports.contains_key("dict"));

    let typing = cache.get("typing").expect("typing not loaded");
    assert!(typing.exports.contains_key("List"));
    assert!(typing.exports.contains_key("Dict"));
    assert!(typing.exports.contains_key("Optional"));

    let dataclasses = cache.get("dataclasses").expect("dataclasses not loaded");
    assert!(dataclasses.exports.contains_key("dataclass"));
    assert!(dataclasses.exports.contains_key("field"));

    let os = cache.get("os").expect("os not loaded");
    assert!(os.exports.contains_key("path"));
    assert!(os.exports.contains_key("getcwd"));

    let enum_stub = cache.get("enum").expect("enum not loaded");
    assert!(enum_stub.exports.contains_key("Enum"));
    assert!(enum_stub.exports.contains_key("IntEnum"));

    let pathlib = cache.get("pathlib").expect("pathlib not loaded");
    assert!(pathlib.exports.contains_key("Path"));
    assert!(pathlib.exports.contains_key("PurePath"));
}

#[test]
fn stdlib_imports_can_be_resolved() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(None, config, documents);

    workspace.load_builtin_stubs();

    let stdlib_modules = beacon_analyzer::EMBEDDED_STDLIB_MODULES;
    for module_name in stdlib_modules.iter().copied() {
        let resolved = workspace.resolve_import(module_name);
        assert!(resolved.is_some(), "Failed to resolve stdlib import '{module_name}'");

        let uri = resolved.unwrap();
        assert_eq!(
            uri.scheme(),
            "builtin",
            "Expected builtin:// scheme for {module_name}, got {}",
            uri.scheme()
        );
    }
}

#[test]
fn initialize_registers_stdlib_modules() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(None, config, documents.clone());

    workspace.initialize().unwrap();

    let stdlib_modules = beacon_analyzer::EMBEDDED_STDLIB_MODULES;
    for module_name in stdlib_modules.iter().copied() {
        let resolved = workspace.resolve_import(module_name);
        assert!(
            resolved.is_some(),
            "Failed to resolve stdlib import '{module_name}' after initialize()"
        );
    }

    let test_uri = Url::parse("file:///test.py").unwrap();
    documents
        .open_document(
            test_uri.clone(),
            1,
            "from typing import List\nfrom dataclasses import dataclass\n",
        )
        .unwrap();

    let unresolved = workspace.unresolved_imports(&test_uri);
    assert!(
        unresolved.is_empty(),
        "Expected no unresolved imports, got: {unresolved:?}"
    );
}

#[test]
fn stdlib_loaded_without_root_uri() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(None, config, documents);

    workspace.initialize().unwrap();

    for module_name in ["typing", "dataclasses", "os", "enum", "pathlib"] {
        assert!(
            workspace.resolve_import(module_name).is_some(),
            "Stdlib module {module_name} not available without root_uri"
        );
    }
}

#[test]
fn typing_module_protocol_types_available() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(None, config, documents);

    workspace.initialize().unwrap();

    let typing_exports = workspace
        .get_stub_exports("typing")
        .expect("typing module should be loaded");

    let protocol_types = vec![
        "Generator",
        "Iterator",
        "Iterable",
        "AsyncGenerator",
        "AsyncIterator",
        "AsyncIterable",
        "Sequence",
        "Mapping",
        "cast",
        "overload",
    ];

    for protocol_name in protocol_types {
        assert!(
            typing_exports.contains_key(protocol_name),
            "typing module should export {protocol_name}"
        );
    }

    assert!(
        workspace.get_stub_type("typing", "Generator").is_some(),
        "typing.Generator should be available"
    );
    assert!(
        workspace.get_stub_type("typing", "Iterator").is_some(),
        "typing.Iterator should be available"
    );
}

#[test]
fn discover_files_with_custom_exclude_patterns() {
    let temp_dir = TempDir::new().unwrap();
    let root_path = temp_dir.path();

    fs::create_dir_all(root_path.join("src")).unwrap();
    fs::create_dir_all(root_path.join("tests")).unwrap();
    fs::create_dir_all(root_path.join("build")).unwrap();
    fs::create_dir_all(root_path.join("venv")).unwrap();

    fs::write(root_path.join("src/main.py"), "# main").unwrap();
    fs::write(root_path.join("tests/test_main.py"), "# test").unwrap();
    fs::write(root_path.join("build/generated.py"), "# build").unwrap();
    fs::write(root_path.join("venv/lib.py"), "# venv").unwrap();

    let config =
        Config { exclude_patterns: vec!["**/build/**".to_string(), "**/venv/**".to_string()], ..Default::default() };
    let documents = DocumentManager::new().unwrap();
    let root_uri = Url::from_directory_path(root_path).unwrap();
    let mut workspace = Workspace::new(Some(root_uri), config, documents);

    workspace.discover_files().unwrap();

    let indexed_files = workspace.all_indexed_files();
    let file_paths: Vec<String> = indexed_files.iter().map(|uri| uri.path().to_string()).collect();

    assert!(
        file_paths.iter().any(|p| p.contains("src/main.py")),
        "src/main.py should be indexed"
    );
    assert!(
        file_paths.iter().any(|p| p.contains("tests/test_main.py")),
        "tests/test_main.py should be indexed"
    );
    assert!(
        !file_paths.iter().any(|p| p.contains("build/generated.py")),
        "build/generated.py should be excluded"
    );
    assert!(
        !file_paths.iter().any(|p| p.contains("venv/lib.py")),
        "venv/lib.py should be excluded"
    );
}

#[test]
fn discover_files_default_excludes_venv() {
    let temp_dir = TempDir::new().unwrap();
    let root_path = temp_dir.path();

    fs::create_dir_all(root_path.join("src")).unwrap();
    fs::create_dir_all(root_path.join(".venv/lib/python3.12")).unwrap();
    fs::create_dir_all(root_path.join("venv/lib")).unwrap();

    fs::write(root_path.join("src/app.py"), "# app").unwrap();
    fs::write(root_path.join(".venv/lib/python3.12/site.py"), "# site").unwrap();
    fs::write(root_path.join("venv/lib/foo.py"), "# foo").unwrap();

    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let root_uri = Url::from_directory_path(root_path).unwrap();
    let mut workspace = Workspace::new(Some(root_uri), config, documents);

    workspace.discover_files().unwrap();

    let indexed_files = workspace.all_indexed_files();
    let file_paths: Vec<String> = indexed_files.iter().map(|uri| uri.path().to_string()).collect();

    assert!(
        file_paths.iter().any(|p| p.contains("src/app.py")),
        "src/app.py should be indexed"
    );
    assert!(
        !file_paths.iter().any(|p| p.contains(".venv")),
        ".venv files should be excluded by default"
    );
    assert!(
        !file_paths.iter().any(|p| p.contains("venv/lib/foo.py")),
        "venv files should be excluded by default"
    );
}

#[test]
fn discover_files_excludes_cache_directories() {
    let temp_dir = TempDir::new().unwrap();
    let root_path = temp_dir.path();

    fs::create_dir_all(root_path.join("src")).unwrap();
    fs::create_dir_all(root_path.join("__pycache__")).unwrap();
    fs::create_dir_all(root_path.join(".mypy_cache")).unwrap();
    fs::create_dir_all(root_path.join(".pytest_cache")).unwrap();

    fs::write(root_path.join("src/module.py"), "# module").unwrap();
    fs::write(root_path.join("__pycache__/module.cpython-312.pyc"), "# compiled").unwrap();
    fs::write(root_path.join(".mypy_cache/cache.py"), "# cache").unwrap();
    fs::write(root_path.join(".pytest_cache/data.py"), "# pytest").unwrap();

    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let root_uri = Url::from_directory_path(root_path).unwrap();
    let mut workspace = Workspace::new(Some(root_uri), config, documents);

    workspace.discover_files().unwrap();

    let indexed_files = workspace.all_indexed_files();
    let file_paths: Vec<String> = indexed_files.iter().map(|uri| uri.path().to_string()).collect();

    assert!(
        file_paths.iter().any(|p| p.contains("src/module.py")),
        "src/module.py should be indexed"
    );
    assert!(
        !file_paths.iter().any(|p| p.contains("__pycache__")),
        "__pycache__ should be excluded"
    );
    assert!(
        !file_paths.iter().any(|p| p.contains(".mypy_cache")),
        ".mypy_cache should be excluded"
    );
    assert!(
        !file_paths.iter().any(|p| p.contains(".pytest_cache")),
        ".pytest_cache should be excluded"
    );
}

#[test]
fn exclude_patterns_normalization() {
    let temp_dir = TempDir::new().unwrap();
    let root_path = temp_dir.path();

    fs::create_dir_all(root_path.join("src")).unwrap();
    fs::create_dir_all(root_path.join("dist")).unwrap();
    fs::create_dir_all(root_path.join("node_modules")).unwrap();

    fs::write(root_path.join("src/app.py"), "# app").unwrap();
    fs::write(root_path.join("dist/bundle.py"), "# bundle").unwrap();
    fs::write(root_path.join("node_modules/pkg.py"), "# pkg").unwrap();

    let config =
        Config { exclude_patterns: vec!["dist/".to_string(), "!node_modules/".to_string()], ..Default::default() };
    let documents = DocumentManager::new().unwrap();
    let root_uri = Url::from_directory_path(root_path).unwrap();
    let mut workspace = Workspace::new(Some(root_uri), config, documents);

    workspace.discover_files().unwrap();

    let indexed_files = workspace.all_indexed_files();
    let file_paths: Vec<String> = indexed_files.iter().map(|uri| uri.path().to_string()).collect();

    assert!(
        file_paths.iter().any(|p| p.contains("src/app.py")),
        "src/app.py should be indexed"
    );
    assert!(
        !file_paths.iter().any(|p| p.contains("dist/bundle.py")),
        "dist/bundle.py should be excluded (pattern without !)"
    );
    assert!(
        !file_paths.iter().any(|p| p.contains("node_modules/pkg.py")),
        "node_modules/pkg.py should be excluded (pattern with !)"
    );
}

#[test]
fn extract_all_exports() {
    let temp_dir = TempDir::new().unwrap();
    let test_file = temp_dir.path().join("test_module.py");

    let source_code = r#"
def foo():
   pass

def bar():
   pass

__all__ = ["foo", "bar", "baz"]
"#;

    fs::write(&test_file, source_code).unwrap();

    let uri = Url::from_file_path(&test_file).unwrap();
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    documents.open_document(uri.clone(), 1, source_code).unwrap();
    let workspace = Workspace::new(Some(uri.clone()), config, documents);

    let all_exports = workspace.extract_all_exports(&uri);
    assert!(all_exports.is_some());
    let exports = all_exports.unwrap();
    assert_eq!(exports.len(), 3);
    assert!(exports.contains(&"foo".to_string()));
    assert!(exports.contains(&"bar".to_string()));
    assert!(exports.contains(&"baz".to_string()));
}

#[test]
fn extract_all_exports_empty() {
    let temp_dir = TempDir::new().unwrap();
    let test_file = temp_dir.path().join("test_module.py");

    let source_code = r#"
def foo():
   pass
"#;

    fs::write(&test_file, source_code).unwrap();

    let uri = Url::from_file_path(&test_file).unwrap();
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    documents.open_document(uri.clone(), 1, source_code).unwrap();
    let workspace = Workspace::new(Some(uri.clone()), config, documents);

    let all_exports = workspace.extract_all_exports(&uri);
    assert!(all_exports.is_none());
}

#[test]
fn get_module_symbols() {
    let temp_dir = TempDir::new().unwrap();
    let test_file = temp_dir.path().join("test_module.py");

    let source_code = r#"
import os

def foo():
   pass

class Bar:
   pass

my_var = 42
"#;

    fs::write(&test_file, source_code).unwrap();

    let uri = Url::from_file_path(&test_file).unwrap();
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    documents.open_document(uri.clone(), 1, source_code).unwrap();
    let workspace = Workspace::new(Some(uri.clone()), config, documents);

    let symbols = workspace.get_module_symbols(&uri);
    assert!(symbols.contains("foo"));
    assert!(symbols.contains("Bar"));
    assert!(symbols.contains("my_var"));
    assert!(symbols.contains("os"));
}

#[test]
fn inconsistent_export_detection() {
    let temp_dir = TempDir::new().unwrap();
    let test_file = temp_dir.path().join("test_module.py");

    let source_code = r#"
def foo():
   pass

def bar():
   pass

__all__ = ["foo", "baz"]
"#;

    fs::write(&test_file, source_code).unwrap();

    let file_uri = Url::from_file_path(&test_file).unwrap();
    let root_uri = Url::from_directory_path(temp_dir.path()).unwrap();
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    documents.open_document(file_uri.clone(), 1, source_code).unwrap();
    let mut workspace = Workspace::new(Some(root_uri), config, documents);

    workspace.initialize().unwrap();

    let all_exports = workspace.get_all_exports(&file_uri);
    assert!(
        all_exports.is_some(),
        "all_exports should be populated after initialize"
    );

    let exports = all_exports.unwrap();
    let module_symbols = workspace.get_module_symbols(&file_uri);

    assert!(exports.contains(&"foo".to_string()));
    assert!(exports.contains(&"baz".to_string()));
    assert!(module_symbols.contains("foo"));
    assert!(module_symbols.contains("bar"));
    assert!(!module_symbols.contains("baz"));
}

#[test]
fn conflicting_stub_definitions() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(None, config, documents);

    let stub1_content = r#"
def my_function(x: int) -> str: ...
class MyClass: ...
my_var: int
"#;

    let stub2_content = r#"
def my_function(x: str) -> int: ...
class MyClass: ...
my_var: str
"#;

    let stub1 = workspace.parse_stub_from_string("testmodule", stub1_content).unwrap();
    let stub2 = workspace.parse_stub_from_string("testmodule", stub2_content).unwrap();

    let mut conflicts: FxHashMap<String, Vec<Type>> = FxHashMap::default();

    for symbol_name in stub1.exports.keys() {
        if let (Some(ty1), Some(ty2)) = (stub1.exports.get(symbol_name), stub2.exports.get(symbol_name))
            && ty1 != ty2
        {
            conflicts.insert(symbol_name.clone(), vec![ty1.clone(), ty2.clone()]);
        }
    }

    assert!(
        conflicts.contains_key("my_function"),
        "Should detect conflicting signatures for my_function"
    );

    assert!(
        conflicts.contains_key("my_var"),
        "Should detect conflicting types for my_var"
    );

    assert!(
        !conflicts.contains_key("MyClass"),
        "MyClass should not have conflicts (same type in both)"
    );
}

#[test]
fn get_source_function_type_basic() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(None, config, documents.clone());

    let utils_uri = Url::parse("file:///workspace/utils.py").unwrap();
    let utils_content = r#"
def greet(name: str) -> str:
   return f"Hello, {name}!"
"#;

    documents.open_document(utils_uri.clone(), 0, utils_content).unwrap();

    workspace.add_test_module(utils_uri, "utils".to_string(), std::path::PathBuf::from("/workspace"));

    let func_type = workspace.get_source_function_type("utils", "greet");
    assert!(func_type.is_some(), "Failed to get function type for 'greet'");

    if let Some(Type::Fun(params, return_type)) = func_type {
        assert_eq!(params.len(), 1, "Expected 1 parameter");
        assert_eq!(params[0].0, "name", "Parameter name should be 'name'");
        assert_eq!(
            params[0].1,
            Type::Con(beacon_core::TypeCtor::String),
            "Parameter type should be str"
        );
        assert_eq!(
            *return_type,
            Type::Con(beacon_core::TypeCtor::String),
            "Return type should be str"
        );
    } else {
        panic!("Expected function type, got something else");
    }
}

#[test]
fn get_source_function_type_multiple_params() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(None, config, documents.clone());

    let math_uri = Url::parse("file:///workspace/math_ops.py").unwrap();
    let math_content = r#"
def add(a: int, b: int) -> int:
   return a + b
"#;

    documents.open_document(math_uri.clone(), 0, math_content).unwrap();

    workspace.add_test_module(math_uri, "math_ops".to_string(), std::path::PathBuf::from("/workspace"));

    let func_type = workspace.get_source_function_type("math_ops", "add");
    assert!(func_type.is_some(), "Failed to get function type for 'add'");

    if let Some(Type::Fun(params, return_type)) = func_type {
        assert_eq!(params.len(), 2, "Expected 2 parameters");
        assert_eq!(params[0].0, "a", "First parameter name should be 'a'");
        assert_eq!(
            params[0].1,
            Type::Con(beacon_core::TypeCtor::Int),
            "First parameter type should be int"
        );
        assert_eq!(params[1].0, "b", "Second parameter name should be 'b'");
        assert_eq!(
            params[1].1,
            Type::Con(beacon_core::TypeCtor::Int),
            "Second parameter type should be int"
        );
        assert_eq!(
            *return_type,
            Type::Con(beacon_core::TypeCtor::Int),
            "Return type should be int"
        );
    } else {
        panic!("Expected function type, got something else");
    }
}

#[test]
fn get_source_function_type_missing_annotations() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(None, config, documents.clone());

    let untyped_uri = Url::parse("file:///workspace/untyped.py").unwrap();
    let untyped_content = r#"
def do_something(x, y):
   return x + y
"#;

    documents
        .open_document(untyped_uri.clone(), 0, untyped_content)
        .unwrap();

    workspace.add_test_module(
        untyped_uri,
        "untyped".to_string(),
        std::path::PathBuf::from("/workspace"),
    );

    let func_type = workspace.get_source_function_type("untyped", "do_something");
    assert!(
        func_type.is_none(),
        "Should return None for function without type annotations"
    );
}

#[test]
fn get_source_function_type_nonexistent_module() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(None, config, documents);

    let func_type = workspace.get_source_function_type("nonexistent", "some_func");
    assert!(func_type.is_none(), "Should return None for nonexistent module");
}

#[test]
fn get_source_function_type_nonexistent_function() {
    let config = Config::default();
    let documents = DocumentManager::new().unwrap();
    let mut workspace = Workspace::new(None, config, documents.clone());

    let utils_uri = Url::parse("file:///workspace/utils.py").unwrap();
    let utils_content = r#"
def greet(name: str) -> str:
   return f"Hello, {name}!"
"#;

    documents.open_document(utils_uri.clone(), 0, utils_content).unwrap();

    workspace.add_test_module(utils_uri, "utils".to_string(), std::path::PathBuf::from("/workspace"));

    let func_type = workspace.get_source_function_type("utils", "nonexistent_func");
    assert!(func_type.is_none(), "Should return None for nonexistent function");
}
