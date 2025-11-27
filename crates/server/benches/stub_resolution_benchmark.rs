use beacon_lsp::config::Config;
use beacon_lsp::document::DocumentManager;
use beacon_lsp::workspace::Workspace;
use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use std::fs;
use std::hint::black_box;
use std::path::PathBuf;
use tempfile::TempDir;
use url::Url;

/// Create a workspace for benchmarking
fn create_benchmark_workspace() -> (Workspace, TempDir) {
    let project_dir = TempDir::new().unwrap();
    let project_path = project_dir.path().to_path_buf();

    let config = Config::default();
    let root_uri = Url::from_directory_path(&project_path).unwrap();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(Some(root_uri), config, documents);

    (workspace, project_dir)
}

/// Create a workspace with custom stub paths
fn create_workspace_with_custom_stubs(stub_count: usize) -> (Workspace, TempDir, TempDir) {
    let project_dir = TempDir::new().unwrap();
    let custom_stubs_dir = TempDir::new().unwrap();

    for i in 0..stub_count {
        let module_name = format!("custom_module_{}", i);
        let stub_path = custom_stubs_dir.path().join(format!("{}.pyi", module_name));
        let stub_content = format!(
            r#"
# Custom stub for {}
def custom_function_{}() -> str: ...
class CustomClass_{}:
    def method(self) -> None: ...
"#,
            module_name, i, i
        );
        fs::write(&stub_path, stub_content).unwrap();
    }

    let config = Config { stub_paths: vec![custom_stubs_dir.path().to_path_buf()], ..Default::default() };

    let root_uri = Url::from_directory_path(project_dir.path()).unwrap();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(Some(root_uri), config, documents);

    (workspace, project_dir, custom_stubs_dir)
}

/// Benchmark loading embedded typeshed stubs (baseline)
fn bench_typeshed_stub_loading(c: &mut Criterion) {
    let mut group = c.benchmark_group("typeshed_stub_loading");

    let common_modules = vec!["builtins", "typing", "collections", "os", "sys", "pathlib"];

    for module in &common_modules {
        group.bench_with_input(BenchmarkId::from_parameter(module), module, |b, &module| {
            let (workspace, _temp_dir) = create_benchmark_workspace();

            b.iter(|| {
                let stub = workspace.load_stub(black_box(module));
                black_box(stub)
            });
        });
    }

    group.finish();
}

/// Benchmark custom stub resolution with varying numbers of stub files
fn bench_custom_stub_resolution_scale(c: &mut Criterion) {
    let mut group = c.benchmark_group("custom_stub_resolution_scale");

    let stub_counts = vec![10, 50, 100, 500];

    for &count in &stub_counts {
        group.bench_with_input(BenchmarkId::from_parameter(count), &count, |b, &count| {
            let (workspace, _project_dir, _stubs_dir) = create_workspace_with_custom_stubs(count);

            b.iter(|| {
                let module_name = format!("custom_module_{}", count / 2);
                let stub = workspace.load_stub(black_box(&module_name));
                black_box(stub)
            });
        });
    }

    group.finish();
}

/// Benchmark stub resolution with multiple stub paths
fn bench_multiple_stub_paths(c: &mut Criterion) {
    let mut group = c.benchmark_group("multiple_stub_paths");

    let path_counts = vec![1, 3, 5, 10];

    for &count in &path_counts {
        group.bench_with_input(BenchmarkId::from_parameter(count), &count, |b, &count| {
            let project_dir = TempDir::new().unwrap();

            let mut stub_dirs: Vec<TempDir> = Vec::new();
            let mut stub_paths: Vec<PathBuf> = Vec::new();

            for i in 0..count {
                let stub_dir = TempDir::new().unwrap();
                let stub_path = stub_dir.path().join(format!("module_{}.pyi", i));
                fs::write(&stub_path, format!("# Stub {}", i)).unwrap();
                stub_paths.push(stub_dir.path().to_path_buf());
                stub_dirs.push(stub_dir);
            }

            let config = Config { stub_paths, ..Default::default() };

            let root_uri = Url::from_directory_path(project_dir.path()).unwrap();
            let documents = DocumentManager::new().unwrap();
            let workspace = Workspace::new(Some(root_uri), config, documents);

            b.iter(|| {
                let stub = workspace.load_stub(black_box("nonexistent_module"));
                black_box(stub)
            });

            drop(stub_dirs);
        });
    }

    group.finish();
}

/// Benchmark fallback to typeshed when custom stubs don't exist
fn bench_typeshed_fallback(c: &mut Criterion) {
    let custom_stubs_dir = TempDir::new().unwrap();

    let custom_stub_path = custom_stubs_dir.path().join("mymodule.pyi");
    fs::write(&custom_stub_path, "# Custom stub").unwrap();

    let project_dir = TempDir::new().unwrap();
    let config = Config { stub_paths: vec![custom_stubs_dir.path().to_path_buf()], ..Default::default() };

    let root_uri = Url::from_directory_path(project_dir.path()).unwrap();
    let documents = DocumentManager::new().unwrap();
    let workspace = Workspace::new(Some(root_uri), config, documents);

    c.bench_function("typeshed_fallback", |b| {
        b.iter(|| {
            let stub = workspace.load_stub(black_box("builtins"));
            black_box(stub)
        });
    });
}

/// Benchmark concurrent stub loading (simulating large project analysis)
fn bench_concurrent_stub_loading(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_stub_loading");

    let module_counts = vec![10, 25, 50];

    for &count in &module_counts {
        group.bench_with_input(BenchmarkId::from_parameter(count), &count, |b, &count| {
            let (workspace, _temp_dir) = create_benchmark_workspace();
            let modules: Vec<&str> = vec!["builtins", "typing", "collections", "os", "sys"]
                .into_iter()
                .cycle()
                .take(count)
                .collect();

            b.iter(|| {
                for module in &modules {
                    let stub = workspace.load_stub(black_box(module));
                    black_box(stub);
                }
            });
        });
    }

    group.finish();
}

/// Benchmark nested module resolution (e.g., collections.abc)
fn bench_nested_module_resolution(c: &mut Criterion) {
    let (workspace, _temp_dir) = create_benchmark_workspace();

    let nested_modules = vec!["collections.abc", "os.path", "email.mime.text", "http.client"];

    let mut group = c.benchmark_group("nested_module_resolution");

    for module in &nested_modules {
        group.bench_with_input(BenchmarkId::from_parameter(module), module, |b, &module| {
            b.iter(|| {
                let stub = workspace.load_stub(black_box(module));
                black_box(stub)
            });
        });
    }

    group.finish();
}

/// Benchmark stub caching behavior
fn bench_stub_caching(c: &mut Criterion) {
    let (workspace, _temp_dir) = create_benchmark_workspace();

    c.bench_function("stub_caching_repeated_loads", |b| {
        b.iter(|| {
            for _ in 0..10 {
                let stub = workspace.load_stub(black_box("builtins"));
                black_box(stub);
            }
        });
    });
}

/// Benchmark large project stub resolution overhead
fn bench_large_project_stub_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("large_project_stub_overhead");
    let project_sizes = vec![("small", 20), ("medium", 100), ("large", 500)];

    for (name, module_count) in &project_sizes {
        group.bench_with_input(BenchmarkId::from_parameter(name), module_count, |b, &count| {
            let (workspace, _project_dir, _stubs_dir) = create_workspace_with_custom_stubs(count);
            let modules: Vec<String> = (0..count)
                .map(|i| {
                    if i % 3 == 0 {
                        format!("custom_module_{}", i)
                    } else if i % 3 == 1 {
                        "builtins".to_string()
                    } else {
                        "typing".to_string()
                    }
                })
                .collect();

            b.iter(|| {
                for module in &modules {
                    let stub = workspace.load_stub(black_box(module));
                    black_box(stub);
                }
            });
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_typeshed_stub_loading,
    bench_custom_stub_resolution_scale,
    bench_multiple_stub_paths,
    bench_typeshed_fallback,
    bench_concurrent_stub_loading,
    bench_nested_module_resolution,
    bench_stub_caching,
    bench_large_project_stub_overhead,
);

criterion_main!(benches);
