use beacon_cli::diagnostics::run_workspace_diagnostics;
use criterion::{Criterion, criterion_group, criterion_main};
use std::fs;
use tempfile::TempDir;

fn bench_workspace_analysis(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    fs::write(
        root.join("base.py"),
        r#"
class Base:
    def __init__(self, x: int):
        self.x = x

    def get(self) -> int:
        return self.x
"#,
    )
    .unwrap();

    for i in 0..10 {
        fs::write(
            root.join(format!("mod_{}.py", i)),
            format!(
                r#"
from .base import Base

def process_{}(b: Base) -> int:
    return b.get() * {}
"#,
                i, i
            ),
        )
        .unwrap();
    }

    let imports: String = (0..10)
        .map(|i| format!("from .mod_{} import process_{}\n", i, i))
        .collect();
    let usage: String = (0..10).map(|i| format!("process_{}(Base(10))\n", i)).collect();
    fs::write(
        root.join("__init__.py"),
        format!("{}\nfrom .base import Base\n\n{}", imports, usage),
    )
    .unwrap();

    let paths = vec![root.to_path_buf()];
    let root_path = Some(root.to_path_buf());

    c.bench_function("workspace_analysis_10_files", |b| {
        b.to_async(&rt).iter(|| async {
            let _ = run_workspace_diagnostics(paths.clone(), root_path.clone())
                .await
                .unwrap();
        })
    });
}

criterion_group!(benches, bench_workspace_analysis);
criterion_main!(benches);
