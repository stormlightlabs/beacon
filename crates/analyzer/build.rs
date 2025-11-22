use std::env;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

include!("src/embedded_stdlib_modules.rs");

fn main() {
    println!("cargo:rerun-if-changed=src/embedded_stdlib_modules.rs");
    println!("cargo:rerun-if-changed=../../.git/modules/typeshed");

    let out_dir = env::var("OUT_DIR").expect("OUT_DIR not set");
    let dest_path = Path::new(&out_dir).join("embedded_stubs.rs");

    let typeshed_path = PathBuf::from("../../typeshed/stubs");

    if !typeshed_path.exists() {
        eprintln!("Warning: typeshed stdlib not found at {typeshed_path:?}");
        eprintln!("Falling back to empty stub bundle");
        write_empty_stub_bundle(&dest_path);
        return;
    }

    let stub_manifest = generate_stub_manifest(&typeshed_path, EMBEDDED_STDLIB_MODULES);
    for (_, path) in &stub_manifest {
        println!("cargo:rerun-if-changed={}", path.display());
    }
    let total_stubs = stub_manifest.len();
    let version_info = generate_version_info();

    let mut output = fs::File::create(&dest_path).expect("Failed to create embedded_stubs.rs");

    write!(
        output,
        "/// Embedded typeshed stdlib stubs\n\
/// Auto-generated at build time from typeshed submodule\n\
use std::collections::HashMap;\n\
use once_cell::sync::Lazy;\n\n\
/// Typeshed version information\n\
#[derive(Debug, Clone)]\n\
pub struct TypeshedVersion {{\n\
    pub commit_hash: &'static str,\n\
    pub build_timestamp: &'static str,\n\
    pub stub_count: usize,\n\
}}\n\n\
pub static TYPESHED_VERSION: TypeshedVersion = TypeshedVersion {{\n\
    commit_hash: \"{}\",\n\
    build_timestamp: \"{}\",\n\
    stub_count: {},\n\
}};\n\n",
        version_info.0, version_info.1, total_stubs
    )
    .expect("Failed to write version info");

    write!(
        output,
        "/// Embedded stub files as a lazy-initialized HashMap\n\
pub static EMBEDDED_STUBS: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {{\n\
    let mut stubs = HashMap::new();\n"
    )
    .expect("Failed to write stub map");

    for (module_name, file_path) in stub_manifest {
        let relative_path = file_path.strip_prefix("../../").unwrap_or(&file_path);
        writeln!(
            output,
            "    stubs.insert(\"{}\", include_str!(\"../../../../../{}\"));",
            module_name,
            relative_path.display()
        )
        .expect("Failed to write stub entry");
    }

    writeln!(output, "    stubs\n}});").expect("Failed to close stub map");

    println!("cargo:warning=Bundled {total_stubs} typeshed stubs");
}

fn generate_stub_manifest(typeshed_path: &Path, modules: &[&str]) -> Vec<(String, PathBuf)> {
    modules
        .iter()
        .map(|module_name| {
            let stub_path = find_stub_file(typeshed_path, module_name).unwrap_or_else(|| {
                panic!(
                    "Failed to locate stub for module '{module_name}' in {}",
                    typeshed_path.display()
                )
            });
            ((*module_name).to_string(), stub_path)
        })
        .collect()
}

fn find_stub_file(typeshed_path: &Path, module_name: &str) -> Option<PathBuf> {
    let relative_path = module_name.replace('.', "/");
    let direct_file = typeshed_path.join(format!("{relative_path}.pyi"));
    if direct_file.exists() {
        return Some(direct_file);
    }

    let package_init = typeshed_path.join(&relative_path).join("__init__.pyi");
    if package_init.exists() {
        return Some(package_init);
    }

    None
}

fn generate_version_info() -> (String, String) {
    let commit_hash = Command::new("git")
        .args(["submodule", "status", "typeshed"])
        .output()
        .ok()
        .and_then(
            |output| {
                if output.status.success() { String::from_utf8(output.stdout).ok() } else { None }
            },
        )
        .and_then(|s| {
            s.split_whitespace()
                .next()
                .map(|h| h.trim_start_matches('-').to_string())
        })
        .unwrap_or_else(|| "unknown".to_string());

    let build_timestamp = chrono::Utc::now().to_rfc3339();

    (commit_hash, build_timestamp)
}

fn write_empty_stub_bundle(dest_path: &Path) {
    let mut output = fs::File::create(dest_path).expect("Failed to create embedded_stubs.rs");

    write!(
        output,
        "/// Embedded typeshed stdlib stubs (empty fallback)\n\
use std::collections::HashMap;\n\
use once_cell::sync::Lazy;\n\n\
#[derive(Debug, Clone)]\n\
pub struct TypeshedVersion {{\n\
    pub commit_hash: &'static str,\n\
    pub build_timestamp: &'static str,\n\
    pub stub_count: usize,\n\
}}\n\n\
pub static TYPESHED_VERSION: TypeshedVersion = TypeshedVersion {{\n\
    commit_hash: \"none\",\n\
    build_timestamp: \"none\",\n\
    stub_count: 0,\n\
}};\n\n\
pub static EMBEDDED_STUBS: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(HashMap::new);\n"
    )
    .expect("Failed to write empty stub bundle");
}
