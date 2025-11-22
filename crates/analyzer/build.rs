use std::env;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=../../typeshed");
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

    let (stub_manifest, total_stubs) = generate_stub_manifest(&typeshed_path);
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

fn generate_stub_manifest(typeshed_path: &Path) -> (Vec<(String, PathBuf)>, usize) {
    let mut stubs = Vec::new();

    if let Ok(entries) = fs::read_dir(typeshed_path) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("pyi") {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    let module_name = stem.to_string();
                    stubs.push((module_name, path));
                }
            } else if path.is_dir() {
                collect_stubs_from_package(&path, &mut stubs);
            }
        }
    }

    let count = stubs.len();
    (stubs, count)
}

fn collect_stubs_from_package(package_path: &Path, stubs: &mut Vec<(String, PathBuf)>) {
    let package_name = package_path.file_name().and_then(|s| s.to_str());
    if package_name.is_none() {
        return;
    }
    let package_name = package_name.unwrap();

    if package_name.starts_with('@') || package_name.starts_with('.') {
        return;
    }

    let init_stub = package_path.join("__init__.pyi");
    if init_stub.exists() {
        stubs.push((package_name.to_string(), init_stub));
    }

    if let Ok(entries) = fs::read_dir(package_path) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("pyi") {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    if stem != "__init__" {
                        let module_name = format!("{package_name}.{stem}");
                        stubs.push((module_name, path));
                    }
                }
            }
        }
    }
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
