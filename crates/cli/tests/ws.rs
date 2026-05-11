use assert_cmd::cargo::cargo_bin_cmd;
use beacon_core::fixtures::file;
use std::io::Write;
use tempfile::Builder;

fn diagnostic_codes(stdout: &[u8]) -> Vec<String> {
    let output: serde_json::Value = serde_json::from_slice(stdout).expect("CLI output should be JSON");
    output["diagnostics"]
        .as_array()
        .expect("diagnostics should be an array")
        .iter()
        .map(|diagnostic| {
            diagnostic["code"]
                .as_str()
                .expect("diagnostic code should be a string")
                .to_string()
        })
        .collect()
}

#[test]
fn workspace_fixture_cli_parse_smoke() {
    cargo_bin_cmd!("beacon")
        .arg("parse")
        .arg("--json")
        .arg(file("app/models.py"))
        .assert()
        .success();
}

#[test]
fn workspace_fixture_cli_typecheck_json_smoke() {
    cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("app/dynamic.py"))
        .assert()
        .success();
}

#[test]
fn workspace_fixture_cli_typecheck_json_uses_lsp_diagnostics() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("app/broken.py"))
        .assert()
        .failure();

    let codes = diagnostic_codes(&assert.get_output().stdout);
    assert!(codes.contains(&"HM001".to_string()));
    assert!(codes.contains(&"HM007".to_string()));
    assert!(!codes.contains(&"MODE_INFO".to_string()));
}

#[test]
fn workspace_fixture_cli_lint_json_uses_lsp_diagnostics() {
    let mut temp_file = Builder::new().suffix(".py").tempfile().unwrap();
    writeln!(temp_file, "import os\nvalue = 1").unwrap();

    let assert = cargo_bin_cmd!("beacon")
        .arg("lint")
        .arg("--format")
        .arg("json")
        .arg(temp_file.path())
        .assert()
        .failure();

    let codes = diagnostic_codes(&assert.get_output().stdout);
    assert!(!codes.is_empty());
    assert!(codes.iter().all(|code| code.starts_with("BEA")));
}

#[test]
fn workspace_fixture_cli_analyze_file_json_uses_lsp_diagnostics() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("file")
        .arg(file("app/broken.py"))
        .assert()
        .failure();

    let codes = diagnostic_codes(&assert.get_output().stdout);
    assert!(codes.contains(&"HM001".to_string()));
    assert!(codes.contains(&"MODE_INFO".to_string()));
}
