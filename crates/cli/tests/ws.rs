use assert_cmd::cargo::cargo_bin_cmd;
use beacon_core::fixtures::file;

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
