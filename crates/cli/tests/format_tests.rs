use assert_cmd::{Command, cargo::cargo_bin_cmd};
use beacon_core::fixtures::file;
use predicates::prelude::*;
use std::fs;
use std::io::Write;
use tempfile::Builder;

#[test]
fn format_reads_from_stdin_and_writes_to_stdout() {
    let mut cmd = cli();
    cmd.arg("format")
        .write_stdin("x=1\n")
        .assert()
        .success()
        .stdout(predicate::str::contains("x = 1"));
}

#[test]
fn format_check_detects_differences() {
    let mut cmd = cli();
    cmd.args(["format", "--check"])
        .write_stdin("x=1\n")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Formatting would change"));
}

#[test]
fn format_check_respects_fmt_skip() {
    let mut cmd = cli();
    cmd.args(["format", "--check"])
        .write_stdin("x=1  # fmt: skip\n")
        .assert()
        .success();
}

#[test]
fn format_check_respects_fmt_off_block() {
    let mut cmd = cli();
    cmd.args(["format", "--check"])
        .write_stdin("# fmt: off\nx=1\n# fmt: on\n")
        .assert()
        .success();
}

#[test]
fn format_check_respects_workspace_suppression_fixture() {
    let mut cmd = cli();
    cmd.args([
        "format",
        "--check",
        file("cases/formatting/suppressions.py").to_str().expect("fixture path"),
    ])
    .assert()
    .success();
}

#[test]
fn format_write_updates_file_in_place() {
    let mut file = Builder::new().suffix(".py").tempfile().expect("tmp file");
    writeln!(file, "x=1").expect("write input");
    let path = file.path().to_path_buf();

    let mut cmd = cli();
    cmd.args(["format", path.to_str().expect("path"), "--write"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Formatted"));

    let contents = fs::read_to_string(&path).expect("read output");
    assert!(contents.starts_with("x = 1"));
}

fn cli() -> Command {
    cargo_bin_cmd!("beacon")
}
