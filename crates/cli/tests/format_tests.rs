use assert_cmd::{Command, cargo::cargo_bin_cmd};
use predicates::prelude::*;
use std::fs;
use std::io::Write;
use tempfile::NamedTempFile;

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
fn format_write_updates_file_in_place() {
    let mut file = NamedTempFile::new().expect("tmp file");
    writeln!(file, "x=1").expect("write input");
    let path = file.path().to_path_buf();

    let mut cmd = cli();
    cmd.args(["format", path.to_str().expect("path"), "--write"])
        .assert()
        .success()
        .stdout(predicate::str::is_empty());

    let contents = fs::read_to_string(&path).expect("read output");
    assert!(contents.starts_with("x = 1"));
}

fn cli() -> Command {
    cargo_bin_cmd!("beacon")
}
