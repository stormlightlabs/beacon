use assert_cmd::cargo::cargo_bin_cmd;
use beacon_core::fixtures::{file, workspace};
use std::io::Write;
use std::path::Path;
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

fn write_temp_python(source: &str) -> tempfile::NamedTempFile {
    let mut temp_file = Builder::new().suffix(".py").tempfile().unwrap();
    write!(temp_file, "{source}").unwrap();
    temp_file
}

fn normalize_fixture_paths(output: &mut serde_json::Value) {
    normalize_path_array(output, "diagnostics");
    normalize_path_array(output, "failures");
}

fn normalize_path_array(output: &mut serde_json::Value, key: &str) {
    let Some(items) = output[key].as_array_mut() else {
        return;
    };

    for item in items {
        let Some(file_value) = item.get_mut("file") else {
            continue;
        };
        let Some(file) = file_value.as_str() else {
            continue;
        };
        if let Ok(relative) = Path::new(file).strip_prefix(workspace()) {
            *file_value = serde_json::Value::String(relative.to_string_lossy().replace('\\', "/"));
        }
    }
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
fn workspace_fixture_cli_typecheck_json_matches_golden() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("app/broken.py"))
        .assert()
        .failure();

    let mut actual: serde_json::Value =
        serde_json::from_slice(&assert.get_output().stdout).expect("CLI output should be JSON");
    normalize_fixture_paths(&mut actual);

    let expected: serde_json::Value =
        serde_json::from_str(include_str!("golden/typecheck_broken.json")).expect("golden output should be JSON");
    assert_eq!(actual, expected);
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

#[test]
fn workspace_fixture_cli_analyze_show_types_json_is_structured() {
    let temp_file = write_temp_python("value: int = 1\nname: str = \"beacon\"\n");

    let assert = cargo_bin_cmd!("beacon")
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("--show-types")
        .arg("file")
        .arg(temp_file.path())
        .assert()
        .success();

    let stdout = &assert.get_output().stdout;
    let output: serde_json::Value = serde_json::from_slice(stdout).expect("CLI output should be valid JSON");
    assert_eq!(output["schema_version"], 1);
    assert!(output["inferred_types"].as_array().unwrap().len() >= 2);
    assert!(output["diagnostics"].as_array().unwrap().is_empty());
    assert!(!String::from_utf8_lossy(stdout).contains("TODO"));
}

#[test]
fn workspace_fixture_cli_generics_typecheck_smoke() {
    cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("cases/generics.py"))
        .assert()
        .success();
}

#[test]
fn workspace_fixture_cli_protocols_typecheck_smoke() {
    cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("cases/protocols_extra.py"))
        .assert()
        .success();
}

#[test]
fn workspace_fixture_cli_data_model_extra_typecheck_smoke() {
    cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("cases/data_model_extra.py"))
        .assert()
        .success();
}

#[test]
fn workspace_fixture_cli_async_extra_typecheck_smoke() {
    cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("cases/async_extra.py"))
        .assert()
        .success();
}

#[test]
fn workspace_fixture_cli_guard_narrowing_typecheck_smoke() {
    cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("cases/guard_narrowing.py"))
        .assert()
        .success();
}

#[test]
fn workspace_fixture_cli_pattern_matching_reports_exhaustiveness() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("cases/pattern_matching.py"))
        .assert()
        .failure();

    let codes = diagnostic_codes(&assert.get_output().stdout);
    assert!(codes.contains(&"PM001".to_string()));
}

#[test]
fn workspace_fixture_cli_pattern_matching_parse_smoke() {
    cargo_bin_cmd!("beacon")
        .arg("parse")
        .arg("--json")
        .arg(file("cases/pattern_matching.py"))
        .assert()
        .success();
}

#[test]
fn workspace_fixture_cli_flow_joins_show_types_has_stable_fragments() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("--show-types")
        .arg("file")
        .arg(file("cases/flow_joins.py"))
        .assert()
        .failure();

    let output: serde_json::Value =
        serde_json::from_slice(&assert.get_output().stdout).expect("CLI output should be JSON");
    let inferred = output["inferred_types"]
        .as_array()
        .expect("inferred_types should be an array");
    let has_type_at = |line: u64, col: u64, expected: &str| {
        inferred.iter().any(|item| {
            item["span"]["start"]["line"].as_u64() == Some(line)
                && item["span"]["start"]["col"].as_u64() == Some(col)
                && item["type"].as_str().is_some_and(|ty| ty.contains(expected))
        })
    };

    assert!(has_type_at(7, 5, "int | str | None"), "branch pre-guard type missing");
    assert!(has_type_at(9, 9, "None"), "None branch type missing");
    assert!(has_type_at(12, 9, "int"), "int branch type missing");
    assert!(has_type_at(15, 9, "str"), "str branch type missing");
    assert!(has_type_at(17, 5, "str | None"), "branch join type missing");
    assert!(has_type_at(23, 5, "int | None"), "loop pre-guard type missing");
    assert!(has_type_at(30, 5, "None"), "loop join type missing");
    assert!(has_type_at(35, 5, "int | str"), "try pre-guard type missing");
    assert!(has_type_at(38, 13, "int"), "try int branch type missing");
    assert!(has_type_at(41, 13, "str"), "try str branch type missing");
    assert!(has_type_at(44, 9, "int"), "finally join type missing");
    assert!(has_type_at(49, 5, "int | str | None"), "match pre-guard type missing");
    assert!(has_type_at(53, 13, "str"), "match int label type missing");
    assert!(has_type_at(55, 13, "int"), "match int binding type missing");
    assert!(has_type_at(58, 13, "str"), "match str binding type missing");
    assert!(has_type_at(60, 5, "int | str | None"), "match join type missing");
}

#[test]
fn workspace_fixture_cli_data_flow_diagnostics_are_reported() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("file")
        .arg(file("cases/data_flow_diagnostics.py"))
        .assert()
        .failure();

    let output: serde_json::Value =
        serde_json::from_slice(&assert.get_output().stdout).expect("CLI output should be JSON");
    let diagnostics = output["diagnostics"]
        .as_array()
        .expect("diagnostics should be an array");
    let has_diagnostic = |code: &str, message_fragment: &str| {
        diagnostics.iter().any(|diagnostic| {
            diagnostic["code"] == code
                && diagnostic["message"]
                    .as_str()
                    .is_some_and(|message| message.contains(message_fragment))
        })
    };

    assert!(has_diagnostic("use-before-def", "later_value"));
    assert!(has_diagnostic("unreachable-code", "Unreachable code"));
    assert!(has_diagnostic("unused-variable", "unused_local"));
    assert!(has_diagnostic("unused-variable", "constant_dead"));
}

#[test]
fn workspace_fixture_cli_any_narrowing_show_types_has_stable_fragments() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("--show-types")
        .arg("file")
        .arg(file("cases/any_narrowing.py"))
        .assert()
        .failure();

    let output: serde_json::Value =
        serde_json::from_slice(&assert.get_output().stdout).expect("CLI output should be JSON");
    let inferred = output["inferred_types"]
        .as_array()
        .expect("inferred_types should be an array");
    let has_type_at = |line: u64, col: u64, expected: &str| {
        inferred.iter().any(|item| {
            item["span"]["start"]["line"].as_u64() == Some(line)
                && item["span"]["start"]["col"].as_u64() == Some(col)
                && item["type"].as_str().is_some_and(|ty| ty.contains(expected))
        })
    };

    assert!(has_type_at(17, 5, "Any"), "pre-guard Any should remain Any");
    assert!(has_type_at(19, 9, "str"), "isinstance true branch should get local str precision");
    assert!(has_type_at(22, 9, "Any"), "isinstance false branch should remain Any");
    assert!(has_type_at(24, 5, "Any"), "isinstance join should remain Any");
    assert!(has_type_at(30, 9, "None"), "None true branch should get local None precision");
    assert!(has_type_at(33, 9, "Any"), "None false branch should remain Any");
    assert!(has_type_at(35, 5, "Any"), "None join should remain Any");
    assert!(has_type_at(41, 9, "str"), "TypeGuard true branch should get local str precision");
    assert!(has_type_at(44, 9, "Any"), "TypeGuard false branch should remain Any");
    assert!(has_type_at(46, 5, "Any"), "TypeGuard join should remain Any");
    assert!(has_type_at(52, 9, "str"), "TypeIs true branch should get local str precision");
    assert!(has_type_at(55, 9, "Any"), "TypeIs false branch should remain Any");
    assert!(has_type_at(57, 5, "Any"), "TypeIs join should remain Any");
}

#[test]
fn workspace_fixture_cli_protocol_mismatch_reports_hm009() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("cases/protocol_mismatch.py"))
        .assert()
        .failure();

    let output: serde_json::Value =
        serde_json::from_slice(&assert.get_output().stdout).expect("CLI output should be JSON");
    let diagnostics = output["diagnostics"]
        .as_array()
        .expect("diagnostics should be an array");
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic["code"] == "HM009"
            && diagnostic["message"]
                .as_str()
                .is_some_and(|message| message.contains("MissingClose") && message.contains("Protocol<Closeable>"))
            && diagnostic["span"]["start"]["line"] == 21
            && diagnostic["span"]["start"]["col"] == 10
    }));
}

#[test]
fn workspace_fixture_cli_call_parameter_metadata_reports_hm011() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("typecheck")
        .arg("--format")
        .arg("json")
        .arg(file("cases/call_diagnostics.py"))
        .assert()
        .failure();

    let output: serde_json::Value =
        serde_json::from_slice(&assert.get_output().stdout).expect("CLI output should be JSON");
    let diagnostics = output["diagnostics"]
        .as_array()
        .expect("diagnostics should be an array");
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic["code"] == "HM011"
            && diagnostic["message"]
                .as_str()
                .is_some_and(|message| message.contains("unexpected keyword argument: 'user_id'"))
    }));
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic["code"] == "HM011"
            && diagnostic["message"]
                .as_str()
                .is_some_and(|message| message.contains("missing required argument: 'required'"))
    }));
}

#[test]
fn workspace_fixture_cli_data_model_show_types_has_stable_fragments() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("--show-types")
        .arg("file")
        .arg(file("cases/data_model_extra.py"))
        .assert()
        .failure();

    let output: serde_json::Value =
        serde_json::from_slice(&assert.get_output().stdout).expect("CLI output should be JSON");
    let inferred = output["inferred_types"]
        .as_array()
        .expect("inferred_types should be an array");
    let has_type_at = |line: u64, col: u64, expected: &str| {
        inferred.iter().any(|item| {
            item["span"]["start"]["line"].as_u64() == Some(line)
                && item["span"]["start"]["col"].as_u64() == Some(col)
                && item["type"].as_str().is_some_and(|ty| ty.contains(expected))
        })
    };

    assert!(has_type_at(55, 5, "(name: str) -> Config"));
    assert!(has_type_at(59, 5, "(job: Job, state: State) -> str"));
    assert!(has_type_at(65, 1, "Job"));
}

#[test]
fn workspace_fixture_cli_async_show_types_has_stable_fragments() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("--show-types")
        .arg("file")
        .arg(file("cases/async_extra.py"))
        .assert()
        .failure();

    let output: serde_json::Value =
        serde_json::from_slice(&assert.get_output().stdout).expect("CLI output should be JSON");
    let inferred = output["inferred_types"]
        .as_array()
        .expect("inferred_types should be an array");
    let has_type_at = |line: u64, col: u64, expected: &str| {
        inferred.iter().any(|item| {
            item["span"]["start"]["line"].as_u64() == Some(line)
                && item["span"]["start"]["col"].as_u64() == Some(col)
                && item["type"].as_str().is_some_and(|ty| ty.contains(expected))
        })
    };

    assert!(has_type_at(15, 11, "Coroutine"));
    assert!(has_type_at(19, 11, "AsyncGenerator Record"));
    assert!(has_type_at(24, 5, "Generator str"));
    assert!(has_type_at(39, 11, "Coroutine"));
}

#[test]
fn workspace_fixture_cli_typing_breadth_show_types_has_stable_fragments() {
    let assert = cargo_bin_cmd!("beacon")
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("--show-types")
        .arg("file")
        .arg(file("cases/typing_breadth.py"))
        .assert()
        .failure();

    let output: serde_json::Value =
        serde_json::from_slice(&assert.get_output().stdout).expect("CLI output should be JSON");
    let inferred = output["inferred_types"]
        .as_array()
        .expect("inferred_types should be an array");
    let has_type_at = |line: u64, col: u64, expected: &str| {
        inferred.iter().any(|item| {
            item["span"]["start"]["line"].as_u64() == Some(line)
                && item["span"]["start"]["col"].as_u64() == Some(col)
                && item["type"].as_str().is_some_and(|ty| ty.contains(expected))
        })
    };

    assert!(has_type_at(65, 5, "(value: T) -> T"), "identity function type missing");
    assert!(has_type_at(104, 5, "tuple[list[str], dict[str, int], set[Status]]"));
    assert!(has_type_at(140, 5, "tuple[bool, str, int]"));
}

#[test]
fn workspace_fixture_cli_analyze_show_cfg_compact_uses_cfg_blocks() {
    let temp_file = write_temp_python(
        "# beacon: mode=relaxed\ndef choose(flag: bool) -> int:\n    if flag:\n        return 1\n    return 0\n",
    );

    let assert = cargo_bin_cmd!("beacon")
        .arg("analyze")
        .arg("--format")
        .arg("compact")
        .arg("--show-cfg")
        .arg("file")
        .arg(temp_file.path())
        .assert()
        .success();

    let stdout = String::from_utf8_lossy(&assert.get_output().stdout);
    assert!(stdout.contains("cfg:"));
    assert!(stdout.contains("cfg-block:"));
    assert!(!stdout.contains("TODO"));
}

#[cfg(debug_assertions)]
#[test]
fn workspace_fixture_cli_debug_unify_prints_trace() {
    let temp_file = write_temp_python("value: int = 1\n");

    let assert = cargo_bin_cmd!("beacon")
        .arg("debug")
        .arg("unify")
        .arg(temp_file.path())
        .assert()
        .success();

    let stdout = String::from_utf8_lossy(&assert.get_output().stdout);
    assert!(stdout.contains("Unification trace"));
    assert!(stdout.contains("generated constraints and invoked solver"));
    assert!(stdout.contains("Unification completed"));
    assert!(!stdout.contains("TODO"));
}

#[test]
fn workspace_fixture_cli_analyze_suppressions_fixture_has_no_diagnostics() {
    cargo_bin_cmd!("beacon")
        .arg("analyze")
        .arg("--format")
        .arg("json")
        .arg("file")
        .arg(file("cases/suppressions.py"))
        .assert()
        .success();
}

#[test]
fn workspace_fixture_cli_analyze_respects_suppression_categories() {
    let cases = [
        "def type_error() -> int:\n    return \"bad\"  # type: ignore[HM001]\n",
        "return 42  # noqa: BEA003\n",
        "def data_flow() -> None:\n    print(value)  # type: ignore\n    value = 1  # type: ignore[ANN002]\n    _ = value  # type: ignore[ANN002]\n",
        "import missing_module  # type: ignore  # noqa: BEA015\n",
    ];

    for source in cases {
        let temp_file = write_temp_python(source);
        let assert = cargo_bin_cmd!("beacon")
            .arg("analyze")
            .arg("--format")
            .arg("json")
            .arg("file")
            .arg(temp_file.path())
            .assert()
            .success();

        let output: serde_json::Value =
            serde_json::from_slice(&assert.get_output().stdout).expect("CLI output should be JSON");
        assert_eq!(output["diagnostics"].as_array().unwrap().len(), 0);
    }
}

#[test]
fn workspace_fixture_cli_analyze_reports_unsuppressed_categories() {
    let cases = [
        ("def type_error() -> int:\n    return \"bad\"\n", "HM001"),
        ("return 42\n", "BEA003"),
        (
            "def data_flow() -> None:\n    print(value)\n    value = 1\n    _ = value\n",
            "use-before-def",
        ),
        ("import missing_module\n", "unresolved-import"),
    ];

    for (source, expected_code) in cases {
        let temp_file = write_temp_python(source);
        let assert = cargo_bin_cmd!("beacon")
            .arg("analyze")
            .arg("--format")
            .arg("json")
            .arg("file")
            .arg(temp_file.path())
            .assert()
            .failure();

        let codes = diagnostic_codes(&assert.get_output().stdout);
        assert!(
            codes.contains(&expected_code.to_string()),
            "expected {expected_code} in {codes:?}"
        );
    }
}
