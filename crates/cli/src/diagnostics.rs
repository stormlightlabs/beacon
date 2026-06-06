use crate::helpers;

use anyhow::Result;
use beacon_lsp::{
    Config, analysis::Analyzer, document::DocumentManager, features::DiagnosticProvider, workspace::Workspace,
};
use clap::ValueEnum;
use lsp_types::{DiagnosticSeverity, DiagnosticTag, Position, Url};
use owo_colors::OwoColorize;
use serde_json::json;
use std::path::PathBuf;

pub type CliDiagnostic = (PathBuf, String, lsp_types::Diagnostic);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagnosticFilter {
    All,
    Typecheck,
    Lint,
    DataFlow,
}

#[derive(Clone, Copy, Debug, ValueEnum, Default)]
pub enum OutputFormat {
    #[default]
    Human,
    Json,
    Compact,
}

pub async fn run_workspace_diagnostics(
    paths: Vec<PathBuf>, workspace_root: Option<PathBuf>,
) -> Result<(Vec<CliDiagnostic>, Vec<(PathBuf, String)>)> {
    let files = helpers::discover_python_files(&paths)?;
    let files: Vec<PathBuf> = files.into_iter().filter_map(|p| p.canonicalize().ok()).collect();

    let documents = DocumentManager::new()?;
    let config = Config::default();
    let mut analyzer = Analyzer::new(config.clone(), documents.clone());

    let root_uri = workspace_root.and_then(|p| Url::from_file_path(p).ok());

    let mut workspace = Workspace::new(root_uri, config, documents.clone());
    workspace.initialize()?;
    let workspace = std::sync::Arc::new(tokio::sync::RwLock::new(workspace));
    let diagnostic_provider = DiagnosticProvider::new(documents.clone(), workspace.clone());

    let mut all_diagnostics = Vec::new();
    let mut failed_files = Vec::new();

    for file_path in &files {
        match std::fs::read_to_string(file_path) {
            Ok(source) => {
                let uri = match Url::from_file_path(file_path) {
                    Ok(u) => u,
                    Err(_) => {
                        failed_files.push((file_path.clone(), "Failed to create URL".to_string()));
                        continue;
                    }
                };

                let _ = documents.open_document(uri.clone(), 1, &source);

                {
                    let mut ws = workspace.write().await;
                    ws.update_dependencies(&uri);
                }
            }
            Err(e) => {
                failed_files.push((file_path.clone(), e.to_string()));
            }
        }
    }

    for file_path in &files {
        let uri = match Url::from_file_path(file_path) {
            Ok(u) => u,
            Err(_) => continue,
        };

        let diagnostics = diagnostic_provider.generate_diagnostics(&uri, &mut analyzer);

        let source = documents.get_document(&uri, |d| d.text()).unwrap_or_default();
        for diagnostic in diagnostics {
            all_diagnostics.push((file_path.clone(), source.clone(), diagnostic));
        }
    }

    Ok((all_diagnostics, failed_files))
}

pub async fn run_stdin_diagnostics(source: String) -> Result<Vec<CliDiagnostic>> {
    let documents = DocumentManager::new()?;
    let config = Config::default();
    let mut analyzer = Analyzer::new(config.clone(), documents.clone());
    let file_path = PathBuf::from("stdin.py");
    let uri = Url::parse("file:///stdin.py")?;

    documents.open_document(uri.clone(), 1, &source)?;

    let mut workspace = Workspace::new(None, config, documents.clone());
    workspace.initialize()?;
    let workspace = std::sync::Arc::new(tokio::sync::RwLock::new(workspace));
    let diagnostic_provider = DiagnosticProvider::new(documents, workspace);

    Ok(diagnostic_provider
        .generate_diagnostics(&uri, &mut analyzer)
        .into_iter()
        .map(|diagnostic| (file_path.clone(), source.clone(), diagnostic))
        .collect())
}

pub fn filter_diagnostics(diagnostics: Vec<CliDiagnostic>, filter: DiagnosticFilter) -> Vec<CliDiagnostic> {
    diagnostics
        .into_iter()
        .filter(|(_, _, diagnostic)| diagnostic_matches_filter(diagnostic, filter))
        .collect()
}

fn diagnostic_matches_filter(diagnostic: &lsp_types::Diagnostic, filter: DiagnosticFilter) -> bool {
    match filter {
        DiagnosticFilter::All => true,
        DiagnosticFilter::Lint => diagnostic.source.as_deref() == Some("beacon-linter"),
        DiagnosticFilter::DataFlow => diagnostic_code(diagnostic).is_some_and(|code| {
            matches!(
                code,
                "use-before-def" | "unreachable-code" | "unused-variable" | "shadowed-variable"
            )
        }),
        DiagnosticFilter::Typecheck => {
            let Some(code) = diagnostic_code(diagnostic) else {
                return diagnostic.source.as_deref() == Some("beacon")
                    && diagnostic.severity == Some(lsp_types::DiagnosticSeverity::ERROR);
            };

            code.starts_with("HM") || code.starts_with("PM") || code == "undefined-variable"
        }
    }
}

fn diagnostic_code(diagnostic: &lsp_types::Diagnostic) -> Option<&str> {
    match diagnostic.code.as_ref()? {
        lsp_types::NumberOrString::String(code) => Some(code.as_str()),
        lsp_types::NumberOrString::Number(_) => None,
    }
}

pub fn format_lsp_diagnostics(
    diagnostics: &[CliDiagnostic], failed_files: &[(PathBuf, String)], format: OutputFormat,
) -> Result<()> {
    let mut all_diagnostics = diagnostics.to_vec();
    all_diagnostics.sort_by(|a, b| {
        a.0.cmp(&b.0)
            .then_with(|| a.2.range.start.line.cmp(&b.2.range.start.line))
            .then_with(|| a.2.range.start.character.cmp(&b.2.range.start.character))
    });

    match format {
        OutputFormat::Human => {
            if all_diagnostics.is_empty() && failed_files.is_empty() {
                println!("{}", "✓ All checks passed!".green());
                return Ok(());
            }

            for (file_path, message) in failed_files {
                println!("{}: {}", file_path.display().yellow(), message.red());
            }

            for (file_path, source, diagnostic) in &all_diagnostics {
                let range = diagnostic.range;
                let line = range.start.line + 1;
                let col = range.start.character + 1;

                let severity = match diagnostic.severity {
                    Some(lsp_types::DiagnosticSeverity::ERROR) => "▸".red().to_string(),
                    Some(lsp_types::DiagnosticSeverity::WARNING) => "▸".yellow().to_string(),
                    Some(lsp_types::DiagnosticSeverity::INFORMATION) => "▸".blue().to_string(),
                    Some(lsp_types::DiagnosticSeverity::HINT) => "▸".cyan().to_string(),
                    _ => "▸".white().to_string(),
                };

                let code = diagnostic
                    .code
                    .as_ref()
                    .map(|c| match c {
                        lsp_types::NumberOrString::String(s) => s.as_str(),
                        lsp_types::NumberOrString::Number(_) => "code",
                    })
                    .unwrap_or("unknown");

                println!(
                    "  {} {}:{}:{} [{}] {}",
                    severity,
                    file_path.display(),
                    line,
                    col,
                    code.bold(),
                    diagnostic.message
                );

                if let Some(line_content) = source.lines().nth(range.start.line as usize) {
                    let line_num_str = format!("{:4}", line);
                    println!("    {} {}", line_num_str.dimmed(), line_content);

                    let indent = 9; // 4 spaces padding + 4 chars line num + 1 space
                    let offset = range.start.character as usize;
                    let len = if range.end.line == range.start.line {
                        (range.end.character.saturating_sub(range.start.character) as usize).max(1)
                    } else {
                        1
                    };

                    let mut indicator = String::with_capacity(indent + offset + len);
                    for _ in 0..indent + offset {
                        indicator.push(' ');
                    }
                    for _ in 0..len {
                        indicator.push('^');
                    }
                    println!("{}", indicator.red());
                    println!(); // Add empty line spacing between items
                }
            }

            if !all_diagnostics.is_empty() {
                println!();
                println!(
                    "{} {} total issue(s) found",
                    "Summary:".bold(),
                    all_diagnostics.len().to_string().yellow()
                );
            }
        }
        OutputFormat::Json => {
            let output = json!({
                "schema_version": 1,
                "diagnostics": all_diagnostics.iter().map(|(file, _source, diagnostic)| {
                    let code_str = match &diagnostic.code {
                        Some(lsp_types::NumberOrString::String(s)) => s.clone(),
                        Some(lsp_types::NumberOrString::Number(n)) => n.to_string(),
                        None => "unknown".to_string(),
                    };

                    json!({
                        "file": file,
                        "code": code_str,
                        "severity": diagnostic_severity_name(diagnostic.severity),
                        "message": diagnostic.message,
                        "span": {
                            "start": diagnostic_position_json(diagnostic.range.start),
                            "end": diagnostic_position_json(diagnostic.range.end),
                        },
                        "tags": diagnostic.tags.as_deref().unwrap_or(&[]).iter().map(diagnostic_tag_name).collect::<Vec<_>>(),
                    })
                }).collect::<Vec<_>>(),
                "failures": failed_files.iter().map(|(file, msg)| {
                    json!({
                        "file": file,
                        "error": msg
                    })
                }).collect::<Vec<_>>()
            });
            println!("{}", serde_json::to_string_pretty(&output)?);
        }
        OutputFormat::Compact => {
            for (file_path, message) in failed_files {
                println!("{}:0:0: error: {}", file_path.display(), message);
            }
            for (file_path, _source, diagnostic) in &all_diagnostics {
                let code_str = match &diagnostic.code {
                    Some(lsp_types::NumberOrString::String(s)) => s.clone(),
                    Some(lsp_types::NumberOrString::Number(n)) => n.to_string(),
                    None => "unknown".to_string(),
                };
                println!(
                    "{}:{}:{}: [{}]: {}",
                    file_path.display(),
                    diagnostic.range.start.line + 1,
                    diagnostic.range.start.character + 1,
                    code_str,
                    diagnostic.message
                );
            }
        }
    }
    Ok(())
}

fn diagnostic_position_json(position: Position) -> serde_json::Value {
    json!({
        "line": position.line + 1,
        "col": position.character + 1,
    })
}

fn diagnostic_severity_name(severity: Option<DiagnosticSeverity>) -> &'static str {
    match severity {
        Some(DiagnosticSeverity::ERROR) => "error",
        Some(DiagnosticSeverity::WARNING) => "warning",
        Some(DiagnosticSeverity::INFORMATION) => "information",
        Some(DiagnosticSeverity::HINT) => "hint",
        _ => "information",
    }
}

fn diagnostic_tag_name(tag: &DiagnosticTag) -> &'static str {
    if *tag == DiagnosticTag::UNNECESSARY {
        "unnecessary"
    } else if *tag == DiagnosticTag::DEPRECATED {
        "deprecated"
    } else {
        "unknown"
    }
}
