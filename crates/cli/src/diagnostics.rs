use crate::helpers;

use anyhow::Result;
use beacon_lsp::{
    Config, analysis::Analyzer, document::DocumentManager, features::DiagnosticProvider, workspace::Workspace,
};
use clap::ValueEnum;
use lsp_types::Url;
use owo_colors::OwoColorize;
use serde_json::json;
use std::path::PathBuf;

#[derive(Clone, Copy, Debug, ValueEnum, Default)]
pub enum OutputFormat {
    #[default]
    Human,
    Json,
    Compact,
}

pub async fn run_workspace_diagnostics(
    paths: Vec<PathBuf>, workspace_root: Option<PathBuf>,
) -> Result<(Vec<(PathBuf, String, lsp_types::Diagnostic)>, Vec<(PathBuf, String)>)> {
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

                let _ = documents.open_document(uri.clone(), 1, source.clone());

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

pub fn format_lsp_diagnostics(
    diagnostics: &[(PathBuf, String, lsp_types::Diagnostic)], failed_files: &[(PathBuf, String)], format: OutputFormat,
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
                "diagnostics": all_diagnostics.iter().map(|(file, _source, diagnostic)| {
                    let code_str = match &diagnostic.code {
                        Some(lsp_types::NumberOrString::String(s)) => s.clone(),
                        Some(lsp_types::NumberOrString::Number(n)) => n.to_string(),
                        None => "unknown".to_string(),
                    };

                    json!({
                        "file": file,
                        "line": diagnostic.range.start.line + 1,
                        "col": diagnostic.range.start.character + 1,
                        "message": diagnostic.message,
                        "code": code_str,
                        "severity": match diagnostic.severity {
                            Some(lsp_types::DiagnosticSeverity::ERROR) => "error",
                            Some(lsp_types::DiagnosticSeverity::WARNING) => "warning",
                            _ => "info",
                        }
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
