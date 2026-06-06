use crate::cfg;

use anyhow::Result;
use beacon_analyzer::DiagnosticMessage;
use beacon_cli::diagnostics::{CliDiagnostic, OutputFormat, format_lsp_diagnostics};
use beacon_parser::SymbolTable;
use lsp_types::{DiagnosticSeverity, DiagnosticTag, NumberOrString, Position};
use owo_colors::OwoColorize;
use serde_json::json;
use std::path::{Path, PathBuf};
use tree_sitter as ts;

pub fn print_parse_errors(node: ts::Node, source: &str, depth: usize) {
    if node.is_error() {
        let start_pos = node.start_position();
        let end_pos = node.end_position();
        let text = node.utf8_text(source.as_bytes()).unwrap_or("<invalid>");

        println!(
            "{}{} at {}, column {}-{}: {}",
            "  ".repeat(depth),
            "Error".red().bold(),
            format!("line {}", start_pos.row + 1).cyan(),
            (start_pos.column + 1).to_string().cyan(),
            (end_pos.column + 1).to_string().cyan(),
            format!("'{}'", text.replace('\n', "\\n")).yellow()
        );
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        print_parse_errors(child, source, depth + 1);
    }
}

pub fn print_symbol_table(table: &SymbolTable, verbose: bool) {
    print_scope(table, table.root_scope, 0, verbose);
}

fn print_scope(table: &SymbolTable, scope_id: beacon_parser::ScopeId, depth: usize, verbose: bool) {
    let scope = table.get_scope(scope_id).unwrap();
    let indent = "  ".repeat(depth);
    let scope_name = match scope.kind {
        beacon_parser::ScopeKind::Module => "Module",
        beacon_parser::ScopeKind::Function => "Function",
        beacon_parser::ScopeKind::Class => "Class",
        beacon_parser::ScopeKind::Block => "Block",
    };

    if depth == 0 {
        println!("▼ {scope_name} Scope:");
    } else {
        println!("{indent}▼ {scope_name} Scope:");
    }

    let mut symbols: Vec<_> = scope.symbols.values().collect();
    symbols.sort_by_key(|s| (&s.kind, &s.name));

    for symbol in symbols {
        let symbol_icon = symbol.kind.icon();
        let kind_name = symbol.kind.name();

        if verbose {
            println!(
                "{}  {} {} '{}' (line {}, col {})",
                indent, symbol_icon, kind_name, symbol.name, symbol.line, symbol.col
            );
        } else {
            println!("{}  {} {} '{}'", indent, symbol_icon, kind_name, symbol.name);
        }
    }

    for &child_id in &scope.children {
        println!();
        print_scope(table, child_id, depth + 1, verbose);
    }
}

pub fn format_diagnostics(
    diagnostics: &[DiagnosticMessage], source: &str, file: Option<&Path>, format: OutputFormat,
) -> Result<()> {
    match format {
        OutputFormat::Human => format_diagnostics_human(diagnostics, source, file),
        OutputFormat::Json => format_diagnostics_json(diagnostics),
        OutputFormat::Compact => format_diagnostics_compact(diagnostics, file),
    }
}

pub fn format_analysis_lsp_output(
    extras: &cfg::AnalysisExtras, diagnostics: &[CliDiagnostic], failed_files: &[(PathBuf, String)],
    format: OutputFormat,
) -> Result<()> {
    if extras.is_empty() {
        return format_lsp_diagnostics(diagnostics, failed_files, format);
    }

    match format {
        OutputFormat::Human => {
            extras.format_human();
            format_lsp_diagnostics(diagnostics, failed_files, format)
        }
        OutputFormat::Json => {
            let output = json!({
                "schema_version": 1,
                "cfg": extras.cfg_json(),
                "inferred_types": extras.inferred_types_json(),
                "diagnostics": lsp_diagnostics_json(diagnostics),
                "failures": failures_json(failed_files),
            });
            println!("{}", serde_json::to_string_pretty(&output)?);
            Ok(())
        }
        OutputFormat::Compact => {
            extras.format_compact();
            format_lsp_diagnostics(diagnostics, failed_files, format)
        }
    }
}

pub fn format_analysis_local_output(
    extras: &cfg::AnalysisExtras, diagnostics: &[DiagnosticMessage], source: &str, file: Option<&Path>,
    format: OutputFormat,
) -> Result<()> {
    if extras.is_empty() {
        return format_diagnostics(diagnostics, source, file, format);
    }

    match format {
        OutputFormat::Human => {
            extras.format_human();
            format_diagnostics(diagnostics, source, file, format)
        }
        OutputFormat::Json => {
            let output = json!({
                "schema_version": 1,
                "cfg": extras.cfg_json(),
                "inferred_types": extras.inferred_types_json(),
                "diagnostics": diagnostics,
            });
            println!("{}", serde_json::to_string_pretty(&output)?);
            Ok(())
        }
        OutputFormat::Compact => {
            extras.format_compact();
            format_diagnostics(diagnostics, source, file, format)
        }
    }
}

fn lsp_diagnostics_json(diagnostics: &[CliDiagnostic]) -> Vec<serde_json::Value> {
    let mut diagnostics = diagnostics.to_vec();
    diagnostics.sort_by(|a, b| {
        a.0.cmp(&b.0)
            .then_with(|| a.2.range.start.line.cmp(&b.2.range.start.line))
            .then_with(|| a.2.range.start.character.cmp(&b.2.range.start.character))
    });

    diagnostics
        .iter()
        .map(|(file, _source, diagnostic)| {
            let code = match &diagnostic.code {
                Some(NumberOrString::String(code)) => code.clone(),
                Some(NumberOrString::Number(code)) => code.to_string(),
                None => "unknown".to_string(),
            };

            json!({
                "file": file,
                "code": code,
                "severity": lsp_severity_name(diagnostic.severity),
                "message": diagnostic.message,
                "span": {
                    "start": lsp_position_json(diagnostic.range.start),
                    "end": lsp_position_json(diagnostic.range.end),
                },
                "tags": diagnostic.tags.as_deref().unwrap_or(&[]).iter().map(lsp_tag_name).collect::<Vec<_>>(),
            })
        })
        .collect()
}

fn failures_json(failed_files: &[(PathBuf, String)]) -> Vec<serde_json::Value> {
    failed_files
        .iter()
        .map(|(file, message)| {
            json!({
                "file": file,
                "error": message,
            })
        })
        .collect()
}

fn lsp_position_json(position: Position) -> serde_json::Value {
    json!({
        "line": position.line + 1,
        "col": position.character + 1,
    })
}

fn lsp_severity_name(severity: Option<DiagnosticSeverity>) -> &'static str {
    match severity {
        Some(DiagnosticSeverity::ERROR) => "error",
        Some(DiagnosticSeverity::WARNING) => "warning",
        Some(DiagnosticSeverity::INFORMATION) => "information",
        Some(DiagnosticSeverity::HINT) => "hint",
        _ => "information",
    }
}

fn lsp_tag_name(tag: &DiagnosticTag) -> &'static str {
    if *tag == DiagnosticTag::UNNECESSARY {
        "unnecessary"
    } else if *tag == DiagnosticTag::DEPRECATED {
        "deprecated"
    } else {
        "unknown"
    }
}

pub fn format_diagnostics_human(diagnostics: &[DiagnosticMessage], source: &str, file: Option<&Path>) -> Result<()> {
    if diagnostics.is_empty() {
        println!("{} No issues found", "✓".green().bold());
        return Ok(());
    }

    let filename = file
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "stdin".to_string());
    println!(
        "{} {} issues found in {}",
        "✗".red().bold(),
        diagnostics.len(),
        filename.cyan()
    );
    println!();

    for diagnostic in diagnostics {
        println!(
            "{} {}:{}:{} [{}]",
            "▸".bright_red(),
            diagnostic.filename.cyan(),
            diagnostic.line.to_string().yellow(),
            diagnostic.col.to_string().yellow(),
            diagnostic.rule.code().dimmed()
        );
        println!("  {}", diagnostic.message.bright_white());

        let lines: Vec<&str> = source.lines().collect();
        if diagnostic.line > 0 && diagnostic.line <= lines.len() {
            let line = lines[diagnostic.line - 1];
            println!("  {} {}", diagnostic.line.to_string().dimmed(), line.dimmed());
            if diagnostic.col > 0 {
                let spaces = " ".repeat(diagnostic.line.to_string().len() + diagnostic.col);
                println!("  {}{}", spaces, "^".bright_red());
            }
        }
        println!();
    }

    Ok(())
}

pub fn format_diagnostics_json(diagnostics: &[DiagnosticMessage]) -> Result<()> {
    let json_output = serde_json::to_string_pretty(&diagnostics)?;
    println!("{json_output}");
    Ok(())
}

pub fn format_diagnostics_compact(diagnostics: &[DiagnosticMessage], file: Option<&Path>) -> Result<()> {
    let filename = file
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "stdin".to_string());

    for diagnostic in diagnostics {
        println!(
            "{}:{}:{}: [{}] {}",
            filename,
            diagnostic.line,
            diagnostic.col,
            diagnostic.rule.code(),
            diagnostic.message
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_diagnostics_json() {
        let diagnostics = vec![];
        let result = format_diagnostics_json(&diagnostics);
        assert!(result.is_ok());
    }

    #[test]
    fn test_format_diagnostics_compact() {
        let diagnostics = vec![];
        let result = format_diagnostics_compact(&diagnostics, None);
        assert!(result.is_ok());
    }

    #[test]
    fn test_format_diagnostics_human_no_errors() {
        let diagnostics = vec![];
        let source = "x = 42";
        let result = format_diagnostics_human(&diagnostics, source, None);
        assert!(result.is_ok());
    }
}
