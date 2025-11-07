use anyhow::Result;
use beacon_constraint::TypeErrorInfo;
use beacon_parser::SymbolTable;
use owo_colors::OwoColorize;
use serde_json::json;
use std::path::Path;

pub fn print_parse_errors(node: tree_sitter::Node, source: &str, depth: usize) {
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

pub fn format_human(source: &str, errors: &[TypeErrorInfo], file_path: &Path) {
    if errors.is_empty() {
        println!("{} No type errors found", "✓".green().bold());
        return;
    }

    let lines: Vec<&str> = source.lines().collect();
    println!(
        "{} Found {} type error(s):\n",
        "✗".red().bold(),
        errors.len().to_string().yellow()
    );

    for (idx, error_info) in errors.iter().enumerate() {
        let span = &error_info.span;
        println!(
            "{} {}: {} {}",
            format!("Error {}", idx + 1).red().bold(),
            error_info.error.to_string().bright_red(),
            "at".dimmed(),
            format!("line {}, col {}", span.line, span.col).cyan()
        );
        println!(
            "  {} {}:{}:{}",
            "-->".blue().bold(),
            file_path.display(),
            span.line.to_string().cyan(),
            span.col.to_string().cyan()
        );

        if span.line > 0 && span.line <= lines.len() {
            let line_content = lines[span.line - 1];
            println!("   {}", "|".blue());
            println!(
                "{} {} {}",
                span.line.to_string().cyan().bold(),
                "|".blue(),
                line_content
            );
            println!(
                "   {} {}{}",
                "|".blue(),
                " ".repeat(span.col.saturating_sub(1)),
                "^".red().bold()
            );
        }
        println!();
    }
}

pub fn format_json(errors: &[TypeErrorInfo]) -> Result<()> {
    let json_errors: Vec<_> = errors
        .iter()
        .map(|e| {
            json!({
                "error": e.error.to_string(),
                "line": e.span.line,
                "col": e.span.col,
                "end_line": e.span.end_line,
                "end_col": e.span.end_col,
            })
        })
        .collect();

    let output = json!({
        "errors": json_errors,
        "error_count": errors.len(),
    });

    println!("{}", serde_json::to_string_pretty(&output)?);
    Ok(())
}

pub fn format_compact(errors: &[TypeErrorInfo], file_path: &Path) {
    for error_info in errors {
        let span = &error_info.span;
        println!(
            "{}:{}:{}: {}",
            file_path.display(),
            span.line,
            span.col,
            error_info.error
        );
    }
}
