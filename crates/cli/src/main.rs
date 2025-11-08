mod formatters;

use anyhow::{Context, Result};
use beacon_constraint::ConstraintResult;
use beacon_core::logging::default_log_path;
use beacon_lsp::{Config, analysis::Analyzer, document::DocumentManager};
use beacon_parser::{PythonHighlighter, PythonParser};
use clap::{Parser, Subcommand, ValueEnum};
use formatters::{format_compact, format_human, format_json, print_parse_errors, print_symbol_table};
use owo_colors::OwoColorize;
use serde_json::json;
use std::fs;
use std::io::{self, BufRead, Read, Seek, SeekFrom};
use std::path::PathBuf;
use std::time::Duration;
use tree_sitter::Node;
use url::Url;

/// Output format for diagnostics
#[derive(Debug, Clone, Copy, ValueEnum)]
enum OutputFormat {
    /// Human-readable output with colors and context
    Human,
    /// JSON format for machine processing
    Json,
    /// Compact single-line format (file:line:col)
    Compact,
}

#[derive(Parser)]
#[command(name = "beacon-cli")]
#[command(about = "Beacon - Language Server & Hindley-Milner type system for Python")]
#[command(version = "0.1.0")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Parse Python file and show AST
    Parse {
        /// Python file to parse
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
        /// Pretty print the AST
        #[arg(short, long)]
        pretty: bool,
        /// Show tree-sitter CST structure
        #[arg(short, long)]
        tree: bool,
    },
    /// Highlight Python source code with syntax colors
    Highlight {
        /// Python file to highlight
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
        /// Disable colors (plain text output)
        #[arg(long)]
        no_color: bool,
    },
    /// Check Python file for parse errors
    Check {
        /// Python file to check
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
    },
    /// Analyze name resolution and show symbol table
    Resolve {
        /// Python file to analyze
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
        /// Show detailed scope information
        #[arg(short, long)]
        verbose: bool,
    },
    /// Type check Python file with Hindley-Milner inference
    Typecheck {
        /// Python file to type check
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
        /// Output format
        #[arg(short, long, value_enum, default_value = "human")]
        format: OutputFormat,
        /// Analyze all files in workspace (follow imports)
        #[arg(short, long)]
        workspace: bool,
    },
    /// Start Beacon Language Server
    Lsp {
        /// Use TCP instead of stdio
        #[arg(long)]
        tcp: Option<u16>,
        /// Log file path for debugging
        #[arg(long)]
        log_file: Option<PathBuf>,
    },
    /// Debug tools (available only in debug builds)
    #[cfg(debug_assertions)]
    Debug {
        #[command(subcommand)]
        command: DebugCommands,
    },
}

#[cfg(debug_assertions)]
#[derive(Subcommand)]
enum DebugCommands {
    /// Show tree-sitter CST structure
    Tree {
        /// Python file to analyze
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
        /// Output in JSON format
        #[arg(long)]
        json: bool,
    },
    /// Show AST with inferred types
    Ast {
        /// Python file to analyze
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
        /// Output format (json or tree)
        #[arg(long, default_value = "tree")]
        format: String,
    },
    /// Show generated constraints
    Constraints {
        /// Python file to analyze
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
    },
    /// Show unification trace
    Unify {
        /// Python file to analyze
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
    },
    /// Watch and display LSP server logs
    Logs {
        /// Follow mode - continuously watch for new log entries
        #[arg(short, long)]
        follow: bool,
        /// Optional log file path (defaults to logs/lsp.log or $LSP_LOG_PATH)
        #[arg(long)]
        path: Option<PathBuf>,
        /// Filter logs by regex pattern
        #[arg(long)]
        filter: Option<String>,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    match Cli::parse().command {
        Commands::Parse { file, pretty, tree } => parse_command(&read_input(file)?, pretty, tree),
        Commands::Highlight { file, no_color } => highlight_command(&read_input(file)?, !no_color),
        Commands::Check { file } => check_command(&read_input(file)?),
        Commands::Resolve { file, verbose } => resolve_command(&read_input(file)?, verbose),
        Commands::Typecheck { file, format, workspace } => typecheck_command(file, format, workspace).await,
        Commands::Lsp { tcp, log_file } => lsp_command(tcp, log_file).await,

        #[cfg(debug_assertions)]
        Commands::Debug { command } => debug_command(command),
    }
}

fn read_input(file: Option<PathBuf>) -> Result<String> {
    match file {
        Some(path) => fs::read_to_string(&path).with_context(|| format!("Failed to read file: {}", path.display())),
        None => {
            let mut buffer = String::new();
            io::stdin()
                .read_to_string(&mut buffer)
                .with_context(|| "Failed to read from stdin")?;
            Ok(buffer)
        }
    }
}

fn parse_command(source: &str, pretty: bool, show_tree: bool) -> Result<()> {
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;

    let parsed = parser.parse(source).with_context(|| "Failed to parse Python source")?;

    if show_tree {
        println!("{}", "Tree-sitter CST structure:".cyan().bold());
        println!("{}", parser.debug_tree(&parsed));
        println!();
    }

    let ast = parser.to_ast(&parsed).with_context(|| "Failed to convert to AST")?;

    if pretty {
        println!("{}", "Python AST:".cyan().bold());
        println!("{ast:#?}");
    } else {
        println!("{ast:?}");
    }

    Ok(())
}

fn highlight_command(source: &str, enable_colors: bool) -> Result<()> {
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;
    let parsed = parser.parse(source).with_context(|| "Failed to parse Python source")?;
    let highlighter = PythonHighlighter::new(enable_colors);
    let highlighted = highlighter.highlight(source, &parsed.tree);

    println!("{highlighted}");

    Ok(())
}

fn check_command(source: &str) -> Result<()> {
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;

    match parser.parse(source) {
        Ok(parsed) => {
            let root = parsed.tree.root_node();
            if root.has_error() {
                if cfg!(test) {
                    anyhow::bail!("Parse errors found in source code");
                } else {
                    println!("{} Parse errors found:", "✗".red().bold());
                    print_parse_errors(root, source, 0);
                    std::process::exit(1);
                }
            } else {
                println!("{} No parse errors found", "✓".green().bold());
            }
        }
        Err(e) => {
            if cfg!(test) {
                return Err(e).with_context(|| "Failed to parse source code");
            } else {
                println!("{} Failed to parse: {}", "✗".red().bold(), e.to_string().bright_red());
                std::process::exit(1);
            }
        }
    }

    Ok(())
}

fn resolve_command(source: &str, verbose: bool) -> Result<()> {
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;

    let (_ast, symbol_table) = parser
        .parse_and_resolve(source)
        .with_context(|| "Failed to parse and resolve Python source")?;

    println!("{} {}", "⚡".yellow().bold(), "Name Resolution Analysis".cyan().bold());
    println!("{}", "===========================".dimmed());

    print_symbol_table(&symbol_table, verbose);

    if verbose {
        println!("\n{} {}:", "▶".bright_green(), "Statistics".green().bold());
        println!(
            "  • {}: {}",
            "Total scopes".cyan(),
            symbol_table.scopes.len().to_string().yellow()
        );
        let total_symbols: usize = symbol_table.scopes.values().map(|scope| scope.symbols.len()).sum();
        println!("  • {}: {}", "Total symbols".cyan(), total_symbols.to_string().yellow());
    }

    Ok(())
}

async fn typecheck_command(file: Option<PathBuf>, format: OutputFormat, _workspace: bool) -> Result<()> {
    let source = read_input(file.clone())?;
    let documents = DocumentManager::new()?;
    let file_path = file
        .as_ref()
        .map(|p| p.canonicalize().unwrap_or_else(|_| p.clone()))
        .unwrap_or_else(|| PathBuf::from("stdin.py"));

    let uri = Url::from_file_path(&file_path)
        .unwrap_or_else(|_| Url::parse(&format!("file://{}", file_path.display())).expect("Failed to create URL"));

    documents.open_document(uri.clone(), 1, source.clone())?;

    let config = Config::default();
    let mut analyzer = Analyzer::new(config, documents);

    match analyzer.analyze(&uri) {
        Ok(result) => {
            let errors = result.type_errors;

            match format {
                OutputFormat::Human => format_human(&source, &errors, &file_path),
                OutputFormat::Json => format_json(&errors)?,
                OutputFormat::Compact => format_compact(&errors, &file_path),
            }

            if !errors.is_empty() && !cfg!(test) {
                std::process::exit(1);
            }

            Ok(())
        }
        Err(e) => {
            if cfg!(test) {
                anyhow::bail!("Type checking failed: {e}")
            } else {
                eprintln!("Error: Type checking failed: {e}");
                std::process::exit(1);
            }
        }
    }
}

/// TODO: Implement TCP server
async fn lsp_command(tcp: Option<u16>, log_file: Option<PathBuf>) -> Result<()> {
    if let Some(path) = log_file {
        let parent = path.parent().unwrap_or_else(|| std::path::Path::new("."));
        let filename = path
            .file_name()
            .unwrap_or_else(|| std::ffi::OsStr::new("beacon-lsp.log"));

        let file_appender = tracing_appender::rolling::never(parent, filename);
        let (non_blocking, guard) = tracing_appender::non_blocking(file_appender);

        tracing_subscriber::fmt()
            .with_env_filter(
                tracing_subscriber::EnvFilter::from_default_env().add_directive(tracing::Level::INFO.into()),
            )
            .with_writer(non_blocking)
            .init();

        std::mem::forget(guard);
    } else {
        tracing_subscriber::fmt()
            .with_env_filter(
                tracing_subscriber::EnvFilter::from_default_env().add_directive(tracing::Level::INFO.into()),
            )
            .with_writer(std::io::stderr)
            .init();
    }

    if let Some(port) = tcp {
        tracing::info!("Starting Beacon LSP server on TCP port {}", port);
        eprintln!("TCP mode not yet implemented. Use stdio mode (default).");
        std::process::exit(1);
    } else {
        tracing::info!("Starting Beacon LSP server on stdio");
        beacon_lsp::run_server().await;
    }

    Ok(())
}

#[cfg(debug_assertions)]
fn debug_command(command: DebugCommands) -> Result<()> {
    match command {
        DebugCommands::Tree { file, json } => debug_tree_command(file, json),
        DebugCommands::Ast { file, format } => debug_ast_command(file, format),
        DebugCommands::Constraints { file } => debug_constraints_command(file),
        DebugCommands::Unify { file } => debug_unify_command(file),
        DebugCommands::Logs { follow, path, filter } => debug_logs_command(follow, path, filter),
    }
}

#[cfg(debug_assertions)]
fn debug_tree_command(file: Option<PathBuf>, json: bool) -> Result<()> {
    let source = read_input(file)?;
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;
    let parsed = parser.parse(&source).with_context(|| "Failed to parse Python source")?;

    if json {
        fn node_to_json(node: Node, source: &str) -> serde_json::Value {
            let mut children = vec![];
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                children.push(node_to_json(child, source));
            }

            json!({
                "type": node.kind(),
                "start_byte": node.start_byte(),
                "end_byte": node.end_byte(),
                "start_line": node.start_position().row + 1,
                "start_col": node.start_position().column + 1,
                "end_line": node.end_position().row + 1,
                "end_col": node.end_position().column + 1,
                "text": node.utf8_text(source.as_bytes()).unwrap_or("<invalid>"),
                "children": children,
            })
        }

        let json_tree = node_to_json(parsed.tree.root_node(), &source);
        println!("{}", serde_json::to_string_pretty(&json_tree)?);
    } else {
        println!("{}", "Tree-sitter CST:".cyan().bold());
        println!("{}", parser.debug_tree(&parsed));
    }

    Ok(())
}

#[cfg(debug_assertions)]
fn debug_ast_command(file: Option<PathBuf>, format: String) -> Result<()> {
    let source = read_input(file.clone())?;
    let documents = DocumentManager::new()?;
    let file_path = file
        .as_ref()
        .map(|p| p.canonicalize().unwrap_or_else(|_| p.clone()))
        .unwrap_or_else(|| PathBuf::from("stdin.py"));

    let uri = Url::from_file_path(&file_path)
        .unwrap_or_else(|_| Url::parse(&format!("file://{}", file_path.display())).expect("Failed to create URL"));

    documents.open_document(uri.clone(), 1, source)?;

    let config = Config::default();
    let mut analyzer = Analyzer::new(config, documents);

    match analyzer.analyze(&uri) {
        Ok(result) => {
            if format == "json" {
                let ast_json = serde_json::json!({
                    "type_map": result.type_map.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect::<std::collections::HashMap<_, _>>(),
                    "position_map": result.position_map.iter().map(|(k, v)| (format!("{}:{}", k.0, k.1), v)).collect::<std::collections::HashMap<_, _>>(),
                    "error_count": result.type_errors.len(),
                });
                println!("{}", serde_json::to_string_pretty(&ast_json)?);
            } else {
                println!("{}\n", "AST with inferred types:".cyan().bold());
                println!(
                    "{} {} nodes",
                    "Type mappings:".green(),
                    result.type_map.len().to_string().yellow()
                );
                println!(
                    "{} {} positions",
                    "Position mappings:".green(),
                    result.position_map.len().to_string().yellow()
                );
                println!(
                    "\n{} {}",
                    "Type errors:".red().bold(),
                    result.type_errors.len().to_string().yellow()
                );

                if !result.type_map.is_empty() {
                    println!("\n{}:", "Node types".cyan().bold());
                    let mut entries: Vec<_> = result.type_map.iter().collect();
                    entries.sort_by_key(|(k, _)| *k);
                    for (node_id, ty) in entries.iter().take(50) {
                        println!(
                            "  {} {}: {}",
                            "▸".blue(),
                            node_id.to_string().cyan(),
                            ty.to_string().bright_white()
                        );
                    }
                    if entries.len() > 50 {
                        println!(
                            "  {} and {} more",
                            "...".dimmed(),
                            (entries.len() - 50).to_string().yellow()
                        );
                    }
                }
            }
            Ok(())
        }
        Err(e) => {
            anyhow::bail!("Analysis failed: {e}")
        }
    }
}

#[cfg(debug_assertions)]
fn debug_constraints_command(file: Option<PathBuf>) -> Result<()> {
    let source = read_input(file)?;
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;
    let parsed = parser.parse(&source).with_context(|| "Failed to parse Python source")?;
    let ast = parser.to_ast(&parsed).with_context(|| "Failed to convert to AST")?;
    let (_, symbol_table) = parser
        .parse_and_resolve(&source)
        .with_context(|| "Failed to parse and resolve Python source")?;

    let ConstraintResult(constraint_set, _type_map, _position_map, _class_registry) =
        beacon_lsp::analysis::walker::generate_constraints(&None, &ast, &symbol_table)?;

    println!(
        "{} {} constraints:\n",
        "Generated".green().bold(),
        constraint_set.constraints.len().to_string().yellow()
    );

    let mut constraint_groups: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();

    for constraint in constraint_set.constraints {
        let constraint_type = format!("{constraint:?}")
            .split('(')
            .next()
            .unwrap_or("Unknown")
            .to_string();
        let constraint_str = format!("{constraint:?}");
        constraint_groups
            .entry(constraint_type)
            .or_default()
            .push(constraint_str);
    }

    for (constraint_type, instances) in constraint_groups.iter() {
        println!(
            "{} {} ({} instances)",
            "▸".blue().bold(),
            constraint_type.cyan().bold(),
            instances.len().to_string().yellow()
        );
        for (idx, instance) in instances.iter().take(3).enumerate() {
            println!("  {}. {}", (idx + 1).to_string().dimmed(), instance.bright_white());
        }
        if instances.len() > 3 {
            println!(
                "  {} and {} more",
                "...".dimmed(),
                (instances.len() - 3).to_string().yellow()
            );
        }
        println!();
    }

    Ok(())
}

#[cfg(debug_assertions)]
fn debug_unify_command(file: Option<PathBuf>) -> Result<()> {
    println!("{}", "Unification trace:".cyan().bold());
    println!("{}", "TODO: Implement unification step-by-step trace".yellow());
    println!(
        "{}",
        "This requires adding tracing to the solver implementation".dimmed()
    );

    let source = read_input(file)?;
    let documents = DocumentManager::new()?;
    let uri = Url::parse("file:///stdin.py").unwrap();

    documents.open_document(uri.clone(), 1, source)?;

    let config = Config::default();
    let mut analyzer = Analyzer::new(config, documents);

    match analyzer.analyze(&uri) {
        Ok(result) => {
            println!("\n{}:", "Unification completed".green().bold());
            println!(
                "  {}: {}",
                "Type map size".cyan(),
                result.type_map.len().to_string().yellow()
            );
            if result.type_errors.is_empty() {
                println!(
                    "  {}: {}",
                    "Type errors".green(),
                    result.type_errors.len().to_string().yellow()
                );
            } else {
                println!(
                    "  {}: {}",
                    "Type errors".red(),
                    result.type_errors.len().to_string().yellow()
                );
            }

            if !result.type_errors.is_empty() {
                println!("\n{}:", "Errors during unification".red().bold());
                for (idx, error) in result.type_errors.iter().enumerate() {
                    println!(
                        "  {}. {} {} {}",
                        (idx + 1).to_string().dimmed(),
                        error.error.to_string().bright_red(),
                        "at".dimmed(),
                        format!("line {}, col {}", error.span.line, error.span.col).cyan()
                    );
                }
            }

            Ok(())
        }
        Err(e) => anyhow::bail!("Unification failed: {e}"),
    }
}

#[cfg(debug_assertions)]
fn debug_logs_command(follow: bool, path: Option<PathBuf>, filter: Option<String>) -> Result<()> {
    let log_path = path.unwrap_or_else(default_log_path);

    if !log_path.exists() {
        anyhow::bail!(
            "Log file does not exist: {}\n{}",
            log_path.display(),
            "Start the LSP server to generate logs.".dimmed()
        );
    }

    let filter_regex = if let Some(pattern) = filter {
        Some(regex::Regex::new(&pattern).with_context(|| format!("Invalid regex pattern: {pattern}"))?)
    } else {
        None
    };

    println!(
        "{} {}",
        "Watching log file:".cyan().bold(),
        log_path.display().to_string().yellow()
    );

    if let Some(ref regex) = filter_regex {
        println!("{} {}", "Filter:".cyan(), regex.as_str().yellow());
    }

    println!();

    if follow {
        let mut file =
            fs::File::open(&log_path).with_context(|| format!("Failed to open log file: {}", log_path.display()))?;

        file.seek(SeekFrom::End(0))?;

        let mut reader = io::BufReader::new(file);
        let mut line = String::new();

        loop {
            match reader.read_line(&mut line) {
                Ok(0) => std::thread::sleep(Duration::from_millis(100)),
                Ok(_) => {
                    if let Some(ref regex) = filter_regex {
                        if regex.is_match(&line) {
                            print_log_line(&line);
                        }
                    } else {
                        print_log_line(&line);
                    }
                    line.clear();
                }
                Err(e) => {
                    anyhow::bail!("Error reading log file: {e}")
                }
            }
        }
    } else {
        let content = fs::read_to_string(&log_path)
            .with_context(|| format!("Failed to read log file: {}", log_path.display()))?;

        for line in content.lines() {
            if let Some(ref regex) = filter_regex {
                if regex.is_match(line) {
                    print_log_line(line);
                }
            } else {
                print_log_line(line);
            }
        }

        Ok(())
    }
}

#[cfg(debug_assertions)]
fn print_log_line(line: &str) {
    if line.contains("ERROR") {
        println!("{}", line.bright_red());
    } else if line.contains("WARN") {
        println!("{}", line.yellow());
    } else if line.contains("INFO") {
        println!("{}", line.bright_white());
    } else if line.contains("DEBUG") {
        println!("{}", line.cyan());
    } else if line.contains("TRACE") {
        println!("{}", line.dimmed());
    } else {
        println!("{line}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::SymbolTable;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn format_symbol_table_for_test(table: &SymbolTable, verbose: bool) -> String {
        let mut output = String::new();
        format_scope_for_test(table, table.root_scope, 0, verbose, &mut output);

        if verbose {
            output.push_str("\nStatistics:\n");
            output.push_str(&format!("  • Total scopes: {}\n", table.scopes.len()));
            let total_symbols: usize = table.scopes.values().map(|scope| scope.symbols.len()).sum();
            output.push_str(&format!("  • Total symbols: {total_symbols}\n"));
        }

        output
    }

    fn format_scope_for_test(
        table: &SymbolTable, scope_id: beacon_parser::ScopeId, depth: usize, verbose: bool, output: &mut String,
    ) {
        let scope = table.get_scope(scope_id).unwrap();
        let indent = "  ".repeat(depth);

        let scope_name = match scope.kind {
            beacon_parser::ScopeKind::Module => "Module",
            beacon_parser::ScopeKind::Function => "Function",
            beacon_parser::ScopeKind::Class => "Class",
            beacon_parser::ScopeKind::Block => "Block",
        };

        if depth == 0 {
            output.push_str(&format!("▼ {scope_name} Scope:\n"));
        } else {
            output.push_str(&format!("{indent}▼ {scope_name} Scope:\n"));
        }

        let mut symbols: Vec<_> = scope.symbols.values().collect();
        symbols.sort_by_key(|s| (&s.kind, &s.name));

        for symbol in symbols {
            let symbol_icon = symbol.kind.icon();
            let kind_name = symbol.kind.name();

            if verbose {
                output.push_str(&format!(
                    "{}  {} {} '{}' (line {}, col {})\n",
                    indent, symbol_icon, kind_name, symbol.name, symbol.line, symbol.col
                ));
            } else {
                output.push_str(&format!(
                    "{}  {} {} '{}'\n",
                    indent, symbol_icon, kind_name, symbol.name
                ));
            }
        }

        for &child_id in &scope.children {
            output.push('\n');
            format_scope_for_test(table, child_id, depth + 1, verbose, output);
        }
    }

    #[test]
    fn test_read_input_from_file() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(
            temp_file,
            "def factorial(n):\n    return n * factorial(n-1) if n > 1 else 1"
        )
        .unwrap();

        let content = read_input(Some(temp_file.path().to_path_buf())).unwrap();
        assert!(content.contains("factorial"));
        assert!(content.contains("return"));
        assert!(content.trim().lines().count() >= 1);
    }

    #[test]
    fn test_read_input_from_stdin() {
        let non_existent = PathBuf::from("/non/existent/file.py");
        let result = read_input(Some(non_existent));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Failed to read file"));
    }

    #[test]
    fn test_read_input_empty_file() {
        let temp_file = NamedTempFile::new().unwrap();
        let content = read_input(Some(temp_file.path().to_path_buf())).unwrap();
        assert_eq!(content, "");
    }

    #[test]
    fn test_parse_command_simple() {
        let source = "x = 42\ny = 'hello'";
        let result = parse_command(source, false, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_command_complex_code() {
        let source = r#"
class Calculator:
    def __init__(self):
        self.value = 0

    def add(self, x):
        self.value += x
        return self

    def get(self):
        return self.value

calc = Calculator().add(5).add(10)
print(calc.get())
"#;
        let result = parse_command(source, false, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_command_with_syntax_errors() {
        let source = "def incomplete_function(\nprint('missing closing paren')";
        let result = parse_command(source, false, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_command_with_tree_flag() {
        let source = "def greet(name): return f'Hello {name}'";
        let result = parse_command(source, false, true);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_command_with_pretty_flag() {
        let source = "class Person:\n    def __init__(self, name):\n        self.name = name";
        let result = parse_command(source, true, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_command_with_both_flags() {
        let source = "[x**2 for x in range(10) if x % 2 == 0]";
        let result = parse_command(source, true, true);
        assert!(result.is_ok());
    }

    #[test]
    fn test_highlight_command_with_colors() {
        let source = r#"
import os
from typing import List

def process_files(paths: List[str]) -> None:
    for path in paths:
        if os.path.exists(path):
            print(f"Processing {path}")
"#;
        let result = highlight_command(source, true);
        assert!(result.is_ok());
    }

    #[test]
    fn test_highlight_command_no_colors() {
        let source = "def factorial(n): return n * factorial(n-1) if n > 1 else 1";
        let result = highlight_command(source, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_highlight_command_empty_source() {
        let source = "";
        let result = highlight_command(source, true);
        assert!(result.is_ok());
    }

    #[test]
    fn test_highlight_command_complex_syntax() {
        let source = r#"
async def fetch_data(url: str) -> dict:
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            return await response.json()

@dataclass
class User:
    name: str
    age: int

    def __post_init__(self):
        if self.age < 0:
            raise ValueError("Age cannot be negative")
"#;
        let result = highlight_command(source, true);
        assert!(result.is_ok());
    }

    #[test]
    fn test_check_command_valid_syntax() {
        let source = r#"
def bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        for j in range(0, n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
    return arr

numbers = [64, 34, 25, 12, 22, 11, 90]
sorted_numbers = bubble_sort(numbers.copy())
"#;
        let result = check_command(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_check_command_with_syntax_errors() {
        let source = "x = 42\ndef func(\nprint('missing closing paren')";
        let result = check_command(source);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Parse errors found"));
    }

    #[test]
    fn test_resolve_command_basic() {
        let source = r#"
x = 10
y = 20

def add(a, b):
    result = a + b
    return result

sum_result = add(x, y)
"#;
        let result = resolve_command(source, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_command_class_methods() {
        let source = r#"
class BankAccount:
    def __init__(self, initial_balance=0):
        self.balance = initial_balance
        self.transaction_history = []

    def deposit(self, amount):
        self.balance += amount
        self.transaction_history.append(f"Deposited {amount}")

    def withdraw(self, amount):
        if amount <= self.balance:
            self.balance -= amount
            self.transaction_history.append(f"Withdrew {amount}")
            return True
        return False

account = BankAccount(100)
account.deposit(50)
"#;
        let result = resolve_command(source, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_command_nested_functions() {
        let source = r#"
def make_counter(start=0):
    count = start

    def increment(step=1):
        nonlocal count
        count += step
        return count

    def decrement(step=1):
        nonlocal count
        count -= step
        return count

    def reset():
        nonlocal count
        count = start

    return increment, decrement, reset

inc, dec, reset = make_counter(10)
"#;
        let result = resolve_command(source, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_command_verbose() {
        let source = r#"
from typing import Optional

def fibonacci(n: int) -> Optional[int]:
    if n < 0:
        return None
    elif n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fibonacci(n-1) + fibonacci(n-2)

result = fibonacci(10)
"#;
        let result = resolve_command(source, true);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_command_empty_source() {
        let source = "";
        let result = resolve_command(source, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resolve_command_only_comments() {
        let source = r#"
# This is just a comment
# Another comment
# TODO: implement something
"#;
        let result = resolve_command(source, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_print_parse_errors_with_errors() {
        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let source = "def incomplete_func(\nclass MissingColon\nif True\n    print('bad indent')";
        let parsed = parser.parse(source).unwrap();
        let root = parsed.tree.root_node();

        assert!(root.child_count() > 0);

        print_parse_errors(root, source, 0);
    }

    #[test]
    fn test_print_parse_errors_valid_syntax() {
        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let source = "def valid_function(x, y):\n    return x + y\n\nresult = valid_function(1, 2)";
        let parsed = parser.parse(source).unwrap();
        let root = parsed.tree.root_node();

        assert!(root.child_count() > 0);
        assert!(!root.has_error());

        print_parse_errors(root, source, 0);
    }

    #[test]
    fn test_print_symbol_table() {
        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let source = r#"
global_var = "hello"

class TestClass:
    class_var = 42

    def method(self, param):
        local_var = param + 1
        return local_var

def standalone_func():
    return True

obj = TestClass()
result = obj.method(10)
"#;

        let (_ast, symbol_table) = parser.parse_and_resolve(source).unwrap();
        let output = format_symbol_table_for_test(&symbol_table, false);
        assert!(output.contains("variable 'global_var'"));
        assert!(output.contains("class 'TestClass'"));
        assert!(output.contains("function 'standalone_func'"));
        assert!(output.contains("Module Scope"));
        assert!(output.contains("Class Scope"));
        assert!(output.contains("Function Scope"));

        let verbose_output = format_symbol_table_for_test(&symbol_table, true);
        assert!(verbose_output.contains("line "));
        assert!(verbose_output.contains("col "));
        assert!(verbose_output.contains("Statistics"));
        assert!(verbose_output.contains("Total scopes:"));
        assert!(verbose_output.contains("Total symbols:"));

        print_symbol_table(&symbol_table, false);
        print_symbol_table(&symbol_table, true);
    }

    #[test]
    fn test_print_symbol_table_empty() {
        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let source = "";
        let (_, symbol_table) = parser.parse_and_resolve(source).unwrap();

        let output = format_symbol_table_for_test(&symbol_table, false);
        assert!(output.contains("Module Scope"));

        let verbose_output = format_symbol_table_for_test(&symbol_table, true);
        assert!(verbose_output.contains("Total scopes: 1"));
        assert!(verbose_output.contains("Total symbols: 4"));
    }

    #[tokio::test]
    async fn test_typecheck_command_no_errors() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(
            temp_file,
            "def add(x: int, y: int) -> int:\n    return x + y\n\nresult = add(1, 2)"
        )
        .unwrap();

        let _ = typecheck_command(Some(temp_file.path().to_path_buf()), OutputFormat::Human, false).await;
    }

    #[tokio::test]
    async fn test_typecheck_command_json_format() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "x = 42\ny = 'hello'\nz = x + y").unwrap();

        let _ = typecheck_command(Some(temp_file.path().to_path_buf()), OutputFormat::Json, false).await;
    }

    #[tokio::test]
    async fn test_typecheck_command_compact_format() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "def greet(name: str) -> str:\n    return f'Hello {{name}}'").unwrap();

        let _ = typecheck_command(Some(temp_file.path().to_path_buf()), OutputFormat::Compact, false).await;
    }

    #[cfg(debug_assertions)]
    #[test]
    fn test_debug_tree_command() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "x = 42\ny = 'hello'").unwrap();

        let result = debug_tree_command(Some(temp_file.path().to_path_buf()), false);
        assert!(result.is_ok());
    }

    #[cfg(debug_assertions)]
    #[test]
    fn test_debug_tree_command_json() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(
            temp_file,
            "def factorial(n):\n    return n if n <= 1 else n * factorial(n-1)"
        )
        .unwrap();

        let result = debug_tree_command(Some(temp_file.path().to_path_buf()), true);
        assert!(result.is_ok());
    }

    #[cfg(debug_assertions)]
    #[test]
    fn test_debug_ast_command() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "x = [1, 2, 3]\ny = sum(x)").unwrap();

        let result = debug_ast_command(Some(temp_file.path().to_path_buf()), "tree".to_string());
        assert!(result.is_ok());
    }

    #[cfg(debug_assertions)]
    #[test]
    fn test_debug_constraints_command() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "def add(a, b):\n    return a + b\n\nresult = add(1, 2)").unwrap();

        let result = debug_constraints_command(Some(temp_file.path().to_path_buf()));
        assert!(result.is_ok());
    }

    #[cfg(debug_assertions)]
    #[test]
    fn test_debug_unify_command() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "x = 42\ny = x + 10").unwrap();

        let result = debug_unify_command(Some(temp_file.path().to_path_buf()));
        assert!(result.is_ok());
    }

    #[cfg(debug_assertions)]
    #[test]
    fn test_debug_logs_command_missing_file() {
        let non_existent_path = PathBuf::from("/tmp/non_existent_beacon_log.txt");
        let result = debug_logs_command(false, Some(non_existent_path), None);
        assert!(result.is_err());
    }

    #[cfg(debug_assertions)]
    #[test]
    fn test_debug_logs_command_read_once() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "2025-11-08T12:15:42Z [INFO] Server initialized").unwrap();
        writeln!(temp_file, "2025-11-08T12:15:43Z [DEBUG] Analyzing file").unwrap();
        writeln!(temp_file, "2025-11-08T12:15:44Z [ERROR] Type error found").unwrap();
        temp_file.flush().unwrap();

        let result = debug_logs_command(false, Some(temp_file.path().to_path_buf()), None);
        assert!(result.is_ok());
    }

    #[cfg(debug_assertions)]
    #[test]
    fn test_debug_logs_command_with_filter() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "2025-11-08T12:15:42Z [INFO] Server initialized").unwrap();
        writeln!(temp_file, "2025-11-08T12:15:43Z [DEBUG] Analyzing file").unwrap();
        writeln!(temp_file, "2025-11-08T12:15:44Z [ERROR] Type error found").unwrap();
        temp_file.flush().unwrap();

        let result = debug_logs_command(false, Some(temp_file.path().to_path_buf()), Some("ERROR".to_string()));
        assert!(result.is_ok());
    }

    #[test]
    /// TODO: check output
    fn test_format_human_no_errors() {
        let source = "x = 42";
        let errors = vec![];
        let path = PathBuf::from("test.py");

        format_human(source, &errors, &path);
    }

    #[test]
    /// TODO: check output
    fn test_format_json_no_errors() {
        let errors = vec![];
        let result = format_json(&errors);
        assert!(result.is_ok());
    }

    #[test]
    /// TODO: check output
    fn test_format_compact_no_errors() {
        let errors = vec![];
        let path = PathBuf::from("test.py");
        format_compact(&errors, &path);
    }
}
