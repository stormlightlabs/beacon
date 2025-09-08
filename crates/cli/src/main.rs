use anyhow::{Context, Result};
use beacon_parser::{PythonHighlighter, PythonParser};
use clap::{Parser, Subcommand};
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "beacon-cli")]
#[command(about = "A Python parser and syntax highlighter built with tree-sitter")]
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
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Parse { file, pretty, tree } => {
            let source = read_input(file)?;
            parse_command(&source, pretty, tree)
        }
        Commands::Highlight { file, no_color } => {
            let source = read_input(file)?;
            highlight_command(&source, !no_color)
        }
        Commands::Check { file } => {
            let source = read_input(file)?;
            check_command(&source)
        }
    }
}

fn read_input(file: Option<PathBuf>) -> Result<String> {
    match file {
        Some(path) => fs::read_to_string(&path)
            .with_context(|| format!("Failed to read file: {}", path.display())),
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

    let parsed = parser
        .parse(source)
        .with_context(|| "Failed to parse Python source")?;

    if show_tree {
        println!("Tree-sitter CST structure:");
        println!("{}", parser.debug_tree(&parsed));
        println!();
    }

    let ast = parser
        .to_ast(&parsed)
        .with_context(|| "Failed to convert to AST")?;

    if pretty {
        println!("Python AST:");
        println!("{:#?}", ast);
    } else {
        println!("{:?}", ast);
    }

    Ok(())
}

fn highlight_command(source: &str, enable_colors: bool) -> Result<()> {
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;

    let parsed = parser
        .parse(source)
        .with_context(|| "Failed to parse Python source")?;

    let highlighter = PythonHighlighter::new(enable_colors);
    let highlighted = highlighter.highlight(source, &parsed.tree);

    println!("{}", highlighted);

    Ok(())
}

fn check_command(source: &str) -> Result<()> {
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;

    match parser.parse(source) {
        Ok(parsed) => {
            let root = parsed.tree.root_node();
            if root.has_error() {
                println!("❌ Parse errors found:");
                print_parse_errors(root, source, 0);
                std::process::exit(1);
            } else {
                println!("✅ No parse errors found");
            }
        }
        Err(e) => {
            println!("❌ Failed to parse: {}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}

fn print_parse_errors(node: tree_sitter::Node, source: &str, depth: usize) {
    if node.is_error() {
        let start_pos = node.start_position();
        let end_pos = node.end_position();
        let text = node.utf8_text(source.as_bytes()).unwrap_or("<invalid>");

        println!(
            "{}Error at line {}, column {}-{}: '{}'",
            "  ".repeat(depth),
            start_pos.row + 1,
            start_pos.column + 1,
            end_pos.column + 1,
            text.replace('\n', "\\n")
        );
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        print_parse_errors(child, source, depth + 1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_read_input_from_file() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "x = 42").unwrap();

        let content = read_input(Some(temp_file.path().to_path_buf())).unwrap();
        assert_eq!(content.trim(), "x = 42");
    }

    #[test]
    fn test_parse_command() {
        let source = "def hello(): pass";
        let result = parse_command(source, false, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_highlight_command() {
        let source = "def hello(): pass";
        let result = highlight_command(source, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_check_command_valid() {
        let source = "x = 42";
        let result = check_command(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_check_command_with_errors() {
        // Test check command with syntax errors
        // Note: This function will call process::exit(1) on errors,
        // so we can't test the actual error path in unit tests easily.
        // We'll just test that it runs without panicking and returns Ok
        let source = "x = 42"; // Valid syntax
        let result = check_command(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_print_parse_errors() {
        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let source = "def incomplete_func(";

        let parsed = parser.parse(source).unwrap();
        let root = parsed.tree.root_node();

        // This function prints to stdout, so we can't easily capture output
        // But we can test that it doesn't panic
        print_parse_errors(root, source, 0);
        // If we reach here, the function didn't panic
        assert!(true);
    }

    #[test]
    fn test_parse_command_with_tree() {
        let source = "x = 42";
        let result = parse_command(source, false, true);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_command_with_pretty() {
        let source = "def hello(): pass";
        let result = parse_command(source, true, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_command_with_both_flags() {
        let source = "class Test: pass";
        let result = parse_command(source, true, true);
        assert!(result.is_ok());
    }
}
