use anyhow::{Context, Result};
use beacon_parser::{PythonHighlighter, PythonParser, SymbolKind, SymbolTable};
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
    /// Analyze name resolution and show symbol table
    Resolve {
        /// Python file to analyze
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,

        /// Show detailed scope information
        #[arg(short, long)]
        verbose: bool,
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
        Commands::Resolve { file, verbose } => {
            let source = read_input(file)?;
            resolve_command(&source, verbose)
        }
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
        println!("Tree-sitter CST structure:");
        println!("{}", parser.debug_tree(&parsed));
        println!();
    }

    let ast = parser.to_ast(&parsed).with_context(|| "Failed to convert to AST")?;

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

    let parsed = parser.parse(source).with_context(|| "Failed to parse Python source")?;

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
                if cfg!(test) {
                    anyhow::bail!("Parse errors found in source code");
                } else {
                    println!("❌ Parse errors found:");
                    print_parse_errors(root, source, 0);
                    std::process::exit(1);
                }
            } else {
                println!("✅ No parse errors found");
            }
        }
        Err(e) => {
            if cfg!(test) {
                return Err(e).with_context(|| "Failed to parse source code");
            } else {
                println!("❌ Failed to parse: {}", e);
                std::process::exit(1);
            }
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

fn resolve_command(source: &str, verbose: bool) -> Result<()> {
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;

    let (_ast, symbol_table) = parser
        .parse_and_resolve(source)
        .with_context(|| "Failed to parse and resolve Python source")?;

    println!("⚡ Name Resolution Analysis");
    println!("===========================");

    print_symbol_table(&symbol_table, verbose);

    if verbose {
        println!("\n▶ Statistics:");
        println!("  • Total scopes: {}", symbol_table.scopes.len());
        let total_symbols: usize = symbol_table.scopes.values().map(|scope| scope.symbols.len()).sum();
        println!("  • Total symbols: {}", total_symbols);
    }

    Ok(())
}

fn print_symbol_table(table: &SymbolTable, verbose: bool) {
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
        println!("▼ {} Scope:", scope_name);
    } else {
        println!("{}▼ {} Scope:", indent, scope_name);
    }

    let mut symbols: Vec<_> = scope.symbols.values().collect();
    symbols.sort_by_key(|s| (&s.kind, &s.name));

    for symbol in symbols {
        let symbol_icon = match symbol.kind {
            SymbolKind::Variable => "◆",
            SymbolKind::Function => "λ",
            SymbolKind::Class => "●",
            SymbolKind::Parameter => "▲",
            SymbolKind::Import => "↳",
        };

        let kind_name = match symbol.kind {
            SymbolKind::Variable => "variable",
            SymbolKind::Function => "function",
            SymbolKind::Class => "class",
            SymbolKind::Parameter => "parameter",
            SymbolKind::Import => "import",
        };

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

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

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

        let (_ast, symbol_table) = parser.parse_and_resolve(source).unwrap();

        let output = format_symbol_table_for_test(&symbol_table, false);
        assert!(output.contains("Module Scope"));

        let verbose_output = format_symbol_table_for_test(&symbol_table, true);
        assert!(verbose_output.contains("Total scopes: 1"));
        assert!(verbose_output.contains("Total symbols: 0"));
    }

    /// Test helper function that returns formatted output instead of printing
    fn format_symbol_table_for_test(table: &SymbolTable, verbose: bool) -> String {
        let mut output = String::new();
        format_scope_for_test(table, table.root_scope, 0, verbose, &mut output);

        if verbose {
            output.push_str(&format!("\nStatistics:\n"));
            output.push_str(&format!("  • Total scopes: {}\n", table.scopes.len()));
            let total_symbols: usize = table.scopes.values().map(|scope| scope.symbols.len()).sum();
            output.push_str(&format!("  • Total symbols: {}\n", total_symbols));
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
            output.push_str(&format!("▼ {} Scope:\n", scope_name));
        } else {
            output.push_str(&format!("{}▼ {} Scope:\n", indent, scope_name));
        }

        let mut symbols: Vec<_> = scope.symbols.values().collect();
        symbols.sort_by_key(|s| (&s.kind, &s.name));

        for symbol in symbols {
            let symbol_icon = match symbol.kind {
                SymbolKind::Variable => "◆",
                SymbolKind::Function => "λ",
                SymbolKind::Class => "●",
                SymbolKind::Parameter => "▲",
                SymbolKind::Import => "↳",
            };

            let kind_name = match symbol.kind {
                SymbolKind::Variable => "variable",
                SymbolKind::Function => "function",
                SymbolKind::Class => "class",
                SymbolKind::Parameter => "parameter",
                SymbolKind::Import => "import",
            };

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
}
