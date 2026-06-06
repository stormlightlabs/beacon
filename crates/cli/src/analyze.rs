use super::AnalyzeTarget;
use super::cfg;
use super::formatters::{format_analysis_local_output, format_analysis_lsp_output};
use anyhow::{Context, Result};
use beacon_analyzer::Linter;
use beacon_cli::diagnostics::{DiagnosticFilter, OutputFormat, filter_diagnostics, run_workspace_diagnostics};
use beacon_parser::{AstNode, PythonParser};
use std::fs;
use std::path::{Path, PathBuf};

pub(crate) async fn analyze_command(
    target: AnalyzeTarget, format: OutputFormat, show_cfg: bool, show_types: bool, lint_only: bool, dataflow_only: bool,
) -> Result<()> {
    match target {
        AnalyzeTarget::File { file } => {
            analyze_file(&file, format, show_cfg, show_types, lint_only, dataflow_only).await
        }
        AnalyzeTarget::Function { target } => {
            let (file, name) = parse_target(&target)?;
            analyze_function(&file, &name, format, show_cfg, show_types, lint_only, dataflow_only)
        }
        AnalyzeTarget::Class { target } => {
            let (file, name) = parse_target(&target)?;
            analyze_class(&file, &name, format, show_cfg, show_types, lint_only, dataflow_only)
        }
        AnalyzeTarget::Package { path } => {
            analyze_package(&path, format, show_cfg, show_types, lint_only, dataflow_only).await
        }
        AnalyzeTarget::Project { path } => {
            analyze_project(&path, format, show_cfg, show_types, lint_only, dataflow_only).await
        }
    }
}

fn parse_target(target: &str) -> Result<(PathBuf, String)> {
    let parts: Vec<&str> = target.rsplitn(2, ':').collect();
    if parts.len() != 2 {
        anyhow::bail!("Invalid target format. Expected 'file.py:name', got '{target}'");
    }
    let name = parts[0].to_string();
    let file = PathBuf::from(parts[1]);
    Ok((file, name))
}

async fn analyze_file(
    file: &PathBuf, format: OutputFormat, show_cfg: bool, show_types: bool, lint_only: bool, dataflow_only: bool,
) -> Result<()> {
    let extras = if show_cfg || show_types {
        let source = fs::read_to_string(file).with_context(|| format!("Failed to read file: {}", file.display()))?;
        let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;
        let (ast, _) = parser
            .parse_and_resolve(&source)
            .with_context(|| "Failed to parse and resolve Python source")?;
        cfg::AnalysisExtras::for_file(
            file,
            &source,
            &ast,
            !lint_only && !dataflow_only && show_cfg,
            show_types,
            None,
        )?
    } else {
        cfg::AnalysisExtras::default()
    };

    let workspace_root = file
        .parent()
        .map(|path| path.canonicalize().unwrap_or_else(|_| path.to_path_buf()));
    let (diagnostics, failed_files) = run_workspace_diagnostics(vec![file.clone()], workspace_root).await?;
    let diagnostics = filter_diagnostics(diagnostics, analyze_filter(lint_only, dataflow_only));
    format_analysis_lsp_output(&extras, &diagnostics, &failed_files, format)?;

    if (!diagnostics.is_empty() || !failed_files.is_empty()) && !cfg!(test) {
        std::process::exit(1);
    }

    Ok(())
}

fn analyze_filter(lint_only: bool, dataflow_only: bool) -> DiagnosticFilter {
    if lint_only {
        DiagnosticFilter::Lint
    } else if dataflow_only {
        DiagnosticFilter::DataFlow
    } else {
        DiagnosticFilter::All
    }
}

fn analyze_function(
    file: &PathBuf, name: &str, format: OutputFormat, show_cfg: bool, show_types: bool, lint_only: bool,
    dataflow_only: bool,
) -> Result<()> {
    let source = fs::read_to_string(file).with_context(|| format!("Failed to read file: {}", file.display()))?;
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;

    let (ast, symbol_table) = parser
        .parse_and_resolve(&source)
        .with_context(|| "Failed to parse and resolve Python source")?;

    let function_node = extract_function(&ast, name)
        .ok_or_else(|| anyhow::anyhow!("Function '{}' not found in {}", name, file.display()))?;

    let filename = file.display().to_string();
    let mut all_diagnostics = Vec::new();

    if !dataflow_only {
        let mut linter = Linter::new(&symbol_table, filename, source.as_str());
        let lint_diagnostics = linter.analyze(function_node);
        all_diagnostics.extend(lint_diagnostics);
    }

    let extras = cfg::AnalysisExtras::for_file(
        file,
        &source,
        &ast,
        !lint_only && !dataflow_only && show_cfg,
        show_types,
        Some((name, function_node)),
    )?;

    format_analysis_local_output(&extras, &all_diagnostics, &source, Some(file), format)?;

    if !all_diagnostics.is_empty() && !cfg!(test) {
        std::process::exit(1);
    }

    Ok(())
}

fn analyze_class(
    file: &PathBuf, name: &str, format: OutputFormat, show_cfg: bool, show_types: bool, lint_only: bool,
    dataflow_only: bool,
) -> Result<()> {
    let source = fs::read_to_string(file).with_context(|| format!("Failed to read file: {}", file.display()))?;
    let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;

    let (ast, symbol_table) = parser
        .parse_and_resolve(&source)
        .with_context(|| "Failed to parse and resolve Python source")?;

    let class_node =
        extract_class(&ast, name).ok_or_else(|| anyhow::anyhow!("Class '{}' not found in {}", name, file.display()))?;

    let filename = file.display().to_string();
    let mut all_diagnostics = Vec::new();

    if !dataflow_only {
        let mut linter = Linter::new(&symbol_table, filename, source.as_str());
        let lint_diagnostics = linter.analyze(class_node);
        all_diagnostics.extend(lint_diagnostics);
    }

    let extras = cfg::AnalysisExtras::for_file(
        file,
        &source,
        &ast,
        !lint_only && !dataflow_only && show_cfg,
        show_types,
        Some((name, class_node)),
    )?;

    format_analysis_local_output(&extras, &all_diagnostics, &source, Some(file), format)?;

    if !all_diagnostics.is_empty() && !cfg!(test) {
        std::process::exit(1);
    }

    Ok(())
}

async fn analyze_package(
    path: &Path, format: OutputFormat, show_cfg: bool, show_types: bool, lint_only: bool, dataflow_only: bool,
) -> Result<()> {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let paths = vec![path.clone()];
    let extras = cfg::AnalysisExtras::for_paths(&paths, !lint_only && !dataflow_only && show_cfg, show_types)?;
    let (diagnostics, failed_files) = run_workspace_diagnostics(paths, Some(path)).await?;
    let diagnostics = filter_diagnostics(diagnostics, analyze_filter(lint_only, dataflow_only));

    format_analysis_lsp_output(&extras, &diagnostics, &failed_files, format)?;

    if (!diagnostics.is_empty() || !failed_files.is_empty()) && !cfg!(test) {
        std::process::exit(1);
    }
    Ok(())
}

async fn analyze_project(
    path: &Path, format: OutputFormat, show_cfg: bool, show_types: bool, lint_only: bool, dataflow_only: bool,
) -> Result<()> {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let paths = vec![path.clone()];
    let extras = cfg::AnalysisExtras::for_paths(&paths, !lint_only && !dataflow_only && show_cfg, show_types)?;
    let (diagnostics, failed_files) = run_workspace_diagnostics(paths, Some(path)).await?;
    let diagnostics = filter_diagnostics(diagnostics, analyze_filter(lint_only, dataflow_only));

    format_analysis_lsp_output(&extras, &diagnostics, &failed_files, format)?;

    if (!diagnostics.is_empty() || !failed_files.is_empty()) && !cfg!(test) {
        std::process::exit(1);
    }
    Ok(())
}

fn extract_function<'a>(ast: &'a AstNode, name: &str) -> Option<&'a AstNode> {
    match ast {
        AstNode::Module { body, .. } => {
            for node in body {
                if let AstNode::FunctionDef { name: func_name, .. } = node
                    && func_name == name
                {
                    return Some(node);
                }
                if let Some(found) = extract_function(node, name) {
                    return Some(found);
                }
            }
            None
        }
        AstNode::FunctionDef { name: func_name, body, .. } => {
            if func_name == name {
                return Some(ast);
            }
            for node in body {
                if let Some(found) = extract_function(node, name) {
                    return Some(found);
                }
            }
            None
        }
        AstNode::ClassDef { body, .. } => {
            for node in body {
                if let Some(found) = extract_function(node, name) {
                    return Some(found);
                }
            }
            None
        }
        _ => None,
    }
}

fn extract_class<'a>(ast: &'a AstNode, name: &str) -> Option<&'a AstNode> {
    match ast {
        AstNode::Module { body, .. } => {
            for node in body {
                if let AstNode::ClassDef { name: class_name, .. } = node
                    && class_name == name
                {
                    return Some(node);
                }
                if let Some(found) = extract_class(node, name) {
                    return Some(found);
                }
            }
            None
        }
        AstNode::ClassDef { name: class_name, body, .. } => {
            if class_name == name {
                return Some(ast);
            }
            for node in body {
                if let Some(found) = extract_class(node, name) {
                    return Some(found);
                }
            }
            None
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::io::Write;
    use tempfile::{Builder, NamedTempFile};

    #[test]
    fn test_parse_target_valid() {
        let result = parse_target("path/to/file.py:MyClass");
        assert!(result.is_ok());
        let (file, name) = result.unwrap();
        assert_eq!(file, PathBuf::from("path/to/file.py"));
        assert_eq!(name, "MyClass");
    }

    #[test]
    fn test_parse_target_invalid() {
        let result = parse_target("invalid_format");
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_analyze_file() {
        let mut temp_file = Builder::new().suffix(".py").tempfile().unwrap();
        writeln!(temp_file, "def add(x, y):\n    return x + y").unwrap();

        let result = analyze_file(
            &temp_file.path().to_path_buf(),
            OutputFormat::Human,
            false,
            false,
            false,
            false,
        )
        .await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_analyze_function_found() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "def foo():\n    pass\n\ndef bar():\n    pass").unwrap();

        let result = analyze_function(
            &temp_file.path().to_path_buf(),
            "foo",
            OutputFormat::Human,
            false,
            false,
            false,
            false,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_analyze_function_not_found() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "def foo():\n    pass").unwrap();

        let result = analyze_function(
            &temp_file.path().to_path_buf(),
            "nonexistent",
            OutputFormat::Human,
            false,
            false,
            false,
            false,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_analyze_class_found() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "class Foo:\n    pass\n\nclass Bar:\n    pass").unwrap();

        let result = analyze_class(
            &temp_file.path().to_path_buf(),
            "Foo",
            OutputFormat::Human,
            false,
            false,
            false,
            false,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_analyze_class_not_found() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "class Foo:\n    pass").unwrap();

        let result = analyze_class(
            &temp_file.path().to_path_buf(),
            "NonExistent",
            OutputFormat::Human,
            false,
            false,
            false,
            false,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_extract_function() {
        let source = "def foo():\n    pass\n\ndef bar():\n    pass";
        let mut parser = PythonParser::new().unwrap();
        let (ast, _) = parser.parse_and_resolve(source).unwrap();

        let found = extract_function(&ast, "foo");
        assert!(found.is_some());

        let not_found = extract_function(&ast, "baz");
        assert!(not_found.is_none());
    }

    #[test]
    fn test_extract_class() {
        let source = "class Foo:\n    pass\n\nclass Bar:\n    pass";
        let mut parser = PythonParser::new().unwrap();
        let (ast, _) = parser.parse_and_resolve(source).unwrap();

        let found = extract_class(&ast, "Foo");
        assert!(found.is_some());

        let not_found = extract_class(&ast, "Baz");
        assert!(not_found.is_none());
    }
}
