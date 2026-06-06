use crate::helpers;

use anyhow::{Context, Result};
use beacon_analyzer::cfg::{CfgBuilder, ControlFlowGraph};
#[cfg(debug_assertions)]
use beacon_lsp::workspace::Workspace;
use beacon_lsp::{Config, analysis::AnalysisResult, analysis::Analyzer, document::DocumentManager};
use beacon_parser::{AstNode, PythonParser};
use owo_colors::OwoColorize;
use serde_json::json;
use std::fs;
use std::path::{Path, PathBuf};
use url::Url;

#[derive(Debug, Default)]
pub(crate) struct AnalysisExtras {
    cfg_graphs: Vec<CliCfgGraph>,
    inferred_types: Vec<CliTypeEntry>,
}

impl AnalysisExtras {
    pub(crate) fn is_empty(&self) -> bool {
        self.cfg_graphs.is_empty() && self.inferred_types.is_empty()
    }

    pub(crate) fn for_file(
        file: &Path, source: &str, ast: &AstNode, show_cfg: bool, show_types: bool, scope: Option<(&str, &AstNode)>,
    ) -> Result<Self> {
        let mut extras = Self::default();

        if show_cfg {
            extras.cfg_graphs = collect_cfg_graphs(file, ast, scope);
        }

        if show_types {
            let analysis = analyze_source_for_cli(file, source)?;
            extras.inferred_types = collect_type_entries(file, &analysis, scope.map(|(_, node)| node));
        }

        Ok(extras)
    }

    pub(crate) fn for_paths(paths: &[PathBuf], show_cfg: bool, show_types: bool) -> Result<Self> {
        let mut extras = Self::default();

        if !show_cfg && !show_types {
            return Ok(extras);
        }

        let files = helpers::discover_python_files(paths)?;
        for file in files {
            let source =
                fs::read_to_string(&file).with_context(|| format!("Failed to read file: {}", file.display()))?;
            let mut parser = PythonParser::new().with_context(|| "Failed to create Python parser")?;
            let (ast, _) = parser
                .parse_and_resolve(&source)
                .with_context(|| format!("Failed to parse Python source: {}", file.display()))?;
            let file_extras = Self::for_file(&file, &source, &ast, show_cfg, show_types, None)?;
            extras.cfg_graphs.extend(file_extras.cfg_graphs);
            extras.inferred_types.extend(file_extras.inferred_types);
        }

        Ok(extras)
    }

    pub(crate) fn format_human(&self) {
        if !self.cfg_graphs.is_empty() {
            println!("{} Control Flow Graph:", "▶".bright_green().bold());
            for graph in &self.cfg_graphs {
                println!(
                    "  {} {} {} {}",
                    "▸".blue(),
                    graph.file.cyan(),
                    "::".dimmed(),
                    graph.scope.yellow()
                );
                println!(
                    "    {}: {}  {}: {}  {}: {}",
                    "blocks".cyan(),
                    graph.blocks.len().to_string().yellow(),
                    "entry".cyan(),
                    graph.entry.to_string().yellow(),
                    "exit".cyan(),
                    graph.exit.to_string().yellow()
                );
                for block in &graph.blocks {
                    let edges = if block.successors.is_empty() {
                        "none".dimmed().to_string()
                    } else {
                        block
                            .successors
                            .iter()
                            .map(|edge| format!("{}({})", edge.to, edge.kind))
                            .collect::<Vec<_>>()
                            .join(", ")
                    };
                    let reachability =
                        if block.reachable { "reachable".green().to_string() } else { "unreachable".red().to_string() };
                    println!(
                        "    block {} [{}] stmts={:?} -> {}",
                        block.id.to_string().cyan(),
                        reachability,
                        block.statements,
                        edges
                    );
                }
            }
            println!();
        }

        if !self.inferred_types.is_empty() {
            println!("{} Inferred Types:", "▶".bright_green().bold());
            for entry in self.inferred_types.iter().take(50) {
                println!(
                    "  {} {}:{}:{} node {} -> {}",
                    "▸".blue(),
                    entry.file.cyan(),
                    entry.line.to_string().yellow(),
                    entry.col.to_string().yellow(),
                    entry.node_id.to_string().cyan(),
                    entry.type_name.bright_white()
                );
            }
            if self.inferred_types.len() > 50 {
                println!(
                    "  {} and {} more",
                    "...".dimmed(),
                    (self.inferred_types.len() - 50).to_string().yellow()
                );
            }
            println!();
        }
    }

    pub(crate) fn format_compact(&self) {
        for graph in &self.cfg_graphs {
            println!(
                "{}: cfg: scope={} blocks={} entry={} exit={} unreachable={}",
                graph.file,
                graph.scope,
                graph.blocks.len(),
                graph.entry,
                graph.exit,
                graph.unreachable_blocks.len()
            );
            for block in &graph.blocks {
                let edges = block
                    .successors
                    .iter()
                    .map(|edge| format!("{}:{}", edge.to, edge.kind))
                    .collect::<Vec<_>>()
                    .join(",");
                println!(
                    "{}: cfg-block: scope={} id={} stmts={:?} succ=[{}]",
                    graph.file, graph.scope, block.id, block.statements, edges
                );
            }
        }

        for entry in &self.inferred_types {
            println!(
                "{}:{}:{}: [type]: node_id={} {}",
                entry.file, entry.line, entry.col, entry.node_id, entry.type_name
            );
        }
    }

    pub(crate) fn cfg_json(&self) -> Vec<serde_json::Value> {
        self.cfg_graphs
            .iter()
            .map(|graph| {
                json!({
                    "file": graph.file,
                    "scope": graph.scope,
                    "entry": graph.entry,
                    "exit": graph.exit,
                    "unreachable_blocks": graph.unreachable_blocks,
                    "blocks": graph.blocks.iter().map(|block| {
                        json!({
                            "id": block.id,
                            "statements": block.statements,
                            "successors": block.successors.iter().map(|edge| {
                                json!({
                                    "to": edge.to,
                                    "kind": edge.kind,
                                })
                            }).collect::<Vec<_>>(),
                            "predecessor_count": block.predecessor_count,
                            "reachable": block.reachable,
                        })
                    }).collect::<Vec<_>>(),
                })
            })
            .collect()
    }

    pub(crate) fn inferred_types_json(&self) -> Vec<serde_json::Value> {
        self.inferred_types
            .iter()
            .map(|entry| {
                json!({
                    "file": entry.file,
                    "node_id": entry.node_id,
                    "span": {
                        "start": {
                            "line": entry.line,
                            "col": entry.col,
                        },
                        "end": {
                            "line": entry.end_line,
                            "col": entry.end_col,
                        },
                    },
                    "type": entry.type_name,
                })
            })
            .collect()
    }
}

#[derive(Debug)]
struct CliCfgGraph {
    file: String,
    scope: String,
    entry: usize,
    exit: usize,
    blocks: Vec<CliCfgBlock>,
    unreachable_blocks: Vec<usize>,
}

impl CliCfgGraph {
    fn from_node(file: &Path, scope_name: &str, node: &AstNode) -> Option<Self> {
        match node {
            AstNode::Module { body, .. } => Some(Self::from_body(file, scope_name, body, false)),
            AstNode::FunctionDef { body, .. } => Some(Self::from_body(file, scope_name, body, true)),
            AstNode::ClassDef { body, .. } => Some(Self::from_body(file, scope_name, body, false)),
            _ => None,
        }
    }

    fn from_body(file: &Path, scope_name: &str, body: &[AstNode], function_body: bool) -> Self {
        let mut builder = CfgBuilder::new();
        if function_body {
            builder.build_function(body);
        } else {
            builder.build_module(body);
        }

        Self::from_cfg(file, scope_name, &builder.build())
    }

    fn from_cfg(file: &Path, scope_name: &str, cfg: &ControlFlowGraph) -> Self {
        let mut reachable = cfg.reachable_blocks();
        reachable.sort_by_key(|block| block.0);
        let reachable_set: std::collections::HashSet<_> = reachable.iter().map(|block| block.0).collect();

        let mut unreachable_blocks = cfg.unreachable_blocks();
        unreachable_blocks.sort_by_key(|block| block.0);

        let mut blocks: Vec<_> = cfg.blocks.values().collect();
        blocks.sort_by_key(|block| block.id.0);

        Self {
            file: display_source_file(file),
            scope: scope_name.to_string(),
            entry: cfg.entry.0,
            exit: cfg.exit.0,
            unreachable_blocks: unreachable_blocks.iter().map(|block| block.0).collect(),
            blocks: blocks
                .into_iter()
                .map(|block| {
                    let mut successors: Vec<_> = block
                        .successors
                        .iter()
                        .map(|(to, kind)| CliCfgEdge { to: to.0, kind: kind.to_string() })
                        .collect();
                    successors.sort_by(|a, b| a.to.cmp(&b.to).then_with(|| a.kind.cmp(&b.kind)));

                    CliCfgBlock {
                        id: block.id.0,
                        statements: block.statements.clone(),
                        successors,
                        predecessor_count: block.predecessors.len(),
                        reachable: reachable_set.contains(&block.id.0),
                    }
                })
                .collect(),
        }
    }
}

#[derive(Debug)]
struct CliCfgBlock {
    id: usize,
    statements: Vec<usize>,
    successors: Vec<CliCfgEdge>,
    predecessor_count: usize,
    reachable: bool,
}

#[derive(Debug)]
struct CliCfgEdge {
    to: usize,
    kind: String,
}

#[derive(Debug)]
struct CliTypeEntry {
    file: String,
    node_id: usize,
    line: usize,
    col: usize,
    end_line: usize,
    end_col: usize,
    type_name: String,
}

fn analyze_source_for_cli(file: &Path, source: &str) -> Result<AnalysisResult> {
    let documents = DocumentManager::new()?;
    let file_path = file.canonicalize().unwrap_or_else(|_| file.to_path_buf());
    let uri = Url::from_file_path(&file_path)
        .map_err(|_| anyhow::anyhow!("Failed to create URL for {}", file_path.display()))?;

    documents.open_document(uri.clone(), 1, source)?;

    let config = Config::default();
    let mut analyzer = Analyzer::new(config, documents);
    analyzer
        .analyze(&uri)
        .with_context(|| format!("Failed to analyze {}", file.display()))
}

fn collect_cfg_graphs(file: &Path, ast: &AstNode, scope: Option<(&str, &AstNode)>) -> Vec<CliCfgGraph> {
    if let Some((name, node)) = scope {
        return CliCfgGraph::from_node(file, name, node).into_iter().collect();
    }

    let mut graphs = Vec::new();
    if let Some(graph) = CliCfgGraph::from_node(file, "<module>", ast) {
        graphs.push(graph);
    }
    collect_named_cfg_graphs(file, ast, &mut graphs);
    graphs
}

fn collect_named_cfg_graphs(file: &Path, node: &AstNode, graphs: &mut Vec<CliCfgGraph>) {
    match node {
        AstNode::Module { body, .. } | AstNode::ClassDef { body, .. } => {
            for child in body {
                collect_named_cfg_graphs(file, child, graphs);
            }
        }
        AstNode::FunctionDef { name, body, .. } => {
            graphs.push(CliCfgGraph::from_body(file, name, body, true));
            for child in body {
                collect_named_cfg_graphs(file, child, graphs);
            }
        }
        _ => {}
    }
}

fn collect_type_entries(file: &Path, analysis: &AnalysisResult, scope: Option<&AstNode>) -> Vec<CliTypeEntry> {
    let scope_range = scope.map(AstNode::source_range);
    let mut entries: Vec<_> = analysis
        .type_map
        .iter()
        .filter_map(|(node_id, ty)| {
            let span = analysis.node_spans.get(node_id)?;
            if let Some(range) = scope_range
                && !span_intersects_range(span.line, span.end_line.unwrap_or(span.line), &range)
            {
                return None;
            }

            Some(CliTypeEntry {
                file: display_source_file(file),
                node_id: *node_id,
                line: span.line,
                col: span.col,
                end_line: span.end_line.unwrap_or(span.line),
                end_col: span.end_col.unwrap_or(span.col),
                type_name: ty.to_string(),
            })
        })
        .collect();

    entries.sort_by(|a, b| {
        a.file
            .cmp(&b.file)
            .then_with(|| a.line.cmp(&b.line))
            .then_with(|| a.col.cmp(&b.col))
            .then_with(|| a.node_id.cmp(&b.node_id))
    });
    entries
}

fn display_source_file(file: &Path) -> String {
    file.canonicalize()
        .unwrap_or_else(|_| file.to_path_buf())
        .display()
        .to_string()
}

fn span_intersects_range(start_line: usize, end_line: usize, range: &beacon_parser::SourceRange) -> bool {
    start_line <= range.end_line && end_line >= range.line
}

#[cfg(debug_assertions)]
pub(crate) async fn debug_cfg_command(path: PathBuf, json: bool) -> Result<()> {
    let canonical_path = path
        .canonicalize()
        .with_context(|| format!("Failed to resolve path: {}", path.display()))?;

    let workspace_uri = Url::from_file_path(&canonical_path)
        .map_err(|_| anyhow::anyhow!("Failed to create URL for path: {}", canonical_path.display()))?;

    let documents = DocumentManager::new()?;
    let config = Config::default();
    let mut workspace = Workspace::new(Some(workspace_uri), config, documents.clone());

    println!(
        "{} {} ...",
        "Initializing workspace at".cyan(),
        canonical_path.display().to_string().yellow()
    );

    workspace.initialize()?;

    let python_files = helpers::discover_python_files(std::slice::from_ref(&canonical_path))?;
    println!(
        "{} {} Python files",
        "Found".green(),
        python_files.len().to_string().yellow()
    );

    for file_path in &python_files {
        let source =
            fs::read_to_string(file_path).with_context(|| format!("Failed to read file: {}", file_path.display()))?;

        let uri = Url::from_file_path(file_path)
            .map_err(|_| anyhow::anyhow!("Failed to create URL for {}", file_path.display()))?;

        documents.open_document(uri.clone(), 1, &source)?;
        workspace.update_dependencies(&uri);
        workspace.build_module_cfg(&uri);
    }

    workspace.link_workspace_cfg();

    let workspace_cfg = workspace.workspace_cfg();
    let cfg = workspace_cfg
        .read()
        .map_err(|_| anyhow::anyhow!("Failed to read workspace CFG"))?;
    let summary = cfg.debug_summary();

    if json {
        let json_output = serde_json::json!({
            "module_count": summary.module_count,
            "function_count": summary.function_count,
            "call_edge_count": summary.call_edge_count,
            "has_circular_dependencies": summary.has_circular_deps,
            "functions": summary.functions.iter().map(|f| serde_json::json!({
                "module": f.module,
                "name": f.name,
            })).collect::<Vec<_>>(),
            "call_edges": summary.call_edges.iter().map(|e| serde_json::json!({
                "caller_module": e.caller_module,
                "caller_name": e.caller_name,
                "callee_module": e.callee_module,
                "callee_name": e.callee_name,
                "line": e.line,
            })).collect::<Vec<_>>(),
        });
        println!("{}", serde_json::to_string_pretty(&json_output)?);
    } else {
        println!("\n{}", "Workspace CFG Summary".cyan().bold());
        println!("{}", "=".repeat(40).dimmed());
        println!("  {}: {}", "Modules".cyan(), summary.module_count.to_string().yellow());
        println!(
            "  {}: {}",
            "Functions".cyan(),
            summary.function_count.to_string().yellow()
        );
        println!(
            "  {}: {}",
            "Call edges".cyan(),
            summary.call_edge_count.to_string().yellow()
        );

        if summary.has_circular_deps {
            println!(
                "  {}: {}",
                "Circular dependencies".red().bold(),
                "DETECTED".bright_red().bold()
            );
        } else {
            println!("  {}: {}", "Circular dependencies".green(), "None".green());
        }

        if !summary.functions.is_empty() {
            println!("\n{}", "Functions".cyan().bold());
            for func in &summary.functions {
                println!("  {} {}.{}", "▸".blue(), func.module.dimmed(), func.name.bright_white());
            }
        }

        if !summary.call_edges.is_empty() {
            println!("\n{}", "Call Graph Edges".cyan().bold());
            for edge in &summary.call_edges {
                let caller = format!("{} (line {})", edge.caller_name, edge.line);
                let callee = &edge.callee_name;
                println!(
                    "  {} {} {} {}",
                    caller.bright_white(),
                    "→".yellow(),
                    callee.green(),
                    format!("({})", edge.callee_module).dimmed()
                );
            }
        }
    }

    Ok(())
}
