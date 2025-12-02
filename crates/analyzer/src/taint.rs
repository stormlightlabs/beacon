//! Taint analysis for tracking untrusted data flow across module boundaries
//!
//! Identifies security vulnerabilities by tracking how tainted data from [TaintSource] flows through the program to dangerous [TaintSink].
//! Supports both intra-module and cross-module taint propagation using the workspace call graph.

use beacon_parser::{AstNode, ScopeId, SymbolTable};
use rustc_hash::{FxHashMap, FxHashSet};
use url::Url;

use crate::cfg::{BlockId, CallGraph, ControlFlowGraph, FunctionId};

/// A taint source is anywhere untrusted data enters the program
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TaintSource {
    /// Variable name that becomes tainted
    pub var_name: String,
    pub line: usize,
    pub col: usize,
    /// Type of taint source
    pub kind: TaintSourceKind,
}

/// Types of taint sources
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TaintSourceKind {
    /// User input (e.g., input(), sys.stdin.read())
    UserInput,
    /// Environment variables (e.g., os.environ, os.getenv())
    Environment,
    /// File reads (e.g., open().read(), pathlib.Path.read_text())
    FileRead,
    /// Network data (e.g., requests.get(), socket.recv())
    Network,
    /// Command-line arguments (e.g., sys.argv)
    CommandLine,
    /// Database queries (e.g., cursor.execute().fetchall())
    Database,
}

/// A taint sink - where tainted data could cause security issues
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TaintSink {
    /// Expression that receives tainted data
    pub line: usize,
    pub col: usize,
    /// Type of sink
    pub kind: TaintSinkKind,
}

/// Types of taint sinks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TaintSinkKind {
    /// Code execution (e.g., eval(), exec(), compile())
    CodeExecution,
    /// Command execution (e.g., os.system(), subprocess.run())
    CommandExecution,
    /// SQL queries (e.g., cursor.execute() with string concatenation)
    SqlQuery,
    /// File writes (e.g., open().write(), pathlib.Path.write_text())
    FileWrite,
    /// Path operations (e.g., os.path.join() leading to traversal)
    PathOperation,
    /// Deserialization (e.g., pickle.loads(), yaml.load())
    Deserialization,
    /// Template rendering (e.g., Template().render() with untrusted input)
    TemplateRender,
}

/// A detected taint flow violation
#[derive(Debug, Clone)]
pub struct TaintViolation {
    /// Source of the tainted data
    pub source: TaintSource,
    /// Sink where tainted data is used unsafely
    pub sink: TaintSink,
    /// Path of variable names showing the taint flow
    pub flow_path: Vec<String>,
}

/// Result of taint analysis
#[derive(Debug, Clone)]
pub struct TaintAnalysisResult {
    /// Detected taint violations
    pub violations: Vec<TaintViolation>,
    /// All identified taint sources
    pub sources: Vec<TaintSource>,
    /// All identified taint sinks
    pub sinks: Vec<TaintSink>,
}

/// Intra-module taint analyzer
///
/// Performs taint analysis within a single module using data flow analysis on the CFG.
pub struct IntraModuleTaintAnalyzer<'a> {
    cfg: &'a ControlFlowGraph,
    #[allow(dead_code)]
    function_body: &'a [AstNode],
    #[allow(dead_code)]
    symbol_table: &'a SymbolTable,
    #[allow(dead_code)]
    scope_id: ScopeId,
    /// Flattened list of all statements for indexing
    all_statements: Vec<&'a AstNode>,
}

impl<'a> IntraModuleTaintAnalyzer<'a> {
    pub fn new(
        cfg: &'a ControlFlowGraph, function_body: &'a [AstNode], symbol_table: &'a SymbolTable, scope_id: ScopeId,
    ) -> Self {
        let mut all_statements = Vec::new();
        Self::collect_all_statements(function_body, &mut all_statements);
        Self { cfg, function_body, symbol_table, scope_id, all_statements }
    }

    /// Recursively collect all statements including nested ones
    fn collect_all_statements(nodes: &'a [AstNode], statements: &mut Vec<&'a AstNode>) {
        for node in nodes {
            Self::collect_from_node(node, statements);
        }
    }

    fn collect_from_node(node: &'a AstNode, statements: &mut Vec<&'a AstNode>) {
        statements.push(node);
        match node {
            AstNode::If { body, elif_parts, else_body, .. } => {
                Self::collect_all_statements(body, statements);
                for (_, elif_body) in elif_parts {
                    Self::collect_all_statements(elif_body, statements);
                }
                if let Some(else_stmts) = else_body {
                    Self::collect_all_statements(else_stmts, statements);
                }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                Self::collect_all_statements(body, statements);
                if let Some(else_stmts) = else_body {
                    Self::collect_all_statements(else_stmts, statements);
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                Self::collect_all_statements(body, statements);
                for handler in handlers {
                    Self::collect_all_statements(&handler.body, statements);
                }
                if let Some(else_stmts) = else_body {
                    Self::collect_all_statements(else_stmts, statements);
                }
                if let Some(finally_stmts) = finally_body {
                    Self::collect_all_statements(finally_stmts, statements);
                }
            }
            AstNode::With { body, .. } => {
                Self::collect_all_statements(body, statements);
            }
            AstNode::Match { cases, .. } => {
                for case in cases {
                    Self::collect_all_statements(&case.body, statements);
                }
            }
            _ => {}
        }
    }

    /// Perform taint analysis and return violations
    pub fn analyze(&self) -> TaintAnalysisResult {
        let sources = self.identify_sources();
        let sinks = self.identify_sinks();

        let mut taint_in: FxHashMap<BlockId, FxHashSet<String>> = FxHashMap::default();
        let mut taint_out: FxHashMap<BlockId, FxHashSet<String>> = FxHashMap::default();

        for block_id in self.cfg.blocks.keys() {
            taint_in.insert(*block_id, FxHashSet::default());
            taint_out.insert(*block_id, FxHashSet::default());
        }

        let mut worklist: Vec<BlockId> = self.cfg.blocks.keys().copied().collect();

        while let Some(block_id) = worklist.pop() {
            let block = self.cfg.blocks.get(&block_id).unwrap();

            let new_taint_in = if block.predecessors.is_empty() {
                FxHashSet::default()
            } else {
                let mut result = FxHashSet::default();
                for &(pred_id, _) in &block.predecessors {
                    if let Some(pred_taint_out) = taint_out.get(&pred_id) {
                        result.extend(pred_taint_out.iter().cloned());
                    }
                }
                result
            };

            let mut new_taint_out = new_taint_in.clone();
            for &stmt_idx in &block.statements {
                if stmt_idx < self.all_statements.len() {
                    let stmt = self.all_statements[stmt_idx];
                    self.propagate_taint(stmt, &mut new_taint_out);
                }
            }

            let old_taint_in = taint_in.get(&block_id).cloned().unwrap_or_default();
            let old_taint_out = taint_out.get(&block_id).cloned().unwrap_or_default();

            if new_taint_in != old_taint_in || new_taint_out != old_taint_out {
                taint_in.insert(block_id, new_taint_in);
                taint_out.insert(block_id, new_taint_out);

                for &(succ_id, _) in &block.successors {
                    if !worklist.contains(&succ_id) {
                        worklist.push(succ_id);
                    }
                }
            }
        }

        let violations = self.find_violations(&taint_in, &taint_out, &sources, &sinks);

        TaintAnalysisResult { violations, sources, sinks }
    }

    /// Identify all taint sources in the function
    fn identify_sources(&self) -> Vec<TaintSource> {
        let mut sources = Vec::new();

        for stmt in &self.all_statements {
            if let Some(source) = self.check_taint_source(stmt) {
                sources.push(source);
            }
        }

        sources
    }

    /// Check if a statement introduces a taint source
    fn check_taint_source(&self, stmt: &AstNode) -> Option<TaintSource> {
        match stmt {
            AstNode::Assignment { target, value, line, col, .. } => {
                let kind = self.classify_taint_source(value)?;
                let var_name = self.extract_target_name(target)?;
                Some(TaintSource { var_name, line: *line, col: *col, kind })
            }
            _ => None,
        }
    }

    /// Classify the type of taint source based on the expression
    fn classify_taint_source(&self, expr: &AstNode) -> Option<TaintSourceKind> {
        match expr {
            AstNode::Call { function, .. } => match &**function {
                AstNode::Identifier { name, .. } => match name.as_str() {
                    "input" => Some(TaintSourceKind::UserInput),
                    "open" => Some(TaintSourceKind::FileRead),
                    _ => None,
                },
                AstNode::Attribute { attribute, .. } => match attribute.as_str() {
                    "read" | "read_text" | "readlines" => Some(TaintSourceKind::FileRead),
                    "recv" | "recvfrom" => Some(TaintSourceKind::Network),
                    "fetchall" | "fetchone" | "fetchmany" => Some(TaintSourceKind::Database),
                    _ => None,
                },
                _ => None,
            },
            AstNode::Attribute { object, attribute, .. } => {
                let obj_name = self.extract_identifier_name(object)?;
                match (obj_name.as_str(), attribute.as_str()) {
                    ("sys", "argv") => Some(TaintSourceKind::CommandLine),
                    ("os", "environ") => Some(TaintSourceKind::Environment),
                    (_, "read" | "read_text" | "readlines") => Some(TaintSourceKind::FileRead),
                    (_, "recv" | "recvfrom") => Some(TaintSourceKind::Network),
                    (_, "fetchall" | "fetchone" | "fetchmany") => Some(TaintSourceKind::Database),
                    _ => None,
                }
            }
            AstNode::Subscript { value, .. } => {
                if let AstNode::Attribute { object, attribute, .. } = &**value {
                    let obj_name = self.extract_identifier_name(object)?;
                    if obj_name == "os" && attribute == "environ" {
                        return Some(TaintSourceKind::Environment);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Identify all taint sinks in the function
    fn identify_sinks(&self) -> Vec<TaintSink> {
        let mut sinks = Vec::new();

        for stmt in &self.all_statements {
            if let Some(sink) = self.check_taint_sink(stmt) {
                sinks.push(sink);
            }
        }

        sinks
    }

    /// Check if a statement is a taint sink
    fn check_taint_sink(&self, stmt: &AstNode) -> Option<TaintSink> {
        match stmt {
            AstNode::Call { function, line, col, .. } => {
                let func_name = self.extract_function_name(function)?;
                let kind = match func_name.as_str() {
                    "eval" | "exec" | "compile" | "__import__" => TaintSinkKind::CodeExecution,
                    "system" => TaintSinkKind::CommandExecution,
                    "execute" => TaintSinkKind::SqlQuery,
                    "write" | "write_text" | "writelines" => TaintSinkKind::FileWrite,
                    "loads" => TaintSinkKind::Deserialization,
                    "render" => TaintSinkKind::TemplateRender,
                    _ => return None,
                };
                Some(TaintSink { line: *line, col: *col, kind })
            }
            AstNode::Attribute { attribute, line, col, .. } => {
                let kind = match attribute.as_str() {
                    "system" | "popen" => TaintSinkKind::CommandExecution,
                    "execute" => TaintSinkKind::SqlQuery,
                    "write" | "write_text" | "writelines" => TaintSinkKind::FileWrite,
                    _ => return None,
                };
                Some(TaintSink { line: *line, col: *col, kind })
            }
            _ => None,
        }
    }

    /// Propagate taint information through a statement
    fn propagate_taint(&self, stmt: &AstNode, tainted: &mut FxHashSet<String>) {
        match stmt {
            AstNode::Assignment { target, value, .. } => {
                if self.is_tainted_expr(value, tainted) {
                    if let Some(target_name) = self.extract_target_name(target) {
                        tainted.insert(target_name);
                    }
                } else {
                    if let Some(target_name) = self.extract_target_name(target) {
                        tainted.remove(&target_name);
                    }
                }
            }
            AstNode::AnnotatedAssignment { target, value: Some(val), .. } => {
                if self.is_tainted_expr(val, tainted) {
                    if let Some(target_name) = self.extract_target_name(target) {
                        tainted.insert(target_name);
                    }
                } else {
                    if let Some(target_name) = self.extract_target_name(target) {
                        tainted.remove(&target_name);
                    }
                }
            }
            _ => {}
        }
    }

    /// Check if an expression is tainted
    fn is_tainted_expr(&self, expr: &AstNode, tainted: &FxHashSet<String>) -> bool {
        match expr {
            AstNode::Identifier { name, .. } => tainted.contains(name),
            AstNode::BinaryOp { left, right, .. } => {
                self.is_tainted_expr(left, tainted) || self.is_tainted_expr(right, tainted)
            }
            AstNode::Call { function, args, .. } => {
                if self.is_tainted_expr(function, tainted) {
                    return true;
                }
                if args.iter().any(|arg| self.is_tainted_expr(arg, tainted)) {
                    return true;
                }

                self.classify_taint_source(expr).is_some()
            }
            AstNode::Attribute { object, .. } => self.is_tainted_expr(object, tainted),
            AstNode::Subscript { value, slice, .. } => {
                self.is_tainted_expr(value, tainted) || self.is_tainted_expr(slice, tainted)
            }
            _ => self.classify_taint_source(expr).is_some(),
        }
    }

    /// Find violations where tainted data reaches a sink
    fn find_violations(
        &self, taint_in: &FxHashMap<BlockId, FxHashSet<String>>, _taint_out: &FxHashMap<BlockId, FxHashSet<String>>,
        sources: &[TaintSource], _sinks: &[TaintSink],
    ) -> Vec<TaintViolation> {
        let mut violations = Vec::new();

        for (block_id, block) in &self.cfg.blocks {
            let mut current_taint = taint_in.get(block_id).cloned().unwrap_or_default();

            for &stmt_idx in &block.statements {
                if stmt_idx < self.all_statements.len() {
                    let stmt = self.all_statements[stmt_idx];

                    match self.check_taint_sink(stmt) {
                        Some(sink) => {
                            if self.sink_receives_taint(stmt, &current_taint) {
                                for tainted_var in &current_taint {
                                    match sources.iter().find(|s| &s.var_name == tainted_var) {
                                        Some(source) => violations.push(TaintViolation {
                                            source: source.clone(),
                                            sink: sink.clone(),
                                            flow_path: vec![tainted_var.clone()],
                                        }),
                                        None => (),
                                    }
                                }
                            }
                        }
                        None => (),
                    }

                    self.propagate_taint(stmt, &mut current_taint);
                }
            }
        }

        violations
    }

    /// Check if a sink receives tainted data
    fn sink_receives_taint(&self, stmt: &AstNode, tainted: &FxHashSet<String>) -> bool {
        match stmt {
            AstNode::Call { args, keywords, .. } => {
                args.iter().any(|arg| self.is_tainted_expr(arg, tainted))
                    || keywords.iter().any(|(_, val)| self.is_tainted_expr(val, tainted))
            }
            _ => false,
        }
    }

    fn extract_target_name(&self, target: &AstNode) -> Option<String> {
        match target {
            AstNode::Identifier { name, .. } => Some(name.clone()),
            _ => None,
        }
    }

    fn extract_function_name(&self, func: &AstNode) -> Option<String> {
        match func {
            AstNode::Identifier { name, .. } => Some(name.clone()),
            AstNode::Attribute { attribute, .. } => Some(attribute.clone()),
            _ => None,
        }
    }

    fn extract_identifier_name(&self, node: &AstNode) -> Option<String> {
        match node {
            AstNode::Identifier { name, .. } => Some(name.clone()),
            _ => None,
        }
    }
}

/// Cross-module taint analyzer
///
/// Propagates taint information across module boundaries using the workspace call graph.
pub struct CrossModuleTaintAnalyzer<'a> {
    call_graph: &'a CallGraph,
    module_results: FxHashMap<Url, TaintAnalysisResult>,
    max_iterations: usize,
}

impl<'a> CrossModuleTaintAnalyzer<'a> {
    pub fn new(call_graph: &'a CallGraph) -> Self {
        Self { call_graph, module_results: FxHashMap::default(), max_iterations: 10 }
    }

    /// Add taint analysis result for a module
    pub fn add_module_result(&mut self, uri: Url, result: TaintAnalysisResult) {
        self.module_results.insert(uri, result);
    }

    /// Perform cross-module taint propagation
    pub fn propagate_taints(&mut self) -> CrossModuleTaintResult {
        let mut iteration = 0;
        let mut changed = true;
        let mut cross_module_violations = Vec::new();

        while changed && iteration < self.max_iterations {
            changed = false;
            iteration += 1;

            for (caller_fn, call_sites) in self.call_graph.all_call_sites() {
                for call_site in call_sites {
                    if let Some(callee_fn) = &call_site.receiver {
                        if caller_fn.uri != callee_fn.uri {
                            if let Some(violation) = self.check_cross_module_taint(caller_fn, callee_fn) {
                                cross_module_violations.push(violation);
                                changed = true;
                            }
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }

        CrossModuleTaintResult { violations: cross_module_violations, iterations: iteration, converged: !changed }
    }

    /// Check for taint flow between two modules
    fn check_cross_module_taint(
        &self, caller_fn: &FunctionId, callee_fn: &FunctionId,
    ) -> Option<CrossModuleTaintViolation> {
        let caller_result = self.module_results.get(&caller_fn.uri)?;
        let callee_result = self.module_results.get(&callee_fn.uri)?;

        if caller_result.sources.is_empty() || callee_result.sinks.is_empty() {
            return None;
        }

        Some(CrossModuleTaintViolation {
            caller: caller_fn.clone(),
            callee: callee_fn.clone(),
            source_module: caller_fn.uri.clone(),
            sink_module: callee_fn.uri.clone(),
        })
    }
}

/// Result of cross-module taint analysis
#[derive(Debug, Clone)]
pub struct CrossModuleTaintResult {
    pub violations: Vec<CrossModuleTaintViolation>,
    pub iterations: usize,
    pub converged: bool,
}

/// A taint violation spanning multiple modules
#[derive(Debug, Clone)]
pub struct CrossModuleTaintViolation {
    pub caller: FunctionId,
    pub callee: FunctionId,
    pub source_module: Url,
    pub sink_module: Url,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfg::CfgBuilder;
    use beacon_parser::{NameResolver, ScopeId, ScopeKind};

    fn find_function_scope(symbol_table: &SymbolTable) -> ScopeId {
        symbol_table
            .scopes
            .values()
            .find(|scope| scope.kind == ScopeKind::Function)
            .map(|scope| scope.id)
            .expect("No function scope found")
    }

    #[test]
    fn test_detect_user_input_source() {
        let source = "def foo():\n    x = input()".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 2,
                    col: 5,
                    end_line: 2,
                    end_col: 6,
                }),
                value: Box::new(AstNode::Call {
                    function: Box::new(AstNode::Identifier {
                        name: "input".to_string(),
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 14,
                    }),
                    args: vec![],
                    keywords: vec![],
                    line: 2,
                    col: 9,
                    end_line: 2,
                    end_col: 16,
                }),
                line: 2,
                col: 5,
                end_col: 16,
                end_line: 2,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 2,
            end_col: 16,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = IntraModuleTaintAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.analyze();

            assert_eq!(result.sources.len(), 1);
            assert_eq!(result.sources[0].var_name, "x");
            assert_eq!(result.sources[0].kind, TaintSourceKind::UserInput);
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_detect_eval_sink() {
        let source = "def foo():\n    eval(x)".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "eval".to_string(),
                    line: 2,
                    col: 5,
                    end_line: 2,
                    end_col: 9,
                }),
                args: vec![AstNode::Identifier { name: "x".to_string(), line: 2, col: 10, end_line: 2, end_col: 11 }],
                keywords: vec![],
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 12,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 2,
            end_col: 12,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = IntraModuleTaintAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.analyze();

            assert_eq!(result.sinks.len(), 1);
            assert_eq!(result.sinks[0].kind, TaintSinkKind::CodeExecution);
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_taint_propagation_through_assignment() {
        let source = "def foo():\n    x = input()\n    y = x".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 6,
                    }),
                    value: Box::new(AstNode::Call {
                        function: Box::new(AstNode::Identifier {
                            name: "input".to_string(),
                            line: 2,
                            col: 9,
                            end_line: 2,
                            end_col: 14,
                        }),
                        args: vec![],
                        keywords: vec![],
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 16,
                    }),
                    line: 2,
                    col: 5,
                    end_col: 16,
                    end_line: 2,
                },
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "y".to_string(),
                        line: 3,
                        col: 5,
                        end_line: 3,
                        end_col: 6,
                    }),
                    value: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 3,
                        col: 9,
                        end_line: 3,
                        end_col: 10,
                    }),
                    line: 3,
                    col: 5,
                    end_col: 10,
                    end_line: 3,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 3,
            end_col: 10,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = IntraModuleTaintAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);

            let sources = analyzer.identify_sources();
            assert_eq!(sources.len(), 1);
            assert_eq!(sources[0].var_name, "x");

            let mut tainted = FxHashSet::default();
            tainted.insert("x".to_string());

            if let AstNode::Assignment { value, .. } = &body[1] {
                assert!(analyzer.is_tainted_expr(value, &tainted));
            } else {
                panic!("Expected Assignment");
            }
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_detect_violation_input_to_eval() {
        let source = "def foo():\n    x = input()\n    eval(x)".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 6,
                    }),
                    value: Box::new(AstNode::Call {
                        function: Box::new(AstNode::Identifier {
                            name: "input".to_string(),
                            line: 2,
                            col: 9,
                            end_line: 2,
                            end_col: 14,
                        }),
                        args: vec![],
                        keywords: vec![],
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 16,
                    }),
                    line: 2,
                    col: 5,
                    end_col: 16,
                    end_line: 2,
                },
                AstNode::Call {
                    function: Box::new(AstNode::Identifier {
                        name: "eval".to_string(),
                        line: 3,
                        col: 5,
                        end_line: 3,
                        end_col: 9,
                    }),
                    args: vec![AstNode::Identifier {
                        name: "x".to_string(),
                        line: 3,
                        col: 10,
                        end_line: 3,
                        end_col: 11,
                    }],
                    keywords: vec![],
                    line: 3,
                    col: 5,
                    end_line: 3,
                    end_col: 12,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 3,
            end_col: 12,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = IntraModuleTaintAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.analyze();

            assert_eq!(result.violations.len(), 1);
            assert_eq!(result.violations[0].source.var_name, "x");
            assert_eq!(result.violations[0].sink.kind, TaintSinkKind::CodeExecution);
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_taint_removed_after_safe_assignment() {
        let source = "def foo():\n    x = input()\n    x = 5\n    eval(x)".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 6,
                    }),
                    value: Box::new(AstNode::Call {
                        function: Box::new(AstNode::Identifier {
                            name: "input".to_string(),
                            line: 2,
                            col: 9,
                            end_line: 2,
                            end_col: 14,
                        }),
                        args: vec![],
                        keywords: vec![],
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 16,
                    }),
                    line: 2,
                    col: 5,
                    end_col: 16,
                    end_line: 2,
                },
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 3,
                        col: 5,
                        end_line: 3,
                        end_col: 6,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(5),
                        line: 3,
                        col: 9,
                        end_line: 3,
                        end_col: 10,
                    }),
                    line: 3,
                    col: 5,
                    end_col: 10,
                    end_line: 3,
                },
                AstNode::Call {
                    function: Box::new(AstNode::Identifier {
                        name: "eval".to_string(),
                        line: 4,
                        col: 5,
                        end_line: 4,
                        end_col: 9,
                    }),
                    args: vec![AstNode::Identifier {
                        name: "x".to_string(),
                        line: 4,
                        col: 10,
                        end_line: 4,
                        end_col: 11,
                    }],
                    keywords: vec![],
                    line: 4,
                    col: 5,
                    end_line: 4,
                    end_col: 12,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 4,
            end_col: 12,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = IntraModuleTaintAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.analyze();

            assert_eq!(
                result.violations.len(),
                0,
                "Should have no violations after safe reassignment"
            );
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_taint_propagation_through_binary_op() {
        let source = "def foo():\n    x = input()\n    y = x + 'suffix'".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 6,
                    }),
                    value: Box::new(AstNode::Call {
                        function: Box::new(AstNode::Identifier {
                            name: "input".to_string(),
                            line: 2,
                            col: 9,
                            end_line: 2,
                            end_col: 14,
                        }),
                        args: vec![],
                        keywords: vec![],
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 16,
                    }),
                    line: 2,
                    col: 5,
                    end_col: 16,
                    end_line: 2,
                },
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "y".to_string(),
                        line: 3,
                        col: 5,
                        end_line: 3,
                        end_col: 6,
                    }),
                    value: Box::new(AstNode::BinaryOp {
                        left: Box::new(AstNode::Identifier {
                            name: "x".to_string(),
                            line: 3,
                            col: 9,
                            end_line: 3,
                            end_col: 10,
                        }),
                        op: beacon_parser::BinaryOperator::Add,
                        right: Box::new(AstNode::Literal {
                            value: beacon_parser::LiteralValue::String {
                                value: "suffix".to_string(),
                                prefix: String::new(),
                            },
                            line: 3,
                            col: 13,
                            end_line: 3,
                            end_col: 21,
                        }),
                        line: 3,
                        col: 9,
                        end_line: 3,
                        end_col: 21,
                    }),
                    line: 3,
                    col: 5,
                    end_col: 21,
                    end_line: 3,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 3,
            end_col: 21,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = IntraModuleTaintAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);

            let mut tainted = FxHashSet::default();
            tainted.insert("x".to_string());

            if let AstNode::Assignment { value, .. } = &body[1] {
                assert!(analyzer.is_tainted_expr(value, &tainted));
            } else {
                panic!("Expected Assignment");
            }
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_detect_command_execution_sink() {
        let source = "def foo():\n    os.system(cmd)".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::Call {
                function: Box::new(AstNode::Attribute {
                    object: Box::new(AstNode::Identifier {
                        name: "os".to_string(),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 7,
                    }),
                    attribute: "system".to_string(),
                    line: 2,
                    col: 5,
                    end_line: 2,
                    end_col: 14,
                }),
                args: vec![AstNode::Identifier { name: "cmd".to_string(), line: 2, col: 15, end_line: 2, end_col: 18 }],
                keywords: vec![],
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 19,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 2,
            end_col: 19,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = IntraModuleTaintAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.analyze();

            assert_eq!(result.sinks.len(), 1);
            assert_eq!(result.sinks[0].kind, TaintSinkKind::CommandExecution);
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_cross_module_analyzer_creation() {
        let call_graph = CallGraph::new();
        let analyzer = CrossModuleTaintAnalyzer::new(&call_graph);
        assert_eq!(analyzer.module_results.len(), 0);
    }

    #[test]
    fn test_cross_module_add_module_result() {
        let call_graph = CallGraph::new();
        let mut analyzer = CrossModuleTaintAnalyzer::new(&call_graph);

        let uri = Url::parse("file:///test.py").unwrap();
        let result = TaintAnalysisResult { violations: vec![], sources: vec![], sinks: vec![] };

        analyzer.add_module_result(uri.clone(), result);
        assert!(analyzer.module_results.contains_key(&uri));
    }

    #[test]
    fn test_cross_module_propagation_no_violations() {
        let call_graph = CallGraph::new();
        let mut analyzer = CrossModuleTaintAnalyzer::new(&call_graph);

        let result = analyzer.propagate_taints();
        assert_eq!(result.violations.len(), 0);
        assert!(result.converged);
    }

    #[test]
    fn test_detect_file_read_source() {
        let source = "def foo():\n    x = open('file.txt').read()".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 2,
                    col: 5,
                    end_line: 2,
                    end_col: 6,
                }),
                value: Box::new(AstNode::Call {
                    function: Box::new(AstNode::Attribute {
                        object: Box::new(AstNode::Call {
                            function: Box::new(AstNode::Identifier {
                                name: "open".to_string(),
                                line: 2,
                                col: 9,
                                end_line: 2,
                                end_col: 13,
                            }),
                            args: vec![AstNode::Literal {
                                value: beacon_parser::LiteralValue::String {
                                    value: "file.txt".to_string(),
                                    prefix: String::new(),
                                },
                                line: 2,
                                col: 14,
                                end_line: 2,
                                end_col: 24,
                            }],
                            keywords: vec![],
                            line: 2,
                            col: 9,
                            end_line: 2,
                            end_col: 25,
                        }),
                        attribute: "read".to_string(),
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 30,
                    }),
                    args: vec![],
                    keywords: vec![],
                    line: 2,
                    col: 9,
                    end_line: 2,
                    end_col: 32,
                }),
                line: 2,
                col: 5,
                end_col: 32,
                end_line: 2,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 2,
            end_col: 32,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = IntraModuleTaintAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);

            if let AstNode::Assignment { value, .. } = &body[0] {
                let kind = analyzer.classify_taint_source(value);
                assert_eq!(kind, Some(TaintSourceKind::FileRead));
            } else {
                panic!("Expected Assignment");
            }
        } else {
            panic!("Expected FunctionDef");
        }
    }
}
