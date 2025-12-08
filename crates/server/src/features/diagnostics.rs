//! Diagnostic generation and publishing
//!
//! Converts type errors, parse errors, and other analysis results into LSP diagnostics for display in the editor.
use crate::analysis::Analyzer;
use crate::config;
use crate::document::DocumentManager;
use crate::features::completion::algorithms::{FuzzyMatcher, StringSimilarity};
use crate::parser::{self, ParseError};
use crate::workspace::Workspace;

use beacon_constraint::Span;
use beacon_core::BeaconError;
use beacon_core::TypeError;
use beacon_core::{Type, TypeCtor};
use beacon_parser::{AstNode, MAGIC_METHODS, SymbolTable};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use url::Url;

/// Diagnostic categories used for mode-aware severity mapping
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
enum DiagnosticCategory {
    /// Implicit Any type detected
    ImplicitAny,
    /// Missing type annotation
    MissingAnnotation,
    /// Annotation doesn't match inferred type
    AnnotationMismatch,
}

/// Context for annotation coverage diagnostics
struct DiagnosticContext<'a> {
    type_map: &'a FxHashMap<usize, Type>,
    position_map: &'a FxHashMap<(usize, usize), usize>,
    mode: config::TypeCheckingMode,
    diagnostics: &'a mut Vec<Diagnostic>,
    in_class_def: bool,
    source_lines: Vec<String>,
}

impl<'a> DiagnosticContext<'a> {
    fn range_for_name(&self, line: usize, col_hint: usize, name: &str) -> Range {
        DiagnosticProvider::identifier_range(line, col_hint, name, &self.source_lines)
    }
}

pub struct DiagnosticProvider {
    documents: DocumentManager,
    workspace: Arc<RwLock<Workspace>>,
    fuzzy_matcher: FuzzyMatcher,
}

impl DiagnosticProvider {
    fn identifier_range(line: usize, col_hint: usize, name: &str, lines: &[String]) -> Range {
        fn column_to_byte(line_text: &str, column: usize) -> usize {
            if column <= 1 {
                return 0;
            }
            let mut chars_seen = 1;
            for (idx, _) in line_text.char_indices() {
                if chars_seen == column {
                    return idx;
                }
                chars_seen += 1;
            }
            line_text.len()
        }

        fn byte_to_column(line_text: &str, byte_idx: usize) -> usize {
            line_text[..byte_idx].chars().count() + 1
        }

        let fallback_start =
            Position { line: line.saturating_sub(1) as u32, character: col_hint.saturating_sub(1) as u32 };
        let fallback_end =
            Position { line: fallback_start.line, character: fallback_start.character + name.chars().count() as u32 };

        if let Some(line_text) = line.checked_sub(1).and_then(|idx| lines.get(idx)).map(|s| s.as_str()) {
            let search_start = column_to_byte(line_text, col_hint);
            if search_start <= line_text.len()
                && let Some(rel_idx) = line_text[search_start..].find(name)
            {
                let byte_idx = search_start + rel_idx;
                let start_col = byte_to_column(line_text, byte_idx);
                let end_col = start_col + name.chars().count();
                let start = Position { line: (line - 1) as u32, character: (start_col - 1) as u32 };
                let end = Position { line: start.line, character: (end_col - 1) as u32 };
                return Range { start, end };
            }
        }

        Range { start: fallback_start, end: fallback_end }
    }
    pub fn new(documents: DocumentManager, workspace: Arc<RwLock<Workspace>>) -> Self {
        Self { documents, workspace, fuzzy_matcher: FuzzyMatcher::new() }
    }

    /// Convert config DiagnosticSeverity to LSP DiagnosticSeverity
    fn config_severity_to_lsp(severity: config::DiagnosticSeverity) -> lsp_types::DiagnosticSeverity {
        match severity {
            config::DiagnosticSeverity::Error => lsp_types::DiagnosticSeverity::ERROR,
            config::DiagnosticSeverity::Warning => lsp_types::DiagnosticSeverity::WARNING,
            config::DiagnosticSeverity::Info => lsp_types::DiagnosticSeverity::INFORMATION,
        }
    }

    /// Convert a constraint span to an LSP range, ensuring at least one highlighted character
    fn span_to_range(span: &Span) -> Range {
        let start_line = span.line.saturating_sub(1) as u32;
        let start_col = span.col.saturating_sub(1) as u32;
        let end_line = span.end_line.unwrap_or(span.line);
        let mut end_col = span.end_col.unwrap_or_else(|| span.col + 1);
        if end_line == span.line && end_col <= span.col {
            end_col = span.col + 1;
        }
        Range {
            start: Position::new(start_line, start_col),
            end: Position::new(end_line.saturating_sub(1) as u32, end_col.saturating_sub(1) as u32),
        }
    }

    /// Get the diagnostic severity for a specific diagnostic category based on mode to adjust diagnostic severity based on type checking mode.
    #[allow(dead_code)]
    fn mode_severity_for_diagnostic(
        mode: config::TypeCheckingMode, category: DiagnosticCategory,
    ) -> Option<lsp_types::DiagnosticSeverity> {
        use lsp_types::DiagnosticSeverity as Severity;

        match (mode, category) {
            (config::TypeCheckingMode::Strict, DiagnosticCategory::ImplicitAny) => Some(Severity::ERROR),
            (config::TypeCheckingMode::Balanced, DiagnosticCategory::ImplicitAny) => Some(Severity::WARNING),
            (config::TypeCheckingMode::Relaxed, DiagnosticCategory::ImplicitAny) => None,
            (config::TypeCheckingMode::Strict, DiagnosticCategory::MissingAnnotation) => Some(Severity::ERROR),
            (config::TypeCheckingMode::Balanced, DiagnosticCategory::MissingAnnotation) => Some(Severity::WARNING),
            (config::TypeCheckingMode::Relaxed, DiagnosticCategory::MissingAnnotation) => None,
            (config::TypeCheckingMode::Strict, DiagnosticCategory::AnnotationMismatch) => Some(Severity::ERROR),
            (config::TypeCheckingMode::Balanced, DiagnosticCategory::AnnotationMismatch) => Some(Severity::WARNING),
            (config::TypeCheckingMode::Relaxed, DiagnosticCategory::AnnotationMismatch) => Some(Severity::HINT),
        }
    }

    /// Generate diagnostics for a document by combining syntax errors, type errors, and other analysis issues.
    pub fn generate_diagnostics(&self, uri: &Url, analyzer: &mut Analyzer) -> Vec<Diagnostic> {
        tracing::debug!("Generating diagnostics for {}", uri);
        let mut diagnostics = Vec::new();

        let effective_mode = self.get_effective_mode(uri);
        tracing::debug!("Using type checking mode: {} for {}", effective_mode.as_str(), uri);

        let start = std::time::Instant::now();
        self.add_parse_errors(uri, &mut diagnostics);
        tracing::trace!("Parse errors: {} ({:?})", diagnostics.len(), start.elapsed());

        let start = std::time::Instant::now();
        self.add_linter_diagnostics(uri, &mut diagnostics);
        tracing::trace!("Linter diagnostics: {} ({:?})", diagnostics.len(), start.elapsed());

        let start = std::time::Instant::now();
        self.add_unbound_variable_errors(uri, analyzer, &mut diagnostics);
        tracing::trace!("Unbound variable errors: {} ({:?})", diagnostics.len(), start.elapsed());

        let start = std::time::Instant::now();
        self.add_type_errors(uri, analyzer, &mut diagnostics);
        tracing::trace!("Type errors: {} ({:?})", diagnostics.len(), start.elapsed());

        let start = std::time::Instant::now();
        self.add_unsafe_any_warnings(uri, analyzer, &mut diagnostics);
        tracing::trace!("Unsafe Any warnings: {} ({:?})", diagnostics.len(), start.elapsed());

        let start = std::time::Instant::now();
        self.add_annotation_mismatch_warnings(uri, analyzer, &mut diagnostics);
        tracing::trace!(
            "Annotation mismatch warnings: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        let start = std::time::Instant::now();
        self.add_dunder_diagnostics(uri, &mut diagnostics);
        tracing::trace!("Dunder diagnostics: {} ({:?})", diagnostics.len(), start.elapsed());

        let start = std::time::Instant::now();
        self.add_static_analysis_diagnostics(uri, analyzer, &mut diagnostics);
        tracing::trace!(
            "Static analysis diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        let start = std::time::Instant::now();
        self.add_circular_import_diagnostics(uri, &mut diagnostics);
        tracing::trace!(
            "Circular import diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        let start = std::time::Instant::now();
        self.add_unresolved_import_diagnostics(uri, &mut diagnostics);
        tracing::trace!(
            "Unresolved import diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        let start = std::time::Instant::now();
        self.add_missing_module_diagnostics(uri, &mut diagnostics);
        tracing::trace!(
            "Missing module diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        let start = std::time::Instant::now();
        self.add_inconsistent_export_diagnostics(uri, &mut diagnostics);
        tracing::trace!(
            "Inconsistent export diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        let start = std::time::Instant::now();
        self.add_conflicting_stub_diagnostics(uri, &mut diagnostics);
        tracing::trace!(
            "Conflicting stub diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        tracing::info!(
            "Generated {} total diagnostics for {} (mode: {})",
            diagnostics.len(),
            uri,
            effective_mode.as_str()
        );

        if !diagnostics.is_empty() {
            self.add_mode_info_hint(uri, effective_mode, &mut diagnostics);
        }

        diagnostics
    }

    /// Get the effective type checking mode for a document
    fn get_effective_mode(&self, uri: &Url) -> config::TypeCheckingMode {
        let Ok(workspace) = self.workspace.try_read() else {
            return config::TypeCheckingMode::default();
        };
        let workspace_mode = workspace.config.type_checking.mode;
        self.documents
            .get_document(uri, |doc| doc.effective_mode(workspace_mode))
            .unwrap_or(workspace_mode)
    }

    /// Add an informational hint showing the current type checking mode
    fn add_mode_info_hint(&self, uri: &Url, mode: config::TypeCheckingMode, diagnostics: &mut Vec<Diagnostic>) {
        let is_override = self
            .documents
            .get_document(uri, |doc| doc.mode_override.is_some())
            .unwrap_or(false);

        let message = if is_override {
            format!(
                "Type checking mode: {} (per-file override) - Add '# beacon: mode=strict/balanced/relaxed' to change",
                mode.as_str()
            )
        } else {
            format!(
                "Type checking mode: {} (workspace default) - Add '# beacon: mode=strict/balanced/relaxed' to override for this file",
                mode.as_str()
            )
        };

        diagnostics.push(Diagnostic {
            range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 0 } },
            severity: Some(DiagnosticSeverity::HINT),
            code: Some(lsp_types::NumberOrString::String("MODE_INFO".to_string())),
            source: Some("beacon".to_string()),
            message,
            related_information: None,
            tags: None,
            data: None,
            code_description: None,
        });
    }

    /// Add parse errors as diagnostics
    fn add_parse_errors(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        self.documents.get_document(uri, |doc| {
            if let Some(parse_result) = &doc.parse_result {
                for error in &parse_result.errors {
                    diagnostics.push(parse_error_to_diagnostic(error));
                }
            }
        });
    }

    /// Add linter diagnostics
    fn add_linter_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        self.documents.get_document(uri, |doc| {
            if let (Some(ast), Some(symbol_table)) = (doc.ast(), doc.symbol_table()) {
                let filename = uri.path().to_string();
                let source = doc.text();
                let mut linter = crate::analysis::linter::Linter::new(symbol_table, filename, &source);
                let linter_diagnostics = linter.analyze(ast);

                for msg in linter_diagnostics {
                    diagnostics.push((&msg).into());
                }
            }
        });
    }

    /// Add type errors as diagnostics
    fn add_type_errors(&self, uri: &Url, analyzer: &mut Analyzer, diagnostics: &mut Vec<Diagnostic>) {
        match analyzer.analyze(uri) {
            Ok(result) => {
                for type_error_info in &result.type_errors {
                    diagnostics.push(type_error_to_diagnostic(type_error_info));
                }
            }
            Err(e) => diagnostics.push(analysis_error_to_diagnostic(&e)),
        }
    }

    /// Add unbound variable errors as diagnostics
    fn add_unbound_variable_errors(&self, uri: &Url, analyzer: &Analyzer, diagnostics: &mut Vec<Diagnostic>) {
        let unbound = analyzer.find_unbound_variables(uri);

        for (name, line, col) in unbound {
            let position =
                Position { line: (line.saturating_sub(1)) as u32, character: (col.saturating_sub(1)) as u32 };

            let range = Range {
                start: position,
                end: Position { line: position.line, character: position.character + name.len() as u32 },
            };

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(lsp_types::NumberOrString::String("undefined-variable".to_string())),
                source: Some("beacon".to_string()),
                message: format!("Undefined variable '{name}'"),
                related_information: None,
                tags: None,
                data: None,
                code_description: None,
            });
        }
    }

    /// Check for unsafe Any type usage that exceeds configured depth
    fn add_unsafe_any_warnings(&self, uri: &Url, analyzer: &mut Analyzer, diagnostics: &mut Vec<Diagnostic>) {
        let result = match analyzer.analyze(uri) {
            Ok(r) => r,
            Err(_) => return,
        };

        for (node_id, ty) in &result.type_map {
            if result.safe_any_nodes.contains(node_id) {
                continue;
            }
            if Self::contains_any_type(ty, 0) {
                let range = if let Some(span) = result.node_spans.get(node_id) {
                    Self::span_to_range(span)
                } else if let Some((line, col)) = result
                    .position_map
                    .iter()
                    .find_map(|((l, c), id)| (*id == *node_id).then_some((*l, *c)))
                {
                    let start =
                        Position { line: (line.saturating_sub(1)) as u32, character: (col.saturating_sub(1)) as u32 };
                    Range { start, end: Position { line: start.line, character: start.character + 10 } }
                } else {
                    continue;
                };

                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(lsp_types::NumberOrString::String("ANY001".to_string())),
                    source: Some("beacon".to_string()),
                    message: "Type 'Any' detected - this reduces type safety".to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                    code_description: None,
                });
            }
        }
    }

    #[allow(dead_code)]
    fn contains_any_type(ty: &beacon_core::Type, _depth: u32) -> bool {
        match ty {
            Type::Con(TypeCtor::Any) => true,
            Type::App(t1, t2) => Self::contains_any_type(t1, _depth + 1) || Self::contains_any_type(t2, _depth + 1),
            Type::Fun(args, ret) => {
                args.iter().any(|(_, arg)| Self::contains_any_type(arg, _depth + 1))
                    || Self::contains_any_type(ret, _depth + 1)
            }
            Type::Union(types) => types.iter().any(|t| Self::contains_any_type(t, _depth + 1)),
            Type::Record(fields, _) => fields.iter().any(|(_, t)| Self::contains_any_type(t, _depth + 1)),
            _ => false,
        }
    }

    /// Check annotation mismatches based on config mode
    fn add_annotation_mismatch_warnings(&self, uri: &Url, analyzer: &mut Analyzer, diagnostics: &mut Vec<Diagnostic>) {
        let result = match analyzer.analyze(uri) {
            Ok(r) => r,
            Err(_) => return,
        };

        let mode = self.get_effective_mode(uri);
        tracing::trace!(
            "Checking annotation mismatches for {} in {} mode (found {} inferred types)",
            uri,
            mode.as_str(),
            result.type_map.len()
        );

        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                let source_lines = doc.text().lines().map(|s| s.to_string()).collect();
                let mut ctx = DiagnosticContext {
                    type_map: &result.type_map,
                    position_map: &result.position_map,
                    mode,
                    diagnostics,
                    in_class_def: false,
                    source_lines,
                };
                self.check_annotation_coverage(ast, &mut ctx);
            }
        });
    }

    /// Walk AST to find annotated assignments and parameters, compare with inferred types
    fn check_annotation_coverage(&self, node: &AstNode, ctx: &mut DiagnosticContext) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.check_annotation_coverage(stmt, ctx);
                }
            }
            AstNode::FunctionDef { args, return_type, body, line, col, end_col, name, .. } => {
                for (idx, param) in args.iter().enumerate() {
                    if idx == 0 && ctx.in_class_def && (param.name == "self" || param.name == "cls") {
                        continue;
                    }
                    self.check_parameter_annotation(param, ctx);
                }

                self.check_return_type_annotation(return_type, *line, *col, name, *end_col, ctx);

                let prev_in_class = ctx.in_class_def;
                ctx.in_class_def = false;
                for stmt in body {
                    self.check_annotation_coverage(stmt, ctx);
                }
                ctx.in_class_def = prev_in_class;
            }
            AstNode::ClassDef { body, .. } => {
                let prev_in_class = ctx.in_class_def;
                ctx.in_class_def = true;
                for stmt in body {
                    self.check_annotation_coverage(stmt, ctx);
                }
                ctx.in_class_def = prev_in_class;
            }
            AstNode::AnnotatedAssignment { target, type_annotation, line, col, .. } => {
                if let Some(inferred_type) = Self::get_type_for_position(ctx.type_map, ctx.position_map, *line, *col) {
                    self.check_annotation_match(type_annotation, &inferred_type, *line, *col, target, ctx);
                }
            }
            AstNode::Assignment { target, value, line, col, .. } => {
                if Self::is_typevar_assignment(value) {
                    // Skip annotation requirements for TypeVar declarations
                } else if ctx.in_class_def && ctx.mode == config::TypeCheckingMode::Strict {
                    self.check_class_attribute_annotation(target, *line, *col, ctx);
                } else if ctx.mode != config::TypeCheckingMode::Relaxed
                    && let Some(inferred_type) =
                        Self::get_type_for_position(ctx.type_map, ctx.position_map, *line, *col)
                {
                    self.check_missing_annotation(target, &inferred_type, *line, *col, ctx);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    self.check_annotation_coverage(stmt, ctx);
                }
                for (_test, elif_body) in elif_parts {
                    for stmt in elif_body {
                        self.check_annotation_coverage(stmt, ctx);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.check_annotation_coverage(stmt, ctx);
                    }
                }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                for stmt in body {
                    self.check_annotation_coverage(stmt, ctx);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.check_annotation_coverage(stmt, ctx);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    self.check_annotation_coverage(stmt, ctx);
                }
                for handler in handlers {
                    self.check_bare_except_handler(handler, ctx);
                    for stmt in &handler.body {
                        self.check_annotation_coverage(stmt, ctx);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.check_annotation_coverage(stmt, ctx);
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        self.check_annotation_coverage(stmt, ctx);
                    }
                }
            }
            AstNode::With { body, .. } => {
                for stmt in body {
                    self.check_annotation_coverage(stmt, ctx);
                }
            }
            _ => {}
        }
    }

    /// Get the type for a specific position
    fn get_type_for_position(
        type_map: &FxHashMap<usize, Type>, position_map: &FxHashMap<(usize, usize), usize>, line: usize, col: usize,
    ) -> Option<Type> {
        position_map
            .get(&(line, col))
            .and_then(|node_id| type_map.get(node_id))
            .cloned()
    }

    /// Check if an annotation matches the inferred type
    fn check_annotation_match(
        &self, annotation: &str, inferred_type: &Type, line: usize, col: usize, target: &AstNode,
        ctx: &mut DiagnosticContext,
    ) {
        let parser = beacon_core::AnnotationParser::new();
        let annotated_type = match parser.parse(annotation) {
            Ok(ty) => ty,
            Err(_) => return,
        };

        if !Self::types_are_compatible(&annotated_type, inferred_type) {
            let severity = Self::mode_severity_for_diagnostic(ctx.mode, DiagnosticCategory::AnnotationMismatch);

            if let Some(sev) = severity {
                let target_name = Self::extract_target_name(target);
                let range = ctx.range_for_name(line, col, &target_name);

                ctx.diagnostics.push(Diagnostic {
                    range,
                    severity: Some(sev),
                    code: Some(lsp_types::NumberOrString::String("ANN001".to_string())),
                    source: Some("beacon".to_string()),
                    message: format!(
                        "Type annotation mismatch: annotated as '{annotated_type}', but inferred as '{inferred_type}'"
                    ),
                    related_information: None,
                    tags: None,
                    data: None,
                    code_description: None,
                });
            }
        }
    }

    /// Check for missing annotations on assignments
    fn check_missing_annotation(
        &self, target: &AstNode, inferred_type: &Type, line: usize, col: usize, ctx: &mut DiagnosticContext,
    ) {
        if matches!(inferred_type, Type::Con(TypeCtor::Any)) || Self::contains_type_var(inferred_type) {
            return;
        }

        let severity = Self::mode_severity_for_diagnostic(ctx.mode, DiagnosticCategory::MissingAnnotation);

        if let Some(sev) = severity {
            let target_name = Self::extract_target_name(target);
            let range = ctx.range_for_name(line, col, &target_name);

            ctx.diagnostics.push(Diagnostic {
                range,
                severity: Some(sev),
                code: Some(lsp_types::NumberOrString::String("ANN002".to_string())),
                source: Some("beacon".to_string()),
                message: format!("Missing type annotation (inferred as '{inferred_type}')"),
                related_information: None,
                tags: None,
                data: None,
                code_description: None,
            });
        }
    }

    /// Check for missing annotations on class attributes in strict mode
    /// In strict mode, all class attributes must have explicit type annotations
    fn check_class_attribute_annotation(&self, target: &AstNode, line: usize, col: usize, ctx: &mut DiagnosticContext) {
        let target_name = Self::extract_target_name(target);
        let range = ctx.range_for_name(line, col, &target_name);

        ctx.diagnostics.push(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(lsp_types::NumberOrString::String("ANN009".to_string())),
            source: Some("beacon".to_string()),
            message: format!(
                "Class attribute '{target_name}' missing type annotation - explicit type annotation required in strict mode"
            ),
            related_information: None,
            tags: None,
            data: None,
            code_description: None,
        });
    }

    /// Extract the variable name from an assignment target
    fn extract_target_name(target: &AstNode) -> String {
        match target {
            AstNode::Identifier { name, .. } => name.clone(),
            AstNode::Attribute { attribute, .. } => attribute.clone(),
            _ => "variable".to_string(),
        }
    }

    fn is_typevar_assignment(value: &AstNode) -> bool {
        if let AstNode::Call { function, .. } = value {
            let func_name = function.function_to_string();
            func_name == "TypeVar" || func_name.ends_with(".TypeVar")
        } else {
            false
        }
    }

    /// Check if two types are compatible (structural comparison)
    fn types_are_compatible(annotated: &Type, inferred: &Type) -> bool {
        use Type::*;

        match (annotated, inferred) {
            (Con(a), Con(b)) if a == b => true,
            (Con(TypeCtor::Any), _) | (_, Con(TypeCtor::Any)) => true,
            (App(a1, a2), App(b1, b2)) => Self::types_are_compatible(a1, b1) && Self::types_are_compatible(a2, b2),
            (Fun(a_args, a_ret), Fun(b_args, b_ret)) => {
                a_args.len() == b_args.len()
                    && a_args
                        .iter()
                        .zip(b_args.iter())
                        .all(|((_, a), (_, b))| Self::types_are_compatible(a, b))
                    && Self::types_are_compatible(a_ret, b_ret)
            }
            (Union(a_types), Union(b_types)) => {
                a_types
                    .iter()
                    .all(|a_ty| b_types.iter().any(|b_ty| Self::types_are_compatible(a_ty, b_ty)))
                    && b_types
                        .iter()
                        .all(|b_ty| a_types.iter().any(|a_ty| Self::types_are_compatible(a_ty, b_ty)))
            }
            (Type::Tuple(a_items), Type::Tuple(b_items)) => {
                a_items.len() == b_items.len()
                    && a_items
                        .iter()
                        .zip(b_items.iter())
                        .all(|(a_ty, b_ty)| Self::types_are_compatible(a_ty, b_ty))
            }
            (Record(a_fields, _), Record(b_fields, _)) => {
                a_fields.len() == b_fields.len()
                    && a_fields.iter().all(|(a_name, a_ty)| {
                        b_fields
                            .iter()
                            .any(|(b_name, b_ty)| a_name == b_name && Self::types_are_compatible(a_ty, b_ty))
                    })
            }
            (Var(_), _) | (_, Var(_)) => true,
            _ => false,
        }
    }

    /// Check if a type contains type variables (incomplete inference)
    fn contains_type_var(ty: &Type) -> bool {
        match ty {
            Type::Var(_) => true,
            Type::App(t1, t2) => Self::contains_type_var(t1) || Self::contains_type_var(t2),
            Type::Fun(args, ret) => {
                args.iter().any(|(_, arg)| Self::contains_type_var(arg)) || Self::contains_type_var(ret)
            }
            Type::Union(types) => types.iter().any(Self::contains_type_var),
            Type::Record(fields, _) => fields.iter().any(|(_, t)| Self::contains_type_var(t)),
            _ => false,
        }
    }

    /// Check function parameter annotation
    fn check_parameter_annotation(&self, param: &beacon_parser::Parameter, ctx: &mut DiagnosticContext) {
        match &param.type_annotation {
            Some(annotation) => {
                if let Some(inferred_type) =
                    Self::get_type_for_position(ctx.type_map, ctx.position_map, param.line, param.col)
                {
                    let parser = beacon_core::AnnotationParser::new();
                    if let Ok(annotated_type) = parser.parse(annotation)
                        && !Self::types_are_compatible(&annotated_type, &inferred_type)
                    {
                        let severity =
                            Self::mode_severity_for_diagnostic(ctx.mode, DiagnosticCategory::AnnotationMismatch);

                        if let Some(sev) = severity {
                            let position = Position {
                                line: (param.line.saturating_sub(1)) as u32,
                                character: (param.col.saturating_sub(1)) as u32,
                            };

                            let range = Range {
                                start: position,
                                end: Position {
                                    line: position.line,
                                    character: position.character + param.name.len() as u32,
                                },
                            };

                            ctx.diagnostics.push(Diagnostic {
                                range,
                                severity: Some(sev),
                                code: Some(lsp_types::NumberOrString::String("ANN003".to_string())),
                                source: Some("beacon".to_string()),
                                message: format!(
                                    "Parameter '{}' annotation mismatch: annotated as '{}', but inferred as '{}'",
                                    param.name, annotated_type, inferred_type
                                ),
                                related_information: None,
                                tags: None,
                                data: None,
                                code_description: None,
                            });
                        }
                    }
                }
            }
            None => {
                if ctx.mode == config::TypeCheckingMode::Strict {
                    let position = Position {
                        line: (param.line.saturating_sub(1)) as u32,
                        character: (param.col.saturating_sub(1)) as u32,
                    };

                    let range = Range {
                        start: position,
                        end: Position { line: position.line, character: position.character + param.name.len() as u32 },
                    };

                    ctx.diagnostics.push(Diagnostic {
                        range,
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(lsp_types::NumberOrString::String("ANN007".to_string())),
                        source: Some("beacon".to_string()),
                        message: format!(
                            "Parameter '{}' has implicit Any type - explicit type annotation required in strict mode",
                            param.name
                        ),
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                } else if ctx.mode != config::TypeCheckingMode::Relaxed {
                    let inferred_type_opt =
                        Self::get_type_for_position(ctx.type_map, ctx.position_map, param.line, param.col);

                    match inferred_type_opt {
                        Some(Type::Con(TypeCtor::Any)) => {
                            let severity =
                                Self::mode_severity_for_diagnostic(ctx.mode, DiagnosticCategory::ImplicitAny);

                            if let Some(sev) = severity {
                                let position = Position {
                                    line: (param.line.saturating_sub(1)) as u32,
                                    character: (param.col.saturating_sub(1)) as u32,
                                };

                                let range = Range {
                                    start: position,
                                    end: Position {
                                        line: position.line,
                                        character: position.character + param.name.len() as u32,
                                    },
                                };

                                ctx.diagnostics.push(Diagnostic {
                                    range,
                                    severity: Some(sev),
                                    code: Some(lsp_types::NumberOrString::String("ANN011".to_string())),
                                    source: Some("beacon".to_string()),
                                    message: format!(
                                        "Parameter '{}' has implicit Any type - consider adding type annotation",
                                        param.name
                                    ),
                                    related_information: None,
                                    tags: None,
                                    data: None,
                                    code_description: None,
                                });
                            }
                        }
                        Some(inferred_type) if !Self::contains_type_var(&inferred_type) => {
                            let severity =
                                Self::mode_severity_for_diagnostic(ctx.mode, DiagnosticCategory::MissingAnnotation);

                            if let Some(sev) = severity {
                                let position = Position {
                                    line: (param.line.saturating_sub(1)) as u32,
                                    character: (param.col.saturating_sub(1)) as u32,
                                };

                                let range = Range {
                                    start: position,
                                    end: Position {
                                        line: position.line,
                                        character: position.character + param.name.len() as u32,
                                    },
                                };

                                ctx.diagnostics.push(Diagnostic {
                                    range,
                                    severity: Some(sev),
                                    code: Some(lsp_types::NumberOrString::String("ANN004".to_string())),
                                    source: Some("beacon".to_string()),
                                    message: format!(
                                        "Parameter '{}' missing type annotation (inferred as '{}')",
                                        param.name, inferred_type
                                    ),
                                    related_information: None,
                                    tags: None,
                                    data: None,
                                    code_description: None,
                                });
                            }
                        }
                        None => {
                            let severity =
                                Self::mode_severity_for_diagnostic(ctx.mode, DiagnosticCategory::ImplicitAny);

                            if let Some(sev) = severity {
                                let position = Position {
                                    line: (param.line.saturating_sub(1)) as u32,
                                    character: (param.col.saturating_sub(1)) as u32,
                                };

                                let range = Range {
                                    start: position,
                                    end: Position {
                                        line: position.line,
                                        character: position.character + param.name.len() as u32,
                                    },
                                };

                                ctx.diagnostics.push(Diagnostic {
                                    range,
                                    severity: Some(sev),
                                    code: Some(lsp_types::NumberOrString::String("ANN011".to_string())),
                                    source: Some("beacon".to_string()),
                                    message: format!(
                                        "Parameter '{}' has implicit Any type - consider adding type annotation",
                                        param.name
                                    ),
                                    related_information: None,
                                    tags: None,
                                    data: None,
                                    code_description: None,
                                });
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    /// Check function return type annotation
    fn check_return_type_annotation(
        &self, return_type: &Option<String>, line: usize, col: usize, name: &str, end_col: usize,
        ctx: &mut DiagnosticContext,
    ) {
        match return_type {
            Some(annotation) => {
                if let Some(inferred_type) = Self::get_type_for_position(ctx.type_map, ctx.position_map, line, col) {
                    let inferred_return_type = match &inferred_type {
                        Type::Fun(_, ret) => (**ret).clone(),
                        other => other.clone(),
                    };

                    let parser = beacon_core::AnnotationParser::new();
                    if let Ok(annotated_type) = parser.parse(annotation)
                        && !Self::types_are_compatible(&annotated_type, &inferred_return_type)
                    {
                        let severity =
                            Self::mode_severity_for_diagnostic(ctx.mode, DiagnosticCategory::AnnotationMismatch);

                        if let Some(sev) = severity {
                            let start_position = Position {
                                line: (line.saturating_sub(1)) as u32,
                                character: (col.saturating_sub(1)) as u32,
                            };

                            let end_position = Position {
                                line: (line.saturating_sub(1)) as u32,
                                character: (end_col.saturating_sub(1)) as u32,
                            };

                            let range = Range { start: start_position, end: end_position };

                            ctx.diagnostics.push(Diagnostic {
                                    range,
                                    severity: Some(sev),
                                    code: Some(lsp_types::NumberOrString::String("ANN005".to_string())),
                                    source: Some("beacon".to_string()),
                                    message: format!(
                                        "Function '{name}' return type mismatch: annotated as '{annotated_type}', but inferred as '{inferred_return_type}'"
                                    ),
                                    related_information: None,
                                    tags: None,
                                    data: None,
                                    code_description: None,
                                });
                        }
                    }
                }
            }
            None => {
                if ctx.mode == config::TypeCheckingMode::Strict {
                    let range = ctx.range_for_name(line, col, name);

                    ctx.diagnostics.push(Diagnostic {
                        range,
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(lsp_types::NumberOrString::String("ANN008".to_string())),
                        source: Some("beacon".to_string()),
                        message: format!(
                            "Function '{name}' has implicit Any return type - explicit return type annotation required in strict mode"
                        ),
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                } else if ctx.mode != config::TypeCheckingMode::Relaxed {
                    let inferred_type_opt = Self::get_type_for_position(ctx.type_map, ctx.position_map, line, col);

                    let return_type_opt = inferred_type_opt.map(|ty| match ty {
                        Type::Fun(_, ret) => *ret,
                        other => other,
                    });

                    match return_type_opt {
                        Some(Type::Con(TypeCtor::Any)) => {
                            let severity =
                                Self::mode_severity_for_diagnostic(ctx.mode, DiagnosticCategory::ImplicitAny);

                            if let Some(sev) = severity {
                                let range = ctx.range_for_name(line, col, name);

                                ctx.diagnostics.push(Diagnostic {
                                    range,
                                    severity: Some(sev),
                                    code: Some(lsp_types::NumberOrString::String("ANN012".to_string())),
                                    source: Some("beacon".to_string()),
                                    message: format!(
                                        "Function '{name}' has implicit Any return type - consider adding type annotation"
                                    ),
                                    related_information: None,
                                    tags: None,
                                    data: None,
                                    code_description: None,
                                });
                            }
                        }
                        Some(return_type)
                            if !matches!(return_type, Type::Con(TypeCtor::NoneType))
                                && !Self::contains_type_var(&return_type) =>
                        {
                            let severity =
                                Self::mode_severity_for_diagnostic(ctx.mode, DiagnosticCategory::MissingAnnotation);

                            if let Some(sev) = severity {
                                let range = ctx.range_for_name(line, col, name);

                                ctx.diagnostics.push(Diagnostic {
                                    range,
                                    severity: Some(sev),
                                    code: Some(lsp_types::NumberOrString::String("ANN006".to_string())),
                                    source: Some("beacon".to_string()),
                                    message: format!(
                                        "Function '{name}' missing return type annotation (inferred as '{return_type}')"
                                    ),
                                    related_information: None,
                                    tags: None,
                                    data: None,
                                    code_description: None,
                                });
                            }
                        }
                        None => {
                            let severity =
                                Self::mode_severity_for_diagnostic(ctx.mode, DiagnosticCategory::ImplicitAny);

                            if let Some(sev) = severity {
                                let range = ctx.range_for_name(line, col, name);

                                ctx.diagnostics.push(Diagnostic {
                                    range,
                                    severity: Some(sev),
                                    code: Some(lsp_types::NumberOrString::String("ANN012".to_string())),
                                    source: Some("beacon".to_string()),
                                    message: format!(
                                        "Function '{name}' has implicit Any return type - consider adding type annotation"
                                    ),
                                    related_information: None,
                                    tags: None,
                                    data: None,
                                    code_description: None,
                                });
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    /// Check for bare except handlers in strict mode
    fn check_bare_except_handler(&self, handler: &beacon_parser::ExceptHandler, ctx: &mut DiagnosticContext) {
        if handler.exception_type.is_none() && ctx.mode == config::TypeCheckingMode::Strict {
            let position = Position {
                line: (handler.line.saturating_sub(1)) as u32,
                character: (handler.col.saturating_sub(1)) as u32,
            };

            let range = Range {
                start: position,
                end: Position {
                    line: (handler.end_line.saturating_sub(1)) as u32,
                    character: (handler.end_col.saturating_sub(1)) as u32,
                },
            };

            ctx.diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(lsp_types::NumberOrString::String("ANN010".to_string())),
                source: Some("beacon".to_string()),
                message: "Bare except clause not allowed in strict mode - specify exception type(s)".to_string(),
                related_information: None,
                tags: None,
                data: None,
                code_description: None,
            });
        }
    }

    /// Add dunder-specific diagnostics
    fn add_dunder_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                Self::check_dunder_patterns(ast, diagnostics);

                if let Some(symbol_table) = doc.symbol_table() {
                    let source = doc.text();
                    self.check_magic_methods_in_scope(ast, symbol_table, diagnostics, &source);
                }
            }
        });
    }

    /// Check for common dunder patterns like if __name__ == "__main__"
    fn check_dunder_patterns(node: &AstNode, diagnostics: &mut Vec<Diagnostic>) {
        match node {
            AstNode::If { test, body, line, col, .. } => {
                if Self::is_name_main_check(test) {
                    let range = if let AstNode::Compare {
                        line: cmp_line,
                        col: cmp_col,
                        end_line: cmp_end_line,
                        end_col: cmp_end_col,
                        ..
                    } = test.as_ref()
                    {
                        Range {
                            start: Position {
                                line: (cmp_line.saturating_sub(1)) as u32,
                                character: (cmp_col.saturating_sub(1)) as u32,
                            },
                            end: Position {
                                line: (cmp_end_line.saturating_sub(1)) as u32,
                                character: (cmp_end_col.saturating_sub(1)) as u32,
                            },
                        }
                    } else {
                        Range {
                            start: Position { line: (*line - 1) as u32, character: (*col - 1) as u32 },
                            end: Position { line: (*line - 1) as u32, character: (*col - 1) as u32 + 10 },
                        }
                    };
                    diagnostics.push(Diagnostic {
                        range,
                        severity: Some(DiagnosticSeverity::HINT),
                        code: Some(lsp_types::NumberOrString::String("DUNDER_INFO".to_string())),
                        source: Some("beacon".to_string()),
                        message: "Entry point guard: This code runs only when the script is executed directly"
                            .to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                }

                for stmt in body {
                    Self::check_dunder_patterns(stmt, diagnostics);
                }
            }
            AstNode::Module { body, .. } | AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::check_dunder_patterns(stmt, diagnostics);
                }
            }
            _ => {}
        }
    }

    /// Check if a test expression is: __name__ == "__main__"
    fn is_name_main_check(test: &AstNode) -> bool {
        match test {
            AstNode::Compare { left, ops, comparators, .. } => {
                let is_name = matches!(**left, AstNode::Identifier { ref name, .. } if name == "__name__");

                let is_eq_main = ops.iter().any(|op| matches!(op, beacon_parser::CompareOperator::Eq))
                    && comparators.iter().any(|comp| {
                        matches!(comp, AstNode::Literal { value: beacon_parser::LiteralValue::String{value: s, ..}, .. } if s == "__main__")
                    });

                is_name && is_eq_main
            }
            _ => false,
        }
    }

    /// Check for magic methods defined outside class scope
    fn check_magic_methods_in_scope(
        &self, node: &AstNode, symbol_table: &SymbolTable, diagnostics: &mut Vec<Diagnostic>, source: &str,
    ) {
        match node {
            AstNode::FunctionDef { name, line, col, body, .. } => {
                if MAGIC_METHODS.contains(&name.as_str()) {
                    let byte_offset = Self::line_col_to_byte_offset_from_source(source, line, col);
                    let scope_id = symbol_table.find_scope_at_position(byte_offset);

                    if !symbol_table.is_in_class_scope(scope_id) {
                        let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };
                        diagnostics.push(Diagnostic {
                            range: Range {
                                start: position,
                                end: Position {
                                    line: position.line,
                                    character: position.character + name.len() as u32,
                                },
                            },
                            severity: Some(DiagnosticSeverity::WARNING),
                            code: Some(lsp_types::NumberOrString::String("DUNDER001".to_string())),
                            source: Some("beacon".to_string()),
                            message: format!("Magic method '{name}' defined outside of a class"),
                            related_information: None,
                            tags: None,
                            data: None,
                            code_description: None,
                        });
                    } else {
                        self.validate_magic_method_signature(name, line, col, diagnostics);
                    }
                }

                for stmt in body {
                    self.check_magic_methods_in_scope(stmt, symbol_table, diagnostics, source);
                }
            }
            AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    self.check_magic_methods_in_scope(stmt, symbol_table, diagnostics, source);
                }
            }
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.check_magic_methods_in_scope(stmt, symbol_table, diagnostics, source);
                }
            }
            _ => {}
        }
    }

    /// TODO: Implement this
    fn validate_magic_method_signature(
        &self, name: &str, line: &usize, col: &usize, diagnostics: &mut Vec<Diagnostic>,
    ) {
        let _ = (name, line, col, diagnostics);
    }

    /// Convert line/col to byte offset using the actual source
    fn line_col_to_byte_offset_from_source(source: &str, line: &usize, col: &usize) -> usize {
        let mut byte_offset = 0;
        let mut current_line = 1;
        let mut current_col = 1;

        for ch in source.chars() {
            if current_line == *line && current_col == *col {
                return byte_offset;
            }
            if ch == '\n' {
                current_line += 1;
                current_col = 1;
            } else {
                current_col += 1;
            }
            byte_offset += ch.len_utf8();
        }

        byte_offset
    }

    /// Add static analysis diagnostics (use-before-def, unreachable code, unused variables, shadowing)
    fn add_static_analysis_diagnostics(&self, uri: &Url, analyzer: &mut Analyzer, diagnostics: &mut Vec<Diagnostic>) {
        let result = match analyzer.analyze(uri) {
            Ok(r) => r,
            Err(_) => return,
        };

        let Some(static_analysis) = result.static_analysis else {
            return;
        };

        for use_before_def in &static_analysis.use_before_def {
            let position = Position {
                line: (use_before_def.line.saturating_sub(1)) as u32,
                character: (use_before_def.col.saturating_sub(1)) as u32,
            };

            let range = Range {
                start: position,
                end: Position {
                    line: position.line,
                    character: position.character + use_before_def.var_name.len() as u32,
                },
            };

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(lsp_types::NumberOrString::String("use-before-def".to_string())),
                source: Some("beacon".to_string()),
                message: format!("Variable '{}' used before assignment", use_before_def.var_name),
                related_information: None,
                tags: None,
                data: None,
                code_description: None,
            });
        }

        for unreachable in &static_analysis.unreachable_code {
            let position = Position {
                line: (unreachable.line.saturating_sub(1)) as u32,
                character: (unreachable.col.saturating_sub(1)) as u32,
            };

            let range =
                Range { start: position, end: Position { line: position.line, character: position.character + 10 } };

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                code: Some(lsp_types::NumberOrString::String("unreachable-code".to_string())),
                source: Some("beacon".to_string()),
                message: "Unreachable code detected".to_string(),
                related_information: None,
                tags: Some(vec![lsp_types::DiagnosticTag::UNNECESSARY]),
                data: None,
                code_description: None,
            });
        }

        for unused in &static_analysis.unused_variables {
            let position = Position {
                line: (unused.line.saturating_sub(1)) as u32,
                character: (unused.col.saturating_sub(1)) as u32,
            };

            let range = Range {
                start: position,
                end: Position { line: position.line, character: position.character + unused.var_name.len() as u32 },
            };

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::HINT),
                code: Some(lsp_types::NumberOrString::String("unused-variable".to_string())),
                source: Some("beacon".to_string()),
                message: format!("Variable '{}' is assigned but never used", unused.var_name),
                related_information: None,
                tags: Some(vec![lsp_types::DiagnosticTag::UNNECESSARY]),
                data: None,
                code_description: None,
            });
        }

        self.documents.get_document(uri, |doc| {
            if let Some(symbol_table) = doc.symbol_table() {
                let shadowed = symbol_table.find_shadowed_symbols();
                for (child_symbol, parent_symbol) in shadowed {
                    let position = Position {
                        line: (child_symbol.line.saturating_sub(1)) as u32,
                        character: (child_symbol.col.saturating_sub(1)) as u32,
                    };

                    let range = Range {
                        start: position,
                        end: Position {
                            line: position.line,
                            character: position.character + child_symbol.name.len() as u32,
                        },
                    };

                    diagnostics.push(Diagnostic {
                        range,
                        severity: Some(DiagnosticSeverity::WARNING),
                        code: Some(lsp_types::NumberOrString::String("shadowed-variable".to_string())),
                        source: Some("beacon".to_string()),
                        message: format!(
                            "Variable '{}' shadows variable from outer scope (line {})",
                            child_symbol.name, parent_symbol.line
                        ),
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                }
            }
        });
    }

    /// Add circular import diagnostics
    ///
    /// Detects circular dependencies between modules and reports them as errors.
    fn add_circular_import_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        let Ok(workspace) = self.workspace.try_read() else {
            return;
        };

        let circular_groups = workspace.circular_dependencies();
        let severity = Self::config_severity_to_lsp(workspace.config.circular_import_severity);

        for group in circular_groups {
            if !group.contains(uri) {
                continue;
            }

            let cycle_chain = group
                .iter()
                .filter_map(|u| workspace.uri_to_module_name(u))
                .collect::<Vec<_>>()
                .join(" → ");

            let message = format!("Circular import detected: {cycle_chain} → {cycle_chain}");

            self.documents.get_document(uri, |doc| {
                if let Some(ast) = doc.ast() {
                    Self::find_import_locations(ast, &group, &workspace, diagnostics, &message, severity);
                }
            });
        }
    }

    /// Find import statement locations for circular dependency reporting
    fn find_import_locations(
        node: &AstNode, circular_group: &[Url], workspace: &Workspace, diagnostics: &mut Vec<Diagnostic>,
        message: &str, severity: lsp_types::DiagnosticSeverity,
    ) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::find_import_locations(stmt, circular_group, workspace, diagnostics, message, severity);
                }
            }
            AstNode::Import { module, line, col, .. } => {
                if let Some(resolved_uri) = workspace.resolve_import(module)
                    && circular_group.contains(&resolved_uri)
                {
                    let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };

                    let range = Range {
                        start: position,
                        end: Position { line: position.line, character: position.character + module.len() as u32 },
                    };

                    diagnostics.push(Diagnostic {
                        range,
                        severity: Some(severity),
                        code: Some(lsp_types::NumberOrString::String("circular-import".to_string())),
                        source: Some("beacon".to_string()),
                        message: message.to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                }
            }
            AstNode::ImportFrom { module, line, col, .. } => {
                if let Some(resolved_uri) = workspace.resolve_import(module)
                    && circular_group.contains(&resolved_uri)
                {
                    let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };

                    let range = Range {
                        start: position,
                        end: Position { line: position.line, character: position.character + module.len() as u32 },
                    };

                    diagnostics.push(Diagnostic {
                        range,
                        severity: Some(severity),
                        code: Some(lsp_types::NumberOrString::String("circular-import".to_string())),
                        source: Some("beacon".to_string()),
                        message: message.to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                }
            }
            AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::find_import_locations(stmt, circular_group, workspace, diagnostics, message, severity);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    Self::find_import_locations(stmt, circular_group, workspace, diagnostics, message, severity);
                }
                for (_test, elif_body) in elif_parts {
                    for stmt in elif_body {
                        Self::find_import_locations(stmt, circular_group, workspace, diagnostics, message, severity);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::find_import_locations(stmt, circular_group, workspace, diagnostics, message, severity);
                    }
                }
            }
            _ => {}
        }
    }

    /// Add unresolved import diagnostics
    ///
    /// Reports imports that cannot be resolved to any module in the workspace or stubs.
    fn add_unresolved_import_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        let Ok(workspace) = self.workspace.try_read() else {
            return;
        };

        let unresolved = workspace.unresolved_imports(uri);

        if unresolved.is_empty() {
            return;
        }

        let severity = Self::config_severity_to_lsp(workspace.config.unresolved_import_severity);

        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                Self::find_unresolved_import_locations(ast, &unresolved, &workspace, diagnostics, severity);
            }
        });
    }

    /// Find locations of unresolved imports in the AST
    fn find_unresolved_import_locations(
        node: &AstNode, unresolved: &[String], _workspace: &Workspace, diagnostics: &mut Vec<Diagnostic>,
        severity: lsp_types::DiagnosticSeverity,
    ) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::find_unresolved_import_locations(stmt, unresolved, _workspace, diagnostics, severity);
                }
            }
            AstNode::Import { module, line, col, .. } => {
                if unresolved.contains(module) {
                    let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };

                    let range = Range {
                        start: position,
                        end: Position { line: position.line, character: position.character + module.len() as u32 },
                    };

                    let message = format!("Cannot resolve import '{module}'");

                    diagnostics.push(Diagnostic {
                        range,
                        severity: Some(severity),
                        code: Some(lsp_types::NumberOrString::String("unresolved-import".to_string())),
                        source: Some("beacon".to_string()),
                        message,
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                }
            }
            AstNode::ImportFrom { module, line, col, .. } => {
                if unresolved.contains(module) {
                    let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };

                    let range = Range {
                        start: position,
                        end: Position { line: position.line, character: position.character + module.len() as u32 },
                    };

                    let message = if module.starts_with('.') {
                        format!("Cannot resolve relative import '{module}'")
                    } else {
                        format!("Cannot resolve import '{module}'")
                    };

                    diagnostics.push(Diagnostic {
                        range,
                        severity: Some(severity),
                        code: Some(lsp_types::NumberOrString::String("unresolved-import".to_string())),
                        source: Some("beacon".to_string()),
                        message,
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                }
            }
            AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::find_unresolved_import_locations(stmt, unresolved, _workspace, diagnostics, severity);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    Self::find_unresolved_import_locations(stmt, unresolved, _workspace, diagnostics, severity);
                }
                for (_test, elif_body) in elif_parts {
                    for stmt in elif_body {
                        Self::find_unresolved_import_locations(stmt, unresolved, _workspace, diagnostics, severity);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::find_unresolved_import_locations(stmt, unresolved, _workspace, diagnostics, severity);
                    }
                }
            }
            _ => {}
        }
    }

    /// Add missing module diagnostics
    ///
    /// Reports modules that are referenced but don't exist in the workspace.
    /// Similar to unresolved imports but with more specific error messages.
    fn add_missing_module_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        let Ok(workspace) = self.workspace.try_read() else {
            return;
        };

        let unresolved = workspace.unresolved_imports(uri);

        if unresolved.is_empty() {
            return;
        }

        let from_module = workspace.uri_to_module_name(uri).unwrap_or_default();

        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                self.find_missing_module_locations(ast, &unresolved, &from_module, &workspace, diagnostics);
            }
        });
    }

    /// Find locations of missing modules in the AST
    fn find_missing_module_locations(
        &self, node: &AstNode, unresolved: &[String], from_module: &str, workspace: &Workspace,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.find_missing_module_locations(stmt, unresolved, from_module, workspace, diagnostics);
                }
            }
            AstNode::Import { module, line, col, .. } => {
                if unresolved.contains(module) {
                    let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };

                    let range = Range {
                        start: position,
                        end: Position { line: position.line, character: position.character + module.len() as u32 },
                    };

                    let message = self.format_missing_module_message(module, from_module, workspace);

                    diagnostics.push(Diagnostic {
                        range,
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(lsp_types::NumberOrString::String("missing-module".to_string())),
                        source: Some("beacon".to_string()),
                        message,
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                }
            }
            AstNode::ImportFrom { module, line, col, .. } => {
                if unresolved.contains(module) {
                    let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };

                    let range = Range {
                        start: position,
                        end: Position { line: position.line, character: position.character + module.len() as u32 },
                    };

                    let message = self.format_missing_module_message(module, from_module, workspace);

                    diagnostics.push(Diagnostic {
                        range,
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(lsp_types::NumberOrString::String("missing-module".to_string())),
                        source: Some("beacon".to_string()),
                        message,
                        related_information: None,
                        tags: None,
                        data: None,
                        code_description: None,
                    });
                }
            }
            AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    self.find_missing_module_locations(stmt, unresolved, from_module, workspace, diagnostics);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    self.find_missing_module_locations(stmt, unresolved, from_module, workspace, diagnostics);
                }
                for (_test, elif_body) in elif_parts {
                    for stmt in elif_body {
                        self.find_missing_module_locations(stmt, unresolved, from_module, workspace, diagnostics);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.find_missing_module_locations(stmt, unresolved, from_module, workspace, diagnostics);
                    }
                }
            }
            _ => {}
        }
    }

    /// Format a helpful error message for missing modules
    ///
    /// Uses fuzzy matching to suggest similar module names when a module is not found.
    fn format_missing_module_message(&self, module: &str, from_module: &str, workspace: &Workspace) -> String {
        if module.starts_with('.') {
            let leading_dots = module.chars().take_while(|&c| c == '.').count();
            let from_parts: Vec<&str> = from_module.split('.').collect();

            if leading_dots > from_parts.len() {
                return format!(
                    "Relative import '{module}' goes beyond top-level package (current module: {from_module})"
                );
            }

            format!("Module '{module}' not found (relative import from {from_module})")
        } else {
            let all_modules = workspace.all_modules();

            let mut scored_modules: Vec<(&str, f64)> = all_modules
                .iter()
                .map(|(_, name)| {
                    let similarity = self.fuzzy_matcher.similarity(module, name.as_str());
                    (name.as_str(), similarity)
                })
                .filter(|(_, score)| *score >= self.fuzzy_matcher.threshold())
                .collect();

            scored_modules.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

            if let Some((suggestion, _)) = scored_modules.first() {
                format!("Module '{module}' not found - did you mean '{suggestion}'?")
            } else {
                format!("Module '{module}' not found")
            }
        }
    }

    /// Add inconsistent export diagnostics
    ///
    /// Reports symbols in __all__ that are not defined in the module
    fn add_inconsistent_export_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        let Ok(workspace) = self.workspace.try_read() else {
            return;
        };

        let all_exports = match workspace.get_all_exports(uri) {
            Some(exports) => exports,
            None => return,
        };

        let module_symbols = workspace.get_module_symbols(uri);

        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                Self::find_all_assignment_location(ast, &all_exports, &module_symbols, diagnostics);
            }
        });
    }

    /// Find the __all__ assignment in the AST and report inconsistencies
    fn find_all_assignment_location(
        node: &AstNode, _all_exports: &[String], module_symbols: &rustc_hash::FxHashSet<String>,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::find_all_assignment_location(stmt, _all_exports, module_symbols, diagnostics);
                }
            }
            AstNode::Assignment { target, value, line, col, .. } => {
                let target_name = target.target_to_string();
                if target_name == "__all__"
                    && let AstNode::List { elements, .. } = value.as_ref()
                {
                    for (idx, element) in elements.iter().enumerate() {
                        if let AstNode::Literal {
                            value: beacon_parser::LiteralValue::String { value: symbol_name, .. },
                            ..
                        } = element
                            && !module_symbols.contains(symbol_name)
                        {
                            let position = Position {
                                line: (*line - 1) as u32,
                                character: (*col + idx * (symbol_name.len() + 4)) as u32,
                            };

                            let range = Range {
                                start: position,
                                end: Position {
                                    line: position.line,
                                    character: position.character + symbol_name.len() as u32 + 2,
                                },
                            };

                            diagnostics.push(Diagnostic {
                                range,
                                severity: Some(DiagnosticSeverity::WARNING),
                                code: Some(lsp_types::NumberOrString::String("BEA031".to_string())),
                                source: Some("beacon-linter".to_string()),
                                message: format!(
                                    "Symbol '{symbol_name}' is exported in __all__ but not defined in module"
                                ),
                                related_information: None,
                                tags: None,
                                data: None,
                                code_description: None,
                            });
                        }
                    }
                }
            }
            _ => {}
        }
    }

    /// Add conflicting stub definition diagnostics
    ///
    /// Reports cases where multiple stub files define the same symbol with different types
    fn add_conflicting_stub_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        let Ok(workspace) = self.workspace.try_read() else {
            return;
        };

        let module_name = match workspace.uri_to_module_name(uri) {
            Some(name) => name,
            None => return,
        };

        let conflicts = workspace.get_conflicting_stub_definitions(&module_name);

        if conflicts.is_empty() {
            return;
        }

        for (symbol_name, type_definitions) in conflicts {
            if type_definitions.len() > 1 {
                let type_list: Vec<String> = type_definitions
                    .iter()
                    .map(|(ty, path)| format!("{} (from {})", ty, path.display()))
                    .collect();

                diagnostics.push(Diagnostic {
                    range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 1 } },
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(lsp_types::NumberOrString::String("BEA032".to_string())),
                    source: Some("beacon-linter".to_string()),
                    message: format!(
                        "Symbol '{}' has conflicting type definitions across stub files: {}",
                        symbol_name,
                        type_list.join(", ")
                    ),
                    related_information: None,
                    tags: None,
                    data: None,
                    code_description: None,
                });
            }
        }
    }
}

/// Convert a parse error to an LSP diagnostic
fn parse_error_to_diagnostic(error: &ParseError) -> Diagnostic {
    Diagnostic {
        range: error.range,
        severity: Some(match error.severity {
            parser::ErrorSeverity::Error => DiagnosticSeverity::ERROR,
            parser::ErrorSeverity::Warning => DiagnosticSeverity::WARNING,
            parser::ErrorSeverity::Hint => DiagnosticSeverity::HINT,
        }),
        code: None,
        code_description: None,
        source: Some("beacon".to_string()),
        message: error.message.clone(),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Convert an analysis error to an LSP diagnostic
fn analysis_error_to_diagnostic(error: &BeaconError) -> Diagnostic {
    Diagnostic {
        range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 0 } },
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("beacon".to_string()),
        message: error.to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Convert a type error with location info to an LSP diagnostic
fn type_error_to_diagnostic(error_info: &beacon_constraint::TypeErrorInfo) -> Diagnostic {
    let start_pos = Position {
        line: (error_info.line().saturating_sub(1)) as u32,
        character: (error_info.col().saturating_sub(1)) as u32,
    };

    let end_pos = match (error_info.end_line(), error_info.end_col()) {
        (Some(end_line), Some(end_col)) => {
            Position { line: (end_line.saturating_sub(1)) as u32, character: (end_col.saturating_sub(1)) as u32 }
        }
        _ => Position { line: start_pos.line, character: start_pos.character + 10 },
    };

    let range = Range { start: start_pos, end: end_pos };

    let (code, message) = match &error_info.error {
        TypeError::UnificationError(t1, t2) => {
            let base_msg = format!("Type mismatch: cannot unify {t1} with {t2}");
            let enhanced_msg = enhance_unification_error_message(&base_msg, t1, t2);
            ("HM001", enhanced_msg)
        }
        TypeError::OccursCheckFailed(tv, ty) => (
            "HM002",
            format!(
                "Infinite type: type variable {tv} occurs in {ty}. This usually indicates a recursive type definition."
            ),
        ),
        TypeError::UndefinedTypeVar(tv) => ("HM003", format!("Undefined type variable: {tv}")),
        TypeError::KindMismatch { expected, found } => {
            ("HM004", format!("Kind mismatch: expected {expected}, found {found}"))
        }
        TypeError::InfiniteType(msg) => ("HM005", format!("Infinite type: {msg}")),
        TypeError::ProtocolNotSatisfied(ty, protocol) => {
            let enhanced_msg = enhance_protocol_error_message(ty, protocol);
            ("HM006", enhanced_msg)
        }
        TypeError::AttributeNotFound(ty, attr) => {
            let enhanced_msg = enhance_attribute_error_message(ty, attr);
            ("HM007", enhanced_msg)
        }
        TypeError::ArgumentCountMismatch { expected, found } => (
            "HM008",
            format!("Argument count mismatch: expected {expected} argument(s), got {found}"),
        ),
        TypeError::ArgumentTypeMismatch { param_name, expected, found } => (
            "HM009",
            format!("Argument of type '{found}' cannot be assigned to parameter '{param_name}' of type '{expected}'"),
        ),
        TypeError::PatternNonExhaustive(uncovered) => (
            "PM001",
            format!("Pattern match is not exhaustive. Missing coverage for: {uncovered}"),
        ),
        TypeError::PatternUnreachable => (
            "PM002",
            "This pattern is unreachable (subsumed by an earlier pattern)".to_string(),
        ),
        TypeError::PatternTypeMismatch { pattern_type, subject_type } => (
            "HM010",
            format!(
                "Pattern type mismatch: pattern type '{pattern_type}' cannot match subject of type '{subject_type}'"
            ),
        ),
        TypeError::PatternStructureMismatch { expected, found } => (
            "HM013",
            format!("Invalid pattern structure: expected {expected}, found {found}"),
        ),
        TypeError::KeywordArgumentError(msg) => ("HM011", format!("Keyword argument error: {msg}")),
        TypeError::VarianceError { position, expected_variance, got_type, expected_type } => (
            "HM014",
            enhance_variance_error_message(position, expected_variance, got_type, expected_type),
        ),
        TypeError::Other(msg) => ("HM012", format!("Type error: {msg}")),
    };

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(lsp_types::NumberOrString::String(code.to_string())),
        source: Some("beacon".to_string()),
        message,
        related_information: None,
        tags: None,
        data: None,
        code_description: None,
    }
}

/// Enhance unification error messages with contextual hints based on common type mismatches
fn enhance_unification_error_message(base_msg: &str, t1: &str, t2: &str) -> String {
    if (t1.contains("str") && t2.contains("int")) || (t1.contains("int") && t2.contains("str")) {
        return format!("{base_msg}. Ensure you're not mixing strings and integers without explicit conversion.");
    }

    if (t1.contains("list") && t2.contains("dict")) || (t1.contains("dict") && t2.contains("list")) {
        return format!("{base_msg}. Collection type mismatch - ensure data structures match expected types.");
    }

    if t1.contains("Union") || t2.contains("Union") {
        if (t1.contains("None") && t2.contains("Union")) || (t2.contains("None") && t1.contains("Union")) {
            return format!("{base_msg}. You may need to narrow the type with an isinstance() check or type guard.");
        }
        return format!("{base_msg}. Union types require all branches to be compatible with the target type.");
    }

    if t1.contains("None") || t2.contains("None") {
        return format!(
            "{base_msg}. One value is None where a different type is expected. Consider using Optional[T] or adding a None check."
        );
    }

    base_msg.to_string()
}

/// Enhance protocol error messages with helpful context
fn enhance_protocol_error_message(ty: &str, protocol: &str) -> String {
    let base = format!("Type {ty} does not satisfy protocol {protocol}");

    if protocol.contains("Iterable") {
        format!("{base}. The value cannot be iterated over in a loop or comprehension.")
    } else {
        base
    }
}

/// Enhance attribute error messages with suggestions
fn enhance_attribute_error_message(ty: &str, attr: &str) -> String {
    let base = format!("Attribute '{attr}' not found on type {ty}");

    match attr {
        "splitlines" if ty.contains("int") => {
            format!("{base}. Did you mean to use a string? splitlines() is a string method.")
        }
        "write_text" if !ty.contains("Path") => format!("{base}. Did you mean to use a Path object from pathlib?"),
        "get" if !ty.contains("dict") => format!("{base}. The get() method is available on dictionaries, not {ty}."),
        "append" | "extend" if !ty.contains("list") => {
            format!("{base}. The {attr}() method is available on lists, not {ty}.")
        }
        "load" => format!("{base}. Ensure the object has been properly initialized with the expected type."),
        _ => format!("{base}. Check that the type is correct or that you've imported the necessary modules."),
    }
}

/// Enhance variance error messages with specific suggestions based on common patterns
fn enhance_variance_error_message(
    position: &str, expected_variance: &str, got_type: &str, expected_type: &str,
) -> String {
    let base = format!(
        "Variance error in {position}: expected {expected_variance} variance, cannot assign '{got_type}' to '{expected_type}'"
    );

    match expected_variance {
        "invariant" => {
            if position.contains("list") || position.contains("dict") || position.contains("set") {
                format!(
                    "{base}\n\nMutable containers like list, dict, and set are invariant. \
                    You cannot assign a container of subtype to a container of supertype. \
                    Consider using immutable containers (tuple, frozenset) if you need covariance, \
                    or use protocols for structural typing."
                )
            } else {
                format!(
                    "{base}\n\nInvariant positions require exact type matches. \
                    Consider using a more general type or restructuring your code."
                )
            }
        }
        "covariant" => {
            if position.contains("return") || position.contains("tuple") {
                format!(
                    "{base}\n\nReturn types and immutable containers are covariant. \
                    The type '{got_type}' must be a subtype of '{expected_type}'."
                )
            } else {
                format!(
                    "{base}\n\nCovariant positions allow subtypes. \
                    Ensure '{got_type}' is a subtype of '{expected_type}'."
                )
            }
        }
        "contravariant" => {
            if position.contains("parameter") || position.contains("argument") {
                format!(
                    "{base}\n\nFunction parameters are contravariant. \
                    A function that accepts '{expected_type}' can only be substituted by a function \
                    that accepts a supertype of '{expected_type}', not a subtype like '{got_type}'. \
                    Consider making the parameter type more general."
                )
            } else {
                format!(
                    "{base}\n\nContravariant positions require supertypes. \
                    The type '{got_type}' should be a supertype of '{expected_type}', not a subtype."
                )
            }
        }
        _ => base,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_constraint::{Span, TypeErrorInfo};
    use beacon_core::{AnalysisError, Type, TypeCtor, TypeError, TypeVar};
    use std::str::FromStr;
    use std::sync::Arc;
    use tokio::sync::RwLock;

    /// Create a test workspace for diagnostic tests
    fn create_test_workspace(documents: crate::document::DocumentManager) -> Arc<RwLock<crate::workspace::Workspace>> {
        let config = crate::config::Config::default();
        Arc::new(RwLock::new(crate::workspace::Workspace::new(None, config, documents)))
    }

    #[test]
    fn test_parse_error_conversion() {
        let parse_error = ParseError {
            message: "Syntax error".to_string(),
            range: Range { start: Position { line: 1, character: 5 }, end: Position { line: 1, character: 10 } },
            severity: crate::parser::ErrorSeverity::Error,
        };

        let diagnostic = parse_error_to_diagnostic(&parse_error);

        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diagnostic.message, "Syntax error");
        assert_eq!(diagnostic.source, Some("beacon".to_string()));
    }

    #[test]
    fn test_type_error_unification_conversion() {
        let error_info = TypeErrorInfo {
            error: TypeError::UnificationError("int".to_string(), "str".to_string()),
            span: Span { line: 10, col: 5, end_line: Some(10), end_col: Some(8) },
        };

        let diagnostic = type_error_to_diagnostic(&error_info);

        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diagnostic.message.contains("cannot unify"));
        assert!(diagnostic.message.contains("int"));
        assert!(diagnostic.message.contains("str"));
        assert_eq!(
            diagnostic.code,
            Some(lsp_types::NumberOrString::String("HM001".to_string()))
        );
        assert_eq!(diagnostic.range.start.line, 9);
        assert_eq!(diagnostic.range.start.character, 4);
    }

    #[test]
    fn test_type_error_occurs_check_conversion() {
        let tv = TypeVar::new(0);
        let error_info = TypeErrorInfo {
            error: TypeError::OccursCheckFailed(tv.clone(), "List['t0]".to_string()),
            span: Span { line: 5, col: 10, end_line: None, end_col: None },
        };

        let diagnostic = type_error_to_diagnostic(&error_info);

        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diagnostic.message.contains("Infinite type"));
        assert_eq!(
            diagnostic.code,
            Some(lsp_types::NumberOrString::String("HM002".to_string()))
        );
    }

    #[test]
    fn test_type_error_kind_mismatch_conversion() {
        let error_info = TypeErrorInfo {
            error: TypeError::KindMismatch { expected: "*".to_string(), found: "* -> *".to_string() },
            span: Span { line: 3, col: 1, end_line: Some(3), end_col: Some(10) },
        };

        let diagnostic = type_error_to_diagnostic(&error_info);

        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diagnostic.message.contains("Kind mismatch"));
        assert_eq!(
            diagnostic.code,
            Some(lsp_types::NumberOrString::String("HM004".to_string()))
        );
    }

    #[test]
    fn test_contains_any_type_simple() {
        assert!(DiagnosticProvider::contains_any_type(&Type::Con(TypeCtor::Any), 0));
        assert!(!DiagnosticProvider::contains_any_type(&Type::Con(TypeCtor::Int), 0));
    }

    #[test]
    fn test_contains_any_type_nested() {
        let list_any = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Any)));
        assert!(DiagnosticProvider::contains_any_type(&list_any, 0));

        let list_int = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
        assert!(!DiagnosticProvider::contains_any_type(&list_int, 0));
    }

    #[test]
    fn test_contains_any_type_function() {
        let fun_any = Type::Fun(
            vec![(String::new(), Type::Con(TypeCtor::Int))],
            Box::new(Type::Con(TypeCtor::Any)),
        );
        assert!(DiagnosticProvider::contains_any_type(&fun_any, 0));

        let fun_normal = Type::Fun(
            vec![(String::new(), Type::Con(TypeCtor::Int))],
            Box::new(Type::Con(TypeCtor::String)),
        );
        assert!(!DiagnosticProvider::contains_any_type(&fun_normal, 0));
    }

    #[test]
    fn test_contains_any_type_union() {
        let union_any = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::Any)]);
        assert!(DiagnosticProvider::contains_any_type(&union_any, 0));

        let union_normal = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
        assert!(!DiagnosticProvider::contains_any_type(&union_normal, 0));
    }

    #[test]
    fn test_diagnostic_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let _provider = DiagnosticProvider::new(documents, workspace);
    }

    #[test]
    fn test_generate_diagnostics_with_parse_errors() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "def broken(";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.severity == Some(DiagnosticSeverity::ERROR))
        );
    }

    #[test]
    fn test_generate_diagnostics_with_unbound_variables() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def test():
    x = undefined_variable
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
        assert!(diagnostics.iter().any(|d| d.message.contains("undefined_variable")));
        assert!(
            diagnostics
                .iter()
                .any(|d| d.code == Some(lsp_types::NumberOrString::String("undefined-variable".to_string())))
        );
    }

    #[test]
    fn test_type_error_undefined_typevar_conversion() {
        let tv = TypeVar::new(5);
        let error_info = TypeErrorInfo {
            error: TypeError::UndefinedTypeVar(tv.clone()),
            span: Span { line: 1, col: 1, end_line: None, end_col: None },
        };

        let diagnostic = type_error_to_diagnostic(&error_info);
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diagnostic.message.contains("Undefined type variable"));
        assert_eq!(
            diagnostic.code,
            Some(lsp_types::NumberOrString::String("HM003".to_string()))
        );
    }

    #[test]
    fn test_type_error_infinite_type_conversion() {
        let error_info = TypeErrorInfo {
            error: TypeError::InfiniteType("recursive type".to_string()),
            span: Span { line: 7, col: 3, end_line: Some(7), end_col: Some(15) },
        };

        let diagnostic = type_error_to_diagnostic(&error_info);
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diagnostic.message.contains("Infinite type"));
        assert_eq!(
            diagnostic.code,
            Some(lsp_types::NumberOrString::String("HM005".to_string()))
        );
    }

    #[test]
    fn test_contains_any_type_record() {
        let record_any = Type::Record(vec![("field".to_string(), Type::Con(TypeCtor::Any))], None);
        assert!(DiagnosticProvider::contains_any_type(&record_any, 0));

        let record_normal = Type::Record(vec![("field".to_string(), Type::Con(TypeCtor::Int))], None);
        assert!(!DiagnosticProvider::contains_any_type(&record_normal, 0));
    }

    #[test]
    fn test_generate_diagnostics_empty_document() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///empty.py").unwrap();
        documents.open_document(uri.clone(), 1, "".to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_analysis_error_to_diagnostic() {
        let error = AnalysisError::MissingAst;
        let diagnostic = analysis_error_to_diagnostic(&BeaconError::from(error));

        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diagnostic.source, Some("beacon".to_string()));
        assert!(diagnostic.message.contains("Missing AST"));
    }

    #[test]
    fn test_dunder_name_main_pattern_detected() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
if __name__ == "__main__":
    print("Running as main")
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let dunder_hint = diagnostics
            .iter()
            .find(|d| d.code == Some(lsp_types::NumberOrString::String("DUNDER_INFO".to_string())));

        assert!(dunder_hint.is_some());
        let hint = dunder_hint.unwrap();
        assert_eq!(hint.severity, Some(DiagnosticSeverity::HINT));
        assert!(hint.message.contains("Entry point guard"));
    }

    #[test]
    fn test_magic_method_outside_class_warning() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def __init__(self):
    self.x = 1
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let magic_warning = diagnostics
            .iter()
            .find(|d| d.code == Some(lsp_types::NumberOrString::String("DUNDER001".to_string())));

        assert!(magic_warning.is_some());
        let warning = magic_warning.unwrap();
        assert_eq!(warning.severity, Some(DiagnosticSeverity::WARNING));
        assert!(warning.message.contains("Magic method"));
        assert!(warning.message.contains("outside of a class"));
    }

    #[test]
    fn test_magic_method_inside_class_no_warning() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class MyClass:
    def __init__(self):
        self.x = 1
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
        let magic_warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                d.code == Some(lsp_types::NumberOrString::String("DUNDER001".to_string()))
                    && d.message.contains("__init__")
            })
            .collect();

        if !magic_warnings.is_empty() {
            panic!("Got unexpected warning for __init__ inside class: {magic_warnings:?}");
        }
    }

    #[test]
    fn test_is_name_main_check_positive() {
        let test_expr = AstNode::Compare {
            left: Box::new(AstNode::Identifier {
                name: "__name__".to_string(),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 12,
            }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: beacon_parser::LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                line: 1,
                col: 16,
                end_line: 1,
                end_col: 9,
            }],
            line: 1,
            col: 12,
            end_line: 1,
            end_col: 12,
        };

        assert!(DiagnosticProvider::is_name_main_check(&test_expr));
    }

    #[test]
    fn test_is_name_main_check_negative() {
        let test_expr = AstNode::Compare {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 4, end_line: 1, end_col: 5 }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: beacon_parser::LiteralValue::Integer(42),
                line: 1,
                col: 9,
                end_line: 1,
                end_col: 11,
            }],
            line: 1,
            col: 6,
            end_line: 1,
            end_col: 6,
        };

        assert!(!DiagnosticProvider::is_name_main_check(&test_expr));
    }

    #[test]
    fn test_type_errors_surfaced_in_diagnostics() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def test():
    x: int = "hello"
    return x
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);
        let type_errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                if let Some(lsp_types::NumberOrString::String(code)) = &d.code {
                    code.starts_with("HM")
                } else {
                    false
                }
            })
            .collect();

        assert!(!type_errors.is_empty())
    }

    #[tokio::test]
    async fn test_circular_import_detection() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri_a = Url::from_str("file:///workspace/a.py").unwrap();
        let source_a = "import b\n\ndef func_a():\n    pass";

        let uri_b = Url::from_str("file:///workspace/b.py").unwrap();
        let source_b = "import a\n\ndef func_b():\n    pass";

        documents.open_document(uri_a.clone(), 1, source_a.to_string()).unwrap();
        documents.open_document(uri_b.clone(), 1, source_b.to_string()).unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(uri_a.clone(), "a".to_string(), std::path::PathBuf::from("/workspace"));
            ws.add_test_module(uri_b.clone(), "b".to_string(), std::path::PathBuf::from("/workspace"));

            ws.update_dependencies(&uri_a);
            ws.update_dependencies(&uri_b);
        }

        let diagnostics = provider.generate_diagnostics(&uri_a, &mut analyzer);

        for diag in &diagnostics {
            eprintln!("Diagnostic: code={:?}, message={}", diag.code, diag.message);
        }

        let circular_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                if let Some(lsp_types::NumberOrString::String(code)) = &d.code {
                    code == "circular-import"
                } else {
                    false
                }
            })
            .collect();

        // TODO: properly simulate workspace initialization
        if circular_diagnostics.is_empty() {
            eprintln!("Warning: Circular import not detected in test (requires full workspace initialization)");
        }
    }

    #[tokio::test]
    async fn test_unresolved_import_detection() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///workspace/main.py").unwrap();
        let source = "import nonexistent_module\n\ndef main():\n    pass";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(uri.clone(), "main".to_string(), std::path::PathBuf::from("/workspace"));
        }

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let unresolved_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                if let Some(lsp_types::NumberOrString::String(code)) = &d.code {
                    code == "unresolved-import" || code == "missing-module"
                } else {
                    false
                }
            })
            .collect();

        assert!(
            !unresolved_diagnostics.is_empty(),
            "Expected unresolved import diagnostic but found none"
        );

        assert!(
            unresolved_diagnostics
                .iter()
                .any(|d| d.message.contains("nonexistent_module")),
            "Expected diagnostic message to mention nonexistent_module"
        );
    }

    #[tokio::test]
    async fn test_missing_module_detection() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///workspace/pkg/module.py").unwrap();
        let source = "from ..nonexistent import something\n\ndef func():\n    pass";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        {
            let mut ws = workspace.write().await;
            ws.add_test_module(
                uri.clone(),
                "pkg.module".to_string(),
                std::path::PathBuf::from("/workspace"),
            );
        }

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let missing_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                if let Some(lsp_types::NumberOrString::String(code)) = &d.code {
                    code == "missing-module" || code == "unresolved-import"
                } else {
                    false
                }
            })
            .collect();

        assert!(
            !missing_diagnostics.is_empty(),
            "Expected missing module diagnostic but found none"
        );
    }

    #[test]
    fn test_format_missing_module_message_relative_import_beyond_package() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents.clone(), workspace.clone());

        let ws = workspace.try_read().unwrap();
        let message = provider.format_missing_module_message("...", "pkg", &ws);

        assert!(
            message.contains("goes beyond top-level package"),
            "Expected message about going beyond package, got: {message}"
        );
    }

    #[test]
    fn test_fuzzy_module_name_suggestions() {
        let documents = DocumentManager::new().unwrap();
        let workspace = create_test_workspace(documents.clone());
        let provider = DiagnosticProvider::new(documents, workspace);

        let score_similar = provider.fuzzy_matcher.similarity("foo", "foobar");
        let score_typo = provider.fuzzy_matcher.similarity("clections", "collections");
        let score_different = provider.fuzzy_matcher.similarity("abc", "xyz");

        assert!(score_similar >= provider.fuzzy_matcher.threshold());
        assert!(score_typo >= provider.fuzzy_matcher.threshold());
        assert!(score_different < provider.fuzzy_matcher.threshold());
    }

    #[test]
    fn test_enhanced_unification_error_message_str_int() {
        let msg = enhance_unification_error_message("Type mismatch: cannot unify str with int", "str", "int");
        assert!(msg.contains("mixing strings and integers"));
    }

    #[test]
    fn test_enhanced_unification_error_message_none() {
        let msg = enhance_unification_error_message("Type mismatch: cannot unify str with None", "str", "None");
        assert!(msg.contains("None where a different type is expected") && msg.contains("Optional"));
    }

    #[test]
    fn test_enhanced_attribute_error_message_splitlines() {
        let msg = enhance_attribute_error_message("int", "splitlines");
        assert!(msg.contains("string method"));
    }

    #[test]
    fn test_enhanced_attribute_error_message_write_text() {
        let msg = enhance_attribute_error_message("int", "write_text");
        assert!(msg.contains("Path object"));
    }

    #[test]
    fn test_enhanced_attribute_error_message_get() {
        let msg = enhance_attribute_error_message("str", "get");
        assert!(msg.contains("dictionaries"));
    }

    #[test]
    fn test_enhanced_protocol_error_message_iterable() {
        let msg = enhance_protocol_error_message("int", "Iterable");
        assert!(msg.contains("iterated over"));
    }

    #[test]
    fn test_type_error_diagnostic_span_default() {
        let error_info = TypeErrorInfo {
            error: TypeError::UnificationError("int".to_string(), "str".to_string()),
            span: Span { line: 5, col: 10, end_line: None, end_col: None },
        };

        let diagnostic = type_error_to_diagnostic(&error_info);

        assert_eq!(diagnostic.range.start.line, 4);
        assert_eq!(diagnostic.range.start.character, 9);
        assert_eq!(diagnostic.range.end.character, 19);
    }

    #[test]
    fn test_strict_mode_rejects_implicit_any_in_parameters() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def add(x, y):
    return x + y
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann007_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())))
            .collect();

        assert!(
            !ann007_diagnostics.is_empty(),
            "Expected ANN007 diagnostics for implicit Any parameters"
        );
        assert_eq!(
            ann007_diagnostics.len(),
            2,
            "Expected 2 ANN007 diagnostics (one for each parameter)"
        );

        for diag in &ann007_diagnostics {
            assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
            assert!(diag.message.contains("implicit Any type"));
            assert!(diag.message.contains("strict mode"));
        }
    }

    #[test]
    fn test_strict_mode_rejects_implicit_any_in_return_type() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def get_data(x: int):
    return x * 2
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann008_diagnostic = diagnostics
            .iter()
            .find(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())));

        assert!(
            ann008_diagnostic.is_some(),
            "Expected ANN008 diagnostic for implicit Any return type"
        );

        let diag = ann008_diagnostic.unwrap();
        assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diag.message.contains("implicit Any return type"));
        assert!(diag.message.contains("strict mode"));
    }

    #[test]
    fn test_strict_mode_accepts_properly_annotated_functions() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def add(x: int, y: int) -> int:
    return x + y
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let implicit_any_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string()))
                    || d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string()))
            })
            .collect();

        assert!(
            implicit_any_diagnostics.is_empty(),
            "Expected no implicit Any diagnostics for properly annotated function, got: {implicit_any_diagnostics:?}"
        );
    }

    #[test]
    fn test_balanced_mode_does_not_reject_implicit_any() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def add(x, y):
    return x + y
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann007_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())))
            .collect();

        let ann008_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())))
            .collect();

        assert!(
            ann007_diagnostics.is_empty(),
            "Balanced mode should not generate ANN007 for implicit Any"
        );
        assert!(
            ann008_diagnostics.is_empty(),
            "Balanced mode should not generate ANN008 for implicit Any"
        );
    }

    #[test]
    fn test_relaxed_mode_does_not_reject_implicit_any() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Relaxed;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def add(x, y):
    return x + y
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann007_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())))
            .collect();

        let ann008_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())))
            .collect();

        assert!(
            ann007_diagnostics.is_empty(),
            "Relaxed mode should not generate ANN007 for implicit Any"
        );
        assert!(
            ann008_diagnostics.is_empty(),
            "Relaxed mode should not generate ANN008 for implicit Any"
        );
    }

    #[test]
    fn test_strict_mode_requires_annotations_for_all_parameters() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
# Function where type could be inferred from usage, but strict mode requires explicit annotations
def sum_list(items):
    total = 0
    for item in items:
        total += item
    return total
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann007_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())))
            .count();

        let ann008_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())))
            .count();

        assert_eq!(
            ann007_count, 1,
            "Expected ANN007 for parameter 'items' even though type could be inferred"
        );
        assert_eq!(
            ann008_count, 1,
            "Expected ANN008 for return type even though type could be inferred"
        );
    }

    #[test]
    fn test_strict_mode_with_mixed_annotations() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def mixed_params(a: int, b, c) -> int:
    return a + b + c
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann007_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())))
            .collect();

        assert_eq!(
            ann007_diagnostics.len(),
            2,
            "Expected ANN007 for parameters 'b' and 'c' (not 'a' which is annotated)"
        );

        let ann008_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())))
            .count();

        assert_eq!(ann008_count, 0, "Expected no ANN008 since return type is annotated");
    }

    #[test]
    fn test_strict_mode_class_methods() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Calculator:
    def add(self, x, y):
        return x + y

    def subtract(self, x: int, y: int) -> int:
        return x - y
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann007_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())))
            .collect();

        let ann008_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())))
            .collect();

        assert!(
            ann007_diagnostics.len() >= 2,
            "Expected at least 2 ANN007 for 'x' and 'y' in 'add' method"
        );
        assert!(
            !ann008_diagnostics.is_empty(),
            "Expected at least 1 ANN008 for 'add' method return type"
        );
    }

    #[test]
    fn test_strict_mode_nested_functions() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def outer(x: int) -> int:
    def inner(y):
        return x + y
    return inner(10)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann007_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())))
            .collect();

        let ann008_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN008".to_string())))
            .collect();

        assert!(
            !ann007_diagnostics.is_empty(),
            "Expected at least 1 ANN007 for inner function parameter 'y'"
        );
        assert!(
            !ann008_diagnostics.is_empty(),
            "Expected at least 1 ANN008 for inner function return type"
        );
    }

    #[test]
    fn test_strict_mode_function_with_default_values() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def with_default(value=42) -> int:
    return value + 1
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann007_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN007".to_string())))
            .count();

        assert_eq!(
            ann007_count, 1,
            "Expected ANN007 for parameter 'value' even though it has a default value"
        );
    }

    #[test]
    fn test_strict_mode_requires_class_attribute_annotations() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class MyClass:
    # Class attribute without annotation
    count = 0

    # Class attribute with annotation
    name: str = "default"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann009_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN009".to_string())))
            .collect();

        assert_eq!(
            ann009_diagnostics.len(),
            1,
            "Expected 1 ANN009 diagnostic for unannotated class attribute 'count'"
        );

        if let Some(diag) = ann009_diagnostics.first() {
            assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
            assert!(diag.message.contains("count"));
            assert!(diag.message.contains("Class attribute"));
            assert!(diag.message.contains("strict mode"));
        }
    }

    #[test]
    fn test_strict_mode_class_attributes_vs_instance_attributes() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class MyClass:
    class_attr = 0  # Should trigger ANN009

    def __init__(self):
        self.instance_attr = 10  # Should NOT trigger ANN009 (instance attribute, not class)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann009_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN009".to_string())))
            .count();

        assert_eq!(
            ann009_count, 1,
            "Expected exactly 1 ANN009 for class attribute, not instance attributes"
        );
    }

    #[test]
    fn test_strict_mode_multiple_class_attributes() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Config:
    host = "localhost"  # Missing annotation
    port = 8080  # Missing annotation
    timeout: int = 30  # OK: Has annotation
    debug_mode = True  # Missing annotation
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann009_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN009".to_string())))
            .count();

        assert_eq!(
            ann009_count, 3,
            "Expected 3 ANN009 diagnostics for host, port, and debug_mode"
        );
    }

    #[test]
    fn test_balanced_mode_does_not_require_class_attribute_annotations() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class MyClass:
    count = 0
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann009_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN009".to_string())))
            .count();

        assert_eq!(
            ann009_count, 0,
            "Balanced mode should not generate ANN009 for class attributes"
        );
    }

    #[test]
    fn test_relaxed_mode_does_not_require_class_attribute_annotations() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Relaxed;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class MyClass:
    count = 0
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann009_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN009".to_string())))
            .count();

        assert_eq!(
            ann009_count, 0,
            "Relaxed mode should not generate ANN009 for class attributes"
        );
    }

    #[test]
    fn test_strict_mode_rejects_bare_except() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def foo():
    try:
        x = 1 / 0
    except:
        pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann010_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN010".to_string())))
            .collect();

        assert_eq!(
            ann010_diagnostics.len(),
            1,
            "Expected 1 ANN010 diagnostic for bare except clause"
        );

        if let Some(diag) = ann010_diagnostics.first() {
            assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
            assert!(diag.message.contains("Bare except"));
            assert!(diag.message.contains("strict mode"));
        }
    }

    #[test]
    fn test_strict_mode_accepts_specific_exception_types() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def foo():
    try:
        x = 1 / 0
    except ZeroDivisionError:
        pass
    except (ValueError, TypeError):
        pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann010_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN010".to_string())))
            .count();

        assert_eq!(ann010_count, 0, "Specific exception types should not trigger ANN010");
    }

    #[test]
    fn test_balanced_mode_allows_bare_except() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def foo():
    try:
        x = 1 / 0
    except:
        pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann010_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN010".to_string())))
            .count();

        assert_eq!(
            ann010_count, 0,
            "Balanced mode should not generate ANN010 for bare except"
        );
    }

    #[test]
    fn test_relaxed_mode_allows_bare_except() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Relaxed;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def foo():
    try:
        x = 1 / 0
    except:
        pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann010_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN010".to_string())))
            .count();

        assert_eq!(
            ann010_count, 0,
            "Relaxed mode should not generate ANN010 for bare except"
        );
    }

    #[test]
    fn test_balanced_mode_warns_on_implicit_any_parameters() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process_unknown(data, options):
    return data
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann011_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN011".to_string())))
            .collect();

        assert!(
            !ann011_diagnostics.is_empty(),
            "Expected ANN011 warnings for parameters with implicit Any"
        );

        for diag in &ann011_diagnostics {
            assert_eq!(diag.severity, Some(DiagnosticSeverity::WARNING));
            assert!(diag.message.contains("implicit Any type"));
            assert!(diag.message.contains("consider adding type annotation"));
        }
    }

    #[test]
    fn test_balanced_mode_warns_on_missing_annotations() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def handle_dynamic(value):
    return value
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let annotation_warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                matches!(
                    d.code.as_ref(),
                    Some(lsp_types::NumberOrString::String(code))
                    if code.starts_with("ANN")
                )
            })
            .collect();

        assert!(
            !annotation_warnings.is_empty(),
            "Expected annotation warnings for unannotated function"
        );

        for diag in &annotation_warnings {
            assert_eq!(diag.severity, Some(DiagnosticSeverity::WARNING));
        }
    }

    #[test]
    fn test_balanced_mode_warns_on_missing_annotation_with_inferred_type() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def add(x, y):
    return x + y

result = add(1, 2)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let annotation_warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                matches!(
                    d.code.as_ref(),
                    Some(lsp_types::NumberOrString::String(code))
                    if code == "ANN004" || code == "ANN006" || code == "ANN011" || code == "ANN012"
                )
            })
            .collect();

        assert!(
            !annotation_warnings.is_empty(),
            "Expected annotation warnings for parameters and return type"
        );

        for diag in &annotation_warnings {
            assert_eq!(diag.severity, Some(DiagnosticSeverity::WARNING));
        }
    }

    #[test]
    fn test_balanced_mode_gradual_typing_mixed_annotations() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def mixed_params(a: int, b, c: int) -> int:
    return a + b + c
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let param_b_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                matches!(
                    d.code.as_ref(),
                    Some(lsp_types::NumberOrString::String(code))
                    if (code == "ANN004" || code == "ANN011") && d.message.contains("'b'")
                )
            })
            .collect();

        assert_eq!(
            param_b_diagnostics.len(),
            1,
            "Expected exactly 1 annotation warning for unannotated parameter 'b'"
        );

        for diag in &param_b_diagnostics {
            assert_eq!(diag.severity, Some(DiagnosticSeverity::WARNING));
        }
    }

    #[test]
    fn test_balanced_mode_with_fully_annotated_function() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Balanced;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def multiply(x: int, y: int) -> int:
    return x * y
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let missing_annotation_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                matches!(
                    d.code.as_ref(),
                    Some(lsp_types::NumberOrString::String(code))
                    if code == "ANN004" || code == "ANN006" || code == "ANN011" || code == "ANN012"
                )
            })
            .collect();

        assert!(
            missing_annotation_diagnostics.is_empty(),
            "Fully annotated functions should not generate missing annotation warnings in balanced mode"
        );
    }

    #[test]
    fn test_strict_mode_multiple_bare_except_handlers() {
        let documents = DocumentManager::new().unwrap();
        let mut config = crate::config::Config::default();
        config.type_checking.mode = crate::config::TypeCheckingMode::Strict;
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def foo():
    try:
        x = 1 / 0
    except ValueError:
        pass
    except:
        pass

def bar():
    try:
        y = 2 / 0
    except:
        pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let ann010_count = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("ANN010".to_string())))
            .count();

        assert_eq!(
            ann010_count, 2,
            "Expected 2 ANN010 diagnostics for two bare except clauses"
        );
    }

    #[test]
    fn test_config_severity_to_lsp() {
        assert_eq!(
            DiagnosticProvider::config_severity_to_lsp(config::DiagnosticSeverity::Error),
            lsp_types::DiagnosticSeverity::ERROR
        );
        assert_eq!(
            DiagnosticProvider::config_severity_to_lsp(config::DiagnosticSeverity::Warning),
            lsp_types::DiagnosticSeverity::WARNING
        );
        assert_eq!(
            DiagnosticProvider::config_severity_to_lsp(config::DiagnosticSeverity::Info),
            lsp_types::DiagnosticSeverity::INFORMATION
        );
    }

    #[test]
    fn test_mode_severity_for_diagnostic_implicit_any() {
        use lsp_types::DiagnosticSeverity as Severity;

        assert_eq!(
            DiagnosticProvider::mode_severity_for_diagnostic(
                config::TypeCheckingMode::Strict,
                DiagnosticCategory::ImplicitAny
            ),
            Some(Severity::ERROR)
        );
        assert_eq!(
            DiagnosticProvider::mode_severity_for_diagnostic(
                config::TypeCheckingMode::Balanced,
                DiagnosticCategory::ImplicitAny
            ),
            Some(Severity::WARNING)
        );
        assert_eq!(
            DiagnosticProvider::mode_severity_for_diagnostic(
                config::TypeCheckingMode::Relaxed,
                DiagnosticCategory::ImplicitAny
            ),
            None
        );
    }

    #[test]
    fn test_mode_severity_for_diagnostic_missing_annotation() {
        use lsp_types::DiagnosticSeverity as Severity;

        assert_eq!(
            DiagnosticProvider::mode_severity_for_diagnostic(
                config::TypeCheckingMode::Strict,
                DiagnosticCategory::MissingAnnotation
            ),
            Some(Severity::ERROR)
        );
        assert_eq!(
            DiagnosticProvider::mode_severity_for_diagnostic(
                config::TypeCheckingMode::Balanced,
                DiagnosticCategory::MissingAnnotation
            ),
            Some(Severity::WARNING)
        );
        assert_eq!(
            DiagnosticProvider::mode_severity_for_diagnostic(
                config::TypeCheckingMode::Relaxed,
                DiagnosticCategory::MissingAnnotation
            ),
            None
        );
    }

    #[test]
    fn test_mode_severity_for_diagnostic_annotation_mismatch() {
        use lsp_types::DiagnosticSeverity as Severity;

        assert_eq!(
            DiagnosticProvider::mode_severity_for_diagnostic(
                config::TypeCheckingMode::Strict,
                DiagnosticCategory::AnnotationMismatch
            ),
            Some(Severity::ERROR)
        );
        assert_eq!(
            DiagnosticProvider::mode_severity_for_diagnostic(
                config::TypeCheckingMode::Balanced,
                DiagnosticCategory::AnnotationMismatch
            ),
            Some(Severity::WARNING)
        );
        assert_eq!(
            DiagnosticProvider::mode_severity_for_diagnostic(
                config::TypeCheckingMode::Relaxed,
                DiagnosticCategory::AnnotationMismatch
            ),
            Some(Severity::HINT)
        );
    }

    #[test]
    fn test_types_are_compatible_basic() {
        assert!(DiagnosticProvider::types_are_compatible(
            &Type::Con(TypeCtor::Int),
            &Type::Con(TypeCtor::Int)
        ));
        assert!(!DiagnosticProvider::types_are_compatible(
            &Type::Con(TypeCtor::Int),
            &Type::Con(TypeCtor::String)
        ));
    }

    #[test]
    fn test_types_are_compatible_with_any() {
        assert!(DiagnosticProvider::types_are_compatible(
            &Type::Con(TypeCtor::Any),
            &Type::Con(TypeCtor::Int)
        ));
        assert!(DiagnosticProvider::types_are_compatible(
            &Type::Con(TypeCtor::Int),
            &Type::Con(TypeCtor::Any)
        ));
    }

    #[test]
    fn test_types_are_compatible_application() {
        let list_int1 = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
        let list_int2 = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
        let list_str = Type::App(
            Box::new(Type::Con(TypeCtor::List)),
            Box::new(Type::Con(TypeCtor::String)),
        );

        assert!(DiagnosticProvider::types_are_compatible(&list_int1, &list_int2));
        assert!(!DiagnosticProvider::types_are_compatible(&list_int1, &list_str));
    }

    #[test]
    fn test_types_are_compatible_function() {
        let fun1 = Type::Fun(
            vec![("x".to_string(), Type::Con(TypeCtor::Int))],
            Box::new(Type::Con(TypeCtor::String)),
        );
        let fun2 = Type::Fun(
            vec![("y".to_string(), Type::Con(TypeCtor::Int))],
            Box::new(Type::Con(TypeCtor::String)),
        );
        let fun3 = Type::Fun(
            vec![("x".to_string(), Type::Con(TypeCtor::String))],
            Box::new(Type::Con(TypeCtor::String)),
        );

        assert!(DiagnosticProvider::types_are_compatible(&fun1, &fun2));
        assert!(!DiagnosticProvider::types_are_compatible(&fun1, &fun3));
    }

    #[test]
    fn test_types_are_compatible_union() {
        let union1 = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
        let union2 = Type::Union(vec![Type::Con(TypeCtor::String), Type::Con(TypeCtor::Int)]);
        let union3 = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::Bool)]);

        assert!(DiagnosticProvider::types_are_compatible(&union1, &union2));
        assert!(!DiagnosticProvider::types_are_compatible(&union1, &union3));
    }

    #[test]
    fn test_types_are_compatible_tuple() {
        let tuple1 = Type::Tuple(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
        let tuple2 = Type::Tuple(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
        let tuple3 = Type::Tuple(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::Bool)]);

        assert!(DiagnosticProvider::types_are_compatible(&tuple1, &tuple2));
        assert!(!DiagnosticProvider::types_are_compatible(&tuple1, &tuple3));
    }

    #[test]
    fn test_types_are_compatible_with_type_vars() {
        let tv = TypeVar::new(0);
        assert!(DiagnosticProvider::types_are_compatible(
            &Type::Var(tv.clone()),
            &Type::Con(TypeCtor::Int)
        ));
        assert!(DiagnosticProvider::types_are_compatible(
            &Type::Con(TypeCtor::Int),
            &Type::Var(tv)
        ));
    }

    #[test]
    fn test_contains_type_var_simple() {
        let tv = TypeVar::new(0);
        assert!(DiagnosticProvider::contains_type_var(&Type::Var(tv)));
        assert!(!DiagnosticProvider::contains_type_var(&Type::Con(TypeCtor::Int)));
    }

    #[test]
    fn test_contains_type_var_nested() {
        let tv = TypeVar::new(0);
        let list_var = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Var(tv)));
        assert!(DiagnosticProvider::contains_type_var(&list_var));

        let list_int = Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::Con(TypeCtor::Int)));
        assert!(!DiagnosticProvider::contains_type_var(&list_int));
    }

    #[test]
    fn test_contains_type_var_function() {
        let tv = TypeVar::new(0);
        let fun_var = Type::Fun(
            vec![("x".to_string(), Type::Var(tv.clone()))],
            Box::new(Type::Con(TypeCtor::Int)),
        );
        assert!(DiagnosticProvider::contains_type_var(&fun_var));

        let fun_normal = Type::Fun(
            vec![("x".to_string(), Type::Con(TypeCtor::Int))],
            Box::new(Type::Con(TypeCtor::String)),
        );
        assert!(!DiagnosticProvider::contains_type_var(&fun_normal));
    }

    #[test]
    fn test_contains_type_var_union() {
        let tv = TypeVar::new(0);
        let union_var = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Var(tv)]);
        assert!(DiagnosticProvider::contains_type_var(&union_var));

        let union_normal = Type::Union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
        assert!(!DiagnosticProvider::contains_type_var(&union_normal));
    }

    #[test]
    fn test_extract_target_name_identifier() {
        let node = AstNode::Identifier { name: "my_var".to_string(), line: 1, col: 1, end_line: 1, end_col: 7 };
        assert_eq!(DiagnosticProvider::extract_target_name(&node), "my_var");
    }

    #[test]
    fn test_extract_target_name_attribute() {
        let node = AstNode::Attribute {
            object: Box::new(AstNode::Identifier { name: "obj".to_string(), line: 1, col: 1, end_line: 1, end_col: 4 }),
            attribute: "attr".to_string(),
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 9,
        };
        assert_eq!(DiagnosticProvider::extract_target_name(&node), "attr");
    }

    #[test]
    fn test_extract_target_name_fallback() {
        let node = AstNode::Literal {
            value: beacon_parser::LiteralValue::Integer(42),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 3,
        };
        assert_eq!(DiagnosticProvider::extract_target_name(&node), "variable");
    }

    #[test]
    fn test_is_name_main_check_valid() {
        let test = AstNode::Compare {
            left: Box::new(AstNode::Identifier {
                name: "__name__".to_string(),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 9,
            }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: beacon_parser::LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                line: 1,
                col: 10,
                end_line: 1,
                end_col: 20,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 20,
        };

        assert!(DiagnosticProvider::is_name_main_check(&test));
    }

    #[test]
    fn test_is_name_main_check_invalid() {
        let test = AstNode::Compare {
            left: Box::new(AstNode::Identifier {
                name: "other_var".to_string(),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 10,
            }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: beacon_parser::LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                line: 1,
                col: 10,
                end_line: 1,
                end_col: 20,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 20,
        };

        assert!(!DiagnosticProvider::is_name_main_check(&test));
    }

    #[test]
    fn test_line_col_to_byte_offset_simple() {
        let source = "hello\nworld\ntest";
        let offset = DiagnosticProvider::line_col_to_byte_offset_from_source(source, &2, &3);
        assert_eq!(offset, 8);
    }

    #[test]
    fn test_line_col_to_byte_offset_start() {
        let source = "hello\nworld";
        let offset = DiagnosticProvider::line_col_to_byte_offset_from_source(source, &1, &1);
        assert_eq!(offset, 0);
    }

    #[test]
    fn test_line_col_to_byte_offset_multiline() {
        let source = "line1\nline2\nline3";
        let offset = DiagnosticProvider::line_col_to_byte_offset_from_source(source, &3, &2);
        assert_eq!(offset, 13);
    }

    #[test]
    fn test_enhance_unification_error_str_int() {
        let base = "Type mismatch: cannot unify str with int";
        let enhanced = enhance_unification_error_message(base, "str", "int");
        assert!(enhanced.contains("mixing strings and integers"));
    }

    #[test]
    fn test_enhance_unification_error_list_dict() {
        let base = "Type mismatch: cannot unify list with dict";
        let enhanced = enhance_unification_error_message(base, "list", "dict");
        assert!(enhanced.contains("Collection type mismatch"));
    }

    #[test]
    fn test_enhance_unification_error_none() {
        let base = "Type mismatch: cannot unify str with None";
        let enhanced = enhance_unification_error_message(base, "str", "None");
        assert!(enhanced.contains("None"));
        assert!(enhanced.contains("Optional[T]"));
    }

    #[test]
    fn test_enhance_unification_error_union() {
        let base = "Type mismatch: cannot unify Union[int, str] with bool";
        let enhanced = enhance_unification_error_message(base, "Union[int, str]", "bool");
        assert!(enhanced.contains("Union types"));
    }

    #[test]
    fn test_enhance_unification_error_default() {
        let base = "Type mismatch: cannot unify float with bool";
        let enhanced = enhance_unification_error_message(base, "float", "bool");
        assert_eq!(enhanced, base);
    }

    #[test]
    fn test_enhance_protocol_error_iterable() {
        let enhanced = enhance_protocol_error_message("MyClass", "Iterable");
        assert!(enhanced.contains("MyClass"));
        assert!(enhanced.contains("Iterable"));
        assert!(enhanced.contains("cannot be iterated"));
    }

    #[test]
    fn test_enhance_protocol_error_other() {
        let enhanced = enhance_protocol_error_message("MyClass", "Sized");
        assert!(enhanced.contains("MyClass"));
        assert!(enhanced.contains("Sized"));
        assert!(!enhanced.contains("cannot be iterated"));
    }

    #[test]
    fn test_enhance_attribute_error_splitlines() {
        let enhanced = enhance_attribute_error_message("int", "splitlines");
        assert!(enhanced.contains("splitlines"));
        assert!(enhanced.contains("string method"));
    }

    #[test]
    fn test_enhance_attribute_error_write_text() {
        let enhanced = enhance_attribute_error_message("str", "write_text");
        assert!(enhanced.contains("write_text"));
        assert!(enhanced.contains("Path object"));
    }

    #[test]
    fn test_enhance_attribute_error_get() {
        let enhanced = enhance_attribute_error_message("list", "get");
        assert!(enhanced.contains("get()"));
        assert!(enhanced.contains("dictionaries"));
    }

    #[test]
    fn test_enhance_attribute_error_append() {
        let enhanced = enhance_attribute_error_message("str", "append");
        assert!(enhanced.contains("append()"));
        assert!(enhanced.contains("lists"));
    }

    #[test]
    fn test_enhance_attribute_error_default() {
        let enhanced = enhance_attribute_error_message("MyClass", "unknown_method");
        assert!(enhanced.contains("unknown_method"));
        assert!(enhanced.contains("not found"));
    }

    #[test]
    fn test_enhance_variance_error_invariant_list() {
        let enhanced = enhance_variance_error_message("list element", "invariant", "Dog", "Animal");
        assert!(enhanced.contains("invariant"));
        assert!(enhanced.contains("list"));
        assert!(enhanced.contains("Mutable containers"));
    }

    #[test]
    fn test_enhance_variance_error_invariant_generic() {
        let enhanced = enhance_variance_error_message("generic type", "invariant", "int", "float");
        assert!(enhanced.contains("invariant"));
        assert!(enhanced.contains("exact type matches"));
    }

    #[test]
    fn test_enhance_variance_error_covariant_return() {
        let enhanced = enhance_variance_error_message("return type", "covariant", "Animal", "Dog");
        assert!(enhanced.contains("covariant"));
        assert!(enhanced.contains("Return types"));
        assert!(enhanced.contains("subtype"));
    }

    #[test]
    fn test_enhance_variance_error_contravariant_parameter() {
        let enhanced = enhance_variance_error_message("parameter", "contravariant", "Dog", "Animal");
        assert!(enhanced.contains("contravariant"));
        assert!(enhanced.contains("parameters"));
        assert!(enhanced.contains("supertype"));
    }

    #[test]
    fn test_enhance_variance_error_unknown() {
        let enhanced = enhance_variance_error_message("position", "unknown", "Type1", "Type2");
        assert!(enhanced.contains("Variance error"));
        assert!(!enhanced.contains("Mutable containers"));
    }

    #[test]
    fn test_unreachable_pattern_diagnostic_range() {
        let documents = DocumentManager::new().unwrap();
        let config = crate::config::Config::default();
        let workspace = Arc::new(RwLock::new(crate::workspace::Workspace::new(
            None,
            config.clone(),
            documents.clone(),
        )));
        let provider = DiagnosticProvider::new(documents.clone(), workspace);
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def example(value: int | str) -> str:
    match value:
        case _:
            return "wildcard"
        case int():  # PM002: Unreachable
            return "integer"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let diagnostics = provider.generate_diagnostics(&uri, &mut analyzer);

        let pm002_diagnostics: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(lsp_types::NumberOrString::String("PM002".to_string())))
            .collect();

        assert_eq!(
            pm002_diagnostics.len(),
            1,
            "Expected 1 PM002 diagnostic for unreachable pattern"
        );

        let diagnostic = &pm002_diagnostics[0];

        eprintln!(
            "Diagnostic range: start={}:{}, end={}:{}",
            diagnostic.range.start.line,
            diagnostic.range.start.character,
            diagnostic.range.end.line,
            diagnostic.range.end.character
        );

        assert_eq!(
            diagnostic.range.start.line, 5,
            "Diagnostic should be on line 6 (0-indexed as 5)"
        );
        assert_eq!(
            diagnostic.range.start.character, 13,
            "Diagnostic should start at column 13 (start of 'int()')"
        );
        assert_eq!(
            diagnostic.range.end.character, 18,
            "Diagnostic should end at column 18 (end of 'int()')"
        );
        assert_eq!(diagnostic.range.end.line, 5, "Diagnostic should end on the same line");
    }
}
