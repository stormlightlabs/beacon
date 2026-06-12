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
use beacon_core::SuppressionMap;
use beacon_core::TypeError;
use beacon_core::{Type, TypeCtor};
use beacon_parser::{AstNode, LiteralValue, MAGIC_METHODS, SymbolTable, line_col_to_byte_offset_lossy};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use url::Url;

#[cfg(test)]
mod tests;

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

            for (chars_seen, (idx, _)) in (1..).zip(line_text.char_indices()) {
                if chars_seen == column {
                    return idx;
                }
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
        match (mode, category) {
            (config::TypeCheckingMode::Strict, DiagnosticCategory::ImplicitAny) => Some(DiagnosticSeverity::ERROR),
            (config::TypeCheckingMode::Balanced, DiagnosticCategory::ImplicitAny) => Some(DiagnosticSeverity::WARNING),
            (config::TypeCheckingMode::Relaxed, DiagnosticCategory::ImplicitAny) => None,
            (config::TypeCheckingMode::Strict, DiagnosticCategory::MissingAnnotation) => {
                Some(DiagnosticSeverity::ERROR)
            }
            (config::TypeCheckingMode::Balanced, DiagnosticCategory::MissingAnnotation) => {
                Some(DiagnosticSeverity::WARNING)
            }
            (config::TypeCheckingMode::Relaxed, DiagnosticCategory::MissingAnnotation) => None,
            (config::TypeCheckingMode::Strict, DiagnosticCategory::AnnotationMismatch) => {
                Some(DiagnosticSeverity::ERROR)
            }
            (config::TypeCheckingMode::Balanced, DiagnosticCategory::AnnotationMismatch) => {
                Some(DiagnosticSeverity::WARNING)
            }
            (config::TypeCheckingMode::Relaxed, DiagnosticCategory::AnnotationMismatch) => {
                Some(DiagnosticSeverity::HINT)
            }
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
        self.add_dynamic_python_diagnostics(uri, effective_mode, &mut diagnostics);
        tracing::trace!(
            "Dynamic Python diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

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

        let start = std::time::Instant::now();
        self.add_invalid_symbol_import_diagnostics(uri, &mut diagnostics);
        tracing::trace!(
            "Invalid symbol import diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        let start = std::time::Instant::now();
        self.add_private_symbol_import_diagnostics(uri, &mut diagnostics);
        tracing::trace!(
            "Private symbol import diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        let start = std::time::Instant::now();
        self.add_reexport_chain_diagnostics(uri, &mut diagnostics);
        tracing::trace!(
            "Re-export chain diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        let start = std::time::Instant::now();
        self.add_cross_module_type_mismatch_diagnostics(uri, analyzer, &mut diagnostics);
        tracing::trace!(
            "Cross-module type mismatch diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        let start = std::time::Instant::now();
        self.add_cross_file_dead_code_diagnostics(uri, &mut diagnostics);
        tracing::trace!(
            "Cross-file dead code diagnostics: {} ({:?})",
            diagnostics.len(),
            start.elapsed()
        );

        diagnostics = self.apply_suppressions(uri, diagnostics);

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

    fn apply_suppressions(&self, uri: &Url, diagnostics: Vec<Diagnostic>) -> Vec<Diagnostic> {
        let Some(suppression_map) = self
            .documents
            .get_document(uri, |doc| SuppressionMap::from_source(&doc.text()))
        else {
            return diagnostics;
        };

        diagnostics
            .into_iter()
            .filter(|diagnostic| !Self::is_suppressed_diagnostic(&suppression_map, diagnostic))
            .collect()
    }

    fn is_suppressed_diagnostic(suppression_map: &SuppressionMap, diagnostic: &Diagnostic) -> bool {
        let line = diagnostic.range.start.line as usize + 1;
        let code = Self::diagnostic_code(diagnostic);

        if diagnostic.source.as_deref() == Some("beacon-linter") {
            suppression_map.is_lint_suppressed(line, code)
        } else {
            suppression_map.is_type_suppressed(line, code)
        }
    }

    fn diagnostic_code(diagnostic: &Diagnostic) -> Option<&str> {
        match diagnostic.code.as_ref()? {
            lsp_types::NumberOrString::String(code) => Some(code.as_str()),
            lsp_types::NumberOrString::Number(_) => None,
        }
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
            Err(e) => diagnostics.push(analysis_error_into_diagnostic(&e)),
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
            let contains_any = Self::contains_any_type(ty, 0);
            let contains_unknown = Self::contains_unknown_type(ty, 0);
            if contains_any || contains_unknown {
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
                    message: if contains_unknown && !contains_any {
                        "Type 'Unknown' detected - Beacon could not infer a precise type".to_string()
                    } else {
                        "Type 'Any' detected - this reduces type safety".to_string()
                    },
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

    fn contains_unknown_type(ty: &beacon_core::Type, _depth: u32) -> bool {
        match ty {
            Type::Con(TypeCtor::Unknown) => true,
            Type::App(t1, t2) => {
                Self::contains_unknown_type(t1, _depth + 1) || Self::contains_unknown_type(t2, _depth + 1)
            }
            Type::Fun(args, ret) => {
                args.iter().any(|(_, arg)| Self::contains_unknown_type(arg, _depth + 1))
                    || Self::contains_unknown_type(ret, _depth + 1)
            }
            Type::FunWithParams(params, ret) => {
                params
                    .iter()
                    .any(|param| Self::contains_unknown_type(&param.ty, _depth + 1))
                    || Self::contains_unknown_type(ret, _depth + 1)
            }
            Type::Union(types) | Type::Intersection(types) | Type::Tuple(types) => {
                types.iter().any(|t| Self::contains_unknown_type(t, _depth + 1))
            }
            Type::Record(fields, _) => fields.iter().any(|(_, t)| Self::contains_unknown_type(t, _depth + 1)),
            Type::ForAll(_, body) => Self::contains_unknown_type(body, _depth + 1),
            Type::BoundMethod(receiver, _, method) => {
                Self::contains_unknown_type(receiver, _depth + 1) || Self::contains_unknown_type(method, _depth + 1)
            }
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
        if matches!(inferred_type, Type::Con(TypeCtor::Any | TypeCtor::Unknown))
            || Self::contains_type_var(inferred_type)
        {
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
            function
                .qualified_name()
                .is_some_and(|func_name| func_name == "TypeVar" || func_name.ends_with(".TypeVar"))
        } else {
            false
        }
    }

    /// Check if two types are compatible (structural comparison)
    fn types_are_compatible(annotated: &Type, inferred: &Type) -> bool {
        use Type::*;

        match (annotated, inferred) {
            (Con(a), Con(b)) if a == b => true,
            (Con(TypeCtor::Any | TypeCtor::Unknown), _) | (_, Con(TypeCtor::Any | TypeCtor::Unknown)) => true,
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
                        Some(Type::Con(TypeCtor::Any | TypeCtor::Unknown)) => {
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
                        Type::Fun(_, ret) | Type::FunWithParams(_, ret) => (**ret).clone(),
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
                        Type::Fun(_, ret) | Type::FunWithParams(_, ret) => *ret,
                        other => other,
                    });

                    match return_type_opt {
                        Some(Type::Con(TypeCtor::Any | TypeCtor::Unknown)) => {
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
            AstNode::FunctionDef { name, line, col, body, args, .. } => {
                if MAGIC_METHODS.contains(&name.as_str()) {
                    let byte_offset = line_col_to_byte_offset_lossy(source, *line, *col);
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
                        self.validate_magic_method_signature(name, args, line, col, diagnostics);
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

    /// Validate magic method signatures against expected parameter counts and types
    fn validate_magic_method_signature(
        &self, name: &str, args: &[beacon_parser::Parameter], line: &usize, col: &usize,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        let expected_params = match name {
            "__init__" => Some((1, None, "self")),
            "__new__" => Some((1, None, "cls")),
            "__repr__" | "__str__" | "__bytes__" | "__format__" => Some((1, Some(1), "self")),
            "__int__" | "__float__" | "__complex__" | "__bool__" | "__index__" | "__round__" => {
                Some((1, Some(1), "self"))
            }
            "__len__" | "__iter__" | "__next__" => Some((1, Some(1), "self")),
            "__getitem__" | "__delitem__" => Some((2, Some(2), "self")),
            "__setitem__" => Some((3, Some(3), "self")),
            "__eq__" | "__ne__" | "__lt__" | "__le__" | "__gt__" | "__ge__" => Some((2, Some(2), "self")),
            "__add__" | "__sub__" | "__mul__" | "__truediv__" | "__floordiv__" | "__mod__" | "__pow__" | "__radd__"
            | "__rand__" => Some((2, Some(2), "self")),
            "__neg__" | "__pos__" | "__abs__" => Some((1, Some(1), "self")),
            "__enter__" => Some((1, Some(1), "self")),
            "__exit__" => Some((4, Some(4), "self")),
            "__call__" => Some((1, None, "self")),
            "__getattr__" | "__setattr__" | "__delattr__" | "__getattribute__" => Some((2, None, "self")),
            "__hash__" => Some((1, Some(1), "self")),
            "__copy__" => Some((1, Some(1), "self")),
            "__deepcopy__" => Some((2, Some(2), "self")),
            "__reduce__" | "__reduce_ex__" | "__getnewargs__" => Some((1, None, "self")),
            "__sizeof__" | "__dir__" => Some((1, Some(1), "self")),
            "__ior__" | "__imul__" => Some((2, Some(2), "self")),
            _ => None,
        };

        if let Some((min_params, max_params, expected_first_param)) = expected_params {
            let actual_param_count = args.len();
            let has_count_mismatch = if let Some(max) = max_params {
                actual_param_count < min_params || actual_param_count > max
            } else {
                actual_param_count < min_params
            };

            let has_first_param_mismatch = args
                .first()
                .is_none_or(|first_param| first_param.name != expected_first_param);

            if has_count_mismatch || has_first_param_mismatch {
                let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };
                let range = Range {
                    start: position,
                    end: Position { line: position.line, character: position.character + name.len() as u32 },
                };

                let message = if has_count_mismatch && has_first_param_mismatch {
                    let max_desc = max_params.map_or_else(|| "+".to_string(), |m| m.to_string());
                    format!(
                        "Magic method '{}' has {} parameter{} but expected {} (first parameter should be '{}')",
                        name,
                        actual_param_count,
                        if actual_param_count == 1 { "" } else { "s" },
                        if min_params == max_params.unwrap_or(min_params) {
                            min_params.to_string()
                        } else {
                            format!("{}-{}", min_params, max_desc)
                        },
                        expected_first_param
                    )
                } else if has_count_mismatch {
                    let max_desc = max_params.map_or_else(|| "+".to_string(), |m| m.to_string());
                    format!(
                        "Magic method '{}' has {} parameter{} but expected {}",
                        name,
                        actual_param_count,
                        if actual_param_count == 1 { "" } else { "s" },
                        if min_params == max_params.unwrap_or(min_params) {
                            min_params.to_string()
                        } else {
                            format!("{}-{}", min_params, max_desc)
                        }
                    )
                } else {
                    format!(
                        "Magic method '{}' first parameter should be '{}' not '{}'",
                        name,
                        expected_first_param,
                        args.first().map_or("", |p| p.name.as_str())
                    )
                };

                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(lsp_types::NumberOrString::String("DUNDER002".to_string())),
                    source: Some("beacon".to_string()),
                    message,
                    related_information: None,
                    tags: None,
                    data: None,
                    code_description: None,
                });
            }
        }
    }

    fn add_dynamic_python_diagnostics(
        &self, uri: &Url, mode: config::TypeCheckingMode, diagnostics: &mut Vec<Diagnostic>,
    ) {
        let severity = match mode {
            config::TypeCheckingMode::Strict => DiagnosticSeverity::ERROR,
            config::TypeCheckingMode::Balanced => DiagnosticSeverity::WARNING,
            config::TypeCheckingMode::Relaxed => DiagnosticSeverity::HINT,
        };

        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                Self::check_dynamic_python_patterns(ast, severity, diagnostics);
            }
        });
    }

    fn check_dynamic_python_patterns(node: &AstNode, severity: DiagnosticSeverity, diagnostics: &mut Vec<Diagnostic>) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::check_dynamic_python_patterns(stmt, severity, diagnostics);
                }
            }
            AstNode::FunctionDef { decorators, body, line, col, name, .. } => {
                for decorator in decorators {
                    if Self::is_dynamic_decorator(decorator) {
                        Self::push_dynamic_diagnostic(
                            diagnostics,
                            severity,
                            *line,
                            *col,
                            name.len().max(1),
                            "decorator may replace the function object at runtime",
                        );
                    }
                }
                for stmt in body {
                    Self::check_dynamic_python_patterns(stmt, severity, diagnostics);
                }
            }
            AstNode::ClassDef { decorators, metaclass, body, line, col, name, .. } => {
                if let Some(meta) = metaclass
                    && meta != "type"
                {
                    Self::push_dynamic_diagnostic(
                        diagnostics,
                        severity,
                        *line,
                        *col,
                        name.len().max(1),
                        "custom metaclass can change class creation semantics",
                    );
                }
                for decorator in decorators {
                    if Self::is_dynamic_decorator(decorator) {
                        Self::push_dynamic_diagnostic(
                            diagnostics,
                            severity,
                            *line,
                            *col,
                            name.len().max(1),
                            "decorator may replace the class object at runtime",
                        );
                    }
                }
                for stmt in body {
                    Self::check_dynamic_python_patterns(stmt, severity, diagnostics);
                }
            }
            AstNode::Assignment { target, value, line, col, .. } => {
                if let Some(message) = Self::dynamic_assignment_message(target) {
                    Self::push_dynamic_diagnostic(diagnostics, severity, *line, *col, 1, message);
                }
                Self::check_dynamic_python_patterns(value, severity, diagnostics);
            }
            AstNode::AnnotatedAssignment { target, value, line, col, .. } => {
                if let Some(message) = Self::dynamic_assignment_message(target) {
                    Self::push_dynamic_diagnostic(diagnostics, severity, *line, *col, 1, message);
                }
                if let Some(value) = value {
                    Self::check_dynamic_python_patterns(value, severity, diagnostics);
                }
            }
            AstNode::Call { function, args, keywords, line, col, .. } => {
                if let Some((name, len)) = Self::dynamic_call_name(function)
                    && let Some(message) = Self::dynamic_call_message(&name, args)
                {
                    Self::push_dynamic_diagnostic(diagnostics, severity, *line, *col, len.max(1), message);
                }

                Self::check_dynamic_python_patterns(function, severity, diagnostics);
                for arg in args {
                    Self::check_dynamic_python_patterns(arg, severity, diagnostics);
                }
                for (_name, value) in keywords {
                    Self::check_dynamic_python_patterns(value, severity, diagnostics);
                }
            }
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                Self::check_dynamic_python_patterns(test, severity, diagnostics);
                for stmt in body {
                    Self::check_dynamic_python_patterns(stmt, severity, diagnostics);
                }
                for (test, body) in elif_parts {
                    Self::check_dynamic_python_patterns(test, severity, diagnostics);
                    for stmt in body {
                        Self::check_dynamic_python_patterns(stmt, severity, diagnostics);
                    }
                }
                if let Some(else_body) = else_body {
                    for stmt in else_body {
                        Self::check_dynamic_python_patterns(stmt, severity, diagnostics);
                    }
                }
            }
            AstNode::For { target, iter, body, else_body, .. } => {
                Self::check_dynamic_python_patterns(target, severity, diagnostics);
                Self::check_dynamic_python_patterns(iter, severity, diagnostics);
                for stmt in body {
                    Self::check_dynamic_python_patterns(stmt, severity, diagnostics);
                }
                if let Some(else_body) = else_body {
                    for stmt in else_body {
                        Self::check_dynamic_python_patterns(stmt, severity, diagnostics);
                    }
                }
            }
            AstNode::While { test, body, else_body, .. } => {
                Self::check_dynamic_python_patterns(test, severity, diagnostics);
                for stmt in body {
                    Self::check_dynamic_python_patterns(stmt, severity, diagnostics);
                }
                if let Some(else_body) = else_body {
                    for stmt in else_body {
                        Self::check_dynamic_python_patterns(stmt, severity, diagnostics);
                    }
                }
            }
            AstNode::Return { value: Some(value), .. } => {
                Self::check_dynamic_python_patterns(value, severity, diagnostics);
            }
            AstNode::Attribute { object, .. } => {
                Self::check_dynamic_python_patterns(object, severity, diagnostics);
            }
            _ => {}
        }
    }

    fn dynamic_call_name(function: &AstNode) -> Option<(String, usize)> {
        Self::attribute_path(function).map(|name| {
            let len = name.len();
            (name, len)
        })
    }

    fn attribute_path(node: &AstNode) -> Option<String> {
        match node {
            AstNode::Identifier { name, .. } => Some(name.clone()),
            AstNode::Attribute { object, attribute, .. } => {
                Some(format!("{}.{}", Self::attribute_path(object)?, attribute))
            }
            _ => None,
        }
    }

    fn dynamic_call_message(name: &str, args: &[AstNode]) -> Option<&'static str> {
        match name {
            "eval" | "exec" | "compile" => Some("generated code crosses a dynamic execution boundary"),
            "__import__" | "importlib.import_module" => Some("dynamic import cannot be resolved statically"),
            "setattr" | "delattr" => Some("runtime attribute mutation cannot be modeled precisely"),
            "__all__.append" | "__all__.extend" => Some("runtime __all__ mutation changes exported names dynamically"),
            "sys.meta_path.append" | "sys.meta_path.insert" | "sys.path_hooks.append" | "sys.path_hooks.insert" => {
                Some("custom import hooks can change module resolution dynamically")
            }
            "sys.path.append" | "sys.path.insert" => {
                Some("runtime sys.path mutation can change import resolution dynamically")
            }
            "getattr" => {
                if args.get(1).is_some_and(|arg| {
                    !matches!(
                        arg,
                        AstNode::Literal { value: beacon_parser::LiteralValue::String { .. }, .. }
                    )
                }) {
                    Some("reflective attribute lookup falls back to Any/unknown")
                } else {
                    None
                }
            }
            "type" => {
                if args.len() >= 3 {
                    Some("dynamic class creation cannot be modeled precisely")
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn dynamic_assignment_message(target: &AstNode) -> Option<&'static str> {
        match target {
            AstNode::Attribute { attribute, .. } if attribute == "__class__" => {
                Some("runtime __class__ mutation can invalidate inferred instance types")
            }
            AstNode::Attribute { attribute, .. } if attribute == "__bases__" => {
                Some("runtime __bases__ mutation can invalidate inferred class hierarchy")
            }
            AstNode::Identifier { name, .. } if name == "__all__" => None,
            AstNode::Attribute { object, attribute, .. } if attribute == "append" || attribute == "extend" => {
                if matches!(object.as_ref(), AstNode::Identifier { name, .. } if name == "__all__") {
                    Some("runtime __all__ mutation changes exported names dynamically")
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn is_dynamic_decorator(decorator: &str) -> bool {
        !matches!(
            decorator,
            "property" | "staticmethod" | "classmethod" | "dataclass" | "typing.override" | "override"
        )
    }

    fn push_dynamic_diagnostic(
        diagnostics: &mut Vec<Diagnostic>, severity: DiagnosticSeverity, line: usize, col: usize, width: usize,
        message: &str,
    ) {
        let start = Position { line: line.saturating_sub(1) as u32, character: col.saturating_sub(1) as u32 };
        diagnostics.push(Diagnostic {
            range: Range { start, end: Position { line: start.line, character: start.character + width as u32 } },
            severity: Some(severity),
            code: Some(lsp_types::NumberOrString::String("DYN001".to_string())),
            source: Some("beacon".to_string()),
            message: format!("Dynamic Python boundary: {message}"),
            related_information: None,
            tags: None,
            data: None,
            code_description: None,
        });
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

            if leading_dots > from_module.split('.').count() {
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
                let target_name = target.target_display();
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

    /// Add diagnostics for importing non-existent symbols from valid modules
    ///
    /// Reports when a specific symbol is imported from a module that exists, but that symbol is not defined or exported by the module.
    fn add_invalid_symbol_import_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        let Ok(workspace) = self.workspace.try_read() else {
            return;
        };

        let symbol_imports = workspace.get_symbol_imports(uri);

        if symbol_imports.is_empty() {
            return;
        }

        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                Self::find_invalid_symbol_imports(ast, &symbol_imports, &workspace, diagnostics);
            }
        });
    }

    /// Find invalid symbol imports in the AST
    fn find_invalid_symbol_imports(
        node: &AstNode, _symbol_imports: &[crate::workspace::SymbolImport], workspace: &Workspace,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::find_invalid_symbol_imports(stmt, _symbol_imports, workspace, diagnostics);
                }
            }
            AstNode::ImportFrom { module, names, .. } => {
                if names.iter().any(|n| n.name == "*") {
                    return;
                }

                let module_uri = match workspace.resolve_import(module) {
                    Some(uri) => uri,
                    None => return,
                };

                let available_symbols = workspace.get_module_symbols(&module_uri);
                let stub_exports = workspace.get_stub_exports(module);

                for import_name in names {
                    if import_name.name == "*" {
                        continue;
                    }

                    let symbol_exists = available_symbols.contains(&import_name.name)
                        || stub_exports
                            .as_ref()
                            .is_some_and(|exports| exports.contains_key(&import_name.name));

                    if !symbol_exists {
                        let range = Range {
                            start: Position::new((import_name.line - 1) as u32, (import_name.col - 1) as u32),
                            end: Position::new((import_name.end_line - 1) as u32, (import_name.end_col - 1) as u32),
                        };

                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::ERROR),
                            code: Some(lsp_types::NumberOrString::String("invalid-import".to_string())),
                            source: Some("beacon".to_string()),
                            message: format!(
                                "Cannot import '{}' from '{}': symbol not found in module",
                                import_name.name, module
                            ),
                            related_information: None,
                            tags: None,
                            data: None,
                            code_description: None,
                        });
                    }
                }
            }
            AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::find_invalid_symbol_imports(stmt, _symbol_imports, workspace, diagnostics);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    Self::find_invalid_symbol_imports(stmt, _symbol_imports, workspace, diagnostics);
                }
                for (_test, elif_body) in elif_parts {
                    for stmt in elif_body {
                        Self::find_invalid_symbol_imports(stmt, _symbol_imports, workspace, diagnostics);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::find_invalid_symbol_imports(stmt, _symbol_imports, workspace, diagnostics);
                    }
                }
            }
            _ => {}
        }
    }

    /// Add diagnostics for importing private symbols (starting with underscore)
    ///
    /// Reports warnings when importing symbols that start with underscore,
    /// which conventionally indicates they are private/internal.
    fn add_private_symbol_import_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        let Ok(workspace) = self.workspace.try_read() else {
            return;
        };

        let symbol_imports = workspace.get_symbol_imports(uri);

        if symbol_imports.is_empty() {
            return;
        }

        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                Self::find_private_symbol_imports(ast, &symbol_imports, diagnostics);
            }
        });
    }

    /// Find private symbol imports in the AST
    fn find_private_symbol_imports(
        node: &AstNode, _symbol_imports: &[crate::workspace::SymbolImport], diagnostics: &mut Vec<Diagnostic>,
    ) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::find_private_symbol_imports(stmt, _symbol_imports, diagnostics);
                }
            }
            AstNode::ImportFrom { names, line, col, module, .. } => {
                for import_name in names {
                    if import_name.name.starts_with('_') && import_name.name != "*" {
                        let position = Position { line: (*line - 1) as u32, character: (*col - 1) as u32 };

                        let range = Range {
                            start: position,
                            end: Position {
                                line: position.line,
                                character: position.character + import_name.name.len() as u32,
                            },
                        };

                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::WARNING),
                            code: Some(lsp_types::NumberOrString::String("private-import".to_string())),
                            source: Some("beacon".to_string()),
                            message: format!(
                                "Importing private symbol '{}' from '{}' (names starting with underscore are conventionally private)",
                                import_name.name, module
                            ),
                            related_information: None,
                            tags: None,
                            data: None,
                            code_description: None,
                        });
                    }
                }
            }
            AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::find_private_symbol_imports(stmt, _symbol_imports, diagnostics);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    Self::find_private_symbol_imports(stmt, _symbol_imports, diagnostics);
                }
                for (_test, elif_body) in elif_parts {
                    for stmt in elif_body {
                        Self::find_private_symbol_imports(stmt, _symbol_imports, diagnostics);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::find_private_symbol_imports(stmt, _symbol_imports, diagnostics);
                    }
                }
            }
            _ => {}
        }
    }

    /// Add diagnostics for broken re-export chains
    ///
    /// Reports when a module exports a symbol in __all__ that it imported, but that symbol doesn't actually exist in the source module.
    fn add_reexport_chain_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        let Ok(workspace) = self.workspace.try_read() else {
            return;
        };

        let all_exports = match workspace.get_all_exports(uri) {
            Some(exports) => exports,
            None => return,
        };

        let symbol_imports = workspace.get_symbol_imports(uri);
        let local_symbols = workspace.get_module_symbols(uri);

        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                Self::find_reexport_chain_issues(
                    ast,
                    &all_exports,
                    &symbol_imports,
                    &local_symbols,
                    &workspace,
                    diagnostics,
                );
            }
        });
    }

    /// Find re-export chain issues in the AST
    ///
    /// Collects defined symbols (functions, classes, variables) separately from local_symbols to properly distinguish between defined and imported symbols
    fn find_reexport_chain_issues(
        node: &AstNode, _all_exports: &[String], symbol_imports: &[crate::workspace::SymbolImport],
        _local_symbols: &rustc_hash::FxHashSet<String>, workspace: &Workspace, diagnostics: &mut Vec<Diagnostic>,
    ) {
        if let AstNode::Module { body, .. } = node {
            let mut defined_symbols = rustc_hash::FxHashSet::default();
            for stmt in body {
                match stmt {
                    AstNode::FunctionDef { name, .. } => {
                        defined_symbols.insert(name.clone());
                    }
                    AstNode::ClassDef { name, .. } => {
                        defined_symbols.insert(name.clone());
                    }
                    AstNode::Assignment { target, .. } => {
                        let target_name = target.target_display();
                        if !target_name.is_empty() && target_name != "__all__" {
                            defined_symbols.insert(target_name);
                        }
                    }
                    AstNode::AnnotatedAssignment { target, .. } => {
                        let target_name = target.target_display();
                        if !target_name.is_empty() {
                            defined_symbols.insert(target_name);
                        }
                    }
                    _ => {}
                }
            }

            for stmt in body {
                if let AstNode::Assignment { target, value, line, col, .. } = stmt {
                    let target_name = target.target_display();
                    if target_name == "__all__"
                        && let AstNode::List { elements, .. } = value.as_ref()
                    {
                        for (idx, element) in elements.iter().enumerate() {
                            if let AstNode::Literal {
                                value: beacon_parser::LiteralValue::String { value: symbol_name, .. },
                                ..
                            } = element
                                && !defined_symbols.contains(symbol_name)
                                && let Some(import_info) = symbol_imports.iter().find(|imp| imp.symbol == *symbol_name)
                                && let Some(source_uri) = workspace.resolve_import(&import_info.from_module)
                            {
                                let source_symbols = workspace.get_module_symbols(&source_uri);
                                let stub_exports = workspace.get_stub_exports(&import_info.from_module);

                                let symbol_exists = source_symbols.contains(symbol_name)
                                    || stub_exports
                                        .as_ref()
                                        .is_some_and(|exports| exports.contains_key(symbol_name));

                                if !symbol_exists {
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
                                        code: Some(lsp_types::NumberOrString::String("broken-reexport".to_string())),
                                        source: Some("beacon".to_string()),
                                        message: format!(
                                            "Re-exported symbol '{}' does not exist in source module '{}'",
                                            symbol_name, import_info.from_module
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
                }
            }
        }
    }

    /// Add diagnostics for type mismatches across module boundaries
    ///
    /// Reports when an imported symbol with a known type signature is used with incompatible types.
    /// Works with both stdlib functions (from stubs) and user-defined functions (from source).
    fn add_cross_module_type_mismatch_diagnostics(
        &self, uri: &Url, analyzer: &mut Analyzer, diagnostics: &mut Vec<Diagnostic>,
    ) {
        let Ok(workspace) = self.workspace.try_read() else {
            return;
        };

        let symbol_imports = workspace.get_symbol_imports(uri);

        if symbol_imports.is_empty() {
            return;
        }

        let analysis_result = match analyzer.analyze(uri) {
            Ok(result) => result,
            Err(_) => return,
        };

        self.documents.get_document(uri, |doc| {
            if let Some(ast) = doc.ast() {
                Self::find_cross_module_type_mismatches(
                    ast,
                    &symbol_imports,
                    &workspace,
                    &analysis_result.type_map,
                    diagnostics,
                );
            }
        });
    }

    /// Find type mismatches for imported symbols in the AST
    ///
    /// Validates function calls against both stub signatures and user-defined function signatures
    fn find_cross_module_type_mismatches(
        node: &AstNode, symbol_imports: &[crate::workspace::SymbolImport], workspace: &Workspace,
        _type_map: &rustc_hash::FxHashMap<usize, beacon_core::Type>, diagnostics: &mut Vec<Diagnostic>,
    ) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::find_cross_module_type_mismatches(stmt, symbol_imports, workspace, _type_map, diagnostics);
                }
            }
            AstNode::Call { function, args, line, col, .. } => {
                if let AstNode::Identifier { name: func_name, .. } = function.as_ref()
                    && let Some(import_info) = symbol_imports.iter().find(|imp| imp.symbol == *func_name)
                {
                    let expected_func_type = workspace
                        .get_stub_type(&import_info.from_module, func_name)
                        .or_else(|| workspace.get_source_function_type(&import_info.from_module, func_name));

                    if let Some(expected_func_type) = expected_func_type
                        && let Type::Fun(params, _return_type) = &expected_func_type
                    {
                        if args.len() != params.len() {
                            let range = Range::new(
                                Position::new((*line - 1) as u32, (*col - 1) as u32),
                                Position::new((*line - 1) as u32, (*col - 1 + func_name.len()) as u32),
                            );
                            Self::report_argument_count_mismatch(
                                func_name,
                                &import_info.from_module,
                                params.len(),
                                args.len(),
                                range,
                                diagnostics,
                            );
                        } else {
                            for (idx, (arg, (param_name, expected_type))) in args.iter().zip(params.iter()).enumerate()
                            {
                                if let Some(arg_type) = Self::infer_literal_type(arg)
                                    && !Self::types_are_compatible(expected_type, &arg_type)
                                {
                                    Self::report_argument_type_mismatch(
                                        func_name,
                                        idx,
                                        param_name,
                                        expected_type,
                                        &arg_type,
                                        arg,
                                        diagnostics,
                                    );
                                }
                            }
                        }
                    }
                }

                for arg in args {
                    Self::find_cross_module_type_mismatches(arg, symbol_imports, workspace, _type_map, diagnostics);
                }
            }
            AstNode::Assignment { value, .. } => {
                Self::find_cross_module_type_mismatches(value, symbol_imports, workspace, _type_map, diagnostics);
            }
            AstNode::AnnotatedAssignment { value: Some(val), .. } => {
                Self::find_cross_module_type_mismatches(val, symbol_imports, workspace, _type_map, diagnostics);
            }
            AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    Self::find_cross_module_type_mismatches(stmt, symbol_imports, workspace, _type_map, diagnostics);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    Self::find_cross_module_type_mismatches(stmt, symbol_imports, workspace, _type_map, diagnostics);
                }
                for (_, elif_body) in elif_parts {
                    for stmt in elif_body {
                        Self::find_cross_module_type_mismatches(
                            stmt,
                            symbol_imports,
                            workspace,
                            _type_map,
                            diagnostics,
                        );
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::find_cross_module_type_mismatches(
                            stmt,
                            symbol_imports,
                            workspace,
                            _type_map,
                            diagnostics,
                        );
                    }
                }
            }
            AstNode::For { body, else_body, .. } => {
                for stmt in body {
                    Self::find_cross_module_type_mismatches(stmt, symbol_imports, workspace, _type_map, diagnostics);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::find_cross_module_type_mismatches(
                            stmt,
                            symbol_imports,
                            workspace,
                            _type_map,
                            diagnostics,
                        );
                    }
                }
            }
            AstNode::While { body, else_body, .. } => {
                for stmt in body {
                    Self::find_cross_module_type_mismatches(stmt, symbol_imports, workspace, _type_map, diagnostics);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::find_cross_module_type_mismatches(
                            stmt,
                            symbol_imports,
                            workspace,
                            _type_map,
                            diagnostics,
                        );
                    }
                }
            }
            AstNode::With { body, .. } | AstNode::Try { body, .. } => {
                for stmt in body {
                    Self::find_cross_module_type_mismatches(stmt, symbol_imports, workspace, _type_map, diagnostics);
                }
            }
            _ => {}
        }
    }

    /// Infer the type of a literal AST node
    fn infer_literal_type(node: &AstNode) -> Option<beacon_core::Type> {
        match node {
            AstNode::Literal { value, .. } => match value {
                LiteralValue::Integer(_) => Some(Type::Con(TypeCtor::Int)),
                LiteralValue::Float(_) => Some(Type::Con(TypeCtor::Float)),
                LiteralValue::String { .. } => Some(Type::Con(TypeCtor::String)),
                LiteralValue::Boolean(_) => Some(Type::Con(TypeCtor::Bool)),
                LiteralValue::None => Some(Type::Con(TypeCtor::NoneType)),
            },
            _ => None,
        }
    }

    /// Report an argument type mismatch
    fn report_argument_type_mismatch(
        func_name: &str, arg_index: usize, param_name: &str, expected_type: &beacon_core::Type,
        actual_type: &beacon_core::Type, arg_node: &AstNode, diagnostics: &mut Vec<Diagnostic>,
    ) {
        let (line, col, end_line, end_col) = match arg_node {
            AstNode::Literal { line, col, end_line, end_col, .. } => (*line, *col, *end_line, *end_col),
            AstNode::Identifier { line, col, end_line, end_col, .. } => (*line, *col, *end_line, *end_col),
            AstNode::Call { line, col, end_line, end_col, .. } => (*line, *col, *end_line, *end_col),
            _ => return,
        };

        let range = Range {
            start: Position::new((line - 1) as u32, (col - 1) as u32),
            end: Position::new((end_line - 1) as u32, (end_col - 1) as u32),
        };

        let param_desc = if !param_name.is_empty() {
            format!("parameter '{}' (position {})", param_name, arg_index + 1)
        } else {
            format!("parameter at position {}", arg_index + 1)
        };

        diagnostics.push(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::WARNING),
            code: Some(lsp_types::NumberOrString::String("type-mismatch-argument".to_string())),
            source: Some("beacon".to_string()),
            message: format!(
                "Type mismatch in call to '{}': {} expects {}, got {}",
                func_name,
                param_desc,
                Self::type_to_display_string(expected_type),
                Self::type_to_display_string(actual_type)
            ),
            related_information: None,
            tags: None,
            data: None,
            code_description: None,
        });
    }

    /// Report an argument count mismatch for a function call
    fn report_argument_count_mismatch(
        func_name: &str, module_name: &str, expected_count: usize, actual_count: usize, range: Range,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        diagnostics.push(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::WARNING),
            code: Some(lsp_types::NumberOrString::String("argument-count-mismatch".to_string())),
            source: Some("beacon".to_string()),
            message: format!(
                "Function '{}' from '{}' expects {} argument{}, but {} {} provided",
                func_name,
                module_name,
                expected_count,
                if expected_count == 1 { "" } else { "s" },
                actual_count,
                if actual_count == 1 { "was" } else { "were" }
            ),
            related_information: None,
            tags: None,
            data: None,
            code_description: None,
        });
    }

    /// Convert a Type to a human-readable display string
    fn type_to_display_string(ty: &beacon_core::Type) -> String {
        use beacon_core::{Type, TypeCtor};

        match ty {
            Type::Con(TypeCtor::Int) => "int".to_string(),
            Type::Con(TypeCtor::Float) => "float".to_string(),
            Type::Con(TypeCtor::String) => "str".to_string(),
            Type::Con(TypeCtor::Bool) => "bool".to_string(),
            Type::Con(TypeCtor::NoneType) => "None".to_string(),
            Type::Con(TypeCtor::Any) => "Any".to_string(),
            Type::Con(TypeCtor::Unknown) => "Unknown".to_string(),
            Type::Con(TypeCtor::List) => "list".to_string(),
            Type::Con(TypeCtor::Dict) => "dict".to_string(),
            Type::Con(TypeCtor::Set) => "set".to_string(),
            Type::Con(TypeCtor::Tuple) => "tuple".to_string(),
            Type::Con(TypeCtor::Class(name)) => name.clone(),
            Type::App(ctor, arg) => format!(
                "{}[{}]",
                Self::type_to_display_string(ctor),
                Self::type_to_display_string(arg)
            ),
            Type::Fun(params, return_type) => {
                let param_types: Vec<String> = params.iter().map(|(_, ty)| Self::type_to_display_string(ty)).collect();
                format!(
                    "({}) -> {}",
                    param_types.join(", "),
                    Self::type_to_display_string(return_type)
                )
            }
            Type::Var(tv) => format!("'{}", tv.id),
            _ => format!("{:?}", ty),
        }
    }

    /// Add diagnostics for cross-file dead code (unused exports)
    ///
    /// Reports functions and classes that are defined but never used across the workspace.
    fn add_cross_file_dead_code_diagnostics(&self, uri: &Url, diagnostics: &mut Vec<Diagnostic>) {
        let Ok(workspace) = self.workspace.try_read() else {
            return;
        };

        workspace.populate_entry_points();

        let workspace_cfg_arc = workspace.workspace_cfg();
        let Ok(workspace_cfg) = workspace_cfg_arc.try_read() else {
            return;
        };

        let unreachable_functions = workspace_cfg.unreachable_functions();

        for func_id in unreachable_functions {
            if func_id.uri != *uri {
                continue;
            }

            let Some((symbol_table, source_lines)) = self
                .documents
                .get_document(uri, |doc| {
                    doc.symbol_table()
                        .map(|st| (st.clone(), doc.text().lines().map(String::from).collect::<Vec<_>>()))
                })
                .flatten()
            else {
                continue;
            };

            let mut found_symbol = None;
            for scope in symbol_table.scopes.values() {
                if let Some(symbol) = scope.symbols.get(&func_id.name) {
                    found_symbol = Some(symbol);
                    break;
                }
            }

            if let Some(symbol) = found_symbol {
                let range = Self::identifier_range(symbol.line, symbol.col, &func_id.name, &source_lines);

                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(lsp_types::NumberOrString::String("BEA033".to_string())),
                    source: Some("beacon-linter".to_string()),
                    message: format!(
                        "Function '{}' is never used across the workspace. Consider removing it or marking it as private (prefix with '_')",
                        func_id.name
                    ),
                    related_information: None,
                    tags: Some(vec![lsp_types::DiagnosticTag::UNNECESSARY]),
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
fn analysis_error_into_diagnostic(error: &BeaconError) -> Diagnostic {
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
