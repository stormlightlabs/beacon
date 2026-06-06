use super::{Constraint, ControlFlowContext, Span};
use beacon_core::{ClassRegistry, Type};
use beacon_parser::{ScopeId, line_col_to_byte_offset};
use rustc_hash::{FxHashMap, FxHashSet};

/// Context for tracking node information during constraint generation.
pub struct ConstraintGenContext<'a> {
    pub constraints: Vec<Constraint>,
    pub node_counter: usize,
    pub type_map: FxHashMap<usize, Type>,
    pub position_map: FxHashMap<(usize, usize), usize>,
    pub node_spans: FxHashMap<usize, Span>,
    pub safe_any_nodes: FxHashSet<usize>,
    pub class_registry: ClassRegistry,
    pub control_flow: ControlFlowContext,
    pub loaded_stub_modules: FxHashSet<String>,
    pub scope_stack: Vec<ScopeId>,
    pub node_to_scope: FxHashMap<usize, ScopeId>,
    pub scope_dependencies: FxHashMap<ScopeId, FxHashSet<ScopeId>>,
    pub typevar_registry: beacon_core::TypeVarConstraintRegistry,
    symbol_table: Option<&'a beacon_parser::SymbolTable>,
    pub source: Option<&'a str>,
}

impl<'a> ConstraintGenContext<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_context(&mut self, symbol_table: &'a beacon_parser::SymbolTable, source: &'a str) {
        self.symbol_table = Some(symbol_table);
        self.source = Some(source);
    }

    pub fn find_scope_at_position(&self, line: usize, col: usize) -> Option<ScopeId> {
        let symbol_table = self.symbol_table?;
        let byte_offset = self
            .source
            .and_then(|source| line_col_to_byte_offset(source, line, col))?;
        Some(symbol_table.find_scope_at_position(byte_offset))
    }

    pub fn symbol_table(&self) -> Option<&beacon_parser::SymbolTable> {
        self.symbol_table
    }

    pub fn record_type(&mut self, line: usize, col: usize, ty: Type) -> usize {
        self.record_type_with_end(line, col, line, col + 1, ty)
    }

    pub fn record_type_with_end(
        &mut self, line: usize, col: usize, end_line: usize, end_col: usize, ty: Type,
    ) -> usize {
        let span = Self::normalized_span(line, col, end_line, end_col);
        self.record_type_with_span(span, ty)
    }

    pub fn record_type_with_span(&mut self, span: Span, ty: Type) -> usize {
        let node_id = self.node_counter;
        self.node_counter += 1;
        self.type_map.insert(node_id, ty);
        self.position_map.insert((span.line, span.col), node_id);
        self.node_spans.insert(node_id, span);

        if let Some(&scope_id) = self.scope_stack.last() {
            self.node_to_scope.insert(node_id, scope_id);
        }

        node_id
    }

    pub fn override_span(&mut self, node_id: usize, span: Span) {
        self.node_spans.insert(node_id, span);
    }

    pub fn mark_safe_any_node(&mut self, node_id: usize) {
        self.safe_any_nodes.insert(node_id);
    }

    fn line_text(&self, line: usize) -> Option<&str> {
        let source = self.source?;
        let idx = line.checked_sub(1)?;
        source.lines().nth(idx)
    }

    fn column_to_byte(line_text: &str, column: usize) -> usize {
        if column <= 1 {
            return 0;
        }

        for (chars_seen, (byte_idx, _)) in (1..).zip(line_text.char_indices()) {
            if chars_seen == column {
                return byte_idx;
            }
        }
        line_text.len()
    }

    fn byte_to_column(line_text: &str, byte_idx: usize) -> usize {
        line_text[..byte_idx].chars().count() + 1
    }

    pub fn span_for_identifier(&self, line: usize, column_hint: usize, name: &str) -> Span {
        let name_width = name.chars().count().max(1);
        let fallback_end = column_hint + name_width;

        if let Some(line_text) = self.line_text(line) {
            let search_start = Self::column_to_byte(line_text, column_hint);
            if search_start <= line_text.len()
                && let Some(rel_idx) = line_text[search_start..].find(name)
            {
                let byte_idx = search_start + rel_idx;
                let start_col = Self::byte_to_column(line_text, byte_idx);
                let end_col = start_col + name_width;
                return Span::with_end(line, start_col, line, end_col);
            }
        }

        Span::with_end(line, column_hint, line, fallback_end)
    }

    fn normalized_span(line: usize, col: usize, end_line: usize, end_col: usize) -> Span {
        let normalized_end_line = end_line.max(line);
        let mut normalized_end_col = end_col;
        if normalized_end_line == line && normalized_end_col <= col {
            normalized_end_col = col + 1;
        }
        Span::with_end(line, col, normalized_end_line, normalized_end_col)
    }

    pub fn push_scope(&mut self, scope_id: ScopeId) {
        self.scope_stack.push(scope_id);
    }

    pub fn pop_scope(&mut self) -> Option<ScopeId> {
        self.scope_stack.pop()
    }

    pub fn add_scope_dependency(&mut self, from_scope: ScopeId, to_scope: ScopeId) {
        self.scope_dependencies.entry(from_scope).or_default().insert(to_scope);
    }
}

impl<'a> Default for ConstraintGenContext<'a> {
    fn default() -> Self {
        Self {
            constraints: Vec::new(),
            node_counter: 0,
            type_map: FxHashMap::default(),
            position_map: FxHashMap::default(),
            node_spans: FxHashMap::default(),
            safe_any_nodes: FxHashSet::default(),
            class_registry: ClassRegistry::new(),
            control_flow: ControlFlowContext::new(),
            loaded_stub_modules: FxHashSet::default(),
            scope_stack: Vec::new(),
            node_to_scope: FxHashMap::default(),
            scope_dependencies: FxHashMap::default(),
            typevar_registry: beacon_core::TypeVarConstraintRegistry::new(),
            symbol_table: None,
            source: None,
        }
    }
}
