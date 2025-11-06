use crate::AstNode;
use beacon_core::Result;
use rustc_hash::FxHashMap;
use std::collections::HashMap;

/// Builtin dunder variables available at module level
pub static BUILTIN_DUNDERS: &[&str] = &["__name__", "__file__", "__doc__", "__package__"];

/// Magic methods that can be defined in classes
pub static MAGIC_METHODS: &[&str] = &[
    "__init__",
    "__new__",
    "__repr__",
    "__str__",
    "__len__",
    "__eq__",
    "__lt__",
    "__le__",
    "__gt__",
    "__ge__",
    "__ne__",
    "__getitem__",
    "__setitem__",
    "__delitem__",
    "__enter__",
    "__exit__",
    "__iter__",
    "__next__",
    "__call__",
    "__add__",
    "__sub__",
    "__mul__",
    "__truediv__",
    "__floordiv__",
    "__mod__",
    "__pow__",
    "__neg__",
    "__pos__",
    "__abs__",
    "__bool__",
    "__copy__",
    "__deepcopy__",
    "__sizeof__",
    "__bytes__",
    "__format__",
    "__complex__",
    "__int__",
    "__float__",
    "__round__",
    "__index__",
    "__hash__",
    "__getattr__",
    "__setattr__",
    "__delattr__",
    "__getattribute__",
    "__dir__",
    "__radd__",
    "__rand__",
    "__ior__",
    "__imul__",
    "__reduce_ex__",
    "__reduce__",
    "__getnewargs__",
];

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SymbolKind {
    Variable,
    Function,
    Class,
    Parameter,
    Import,
    MagicMethod,
    BuiltinVar,
}

impl SymbolKind {
    pub fn icon(&self) -> &'static str {
        match self {
            SymbolKind::Variable => "◆",
            SymbolKind::Function => "λ",
            SymbolKind::Class => "●",
            SymbolKind::Parameter => "▲",
            SymbolKind::Import => "↳",
            SymbolKind::BuiltinVar => "⧉",
            SymbolKind::MagicMethod => "★",
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            SymbolKind::Variable => "variable",
            SymbolKind::Function => "function",
            SymbolKind::Class => "class",
            SymbolKind::Parameter => "parameter",
            SymbolKind::Import => "import",
            _ => "dunder",
        }
    }
}

/// Kind of reference to a symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceKind {
    Read,
    Write,
}

/// A reference to a symbol at a specific location
#[derive(Debug, Clone)]
pub struct SymbolReference {
    pub line: usize,
    pub col: usize,
    pub kind: ReferenceKind,
}

/// Information about a symbol definition
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub line: usize,
    pub col: usize,
    pub scope_id: ScopeId,
    pub docstring: Option<String>,
    /// References to this symbol (reads and writes)
    pub references: Vec<SymbolReference>,
}

/// Unique identifier for scopes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

impl ScopeId {
    /// Create a new ScopeId for testing purposes
    ///
    /// In production, ScopeIds are created internally by SymbolTable.create_scope().
    /// This constructor is primarily for testing and cache key creation.
    #[cfg(test)]
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    /// Create a ScopeId from a raw u32 value (for cache and testing)
    ///
    /// SAFETY: This should only be used in controlled contexts like caching
    /// where you need to reconstruct a ScopeId from a stored value.
    pub fn from_raw(id: u32) -> Self {
        Self(id)
    }
}

/// Types of scopes in Python
#[derive(Debug, Clone, PartialEq)]
pub enum ScopeKind {
    Module,
    Function,
    Class,
    Block,
}

/// Represents a scope with its symbols and nested scopes
#[derive(Debug, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub symbols: FxHashMap<String, Symbol>,
    pub children: Vec<ScopeId>,
    /// Start byte offset of this scope in the source
    pub start_byte: usize,
    /// End byte offset of this scope in the source
    pub end_byte: usize,
}

/// Symbol table with scope hierarchy
#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub scopes: HashMap<ScopeId, Scope>,
    pub root_scope: ScopeId,
    next_scope_id: u32,
}

impl SymbolTable {
    /// Creates new instance of [SymbolTable] & injects builtin dunder variables into root scope
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new scope as a child of the given parent with position information
    pub fn create_scope(&mut self, kind: ScopeKind, parent: ScopeId, start_byte: usize, end_byte: usize) -> ScopeId {
        let new_id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;

        let new_scope = Scope {
            id: new_id,
            kind,
            parent: Some(parent),
            symbols: FxHashMap::default(),
            children: Vec::new(),
            start_byte,
            end_byte,
        };

        self.scopes.insert(new_id, new_scope);

        if let Some(parent_scope) = self.scopes.get_mut(&parent) {
            parent_scope.children.push(new_id);
        }

        new_id
    }

    /// Add a symbol to a scope
    pub fn add_symbol(&mut self, scope_id: ScopeId, symbol: Symbol) {
        if let Some(scope) = self.scopes.get_mut(&scope_id) {
            scope.symbols.insert(symbol.name.clone(), symbol);
        }
    }

    /// Look up a symbol, walking up the scope chain
    pub fn lookup_symbol(&self, name: &str, from_scope: ScopeId) -> Option<&Symbol> {
        let mut current_scope = from_scope;
        while let Some(scope) = self.scopes.get(&current_scope) {
            let symbol = scope.symbols.get(name);
            if symbol.is_some() {
                return symbol;
            }

            match scope.parent {
                Some(parent_id) => current_scope = parent_id,
                None => break,
            }
        }

        None
    }

    /// Get all symbols in a scope (not including parents)
    pub fn get_scope_symbols(&self, scope_id: ScopeId) -> Option<&FxHashMap<String, Symbol>> {
        self.scopes.get(&scope_id).map(|scope| &scope.symbols)
    }

    /// Get scope by ID
    pub fn get_scope(&self, scope_id: ScopeId) -> Option<&Scope> {
        self.scopes.get(&scope_id)
    }

    /// Find the innermost scope containing the given byte offset by searching the scope hierarchy to find the most specific scope that contains the given position.
    /// Returns root_scope if no more specific scope is found.
    pub fn find_scope_at_position(&self, byte_offset: usize) -> ScopeId {
        self.find_scope_at_position_recursive(self.root_scope, byte_offset)
            .unwrap_or(self.root_scope)
    }

    /// Check if the given scope or any of its ancestors is a class scope
    pub fn is_in_class_scope(&self, scope_id: ScopeId) -> bool {
        let mut current_scope = scope_id;
        while let Some(scope) = self.scopes.get(&current_scope) {
            if scope.kind == ScopeKind::Class {
                return true;
            }
            match scope.parent {
                Some(parent_id) => current_scope = parent_id,
                None => break,
            }
        }
        false
    }

    /// Get all visible symbols from a scope, walking up the scope chain
    ///
    /// Collects all symbols that are visible from the given scope, including:
    /// - Symbols defined in the current scope
    /// - Symbols from parent scopes (walking up the chain)
    /// - Only includes the first occurrence of each name (shadowing)
    ///
    /// Returns a vector of symbol references sorted by scope proximity (closest first).
    pub fn get_visible_symbols(&self, from_scope: ScopeId) -> Vec<&Symbol> {
        let mut symbols = Vec::new();
        let mut seen_names = std::collections::HashSet::new();
        let mut current_scope = from_scope;

        while let Some(scope) = self.scopes.get(&current_scope) {
            for symbol in scope.symbols.values() {
                if seen_names.insert(&symbol.name) {
                    symbols.push(symbol);
                }
            }

            match scope.parent {
                Some(parent_id) => current_scope = parent_id,
                None => break,
            }
        }

        symbols
    }

    /// Recursively search for the innermost scope containing byte_offset
    fn find_scope_at_position_recursive(&self, scope_id: ScopeId, byte_offset: usize) -> Option<ScopeId> {
        let scope = self.scopes.get(&scope_id)?;

        if byte_offset < scope.start_byte || byte_offset > scope.end_byte {
            return None;
        }

        for &child_id in &scope.children {
            if let Some(child_scope_id) = self.find_scope_at_position_recursive(child_id, byte_offset) {
                return Some(child_scope_id);
            }
        }

        Some(scope_id)
    }

    /// Add a reference to a symbol
    ///
    /// Looks up the symbol from the given scope and adds a reference to it.
    /// Returns true if the symbol was found and the reference was added.
    pub fn add_reference(
        &mut self, name: &str, from_scope: ScopeId, line: usize, col: usize, kind: ReferenceKind,
    ) -> bool {
        let mut current_scope = from_scope;

        while let Some(scope) = self.scopes.get(&current_scope) {
            let scope_id = scope.id;

            if scope.symbols.contains_key(name) {
                if let Some(scope_mut) = self.scopes.get_mut(&scope_id) {
                    if let Some(symbol) = scope_mut.symbols.get_mut(name) {
                        symbol.references.push(SymbolReference { line, col, kind });
                        return true;
                    }
                }
                return false;
            }

            match scope.parent {
                Some(parent_id) => current_scope = parent_id,
                None => break,
            }
        }

        false
    }

    /// Find all symbols that shadow symbols in parent scopes
    ///
    /// Returns a list of (child_symbol, parent_symbol) pairs where child_symbol shadows parent_symbol.
    pub fn find_shadowed_symbols(&self) -> Vec<(&Symbol, &Symbol)> {
        let mut shadowed = Vec::new();

        for scope in self.scopes.values() {
            if let Some(parent_id) = scope.parent {
                for (name, child_symbol) in &scope.symbols {
                    if child_symbol.kind == SymbolKind::BuiltinVar || child_symbol.kind == SymbolKind::Parameter {
                        continue;
                    }

                    if let Some(parent_symbol) = self.lookup_symbol(name, parent_id) {
                        if parent_symbol.kind != SymbolKind::BuiltinVar {
                            shadowed.push((child_symbol, parent_symbol));
                        }
                    }
                }
            }
        }

        shadowed
    }

    /// Find all symbols that have no read references
    ///
    /// Returns symbols that are defined but never read. Filters out:
    /// - Builtins
    /// - Names starting with underscore (convention for unused)
    /// - Parameters (often intentionally unused)
    /// - Class and function definitions (their "use" is being defined)
    pub fn find_unused_symbols(&self) -> Vec<&Symbol> {
        let mut unused = Vec::new();

        for scope in self.scopes.values() {
            for symbol in scope.symbols.values() {
                if symbol.kind == SymbolKind::BuiltinVar {
                    continue;
                }

                if symbol.name.starts_with('_') {
                    continue;
                }

                if symbol.kind == SymbolKind::Parameter {
                    continue;
                }

                if symbol.kind == SymbolKind::Class || symbol.kind == SymbolKind::Function {
                    continue;
                }

                let has_read = symbol.references.iter().any(|r| r.kind == ReferenceKind::Read);

                if !has_read {
                    unused.push(symbol);
                }
            }
        }

        unused
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        let root_id = ScopeId(0);
        let mut scopes = HashMap::new();
        let mut root_symbols = FxHashMap::default();

        for &dunder_name in BUILTIN_DUNDERS {
            root_symbols.insert(
                dunder_name.to_string(),
                Symbol {
                    name: dunder_name.to_string(),
                    kind: SymbolKind::BuiltinVar,
                    line: 0,
                    col: 0,
                    scope_id: root_id,
                    docstring: None,
                    references: Vec::new(),
                },
            );
        }

        scopes.insert(
            root_id,
            Scope {
                id: root_id,
                kind: ScopeKind::Module,
                parent: None,
                symbols: root_symbols,
                children: Vec::new(),
                start_byte: 0,
                end_byte: usize::MAX,
            },
        );

        Self { scopes, root_scope: root_id, next_scope_id: 1 }
    }
}

/// Name resolution context for traversing the AST
pub struct NameResolver {
    pub symbol_table: SymbolTable,
    current_scope: ScopeId,
    source: String,
}

impl NameResolver {
    pub fn new(source: String) -> Self {
        Self { source, ..Default::default() }
    }

    /// Convert line and column (1-indexed) to byte offset
    fn line_col_to_byte_offset(&self, line: usize, col: usize) -> usize {
        let mut byte_offset = 0;
        let mut current_line = 1;
        let mut current_col = 1;

        for ch in self.source.chars() {
            if current_line == line && current_col == col {
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

    /// Resolve names in an AST and build the symbol table
    pub fn resolve(&mut self, ast: &AstNode) -> Result<()> {
        self.visit_node(ast)?;
        self.track_references(ast)?;
        Ok(())
    }

    /// Second pass: track all symbol references (reads)
    ///
    /// This walks the AST again and records references to identifiers.
    /// We skip recording writes since those are handled during symbol definition.
    fn track_references(&mut self, node: &AstNode) -> Result<()> {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.track_references(stmt)?;
                }
            }
            AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    self.track_references(stmt)?;
                }
            }
            AstNode::Assignment { value, .. } => self.track_references(value)?,
            AstNode::AnnotatedAssignment { value, .. } => {
                if let Some(val) = value {
                    self.track_references(val)?;
                }
            }
            AstNode::Call { function, args, keywords, line, col, .. } => {
                let byte_offset = self.line_col_to_byte_offset(*line, *col);
                let scope = self.symbol_table.find_scope_at_position(byte_offset);
                self.symbol_table
                    .add_reference(function, scope, *line, *col, ReferenceKind::Read);

                for arg in args {
                    self.track_references(arg)?;
                }

                for (_name, value) in keywords {
                    self.track_references(value)?;
                }
            }
            AstNode::Identifier { name, line, col, .. } => {
                let byte_offset = self.line_col_to_byte_offset(*line, *col);
                let scope = self.symbol_table.find_scope_at_position(byte_offset);
                self.symbol_table
                    .add_reference(name, scope, *line, *col, ReferenceKind::Read);
            }
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    self.track_references(val)?;
                }
            }
            AstNode::Attribute { object, .. } => {
                self.track_references(object)?;
            }
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                self.track_references(test)?;
                for stmt in body {
                    self.track_references(stmt)?;
                }
                for (elif_test, elif_body) in elif_parts {
                    self.track_references(elif_test)?;
                    for stmt in elif_body {
                        self.track_references(stmt)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.track_references(stmt)?;
                    }
                }
            }
            AstNode::For { iter, body, else_body, .. } => {
                self.track_references(iter)?;
                for stmt in body {
                    self.track_references(stmt)?;
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.track_references(stmt)?;
                    }
                }
            }
            AstNode::While { test, body, else_body, .. } => {
                self.track_references(test)?;
                for stmt in body {
                    self.track_references(stmt)?;
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.track_references(stmt)?;
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    self.track_references(stmt)?;
                }
                for handler in handlers {
                    for stmt in &handler.body {
                        self.track_references(stmt)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.track_references(stmt)?;
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        self.track_references(stmt)?;
                    }
                }
            }
            AstNode::With { items, body, .. } => {
                for item in items {
                    self.track_references(&item.context_expr)?;
                }
                for stmt in body {
                    self.track_references(stmt)?;
                }
            }
            AstNode::ListComp { element, generators, .. }
            | AstNode::SetComp { element, generators, .. }
            | AstNode::GeneratorExp { element, generators, .. } => {
                self.track_references(element)?;
                for generator in generators {
                    self.track_references(&generator.iter)?;
                    for if_clause in &generator.ifs {
                        self.track_references(if_clause)?;
                    }
                }
            }
            AstNode::DictComp { key, value, generators, .. } => {
                self.track_references(key)?;
                self.track_references(value)?;
                for generator in generators {
                    self.track_references(&generator.iter)?;
                    for if_clause in &generator.ifs {
                        self.track_references(if_clause)?;
                    }
                }
            }
            AstNode::NamedExpr { value, .. } => self.track_references(value)?,
            AstNode::BinaryOp { left, right, .. } => {
                self.track_references(left)?;
                self.track_references(right)?;
            }
            AstNode::UnaryOp { operand, .. } => {
                self.track_references(operand)?;
            }
            AstNode::Compare { left, comparators, .. } => {
                self.track_references(left)?;
                for comp in comparators {
                    self.track_references(comp)?;
                }
            }
            AstNode::Lambda { body, .. } => {
                self.track_references(body)?;
            }
            AstNode::Subscript { value, slice, .. } => {
                self.track_references(value)?;
                self.track_references(slice)?;
            }
            AstNode::Match { subject, cases, .. } => {
                self.track_references(subject)?;
                for case in cases {
                    if let Some(guard) = &case.guard {
                        self.track_references(guard)?;
                    }
                    for stmt in &case.body {
                        self.track_references(stmt)?;
                    }
                }
            }
            AstNode::Raise { exc, .. } => {
                if let Some(exception) = exc {
                    self.track_references(exception)?;
                }
            }
            AstNode::List { elements, .. } | AstNode::Set { elements, .. } => {
                for elem in elements {
                    self.track_references(elem)?;
                }
            }
            AstNode::Dict { keys, values, .. } => {
                for key in keys {
                    self.track_references(key)?;
                }
                for value in values {
                    self.track_references(value)?;
                }
            }
            AstNode::Tuple { elements, .. } => {
                for elem in elements {
                    self.track_references(elem)?;
                }
            }
            AstNode::Yield { .. }
            | AstNode::YieldFrom { .. }
            | AstNode::Await { .. }
            | AstNode::Literal { .. }
            | AstNode::Pass { .. }
            | AstNode::Break { .. }
            | AstNode::Continue { .. }
            | AstNode::Import { .. }
            | AstNode::ImportFrom { .. } => {}
        }

        Ok(())
    }

    /// Get the approximate end byte of a node by finding the end of its last line
    fn get_node_end_byte(&self, node: &AstNode) -> usize {
        match node {
            AstNode::Module { body, .. } => {
                if let Some(last) = body.last() {
                    self.get_node_end_byte(last)
                } else {
                    self.source.len()
                }
            }
            AstNode::FunctionDef { body, line, col, .. } | AstNode::ClassDef { body, line, col, .. } => {
                if let Some(last) = body.last() {
                    self.get_node_end_byte(last)
                } else {
                    self.line_col_to_byte_offset(*line, *col) + 50
                }
            }
            AstNode::If { body, else_body, .. } => {
                if let Some(else_stmts) = else_body {
                    if let Some(last) = else_stmts.last() {
                        return self.get_node_end_byte(last);
                    }
                }
                if let Some(last) = body.last() { self.get_node_end_byte(last) } else { self.source.len() }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                if let Some(else_stmts) = else_body {
                    if let Some(last) = else_stmts.last() {
                        return self.get_node_end_byte(last);
                    }
                }
                if let Some(last) = body.last() { self.get_node_end_byte(last) } else { self.source.len() }
            }
            AstNode::Try { body, else_body, finally_body, .. } => {
                if let Some(finally_stmts) = finally_body {
                    if let Some(last) = finally_stmts.last() {
                        return self.get_node_end_byte(last);
                    }
                }
                if let Some(else_stmts) = else_body {
                    if let Some(last) = else_stmts.last() {
                        return self.get_node_end_byte(last);
                    }
                }
                if let Some(last) = body.last() { self.get_node_end_byte(last) } else { self.source.len() }
            }
            AstNode::With { body, .. } => {
                if let Some(last) = body.last() {
                    self.get_node_end_byte(last)
                } else {
                    self.source.len()
                }
            }
            AstNode::Match { cases, .. } => {
                if let Some(last_case) = cases.last() {
                    if let Some(last_stmt) = last_case.body.last() {
                        return self.get_node_end_byte(last_stmt);
                    }
                }
                self.source.len()
            }
            AstNode::Yield { line, col, .. }
            | AstNode::YieldFrom { line, col, .. }
            | AstNode::Await { line, col, .. }
            | AstNode::Tuple { line, col, .. }
            | AstNode::List { line, col, .. }
            | AstNode::Dict { line, col, .. }
            | AstNode::Set { line, col, .. }
            | AstNode::Assignment { line, col, .. }
            | AstNode::AnnotatedAssignment { line, col, .. }
            | AstNode::Call { line, col, .. }
            | AstNode::Identifier { line, col, .. }
            | AstNode::Literal { line, col, .. }
            | AstNode::Return { line, col, .. }
            | AstNode::Import { line, col, .. }
            | AstNode::ImportFrom { line, col, .. }
            | AstNode::Attribute { line, col, .. }
            | AstNode::ListComp { line, col, .. }
            | AstNode::DictComp { line, col, .. }
            | AstNode::SetComp { line, col, .. }
            | AstNode::GeneratorExp { line, col, .. }
            | AstNode::NamedExpr { line, col, .. }
            | AstNode::BinaryOp { line, col, .. }
            | AstNode::UnaryOp { line, col, .. }
            | AstNode::Compare { line, col, .. }
            | AstNode::Lambda { line, col, .. }
            | AstNode::Subscript { line, col, .. }
            | AstNode::Pass { line, col, .. }
            | AstNode::Break { line, col, .. }
            | AstNode::Continue { line, col, .. }
            | AstNode::Raise { line, col, .. } => {
                let start_byte = self.line_col_to_byte_offset(*line, *col);
                let mut end_byte = start_byte;
                for (i, ch) in self.source[start_byte..].chars().enumerate() {
                    if ch == '\n' {
                        break;
                    }
                    end_byte = start_byte + i + ch.len_utf8();
                }
                end_byte
            }
        }
    }

    fn visit_node(&mut self, node: &AstNode) -> Result<()> {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.visit_node(stmt)?;
                }
            }
            AstNode::FunctionDef { name, args, body, docstring, line, col, .. } => {
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Function,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                    docstring: docstring.clone(),
                    references: Vec::new(),
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);

                let start_byte = self.line_col_to_byte_offset(*line, *col);
                let end_byte = if let Some(last_stmt) = body.last() {
                    self.get_node_end_byte(last_stmt)
                } else {
                    start_byte + name.len()
                };

                let func_scope =
                    self.symbol_table
                        .create_scope(ScopeKind::Function, self.current_scope, start_byte, end_byte);
                let prev_scope = self.current_scope;
                self.current_scope = func_scope;

                for param in args.iter() {
                    let param_symbol = Symbol {
                        name: param.name.clone(),
                        kind: SymbolKind::Parameter,
                        line: param.line,
                        col: param.col,
                        scope_id: func_scope,
                        docstring: None,
                        references: Vec::new(),
                    };
                    self.symbol_table.add_symbol(func_scope, param_symbol);
                }
                for stmt in body {
                    self.visit_node(stmt)?;
                }

                self.current_scope = prev_scope;
            }
            AstNode::ClassDef { name, body, docstring, line, col, .. } => {
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Class,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                    docstring: docstring.clone(),
                    references: Vec::new(),
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);

                let start_byte = self.line_col_to_byte_offset(*line, *col);
                let end_byte = if let Some(last_stmt) = body.last() {
                    self.get_node_end_byte(last_stmt)
                } else {
                    start_byte + name.len()
                };

                let class_scope =
                    self.symbol_table
                        .create_scope(ScopeKind::Class, self.current_scope, start_byte, end_byte);
                let prev_scope = self.current_scope;
                self.current_scope = class_scope;

                for stmt in body {
                    self.visit_node(stmt)?;
                }

                self.current_scope = prev_scope;
            }
            AstNode::Assignment { target, value, line, col, .. } => {
                self.visit_node(value)?;

                let symbol = Symbol {
                    name: target.clone(),
                    kind: SymbolKind::Variable,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                    docstring: None,
                    references: Vec::new(),
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);
            }
            AstNode::AnnotatedAssignment { target, value, line, col, .. } => {
                self.symbol_table.add_symbol(
                    self.current_scope,
                    Symbol {
                        name: target.clone(),
                        kind: SymbolKind::Variable,
                        line: *line,
                        col: *col,
                        scope_id: self.current_scope,
                        docstring: None,
                        references: Vec::new(),
                    },
                );

                if let Some(val) = value {
                    self.visit_node(val)?;
                }
            }
            AstNode::Call { args, .. } => {
                for arg in args {
                    self.visit_node(arg)?;
                }
            }
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    self.visit_node(val)?;
                }
            }
            AstNode::Import { module, alias, line, col, .. } => self.symbol_table.add_symbol(
                self.current_scope,
                Symbol {
                    name: alias.as_ref().unwrap_or(module).clone(),
                    kind: SymbolKind::Import,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                    docstring: None,
                    references: Vec::new(),
                },
            ),
            AstNode::ImportFrom { names, line, col, .. } => {
                for (i, name) in names.iter().enumerate() {
                    self.symbol_table.add_symbol(
                        self.current_scope,
                        Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Import,
                            line: *line,
                            col: *col + i,
                            scope_id: self.current_scope,
                            docstring: None,
                            references: Vec::new(),
                        },
                    );
                }
            }
            AstNode::Attribute { object, .. } => self.visit_node(object)?,
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                self.visit_node(test)?;
                for stmt in body {
                    self.visit_node(stmt)?;
                }
                for (elif_test, elif_body) in elif_parts {
                    self.visit_node(elif_test)?;
                    for stmt in elif_body {
                        self.visit_node(stmt)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt)?;
                    }
                }
            }
            AstNode::For { target, iter, body, else_body, line, col, .. } => {
                self.visit_node(iter)?;

                let symbol = Symbol {
                    name: target.clone(),
                    kind: SymbolKind::Variable,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                    docstring: None,
                    references: Vec::new(),
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);

                for stmt in body {
                    self.visit_node(stmt)?;
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt)?;
                    }
                }
            }
            AstNode::While { test, body, else_body, .. } => {
                self.visit_node(test)?;
                for stmt in body {
                    self.visit_node(stmt)?;
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt)?;
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    self.visit_node(stmt)?;
                }
                for handler in handlers {
                    if let Some(name) = &handler.name {
                        let symbol = Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Variable,
                            line: handler.line,
                            col: handler.col,
                            scope_id: self.current_scope,
                            docstring: None,
                            references: Vec::new(),
                        };
                        self.symbol_table.add_symbol(self.current_scope, symbol);
                    }
                    for stmt in &handler.body {
                        self.visit_node(stmt)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.visit_node(stmt)?;
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        self.visit_node(stmt)?;
                    }
                }
            }
            AstNode::With { items, body, .. } => {
                for item in items {
                    self.visit_node(&item.context_expr)?;
                    if let Some(var_name) = &item.optional_vars {
                        // TODO: extract position
                        let symbol = Symbol {
                            name: var_name.clone(),
                            kind: SymbolKind::Variable,
                            line: 0,
                            col: 0,
                            scope_id: self.current_scope,
                            docstring: None,
                            references: Vec::new(),
                        };
                        self.symbol_table.add_symbol(self.current_scope, symbol);
                    }
                }
                for stmt in body {
                    self.visit_node(stmt)?;
                }
            }
            AstNode::ListComp { element, generators, line, col, .. }
            | AstNode::SetComp { element, generators, line, col, .. }
            | AstNode::GeneratorExp { element, generators, line, col, .. } => {
                for generator in generators {
                    self.visit_node(&generator.iter)?;

                    let symbol = Symbol {
                        name: generator.target.clone(),
                        kind: SymbolKind::Variable,
                        line: *line,
                        col: *col,
                        scope_id: self.current_scope,
                        docstring: None,
                        references: Vec::new(),
                    };
                    self.symbol_table.add_symbol(self.current_scope, symbol);

                    for if_clause in &generator.ifs {
                        self.visit_node(if_clause)?;
                    }
                }
                self.visit_node(element)?;
            }
            AstNode::DictComp { key, value, generators, line, col, .. } => {
                for generator in generators {
                    self.visit_node(&generator.iter)?;

                    let symbol = Symbol {
                        name: generator.target.clone(),
                        kind: SymbolKind::Variable,
                        line: *line,
                        col: *col,
                        scope_id: self.current_scope,
                        docstring: None,
                        references: Vec::new(),
                    };

                    self.symbol_table.add_symbol(self.current_scope, symbol);

                    for if_clause in &generator.ifs {
                        self.visit_node(if_clause)?;
                    }
                }
                self.visit_node(key)?;
                self.visit_node(value)?;
            }
            AstNode::NamedExpr { target, value, line, col, .. } => {
                self.visit_node(value)?;
                let symbol = Symbol {
                    name: target.clone(),
                    kind: SymbolKind::Variable,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                    docstring: None,
                    references: Vec::new(),
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);
            }
            AstNode::BinaryOp { left, right, .. } => {
                self.visit_node(left)?;
                self.visit_node(right)?;
            }
            AstNode::UnaryOp { operand, .. } => self.visit_node(operand)?,
            AstNode::Compare { left, comparators, .. } => {
                self.visit_node(left)?;
                for comp in comparators {
                    self.visit_node(comp)?;
                }
            }
            AstNode::Lambda { args, body, line, col, .. } => {
                let start_byte = self.line_col_to_byte_offset(*line, *col);
                let end_byte = self.get_node_end_byte(node);

                let lambda_scope =
                    self.symbol_table
                        .create_scope(ScopeKind::Function, self.current_scope, start_byte, end_byte);
                let prev_scope = self.current_scope;

                self.current_scope = lambda_scope;

                for param in args {
                    let param_symbol = Symbol {
                        name: param.name.clone(),
                        kind: SymbolKind::Parameter,
                        line: param.line,
                        col: param.col,
                        scope_id: lambda_scope,
                        docstring: None,
                        references: Vec::new(),
                    };
                    self.symbol_table.add_symbol(lambda_scope, param_symbol);
                }

                self.visit_node(body)?;
                self.current_scope = prev_scope;
            }
            AstNode::Subscript { value, slice, .. } => {
                self.visit_node(value)?;
                self.visit_node(slice)?;
            }
            AstNode::Match { subject, cases, .. } => {
                self.visit_node(subject)?;
                for case in cases {
                    if let Some(guard) = &case.guard {
                        self.visit_node(guard)?;
                    }
                    for stmt in &case.body {
                        self.visit_node(stmt)?;
                    }
                }
            }
            AstNode::Raise { exc, .. } => {
                if let Some(exception) = exc {
                    self.visit_node(exception)?;
                }
            }
            AstNode::List { elements, .. } | AstNode::Set { elements, .. } => {
                for elem in elements {
                    self.visit_node(elem)?;
                }
            }
            AstNode::Dict { keys, values, .. } => {
                for key in keys {
                    self.visit_node(key)?;
                }
                for value in values {
                    self.visit_node(value)?;
                }
            }
            AstNode::Tuple { elements, .. } => {
                for elem in elements {
                    self.visit_node(elem)?;
                }
            }
            AstNode::Yield { .. }
            | AstNode::YieldFrom { .. }
            | AstNode::Await { .. }
            | AstNode::Identifier { .. }
            | AstNode::Literal { .. }
            | AstNode::Pass { .. }
            | AstNode::Break { .. }
            | AstNode::Continue { .. } => {}
        }

        Ok(())
    }

    /// Look up a symbol by name from the current scope
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbol_table.lookup_symbol(name, self.current_scope)
    }

    /// Get the current scope ID
    pub fn current_scope(&self) -> ScopeId {
        self.current_scope
    }
}

impl Default for NameResolver {
    fn default() -> Self {
        let symbol_table = SymbolTable::new();
        let root_scope = symbol_table.root_scope;
        Self { symbol_table, current_scope: root_scope, source: String::new() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Parameter;

    // NOTE: All test AST node constructions below include end_line and end_col fields
    // to match the updated AST structure that supports precise span tracking.
    // For test purposes, end positions are set to reasonable placeholder values
    // (typically same as start position for simple literals, or estimated based on
    // typical Python syntax for complex nodes).

    #[test]
    fn test_symbol_table_creation() {
        let table = SymbolTable::new();
        assert_eq!(table.scopes.len(), 1);
        assert!(table.scopes.contains_key(&table.root_scope));
    }

    #[test]
    fn test_scope_creation() {
        let mut table = SymbolTable::new();
        let func_scope = table.create_scope(ScopeKind::Function, table.root_scope, 0, 100);
        assert_eq!(table.scopes.len(), 2);
        assert!(table.scopes.contains_key(&func_scope));

        let root_scope = table.scopes.get(&table.root_scope).unwrap();
        assert_eq!(root_scope.children.len(), 1);
        assert_eq!(root_scope.children[0], func_scope);
    }

    #[test]
    fn test_symbol_lookup() {
        let mut table = SymbolTable::new();
        let func_scope = table.create_scope(ScopeKind::Function, table.root_scope, 10, 50);

        let root_symbol = Symbol {
            name: "global_var".to_string(),
            kind: SymbolKind::Variable,
            line: 1,
            col: 1,
            scope_id: table.root_scope,
            docstring: None,
            references: Vec::new(),
        };
        table.add_symbol(table.root_scope, root_symbol);

        let func_symbol = Symbol {
            name: "local_var".to_string(),
            kind: SymbolKind::Variable,
            line: 2,
            col: 1,
            scope_id: func_scope,
            docstring: None,
            references: Vec::new(),
        };
        table.add_symbol(func_scope, func_symbol);

        assert!(table.lookup_symbol("local_var", func_scope).is_some());
        assert!(table.lookup_symbol("global_var", func_scope).is_some());
        assert!(table.lookup_symbol("nonexistent", func_scope).is_none());
        assert!(table.lookup_symbol("global_var", table.root_scope).is_some());
        assert!(table.lookup_symbol("local_var", table.root_scope).is_none());
    }

    #[test]
    fn test_name_resolver_basic() {
        let source = "x = 42".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Assignment {
            target: "x".to_string(),
            value: Box::new(AstNode::Literal {
                value: crate::LiteralValue::Integer(42),
                line: 1,
                col: 5,
                end_line: 1,
                end_col: 7,  // "42" is 2 chars
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,  // "x = 42" ends at col 7
        };

        resolver.resolve(&ast).unwrap();

        let symbol = resolver.lookup("x");
        assert!(symbol.is_some());
        assert_eq!(symbol.unwrap().kind, SymbolKind::Variable);
    }

    #[test]
    fn test_name_resolver_function() {
        let source = "def test_func(param1, param2):\n    local_var = param1".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "test_func".to_string(),
            args: vec![
                Parameter {
                    name: "param1".to_string(),
                    line: 1,
                    col: 15,
                    end_line: 1,
                    end_col: 21,  // "param1" is 6 chars
                    type_annotation: None,
                    default_value: None,
                },
                Parameter {
                    name: "param2".to_string(),
                    line: 1,
                    col: 23,
                    end_line: 1,
                    end_col: 29,  // "param2" is 6 chars
                    type_annotation: None,
                    default_value: None,
                },
            ],
            body: vec![AstNode::Assignment {
                target: "local_var".to_string(),
                value: Box::new(AstNode::Identifier {
                    name: "param1".to_string(),
                    line: 2,
                    col: 15,
                    end_line: 2,
                    end_col: 21,  // "param1" is 6 chars
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 21,  // "local_var = param1" ends at col 21
            }],
            line: 1,
            col: 1,
            end_line: 2,
            end_col: 21,  // Function ends at last statement
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        let func_symbol = resolver
            .symbol_table
            .lookup_symbol("test_func", resolver.symbol_table.root_scope);
        assert!(func_symbol.is_some());
        assert_eq!(func_symbol.unwrap().kind, SymbolKind::Function);

        let func_scope_id = resolver
            .symbol_table
            .scopes
            .get(&resolver.symbol_table.root_scope)
            .unwrap()
            .children[0];

        let param_symbol = resolver.symbol_table.lookup_symbol("param1", func_scope_id);
        assert!(param_symbol.is_some());
        assert_eq!(param_symbol.unwrap().kind, SymbolKind::Parameter);

        let local_symbol = resolver.symbol_table.lookup_symbol("local_var", func_scope_id);
        assert!(local_symbol.is_some());
        assert_eq!(local_symbol.unwrap().kind, SymbolKind::Variable);
    }

    #[test]
    fn test_find_scope_at_position() {
        let source = "x = 1\ndef foo():\n    y = 2\n    z = 3\nw = 4".to_string();
        let mut resolver = NameResolver::new(source.clone());

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::FunctionDef {
                    name: "foo".to_string(),
                    args: vec![],
                    body: vec![
                        AstNode::Assignment {
                            target: "y".to_string(),
                            value: Box::new(AstNode::Literal {
                                value: crate::LiteralValue::Integer(2),
                                line: 3,
                                col: 9,
                                end_line: 3,
                                end_col: 10,
                            }),
                            line: 3,
                            col: 5,
                            end_line: 3,
                            end_col: 10,
                        },
                        AstNode::Assignment {
                            target: "z".to_string(),
                            value: Box::new(AstNode::Literal {
                                value: crate::LiteralValue::Integer(3),
                                line: 4,
                                col: 9,
                                end_line: 4,
                                end_col: 10,
                            }),
                            line: 4,
                            col: 5,
                            end_line: 4,
                            end_col: 10,
                        },
                    ],
                    line: 2,
                    col: 1,
                    end_line: 4,
                    end_col: 10,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    is_async: false,
                },
                AstNode::Assignment {
                    target: "w".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(4),
                        line: 5,
                        col: 5,
                        end_line: 5,
                        end_col: 6,
                    }),
                    line: 5,
                    col: 1,
                    end_line: 5,
                    end_col: 6,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let byte_offset_x = 0;
        let scope_x = resolver.symbol_table.find_scope_at_position(byte_offset_x);
        assert_eq!(scope_x, resolver.symbol_table.root_scope);

        let byte_offset_y = 23;
        let scope_y = resolver.symbol_table.find_scope_at_position(byte_offset_y);
        assert_ne!(scope_y, resolver.symbol_table.root_scope);

        let y_symbol = resolver.symbol_table.lookup_symbol("y", scope_y);
        assert!(y_symbol.is_some());
        assert_eq!(y_symbol.unwrap().kind, SymbolKind::Variable);

        let y_from_root = resolver
            .symbol_table
            .lookup_symbol("y", resolver.symbol_table.root_scope);
        assert!(y_from_root.is_none());
    }

    #[test]
    fn test_nested_scopes() {
        let source = "global_var = 1\ndef outer(param):\n    outer_var = 2".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: "global_var".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 15,
                        end_line: 1,
                        end_col: 16,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 16,
                },
                AstNode::FunctionDef {
                    name: "outer".to_string(),
                    args: vec![Parameter {
                        name: "param".to_string(),
                        line: 2,
                        col: 11,
                        end_line: 2,
                        end_col: 16,
                        type_annotation: None,
                        default_value: None,
                    }],
                    body: vec![AstNode::Assignment {
                        target: "outer_var".to_string(),
                        value: Box::new(AstNode::Literal {
                            value: crate::LiteralValue::Integer(2),
                            line: 3,
                            col: 20,
                            end_line: 3,
                            end_col: 21,
                        }),
                        line: 3,
                        col: 9,
                        end_line: 3,
                        end_col: 21,
                    }],
                    line: 2,
                    col: 1,
                    end_line: 3,
                    end_col: 21,
                    docstring: None,
                    return_type: None,
                    is_async: false,
                    decorators: Vec::new(),
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let func_scope_id = resolver
            .symbol_table
            .scopes
            .get(&resolver.symbol_table.root_scope)
            .unwrap()
            .children[0];

        assert!(
            resolver
                .symbol_table
                .lookup_symbol("global_var", func_scope_id)
                .is_some()
        );
        assert!(
            resolver
                .symbol_table
                .lookup_symbol("outer_var", func_scope_id)
                .is_some()
        );
        assert!(resolver.symbol_table.lookup_symbol("param", func_scope_id).is_some());
        assert!(
            resolver
                .symbol_table
                .lookup_symbol("global_var", resolver.symbol_table.root_scope)
                .is_some()
        );
        assert!(
            resolver
                .symbol_table
                .lookup_symbol("outer_var", resolver.symbol_table.root_scope)
                .is_none()
        );
        assert!(
            resolver
                .symbol_table
                .lookup_symbol("param", resolver.symbol_table.root_scope)
                .is_none()
        );
    }

    #[test]
    fn test_attribute_node() {
        let source = "obj.field".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::Attribute {
            object: Box::new(AstNode::Identifier { name: "obj".to_string(), line: 1, col: 1, end_line: 1, end_col: 4 }),
            attribute: "field".to_string(),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 10,  // "obj.field" is 9 chars
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_named_expr() {
        let source = "(x := 42)".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::NamedExpr {
            target: "x".to_string(),
            value: Box::new(AstNode::Literal {
                value: crate::LiteralValue::Integer(42),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 8,
            }),
            line: 1,
            col: 2,
            end_line: 1,
            end_col: 8,
        };

        resolver.resolve(&ast).unwrap();

        let symbol = resolver.lookup("x");
        assert!(symbol.is_some());
        assert_eq!(symbol.unwrap().kind, SymbolKind::Variable);
    }

    #[test]
    fn test_binary_op() {
        let source = "x + y".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::BinaryOp {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
            op: crate::BinaryOperator::Add,
            right: Box::new(AstNode::Identifier { name: "y".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            line: 1,
            col: 3,
            end_line: 1,
            end_col: 6,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_unary_op() {
        let source = "-x".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::UnaryOp {
            op: crate::UnaryOperator::Minus,
            operand: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 2, end_line: 1, end_col: 3 }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 3,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_compare() {
        let source = "x < y".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::Compare {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
            ops: vec![crate::CompareOperator::Lt],
            comparators: vec![AstNode::Identifier { name: "y".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }],
            line: 1,
            col: 3,
            end_line: 1,
            end_col: 6,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_subscript() {
        let source = "arr[0]".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::Subscript {
            value: Box::new(AstNode::Identifier { name: "arr".to_string(), line: 1, col: 1, end_line: 1, end_col: 4 }),
            slice: Box::new(AstNode::Literal {
                value: crate::LiteralValue::Integer(0),
                line: 1,
                col: 5,
                end_line: 1,
                end_col: 6,
            }),
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 7,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_list_comprehension() {
        let source = "[x for x in items]".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::ListComp {
            element: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 2, end_line: 1, end_col: 3 }),
            generators: vec![crate::Comprehension {
                target: "x".to_string(),
                iter: AstNode::Identifier { name: "items".to_string(), line: 1, col: 13, end_line: 1, end_col: 18 },
                ifs: vec![],
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 19,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_dict_comprehension() {
        let source = "{k: v for k, v in items}".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::DictComp {
            key: Box::new(AstNode::Identifier { name: "k".to_string(), line: 1, col: 2, end_line: 1, end_col: 3 }),
            value: Box::new(AstNode::Identifier { name: "v".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            generators: vec![crate::Comprehension {
                target: "k".to_string(),
                iter: AstNode::Identifier { name: "items".to_string(), line: 1, col: 19, end_line: 1, end_col: 24 },
                ifs: vec![],
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 25,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_set_comprehension() {
        let source = "{x for x in items}".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::SetComp {
            element: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 2, end_line: 1, end_col: 3 }),
            generators: vec![crate::Comprehension {
                target: "x".to_string(),
                iter: AstNode::Identifier { name: "items".to_string(), line: 1, col: 13, end_line: 1, end_col: 18 },
                ifs: vec![],
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 19,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_generator_expression() {
        let source = "(x for x in items)".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::GeneratorExp {
            element: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 2, end_line: 1, end_col: 3 }),
            generators: vec![crate::Comprehension {
                target: "x".to_string(),
                iter: AstNode::Identifier { name: "items".to_string(), line: 1, col: 13, end_line: 1, end_col: 18 },
                ifs: vec![],
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 19,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_builtin_dunders_injected() {
        let table = SymbolTable::new();

        for &dunder_name in BUILTIN_DUNDERS {
            let symbol = table.lookup_symbol(dunder_name, table.root_scope);
            assert!(symbol.is_some(), "Expected {dunder_name} to be in symbol table");

            let sym = symbol.unwrap();
            assert_eq!(sym.kind, SymbolKind::BuiltinVar);
            assert_eq!(sym.name, dunder_name);
        }
    }

    #[test]
    fn test_is_in_class_scope() {
        let source = "class MyClass:\\n    def method(self):\\n        pass".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::ClassDef {
                name: "MyClass".to_string(),
                metaclass: None,
                bases: Vec::new(),
                body: vec![AstNode::FunctionDef {
                    name: "method".to_string(),
                    args: vec![Parameter {
                        name: "self".to_string(),
                        line: 2,
                        col: 16,
                        end_line: 2,
                        end_col: 20,
                        type_annotation: None,
                        default_value: None,
                    }],
                    body: vec![AstNode::Pass { line: 3, col: 9, end_line: 3, end_col: 13 }],
                    line: 2,
                    col: 5,
                    end_line: 3,
                    end_col: 13,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    is_async: false,
                }],
                line: 1,
                col: 1,
                end_line: 3,
                end_col: 13,
                docstring: None,
                decorators: Vec::new(),
            }],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        assert!(
            !resolver
                .symbol_table
                .is_in_class_scope(resolver.symbol_table.root_scope)
        );

        let class_scope_id = resolver
            .symbol_table
            .scopes
            .get(&resolver.symbol_table.root_scope)
            .unwrap()
            .children[0];
        assert!(resolver.symbol_table.is_in_class_scope(class_scope_id));

        let method_scope_id = resolver.symbol_table.scopes.get(&class_scope_id).unwrap().children[0];
        assert!(resolver.symbol_table.is_in_class_scope(method_scope_id));
    }

    #[test]
    fn test_builtin_dunders_accessible_in_nested_scopes() {
        let source = "def foo():\\n    x = __name__".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::FunctionDef {
                name: "foo".to_string(),
                args: vec![],
                body: vec![AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Identifier {
                        name: "__name__".to_string(),
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 17,
                    }),
                    line: 2,
                    col: 5,
                    end_line: 2,
                    end_col: 17,
                }],
                line: 1,
                col: 1,
                end_line: 2,
                end_col: 17,
                docstring: None,
                return_type: None,
                decorators: Vec::new(),
                is_async: false,
            }],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let func_scope_id = resolver
            .symbol_table
            .scopes
            .get(&resolver.symbol_table.root_scope)
            .unwrap()
            .children[0];

        let symbol = resolver.symbol_table.lookup_symbol("__name__", func_scope_id);
        assert!(symbol.is_some());
        assert_eq!(symbol.unwrap().kind, SymbolKind::BuiltinVar);
    }

    #[test]
    fn test_match_statement() {
        let source = "match x:\n    case 1:\n        pass".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::Match {
            subject: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 7, end_line: 1, end_col: 8 }),
            cases: vec![crate::MatchCase {
                pattern: crate::Pattern::MatchValue(AstNode::Literal {
                    value: crate::LiteralValue::Integer(1),
                    line: 2,
                    col: 10,
                    end_line: 2,
                    end_col: 11,
                }),
                guard: None,
                body: vec![AstNode::Pass { line: 3, col: 9, end_line: 3, end_col: 13 }],
            }],
            line: 1,
            col: 1,
            end_line: 3,
            end_col: 13,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_find_shadowed_symbols_simple() {
        let source = "x = 1\ndef foo():\n    x = 2".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::FunctionDef {
                    name: "foo".to_string(),
                    args: vec![],
                    body: vec![AstNode::Assignment {
                        target: "x".to_string(),
                        value: Box::new(AstNode::Literal {
                            value: crate::LiteralValue::Integer(2),
                            line: 3,
                            col: 9,
                            end_line: 3,
                            end_col: 10,
                        }),
                        line: 3,
                        col: 5,
                        end_line: 3,
                        end_col: 10,
                    }],
                    line: 2,
                    col: 1,
                    end_line: 3,
                    end_col: 10,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    is_async: false,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let shadowed = resolver.symbol_table.find_shadowed_symbols();
        assert_eq!(shadowed.len(), 1);
        assert_eq!(shadowed[0].0.name, "x");
        assert_eq!(shadowed[0].0.line, 3);
        assert_eq!(shadowed[0].1.name, "x");
        assert_eq!(shadowed[0].1.line, 1);
    }

    #[test]
    fn test_find_shadowed_symbols_nested() {
        let source = "x = 1\ndef outer():\n    x = 2\n    def inner():\n        x = 3".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::FunctionDef {
                    name: "outer".to_string(),
                    args: vec![],
                    body: vec![
                        AstNode::Assignment {
                            target: "x".to_string(),
                            value: Box::new(AstNode::Literal {
                                value: crate::LiteralValue::Integer(2),
                                line: 3,
                                col: 9,
                                end_line: 3,
                                end_col: 10,
                            }),
                            line: 3,
                            col: 5,
                            end_line: 3,
                            end_col: 10,
                        },
                        AstNode::FunctionDef {
                            name: "inner".to_string(),
                            args: vec![],
                            body: vec![AstNode::Assignment {
                                target: "x".to_string(),
                                value: Box::new(AstNode::Literal {
                                    value: crate::LiteralValue::Integer(3),
                                    line: 5,
                                    col: 13,
                                    end_line: 5,
                                    end_col: 14,
                                }),
                                line: 5,
                                col: 9,
                                end_line: 5,
                                end_col: 14,
                            }],
                            line: 4,
                            col: 5,
                            end_line: 5,
                            end_col: 14,
                            docstring: None,
                            return_type: None,
                            decorators: Vec::new(),
                            is_async: false,
                        },
                    ],
                    line: 2,
                    col: 1,
                    end_line: 5,
                    end_col: 14,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    is_async: false,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let shadowed = resolver.symbol_table.find_shadowed_symbols();
        assert!(shadowed.len() >= 2);
        assert!(shadowed.iter().any(|(child, _)| child.line == 3));
        assert!(shadowed.iter().any(|(child, _)| child.line == 5));
    }

    #[test]
    fn test_find_shadowed_symbols_no_shadowing() {
        let source = "x = 1\ny = 2".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::Assignment {
                    target: "y".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(2),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 6,
                    }),
                    line: 2,
                    col: 1,
                    end_line: 2,
                    end_col: 6,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let shadowed = resolver.symbol_table.find_shadowed_symbols();
        assert_eq!(shadowed.len(), 0);
    }

    #[test]
    fn test_find_shadowed_symbols_parameters_ignored() {
        let source = "x = 1\ndef foo(x):\n    pass".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::FunctionDef {
                    name: "foo".to_string(),
                    args: vec![Parameter {
                        name: "x".to_string(),
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 10,
                        type_annotation: None,
                        default_value: None,
                    }],
                    body: vec![AstNode::Pass { line: 3, col: 5, end_line: 3, end_col: 9 }],
                    line: 2,
                    col: 1,
                    end_line: 3,
                    end_col: 9,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    is_async: false,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let shadowed = resolver.symbol_table.find_shadowed_symbols();
        assert_eq!(shadowed.len(), 0);
    }

    #[test]
    fn test_symbol_references_tracking() {
        let source = "x = 1\ny = x + x".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::Assignment {
                    target: "y".to_string(),
                    value: Box::new(AstNode::BinaryOp {
                        left: Box::new(AstNode::Identifier {
                            name: "x".to_string(),
                            line: 2,
                            col: 5,
                            end_line: 2,
                            end_col: 6,
                        }),
                        op: crate::BinaryOperator::Add,
                        right: Box::new(AstNode::Identifier {
                            name: "x".to_string(),
                            line: 2,
                            col: 9,
                            end_line: 2,
                            end_col: 10,
                        }),
                        line: 2,
                        col: 7,
                        end_line: 2,
                        end_col: 10,
                    }),
                    line: 2,
                    col: 1,
                    end_line: 2,
                    end_col: 10,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let x_symbol = resolver
            .symbol_table
            .lookup_symbol("x", resolver.symbol_table.root_scope)
            .unwrap();

        let read_refs = x_symbol
            .references
            .iter()
            .filter(|r| r.kind == ReferenceKind::Read)
            .count();
        assert_eq!(read_refs, 2);
    }

    #[test]
    fn test_find_unused_symbols_basic() {
        let source = "x = 1\ny = 2\nprint(y)".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::Assignment {
                    target: "y".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(2),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 6,
                    }),
                    line: 2,
                    col: 1,
                    end_line: 2,
                    end_col: 6,
                },
                AstNode::Call {
                    function: "print".to_string(),
                    args: vec![AstNode::Identifier { name: "y".to_string(), line: 3, col: 7, end_line: 3, end_col: 8 }],
                    line: 3,
                    col: 1,
                    end_line: 3,
                    end_col: 9,
                    keywords: Vec::new(),
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let unused = resolver.symbol_table.find_unused_symbols();
        assert_eq!(unused.len(), 1);
        assert_eq!(unused[0].name, "x");
    }

    #[test]
    fn test_find_unused_symbols_underscore_prefix() {
        let source = "_x = 1".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::Assignment {
                target: "_x".to_string(),
                value: Box::new(AstNode::Literal {
                    value: crate::LiteralValue::Integer(1),
                    line: 1,
                    col: 6,
                    end_line: 1,
                    end_col: 7,
                }),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 7,
            }],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let unused = resolver.symbol_table.find_unused_symbols();
        assert_eq!(unused.len(), 0);
    }

    #[test]
    fn test_find_unused_symbols_functions_ignored() {
        let source = "def foo():\n    pass".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::FunctionDef {
                name: "foo".to_string(),
                args: vec![],
                body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
                line: 1,
                col: 1,
                end_line: 2,
                end_col: 9,
                docstring: None,
                return_type: None,
                is_async: false,
                decorators: Vec::new(),
            }],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let unused = resolver.symbol_table.find_unused_symbols();
        assert_eq!(unused.len(), 0);
    }

    #[test]
    fn test_find_unused_symbols_classes_ignored() {
        let source = "class MyClass:\n    pass".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::ClassDef {
                name: "MyClass".to_string(),
                metaclass: None,
                bases: Vec::new(),
                body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
                line: 1,
                col: 1,
                end_line: 2,
                end_col: 9,
                docstring: None,
                decorators: Vec::new(),
            }],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let unused = resolver.symbol_table.find_unused_symbols();
        assert_eq!(unused.len(), 0);
    }

    #[test]
    fn test_add_reference_returns_true_on_success() {
        let mut table = SymbolTable::new();
        let func_scope = table.create_scope(ScopeKind::Function, table.root_scope, 10, 50);

        let symbol = Symbol {
            name: "x".to_string(),
            kind: SymbolKind::Variable,
            line: 1,
            col: 1,
            scope_id: func_scope,
            docstring: None,
            references: Vec::new(),
        };
        table.add_symbol(func_scope, symbol);

        let result = table.add_reference("x", func_scope, 5, 10, ReferenceKind::Read);
        assert!(result);

        let x_symbol = table.lookup_symbol("x", func_scope).unwrap();
        assert_eq!(x_symbol.references.len(), 1);
        assert_eq!(x_symbol.references[0].kind, ReferenceKind::Read);
    }

    #[test]
    fn test_add_reference_returns_false_on_missing_symbol() {
        let mut table = SymbolTable::new();
        let func_scope = table.create_scope(ScopeKind::Function, table.root_scope, 10, 50);

        let result = table.add_reference("nonexistent", func_scope, 5, 10, ReferenceKind::Read);
        assert!(!result);
    }
}
