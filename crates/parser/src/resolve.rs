use crate::AstNode;
use beacon_core::Result;
use rustc_hash::FxHashMap;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SymbolKind {
    Variable,
    Function,
    Class,
    Parameter,
    Import,
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
}

/// Unique identifier for scopes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

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
    pub fn new() -> Self {
        let root_id = ScopeId(0);
        let mut scopes = HashMap::new();

        scopes.insert(
            root_id,
            Scope {
                id: root_id,
                kind: ScopeKind::Module,
                parent: None,
                symbols: FxHashMap::default(),
                children: Vec::new(),
                start_byte: 0,
                end_byte: usize::MAX,
            },
        );

        Self { scopes, root_scope: root_id, next_scope_id: 1 }
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

        loop {
            if let Some(scope) = self.scopes.get(&current_scope) {
                if let Some(symbol) = scope.symbols.get(name) {
                    return Some(symbol);
                }

                match scope.parent {
                    Some(parent_id) => current_scope = parent_id,
                    None => break,
                }
            } else {
                break;
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
}

/// Name resolution context for traversing the AST
pub struct NameResolver {
    pub symbol_table: SymbolTable,
    current_scope: ScopeId,
    source: String,
}

impl NameResolver {
    pub fn new(source: String) -> Self {
        let symbol_table = SymbolTable::new();
        let root_scope = symbol_table.root_scope;

        Self { symbol_table, current_scope: root_scope, source }
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
        self.visit_node(ast)
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
            AstNode::Assignment { line, col, .. }
            | AstNode::Call { line, col, .. }
            | AstNode::Identifier { line, col, .. }
            | AstNode::Literal { line, col, .. }
            | AstNode::Return { line, col, .. }
            | AstNode::Import { line, col, .. }
            | AstNode::ImportFrom { line, col, .. }
            | AstNode::Attribute { line, col, .. } => {
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
            AstNode::FunctionDef { name, args, body, docstring, line, col } => {
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Function,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                    docstring: docstring.clone(),
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

                for (i, param) in args.iter().enumerate() {
                    let param_symbol = Symbol {
                        name: param.clone(),
                        kind: SymbolKind::Parameter,
                        line: *line,
                        col: *col + i,
                        scope_id: func_scope,
                        docstring: None,
                    };
                    self.symbol_table.add_symbol(func_scope, param_symbol);
                }
                for stmt in body {
                    self.visit_node(stmt)?;
                }

                self.current_scope = prev_scope;
            }
            AstNode::ClassDef { name, body, docstring, line, col } => {
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Class,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                    docstring: docstring.clone(),
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
            AstNode::Assignment { target, value, line, col } => {
                self.visit_node(value)?;

                let symbol = Symbol {
                    name: target.clone(),
                    kind: SymbolKind::Variable,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                    docstring: None,
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);
            }
            AstNode::Call { args, .. } => {
                for arg in args {
                    self.visit_node(arg)?;
                }
            }
            // TODO: Track Identifier usage
            AstNode::Identifier { .. } => {}
            // Literals don't affect name resolution
            AstNode::Literal { .. } => {}
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    self.visit_node(val)?;
                }
            }
            AstNode::Import { module, alias, line, col } => self.symbol_table.add_symbol(
                self.current_scope,
                Symbol {
                    name: alias.as_ref().unwrap_or(module).clone(),
                    kind: SymbolKind::Import,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                    docstring: None,
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
                        },
                    );
                }
            }
            AstNode::Attribute { object, .. } => self.visit_node(object)?,
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

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for NameResolver {
    fn default() -> Self {
        Self::new(String::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        };
        table.add_symbol(table.root_scope, root_symbol);

        let func_symbol = Symbol {
            name: "local_var".to_string(),
            kind: SymbolKind::Variable,
            line: 2,
            col: 1,
            scope_id: func_scope,
            docstring: None,
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
            value: Box::new(AstNode::Literal { value: crate::LiteralValue::Integer(42), line: 1, col: 5 }),
            line: 1,
            col: 1,
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
            args: vec!["param1".to_string(), "param2".to_string()],
            body: vec![AstNode::Assignment {
                target: "local_var".to_string(),
                value: Box::new(AstNode::Identifier { name: "param1".to_string(), line: 2, col: 15 }),
                line: 2,
                col: 5,
            }],
            docstring: None,
            line: 1,
            col: 1,
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
                    value: Box::new(AstNode::Literal { value: crate::LiteralValue::Integer(1), line: 1, col: 5 }),
                    line: 1,
                    col: 1,
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
                            }),
                            line: 3,
                            col: 5,
                        },
                        AstNode::Assignment {
                            target: "z".to_string(),
                            value: Box::new(AstNode::Literal {
                                value: crate::LiteralValue::Integer(3),
                                line: 4,
                                col: 9,
                            }),
                            line: 4,
                            col: 5,
                        },
                    ],
                    docstring: None,
                    line: 2,
                    col: 1,
                },
                AstNode::Assignment {
                    target: "w".to_string(),
                    value: Box::new(AstNode::Literal { value: crate::LiteralValue::Integer(4), line: 5, col: 5 }),
                    line: 5,
                    col: 1,
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
                    value: Box::new(AstNode::Literal { value: crate::LiteralValue::Integer(1), line: 1, col: 15 }),
                    line: 1,
                    col: 1,
                },
                AstNode::FunctionDef {
                    name: "outer".to_string(),
                    args: vec!["param".to_string()],
                    body: vec![AstNode::Assignment {
                        target: "outer_var".to_string(),
                        value: Box::new(AstNode::Literal { value: crate::LiteralValue::Integer(2), line: 3, col: 20 }),
                        line: 3,
                        col: 9,
                    }],
                    docstring: None,
                    line: 2,
                    col: 1,
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
}
