use crate::AstNode;
use beacon_core::Result;
use rustc_hash::FxHashMap;
use std::collections::HashMap;

/// Represents different kinds of symbols in Python
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
            },
        );

        Self { scopes, root_scope: root_id, next_scope_id: 1 }
    }

    /// Create a new scope as a child of the given parent
    pub fn create_scope(&mut self, kind: ScopeKind, parent: ScopeId) -> ScopeId {
        let new_id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;

        let new_scope =
            Scope { id: new_id, kind, parent: Some(parent), symbols: FxHashMap::default(), children: Vec::new() };

        self.scopes.insert(new_id, new_scope);

        // Add to parent's children
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

                // Move to parent scope
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
}

/// Name resolution context for traversing the AST
pub struct NameResolver {
    pub symbol_table: SymbolTable,
    current_scope: ScopeId,
}

impl NameResolver {
    pub fn new() -> Self {
        let symbol_table = SymbolTable::new();
        let root_scope = symbol_table.root_scope;

        Self { symbol_table, current_scope: root_scope }
    }

    /// Resolve names in an AST and build the symbol table
    pub fn resolve(&mut self, ast: &AstNode) -> Result<()> {
        self.visit_node(ast)
    }

    fn visit_node(&mut self, node: &AstNode) -> Result<()> {
        match node {
            AstNode::Module { body } => {
                for stmt in body {
                    self.visit_node(stmt)?;
                }
            }

            AstNode::FunctionDef { name, args, body, line, col } => {
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Function,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);

                let func_scope = self.symbol_table.create_scope(ScopeKind::Function, self.current_scope);
                let prev_scope = self.current_scope;
                self.current_scope = func_scope;

                for (i, param) in args.iter().enumerate() {
                    let param_symbol = Symbol {
                        name: param.clone(),
                        kind: SymbolKind::Parameter,
                        line: *line,
                        col: *col + i, // Rough approximation
                        scope_id: func_scope,
                    };
                    self.symbol_table.add_symbol(func_scope, param_symbol);
                }
                for stmt in body {
                    self.visit_node(stmt)?;
                }

                self.current_scope = prev_scope;
            }

            AstNode::ClassDef { name, body, line, col } => {
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Class,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);

                // Create new class scope
                let class_scope = self.symbol_table.create_scope(ScopeKind::Class, self.current_scope);
                let prev_scope = self.current_scope;
                self.current_scope = class_scope;

                // Visit class body
                for stmt in body {
                    self.visit_node(stmt)?;
                }

                // Restore previous scope
                self.current_scope = prev_scope;
            }

            AstNode::Assignment { target, value, line, col } => {
                // Visit the value expression first
                self.visit_node(value)?;

                // Add variable to current scope
                let symbol = Symbol {
                    name: target.clone(),
                    kind: SymbolKind::Variable,
                    line: *line,
                    col: *col,
                    scope_id: self.current_scope,
                };
                self.symbol_table.add_symbol(self.current_scope, symbol);
            }

            AstNode::Call { args, .. } => {
                // For calls, we just check arguments since function is a String
                for arg in args {
                    self.visit_node(arg)?;
                }
            }

            AstNode::Identifier { .. } => {
                // For identifiers, we could track usage here
                // For now, just continue
            }

            AstNode::Literal { .. } => {
                // Literals don't affect name resolution
            }

            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    self.visit_node(val)?;
                }
            }
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
        Self::new()
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
        let func_scope = table.create_scope(ScopeKind::Function, table.root_scope);

        assert_eq!(table.scopes.len(), 2);
        assert!(table.scopes.contains_key(&func_scope));

        let root_scope = table.scopes.get(&table.root_scope).unwrap();
        assert_eq!(root_scope.children.len(), 1);
        assert_eq!(root_scope.children[0], func_scope);
    }

    #[test]
    fn test_symbol_lookup() {
        let mut table = SymbolTable::new();
        let func_scope = table.create_scope(ScopeKind::Function, table.root_scope);

        let root_symbol = Symbol {
            name: "global_var".to_string(),
            kind: SymbolKind::Variable,
            line: 1,
            col: 1,
            scope_id: table.root_scope,
        };
        table.add_symbol(table.root_scope, root_symbol);

        let func_symbol =
            Symbol { name: "local_var".to_string(), kind: SymbolKind::Variable, line: 2, col: 1, scope_id: func_scope };
        table.add_symbol(func_scope, func_symbol);

        assert!(table.lookup_symbol("local_var", func_scope).is_some());
        assert!(table.lookup_symbol("global_var", func_scope).is_some());
        assert!(table.lookup_symbol("nonexistent", func_scope).is_none());

        assert!(table.lookup_symbol("global_var", table.root_scope).is_some());
        assert!(table.lookup_symbol("local_var", table.root_scope).is_none());
    }

    #[test]
    fn test_name_resolver_basic() {
        let mut resolver = NameResolver::new();

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
        let mut resolver = NameResolver::new();

        let ast = AstNode::FunctionDef {
            name: "test_func".to_string(),
            args: vec!["param1".to_string(), "param2".to_string()],
            body: vec![AstNode::Assignment {
                target: "local_var".to_string(),
                value: Box::new(AstNode::Identifier { name: "param1".to_string(), line: 2, col: 15 }),
                line: 2,
                col: 5,
            }],
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
    fn test_nested_scopes() {
        let mut resolver = NameResolver::new();

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
                    line: 2,
                    col: 1,
                },
            ],
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
