use rustc_hash::FxHashMap;
use std::collections::HashMap;

use super::{BUILTIN_DUNDERS, ReferenceKind, Scope, ScopeId, ScopeKind, Symbol, SymbolKind, SymbolReference};

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

    pub fn add_symbol(&mut self, scope_id: ScopeId, symbol: Symbol) {
        if let Some(scope) = self.scopes.get_mut(&scope_id) {
            scope.symbols.insert(symbol.name.clone(), symbol);
        }
    }

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

    pub fn get_scope_symbols(&self, scope_id: ScopeId) -> Option<&FxHashMap<String, Symbol>> {
        self.scopes.get(&scope_id).map(|scope| &scope.symbols)
    }

    pub fn get_scope(&self, scope_id: ScopeId) -> Option<&Scope> {
        self.scopes.get(&scope_id)
    }

    pub fn find_scope_at_position(&self, byte_offset: usize) -> ScopeId {
        self.find_scope_at_position_recursive(self.root_scope, byte_offset)
            .unwrap_or(self.root_scope)
    }

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

    pub fn add_reference(
        &mut self, name: &str, from_scope: ScopeId, line: usize, col: usize, end_col: usize, kind: ReferenceKind,
    ) -> bool {
        let mut current_scope = from_scope;

        while let Some(scope) = self.scopes.get(&current_scope) {
            let scope_id = scope.id;

            if scope.symbols.contains_key(name) {
                if let Some(scope_mut) = self.scopes.get_mut(&scope_id)
                    && let Some(symbol) = scope_mut.symbols.get_mut(name)
                {
                    symbol.references.push(SymbolReference { line, col, end_col, kind });
                    return true;
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
                    end_col: dunder_name.len(),
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
