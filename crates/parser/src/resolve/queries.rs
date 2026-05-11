use super::{ReferenceKind, ScopeKind, Symbol, SymbolKind, SymbolTable};

impl SymbolTable {
    pub fn find_shadowed_symbols(&self) -> Vec<(&Symbol, &Symbol)> {
        let mut shadowed = Vec::new();

        for scope in self.scopes.values() {
            if let Some(parent_id) = scope.parent {
                for (name, child_symbol) in &scope.symbols {
                    if child_symbol.kind == SymbolKind::BuiltinVar || child_symbol.kind == SymbolKind::Parameter {
                        continue;
                    }

                    if let Some(parent_symbol) = self.lookup_symbol(name, parent_id)
                        && parent_symbol.kind != SymbolKind::BuiltinVar
                    {
                        shadowed.push((child_symbol, parent_symbol));
                    }
                }
            }
        }

        shadowed
    }

    pub fn find_unused_symbols(&self) -> Vec<&Symbol> {
        let mut unused = Vec::new();

        for scope in self.scopes.values() {
            let is_class_scope = scope.kind == ScopeKind::Class;

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

                if is_class_scope && symbol.kind == SymbolKind::Variable {
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
