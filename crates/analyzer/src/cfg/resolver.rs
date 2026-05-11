use beacon_parser::AstNode;
use url::Url;

use super::call_graph::{CallKind, FunctionId};

/// Resolves call targets to FunctionIds using symbol tables
///
/// Performs call target resolution by analyzing call expressions and looking up the target function in the symbol table.
/// Supports direct calls, method calls, and detects dynamic calls that cannot be statically resolved.
pub struct CallResolver<'a> {
    symbol_table: &'a beacon_parser::SymbolTable,
    current_uri: &'a Url,
    current_scope: beacon_parser::ScopeId,
}

impl<'a> CallResolver<'a> {
    pub fn new(symbol_table: &'a beacon_parser::SymbolTable, uri: &'a Url, scope: beacon_parser::ScopeId) -> Self {
        Self { symbol_table, current_uri: uri, current_scope: scope }
    }

    /// Resolve a call expression to a FunctionId and CallKind
    pub fn resolve_call(&self, call_node: &AstNode) -> (Option<FunctionId>, CallKind) {
        match call_node {
            AstNode::Call { function, .. } => self.resolve_call_target(function),
            _ => (None, CallKind::Dynamic),
        }
    }

    fn resolve_call_target(&self, function: &AstNode) -> (Option<FunctionId>, CallKind) {
        match function {
            AstNode::Identifier { name, .. } => {
                if let Some(symbol) = self.symbol_table.lookup_symbol(name, self.current_scope) {
                    match symbol.kind {
                        beacon_parser::SymbolKind::Function | beacon_parser::SymbolKind::Import => {
                            let func_id = FunctionId::new(self.current_uri.clone(), symbol.scope_id, name.clone());
                            (Some(func_id), CallKind::Direct)
                        }
                        _ => (None, CallKind::Dynamic),
                    }
                } else {
                    (None, CallKind::Direct)
                }
            }
            // FIXME: Full method resolution requires type inference
            AstNode::Attribute { .. } => (None, CallKind::Method),
            _ => (None, CallKind::Dynamic),
        }
    }
}
