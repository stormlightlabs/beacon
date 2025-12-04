//! Workspace-level type environment for cross-module type propagation
//!
//! Aggregates type information from all modules in the workspace to enable
//! transitive type propagation across module boundaries via the call graph.

use beacon_constraint::ConstraintResult;
use beacon_core::{Type, TypeScheme};
use beacon_parser::{ScopeId, SymbolTable};
use rustc_hash::FxHashMap;
use std::sync::Arc;
use url::Url;

use crate::cfg::FunctionId;

/// Type information for a single module
#[derive(Debug, Clone)]
pub struct ModuleTypeInfo {
    /// The constraint result containing all type information for this module
    pub constraint_result: ConstraintResult,
    /// Symbol table for resolving names to scopes
    pub symbol_table: Arc<SymbolTable>,
    /// Source code for this module
    pub source: String,
}

impl ModuleTypeInfo {
    pub fn new(constraint_result: ConstraintResult, symbol_table: Arc<SymbolTable>, source: String) -> Self {
        Self { constraint_result, symbol_table, source }
    }

    /// Look up the type of a symbol by name in a given scope
    ///
    /// Uses the symbol's position to find its AST node ID, then looks up the type in the type_map.
    pub fn lookup_symbol_type(&self, name: &str, scope_id: ScopeId) -> Option<Type> {
        let scope = self.symbol_table.scopes.get(&scope_id)?;
        let symbol = scope.symbols.get(name)?;
        let node_id = self.constraint_result.2.get(&(symbol.line, symbol.col))?;
        self.constraint_result.1.get(node_id).cloned()
    }
}

/// Workspace-wide type environment for cross-module analysis
///
/// Aggregates type information from all analyzed modules and provides methods to resolve types across module boundaries.
#[derive(Debug, Default)]
pub struct WorkspaceTypeEnvironment {
    /// Type information for each module, indexed by URI
    modules: FxHashMap<Url, ModuleTypeInfo>,

    /// Cached function type schemes: FunctionId -> TypeScheme
    /// This cache is populated during iterative type propagation
    function_types: FxHashMap<FunctionId, TypeScheme>,

    /// Import resolution map: (from_uri, symbol_name) -> (source_uri, defining_scope)
    /// Maps imported symbols to their defining location
    import_map: FxHashMap<(Url, String), (Url, ScopeId)>,
}

impl WorkspaceTypeEnvironment {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add type information for a module
    pub fn add_module(&mut self, uri: Url, module_info: ModuleTypeInfo) {
        self.modules.insert(uri, module_info);
    }

    /// Get type information for a module
    pub fn get_module(&self, uri: &Url) -> Option<&ModuleTypeInfo> {
        self.modules.get(uri)
    }

    /// Get mutable reference to module type information
    pub fn get_module_mut(&mut self, uri: &Url) -> Option<&mut ModuleTypeInfo> {
        self.modules.get_mut(uri)
    }

    /// Register an import mapping
    ///
    /// Records that `symbol_name` imported in module at `from_uri` is defined in module at `source_uri` in scope `defining_scope`.
    pub fn add_import(&mut self, from_uri: Url, symbol_name: String, source_uri: Url, defining_scope: ScopeId) {
        self.import_map
            .insert((from_uri, symbol_name), (source_uri, defining_scope));
    }

    /// Resolve an imported symbol to its defining location
    pub fn resolve_import(&self, from_uri: &Url, symbol_name: &str) -> Option<(Url, ScopeId)> {
        self.import_map
            .get(&(from_uri.clone(), symbol_name.to_string()))
            .cloned()
    }

    /// Get the inferred type scheme for a function
    pub fn get_function_type(&self, function_id: &FunctionId) -> Option<&TypeScheme> {
        self.function_types.get(function_id)
    }

    /// Set the inferred type scheme for a function
    pub fn set_function_type(&mut self, function_id: FunctionId, type_scheme: TypeScheme) {
        self.function_types.insert(function_id, type_scheme);
    }

    /// Get all modules in the workspace
    pub fn modules(&self) -> &FxHashMap<Url, ModuleTypeInfo> {
        &self.modules
    }

    /// Check if a module is present in the workspace
    pub fn contains_module(&self, uri: &Url) -> bool {
        self.modules.contains_key(uri)
    }

    /// Clear all cached function types (used when starting a new propagation iteration)
    pub fn clear_function_types(&mut self) {
        self.function_types.clear();
    }
}

/// Builder for constructing a WorkspaceTypeEnvironment from analyzed modules
pub struct WorkspaceTypeEnvironmentBuilder {
    env: WorkspaceTypeEnvironment,
}

impl WorkspaceTypeEnvironmentBuilder {
    pub fn new() -> Self {
        Self { env: WorkspaceTypeEnvironment::new() }
    }

    /// Add a module to the workspace environment
    pub fn add_module(mut self, uri: Url, module_info: ModuleTypeInfo) -> Self {
        self.env.add_module(uri, module_info);
        self
    }

    /// Build the workspace type environment
    pub fn build(self) -> WorkspaceTypeEnvironment {
        self.env
    }
}

impl Default for WorkspaceTypeEnvironmentBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_constraint::{ConstraintGenContext, ConstraintSet};
    use beacon_parser::{Scope, ScopeKind, Symbol, SymbolKind, SymbolTable};

    #[test]
    fn test_workspace_type_environment_creation() {
        let env = WorkspaceTypeEnvironment::new();
        assert_eq!(env.modules().len(), 0);
    }

    #[test]
    fn test_add_module() {
        let mut env = WorkspaceTypeEnvironment::new();

        let uri = Url::parse("file:///test.py").unwrap();
        let symbol_table = Arc::new(SymbolTable::new());
        let ctx = ConstraintGenContext::new();
        let constraint_result = ConstraintResult(
            ConstraintSet { constraints: vec![] },
            FxHashMap::default(),
            FxHashMap::default(),
            ctx.class_registry,
            FxHashMap::default(),
            FxHashMap::default(),
            ctx.typevar_registry,
        );

        let module_info = ModuleTypeInfo::new(constraint_result, symbol_table, "# test".to_string());

        env.add_module(uri.clone(), module_info);
        assert!(env.contains_module(&uri));
    }

    #[test]
    fn test_import_resolution() {
        let mut env = WorkspaceTypeEnvironment::new();

        let from_uri = Url::parse("file:///main.py").unwrap();
        let source_uri = Url::parse("file:///lib.py").unwrap();
        let scope_id = ScopeId::from_raw(1);

        env.add_import(from_uri.clone(), "func".to_string(), source_uri.clone(), scope_id);

        let resolved = env.resolve_import(&from_uri, "func");
        assert_eq!(resolved, Some((source_uri, scope_id)));
    }

    #[test]
    fn test_function_type_caching() {
        let mut env = WorkspaceTypeEnvironment::new();

        let uri = Url::parse("file:///test.py").unwrap();
        let function_id = FunctionId::new(uri, ScopeId::from_raw(1), "test_func".to_string());
        let type_scheme = TypeScheme::mono(Type::int());

        env.set_function_type(function_id.clone(), type_scheme);

        let retrieved = env.get_function_type(&function_id);
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().ty, Type::int());
    }

    #[test]
    fn test_clear_function_types() {
        let mut env = WorkspaceTypeEnvironment::new();

        let uri = Url::parse("file:///test.py").unwrap();
        let function_id = FunctionId::new(uri, ScopeId::from_raw(1), "test_func".to_string());
        let type_scheme = TypeScheme::mono(Type::int());

        env.set_function_type(function_id.clone(), type_scheme);
        assert!(env.get_function_type(&function_id).is_some());

        env.clear_function_types();
        assert!(env.get_function_type(&function_id).is_none());
    }

    #[test]
    fn test_builder_pattern() {
        let uri = Url::parse("file:///test.py").unwrap();
        let symbol_table = Arc::new(SymbolTable::new());
        let ctx = ConstraintGenContext::new();
        let constraint_result = ConstraintResult(
            ConstraintSet { constraints: vec![] },
            FxHashMap::default(),
            FxHashMap::default(),
            ctx.class_registry,
            FxHashMap::default(),
            FxHashMap::default(),
            ctx.typevar_registry,
        );

        let module_info = ModuleTypeInfo::new(constraint_result, symbol_table, "# test".to_string());

        let env = WorkspaceTypeEnvironmentBuilder::new()
            .add_module(uri.clone(), module_info)
            .build();

        assert!(env.contains_module(&uri));
    }

    #[test]
    fn test_lookup_symbol_type_success() {
        let mut symbol_table = SymbolTable::new();
        let scope_id = ScopeId::from_raw(1);

        let mut scope = Scope {
            id: scope_id,
            kind: ScopeKind::Function,
            parent: Some(symbol_table.root_scope),
            symbols: FxHashMap::default(),
            children: vec![],
            start_byte: 0,
            end_byte: 100,
        };

        scope.symbols.insert(
            "foo".to_string(),
            Symbol {
                name: "foo".to_string(),
                kind: SymbolKind::Function,
                line: 5,
                col: 10,
                end_col: 13,
                scope_id,
                docstring: None,
                references: vec![],
            },
        );

        symbol_table.scopes.insert(scope_id, scope);

        let ctx = ConstraintGenContext::new();
        let mut type_map = FxHashMap::default();
        let mut position_map = FxHashMap::default();

        position_map.insert((5, 10), 42);

        type_map.insert(42, Type::int());

        let constraint_result = ConstraintResult(
            ConstraintSet { constraints: vec![] },
            type_map,
            position_map,
            ctx.class_registry,
            FxHashMap::default(),
            FxHashMap::default(),
            ctx.typevar_registry,
        );

        let module_info = ModuleTypeInfo::new(constraint_result, Arc::new(symbol_table), "# test".to_string());
        let result = module_info.lookup_symbol_type("foo", scope_id);
        assert_eq!(result, Some(Type::int()));
    }

    #[test]
    fn test_lookup_symbol_type_nonexistent_symbol() {
        let _uri = Url::parse("file:///test.py").unwrap();
        let symbol_table = Arc::new(SymbolTable::new());
        let ctx = ConstraintGenContext::new();
        let constraint_result = ConstraintResult(
            ConstraintSet { constraints: vec![] },
            FxHashMap::default(),
            FxHashMap::default(),
            ctx.class_registry,
            FxHashMap::default(),
            FxHashMap::default(),
            ctx.typevar_registry,
        );

        let module_info = ModuleTypeInfo::new(constraint_result, symbol_table, "# test".to_string());
        let result = module_info.lookup_symbol_type("nonexistent", ScopeId::from_raw(999));
        assert_eq!(result, None);
    }

    #[test]
    fn test_lookup_symbol_type_missing_position_mapping() {
        let mut symbol_table = SymbolTable::new();
        let scope_id = ScopeId::from_raw(1);

        let mut scope = Scope {
            id: scope_id,
            kind: ScopeKind::Function,
            parent: Some(symbol_table.root_scope),
            symbols: FxHashMap::default(),
            children: vec![],
            start_byte: 0,
            end_byte: 100,
        };

        scope.symbols.insert(
            "bar".to_string(),
            Symbol {
                name: "bar".to_string(),
                kind: SymbolKind::Variable,
                line: 3,
                col: 5,
                end_col: 8,
                scope_id,
                docstring: None,
                references: vec![],
            },
        );

        symbol_table.scopes.insert(scope_id, scope);

        let ctx = ConstraintGenContext::new();
        let constraint_result = ConstraintResult(
            ConstraintSet { constraints: vec![] },
            FxHashMap::default(),
            FxHashMap::default(),
            ctx.class_registry,
            FxHashMap::default(),
            FxHashMap::default(),
            ctx.typevar_registry,
        );

        let module_info = ModuleTypeInfo::new(constraint_result, Arc::new(symbol_table), "# test".to_string());
        let result = module_info.lookup_symbol_type("bar", scope_id);
        assert_eq!(result, None);
    }
}
