use rustc_hash::{FxHashMap, FxHashSet};

/// Source position with line and column information
#[derive(Debug, Clone, Copy)]
pub(super) struct SourcePosition {
    pub(super) line: usize,
    pub(super) col: usize,
    pub(super) end_col: usize,
}

/// Tracks a global or nonlocal declaration
#[derive(Debug, Clone)]
pub(super) struct GlobalOrNonlocalDecl {
    pub(super) name: String,
    pub(super) line: usize,
    pub(super) col: usize,
    pub(super) end_col: usize,
    pub(super) is_global: bool,
}

/// Context for tracking state during AST traversal
#[derive(Debug, Clone)]
pub(super) struct LinterContext {
    /// Depth of function nesting (0 = module level)
    pub(super) function_depth: usize,
    /// Depth of loop nesting (0 = not in loop)
    pub(super) loop_depth: usize,
    /// Depth of class nesting (0 = module level)
    pub(super) class_depth: usize,
    /// Stack of import names at each scope level
    pub(super) import_names: Vec<FxHashSet<String>>,
    /// Stack of loop variable names
    pub(super) loop_vars: Vec<String>,
    /// Global and nonlocal declarations in current function scope
    pub(super) global_nonlocal_decls: Vec<GlobalOrNonlocalDecl>,
    /// Variables assigned in the current function scope
    pub(super) assigned_vars: FxHashSet<String>,
    /// Track class scopes that are dataclasses (line -> is_dataclass)
    pub(super) dataclass_scopes: FxHashMap<usize, bool>,
    /// Track class scopes that inherit from Protocol (line -> is_protocol)
    pub(super) protocol_scopes: FxHashMap<usize, bool>,
}

impl LinterContext {
    pub(super) fn new() -> Self {
        Self {
            function_depth: 0,
            loop_depth: 0,
            class_depth: 0,
            import_names: vec![FxHashSet::default()],
            loop_vars: Vec::new(),
            global_nonlocal_decls: Vec::new(),
            assigned_vars: FxHashSet::default(),
            dataclass_scopes: FxHashMap::default(),
            protocol_scopes: FxHashMap::default(),
        }
    }

    pub(super) fn enter_function(&mut self) {
        self.function_depth += 1;
        self.global_nonlocal_decls.clear();
        self.assigned_vars.clear();
    }

    pub(super) fn exit_function(&mut self) {
        self.function_depth = self.function_depth.saturating_sub(1);
        self.global_nonlocal_decls.clear();
        self.assigned_vars.clear();
    }

    pub(super) fn track_assignment(&mut self, var_name: String) {
        self.assigned_vars.insert(var_name);
    }

    pub(super) fn add_global_decl(&mut self, name: String, line: usize, col: usize, end_col: usize) {
        self.global_nonlocal_decls
            .push(GlobalOrNonlocalDecl { name, line, col, end_col, is_global: true });
    }

    pub(super) fn add_nonlocal_decl(&mut self, name: String, line: usize, col: usize, end_col: usize) {
        self.global_nonlocal_decls
            .push(GlobalOrNonlocalDecl { name, line, col, end_col, is_global: false });
    }

    pub(super) fn enter_loop(&mut self) {
        self.loop_depth += 1;
    }

    pub(super) fn exit_loop(&mut self) {
        self.loop_depth = self.loop_depth.saturating_sub(1);
    }

    pub(super) fn enter_class(&mut self) {
        self.class_depth += 1;
    }

    pub(super) fn exit_class(&mut self) {
        self.class_depth = self.class_depth.saturating_sub(1);
    }

    pub(super) fn add_import(&mut self, name: String) {
        if let Some(imports) = self.import_names.last_mut() {
            imports.insert(name);
        }
    }

    pub(super) fn is_import(&self, name: &str) -> bool {
        self.import_names.iter().any(|imports| imports.contains(name))
    }

    pub(super) fn add_loop_var(&mut self, name: String) {
        self.loop_vars.push(name);
    }
}
