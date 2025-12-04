//! Data flow analysis for Python code
//!
//! Implements various data flow analyses on top of the CFG:
//! - Definite assignment (use-before-def detection)
//! - Unreachable code detection
//! - Unused variable detection

use super::cfg::{BlockId, ControlFlowGraph};

use beacon_parser::{AstNode, BinaryOperator, CompareOperator, LiteralValue, ScopeId, SymbolTable, UnaryOperator};
use rustc_hash::{FxHashMap, FxHashSet};

/// Result of data flow analysis
#[derive(Debug, Clone)]
pub struct DataFlowResult {
    /// Variables used before being definitely assigned
    pub use_before_def: Vec<UseBeforeDef>,
    /// Unreachable code locations
    pub unreachable_code: Vec<UnreachableCode>,
    /// Unused variables (assigned but never read)
    pub unused_variables: Vec<UnusedVariable>,
}

/// A variable used before being definitely assigned
#[derive(Debug, Clone)]
pub struct UseBeforeDef {
    pub var_name: String,
    pub line: usize,
    pub col: usize,
}

/// Unreachable code location
#[derive(Debug, Clone)]
pub struct UnreachableCode {
    pub block_id: BlockId,
    pub line: usize,
    pub col: usize,
}

/// An unused variable
#[derive(Debug, Clone)]
pub struct UnusedVariable {
    pub var_name: String,
    pub line: usize,
    pub col: usize,
}

/// A constant value that can be propagated through data flow analysis
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    /// Boolean constant
    Bool(bool),
    /// Integer constant
    Integer(i64),
    /// String constant
    String(String),
    /// None constant
    None,
    /// Unknown or non-constant value
    Unknown,
}

/// Result of evaluating a condition symbolically
#[derive(Debug, Clone, PartialEq)]
pub enum ConditionResult {
    /// Condition is always true
    AlwaysTrue,
    /// Condition is always false
    AlwaysFalse,
    /// Cannot determine condition at compile time
    Unknown,
}

/// Check if a name is a Python builtin
pub fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "print"
            | "len"
            | "range"
            | "str"
            | "int"
            | "float"
            | "bool"
            | "list"
            | "dict"
            | "set"
            | "tuple"
            | "abs"
            | "all"
            | "any"
            | "ascii"
            | "bin"
            | "callable"
            | "chr"
            | "compile"
            | "complex"
            | "delattr"
            | "dir"
            | "divmod"
            | "enumerate"
            | "eval"
            | "exec"
            | "filter"
            | "format"
            | "frozenset"
            | "getattr"
            | "globals"
            | "hasattr"
            | "hash"
            | "help"
            | "hex"
            | "id"
            | "input"
            | "isinstance"
            | "issubclass"
            | "iter"
            | "locals"
            | "map"
            | "max"
            | "min"
            | "next"
            | "object"
            | "oct"
            | "open"
            | "ord"
            | "pow"
            | "property"
            | "repr"
            | "reversed"
            | "round"
            | "setattr"
            | "slice"
            | "sorted"
            | "staticmethod"
            | "sum"
            | "super"
            | "type"
            | "vars"
            | "zip"
            | "__import__"
            | "True"
            | "False"
            | "None"
            | "NotImplemented"
            | "Ellipsis"
            | "__debug__"
            | "quit"
            | "exit"
            | "copyright"
            | "credits"
            | "license"
    )
}

/// Get all Python builtins as a set
pub fn get_builtins() -> FxHashSet<String> {
    [
        "print",
        "len",
        "range",
        "str",
        "int",
        "float",
        "bool",
        "list",
        "dict",
        "set",
        "tuple",
        "abs",
        "all",
        "any",
        "ascii",
        "bin",
        "callable",
        "chr",
        "compile",
        "complex",
        "delattr",
        "dir",
        "divmod",
        "enumerate",
        "eval",
        "exec",
        "filter",
        "format",
        "frozenset",
        "getattr",
        "globals",
        "hasattr",
        "hash",
        "help",
        "hex",
        "id",
        "input",
        "isinstance",
        "issubclass",
        "iter",
        "locals",
        "map",
        "max",
        "min",
        "next",
        "object",
        "oct",
        "open",
        "ord",
        "pow",
        "property",
        "repr",
        "reversed",
        "round",
        "setattr",
        "slice",
        "sorted",
        "staticmethod",
        "sum",
        "super",
        "type",
        "vars",
        "zip",
        "__import__",
        "True",
        "False",
        "None",
        "NotImplemented",
        "Ellipsis",
        "__debug__",
        "quit",
        "exit",
        "copyright",
        "credits",
        "license",
    ]
    .iter()
    .map(|s| (*s).to_string())
    .collect()
}

/// Performs data flow analysis on a CFG
/// TODO: Store condition expressions in CFG blocks to automatically detect unreachable branches based on constant conditions.
pub struct DataFlowAnalyzer<'a> {
    cfg: &'a ControlFlowGraph,
    /// Flattened list of ALL statements (including nested ones) for indexing
    all_statements: Vec<&'a AstNode>,
    symbol_table: &'a SymbolTable,
    /// Scope ID of the function being analyzed (for parameter lookup)
    scope_id: ScopeId,
    /// Function and class names defined at the top level of this scope (hoisted definitions)
    hoisted_definitions: FxHashSet<String>,
    /// All symbols defined in this scope (used to detect builtin shadowing)
    defined_symbols: FxHashSet<String>,
}

impl<'a> DataFlowAnalyzer<'a> {
    pub fn new(
        cfg: &'a ControlFlowGraph, function_body: &'a [AstNode], symbol_table: &'a SymbolTable, scope_id: ScopeId,
        parent_hoisted: Option<&FxHashSet<String>>,
    ) -> Self {
        let mut all_statements = Vec::new();
        for stmt in function_body {
            Self::collect_all_statements(stmt, &mut all_statements);
        }

        let mut hoisted_definitions = Self::collect_hoisted_definitions(function_body);

        if let Some(parent) = parent_hoisted {
            hoisted_definitions.extend(parent.iter().cloned());
        }

        let defined_symbols = symbol_table
            .get_scope(scope_id)
            .map(|scope| scope.symbols.keys().cloned().collect())
            .unwrap_or_default();

        Self { cfg, all_statements, symbol_table, scope_id, hoisted_definitions, defined_symbols }
    }

    /// Recursively collect all statements including nested ones
    fn collect_all_statements(node: &'a AstNode, statements: &mut Vec<&'a AstNode>) {
        match node {
            AstNode::If { body, elif_parts, else_body, .. } => {
                statements.push(node);
                for stmt in body {
                    Self::collect_all_statements(stmt, statements);
                }

                for (_, elif_body) in elif_parts {
                    for stmt in elif_body {
                        Self::collect_all_statements(stmt, statements);
                    }
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_all_statements(stmt, statements);
                    }
                }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                statements.push(node);
                for stmt in body {
                    Self::collect_all_statements(stmt, statements);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_all_statements(stmt, statements);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    Self::collect_all_statements(stmt, statements);
                }

                for handler in handlers {
                    for stmt in &handler.body {
                        Self::collect_all_statements(stmt, statements);
                    }
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_all_statements(stmt, statements);
                    }
                }

                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        Self::collect_all_statements(stmt, statements);
                    }
                }
            }
            AstNode::With { body, .. } => {
                statements.push(node);
                for stmt in body {
                    Self::collect_all_statements(stmt, statements);
                }
            }
            AstNode::Match { cases, .. } => {
                statements.push(node);
                for case in cases {
                    for stmt in &case.body {
                        Self::collect_all_statements(stmt, statements);
                    }
                }
            }
            _ => {
                statements.push(node);
            }
        }
    }

    /// Collect function and class names defined at the top level of the scope.
    ///
    /// In Python, function and class definitions are hoised.
    /// Here we implement Python's two-pass name resolution.
    pub fn collect_hoisted_definitions(function_body: &[AstNode]) -> FxHashSet<String> {
        let mut hoisted = FxHashSet::default();

        for stmt in function_body {
            Self::collect_hoisted_from_node(stmt, &mut hoisted);
        }

        hoisted
    }

    /// Recursively collect hoisted definitions from a node and its control flow bodies.
    fn collect_hoisted_from_node(node: &AstNode, hoisted: &mut FxHashSet<String>) {
        match node {
            AstNode::FunctionDef { name, .. } => {
                hoisted.insert(name.clone());
            }
            AstNode::ClassDef { name, .. } => {
                hoisted.insert(name.clone());
            }
            AstNode::Import { module, alias, .. } => {
                let name = alias.as_ref().unwrap_or(module);
                hoisted.insert(name.clone());
            }
            AstNode::ImportFrom { names, .. } => {
                for iname in names {
                    hoisted.insert(iname.name.clone());
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    Self::collect_hoisted_from_node(stmt, hoisted);
                }
                for (_, elif_body) in elif_parts {
                    for stmt in elif_body {
                        Self::collect_hoisted_from_node(stmt, hoisted);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_hoisted_from_node(stmt, hoisted);
                    }
                }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                for stmt in body {
                    Self::collect_hoisted_from_node(stmt, hoisted);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_hoisted_from_node(stmt, hoisted);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    Self::collect_hoisted_from_node(stmt, hoisted);
                }
                for handler in handlers {
                    for stmt in &handler.body {
                        Self::collect_hoisted_from_node(stmt, hoisted);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_hoisted_from_node(stmt, hoisted);
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        Self::collect_hoisted_from_node(stmt, hoisted);
                    }
                }
            }
            AstNode::With { body, .. } => {
                for stmt in body {
                    Self::collect_hoisted_from_node(stmt, hoisted);
                }
            }
            AstNode::Match { cases, .. } => {
                for case in cases {
                    for stmt in &case.body {
                        Self::collect_hoisted_from_node(stmt, hoisted);
                    }
                }
            }
            _ => {}
        }
    }

    /// Run all data flow analyses and return combined results
    pub fn analyze(&self) -> DataFlowResult {
        let use_before_def = self.find_use_before_def();
        let unreachable_code = self.find_unreachable_code();
        let unused_variables = self.find_unused_variables();
        DataFlowResult { use_before_def, unreachable_code, unused_variables }
    }

    /// Find variables used before being definitely assigned
    ///
    /// Uses forward data flow analysis to track which variables are definitely assigned on all paths to each program point.
    fn find_use_before_def(&self) -> Vec<UseBeforeDef> {
        let mut def_in: FxHashMap<BlockId, FxHashSet<String>> = FxHashMap::default();
        let mut def_out: FxHashMap<BlockId, FxHashSet<String>> = FxHashMap::default();
        let mut initialized_blocks: FxHashSet<BlockId> = FxHashSet::default();

        let builtins = get_builtins();
        let parameters = self.get_parameters();
        let mut worklist: Vec<BlockId> = self.cfg.blocks.keys().copied().collect();

        while let Some(block_id) = worklist.pop() {
            let block = self.cfg.blocks.get(&block_id).unwrap();

            let new_def_in = if block.predecessors.is_empty() {
                let mut initial_defs = self.hoisted_definitions.clone();
                initial_defs.extend(builtins.iter().filter(|name| !self.scope_defines(name)).cloned());
                initial_defs.extend(parameters.iter().cloned());
                initial_defs
            } else {
                let mut result_opt: Option<FxHashSet<String>> = None;
                for &(pred_id, _) in &block.predecessors {
                    if !initialized_blocks.contains(&pred_id) {
                        continue;
                    }

                    if let Some(pred_def_out) = def_out.get(&pred_id) {
                        match &mut result_opt {
                            Some(res) => res.retain(|var| pred_def_out.contains(var)),
                            None => {
                                result_opt = Some(pred_def_out.clone());
                            }
                        }
                    }
                }

                match result_opt {
                    Some(set) => set,
                    None => continue,
                }
            };

            let mut new_def_out = new_def_in.clone();
            for &stmt_idx in &block.statements {
                if stmt_idx < self.all_statements.len() {
                    let stmt = self.all_statements[stmt_idx];
                    let (_, defs) = self.get_uses_and_defs(stmt);

                    for def_var in defs {
                        new_def_out.insert(def_var);
                    }
                }
            }

            let old_def_in = def_in.get(&block_id).cloned().unwrap_or_default();
            let old_def_out = def_out.get(&block_id).cloned().unwrap_or_default();
            if new_def_in != old_def_in || new_def_out != old_def_out || !initialized_blocks.contains(&block_id) {
                def_in.insert(block_id, new_def_in);
                def_out.insert(block_id, new_def_out);
                initialized_blocks.insert(block_id);

                for &(succ_id, _) in &block.successors {
                    if !worklist.contains(&succ_id) {
                        worklist.push(succ_id);
                    }
                }
            }
        }

        let mut errors = Vec::new();
        for (block_id, block) in &self.cfg.blocks {
            let mut current_def_in = def_in.get(block_id).cloned().unwrap_or_default();

            for &stmt_idx in &block.statements {
                if stmt_idx < self.all_statements.len() {
                    let stmt = self.all_statements[stmt_idx];
                    let (uses, defs) = self.get_uses_and_defs(stmt);

                    for (var_name, line, col) in uses {
                        if is_builtin(&var_name) && !self.scope_defines(&var_name) {
                            continue;
                        }

                        if !current_def_in.contains(&var_name) && !self.is_parameter(&var_name) {
                            errors.push(UseBeforeDef { var_name, line, col });
                        }
                    }

                    for def_var in defs {
                        current_def_in.insert(def_var);
                    }
                }
            }
        }

        errors
    }

    fn find_unreachable_code(&self) -> Vec<UnreachableCode> {
        let unreachable_blocks = self.cfg.unreachable_blocks();
        let mut unreachable = Vec::new();

        for block_id in unreachable_blocks {
            if let Some(block) = self.cfg.blocks.get(&block_id) {
                if let Some(&stmt_idx) = block.statements.first() {
                    if stmt_idx < self.all_statements.len() {
                        let (line, col) = self.get_stmt_location(self.all_statements[stmt_idx]);
                        unreachable.push(UnreachableCode { block_id, line, col });
                    }
                }
            }
        }

        unreachable
    }

    /// Find unused variables (already implemented in symbol table)
    ///
    /// This delegates to the symbol table's unused variable detection, which tracks references during name resolution.
    fn find_unused_variables(&self) -> Vec<UnusedVariable> {
        self.symbol_table
            .find_unused_symbols()
            .iter()
            .map(|sym| UnusedVariable { var_name: sym.name.clone(), line: sym.line, col: sym.col })
            .collect()
    }

    /// Extract variable uses and definitions from a statement
    fn get_uses_and_defs(&self, stmt: &AstNode) -> (Vec<(String, usize, usize)>, Vec<String>) {
        let mut uses = Vec::new();
        let mut defs = Vec::new();

        match stmt {
            AstNode::Assignment { target, value, .. } => {
                Self::collect_uses(value, &mut uses);
                defs.extend(target.extract_target_names());
            }
            AstNode::AnnotatedAssignment { target, value, .. } => {
                if let Some(val) = value {
                    Self::collect_uses(val, &mut uses);
                }
                defs.extend(target.extract_target_names());
            }
            AstNode::For { target, iter, .. } => {
                Self::collect_uses(iter, &mut uses);
                defs.extend(target.extract_target_names());
            }
            AstNode::NamedExpr { target, value, .. } => {
                Self::collect_uses(value, &mut uses);
                defs.push(target.clone());
            }
            _ => Self::collect_uses(stmt, &mut uses),
        }

        (uses, defs)
    }

    /// Recursively collect all identifier uses in an expression
    fn collect_uses(node: &AstNode, uses: &mut Vec<(String, usize, usize)>) {
        match node {
            AstNode::Identifier { name, line, col, .. } => {
                uses.push((name.clone(), *line, *col));
            }
            AstNode::Call { function, args, keywords, .. } => {
                Self::collect_uses(function, uses);
                for arg in args {
                    Self::collect_uses(arg, uses);
                }

                for (_name, value) in keywords {
                    Self::collect_uses(value, uses);
                }
            }
            AstNode::BinaryOp { left, right, .. } => {
                Self::collect_uses(left, uses);
                Self::collect_uses(right, uses);
            }
            AstNode::UnaryOp { operand, .. } => {
                Self::collect_uses(operand, uses);
            }
            AstNode::Compare { left, comparators, .. } => {
                Self::collect_uses(left, uses);
                for comp in comparators {
                    Self::collect_uses(comp, uses);
                }
            }
            AstNode::Attribute { object, .. } => {
                Self::collect_uses(object, uses);
            }
            AstNode::Subscript { value, slice, .. } => {
                Self::collect_uses(value, uses);
                Self::collect_uses(slice, uses);
            }
            AstNode::Return { value: Some(val), .. } => Self::collect_uses(val, uses),
            AstNode::Raise { exc: Some(exception), .. } => Self::collect_uses(exception, uses),
            _ => {}
        }
    }

    fn is_parameter(&self, name: &str) -> bool {
        if let Some(scope) = self.symbol_table.get_scope(self.scope_id) {
            scope
                .symbols
                .values()
                .any(|sym| sym.name == name && sym.kind == beacon_parser::SymbolKind::Parameter)
        } else {
            false
        }
    }

    fn scope_defines(&self, name: &str) -> bool {
        self.defined_symbols.contains(name)
    }

    /// Get all function parameters for this scope
    fn get_parameters(&self) -> FxHashSet<String> {
        if let Some(scope) = self.symbol_table.get_scope(self.scope_id) {
            scope
                .symbols
                .values()
                .filter(|sym| sym.kind == beacon_parser::SymbolKind::Parameter)
                .map(|sym| sym.name.clone())
                .collect()
        } else {
            FxHashSet::default()
        }
    }

    fn get_stmt_location(&self, stmt: &AstNode) -> (usize, usize) {
        match stmt {
            AstNode::Assignment { line, col, .. }
            | AstNode::AnnotatedAssignment { line, col, .. }
            | AstNode::Call { line, col, .. }
            | AstNode::Return { line, col, .. }
            | AstNode::If { line, col, .. }
            | AstNode::For { line, col, .. }
            | AstNode::While { line, col, .. }
            | AstNode::Try { line, col, .. }
            | AstNode::With { line, col, .. }
            | AstNode::Match { line, col, .. }
            | AstNode::FunctionDef { line, col, .. }
            | AstNode::ClassDef { line, col, .. }
            | AstNode::Import { line, col, .. }
            | AstNode::ImportFrom { line, col, .. }
            | AstNode::Pass { line, col, .. }
            | AstNode::Break { line, col, .. }
            | AstNode::Continue { line, col, .. }
            | AstNode::Raise { line, col, .. } => (*line, *col),
            _ => (0, 0),
        }
    }

    /// Perform constant propagation analysis
    ///
    /// Tracks which variables have known constant values at each program point.
    pub fn propagate_constants(&self) -> FxHashMap<BlockId, FxHashMap<String, ConstantValue>> {
        let mut const_in: FxHashMap<BlockId, FxHashMap<String, ConstantValue>> = FxHashMap::default();
        let mut const_out: FxHashMap<BlockId, FxHashMap<String, ConstantValue>> = FxHashMap::default();

        for block_id in self.cfg.blocks.keys() {
            const_in.insert(*block_id, FxHashMap::default());
            const_out.insert(*block_id, FxHashMap::default());
        }

        let mut worklist: Vec<BlockId> = self.cfg.blocks.keys().copied().collect();

        while let Some(block_id) = worklist.pop() {
            let block = self.cfg.blocks.get(&block_id).unwrap();

            let new_const_in = if block.predecessors.is_empty() {
                FxHashMap::default()
            } else {
                let first_pred = block.predecessors[0].0;
                let mut result = const_out.get(&first_pred).cloned().unwrap_or_default();

                for &(pred_id, _) in &block.predecessors[1..] {
                    if let Some(pred_const_out) = const_out.get(&pred_id) {
                        result
                            .retain(|var, value| pred_const_out.get(var).is_some_and(|pred_value| pred_value == value));
                    } else {
                        result.clear();
                    }
                }
                result
            };

            let mut new_const_out = new_const_in.clone();
            for &stmt_idx in &block.statements {
                if stmt_idx < self.all_statements.len() {
                    let stmt = self.all_statements[stmt_idx];
                    self.update_constants_for_stmt(stmt, &mut new_const_out);
                }
            }

            let old_const_in = const_in.get(&block_id).cloned().unwrap_or_default();
            let old_const_out = const_out.get(&block_id).cloned().unwrap_or_default();
            if new_const_in != old_const_in || new_const_out != old_const_out {
                const_in.insert(block_id, new_const_in);
                const_out.insert(block_id, new_const_out);

                for &(succ_id, _) in &block.successors {
                    if !worklist.contains(&succ_id) {
                        worklist.push(succ_id);
                    }
                }
            }
        }

        const_in
    }

    fn update_constants_for_stmt(&self, stmt: &AstNode, constants: &mut FxHashMap<String, ConstantValue>) {
        match stmt {
            AstNode::Assignment { target, value, .. } => {
                let const_value = self.evaluate_to_constant(value, constants);
                for name in target.extract_target_names() {
                    constants.insert(name, const_value.clone());
                }
            }
            AstNode::AnnotatedAssignment { target, value: Some(val), .. } => {
                let const_value = self.evaluate_to_constant(val, constants);
                for name in target.extract_target_names() {
                    constants.insert(name, const_value.clone());
                }
            }
            AstNode::For { target, .. } => {
                for name in target.extract_target_names() {
                    constants.insert(name, ConstantValue::Unknown);
                }
            }
            AstNode::NamedExpr { target, value, .. } => {
                let const_value = self.evaluate_to_constant(value, constants);
                constants.insert(target.clone(), const_value);
            }
            _ => {}
        };
    }

    /// Evaluate an expression to a constant value if possible
    fn evaluate_to_constant(&self, expr: &AstNode, constants: &FxHashMap<String, ConstantValue>) -> ConstantValue {
        match expr {
            AstNode::Literal { value, .. } => match value {
                LiteralValue::Integer(i) => ConstantValue::Integer(*i),
                LiteralValue::Float(_) => ConstantValue::Unknown,
                LiteralValue::String { value, .. } => ConstantValue::String(value.clone()),
                LiteralValue::Boolean(b) => ConstantValue::Bool(*b),
                LiteralValue::None => ConstantValue::None,
            },
            AstNode::Identifier { name, .. } => constants.get(name).cloned().unwrap_or(ConstantValue::Unknown),
            AstNode::UnaryOp { op, operand, .. } => {
                let operand_value = self.evaluate_to_constant(operand, constants);
                match (op, operand_value) {
                    (UnaryOperator::Not, ConstantValue::Bool(b)) => ConstantValue::Bool(!b),
                    (UnaryOperator::Plus, ConstantValue::Integer(i)) => ConstantValue::Integer(i),
                    (UnaryOperator::Minus, ConstantValue::Integer(i)) => ConstantValue::Integer(-i),
                    _ => ConstantValue::Unknown,
                }
            }
            AstNode::BinaryOp { left, op, right, .. } => {
                let left_value = self.evaluate_to_constant(left, constants);
                let right_value = self.evaluate_to_constant(right, constants);
                self.evaluate_binary_op(&left_value, op, &right_value)
            }
            _ => ConstantValue::Unknown,
        }
    }

    /// Evaluate a binary operation on constant values
    fn evaluate_binary_op(&self, left: &ConstantValue, op: &BinaryOperator, right: &ConstantValue) -> ConstantValue {
        match (left, op, right) {
            (ConstantValue::Integer(a), BinaryOperator::Add, ConstantValue::Integer(b)) => {
                ConstantValue::Integer(a.wrapping_add(*b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Sub, ConstantValue::Integer(b)) => {
                ConstantValue::Integer(a.wrapping_sub(*b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Mult, ConstantValue::Integer(b)) => {
                ConstantValue::Integer(a.wrapping_mul(*b))
            }
            (ConstantValue::String(a), BinaryOperator::Add, ConstantValue::String(b)) => {
                ConstantValue::String(format!("{a}{b}"))
            }
            (ConstantValue::Bool(a), BinaryOperator::And, ConstantValue::Bool(b)) => ConstantValue::Bool(*a && *b),
            (ConstantValue::Bool(a), BinaryOperator::Or, ConstantValue::Bool(b)) => ConstantValue::Bool(*a || *b),
            _ => ConstantValue::Unknown,
        }
    }

    /// Evaluate a condition expression symbolically
    pub fn evaluate_condition(
        &self, condition: &AstNode, constants: &FxHashMap<String, ConstantValue>,
    ) -> ConditionResult {
        match condition {
            AstNode::Literal { value: LiteralValue::Boolean(b), .. } => {
                if *b {
                    ConditionResult::AlwaysTrue
                } else {
                    ConditionResult::AlwaysFalse
                }
            }
            AstNode::Identifier { name, .. } => match constants.get(name) {
                Some(ConstantValue::Bool(true)) => ConditionResult::AlwaysTrue,
                Some(ConstantValue::Bool(false)) => ConditionResult::AlwaysFalse,
                _ => ConditionResult::Unknown,
            },
            AstNode::UnaryOp { op: beacon_parser::UnaryOperator::Not, operand, .. } => {
                match self.evaluate_condition(operand, constants) {
                    ConditionResult::AlwaysTrue => ConditionResult::AlwaysFalse,
                    ConditionResult::AlwaysFalse => ConditionResult::AlwaysTrue,
                    ConditionResult::Unknown => ConditionResult::Unknown,
                }
            }
            AstNode::Compare { left, ops, comparators, .. } => {
                if ops.len() == 1 && comparators.len() == 1 {
                    let left_value = self.evaluate_to_constant(left, constants);
                    let right_value = self.evaluate_to_constant(&comparators[0], constants);
                    self.evaluate_comparison(&left_value, &ops[0], &right_value)
                } else {
                    ConditionResult::Unknown
                }
            }
            AstNode::BinaryOp { left, op, right, .. } => match op {
                beacon_parser::BinaryOperator::And => {
                    let left_result = self.evaluate_condition(left, constants);
                    let right_result = self.evaluate_condition(right, constants);
                    match (left_result, right_result) {
                        (ConditionResult::AlwaysFalse, _) | (_, ConditionResult::AlwaysFalse) => {
                            ConditionResult::AlwaysFalse
                        }
                        (ConditionResult::AlwaysTrue, ConditionResult::AlwaysTrue) => ConditionResult::AlwaysTrue,
                        _ => ConditionResult::Unknown,
                    }
                }
                beacon_parser::BinaryOperator::Or => {
                    let left_result = self.evaluate_condition(left, constants);
                    let right_result = self.evaluate_condition(right, constants);
                    match (left_result, right_result) {
                        (ConditionResult::AlwaysTrue, _) | (_, ConditionResult::AlwaysTrue) => {
                            ConditionResult::AlwaysTrue
                        }
                        (ConditionResult::AlwaysFalse, ConditionResult::AlwaysFalse) => ConditionResult::AlwaysFalse,
                        _ => ConditionResult::Unknown,
                    }
                }
                _ => {
                    let value = self.evaluate_to_constant(condition, constants);
                    match value {
                        ConstantValue::Bool(true) => ConditionResult::AlwaysTrue,
                        ConstantValue::Bool(false) => ConditionResult::AlwaysFalse,
                        _ => ConditionResult::Unknown,
                    }
                }
            },
            _ => ConditionResult::Unknown,
        }
    }

    /// Evaluate a comparison operation
    fn evaluate_comparison(
        &self, left: &ConstantValue, op: &CompareOperator, right: &ConstantValue,
    ) -> ConditionResult {
        let result = match (left, op, right) {
            (ConstantValue::Integer(a), CompareOperator::Eq, ConstantValue::Integer(b)) => Some(a == b),
            (ConstantValue::Integer(a), CompareOperator::NotEq, ConstantValue::Integer(b)) => Some(a != b),
            (ConstantValue::Integer(a), CompareOperator::Lt, ConstantValue::Integer(b)) => Some(a < b),
            (ConstantValue::Integer(a), CompareOperator::LtE, ConstantValue::Integer(b)) => Some(a <= b),
            (ConstantValue::Integer(a), CompareOperator::Gt, ConstantValue::Integer(b)) => Some(a > b),
            (ConstantValue::Integer(a), CompareOperator::GtE, ConstantValue::Integer(b)) => Some(a >= b),
            (ConstantValue::Bool(a), CompareOperator::Eq, ConstantValue::Bool(b)) => Some(a == b),
            (ConstantValue::Bool(a), CompareOperator::NotEq, ConstantValue::Bool(b)) => Some(a != b),
            (ConstantValue::String(a), CompareOperator::Eq, ConstantValue::String(b)) => Some(a == b),
            (ConstantValue::String(a), CompareOperator::NotEq, ConstantValue::String(b)) => Some(a != b),
            _ => None,
        };

        match result {
            Some(true) => ConditionResult::AlwaysTrue,
            Some(false) => ConditionResult::AlwaysFalse,
            None => ConditionResult::Unknown,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfg::{CfgBuilder, ControlFlowGraph};
    use beacon_parser::{NameResolver, PythonParser, ScopeId, ScopeKind};
    use serde_json;

    macro_rules! data_flow_fixture {
        ($name:literal) => {{
            serde_json::from_str::<AstNode>(include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/src/fixtures/data_flow/",
                $name,
                ".json"
            )))
            .expect("invalid data flow fixture JSON")
        }};
    }

    struct FunctionFixture {
        ast: AstNode,
        resolver: NameResolver,
        cfg: ControlFlowGraph,
        scope_id: ScopeId,
        function_name: Option<String>,
    }

    impl FunctionFixture {
        fn new(source: &str, ast: AstNode, function_name: Option<&str>) -> Self {
            let mut resolver = NameResolver::new(source.to_string());
            resolver.resolve(&ast).expect("Failed to resolve fixture AST");

            let mut builder = CfgBuilder::new();
            let body = Self::function_body(&ast, function_name);
            builder.build_function(body);
            let cfg = builder.build();
            let function_node = Self::select_function(&ast, function_name);
            let (line, col) = match function_node {
                AstNode::FunctionDef { line, col, .. } => (*line, *col),
                _ => unreachable!("selected node must be a function"),
            };
            let byte_offset = line_col_to_byte_offset(source, line, col);
            let scope_id = resolver.symbol_table.find_scope_at_position(byte_offset);
            debug_assert!(
                resolver
                    .symbol_table
                    .get_scope(scope_id)
                    .is_some_and(|scope| scope.kind == ScopeKind::Function),
                "expected function scope at line {line}, col {col}"
            );

            Self { ast, resolver, cfg, scope_id, function_name: function_name.map(|name| name.to_string()) }
        }

        fn function(&self) -> &AstNode {
            match &self.ast {
                AstNode::FunctionDef { .. } => &self.ast,
                AstNode::Module { body, .. } => {
                    if let Some(name) = &self.function_name {
                        body.iter()
                            .find(|node| matches!(node, AstNode::FunctionDef { name: func_name, .. } if func_name == name))
                            .expect("function not found in module fixture")
                    } else {
                        body.iter()
                            .find(|node| matches!(node, AstNode::FunctionDef { .. }))
                            .expect("no function in module fixture")
                    }
                }
                _ => panic!("fixture root must be FunctionDef or Module"),
            }
        }

        fn function_body<'a>(ast: &'a AstNode, function_name: Option<&'a str>) -> &'a [AstNode] {
            match Self::select_function(ast, function_name) {
                AstNode::FunctionDef { body, .. } => body,
                _ => unreachable!(),
            }
        }

        fn select_function<'a>(ast: &'a AstNode, function_name: Option<&str>) -> &'a AstNode {
            match ast {
                AstNode::FunctionDef { .. } => ast,
                AstNode::Module { body, .. } => {
                    if let Some(name) = function_name {
                        body.iter()
                            .find(|node| matches!(node, AstNode::FunctionDef { name: func_name, .. } if func_name == name))
                            .expect("function not found in module fixture")
                    } else {
                        body.iter()
                            .find(|node| matches!(node, AstNode::FunctionDef { .. }))
                            .expect("no function in module fixture")
                    }
                }
                _ => panic!("fixture root must be FunctionDef or Module"),
            }
        }

        fn analyzer(&self, parent_hoisted: Option<&FxHashSet<String>>) -> DataFlowAnalyzer<'_> {
            DataFlowAnalyzer::new(
                &self.cfg,
                self.body(),
                &self.resolver.symbol_table,
                self.scope_id,
                parent_hoisted,
            )
        }

        fn body(&self) -> &[AstNode] {
            if let AstNode::FunctionDef { body, .. } = self.function() { body } else { unreachable!() }
        }

        fn module_body(&self) -> Option<&[AstNode]> {
            if let AstNode::Module { body, .. } = &self.ast { Some(body) } else { None }
        }
    }

    fn parse_source_to_ast(source: &str) -> AstNode {
        let mut parser = PythonParser::new().expect("failed to initialize parser");
        let parsed = parser.parse(source).expect("failed to parse source");
        parser.to_ast(&parsed).expect("failed to convert parse tree to AST")
    }

    fn line_col_to_byte_offset(source: &str, line: usize, col: usize) -> usize {
        let mut current_line = 1;
        let mut current_col = 1;
        let mut offset = 0;

        for ch in source.chars() {
            if current_line == line && current_col == col {
                return offset;
            }

            if ch == '\n' {
                current_line += 1;
                current_col = 1;
            } else {
                current_col += 1;
            }

            offset += ch.len_utf8();
        }

        offset
    }

    #[test]
    fn test_use_before_def_detection() {
        let source = "def foo():\n    y = x\n    x = 1";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_use_before_def_detection"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(!result.is_empty(), "Expected use-before-def for x");
        assert!(
            result.iter().any(|e| e.var_name == "x"),
            "Expected x to be flagged as use-before-def"
        );
    }

    #[test]
    fn test_unused_variable_detection() {
        let source = "def foo():\n    x = 1\n    y = 2\n    return y";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_unused_variable_detection"), None);
        let analyzer = fixture.analyzer(None);
        let unused = analyzer.find_unused_variables();

        assert!(unused.iter().any(|u| u.var_name == "x"));
        assert!(!unused.iter().any(|u| u.var_name == "y"));
    }

    #[test]
    fn test_use_before_def_in_conditional() {
        let source = "def foo(cond):\n    if cond:\n        x = 1\n    print(x)";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_use_before_def_in_conditional"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();
        assert!(!result.is_empty());
        assert!(result.iter().any(|e| e.var_name == "x"));
    }

    #[test]
    fn test_no_use_before_def_when_all_paths_assign() {
        let source = "def foo(cond):\n    if cond:\n        x = 1\n    else:\n        x = 2\n    print(x)";
        let fixture = FunctionFixture::new(
            source,
            data_flow_fixture!("test_no_use_before_def_when_all_paths_assign"),
            None,
        );
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();
        assert!(!result.iter().any(|e| e.var_name == "x" && e.line == 6));
    }

    #[test]
    fn test_use_before_def_in_loop() {
        let source = "def foo():\n    for i in items:\n        result = total\n        total = i";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_use_before_def_in_loop"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();
        assert!(!result.is_empty(), "Expected use-before-def errors");
    }

    #[test]
    fn test_unreachable_code_after_return() {
        let source = "def foo():\n    return 1\n    x = 2";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_unreachable_code_after_return"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_unreachable_code();
        assert!(!result.is_empty());
    }

    #[test]
    fn test_data_flow_analyze_combined() {
        let source = "def foo():\n    y = x\n    z = 1";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_data_flow_analyze_combined"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.analyze();

        assert!(!result.use_before_def.is_empty());
        assert!(result.use_before_def.iter().any(|e| e.var_name == "x"));
        assert!(result.unused_variables.iter().any(|u| u.var_name == "z"));
    }

    #[test]
    fn test_parameters_not_use_before_def() {
        let source = "def foo(param):\n    x = param";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_parameters_not_use_before_def"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(!result.iter().any(|e| e.var_name == "param"));
    }

    #[test]
    fn test_for_loop_target_not_use_before_def() {
        let source = "def foo():
    for i in range(10):
        print(i)";
        let fixture = FunctionFixture::new(
            source,
            data_flow_fixture!("test_for_loop_target_not_use_before_def"),
            None,
        );
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();
        assert!(!result.iter().any(|e| e.var_name == "i"));
    }

    #[test]
    fn test_binary_op_uses_tracked() {
        let source = "def foo():\n    z = x + y";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_binary_op_uses_tracked"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(result.iter().any(|e| e.var_name == "x"));
        assert!(result.iter().any(|e| e.var_name == "y"));
    }

    #[test]
    fn test_builtins_not_flagged_without_local_assignment() {
        let source = "def controls(n):\n    try:\n        for i in range(n):\n            pass\n    except Exception as e:\n        print(e)";
        let ast = parse_source_to_ast(source);
        let fixture = FunctionFixture::new(source, ast, None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(
            !result.iter().any(|e| e.var_name == "range"),
            "builtin 'range' incorrectly flagged: {result:?}"
        );
        assert!(
            !result.iter().any(|e| e.var_name == "print"),
            "builtin 'print' incorrectly flagged: {result:?}"
        );
    }

    #[test]
    fn test_builtin_shadowing_reports_use_before_def() {
        let source = "def foo():\n    print('hi')\n    print = lambda *args, **kwargs: None\n    print('bye')";
        let ast = parse_source_to_ast(source);
        let fixture = FunctionFixture::new(source, ast, None);
        let scope = fixture
            .resolver
            .symbol_table
            .get_scope(fixture.scope_id)
            .expect("function scope missing");
        assert!(
            scope.symbols.contains_key("print"),
            "expected assignment to register symbol, found symbols: {:?}",
            scope.symbols.keys().collect::<Vec<_>>()
        );
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(
            result.iter().any(|e| e.var_name == "print" && e.line == 2),
            "expected builtin shadowing to be reported: {result:?}"
        );
    }

    #[test]
    fn test_try_preserves_prior_assignments() {
        let source = "def foo():\n    results = []\n    try:\n        pass\n    except Exception:\n        pass\n    return results";
        let ast = parse_source_to_ast(source);
        let fixture = FunctionFixture::new(source, ast, None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(
            !result.iter().any(|e| e.var_name == "results"),
            "assignment before try should be visible at return: {result:?}"
        );
    }

    #[test]
    fn test_controls_sample_not_flagged() {
        let source = r#"def controls(n: int) -> list[int]:
    results = []
    try:
        for i in range(n):
            if i % 2 == 0:
                results.append(i)
            else:
                results.append(i * 2)
        count = 0
        while count < 3:
            results.append(count + n)
            count += 1
    except Exception as e:
        print(f"An error occurred: {e}")
    return results"#;
        let ast = parse_source_to_ast(source);
        let fixture = FunctionFixture::new(source, ast, None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(
            !result.iter().any(|e| e.var_name == "results"),
            "controls() sample should not produce use-before-def: {result:?}"
        );
    }

    #[test]
    fn test_sequential_assignments_no_error() {
        let source = "def foo():\n    x = 1\n    y = x";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_sequential_assignments_no_error"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(!result.iter().any(|e| e.var_name == "x" && e.line == 3));
    }

    #[test]
    fn test_augmented_assignment_not_use_before_def() {
        let source = "def foo():\n    count = 0\n    count += 1\n    return count";
        let ast = parse_source_to_ast(source);
        let fixture = FunctionFixture::new(source, ast, None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(
            !result.iter().any(|e| e.var_name == "count"),
            "Augmented assignments should not trigger use-before-def: {result:?}"
        );
    }

    #[test]
    fn test_constant_propagation_boolean() {
        let source = "def foo():\n    DEBUG = False\n    x = 1";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_constant_propagation_boolean"), None);
        let analyzer = fixture.analyzer(None);
        let debug_identifier =
            AstNode::Identifier { name: "DEBUG".to_string(), line: 2, col: 13, end_line: 2, end_col: 18 };

        let mut test_constants = FxHashMap::default();
        test_constants.insert("DEBUG".to_string(), ConstantValue::Bool(false));

        let result = analyzer.evaluate_condition(&debug_identifier, &test_constants);
        assert_eq!(result, ConditionResult::AlwaysFalse);
    }

    #[test]
    fn test_constant_propagation_integer_arithmetic() {
        let source = "def foo():\n    x = 5\n    y = x + 3";
        let fixture = FunctionFixture::new(
            source,
            data_flow_fixture!("test_constant_propagation_integer_arithmetic"),
            None,
        );
        let analyzer = fixture.analyzer(None);

        let mut test_constants = FxHashMap::default();
        test_constants.insert("x".to_string(), ConstantValue::Integer(5));

        let expr = AstNode::BinaryOp {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 3, col: 9, end_line: 3, end_col: 10 }),
            op: beacon_parser::BinaryOperator::Add,
            right: Box::new(AstNode::Literal {
                value: beacon_parser::LiteralValue::Integer(3),
                line: 3,
                col: 13,
                end_line: 3,
                end_col: 14,
            }),
            line: 3,
            col: 11,
            end_line: 3,
            end_col: 11,
        };

        let result = analyzer.evaluate_to_constant(&expr, &test_constants);
        assert_eq!(result, ConstantValue::Integer(8));
    }

    #[test]
    fn test_evaluate_condition_with_comparison() {
        let source = "def foo():\n    x = 10";
        let fixture = FunctionFixture::new(
            source,
            data_flow_fixture!("test_evaluate_condition_with_comparison"),
            None,
        );
        let analyzer = fixture.analyzer(None);

        let mut test_constants = FxHashMap::default();
        test_constants.insert("x".to_string(), ConstantValue::Integer(10));

        let expr = AstNode::Compare {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 2, col: 9, end_line: 2, end_col: 10 }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: beacon_parser::LiteralValue::Integer(10),
                line: 2,
                col: 15,
                end_line: 2,
                end_col: 17,
            }],
            line: 2,
            col: 11,
            end_line: 2,
            end_col: 11,
        };

        let result = analyzer.evaluate_condition(&expr, &test_constants);
        assert_eq!(result, ConditionResult::AlwaysTrue);
    }

    #[test]
    fn test_constant_propagation_string_concat() {
        let source = "def foo():\n    s = 'hello'";
        let fixture = FunctionFixture::new(
            source,
            data_flow_fixture!("test_constant_propagation_string_concat"),
            None,
        );
        let analyzer = fixture.analyzer(None);

        let mut test_constants = FxHashMap::default();
        test_constants.insert("s".to_string(), ConstantValue::String("hello".to_string()));

        let expr = AstNode::BinaryOp {
            left: Box::new(AstNode::Identifier { name: "s".to_string(), line: 2, col: 9, end_line: 2, end_col: 10 }),
            op: beacon_parser::BinaryOperator::Add,
            right: Box::new(AstNode::Literal {
                value: beacon_parser::LiteralValue::String { value: " world".to_string(), prefix: String::new() },
                line: 2,
                col: 13,
                end_line: 2,
                end_col: 21,
            }),
            line: 2,
            col: 11,
            end_line: 2,
            end_col: 11,
        };

        let result = analyzer.evaluate_to_constant(&expr, &test_constants);
        assert_eq!(result, ConstantValue::String("hello world".to_string()));
    }

    #[test]
    fn test_function_def_hoisting() {
        let source =
            "def outer():\n    result = inner()\n    def inner():\n        return 42\n    return result".to_string();
        let mut resolver = NameResolver::new(source.clone());

        let ast = AstNode::FunctionDef {
            name: "outer".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "result".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 7,
                    }),
                    value: Box::new(AstNode::Call {
                        function: Box::new(AstNode::Identifier {
                            name: "inner".to_string(),
                            line: 1,
                            col: 1,
                            end_line: 1,
                            end_col: 6,
                        }),
                        args: vec![],
                        line: 2,
                        end_line: 2,
                        col: 14,
                        end_col: 14,
                        keywords: Vec::new(),
                    }),
                    line: 2,
                    end_line: 2,
                    col: 5,
                    end_col: 5,
                },
                AstNode::FunctionDef {
                    name: "inner".to_string(),
                    args: vec![],
                    body: vec![AstNode::Return {
                        value: Some(Box::new(AstNode::Literal {
                            value: beacon_parser::LiteralValue::Integer(42),
                            line: 4,
                            end_line: 4,
                            col: 16,
                            end_col: 16,
                        })),
                        line: 4,
                        end_line: 4,
                        col: 9,
                        end_col: 9,
                    }],
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    line: 3,
                    end_line: 3,
                    col: 5,
                    end_col: 5,
                    is_async: false,
                },
                AstNode::Return {
                    value: Some(Box::new(AstNode::Identifier {
                        name: "result".to_string(),
                        line: 5,
                        end_line: 5,
                        col: 12,
                        end_col: 12,
                    })),
                    line: 5,
                    end_line: 5,
                    col: 5,
                    end_col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 1,
            end_col: 1,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, line, col, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let byte_offset = line_col_to_byte_offset(&source, *line, *col);
            let scope_id = resolver.symbol_table.find_scope_at_position(byte_offset);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id, None);
            let result = analyzer.find_use_before_def();
            let inner_errors: Vec<_> = result.iter().filter(|e| e.var_name == "inner").collect();
            assert!(
                inner_errors.is_empty(),
                "Function 'inner' should not be flagged as use-before-def due to hoisting, found: {inner_errors:?}"
            );
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_class_def_hoisting() {
        let source =
            "def factory():\n    obj = MyClass()\n    class MyClass:\n        pass\n    return obj".to_string();
        let mut resolver = NameResolver::new(source.clone());

        let ast = AstNode::FunctionDef {
            name: "factory".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "obj".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 4,
                    }),
                    value: Box::new(AstNode::Call {
                        function: Box::new(AstNode::Identifier {
                            name: "MyClass".to_string(),
                            line: 1,
                            col: 1,
                            end_line: 1,
                            end_col: 8,
                        }),
                        args: vec![],
                        line: 2,
                        end_line: 2,
                        col: 11,
                        end_col: 11,
                        keywords: Vec::new(),
                    }),
                    line: 2,
                    end_line: 2,
                    col: 5,
                    end_col: 5,
                },
                AstNode::ClassDef {
                    name: "MyClass".to_string(),
                    bases: vec![],
                    metaclass: None,
                    body: vec![AstNode::Pass { line: 4, end_line: 4, col: 9, end_col: 9 }],
                    docstring: None,
                    decorators: Vec::new(),
                    line: 3,
                    end_line: 3,
                    col: 5,
                    end_col: 5,
                },
                AstNode::Return {
                    value: Some(Box::new(AstNode::Identifier {
                        name: "obj".to_string(),
                        line: 5,
                        end_line: 5,
                        col: 12,
                        end_col: 12,
                    })),
                    line: 5,
                    end_line: 5,
                    col: 5,
                    end_col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 1,
            end_col: 1,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, line, col, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let byte_offset = line_col_to_byte_offset(&source, *line, *col);
            let scope_id = resolver.symbol_table.find_scope_at_position(byte_offset);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id, None);
            let result = analyzer.find_use_before_def();
            let class_errors: Vec<_> = result.iter().filter(|e| e.var_name == "MyClass").collect();
            assert!(
                class_errors.is_empty(),
                "Class 'MyClass' should not be flagged as use-before-def due to hoisting, found: {class_errors:?}"
            );
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_function_in_conditional_hoisting() {
        let source =
            "def outer():\n    result = helper()\n    if True:\n        def helper():\n            return 1\n    return result"
                .to_string();
        let mut resolver = NameResolver::new(source.clone());

        let ast = AstNode::FunctionDef {
            name: "outer".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "result".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 7,
                    }),
                    value: Box::new(AstNode::Call {
                        function: Box::new(AstNode::Identifier {
                            name: "helper".to_string(),
                            line: 1,
                            col: 1,
                            end_line: 1,
                            end_col: 7,
                        }),
                        args: vec![],
                        line: 2,
                        end_line: 2,
                        col: 14,
                        end_col: 14,
                        keywords: Vec::new(),
                    }),
                    line: 2,
                    end_line: 2,
                    col: 5,
                    end_col: 5,
                },
                AstNode::If {
                    test: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Boolean(true),
                        line: 3,
                        end_line: 3,
                        col: 8,
                        end_col: 8,
                    }),
                    body: vec![AstNode::FunctionDef {
                        name: "helper".to_string(),
                        args: vec![],
                        body: vec![AstNode::Return {
                            value: Some(Box::new(AstNode::Literal {
                                value: beacon_parser::LiteralValue::Integer(1),
                                line: 5,
                                end_line: 5,
                                col: 20,
                                end_col: 20,
                            })),
                            line: 5,
                            end_line: 5,
                            col: 13,
                            end_col: 13,
                        }],
                        docstring: None,
                        return_type: None,
                        decorators: Vec::new(),
                        line: 4,
                        end_line: 4,
                        col: 9,
                        end_col: 9,
                        is_async: false,
                    }],
                    elif_parts: vec![],
                    else_body: None,
                    line: 3,
                    end_line: 3,
                    col: 5,
                    end_col: 5,
                },
                AstNode::Return {
                    value: Some(Box::new(AstNode::Identifier {
                        name: "result".to_string(),
                        line: 6,
                        end_line: 6,
                        col: 12,
                        end_col: 12,
                    })),
                    line: 6,
                    end_line: 6,
                    col: 5,
                    end_col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
            end_line: 1,
            end_col: 1,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, line, col, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let byte_offset = line_col_to_byte_offset(&source, *line, *col);
            let scope_id = resolver.symbol_table.find_scope_at_position(byte_offset);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id, None);
            let result = analyzer.find_use_before_def();
            let helper_errors: Vec<_> = result.iter().filter(|e| e.var_name == "helper").collect();
            assert!(
                helper_errors.is_empty(),
                "Function 'helper' in conditional should not be flagged as use-before-def due to hoisting, found: {helper_errors:?}"
            );
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_evaluate_boolean_operators() {
        let source = "def foo():\n    pass";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_evaluate_boolean_operators"), None);
        let analyzer = fixture.analyzer(None);

        let mut test_constants = FxHashMap::default();
        test_constants.insert("a".to_string(), ConstantValue::Bool(true));
        test_constants.insert("b".to_string(), ConstantValue::Bool(false));

        let and_expr = AstNode::BinaryOp {
            left: Box::new(AstNode::Identifier { name: "a".to_string(), line: 2, end_line: 2, col: 5, end_col: 5 }),
            op: beacon_parser::BinaryOperator::And,
            right: Box::new(AstNode::Identifier { name: "b".to_string(), line: 2, col: 11, end_line: 2, end_col: 11 }),
            line: 2,
            col: 9,
            end_line: 2,
            end_col: 9,
        };
        assert_eq!(
            analyzer.evaluate_condition(&and_expr, &test_constants),
            ConditionResult::AlwaysFalse
        );

        let or_expr = AstNode::BinaryOp {
            left: Box::new(AstNode::Identifier { name: "a".to_string(), line: 2, col: 5, end_line: 2, end_col: 5 }),
            op: beacon_parser::BinaryOperator::Or,
            right: Box::new(AstNode::Identifier { name: "b".to_string(), line: 2, col: 11, end_line: 2, end_col: 11 }),
            line: 2,
            col: 9,
            end_line: 2,
            end_col: 9,
        };
        assert_eq!(
            analyzer.evaluate_condition(&or_expr, &test_constants),
            ConditionResult::AlwaysTrue
        );

        let not_expr = AstNode::UnaryOp {
            op: beacon_parser::UnaryOperator::Not,
            operand: Box::new(AstNode::Identifier { name: "b".to_string(), line: 2, col: 9, end_line: 2, end_col: 9 }),
            line: 2,
            col: 5,
            end_line: 2,
            end_col: 5,
        };
        assert_eq!(
            analyzer.evaluate_condition(&not_expr, &test_constants),
            ConditionResult::AlwaysTrue
        );
    }

    #[test]
    fn test_import_hoisting() {
        let source = "def foo():\n    from bar import baz\n    result = baz()";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_import_hoisting"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(
            !result.iter().any(|e| e.var_name == "baz"),
            "Imported name 'baz' should not be flagged as use-before-def due to import hoisting"
        );
    }

    #[test]
    fn test_simple_import_hoisting() {
        let source = "def foo():\n    import os\n    path = os.path";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_simple_import_hoisting"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(
            !result.iter().any(|e| e.var_name == "os"),
            "Imported module 'os' should not be flagged as use-before-def due to import hoisting"
        );
    }

    #[test]
    fn test_import_with_alias_hoisting() {
        let source = "def foo():\n    import numpy as np\n    arr = np.array([1, 2, 3])";
        let fixture = FunctionFixture::new(source, data_flow_fixture!("test_import_with_alias_hoisting"), None);
        let analyzer = fixture.analyzer(None);
        let result = analyzer.find_use_before_def();

        assert!(
            !result.iter().any(|e| e.var_name == "np"),
            "Alias 'np' should not be flagged as use-before-def due to import hoisting"
        );
    }

    #[test]
    fn test_module_level_class_available_in_function() {
        let source = "class MyClass:\n    pass\n\ndef factory():\n    return MyClass()";
        let fixture = FunctionFixture::new(
            source,
            data_flow_fixture!("test_module_level_class_available_in_function"),
            Some("factory"),
        );
        let module_body = fixture.module_body().expect("module fixture expected");
        let module_hoisted = DataFlowAnalyzer::collect_hoisted_definitions(module_body);
        let analyzer = fixture.analyzer(Some(&module_hoisted));
        let result = analyzer.find_use_before_def();

        assert!(
            !result.iter().any(|e| e.var_name == "MyClass"),
            "Module-level class 'MyClass' should not be flagged as use-before-def"
        );
    }

    #[test]
    fn test_module_level_function_available_in_another_function() {
        let source = "def helper():\n    pass\n\ndef main():\n    helper()";
        let fixture = FunctionFixture::new(
            source,
            data_flow_fixture!("test_module_level_function_available_in_another_function"),
            Some("main"),
        );
        let module_body = fixture.module_body().expect("module fixture expected");
        let module_hoisted = DataFlowAnalyzer::collect_hoisted_definitions(module_body);
        let analyzer = fixture.analyzer(Some(&module_hoisted));
        let result = analyzer.find_use_before_def();

        assert!(
            !result.iter().any(|e| e.var_name == "helper"),
            "Module-level function 'helper' should not be flagged as use-before-def"
        );
    }
}
