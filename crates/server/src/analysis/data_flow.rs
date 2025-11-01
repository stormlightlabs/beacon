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

/// Performs data flow analysis on a CFG
/// TODO: Store condition expressions in CFG blocks to automatically detect unreachable branches based on constant conditions.
pub struct DataFlowAnalyzer<'a> {
    cfg: &'a ControlFlowGraph,
    /// Flattened list of ALL statements (including nested ones) for indexing
    all_statements: Vec<&'a AstNode>,
    symbol_table: &'a SymbolTable,
    /// Scope ID of the function being analyzed (for parameter lookup)
    scope_id: ScopeId,
}

impl<'a> DataFlowAnalyzer<'a> {
    pub fn new(
        cfg: &'a ControlFlowGraph, function_body: &'a [AstNode], symbol_table: &'a SymbolTable, scope_id: ScopeId,
    ) -> Self {
        let mut all_statements = Vec::new();
        for stmt in function_body {
            Self::collect_all_statements(stmt, &mut all_statements);
        }
        Self { cfg, all_statements, symbol_table, scope_id }
    }

    /// Recursively collect all statements including nested ones
    fn collect_all_statements(node: &'a AstNode, statements: &mut Vec<&'a AstNode>) {
        match node {
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    statements.push(stmt);
                }

                for (_, elif_body) in elif_parts {
                    for stmt in elif_body {
                        statements.push(stmt);
                    }
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        statements.push(stmt);
                    }
                }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                statements.push(node);
                for stmt in body {
                    statements.push(stmt);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        statements.push(stmt);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    statements.push(stmt);
                }

                for handler in handlers {
                    for stmt in &handler.body {
                        statements.push(stmt);
                    }
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        statements.push(stmt);
                    }
                }

                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        statements.push(stmt);
                    }
                }
            }
            AstNode::With { body, .. } => {
                for stmt in body {
                    statements.push(stmt);
                }
            }
            AstNode::Match { cases, .. } => {
                for case in cases {
                    for stmt in &case.body {
                        statements.push(stmt);
                    }
                }
            }
            AstNode::FunctionDef { .. } | AstNode::ClassDef { .. } => {}
            _ => {
                statements.push(node);
            }
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

        for block_id in self.cfg.blocks.keys() {
            def_in.insert(*block_id, FxHashSet::default());
            def_out.insert(*block_id, FxHashSet::default());
        }

        let mut worklist: Vec<BlockId> = self.cfg.blocks.keys().copied().collect();

        while let Some(block_id) = worklist.pop() {
            let block = self.cfg.blocks.get(&block_id).unwrap();

            let new_def_in = if block.predecessors.is_empty() {
                FxHashSet::default()
            } else {
                let first_pred = block.predecessors[0].0;
                let mut result = def_out.get(&first_pred).cloned().unwrap_or_default();

                for &(pred_id, _) in &block.predecessors[1..] {
                    if let Some(pred_def_out) = def_out.get(&pred_id) {
                        result.retain(|var| pred_def_out.contains(var));
                    }
                }
                result
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
            if new_def_in != old_def_in || new_def_out != old_def_out {
                def_in.insert(block_id, new_def_in);
                def_out.insert(block_id, new_def_out);

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
                defs.push(target.clone());
            }
            AstNode::AnnotatedAssignment { target, value, .. } => {
                if let Some(val) = value {
                    Self::collect_uses(val, &mut uses);
                }
                defs.push(target.clone());
            }
            AstNode::For { target, iter, .. } => {
                Self::collect_uses(iter, &mut uses);
                defs.push(target.clone());
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
            AstNode::Identifier { name, line, col } => {
                uses.push((name.clone(), *line, *col));
            }
            AstNode::Call { function, args, line, col } => {
                uses.push((function.clone(), *line, *col));
                for arg in args {
                    Self::collect_uses(arg, uses);
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
            | AstNode::Pass { line, col }
            | AstNode::Break { line, col }
            | AstNode::Continue { line, col }
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
                constants.insert(target.clone(), const_value)
            }
            AstNode::AnnotatedAssignment { target, value: Some(val), .. } => {
                let const_value = self.evaluate_to_constant(val, constants);
                constants.insert(target.clone(), const_value)
            }
            AstNode::For { target, .. } => constants.insert(target.clone(), ConstantValue::Unknown),
            AstNode::NamedExpr { target, value, .. } => {
                let const_value = self.evaluate_to_constant(value, constants);
                constants.insert(target.clone(), const_value)
            }
            _ => None,
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
    use crate::analysis::cfg::CfgBuilder;
    use beacon_parser::{NameResolver, ScopeId, ScopeKind, SymbolTable};

    /// Helper to find the first function scope in the symbol table (for tests)
    fn find_function_scope(symbol_table: &SymbolTable) -> ScopeId {
        symbol_table
            .scopes
            .values()
            .find(|scope| scope.kind == ScopeKind::Function)
            .map(|scope| scope.id)
            .expect("No function scope found in symbol table")
    }

    #[test]
    fn test_use_before_def_detection() {
        let source = "def foo():\n    y = x\n    x = 1".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: "y".to_string(),
                    value: Box::new(AstNode::Identifier { name: "x".to_string(), line: 2, col: 9 }),
                    line: 2,
                    col: 5,
                },
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(1),
                        line: 3,
                        col: 9,
                    }),
                    line: 3,
                    col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.find_use_before_def();

            assert!(!result.is_empty(), "Expected use-before-def for x");
            assert!(
                result.iter().any(|e| e.var_name == "x"),
                "Expected x to be flagged as use-before-def"
            );
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_unused_variable_detection() {
        let source = "def foo():\n    x = 1\n    y = 2\n    return y".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(1),
                        line: 2,
                        col: 9,
                    }),
                    line: 2,
                    col: 5,
                },
                AstNode::Assignment {
                    target: "y".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(2),
                        line: 3,
                        col: 9,
                    }),
                    line: 3,
                    col: 5,
                },
                AstNode::Return {
                    value: Some(Box::new(AstNode::Identifier {
                        name: "y".to_string(),
                        line: 4,
                        col: 12,
                    })),
                    line: 4,
                    col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            is_async: false,
            col: 1,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let unused = analyzer.find_unused_variables();

            assert!(unused.iter().any(|u| u.var_name == "x"));
            assert!(!unused.iter().any(|u| u.var_name == "y"));
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_use_before_def_in_conditional() {
        let source = "def foo(cond):\n    if cond:\n        x = 1\n    print(x)".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![beacon_parser::Parameter {
                name: "cond".to_string(),
                line: 1,
                col: 9,
                type_annotation: None,
                default_value: None,
            }],
            body: vec![
                AstNode::If {
                    test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 2, col: 8 }),
                    body: vec![AstNode::Assignment {
                        target: "x".to_string(),
                        value: Box::new(AstNode::Literal {
                            value: beacon_parser::LiteralValue::Integer(1),
                            line: 3,
                            col: 13,
                        }),
                        line: 3,
                        col: 9,
                    }],
                    elif_parts: vec![],
                    else_body: None,
                    line: 2,
                    col: 5,
                },
                AstNode::Call {
                    function: "print".to_string(),
                    args: vec![AstNode::Identifier { name: "x".to_string(), line: 4, col: 11 }],
                    line: 4,
                    col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            is_async: false,
            col: 1,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.find_use_before_def();
            assert!(!result.is_empty());
            assert!(result.iter().any(|e| e.var_name == "x"));
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_no_use_before_def_when_all_paths_assign() {
        let source = "def foo(cond):\n    if cond:\n        x = 1\n    else:\n        x = 2\n    print(x)".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![beacon_parser::Parameter {
                name: "cond".to_string(),
                line: 1,
                col: 9,
                type_annotation: None,
                default_value: None,
            }],
            body: vec![
                AstNode::If {
                    test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 2, col: 8 }),
                    body: vec![AstNode::Assignment {
                        target: "x".to_string(),
                        value: Box::new(AstNode::Literal {
                            value: beacon_parser::LiteralValue::Integer(1),
                            line: 3,
                            col: 13,
                        }),
                        line: 3,
                        col: 9,
                    }],
                    elif_parts: vec![],
                    else_body: Some(vec![AstNode::Assignment {
                        target: "x".to_string(),
                        value: Box::new(AstNode::Literal {
                            value: beacon_parser::LiteralValue::Integer(2),
                            line: 5,
                            col: 13,
                        }),
                        line: 5,
                        col: 9,
                    }]),
                    line: 2,
                    col: 5,
                },
                AstNode::Call {
                    function: "print".to_string(),
                    args: vec![AstNode::Identifier { name: "x".to_string(), line: 6, col: 11 }],
                    line: 6,
                    col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.find_use_before_def();
            let x_errors: Vec<_> = result.iter().filter(|e| e.var_name == "x" && e.line == 6).collect();
            assert!(x_errors.is_empty());
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_use_before_def_in_loop() {
        let source = "def foo():\n    for i in items:\n        result = total\n        total = i".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::For {
                target: "i".to_string(),
                iter: Box::new(AstNode::Identifier { name: "items".to_string(), line: 2, col: 14 }),
                body: vec![
                    AstNode::Assignment {
                        target: "result".to_string(),
                        value: Box::new(AstNode::Identifier { name: "total".to_string(), line: 3, col: 18 }),
                        line: 3,
                        col: 9,
                    },
                    AstNode::Assignment {
                        target: "total".to_string(),
                        value: Box::new(AstNode::Identifier { name: "i".to_string(), line: 4, col: 17 }),
                        line: 4,
                        col: 9,
                    },
                ],
                else_body: None,
                is_async: false,
                line: 2,
                col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.find_use_before_def();
            assert!(!result.is_empty(), "Expected use-before-def errors");
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_unreachable_code_after_return() {
        let source = "def foo():\n    return 1\n    x = 2".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Return {
                    value: Some(Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(1),
                        line: 2,
                        col: 12,
                    })),
                    line: 2,
                    col: 5,
                },
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(2),
                        line: 3,
                        col: 9,
                    }),
                    line: 3,
                    col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.find_unreachable_code();
            assert!(!result.is_empty());
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_data_flow_analyze_combined() {
        let source = "def foo():\n    y = x\n    z = 1".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: "y".to_string(),
                    value: Box::new(AstNode::Identifier { name: "x".to_string(), line: 2, col: 9 }),
                    line: 2,
                    col: 5,
                },
                AstNode::Assignment {
                    target: "z".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(1),
                        line: 3,
                        col: 9,
                    }),
                    line: 3,
                    col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.analyze();

            assert!(!result.use_before_def.is_empty());
            assert!(result.use_before_def.iter().any(|e| e.var_name == "x"));
            assert!(result.unused_variables.iter().any(|u| u.var_name == "z"));
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_parameters_not_use_before_def() {
        let source = "def foo(param):\n    x = param".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![beacon_parser::Parameter {
                name: "param".to_string(),
                line: 1,
                col: 9,
                type_annotation: None,
                default_value: None,
            }],
            body: vec![AstNode::Assignment {
                target: "x".to_string(),
                value: Box::new(AstNode::Identifier { name: "param".to_string(), line: 2, col: 9 }),
                line: 2,
                col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.find_use_before_def();

            assert!(!result.iter().any(|e| e.var_name == "param"));
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_for_loop_target_not_use_before_def() {
        let source = "def foo():\n    for i in range(10):\n        print(i)".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::For {
                target: "i".to_string(),
                iter: Box::new(AstNode::Call {
                    function: "range".to_string(),
                    args: vec![AstNode::Literal { value: beacon_parser::LiteralValue::Integer(10), line: 2, col: 20 }],
                    line: 2,
                    col: 14,
                }),
                body: vec![AstNode::Call {
                    function: "print".to_string(),
                    args: vec![AstNode::Identifier { name: "i".to_string(), line: 3, col: 15 }],
                    line: 3,
                    col: 9,
                }],
                else_body: None,
                is_async: false,
                line: 2,
                col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.find_use_before_def();

            // Loop target 'i' is defined by the for statement before the body executes
            // The For node creates a definition, so 'i' should not be flagged as use-before-def
            let i_errors: Vec<_> = result.iter().filter(|e| e.var_name == "i").collect();
            assert!(
                i_errors.is_empty(),
                "Loop variable 'i' should not be flagged as use-before-def, found: {i_errors:?}"
            );
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_binary_op_uses_tracked() {
        let source = "def foo():\n    z = x + y".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::Assignment {
                target: "z".to_string(),
                value: Box::new(AstNode::BinaryOp {
                    left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 2, col: 9 }),
                    op: beacon_parser::BinaryOperator::Add,
                    right: Box::new(AstNode::Identifier { name: "y".to_string(), line: 2, col: 13 }),
                    line: 2,
                    col: 11,
                }),
                line: 2,
                col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.find_use_before_def();

            assert!(result.iter().any(|e| e.var_name == "x"));
            assert!(result.iter().any(|e| e.var_name == "y"));
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_sequential_assignments_no_error() {
        let source = "def foo():\n    x = 1\n    y = x".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(1),
                        line: 2,
                        col: 9,
                    }),
                    line: 2,
                    col: 5,
                },
                AstNode::Assignment {
                    target: "y".to_string(),
                    value: Box::new(AstNode::Identifier { name: "x".to_string(), line: 3, col: 9 }),
                    line: 3,
                    col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let result = analyzer.find_use_before_def();
            let x_errors: Vec<_> = result.iter().filter(|e| e.var_name == "x" && e.line == 3).collect();
            assert!(x_errors.is_empty());
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_constant_propagation_boolean() {
        let source = "def foo():\n    DEBUG = False\n    x = 1".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: "DEBUG".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Boolean(false),
                        line: 2,
                        col: 13,
                    }),
                    line: 2,
                    col: 5,
                },
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(1),
                        line: 3,
                        col: 9,
                    }),
                    line: 3,
                    col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);
            let debug_identifier = AstNode::Identifier { name: "DEBUG".to_string(), line: 2, col: 13 };

            let mut test_constants = FxHashMap::default();
            test_constants.insert("DEBUG".to_string(), ConstantValue::Bool(false));

            let result = analyzer.evaluate_condition(&debug_identifier, &test_constants);
            assert_eq!(result, ConditionResult::AlwaysFalse);
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_constant_propagation_integer_arithmetic() {
        let source = "def foo():\n    x = 5\n    y = x + 3".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![
                AstNode::Assignment {
                    target: "x".to_string(),
                    value: Box::new(AstNode::Literal {
                        value: beacon_parser::LiteralValue::Integer(5),
                        line: 2,
                        col: 9,
                    }),
                    line: 2,
                    col: 5,
                },
                AstNode::Assignment {
                    target: "y".to_string(),
                    value: Box::new(AstNode::BinaryOp {
                        left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 3, col: 9 }),
                        op: beacon_parser::BinaryOperator::Add,
                        right: Box::new(AstNode::Literal {
                            value: beacon_parser::LiteralValue::Integer(3),
                            line: 3,
                            col: 13,
                        }),
                        line: 3,
                        col: 11,
                    }),
                    line: 3,
                    col: 5,
                },
            ],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);

            let mut test_constants = FxHashMap::default();
            test_constants.insert("x".to_string(), ConstantValue::Integer(5));

            let expr = AstNode::BinaryOp {
                left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 3, col: 9 }),
                op: beacon_parser::BinaryOperator::Add,
                right: Box::new(AstNode::Literal { value: beacon_parser::LiteralValue::Integer(3), line: 3, col: 13 }),
                line: 3,
                col: 11,
            };

            let result = analyzer.evaluate_to_constant(&expr, &test_constants);
            assert_eq!(result, ConstantValue::Integer(8));
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_evaluate_condition_with_comparison() {
        let source = "def foo():\n    x = 10".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::Assignment {
                target: "x".to_string(),
                value: Box::new(AstNode::Literal { value: beacon_parser::LiteralValue::Integer(10), line: 2, col: 9 }),
                line: 2,
                col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);

            let mut test_constants = FxHashMap::default();
            test_constants.insert("x".to_string(), ConstantValue::Integer(10));

            let condition = AstNode::Compare {
                left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 2, col: 9 }),
                ops: vec![beacon_parser::CompareOperator::Gt],
                comparators: vec![AstNode::Literal {
                    value: beacon_parser::LiteralValue::Integer(5),
                    line: 2,
                    col: 13,
                }],
                line: 2,
                col: 11,
            };

            let result = analyzer.evaluate_condition(&condition, &test_constants);
            assert_eq!(result, ConditionResult::AlwaysTrue);

            let condition2 = AstNode::Compare {
                left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 2, col: 9 }),
                ops: vec![beacon_parser::CompareOperator::Lt],
                comparators: vec![AstNode::Literal {
                    value: beacon_parser::LiteralValue::Integer(5),
                    line: 2,
                    col: 13,
                }],
                line: 2,
                col: 11,
            };

            let result2 = analyzer.evaluate_condition(&condition2, &test_constants);
            assert_eq!(result2, ConditionResult::AlwaysFalse);
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_constant_propagation_string_concat() {
        let source = "def foo():\n    s = 'hello'".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::Assignment {
                target: "s".to_string(),
                value: Box::new(AstNode::Literal {
                    value: beacon_parser::LiteralValue::String { value: "hello".to_string(), prefix: String::new() },
                    line: 2,
                    col: 9,
                }),
                line: 2,
                col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);

            let mut test_constants = FxHashMap::default();
            test_constants.insert("s".to_string(), ConstantValue::String("hello".to_string()));

            let expr = AstNode::BinaryOp {
                left: Box::new(AstNode::Identifier { name: "s".to_string(), line: 2, col: 9 }),
                op: beacon_parser::BinaryOperator::Add,
                right: Box::new(AstNode::Literal {
                    value: beacon_parser::LiteralValue::String { value: " world".to_string(), prefix: String::new() },
                    line: 2,
                    col: 13,
                }),
                line: 2,
                col: 11,
            };

            let result = analyzer.evaluate_to_constant(&expr, &test_constants);
            assert_eq!(result, ConstantValue::String("hello world".to_string()));
        } else {
            panic!("Expected FunctionDef");
        }
    }

    #[test]
    fn test_evaluate_boolean_operators() {
        let source = "def foo():\n    pass".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "foo".to_string(),
            args: vec![],
            body: vec![AstNode::Pass { line: 2, col: 5 }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
            is_async: false,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let scope_id = find_function_scope(&resolver.symbol_table);
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table, scope_id);

            let mut test_constants = FxHashMap::default();
            test_constants.insert("a".to_string(), ConstantValue::Bool(true));
            test_constants.insert("b".to_string(), ConstantValue::Bool(false));

            let and_expr = AstNode::BinaryOp {
                left: Box::new(AstNode::Identifier { name: "a".to_string(), line: 2, col: 5 }),
                op: beacon_parser::BinaryOperator::And,
                right: Box::new(AstNode::Identifier { name: "b".to_string(), line: 2, col: 11 }),
                line: 2,
                col: 9,
            };
            let and_result = analyzer.evaluate_condition(&and_expr, &test_constants);
            assert_eq!(and_result, ConditionResult::AlwaysFalse);

            let or_expr = AstNode::BinaryOp {
                left: Box::new(AstNode::Identifier { name: "a".to_string(), line: 2, col: 5 }),
                op: beacon_parser::BinaryOperator::Or,
                right: Box::new(AstNode::Identifier { name: "b".to_string(), line: 2, col: 10 }),
                line: 2,
                col: 8,
            };
            let or_result = analyzer.evaluate_condition(&or_expr, &test_constants);
            assert_eq!(or_result, ConditionResult::AlwaysTrue);

            let not_expr = AstNode::UnaryOp {
                op: beacon_parser::UnaryOperator::Not,
                operand: Box::new(AstNode::Identifier { name: "b".to_string(), line: 2, col: 9 }),
                line: 2,
                col: 5,
            };
            let not_result = analyzer.evaluate_condition(&not_expr, &test_constants);
            assert_eq!(not_result, ConditionResult::AlwaysTrue);
        } else {
            panic!("Expected FunctionDef");
        }
    }
}
