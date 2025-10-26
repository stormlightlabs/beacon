//! Data flow analysis for Python code
//!
//! Implements various data flow analyses on top of the CFG:
//! - Definite assignment (use-before-def detection)
//! - Unreachable code detection
//! - Unused variable detection

use super::cfg::{BlockId, ControlFlowGraph};
use beacon_parser::{AstNode, SymbolTable};
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

/// Performs data flow analysis on a CFG
pub struct DataFlowAnalyzer<'a> {
    cfg: &'a ControlFlowGraph,
    function_body: &'a [AstNode],
    symbol_table: &'a SymbolTable,
}

impl<'a> DataFlowAnalyzer<'a> {
    pub fn new(cfg: &'a ControlFlowGraph, function_body: &'a [AstNode], symbol_table: &'a SymbolTable) -> Self {
        Self { cfg, function_body, symbol_table }
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
        let mut errors = Vec::new();
        let mut def_in: FxHashMap<BlockId, FxHashSet<String>> = FxHashMap::default();
        let mut def_out: FxHashMap<BlockId, FxHashSet<String>> = FxHashMap::default();

        for block_id in self.cfg.blocks.keys() {
            def_in.insert(*block_id, FxHashSet::default());
            def_out.insert(*block_id, FxHashSet::default());
        }

        let mut worklist: Vec<BlockId> = self.cfg.blocks.keys().copied().collect();
        let mut changed = true;

        while changed && !worklist.is_empty() {
            changed = false;
            let block_id = worklist.pop().unwrap();

            let block = self.cfg.blocks.get(&block_id).unwrap();
            let mut new_def_in = if block.predecessors.is_empty() {
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

            for &stmt_idx in &block.statements {
                if stmt_idx < self.function_body.len() {
                    let stmt = &self.function_body[stmt_idx];
                    let (uses, defs) = self.get_uses_and_defs(stmt);

                    for (var_name, line, col) in uses {
                        if !new_def_in.contains(&var_name) && !self.is_parameter(&var_name) {
                            errors.push(UseBeforeDef { var_name, line, col });
                        }
                    }

                    for def_var in defs {
                        new_def_in.insert(def_var);
                    }
                }
            }

            let old_def_in = def_in.get(&block_id).cloned().unwrap_or_default();
            let old_def_out = def_out.get(&block_id).cloned().unwrap_or_default();
            if new_def_in != old_def_in || new_def_in != old_def_out {
                def_in.insert(block_id, new_def_in.clone());
                def_out.insert(block_id, new_def_in);
                changed = true;

                for &(succ_id, _) in &block.successors {
                    if !worklist.contains(&succ_id) {
                        worklist.push(succ_id);
                    }
                }
            }
        }

        errors
    }

    /// Find unreachable code blocks
    fn find_unreachable_code(&self) -> Vec<UnreachableCode> {
        let unreachable_blocks = self.cfg.unreachable_blocks();
        let mut unreachable = Vec::new();

        for block_id in unreachable_blocks {
            if let Some(block) = self.cfg.blocks.get(&block_id) {
                if let Some(&stmt_idx) = block.statements.first() {
                    if stmt_idx < self.function_body.len() {
                        let (line, col) = self.get_stmt_location(&self.function_body[stmt_idx]);
                        unreachable.push(UnreachableCode { block_id, line, col });
                    }
                }
            }
        }

        unreachable
    }

    /// Find unused variables (already implemented in symbol table)
    ///
    /// This delegates to the symbol table's unused variable detection,
    /// which tracks references during name resolution.
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

    /// Check if a variable is a function parameter in the current scope
    fn is_parameter(&self, name: &str) -> bool {
        // TODO: need scope context
        self.symbol_table
            .scopes
            .values()
            .flat_map(|scope| scope.symbols.values())
            .any(|sym| sym.name == name && sym.kind == beacon_parser::SymbolKind::Parameter)
    }

    /// Get the location of a statement
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
}

// TODO: Implement constant propagation for detecting always-true/always-false conditionals
// This would enable more precise unreachable code detection.
//
// Algorithm:
// 1. Track known constant values for variables
// 2. Evaluate conditions symbolically when possible
// 3. Identify branches that can never be taken
// 4. Mark those blocks as unreachable
//
// Example:
// ```python
// DEBUG = False
// if DEBUG:
//     print("debug mode")  # unreachable
// ```

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::cfg::CfgBuilder;
    use beacon_parser::NameResolver;

    // TODO: CFG builder needs to populate statement indices for DFA to work
    #[test]
    #[ignore]
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
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
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
            col: 1,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
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
            col: 1,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
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
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
            let result = analyzer.find_use_before_def();
            let x_errors: Vec<_> = result.iter().filter(|e| e.var_name == "x" && e.line == 6).collect();
            assert!(x_errors.is_empty());
        } else {
            panic!("Expected FunctionDef");
        }
    }

    // TODO: CFG builder needs to populate statement indices for DFA to work
    #[test]
    #[ignore]
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
                line: 2,
                col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
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
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
            let result = analyzer.find_unreachable_code();
            assert!(!result.is_empty());
        } else {
            panic!("Expected FunctionDef");
        }
    }

    // TODO: CFG builder needs to populate statement indices for DFA to work
    #[test]
    #[ignore]
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
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
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
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
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
                line: 2,
                col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            line: 1,
            col: 1,
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);
            let cfg = builder.build();

            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
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

    // TODO: CFG builder needs to populate statement indices for DFA to work
    #[test]
    #[ignore]
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
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
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
        };

        resolver.resolve(&ast).unwrap();

        if let AstNode::FunctionDef { body, .. } = &ast {
            let mut builder = CfgBuilder::new();
            builder.build_function(body);

            let cfg = builder.build();
            let analyzer = DataFlowAnalyzer::new(&cfg, body, &resolver.symbol_table);
            let result = analyzer.find_use_before_def();
            let x_errors: Vec<_> = result.iter().filter(|e| e.var_name == "x" && e.line == 3).collect();
            assert!(x_errors.is_empty());
        } else {
            panic!("Expected FunctionDef");
        }
    }
}
