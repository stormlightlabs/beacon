//! Control Flow Graph (CFG) construction and analysis.

pub mod builder;
pub mod call_graph;
pub mod graph;
pub mod module;
pub mod module_builder;
pub mod resolver;

pub use builder::CfgBuilder;
pub use call_graph::{CallGraph, CallKind, CallSite, FunctionId, NodeTracking};
pub use graph::{BasicBlock, BlockId, ControlFlowGraph, EdgeKind};
pub use module::{CallEdgeSummary, FunctionSummary, ModuleCFG, WorkspaceCFG, WorkspaceCfgSummary};
pub use module_builder::ModuleCFGBuilder;
pub use resolver::CallResolver;

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::{AstNode, ExceptHandler, LiteralValue, MatchCase, Pattern, WithItem};

    #[test]
    fn test_cfg_creation() {
        let cfg = ControlFlowGraph::new();
        assert_eq!(cfg.blocks.len(), 2);
        assert!(cfg.blocks.contains_key(&cfg.entry));
        assert!(cfg.blocks.contains_key(&cfg.exit));
    }

    #[test]
    fn test_edge_kind_display_labels_are_stable() {
        assert_eq!(EdgeKind::Normal.to_string(), "normal");
        assert_eq!(EdgeKind::True.to_string(), "true");
        assert_eq!(EdgeKind::False.to_string(), "false");
        assert_eq!(EdgeKind::Exception.to_string(), "exception");
        assert_eq!(EdgeKind::Break.to_string(), "break");
        assert_eq!(EdgeKind::Continue.to_string(), "continue");
        assert_eq!(EdgeKind::Finally.to_string(), "finally");
    }

    #[test]
    fn test_cfg_builder_empty_function() {
        let mut builder = CfgBuilder::new();
        builder.build_function(&[]);

        let cfg = builder.build();
        let entry_block = cfg.blocks.get(&cfg.entry).unwrap();
        assert_eq!(entry_block.successors.len(), 1);
        assert_eq!(entry_block.successors[0].0, cfg.exit);
    }

    #[test]
    fn test_reachable_blocks() {
        let mut cfg = ControlFlowGraph::new();
        let block1 = cfg.new_block();
        let block2 = cfg.new_block();

        cfg.add_edge(cfg.entry, block1, EdgeKind::Normal);
        cfg.add_edge(block1, block2, EdgeKind::Normal);
        cfg.add_edge(block2, cfg.exit, EdgeKind::Normal);

        let reachable = cfg.reachable_blocks();
        assert!(reachable.contains(&cfg.entry));
        assert!(reachable.contains(&block1));
        assert!(reachable.contains(&block2));
        assert!(reachable.contains(&cfg.exit));
    }

    #[test]
    fn test_unreachable_blocks() {
        let mut cfg = ControlFlowGraph::new();
        let block1 = cfg.new_block();
        let unreachable = cfg.new_block();

        cfg.add_edge(cfg.entry, block1, EdgeKind::Normal);
        cfg.add_edge(block1, cfg.exit, EdgeKind::Normal);

        let unreachable_blocks = cfg.unreachable_blocks();
        assert!(unreachable_blocks.contains(&unreachable));
        assert!(!unreachable_blocks.contains(&block1));
    }

    #[test]
    fn test_cfg_if_statement() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 1, col: 4, end_line: 1, end_col: 4 }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 2);

        let has_true_edge = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::True));
        let has_false_edge = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::False));
        assert!(has_true_edge);
        assert!(has_false_edge);
    }

    #[test]
    fn test_cfg_if_else_statement() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 1, col: 4, end_line: 1, end_col: 8 }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            elif_parts: vec![],
            else_body: Some(vec![AstNode::Pass { line: 4, col: 5, end_line: 4, end_col: 9 }]),
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_true_edge = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::True));
        let has_false_edge = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::False));
        assert!(has_true_edge);
        assert!(has_false_edge);
    }

    #[test]
    fn test_cfg_for_loop() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Identifier {
                name: "items".to_string(),
                line: 1,
                col: 14,
                end_line: 1,
                end_col: 19,
            }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            else_body: None,
            is_async: false,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 5);

        let reachable = cfg.reachable_blocks();
        assert!(reachable.contains(&cfg.entry));
        assert!(reachable.contains(&cfg.exit));
    }

    #[test]
    fn test_cfg_for_loop_with_break() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Identifier {
                name: "items".to_string(),
                line: 1,
                col: 14,
                end_line: 1,
                end_col: 19,
            }),
            body: vec![AstNode::Break { line: 2, col: 5, end_line: 2, end_col: 10 }],
            else_body: None,
            is_async: false,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_break_edge = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Break));
        assert!(has_break_edge);
    }

    #[test]
    fn test_cfg_for_loop_with_continue() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Identifier {
                name: "items".to_string(),
                line: 1,
                col: 14,
                end_line: 1,
                end_col: 19,
            }),
            body: vec![AstNode::Continue { line: 2, col: 5, end_line: 2, end_col: 13 }],
            else_body: None,
            is_async: false,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_continue_edge = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Continue));
        assert!(has_continue_edge);
    }

    #[test]
    fn test_cfg_while_loop() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::While {
            test: Box::new(AstNode::Identifier { name: "cond".to_string(), line: 1, col: 7, end_line: 1, end_col: 11 }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            else_body: None,
            line: 1,
            end_line: 1,
            col: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 5);
        let reachable = cfg.reachable_blocks();
        assert!(reachable.contains(&cfg.entry));
        assert!(reachable.contains(&cfg.exit));
    }

    #[test]
    fn test_cfg_try_except() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::Try {
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            handlers: vec![ExceptHandler {
                exception_type: None,
                name: None,
                body: vec![AstNode::Pass { line: 4, col: 5, end_col: 9, end_line: 4 }],
                line: 3,
                col: 1,
                end_line: 3,
                end_col: 1,
            }],
            else_body: None,
            finally_body: None,
            line: 1,
            col: 1,
            end_col: 1,
            end_line: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_exception_edge = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Exception));
        assert!(has_exception_edge);
    }

    #[test]
    fn test_cfg_try_finally() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::Try {
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            handlers: vec![],
            else_body: None,
            finally_body: Some(vec![AstNode::Pass { line: 4, col: 5, end_line: 4, end_col: 9 }]),
            line: 1,
            col: 1,
            end_col: 1,
            end_line: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_finally_edge = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Finally));
        assert!(has_finally_edge);
    }

    #[test]
    fn test_cfg_with_statement() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::With {
            items: vec![WithItem {
                context_expr: AstNode::Identifier { name: "ctx".to_string(), line: 1, col: 6, end_col: 9, end_line: 1 },
                optional_vars: None,
            }],
            body: vec![AstNode::Pass { line: 2, col: 5, end_col: 9, end_line: 2 }],
            is_async: false,
            line: 1,
            col: 1,
            end_col: 1,
            end_line: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_finally_edge = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Finally));
        assert!(has_finally_edge);
    }

    #[test]
    fn test_cfg_match_statement() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::Match {
            subject: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 7, end_col: 8, end_line: 1 }),
            cases: vec![
                MatchCase {
                    pattern: Pattern::MatchValue(AstNode::Literal {
                        value: LiteralValue::Integer(1),
                        line: 2,
                        col: 10,
                        end_line: 2,
                        end_col: 11,
                    }),
                    guard: None,
                    body: vec![AstNode::Pass { line: 3, col: 9, end_col: 13, end_line: 3 }],
                    pattern_line: 2,
                    pattern_col: 10,
                    pattern_end_line: 2,
                    pattern_end_col: 11,
                    line: 2,
                    col: 5,
                    end_line: 3,
                    end_col: 13,
                },
                MatchCase {
                    pattern: Pattern::MatchValue(AstNode::Literal {
                        value: LiteralValue::Integer(2),
                        line: 4,
                        col: 10,
                        end_col: 11,
                        end_line: 4,
                    }),
                    guard: None,
                    body: vec![AstNode::Pass { line: 5, col: 9, end_line: 5, end_col: 13 }],
                    pattern_line: 4,
                    pattern_col: 10,
                    pattern_end_line: 4,
                    pattern_end_col: 11,
                    line: 4,
                    col: 5,
                    end_line: 5,
                    end_col: 13,
                },
            ],
            line: 1,
            col: 1,
            end_col: 1,
            end_line: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(
            cfg.blocks
                .values()
                .flat_map(|block| block.successors.iter())
                .filter(|(_, kind)| *kind == EdgeKind::True)
                .count()
                >= 2
        );
    }

    #[test]
    fn test_cfg_return_creates_unreachable_block() {
        let mut builder = CfgBuilder::new();
        let body = vec![
            AstNode::Return { value: None, line: 1, col: 1, end_col: 7, end_line: 1 },
            AstNode::Pass { line: 2, col: 1, end_col: 5, end_line: 2 },
        ];

        builder.build_function(&body);
        let cfg = builder.build();

        let unreachable = cfg.unreachable_blocks();
        assert!(!unreachable.is_empty());
    }

    #[test]
    fn test_cfg_nested_if_statements() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier { name: "cond1".to_string(), line: 1, col: 4, end_col: 9, end_line: 1 }),
            body: vec![AstNode::If {
                test: Box::new(AstNode::Identifier {
                    name: "cond2".to_string(),
                    line: 2,
                    col: 8,
                    end_col: 0,
                    end_line: 0,
                }),
                body: vec![AstNode::Pass { line: 3, col: 9, end_col: 0, end_line: 0 }],
                elif_parts: vec![],
                else_body: None,
                line: 2,
                col: 5,
                end_col: 0,
                end_line: 0,
            }],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            col: 1,
            end_col: 0,
            end_line: 0,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 2);

        let true_edge_count = cfg
            .blocks
            .values()
            .flat_map(|b| &b.successors)
            .filter(|(_, k)| *k == EdgeKind::True)
            .count();
        assert!(
            true_edge_count >= 2,
            "Expected at least 2 True edges for nested if statements"
        );
    }

    #[test]
    fn test_cfg_complex_control_flow() {
        let mut builder = CfgBuilder::new();
        let body = vec![
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 2,
                }),
                value: Box::new(AstNode::Literal {
                    value: beacon_parser::LiteralValue::Integer(1),
                    line: 1,
                    col: 5,
                    end_col: 0,
                    end_line: 0,
                }),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 1,
            },
            AstNode::For {
                target: Box::new(AstNode::Identifier {
                    name: "i".to_string(),
                    line: 1,
                    col: 5,
                    end_line: 1,
                    end_col: 6,
                }),
                iter: Box::new(AstNode::Identifier {
                    name: "range(10)".to_string(),
                    line: 2,
                    col: 14,
                    end_col: 0,
                    end_line: 0,
                }),
                body: vec![AstNode::If {
                    test: Box::new(AstNode::Identifier {
                        name: "cond".to_string(),
                        line: 3,
                        col: 12,
                        end_col: 0,
                        end_line: 0,
                    }),
                    body: vec![AstNode::Break { line: 4, col: 13, end_line: 4, end_col: 18 }],
                    elif_parts: vec![],
                    else_body: Some(vec![AstNode::Continue { line: 6, col: 13, end_line: 6, end_col: 21 }]),
                    line: 3,
                    col: 9,
                    end_line: 3,
                    end_col: 11,
                }],
                else_body: None,
                is_async: false,
                line: 2,
                col: 1,
                end_line: 2,
                end_col: 1,
            },
            AstNode::Return { value: None, line: 7, col: 1, end_line: 7, end_col: 7 },
        ];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_break = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::Break));
        let has_continue = cfg
            .blocks
            .values()
            .any(|b| b.successors.iter().any(|(_, k)| *k == EdgeKind::Continue));

        assert!(has_break);
        assert!(has_continue);
    }

    #[test]
    fn test_cfg_if_elif_chain() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::If {
            test: Box::new(AstNode::Identifier { name: "cond1".to_string(), line: 1, col: 4, end_line: 1, end_col: 9 }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            elif_parts: vec![
                (
                    AstNode::Identifier { name: "cond2".to_string(), line: 3, col: 6, end_line: 3, end_col: 11 },
                    vec![AstNode::Pass { line: 4, col: 5, end_line: 4, end_col: 9 }],
                ),
                (
                    AstNode::Identifier { name: "cond3".to_string(), line: 5, col: 6, end_line: 5, end_col: 11 },
                    vec![AstNode::Pass { line: 6, col: 5, end_line: 6, end_col: 9 }],
                ),
            ],
            else_body: Some(vec![AstNode::Pass { line: 8, col: 5, end_line: 8, end_col: 9 }]),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let true_edge_count = cfg
            .blocks
            .values()
            .flat_map(|b| &b.successors)
            .filter(|(_, k)| *k == EdgeKind::True)
            .count();

        assert!(
            true_edge_count >= 3,
            "Expected at least 3 True edges for if-elif-elif chain, got {true_edge_count}"
        );
    }

    #[test]
    fn test_cfg_nested_loops() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Identifier {
                name: "outer".to_string(),
                line: 1,
                col: 10,
                end_line: 1,
                end_col: 15,
            }),
            body: vec![AstNode::For {
                target: Box::new(AstNode::Identifier {
                    name: "j".to_string(),
                    line: 2,
                    col: 9,
                    end_line: 2,
                    end_col: 10,
                }),
                iter: Box::new(AstNode::Identifier {
                    name: "inner".to_string(),
                    line: 2,
                    col: 14,
                    end_line: 2,
                    end_col: 19,
                }),
                body: vec![AstNode::Pass { line: 3, col: 9, end_line: 3, end_col: 13 }],
                else_body: None,
                is_async: false,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            }],
            else_body: None,
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 7);
        let reachable = cfg.reachable_blocks();
        assert!(reachable.contains(&cfg.entry));
        assert!(reachable.contains(&cfg.exit));
    }

    #[test]
    fn test_cfg_try_except_else_finally() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::Try {
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            handlers: vec![ExceptHandler {
                exception_type: None,
                name: None,
                body: vec![AstNode::Pass { line: 4, col: 5, end_col: 9, end_line: 4 }],
                line: 3,
                col: 1,
                end_line: 3,
                end_col: 1,
            }],
            else_body: Some(vec![AstNode::Pass { line: 6, col: 5, end_line: 6, end_col: 9 }]),
            finally_body: Some(vec![AstNode::Pass { line: 8, col: 5, end_line: 8, end_col: 9 }]),
            line: 1,
            col: 1,
            end_col: 1,
            end_line: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        let has_exception = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Exception));
        let has_finally = cfg
            .blocks
            .values()
            .any(|block| block.successors.iter().any(|(_, kind)| *kind == EdgeKind::Finally));

        assert!(has_exception);
        assert!(has_finally);
    }

    #[test]
    fn test_cfg_for_loop_with_else() {
        let mut builder = CfgBuilder::new();
        let body = vec![AstNode::For {
            target: Box::new(AstNode::Identifier { name: "i".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            iter: Box::new(AstNode::Identifier {
                name: "items".to_string(),
                line: 1,
                col: 10,
                end_line: 1,
                end_col: 15,
            }),
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            else_body: Some(vec![AstNode::Pass { line: 4, col: 5, end_line: 4, end_col: 9 }]),
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        }];

        builder.build_function(&body);
        let cfg = builder.build();

        assert!(cfg.blocks.len() >= 5);
        let reachable = cfg.reachable_blocks();
        assert!(reachable.contains(&cfg.entry));
        assert!(reachable.contains(&cfg.exit));
    }
}
