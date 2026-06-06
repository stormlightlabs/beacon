mod annotations;
mod definition;
mod fstrings;
mod helpers;
mod model;
mod queries;
mod references;
mod scope_table;

use beacon_core::Result;

use crate::AstNode;

pub use model::{
    BUILTIN_DUNDERS, MAGIC_METHODS, ReferenceKind, Scope, ScopeId, ScopeKind, Symbol, SymbolKind, SymbolReference,
};
pub use scope_table::SymbolTable;

/// Name resolution context for traversing the AST
pub struct NameResolver {
    pub symbol_table: SymbolTable,
    current_scope: ScopeId,
    source: String,
}

impl NameResolver {
    pub fn new(source: String) -> Self {
        Self { source, ..Default::default() }
    }

    /// Resolve names in an AST and build the symbol table
    pub fn resolve(&mut self, ast: &AstNode) -> Result<()> {
        self.visit_node(ast)?;
        self.track_references(ast)?;
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

impl Default for NameResolver {
    fn default() -> Self {
        let symbol_table = SymbolTable::new();
        let root_scope = symbol_table.root_scope;
        Self { symbol_table, current_scope: root_scope, source: String::new() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Parameter;

    // NOTE: All test AST node constructions below include end_line and end_col fields
    // to match the updated AST structure that supports precise span tracking.
    // For test purposes, end positions are set to reasonable placeholder values
    // (typically same as start position for simple literals, or estimated based on
    // typical Python syntax for complex nodes).

    #[test]
    fn test_symbol_table_creation() {
        let table = SymbolTable::new();
        assert_eq!(table.scopes.len(), 1);
        assert!(table.scopes.contains_key(&table.root_scope));
    }

    #[test]
    fn test_scope_creation() {
        let mut table = SymbolTable::new();
        let func_scope = table.create_scope(ScopeKind::Function, table.root_scope, 0, 100);
        assert_eq!(table.scopes.len(), 2);
        assert!(table.scopes.contains_key(&func_scope));

        let root_scope = table.scopes.get(&table.root_scope).unwrap();
        assert_eq!(root_scope.children.len(), 1);
        assert_eq!(root_scope.children[0], func_scope);
    }

    #[test]
    fn test_symbol_lookup() {
        let mut table = SymbolTable::new();
        let func_scope = table.create_scope(ScopeKind::Function, table.root_scope, 10, 50);

        let root_symbol = Symbol {
            name: "global_var".to_string(),
            kind: SymbolKind::Variable,
            line: 1,
            col: 1,
            end_col: 11,
            scope_id: table.root_scope,
            docstring: None,
            references: Vec::new(),
        };
        table.add_symbol(table.root_scope, root_symbol);

        let func_symbol = Symbol {
            name: "local_var".to_string(),
            kind: SymbolKind::Variable,
            line: 2,
            col: 1,
            end_col: 10,
            scope_id: func_scope,
            docstring: None,
            references: Vec::new(),
        };
        table.add_symbol(func_scope, func_symbol);

        assert!(table.lookup_symbol("local_var", func_scope).is_some());
        assert!(table.lookup_symbol("global_var", func_scope).is_some());
        assert!(table.lookup_symbol("nonexistent", func_scope).is_none());
        assert!(table.lookup_symbol("global_var", table.root_scope).is_some());
        assert!(table.lookup_symbol("local_var", table.root_scope).is_none());
    }

    #[test]
    fn test_name_resolver_basic() {
        let source = "x = 42".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Assignment {
            target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
            value: Box::new(AstNode::Literal {
                value: crate::LiteralValue::Integer(42),
                line: 1,
                col: 5,
                end_line: 1,
                end_col: 7,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 7,
        };

        resolver.resolve(&ast).unwrap();

        let symbol = resolver.lookup("x");
        assert!(symbol.is_some());
        assert_eq!(symbol.unwrap().kind, SymbolKind::Variable);
    }

    #[test]
    fn test_name_resolver_function() {
        let source = "def test_func(param1, param2):\n    local_var = param1".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::FunctionDef {
            name: "test_func".to_string(),
            args: vec![
                Parameter {
                    name: "param1".to_string(),
                    line: 1,
                    col: 15,
                    end_line: 1,
                    end_col: 21,
                    type_annotation: None,
                    default_value: None,
                    kind: crate::ParameterKind::PositionalOrKeyword,
                },
                Parameter {
                    name: "param2".to_string(),
                    line: 1,
                    col: 23,
                    end_line: 1,
                    end_col: 29,
                    type_annotation: None,
                    default_value: None,
                    kind: crate::ParameterKind::PositionalOrKeyword,
                },
            ],
            body: vec![AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "local_var".to_string(),
                    line: 2,
                    col: 5,
                    end_line: 2,
                    end_col: 14,
                }),
                value: Box::new(AstNode::Identifier {
                    name: "param1".to_string(),
                    line: 2,
                    col: 15,
                    end_line: 2,
                    end_col: 21,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 21,
            }],
            line: 1,
            col: 1,
            end_line: 2,
            end_col: 21,
            docstring: None,
            return_type: None,
            decorators: Vec::new(),
            is_async: false,
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
    fn test_find_scope_at_position() {
        let source = "x = 1\ndef foo():\n    y = 2\n    z = 3\nw = 4".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::FunctionDef {
                    name: "foo".to_string(),
                    args: vec![],
                    body: vec![
                        AstNode::Assignment {
                            target: Box::new(AstNode::Identifier {
                                name: "y".to_string(),
                                line: 3,
                                col: 5,
                                end_line: 3,
                                end_col: 6,
                            }),
                            value: Box::new(AstNode::Literal {
                                value: crate::LiteralValue::Integer(2),
                                line: 3,
                                col: 9,
                                end_line: 3,
                                end_col: 10,
                            }),
                            line: 3,
                            col: 5,
                            end_line: 3,
                            end_col: 10,
                        },
                        AstNode::Assignment {
                            target: Box::new(AstNode::Identifier {
                                name: "z".to_string(),
                                line: 4,
                                col: 5,
                                end_line: 4,
                                end_col: 6,
                            }),
                            value: Box::new(AstNode::Literal {
                                value: crate::LiteralValue::Integer(3),
                                line: 4,
                                col: 9,
                                end_line: 4,
                                end_col: 10,
                            }),
                            line: 4,
                            col: 5,
                            end_line: 4,
                            end_col: 10,
                        },
                    ],
                    line: 2,
                    col: 1,
                    end_line: 4,
                    end_col: 10,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    is_async: false,
                },
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "w".to_string(),
                        line: 5,
                        col: 1,
                        end_line: 5,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(4),
                        line: 5,
                        col: 5,
                        end_line: 5,
                        end_col: 6,
                    }),
                    line: 5,
                    col: 1,
                    end_line: 5,
                    end_col: 6,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let byte_offset_x = 0;
        let scope_x = resolver.symbol_table.find_scope_at_position(byte_offset_x);
        assert_eq!(scope_x, resolver.symbol_table.root_scope);

        let byte_offset_y = 23;
        let scope_y = resolver.symbol_table.find_scope_at_position(byte_offset_y);
        assert_ne!(scope_y, resolver.symbol_table.root_scope);

        let y_symbol = resolver.symbol_table.lookup_symbol("y", scope_y);
        assert!(y_symbol.is_some());
        assert_eq!(y_symbol.unwrap().kind, SymbolKind::Variable);

        let y_from_root = resolver
            .symbol_table
            .lookup_symbol("y", resolver.symbol_table.root_scope);
        assert!(y_from_root.is_none());
    }

    #[test]
    fn test_nested_scopes() {
        let source = "global_var = 1\ndef outer(param):\n    outer_var = 2".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "global_var".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 11,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 15,
                        end_line: 1,
                        end_col: 16,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 16,
                },
                AstNode::FunctionDef {
                    name: "outer".to_string(),
                    args: vec![Parameter {
                        name: "param".to_string(),
                        line: 2,
                        col: 11,
                        end_line: 2,
                        end_col: 16,
                        type_annotation: None,
                        default_value: None,
                        kind: crate::ParameterKind::PositionalOrKeyword,
                    }],
                    body: vec![AstNode::Assignment {
                        target: Box::new(AstNode::Identifier {
                            name: "outer_var".to_string(),
                            line: 2,
                            col: 5,
                            end_line: 2,
                            end_col: 14,
                        }),
                        value: Box::new(AstNode::Literal {
                            value: crate::LiteralValue::Integer(2),
                            line: 3,
                            col: 20,
                            end_line: 3,
                            end_col: 21,
                        }),
                        line: 3,
                        col: 9,
                        end_line: 3,
                        end_col: 21,
                    }],
                    line: 2,
                    col: 1,
                    end_line: 3,
                    end_col: 21,
                    docstring: None,
                    return_type: None,
                    is_async: false,
                    decorators: Vec::new(),
                },
            ],
            docstring: None,
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

    #[test]
    fn test_attribute_node() {
        let source = "obj.field".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::Attribute {
            object: Box::new(AstNode::Identifier { name: "obj".to_string(), line: 1, col: 1, end_line: 1, end_col: 4 }),
            attribute: "field".to_string(),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 10,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_named_expr() {
        let source = "(x := 42)".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::NamedExpr {
            target: "x".to_string(),
            value: Box::new(AstNode::Literal {
                value: crate::LiteralValue::Integer(42),
                line: 1,
                col: 6,
                end_line: 1,
                end_col: 8,
            }),
            line: 1,
            col: 2,
            end_line: 1,
            end_col: 8,
        };

        resolver.resolve(&ast).unwrap();

        let symbol = resolver.lookup("x");
        assert!(symbol.is_some());
        assert_eq!(symbol.unwrap().kind, SymbolKind::Variable);
    }

    #[test]
    fn test_binary_op() {
        let source = "x + y".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::BinaryOp {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
            op: crate::BinaryOperator::Add,
            right: Box::new(AstNode::Identifier { name: "y".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            line: 1,
            col: 3,
            end_line: 1,
            end_col: 6,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_unary_op() {
        let source = "-x".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::UnaryOp {
            op: crate::UnaryOperator::Minus,
            operand: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 2, end_line: 1, end_col: 3 }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 3,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_compare() {
        let source = "x < y".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::Compare {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
            ops: vec![crate::CompareOperator::Lt],
            comparators: vec![AstNode::Identifier { name: "y".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }],
            line: 1,
            col: 3,
            end_line: 1,
            end_col: 6,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_subscript() {
        let source = "arr[0]".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::Subscript {
            value: Box::new(AstNode::Identifier { name: "arr".to_string(), line: 1, col: 1, end_line: 1, end_col: 4 }),
            slice: Box::new(AstNode::Literal {
                value: crate::LiteralValue::Integer(0),
                line: 1,
                col: 5,
                end_line: 1,
                end_col: 6,
            }),
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 7,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_list_comprehension() {
        let source = "[x for x in items]".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::ListComp {
            element: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 2, end_line: 1, end_col: 3 }),
            generators: vec![crate::Comprehension {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 8,
                    end_line: 1,
                    end_col: 9,
                }),
                iter: AstNode::Identifier { name: "items".to_string(), line: 1, col: 13, end_line: 1, end_col: 18 },
                ifs: vec![],
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 19,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_dict_comprehension() {
        let source = "{k: v for k, v in items}".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::DictComp {
            key: Box::new(AstNode::Identifier { name: "k".to_string(), line: 1, col: 2, end_line: 1, end_col: 3 }),
            value: Box::new(AstNode::Identifier { name: "v".to_string(), line: 1, col: 5, end_line: 1, end_col: 6 }),
            generators: vec![crate::Comprehension {
                target: Box::new(AstNode::Identifier {
                    name: "k".to_string(),
                    line: 1,
                    col: 11,
                    end_line: 1,
                    end_col: 12,
                }),
                iter: AstNode::Identifier { name: "items".to_string(), line: 1, col: 19, end_line: 1, end_col: 24 },
                ifs: vec![],
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 25,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_set_comprehension() {
        let source = "{x for x in items}".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::SetComp {
            element: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 2, end_line: 1, end_col: 3 }),
            generators: vec![crate::Comprehension {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 8,
                    end_line: 1,
                    end_col: 9,
                }),
                iter: AstNode::Identifier { name: "items".to_string(), line: 1, col: 13, end_line: 1, end_col: 18 },
                ifs: vec![],
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 19,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_generator_expression() {
        let source = "(x for x in items)".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::GeneratorExp {
            element: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 2, end_line: 1, end_col: 3 }),
            generators: vec![crate::Comprehension {
                target: Box::new(AstNode::Identifier {
                    name: "x".to_string(),
                    line: 1,
                    col: 8,
                    end_line: 1,
                    end_col: 9,
                }),
                iter: AstNode::Identifier { name: "items".to_string(), line: 1, col: 13, end_line: 1, end_col: 18 },
                ifs: vec![],
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 19,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_builtin_dunders_injected() {
        let table = SymbolTable::new();

        for &dunder_name in BUILTIN_DUNDERS {
            let symbol = table.lookup_symbol(dunder_name, table.root_scope);
            assert!(symbol.is_some(), "Expected {dunder_name} to be in symbol table");

            let sym = symbol.unwrap();
            assert_eq!(sym.kind, SymbolKind::BuiltinVar);
            assert_eq!(sym.name, dunder_name);
        }
    }

    #[test]
    fn test_is_in_class_scope() {
        let source = "class MyClass:\\n    def method(self):\\n        pass".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::ClassDef {
                name: "MyClass".to_string(),
                metaclass: None,
                bases: Vec::new(),
                body: vec![AstNode::FunctionDef {
                    name: "method".to_string(),
                    args: vec![Parameter {
                        name: "self".to_string(),
                        line: 2,
                        col: 16,
                        end_line: 2,
                        end_col: 20,
                        type_annotation: None,
                        default_value: None,
                        kind: crate::ParameterKind::PositionalOrKeyword,
                    }],
                    body: vec![AstNode::Pass { line: 3, col: 9, end_line: 3, end_col: 13 }],
                    line: 2,
                    col: 5,
                    end_line: 3,
                    end_col: 13,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    is_async: false,
                }],
                line: 1,
                col: 1,
                end_line: 3,
                end_col: 13,
                docstring: None,
                decorators: Vec::new(),
            }],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        assert!(
            !resolver
                .symbol_table
                .is_in_class_scope(resolver.symbol_table.root_scope)
        );

        let class_scope_id = resolver
            .symbol_table
            .scopes
            .get(&resolver.symbol_table.root_scope)
            .unwrap()
            .children[0];
        assert!(resolver.symbol_table.is_in_class_scope(class_scope_id));

        let method_scope_id = resolver.symbol_table.scopes.get(&class_scope_id).unwrap().children[0];
        assert!(resolver.symbol_table.is_in_class_scope(method_scope_id));
    }

    #[test]
    fn test_builtin_dunders_accessible_in_nested_scopes() {
        let source = "def foo():\\n    x = __name__".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::FunctionDef {
                name: "foo".to_string(),
                args: vec![],
                body: vec![AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Identifier {
                        name: "__name__".to_string(),
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 17,
                    }),
                    line: 2,
                    col: 5,
                    end_line: 2,
                    end_col: 17,
                }],
                line: 1,
                col: 1,
                end_line: 2,
                end_col: 17,
                docstring: None,
                return_type: None,
                decorators: Vec::new(),
                is_async: false,
            }],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let func_scope_id = resolver
            .symbol_table
            .scopes
            .get(&resolver.symbol_table.root_scope)
            .unwrap()
            .children[0];

        let symbol = resolver.symbol_table.lookup_symbol("__name__", func_scope_id);
        assert!(symbol.is_some());
        assert_eq!(symbol.unwrap().kind, SymbolKind::BuiltinVar);
    }

    #[test]
    fn test_match_statement() {
        let source = "match x:\n    case 1:\n        pass".to_string();
        let mut resolver = NameResolver::new(source);
        let ast = AstNode::Match {
            subject: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 7, end_line: 1, end_col: 8 }),
            cases: vec![crate::MatchCase {
                pattern: crate::Pattern::MatchValue(AstNode::Literal {
                    value: crate::LiteralValue::Integer(1),
                    line: 2,
                    col: 10,
                    end_line: 2,
                    end_col: 11,
                }),
                guard: None,
                body: vec![AstNode::Pass { line: 3, col: 9, end_line: 3, end_col: 13 }],
                pattern_line: 2,
                pattern_col: 10,
                pattern_end_line: 2,
                pattern_end_col: 11,
                line: 2,
                col: 5,
                end_line: 3,
                end_col: 13,
            }],
            line: 1,
            col: 1,
            end_line: 3,
            end_col: 13,
        };

        resolver.resolve(&ast).unwrap();
    }

    #[test]
    fn test_find_shadowed_symbols_simple() {
        let source = "x = 1\ndef foo():\n    x = 2".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::FunctionDef {
                    name: "foo".to_string(),
                    args: vec![],
                    body: vec![AstNode::Assignment {
                        target: Box::new(AstNode::Identifier {
                            name: "x".to_string(),
                            line: 1,
                            col: 1,
                            end_line: 1,
                            end_col: 2,
                        }),
                        value: Box::new(AstNode::Literal {
                            value: crate::LiteralValue::Integer(2),
                            line: 3,
                            col: 9,
                            end_line: 3,
                            end_col: 10,
                        }),
                        line: 3,
                        col: 5,
                        end_line: 3,
                        end_col: 10,
                    }],
                    line: 2,
                    col: 1,
                    end_line: 3,
                    end_col: 10,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    is_async: false,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let shadowed = resolver.symbol_table.find_shadowed_symbols();
        assert_eq!(shadowed.len(), 1);
        assert_eq!(shadowed[0].0.name, "x");
        assert_eq!(shadowed[0].0.line, 3);
        assert_eq!(shadowed[0].1.name, "x");
        assert_eq!(shadowed[0].1.line, 1);
    }

    #[test]
    fn test_find_shadowed_symbols_nested() {
        let source = "x = 1\ndef outer():\n    x = 2\n    def inner():\n        x = 3".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::FunctionDef {
                    name: "outer".to_string(),
                    args: vec![],
                    body: vec![
                        AstNode::Assignment {
                            target: Box::new(AstNode::Identifier {
                                name: "x".to_string(),
                                line: 1,
                                col: 1,
                                end_line: 1,
                                end_col: 2,
                            }),
                            value: Box::new(AstNode::Literal {
                                value: crate::LiteralValue::Integer(2),
                                line: 3,
                                col: 9,
                                end_line: 3,
                                end_col: 10,
                            }),
                            line: 3,
                            col: 5,
                            end_line: 3,
                            end_col: 10,
                        },
                        AstNode::FunctionDef {
                            name: "inner".to_string(),
                            args: vec![],
                            body: vec![AstNode::Assignment {
                                target: Box::new(AstNode::Identifier {
                                    name: "x".to_string(),
                                    line: 1,
                                    col: 1,
                                    end_line: 1,
                                    end_col: 2,
                                }),
                                value: Box::new(AstNode::Literal {
                                    value: crate::LiteralValue::Integer(3),
                                    line: 5,
                                    col: 13,
                                    end_line: 5,
                                    end_col: 14,
                                }),
                                line: 5,
                                col: 9,
                                end_line: 5,
                                end_col: 14,
                            }],
                            line: 4,
                            col: 5,
                            end_line: 5,
                            end_col: 14,
                            docstring: None,
                            return_type: None,
                            decorators: Vec::new(),
                            is_async: false,
                        },
                    ],
                    line: 2,
                    col: 1,
                    end_line: 5,
                    end_col: 14,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    is_async: false,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let shadowed = resolver.symbol_table.find_shadowed_symbols();
        assert!(shadowed.len() >= 2);
        assert!(shadowed.iter().any(|(child, _)| child.line == 3));
        assert!(shadowed.iter().any(|(child, _)| child.line == 5));
    }

    #[test]
    fn test_find_shadowed_symbols_no_shadowing() {
        let source = "x = 1\ny = 2".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "y".to_string(),
                        line: 2,
                        col: 1,
                        end_line: 2,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(2),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 6,
                    }),
                    line: 2,
                    col: 1,
                    end_line: 2,
                    end_col: 6,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let shadowed = resolver.symbol_table.find_shadowed_symbols();
        assert_eq!(shadowed.len(), 0);
    }

    #[test]
    fn test_find_shadowed_symbols_parameters_ignored() {
        let source = "x = 1\ndef foo(x):\n    pass".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::FunctionDef {
                    name: "foo".to_string(),
                    args: vec![Parameter {
                        name: "x".to_string(),
                        line: 2,
                        col: 9,
                        end_line: 2,
                        end_col: 10,
                        type_annotation: None,
                        default_value: None,
                        kind: crate::ParameterKind::PositionalOrKeyword,
                    }],
                    body: vec![AstNode::Pass { line: 3, col: 5, end_line: 3, end_col: 9 }],
                    line: 2,
                    col: 1,
                    end_line: 3,
                    end_col: 9,
                    docstring: None,
                    return_type: None,
                    decorators: Vec::new(),
                    is_async: false,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let shadowed = resolver.symbol_table.find_shadowed_symbols();
        assert_eq!(shadowed.len(), 0);
    }

    #[test]
    fn test_symbol_references_tracking() {
        let source = "x = 1\ny = x + x".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "y".to_string(),
                        line: 2,
                        col: 1,
                        end_line: 2,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::BinaryOp {
                        left: Box::new(AstNode::Identifier {
                            name: "x".to_string(),
                            line: 2,
                            col: 5,
                            end_line: 2,
                            end_col: 6,
                        }),
                        op: crate::BinaryOperator::Add,
                        right: Box::new(AstNode::Identifier {
                            name: "x".to_string(),
                            line: 2,
                            col: 9,
                            end_line: 2,
                            end_col: 10,
                        }),
                        line: 2,
                        col: 7,
                        end_line: 2,
                        end_col: 10,
                    }),
                    line: 2,
                    col: 1,
                    end_line: 2,
                    end_col: 10,
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let x_symbol = resolver
            .symbol_table
            .lookup_symbol("x", resolver.symbol_table.root_scope)
            .unwrap();

        let read_refs = x_symbol
            .references
            .iter()
            .filter(|r| r.kind == ReferenceKind::Read)
            .count();
        assert_eq!(read_refs, 2);
    }

    #[test]
    fn test_find_unused_symbols_basic() {
        let source = "x = 1\ny = 2\nprint(y)".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(1),
                        line: 1,
                        col: 5,
                        end_line: 1,
                        end_col: 6,
                    }),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "y".to_string(),
                        line: 2,
                        col: 1,
                        end_line: 2,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: crate::LiteralValue::Integer(2),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 6,
                    }),
                    line: 2,
                    col: 1,
                    end_line: 2,
                    end_col: 6,
                },
                AstNode::Call {
                    function: Box::new(AstNode::Identifier {
                        name: "print".to_string(),
                        line: 3,
                        col: 1,
                        end_line: 3,
                        end_col: 6,
                    }),
                    args: vec![AstNode::Identifier { name: "y".to_string(), line: 3, col: 7, end_line: 3, end_col: 8 }],
                    line: 3,
                    col: 1,
                    end_line: 3,
                    end_col: 9,
                    keywords: Vec::new(),
                },
            ],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let unused = resolver.symbol_table.find_unused_symbols();
        assert_eq!(unused.len(), 1);
        assert_eq!(unused[0].name, "x");
    }

    #[test]
    fn test_find_unused_symbols_underscore_prefix() {
        let source = "_x = 1".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "_x".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 3,
                }),
                value: Box::new(AstNode::Literal {
                    value: crate::LiteralValue::Integer(1),
                    line: 1,
                    col: 6,
                    end_line: 1,
                    end_col: 7,
                }),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 7,
            }],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let unused = resolver.symbol_table.find_unused_symbols();
        assert_eq!(unused.len(), 0);
    }

    #[test]
    fn test_find_unused_symbols_functions_ignored() {
        let source = "def foo():\n    pass".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::FunctionDef {
                name: "foo".to_string(),
                args: vec![],
                body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
                line: 1,
                col: 1,
                end_line: 2,
                end_col: 9,
                docstring: None,
                return_type: None,
                is_async: false,
                decorators: Vec::new(),
            }],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let unused = resolver.symbol_table.find_unused_symbols();
        assert_eq!(unused.len(), 0);
    }

    #[test]
    fn test_find_unused_symbols_classes_ignored() {
        let source = "class MyClass:\n    pass".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::ClassDef {
                name: "MyClass".to_string(),
                metaclass: None,
                bases: Vec::new(),
                body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
                line: 1,
                col: 1,
                end_line: 2,
                end_col: 9,
                docstring: None,
                decorators: Vec::new(),
            }],
            docstring: None,
        };

        resolver.resolve(&ast).unwrap();

        let unused = resolver.symbol_table.find_unused_symbols();
        assert_eq!(unused.len(), 0);
    }

    #[test]
    fn test_decorator_reference_span_matches_decorator_line() {
        let source = "from dataclasses import dataclass\n\n@dataclass\nclass Foo:\n    pass\n";
        let mut parser = crate::PythonParser::new().unwrap();
        let parsed = parser.parse(source).unwrap();
        let ast = parser.to_ast(&parsed).unwrap();
        let mut resolver = NameResolver::new(source.to_string());
        resolver.resolve(&ast).unwrap();

        let dataclass_symbol = resolver
            .symbol_table
            .lookup_symbol("dataclass", resolver.symbol_table.root_scope)
            .expect("expected dataclass symbol");

        assert_eq!(dataclass_symbol.references.len(), 1);
        let reference = &dataclass_symbol.references[0];
        assert_eq!(reference.line, 3);
        assert_eq!(reference.col, 2);
        assert_eq!(reference.end_col, 11);
    }

    #[test]
    fn test_add_reference_returns_true_on_success() {
        let mut table = SymbolTable::new();
        let func_scope = table.create_scope(ScopeKind::Function, table.root_scope, 10, 50);

        let symbol = Symbol {
            name: "x".to_string(),
            kind: SymbolKind::Variable,
            line: 1,
            col: 1,
            end_col: 2,
            scope_id: func_scope,
            docstring: None,
            references: Vec::new(),
        };
        table.add_symbol(func_scope, symbol);

        let result = table.add_reference("x", func_scope, 5, 10, 11, ReferenceKind::Read);
        assert!(result);

        let x_symbol = table.lookup_symbol("x", func_scope).unwrap();
        assert_eq!(x_symbol.references.len(), 1);
        assert_eq!(x_symbol.references[0].kind, ReferenceKind::Read);
    }

    #[test]
    fn test_add_reference_returns_false_on_missing_symbol() {
        let mut table = SymbolTable::new();
        let func_scope = table.create_scope(ScopeKind::Function, table.root_scope, 10, 50);

        let result = table.add_reference("nonexistent", func_scope, 5, 10, 21, ReferenceKind::Read);
        assert!(!result);
    }
}
