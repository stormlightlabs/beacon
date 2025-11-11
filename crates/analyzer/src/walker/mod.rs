// Submodules
mod class;
mod function;
mod guards;
mod utils;
mod visitors;

use super::loader::{self};
use super::type_env::TypeEnvironment;

use beacon_constraint::{Constraint, ConstraintGenContext, ConstraintResult, ConstraintSet, Span};
use beacon_core::{Type, TypeScheme, errors::Result};
use beacon_parser::{AstNode, LiteralValue, SymbolTable};

/// Expression context for constraint generation
///
/// Tracks whether an expression's result is used (value context) or discarded (void context) to avoid generating spurious constraints.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprContext {
    /// Value context: expression result is used (e.g., in assignment, return, argument)
    Value,
    /// Void context: expression result is discarded (e.g., statement position)
    Void,
}

// Note: FunctionKind, detect_function_kind, contains_yield, analyze_return_paths,
// and all_paths_exit are now defined in function.rs and imported above.
// is_docstring, get_node_position, type_name_to_type, is_main_guard are now defined
// in utils.rs and imported above.

pub fn generate_constraints(
    stub_cache: &Option<visitors::TStubCache>, ast: &AstNode, symbol_table: &SymbolTable,
) -> Result<ConstraintResult> {
    let mut ctx = ConstraintGenContext::new();

    if let Some(stub_cache) = &stub_cache {
        if let Ok(cache) = stub_cache.try_read() {
            if let Some(builtins) = cache.get("builtins") {
                loader::load_stub_into_registry(builtins, &mut ctx.class_registry)?;
            }
        }
    }

    let mut env = TypeEnvironment::from_symbol_table(symbol_table, ast);

    visit_node_with_env(ast, &mut env, &mut ctx, stub_cache.as_ref())?;

    Ok(ConstraintResult(
        ConstraintSet { constraints: ctx.constraints },
        ctx.type_map,
        ctx.position_map,
        ctx.class_registry,
    ))
}

/// Visit an AST node and generate constraints with type environment
///
/// Implements constraint generation for core Python constructs and records type information in the context for type-at-position queries.
pub fn visit_node_with_env(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext,
    stub_cache: Option<&visitors::TStubCache>,
) -> Result<Type> {
    visit_node_with_context(node, env, ctx, stub_cache, ExprContext::Value)
}

/// Internal visitor with expression context tracking
///
/// The `expr_ctx` parameter determines whether to generate unification constraints for expression results.
/// In void contexts (statement position), we skip generating Equal constraints since the result is discarded.
fn visit_node_with_context(
    node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext,
    stub_cache: Option<&visitors::TStubCache>, expr_ctx: ExprContext,
) -> Result<Type> {
    match node {
        AstNode::Module { .. } => visitors::visit_module(node, env, ctx, stub_cache),
        AstNode::FunctionDef { .. } => visitors::visit_function(node, env, ctx, stub_cache),
        AstNode::ClassDef { .. } => visitors::visit_class_def(node, env, ctx, stub_cache),
        // TODO: Determine if value is non-expansive for generalization
        AstNode::Assignment { .. } | AstNode::AnnotatedAssignment { .. } => {
            visitors::visit_assignments(node, env, ctx, stub_cache)
        }
        AstNode::Call { .. } => visitors::visit_call(node, env, ctx, stub_cache),
        AstNode::Identifier { name, line, col, .. } => {
            let ty = env.lookup(name).unwrap_or_else(|| Type::Var(env.fresh_var()));
            ctx.record_type(*line, *col, ty.clone());
            Ok(ty)
        }
        AstNode::Literal { value, line, col, .. } => {
            let ty = match value {
                LiteralValue::Integer(_) => Type::int(),
                LiteralValue::Float(_) => Type::float(),
                LiteralValue::String { .. } => Type::string(),
                LiteralValue::Boolean(_) => Type::bool(),
                LiteralValue::None => Type::none(),
            };
            ctx.record_type(*line, *col, ty.clone());
            Ok(ty)
        }
        AstNode::Return { value, line, col, end_line, end_col, .. } => {
            let ty = if let Some(val) = value {
                visit_node_with_context(val, env, ctx, stub_cache, ExprContext::Value)?
            } else {
                Type::none()
            };

            if let Some(expected_ret_ty) = env.get_expected_return_type() {
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints
                    .push(Constraint::Equal(ty.clone(), expected_ret_ty.clone(), span));
            }

            ctx.record_type(*line, *col, ty.clone());
            Ok(ty)
        }
        AstNode::Yield { .. } | AstNode::YieldFrom { .. } => visitors::visit_yield(node, env, ctx, stub_cache),
        AstNode::Await { .. } => visitors::visit_await(node, env, ctx, stub_cache),
        AstNode::Attribute { object, attribute, line, col, end_line, end_col, .. } => {
            let obj_ty = visit_node_with_env(object, env, ctx, stub_cache)?;
            let attr_ty = Type::Var(env.fresh_var());
            let span = Span::with_end(*line, *col, *end_line, *end_col);
            ctx.constraints
                .push(Constraint::HasAttr(obj_ty, attribute.clone(), attr_ty.clone(), span));
            ctx.record_type(*line, *col, attr_ty.clone());
            Ok(attr_ty)
        }
        AstNode::BinaryOp { .. } | AstNode::UnaryOp { .. } => visitors::visit_ops(node, env, ctx, stub_cache),
        AstNode::Subscript { value, slice, line, col, end_line, end_col, .. } => {
            let value_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
            let slice_ty = visit_node_with_env(slice, env, ctx, stub_cache)?;
            let span = Span::with_end(*line, *col, *end_line, *end_col);

            let getitem_method_ty = Type::Var(env.fresh_var());
            ctx.constraints.push(Constraint::HasAttr(
                value_ty,
                "__getitem__".to_string(),
                getitem_method_ty.clone(),
                span,
            ));

            let result_ty = Type::Var(env.fresh_var());
            ctx.constraints.push(Constraint::Call(
                getitem_method_ty,
                vec![(slice_ty, span)],
                vec![],
                result_ty.clone(),
                span,
            ));

            ctx.record_type(*line, *col, result_ty.clone());
            Ok(result_ty)
        }
        AstNode::If { .. } => visitors::visit_if(node, env, ctx, stub_cache, expr_ctx),
        AstNode::Import { .. } | AstNode::ImportFrom { .. } => visitors::visit_imports(node, env, ctx, stub_cache),
        AstNode::For { .. } => visitors::visit_for(node, env, ctx, stub_cache),
        AstNode::While { .. } => visitors::visit_while(node, env, ctx, stub_cache),
        AstNode::Try { .. } => visitors::visit_try_raise(node, env, ctx, stub_cache),
        AstNode::Match { .. } => visitors::visit_match(node, env, ctx, stub_cache),
        AstNode::Raise { .. } => visitors::visit_try_raise(node, env, ctx, stub_cache),
        AstNode::With { .. } => visitors::visit_with(node, env, ctx, stub_cache),
        AstNode::Lambda { .. } => visitors::visit_lambda(node, env, ctx, stub_cache),
        AstNode::Compare { left, comparators, line, col, .. } => {
            visit_node_with_env(left, env, ctx, stub_cache)?;

            for comp in comparators {
                visit_node_with_env(comp, env, ctx, stub_cache)?;
            }

            ctx.record_type(*line, *col, Type::bool());
            Ok(Type::bool())
        }
        AstNode::NamedExpr { target, value, line, col, .. } => {
            let value_ty = visit_node_with_env(value, env, ctx, stub_cache)?;
            env.bind(target.clone(), TypeScheme::mono(value_ty.clone()));
            ctx.record_type(*line, *col, value_ty.clone());
            Ok(value_ty)
        }
        AstNode::ListComp { .. } | AstNode::SetComp { .. } | AstNode::DictComp { .. } => {
            visitors::visit_comprehension(node, env, ctx, stub_cache)
        }
        AstNode::GeneratorExp { element, generators, line, col, end_col, end_line } => {
            let mut comp_env = env.clone();

            for generator in generators {
                let iter_ty = visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                let element_ty = Type::Var(comp_env.fresh_var());
                let span = Span::with_end(*line, *col, *end_line, *end_col);
                ctx.constraints.push(Constraint::Protocol(
                    iter_ty,
                    beacon_core::ProtocolName::Iterable,
                    element_ty.clone(),
                    span,
                ));

                comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                for if_clause in &generator.ifs {
                    visit_node_with_env(if_clause, &mut comp_env, ctx, stub_cache)?;
                }
            }

            let elem_ty = visit_node_with_env(element, &mut comp_env, ctx, stub_cache)?;
            let generator_ty = Type::generator(elem_ty, Type::none(), Type::none());

            ctx.record_type(*line, *col, generator_ty.clone());
            Ok(generator_ty)
        }
        AstNode::Tuple { .. } | AstNode::List { .. } | AstNode::Dict { .. } | AstNode::Set { .. } => {
            visitors::visit_collections(node, env, ctx, stub_cache)
        }
        AstNode::Pass { .. } | AstNode::Break { .. } | AstNode::Continue { .. } => Ok(Type::none()),
        AstNode::Global { .. } | AstNode::Nonlocal { .. } => Ok(Type::none()),
        AstNode::Assert { .. } | AstNode::Starred { .. } => Ok(Type::none()),
        AstNode::ParenthesizedExpression { expression, .. } => {
            visit_node_with_context(expression, env, ctx, stub_cache, expr_ctx)
        }
    }
}

/// Extract the variable being guarded by a test expression (TODO)
#[allow(dead_code)]
fn extract_guarded_variable(test: &AstNode, env: &mut TypeEnvironment) -> Option<(String, Type)> {
    let var_name = match test {
        AstNode::Compare { left, .. } => {
            if let AstNode::Identifier { name, .. } = left.as_ref() {
                Some(name.clone())
            } else {
                None
            }
        }
        AstNode::Identifier { name, .. } => Some(name.clone()),
        _ => None,
    }?;

    let original_type = env.lookup(&var_name)?;
    Some((var_name, original_type))
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_core::TypeCtor;
    use beacon_parser::{AstNode, SymbolTable};

    #[test]
    fn test_conditional_assignment_merges_branch_types() {
        let mut ctx = ConstraintGenContext::new();
        let mut env = super::super::type_env::TypeEnvironment::new();

        env.bind("provider".to_string(), TypeScheme::mono(Type::optional(Type::string())));

        let condition = AstNode::Compare {
            left: Box::new(AstNode::Identifier {
                name: "provider".to_string(),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 9,
            }),
            ops: vec![beacon_parser::CompareOperator::Is],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::None,
                line: 1,
                col: 15,
                end_col: 19,
                end_line: 1,
            }],
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let reassignment = AstNode::Assignment {
            target: Box::new(AstNode::Identifier {
                name: "provider".to_string(),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 9,
            }),
            value: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "fallback".to_string(), prefix: String::new() },
                line: 2,
                end_line: 2,
                col: 4,
                end_col: 12,
            }),
            line: 2,
            col: 4,
            end_line: 2,
            end_col: 4,
        };

        let if_node = AstNode::If {
            test: Box::new(condition),
            body: vec![reassignment],
            elif_parts: vec![],
            else_body: None,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        visit_node_with_context(&if_node, &mut env, &mut ctx, None, ExprContext::Void).unwrap();

        let scheme = env.get_scheme("provider").expect("provider binding");
        assert_eq!(scheme.ty, Type::string());
    }

    #[test]
    fn test_await_coroutine_type() {
        let mut ctx = ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let async_fn = AstNode::FunctionDef {
            name: "async_fn".to_string(),
            args: vec![],
            body: vec![AstNode::Return {
                value: Some(Box::new(AstNode::Literal {
                    value: beacon_parser::LiteralValue::Integer(42),
                    line: 2,
                    end_line: 2,
                    col: 12,
                    end_col: 12,
                })),
                line: 2,
                end_line: 2,
                col: 5,
                end_col: 5,
            }],
            docstring: None,
            return_type: Some("int".to_string()),
            decorators: vec![],
            is_async: true,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&async_fn, &mut env, &mut ctx, None).unwrap();

        assert!(matches!(fn_ty, Type::Fun(_, _)));
    }

    #[test]
    fn test_await_generates_awaitable_constraint() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let await_expr = AstNode::Await {
            value: Box::new(AstNode::Identifier {
                name: "unknown_awaitable".to_string(),
                line: 1,
                col: 7,
                end_line: 1,
                end_col: 24,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let result_ty = visit_node_with_env(&await_expr, &mut env, &mut ctx, None).unwrap();

        assert!(matches!(result_ty, Type::Var(_)));
        assert!(!ctx.constraints.is_empty());
    }

    #[test]
    fn test_void_context_module_level_call() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "create_calculator".to_string(),
            args: vec![],
            body: vec![],
            docstring: None,
            return_type: Some("Calculator".to_string()),
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };
        visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        let module = AstNode::Module {
            body: vec![AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "create_calculator".to_string(),
                    line: 2,
                    col: 1,
                    end_line: 2,
                    end_col: 1,
                }),
                args: vec![],
                keywords: vec![],
                line: 2,
                col: 1,
                end_line: 2,
                end_col: 1,
            }],
            docstring: None,
        };

        visit_node_with_env(&module, &mut env, &mut ctx, None).unwrap();

        let has_none_calculator_equal = ctx.constraints.iter().any(|c| {
            matches!(
                c,
                Constraint::Equal(Type::Con(TypeCtor::NoneType), Type::Con(_), _)
                    | Constraint::Equal(Type::Con(_), Type::Con(TypeCtor::NoneType), _)
            )
        });

        assert!(
            !has_none_calculator_equal,
            "Should not generate Equal constraints involving None for expression statements in void context"
        );

        let has_call_constraint = ctx
            .constraints
            .iter()
            .any(|c| matches!(c, Constraint::Call(_, _, _, _, _)));

        assert!(
            has_call_constraint,
            "Should still generate Call constraint to type-check the function call"
        );
    }

    #[test]
    fn test_if_main_guard_void_context() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let main_func = AstNode::FunctionDef {
            name: "main".to_string(),
            args: vec![],
            body: vec![],
            docstring: None,
            return_type: Some("Calculator".to_string()),
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };
        visit_node_with_env(&main_func, &mut env, &mut ctx, None).unwrap();

        let if_main = AstNode::If {
            test: Box::new(AstNode::Compare {
                left: Box::new(AstNode::Identifier {
                    name: "__name__".to_string(),
                    line: 3,
                    col: 4,
                    end_line: 3,
                    end_col: 12,
                }),
                ops: vec![beacon_parser::CompareOperator::Eq],
                comparators: vec![AstNode::Literal {
                    value: LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                    line: 3,
                    end_line: 3,
                    col: 15,
                    end_col: 15,
                }],
                line: 3,
                end_line: 3,
                col: 4,
                end_col: 4,
            }),
            body: vec![AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "main".to_string(),
                    line: 4,
                    col: 5,
                    end_line: 4,
                    end_col: 8,
                }),
                args: vec![],
                keywords: vec![],
                line: 4,
                col: 5,
                end_line: 4,
                end_col: 8,
            }],
            elif_parts: vec![],
            else_body: None,
            line: 3,
            col: 1,
            end_line: 3,
            end_col: 1,
        };

        visit_node_with_env(&if_main, &mut env, &mut ctx, None).unwrap();

        let has_none_calculator_equal = ctx.constraints.iter().any(|c| {
            matches!(
                c,
                Constraint::Equal(Type::Con(TypeCtor::NoneType), Type::Con(_), _)
                    | Constraint::Equal(Type::Con(_), Type::Con(TypeCtor::NoneType), _)
            )
        });

        assert!(
            !has_none_calculator_equal,
            "if __name__ == \"__main__\" body should be void context, not generating Equal constraints with None"
        );

        let has_call_constraint = ctx
            .constraints
            .iter()
            .any(|c| matches!(c, Constraint::Call(_, _, _, _, _)));

        assert!(has_call_constraint, "Should still type-check the main() call");
    }

    #[test]
    fn test_if_main_guard_detection() {
        let test1 = AstNode::Compare {
            left: Box::new(AstNode::Identifier {
                name: "__name__".to_string(),
                line: 1,
                col: 4,
                end_line: 1,
                end_col: 12,
            }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                line: 1,
                end_line: 1,
                col: 15,
                end_col: 15,
            }],
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 4,
        };
        assert!(utils::is_main_guard(&test1), "Should detect __name__ == \"__main__\"");

        let test2 = AstNode::Compare {
            left: Box::new(AstNode::Literal {
                value: LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                line: 1,
                end_line: 1,
                col: 4,
                end_col: 4,
            }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Identifier {
                name: "__name__".to_string(),
                line: 1,
                col: 18,
                end_line: 1,
                end_col: 26,
            }],
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 4,
        };
        assert!(utils::is_main_guard(&test2), "Should detect \"__main__\" == __name__");

        let test3 = AstNode::Compare {
            left: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 4, end_line: 1, end_col: 5 }),
            ops: vec![beacon_parser::CompareOperator::Eq],
            comparators: vec![AstNode::Literal {
                value: LiteralValue::Integer(42),
                line: 1,
                col: 9,
                end_line: 1,
                end_col: 11,
            }],
            line: 1,
            col: 4,
            end_line: 1,
            end_col: 4,
        };
        assert!(!utils::is_main_guard(&test3), "Should not detect regular comparison");
    }

    #[test]
    fn test_function_with_implicit_none_return() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "do_nothing".to_string(),
            args: vec![],
            body: vec![AstNode::Pass { line: 2, col: 5, end_line: 2, end_col: 9 }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = fn_ty {
            assert!(params.is_empty(), "Should have no parameters");
            assert!(
                matches!(*ret_ty, Type::Con(TypeCtor::NoneType)),
                "Should infer None return type"
            );
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_function_with_explicit_return_constrains_type() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "get_number".to_string(),
            args: vec![],
            body: vec![AstNode::Return {
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(42),
                    line: 2,
                    end_line: 2,
                    col: 12,
                    end_col: 12,
                })),
                line: 2,
                end_line: 2,
                col: 5,
                end_col: 5,
            }],
            docstring: None,
            return_type: Some("int".to_string()),
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        let has_return_constraint = ctx.constraints.iter().any(|c| {
            matches!(
                c,
                Constraint::Equal(Type::Con(TypeCtor::Int), Type::Con(TypeCtor::Int), _)
            )
        });

        assert!(
            has_return_constraint || !ctx.constraints.is_empty(),
            "Should generate constraint for return value"
        );
    }

    #[test]
    fn test_function_with_mixed_returns_infers_optional() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "maybe_number".to_string(),
            args: vec![],
            body: vec![AstNode::If {
                test: Box::new(AstNode::Identifier {
                    name: "condition".to_string(),
                    line: 2,
                    col: 8,
                    end_line: 2,
                    end_col: 17,
                }),
                body: vec![AstNode::Return {
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(42),
                        line: 3,
                        end_line: 3,
                        col: 16,
                        end_col: 16,
                    })),
                    line: 3,
                    end_line: 3,
                    col: 9,
                    end_col: 9,
                }],
                elif_parts: vec![],
                else_body: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = fn_ty {
            assert!(params.is_empty(), "Should have no parameters");
            match ret_ty.as_ref() {
                Type::Union(members) => {
                    assert_eq!(members.len(), 2, "Optional should be a union of 2 types");
                    assert!(
                        members.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType))),
                        "Union should contain None type"
                    );
                    assert!(
                        members.iter().any(|t| matches!(t, Type::Var(_))),
                        "Union should contain a type variable for the inner type"
                    );
                }
                _ => panic!("Expected Union type for Optional, got {ret_ty:?}"),
            }
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_function_with_explicit_none_return_infers_optional() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "maybe_string".to_string(),
            args: vec![],
            body: vec![AstNode::If {
                test: Box::new(AstNode::Identifier {
                    name: "condition".to_string(),
                    line: 2,
                    col: 8,
                    end_line: 2,
                    end_col: 17,
                }),
                body: vec![AstNode::Return {
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::String { value: "hello".to_string(), prefix: String::new() },
                        line: 3,
                        end_line: 3,
                        col: 16,
                        end_col: 16,
                    })),
                    line: 3,
                    col: 9,
                    end_line: 3,
                    end_col: 9,
                }],
                elif_parts: vec![],
                else_body: Some(vec![AstNode::Return {
                    value: None,
                    line: 5,
                    col: 9,
                    end_line: 5,
                    end_col: 9,
                }]),
                line: 2,
                end_line: 2,
                col: 5,
                end_col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = fn_ty {
            assert!(params.is_empty(), "Should have no parameters");
            match ret_ty.as_ref() {
                Type::Union(members) => {
                    assert_eq!(members.len(), 2, "Optional should be a union of 2 types");
                    assert!(
                        members.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType))),
                        "Union should contain None type"
                    );
                }
                _ => panic!("Expected Union type for Optional, got {ret_ty:?}"),
            }
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_function_all_paths_return_value_not_optional() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "always_number".to_string(),
            args: vec![],
            body: vec![AstNode::If {
                test: Box::new(AstNode::Identifier {
                    name: "condition".to_string(),
                    line: 2,
                    col: 8,
                    end_line: 2,
                    end_col: 17,
                }),
                body: vec![AstNode::Return {
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(1),
                        line: 3,
                        end_line: 3,
                        col: 16,
                        end_col: 16,
                    })),
                    line: 3,
                    end_line: 3,
                    col: 9,
                    end_col: 9,
                }],
                elif_parts: vec![],
                else_body: Some(vec![AstNode::Return {
                    value: Some(Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(2),
                        line: 5,
                        end_line: 5,
                        col: 16,
                        end_col: 16,
                    })),
                    line: 5,
                    end_line: 5,
                    col: 9,
                    end_col: 9,
                }]),
                line: 2,
                end_line: 2,
                col: 5,
                end_col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = fn_ty {
            assert!(params.is_empty(), "Should have no parameters");
            match ret_ty.as_ref() {
                Type::Var(_) => {}
                Type::Union(_) => {
                    panic!("Should not infer Optional when all paths return values");
                }
                _ => {}
            }
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_function_only_implicit_none_not_optional() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let func_def = AstNode::FunctionDef {
            name: "side_effect".to_string(),
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
                    value: LiteralValue::Integer(42),
                    line: 2,
                    col: 9,
                    end_line: 2,
                    end_col: 9,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        let fn_ty = visit_node_with_env(&func_def, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = fn_ty {
            assert!(params.is_empty(), "Should have no parameters");
            assert!(
                matches!(ret_ty.as_ref(), Type::Con(TypeCtor::NoneType)),
                "Should infer None type for functions with only implicit returns, got {ret_ty:?}"
            );
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_main_guard_with_implicit_none_return() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let main_func = AstNode::FunctionDef {
            name: "main".to_string(),
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
                    value: LiteralValue::Integer(42),
                    line: 2,
                    col: 9,
                    end_line: 2,
                    end_col: 11,
                }),
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 6,
            }],
            docstring: None,
            return_type: None,
            decorators: vec![],
            is_async: false,
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };
        let main_ty = visit_node_with_env(&main_func, &mut env, &mut ctx, None).unwrap();

        if let Type::Fun(params, ret_ty) = &main_ty {
            assert!(params.is_empty());
            assert!(
                matches!(ret_ty.as_ref(), Type::Con(TypeCtor::NoneType)),
                "main should be inferred as returning None, got {ret_ty:?}"
            );
        } else {
            panic!("Expected function type for main");
        }

        let if_main = AstNode::If {
            test: Box::new(AstNode::Compare {
                left: Box::new(AstNode::Identifier {
                    name: "__name__".to_string(),
                    line: 5,
                    col: 4,
                    end_line: 5,
                    end_col: 12,
                }),
                ops: vec![beacon_parser::CompareOperator::Eq],
                comparators: vec![AstNode::Literal {
                    value: LiteralValue::String { value: "__main__".to_string(), prefix: String::new() },
                    line: 5,
                    col: 15,
                    end_line: 5,
                    end_col: 15,
                }],
                line: 5,
                end_line: 5,
                col: 4,
                end_col: 4,
            }),
            body: vec![AstNode::Call {
                function: Box::new(AstNode::Identifier {
                    name: "main".to_string(),
                    line: 6,
                    col: 5,
                    end_line: 6,
                    end_col: 5,
                }),
                args: vec![],
                keywords: vec![],
                line: 6,
                col: 5,
                end_line: 6,
                end_col: 5,
            }],
            elif_parts: vec![],
            else_body: None,
            line: 5,
            col: 1,
            end_line: 5,
            end_col: 1,
        };

        visit_node_with_env(&if_main, &mut env, &mut ctx, None).unwrap();

        let has_calculator_none_constraint = ctx.constraints.iter().any(|c| {
            matches!(
                c,
                Constraint::Equal(Type::Con(TypeCtor::Class(name)), Type::Con(TypeCtor::NoneType), _)
                    | Constraint::Equal(Type::Con(TypeCtor::NoneType), Type::Con(TypeCtor::Class(name)), _)
                if name.contains("Calculator") || name.contains("Processor")
            )
        });

        assert!(
            !has_calculator_none_constraint,
            "Should NOT generate Equal constraints between class types and None for main() call in void context"
        );

        let has_call_to_main = ctx.constraints.iter().any(|c| {
            if let Constraint::Call(func_ty, args, _, _, _) = c {
                args.is_empty() && matches!(func_ty, Type::Fun(_, _))
            } else {
                false
            }
        });

        assert!(
            has_call_to_main,
            "Should still generate Call constraint for main() to verify it's called correctly"
        );
    }

    #[test]
    fn test_list_literal_empty() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let list_node = AstNode::List { elements: vec![], line: 1, col: 5, end_line: 1, end_col: 7 };
        let list_ty = visit_node_with_env(&list_node, &mut env, &mut ctx, None).unwrap();

        assert!(
            matches!(&list_ty, Type::App(inner, _) if matches!(**inner, Type::Con(TypeCtor::List))),
            "Empty list should create list type, got {list_ty:?}"
        );
    }

    #[test]
    fn test_list_literal_with_elements() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let list_node = AstNode::List {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 6, end_line: 1, end_col: 6 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 9, end_line: 1, end_col: 9 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 12, end_line: 1, end_col: 12 },
            ],
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };

        let list_ty = visit_node_with_env(&list_node, &mut env, &mut ctx, None).unwrap();
        assert!(
            matches!(&list_ty, Type::App(inner, _) if matches!(**inner, Type::Con(TypeCtor::List))),
            "List literal should create list type, got {list_ty:?}"
        );

        let equal_constraints = ctx
            .constraints
            .iter()
            .filter(|c| matches!(c, Constraint::Equal(_, _, _)))
            .count();
        assert!(
            equal_constraints >= 3,
            "Should have equality constraints for unifying element types"
        );
    }

    #[test]
    fn test_dict_literal_empty() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let dict_node = AstNode::Dict { keys: vec![], values: vec![], line: 1, col: 5, end_line: 1, end_col: 5 };
        let dict_ty = visit_node_with_env(&dict_node, &mut env, &mut ctx, None).unwrap();

        assert!(
            matches!(&dict_ty, Type::App(inner, _) if matches!(inner.as_ref(), Type::App(dict_inner, _) if matches!(dict_inner.as_ref(), Type::Con(TypeCtor::Dict)))),
            "Empty dict should create dict type, got {dict_ty:?}"
        );
    }

    #[test]
    fn test_dict_literal_with_pairs() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let dict_node = AstNode::Dict {
            keys: vec![
                AstNode::Literal {
                    value: LiteralValue::String { value: "a".to_string(), prefix: "".to_string() },
                    line: 1,
                    col: 6,
                    end_line: 1,
                    end_col: 6,
                },
                AstNode::Literal {
                    value: LiteralValue::String { value: "b".to_string(), prefix: "".to_string() },
                    line: 1,
                    col: 15,
                    end_line: 1,
                    end_col: 15,
                },
            ],
            values: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 11, end_line: 1, end_col: 11 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 20, end_line: 1, end_col: 20 },
            ],
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };
        let dict_ty = visit_node_with_env(&dict_node, &mut env, &mut ctx, None).unwrap();

        assert!(
            matches!(&dict_ty, Type::App(inner, _) if matches!(inner.as_ref(), Type::App(dict_inner, _) if matches!(dict_inner.as_ref(), Type::Con(TypeCtor::Dict)))),
            "Dict literal should create dict type, got {dict_ty:?}"
        );

        let equal_constraints = ctx
            .constraints
            .iter()
            .filter(|c| matches!(c, Constraint::Equal(_, _, _)))
            .count();
        assert!(
            equal_constraints >= 4,
            "Should have equality constraints for unifying key and value types"
        );
    }

    #[test]
    fn test_set_literal() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let set_node = AstNode::Set {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 6, end_line: 1, end_col: 6 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 9, end_line: 1, end_col: 9 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 12, end_line: 1, end_col: 12 },
            ],
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };
        let set_ty = visit_node_with_env(&set_node, &mut env, &mut ctx, None).unwrap();

        assert!(
            matches!(&set_ty, Type::App(inner, _) if matches!(**inner, Type::Con(TypeCtor::Set))),
            "Set literal should create set type, got {set_ty:?}",
        );

        let equal_constraints = ctx
            .constraints
            .iter()
            .filter(|c| matches!(c, Constraint::Equal(_, _, _)))
            .count();
        assert!(
            equal_constraints >= 3,
            "Should have equality constraints for unifying element types"
        );
    }

    #[test]
    fn test_tuple_literal() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let tuple_node = AstNode::Tuple {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 6, end_line: 1, end_col: 6 },
                AstNode::Literal {
                    value: LiteralValue::String { value: "a".to_string(), prefix: "".to_string() },
                    line: 1,
                    col: 9,
                    end_line: 1,
                    end_col: 9,
                },
                AstNode::Literal { value: LiteralValue::Boolean(true), line: 1, col: 14, end_line: 1, end_col: 14 },
            ],
            is_parenthesized: true,
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };
        let tuple_ty = visit_node_with_env(&tuple_node, &mut env, &mut ctx, None).unwrap();

        match &tuple_ty {
            Type::Tuple(types) => {
                assert_eq!(types.len(), 3, "Tuple should have 3 elements");
                assert!(
                    matches!(types[0], Type::Con(TypeCtor::Int)),
                    "First element should be int, got {:?}",
                    types[0]
                );
                assert!(
                    matches!(types[1], Type::Con(TypeCtor::String)),
                    "Second element should be str, got {:?}",
                    types[1]
                );
                assert!(
                    matches!(types[2], Type::Con(TypeCtor::Bool)),
                    "Third element should be bool, got {:?}",
                    types[2]
                );
            }
            _ => panic!("Expected heterogeneous tuple type, got {tuple_ty:?}"),
        }
    }

    #[test]
    fn test_subscript_generates_call_constraint() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let list_node = AstNode::List {
            elements: vec![
                AstNode::Literal { value: LiteralValue::Integer(1), line: 1, col: 6, end_line: 1, end_col: 6 },
                AstNode::Literal { value: LiteralValue::Integer(2), line: 1, col: 9, end_line: 1, end_col: 9 },
                AstNode::Literal { value: LiteralValue::Integer(3), line: 1, col: 12, end_line: 1, end_col: 12 },
            ],
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };

        let subscript_node = AstNode::Subscript {
            value: Box::new(list_node),
            slice: Box::new(AstNode::Literal {
                value: LiteralValue::Integer(0),
                line: 1,
                col: 16,
                end_line: 1,
                end_col: 16,
            }),
            line: 1,
            col: 5,
            end_line: 1,
            end_col: 5,
        };

        let _result_ty = visit_node_with_env(&subscript_node, &mut env, &mut ctx, None).unwrap();

        let has_attr_count = ctx
            .constraints
            .iter()
            .filter(|c| matches!(c, Constraint::HasAttr(_, name, _, _) if name == "__getitem__"))
            .count();
        assert_eq!(
            has_attr_count, 1,
            "Should generate one HasAttr constraint for __getitem__"
        );

        let call_count = ctx
            .constraints
            .iter()
            .filter(|c| matches!(c, Constraint::Call(_, args, _, _, _) if args.len() == 1))
            .count();
        assert!(call_count >= 1, "Should generate Call constraint to invoke __getitem__");
    }

    #[test]
    fn test_subscript_result_type() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let assignment = AstNode::Assignment {
            target: Box::new(AstNode::Identifier { name: "x".to_string(), line: 1, col: 1, end_line: 1, end_col: 2 }),
            value: Box::new(AstNode::Subscript {
                value: Box::new(AstNode::List {
                    elements: vec![AstNode::Literal {
                        value: LiteralValue::Integer(1),
                        line: 1,
                        col: 10,
                        end_line: 1,
                        end_col: 10,
                    }],
                    line: 1,
                    col: 9,
                    end_line: 1,
                    end_col: 9,
                }),
                slice: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(0),
                    line: 1,
                    col: 12,
                    end_line: 1,
                    end_col: 12,
                }),
                line: 1,
                col: 9,
                end_line: 1,
                end_col: 9,
            }),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 1,
        };

        visit_node_with_env(&assignment, &mut env, &mut ctx, None).unwrap();

        let has_bound_method = ctx.type_map.values().any(|t| matches!(t, Type::BoundMethod(_, _, _)));
        assert!(!has_bound_method, "Subscript result should not be a BoundMethod type");
    }

    #[test]
    fn test_class_with_mixed_annotated_and_non_annotated() {
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let class_body = vec![
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "typed_field".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 12,
                }),
                type_annotation: "int".to_string(),
                value: None,
                line: 2,
                col: 5,
                end_line: 2,
                end_col: 5,
            },
            AstNode::Assignment {
                target: Box::new(AstNode::Identifier {
                    name: "untyped_field".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 14,
                }),
                value: Box::new(AstNode::Literal {
                    value: LiteralValue::Integer(42),
                    line: 3,
                    col: 23,
                    end_line: 3,
                    end_col: 25,
                }),
                line: 3,
                col: 5,
                end_line: 3,
                end_col: 5,
            },
            AstNode::AnnotatedAssignment {
                target: Box::new(AstNode::Identifier {
                    name: "another_typed".to_string(),
                    line: 1,
                    col: 1,
                    end_line: 1,
                    end_col: 14,
                }),
                type_annotation: "str".to_string(),
                value: Some(Box::new(AstNode::Literal {
                    value: LiteralValue::String { value: "test".to_string(), prefix: "".to_string() },
                    line: 4,
                    end_line: 4,
                    col: 25,
                    end_col: 25,
                })),
                line: 4,
                col: 5,
                end_line: 4,
                end_col: 5,
            },
        ];

        let metadata = class::extract_class_metadata("MixedClass", &class_body, &mut env);

        assert!(
            metadata.fields.contains_key("typed_field"),
            "Should extract typed_field"
        );
        assert!(
            metadata.fields.contains_key("untyped_field"),
            "Should extract untyped_field"
        );
        assert!(
            metadata.fields.contains_key("another_typed"),
            "Should extract another_typed"
        );

        if let Some(typed_field_type) = metadata.fields.get("typed_field") {
            assert!(
                matches!(typed_field_type, Type::Con(TypeCtor::Int)),
                "typed_field should have type int"
            );
        }

        if let Some(untyped_field_type) = metadata.fields.get("untyped_field") {
            assert!(
                matches!(untyped_field_type, Type::Var(_)),
                "untyped_field should have fresh type variable"
            );
        }

        if let Some(another_typed_type) = metadata.fields.get("another_typed") {
            assert!(
                matches!(another_typed_type, Type::Con(TypeCtor::String)),
                "another_typed should have type str"
            );
        }
    }

    #[test]
    fn test_docstring_filtering() {
        let mut ctx = beacon_constraint::ConstraintGenContext::new();
        let symbol_table = SymbolTable::new();
        let mut env = super::super::type_env::TypeEnvironment::from_symbol_table(
            &symbol_table,
            &AstNode::Module { body: vec![], docstring: None },
        );

        let module_with_docstring = AstNode::Module {
            body: vec![
                AstNode::Literal {
                    value: LiteralValue::String { value: "Module docstring".to_string(), prefix: "".to_string() },
                    line: 1,
                    end_line: 1,
                    col: 1,
                    end_col: 1,
                },
                AstNode::Assignment {
                    target: Box::new(AstNode::Identifier {
                        name: "x".to_string(),
                        line: 1,
                        col: 1,
                        end_line: 1,
                        end_col: 2,
                    }),
                    value: Box::new(AstNode::Literal {
                        value: LiteralValue::Integer(42),
                        line: 2,
                        col: 5,
                        end_line: 2,
                        end_col: 7,
                    }),
                    line: 2,
                    end_line: 2,
                    col: 1,
                    end_col: 1,
                },
            ],
            docstring: Some("Module docstring".to_string()),
        };

        visit_node_with_env(&module_with_docstring, &mut env, &mut ctx, None).unwrap();

        let string_type_count = ctx
            .type_map
            .values()
            .filter(|t| matches!(t, Type::Con(TypeCtor::String)))
            .count();

        assert_eq!(
            string_type_count, 0,
            "Docstring should not generate string type in type_map"
        );

        let has_string_constraints = ctx.constraints.iter().any(|c| match c {
            beacon_constraint::Constraint::Equal(t1, t2, _) => {
                matches!(t1, Type::Con(TypeCtor::String)) || matches!(t2, Type::Con(TypeCtor::String))
            }
            _ => false,
        });

        assert!(
            !has_string_constraints,
            "Docstring should not generate constraints involving string type"
        );
    }
}
