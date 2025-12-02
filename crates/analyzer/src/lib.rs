//! Static analysis library for Python code
//!
//! This crate provides standalone static analysis capabilities including:
//! - Control Flow Graph (CFG) construction
//! - Data flow analysis (use-before-def, unreachable code)
//! - Static linting (PyFlakes-style rules)
//! - Pattern matching exhaustiveness
//! - Type inference via constraint generation

mod embedded_stdlib_modules;

pub mod cfg;
pub mod const_eval;
pub mod data_flow;
pub mod embedded_stubs;
pub mod linter;
pub mod loader;
pub mod pattern;
pub mod rules;
pub mod type_env;
pub mod walker;

pub use cfg::{
    BasicBlock, BlockId, CallGraph, CallResolver, CallSite, CfgBuilder, ControlFlowGraph, EdgeKind, FunctionId,
    ModuleCFG, ModuleCFGBuilder, WorkspaceCFG,
};
pub use const_eval::{ConstValue, evaluate_const_expr};
pub use data_flow::{DataFlowAnalyzer, DataFlowResult, UnreachableCode, UnusedVariable, UseBeforeDef};
pub use embedded_stdlib_modules::EMBEDDED_STDLIB_MODULES;
pub use embedded_stubs::{TypeshedVersion, available_stubs, get_embedded_stub, version_info};
pub use linter::Linter;
pub use loader::{
    StubCache, StubFile, StubTypeContext, new_class_registry_with_stdlib, new_typevar_registry_with_stdlib,
};
pub use rules::{DiagnosticMessage, Rule, RuleKind, RuleSeverity};
pub use type_env::TypeEnvironment;
pub use walker::generate_constraints;
