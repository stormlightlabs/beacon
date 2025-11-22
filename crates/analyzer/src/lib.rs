//! Static analysis library for Python code
//!
//! This crate provides standalone static analysis capabilities including:
//! - Control Flow Graph (CFG) construction
//! - Data flow analysis (use-before-def, unreachable code)
//! - Static linting (PyFlakes-style rules)
//! - Pattern matching exhaustiveness
//! - Type inference via constraint generation

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

pub use cfg::{BasicBlock, BlockId, CfgBuilder, ControlFlowGraph, EdgeKind};
pub use const_eval::{ConstValue, evaluate_const_expr};
pub use data_flow::{DataFlowAnalyzer, DataFlowResult, UnreachableCode, UnusedVariable, UseBeforeDef};
pub use embedded_stubs::{TypeshedVersion, available_stubs, get_embedded_stub, version_info};
pub use linter::Linter;
pub use loader::{StubCache, StubFile, StubTypeContext};
pub use rules::{DiagnosticMessage, Rule, RuleKind, RuleSeverity};
pub use type_env::TypeEnvironment;
pub use walker::generate_constraints;
