use crate::TypeVar;
use std::path::PathBuf;
use url::Url;

/// Errors that can occur during type operations
#[derive(thiserror::Error, Debug, Clone, PartialEq)]
pub enum TypeError {
    #[error("Cannot unify types: {0} ~ {1}")]
    UnificationError(String, String),

    #[error("Occurs check failed: type variable {0} occurs in {1}")]
    OccursCheckFailed(TypeVar, String),

    #[error("Undefined type variable: {0}")]
    UndefinedTypeVar(TypeVar),

    #[error("Kind mismatch: expected {expected}, found {found}")]
    KindMismatch { expected: String, found: String },

    #[error("Infinite type: {0}")]
    InfiniteType(String),
}

/// Analysis errors
#[derive(Debug, thiserror::Error)]
pub enum AnalysisError {
    #[error("Document not found: {0:?}")]
    DocumentNotFound(Url),

    #[error("Missing AST for document")]
    MissingAst,

    #[error("Missing symbol table for document")]
    MissingSymbolTable,

    #[error("Constraint generation failed: {0}")]
    ConstraintGeneration(String),

    #[error("Type inference failed: {0}")]
    InferenceFailed(String),

    #[error("Unification error: {0}")]
    UnificationError(String),
}

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("Failed to parse Python code: {0}")]
    TreeSitterError(String),

    #[error("Invalid UTF-8 in source code")]
    InvalidUtf8,
}

#[derive(Debug, thiserror::Error)]
pub enum ResolveError {
    #[error("Symbol '{0}' not found")]
    SymbolNotFound(String),
    #[error("Scope error: {0}")]
    ScopeError(String),
}

/// Errors that can occur during document operations
#[derive(Debug, thiserror::Error)]
pub enum DocumentError {
    #[error("Document not found: {0:?}")]
    DocumentNotFound(Url),

    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Invalid document state: {0}")]
    InvalidState(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("Invalid stub path: {0}")]
    InvalidStubPath(PathBuf),

    #[error("Invalid decorator stub syntax: {0}")]
    InvalidDecoratorStub(String),

    #[error("Unsupported Python version: {0}")]
    UnsupportedPythonVersion(String),
}

#[derive(Debug, thiserror::Error)]
pub enum BeaconError {
    #[error(transparent)]
    ConfigError(ConfigError),
    #[error(transparent)]
    ResolveError(ResolveError),
    #[error(transparent)]
    ParseError(ParseError),
    #[error(transparent)]
    AnalysisError(AnalysisError),
    #[error(transparent)]
    TypeError(TypeError),
    #[error(transparent)]
    DocumentError(DocumentError),
}

impl From<TypeError> for BeaconError {
    fn from(value: TypeError) -> Self {
        Self::TypeError(value)
    }
}

impl From<ResolveError> for BeaconError {
    fn from(value: ResolveError) -> Self {
        Self::ResolveError(value)
    }
}

impl From<ParseError> for BeaconError {
    fn from(value: ParseError) -> Self {
        Self::ParseError(value)
    }
}

impl From<AnalysisError> for BeaconError {
    fn from(value: AnalysisError) -> Self {
        Self::AnalysisError(value)
    }
}

impl From<DocumentError> for BeaconError {
    fn from(value: DocumentError) -> Self {
        Self::DocumentError(value)
    }
}

impl From<ConfigError> for BeaconError {
    fn from(value: ConfigError) -> Self {
        Self::ConfigError(value)
    }
}

pub type Result<T> = std::result::Result<T, BeaconError>;
