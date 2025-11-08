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

    #[error("Type {0} does not satisfy protocol {1}")]
    ProtocolNotSatisfied(String, String),

    #[error("Attribute '{1}' not found on type {0}")]
    AttributeNotFound(String, String),

    #[error("Argument count mismatch: expected {expected}, got {found}")]
    ArgumentCountMismatch { expected: usize, found: usize },

    #[error("Argument of type '{found}' cannot be assigned to parameter '{param_name}' of type '{expected}'")]
    ArgumentTypeMismatch {
        param_name: String,
        expected: String,
        found: String,
    },

    #[error("Match pattern is not exhaustive. Missing coverage for: {0}")]
    PatternNonExhaustive(String),

    #[error("Pattern is unreachable (subsumed by earlier pattern)")]
    PatternUnreachable,

    #[error("Pattern type mismatch: pattern type '{pattern_type}' cannot match subject of type '{subject_type}'")]
    PatternTypeMismatch { pattern_type: String, subject_type: String },

    #[error("Invalid pattern structure: expected {expected}, found {found}")]
    PatternStructureMismatch { expected: String, found: String },

    #[error("Keyword argument error: {0}")]
    KeywordArgumentError(String),

    #[error("Error: {0}")]
    Other(String),
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

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("Failed to parse Python code: {0}")]
    TreeSitterError(String),

    #[error("Invalid UTF-8 in source code")]
    InvalidUtf8,

    #[error("Missing node {0}")]
    MissingNode(String),
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

    #[error("Failed to parse TOML: {0}")]
    TOMLError(#[from] toml::de::Error),

    #[error("IO Error: {0}")]
    IOError(#[from] std::io::Error),
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::io;

    #[test]
    fn type_error_formats_are_human_readable() {
        let err = TypeError::UnificationError("int".into(), "str".into());
        assert_eq!(err.to_string(), "Cannot unify types: int ~ str");

        let occurs = TypeError::OccursCheckFailed(TypeVar::new(42), "List['t42]".into());
        assert!(
            occurs
                .to_string()
                .contains("Occurs check failed: type variable 't42 occurs in List['t42]")
        );

        let infinite = TypeError::InfiniteType("τ = List[τ]".into());
        assert_eq!(infinite.to_string(), "Infinite type: τ = List[τ]");
    }

    #[test]
    fn analysis_error_variants_round_trip_into_beacon_error() {
        let not_found = AnalysisError::DocumentNotFound(Url::parse("file:///tmp/test.py").expect("valid uri for test"));
        let beacon: BeaconError = not_found.into();
        matches!(beacon, BeaconError::AnalysisError(AnalysisError::DocumentNotFound(_)))
            .then_some(())
            .expect("conversion should preserve variant");

        let inference = BeaconError::from(AnalysisError::InferenceFailed("boom".into()));
        assert!(matches!(
            inference,
            BeaconError::AnalysisError(AnalysisError::InferenceFailed(message)) if message == "boom"
        ));
    }

    #[test]
    fn document_error_supports_io_conversion() {
        let io_err = io::Error::other("disk failure");
        let doc_err: DocumentError = io_err.into();
        let beacon_err: BeaconError = doc_err.into();
        assert!(matches!(beacon_err, BeaconError::DocumentError(_)));
        assert!(beacon_err.to_string().contains("disk failure"));
    }

    #[test]
    fn config_error_variants_are_descriptive() {
        let path_error = ConfigError::InvalidStubPath(PathBuf::from("/missing"));
        assert!(path_error.to_string().contains("/missing"));

        let decorator = ConfigError::InvalidDecoratorStub("bad syntax".into());
        assert!(decorator.to_string().contains("bad syntax"));

        let beacon = BeaconError::from(path_error);
        assert!(matches!(beacon, BeaconError::ConfigError(_)));
    }

    #[test]
    fn result_type_alias_handles_ok_and_err() {
        fn fallible(ok: bool) -> Result<&'static str> {
            if ok { Ok("success") } else { Err(TypeError::UndefinedTypeVar(TypeVar::new(0)).into()) }
        }

        assert_eq!(fallible(true).unwrap(), "success");
        let err = fallible(false).unwrap_err();
        assert!(matches!(err, BeaconError::TypeError(TypeError::UndefinedTypeVar(tv)) if tv.id == 0));
    }
}
