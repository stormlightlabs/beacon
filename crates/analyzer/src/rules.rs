//! Internal implementation of [PyFlakes](https://pypi.org/project/pyflakes/)
//!
//! See https://github.com/PyCQA/pyflakes/blob/main/pyflakes/messages.py
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum RuleKind {
    /// BEA001
    UndefinedName,
    /// BEA002
    DuplicateArgument,
    /// BEA003
    ReturnOutsideFunction,
    /// BEA004
    YieldOutsideFunction,
    /// BEA005
    BreakOutsideLoop,
    /// BEA006
    ContinueOutsideLoop,
    /// BEA007 & BEA027
    DefaultExceptNotLast,
    /// BEA008
    RaiseNotImplemented,
    /// BEA009
    TwoStarredExpressions,
    /// BEA010
    TooManyExpressionsInStarredAssignment,
    /// BEA011
    IfTuple,
    /// BEA012
    AssertTuple,
    /// BEA013
    FStringMissingPlaceholders,
    /// BEA014
    TStringMissingPlaceholders,
    /// BEA015
    UnusedImport,
    /// BEA016
    UnusedVariable,
    /// BEA017
    UnusedAnnotation,
    /// BEA018
    RedefinedWhileUnused,
    /// BEA019
    ImportShadowedByLoopVar,
    //// BEA020
    ImportStarNotPermitted,
    /// BEA021
    ImportStarUsed,
    /// BEA022
    UnusedIndirectAssignment,
    /// BEA023
    ForwardAnnotationSyntaxError,
    /// BEA024
    MultiValueRepeatedKeyLiteral,
    /// BEA025
    PercentFormatInvalidFormat,
    /// BEA026
    IsLiteral,
    /// BEA027
    UseBeforeDef,
    /// BEA028
    UnreachableCode,
    /// BEA029
    RedundantPass,
    /// BEA030
    EmptyExcept,
    /// BEA031
    InconsistentExport,
    /// BEA032
    ConflictingStubDefinition,
    Unknown,
}

impl RuleKind {
    /// Returns the rule code for this rule kind
    pub fn code(&self) -> &'static str {
        match self {
            RuleKind::UndefinedName => "BEA001",
            RuleKind::DuplicateArgument => "BEA002",
            RuleKind::ReturnOutsideFunction => "BEA003",
            RuleKind::YieldOutsideFunction => "BEA004",
            RuleKind::BreakOutsideLoop => "BEA005",
            RuleKind::ContinueOutsideLoop => "BEA006",
            RuleKind::DefaultExceptNotLast => "BEA007",
            RuleKind::RaiseNotImplemented => "BEA008",
            RuleKind::TwoStarredExpressions => "BEA009",
            RuleKind::TooManyExpressionsInStarredAssignment => "BEA010",
            RuleKind::IfTuple => "BEA011",
            RuleKind::AssertTuple => "BEA012",
            RuleKind::FStringMissingPlaceholders => "BEA013",
            RuleKind::TStringMissingPlaceholders => "BEA014",
            RuleKind::UnusedImport => "BEA015",
            RuleKind::UnusedVariable => "BEA016",
            RuleKind::UnusedAnnotation => "BEA017",
            RuleKind::RedefinedWhileUnused => "BEA018",
            RuleKind::ImportShadowedByLoopVar => "BEA019",
            RuleKind::ImportStarNotPermitted => "BEA020",
            RuleKind::ImportStarUsed => "BEA021",
            RuleKind::UnusedIndirectAssignment => "BEA022",
            RuleKind::ForwardAnnotationSyntaxError => "BEA023",
            RuleKind::MultiValueRepeatedKeyLiteral => "BEA024",
            RuleKind::PercentFormatInvalidFormat => "BEA025",
            RuleKind::IsLiteral => "BEA026",
            RuleKind::UseBeforeDef => "BEA027",
            RuleKind::UnreachableCode => "BEA028",
            RuleKind::RedundantPass => "BEA029",
            RuleKind::EmptyExcept => "BEA030",
            RuleKind::InconsistentExport => "BEA031",
            RuleKind::ConflictingStubDefinition => "BEA032",
            RuleKind::Unknown => "BEA000",
        }
    }
}

/// Rule severity
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RuleSeverity {
    Error,
    Warning,
    Info,
}

/// Rule definition metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rule {
    pub kind: RuleKind,
    pub name: &'static str,
    pub description: &'static str,
    pub default_severity: RuleSeverity,
    pub enabled: bool,
}

/// Concrete message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticMessage {
    pub rule: RuleKind,
    pub message: String,
    pub filename: String,
    pub line: usize,
    pub col: usize,
    pub end_col: usize,
}

/// Conversion to LSP diagnostic
impl From<&DiagnosticMessage> for Diagnostic {
    fn from(msg: &DiagnosticMessage) -> Self {
        Diagnostic {
            range: Range {
                start: Position::new(msg.line.saturating_sub(1) as u32, msg.col.saturating_sub(1) as u32),
                end: Position::new(msg.line.saturating_sub(1) as u32, msg.end_col.saturating_sub(1) as u32),
            },
            severity: Some(match msg.rule {
                RuleKind::UndefinedName
                | RuleKind::DuplicateArgument
                | RuleKind::ReturnOutsideFunction
                | RuleKind::YieldOutsideFunction
                | RuleKind::BreakOutsideLoop
                | RuleKind::ContinueOutsideLoop
                | RuleKind::TwoStarredExpressions
                | RuleKind::TooManyExpressionsInStarredAssignment
                | RuleKind::ImportStarNotPermitted
                | RuleKind::ForwardAnnotationSyntaxError => DiagnosticSeverity::ERROR,
                RuleKind::RedundantPass => DiagnosticSeverity::HINT,
                _ => DiagnosticSeverity::WARNING,
            }),
            message: msg.message.clone(),
            source: Some("beacon-linter".into()),
            ..Default::default()
        }
    }
}

/// [Rule] registry
#[derive(Default)]
pub struct RuleEngine {
    pub rules: Vec<Rule>,
}

impl RuleEngine {
    pub fn new() -> Self {
        let rules = vec![
            Rule {
                kind: RuleKind::UndefinedName,
                name: "UndefinedName",
                description: "Variable or function used before being defined",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::DuplicateArgument,
                name: "DuplicateArgument",
                description: "Duplicate parameter names in a function definition",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::ReturnOutsideFunction,
                name: "ReturnOutsideFunction",
                description: "return statement outside of a function or method body",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::YieldOutsideFunction,
                name: "YieldOutsideFunction",
                description: "yield or yield from used outside a function context",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::BreakOutsideLoop,
                name: "BreakOutsideLoop",
                description: "break used outside a for/while loop",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::ContinueOutsideLoop,
                name: "ContinueOutsideLoop",
                description: "continue used outside a for/while loop",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::DefaultExceptNotLast,
                name: "DefaultExceptNotLast",
                description: "A bare except: is not the final exception handler in a try block",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::RaiseNotImplemented,
                name: "RaiseNotImplemented",
                description: "Using raise NotImplemented instead of raise NotImplementedError",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::TwoStarredExpressions,
                name: "TwoStarredExpressions",
                description: "Two or more * unpacking expressions in assignment",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::TooManyExpressionsInStarredAssignment,
                name: "TooManyExpressionsInStarredAssignment",
                description: "Too many expressions when unpacking into a starred target",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::IfTuple,
                name: "IfTuple",
                description: "A tuple literal used as an if condition - always True",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::AssertTuple,
                name: "AssertTuple",
                description: "Assertion always true due to tuple literal",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::FStringMissingPlaceholders,
                name: "FStringMissingPlaceholders",
                description: "f-string declared but contains no {} placeholders",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::TStringMissingPlaceholders,
                name: "TStringMissingPlaceholders",
                description: "t-string declared but contains no placeholders",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::UnusedImport,
                name: "UnusedImport",
                description: "Import is never used within the file",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::UnusedVariable,
                name: "UnusedVariable",
                description: "Local variable assigned but never used",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::UnusedAnnotation,
                name: "UnusedAnnotation",
                description: "Annotated variable never referenced",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::RedefinedWhileUnused,
                name: "RedefinedWhileUnused",
                description: "Variable redefined before original was used",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::ImportShadowedByLoopVar,
                name: "ImportShadowedByLoopVar",
                description: "Import name shadowed by a loop variable",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::ImportStarNotPermitted,
                name: "ImportStarNotPermitted",
                description: "from module import * used inside a function or class",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::ImportStarUsed,
                name: "ImportStarUsed",
                description: "import * prevents detection of undefined names",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::UnusedIndirectAssignment,
                name: "UnusedIndirectAssignment",
                description: "Global or nonlocal declared but never reassigned",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::ForwardAnnotationSyntaxError,
                name: "ForwardAnnotationSyntaxError",
                description: "Syntax error in forward type annotation",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::MultiValueRepeatedKeyLiteral,
                name: "MultiValueRepeatedKeyLiteral",
                description: "Dictionary literal repeats key with different values",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::PercentFormatInvalidFormat,
                name: "PercentFormatInvalidFormat",
                description: "Invalid % format string",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::IsLiteral,
                name: "IsLiteral",
                description: "Comparing constants with is or is not instead of ==/!=",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::UseBeforeDef,
                name: "UseBeforeDef",
                description: "Variable may be used before being defined",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::UnreachableCode,
                name: "UnreachableCode",
                description: "Code after a return, raise, or break is never executed",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::RedundantPass,
                name: "RedundantPass",
                description: "pass used in a block that already has content",
                default_severity: RuleSeverity::Info,
                enabled: true,
            },
            Rule {
                kind: RuleKind::EmptyExcept,
                name: "EmptyExcept",
                description: "except: with no handling code (silent failure)",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::InconsistentExport,
                name: "InconsistentExport",
                description: "__all__ exports symbol that is not defined in module",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::ConflictingStubDefinition,
                name: "ConflictingStubDefinition",
                description: "Conflicting type definitions for the same symbol across stub files",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
        ];
        Self { rules }
    }

    pub fn report(&self, msg: &DiagnosticMessage) -> Diagnostic {
        msg.into()
    }

    pub fn get_rule(&self, kind: &RuleKind) -> Option<&Rule> {
        self.rules.iter().find(|r| r.kind == *kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{DiagnosticSeverity, Position, Range};

    fn sample_message() -> DiagnosticMessage {
        DiagnosticMessage {
            rule: RuleKind::UnusedImport,
            message: "'os' imported but unused".to_string(),
            filename: "main.py".to_string(),
            line: 3,
            col: 5,
            end_col: 7,
        }
    }

    #[test]
    fn test_rule_engine_initialization() {
        let engine = RuleEngine::new();
        assert!(!engine.rules.is_empty(), "RuleEngine should register rules");
        assert!(engine.rules.iter().any(|r| r.name == "UnusedImport"));
        assert!(engine.rules.iter().any(|r| r.kind == RuleKind::UndefinedName));
    }

    #[test]
    fn test_diagnostic_message_to_lsp_conversion() {
        let msg = sample_message();
        let diagnostic: lsp_types::Diagnostic = (&msg).into();

        assert_eq!(diagnostic.message, "'os' imported but unused");
        assert_eq!(diagnostic.source.as_deref(), Some("beacon-linter"));
        assert_eq!(
            diagnostic.range,
            Range { start: Position::new(2, 4), end: Position::new(2, 6) }
        );
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::WARNING));
    }

    #[test]
    fn test_rule_engine_report() {
        let engine = RuleEngine::new();
        let msg = sample_message();
        let diag = engine.report(&msg);

        assert_eq!(diag.message, msg.message);
        assert_eq!(diag.source.as_deref(), Some("beacon-linter"));
    }

    #[test]
    fn test_severity_mapping_for_error_rule() {
        let msg = DiagnosticMessage {
            rule: RuleKind::UndefinedName,
            message: "undefined name 'foo'".to_string(),
            filename: "example.py".into(),
            line: 10,
            col: 2,
            end_col: 5,
        };
        let diagnostic: lsp_types::Diagnostic = (&msg).into();
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    }

    #[test]
    fn test_all_rule_kinds_have_descriptions() {
        let engine = RuleEngine::new();
        for rule in &engine.rules {
            assert!(
                !rule.description.is_empty(),
                "Rule {:?} must have a description",
                rule.kind
            );
        }
    }

    #[test]
    fn test_enable_disable_rules() {
        let mut engine = RuleEngine::new();
        let rule = engine
            .rules
            .iter_mut()
            .find(|r| r.kind == RuleKind::UnusedImport)
            .unwrap();
        rule.enabled = false;
        assert!(!rule.enabled);
    }
}
