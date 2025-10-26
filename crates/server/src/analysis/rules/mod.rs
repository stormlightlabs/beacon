//! Internal implementation of [PyFlakes](https://pypi.org/project/pyflakes/)
//!
//! See https://github.com/PyCQA/pyflakes/blob/main/pyflakes/messages.py
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum RuleKind {
    UnusedImport,
    UndefinedName,
    UnusedVariable,
    DuplicateArgument,
    RedefinedWhileUnused,
    ImportStarNotPermitted,
    ImportStarUsed,
    ReturnOutsideFunction,
    YieldOutsideFunction,
    BreakOutsideLoop,
    ContinueOutsideLoop,
    DefaultExceptNotLast,
    RaiseNotImplemented,
    FStringMissingPlaceholders,
    PercentFormatInvalidFormat,
    Unknown,
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
}

/// Conversion to LSP diagnostic
impl From<&DiagnosticMessage> for Diagnostic {
    fn from(msg: &DiagnosticMessage) -> Self {
        Diagnostic {
            range: Range {
                start: Position::new(msg.line.saturating_sub(1) as u32, msg.col as u32),
                end: Position::new(msg.line.saturating_sub(1) as u32, msg.col as u32 + 1),
            },
            severity: Some(match msg.rule {
                RuleKind::UndefinedName | RuleKind::DuplicateArgument | RuleKind::ReturnOutsideFunction => {
                    DiagnosticSeverity::ERROR
                }
                _ => DiagnosticSeverity::WARNING,
            }),
            message: msg.message.clone(),
            source: Some("python-lsp-rule-engine".into()),
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
                kind: RuleKind::UnusedImport,
                name: "Unused Import",
                description: "Detect imports that are never used",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
            Rule {
                kind: RuleKind::UndefinedName,
                name: "Undefined Name",
                description: "Detect variables or functions used before definition",
                default_severity: RuleSeverity::Error,
                enabled: true,
            },
            Rule {
                kind: RuleKind::UnusedVariable,
                name: "Unused Variable",
                description: "Detect local variables that are assigned but never used",
                default_severity: RuleSeverity::Warning,
                enabled: true,
            },
        ];
        Self { rules }
    }

    pub fn report(&self, msg: DiagnosticMessage) -> Diagnostic {
        (&msg).into()
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
        }
    }

    #[test]
    fn test_rule_engine_initialization() {
        let engine = RuleEngine::new();
        assert!(!engine.rules.is_empty(), "RuleEngine should register rules");
        assert!(engine.rules.iter().any(|r| r.name == "Unused Import"));
        assert!(engine.rules.iter().any(|r| r.kind == RuleKind::UndefinedName));
    }

    #[test]
    fn test_diagnostic_message_to_lsp_conversion() {
        let msg = sample_message();
        let diagnostic: lsp_types::Diagnostic = (&msg).into();

        assert_eq!(diagnostic.message, "'os' imported but unused");
        assert_eq!(diagnostic.source.as_deref(), Some("python-lsp-rule-engine"));
        assert_eq!(
            diagnostic.range,
            Range { start: Position::new(2, 5), end: Position::new(2, 6) }
        );
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::WARNING));
    }

    #[test]
    fn test_rule_engine_report() {
        let engine = RuleEngine::new();
        let msg = sample_message();
        let diag = engine.report(msg.clone());

        assert_eq!(diag.message, msg.message);
        assert_eq!(diag.source.as_deref(), Some("python-lsp-rule-engine"));
    }

    #[test]
    fn test_severity_mapping_for_error_rule() {
        let msg = DiagnosticMessage {
            rule: RuleKind::UndefinedName,
            message: "undefined name 'foo'".to_string(),
            filename: "example.py".into(),
            line: 10,
            col: 2,
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
