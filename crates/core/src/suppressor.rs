//! Suppression support for formatters, linters, and type checkers
//!
//! Implements Python-style suppression comments:
//! - `# noqa` / `# noqa: CODE` - Suppress linter warnings
//! - `# type: ignore` / `# type: ignore[code]` - Suppress type checker errors
//! - `# fmt: off` / `# fmt: on` / `# fmt: skip` - Control formatter behavior

use regex::Regex;
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

/// Types of suppressions that can appear on a line
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Suppression {
    /// `# noqa` or `# noqa: CODE1, CODE2`
    NoQA { codes: Option<Vec<String>> },
    /// `# type: ignore` or `# type: ignore[code1, code2]`
    TypeIgnore { codes: Option<Vec<String>> },
    /// `# beacon: ignore` or `# beacon: ignore[code1, code2]`
    BeaconIgnore { codes: Option<Vec<String>> },
    /// `# fmt: skip` - Skip formatting for this statement
    FmtSkip,
    /// `# fmt: off` - Disable formatting until `# fmt: on`
    FmtOff,
    /// `# fmt: on` - Re-enable formatting after `# fmt: off`
    FmtOn,
}

impl Suppression {
    /// Check if this suppression applies to a specific code
    ///
    /// Returns true if:
    /// - The suppression has no specific codes (suppresses all)
    /// - The suppression explicitly lists the given code
    pub fn applies_to(&self, code: &str) -> bool {
        match self {
            Suppression::NoQA { codes } => match codes {
                None => true,
                Some(codes) => codes.iter().any(|c| c.eq_ignore_ascii_case(code)),
            },
            Suppression::TypeIgnore { codes } => match codes {
                None => true,
                Some(codes) => codes.iter().any(|c| c.eq_ignore_ascii_case(code)),
            },
            Suppression::BeaconIgnore { codes } => match codes {
                None => true,
                Some(codes) => codes.iter().any(|c| c.eq_ignore_ascii_case(code)),
            },
            Suppression::FmtSkip | Suppression::FmtOff | Suppression::FmtOn => false,
        }
    }

    /// Check if this is a formatter-related suppression
    pub fn is_formatter_suppression(&self) -> bool {
        matches!(self, Suppression::FmtSkip | Suppression::FmtOff | Suppression::FmtOn)
    }
}

/// Map of line numbers to suppressions on that line
#[derive(Debug, Clone)]
pub struct SuppressionMap {
    /// Line number (1-indexed) -> list of suppressions on that line
    suppressions: FxHashMap<usize, Vec<Suppression>>,
    /// Ranges where formatting is disabled (start_line, end_line)
    fmt_off_ranges: Vec<(usize, usize)>,
    /// Ranges where diagnostics are disabled (start_line, end_line, codes)
    diagnostic_off_ranges: Vec<(usize, usize, Option<Vec<String>>)>,
}

impl SuppressionMap {
    /// Parse source code and extract all suppression comments
    pub fn from_source(source: &str) -> Self {
        let mut suppressions = FxHashMap::default();
        let mut fmt_off_ranges = Vec::new();
        let mut fmt_off_start: Option<usize> = None;
        let mut diagnostic_off_ranges = Vec::new();
        let mut diagnostic_off_start: Option<(usize, Option<Vec<String>>)> = None;

        for (line_num, line) in source.lines().enumerate() {
            let line_num = line_num + 1;
            let mut line_suppressions = Vec::new();

            if let Some(suppression_list) = Self::parse_line(line) {
                for suppression in suppression_list {
                    match &suppression {
                        Suppression::BeaconIgnore { .. } if line.trim_start().starts_with('#') => {
                            suppressions
                                .entry(line_num + 1)
                                .or_insert_with(Vec::new)
                                .push(suppression.clone());
                        }
                        Suppression::FmtOff => {
                            if fmt_off_start.is_none() {
                                fmt_off_start = Some(line_num);
                            }
                            line_suppressions.push(suppression);
                        }
                        Suppression::FmtOn => {
                            if let Some(start) = fmt_off_start.take() {
                                fmt_off_ranges.push((start, line_num));
                            }
                            line_suppressions.push(suppression);
                        }
                        _ => line_suppressions.push(suppression),
                    }
                }
            }

            if let Some((codes, is_enable)) = Self::parse_beacon_disable(line) {
                if is_enable {
                    if let Some((start, active_codes)) = diagnostic_off_start.take() {
                        diagnostic_off_ranges.push((start, line_num.saturating_sub(1), active_codes));
                    }
                } else if diagnostic_off_start.is_none() {
                    diagnostic_off_start = Some((line_num + 1, codes));
                }
            }

            if !line_suppressions.is_empty() {
                suppressions
                    .entry(line_num)
                    .or_insert_with(Vec::new)
                    .extend(line_suppressions);
            }
        }

        if let Some(start) = fmt_off_start {
            fmt_off_ranges.push((start, usize::MAX));
        }

        if let Some((start, codes)) = diagnostic_off_start {
            diagnostic_off_ranges.push((start, usize::MAX, codes));
        }

        Self { suppressions, fmt_off_ranges, diagnostic_off_ranges }
    }

    fn parse_codes(codes: &str) -> Vec<String> {
        codes
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect()
    }

    fn parse_beacon_disable(line: &str) -> Option<(Option<Vec<String>>, bool)> {
        static BEACON_DISABLE_RE: OnceLock<Regex> = OnceLock::new();
        let beacon_disable_re = BEACON_DISABLE_RE
            .get_or_init(|| Regex::new(r"#\s*beacon:\s*(disable|enable)(?:\s*=\s*([A-Za-z0-9_,\-\s]+))?").unwrap());

        let captures = beacon_disable_re.captures(line)?;
        let directive = captures.get(1)?.as_str();
        let codes = captures.get(2).map(|m| Self::parse_codes(m.as_str()));
        Some((codes, directive == "enable"))
    }

    /// Parse a single line for suppression comments
    fn parse_line(line: &str) -> Option<Vec<Suppression>> {
        static NOQA_RE: OnceLock<Regex> = OnceLock::new();
        static TYPE_IGNORE_RE: OnceLock<Regex> = OnceLock::new();
        static BEACON_IGNORE_RE: OnceLock<Regex> = OnceLock::new();
        static FMT_RE: OnceLock<Regex> = OnceLock::new();

        let noqa_re = NOQA_RE.get_or_init(|| Regex::new(r"#\s*noqa(?::\s*([A-Za-z0-9,\s]+))?").unwrap());
        let type_ignore_re = TYPE_IGNORE_RE.get_or_init(|| Regex::new(r"#\s*type:\s*ignore(?:\[([^\]]+)\])?").unwrap());
        let beacon_ignore_re =
            BEACON_IGNORE_RE.get_or_init(|| Regex::new(r"#\s*beacon:\s*ignore(?:\[([^\]]+)\])?").unwrap());
        let fmt_re =
            FMT_RE.get_or_init(|| Regex::new(r"#\s*(?:(?:beacon\s*:\s*|beacon-))?fmt:\s*(off|on|skip)").unwrap());

        let mut result = Vec::new();

        if let Some(captures) = noqa_re.captures(line) {
            let codes = captures.get(1).map(|m| Self::parse_codes(m.as_str()));
            result.push(Suppression::NoQA { codes });
        }

        if let Some(captures) = type_ignore_re.captures(line) {
            let codes = captures.get(1).map(|m| Self::parse_codes(m.as_str()));
            result.push(Suppression::TypeIgnore { codes });
        }

        if let Some(captures) = beacon_ignore_re.captures(line) {
            let codes = captures.get(1).map(|m| Self::parse_codes(m.as_str()));
            result.push(Suppression::BeaconIgnore { codes });
        }

        if let Some(captures) = fmt_re.captures(line) {
            let directive = captures.get(1).unwrap().as_str();
            let suppression = match directive {
                "off" => Suppression::FmtOff,
                "on" => Suppression::FmtOn,
                "skip" => Suppression::FmtSkip,
                _ => return None,
            };
            result.push(suppression);
        }

        if result.is_empty() { None } else { Some(result) }
    }

    /// Check if a linter diagnostic at the given line should be suppressed.
    pub fn is_lint_suppressed(&self, line: usize, code: Option<&str>) -> bool {
        self.has_line_suppression(line, code, |suppression| {
            matches!(suppression, Suppression::NoQA { .. } | Suppression::BeaconIgnore { .. })
        }) || self.is_disabled_by_beacon_range(line, code)
    }

    /// Check if a checker diagnostic at the given line should be suppressed.
    pub fn is_type_suppressed(&self, line: usize, code: Option<&str>) -> bool {
        self.has_line_suppression(line, code, |suppression| {
            matches!(
                suppression,
                Suppression::TypeIgnore { .. } | Suppression::BeaconIgnore { .. }
            )
        }) || self.is_disabled_by_beacon_range(line, code)
    }

    fn has_line_suppression(&self, line: usize, code: Option<&str>, predicate: impl Fn(&Suppression) -> bool) -> bool {
        let Some(suppressions) = self.suppressions.get(&line) else {
            return false;
        };

        suppressions
            .iter()
            .filter(|suppression| predicate(suppression))
            .any(|suppression| suppression.applies_to_optional(code))
    }

    fn is_disabled_by_beacon_range(&self, line: usize, code: Option<&str>) -> bool {
        self.diagnostic_off_ranges
            .iter()
            .any(|(start, end, codes)| line >= *start && line <= *end && Self::codes_apply(codes.as_deref(), code))
    }

    fn codes_apply(codes: Option<&[String]>, code: Option<&str>) -> bool {
        match (codes, code) {
            (None, _) => true,
            (Some(_), None) => true,
            (Some(codes), Some(code)) => codes.iter().any(|candidate| candidate.eq_ignore_ascii_case(code)),
        }
    }

    /// Check if a diagnostic at the given line should be suppressed.
    ///
    /// For linter diagnostics, pass the rule code (e.g., "BEA001").
    /// For type errors, pass None or a type error code if available.
    pub fn is_suppressed(&self, line: usize, code: Option<&str>) -> bool {
        self.is_lint_suppressed(line, code) || self.is_type_suppressed(line, code)
    }

    /// Check if formatting should be disabled at the given line
    pub fn is_formatting_disabled(&self, line: usize) -> bool {
        if let Some(suppressions) = self.suppressions.get(&line)
            && suppressions.iter().any(|s| matches!(s, Suppression::FmtSkip))
        {
            return true;
        }

        for (start, end) in &self.fmt_off_ranges {
            if line >= *start && line <= *end {
                return true;
            }
        }

        false
    }

    /// Get all suppressions on a specific line
    pub fn get_suppressions(&self, line: usize) -> Option<&[Suppression]> {
        self.suppressions.get(&line).map(|v| v.as_slice())
    }
}

impl Suppression {
    fn applies_to_optional(&self, code: Option<&str>) -> bool {
        match self {
            Suppression::NoQA { codes } | Suppression::TypeIgnore { codes } | Suppression::BeaconIgnore { codes } => {
                SuppressionMap::codes_apply(codes.as_deref(), code)
            }
            Suppression::FmtSkip | Suppression::FmtOff | Suppression::FmtOn => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_suppression_applies_to() {
        let noqa_all = Suppression::NoQA { codes: None };
        assert!(noqa_all.applies_to("BEA001"));
        assert!(noqa_all.applies_to("BEA999"));

        let noqa_specific = Suppression::NoQA { codes: Some(vec!["BEA001".to_string(), "BEA002".to_string()]) };
        assert!(noqa_specific.applies_to("BEA001"));
        assert!(noqa_specific.applies_to("BEA002"));
        assert!(!noqa_specific.applies_to("BEA003"));

        let type_ignore_all = Suppression::TypeIgnore { codes: None };
        assert!(type_ignore_all.applies_to("any-error"));

        let fmt_skip = Suppression::FmtSkip;
        assert!(!fmt_skip.applies_to("BEA001"));
    }

    #[test]
    fn test_parse_noqa() {
        let map = SuppressionMap::from_source("x = 1  # noqa");
        assert!(map.is_suppressed(1, Some("BEA001")));
        assert!(map.is_suppressed(1, Some("BEA999")));

        let map = SuppressionMap::from_source("x = 1  # noqa: BEA001");
        assert!(map.is_suppressed(1, Some("BEA001")));
        assert!(!map.is_suppressed(1, Some("BEA002")));

        let map = SuppressionMap::from_source("x = 1  # noqa: BEA001, BEA002");
        assert!(map.is_suppressed(1, Some("BEA001")));
        assert!(map.is_suppressed(1, Some("BEA002")));
        assert!(!map.is_suppressed(1, Some("BEA003")));
    }

    #[test]
    fn test_parse_type_ignore() {
        let map = SuppressionMap::from_source("x: int = 'str'  # type: ignore");
        assert!(map.is_type_suppressed(1, None));

        let map = SuppressionMap::from_source("x: int = 'str'  # type: ignore[assignment]");
        assert!(map.is_type_suppressed(1, Some("assignment")));
        assert!(!map.is_type_suppressed(1, Some("other-error")));
        assert!(!map.is_lint_suppressed(1, Some("assignment")));
    }

    #[test]
    fn test_parse_beacon_ignore_next_line() {
        let source = "# beacon: ignore[HM007]\nvalue.missing";
        let map = SuppressionMap::from_source(source);
        assert!(map.is_type_suppressed(2, Some("HM007")));
        assert!(map.is_lint_suppressed(2, Some("HM007")));
        assert!(!map.is_type_suppressed(1, Some("HM007")));
    }

    #[test]
    fn test_parse_beacon_disable_enable() {
        let source = "\
# beacon: disable=unused-variable
unused = 1
# beacon: enable=unused-variable
still_unused = 2
";
        let map = SuppressionMap::from_source(source);
        assert!(map.is_type_suppressed(2, Some("unused-variable")));
        assert!(!map.is_type_suppressed(4, Some("unused-variable")));
    }

    #[test]
    fn test_parse_beacon_fmt_directives() {
        let source = "\
# beacon: fmt: off
x=1
# beacon: fmt: on
y=2  # beacon-fmt: skip
";
        let map = SuppressionMap::from_source(source);
        assert!(map.is_formatting_disabled(1));
        assert!(map.is_formatting_disabled(2));
        assert!(map.is_formatting_disabled(3));
        assert!(map.is_formatting_disabled(4));
    }

    #[test]
    fn test_parse_fmt_skip() {
        let map = SuppressionMap::from_source("x=1  # fmt: skip");
        assert!(map.is_formatting_disabled(1));
        assert!(!map.is_formatting_disabled(2));
    }

    #[test]
    fn test_parse_fmt_off_on() {
        let source = r#"
x = 1
# fmt: off
y = 2
z = 3
# fmt: on
a = 4
"#;
        let map = SuppressionMap::from_source(source);
        assert!(!map.is_formatting_disabled(2));
        assert!(map.is_formatting_disabled(3));
        assert!(map.is_formatting_disabled(4));
        assert!(map.is_formatting_disabled(5));
        assert!(map.is_formatting_disabled(6));
        assert!(!map.is_formatting_disabled(7));
    }

    #[test]
    fn test_parse_fmt_off_unclosed() {
        let source = r#"
x = 1
# fmt: off
y = 2
z = 3
"#;
        let map = SuppressionMap::from_source(source);
        assert!(!map.is_formatting_disabled(2));
        assert!(map.is_formatting_disabled(3));
        assert!(map.is_formatting_disabled(4));
        assert!(map.is_formatting_disabled(5));
        assert!(map.is_formatting_disabled(1000));
    }

    #[test]
    fn test_combined_suppressions() {
        let map = SuppressionMap::from_source("x: int = 'str'  # type: ignore  # noqa: BEA001");
        assert!(map.is_type_suppressed(1, None));
        assert!(map.is_lint_suppressed(1, Some("BEA001")));
    }

    #[test]
    fn test_case_insensitive() {
        let map = SuppressionMap::from_source("x = 1  # noqa: bea001");
        assert!(map.is_suppressed(1, Some("BEA001")));

        let map = SuppressionMap::from_source("x: int = 'str'  # type: ignore[Assignment]");
        assert!(map.is_suppressed(1, Some("assignment")));
    }

    #[test]
    fn test_multiple_fmt_off_on_ranges() {
        let source = r#"
x = 1
# fmt: off
y = 2
# fmt: on
z = 3
# fmt: off
a = 4
# fmt: on
b = 5
"#;
        let map = SuppressionMap::from_source(source);
        assert!(!map.is_formatting_disabled(2));
        assert!(map.is_formatting_disabled(3));
        assert!(map.is_formatting_disabled(4));
        assert!(map.is_formatting_disabled(5));
        assert!(!map.is_formatting_disabled(6));
        assert!(map.is_formatting_disabled(7));
        assert!(map.is_formatting_disabled(8));
        assert!(map.is_formatting_disabled(9));
        assert!(!map.is_formatting_disabled(10));
    }

    #[test]
    fn test_whitespace_variations() {
        let map = SuppressionMap::from_source("x = 1  #noqa:BEA001");
        assert!(map.is_suppressed(1, Some("BEA001")));

        let map = SuppressionMap::from_source("x = 1  #  noqa:  BEA001");
        assert!(map.is_suppressed(1, Some("BEA001")));

        let map = SuppressionMap::from_source("x = 1  #type:ignore");
        assert!(map.is_suppressed(1, None));

        let map = SuppressionMap::from_source("x = 1  #  fmt:  skip");
        assert!(map.is_formatting_disabled(1));
    }

    #[test]
    fn test_empty_source() {
        let map = SuppressionMap::from_source("");
        assert!(!map.is_suppressed(1, Some("BEA001")));
        assert!(!map.is_formatting_disabled(1));
    }

    #[test]
    fn test_no_suppressions() {
        let map = SuppressionMap::from_source("x = 1\ny = 2\nz = 3");
        assert!(!map.is_suppressed(1, Some("BEA001")));
        assert!(!map.is_suppressed(2, None));
        assert!(!map.is_formatting_disabled(1));
    }
}
