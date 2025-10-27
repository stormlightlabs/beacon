//! Structured docstring parsing for NumPy, Google, and reStructuredText styles
//!
//! This module provides auto-detection and parsing of structured docstring formats,
//! converting them into a common representation that can be formatted as rich markdown.

mod google;
mod numpy;

use std::collections::HashMap;

/// Detected docstring style
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocstringStyle {
    /// NumPy-style docstrings with sections like "Parameters\n----------"
    NumPy,
    /// Google-style docstrings with sections like "Args:"
    Google,
    /// reStructuredText with field lists like `:param name:`
    ReST,
    /// Plain text without structured sections
    Plain,
}

/// A parsed parameter or argument
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_info: Option<String>,
    pub description: String,
}

/// A parsed exception/raise entry
#[derive(Debug, Clone, PartialEq)]
pub struct RaisesEntry {
    pub exception_type: Option<String>,
    pub description: String,
}

/// A parsed return value description
#[derive(Debug, Clone, PartialEq)]
pub struct Returns {
    pub type_info: Option<String>,
    pub description: String,
}

/// Structured representation of a parsed docstring
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedDocstring {
    /// Brief summary (first line or paragraph)
    pub summary: String,
    /// Extended description (paragraphs after summary)
    pub description: Option<String>,
    /// Parameters/Arguments
    pub parameters: Vec<Parameter>,
    /// Return value information
    pub returns: Option<Returns>,
    /// Exceptions that may be raised
    pub raises: Vec<RaisesEntry>,
    /// Examples section
    pub examples: Option<String>,
    /// Notes section
    pub notes: Option<String>,
    /// See Also section
    pub see_also: Option<String>,
    /// Original style detected
    pub style: DocstringStyle,
}

impl ParsedDocstring {
    /// Create an empty parsed docstring
    pub fn empty(style: DocstringStyle) -> Self {
        Self {
            summary: String::new(),
            description: None,
            parameters: Vec::new(),
            returns: None,
            raises: Vec::new(),
            examples: None,
            notes: None,
            see_also: None,
            style,
        }
    }

    /// Convert the parsed docstring to markdown format
    pub fn to_markdown(&self) -> String {
        let mut md = String::new();

        if !self.summary.is_empty() {
            md.push_str(&self.summary);
            md.push_str("\n\n");
        }

        if let Some(desc) = &self.description {
            md.push_str(desc);
            md.push_str("\n\n");
        }

        if !self.parameters.is_empty() {
            md.push_str("**Parameters:**\n\n");
            for param in &self.parameters {
                md.push_str("- `");
                md.push_str(&param.name);
                md.push('`');
                if let Some(ref type_info) = param.type_info {
                    md.push_str(" (");
                    md.push_str(type_info);
                    md.push(')');
                }
                if !param.description.is_empty() {
                    md.push_str(": ");
                    md.push_str(&param.description);
                }
                md.push('\n');
            }
            md.push('\n');
        }

        if let Some(ref returns) = self.returns {
            md.push_str("**Returns:**\n\n");
            if let Some(ref type_info) = returns.type_info {
                md.push_str(&format!("- `{type_info}`: "));
            } else {
                md.push_str("- ");
            }
            md.push_str(&returns.description);
            md.push_str("\n\n");
        }

        if !self.raises.is_empty() {
            md.push_str("**Raises:**\n\n");
            for raise in &self.raises {
                md.push_str("- ");
                if let Some(ref exc_type) = raise.exception_type {
                    md.push_str(&format!("`{exc_type}`: "));
                }
                md.push_str(&raise.description);
                md.push('\n');
            }
            md.push('\n');
        }

        if let Some(ref examples) = self.examples {
            md.push_str("**Examples:**\n\n");
            md.push_str(examples);
            md.push_str("\n\n");
        }

        if let Some(ref notes) = self.notes {
            md.push_str("**Notes:**\n\n");
            md.push_str(notes);
            md.push_str("\n\n");
        }

        if let Some(ref see_also) = self.see_also {
            md.push_str("**See Also:**\n\n");
            md.push_str(see_also);
            md.push_str("\n\n");
        }

        md.trim_end().to_string()
    }
}

/// Auto-detect the docstring style
pub fn detect_style(docstring: &str) -> DocstringStyle {
    let lines: Vec<&str> = docstring.lines().collect();

    for i in 0..lines.len().saturating_sub(1) {
        let line = lines[i].trim();
        let next_line = lines[i + 1].trim();

        if (!line.is_empty() && !next_line.is_empty())
            && (next_line.chars().all(|c| c == '-' || c == '=') && next_line.len() >= 3)
        {
            let line_lower = line.to_lowercase();
            if line_lower == "parameters"
                || line_lower == "returns"
                || line_lower == "raises"
                || line_lower == "examples"
                || line_lower == "notes"
                || line_lower == "see also"
                || line_lower == "attributes"
            {
                return DocstringStyle::NumPy;
            }
        }
    }

    for line in &lines {
        let trimmed = line.trim();
        let trimmed_lower = trimmed.to_lowercase();

        if trimmed_lower == "args:"
            || trimmed_lower == "arguments:"
            || trimmed_lower == "parameters:"
            || trimmed_lower == "returns:"
            || trimmed_lower == "return:"
            || trimmed_lower == "yields:"
            || trimmed_lower == "yield:"
            || trimmed_lower == "raises:"
            || trimmed_lower == "examples:"
            || trimmed_lower == "example:"
            || trimmed_lower == "note:"
            || trimmed_lower == "notes:"
            || trimmed_lower == "see also:"
            || trimmed_lower == "attributes:"
        {
            return DocstringStyle::Google;
        }
    }

    for line in &lines {
        let trimmed = line.trim();
        if trimmed.starts_with(":param ")
            || trimmed.starts_with(":type ")
            || trimmed.starts_with(":return:")
            || trimmed.starts_with(":rtype:")
            || trimmed.starts_with(":raises ")
        {
            return DocstringStyle::ReST;
        }
    }

    DocstringStyle::Plain
}

/// Parse a docstring with auto-detected style
pub fn parse(docstring: &str) -> ParsedDocstring {
    let style = detect_style(docstring);
    match style {
        DocstringStyle::NumPy => numpy::parse(docstring),
        DocstringStyle::Google => google::parse(docstring),
        DocstringStyle::ReST => parse_rest(docstring),
        DocstringStyle::Plain => parse_plain(docstring),
    }
}

/// Parse a plain docstring (no structured sections)
fn parse_plain(docstring: &str) -> ParsedDocstring {
    let docstring = docstring.trim();
    if docstring.is_empty() {
        return ParsedDocstring::empty(DocstringStyle::Plain);
    }

    let paragraphs: Vec<&str> = docstring
        .split("\n\n")
        .map(|p| p.trim())
        .filter(|p| !p.is_empty())
        .collect();

    let summary = paragraphs.first().unwrap_or(&"").to_string();
    let description = if paragraphs.len() > 1 { Some(paragraphs[1..].join("\n\n")) } else { None };

    ParsedDocstring {
        summary,
        description,
        parameters: Vec::new(),
        returns: None,
        raises: Vec::new(),
        examples: None,
        notes: None,
        see_also: None,
        style: DocstringStyle::Plain,
    }
}

/// Parse a reStructuredText-style docstring with field lists
fn parse_rest(docstring: &str) -> ParsedDocstring {
    let lines: Vec<&str> = docstring.lines().collect();
    let mut result = ParsedDocstring::empty(DocstringStyle::ReST);

    let mut summary_lines = Vec::new();
    let mut description_lines = Vec::new();
    let mut in_summary = true;
    let mut param_map: HashMap<String, Parameter> = HashMap::new();
    let mut current_return_desc = Vec::new();
    let mut current_return_type: Option<String> = None;

    for line in lines {
        let trimmed = line.trim();

        if let Some(rest) = trimmed.strip_prefix(":param ") {
            if let Some(colon_pos) = rest.find(':') {
                let name = rest[..colon_pos].trim().to_string();
                let desc = rest[colon_pos + 1..].trim().to_string();
                param_map
                    .entry(name.clone())
                    .or_insert_with(|| Parameter { name: name.clone(), type_info: None, description: String::new() })
                    .description = desc;
            }
        } else if let Some(rest) = trimmed.strip_prefix(":type ") {
            in_summary = false;
            if let Some(colon_pos) = rest.find(':') {
                let name = rest[..colon_pos].trim().to_string();
                let type_str = rest[colon_pos + 1..].trim().to_string();
                param_map
                    .entry(name.clone())
                    .or_insert_with(|| Parameter { name: name.clone(), type_info: None, description: String::new() })
                    .type_info = Some(type_str);
            }
        } else if let Some(rest) = trimmed.strip_prefix(":return:") {
            in_summary = false;
            current_return_desc.push(rest.trim());
        } else if let Some(rest) = trimmed.strip_prefix(":returns:") {
            in_summary = false;
            current_return_desc.push(rest.trim());
        } else if let Some(rest) = trimmed.strip_prefix(":rtype:") {
            in_summary = false;
            current_return_type = Some(rest.trim().to_string());
        } else if let Some(rest) = trimmed.strip_prefix(":raises ") {
            in_summary = false;
            if let Some(colon_pos) = rest.find(':') {
                let exc_type = rest[..colon_pos].trim().to_string();
                let desc = rest[colon_pos + 1..].trim().to_string();
                result
                    .raises
                    .push(RaisesEntry { exception_type: Some(exc_type), description: desc });
            }
        } else if trimmed.is_empty() {
            if in_summary && !summary_lines.is_empty() {
                in_summary = false;
            }
        } else if in_summary {
            summary_lines.push(trimmed);
        } else {
            description_lines.push(trimmed);
        }
    }

    result.summary = summary_lines.join(" ");
    if !description_lines.is_empty() {
        result.description = Some(description_lines.join(" "));
    }

    result.parameters = param_map.into_values().collect();

    if !current_return_desc.is_empty() || current_return_type.is_some() {
        result.returns = Some(Returns { type_info: current_return_type, description: current_return_desc.join(" ") });
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_numpy_style() {
        let doc = r#"
Brief summary.

Parameters
----------
x : int
    The first parameter.
y : str
    The second parameter.
"#;
        assert_eq!(detect_style(doc), DocstringStyle::NumPy);
    }

    #[test]
    fn test_detect_google_style() {
        let doc = r#"
Brief summary.

Args:
    x (int): The first parameter.
    y (str): The second parameter.
"#;
        assert_eq!(detect_style(doc), DocstringStyle::Google);
    }

    #[test]
    fn test_detect_rest_style() {
        let doc = r#"
Brief summary.

:param x: The first parameter.
:type x: int
:param y: The second parameter.
:type y: str
"#;
        assert_eq!(detect_style(doc), DocstringStyle::ReST);
    }

    #[test]
    fn test_detect_plain_style() {
        let doc = "Just a plain docstring without sections.";
        assert_eq!(detect_style(doc), DocstringStyle::Plain);
    }

    #[test]
    fn test_parse_plain() {
        let doc = "Brief summary.\n\nExtended description.";
        let parsed = parse(doc);
        assert_eq!(parsed.style, DocstringStyle::Plain);
        assert_eq!(parsed.summary, "Brief summary.");
        assert_eq!(parsed.description, Some("Extended description.".to_string()));
    }

    #[test]
    fn test_parse_numpy_parameters() {
        let doc = r#"Brief summary.

Parameters
----------
x : int
    The first parameter.
y : str
    The second parameter.
"#;
        let parsed = parse(doc);
        assert_eq!(parsed.parameters.len(), 2);
        assert_eq!(parsed.parameters[0].name, "x");
        assert_eq!(parsed.parameters[0].type_info, Some("int".to_string()));
        assert_eq!(parsed.parameters[0].description, "The first parameter.");
    }

    #[test]
    fn test_parse_google_parameters() {
        let doc = r#"Brief summary.

Args:
    x (int): The first parameter.
    y (str): The second parameter.
"#;
        let parsed = parse(doc);
        assert_eq!(parsed.parameters.len(), 2);
        assert_eq!(parsed.parameters[0].name, "x");
        assert_eq!(parsed.parameters[0].type_info, Some("int".to_string()));
        assert_eq!(parsed.parameters[0].description, "The first parameter.");
    }

    #[test]
    fn test_to_markdown() {
        let mut parsed = ParsedDocstring::empty(DocstringStyle::Plain);
        parsed.summary = "Brief summary.".to_string();
        parsed.parameters.push(Parameter {
            name: "x".to_string(),
            type_info: Some("int".to_string()),
            description: "The first parameter.".to_string(),
        });
        parsed.returns =
            Some(Returns { type_info: Some("bool".to_string()), description: "True on success.".to_string() });

        let md = parsed.to_markdown();
        assert!(md.contains("Brief summary."));
        assert!(md.contains("**Parameters:**"));
        assert!(md.contains("`x` (int)"));
        assert!(md.contains("**Returns:**"));
        assert!(md.contains("`bool`"));
    }
}
