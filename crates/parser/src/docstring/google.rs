use std::collections::HashMap;

use super::{DocstringStyle, Parameter, ParsedDocstring, RaisesEntry, Returns};

/// Parse a Google-style docstring
pub fn parse(docstring: &str) -> ParsedDocstring {
    let lines: Vec<&str> = docstring.lines().collect();
    let mut result = ParsedDocstring::empty(DocstringStyle::Google);

    let mut sections = HashMap::new();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();
        let line_lower = line.to_lowercase();

        let is_section = line_lower == "args:"
            || line_lower == "arguments:"
            || line_lower == "parameters:"
            || line_lower == "returns:"
            || line_lower == "return:"
            || line_lower == "yields:"
            || line_lower == "yield:"
            || line_lower == "raises:"
            || line_lower == "examples:"
            || line_lower == "example:"
            || line_lower == "note:"
            || line_lower == "notes:"
            || line_lower == "see also:"
            || line_lower == "attributes:";

        if is_section {
            let section_name = line_lower.trim_end_matches(':').to_string();
            let section_start = i + 1;

            let mut section_end = lines.len();
            for (j, item) in lines.iter().enumerate().skip(section_start) {
                let future_line = item.trim().to_lowercase();
                let is_next_section = future_line == "args:"
                    || future_line == "arguments:"
                    || future_line == "parameters:"
                    || future_line == "returns:"
                    || future_line == "return:"
                    || future_line == "yields:"
                    || future_line == "yield:"
                    || future_line == "raises:"
                    || future_line == "examples:"
                    || future_line == "example:"
                    || future_line == "note:"
                    || future_line == "notes:"
                    || future_line == "see also:"
                    || future_line == "attributes:";

                if is_next_section {
                    section_end = j;
                    break;
                }
            }

            sections.insert(section_name, (section_start, section_end));
            i = section_end;
            continue;
        }
        i += 1;
    }

    let first_section_line = sections.values().map(|(start, _)| *start).min().unwrap_or(lines.len());
    let summary_lines: Vec<&str> = lines[..first_section_line.saturating_sub(1)]
        .iter()
        .map(|l| l.trim())
        .collect();

    let summary_text = summary_lines.join("\n");
    let paragraphs: Vec<&str> = summary_text
        .split("\n\n")
        .map(|p| p.trim())
        .filter(|p| !p.is_empty())
        .collect();

    result.summary = paragraphs.first().unwrap_or(&"").to_string();
    if paragraphs.len() > 1 {
        result.description = Some(paragraphs[1..].join("\n\n"));
    }

    let params_key = sections
        .keys()
        .find(|k| *k == "args" || *k == "arguments" || *k == "parameters")
        .cloned();
    if let Some(key) = params_key {
        if let Some(&(start, end)) = sections.get(&key) {
            result.parameters = parse_params(&lines[start..end]);
        }
    }

    if let Some(&(start, end)) = sections.get("returns").or_else(|| sections.get("return")) {
        result.returns = parse_returns(&lines[start..end]);
    }

    if let Some(&(start, end)) = sections.get("raises") {
        result.raises = parse_raises(&lines[start..end]);
    }

    if let Some(&(start, end)) = sections.get("examples").or_else(|| sections.get("example")) {
        result.examples = Some(lines[start..end].join("\n").trim().to_string());
    }

    if let Some(&(start, end)) = sections.get("notes").or_else(|| sections.get("note")) {
        result.notes = Some(lines[start..end].join("\n").trim().to_string());
    }

    if let Some(&(start, end)) = sections.get("see also") {
        result.see_also = Some(lines[start..end].join("\n").trim().to_string());
    }

    result
}

/// Parse Google-style parameters
fn parse_params(lines: &[&str]) -> Vec<Parameter> {
    let mut params = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i];

        if line.trim().is_empty() {
            i += 1;
            continue;
        }

        let trimmed = line.trim();

        if let Some(colon_pos) = trimmed.find(':') {
            let before_colon = trimmed[..colon_pos].trim();
            let after_colon = trimmed[colon_pos + 1..].trim();

            let (name, type_info) = if let Some(open_paren) = before_colon.find('(') {
                if let Some(close_paren) = before_colon.find(')') {
                    let name = before_colon[..open_paren].trim().to_string();
                    let type_str = before_colon[open_paren + 1..close_paren].trim().to_string();
                    (name, if type_str.is_empty() { None } else { Some(type_str) })
                } else {
                    (before_colon.to_string(), None)
                }
            } else {
                (before_colon.to_string(), None)
            };

            let mut description_parts = vec![after_colon];
            let base_indent = line.len() - line.trim_start().len();
            i += 1;

            while i < lines.len() {
                let desc_line = lines[i];
                let desc_indent = desc_line.len() - desc_line.trim_start().len();

                if desc_line.trim().is_empty() {
                    i += 1;
                    break;
                } else if desc_indent > base_indent && !desc_line.trim().contains(':') {
                    description_parts.push(desc_line.trim());
                    i += 1;
                } else {
                    break;
                }
            }

            params.push(Parameter { name, type_info, description: description_parts.join(" ") });
        } else {
            i += 1;
        }
    }

    params
}

/// Parse Google-style returns section
fn parse_returns(lines: &[&str]) -> Option<Returns> {
    let non_empty: Vec<&str> = lines.iter().map(|l| l.trim()).filter(|l| !l.is_empty()).collect();
    if non_empty.is_empty() {
        return None;
    }

    let first = non_empty[0];
    let (type_info, desc_start) = if let Some(colon_pos) = first.find(':') {
        let before = first[..colon_pos].trim();
        let after = first[colon_pos + 1..].trim();
        if before.split_whitespace().count() == 1 || before.contains('(') {
            (Some(before.to_string()), after)
        } else {
            (None, first)
        }
    } else {
        (None, first)
    };

    let mut description_parts = vec![desc_start];
    description_parts.extend(non_empty.iter().skip(1).copied());

    Some(Returns { type_info, description: description_parts.join(" ") })
}

/// Parse Google-style raises section
fn parse_raises(lines: &[&str]) -> Vec<RaisesEntry> {
    let mut raises = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i];

        if line.trim().is_empty() {
            i += 1;
            continue;
        }

        let trimmed = line.trim();

        if let Some(colon_pos) = trimmed.find(':') {
            let exc_type = trimmed[..colon_pos].trim().to_string();
            let desc_start = trimmed[colon_pos + 1..].trim();

            let mut description_parts = vec![desc_start];
            let base_indent = line.len() - line.trim_start().len();
            i += 1;

            while i < lines.len() {
                let desc_line = lines[i];
                let desc_indent = desc_line.len() - desc_line.trim_start().len();

                if desc_line.trim().is_empty() {
                    i += 1;
                    break;
                } else if desc_indent > base_indent && !desc_line.trim().contains(':') {
                    description_parts.push(desc_line.trim());
                    i += 1;
                } else {
                    break;
                }
            }

            raises.push(RaisesEntry { exception_type: Some(exc_type), description: description_parts.join(" ") });
        } else {
            i += 1;
        }
    }

    raises
}
