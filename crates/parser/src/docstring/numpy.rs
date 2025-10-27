use std::collections::HashMap;

use super::{DocstringStyle, Parameter, ParsedDocstring, RaisesEntry, Returns};

/// Parse a NumPy-style docstring
pub fn parse(docstring: &str) -> ParsedDocstring {
    let lines: Vec<&str> = docstring.lines().collect();
    let mut result = ParsedDocstring::empty(DocstringStyle::NumPy);

    let mut sections = HashMap::new();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        if i + 1 < lines.len() {
            let next_line = lines[i + 1].trim();

            if !line.is_empty()
                && !next_line.is_empty()
                && next_line.chars().all(|c| c == '-' || c == '=')
                && next_line.len() >= 3
            {
                let section_name = line.to_lowercase();
                let section_start = i + 2;

                let mut section_end = lines.len();
                for j in (section_start + 1)..lines.len().saturating_sub(1) {
                    let future_line = lines[j + 1].trim();
                    if !future_line.is_empty()
                        && future_line.chars().all(|c| c == '-' || c == '=')
                        && future_line.len() >= 3
                    {
                        section_end = j;
                        break;
                    }
                }

                sections.insert(section_name, (section_start, section_end));
                i = section_end;
                continue;
            }
        }
        i += 1;
    }

    let first_section_line = sections.values().map(|(start, _)| *start).min().unwrap_or(lines.len());
    let summary_lines: Vec<&str> = lines[..first_section_line.saturating_sub(2)]
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

    if let Some(&(start, end)) = sections.get("parameters").or_else(|| sections.get("params")) {
        result.parameters = parse_params(&lines[start..end]);
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

/// Parse NumPy-style parameters
fn parse_params(lines: &[&str]) -> Vec<Parameter> {
    let mut params = Vec::new();
    let mut i = 0;

    let base_indent = lines
        .iter()
        .find(|l| !l.trim().is_empty())
        .map(|l| l.len() - l.trim_start().len())
        .unwrap_or(0);

    while i < lines.len() {
        let line = lines[i];

        if line.trim().is_empty() {
            i += 1;
            continue;
        }

        let line_indent = line.len() - line.trim_start().len();
        let trimmed = line.trim();

        if line_indent == base_indent && !trimmed.is_empty() {
            let (name, type_info) = if let Some(colon_pos) = trimmed.find(':') {
                let name = trimmed[..colon_pos].trim().to_string();
                let type_str = trimmed[colon_pos + 1..].trim().to_string();
                (name, if type_str.is_empty() { None } else { Some(type_str) })
            } else {
                (trimmed.to_string(), None)
            };

            let mut description_lines = Vec::new();
            i += 1;

            while i < lines.len() {
                let desc_line = lines[i];
                let desc_indent = desc_line.len() - desc_line.trim_start().len();

                if desc_line.trim().is_empty() {
                    i += 1;
                    if i < lines.len() {
                        let next_indent = lines[i].len() - lines[i].trim_start().len();
                        if next_indent == base_indent {
                            break;
                        }
                    }
                    continue;
                } else if desc_indent > base_indent {
                    description_lines.push(desc_line.trim());
                    i += 1;
                } else {
                    break;
                }
            }

            params.push(Parameter { name, type_info, description: description_lines.join(" ") });
        } else {
            i += 1;
        }
    }

    params
}

/// Parse NumPy-style returns section
fn parse_returns(lines: &[&str]) -> Option<super::Returns> {
    let non_empty: Vec<&str> = lines.iter().map(|l| l.trim()).filter(|l| !l.is_empty()).collect();
    if non_empty.is_empty() {
        return None;
    }

    let first = non_empty[0];
    let (type_info, desc_start) = if let Some(colon_pos) = first.find(':') {
        let type_str = first[..colon_pos].trim();
        let desc_start = first[colon_pos + 1..].trim();
        (
            if type_str.is_empty() { None } else { Some(type_str.to_string()) },
            desc_start,
        )
    } else {
        (None, first)
    };

    let mut description_parts = vec![desc_start];
    description_parts.extend(non_empty.iter().skip(1).copied());

    Some(Returns { type_info, description: description_parts.join(" ") })
}

/// Parse NumPy-style raises section
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
        if trimmed.is_empty() {
            i += 1;
            continue;
        }

        let exception_type = Some(trimmed.to_string());

        let base_indent = line.len() - line.trim_start().len();
        let mut description_lines = Vec::new();
        i += 1;

        while i < lines.len() {
            let desc_line = lines[i];
            let desc_indent = desc_line.len() - desc_line.trim_start().len();

            if desc_line.trim().is_empty() {
                i += 1;
                continue;
            } else if desc_indent > base_indent {
                description_lines.push(desc_line.trim());
                i += 1;
            } else {
                break;
            }
        }

        raises.push(RaisesEntry { exception_type, description: description_lines.join(" ") });
    }

    raises
}
