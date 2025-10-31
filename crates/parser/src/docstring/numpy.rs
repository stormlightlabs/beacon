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
    let entries: Vec<(String, usize)> = lines
        .iter()
        .filter_map(|line| {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                None
            } else {
                let indent = line.len() - line.trim_start().len();
                Some((trimmed.to_string(), indent))
            }
        })
        .collect();

    if entries.is_empty() {
        return None;
    }

    let (first_text, first_indent) = (&entries[0].0, entries[0].1);
    let mut type_info = None;
    let mut description_parts: Vec<String> = Vec::new();

    if let Some(colon_pos) = first_text.find(':') {
        let type_str = first_text[..colon_pos].trim();
        if !type_str.is_empty() {
            type_info = Some(type_str.to_string());
        }
        let desc_start = first_text[colon_pos + 1..].trim();
        if !desc_start.is_empty() {
            description_parts.push(desc_start.to_string());
        }
    } else {
        let has_indented_follow_up = entries
            .get(1)
            .map(|(_, indent)| *indent > first_indent)
            .unwrap_or(false);

        if has_indented_follow_up {
            type_info = Some(first_text.to_string());
        } else if !first_text.is_empty() {
            description_parts.push(first_text.to_string());
        }
    }

    for (text, _) in entries.iter().skip(1) {
        if !text.is_empty() {
            description_parts.push(text.to_string());
        }
    }

    let description = description_parts.join(" ").trim().to_string();

    if type_info.is_none() && description.is_empty() {
        None
    } else {
        Some(Returns { type_info, description })
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_returns_with_type_and_description() {
        let lines = vec!["int : The result value"];
        let result = parse_returns(&lines);

        assert!(result.is_some());
        let returns = result.unwrap();
        assert_eq!(returns.type_info, Some("int".to_string()));
        assert!(returns.description.contains("The result value"));
    }

    #[test]
    fn test_parse_returns_type_with_colon() {
        let lines = vec!["int : The integer result", "    with more details"];
        let result = parse_returns(&lines);

        assert!(result.is_some());
        let returns = result.unwrap();
        assert_eq!(returns.type_info, Some("int".to_string()));
        assert!(returns.description.contains("The integer result"));
        assert!(returns.description.contains("with more details"));
    }

    #[test]
    fn test_parse_returns_no_type() {
        let lines = vec!["Returns the computed value", "after processing"];
        let result = parse_returns(&lines);

        assert!(result.is_some());
        let returns = result.unwrap();
        assert_eq!(returns.type_info, None);
        assert!(returns.description.contains("Returns the computed value"));
        assert!(returns.description.contains("after processing"));
    }

    #[test]
    fn test_parse_returns_empty() {
        let lines: Vec<&str> = vec![];
        let result = parse_returns(&lines);
        assert!(result.is_none());
    }

    #[test]
    fn test_parse_returns_whitespace_only() {
        let lines = vec!["   ", "  ", ""];
        let result = parse_returns(&lines);
        assert!(result.is_none());
    }

    #[test]
    fn test_parse_returns_complex_type() {
        let lines = vec!["list[dict[str, int]]", "    A list of dictionaries"];
        let result = parse_returns(&lines);

        assert!(result.is_some());
        let returns = result.unwrap();
        assert_eq!(returns.type_info, Some("list[dict[str, int]]".to_string()));
        assert_eq!(returns.description, "A list of dictionaries");
    }

    #[test]
    fn test_parse_returns_multiline_description() {
        let lines = vec![
            "str",
            "    First line of description",
            "    Second line",
            "    Third line",
        ];
        let result = parse_returns(&lines);

        assert!(result.is_some());
        let returns = result.unwrap();
        assert_eq!(returns.type_info, Some("str".to_string()));
        assert!(returns.description.contains("First line"));
        assert!(returns.description.contains("Second line"));
        assert!(returns.description.contains("Third line"));
    }

    #[test]
    fn test_parse_returns_empty_type_with_colon() {
        let lines = vec![": Description without type"];
        let result = parse_returns(&lines);

        assert!(result.is_some());
        let returns = result.unwrap();
        assert_eq!(returns.type_info, None);
        assert_eq!(returns.description, "Description without type");
    }
}
