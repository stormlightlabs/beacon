//! Shared source-position helpers.

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceRange {
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl SourceRange {
    pub const fn new(line: usize, col: usize, end_line: usize, end_col: usize) -> Self {
        Self { line, col, end_line, end_col }
    }
}

/// Convert a 1-based line/column pair to a UTF-8 byte offset.
pub fn line_col_to_byte_offset(source: &str, line: usize, col: usize) -> Option<usize> {
    if line == 0 || col == 0 {
        return None;
    }

    let mut current_line = 1;
    let mut current_col = 1;

    for (byte_offset, ch) in source.char_indices() {
        if current_line == line && current_col == col {
            return Some(byte_offset);
        }

        if ch == '\n' {
            current_line += 1;
            current_col = 1;
        } else {
            current_col += 1;
        }
    }

    if current_line == line && current_col == col { Some(source.len()) } else { None }
}

/// Convert a 1-based line/column pair to a byte offset, clamping missing positions to EOF.
pub fn line_col_to_byte_offset_lossy(source: &str, line: usize, col: usize) -> usize {
    line_col_to_byte_offset(source, line, col).unwrap_or(source.len())
}

/// Convert a UTF-8 byte offset to a 1-based line/column pair.
pub fn byte_offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let target = offset.min(source.len());
    let mut current_line = 1;
    let mut current_col = 1;

    for (byte_offset, ch) in source.char_indices() {
        if byte_offset >= target {
            break;
        }

        if ch == '\n' {
            current_line += 1;
            current_col = 1;
        } else {
            current_col += 1;
        }
    }

    (current_line, current_col)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_col_to_byte_offset_handles_one_based_positions() {
        let source = "x = 1\nvalue = 'é'\n";
        assert_eq!(line_col_to_byte_offset(source, 1, 1), Some(0));
        assert_eq!(line_col_to_byte_offset(source, 2, 1), Some(6));
        assert_eq!(line_col_to_byte_offset(source, 2, 10), Some(15));
        assert_eq!(line_col_to_byte_offset(source, 3, 1), Some(source.len()));
        assert_eq!(line_col_to_byte_offset(source, 0, 1), None);
    }

    #[test]
    fn byte_offset_to_line_col_handles_utf8() {
        let source = "x\néz";
        assert_eq!(byte_offset_to_line_col(source, 0), (1, 1));
        assert_eq!(byte_offset_to_line_col(source, 2), (2, 1));
        assert_eq!(byte_offset_to_line_col(source, source.len()), (2, 3));
    }
}
