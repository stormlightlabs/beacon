//! Utility functions for LSP operations
//!
//! This module provides essential conversion utilities for working with LSP positions, ranges, and URIs.
//! The LSP uses UTF-16 character offsets while Rust strings are UTF-8, requiring careful conversion through [`ropey`].

use lsp_types::{Position, Range};
use ropey::Rope;
use std::path::PathBuf;
use url::Url;

/// Convert a byte offset in UTF-8 text to an LSP Position (line, UTF-16 character offset)
///
/// The LSP spec requires positions to be expressed as (line, character) where:
/// - line is zero-indexed
/// - character is the UTF-16 code unit offset from the start of the line
pub fn byte_offset_to_position(text: &str, offset: usize) -> Position {
    let rope = Rope::from_str(text);
    let line_idx = rope.byte_to_line(offset.min(rope.len_bytes()));
    let line_start = rope.line_to_byte(line_idx);
    let line = rope.line(line_idx);

    let byte_offset_in_line = offset.saturating_sub(line_start);
    let line_str = line.to_string();
    let utf16_offset = utf8_to_utf16_offset(&line_str, byte_offset_in_line);

    Position { line: line_idx as u32, character: utf16_offset as u32 }
}

/// Convert an LSP Position to a byte offset in UTF-8 text
pub fn position_to_byte_offset(text: &str, position: Position) -> usize {
    let rope = Rope::from_str(text);
    let line_idx = position.line as usize;

    if line_idx >= rope.len_lines() {
        return rope.len_bytes();
    }

    let line_start = rope.line_to_byte(line_idx);
    let line = rope.line(line_idx);
    let line_str = line.to_string();
    let utf8_offset = utf16_to_utf8_offset(&line_str, position.character as usize);

    line_start + utf8_offset
}

/// Convert UTF-8 byte offset within a string to UTF-16 code unit offset
///
/// LSP positions use UTF-16 offsets, but Rust strings are UTF-8.
/// This function converts from byte indices to UTF-16 code unit counts.
pub fn utf8_to_utf16_offset(text: &str, byte_offset: usize) -> usize {
    let clamped = byte_offset.min(text.len());
    text[..clamped].encode_utf16().count()
}

/// Convert UTF-16 code unit offset to UTF-8 byte offset within a string
///
/// The inverse of [`utf8_to_utf16_offset`].
pub fn utf16_to_utf8_offset(text: &str, utf16_offset: usize) -> usize {
    let mut utf16_count = 0;

    for (byte_idx, _ch) in text.char_indices() {
        if utf16_count >= utf16_offset {
            return byte_idx;
        }
        utf16_count += text[byte_idx..].chars().next().unwrap().len_utf16();
    }

    text.len()
}

/// Convert an LSP Range to byte offsets (start, end)
pub fn range_to_byte_offsets(text: &str, range: Range) -> (usize, usize) {
    let start = position_to_byte_offset(text, range.start);
    let end = position_to_byte_offset(text, range.end);
    (start, end)
}

/// Convert byte offsets to an LSP Range
pub fn byte_offsets_to_range(text: &str, start: usize, end: usize) -> Range {
    Range { start: byte_offset_to_position(text, start), end: byte_offset_to_position(text, end) }
}

/// Convert tree-sitter Point (row, column in bytes) to LSP Position
///
/// Tree-sitter uses (row, column) where column is a byte offset.
/// LSP uses (line, character) where character is a UTF-16 offset.
pub fn tree_sitter_point_to_position(text: &str, point: tree_sitter::Point) -> Position {
    let rope = Rope::from_str(text);
    let line_idx = point.row.min(rope.len_lines().saturating_sub(1));

    if line_idx >= rope.len_lines() {
        return Position { line: line_idx as u32, character: 0 };
    }

    let line = rope.line(line_idx);
    let line_str = line.to_string();

    let utf16_offset = utf8_to_utf16_offset(&line_str, point.column);

    Position { line: point.row as u32, character: utf16_offset as u32 }
}

/// Convert tree-sitter Range to LSP Range
pub fn tree_sitter_range_to_lsp_range(text: &str, range: tree_sitter::Range) -> Range {
    Range {
        start: tree_sitter_point_to_position(text, range.start_point),
        end: tree_sitter_point_to_position(text, range.end_point),
    }
}

/// Convert LSP URI to PathBuf
///
/// Handles file:// URIs and converts them to filesystem paths.
pub fn uri_to_path(uri: &Url) -> Option<PathBuf> {
    uri.to_file_path().ok()
}

/// Convert PathBuf to LSP URI
///
/// Creates a file:// URI from a filesystem path.
pub fn path_to_uri(path: &PathBuf) -> Option<Url> {
    Url::from_file_path(path).ok()
}

/// Get text within a range
///
/// TODO: Optimize using Ropey slicing
pub fn text_in_range(text: &str, range: Range) -> String {
    let (start, end) = range_to_byte_offsets(text, range);
    text.get(start..end).unwrap_or("").to_string()
}

/// Calculate the line and column (in UTF-8 bytes) for a byte offset
///
/// Returns (line_number, column_offset) both zero-indexed.
pub fn byte_offset_to_line_col(text: &str, offset: usize) -> (usize, usize) {
    let rope = Rope::from_str(text);
    let line_idx = rope.byte_to_line(offset.min(rope.len_bytes()));
    let line_start = rope.line_to_byte(line_idx);
    let col = offset.saturating_sub(line_start);
    (line_idx, col)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_utf8_to_utf16_ascii() {
        let text = "hello";
        assert_eq!(utf8_to_utf16_offset(text, 0), 0);
        assert_eq!(utf8_to_utf16_offset(text, 5), 5);
    }

    #[test]
    fn test_utf8_to_utf16_multibyte() {
        // "café" - 'é' is 2 bytes in UTF-8, 1 UTF-16 code unit
        let text = "café";
        assert_eq!(utf8_to_utf16_offset(text, 3), 3); // up to 'f'
        assert_eq!(utf8_to_utf16_offset(text, 5), 4); // up to end (é is 2 bytes)
    }

    #[test]
    fn test_utf16_to_utf8_ascii() {
        let text = "hello";
        assert_eq!(utf16_to_utf8_offset(text, 0), 0);
        assert_eq!(utf16_to_utf8_offset(text, 5), 5);
    }

    #[test]
    fn test_utf16_to_utf8_multibyte() {
        let text = "café";
        assert_eq!(utf16_to_utf8_offset(text, 3), 3); // 'f' position
        assert_eq!(utf16_to_utf8_offset(text, 4), 5); // end (é is 2 bytes)
    }

    #[test]
    fn test_position_conversions() {
        let text = "line1\nline2\nline3";
        let pos = byte_offset_to_position(text, 6);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);

        let offset = position_to_byte_offset(text, pos);
        assert_eq!(offset, 6);
    }

    #[test]
    fn test_byte_offset_to_line_col() {
        let text = "line1\nline2\nline3";

        assert_eq!(byte_offset_to_line_col(text, 0), (0, 0));
        assert_eq!(byte_offset_to_line_col(text, 6), (1, 0));
        assert_eq!(byte_offset_to_line_col(text, 12), (2, 0));
    }

    #[test]
    fn test_range_conversions() {
        let text = "hello\nworld";
        let range = Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 5 } };
        let (start, end) = range_to_byte_offsets(text, range);
        assert_eq!(start, 0);
        assert_eq!(end, 5);

        let extracted = text_in_range(text, range);
        assert_eq!(extracted, "hello");
    }

    #[test]
    fn test_uri_path_conversion() {
        let path = PathBuf::from("/tmp/test.py");
        let uri = path_to_uri(&path).expect("Failed to convert path to URI");
        let back = uri_to_path(&uri).expect("Failed to convert URI to path");
        assert_eq!(path, back);
    }
}
