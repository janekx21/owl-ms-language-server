use crate::error::{Error, Result, ResultExt};
use log::debug;
use ropey::Rope;
use std::fmt::Display;

/// Zero and utf-8 byte offst based 2D text position.
/// Positions are always related to documents (rope or string).
/// There are special functions for converting from and to LSP types.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Position {
    line: u32,
    character: u32,
}

impl Position {
    pub const ZERO: Self = Self {
        line: 0,
        character: 0,
    };

    pub fn new(line: u32, character: u32) -> Self {
        Self { line, character }
    }

    /// Consumes an LSP Position to create a UTF-8 based Position
    pub fn from_lsp(
        pos: tower_lsp::lsp_types::Position,
        rope: &Rope,
        encoding: &tower_lsp::lsp_types::PositionEncodingKind,
    ) -> Result<Self> {
        let character = match encoding.as_str() {
            "utf-8" => pos.character,
            "utf-16" => {
                // because the source is u32
                #[allow(clippy::cast_possible_truncation)]
                let i = utf16_offset_to_utf8_offset(
                    rope.get_line(pos.line as usize)
                        .ok_or(Error::PositionOutOfBoundsTowerLsp(pos))?
                        .to_string()
                        .as_str(),
                    pos.character as usize,
                )
                .ok_or(Error::PositionOutOfBoundsTowerLsp(pos))? as u32;
                debug!(
                    "Converting utf-16 ({}) index into utf-8 ({})",
                    pos.character, i
                );
                i
            }
            _ => todo!(),
        };
        Ok(Self {
            line: pos.line,
            character,
        })
    }

    pub fn into_lsp(
        self,
        rope: &Rope,
        encoding: &tower_lsp::lsp_types::PositionEncodingKind,
    ) -> Result<tower_lsp::lsp_types::Position> {
        let character = match encoding.as_str() {
            "utf-8" => self.character,
            "utf-16" => {
                // because the source is u32
                #[allow(clippy::cast_possible_truncation)]
                let i = utf8_offset_to_utf16_offset(
                    rope.get_line(self.line as usize)
                        .ok_or(Error::PositionOutOfBounds(self))?
                        .to_string()
                        .as_str(),
                    self.character as usize,
                )
                .ok_or(Error::PositionOutOfBounds(self))? as u32;
                i
            }
            _ => todo!(),
        };
        Ok(tower_lsp::lsp_types::Position {
            line: self.line,
            character,
        })
    }

    pub fn new_from_byte_index(rope: &Rope, index: usize) -> Self {
        let line = rope.try_byte_to_line(index).unwrap_or(rope.len_lines() - 1);
        // TODO this could be out of bounds
        let character = index - rope.line_to_byte(line);
        // because the source is u32
        #[allow(clippy::cast_possible_truncation)]
        Self {
            line: line as u32,
            character: character as u32,
        }
    }

    pub fn line(self) -> u32 {
        self.line
    }

    pub fn character_byte(self) -> u32 {
        self.character
    }

    pub fn byte_index(self, rope: &Rope) -> usize {
        rope.try_line_to_byte(self.line as usize)
            .map_err(std::convert::Into::into)
            .inspect_log()
            .unwrap_or(rope.line_to_byte(rope.len_lines() - 1))
            + self.character as usize
    }

    pub fn char_index(self, rope: &Rope) -> usize {
        rope.try_byte_to_char(self.byte_index(rope))
            .map_err(std::convert::Into::into)
            .inspect_log()
            .unwrap_or(rope.len_chars() - 1)
    }

    pub fn moved_right(self, char_offset: u32, rope: &Rope) -> Self {
        let char_idx = self.char_index(rope);
        let char_idx = char_idx.saturating_add(char_offset as usize);
        let char_idx = char_idx.min(rope.len_chars() - 1); // clamp
        Self::new_from_byte_index(rope, rope.char_to_byte(char_idx))
    }

    pub fn moved_left(self, char_offset: u32, rope: &Rope) -> Self {
        let char_idx = self.char_index(rope);
        let char_idx = char_idx.saturating_sub(char_offset as usize);
        Self::new_from_byte_index(rope, rope.char_to_byte(char_idx))
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{} (utf-8)", self.line, self.character)
    }
}

fn utf16_offset_to_utf8_offset(s: &str, utf16_offset: usize) -> Option<usize> {
    let mut utf16_count = 0;
    let mut utf8_offset = 0;

    for ch in s.chars() {
        // If we've reached the target UTF-16 offset, return the current UTF-8 offset
        if utf16_count == utf16_offset {
            return Some(utf8_offset);
        }

        // Count UTF-16 code units for this character
        utf16_count += ch.len_utf16();

        // Count UTF-8 bytes for this character
        utf8_offset += ch.len_utf8();
    }

    // Check if the offset is exactly at the end of the string
    if utf16_count == utf16_offset {
        Some(utf8_offset)
    } else {
        None // UTF-16 offset is beyond the string
    }
}

fn utf8_offset_to_utf16_offset(s: &str, utf8_offset: usize) -> Option<usize> {
    let mut utf8_count = 0;
    let mut utf16_offset = 0;

    for ch in s.chars() {
        // If we've reached the target UTF-8 offset, return the current UTF-16 offset
        if utf8_count == utf8_offset {
            return Some(utf16_offset);
        }

        // Count UTF-16 bytes for this character
        utf16_offset += ch.len_utf16();

        // Count UTF-8 code units for this character
        utf8_count += ch.len_utf8();
    }

    // Check if the offset is exactly at the end of the string
    if utf8_count == utf8_offset {
        Some(utf16_offset)
    } else {
        None // UTF-8 offset is beyond the string
    }
}

impl From<tree_sitter_c2rust::Point> for Position {
    fn from(value: tree_sitter_c2rust::Point) -> Self {
        // The assumption is that this is also byte based!

        // because the source is u32
        #[allow(clippy::cast_possible_truncation)]
        Position {
            line: value.row as u32,
            character: value.column as u32,
        }
    }
}

impl From<Position> for tree_sitter_c2rust::Point {
    fn from(value: Position) -> tree_sitter_c2rust::Point {
        // The assumption is that this is also byte based!
        tree_sitter_c2rust::Point {
            row: value.line as usize,
            column: value.character as usize,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ascii_only() {
        let s = "hello";

        // UTF-16 to UTF-8
        assert_eq!(utf16_offset_to_utf8_offset(s, 0), Some(0));
        assert_eq!(utf16_offset_to_utf8_offset(s, 3), Some(3));
        assert_eq!(utf16_offset_to_utf8_offset(s, 5), Some(5));
        assert_eq!(utf16_offset_to_utf8_offset(s, 6), None);

        // UTF-8 to UTF-16
        assert_eq!(utf8_offset_to_utf16_offset(s, 0), Some(0));
        assert_eq!(utf8_offset_to_utf16_offset(s, 3), Some(3));
        assert_eq!(utf8_offset_to_utf16_offset(s, 5), Some(5));
        assert_eq!(utf8_offset_to_utf16_offset(s, 6), None);
    }

    #[test]
    fn test_with_emoji() {
        let s = "a🌍b"; // 'a' (1 UTF-16), '🌍' (2 UTF-16), 'b' (1 UTF-16)

        // UTF-16 to UTF-8
        assert_eq!(utf16_offset_to_utf8_offset(s, 0), Some(0)); // start of 'a'
        assert_eq!(utf16_offset_to_utf8_offset(s, 1), Some(1)); // start of '🌍'
        assert_eq!(utf16_offset_to_utf8_offset(s, 3), Some(5)); // start of 'b'
        assert_eq!(utf16_offset_to_utf8_offset(s, 4), Some(6)); // end of string

        // UTF-8 to UTF-16
        assert_eq!(utf8_offset_to_utf16_offset(s, 0), Some(0)); // start of 'a'
        assert_eq!(utf8_offset_to_utf16_offset(s, 1), Some(1)); // start of '🌍'
        assert_eq!(utf8_offset_to_utf16_offset(s, 2), None); // mid-emoji (invalid)
        assert_eq!(utf8_offset_to_utf16_offset(s, 3), None); // mid-emoji (invalid)
        assert_eq!(utf8_offset_to_utf16_offset(s, 4), None); // mid-emoji (invalid)
        assert_eq!(utf8_offset_to_utf16_offset(s, 5), Some(3)); // start of 'b'
        assert_eq!(utf8_offset_to_utf16_offset(s, 6), Some(4)); // end of string
    }

    #[test]
    fn test_empty_string() {
        let s = "";

        // UTF-16 to UTF-8
        assert_eq!(utf16_offset_to_utf8_offset(s, 0), Some(0));
        assert_eq!(utf16_offset_to_utf8_offset(s, 1), None);

        // UTF-8 to UTF-16
        assert_eq!(utf8_offset_to_utf16_offset(s, 0), Some(0));
        assert_eq!(utf8_offset_to_utf16_offset(s, 1), None);
    }

    #[test]
    fn test_round_trip_conversion() {
        let s = "Hello 🌍 World! 🦀";

        // Test that converting UTF-16 -> UTF-8 -> UTF-16 gives the original
        for utf16_offset in 0..=s.chars().map(char::len_utf16).sum::<usize>() {
            if let Some(utf8_offset) = utf16_offset_to_utf8_offset(s, utf16_offset) {
                assert_eq!(
                    utf8_offset_to_utf16_offset(s, utf8_offset),
                    Some(utf16_offset)
                );
            }
        }
    }
}
