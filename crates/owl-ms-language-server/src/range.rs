use crate::error::Result;
use crate::pos::Position;
use ropey::Rope;
use std::fmt::Display;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
// Range like selection therefore endposition is exclusive
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub const ZERO: Range = Range {
        start: Position::ZERO,
        end: Position::ZERO,
    };

    pub fn from_lsp(
        range: &tower_lsp::lsp_types::Range,
        rope: &Rope,
        encoding: &tower_lsp::lsp_types::PositionEncodingKind,
    ) -> Result<Self> {
        Ok(Range {
            start: Position::from_lsp(&range.start, rope, encoding)?,
            end: Position::from_lsp(&range.end, rope, encoding)?,
        })
    }

    pub fn into_lsp(
        &self,
        rope: &Rope,
        encoding: &tower_lsp::lsp_types::PositionEncodingKind,
    ) -> Result<tower_lsp::lsp_types::Range> {
        Ok(tower_lsp::lsp_types::Range {
            start: self.start.into_lsp(rope, encoding)?,
            end: self.end.into_lsp(rope, encoding)?,
        })
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{} - {}:{}",
            self.start.line(),
            self.start.character_byte(),
            self.end.line(),
            self.end.character_byte()
        )
    }
}

impl From<tree_sitter_c2rust::Range> for Range {
    fn from(value: tree_sitter_c2rust::Range) -> Self {
        Range {
            start: value.start_point.into(),
            end: value.end_point.into(),
        }
    }
}

impl From<Range> for std::ops::Range<tree_sitter_c2rust::Point> {
    fn from(value: Range) -> std::ops::Range<tree_sitter_c2rust::Point> {
        value.start.into()..value.end.into()
    }
}
