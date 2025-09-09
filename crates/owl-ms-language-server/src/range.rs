use std::fmt::Display;

use crate::position::Position;

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

impl From<tower_lsp::lsp_types::Range> for Range {
    fn from(value: tower_lsp::lsp_types::Range) -> Self {
        Range {
            start: value.start.into(),
            end: value.end.into(),
        }
    }
}

impl From<Range> for tower_lsp::lsp_types::Range {
    fn from(value: Range) -> tower_lsp::lsp_types::Range {
        tower_lsp::lsp_types::Range {
            start: value.start.into(),
            end: value.end.into(),
        }
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
