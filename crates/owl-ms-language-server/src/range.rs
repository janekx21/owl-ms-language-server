use crate::position::Position;

#[derive(Clone, Copy, Debug)]
pub struct Range {
    pub start: Position,
    pub end: Position,
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

impl From<tree_sitter::Range> for Range {
    fn from(value: tree_sitter::Range) -> Self {
        Range {
            start: value.start_point.into(),
            end: value.end_point.into(),
        }
    }
}

impl From<Range> for std::ops::Range<tree_sitter::Point> {
    fn from(value: Range) -> std::ops::Range<tree_sitter::Point> {
        value.start.into()..value.end.into()
    }
}

/// only looks at the lines
pub fn range_overlaps(a: &Range, b: &Range) -> bool {
    !(a.start.line > b.end.line || a.end.line < b.start.line)
        || (b.start.line <= a.end.line && b.end.line >= a.start.line)
}

/// is one range "inner" inside the range "outer"
pub fn range_exclusive_inside(inner: &Range, outer: &Range) -> bool {
    inner.start.line > outer.start.line && inner.end.line < outer.end.line
}
