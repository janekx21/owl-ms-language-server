use crate::error::Result;
use crate::pos::Position;
use ropey::Rope;
use std::fmt::Display;
use std::hash::Hash;

/// Range like selection therefore endposition is exclusive
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub const ZERO: Range = Range {
        start: Position::ZERO,
        end: Position::ZERO,
    };

    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn from_lsp(
        range: &tower_lsp::lsp_types::Range,
        rope: &Rope,
        encoding: &tower_lsp::lsp_types::PositionEncodingKind,
    ) -> Result<Self> {
        Ok(Range {
            start: Position::from_lsp(range.start, rope, encoding)?,
            end: Position::from_lsp(range.end, rope, encoding)?,
        })
    }

    pub fn into_lsp(
        self,
        rope: &Rope,
        encoding: &tower_lsp::lsp_types::PositionEncodingKind,
    ) -> Result<tower_lsp::lsp_types::Range> {
        Ok(tower_lsp::lsp_types::Range {
            start: self.start.into_lsp(rope, encoding)?,
            end: self.end.into_lsp(rope, encoding)?,
        })
    }

    pub fn len_lsp(
        self,
        rope: &Rope,
        encoding: &tower_lsp::lsp_types::PositionEncodingKind,
    ) -> usize {
        let start_byte = self.start.byte_index(rope);
        let end_byte = self.end.byte_index(rope);
        let slice = rope.byte_slice(start_byte..end_byte);
        match encoding.as_str() {
            "utf-8" => slice.len_chars(),
            "utf-16" => slice.len_utf16_cu(),
            e => unimplemented!("encoding {e} not implemented"),
        }
    }

    /// Checks if the position is inside the range exclusive
    pub fn contains(&self, pos: Position) -> bool {
        // pos is above start?
        if pos.line() < self.start.line() {
            return false;
        }
        // pos is below end?
        if self.end.line() < pos.line() {
            return false;
        }
        // pos is left of start char?
        if self.start.line() == pos.line() && pos.character_byte() < self.start.character_byte() {
            return false;
        }
        // pos is right or eq to end char?
        if self.end.line() == pos.line() && self.end.character_byte() <= pos.character_byte() {
            return false;
        }

        true
    }

    /// Returns true if this range overlaps with another range.
    /// Since end positions are exclusive, ranges [a,b) and [b,c) do NOT overlap.
    pub fn overlaps(&self, other: &Range) -> bool {
        self.start < other.end && other.start < self.end
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

#[derive(Debug)]
pub struct RangeBox<T>(T, Range);

impl<T> RangeBox<T> {
    pub fn new(value: T, range: Range) -> Self {
        Self(value, range)
    }

    pub fn range(&self) -> &Range {
        &self.1
    }

    pub fn value(&self) -> &T {
        &self.0
    }

    pub fn unpack(self) -> (T, Range) {
        (self.0, self.1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to create a Position and Range — adjust these constructors to match your actual API
    fn pos(line: u32, character_byte: u32) -> Position {
        Position::new(line, character_byte)
    }

    fn range(start: Position, end: Position) -> Range {
        Range::new(start, end)
    }

    // --- Clearly inside ---

    #[test]
    fn test_contains_middle_of_range() {
        let r = range(pos(2, 5), pos(4, 10));
        assert!(
            r.contains(pos(3, 0)),
            "A position on a middle line should be inside"
        );
    }

    #[test]
    fn test_contains_same_line_middle_char() {
        let r = range(pos(2, 5), pos(2, 10));
        assert!(
            r.contains(pos(2, 7)),
            "A character between start and end on the same line should be inside"
        );
    }

    // --- Start boundary (inclusive) ---

    #[test]
    fn test_contains_at_start_position() {
        let r = range(pos(2, 5), pos(4, 10));
        assert!(
            r.contains(pos(2, 5)),
            "The exact start position should be inside (inclusive start)"
        );
    }

    #[test]
    fn test_contains_start_line_at_start_char() {
        let r = range(pos(2, 5), pos(2, 10));
        assert!(
            r.contains(pos(2, 5)),
            "Start char on start line should be inside"
        );
    }

    // --- End boundary (exclusive) ---

    #[test]
    fn test_contains_at_end_position_exclusive() {
        let r = range(pos(2, 5), pos(4, 10));
        assert!(
            !r.contains(pos(4, 10)),
            "The exact end position should be outside (exclusive end)"
        );
    }

    #[test]
    fn test_contains_end_line_at_end_char() {
        let r = range(pos(2, 5), pos(2, 10));
        assert!(
            !r.contains(pos(2, 10)),
            "End char on end line should be outside (exclusive)"
        );
    }

    #[test]
    fn test_contains_end_line_before_end_char() {
        let r = range(pos(2, 5), pos(4, 10));
        assert!(
            r.contains(pos(4, 9)),
            "A character just before end on the end line should be inside"
        );
    }

    // --- Above start line ---

    #[test]
    fn test_contains_line_above_start() {
        let r = range(pos(2, 5), pos(4, 10));
        assert!(
            !r.contains(pos(1, 5)),
            "A line above start should be outside"
        );
    }

    #[test]
    fn test_contains_line_zero_when_start_is_nonzero() {
        let r = range(pos(3, 0), pos(5, 0));
        assert!(
            !r.contains(pos(0, 0)),
            "Line 0 should be outside a range starting at line 3"
        );
    }

    // --- Below end line ---

    #[test]
    fn test_contains_line_below_end() {
        let r = range(pos(2, 5), pos(4, 10));
        assert!(!r.contains(pos(5, 0)), "A line below end should be outside");
    }

    // --- Left of start char on start line ---

    #[test]
    fn test_contains_start_line_before_start_char() {
        let r = range(pos(2, 5), pos(4, 10));
        assert!(
            !r.contains(pos(2, 4)),
            "A character left of start on the start line should be outside"
        );
    }

    #[test]
    fn test_contains_start_line_char_zero_when_start_char_nonzero() {
        let r = range(pos(2, 5), pos(4, 10));
        assert!(
            !r.contains(pos(2, 0)),
            "Char 0 should be outside when start char is 5"
        );
    }

    // --- Right of or equal to end char on end line ---

    #[test]
    fn test_contains_end_line_after_end_char() {
        let r = range(pos(2, 5), pos(4, 10));
        assert!(
            !r.contains(pos(4, 11)),
            "A character right of end on the end line should be outside"
        );
    }

    // --- Single-position degenerate range ---

    #[test]
    fn test_contains_empty_range() {
        // start == end => no position can be inside (end is exclusive and start == end)
        let r = range(pos(2, 5), pos(2, 5));
        assert!(
            !r.contains(pos(2, 5)),
            "An empty range should contain nothing"
        );
    }

    // --- overlap cases -----------------------------

    #[test]
    fn same_range_overlaps() {
        let r = Range::new(Position::new(1, 0), Position::new(1, 5));
        assert!(r.overlaps(&r));
    }

    #[test]
    fn partial_overlap_on_same_line() {
        // [0,3) vs [2,5)  -> overlap at [2,3)
        assert!(Range::new(Position::new(0, 0), Position::new(0, 3))
            .overlaps(&Range::new(Position::new(0, 2), Position::new(0, 5))));
    }

    #[test]
    fn one_range_contained_in_other() {
        // [0,10) contains [2,5)
        assert!(Range::new(Position::new(0, 0), Position::new(0, 10))
            .overlaps(&Range::new(Position::new(0, 2), Position::new(0, 5))));
        assert!(Range::new(Position::new(0, 2), Position::new(0, 5))
            .overlaps(&Range::new(Position::new(0, 0), Position::new(0, 10))));
    }

    #[test]
    fn overlap_across_lines() {
        // [1:5 -> 3:0) vs [2:0 -> 4:0)
        assert!(Range::new(Position::new(1, 5), Position::new(3, 0))
            .overlaps(&Range::new(Position::new(2, 0), Position::new(4, 0))));
    }

    #[test]
    fn overlap_reversed_argument_order() {
        let a = Range::new(Position::new(0, 0), Position::new(0, 5));
        let b = Range::new(Position::new(0, 3), Position::new(0, 8));
        assert!(a.overlaps(&b));
        assert!(b.overlaps(&a));
    }

    // --- non-overlap cases -------------------------

    #[test]
    fn adjacent_ranges_do_not_overlap() {
        // [0,5) and [5,10) share only the boundary — end is exclusive
        assert!(!Range::new(Position::new(0, 0), Position::new(0, 5))
            .overlaps(&Range::new(Position::new(0, 5), Position::new(0, 10))));
        assert!(!Range::new(Position::new(0, 5), Position::new(0, 10))
            .overlaps(&Range::new(Position::new(0, 0), Position::new(0, 5))));
    }

    #[test]
    fn disjoint_ranges_same_line() {
        // [0,3) and [5,8)
        assert!(!Range::new(Position::new(0, 0), Position::new(0, 3))
            .overlaps(&Range::new(Position::new(0, 5), Position::new(0, 8))));
    }

    #[test]
    fn disjoint_ranges_different_lines() {
        // [1:0 -> 1:10) and [3:0 -> 3:10)
        assert!(!Range::new(Position::new(1, 0), Position::new(1, 10))
            .overlaps(&Range::new(Position::new(3, 0), Position::new(3, 10))));
    }

    #[test]
    fn adjacent_across_lines_do_not_overlap() {
        // [1:0 -> 2:5) and [2:5 -> 3:0)
        assert!(!Range::new(Position::new(1, 0), Position::new(2, 5))
            .overlaps(&Range::new(Position::new(2, 5), Position::new(3, 0))));
    }
}
