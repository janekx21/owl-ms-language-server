use std::fmt::Display;

use crate::position::Position;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
// Range like selection therefore endposition is exclusive
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{} - {}:{}",
            self.start.line, self.start.character, self.end.line, self.end.character
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

/// Function that checks if two ranges overlap.
/// Overlapping means that they share at least one character.
pub fn range_overlaps(a: &Range, b: &Range) -> bool {
    // If a ends before b starts (strictly before)
    if a.end.line < b.start.line
        || (a.end.line == b.start.line && a.end.character <= b.start.character)
    {
        return false;
    }

    // If b ends before a starts (strictly before)
    if b.end.line < a.start.line
        || (b.end.line == a.start.line && b.end.character <= a.start.character)
    {
        return false;
    }

    // Otherwise, they overlap
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper function to create a range
    fn range(start_line: u32, start_char: u32, end_line: u32, end_char: u32) -> Range {
        Range {
            start: Position {
                line: start_line,
                character: start_char,
            },
            end: Position {
                line: end_line,
                character: end_char,
            },
        }
    }

    #[test]
    fn test_same_line_overlaps() {
        // Case 1: Ranges overlap on the same line
        let a = range(1, 5, 1, 10);
        let b = range(1, 7, 1, 15);
        assert!(range_overlaps(&a, &b));
        assert!(range_overlaps(&b, &a)); // Symmetry check

        // Case 2: One range contains the other on the same line
        let a = range(1, 5, 1, 15);
        let b = range(1, 7, 1, 10);
        assert!(range_overlaps(&a, &b));
        assert!(range_overlaps(&b, &a)); // Symmetry check

        // Case 3: Ranges touch at exact boundary (no overlap by definition since they don't share a character)
        let a = range(1, 5, 1, 10);
        let b = range(1, 10, 1, 15);
        assert!(!range_overlaps(&a, &b));
        assert!(!range_overlaps(&b, &a)); // Symmetry check

        // Case 4: No overlap on same line
        let a = range(1, 5, 1, 10);
        let b = range(1, 15, 1, 20);
        assert!(!range_overlaps(&a, &b));
        assert!(!range_overlaps(&b, &a)); // Symmetry check
    }

    #[test]
    fn test_multi_line_overlaps() {
        // Case 1: Ranges overlap across multiple lines
        let a = range(1, 5, 3, 10);
        let b = range(2, 7, 4, 15);
        assert!(range_overlaps(&a, &b));
        assert!(range_overlaps(&b, &a)); // Symmetry check

        // Case 2: One range contains the other across multiple lines
        let a = range(1, 5, 5, 15);
        let b = range(2, 7, 4, 10);
        assert!(range_overlaps(&a, &b));
        assert!(range_overlaps(&b, &a)); // Symmetry check

        // Case 3: Ranges touch at line boundary with character check
        let a = range(1, 5, 2, 0);
        let b = range(2, 0, 3, 15);
        assert!(!range_overlaps(&a, &b));
        assert!(!range_overlaps(&b, &a)); // Symmetry check

        // Case 4: No overlap across multiple lines
        let a = range(1, 5, 2, 10);
        let b = range(3, 0, 4, 15);
        assert!(!range_overlaps(&a, &b));
        assert!(!range_overlaps(&b, &a)); // Symmetry check
    }

    #[test]
    fn test_boundary_cases() {
        // Case 1: End of first range is exactly at start of second range (no overlap)
        let a = range(1, 5, 2, 7);
        let b = range(2, 7, 3, 10);
        assert!(!range_overlaps(&a, &b));
        assert!(!range_overlaps(&b, &a)); // Symmetry check

        // Case 2: Ranges meet at line boundary but don't share character
        let a = range(1, 0, 1, 10);
        let b = range(1, 10, 1, 20);
        assert!(!range_overlaps(&a, &b));
        assert!(!range_overlaps(&b, &a)); // Symmetry check

        // Case 3: Ranges overlap by exactly one character
        let a = range(1, 5, 1, 10);
        let b = range(1, 9, 1, 15);
        assert!(range_overlaps(&a, &b));
        assert!(range_overlaps(&b, &a)); // Symmetry check

        // Case 4: Ranges overlap at line boundary
        let a = range(1, 5, 2, 5);
        let b = range(2, 0, 3, 10);
        assert!(range_overlaps(&a, &b));
        assert!(range_overlaps(&b, &a)); // Symmetry check
    }

    #[test]
    fn test_edge_cases() {
        // Case 1: Identical ranges
        let a = range(1, 5, 2, 10);
        let b = range(1, 5, 2, 10);
        assert!(range_overlaps(&a, &b));
    }

    #[test]
    fn test_complex_multi_line_scenarios() {
        // Case 1: First range spans multiple lines, second range is within
        let a = range(1, 0, 5, 0);
        let b = range(2, 5, 3, 10);
        assert!(range_overlaps(&a, &b));
        assert!(range_overlaps(&b, &a)); // Symmetry check

        // Case 2: Ranges overlap at single point on line boundary
        let a = range(1, 0, 2, 5);
        let b = range(2, 5, 3, 0);
        assert!(!range_overlaps(&a, &b)); // They touch but don't share a character
        assert!(!range_overlaps(&b, &a)); // Symmetry check

        // Case 3: Ranges overlap on boundary with shared character
        let a = range(1, 0, 2, 6);
        let b = range(2, 5, 3, 0);
        assert!(range_overlaps(&a, &b)); // They share character(s) on line 2
        assert!(range_overlaps(&b, &a)); // Symmetry check

        // Case 4: Completely separate ranges
        let a = range(1, 0, 2, 0);
        let b = range(3, 0, 4, 0);
        assert!(!range_overlaps(&a, &b));
        assert!(!range_overlaps(&b, &a)); // Symmetry check
    }
}

// Function that checks if two ranges share any lines.
// It returns true if the ranges have at least one line in common,
// regardless of character positions.
pub fn lines_overlap(a: &Range, b: &Range) -> bool {
    // If range a ends before range b starts (line-wise)
    if a.end.line < b.start.line {
        return false;
    }

    // If range b ends before range a starts (line-wise)
    if b.end.line < a.start.line {
        return false;
    }

    // Otherwise, they share at least one line
    true
}

/// is one range "inner" inside the range "outer"
pub fn range_exclusive_inside(inner: &Range, outer: &Range) -> bool {
    inner.start.line > outer.start.line && inner.end.line < outer.end.line
}
