#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

impl From<tower_lsp::lsp_types::Position> for Position {
    fn from(value: tower_lsp::lsp_types::Position) -> Self {
        Position {
            line: value.line,
            character: value.character,
        }
    }
}

impl From<Position> for tower_lsp::lsp_types::Position {
    fn from(value: Position) -> Self {
        tower_lsp::lsp_types::Position {
            line: value.line,
            character: value.character,
        }
    }
}

impl From<tree_sitter_c2rust::Point> for Position {
    fn from(value: tree_sitter_c2rust::Point) -> Self {
        Position {
            line: value.row as u32,
            character: value.column as u32,
        }
    }
}

impl From<Position> for tree_sitter_c2rust::Point {
    fn from(value: Position) -> tree_sitter_c2rust::Point {
        tree_sitter_c2rust::Point {
            row: value.line as usize,
            column: value.character as usize,
        }
    }
}
