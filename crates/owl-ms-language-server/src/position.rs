use ropey::Rope;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    pub fn new_from_byte_index(rope: &Rope, index: usize) -> Self {
        let line = rope.byte_to_line(index);
        let character = index - rope.line_to_byte(line);
        Self {
            line: line as u32,
            character: character as u32,
        }
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn character_byte(&self) -> u32 {
        self.character
    }

    // TODO
    // fn character_char(&self, rope: &Rope) -> u32 {
    //     // let byte_idx
    //     // rope.byte_to_char(byte_idx)
    // }

    pub fn byte_index(&self, rope: &Rope) -> usize {
        rope.line_to_byte(self.line as usize) + self.character as usize
    }

    pub fn char_index(&self, rope: &Rope) -> usize {
        rope.byte_to_char(self.byte_index(rope))
    }

    pub fn moved_right(&self, char_offset: u32, rope: &Rope) -> Self {
        let char_idx = self.char_index(rope);
        let char_idx = char_idx.saturating_add(char_offset as usize);
        let char_idx = char_idx.min(rope.len_chars() - 1); // clamp
        Self::new_from_byte_index(rope, rope.char_to_byte(char_idx))
    }

    pub fn moved_left(&self, char_offset: u32, rope: &Rope) -> Self {
        let char_idx = self.char_index(rope);
        let char_idx = char_idx.saturating_sub(char_offset as usize);
        Self::new_from_byte_index(rope, rope.char_to_byte(char_idx))
    }
}

impl From<tower_lsp::lsp_types::Position> for Position {
    fn from(value: tower_lsp::lsp_types::Position) -> Self {
        // The assumption is that this is also byte based!
        Position {
            line: value.line,
            character: value.character,
        }
    }
}

impl From<Position> for tower_lsp::lsp_types::Position {
    fn from(value: Position) -> Self {
        // The assumption is that this is also byte based!
        tower_lsp::lsp_types::Position {
            line: value.line,
            character: value.character,
        }
    }
}

impl From<tree_sitter_c2rust::Point> for Position {
    fn from(value: tree_sitter_c2rust::Point) -> Self {
        // The assumption is that this is also byte based!
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
