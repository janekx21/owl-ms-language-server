use ropey::Rope;
use tree_sitter::Node;
use tree_sitter::TextProvider;

pub struct RopeProvider<'a>(pub &'a Rope);

impl<'a> TextProvider<&'a [u8]> for RopeProvider<'a> {
    type I = ChunksBytes<'a>;

    fn text(&mut self, node: Node) -> Self::I {
        let fragment = self.0.byte_slice(node.start_byte()..node.end_byte());
        ChunksBytes {
            chunks: fragment.chunks(),
        }
    }
}

impl<'a> RopeProvider<'a> {
    pub fn new(value: &'a Rope) -> Self {
        RopeProvider(value)
    }

    pub fn chunk_callback(&self, byte_idx: usize) -> &[u8] {
        // TODO Why is reparsing this not O(log n)?
        // Maybe the rope traversal takes too long.
        // Report(i, j) complexity is in O(j + log N)
        // See https://en.wikipedia.org/wiki/Rope_(data_structure)
        if byte_idx > self.0.len_bytes() {
            return b""; // out of bounds
        }
        let (chunk, chunk_byte_idx, _, _) = self.0.chunk_at_byte(byte_idx);
        let start = byte_idx - chunk_byte_idx;
        &chunk.as_bytes()[start..]
    }
}

impl<'a> From<&'a Rope> for RopeProvider<'a> {
    fn from(value: &'a Rope) -> Self {
        RopeProvider::new(value)
    }
}

// Thanks to the helix team
// https://github.com/helix-editor/helix/blob/master/helix-core/src/syntax.rs#L1747
pub struct ChunksBytes<'a> {
    chunks: ropey::iter::Chunks<'a>,
}
impl<'a> Iterator for ChunksBytes<'a> {
    type Item = &'a [u8];
    fn next(&mut self) -> Option<Self::Item> {
        self.chunks.next().map(str::as_bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    #[test]
    fn test_rope_chunk_callback() {
        let rope = Rope::from_str("0123456789".repeat(10000).as_str());
        let rope_provider = RopeProvider::new(&rope);

        let chunk = rope_provider.chunk_callback(5);
        assert_eq!(chunk.len(), 984 - 5);
        assert!(chunk.starts_with(b"5678"));
    }

    #[test]
    fn test_rope_chunk_callback_end() {
        let rope = Rope::from_str("");
        let rope_provider = RopeProvider::new(&rope);

        let chunk = rope_provider.chunk_callback(1);
        assert_eq!(chunk.len(), 0);
    }
}
