use ropey::Rope;
use ropey::RopeSlice;
use tree_sitter::Node;
use tree_sitter::TextProvider;

pub struct RopeProvider<'a>(pub RopeSlice<'a>);

impl<'a> TextProvider<'a> for RopeProvider<'a> {
    type I = ChunksBytes<'a>;

    fn text(&mut self, node: Node) -> Self::I {
        let fragment = self.0.byte_slice(node.start_byte()..node.end_byte());
        ChunksBytes {
            chunks: fragment.chunks(),
        }
    }
}

impl<'a> RopeProvider<'a> {
    pub(crate) fn new(value: &'a Rope) -> Self {
        RopeProvider(value.slice(..))
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
