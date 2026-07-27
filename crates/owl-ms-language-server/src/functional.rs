use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    sync::LazyLock,
};

use itertools::Itertools;
use log::{debug, trace};
use ropey::Rope;
use tower_lsp::lsp_types::{DidChangeTextDocumentParams, PositionEncodingKind, Url};
use tree_sitter_c2rust::{Language, Parser, Query, QueryCursor, StreamingIterator};

use crate::{
    debugging::timeit,
    error::{Error, Result},
    pos::Position,
    queries::treesitter_highlight_capture_into_semantic_token_type_index,
    range::{Range, RangeBox},
    rope_provider::RopeProvider,
    workspace::{
        changes_from_lsp, Diagnostic, DocumentId, FormattingSettings, FrameInfo, Highlights,
        HoverResult, Iri, IriAtPosition, IriDefinition, KeywordAction, OntologyDocument,
        ParsedDocument, RenameInfo, Workspace,
    },
};

pub static LANGUAGE_OFN: LazyLock<Language> = LazyLock::new(|| tree_sitter_owl_fn::LANGUAGE.into());

thread_local! {
    static OFN_PARSER: LazyLock<RefCell<Parser>> = LazyLock::new(|| {
        let mut parser = Parser::new();
        parser
            .set_language(&LANGUAGE_OFN)
            .expect("the language to be valid");
        parser.set_logger(Some(Box::new(|type_, str| match type_ {
            tree_sitter_c2rust::LogType::Parse => trace!(target: "ofn tree-sitter-parse", "{str}"),
            tree_sitter_c2rust::LogType::Lex => trace!(target: "ofn tree-sitter-lex", "{str}"),
        })));

        RefCell::new(parser)
    });
}

#[derive(Debug)]
pub struct InternalOfnDocument {
    id: DocumentId,
    parsed_document: ParsedDocument,
    // TODO
    // pub queried_document: QueriedDocument,
    // pub stage2: Stage2Document,
}

impl InternalOfnDocument {
    pub fn new(uri: Url, version: i32, text: String) -> Self {
        // TODO what should happen with non local files?
        let path = uri.to_file_path().expect("URL should be a file path");
        Self::new_with_path(uri, version, text, path)
    }

    pub fn new_with_path(uri: Url, version: i32, text: String, path: PathBuf) -> Self {
        let id = DocumentId { path, uri, version };

        let tree = timeit("create_document (ofn) / parse", || {
            OFN_PARSER.with(|parser| {
                parser
                    .borrow_mut() // This shoud exit scope after the with
                    .parse(&text, None)
                    .expect("language to be set, no timeout to be used, no cancellation flag")
            })
        });

        // TODO
        let rope = Rope::from(text);
        let parsed_document = ParsedDocument::new(tree, rope);

        // let queried_document: QueriedDocument = parsed_document.into_queried();

        // let stage2: Stage2Document = queried_document.analyze(&parsed_document, &id);

        // debug!("Stage2Document -> InternalDocument");

        Self {
            id,
            parsed_document,
            // queried_document,
            // stage2,
        }
    }

    pub fn edit_inner(
        self, // TODO #30 do a mut instead so the analytics do not get dropped
        params: DidChangeTextDocumentParams,
        encoding: &PositionEncodingKind,
    ) -> Result<Self> {
        let new_version = params.text_document.version;
        if self.version() >= new_version {
            return Ok(self); // no change needed
        }

        if params
            .content_changes
            .iter()
            .any(|change| change.range.is_none())
        {
            // Change the whole file
            return Err(Error::LspFeatureNotSupported(
                "Whole file (null range) change event",
            ));
        }

        debug!("content changes {:#?}", params.content_changes);

        let InternalOfnDocument {
            id,
            parsed_document,
        } = self;

        let changes = changes_from_lsp(params, encoding, parsed_document.rope());

        // Note that these ranges are in the pre edit form
        for change in &changes {
            debug!("Updating changed range (pre edit) {change:?}");
        }

        let (parsed_document, old_tree) = OFN_PARSER.with(|parser| {
            parsed_document.edit_parsed_document(changes.iter(), &mut parser.borrow_mut())
        })?;

        // Increment ID
        let id = DocumentId {
            version: new_version,
            ..id
        };

        // This is a combination of syntax and text changes
        // let mut post_change_ranges: &[Range] =
        //     &post_change_ranges(&changes, &parsed_document, &old_tree);

        // debug!("Post change ranges: {post_change_ranges:#?}");

        // let dirty_prefix = timeit("document.edit / queries", || {
        //     queried_document.update(&changes, post_change_ranges, &parsed_document)
        // });

        // // The problem is that the references and definitions (and other stuff) depends on
        // // prefixes. So the change in a prefix can change a lot of references that are not
        // // located at the prefix.
        // // Solution 1: Remove the dependency and move the resolution of abbriv iri -> full iri
        // // into a later step.
        // // Solution 2: Mark all references dirty when ever a prefix changes, which is not often.
        // // ==========
        // // I Chose Solution 2
        // // Do a whole new analysis when the prefixes change!
        // if dirty_prefix {
        //     info!("document.edit Dirty prefix. New post change range is the max range.");
        //     post_change_ranges = &[Range::FULL_RANGE];
        // }

        // timeit("document.edit / analyze", || {
        //     stage2.update(
        //         &changes,
        //         post_change_ranges,
        //         &parsed_document,
        //         &queried_document,
        //         &id,
        //     );
        // });

        let doc = Self {
            id,
            parsed_document,
            // queried_document,
            // stage2,
        };

        Ok(doc)
    }
}

impl PartialEq for InternalOfnDocument {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for InternalOfnDocument {}

impl OntologyDocument for InternalOfnDocument {
    #[doc = " The file path of this text file"]
    fn path(&self) -> &Path {
        self.id.path.as_path()
    }

    #[doc = " The file url of this text file"]
    fn uri(&self) -> &Url {
        &self.id.uri
    }

    #[doc = " LSP version. Gets incremented when editing. Not to be confused with the ontology version."]
    fn version(&self) -> i32 {
        self.id.version
    }

    #[doc = " Text content stored as a rope"]
    fn rope(&self) -> &Rope {
        self.parsed_document.rope()
    }

    fn frame_infos(&self) -> Vec<&FrameInfo> {
        vec![] // TODO
    }

    fn find_frame_info(&self, iri: &Iri) -> Option<FrameInfo> {
        None // TODO
    }

    fn ontology_iri(&self) -> Option<Iri> {
        None // TODO
    }

    fn version_iri(&self) -> Option<Iri> {
        None // TODO
    }

    fn definitions(&self) -> Vec<&RangeBox<IriDefinition>> {
        vec![] // TODO
    }

    fn references(&self) -> Vec<&RangeBox<Iri>> {
        vec![] // TODO
    }

    #[doc = " OWL ontolgies that are imported by this ontology."]
    #[doc = " There are also indirect imports, but they need to be resolved in the workspace,"]
    #[doc = " because a single document has no context of the workspace."]
    fn directly_imports(&self) -> Vec<&Url> {
        vec![] // TODO
    }

    #[doc = " This includes prefixes, references and imports"]
    fn directly_references_doc(&self) -> Vec<&Url> {
        vec![] // TODO
    }

    #[doc = " Errors/Diagnostics that are created by this document alone. This"]
    #[doc = " includes e.g. syntax errors."]
    fn local_diagnostics(&self) -> &[Diagnostic] {
        &[] // TODO
    }

    #[doc = " Map of IRIs and where to find them"]
    fn iri_locations(&self) -> HashMap<&Iri, &Vec<RangeBox<()>>> {
        HashMap::new() // TODO
    }

    #[doc = " Taking a (relative) abbreviated or simple IRI and resolving the (absolute) full IRI."]
    #[doc = " The reverse of [`full_iri_to_abbreviated_iri`]."]
    fn abbreviated_iri_to_full_iri(&self, iri: &str) -> Option<Iri> {
        None // TODO
    }

    #[doc = " Taking a (absolute) full IRI and by prefixing it making it shorter into a (relative)"]
    #[doc = " abbriviated or simple IRI."]
    #[doc = " The reverse of [`abbreviated_iri_to_full_iri`]."]
    fn full_iri_to_abbreviated_iri(&self, full_iri: &str) -> Option<String> {
        None // TODO
    }

    #[doc = " Prefix map with key prefix and value prefix target"]
    fn prefixes(&self) -> HashMap<String, String> {
        HashMap::new()
    }

    #[doc = " The range of the document content."]
    fn range(&self) -> Range {
        self.parsed_document.tree().root_node().range().into()
    }

    #[doc = " Takes a positon and returns an IRI and a range when one is found at that position."]
    #[doc = " It does not include the \'<\' \'>\' chars of a full iri."]
    fn iri_at(&self, pos: Position) -> Option<RangeBox<IriAtPosition>> {
        None // TODO
    }

    #[doc = " The range of an IRI that can be renamed. Does not include \'<\' \'>\' or prefixes."]
    #[doc = " So it should trigger like this:"]
    #[doc = " ```txt"]
    #[doc = " foo:bar    --->    \"bar\""]
    #[doc = "   ^"]
    #[doc = "  Cursor"]
    #[doc = " ```"]
    fn rename_range(&self, pos: Position) -> Option<Range> {
        None // TODO
    }

    #[doc = " Generate informations for renaming"]
    fn rename_info_at(&self, pos: Position, new_name: &str) -> Result<Option<RenameInfo>> {
        Ok(None) // TODO
    }

    #[doc = " Takes a [`RenameInfo`] and returns the edits that will get performed by the rename"]
    fn rename_edits(&self, rename_info: &RenameInfo) -> Vec<RangeBox<String>> {
        vec![] // TODO
    }

    #[doc = " Get actions for creating keywords at a position"]
    fn keyword_actions_at(&self, pos: Position) -> Vec<KeywordAction> {
        vec![] // TODO
    }

    #[doc = " Get all iris that are in a range"]
    fn all_iris_in_range(&self, range: Range) -> Vec<RangeBox<Iri>> {
        vec![] // TODO
    }

    #[doc = " A formatted version of the document"]
    fn formatted(&self, options: &FormattingSettings) -> String {
        self.parsed_document.rope().to_string() // TODO
    }

    #[doc = " Hover information at a position"]
    fn hover(&self, pos: Position) -> Option<HoverResult> {
        None // TODO
    }

    #[doc = " All highlights"]
    fn highlights(&self, range: Range) -> Highlights {
        let query_source = tree_sitter_owl_fn::HIGHLIGHTS_QUERY;
        let query = Query::new(&LANGUAGE_OFN, query_source).expect("valid query expect");
        let mut query_cursor = QueryCursor::new();
        if range != Range::FULL_RANGE {
            query_cursor.set_point_range(range.into());
        }
        let matches = query_cursor.matches(
            &query,
            self.parsed_document.tree().root_node(),
            RopeProvider::new(self.rope()),
        );

        let nodes = matches
            .map_deref(|m| m.captures)
            .flatten()
            .map(|c| {
                (
                    c.node,
                    treesitter_highlight_capture_into_semantic_token_type_index(
                        query.capture_names()[c.index as usize],
                    ),
                )
            })
            .collect_vec();

        // TODO this is not needed right?
        // nodes.sort_unstable_by_key(|(n, _)| n.start_byte());

        nodes
            .iter()
            .map(|(node, type_index)| RangeBox::new(*type_index, node.range().into()))
            .collect()
    }

    #[doc = " A keyword completion"]
    fn get_keyword_competions_at(&self, pos: Position) -> Vec<String> {
        vec![] // TODO
    }

    #[doc = " An IRI completion at a position"]
    fn get_iri_completions_at(
        &self,
        pos: Position,
        workspace: &Workspace,
    ) -> Vec<(String, String, String)> {
        vec![] // TODO
    }
}
