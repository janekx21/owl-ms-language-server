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
    iri::{Iri, ToIri},
    pos::Position,
    queries::{treesitter_highlight_capture_into_semantic_token_type_index, TTL_ALL_QUERIES},
    range::{Range, RangeBox},
    rope_provider::RopeProvider,
    workspace::{
        changes_from_lsp, trim_full_iri_rope_slice, Annotation, Diagnostic, DocumentId,
        FormattingSettings, FrameInfo, FrameType, Highlights, HoverResult, IriAtPosition,
        IriDefinition, KeywordAction, OntologyDocument, ParsedDocument, RenameInfo, Workspace,
    },
};

pub static LANGUAGE_TTL: LazyLock<Language> = LazyLock::new(|| tree_sitter_turtle::LANGUAGE.into());

thread_local! {
    static TTL_PARSER: LazyLock<RefCell<Parser>> = LazyLock::new(|| {
        let mut parser = Parser::new();
        parser
            .set_language(&LANGUAGE_TTL)
            .expect("the language to be valid");
        parser.set_logger(Some(Box::new(|type_, str| match type_ {
            tree_sitter_c2rust::LogType::Parse => trace!(target: "ttl tree-sitter-parse", "{str}"),
            tree_sitter_c2rust::LogType::Lex => trace!(target: "ttl tree-sitter-lex", "{str}"),
        })));

        RefCell::new(parser)
    });
}

#[derive(Debug)]
pub struct InternalTtlDocument {
    id: DocumentId,
    parsed_document: ParsedDocument,
    prefixes: HashMap<String, RangeBox<String>>,
    definitions: Vec<RangeBox<IriDefinition>>,
}

impl InternalTtlDocument {
    pub fn new(uri: Url, version: i32, text: String) -> Self {
        // TODO what should happen with non local files?
        let path = uri.to_file_path().expect("URL should be a file path");
        Self::new_with_path(uri, version, text, path)
    }

    pub fn new_with_path(uri: Url, version: i32, text: String, path: PathBuf) -> Self {
        let id = DocumentId { path, uri, version };

        let tree = timeit("create_document (ttl) / parse", || {
            TTL_PARSER.with(|parser| {
                parser
                    .borrow_mut() // This shoud exit scope after the with
                    .parse(&text, None)
                    .expect("language to be set, no timeout to be used, no cancellation flag")
            })
        });

        let rope = Rope::from(text);
        let parsed_document = ParsedDocument::new(tree, rope);

        let prefixes = gen_prefixes(&parsed_document);

        let definitions = gen_definitions(&parsed_document, &prefixes);

        Self {
            id,
            parsed_document,
            prefixes,
            definitions,
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

        let InternalTtlDocument {
            id,
            parsed_document,
            prefixes,
            definitions,
        } = self;

        let changes = changes_from_lsp(params, encoding, parsed_document.rope());

        // Note that these ranges are in the pre edit form
        for change in &changes {
            debug!("Updating changed range (pre edit) {change:?}");
        }

        let (parsed_document, old_tree) = TTL_PARSER.with(|parser| {
            parsed_document.edit_parsed_document(changes.iter(), &mut parser.borrow_mut())
        })?;

        // Increment ID
        let id = DocumentId {
            version: new_version,
            ..id
        };

        let prefixes = gen_prefixes(&parsed_document);

        let definitions = gen_definitions(&parsed_document, &prefixes);

        let doc = Self {
            id,
            parsed_document,
            prefixes,
            definitions,
        };

        Ok(doc)
    }
}

fn gen_prefixes(parsed_document: &ParsedDocument) -> HashMap<String, RangeBox<String>> {
    let mut prefixes = HashMap::new();
    for ele in parsed_document.query(&TTL_ALL_QUERIES.prefix) {
        let (prefix_name_c, full_iri_c) = ele
            .captures
            .into_iter()
            .next_tuple()
            .expect("should match query");

        let mut name = prefix_name_c.node.text.to_string();
        if name.ends_with(':') {
            name.pop();
        }

        let iri = trim_full_iri_rope_slice(full_iri_c.node.text).to_string();
        // TODO range is zero!
        prefixes.insert(name, RangeBox::new(iri, Range::ZERO));
    }
    prefixes
}

fn gen_definitions(
    parsed_document: &ParsedDocument,
    prefixes: &HashMap<String, RangeBox<String>>,
) -> Vec<RangeBox<IriDefinition>> {
    parsed_document
        .query(&TTL_ALL_QUERIES.definition)
        .into_iter()
        .map(|m| {
            let iri_c = m.captures.first().expect("should be one");
            let iri = trim_full_iri_rope_slice(iri_c.node.text).to_iri();
            let iri = iri.try_to_full(prefixes).unwrap_or(iri);
            debug!("Found ttl {iri}");
            RangeBox::new(
                IriDefinition {
                    iri,
                    kind: FrameType::Unknown, // TODO parse frame type
                },
                iri_c.node.range,
            )
        })
        .collect()
}

impl PartialEq for InternalTtlDocument {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for InternalTtlDocument {}

impl OntologyDocument for InternalTtlDocument {
    fn path(&self) -> &Path {
        self.id.path.as_path()
    }

    fn uri(&self) -> &Url {
        &self.id.uri
    }

    fn version(&self) -> i32 {
        self.id.version
    }

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
        self.definitions.iter().collect()
    }

    fn references(&self) -> Vec<&RangeBox<Iri>> {
        vec![] // TODO
    }

    fn directly_imports(&self) -> Vec<&Url> {
        vec![] // TODO
    }

    fn directly_references_doc(&self) -> Vec<&Url> {
        vec![] // TODO
    }

    fn local_diagnostics(&self) -> &[Diagnostic] {
        &[] // TODO
    }

    fn iri_locations(&self) -> HashMap<&Iri, &Vec<RangeBox<()>>> {
        HashMap::new() // TODO
    }

    fn abbreviated_iri_to_full_iri(&self, iri: &Iri) -> Option<Iri> {
        iri.try_to_full(&self.prefixes)
    }

    fn full_iri_to_abbreviated_iri(&self, full_iri: &Iri) -> Option<String> {
        full_iri.try_to_abbriviated(&self.prefixes).map(|s| {
            if s.contains(':') {
                s
            } else {
                format!(":{s}") // We have to prefix!
            }
        })
    }

    fn prefixes(&self) -> HashMap<String, String> {
        HashMap::new()
    }

    fn range(&self) -> Range {
        self.parsed_document.tree().root_node().range().into()
    }

    fn iri_at(&self, pos: Position) -> Option<RangeBox<IriAtPosition>> {
        None // TODO
    }

    fn rename_range(&self, pos: Position) -> Option<Range> {
        None // TODO
    }

    fn rename_info_at(&self, pos: Position, new_name: &str) -> Result<Option<RenameInfo>> {
        Ok(None) // TODO
    }

    fn rename_edits(&self, rename_info: &RenameInfo) -> Vec<RangeBox<String>> {
        match rename_info {
            RenameInfo::Iri(iri_rename_info) => {
                self.parsed_document
                    .query(&TTL_ALL_QUERIES.iri_query_all)
                    .into_iter()
                    .filter_map(|ele| {
                        let iri_c = ele.captures.first().expect("should match once"); // TODO trim and to full iri

                        let iri = trim_full_iri_rope_slice(iri_c.node.text).to_iri();
                        let iri = self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);

                        // TODO node frame type check?
                        if iri == iri_rename_info.full_iri {
                            let value = iri_rename_info
                                .new_iri
                                .as_ref()
                                .and_then(|i| self.full_iri_to_abbreviated_iri(i))
                                .unwrap_or(iri_rename_info.original.clone());

                            Some(RangeBox::new(value, iri_c.node.range))
                        } else {
                            None
                        }
                    })
                    .collect()
            }
            RenameInfo::Prefix(_, _) => vec![], // TODO
        }
    }

    fn keyword_actions_at(&self, pos: Position) -> Vec<KeywordAction> {
        vec![] // TODO
    }

    fn all_iris_in_range(&self, range: Range) -> Vec<RangeBox<Iri>> {
        vec![] // TODO
    }

    fn formatted(&self, options: &FormattingSettings) -> String {
        self.parsed_document.rope().to_string() // TODO
    }

    fn hover(&self, pos: Position) -> Option<HoverResult> {
        None // TODO
    }

    fn highlights(&self, range: Range) -> Highlights {
        let query_source = tree_sitter_turtle::HIGHLIGHTS_QUERY;
        let query = Query::new(&LANGUAGE_TTL, query_source).expect("valid query expect");
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

        nodes
            .iter()
            .map(|(node, type_index)| RangeBox::new(*type_index, node.range().into()))
            .collect()
    }

    fn get_keyword_competions_at(&self, pos: Position) -> Vec<String> {
        vec![] // TODO
    }

    fn get_iri_completions_at(
        &self,
        pos: Position,
        workspace: &Workspace,
    ) -> Vec<(String, String, String, bool)> {
        vec![] // TODO
    }

    fn statistic(&self) -> String {
        "Not implemented".into()
    }

    fn add_annotation_edits(&self, annotation: Annotation) -> Vec<RangeBox<String>> {
        vec![]
    }
}
