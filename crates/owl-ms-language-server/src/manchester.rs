use crate::consts::{
    child_keywords_for_kind, keyword_hover_info, DECIMAL_IRI, FLOAT_IRI, INTEGER_IRI, STRING_IRI,
};
use crate::error::{Error, Result, ResultIterator};
use crate::iri::{Iri, ToIri};
use crate::pos::Position;
use crate::queries::{self, treesitter_highlight_capture_into_semantic_token_type_index};
use crate::range::{Change, RangeBox};
use crate::workspace::{
    build_iri_locations, capture_by_name, changes_from_lsp, edit_vec_rb, extend_vec_rb,
    iri_to_parent_url, iri_to_parent_url_str, node_text, post_change_ranges, retain_vec_rb,
    retain_vec_rb_on_remove, trim_full_iri_rope_slice, trim_string_value, word_before_character,
    Annotation, Diagnostic, DocumentId, FormattingSettings, FrameInfo, FrameType, Highlights,
    HoverResult, IriAtPosition, IriDefinition, KeywordAction, Location, OntologyDocument,
    OntologyId, ParsedDocument, RenameInfo, UnwrappedQueryMatch, Workspace,
};
use crate::{
    debugging::timeit, queries::ALL_QUERIES, range::Range, rope_provider::RopeProvider,
    LANGUAGE_OMN,
};
use itertools::Itertools;
use log::{debug, error, info, trace};
use pretty::RcDoc;
use rayon::iter::{IntoParallelRefIterator, IntoParallelRefMutIterator, ParallelIterator};
use rayon::slice::ParallelSliceMut;
use ropey::Rope;
use std::cell::RefCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::path::Path;
use std::string::ToString;
use std::sync::LazyLock;
use std::{collections::HashMap, fmt::Display, path::PathBuf};
use tower_lsp::lsp_types::{DidChangeTextDocumentParams, PositionEncodingKind, Url};
use tree_sitter_c2rust::{Node, Parser, Query, QueryCursor, StreamingIterator, Tree};

thread_local! {
pub static GLOBAL_OMN_PARSER: LazyLock<RefCell<Parser>> = LazyLock::new(|| {
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE_OMN)
        .expect("the language to be valid");
    parser.set_logger(Some(Box::new(|type_, str| match type_ {
        tree_sitter_c2rust::LogType::Parse => trace!(target: "omn tree-sitter-parse", "{str}"),
        tree_sitter_c2rust::LogType::Lex => trace!(target: "omn tree-sitter-lex", "{str}"),
    })));

    RefCell::new(parser)
});
}

#[derive(Debug)]
pub struct InternalOmnDocument {
    id: DocumentId,
    parsed_document: ParsedDocument,
    pub queried_document: QueriedDocument,
    pub stage2: Stage2Document,
}

impl Display for InternalOmnDocument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "InternalDocument {{ path = \"{}\", url = \"{}\" version = {}, rope.len_bytes = {}}}",
            self.path().display(),
            self.uri(),
            self.version(),
            self.rope().len_bytes()
        )
    }
}

impl core::hash::Hash for InternalOmnDocument {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path().hash(state);
        Hash::hash(&self.version(), state);
    }
}
impl Eq for InternalOmnDocument {}
impl PartialEq for InternalOmnDocument {
    fn eq(&self, other: &Self) -> bool {
        self.rope() == other.rope()
    }
}

impl OntologyDocument for InternalOmnDocument {
    fn path(&self) -> &Path {
        &self.id.path
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
        self.stage2.all_frame_infos.values().collect()
    }

    fn find_frame_info(&self, iri: &Iri) -> Option<FrameInfo> {
        self.stage2.all_frame_infos.get(iri).cloned()
    }

    fn definitions(&self) -> Vec<&RangeBox<IriDefinition>> {
        self.stage2.definitions.iter().collect()
    }

    fn ontology_iri(&self) -> Option<Iri> {
        self.queried_document
            .ontology_id
            .as_ref()
            .map(|rb| rb.value().0.clone())
    }

    fn version_iri(&self) -> Option<Iri> {
        self.queried_document
            .ontology_id
            .as_ref()
            .and_then(|rb| rb.value().1.clone())
    }

    fn references(&self) -> Vec<&RangeBox<Iri>> {
        self.stage2.references.iter().collect()
    }

    fn directly_imports(&self) -> Vec<&Url> {
        self.stage2.directly_reachable_import_urls.iter().collect()
    }

    #[doc = " This includes prefixes, references and imports"]
    fn directly_references_doc(&self) -> Vec<&Url> {
        self.stage2
            .directly_reachable_import_urls
            .iter()
            .chain(self.stage2.directly_reachable_other_urls.iter())
            .collect()
    }

    fn local_diagnostics(&self) -> &[Diagnostic] {
        &self.stage2.local_diagnostics
    }

    fn iri_locations(&self) -> HashMap<&Iri, &Vec<RangeBox<()>>> {
        self.stage2.iri_locations.iter().collect()
    }

    fn abbreviated_iri_to_full_iri(&self, iri: &Iri) -> Option<Iri> {
        self.queried_document.abbreviated_iri_to_full_iri(iri)
    }

    fn full_iri_to_abbreviated_iri(&self, full_iri: &Iri) -> Option<String> {
        self.queried_document
            .prefixes
            .iter()
            .map(|(k, v)| (k.clone(), v.value().clone()))
            .filter_map(
                |(prefix, url)| match full_iri.as_str().split_once(url.as_str()) {
                    Some(("", post)) if prefix.is_empty() => Some(post.to_string()),
                    Some(("", post)) => Some(prefix + ":" + post),
                    Some(_) | None => None,
                },
            )
            .sorted_by_key(String::len)
            .next()
    }

    fn prefixes(&self) -> HashMap<String, String> {
        self.queried_document
            .prefixes
            .iter()
            .map(|(k, v)| (k.clone(), v.value().clone()))
            .collect()
    }

    fn formatted(&self, options: &FormattingSettings) -> String {
        let root = self.tree().root_node();
        let doc = to_doc(&root, self.rope(), options);
        debug!("doc:\n{doc:#?}");
        doc.pretty(options.ruler_width as usize).to_string()
    }

    fn hover(&self, pos: Position) -> Option<HoverResult> {
        let node = self
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos.into(), pos.into())?;
        let range: Range = node.range().into();
        match node.kind() {
            "full_iri" => Some(HoverResult::Iri {
                iri: trim_full_iri_rope_slice(node_text(&node, self.rope())).to_iri(),
                range,
            }),
            "simple_iri" | "abbreviated_iri" => {
                let iri = node_text(&node, self.rope());
                let full_iri = self
                    .abbreviated_iri_to_full_iri(&iri.to_iri())
                    .unwrap_or(iri.to_iri());
                Some(HoverResult::Iri {
                    iri: full_iri,
                    range,
                })
            }
            kind => {
                let text = keyword_hover_info(kind);
                if text.is_empty() {
                    None
                } else {
                    Some(HoverResult::Keyword { text, range })
                }
            }
        }
    }

    fn range(&self) -> Range {
        self.tree().root_node().range().into()
    }

    fn iri_at(&self, pos: Position) -> Option<RangeBox<IriAtPosition>> {
        let node = self
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos.into(), pos.into())?;
        let range: Range = node.range().into();
        let parent_kind = node.parent()?.kind();
        let is_import = parent_kind == "import";
        let frame_type = if is_import {
            None
        } else {
            match FrameType::parse(parent_kind) {
                FrameType::Invalid | FrameType::Unknown => None,
                ft => Some(ft),
            }
        };
        match node.kind() {
            "full_iri" => Some(RangeBox::new(
                IriAtPosition {
                    full_iri: trim_full_iri_rope_slice(node_text(&node, self.rope())).to_iri(),
                    is_import,
                    frame_type,
                },
                range,
            )),
            "simple_iri" | "abbreviated_iri" => {
                let iri = node_text(&node, self.rope());
                let full_iri = self
                    .abbreviated_iri_to_full_iri(&iri.to_iri())
                    .unwrap_or(iri.to_iri());
                Some(RangeBox::new(
                    IriAtPosition {
                        full_iri,
                        is_import,
                        frame_type,
                    },
                    range,
                ))
            }
            _ => None,
        }
    }

    fn rename_range(&self, pos: Position) -> Option<Range> {
        // self.iri_at(pos).map(|iri| iri.range().clone())

        let node = self
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos.into(), pos.into())?;
        match node.parent()?.kind() {
            "datatype_iri"
            | "class_iri"
            | "annotation_property_iri"
            | "ontology_iri"
            | "data_property_iri"
            | "version_iri"
            | "object_property_iri"
            | "annotation_property_iri_annotated_list"
            | "individual_iri" => {}
            _ => return None,
        }
        match node.kind() {
            // Only the text not the '<' '>' so <http://a.b/c> -> http://a.b/c
            "full_iri" => {
                let range: Range = node.range().into();
                Some(Range {
                    start: range.start.moved_right(1, self.rope()),
                    end: range.end.moved_left(1, self.rope()),
                })
            }

            "simple_iri" => Some(node.range().into()),

            // Only the text after the ':' so foo:bar -> bar
            "abbreviated_iri" => {
                let range: Range = node.range().into();
                let text = node_text(&node, self.rope()).to_string();
                let col_offset = text
                    .find(':')
                    .expect("abbreviated_iri to contain at least one :")
                    + 1;
                #[allow(clippy::cast_possible_truncation)]
                Some(Range {
                    start: range.start.moved_right(col_offset as u32, self.rope()),
                    ..range
                })
            }
            _ => None,
        }
    }

    fn rename_info_at(&self, pos: Position, new_name: &str) -> Result<Option<RenameInfo>> {
        let node = self
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos.into(), pos.into())
            .ok_or(Error::PositionOutOfBounds(pos))?;
        let parent_kind = match node.parent() {
            Some(p) => p.kind(),
            None => return Ok(None),
        };
        let frame_type = FrameType::parse(parent_kind);
        if matches!(frame_type, FrameType::Invalid | FrameType::Unknown) {
            return Ok(None);
        }
        Ok(match node.kind() {
            "full_iri" => {
                let full_iri = trim_full_iri_rope_slice(node_text(&node, self.rope())).to_iri();
                Some(RenameInfo {
                    full_iri,
                    new_iri: Some(new_name.to_iri()),
                    frame_type,
                    original: new_name.to_string(),
                })
            }
            "simple_iri" => {
                let iri = node_text(&node, self.rope()).to_iri();
                Some(RenameInfo {
                    full_iri: self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri),
                    new_iri: self.abbreviated_iri_to_full_iri(&new_name.into()),
                    frame_type,
                    original: new_name.to_string(),
                })
            }
            "abbreviated_iri" => {
                let annreviated_iri: Iri = node_text(&node, self.rope()).to_iri();
                let (prefix, _) = annreviated_iri
                    .split_once(':')
                    .expect("abbreviated_iri to contain at least one :");
                let new_original = format!("{prefix}:{new_name}");
                Some(RenameInfo {
                    full_iri: self
                        .abbreviated_iri_to_full_iri(&annreviated_iri)
                        .unwrap_or(annreviated_iri),
                    new_iri: self.abbreviated_iri_to_full_iri(&new_original.to_iri()),
                    frame_type,
                    original: new_original,
                })
            }
            _ => None,
        })
    }

    fn keyword_actions_at(&self, pos: Position) -> Vec<KeywordAction> {
        let Some(mut node) = self
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos.into(), pos.into())
        else {
            return vec![];
        };
        let mut actions = vec![];
        while let Some(parent) = node.parent() {
            let kwds = child_keywords_for_kind(node.kind());
            for (parent_name, new_text) in kwds {
                actions.push(KeywordAction {
                    parent_name: (*parent_name).to_string(),
                    new_text: format!("\n{new_text}"),
                    range: node.range().into(),
                });
            }
            node = parent;
        }
        actions
    }

    fn all_iris_in_range(&self, range: Range) -> Vec<RangeBox<Iri>> {
        self.query_range(&ALL_QUERIES.iri_query_all, range)
            .into_iter()
            .flat_map(|match_| match_.captures)
            .map(|capture| {
                let iri = trim_full_iri_rope_slice(capture.node.text).to_iri();
                let iri = self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);
                RangeBox::new(iri, capture.node.range)
            })
            .collect()
    }

    fn highlights(&self, range: Range) -> Highlights {
        let query_source = tree_sitter_owl_ms::HIGHLIGHTS_QUERY;
        let query = Query::new(&LANGUAGE_OMN, query_source).expect("valid query expect");
        let mut query_cursor = QueryCursor::new();
        if range != Range::FULL_RANGE {
            query_cursor.set_point_range(range.into());
        }
        let matches = query_cursor.matches(
            &query,
            self.tree().root_node(),
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

    fn get_keyword_competions_at(&self, pos: Position) -> Vec<String> {
        let pos_one_left = pos.moved_left(1, self.rope());
        let mut node = self
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos_one_left.into(), pos_one_left.into())
            .expect("The pos to be in at least one node");

        let mut lei = if node.parent().is_none() {
            LANGUAGE_OMN
                .lookahead_iterator(1)
                .expect("state 1 should be valid")
        } else {
            let mut lei = LANGUAGE_OMN.lookahead_iterator(node.parse_state());
            while lei.is_none() {
                let parent = node.parent();
                if let Some(parent) = parent {
                    node = parent;
                    lei = LANGUAGE_OMN.lookahead_iterator(node.parse_state());
                } else {
                    lei = LANGUAGE_OMN.lookahead_iterator(1);
                }
            }
            lei.expect("while none loop should have set it to some")
        };

        let line = self
            .rope()
            .get_line(pos.line() as usize)
            .map(|s| s.to_string())
            .unwrap_or_default();
        let partial = word_before_character(pos.character_byte() as usize, &line);

        lei.iter_names()
            .inspect(|n| debug!("- LEI name: {n}"))
            .filter_map(|kind| (*queries::KEYWORDS_MAP).get(kind).cloned())
            .filter(|kw| kw.starts_with(&partial))
            .collect_vec()
    }

    fn get_iri_completions_at(
        &self,
        pos: Position,
        workspace: &Workspace,
    ) -> Vec<(String, String, String, bool)> {
        let pos_one_left = pos.moved_left(1, self.rope());
        let node = self
            .tree()
            .root_node()
            .named_descendant_for_point_range(pos_one_left.into(), pos_one_left.into())
            .expect("The pos to be in at least one node");

        let partial_text = node_text(&node, self.rope()).to_string();

        if node.kind() == "simple_iri" {
            debug!("Try iris...");
            workspace
                .search_frame(&partial_text)
                .into_iter()
                .unique_by(|(_, iri, _)| iri.clone())
                .sorted_unstable_by_key(|(v, _, _)| v.clone())
                .filter_map(|(full, maybe_full_iri, frame)| {
                    let iri = self.full_iri_to_shorter_iri(&maybe_full_iri);
                    if iri == partial_text {
                        None
                    } else {
                        Some((
                            frame.label(workspace).unwrap_or(full),
                            frame.info_display(workspace),
                            iri,
                            frame.is_depricated(),
                        ))
                    }
                })
                .collect_vec()
        } else {
            Vec::new()
        }
    }

    fn rename_edits(&self, rename_info: &RenameInfo) -> Vec<RangeBox<String>> {
        let RenameInfo {
            full_iri,
            new_iri,
            frame_type,
            original,
        } = rename_info;

        self.parsed_document
            .query(&ALL_QUERIES.iri_query_all)
            .into_iter()
            .map(|m| {
                let (iri, range, node_frame_type) = match &m.captures[..] {
                    [iri_capture] => (
                        match iri_capture.node.kind {
                            "full_iri" => trim_full_iri_rope_slice(iri_capture.node.text).to_iri(),
                            "simple_iri" | "abbreviated_iri" => self
                                .abbreviated_iri_to_full_iri(&iri_capture.node.text.to_iri())
                                .unwrap_or(iri_capture.node.text.to_iri()),
                            _ => unreachable!(),
                        },
                        iri_capture.node.range,
                        FrameType::parse(
                            iri_capture
                                .node
                                .parent_kind
                                .expect("iris should have parents"),
                        ),
                    ),
                    _ => unreachable!(),
                };
                if &iri == full_iri && &node_frame_type == frame_type {
                    Ok(Some(RangeBox::new(
                        new_iri
                            .clone()
                            .map(|new_iri| self.full_iri_to_shorter_iri(&new_iri))
                            .unwrap_or(original.to_string()),
                        range,
                    )))
                } else {
                    Ok(None)
                }
            })
            .filter_and_log()
            .flatten()
            .collect_vec()
    }

    fn statistic(&self) -> String {
        format!(
            "path: {}, prefix length: {}, imports length: {}, def length: {}, ref length: {}, anno length: {}, all frame infos len: {}, local diagnostic len: {}, iri locations len: {}, dir import urls: {}, other urls: {}, frame annotations sum: {}, frame definitions sum: {}",
            self.id.path.display(),
            self.queried_document.prefixes.len(),
            self.queried_document.imports.len(),
            self.stage2.definitions.len(),
            self.stage2.references.len(),
            self.stage2.annotations.len(),
            self.stage2.all_frame_infos.len(),
            self.stage2.local_diagnostics.len(),
            self.stage2.iri_locations.len(),
            self.stage2.directly_reachable_import_urls.len(),
            self.stage2.directly_reachable_other_urls.len(),
            self.stage2.all_frame_infos.values().map(|a| a.annotations.len()).sum::<usize>(),
            self.stage2.all_frame_infos.values().map(|a| a.definitions.len()).sum::<usize>(),
        )
    }
}

impl InternalOmnDocument {
    // TODO inline all callers
    pub fn tree(&self) -> &Tree {
        self.parsed_document.tree()
    }

    pub fn query(&'_ self, query: &Query) -> Vec<UnwrappedQueryMatch<'_>> {
        self.parsed_document.query(query)
    }

    pub fn query_range(&'_ self, query: &Query, range: Range) -> Vec<UnwrappedQueryMatch<'_>> {
        self.parsed_document.query_range(query, range)
    }

    pub fn new(uri: Url, version: i32, text: String) -> InternalOmnDocument {
        let path = uri.to_file_path().expect("URL should be a file path");
        Self::new_with_path(uri, version, text, path)
    }

    pub fn new_with_path(
        uri: Url,
        version: i32,
        text: String,
        path: PathBuf,
    ) -> InternalOmnDocument {
        let id = DocumentId { path, uri, version };

        let tree = timeit("create_document / parse", || {
            GLOBAL_OMN_PARSER.with(|parser| {
                parser
                    .borrow_mut()
                    .parse(&text, None)
                    .expect("language to be set, no timeout to be used, no cancellation flag")
            })
        });

        let rope = Rope::from(text);
        let parsed_document = ParsedDocument::new(tree, rope);

        // TODO
        let queried_document: QueriedDocument = parsed_document.into_queried();

        let stage2: Stage2Document = queried_document.analyze(&parsed_document, &id);

        debug!("Stage2Document -> InternalDocument");

        InternalOmnDocument {
            id,
            parsed_document,
            queried_document,
            stage2,
        }
    }

    pub fn edit_inner(
        self, // TODO #30 do a mut instead so the analytics do not get dropped
        params: DidChangeTextDocumentParams,
        encoding: &PositionEncodingKind,
    ) -> Result<InternalOmnDocument> {
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

        let InternalOmnDocument {
            id,
            parsed_document,
            mut queried_document,
            mut stage2,
        } = self;

        let changes = changes_from_lsp(params, encoding, parsed_document.rope());

        // Note that these ranges are in the pre edit form
        for change in &changes {
            debug!("Updating changed range (pre edit) {change:?}");
        }

        let (parsed_document, old_tree) = GLOBAL_OMN_PARSER.with(|parser| {
            parsed_document.edit_parsed_document(changes.iter(), &mut parser.borrow_mut())
        })?;

        // Increment ID
        let id = DocumentId {
            version: new_version,
            ..id
        };

        // This is a combination of syntax and text changes
        let mut post_change_ranges: &[Range] =
            &post_change_ranges(&changes, &parsed_document, &old_tree);

        debug!("Post change ranges: {post_change_ranges:#?}");

        let dirty_prefix = timeit("document.edit / queries", || {
            queried_document.update(&changes, post_change_ranges, &parsed_document)
        });

        // The problem is that the references and definitions (and other stuff) depends on
        // prefixes. So the change in a prefix can change a lot of references that are not
        // located at the prefix.
        // Solution 1: Remove the dependency and move the resolution of abbriv iri -> full iri
        // into a later step.
        // Solution 2: Mark all references dirty when ever a prefix changes, which is not often.
        // ==========
        // I Chose Solution 2
        // Do a whole new analysis when the prefixes change!
        if dirty_prefix {
            info!("document.edit Dirty prefix. New post change range is the max range.");
            post_change_ranges = &[Range::FULL_RANGE];
        }

        timeit("document.edit / analyze", || {
            stage2.update(
                &changes,
                post_change_ranges,
                &parsed_document,
                &queried_document,
                &id,
            );
        });

        let doc = InternalOmnDocument {
            id,
            parsed_document,
            queried_document,
            stage2,
        };

        Ok(doc)
    }
}

#[derive(Debug)]
pub struct QueriedDocument {
    pub ontology_id: Option<RangeBox<OntologyId>>,
    pub prefixes: HashMap<String, RangeBox<String>>,
    pub imports: Vec<RangeBox<Iri>>,
}

impl QueriedDocument {
    /// Finds flat references to other document URL's in this document
    pub fn reachable_urls(
        &self,
        document_references: &[RangeBox<Iri>],
        own_uri: &Url,
    ) -> (Vec<Url>, Vec<Url>) {
        let imports = self
            .imports
            .iter()
            .filter_map(|rb| {
                Url::parse(rb.value().as_str())
                    .inspect_err(|url_err| error!("Import URL invalid {url_err}"))
                    .ok()
            })
            .collect_vec();

        // Other urls include prefixes
        let mut other_urls = self
            .prefixes
            .iter()
            // Filter out the empty prefix ":"
            .filter_map(|(prefix, url)| {
                if prefix.is_empty() {
                    None
                } else {
                    Some(url.value())
                }
            })
            .filter_map(|url| Url::parse(url).ok())
            // Filter out the current document as a prefix (most likely the empty prefix ":")
            .filter(|url| url != own_uri)
            .map(|url| {
                // Remove fragments from prefixes
                if url.fragment().is_some() {
                    let mut url = url.clone();
                    url.set_fragment(Some(""));
                    url
                } else {
                    url
                }
            })
            .collect_vec();

        let referenced_urls = document_references
            .iter()
            .filter_map(|iri| iri_to_parent_url_str(iri.value()))
            .unique()
            .flat_map(str::parse);

        // debug!(
        //     "Extending {} with {}",
        //     own_uri,
        //     referenced_urls.iter().join(", ")
        // );

        other_urls.extend(referenced_urls);

        (imports, other_urls)
    }

    pub fn abbreviated_iri_to_full_iri(&self, abbreviated_iri: &Iri) -> Option<Iri> {
        let prefixes = &self.prefixes;
        if let Some((prefix, simple_iri)) = abbreviated_iri.as_str().split_once(':') {
            prefixes.get(prefix).map(|resolved_prefix| {
                [resolved_prefix.value().as_str(), simple_iri]
                    .join("")
                    .to_iri()
            })
        } else {
            // Simple IRIs get a free colon prepended
            // ref: https://www.w3.org/TR/owl2-manchester-syntax/#IRIs.2C_Integers.2C_Literals.2C_and_Entities
            prefixes.get("").map(|resolved_prefix| {
                [resolved_prefix.value().as_str(), abbreviated_iri.as_str()]
                    .join("")
                    .to_iri()
            })
        }
    }

    // TODO this is still slow (50ms on oeo-full)
    fn document_all_frame_infos(
        definitions: &[RangeBox<IriDefinition>],
        annotations: &[RangeBox<Annotation>],
        path: &Path,
    ) -> HashMap<Iri, FrameInfo> {
        annotations
            .par_iter()
            .map(|rb| {
                let annotation = rb.value();
                FrameInfo {
                    iri: annotation.frame_iri.clone(),
                    annotations: vec![annotation.clone()],
                    frame_type: FrameType::Unknown,
                    definitions: Vec::new(),
                }
            })
            .chain(definitions.par_iter().map(|definiton| FrameInfo {
                iri: definiton.value().iri.clone(),
                annotations: Vec::new(),
                frame_type: definiton.value().kind,
                definitions: vec![Location {
                    uri: Url::from_file_path(path).expect("valid path"),
                    range: *definiton.range(),
                }],
            }))
            .fold(
                HashMap::new, // each thread starts with an empty map
                |mut acc, frame_info| {
                    acc.entry(frame_info.iri.clone())
                        .and_modify(|existing: &mut FrameInfo| existing.extend(frame_info.clone()))
                        .or_insert(frame_info);
                    acc
                },
            )
            .reduce(
                HashMap::new, // merge the per-thread maps together
                |mut a, b| {
                    for (iri, frame_info) in b {
                        a.entry(iri)
                            .and_modify(|existing| existing.extend(frame_info.clone()))
                            .or_insert(frame_info);
                    }
                    a
                },
            )
    }

    fn document_annotations(&self, parsed_document: &ParsedDocument) -> Vec<RangeBox<Annotation>> {
        self.document_annotations_in_range(parsed_document, Range::FULL_RANGE)
    }

    // TODO remove ram usage somehow
    fn document_annotations_in_range(
        &self,
        parsed_document: &ParsedDocument,
        range: Range,
    ) -> Vec<RangeBox<Annotation>> {
        parsed_document
            .query_range(&ALL_QUERIES.annotation_query, range)
            .iter()
            .map(|m| {
                let frame_iri_capture =
                    capture_by_name(&ALL_QUERIES.annotation_query, &m.captures, "frame_iri")
                        .expect("frame capture");
                let annotation_iri_capture =
                    capture_by_name(&ALL_QUERIES.annotation_query, &m.captures, "iri")
                        .expect("iri capture");
                let value_capture =
                    capture_by_name(&ALL_QUERIES.annotation_query, &m.captures, "literal")
                        .expect("value capture");
                let frame_capture =
                    capture_by_name(&ALL_QUERIES.annotation_query, &m.captures, "frame")
                        .expect("frame_capture");

                let datatype_capture =
                    capture_by_name(&ALL_QUERIES.annotation_query, &m.captures, "datatype");

                let language_capture =
                    capture_by_name(&ALL_QUERIES.annotation_query, &m.captures, "language");

                let frame_iri = trim_full_iri_rope_slice(frame_iri_capture.node.text).to_iri();
                let frame_iri = self
                    .abbreviated_iri_to_full_iri(&frame_iri)
                    .unwrap_or(frame_iri);

                let annotation_iri =
                    trim_full_iri_rope_slice(annotation_iri_capture.node.text).to_iri();
                let annotation_iri = self
                    .abbreviated_iri_to_full_iri(&annotation_iri)
                    .unwrap_or(annotation_iri);

                let literal = trim_string_value(&value_capture.node.text.to_string());

                let language = language_capture
                    .map(|c| c.node.text.to_string().trim_start_matches('@').to_string())
                    .and_then(|tag| language::Language::try_from(tag).ok());
                // TODO #180 spawn diagnostics about wrong language

                let datatype = datatype_capture.map_or(STRING_IRI.into(), |c| match c.node.kind {
                    "keyword_integer" => INTEGER_IRI.into(),
                    "keyword_decimal" => DECIMAL_IRI.into(),
                    "keyword_float" => FLOAT_IRI.into(),
                    "keyword_string" => STRING_IRI.into(),
                    _ => self
                        .abbreviated_iri_to_full_iri(&c.node.text.to_iri())
                        .unwrap_or(c.node.text.to_iri()),
                });

                // The value can decide the type
                // TODO maybe check with range of annotation property
                let datatype = match value_capture.node.kind {
                    "integer_literal" => INTEGER_IRI.into(),
                    "decimal_literal" => DECIMAL_IRI.into(),
                    "floating_point_literal" => FLOAT_IRI.into(),
                    _ => datatype,
                };

                let annotation = Annotation {
                    frame_iri,
                    iri: annotation_iri,
                    string_value: literal,
                    language,
                    datatype,
                };

                RangeBox::new(annotation, frame_capture.node.range)
            })
            // TODO remove
            // .collect::<HashSet<RangeBox<Annotation>>>()
            // .into_iter()
            // .sorted_unstable()
            .collect_vec()
    }

    fn document_definitions(
        &self,
        parsed_document: &ParsedDocument,
    ) -> Vec<RangeBox<IriDefinition>> {
        self.document_definitions_in_range(parsed_document, Range::FULL_RANGE)
    }

    fn document_definitions_in_range(
        &self,
        parsed_document: &ParsedDocument,
        range: Range,
    ) -> Vec<RangeBox<IriDefinition>> {
        parsed_document
            .query_range(&ALL_QUERIES.frame_query, range)
            .iter()
            .map(|m| match &m.captures[..] {
                [frame_iri_capture, frame_capture] => {
                    let frame_iri_parent_kind = frame_iri_capture
                        .node
                        .parent_kind
                        .as_ref()
                        .expect("All frame IRIs should have parents");

                    let frame_iri = trim_full_iri_rope_slice(frame_iri_capture.node.text).to_iri();
                    let frame_iri = self
                        .abbreviated_iri_to_full_iri(&frame_iri)
                        .unwrap_or(frame_iri);

                    RangeBox::new(
                        IriDefinition {
                            iri: frame_iri,
                            kind: FrameType::parse(frame_iri_parent_kind),
                        },
                        frame_capture.node.range,
                    )
                }
                _ => unreachable!(),
            })
            .collect()
    }

    fn document_references(&self, parsed_document: &ParsedDocument) -> Vec<RangeBox<Iri>> {
        self.document_references_in_range(parsed_document, Range::FULL_RANGE)
    }

    fn document_references_in_range(
        &self,
        parsed_document: &ParsedDocument,
        range: Range,
    ) -> Vec<RangeBox<Iri>> {
        parsed_document
            .query_range(&ALL_QUERIES.iri_query_references, range)
            .iter()
            .map(|m| match &m.captures[..] {
                [iri_capture] => {
                    let iri = trim_full_iri_rope_slice(iri_capture.node.text).to_iri();
                    let iri = self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);

                    RangeBox::new(iri, iri_capture.node.range)
                }
                _ => unreachable!(),
            })
            .collect()
    }

    fn analyze(&self, parsed_document: &ParsedDocument, id: &DocumentId) -> Stage2Document {
        debug!("QueriedDocument -> Stage2Document");

        let ((references, definitions), annotations) = rayon::join(
            || {
                rayon::join(
                    || self.document_references(parsed_document),
                    || self.document_definitions(parsed_document),
                )
            },
            || self.document_annotations(parsed_document),
        );

        // Find iri locations
        let all_frame_infos = timeit("all frame infos", || {
            QueriedDocument::document_all_frame_infos(&definitions, &annotations, &id.path)
        });

        let (directly_reachable_import_urls, directly_reachable_other_urls) =
            timeit("reachable urls", || {
                self.reachable_urls(&references, &id.uri)
            });

        let iri_locations = build_iri_locations(&references);
        Stage2Document {
            references,
            definitions,
            annotations,
            all_frame_infos,
            local_diagnostics: timeit("syntax errors", || parsed_document.syntax_errors()),
            directly_reachable_import_urls,
            directly_reachable_other_urls,
            iri_locations,
        }
    }

    fn update(
        &mut self,
        changes: &[Change],
        post_change_ranges: &[Range],
        parsed_document: &ParsedDocument,
    ) -> bool {
        // Update ontology id
        let mut dirty = false;
        if let Some(o_id) = &mut self.ontology_id {
            o_id.edit(changes.iter());

            for sc in post_change_ranges {
                if o_id.range().overlaps(sc) {
                    dirty = true;
                }
            }
        } else {
            dirty = true;
        }

        if dirty {
            self.ontology_id = parsed_document.ontology_id();
        }

        // Edit

        for import in &mut self.imports {
            import.edit(changes.iter());
        }

        for prefix_value in self.prefixes.values_mut() {
            prefix_value.edit(changes.iter());
        }

        // I think I dont need the removed items, they overlap with the
        // post_change_ranges, so I can just use the ranges.
        self.imports.retain(|import| {
            for sc in post_change_ranges {
                if import.range().overlaps(sc) || import.range().is_zero() {
                    return false;
                }
            }
            true
        });

        // Retain

        let mut dirty_prefix = false;
        self.prefixes.retain(|_, prefix_value| {
            for sc in post_change_ranges {
                if prefix_value.range().overlaps(sc) || prefix_value.range().is_zero() {
                    dirty_prefix = true;
                    debug!("Removing prefix {prefix_value:?}");
                    return false;
                }
            }
            true
        });

        // Add

        for di in post_change_ranges {
            let additional_imports = parsed_document.imports_in_range(*di);

            self.imports.extend(additional_imports);
        }

        for di in post_change_ranges {
            let additional_prefixes = parsed_document.prefixes_in_range(*di);

            if !additional_prefixes.is_empty() {
                dirty_prefix = true;
            }

            self.prefixes.extend(additional_prefixes);
        }

        // Cleanup

        self.imports.dedup_by_key(|r| *r.range());

        dirty_prefix
    }
}

impl From<ParsedDocument> for QueriedDocument {
    fn from(val: ParsedDocument) -> Self {
        debug!("ParsedDocument -> QueriedDocument");

        let ontology_id = val.ontology_id();
        let prefixes = val.prefixes();
        let imports = val.imports();

        QueriedDocument {
            ontology_id,
            prefixes,
            imports,
        }
    }
}

impl ParsedDocument {
    fn into_queried(self: &ParsedDocument) -> QueriedDocument {
        debug!("ParsedDocument -> QueriedDocument");

        let ((ontology_id, prefixes), imports) = rayon::join(
            || rayon::join(|| self.ontology_id(), || self.prefixes()),
            || self.imports(),
        );

        QueriedDocument {
            ontology_id,
            prefixes,
            imports,
        }
    }
}

fn to_doc(node: &Node, rope: &Rope, options: &FormattingSettings) -> RcDoc<'static, ()> {
    // I do not target 32 systems
    #[allow(clippy::cast_possible_wrap)]
    let nest_depth = options.tab_size as isize;
    let text = node_text(node, rope).to_string();
    debug!(
        "to_doc for {text} that is {} at {:?}",
        node.kind(),
        node.range()
    );
    let mut cursor = node.walk();

    // So if this node as an error child then the translation into RcDoc could exclude that error node.
    // Therefore, lets not translate it at all.
    if node.children(&mut cursor).any(|child| child.is_error()) {
        return RcDoc::text(text);
    }

    match node.kind() {
        "source_file" => {
            source_file_to_doc(node, rope, options)
        },
        "ontology" =>
            ontology_to_doc(node, rope,options, nest_depth)
        ,
        "prefix_declaration" | "import" | "annotation" => RcDoc::intersperse(
            node.children(&mut cursor)
                .map(|n| to_doc(&n, rope,options)),
            RcDoc::line(),
        )
        .nest(nest_depth)
        .group(),
        "annotations"
        // class
        | "sub_class_of" | "class_equivalent_to" | "class_disjoint_with" | "disjoint_union_of" | "has_key"
        // datatype
        | "datatype_equavalent_to" // TODO weird typo that is all over the app
        // individual
        | "individual_facts" | "individual_same_as" | "individual_different_from" | "individual_types"
        // annotation property
        | "annotation_property_domin" // TODO also typo
        | "annotation_property_range" | "annotation_property_sub_property_of"
        // data property
        | "data_property_domain" | "data_property_range" | "data_property_characteristics" | "data_property_sub_property_of" | "data_property_equivalent_to" | "data_property_disjoint_with"
        // object property
        |"domain" |"range" |"sub_property_of" |"object_property_equivalent_to" |"object_property_disjoint_with" |"inverse_of" |"characteristics" |"sub_property_chain"
        // misc
        |"equivalent_classes" |"disjoint_classes" |"equivalent_object_properties" |"disjoint_object_properties" |"equivalent_data_properties" |"disjoint_data_properties" |"same_individual" |"different_individuals"
         => {
            nesting_property_with_keyword_to_frame(node, rope, options, nest_depth)
        },
        "description"
         => {
             let subs=node.children(&mut cursor).chunk_by(|n| n.kind()=="or").into_iter().map(|(is_or, chunks)|{
                 if is_or {
                     RcDoc::line().append(RcDoc::text("or").append(RcDoc::space()))
                 } else {
                     let conjunction_node = chunks.exactly_one().unwrap_or_else(|_| unreachable!("chunk should contain exactly one separator node"));
                     to_doc(&conjunction_node, rope, options)
                 }
             }).collect_vec();
            RcDoc::concat(subs)
        },
        "conjunction"
         => {
             let subs=node.children(&mut cursor).chunk_by(|n| n.kind()=="and").into_iter().map(|(is_or, chunks)|{
                 if is_or {
                     RcDoc::line().append(RcDoc::text("and").append(RcDoc::space()))
                 } else {
                     RcDoc::intersperse(chunks.map(|n| to_doc(&n, rope, options)), RcDoc::space())
                 }
             }).collect_vec();
            RcDoc::concat(subs)
        },
        "primary"=>{
            RcDoc::intersperse(node.children(&mut cursor).map(|n|to_doc(&n, rope, options)), RcDoc::space())
        },
        "nested_description"
         => {
            RcDoc::text("(").append(RcDoc::line()).append(
                to_doc(&node.named_child(0).expect("open parentheses to have sibling"), rope, options)
            ).nest(nest_depth).append(RcDoc::line()).append(")")
        },
        "class_frame"
        | "datatype_frame"
        | "data_property_frame"
        | "object_property_frame"
        | "annotation_property_frame"
        | "individual_frame"
         => frame_to_doc(node, rope, options, nest_depth),
        _ => RcDoc::text(text), // this applies also to "ERROR" nodes!
    }
}

fn nesting_property_with_keyword_to_frame(
    node: &Node,
    rope: &Rope,
    options: &FormattingSettings,
    nest_depth: isize,
) -> RcDoc<'static> {
    let mut cursor = node.walk();
    let mut docs = vec![];

    // This should be the keyword
    if let Some(child) = node.child(0) {
        docs.push(to_doc(&child, rope, options).append(RcDoc::line()));
    }

    for (is_separator, chunk) in &node
        .children(&mut cursor)
        .skip(1)
        .chunk_by(|x| x.kind() == "," || x.kind() == "o")
    {
        if is_separator {
            let n = &chunk.exactly_one().unwrap_or_else(|_| {
                unreachable!("chunk should contain exactly one separator node")
            });

            if n.kind() == "o" {
                docs.push(RcDoc::text(" o").append(RcDoc::line()));
            } else {
                docs.push(RcDoc::text(",").append(RcDoc::line()));
            }
        } else {
            docs.push(RcDoc::intersperse(
                chunk.map(|n| to_doc(&n, rope, options)),
                RcDoc::line(),
            ));
        }
    }

    RcDoc::concat(docs).nest(nest_depth).group()
}

fn source_file_to_doc(
    node: &Node,
    rope: &Rope,
    options: &FormattingSettings,
) -> RcDoc<'static, ()> {
    let mut cursor = node.walk();
    let prefix_docs = node
        .children_by_field_name("prefix", &mut cursor)
        .map(|n| to_doc(&n, rope, options))
        .collect_vec();
    let ontology_doc = node
        .child_by_field_name("ontology")
        .map_or(RcDoc::nil(), |n| to_doc(&n, rope, options));
    if prefix_docs.is_empty() {
        ontology_doc
    } else {
        RcDoc::intersperse(
            [
                RcDoc::intersperse(prefix_docs, RcDoc::hardline()),
                ontology_doc,
            ],
            RcDoc::hardline().append(RcDoc::hardline()),
        )
    }
}

fn ontology_to_doc(
    node: &Node,
    rope: &Rope,
    options: &FormattingSettings,
    nest_depth: isize,
) -> RcDoc<'static> {
    let mut cursor = node.walk();
    RcDoc::intersperse(
        [
            RcDoc::text("Ontology:")
                .append(RcDoc::line())
                .append(RcDoc::intersperse(
                    node.child_by_field_name("iri")
                        .into_iter()
                        .map(|n| to_doc(&n, rope, options))
                        .chain(
                            node.child_by_field_name("version_iri")
                                .into_iter()
                                .map(|n| to_doc(&n, rope, options)),
                        ),
                    RcDoc::line(),
                ))
                .nest(nest_depth)
                .group(),
            // imports
            RcDoc::intersperse(
                node.children_by_field_name("import", &mut cursor.clone())
                    .map(|n| to_doc(&n, rope, options).append(RcDoc::hardline())),
                RcDoc::nil(),
            ),
            // annotations
            RcDoc::intersperse(
                node.children_by_field_name("annotations", &mut cursor.clone())
                    .map(|n| to_doc(&n, rope, options).append(RcDoc::hardline())),
                RcDoc::nil(),
            ),
            // frames
            RcDoc::intersperse(
                {
                    let frame_nodes = node.children_by_field_name("frame", &mut cursor);

                    let maybe_sorted: Box<dyn Iterator<Item = Node<'_>>> = if options.order_frames {
                        Box::new(frame_nodes.sorted_by_key(|n| frame_order(n.kind())))
                    } else {
                        Box::new(frame_nodes)
                    };

                    maybe_sorted.map(|n| to_doc(&n, rope, options).append(RcDoc::hardline()))
                },
                RcDoc::hardline(),
            ),
        ],
        RcDoc::hardline(),
    )
}

fn frame_order(frame_kind: &str) -> u32 {
    match frame_kind {
        "annotation_property_frame" => 1,
        "datatype_frame" => 2,
        "object_property_frame" => 3,
        "data_property_frame" => 4,
        "class_frame" => 5,
        "individual_frame" => 6,
        _ => u32::MAX,
    }
}

fn frame_to_doc(
    node: &Node,
    rope: &Rope,
    options: &FormattingSettings,
    nest_depth: isize,
) -> RcDoc<'static> {
    let mut cursor = node.walk();
    node.child(0)
        .map_or(RcDoc::nil(), |n| to_doc(&n, rope, options))
        .append(RcDoc::line())
        .append(
            node.child(1)
                .map_or(RcDoc::nil(), |n| to_doc(&n, rope, options)),
        )
        .nest(nest_depth)
        .group()
        .append(RcDoc::hardline())
        .append(RcDoc::intersperse(
            node.children(&mut cursor)
                .skip(2)
                .map(|n| to_doc(&n, rope, options)),
            RcDoc::hardline(),
        ))
        .nest(nest_depth)
        .group()
}

/// An internal document that has analysis results.
#[derive(Debug)]
pub struct Stage2Document {
    pub definitions: Vec<RangeBox<IriDefinition>>,
    pub references: Vec<RangeBox<Iri>>,
    pub annotations: Vec<RangeBox<Annotation>>,

    all_frame_infos: HashMap<Iri, FrameInfo>,
    local_diagnostics: Vec<Diagnostic>,
    /// These include only URL's from the imports
    directly_reachable_import_urls: Vec<Url>,
    /// These include all other URL's that can be found in this document
    directly_reachable_other_urls: Vec<Url>,
    iri_locations: HashMap<Iri, Vec<RangeBox<()>>>,
}

impl Stage2Document {
    // TODO maybe https://crates.io/crates/rayon-join
    #[allow(clippy::too_many_lines)] // rayon join takes up most of the lines :(
    fn update(
        &mut self,
        changes: &[Change],
        post_change_ranges: &[Range],
        parsed_document: &ParsedDocument,
        queried_document: &QueriedDocument,
        id: &DocumentId,
    ) {
        debug!("document.edit / analysis post change ranges {post_change_ranges:#?}");

        timeit("document.edit / analysis / edit", || {
            self.edit_range_boxes(changes);
        });

        // Retain
        timeit("document.edit / analysis / retain", || {
            rayon_join::join!(
                || retain_vec_rb(post_change_ranges, &mut self.definitions),
                || retain_vec_rb(post_change_ranges, &mut self.annotations),
                || retain_vec_rb_on_remove(post_change_ranges, &mut self.references, |range_box| {
                    if let Some(values) = self.iri_locations.get_mut(range_box.value()) {
                        // Remove all indexed iri locations
                        values.retain(|rb| rb.range() != range_box.range());
                    }
                })
            );
        });

        // Add

        timeit("document.edit / analysis / extend", || {
            rayon_join::join!(
                || extend_vec_rb(post_change_ranges, &mut self.definitions, |range| {
                    queried_document.document_definitions_in_range(parsed_document, range)
                }),
                || extend_vec_rb(post_change_ranges, &mut self.references, |range| {
                    let add = queried_document.document_references_in_range(parsed_document, range);

                    // Readd the index values
                    for rb in &add {
                        let ranges = self.iri_locations.entry(rb.value().clone()).or_default();
                        ranges.push(RangeBox::new((), *rb.range()));
                    }

                    add
                }),
                ||
                // Annotations
                // The problem was the following insert not removing a faulty info:
                //
                //  Ontology: <http://example.org/fuzz-test>
                //      Class: Foo
                //  Class: OTHER Annotations: rdfs:label "OTHER"
                //          Annotations: rdfs:label "Foo Label"
                //
                // After removing the OTHER class the annotations still contain
                // - OTHER label "Foo Label"
                // So the syntax change ranges did not include the annotation that followed :(
                //
                // Now the range, of each annotation, covers the whole frame.
                 extend_vec_rb(post_change_ranges, &mut self.annotations, |range| {
                    queried_document.document_annotations_in_range(parsed_document, range)
                })
            );
        });

        timeit("document.edit / analyse / cleanup", || {
            rayon_join::join!(
                || {
                    self.definitions.par_sort_unstable();
                    self.definitions.dedup_by_key(|rb| *rb.range());
                },
                || self.iri_locations.par_iter_mut().for_each(|(_, ranges)| {
                    // TODO maybe remove all sorts :) (I dont think we need them actualy)
                    // Lets keep them for now
                    ranges.par_sort();
                    ranges.dedup();
                }),
                || {
                    self.references.par_sort_unstable();
                    self.references.dedup_by_key(|rb| *rb.range());
                },
                || {
                    self.annotations.par_sort_unstable();
                    self.annotations.dedup();
                }
            );
        });

        // Not incremental part --------------------------
        // This is pretty fast now.
        // 10ms/16k
        timeit("document.edit / analysis (not incremental part)", || {
            let all_frame_infos = timeit("all frame infos", || {
                QueriedDocument::document_all_frame_infos(
                    &self.definitions,
                    &self.annotations,
                    &id.path,
                )
            });

            self.local_diagnostics = parsed_document.syntax_errors();
            self.all_frame_infos = all_frame_infos;

            // TODO split into seperate functions
            let (directly_reachable_import_urls, directly_reachable_other_urls) =
                queried_document.reachable_urls(&self.references, &id.uri);
            self.directly_reachable_import_urls = directly_reachable_import_urls;
            self.directly_reachable_other_urls = directly_reachable_other_urls;
        });
    }

    fn edit_range_boxes(&mut self, changes: &[Change]) {
        rayon_join::join!(
            || edit_vec_rb(changes, &mut self.definitions),
            || edit_vec_rb(changes, &mut self.references),
            || edit_vec_rb(changes, &mut self.annotations),
            || self.iri_locations.par_iter_mut().for_each(|(_, rbs)| {
                for rb in rbs {
                    rb.edit(changes.iter());
                }
            })
        );
    }
}
