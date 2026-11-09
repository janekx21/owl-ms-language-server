use crate::consts::{
    child_keywords_for_kind, keyword_hover_info, DECIMAL_IRI, FLOAT_IRI, INTEGER_IRI, STRING_IRI,
};
use crate::error::{Error, Result, ResultIterator};
use crate::iri::{Iri, ToIriUnchecked, TryToIri};
use crate::pos::Position;
use crate::queries::{
    self, treesitter_highlight_capture_into_semantic_token_type_index, OMN_NODE_TYPES,
};
use crate::range::{Change, RangeBox};
use crate::workspace::{
    build_iri_locations, capture_by_name, changes_from_lsp, edit_vec_rb, extend_vec_rb,
    iri_to_onology_iri, node_text, post_change_ranges, retain_vec_rb, retain_vec_rb_on_remove,
    trim_full_iri_rope_slice, trim_string_value, word_before_character, Annotation, Diagnostic,
    DiagnosticKind, DocumentId, FormattingSettings, FrameInfo, FrameType, Highlights, HoverResult,
    IriAtPosition, IriDefinition, IriRenameInfo, KeywordAction, Location, OntologyDocument,
    OntologyId, ParsedDocument, RenameInfo, UnwrappedQueryMatch, Workspace,
};
use crate::{
    debugging::timeit, queries::OMN_ALL_QUERIES, range::Range, rope_provider::RopeProvider,
    LANGUAGE_OMN,
};
use crate::{split_iri, USizeextra};
use itertools::Itertools;
use log::{debug, info, trace};
use pretty::RcDoc;
use rayon::iter::{IntoParallelRefIterator, IntoParallelRefMutIterator, ParallelIterator};
use rayon::slice::ParallelSliceMut;
use ropey::Rope;
use std::cell::RefCell;
use std::collections::HashSet;
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
    queried_document: QueriedDocument,
    stage2: Stage2Document,
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

    fn directly_imports(&self) -> Vec<&Iri> {
        self.stage2.directly_reachable_import_iris.iter().collect()
    }

    fn directly_references_doc(&self) -> Vec<&Iri> {
        self.stage2
            .directly_reachable_import_iris
            .iter()
            .chain(self.stage2.directly_reachable_other_iris.iter())
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
        full_iri.try_to_abbriviated(&self.queried_document.prefixes)
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
                iri: trim_full_iri_rope_slice(node_text(&node, self.rope()))
                    .try_to_iri()
                    .ok()?,
                range,
            }),
            "simple_iri" | "abbreviated_iri" => {
                let iri = node_text(&node, self.rope()).try_to_iri().ok()?;
                let full_iri = self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);
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
        let is_ontology = parent_kind == "ontology_iri";
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
                    full_iri: trim_full_iri_rope_slice(node_text(&node, self.rope()))
                        .try_to_iri()
                        .ok()?,
                    is_import,
                    is_ontology,
                    frame_type,
                },
                range,
            )),
            "simple_iri" | "abbreviated_iri" => {
                let iri = node_text(&node, self.rope()).try_to_iri().ok()?;
                let full_iri = self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);
                Some(RangeBox::new(
                    IriAtPosition {
                        full_iri,
                        is_import,
                        is_ontology,
                        frame_type,
                    },
                    range,
                ))
            }
            _ => None,
        }
    }

    fn rename_range(&self, pos: Position) -> Option<Range> {
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

                let short_iri_start = range.start.moved_right(col_offset.to_u32(), self.rope());

                if pos < short_iri_start {
                    // prefix rename
                    Some(Range {
                        start: range.start,
                        end: short_iri_start.moved_left(1, self.rope()),
                    })
                } else {
                    Some(Range {
                        start: range.start.moved_right(col_offset.to_u32(), self.rope()),
                        ..range
                    })
                }
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
                let full_iri =
                    trim_full_iri_rope_slice(node_text(&node, self.rope())).try_to_iri()?;
                Some(RenameInfo::Iri(IriRenameInfo {
                    full_iri,
                    new_iri: Some(new_name.parse()?),
                    frame_type,
                    original: new_name.to_string(),
                }))
            }
            "simple_iri" => {
                let iri = node_text(&node, self.rope()).try_to_iri()?;
                Some(RenameInfo::Iri(IriRenameInfo {
                    full_iri: self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri),
                    new_iri: self.abbreviated_iri_to_full_iri(&new_name.parse()?),
                    frame_type,
                    original: new_name.to_string(),
                }))
            }
            "abbreviated_iri" => {
                let range: Range = node.range().into();
                let text = node_text(&node, self.rope()).to_string();
                let col_offset = text
                    .find(':')
                    .expect("abbreviated_iri to contain at least one :")
                    + 1;

                let short_iri_start = range.start.moved_right(col_offset.to_u32(), self.rope());

                let (prefix, _) = text
                    .split_once(':')
                    .expect("abbreviated_iri to contain at least one :");

                if pos < short_iri_start {
                    Some(RenameInfo::Prefix(prefix.to_string(), new_name.to_string()))
                } else {
                    let new_original = format!("{prefix}:{new_name}");
                    Some(RenameInfo::Iri(IriRenameInfo {
                        full_iri: self
                            .abbreviated_iri_to_full_iri(&text.parse()?)
                            .unwrap_or(text.parse()?),
                        new_iri: self.abbreviated_iri_to_full_iri(&new_original.parse()?),
                        frame_type,
                        original: new_original,
                    }))
                }
            }
            _ => None,
        })
    }

    fn rename_edits(&self, rename_info: &RenameInfo) -> Vec<RangeBox<String>> {
        match rename_info {
            RenameInfo::Iri(rename_info) => {
                let IriRenameInfo {
                    full_iri,
                    new_iri,
                    frame_type,
                    original,
                } = rename_info;

                self.parsed_document
                    .query(&OMN_ALL_QUERIES.iri_query_all)
                    .into_iter()
                    .map(|m| {
                        let iri_capture = m.captures.first().expect("one capture");

                        let iri = match iri_capture.node.kind {
                            "full_iri" => trim_full_iri_rope_slice(iri_capture.node.text)
                                .try_to_iri()
                                .ok(),
                            "simple_iri" | "abbreviated_iri" => {
                                let iri = iri_capture.node.text.try_to_iri()?;
                                Some(self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri))
                            }
                            _ => unreachable!(),
                        };
                        let range = iri_capture.node.range;
                        let node_frame_type = FrameType::parse(
                            iri_capture
                                .node
                                .parent_kind
                                .expect("iris should have parents"),
                        );

                        if iri.as_ref() == Some(full_iri) && &node_frame_type == frame_type {
                            Ok(Some(RangeBox::new(
                                new_iri
                                    .clone()
                                    .map(|new_iri| self.full_iri_to_shorter_iri(&new_iri))
                                    .unwrap_or(original.clone()),
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
            RenameInfo::Prefix(old_prefix, new_prefix) => {
                if self.queried_document.prefixes.contains_key(new_prefix)
                    && old_prefix != new_prefix
                {
                    // this is a conflict, lets not rename anything!
                    return vec![];
                }
                if let Some(p) = self.queried_document.prefixes.get(old_prefix) {
                    let (name_range, _) = prefix_parts_in_range(&self.parsed_document, *p.range())
                        .expect("valid prefix in this range");

                    let mut edits = vec![RangeBox::new(format!("{new_prefix}:"), name_range)];
                    for iri in self.references() {
                        if let Some(postfix) = iri.value().strip_prefix(p.value()) {
                            if !postfix.contains('/') && !postfix.contains('#') {
                                edits.push(RangeBox::new(
                                    if new_prefix.is_empty() {
                                        postfix.into()
                                    } else {
                                        format!("{new_prefix}:{postfix}")
                                    },
                                    *iri.range(),
                                ));
                            }
                        }
                    }
                    edits
                } else {
                    vec![]
                }
            }
        }
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
        self.query_range(&OMN_ALL_QUERIES.iri_query_all, range)
            .into_iter()
            .flat_map(|match_| match_.captures)
            .filter_map(|capture| {
                let iri = trim_full_iri_rope_slice(capture.node.text)
                    .try_to_iri()
                    .ok()?;
                let iri = self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);
                Some(RangeBox::new(iri, capture.node.range))
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
            .filter_map(|kind| (*queries::OMN_KEYWORDS_MAP).get(kind).cloned())
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
                            frame.is_deprecated(),
                        ))
                    }
                })
                .collect_vec()
        } else {
            Vec::new()
        }
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
            self.stage2.directly_reachable_import_iris.len(),
            self.stage2.directly_reachable_other_iris.len(),
            self.stage2.all_frame_infos.values().map(|a| a.annotations.len()).sum::<usize>(),
            self.stage2.all_frame_infos.values().map(|a| a.definitions.len()).sum::<usize>(),
        )
    }

    fn add_annotation_edits(&self, annotation: Annotation) -> Vec<RangeBox<String>> {
        let iri_under_cursor = annotation.frame_iri;

        let fi = self.find_frame_info(&iri_under_cursor);
        let Some(fi) = fi else {
            debug!("no frame info");
            return vec![];
        };

        let Some(l) = fi.definitions.iter().find(|d| &d.uri == self.uri()) else {
            debug!("not defined here");
            return vec![];
        };

        let Ok((_prefix_start, prefix_end, _split_char)) = split_iri(&iri_under_cursor) else {
            debug!("cant split {iri_under_cursor}");
            return vec![];
        };

        let label = stringcase::title_case(prefix_end);

        // The label is there already
        if fi.annotations.iter().any(|a| {
            a.iri == annotation.iri && a.string_value.to_lowercase() == label.to_lowercase()
        }) {
            debug!("label is here already");
            return vec![];
        }

        let annotation_iri = self
            .full_iri_to_abbreviated_iri(&annotation.iri)
            .unwrap_or(annotation.iri.to_string());

        vec![RangeBox::new(
            format!("\n    Annotations: {annotation_iri} \"{label}\""),
            Range::new(l.range.end, l.range.end),
        )]
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
        let queried_document: QueriedDocument = into_queried(&parsed_document);

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
    pub fn reachable_iris(
        &self,
        document_references: &[RangeBox<Iri>],
        own_iri: &Iri,
    ) -> (Vec<Iri>, Vec<Iri>) {
        let imports = self
            .imports
            .iter()
            .map(|rb| rb.value().clone())
            .collect_vec();

        // Other iris include prefixes
        let mut other_iris = self
            .prefixes
            .iter()
            // Filter out the empty prefix ":"
            .filter_map(|(prefix, iri)| {
                if prefix.is_empty() {
                    None
                } else {
                    Some(iri.value())
                }
            })
            .filter_map(|i| i.parse::<Iri>().ok())
            // Filter out the current document as a prefix (most likely the empty prefix ":")
            .filter(|iri| iri != own_iri)
            .collect_vec();

        let referenced_iris = document_references
            .iter()
            .filter_map(|iri| iri_to_parent_url_str(iri.value()))
            .unique()
            .filter_map(|s| s.parse().ok());

        other_iris.extend(referenced_iris);

        (imports, other_iris)
    }

    pub fn abbreviated_iri_to_full_iri(&self, abbreviated_iri: &Iri) -> Option<Iri> {
        abbreviated_iri.try_to_full(&self.prefixes)
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
            .query_range(&OMN_ALL_QUERIES.annotation_query, range)
            .iter()
            .filter_map(|m| {
                let frame_iri_capture =
                    capture_by_name(&OMN_ALL_QUERIES.annotation_query, &m.captures, "frame_iri")
                        .expect("frame capture");
                let annotation_iri_capture =
                    capture_by_name(&OMN_ALL_QUERIES.annotation_query, &m.captures, "iri")
                        .expect("iri capture");
                let value_capture =
                    capture_by_name(&OMN_ALL_QUERIES.annotation_query, &m.captures, "literal")
                        .expect("value capture");
                let frame_capture =
                    capture_by_name(&OMN_ALL_QUERIES.annotation_query, &m.captures, "frame")
                        .expect("frame_capture");

                let datatype_capture =
                    capture_by_name(&OMN_ALL_QUERIES.annotation_query, &m.captures, "datatype");

                let language_capture =
                    capture_by_name(&OMN_ALL_QUERIES.annotation_query, &m.captures, "language");

                let frame_iri = trim_full_iri_rope_slice(frame_iri_capture.node.text)
                    .try_to_iri()
                    .ok()?;
                let frame_iri = self
                    .abbreviated_iri_to_full_iri(&frame_iri)
                    .unwrap_or(frame_iri);

                let annotation_iri = trim_full_iri_rope_slice(annotation_iri_capture.node.text)
                    .try_to_iri()
                    .ok()?;
                let annotation_iri = self
                    .abbreviated_iri_to_full_iri(&annotation_iri)
                    .unwrap_or(annotation_iri);

                let literal = trim_string_value(&value_capture.node.text.to_string());

                let language = language_capture
                    .map(|c| c.node.text.to_string().trim_start_matches('@').to_string())
                    .and_then(|tag| language::Language::try_from(tag).ok());
                // TODO #180 spawn diagnostics about wrong language

                let datatype =
                    datatype_capture.map_or(Some(STRING_IRI.to_iri_unchecked()), |c| {
                        match c.node.kind {
                            "keyword_integer" => Some(INTEGER_IRI.to_iri_unchecked()),
                            "keyword_decimal" => Some(DECIMAL_IRI.to_iri_unchecked()),
                            "keyword_float" => Some(FLOAT_IRI.to_iri_unchecked()),
                            "keyword_string" => Some(STRING_IRI.to_iri_unchecked()),
                            _ => {
                                let iri = c.node.text.try_to_iri().ok()?;
                                Some(self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri))
                            }
                        }
                    })?;

                // The value can decide the type
                // TODO maybe check with range of annotation property
                let datatype = match value_capture.node.kind {
                    "integer_literal" => INTEGER_IRI.to_iri_unchecked(),
                    "decimal_literal" => DECIMAL_IRI.to_iri_unchecked(),
                    "floating_point_literal" => FLOAT_IRI.to_iri_unchecked(),
                    _ => datatype,
                };

                let annotation = Annotation {
                    frame_iri,
                    iri: annotation_iri,
                    string_value: literal,
                    language,
                    datatype,
                };

                Some(RangeBox::new(annotation, frame_capture.node.range))
            })
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
            .query_range(&OMN_ALL_QUERIES.frame_query, range)
            .iter()
            .filter_map(|m| match &m.captures[..] {
                [frame_iri_capture, frame_capture] => {
                    let frame_iri_parent_kind = frame_iri_capture
                        .node
                        .parent_kind
                        .as_ref()
                        .expect("All frame IRIs should have parents");

                    let frame_iri = trim_full_iri_rope_slice(frame_iri_capture.node.text)
                        .try_to_iri()
                        .ok()?;
                    let frame_iri = self
                        .abbreviated_iri_to_full_iri(&frame_iri)
                        .unwrap_or(frame_iri);

                    Some(RangeBox::new(
                        IriDefinition {
                            iri: frame_iri,
                            kind: FrameType::parse(frame_iri_parent_kind),
                        },
                        frame_capture.node.range,
                    ))
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
            .query_range(&OMN_ALL_QUERIES.iri_query_references, range)
            .iter()
            .filter_map(|m| match &m.captures[..] {
                [iri_capture] => {
                    let iri = trim_full_iri_rope_slice(iri_capture.node.text)
                        .try_to_iri()
                        .ok()?;
                    let iri = self.abbreviated_iri_to_full_iri(&iri).unwrap_or(iri);

                    Some(RangeBox::new(iri, iri_capture.node.range))
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

        let (directly_reachable_import_iris, directly_reachable_other_iris) =
            timeit("reachable iris", || {
                self.reachable_iris(&references, &id.uri.clone().into())
                // TODO!! use id.iri
            });

        let iri_locations = build_iri_locations(&references);

        let mut local_diagnostics = timeit("syntax errors", || syntax_errors(parsed_document));
        local_diagnostics.extend(semantic_local_diagnostics(&references, &self.prefixes));
        Stage2Document {
            definitions,
            references,
            annotations,
            all_frame_infos,
            local_diagnostics,
            directly_reachable_import_iris,
            directly_reachable_other_iris,
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
            self.ontology_id = ontology_id(parsed_document);
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
            let additional_imports = imports_in_range(parsed_document, *di);

            self.imports.extend(additional_imports);
        }

        for di in post_change_ranges {
            let additional_prefixes = prefixes_in_range(parsed_document, *di);

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

        let ontology_id = ontology_id(&val);
        let prefixes = prefixes(&val);
        let imports = imports(&val);

        QueriedDocument {
            ontology_id,
            prefixes,
            imports,
        }
    }
}

fn into_queried(parsed_document: &ParsedDocument) -> QueriedDocument {
    debug!("ParsedDocument -> QueriedDocument");

    let ((ontology_id, prefixes), imports) = rayon::join(
        || {
            rayon::join(
                || ontology_id(parsed_document),
                || prefixes(parsed_document),
            )
        },
        || imports(parsed_document),
    );

    QueriedDocument {
        ontology_id,
        prefixes,
        imports,
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
    /// These include only IRI's from the imports
    directly_reachable_import_iris: Vec<Iri>,
    /// These include all other IRI's that can be found in this document
    directly_reachable_other_iris: Vec<Iri>,
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

            self.all_frame_infos = all_frame_infos;

            // TODO split into seperate functions
            let (directly_reachable_import_iris, directly_reachable_other_iris) =
                queried_document.reachable_iris(&self.references, &id.uri.clone().into()); // TODO!! use id.iri
            self.directly_reachable_import_iris = directly_reachable_import_iris;
            self.directly_reachable_other_iris = directly_reachable_other_iris;

            self.local_diagnostics = syntax_errors(parsed_document);

            self.local_diagnostics.extend(semantic_local_diagnostics(
                &self.references,
                &queried_document.prefixes,
            ));
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

fn semantic_local_diagnostics(
    references: &[RangeBox<Iri>],
    prefixes: &HashMap<String, RangeBox<String>>,
) -> Vec<Diagnostic> {
    // Prefix not defined
    let add = prefix_not_defined_diagnostics(references);
    let add = add.chain(default_prefix_not_defined_diagnostics(prefixes));

    add.collect_vec()
}

fn prefix_not_defined_diagnostics(
    references: &[RangeBox<Iri>],
) -> impl Iterator<Item = Diagnostic> + use<'_> {
    let abbriviated_iris_in_refs = references
        .iter()
        .filter_map(|rb| try_into_abbriviated(rb.value()).map(|a| RangeBox::new(a, *rb.range())));

    let add = abbriviated_iris_in_refs.map(|x| Diagnostic {
        range: *x.range(),
        kind: DiagnosticKind::PrefixNotDefined(x.value().0.to_string()),
    });
    add
}

fn default_prefix_not_defined_diagnostics(
    prefixes: &HashMap<String, RangeBox<String>>,
) -> impl Iterator<Item = Diagnostic> + use<'_> {
    if prefixes.get("").is_none() {
        Some(Diagnostic {
            range: Range::ZERO,
            kind: DiagnosticKind::DefaultPrefixNotDefined,
        })
    } else {
        None
    }
    .into_iter()
}

// TODO this following function will probibly not catch all cases

fn try_into_abbriviated(iri: &Iri) -> Option<(&str, &str)> {
    if iri.contains('/') {
        None
    } else if let Some((l, r)) = iri.as_str().split_once(':') {
        if l.is_empty() {
            None
        } else {
            Some((l, r))
        }
    } else {
        None
    }
}

/// Returns the ontology IRI if possible and the version IRI if possible.
pub fn ontology_id(parsed_document: &ParsedDocument) -> Option<RangeBox<OntologyId>> {
    match &parsed_document.query(&OMN_ALL_QUERIES.ontology)[..] {
        [] => None,
        [ontology] => match &ontology.captures[..] {
            [] => None,
            // This should be a full IRI so lets trim it
            [iri_capture] => Some(RangeBox::new(
                (
                    trim_full_iri_rope_slice(iri_capture.node.text)
                        .try_to_iri()
                        .ok()?,
                    None,
                ),
                iri_capture.node.range,
            )),
            [iri_capture, version_iri_capture] => Some(RangeBox::new(
                (
                    trim_full_iri_rope_slice(iri_capture.node.text)
                        .try_to_iri()
                        .ok()?,
                    Some(
                        trim_full_iri_rope_slice(version_iri_capture.node.text)
                            .try_to_iri()
                            .ok()?,
                    ),
                ),
                Range::new(
                    iri_capture.node.range.start,
                    version_iri_capture.node.range.end,
                ),
            )),
            _ => unreachable!("The query has only one capture"),
        },
        _ => unreachable!("the grammar only parses ontology zero or one time"),
    }
}

/// Returns the prefixes of a document (without colon `:`) in a prefix name to iri map.
///
/// Some prefixes should always be defined
///
/// ```owl-ms
/// Prefix: rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
/// Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
/// Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>
/// Prefix: owl: <http://www.w3.org/2002/07/owl#>
/// ```
pub fn prefixes(parsed_document: &ParsedDocument) -> HashMap<String, RangeBox<String>> {
    parsed_document
        .query(&OMN_ALL_QUERIES.prefix)
        .into_iter()
        .map(|m| match &m.captures[..] {
            [name_capture, iri_capture] => (
                name_capture
                    .node
                    .text
                    .to_string()
                    .trim_end_matches(':')
                    .to_string(),
                RangeBox::new(
                    trim_full_iri_rope_slice(iri_capture.node.text).to_string(),
                    Range::new(name_capture.node.range.start, iri_capture.node.range.end),
                ),
            ),
            _ => unreachable!(),
        })
        // Horned owl has no default here. Let's keep it out for now.
        // .chain(
        //     STANDARD_PREFIX_NAMES
        //         .iter()
        //         .map(|(a, b)| (a.to_string(), b.to_string())),
        // )
        .unique_by(|(k, v)| (k.clone(), v.value().clone()))
        .collect()
}

pub fn prefixes_in_range(
    parsed_document: &ParsedDocument,
    range: Range,
) -> HashMap<String, RangeBox<String>> {
    parsed_document
        .query_range(&OMN_ALL_QUERIES.prefix, range)
        .into_iter()
        .map(|m| match &m.captures[..] {
            [name_capture, iri_capture] => (
                name_capture
                    .node
                    .text
                    .to_string()
                    .trim_end_matches(':')
                    .to_string(),
                RangeBox::new(
                    trim_full_iri_rope_slice(iri_capture.node.text).to_string(),
                    Range::new(name_capture.node.range.start, iri_capture.node.range.end),
                ),
            ),
            _ => unreachable!(),
        })
        .unique_by(|(k, v)| (k.clone(), v.value().clone()))
        .collect()
}

pub fn prefix_parts_in_range(
    parsed_document: &ParsedDocument,
    range: Range,
) -> Option<(Range, Range)> {
    parsed_document
        .query_range(&OMN_ALL_QUERIES.prefix, range)
        .into_iter()
        .map(|m| match &m.captures[..] {
            [name_capture, iri_capture] => (name_capture.node.range, iri_capture.node.range),
            _ => unreachable!(),
        })
        .next()
}

pub fn imports(parsed_document: &ParsedDocument) -> Vec<RangeBox<Iri>> {
    parsed_document
        .query(&OMN_ALL_QUERIES.import_query)
        .iter()
        .filter_map(|m| match &m.captures[..] {
            [iri_capture] => trim_full_iri_rope_slice(iri_capture.node.text)
                .try_to_iri()
                .ok()
                .map(|iri| RangeBox::new(iri, iri_capture.node.range)),
            _ => unimplemented!(),
        })
        .collect_vec()
}

pub fn imports_in_range(parsed_document: &ParsedDocument, range: Range) -> Vec<RangeBox<Iri>> {
    parsed_document
        .query_range(&OMN_ALL_QUERIES.import_query, range)
        .iter()
        .filter_map(|m| match &m.captures[..] {
            [iri_capture] => trim_full_iri_rope_slice(iri_capture.node.text)
                .try_to_iri()
                .ok()
                .map(|iri| RangeBox::new(iri, iri_capture.node.range)),
            _ => unimplemented!(),
        })
        .collect_vec()
}

/// Generate the diagnostics for a single node, walking recursively down to every child and every syntax error within
pub fn syntax_errors(parsed_document: &ParsedDocument) -> Vec<Diagnostic> {
    let mut cursor = parsed_document.tree().root_node().walk();
    let mut diagnostics = Vec::<Diagnostic>::new();

    loop {
        let node = cursor.node();

        if node.is_error() {
            let leaf = first_leaf(node);
            let range: Range = cursor.node().range().into();

            let prev_node = node.prev_sibling().unwrap_or(node.parent().unwrap_or(node));

            let mut prev = if let Some(prev_sib) = node.prev_sibling() {
                let prev_sibs_last_leaf = get_last_leaf(prev_sib);
                prev_sibs_last_leaf
            } else {
                prev_node
            };

            let mut parent_chain = vec![prev.kind()];
            while let Some(par) = prev.parent() {
                parent_chain.push(par.kind());
                prev = par;
            }

            let possible_nodes = possible_nodes(node)
                .iter()
                .map(ToString::to_string)
                .collect();
            debug!("Possible nodes: {possible_nodes:#?}");
            debug!("Parent chain: {parent_chain:#?}");

            let parent_chain_children = parent_chain
                .iter()
                .flat_map(|kind| {
                    OMN_NODE_TYPES
                        .get(&(*kind).to_string())
                        .iter()
                        .flat_map(|t| {
                            t.children
                                .types
                                .iter()
                                .map(|c| c.type_.clone())
                                .collect_vec()
                        })
                        .collect_vec()
                })
                .collect::<HashSet<_>>();

            let valid_children = parent_chain_children.intersection(&possible_nodes);

            let valid_children: String = valid_children
                .map(|nt| node_type_to_string(nt))
                .sorted_unstable()
                .join(", ");

            let parent = parent_chain
                .iter()
                .map(|parent_kind| node_type_to_string(parent_kind))
                .join(" < ");

            diagnostics.push(Diagnostic {
                range,
                kind: DiagnosticKind::SyntaxError {
                    valid_children,
                    parent,
                    msg: leaf
                        .to_string()
                        .trim_start_matches('(')
                        .trim_end_matches(')')
                        .to_string(),
                },
            });

            // move along
            while !cursor.goto_next_sibling() {
                // move out
                if !cursor.goto_parent() {
                    // this node has no parent, it's the root
                    return diagnostics;
                }
            }
        } else if node.has_error() {
            // move in
            let has_child = cursor.goto_first_child(); // should always work

            if !has_child {
                while !cursor.goto_next_sibling() {
                    // move out
                    if !cursor.goto_parent() {
                        // this node has no parent, it's the root
                        return diagnostics;
                    }
                }
            }
        } else {
            // move along
            while !cursor.goto_next_sibling() {
                // move out
                if !cursor.goto_parent() {
                    // this node has no parent, it's the root
                    return diagnostics;
                }
            }
        }
    }
}

fn possible_nodes(node: Node<'_>) -> HashSet<&'static str> {
    if !node.is_error() {
        return HashSet::new();
    }

    let state = first_leaf(node).parse_state();

    let names = LANGUAGE_OMN
        .lookahead_iterator(state)
        .map(|mut it| it.iter_names().collect())
        .unwrap_or_default();

    names
}

fn first_leaf(node: Node<'_>) -> Node<'_> {
    let mut current = node;
    while let Some(child) = current.child(0) {
        current = child;
    }
    current
}

fn get_last_leaf(node: Node<'_>) -> Node<'_> {
    debug!("Get last leaf of {node}");
    if node.child_count() > 0 {
        if let Some(c) = node.child(node.child_count() - 1) {
            debug!("Goto child {c} with index {}", node.child_count() - 1);
            get_last_leaf(c)
        } else {
            node
        }
    } else {
        debug!("End on {node}");
        node
    }
}

fn node_type_to_string(node_type: &str) -> String {
    node_type
        .trim_start_matches("keyword_")
        .split_terminator('_')
        .map(capitalize_string)
        .join(" ")
}

fn capitalize_string(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}
