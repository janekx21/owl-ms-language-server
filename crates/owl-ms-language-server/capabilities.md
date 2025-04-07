Extracted capabilities

### Helix
```rust
ClientCapabilities {
    workspace: Some(WorkspaceClientCapabilities {
        apply_edit: Some(true),
        workspace_edit: Some(WorkspaceEditClientCapabilities {
            document_changes: Some(true),
            resource_operations: Some([Create, Rename, Delete]),
            failure_handling: Some(Abort),
            normalizes_line_endings: Some(false),
            change_annotation_support: None,
        }),
        did_change_configuration: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(false),
        }),
        did_change_watched_files: Some(DidChangeWatchedFilesClientCapabilities {
            dynamic_registration: Some(true),
            relative_pattern_support: Some(false),
        }),
        symbol: Some(WorkspaceSymbolClientCapabilities {
            dynamic_registration: Some(false),
            symbol_kind: None,
            tag_support: None,
            resolve_support: None,
        }),
        execute_command: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(false),
        }),
        workspace_folders: Some(true),
        configuration: Some(true),
        semantic_tokens: None,
        code_lens: None,
        file_operations: None,
        inline_value: None,
        inlay_hint: Some(InlayHintWorkspaceClientCapabilities {
            refresh_support: Some(false),
        }),
        diagnostic: None,
    }),
    text_document: Some(TextDocumentClientCapabilities {
        synchronization: None,
        completion: Some(CompletionClientCapabilities {
            dynamic_registration: None,
            completion_item: Some(CompletionItemCapability {
                snippet_support: Some(true),
                commit_characters_support: None,
                documentation_format: None,
                deprecated_support: Some(true),
                preselect_support: None,
                tag_support: Some(TagSupport {
                    value_set: [Deprecated],
                }),
                insert_replace_support: Some(true),
                resolve_support: Some(CompletionItemCapabilityResolveSupport {
                    properties: ["documentation", "detail", "additionalTextEdits"],
                }),
                insert_text_mode_support: None,
                label_details_support: None,
            }),
            completion_item_kind: Some(CompletionItemKindCapability { value_set: None }),
            context_support: None,
            insert_text_mode: None,
            completion_list: None,
        }),
        hover: Some(HoverClientCapabilities {
            dynamic_registration: None,
            content_format: Some([Markdown]),
        }),
        signature_help: Some(SignatureHelpClientCapabilities {
            dynamic_registration: None,
            signature_information: Some(SignatureInformationSettings {
                documentation_format: Some([Markdown]),
                parameter_information: Some(ParameterInformationSettings {
                    label_offset_support: Some(true),
                }),
                active_parameter_support: Some(true),
            }),
            context_support: None,
        }),
        references: None,
        document_highlight: None,
        document_symbol: None,
        formatting: None,
        range_formatting: None,
        on_type_formatting: None,
        declaration: None,
        definition: None,
        type_definition: None,
        implementation: None,
        code_action: Some(CodeActionClientCapabilities {
            dynamic_registration: None,
            code_action_literal_support: Some(CodeActionLiteralSupport {
                code_action_kind: CodeActionKindLiteralSupport {
                    value_set: [
                        "",
                        "quickfix",
                        "refactor",
                        "refactor.extract",
                        "refactor.inline",
                        "refactor.rewrite",
                        "source",
                        "source.organizeImports",
                    ],
                },
            }),
            is_preferred_support: Some(true),
            disabled_support: Some(true),
            data_support: Some(true),
            resolve_support: Some(CodeActionCapabilityResolveSupport {
                properties: ["edit", "command"],
            }),
            honors_change_annotations: None,
        }),
        code_lens: None,
        document_link: None,
        color_provider: None,
        rename: Some(RenameClientCapabilities {
            dynamic_registration: Some(false),
            prepare_support: Some(true),
            prepare_support_default_behavior: None,
            honors_change_annotations: Some(false),
        }),
        publish_diagnostics: Some(PublishDiagnosticsClientCapabilities {
            related_information: None,
            tag_support: None,
            version_support: Some(true),
            code_description_support: None,
            data_support: None,
        }),
        folding_range: None,
        selection_range: None,
        linked_editing_range: None,
        call_hierarchy: None,
        semantic_tokens: None,
        moniker: None,
        type_hierarchy: None,
        inline_value: None,
        inlay_hint: Some(InlayHintClientCapabilities {
            dynamic_registration: Some(false),
            resolve_support: None,
        }),
        diagnostic: None,
    }),
    window: Some(WindowClientCapabilities {
        work_done_progress: Some(true),
        show_message: None,
        show_document: None,
    }),
    general: Some(GeneralClientCapabilities {
        regular_expressions: None,
        markdown: None,
        stale_request_support: None,
        position_encodings: Some([
            PositionEncodingKind("utf-8"),
            PositionEncodingKind("utf-32"),
            PositionEncodingKind("utf-16"),
        ]),
    }),
    experimental: None,
};
```

### VS Code
```rust
ClientCapabilities {
    workspace: Some(WorkspaceClientCapabilities {
        apply_edit: Some(true),
        workspace_edit: Some(WorkspaceEditClientCapabilities {
            document_changes: Some(true),
            resource_operations: Some([Create, Rename, Delete]),
            failure_handling: Some(TextOnlyTransactional),
            normalizes_line_endings: Some(true),
            change_annotation_support: Some(ChangeAnnotationWorkspaceEditClientCapabilities {
                groups_on_label: Some(true),
            }),
        }),
        did_change_configuration: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        did_change_watched_files: Some(DidChangeWatchedFilesClientCapabilities {
            dynamic_registration: Some(true),
            relative_pattern_support: Some(true),
        }),
        symbol: Some(WorkspaceSymbolClientCapabilities {
            dynamic_registration: Some(true),
            symbol_kind: Some(SymbolKindCapability {
                value_set: Some([
                    File,
                    Module,
                    Namespace,
                    Package,
                    Class,
                    Method,
                    Property,
                    Field,
                    Constructor,
                    Enum,
                    Interface,
                    Function,
                    Variable,
                    Constant,
                    String,
                    Number,
                    Boolean,
                    Array,
                    Object,
                    Key,
                    Null,
                    EnumMember,
                    Struct,
                    Event,
                    Operator,
                    TypeParameter,
                ]),
            }),
            tag_support: Some(TagSupport {
                value_set: [Deprecated],
            }),
            resolve_support: Some(WorkspaceSymbolResolveSupportCapability {
                properties: ["location.range"],
            }),
        }),
        execute_command: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        workspace_folders: Some(true),
        configuration: Some(true),
        semantic_tokens: Some(SemanticTokensWorkspaceClientCapabilities {
            refresh_support: Some(true),
        }),
        code_lens: Some(CodeLensWorkspaceClientCapabilities {
            refresh_support: Some(true),
        }),
        file_operations: Some(WorkspaceFileOperationsClientCapabilities {
            dynamic_registration: Some(true),
            did_create: Some(true),
            will_create: Some(true),
            did_rename: Some(true),
            will_rename: Some(true),
            did_delete: Some(true),
            will_delete: Some(true),
        }),
        inline_value: Some(InlineValueWorkspaceClientCapabilities {
            refresh_support: Some(true),
        }),
        inlay_hint: Some(InlayHintWorkspaceClientCapabilities {
            refresh_support: Some(true),
        }),
        diagnostic: None,
    }),
    text_document: Some(TextDocumentClientCapabilities {
        synchronization: Some(TextDocumentSyncClientCapabilities {
            dynamic_registration: Some(true),
            will_save: Some(true),
            will_save_wait_until: Some(true),
            did_save: Some(true),
        }),
        completion: Some(CompletionClientCapabilities {
            dynamic_registration: Some(true),
            completion_item: Some(CompletionItemCapability {
                snippet_support: Some(true),
                commit_characters_support: Some(true),
                documentation_format: Some([Markdown, PlainText]),
                deprecated_support: Some(true),
                preselect_support: Some(true),
                tag_support: Some(TagSupport {
                    value_set: [Deprecated],
                }),
                insert_replace_support: Some(true),
                resolve_support: Some(CompletionItemCapabilityResolveSupport {
                    properties: ["documentation", "detail", "additionalTextEdits"],
                }),
                insert_text_mode_support: Some(InsertTextModeSupport {
                    value_set: [AsIs, AdjustIndentation],
                }),
                label_details_support: Some(true),
            }),
            completion_item_kind: Some(CompletionItemKindCapability {
                value_set: Some([
                    Text,
                    Method,
                    Function,
                    Constructor,
                    Field,
                    Variable,
                    Class,
                    Interface,
                    Module,
                    Property,
                    Unit,
                    Value,
                    Enum,
                    Keyword,
                    Snippet,
                    Color,
                    File,
                    Reference,
                    Folder,
                    EnumMember,
                    Constant,
                    Struct,
                    Event,
                    Operator,
                    TypeParameter,
                ]),
            }),
            context_support: Some(true),
            insert_text_mode: Some(AdjustIndentation),
            completion_list: Some(CompletionListCapability {
                item_defaults: Some([
                    "commitCharacters",
                    "editRange",
                    "insertTextFormat",
                    "insertTextMode",
                ]),
            }),
        }),
        hover: Some(HoverClientCapabilities {
            dynamic_registration: Some(true),
            content_format: Some([Markdown, PlainText]),
        }),
        signature_help: Some(SignatureHelpClientCapabilities {
            dynamic_registration: Some(true),
            signature_information: Some(SignatureInformationSettings {
                documentation_format: Some([Markdown, PlainText]),
                parameter_information: Some(ParameterInformationSettings {
                    label_offset_support: Some(true),
                }),
                active_parameter_support: Some(true),
            }),
            context_support: Some(true),
        }),
        references: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        document_highlight: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        document_symbol: Some(DocumentSymbolClientCapabilities {
            dynamic_registration: Some(true),
            symbol_kind: Some(SymbolKindCapability {
                value_set: Some([
                    File,
                    Module,
                    Namespace,
                    Package,
                    Class,
                    Method,
                    Property,
                    Field,
                    Constructor,
                    Enum,
                    Interface,
                    Function,
                    Variable,
                    Constant,
                    String,
                    Number,
                    Boolean,
                    Array,
                    Object,
                    Key,
                    Null,
                    EnumMember,
                    Struct,
                    Event,
                    Operator,
                    TypeParameter,
                ]),
            }),
            hierarchical_document_symbol_support: Some(true),
            tag_support: Some(TagSupport {
                value_set: [Deprecated],
            }),
        }),
        formatting: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        range_formatting: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        on_type_formatting: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        declaration: Some(GotoCapability {
            dynamic_registration: Some(true),
            link_support: Some(true),
        }),
        definition: Some(GotoCapability {
            dynamic_registration: Some(true),
            link_support: Some(true),
        }),
        type_definition: Some(GotoCapability {
            dynamic_registration: Some(true),
            link_support: Some(true),
        }),
        implementation: Some(GotoCapability {
            dynamic_registration: Some(true),
            link_support: Some(true),
        }),
        code_action: Some(CodeActionClientCapabilities {
            dynamic_registration: Some(true),
            code_action_literal_support: Some(CodeActionLiteralSupport {
                code_action_kind: CodeActionKindLiteralSupport {
                    value_set: [
                        "",
                        "quickfix",
                        "refactor",
                        "refactor.extract",
                        "refactor.inline",
                        "refactor.rewrite",
                        "source",
                        "source.organizeImports",
                    ],
                },
            }),
            is_preferred_support: Some(true),
            disabled_support: Some(true),
            data_support: Some(true),
            resolve_support: Some(CodeActionCapabilityResolveSupport {
                properties: ["edit"],
            }),
            honors_change_annotations: Some(false),
        }),
        code_lens: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        document_link: Some(DocumentLinkClientCapabilities {
            dynamic_registration: Some(true),
            tooltip_support: Some(true),
        }),
        color_provider: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        rename: Some(RenameClientCapabilities {
            dynamic_registration: Some(true),
            prepare_support: Some(true),
            prepare_support_default_behavior: Some(Identifier),
            honors_change_annotations: Some(true),
        }),
        publish_diagnostics: Some(PublishDiagnosticsClientCapabilities {
            related_information: Some(true),
            tag_support: Some(TagSupport {
                value_set: [Unnecessary, Deprecated],
            }),
            version_support: Some(false),
            code_description_support: Some(true),
            data_support: Some(true),
        }),
        folding_range: Some(FoldingRangeClientCapabilities {
            dynamic_registration: Some(true),
            range_limit: Some(5000),
            line_folding_only: Some(true),
            folding_range_kind: Some(FoldingRangeKindCapability {
                value_set: Some([Comment, Imports, Region]),
            }),
            folding_range: Some(FoldingRangeCapability {
                collapsed_text: Some(false),
            }),
        }),
        selection_range: Some(SelectionRangeClientCapabilities {
            dynamic_registration: Some(true),
        }),
        linked_editing_range: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        call_hierarchy: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        semantic_tokens: Some(SemanticTokensClientCapabilities {
            dynamic_registration: Some(true),
            requests: SemanticTokensClientCapabilitiesRequests {
                range: Some(true),
                full: Some(Delta { delta: Some(true) }),
            },
            token_types: [
                SemanticTokenType("namespace"),
                SemanticTokenType("type"),
                SemanticTokenType("class"),
                SemanticTokenType("enum"),
                SemanticTokenType("interface"),
                SemanticTokenType("struct"),
                SemanticTokenType("typeParameter"),
                SemanticTokenType("parameter"),
                SemanticTokenType("variable"),
                SemanticTokenType("property"),
                SemanticTokenType("enumMember"),
                SemanticTokenType("event"),
                SemanticTokenType("function"),
                SemanticTokenType("method"),
                SemanticTokenType("macro"),
                SemanticTokenType("keyword"),
                SemanticTokenType("modifier"),
                SemanticTokenType("comment"),
                SemanticTokenType("string"),
                SemanticTokenType("number"),
                SemanticTokenType("regexp"),
                SemanticTokenType("operator"),
                SemanticTokenType("decorator"),
            ],
            token_modifiers: [
                SemanticTokenModifier("declaration"),
                SemanticTokenModifier("definition"),
                SemanticTokenModifier("readonly"),
                SemanticTokenModifier("static"),
                SemanticTokenModifier("deprecated"),
                SemanticTokenModifier("abstract"),
                SemanticTokenModifier("async"),
                SemanticTokenModifier("modification"),
                SemanticTokenModifier("documentation"),
                SemanticTokenModifier("defaultLibrary"),
            ],
            formats: [TokenFormat("relative")],
            overlapping_token_support: Some(false),
            multiline_token_support: Some(false),
            server_cancel_support: Some(true),
            augments_syntax_tokens: Some(true),
        }),
        moniker: None,
        type_hierarchy: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        inline_value: Some(DynamicRegistrationClientCapabilities {
            dynamic_registration: Some(true),
        }),
        inlay_hint: Some(InlayHintClientCapabilities {
            dynamic_registration: Some(true),
            resolve_support: Some(InlayHintResolveClientCapabilities {
                properties: [
                    "tooltip",
                    "textEdits",
                    "label.tooltip",
                    "label.location",
                    "label.command",
                ],
            }),
        }),
        diagnostic: Some(DiagnosticClientCapabilities {
            dynamic_registration: Some(true),
            related_document_support: Some(false),
        }),
    }),
    window: Some(WindowClientCapabilities {
        work_done_progress: Some(true),
        show_message: Some(ShowMessageRequestClientCapabilities {
            message_action_item: Some(MessageActionItemCapabilities {
                additional_properties_support: Some(true),
            }),
        }),
        show_document: Some(ShowDocumentClientCapabilities { support: true }),
    }),
    general: Some(GeneralClientCapabilities {
        regular_expressions: Some(RegularExpressionsClientCapabilities {
            engine: "ECMAScript",
            version: Some("ES2020"),
        }),
        markdown: Some(MarkdownClientCapabilities {
            parser: "marked",
            version: Some("1.1.0"),
            allowed_tags: None,
        }),
        stale_request_support: Some(StaleRequestSupportClientCapabilities {
            cancel: true,
            retry_on_content_modified: [
                "textDocument/semanticTokens/full",
                "textDocument/semanticTokens/range",
                "textDocument/semanticTokens/full/delta",
            ],
        }),
        position_encodings: Some([PositionEncodingKind("utf-16")]),
    }),
    experimental: None,
};
```
