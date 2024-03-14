# owl-ms-language-server: Owl Manchester Syntax Language Server

An incremental analysis assistant for writing ontologies with the [OWL 2 Web Ontology Language | Manchester Syntax](https://www.w3.org/TR/owl2-manchester-syntax/).

## Usage / Editor integration

~Cargo install~ (Currently not possible. Use `git clone` and `cargo build` or `cargo install --path .` instead.) the `owl-ms-language-server` crate and integrate it into your editor.

### [Helix Editor](https://helix-editor.com/)

Add the language server, language and grammar to your `languages.toml`.

```
[language-server.owl-ms-language-server]
command = "owl-ms-language-server"

[[language]]
name = "owl-ms"
injection-regex = "owl-ms"
scope = "text.omn"
file-types = ["omn"]
roots = []
language-servers = ["owl-ms-language-server"]
comment-token = "//"
indent = { tab-width = 4, unit = "    " }
grammar = "owl-ms" # this is the default


[[grammar]]
name = "owl-ms"
source = { git = "https://github.com/janekx21/tree-sitter-owl2-manchester-syntax", rev = "a55d6bdd3104cd64bfe7178395aa6a139b5632a9" } # replace rev with head of the repository
```

See the helix documentation page on [adding languages](https://docs.helix-editor.com/guides/adding_languages.html).

### Vim/Neovim with [coc.nvim](https://github.com/neoclide/coc.nvim)

Merge this setting into your `coc-settings.json` (open with `:CocConfig`).

```
{
  "languageserver": {
    "owl-ms": {
      "command": "owl-ms-language-server",
      "filetypes": ["omn"],
      "rootPatterns":  []
    }
  }
}
```
See [the example config for testing](https://github.com/oxalica/nil/blob/main/dev/vim-coc.nix).

## Roadmap

### Done

- diagnostic (only syntax)
- goto_definition
  - class_iri
- hover
  - class_iri
  - class_frame
- inlay_hint
  - class_iri
- goto_definition
  - annotation_property_iri


### Doing
- goto_definition
  - object_property_iri
  - data_property_iri
  - datatype_iri // not working
  - individual_iri

### Planned

- inlay_hint
  - other frame types
  - others
- hover
  - other frame types
  - keywords
  - others
- document_symbol
- code_action (all kinds of quick actions)
- rename
- references (shows all referenced locations)

### Not planned

- goto_declaration
- goto_type_definition
- goto_implementation
- incoming_calls
- outgoing_calls
- supertypes
- subtypes
- document_highlight (highlights text that corresponds to text under the cursor)
- document_link (all links in a document)
- document_link_resolve (how to resolve a link)
- code_lens
- folding_range
- selection_range
- semantic_tokens
- inline_value
- moniker
- completion
- workspace_diagnostic
- signature_help
- document_color
- color_presentation
- formatting
- on_type_formatting
- linked_editing_range (ranges that have the same content)
- symbol (project-wide symbol search)


## Possible implamentation vectors

- iri types
  - class_iri
  - datatype_iri
  - annotation_property_iri
  - ontology_iri
  - data_property_iri
  - version_iri
  - object_property_iri
  - annotation_property_iri_annotated_list
  - individual_iri

- frames
  - datatype
  - class
  - object property
  - data property
  - annotation property
  - individual
  - misc

# Interesting stuff
- When you apply a text edit you should not re generate the cache for each edit.
- What could be paralell
- what to cache and what to query
