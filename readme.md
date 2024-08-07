# owl-ms-language-server: Owl 2 Manchester Syntax Language Server

An incremental analysis assistant for writing ontologies with the [OWL 2 Web Ontology Language | Manchester Syntax](https://www.w3.org/TR/owl2-manchester-syntax/).

## Getting Started

To use the language server you have to first install it and then integrate it into you editor. This differs for each editor.
To install the owl-ms-language-server crate you currently have to use git and Cargo.

```shell
https://github.com/janekx21/owl-ms-language-server 
cd owl-ms-language-server 
cargo install --path .
```

This installs the langauge server into Cargos's local set of installed binary crates, likely located in `$HOME/.cargo`.
Alternatifly you can use `cargo build` instead of `cargo install --path .` and handle the binary yourself.

## Usage / Editor integration

### [Visual Studio Code](https://code.visualstudio.com/)

\[WIP\]

### [Helix Editor](https://helix-editor.com/)

Add the language server, language and grammar to your `languages.toml`.

```toml
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

Then fetch and build the grammar.

```shell
helix --grammar fetch
helix --grammar build
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
  - datatype
  - class
  - object property
  - data property
  - annotation property
  - individual
- goto_definition
  - class_iri
  - annotation_property_iri
  - object_property_iri
  - data_property_iri
  - individual_iri
  - datatype_iri
    - es lag daran, dass in der grammar datatype nicht hidden war
- completion
  - some keywords work
  - iris // just simple

### Doing


### Planned

- full replace
- hover
  - other frame types
  - keywords
  - others
- document_symbol
- code_action (all kinds of quick actions)
- rename
- references (shows all referenced locations)

### Not planned

- test https://www.ebi.ac.uk/chebi/downloadsForward.do

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
- steps that accour when a document changes with edits
