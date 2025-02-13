# owl-ms-language-server: Owl 2 Manchester Syntax Language Server

An incremental analysis assistant for writing ontologies with the [OWL 2 Web Ontology Language | Manchester Syntax](https://www.w3.org/TR/owl2-manchester-syntax/).

## Getting Started / Editor integration

The first thing you need to know is that in order to run a a language server you obviously need a text editor. Most source code editors support the language server protocol in some form or another. You can find a list of supported tools on [this website](https://microsoft.github.io/language-server-protocol/implementors/tools/) but I recommend [Visual Studio Code](https://code.visualstudio.com/) for beginners. The further instructions for each editor are listed below.

### [Visual Studio Code](https://code.visualstudio.com/)

![vscode install instructions](img/vscode_install.png)

Install the visual studio code plugin by (1) opening visual studio and navigating to view > extensions. (2) Search for "owl-ms" and your should find the "Owl Manchester Syntax" Plugin. (3) Click the "Install" button and wait. You can now use the plugin by opening a `*.omn` file or by running the command "Change Language Mode" and selecting "Owl Manchester Syntax".

- [Distributed via Open VSX Marketplace](https://open-vsx.org/extension/JanekWinkler/vscode-owl-ms)
- [Distributed via VisualStudio Marketplace](https://marketplace.visualstudio.com/items?itemName=JanekWinkler.vscode-owl-ms)

### [Helix Editor](https://helix-editor.com/)

Make sure the language server binary is installed (See Section [Installing the binary](#installing-the-binary)). Then add the language server, language and grammar to your `languages.toml`.

```toml
[language-server.owl-ms-language-server]
command = "owl-ms-language-server"

[[language]]
name = "owl-ms"
injection-regex = "owl-ms"
scope = "text.omn"
file-types = ["omn"]
roots = []
language-servers = ["owl-ms-language-server"] # you can use the absolut path to the binary if needed
comment-token = "//"
indent = { tab-width = 4, unit = "    " }
grammar = "owl-ms" # this is the default


[[grammar]]
name = "owl-ms"
source = { git = "https://github.com/janekx21/tree-sitter-owl2-manchester-syntax", rev = "a55d6bdd3104cd64bfe7178395aa6a139b5632a9" } # replace rev with head of the repository
```

Then fetch and build the grammar.

```bash
helix --grammar fetch
helix --grammar build
```

See the helix documentation page on [adding languages](https://docs.helix-editor.com/guides/adding_languages.html).

### Vim/Neovim with [coc.nvim](https://github.com/neoclide/coc.nvim)

Make sure the language server binary is installed (See Section [Installing the binary](#installing-the-binary)).

Merge this setting into your `coc-settings.json` (open with `:CocConfig`).

```json
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

### Installing the binary

> This is not needed for visual studio code.

To use the language server you have to first install it and then integrate it into you editor. This differs for each editor.
Make sure [cargo](https://doc.rust-lang.org/cargo/) (the rust package manager) is installed on your system.
Switch to nightly toolchain.
Then to install the owl-ms-language-server binary use cargo install.

```bash
rustup default nightly
sudo apt install build-essential # on windows you will need some cc linker
cargo install owl-ms-language-server
```

This installs the language server into Cargo's local set of installed binary crates(rust packages), likely located in `$HOME/.cargo`. Make sure your `PATH` variable contains `$HOME/.cargo/bin`.
Alternatively you can clone this repository and use `cargo build` instead. Using this approach you have to handle the created binary yourself.

To integrate the language server into an editor not listed above use the documentation of your editor. A list of supported tools can be found on
[this website](https://microsoft.github.io/language-server-protocol/implementors/tools/).

- Distributed via [crates.io](https://crates.io/crates/owl-ms-language-server)

## FAQ

### Where can i find logs?
Logs can be found under `<tempdir>/owl-ms-lanugage-server.log`. On Windows `<tempdir>` is most likely `C:\Users\<username>\AppData\Local\Temp`, on Linux `/tmp` and on MacOS `/private/var/tmp`.

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

### Ideas

- code action to create a frame when the IRI can not be found
- LS could resolve IRI's that are external and parse the info from there
- pipe logs to LS client

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
