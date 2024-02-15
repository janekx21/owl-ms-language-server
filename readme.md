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
