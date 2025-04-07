# Owl 2 Manchester Syntax Language Server

An incremental analysis assistant for writing ontologies with the [OWL 2 Web Ontology Language | Manchester Syntax](https://www.w3.org/TR/owl2-manchester-syntax/).

## Structure of this repository

- [The Language Server](lib/owl-ms-language-server/)
- [The Grammar written in tree-sitter](lib/tree-sitter-owl-ms/)
- [Visual Studio Code Extension](editors/code/)

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

- [x] Basic Grammar
- [x] Basic Language Server
- [x] VS Code plugin
- [ ] making the LS more practical

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## License

All projects use the [Apache-2.0](https://choosealicense.com/licenses/apache-2.0/) License
