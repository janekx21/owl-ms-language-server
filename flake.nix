{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        tree-sitter-watch = pkgs.writeShellScriptBin "tree-sitter-watch" ''${pkgs.watchexec}/bin/watchexec --clear -e js -e scm -e txt "${pkgs.tree-sitter}/bin/tree-sitter generate && ${pkgs.tree-sitter}/bin/tree-sitter test"'';

        owl-ms-language-server = pkgs.writeShellScriptBin "owl-ms-language-server" ''
          /home/janek/Git/owl-ms-language-server/target/debug/owl-ms-language-server --stdio --offline
        '';

        # To profile the language server, I use samply like this
        # samply record /home/janek/Git/owl-ms-language-server/target/release/owl-ms-language-server --stdio --offline
        # Then samply load profile.json.gz on the result
        tarpaulin-report = pkgs.writeShellScriptBin "tarpaulin-report" ''
          ${pkgs.cargo-tarpaulin}/bin/cargo-tarpaulin --engine llvm --out html && \
          xdg-open tarpaulin-report.html
        '';
      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = [
            (rust-bin.stable.latest.default.override {
              extensions = [
                "rust-analyzer"
                "rust-src"
              ];
              targets = [ "x86_64-unknown-linux-musl" ];
            })
            # clang
          ];
          RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
          # Required for the downloaded VS Code Electron binary used in e2e tests
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (
            with pkgs;
            [
              glib
              nss
              nspr
              atk
              cups
              dbus
              libdrm
              mesa
              pango
              cairo
              alsa-lib
              libxkbcommon
              expat
              nspr
            ]
          );
          nativeBuildInputs = with pkgs; [
            # cargo
            # clippy
            # rust-analyzer
            # rustc
            nodejs_22
            tree-sitter

            # Scripts of this repository
            tree-sitter-watch
            langaugeServerScript

            samply
            owl-ms-language-server
            tarpaulin-report
          ];
        };
      }
    );
}
