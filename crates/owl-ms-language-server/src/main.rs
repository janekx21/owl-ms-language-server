use clap::Parser as ClapParser;
use log::error;
use owl_ms_language_server::{Backend, LANGUAGE};
use tower_lsp::{LspService, Server};
use tree_sitter::Parser;

/// A language server for the owl 2 manchester syntax language
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Use stdin and stdout for communication
    #[arg(long, default_value_t = true)]
    stdio: bool,
}

#[tokio::main]
async fn main() {
    let _ = Args::parse();

    std::panic::set_hook(Box::new(|info| {
        error!("paniced with {}", info);
    }));

    let mut parser = Parser::new();
    parser.set_language(*LANGUAGE).unwrap();

    let (service, socket) = LspService::new(|client| Backend::new(client, parser));

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await;
}
