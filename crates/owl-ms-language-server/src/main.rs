use clap::Parser as ClapParser;
use log::error;
use owl_ms_language_server::{debugging::init_logging, Backend};
use tower_lsp::{LspService, Server};

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

    let (service, socket) = LspService::new(Backend::new);

    // This logs also to the service client
    init_logging(&service);

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await;
}
