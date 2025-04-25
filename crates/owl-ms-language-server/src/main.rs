use std::{env, fs::File};

use clap::Parser as ClapParser;
use log::{error, LevelFilter};
use owl_ms_language_server::{debugging::BackendLogger, Backend, LANGUAGE};
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

    let mut log_file_path = env::temp_dir();
    log_file_path.push("owl-ms-lanugage-server.log");
    let log_file_path = log_file_path.as_path();

    let mut parser = Parser::new();
    parser.set_language(*LANGUAGE).unwrap();

    let (service, socket) = LspService::new(|client| Backend::new(client, parser));

    simple_logging::log_to(
        BackendLogger {
            client: service.inner().client.clone(),
            file: File::create(log_file_path).ok(),
        },
        LevelFilter::Debug,
    );

    std::panic::set_hook(Box::new(|info| {
        error!("paniced with {}", info);
    }));

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await;
}
