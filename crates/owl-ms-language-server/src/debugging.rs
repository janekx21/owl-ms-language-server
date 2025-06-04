use log::{info, LevelFilter};
use std::io::Write;
use std::{env, fs::File};
use tokio::task;
use tower_lsp::{lsp_types::MessageType, Client, LspService};

use crate::Backend;

pub fn timeit<F: FnMut() -> T, T>(name: &str, mut f: F) -> T {
    use std::time::Instant;
    let start = Instant::now();
    let result = f();
    let end = Instant::now();
    let duration = end.duration_since(start);
    info!("⏲ {} took {:?}", name, duration);
    result
}

pub fn log_to_client(client: &Client, msg: String) -> task::JoinHandle<()> {
    let c = client.clone();
    task::spawn(async move {
        c.log_message(MessageType::INFO, msg).await;
    })
}

pub struct BackendLogger {
    pub client: Client,
    pub file: Option<File>,
}

impl Write for BackendLogger {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        log_to_client(&self.client, String::from_utf8_lossy(buf).into());
        if let Some(f) = &mut self.file {
            f.write_all(buf).unwrap_or_else(|err| {
                log_to_client(&self.client, format!("Could not log to file. {}", err));
            });
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub fn init_logging(service: &LspService<Backend>) {
    let mut log_file_path = env::temp_dir();
    log_file_path.push("owl-ms-lanugage-server.log");
    let log_file_path = log_file_path.as_path();

    simple_logging::log_to(
        BackendLogger {
            client: service.inner().client.clone(),
            file: File::create(log_file_path).ok(),
        },
        LevelFilter::Debug,
    );
}
