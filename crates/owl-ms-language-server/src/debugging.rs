use std::{fs::File, io::Write};

use log::info;
use tokio::task;
use tower_lsp::{lsp_types::MessageType, Client};

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
