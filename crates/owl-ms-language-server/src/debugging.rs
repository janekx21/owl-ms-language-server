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

/// Write a log message to the LSP client. The [`msg`] can include log types, they will get converted.
pub fn log_to_client(client: &Client, msg: String) -> task::JoinHandle<()> {
    let c = client.clone();
    task::spawn(async move {
        // Reformat the log message
        let mut msg = msg;
        let mut message_type = MessageType::LOG;
        for (delimiter, type_) in [
            ("ERROR", MessageType::ERROR),
            ("WARN", MessageType::WARNING),
            ("INFO", MessageType::INFO),
        ] {
            if let Some((_, m)) = msg.split_once(delimiter) {
                msg = m.to_string();
                message_type = type_;
            }
        }

        c.log_message(message_type, msg.trim_end()).await;
    })
}

pub struct BackendLogger {
    pub client: Client,
    pub client_log_line: String,
    pub file: Option<File>,
}

impl BackendLogger {
    pub fn new(client: Client, file: Option<File>) -> Self {
        Self {
            client,
            client_log_line: String::new(),
            file,
        }
    }
}

impl Write for BackendLogger {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let str = String::from_utf8_lossy(buf);
        self.client_log_line.push_str(&str);

        if self.client_log_line.ends_with("\n") {
            log_to_client(&self.client, self.client_log_line.clone());
            self.client_log_line.clear();
        }

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
        BackendLogger::new(
            service.inner().client.clone(),
            File::create(log_file_path).ok(),
        ),
        #[cfg(debug_assertions)]
        LevelFilter::Debug,
        #[cfg(not(debug_assertions))]
        LevelFilter::Info,
    );
}
