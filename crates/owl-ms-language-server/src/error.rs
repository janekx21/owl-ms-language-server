use log::error;
use parking_lot::RwLock;
use std::{borrow::Cow, io, path::PathBuf, time::Duration};
use thiserror::Error;
use tower_lsp::lsp_types::Url;

use crate::pos::Position;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("The provided document can not be found {0}")]
    DocumentNotFound(Url),
    #[error("The provided document type is not supported extention: {0}")]
    DocumentNotSupported(String),
    #[error("The provided file path is not a URL {0}")]
    InvalidFilePath(PathBuf),
    #[error("The workspace of the document was not found {0}")]
    WorkspaceNotFound(Url),
    #[error("The lock could not be aquired for {0} sec")]
    LockTimeout(u64),
    #[error("The provided URL is not valid for this LSP {0}")]
    InvalidUrl(Url),
    #[error("The position is not inside the bounds {0}")]
    PositionOutOfBounds(Position),
    #[error("The tower lsp position is not inside the bounds {0:?}")]
    PositionOutOfBoundsTowerLsp(tower_lsp::lsp_types::Position),
    #[error("The LSP Feature is not supported at the moment: {0}")]
    LspFeatureNotSupported(&'static str),
    // From other error types
    #[error("Ureq Error: {0}")]
    Ureq(#[from] ureq::Error),
    #[error("Horned Owl Error: {0}")]
    HornedOwl(#[from] horned_owl::error::HornedError),
    #[error("IO Error: {0}")]
    Io(#[from] io::Error),
    #[error("Tokio Join Error: {0}")]
    TokioJoin(#[from] tokio::task::JoinError),
    #[error("Rope Error: {0}")]
    Rope(#[from] ropey::Error),
}

impl From<Error> for tower_lsp::jsonrpc::Error {
    fn from(value: Error) -> Self {
        match value {
            Error::DocumentNotFound(url) => {
                Self::invalid_params(format!("The provided document can not be found {url}"))
            }
            _ => Self {
                code: 0.into(),
                message: Cow::from(format!("{value}")),
                data: None,
            },
        }
    }
}

pub trait ResultExt<T> {
    /// Consumes and logs the result if it contains error
    fn log_if_error(self);
    /// Inspects the Result and logs an Error if needed
    fn inspect_log(self) -> Self;
}

impl<T> ResultExt<T> for Result<T> {
    fn log_if_error(self) {
        let _ = self.inspect_log();
    }

    fn inspect_log(self) -> Self {
        self.inspect_err(|e| error!("{e}"))
    }
}

pub trait ResultIterator<T> {
    /// Filters out Errors and logs them
    fn filter_and_log(self) -> impl Iterator<Item = T>;
}

impl<T, I: Iterator<Item = Result<T>>> ResultIterator<T> for I {
    fn filter_and_log(self) -> impl Iterator<Item = T> {
        self.filter_map(|r| r.inspect_log().ok())
    }
}

pub trait RwLockExt<T> {
    fn read_timeout(
        &self,
    ) -> Result<parking_lot::lock_api::RwLockReadGuard<parking_lot::RawRwLock, T>>;
}

impl<T> RwLockExt<T> for RwLock<T> {
    fn read_timeout(
        &self,
    ) -> Result<parking_lot::lock_api::RwLockReadGuard<parking_lot::RawRwLock, T>> {
        self.try_read_recursive_for(Duration::from_secs(5))
            .ok_or(Error::LockTimeout(5))
    }
}
