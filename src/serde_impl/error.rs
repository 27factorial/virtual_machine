use serde::{de, ser};
use std::fmt;
use std::io;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    Message(String),
    #[error("io error: {0}")]
    Io(io::Error),
    #[error("unexpected end of file")]
    Eof,
    #[error("float must be finite")]
    InvalidFloat,
    #[error("expected bool")]
    ExpectedBool,
    #[error("expected char")]
    ExpectedChar,
    #[error("expected usize")]
    ExpectedUsize,
    #[error("expected String")]
    ExpectedString,
    #[error("expected map to have value")]
    ExpectedMapValue,
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        match error.kind() {
            io::ErrorKind::UnexpectedEof => Self::Eof,
            _ => Self::Io(error),
        }
    }
}

impl From<String> for Error {
    fn from(value: String) -> Self {
        Self::Message(value)
    }
}

impl ser::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Self::Message(msg.to_string())
    }
}

impl de::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Self::Message(msg.to_string())
    }
}
