use core::fmt;
use std::{
    error::Error as StdError,
    fmt::{Debug, Display},
    sync::Arc,
};

use thiserror::Error;

use crate::{module::ModulePathError, symbol::Symbol};

use super::{function::Function, heap::Reference, ops::OpCode, CallFrame};

/// A trait for exceptions. Exceptions are errors which can be recovered from.
pub trait Exception: StdError {
    fn as_error(&self) -> &dyn StdError;
    fn clone_dyn(&self) -> Box<dyn Exception>;
}

impl<'a, T: Exception + 'a> From<T> for Box<dyn Exception + 'a> {
    fn from(value: T) -> Self {
        Box::new(value)
    }
}

#[derive(Clone, Debug, Error)]
pub struct VmError {
    func: Arc<str>,
    frame: Option<CallFrame>,
    info: VmErrorData,
}

impl VmError {
    pub fn new(
        func: impl AsRef<str>,
        frame: impl Into<Option<CallFrame>>,
        info: VmErrorData,
    ) -> Self {
        Self {
            func: func.as_ref().into(),
            frame: frame.into(),
            info,
        }
    }
}

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let func_string = if !self.func.is_empty() {
            &self.func
        } else {
            "<none>"
        };

        let ip_string = self
            .frame
            .as_ref()
            .map(|frame| format!(" (ip: {})", frame.ip))
            .unwrap_or_default();

        write!(
            f,
            "vm error in function `{func_string}`{ip_string}: {}",
            self.info
        )
    }
}

#[derive(Debug)]
pub enum VmErrorData {
    Type {
        expected: Arc<str>,
        actual: Arc<str>,
    },
    Arithmetic,
    StackOverflow(usize),
    StackUnderflow(usize),
    OutOfMemory {
        requested: usize,
        available: usize,
    },
    FunctionNotFound(Arc<str>),
    SymbolNotFound(Symbol),
    TypeNotFound(Arc<str>),
    InvalidReference(Reference),
    InvalidSize(usize),
    ModuleNotFound(Arc<str>),
    InvalidPath(Arc<str>),
    Exception(Box<dyn Exception>),
}

impl Clone for VmErrorData {
    fn clone(&self) -> Self {
        match self {
            Self::Type { expected, actual } => Self::Type {
                expected: Arc::clone(expected),
                actual: Arc::clone(actual),
            },
            Self::Arithmetic => Self::Arithmetic,
            Self::StackOverflow(max) => Self::StackOverflow(*max),
            Self::StackUnderflow(min) => Self::StackUnderflow(*min),
            Self::OutOfMemory {
                requested,
                available,
            } => Self::OutOfMemory {
                requested: *requested,
                available: *available,
            },
            Self::FunctionNotFound(name) => Self::FunctionNotFound(Arc::clone(name)),
            Self::SymbolNotFound(sym) => Self::SymbolNotFound(*sym),
            Self::TypeNotFound(name) => Self::TypeNotFound(Arc::clone(name)),
            Self::InvalidReference(reference) => Self::InvalidReference(*reference),
            Self::InvalidSize(size) => Self::InvalidSize(*size),
            Self::ModuleNotFound(name) => Self::ModuleNotFound(Arc::clone(name)),
            Self::InvalidPath(path) => Self::InvalidPath(Arc::clone(path)),
            Self::Exception(ex) => Self::Exception(ex.clone_dyn()),
        }
    }
}

impl fmt::Display for VmErrorData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type { expected, actual } => write!(f, "expected type `{expected}`, got `{actual}`"),
            Self::Arithmetic => write!(f, "arithmetic underflow or overflow"),
            Self::StackOverflow(max) => write!(f, "overflowed beyond {max} stack values"),
            Self::StackUnderflow(min) => write!(f, "underflowed below minimum stack element {min}"),
            Self::OutOfMemory { requested, available } => write!(f, "tried to allocate {requested} bytes but only {available} bytes are available"),
            Self::FunctionNotFound(name) => write!(f, "function `{name}` not found"),
            Self::SymbolNotFound(Symbol(symbol)) => write!(f, "symbol {symbol:x} not found"),
            Self::TypeNotFound(name) => write!(f, "type {name} not found"),
            Self::InvalidReference(Reference(reference)) => write!(f, "nonexistent reference {reference:x}"),
            Self::InvalidSize(size) => write!(f, "tried to allocate a structure of {size} bytes, when max allowed size is {} bytes", isize::MAX),
            Self::ModuleNotFound(module) => write!(f, "module `{module}` not found"),
            Self::InvalidPath(path) => write!(f, "`{path}` is not a valid path"),
            Self::Exception(ex) => write!(f, "unhandled exception: {}", ex.as_error())
        }
    }
}
