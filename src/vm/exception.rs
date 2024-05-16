use core::fmt;
use std::{
    backtrace::{Backtrace, BacktraceStatus},
    error::Error as StdError,
    fmt::{Debug, Display},
    sync::Arc,
};

use thiserror::Error;

use crate::{module::ModulePathError, symbol::Symbol};

use super::{function::Function, heap::Reference, ops::OpCode, CallFrame};

/// A trait for exceptions. Exceptions are errors which can be recovered from.
pub trait ExceptionPayload: StdError + sealed::CastToError + 'static {
    /// Whether the exception should be able to be caught from bytecode functions. Native code may
    /// still handle the error in other ways regardless of what this function returns.
    fn is_fatal(&self) -> bool;

    /// Casts the exception to a native `Error` trait object, for easy integration with code
    /// expecting Rust's Error trait.
    fn as_error(&self) -> &(dyn StdError + 'static) {
        self.cast_as_error()
    }

    fn as_error_mut(&mut self) -> &mut (dyn StdError + 'static) {
        self.cast_as_error_mut()
    }

    fn into_error(self: Box<Self>) -> Box<dyn StdError> {
        self.cast_into_error()
    }
}

impl<T: ExceptionPayload> From<T> for Exception {
    fn from(value: T) -> Self {
        Exception::new(value)
    }
}

#[derive(Debug, Error)]
pub struct Exception {
    pub(crate) func: Option<Arc<str>>,
    pub(crate) frame: Option<CallFrame>,
    pub(crate) backtrace: Option<Backtrace>,
    pub(crate) exception: Box<dyn ExceptionPayload>,
}

impl Exception {
    pub fn new(exception: impl ExceptionPayload) -> Self {
        Self {
            func: None,
            frame: None,
            backtrace: None,
            exception: Box::new(exception),
        }
    }

    pub fn with_function(mut self, func: impl Into<Arc<str>>) -> Self {
        self.func = Some(func.into());
        self
    }

    pub fn with_frame(mut self, frame: impl Into<CallFrame>) -> Self {
        self.frame = Some(frame.into());
        self
    }

    pub fn with_backtrace(mut self, backtrace: Backtrace) -> Self {
        self.backtrace = Some(backtrace);
        self
    }

    pub fn is_fatal(&self) -> bool {
        self.exception.is_fatal()
    }

    pub fn as_error(&self) -> &(dyn StdError + 'static) {
        self.exception.as_error()
    }

    pub fn as_error_mut(&mut self) -> &mut (dyn StdError + 'static) {
        self.exception.as_error_mut()
    }

    pub fn into_error(self) -> Box<dyn StdError> {
        self.exception.into_error()
    }

    pub fn downcast_ref<E: ExceptionPayload>(&self) -> Option<&E> {
        self.exception.cast_as_error().downcast_ref()
    }

    pub fn downcast_mut<E: ExceptionPayload>(&mut self) -> Option<&mut E> {
        self.exception.cast_as_error_mut().downcast_mut()
    }

    pub fn downcast<E: ExceptionPayload>(self) -> Result<Box<E>, Self> {
        if self.exception.cast_as_error().is::<E>() {
            Ok(self.exception.cast_into_error().downcast().unwrap())
        } else {
            Err(self)
        }
    }
}

impl fmt::Display for Exception {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let intro = if self.exception.is_fatal() {
            "fatal"
        } else {
            "non-fatal"
        };

        let func = match &self.func {
            Some(f) => f,
            None => "<none>",
        };

        let maybe_ip = self
            .frame
            .as_ref()
            .map(|frame| format!(" (ip: {})", frame.ip))
            .unwrap_or_default();

        let backtrace = self
            .backtrace
            .as_ref()
            .map(|backtrace| match backtrace.status() {
                BacktraceStatus::Unsupported => {
                    String::from("\n\nNative stack traces are not supported on this platform.")
                }
                BacktraceStatus::Disabled => String::from(
                    "\n\nNative stack traces are disabled. set RUST_LIB_BACKTRACE=1 to enable them.",
                ),
                BacktraceStatus::Captured => format!("\n\nNative stack trace:\n{backtrace}"),
                _ => String::from("\n\nNative stack trace support status could not be determined."),
            })
            .unwrap_or_else(|| String::from("\n\nNo native stack trace provided."));

        write!(
            f,
            "{intro} error in function `{func}`{maybe_ip}: {}{backtrace}",
            self.exception.as_error()
        )
    }
}

mod sealed {
    use super::{ExceptionPayload, StdError};

    #[doc(hidden)]
    pub trait CastToError {
        #[doc(hidden)]
        fn cast_as_error(&self) -> &(dyn StdError + 'static);

        #[doc(hidden)]
        fn cast_as_error_mut(&mut self) -> &mut (dyn StdError + 'static);

        #[doc(hidden)]
        fn cast_into_error(self: Box<Self>) -> Box<dyn StdError>;
    }

    impl<T: ExceptionPayload> CastToError for T {
        fn cast_as_error(&self) -> &(dyn StdError + 'static) {
            self
        }

        fn cast_as_error_mut(&mut self) -> &mut (dyn StdError + 'static) {
            self
        }

        fn cast_into_error(self: Box<Self>) -> Box<dyn StdError> {
            self
        }
    }
}
