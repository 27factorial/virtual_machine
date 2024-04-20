/// A trait for exceptions and errors. Exceptions are recoverable, while Errors are not.
pub trait Throwable {
    
}

pub struct VmError {
    ctx: VmErrorContext,
    
    frame: CallFrame,
}