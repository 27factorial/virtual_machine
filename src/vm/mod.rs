use self::exception::{Exception, ExceptionPayload};
use self::memory::VmStack;
use self::ops::OpResult;
use crate::module::{Module, ModulePath, ModulePathError, NativeFn, ToModule};
use crate::object::VmObject;
use crate::symbol::Symbol;
use crate::utils::IntoVmResult;
use crate::value::Value;
use function::Function;
use hashbrown::hash_map::RawEntryMut;
use heap::{Heap, Reference};
use ops::Transition;
use std::backtrace::Backtrace;
use std::cell::{Ref, RefMut};
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::{fmt, mem};
use thiserror::Error;

pub mod builtin;
pub mod exception;
pub mod function;
pub mod gc;
pub mod heap;
pub mod memory;
pub mod ops;

/// A convenience macro for the expression `return Err(VmError::new(kind, frame))`
#[macro_export]
macro_rules! throw {
    ($ex:expr) => {{
        return Err($ex);
    }};
}

/// Catch an exception or propagate another error.
macro_rules! catch {
    ($e:expr) => {};
}

pub(crate) static EXPECTED_PANIC: AtomicBool = AtomicBool::new(false);

pub const KILOBYTE: usize = 1024;
pub const MEGABYTE: usize = 1024 * KILOBYTE;
pub const GIGABYTE: usize = 1024 * MEGABYTE;
pub const TERABYTE: usize = 1024 * GIGABYTE;

// Ugly hack to set a temporary panic hook. We only want the custom panic hook while running code
// on the VM. This may produce unexpected results when used with multithreaded code, but should be
// fine in the context of this crate.
fn with_panic_hook<F, T>(f: F) -> T
where
    F: FnOnce() -> T,
{
    use std::panic;
    use std::sync::atomic::Ordering;

    let original_hook = Arc::new(panic::take_hook());
    let cloned = Arc::clone(&original_hook);

    panic::set_hook(Box::new(move |info| {
        // Fn requires that contents are only immutably borrowed, not moved or mutably borrowed, so
        // taking a reference is required here. It makes sure that the Arc is not dropped at the end
        // of the closure's scope when run, but rather when the closure itself is dropped.
        let original_hook = &cloned;

        if EXPECTED_PANIC.load(Ordering::SeqCst) {
            original_hook(info);

            // The load and store are done separately in case the original panic was this panic
            // handler (for whatever reason). That way the EXPECTED_PANIC flag is propagated down
            // the panicking call stack.
            EXPECTED_PANIC.store(false, Ordering::SeqCst);
        } else {
            eprintln!(
                "The virtual machine has encountered an internal error and panicked. This is a bug.\n\
                 Please be sure to file an issue at the PFVM issue tracker (<LINK_HERE>).\n\
                 Rust panic is as follows:\n"
            );

            original_hook(info);
        }
    }));

    let ret = f();

    // Take the current hook and drop the cloned Arc, which means that the Arc is unique and can
    // be unwrapped to get the original panic hook back.
    let _ = panic::take_hook();
    panic::set_hook(Arc::try_unwrap(original_hook).ok().unwrap());

    ret
}

pub type Result<T> = std::result::Result<T, Exception>;

#[derive(Debug)]
pub struct Vm {
    data_stack: VmStack<Value>,
    heap: Heap,
    module: Module,
}

impl Vm {
    pub fn new(module: Module) -> Self {
        Self {
            data_stack: VmStack::new(8 * MEGABYTE),
            heap: Heap::new(2 * GIGABYTE),
            module,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        const MAIN: &str = "main::main";

        let &main = self.module.functions.get(MAIN).with_exception(|| {
            VmExceptionPayload::FunctionNotFound(MAIN.into()).into_exception()
        })?;

        with_panic_hook(|| self.run_function(main, 0))?;

        Ok(())
    }

    pub(crate) fn run_function(&mut self, func: Function, stack_base: usize) -> OpResult {
        let mut call_frame = CallFrame::new(func, stack_base, 0);

        while let Some(opcode) = self.module.functions.get_opcode(call_frame.ip) {
            match opcode.execute(self, &mut call_frame)? {
                Transition::Continue => call_frame.ip += 1,
                Transition::Jump => {}
                Transition::Return => break,
                Transition::Halt => return Ok(Transition::Halt),
            }
        }

        Ok(Transition::Continue)
    }

    pub fn push_value(&mut self, value: Value, frame: &CallFrame) -> Result<()> {
        self.data_stack.push(value).with_exception(|| {
            VmExceptionPayload::StackOverflow(self.data_stack.len())
                .into_exception()
                .with_frame(frame.clone())
        })
    }

    pub fn pop_value(&mut self, frame: &CallFrame) -> Result<Value> {
        if self.data_stack.len() != frame.stack_base + frame.locals {
            self.data_stack.pop().with_exception(|| {
                VmExceptionPayload::StackUnderflow(frame.stack_base + frame.locals)
                    .into_exception()
                    .with_frame(frame.clone())
            })
        } else {
            throw!(
                VmExceptionPayload::StackUnderflow(frame.stack_base + frame.locals)
                    .into_exception()
                    .with_frame(frame.clone())
            )
        }
    }

    pub fn get_value(&self, index: usize, frame: &CallFrame) -> Result<Value> {
        let index = self
            .data_stack
            .len()
            .checked_sub(1)
            .and_then(|last| last.checked_sub(index))
            .with_exception(|| {
                VmExceptionPayload::InvalidStackIndex {
                    index,
                    min: frame.stack_base,
                    max: frame.stack_base + frame.locals,
                }
                .into_exception()
                .with_frame(frame.clone())
            })?;

        self.data_stack.get(index).cloned().with_exception(|| {
            VmExceptionPayload::InvalidStackIndex {
                index,
                min: frame.stack_base,
                max: frame.stack_base + frame.locals,
            }
            .into_exception()
            .with_frame(frame.clone())
        })
    }

    pub fn pop_int(&mut self, frame: &CallFrame) -> Result<i64> {
        match self.pop_value(frame)? {
            Value::Int(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::INT_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn get_int(&self, index: usize, frame: &CallFrame) -> Result<i64> {
        match self.get_value(index, frame)? {
            Value::Int(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::INT_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn pop_float(&mut self, frame: &CallFrame) -> Result<f64> {
        match self.pop_value(frame)? {
            Value::Float(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::FLOAT_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn get_float(&self, index: usize, frame: &CallFrame) -> Result<f64> {
        match self.get_value(index, frame)? {
            Value::Float(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::FLOAT_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn pop_bool(&mut self, frame: &CallFrame) -> Result<bool> {
        match self.pop_value(frame)? {
            Value::Bool(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::BOOL_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn get_bool(&self, index: usize, frame: &CallFrame) -> Result<bool> {
        match self.get_value(index, frame)? {
            Value::Bool(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::BOOL_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn pop_char(&mut self, frame: &CallFrame) -> Result<char> {
        match self.pop_value(frame)? {
            Value::Char(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::CHAR_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn get_char(&self, index: usize, frame: &CallFrame) -> Result<char> {
        match self.get_value(index, frame)? {
            Value::Char(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::CHAR_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn pop_function(&mut self, frame: &CallFrame) -> Result<Function> {
        match self.pop_value(frame)? {
            Value::Function(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::FUNCTION_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn get_function(&self, index: usize, frame: &CallFrame) -> Result<Function> {
        match self.get_value(index, frame)? {
            Value::Function(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::FUNCTION_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn pop_symbol(&mut self, frame: &CallFrame) -> Result<Symbol> {
        match self.pop_value(frame)? {
            Value::Symbol(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::SYMBOL_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn get_symbol(&self, index: usize, frame: &CallFrame) -> Result<Symbol> {
        match self.get_value(index, frame)? {
            Value::Symbol(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::SYMBOL_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn pop_reference(&mut self, frame: &CallFrame) -> Result<Reference> {
        match self.pop_value(frame)? {
            Value::Reference(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::REFERENCE_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn get_reference(&self, index: usize, frame: &CallFrame) -> Result<Reference> {
        match self.get_value(index, frame)? {
            Value::Reference(v) => Ok(v),
            value => throw!(VmExceptionPayload::Type {
                expected: Value::REFERENCE_TYPE_NAME.into(),
                actual: value.type_name().into()
            }
            .into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn top_value(&self, frame: &CallFrame) -> Result<Value> {
        self.data_stack.top().copied().with_exception(|| {
            VmExceptionPayload::StackEmpty
                .into_exception()
                .with_frame(frame.clone())
        })
    }

    pub fn top_value_mut(&mut self, frame: &CallFrame) -> Result<&mut Value> {
        self.data_stack.top_mut().with_exception(|| {
            VmExceptionPayload::StackEmpty
                .into_exception()
                .with_frame(frame.clone())
        })
    }

    pub fn heap_object<T: VmObject>(
        &self,
        obj: Reference,
        frame: &CallFrame,
    ) -> Result<Ref<'_, T>> {
        let obj_ref = self.heap.get(obj).with_exception(|| {
            VmExceptionPayload::InvalidReference(obj)
                .into_exception()
                .with_frame(frame.clone())
        })?;
        // .exception(VmErrorKind::InvalidReference, frame)?;

        Ref::filter_map(obj_ref, |obj| obj.downcast_ref()).with_exception(|| {
            VmExceptionPayload::Type {
                expected: "TODO: type name here".into(),
                actual: "TODO: type name here".into(),
            }
            .into_exception()
            .with_frame(frame.clone())
        })
    }

    pub fn heap_object_mut<T: VmObject>(
        &self,
        obj: Reference,
        frame: &CallFrame,
    ) -> Result<RefMut<'_, T>> {
        let obj_ref = self.heap.get_mut(obj).with_exception(|| {
            VmExceptionPayload::InvalidReference(obj)
                .into_exception()
                .with_frame(frame.clone())
        })?;

        RefMut::filter_map(obj_ref, |obj| obj.downcast_mut()).with_exception(|| {
            VmExceptionPayload::Type {
                expected: "TODO: type name here".into(),
                actual: "TODO: type name here".into(),
            }
            .into_exception()
            .with_frame(frame.clone())
        })
    }

    pub fn set_reserved(&mut self, n: usize, frame: &mut CallFrame) -> Result<()> {
        if self.data_stack.len() >= frame.stack_base + n {
            frame.locals = n;
            Ok(())
        } else {
            throw!(VmExceptionPayload::StackOverflow(self.data_stack.len())
                .into_exception()
                .with_frame(frame.clone()))
        }
    }

    #[inline(always)]
    pub fn ip(&self, frame: &CallFrame) -> usize {
        frame.ip
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    pub fn alloc<T: VmObject>(&mut self, value: T, frame: &CallFrame) -> Result<Reference> {
        match self.heap.alloc(value) {
            Ok(object) => Ok(object),
            Err(value) => {
                // Current roots:
                // self.data_stack
                // I would like to use a convenience method here, but partial borrows strike again.
                self.heap
                    .mark_and_sweep(self.data_stack.iter().copied().filter_map(Value::reference));

                self.heap.alloc(value).with_exception(|| {
                    VmExceptionPayload::OutOfMemory {
                        requested: mem::size_of::<T>(),
                        available: self.heap.capacity() - self.heap.size(),
                    }
                    .into_exception()
                    .with_frame(frame.clone())
                })
            }
        }
    }

    pub fn resolve_function(
        &mut self,
        symbol: Symbol,
        frame: &CallFrame,
    ) -> Result<(&str, &Function)> {
        match self.module.symbols.get(symbol) {
            Some(name) => {
                let path = ModulePath::new(name).with_exception(|| {
                    VmExceptionPayload::InvalidPath(name.into())
                        .into_exception()
                        .with_frame(frame.clone())
                })?;

                match path {
                    ModulePath::FieldOrMethod {
                        ty,
                        field_or_method: method,
                    } => self
                        .module
                        .types
                        .get(ty)
                        .with_exception(|| {
                            VmExceptionPayload::TypeNotFound(ty.into())
                                .into_exception()
                                .with_frame(frame.clone())
                        })?
                        .methods
                        .get(method)
                        .with_exception(|| {
                            VmExceptionPayload::FunctionNotFound(name.into())
                                .into_exception()
                                .with_frame(frame.clone())
                        })
                        .map(|func| (name, func)),
                    ModulePath::Function(func) => self
                        .module
                        .functions
                        .indices
                        .get(func)
                        .with_exception(|| {
                            VmExceptionPayload::FunctionNotFound(func.into())
                                .into_exception()
                                .with_frame(frame.clone())
                        })
                        .map(|func| (name, func)),
                }
            }
            None => throw!(VmExceptionPayload::SymbolNotFound(symbol)
                .into_exception()
                .with_frame(frame.clone())),
        }
    }

    pub fn resolve_native_function(
        &mut self,
        symbol: Symbol,
        frame: &CallFrame,
    ) -> Result<Arc<NativeFn>> {
        match self.module.symbols.get(symbol) {
            Some(name) => {
                let entry = self.module.native_functions.raw_entry_mut().from_key(name);

                match entry {
                    RawEntryMut::Occupied(entry) => Ok(entry.get().clone()),
                    RawEntryMut::Vacant(_) => {
                        throw!(VmExceptionPayload::FunctionNotFound(name.into())
                            .into_exception()
                            .with_frame(frame.clone()))
                    }
                }
            }
            None => throw!(VmExceptionPayload::SymbolNotFound(symbol).into_exception()
            .with_frame(frame.clone())),
        }
    }

    pub fn reset(&mut self) -> Result<()> {
        self.data_stack = VmStack::new(255);
        self.heap = Heap::new(1024);

        Ok(())
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub struct CallFrame {
    pub(crate) ip: usize,
    pub(crate) stack_base: usize,
    pub(crate) locals: usize,
}

impl CallFrame {
    pub fn new(func: Function, stack_base: usize, locals: usize) -> Self {
        Self {
            ip: func.0,
            stack_base,
            locals,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Error)]
pub enum VmExceptionPayload {
    #[error("expected type `{expected}`, got `{actual}`")]
    Type {
        expected: Arc<str>,
        actual: Arc<str>,
    },
    #[error("arithmetic underflow or overflow")]
    Arithmetic,
    #[error("overflowed beyond {0} stack values")]
    StackOverflow(usize),
    #[error("underflowed below minimum stack element {0}")]
    StackUnderflow(usize),
    #[error("tried to allocate {requested} bytes but only {available} bytes are available")]
    OutOfMemory { requested: usize, available: usize },
    #[error("function `{0}` not found")]
    FunctionNotFound(Arc<str>),
    #[error("symbol {:x} not found", (.0).0)]
    SymbolNotFound(Symbol),
    #[error("type {0} not found")]
    TypeNotFound(Arc<str>),
    #[error("nonexistent reference {:x}", (.0).0)]
    InvalidReference(Reference),
    #[error(
        "tried to allocate a structure of {0} bytes (valid range: [0, {}))",
        isize::MAX
    )]
    InvalidSize(i128),
    #[error("attempted to access stack index {index} (valid range: [{min}, {max}))")]
    InvalidStackIndex {
        index: usize,
        min: usize,
        max: usize,
    },
    #[error("attempted to get top of stack when the stack was empty")]
    StackEmpty,
    #[error("module `{0}` not found")]
    ModuleNotFound(Arc<str>),
    #[error("`{0}` is not a valid path")]
    InvalidPath(Arc<str>),
}

impl ExceptionPayload for VmExceptionPayload {
    fn is_fatal(&self) -> bool {
        match self {
            Self::Type {
                expected: _,
                actual: _,
            } => false,
            Self::Arithmetic => false,
            Self::StackOverflow(_) => true,
            Self::StackUnderflow(_) => true,
            Self::OutOfMemory {
                requested: _,
                available: _,
            } => true,
            Self::FunctionNotFound(_) => false,
            Self::SymbolNotFound(_) => true,
            Self::TypeNotFound(_) => true,
            Self::InvalidReference(_) => true,
            Self::InvalidSize(_) => false,
            Self::InvalidStackIndex {
                index: _,
                min: _,
                max: _,
            } => true,
            Self::StackEmpty => true,
            Self::ModuleNotFound(_) => false,
            Self::InvalidPath(_) => false,
        }
    }

    fn into_exception(self) -> Exception
    where
        Self: Sized,
    {
        Exception::new(self).with_backtrace(Backtrace::capture())
    }
}

#[repr(transparent)]
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct VmPanic(String);

impl VmPanic {
    pub fn new(s: impl fmt::Display) -> Self {
        Self(s.to_string())
    }

    pub fn panic(self) -> ! {
        use std::sync::atomic::Ordering;

        EXPECTED_PANIC.store(true, Ordering::SeqCst);

        if !self.0.is_empty() {
            panic!("{}", self.0);
        } else {
            panic!("internal error");
        }
    }
}

impl From<String> for VmPanic {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl From<&str> for VmPanic {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        module::{core::CoreLib, Module, ToModule},
        value::Value,
        vm::{ops::OpCode, Vm},
    };

    #[test]
    fn looping_fn_call() {
        let mut module = Module::new();

        let main_sym = module.define_symbol("main::main");
        let adder_sym = module.define_symbol("main::adder");

        let adder = module
            .define_function(adder_sym, [OpCode::Add, OpCode::Ret])
            .expect("failed to define `adder` function");

        module
            .define_function(
                main_sym,
                [
                    // Initialize a counter to 1000 and store the counter in local variable 0
                    OpCode::Push(Value::Int(1000)),
                    OpCode::ReserveImm(1),
                    // Load the counter from local variable 0, and if it's zero, jump to the end of
                    // the program.
                    OpCode::Load(0),
                    OpCode::EqImm(Value::Int(0)),
                    OpCode::JumpCondImm(9),
                    // else...
                    // load the value from local variable 0, subtract 1, and store the new counter
                    // back in the local variable 0.
                    OpCode::Load(0),
                    OpCode::SubImm(Value::Int(1)),
                    OpCode::Store(0),
                    // Push two 2s onto the stack
                    OpCode::Push(Value::Int(2)),
                    OpCode::Dup,
                    // Call a function which pops them from the stack, adds them, then returns
                    // OpCode::Add,
                    OpCode::CallImm(adder),
                    // Remove the added value (it's not actually used)
                    OpCode::Pop,
                    // Jump back to the counter check above
                    OpCode::JumpImm(-11),
                    // halt the virtual machine
                    OpCode::Halt,
                ],
            )
            .expect("failed to define `main` function");

        let mut vm = Vm::new(module);

        vm.run().expect("failed to run vm");
    }

    #[test]
    fn function_and_method_resolution() {
        let mut module = CoreLib.to_module();

        let main_sym = module.define_symbol("main::main");
        let array_with_capacity_sym =
            module.define_symbol("core::collections::Array.with_capacity");
        let array_push_sym = module.define_symbol("core::collections::Array.push");

        module
            .define_function(
                main_sym,
                [
                    OpCode::Push(5.into()),
                    OpCode::ResolveImm(array_with_capacity_sym),
                    OpCode::Call,
                    OpCode::ResolveImm(array_push_sym),
                    OpCode::ReserveImm(2),
                    OpCode::Push(0.into()),
                    OpCode::Load(0),
                    OpCode::Load(1),
                    OpCode::Call,
                    OpCode::Push(1.into()),
                    OpCode::Load(0),
                    OpCode::Load(1),
                    OpCode::Call,
                    OpCode::Push(2.into()),
                    OpCode::Load(0),
                    OpCode::Load(1),
                    OpCode::Call,
                    OpCode::Push(3.into()),
                    OpCode::Load(0),
                    OpCode::Load(1),
                    OpCode::Call,
                    OpCode::Push(4.into()),
                    OpCode::Load(0),
                    OpCode::Load(1),
                    OpCode::Call,
                    OpCode::Load(0),
                    OpCode::Dbg(0),
                ],
            )
            .expect("failed to define `main::main` function");

        let mut vm = Vm::new(module);

        vm.run().expect("failed to run vm");
    }
}
