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
use std::cell::{Ref, RefMut};
use std::fmt::Display;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

pub mod builtin;
pub mod function;
pub mod gc;
pub mod heap;
pub mod memory;
pub mod ops;

/// A convenience macro for the expression `return Err(VmError::new(kind, frame))`
#[macro_export]
macro_rules! throw {
    ($kind:path, $frame:expr) => {
        return ::std::result::Result::Err($crate::vm::VmError::new($kind, $frame))
    };
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

pub type Result<T> = std::result::Result<T, VmError>;

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
        let &main = self
            .module
            .functions
            .get("main::main")
            .vm_result(VmErrorKind::FunctionNotFound, None)?;

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
        self.data_stack
            .push(value)
            .vm_result(VmErrorKind::StackOverflow, frame)
    }

    pub fn pop_value(&mut self, frame: &CallFrame) -> Result<Value> {
        if self.data_stack.len() != frame.stack_base + frame.locals {
            self.data_stack
                .pop()
                .vm_result(VmErrorKind::StackUnderflow, frame)
        } else {
            throw!(VmErrorKind::StackUnderflow, frame);
        }
    }

    pub fn get_value(&self, index: usize, frame: &CallFrame) -> Result<Value> {
        let index = self
            .data_stack
            .len()
            .checked_sub(1)
            .and_then(|last| last.checked_sub(index))
            .vm_result(VmErrorKind::OutOfBounds, frame)?;

        self.data_stack
            .get(index)
            .cloned()
            .vm_result(VmErrorKind::OutOfBounds, frame)
    }

    pub fn pop_int(&mut self, frame: &CallFrame) -> Result<i64> {
        self.pop_value(frame)?
            .int()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn get_int(&self, index: usize, frame: &CallFrame) -> Result<i64> {
        self.get_value(index, frame)?
            .int()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn pop_float(&mut self, frame: &CallFrame) -> Result<f64> {
        self.pop_value(frame)?
            .float()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn get_float(&self, index: usize, frame: &CallFrame) -> Result<f64> {
        self.get_value(index, frame)?
            .float()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn pop_bool(&mut self, frame: &CallFrame) -> Result<bool> {
        self.pop_value(frame)?
            .bool()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn get_bool(&self, index: usize, frame: &CallFrame) -> Result<bool> {
        self.get_value(index, frame)?
            .bool()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn pop_char(&mut self, frame: &CallFrame) -> Result<char> {
        self.pop_value(frame)?
            .char()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn get_char(&self, index: usize, frame: &CallFrame) -> Result<char> {
        self.get_value(index, frame)?
            .char()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn pop_function(&mut self, frame: &CallFrame) -> Result<Function> {
        self.pop_value(frame)?
            .function()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn get_function(&self, index: usize, frame: &CallFrame) -> Result<Function> {
        self.get_value(index, frame)?
            .function()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn pop_symbol(&mut self, frame: &CallFrame) -> Result<Symbol> {
        self.pop_value(frame)?
            .symbol()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn get_symbol(&self, index: usize, frame: &CallFrame) -> Result<Symbol> {
        self.get_value(index, frame)?
            .symbol()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn pop_reference(&mut self, frame: &CallFrame) -> Result<Reference> {
        self.pop_value(frame)?
            .reference()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn get_reference(&self, index: usize, frame: &CallFrame) -> Result<Reference> {
        self.get_value(index, frame)?
            .reference()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn top_value(&self, frame: &CallFrame) -> Result<Value> {
        self.data_stack
            .top()
            .copied()
            .vm_result(VmErrorKind::OutOfBounds, frame)
    }

    pub fn top_value_mut(&mut self, frame: &CallFrame) -> Result<&mut Value> {
        self.data_stack
            .top_mut()
            .vm_result(VmErrorKind::OutOfBounds, frame)
    }

    pub fn heap_object<T: VmObject>(
        &self,
        obj: Reference,
        frame: &CallFrame,
    ) -> Result<Ref<'_, T>> {
        let obj_ref = self
            .heap
            .get(obj)
            .vm_result(VmErrorKind::InvalidObject, frame)?;

        Ref::filter_map(obj_ref, |obj| obj.downcast_ref()).vm_result(VmErrorKind::Type, frame)
    }

    pub fn heap_object_mut<T: VmObject>(
        &self,
        obj: Reference,
        frame: &CallFrame,
    ) -> Result<RefMut<'_, T>> {
        let obj_ref = self
            .heap
            .get_mut(obj)
            .vm_result(VmErrorKind::InvalidObject, frame)?;

        RefMut::filter_map(obj_ref, |obj| obj.downcast_mut()).vm_result(VmErrorKind::Type, frame)
    }

    pub fn set_reserved(&mut self, n: usize, frame: &mut CallFrame) -> Result<()> {
        if self.data_stack.len() >= frame.stack_base + n {
            frame.locals = n;
            Ok(())
        } else {
            throw!(VmErrorKind::OutOfBounds, &*frame);
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

                self.heap
                    .alloc(value)
                    .vm_result(VmErrorKind::OutOfMemory, frame)
            }
        }
    }

    pub fn resolve_function(&mut self, symbol: Symbol, frame: &CallFrame) -> Result<&Function> {
        match self.module.symbols.get(symbol) {
            Some(name) => {
                let path = ModulePath::new(name).vm_result(VmErrorKind::InvalidPath, frame)?;

                match path {
                    ModulePath::FieldOrMethod {
                        ty,
                        field_or_method: method,
                    } => self
                        .module
                        .types
                        .get(ty)
                        .vm_result(VmErrorKind::TypeNotFound, frame)?
                        .methods
                        .get(method)
                        .vm_result(VmErrorKind::FunctionNotFound, frame),
                    ModulePath::Function(func) => self
                        .module
                        .functions
                        .indices
                        .get(func)
                        .vm_result(VmErrorKind::FunctionNotFound, frame),
                }
            }
            None => throw!(VmErrorKind::SymbolNotFound, frame),
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
                    RawEntryMut::Vacant(_) => throw!(VmErrorKind::FunctionNotFound, frame),
                }
            }
            None => throw!(VmErrorKind::SymbolNotFound, frame),
        }
    }

    pub fn reset(&mut self) -> Result<()> {
        self.data_stack = VmStack::new(255);
        self.heap = Heap::new(1024);

        Ok(())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct VmError {
    kind: VmErrorKind,
    frame: Option<CallFrame>,
}

impl VmError {
    pub fn new<'a>(kind: VmErrorKind, frame: impl Into<Option<&'a CallFrame>>) -> Self {
        Self {
            kind,
            frame: frame.into().cloned(),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum VmErrorKind {
    Type,
    Arithmetic,
    StackOverflow,
    StackUnderflow,
    OutOfMemory,
    FunctionNotFound,
    SymbolNotFound,
    TypeNotFound,
    InvalidObject,
    OutOfBounds,
    InvalidSize,
    ModuleNotFound,
    InvalidPath,
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

#[repr(transparent)]
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct VmPanic(String);

impl VmPanic {
    pub fn new(s: impl Display) -> Self {
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
        module::{CoreLib, Io, Module, ToModule},
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
