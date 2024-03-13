use self::memory::VmStack;
use self::ops::OpResult;
use crate::object::VmObject;
use crate::program::{NativeFn, Program};
use crate::symbol::Symbol;
use crate::utils::IntoVmResult;
use crate::value::Value;
use function::Function;
use hashbrown::hash_map::RawEntryMut;
use heap::{Heap, Reference};
use ops::Transition;
use std::cell::{Ref, RefMut};
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

pub mod builtin;
pub mod function;
pub mod gc;
pub mod heap;
pub mod memory;
pub mod ops;

pub(crate) static EXPECTED_PANIC: AtomicBool = AtomicBool::new(false);

// Ugly hack to set a temporary panic hook. We only want the custom panic hook while running code
// on the VM.
fn with_panic_hook<F: FnOnce() -> Result<Transition>>(func: F) -> Result<Transition> {
    use std::panic;
    use std::sync::atomic::Ordering;

    let original_hook = Arc::new(panic::take_hook());
    let cloned = Arc::clone(&original_hook);

    panic::set_hook(Box::new(move |info| {
        // dyn Fn + ... requires that contents are only immutably borrowed, not moved or mutably
        // borrowed, so taking a reference is required here. It makes sure that the Arc is not 
        // dropped at the end of the closure's scope when run, but rather when the closure itself is
        // dropped.
        let original_hook = &cloned;

        if EXPECTED_PANIC.load(Ordering::SeqCst) {
            original_hook(info);
        } else {
            eprintln!(
                "The virtual machine has encountered an internal error and panicked. This is a bug.\n\
                 Please be sure to file an issue at the PFVM issue tracker (<LINK_HERE>).\n\
                 Rust panic is as follows:\n"
            );

            original_hook(info);
        }
    }));

    let ret = func()?;

    // Take the current hook and drop the cloned Arc, which means that the Arc is unique and can
    // be unwrapped to get the original panic hook back.
    let _ = panic::take_hook();
    panic::set_hook(
        Arc::try_unwrap(original_hook).unwrap_or_else(|_| panic!("failed to unwrap unique Arc")),
    );

    Ok(ret)
}

pub type Result<T> = std::result::Result<T, VmError>;

#[derive(Debug)]
pub struct Vm {
    data_stack: VmStack<Value>,
    heap: Heap,
    program: Program,
}

impl Vm {
    pub fn new(program: Program) -> Result<Self> {
        Ok(Self {
            data_stack: VmStack::new(255),
            heap: Heap::new(1024),
            program,
        })
    }

    pub fn run(&mut self) -> Result<()> {
        let main = self
            .program
            .function_indices
            .get("main")
            .cloned()
            .ok_or_else(|| VmError::new(VmErrorKind::FunctionNotFound, None))?;

        with_panic_hook(|| self.run_function(main, 0))?;

        Ok(())
    }

    pub(crate) fn run_function(&mut self, func: Function, stack_base: usize) -> OpResult {
        let mut call_frame = CallFrame::new(func, stack_base, 0);

        while let Some(opcode) = self.program.code.get(call_frame.ip) {
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
            Err(self.error(VmErrorKind::StackUnderflow, frame))
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

    pub fn pop_sint(&mut self, frame: &CallFrame) -> Result<i64> {
        self.pop_value(frame)?
            .sint()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn get_sint(&self, index: usize, frame: &CallFrame) -> Result<i64> {
        self.get_value(index, frame)?
            .sint()
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

    pub fn pop_address(&mut self, frame: &CallFrame) -> Result<usize> {
        self.pop_value(frame)?
            .address()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn get_address(&self, index: usize, frame: &CallFrame) -> Result<usize> {
        self.get_value(index, frame)?
            .address()
            .vm_result(VmErrorKind::Type, frame)
    }

    pub fn pop_function(&mut self) -> Result<Function> {
        todo!("pop_function");
        // self.pop_value(frame)?
        //     .function()
        //     .vm_result(VmErrorKind::Type, frame)
    }

    pub fn get_function(&self, index: usize) -> Result<Function> {
        todo!("get_function");
        // self.get_value(index, frame)?
        //     .function()
        //     .vm_result(VmErrorKind::Type, frame)
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

    pub fn get_local(&self, index: usize, frame: &CallFrame) -> Result<Value> {
        let base = frame.stack_base;
        let count = frame.locals;

        let valid_range = base..base + count;

        if valid_range.contains(&index) {
            Ok(self.data_stack.get(index).copied().unwrap())
        } else {
            Err(self.error(VmErrorKind::OutOfBounds, frame))
        }
    }

    pub fn set_local(&mut self, index: usize, value: Value, frame: &CallFrame) -> Result<()> {
        let base = frame.stack_base;
        let count = frame.locals;

        let valid_range = base..base + count;

        if valid_range.contains(&index) {
            let local = self.data_stack.get_mut(index).unwrap();
            *local = value;
            Ok(())
        } else {
            Err(self.error(VmErrorKind::OutOfBounds, frame))
        }
    }

    pub fn set_reserved(&mut self, n: usize, frame: &mut CallFrame) -> Result<()> {
        if self.data_stack.len() >= frame.stack_base + n {
            frame.locals = n;
            Ok(())
        } else {
            Err(self.error(VmErrorKind::OutOfBounds, frame))
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

    pub fn resolve_native_function(
        &mut self,
        symbol: Symbol,
        frame: &CallFrame,
    ) -> Result<Arc<NativeFn>> {
        match self.program.symbols.get(symbol) {
            Some(name) => {
                let entry = self.program.native_functions.raw_entry_mut().from_key(name);

                match entry {
                    RawEntryMut::Occupied(entry) => Ok(entry.get().clone()),
                    RawEntryMut::Vacant(_) => {
                        Err(VmError::new(VmErrorKind::FunctionNotFound, frame))
                    }
                }
            }
            None => Err(VmError::new(VmErrorKind::SymbolNotFound, frame)),
        }
    }

    pub(crate) fn error(&self, kind: VmErrorKind, frame: &CallFrame) -> VmError {
        VmError::new(kind, frame)
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
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub struct CallFrame {
    pub(crate) start: usize,
    pub(crate) ip: usize,
    pub(crate) stack_base: usize,
    pub(crate) locals: usize,
}

impl CallFrame {
    pub fn new(func: Function, stack_base: usize, locals: usize) -> Self {
        Self {
            start: func.0,
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
    pub fn new() -> Self {
        Self(String::new())
    }

    pub fn with_message(s: String) -> Self {
        Self(s)
    }

    pub fn panic(self) -> ! {
        use std::sync::atomic::Ordering;

        EXPECTED_PANIC.store(true, Ordering::SeqCst);

        if !self.0.is_empty() {
            panic!("{}", self.0);
        } else {
            // default message from Rust.
            panic!();
        }
    }
}

impl From<String> for VmPanic {
    fn from(value: String) -> Self {
        Self::with_message(value)
    }
}

impl From<&str> for VmPanic {
    fn from(value: &str) -> Self {
        Self::with_message(value.into())
    }
}
