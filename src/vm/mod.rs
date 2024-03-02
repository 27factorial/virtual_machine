use self::cache::Cache;
use self::function::NewFunction;
use self::memory::VmStack;
use crate::object::VmObject;
use crate::program::{NativeFn, Path, Program};
use crate::symbol::Symbol;
use crate::utils::{IntEntry, IntoVmResult};
use crate::value::Value;
use function::Function;
use heap::{Heap, Reference};
use ops::{OpCode, Transition};
use std::cell::RefCell;
use std::sync::Arc;

pub mod cache;
pub mod function;
pub mod gc;
pub mod heap;
pub mod memory;
pub mod ops;

pub type Result<T> = std::result::Result<T, VmError>;

#[derive(Debug)]
pub struct Vm {
    pub(crate) frame: CallFrame,
    call_stack: VmStack<CallFrame>,
    data_stack: VmStack<Value>,
    heap: Heap,
    cache: RefCell<Cache>,
    program: Program,
}

impl Vm {
    pub fn new(program: Program) -> Result<Self> {
        let main = program
            .functions_2
            .get("main")
            .cloned()
            .ok_or_else(|| VmError::new(VmErrorKind::FunctionNotFound, None))?;

        Ok(Self {
            frame: CallFrame::new(main, 0, 0),
            call_stack: VmStack::new(64),
            data_stack: VmStack::new(255),
            heap: Heap::new(1024),
            cache: RefCell::new(Cache::new()),
            program,
        })
    }

    pub fn run(&mut self) -> Result<()> {
        while let Some(opcode) = self.current_op() {
            match opcode.execute(self)? {
                Transition::Continue => self.frame.ip += 1,
                Transition::Jump => {}
                Transition::Halt => return Ok(()),
            }
        }

        Ok(())
    }

    pub fn push_frame(&mut self, frame: CallFrame) -> Result<()> {
        self.call_stack
            .push(frame)
            .vm_result(VmErrorKind::StackOverflow, &self.frame)
    }

    pub(crate) fn push_current_frame(&mut self) -> Result<()> {
        self.call_stack
            .push(self.frame)
            .vm_result(VmErrorKind::StackOverflow, &self.frame)
    }

    pub fn pop_frame(&mut self) -> Result<CallFrame> {
        self.call_stack
            .pop()
            .vm_result(VmErrorKind::StackUnderflow, &self.frame)
    }

    pub fn push_value(&mut self, value: Value) -> Result<()> {
        self.data_stack
            .push(value)
            .vm_result(VmErrorKind::StackOverflow, &self.frame)
    }


    pub fn pop_value(&mut self) -> Result<Value> {
        if self.data_stack.len() != self.frame.stack_base + self.frame.locals {
            self.data_stack
                .pop()
                .vm_result(VmErrorKind::StackUnderflow, &self.frame)
        } else {
            Err(self.error(VmErrorKind::StackUnderflow))
        }
    }

    pub fn get_value(&self, index: usize) -> Result<Value> {
        let index = self
            .data_stack
            .len()
            .checked_sub(1)
            .and_then(|last| last.checked_sub(index))
            .vm_result(VmErrorKind::OutOfBounds, &self.frame)?;

        self.data_stack
            .get(index)
            .cloned()
            .vm_result(VmErrorKind::OutOfBounds, &self.frame)
    }

    pub fn pop_uint(&mut self) -> Result<u64> {
        self.pop_value()?
            .uint()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn get_uint(&self, index: usize) -> Result<u64> {
        self.get_value(index)?
            .uint()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn pop_sint(&mut self) -> Result<i64> {
        self.pop_value()?
            .sint()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn get_sint(&self, index: usize) -> Result<i64> {
        self.get_value(index)?
            .sint()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn pop_float(&mut self) -> Result<f64> {
        self.pop_value()?
            .float()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn get_float(&self, index: usize) -> Result<f64> {
        self.get_value(index)?
            .float()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn pop_bool(&mut self) -> Result<bool> {
        self.pop_value()?
            .bool()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn get_bool(&self, index: usize) -> Result<bool> {
        self.get_value(index)?
            .bool()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn pop_char(&mut self) -> Result<char> {
        self.pop_value()?
            .char()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn get_char(&self, index: usize) -> Result<char> {
        self.get_value(index)?
            .char()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn pop_address(&mut self) -> Result<usize> {
        self.pop_value()?
            .address()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn get_address(&self, index: usize) -> Result<usize> {
        self.get_value(index)?
            .address()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn pop_function(&mut self) -> Result<NewFunction> {
        todo!("pop_function");
        // self.pop_value()?
        //     .function()
        //     .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn get_function(&self, index: usize) -> Result<NewFunction> {
        todo!("get_function");
        // self.get_value(index)?
        //     .function()
        //     .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn pop_symbol(&mut self) -> Result<Symbol> {
        self.pop_value()?
            .symbol()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn get_symbol(&self, index: usize) -> Result<Symbol> {
        self.get_value(index)?
            .symbol()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn pop_reference(&mut self) -> Result<Reference> {
        self.pop_value()?
            .reference()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn get_reference(&self, index: usize) -> Result<Reference> {
        self.get_value(index)?
            .reference()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn top_value(&self) -> Result<Value> {
        self.data_stack
            .top()
            .copied()
            .vm_result(VmErrorKind::OutOfBounds, &self.frame)
    }

    pub fn top_value_mut(&mut self) -> Result<&mut Value> {
        self.data_stack
            .top_mut()
            .vm_result(VmErrorKind::OutOfBounds, &self.frame)
    }

    pub fn heap_object<T: VmObject>(&self, obj: Reference) -> Result<&T> {
        self.heap
            .get(obj)
            .vm_result(VmErrorKind::InvalidObject, &self.frame)?
            .downcast_ref()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    pub fn heap_object_mut<T: VmObject>(&mut self, obj: Reference) -> Result<&mut T> {
        self.heap
            .get_mut(obj)
            .vm_result(VmErrorKind::InvalidObject, &self.frame)?
            .downcast_mut()
            .vm_result(VmErrorKind::Type, &self.frame)
    }

    #[inline(always)]
    pub fn current_op(&self) -> Option<&OpCode> {
        self.program.code.get(self.ip())
    }

    pub fn get_local(&self, index: usize) -> Result<Value> {
        let base = self.frame.stack_base;
        let count = self.frame.locals;

        let valid_range = base..base + count;

        if valid_range.contains(&index) {
            Ok(self.data_stack.get(index).copied().unwrap())
        } else {
            Err(self.error(VmErrorKind::OutOfBounds))
        }
    }

    pub fn set_local(&mut self, index: usize, value: Value) -> Result<()> {
        let base = self.frame.stack_base;
        let count = self.frame.locals;

        let valid_range = base..base + count;

        if valid_range.contains(&index) {
            let local = self.data_stack.get_mut(index).unwrap();
            *local = value;
            Ok(())
        } else {
            Err(self.error(VmErrorKind::OutOfBounds))
        }
    }

    pub fn set_reserved(&mut self, n: usize) -> Result<()> {
        if self.data_stack.len() >= self.frame.stack_base + n {
            self.frame.locals = n;
            Ok(())
        } else {
            Err(self.error(VmErrorKind::OutOfBounds))
        }
    }

    #[inline(always)]
    pub fn ip(&self) -> usize {
        self.frame.ip
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    pub fn frame(&self) -> &CallFrame {
        &self.frame
    }

    pub fn alloc<T: VmObject>(&mut self, value: T) -> Result<Reference> {
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
                    .vm_result(VmErrorKind::OutOfMemory, &self.frame)
            }
        }
    }

    pub fn resolve_function(&mut self, symbol: Symbol) -> Result<Function> {
        self.cache.get_mut().get_or_insert_function(symbol, || {
            let name = self
                .program
                .symbols
                .get(symbol)
                .vm_result(VmErrorKind::SymbolNotFound, &self.frame)?;

            let path = Path::new(name).vm_result(VmErrorKind::FunctionNotFound, &self.frame)?;

            let functions = match path.object {
                Some(name) => {
                    let ty = self
                        .program
                        .types
                        .get(name)
                        .vm_result(VmErrorKind::TypeNotFound, &self.frame)?;
                    &ty.methods
                }
                None => &self.program.functions,
            };

            let function = functions
                .get(path.member)
                .cloned()
                .vm_result(VmErrorKind::FunctionNotFound, &self.frame)?;

            Ok(function.clone())
        })
    }

    pub fn resolve_function_2(&mut self, symbol: Symbol) -> Result<NewFunction> {
        self.cache.get_mut().get_or_insert_new_function(symbol, || {
            let name = self
                .program
                .symbols
                .get(symbol)
                .vm_result(VmErrorKind::SymbolNotFound, &self.frame)?;

            let path = Path::new(name).vm_result(VmErrorKind::FunctionNotFound, &self.frame)?;

            let functions = match path.object {
                Some(_name) => {
                    todo!("type methods")
                    // let ty = self
                    //     .program
                    //     .types
                    //     .get(name)
                    //     .vm_result(VmErrorKind::TypeNotFound, &self.frame)?;
                    // &ty.methods
                }
                None => &self.program.functions_2,
            };

            let function = functions
                .get(path.member)
                .copied()
                .vm_result(VmErrorKind::FunctionNotFound, &self.frame)?;

            Ok(function)
        })
    }

    pub fn resolve_native_function(&mut self, symbol: Symbol) -> Result<Arc<NativeFn>> {
        let mut cache = self.cache.borrow_mut();

        match cache.native_function_entry(symbol) {
            IntEntry::Occupied(entry) => Ok(entry.get().clone()),
            IntEntry::Vacant(entry) => {
                let name = self
                    .program
                    .symbols
                    .get(symbol)
                    .vm_result(VmErrorKind::SymbolNotFound, &self.frame)?;

                let function = self
                    .program
                    .native_functions
                    .get(name)
                    .cloned()
                    .vm_result(VmErrorKind::FunctionNotFound, &self.frame)?;

                let function = entry.insert(function);

                Ok(function.clone())
            }
        }
    }

    pub(crate) fn error(&self, kind: VmErrorKind) -> VmError {
        VmError::new(kind, &self.frame)
    }

    pub fn reset(&mut self) -> Result<()> {
        let main = self
            .program
            .functions_2
            .get("main")
            .cloned()
            .ok_or_else(|| VmError::new(VmErrorKind::FunctionNotFound, None))?;

        self.frame = CallFrame::new(main, 0, 0);
        self.call_stack = VmStack::new(64);
        self.data_stack = VmStack::new(255);
        self.heap = Heap::new(1024);
        self.cache = RefCell::new(Cache::new());

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
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub struct CallFrame {
    pub(crate) start: usize,
    pub(crate) ip: usize,
    pub(crate) stack_base: usize,
    pub(crate) locals: usize,
}

impl CallFrame {
    pub fn new(func: NewFunction, stack_base: usize, locals: usize) -> Self {
        Self {
            start: func.0,
            ip: func.0,
            stack_base,
            locals,
        }
    }
}
