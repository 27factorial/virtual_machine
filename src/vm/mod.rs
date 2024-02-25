use self::cache::Cache;
use crate::object::VmObject;
use crate::program::{NativeFn, Path, Program};
use crate::symbol::Symbol;
use crate::utils::{IntEntry, IntoVmResult};
use crate::value::Value;
use heap::{Heap, Reference};
use memory::{CallStack, DataStack};
use ops::{Function, OpCode, Transition};
use std::cell::RefCell;
use std::sync::Arc;

pub mod cache;
pub mod gc;
pub mod heap;
pub mod memory;
pub mod ops;

// TODO: Finish DataStack-based locals

pub type Result<T> = std::result::Result<T, VmError>;

#[derive(Debug)]
pub struct Vm {
    pub(crate) frame: CallFrame,
    call_stack: CallStack,
    data_stack: DataStack,
    heap: Heap,
    cache: RefCell<Cache>,
    program: Program,
}

impl Vm {
    pub fn new(program: Program) -> Result<Self> {
        let main = program
            .functions
            .get("main")
            .cloned()
            .ok_or_else(|| VmError::new(VmErrorKind::FunctionNotFound, None))?;

        Ok(Self {
            frame: CallFrame::new(main, 0, 0, 0),
            call_stack: CallStack::new(64),
            data_stack: DataStack::new(255),
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

    pub fn current_op(&self) -> Option<OpCode> {
        self.frame.func.get(self.ip()).copied()
    }

    pub fn get_local(&self, index: usize) -> Result<Value> {
        let base = self.frame.stack_base;
        let count = self.frame.locals;

        let valid_range = base..base + count;

        if valid_range.contains(&index) {
            Ok(self.get_value(index)?)
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

    pub fn ip(&self) -> usize {
        self.frame.ip
    }

    pub fn ip_mut(&mut self) -> &mut usize {
        &mut self.frame.ip
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
                // all CallFrame locals
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

    pub fn resolve_function(&self, symbol: Symbol) -> Result<Function> {
        let mut cache = self.cache.borrow_mut();

        match cache.function_entry(symbol) {
            IntEntry::Occupied(entry) => Ok(entry.get().clone()),
            IntEntry::Vacant(entry) => {
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

                let function = entry.insert(function);

                Ok(function.clone())
            }
        }
    }

    pub fn resolve_native_function(&self, symbol: Symbol) -> Result<Arc<NativeFn>> {
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

    // #[cfg(test)]
    // pub fn reset(&mut self) -> Result<()> {
    //     let main = self
    //         .program
    //         .functions
    //         .get("main")
    //         .cloned()
    //         .vm_err(VmErrorKind::FunctionNotFound, &self.frame)?;

    //     self.frame = CallFrame::new(main, 0, 0..1);
    //     self.call_stack.clear();
    //     self.data_stack.clear();
    //     self.heap = Heap::new(1024);

    //     Ok(())
    // }
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

#[derive(Clone, PartialEq, Debug)]
pub struct CallFrame {
    pub(crate) func: Function,
    pub(crate) ip: usize,
    pub(crate) stack_base: usize,
    pub(crate) locals: usize,
}

impl CallFrame {
    pub fn new(func: Function, ip: usize, stack_base: usize, locals: usize) -> Self {
        Self {
            func,
            ip,
            stack_base,
            locals,
        }
    }
}

// #[cfg(test)]
// mod test {
//     use crate::{program::Program, value::Value};

//     use super::{ops::OpCode, Vm};

//     #[test]
//     fn basic_program() {
//         let mut program = Program::new();

//         let main_sym = program.define_symbol("main");
//         let adder = program.define_symbol("adder");

//         program
//             .define_function(
//                 main_sym,
//                 [
//                     // Initialize a counter to 10 million and store the counter in local variable 0
//                     OpCode::Push(Value::UInt(10_000_000)),
//                     OpCode::Store(0),
//                     // Load the counter from local variable 0, and if it's zero, jump to the end of
//                     // the program.
//                     OpCode::Load(0),
//                     OpCode::EqImm(Value::UInt(0)),
//                     OpCode::JumpCondImm(13),
//                     // else...
//                     // load the value from local variable 0, subtract 1, and store the new counter
//                     // back in the local variable 0.
//                     OpCode::Load(0),
//                     OpCode::SubImm(Value::UInt(1)),
//                     OpCode::Store(0),
//                     // Push two 2s onto the stack
//                     OpCode::Push(Value::UInt(2)),
//                     OpCode::Push(Value::UInt(2)),
//                     // Call a function which pops them from the stack, adds them, then returns
//                     OpCode::CallImm(adder),
//                     // Remove the added value (it's not actually used)
//                     OpCode::Pop,
//                     // Jump back to the counter check above
//                     OpCode::JumpImm(2),
//                     // halt the virtual machine
//                     OpCode::Halt,
//                 ],
//             )
//             .expect("failed to define `main` function");

//         program
//             .define_function(adder, [OpCode::Add, OpCode::Ret])
//             .expect("failed to define `crunch` function");

//         let mut vm = Vm::new(program).expect("failed to create VM");

//         vm.run().expect("failed to run vm");
//     }

//     // #[test]
//     // fn array_object() {
//     //     let mut program = Program::new();

//     //     Array::register_type(&mut program);

//     //     let main_sym = program.define_symbol("main");
//     //     let array_new = program.define_symbol("Array::new");
//     //     let array_push = program.define_symbol("Array::push");

//     //     program
//     //         .define_function(
//     //             main_sym,
//     //             [
//     //                 // Push uints 1-4 to stack
//     //                 OpCode::Push(Value::UInt(4)),
//     //                 OpCode::Push(Value::UInt(3)),
//     //                 OpCode::Push(Value::UInt(2)),
//     //                 OpCode::Push(Value::UInt(1)),
//     //                 // Create a new array and save a copy of the reference to the first local variable
//     //                 OpCode::CallImm(array_new),
//     //                 OpCode::Copy,
//     //                 OpCode::Store(0),
//     //                 // Call the Array::push method four times
//     //                 OpCode::CallImm(array_push),
//     //                 OpCode::Load(0),
//     //                 OpCode::CallImm(array_push),
//     //                 OpCode::Load(0),
//     //                 OpCode::CallImm(array_push),
//     //                 OpCode::Load(0),
//     //                 OpCode::CallImm(array_push),
//     //                 OpCode::Load(0),
//     //                 // print out the current state of the array
//     //                 OpCode::Dbg(0),
//     //                 // halt, we're done.
//     //                 OpCode::Halt,
//     //             ],
//     //         )
//     //         .expect("failed to define `main` function");

//     //     let mut vm = Vm::new(std::hint::black_box(program)).expect("failed to create VM");

//     //     for _ in 0..1_000_000 {
//     //         vm.run().expect("failed to execute program");
//     //         vm.reset().expect("failed to reset vm");
//     //     }
//     // }
// }
