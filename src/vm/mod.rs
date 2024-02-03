use self::{
    heap::Heap,
    memory::{CallStack, ValueMemory},
};
use crate::{
    native::{NativeFn, NativeRegistry},
    ops::{Function, OpError, Transition},
    project::ProgramFile,
    value::Value,
};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Index, IndexMut, Mul, Rem, Sub};
use strum::{EnumCount, EnumIter};

pub mod heap;
pub mod memory;
pub(crate) mod ops_impl;

pub struct Vm {
    registers: Registers,
    current_frame: CallFrame,
    call_stack: CallStack,
    memory: ValueMemory,
    heap: Heap,
    functions: Vec<Function>,
    native_fns: NativeRegistry,
    constants: Box<[Value]>,
}

impl Vm {
    pub fn new(program: ProgramFile) -> Self {
        Self {
            registers: Registers::new(),
            current_frame: CallFrame::default(),
            call_stack: CallStack::new(64),
            memory: ValueMemory::new(128),
            heap: Heap::new(1024),
            functions: program.functions,
            native_fns: NativeRegistry::new(),
            constants: program.constants.into_boxed_slice(),
        }
    }

    pub fn run(&mut self) -> Result<(), OpError> {
        // let mut current_func = &program.functions[self.current_frame.func];
        // let mut func_len = current_func.0.len();

        // while self.current_frame.ip < func_len {
        //     let Some(opcode) = current_func.0.get(self.current_frame.ip).cloned() else {
        //         break;
        //     };

        //     let result = opcode.execute(self, &program)?;

        //     match result {
        //         Transition::Continue => self.current_frame.ip += 1,
        //         Transition::Jump => {
        //             // If the transition was a jump, we may have called a function, and need to
        //             // update the function length.
        //             current_func = &program.functions[self.current_frame.func];
        //             func_len = current_func.0.len();
        //         }
        //         Transition::Halt => break,
        //     }
        // }

        Ok(())
    }

    pub fn push_call_stack(&mut self, frame: CallFrame) {
        self.call_stack.push(frame);
    }

    pub fn pop_call_stack(&mut self) -> Result<CallFrame, OpError> {
        self.call_stack.pop().ok_or(OpError::StackUnderflow)
    }

    pub fn push_data_stack(&mut self, value: Value) {
        self.memory.push(value);
    }

    pub fn pop_data_stack(&mut self) -> Result<Value, OpError> {
        self.memory.pop().ok_or(OpError::StackUnderflow)
    }

    #[cfg(test)]
    pub fn reset(&mut self) {
        self.current_frame = CallFrame::default();
        self.registers = Registers::new();
        self.call_stack.clear();
        self.memory.clear();
    }
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Debug, Default)]
pub struct Registers([Value; Register::COUNT]);

impl Registers {
    pub const fn new() -> Self {
        Self([Value::Null; Register::COUNT])
    }
}

impl Index<Register> for Registers {
    type Output = Value;

    /// Performs the indexing (`container[index]`) operation.
    ///
    /// # Panics
    ///
    /// Unlike most `Index` implementations, this method cannot panic.
    fn index(&self, index: Register) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<Register> for Registers {
    /// Performs the mutable indexing (`container[index]`) operation.
    ///
    /// # Panics
    ///
    /// Unlike most `IndexMut` implementations, this method cannot panic.
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, EnumCount, EnumIter)]
pub enum Register {
    /// Register 0, also called the accumulator.
    R0,
    /// Register 1.
    R1,
    /// Register 2.
    R2,
    /// Register 3.
    R3,
    /// Register 4.
    R4,
    /// Register 5.
    R5,
    /// Register 6.
    R6,
    /// Register 7.
    R7,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub struct CallFrame {
    pub(crate) func: usize,
    pub(crate) ip: usize,
}

impl CallFrame {
    pub fn new(func: usize, ip: usize) -> Self {
        Self { func, ip }
    }
}
