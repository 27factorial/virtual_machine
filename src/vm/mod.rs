use self::{
    heap::Heap,
    memory::{CallStack, ValueMemory},
};
use crate::{
    native::{NativeFn, NativeRegistry},
    ops::{Function, OpError, Transition},
    program::Program,
    string::Symbols,
    utils::HashMap,
    value::Value,
};
use std::{
    mem,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Index, IndexMut, Mul, Rem, Sub},
    sync::Arc,
};
use strum::{EnumCount, EnumIter};

pub mod heap;
pub mod memory;
pub(crate) mod ops_impl;

pub struct Vm {
    registers: Registers,
    call_stack: CallStack,
    memory: ValueMemory,
    heap: Heap,
    functions: HashMap<Arc<str>, Function>,
    native_fns: NativeRegistry,
    constants: Box<[Value]>,
    symbols: Symbols,
}

impl Vm {
    pub fn new(program: Program) -> Self {
        Self {
            registers: Registers::new(),
            call_stack: CallStack::new(64),
            memory: ValueMemory::new(128),
            heap: Heap::new(1024),
            functions: program.functions,
            native_fns: NativeRegistry::new(),
            constants: program.constants.into_boxed_slice(),
            symbols: program.symbols,
        }
    }

    pub fn run(&mut self) -> Result<(), OpError> {
        // Avoids borrowing problems.
        let functions = mem::take(&mut self.functions);

        let mut ip = 0;
        let mut current_func = functions.get("main").ok_or(OpError::FunctionNotFound)?;
        let mut next_opcode = current_func.get(ip).copied();

        while let Some(opcode) = next_opcode {
            match opcode.execute(self)? {
                Transition::Continue => ip += 1,
                Transition::Jump(address) => ip = address,
                Transition::Call(func) => {
                    let (name, called_func) = functions
                        .get_key_value(func)
                        .ok_or(OpError::FunctionNotFound)?;

                    self.push_call_stack(CallFrame::new(Arc::clone(name), ip));

                    current_func = called_func;
                    ip = 0;
                }
                Transition::Ret => {
                    let frame = self.pop_call_stack()?;

                    current_func = functions
                        .get(&frame.func)
                        .ok_or(OpError::FunctionNotFound)?;
                    ip = frame.ip
                }
                Transition::Halt => return Ok(()),
            }

            next_opcode = current_func.get(ip).copied();
        }

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

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct CallFrame {
    pub(crate) func: Arc<str>,
    pub(crate) ip: usize,
}

impl CallFrame {
    pub fn new(func: impl Into<Arc<str>>, ip: usize) -> Self {
        let func = func.into();
        Self { func, ip }
    }
}

#[cfg(test)]
mod tests {
    use std::time::Instant;

    use super::*;

    #[test]
    fn basic_program() {
        use crate::ops::OpCode::*;

        let mut program = Program::new();

        let crunch = program.define_function("crunch", [
            LoadImm(Value::Float(42.0), Register::R1),
            LoadImm(Value::Float(2.0), Register::R2),
            Div(Register::R1, Register::R2),
            Ret,
        ]).unwrap();

        program.define_function("main", [
            LoadImm(Value::Bool(false), Register::R0),
            Not(Register::R0),
            LoadImm(Value::Symbol(crunch), Register::R0),
            Call(Register::R0),
        ]).unwrap();

        let mut vm = Vm::new(program);

        vm.run().unwrap();
    }
}
