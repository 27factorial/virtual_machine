use self::{
    heap::Heap,
    memory::{CallStack, ValueMemory},
};
use crate::{
    native::{NativeFn, NativeRegistry},
    object::VmType,
    ops::{Function, OpCode, OpError, Transition},
    program::{Path, Program},
    string::{SymbolIndex, Symbols},
    utils::HashMap,
    value::Value,
};
use serde_repr::{Deserialize_repr as DeserializeRepr, Serialize_repr as SerializeRepr};
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
    current_frame: CallFrame,
    call_stack: CallStack,
    memory: ValueMemory,
    heap: Heap,
    types: HashMap<Arc<str>, VmType>,
    functions: HashMap<Arc<str>, Function>,
    native_fns: NativeRegistry,
    constants: Box<[Value]>,
    symbols: Symbols,
}

impl Vm {
    pub fn new(program: Program) -> Result<Self, OpError> {
        let main = program
            .functions
            .get("main")
            .cloned()
            .ok_or(OpError::FunctionNotFound)?;

        Ok(Self {
            registers: Registers::new(),
            current_frame: CallFrame::new(main, 0),
            call_stack: CallStack::new(64),
            memory: ValueMemory::new(128),
            heap: Heap::new(1024),
            types: program.types,
            functions: program.functions,
            native_fns: NativeRegistry::new(),
            constants: program.constants.into_boxed_slice(),
            symbols: program.symbols,
        })
    }

    pub fn run(&mut self) -> Result<(), OpError> {
        while let Some(opcode) = self.current_op() {
            match opcode.execute(self)? {
                Transition::Continue => self.current_frame.ip += 1,
                Transition::Jump => {}
                Transition::Halt => return Ok(()),
            }
        }

        Ok(())
    }

    pub fn push_call_stack(&mut self, frame: CallFrame) -> Result<(), OpError> {
        self.call_stack
            .push(frame)
            .map_err(|_| OpError::StackOverflow)
    }

    pub fn pop_call_stack(&mut self) -> Result<CallFrame, OpError> {
        self.call_stack.pop().ok_or(OpError::StackUnderflow)
    }

    pub fn push_data_stack(&mut self, value: Value) -> Result<(), OpError> {
        self.memory.push(value).map_err(|_| OpError::StackOverflow)
    }

    pub fn pop_data_stack(&mut self) -> Result<Value, OpError> {
        self.memory.pop().ok_or(OpError::StackUnderflow)
    }

    pub fn get_func(&self, name: &str) -> Option<Function> {
        self.functions.get(name).cloned()
    }

    pub fn current_op(&self) -> Option<OpCode> {
        self.current_frame.func.get(self.ip()).copied()
    }

    pub fn ip(&self) -> usize {
        self.current_frame.ip
    }

    pub fn ip_mut(&mut self) -> &mut usize {
        &mut self.current_frame.ip
    }

    pub fn resolve_function(&self, symbol: SymbolIndex) -> Result<Function, OpError> {
        let name = self.symbols.get(symbol).ok_or(OpError::SymbolNotFound)?;

        let path = Path::new(name).ok_or(OpError::FunctionNotFound)?;

        let functions = match path.object {
            Some(name) => {
                let vm_type = self.types.get(name).ok_or(OpError::TypeNotFound)?;
                &vm_type.methods
            }
            None => &self.functions,
        };

        let function = functions
            .get(name)
            .cloned()
            .ok_or(OpError::FunctionNotFound)?;

        Ok(function)
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

#[derive(
    Copy,
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Debug,
    SerializeRepr,
    DeserializeRepr,
    EnumCount,
    EnumIter,
)]
#[repr(u8)]
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

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct CallFrame {
    pub(crate) func: Function,
    pub(crate) ip: usize,
}

impl CallFrame {
    pub fn new(func: Function, ip: usize) -> Self {
        let func = func.into();
        Self { func, ip }
    }
}

#[cfg(test)]
mod tests {
    use crate::object::VmObject;

    use super::*;

    #[test]
    fn basic_program() {
        use crate::ops::OpCode::*;

        let mut program = Program::new();

        let crunch = program.define_symbol("crunch");
        let main = program.define_symbol("main");

        program
            .define_function(
                main,
                [
                    LoadImm(Value::Bool(false), Register::R0),
                    Not(Register::R0),
                    LoadImm(Value::Symbol(crunch), Register::R0),
                    Call(Register::R0),
                ],
            )
            .unwrap();

        program
            .define_function(
                crunch,
                [
                    LoadImm(Value::Float(42.0), Register::R1),
                    LoadImm(Value::Float(2.0), Register::R2),
                    Div(Register::R1, Register::R2),
                    Ret,
                ],
            )
            .unwrap();

        let mut vm = Vm::new(program).unwrap();

        vm.run().unwrap();
    }

    fn object_fn() {
        use crate::ops::OpCode::*;

        struct Test;

        impl VmObject for Test {
            fn type_meta() -> VmType
            where
                Self: Sized {
                let mut methods = HashMap::default();

                methods.insert("test".into(), Function::new([
                    LoadImm(Value::UInt(1), Register::R1),
                    AddImm(Register::R1, Value::UInt(1)),
                    Ret,
                ]));

                VmType {
                    name: Arc::from("Test"),
                    fields: Default::default(),
                    methods,
                }
            }

            fn field(&self, name: &str) -> Option<&Value> {
                None
            }

            fn field_mut(&mut self, name: &str) -> Option<&mut Value> {
                None
            }

            fn fields(&self) -> &[Value] {
                &[]
            }
        }

        let mut program = Program::new();

        let object_name = program.define_symbol("Test");

        let ty = program.register_type::<Test>(object_name).unwrap();
    }
}
