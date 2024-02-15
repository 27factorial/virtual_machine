use self::{
    heap::{Heap, ObjectRef, RootsIter},
    memory::{CallStack, ValueMemory},
};
use crate::{
    object::VmObject,
    ops::{Function, OpCode, OpError, Transition},
    program::{NativeFn, Path, Program},
    string::SymbolIndex,
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

pub mod gc;
pub mod heap;
pub mod memory;
pub(crate) mod ops_impl;

pub struct Vm {
    registers: Registers,
    current_frame: CallFrame,
    call_stack: CallStack,
    memory: ValueMemory,
    heap: Heap,
    program: Program,
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
            program,
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

    pub fn current_op(&self) -> Option<OpCode> {
        self.current_frame.func.get(self.ip()).copied()
    }

    pub fn ip(&self) -> usize {
        self.current_frame.ip
    }

    pub fn ip_mut(&mut self) -> &mut usize {
        &mut self.current_frame.ip
    }

    pub fn memory(&mut self) -> MemoryHandle<'_> {
        MemoryHandle {
            registers: &mut self.registers,
            values: &mut self.memory,
            heap: &mut self.heap,
        }
    }

    pub fn alloc<T: VmObject>(&mut self, value: T) -> ObjectRef {
        match self.heap.alloc(value) {
            Ok(object) => object,
            Err(value) => {
                self.heap
                    .collect(RootsIter::new(&self.registers, &self.memory));

                self.heap.alloc(value).unwrap_or_else(|_| {
                    panic!(
                        "VM heap out of memory, failed to allocate {} bytes",
                        mem::size_of::<T>()
                    );
                })
            }
        }
    }

    pub fn resolve_native_function(&self, symbol: SymbolIndex) -> Result<Arc<NativeFn>, OpError> {
        let name = self
            .program
            .symbols
            .get(symbol)
            .ok_or(OpError::SymbolNotFound)?;

        let function = self
            .program
            .native_functions
            .get(name)
            .cloned()
            .ok_or(OpError::FunctionNotFound)?;

        Ok(function)
    }

    pub fn resolve_function(&self, symbol: SymbolIndex) -> Result<Function, OpError> {
        let name = self
            .program
            .symbols
            .get(symbol)
            .ok_or(OpError::SymbolNotFound)?;

        let path = Path::new(name).ok_or(OpError::FunctionNotFound)?;

        let functions = match path.object {
            Some(name) => {
                let vm_type = self.program.types.get(name).ok_or(OpError::TypeNotFound)?;
                &vm_type.methods
            }
            None => &self.program.functions,
        };

        let function = functions
            .get(path.member)
            .cloned()
            .ok_or(OpError::FunctionNotFound)?;

        Ok(function)
    }

    #[cfg(test)]
    pub fn reset(&mut self) -> Result<(), OpError> {
        let main = self
            .program
            .functions
            .get("main")
            .cloned()
            .ok_or(OpError::FunctionNotFound)?;

        self.registers = Registers::new();
        self.current_frame = CallFrame::new(main, 0);
        self.call_stack.clear();
        self.memory.clear();
        self.heap = Heap::new(1024);

        Ok(())
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
        Self { func, ip }
    }
}

pub struct MemoryHandle<'a> {
    pub(crate) registers: &'a mut Registers,
    pub(crate) values: &'a mut ValueMemory,
    pub(crate) heap: &'a mut Heap,
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::object::{Operators, VmObject, VmType};

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

    #[test]
    fn object_fn() {
        use crate::ops::OpCode::*;

        struct Test;

        impl VmObject for Test {
            fn register_type(program: &mut Program) -> &VmType
            where
                Self: Sized,
            {
                let methods = [(
                    Arc::from("test"),
                    Function::new([
                        LoadImm(Value::UInt(1), Register::R0),
                        AddImm(Register::R0, Value::UInt(1)),
                        Ret,
                    ]),
                )]
                .into_iter()
                .collect();

                let operators = Operators {
                    init: Function::new([Ret]),
                    deinit: None,
                    index: None,
                };

                program.register_type(VmType {
                    name: Arc::from("Test"),
                    operators,
                    fields: Default::default(),
                    methods,
                })
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

        let test = program.define_symbol("Test::test");
        let main = program.define_symbol("main");

        Test::register_type(&mut program);

        program.define_function(main, [CallImm(test)]).unwrap();

        let mut vm = Vm::new(program).unwrap();

        vm.run().unwrap();
    }

    #[test]
    fn native() {
        let mut program = Program::new();

        String::register_type(&mut program);

        let main = program.define_symbol("main");
        let string_ty = program.define_symbol("String");
        let print = program.define_symbol("String::print");

        program
            .define_function(
                main,
                [
                    OpCode::LoadImm(Value::Symbol(string_ty), Register::R0),
                    OpCode::Init(Register::R0),
                    OpCode::CallImm(print),
                    OpCode::Halt,
                ],
            )
            .unwrap();

        let mut vm = Vm::new(program).unwrap();

        vm.run().unwrap();
    }
}
