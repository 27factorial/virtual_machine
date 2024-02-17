use self::{
    heap::{Heap, ObjectRef},
    memory::{CallStack, ValueMemory},
};
use crate::{
    object::VmObject,
    program::{NativeFn, Path, Program},
    string::SymbolIndex,
    utils::HashMap,
    value::Value,
};
use ops::{Function, OpCode, Transition, VmError, VmErrorKind};
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
pub mod ops;
mod ops_impl;

pub struct Vm {
    current_frame: CallFrame,
    call_stack: CallStack,
    memory: ValueMemory,
    heap: Heap,
    program: Program,
}

impl Vm {
    pub fn new(program: Program) -> Result<Self, VmError> {
        let main = program
            .functions
            .get("main")
            .cloned()
            .ok_or_else(|| VmError::new(VmErrorKind::FunctionNotFound, None))?;

        Ok(Self {
            current_frame: CallFrame::new(Arc::from("main"), main, 0),
            call_stack: CallStack::new(64),
            memory: ValueMemory::new(128),
            heap: Heap::new(1024),
            program,
        })
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        while let Some(opcode) = self.current_op() {
            match opcode.execute(self)? {
                Transition::Continue => self.current_frame.ip += 1,
                Transition::Jump => {}
                Transition::Halt => return Ok(()),
            }
        }

        Ok(())
    }

    pub fn push_call_stack(&mut self, frame: CallFrame) -> Result<(), VmError> {
        self.call_stack
            .push(frame)
            .map_err(|_| VmError::new(VmErrorKind::StackOverflow, Some(&self.current_frame)))
    }

    pub fn pop_call_stack(&mut self) -> Result<CallFrame, VmError> {
        self.call_stack.pop().ok_or(VmError::new(
            VmErrorKind::StackUnderflow,
            Some(&self.current_frame),
        ))
    }

    pub fn push_data_stack(&mut self, value: Value) -> Result<(), VmError> {
        self.memory
            .push(value)
            .map_err(|_| VmError::new(VmErrorKind::StackOverflow, Some(&self.current_frame)))
    }

    pub fn pop_data_stack(&mut self) -> Result<Value, VmError> {
        self.memory
            .pop()
            .ok_or_else(|| VmError::new(VmErrorKind::StackUnderflow, Some(&self.current_frame)))
    }

    pub fn get_data_stack(&self, index: usize) -> Result<Value, VmError> {
        let index = (self.memory.len().checked_sub(1))
            .and_then(|last| last.checked_sub(index))
            .ok_or_else(|| VmError::new(VmErrorKind::StackUnderflow, &self.current_frame))?;

        self.memory
            .get(index)
            .cloned()
            .ok_or_else(|| VmError::new(VmErrorKind::StackUnderflow, &self.current_frame))
    }

    pub fn get_data_stack_mut(&mut self, index: usize) -> Result<&mut Value, VmError> {
        let index = (self.memory.len().checked_sub(1))
            .and_then(|last| last.checked_sub(index))
            .ok_or_else(|| VmError::new(VmErrorKind::StackUnderflow, &self.current_frame))?;

        self.memory
            .get_mut(index)
            .ok_or_else(|| VmError::new(VmErrorKind::StackUnderflow, &self.current_frame))
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
            values: &mut self.memory,
            heap: &mut self.heap,
        }
    }

    pub fn alloc<T: VmObject>(&mut self, value: T) -> ObjectRef {
        match self.heap.alloc(value) {
            Ok(object) => object,
            Err(value) => {
                let iter = self.memory.iter().copied().filter_map(Value::object);

                self.heap.collect(iter);

                self.heap.alloc(value).unwrap_or_else(|_| {
                    panic!(
                        "VM heap out of memory, failed to allocate {} bytes",
                        mem::size_of::<T>()
                    );
                })
            }
        }
    }

    pub fn resolve_native_function(&self, symbol: SymbolIndex) -> Result<Arc<NativeFn>, VmError> {
        let name =
            self.program.symbols.get(symbol).ok_or_else(|| {
                VmError::new(VmErrorKind::SymbolNotFound, Some(&self.current_frame))
            })?;

        let function = self
            .program
            .native_functions
            .get(name)
            .cloned()
            .ok_or_else(|| {
                VmError::new(VmErrorKind::FunctionNotFound, Some(&self.current_frame))
            })?;

        Ok(function)
    }

    pub fn resolve_function(&self, symbol: SymbolIndex) -> Result<Function, VmError> {
        let name =
            self.program.symbols.get(symbol).ok_or_else(|| {
                VmError::new(VmErrorKind::SymbolNotFound, Some(&self.current_frame))
            })?;

        let path = Path::new(name).ok_or_else(|| {
            VmError::new(VmErrorKind::FunctionNotFound, Some(&self.current_frame))
        })?;

        let functions = match path.object {
            Some(name) => {
                let vm_type = self.program.types.get(name).ok_or_else(|| {
                    VmError::new(VmErrorKind::TypeNotFound, Some(&self.current_frame))
                })?;
                &vm_type.methods
            }
            None => &self.program.functions,
        };

        let function = functions.get(path.member).cloned().ok_or_else(|| {
            VmError::new(VmErrorKind::FunctionNotFound, Some(&self.current_frame))
        })?;

        Ok(function)
    }

    #[cfg(test)]
    pub fn reset(&mut self) -> Result<(), VmError> {
        let main = self.program.functions.get("main").cloned().ok_or_else(|| {
            VmError::new(VmErrorKind::FunctionNotFound, Some(&self.current_frame))
        })?;

        self.current_frame = CallFrame::new(Arc::from("main"), main, 0);
        self.call_stack.clear();
        self.memory.clear();
        self.heap = Heap::new(1024);

        Ok(())
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct CallFrame {
    pub(crate) name: Arc<str>,
    pub(crate) func: Function,
    pub(crate) ip: usize,
}

impl CallFrame {
    pub fn new(name: Arc<str>, func: Function, ip: usize) -> Self {
        Self { name, func, ip }
    }
}

pub struct MemoryHandle<'a> {
    pub(crate) values: &'a mut ValueMemory,
    pub(crate) heap: &'a mut Heap,
}

// #[cfg(test)]
// mod tests {
//     use std::sync::Arc;

//     use crate::object::{Operators, VmObject, VmType};

//     use super::*;

//     #[test]
//     fn basic_program() {
//         use crate::ops::OpCode::*;

//         let mut program = Program::new();

//         let crunch = program.define_symbol("crunch");
//         let main = program.define_symbol("main");

//         program
//             .define_function(
//                 main,
//                 [
//                     LoadImm(Value::Bool(false), Register::R0),
//                     Not(Register::R0),
//                     LoadImm(Value::Symbol(crunch), Register::R0),
//                     Call(Register::R0),
//                 ],
//             )
//             .unwrap();

//         program
//             .define_function(
//                 crunch,
//                 [
//                     LoadImm(Value::Float(42.0), Register::R1),
//                     LoadImm(Value::Float(2.0), Register::R2),
//                     Div(Register::R1, Register::R2),
//                     Ret,
//                 ],
//             )
//             .unwrap();

//         let mut vm = Vm::new(program).unwrap();

//         vm.run().unwrap();
//     }

//     #[test]
//     fn object_fn() {
//         use crate::ops::OpCode::*;

//         struct Test;

//         impl VmObject for Test {
//             fn register_type(program: &mut Program) -> &VmType
//             where
//                 Self: Sized,
//             {
//                 let methods = [(
//                     Arc::from("test"),
//                     Function::new([
//                         LoadImm(Value::UInt(1), Register::R0),
//                         AddImm(Register::R0, Value::UInt(1)),
//                         Ret,
//                     ]),
//                 )]
//                 .into_iter()
//                 .collect();

//                 let operators = Operators {
//                     init: Function::new([Ret]),
//                     deinit: None,
//                     index: None,
//                 };

//                 program.register_type(VmType {
//                     name: Arc::from("Test"),
//                     operators,
//                     fields: Default::default(),
//                     methods,
//                 })
//             }

//             fn field(&self, name: &str) -> Option<&Value> {
//                 None
//             }

//             fn field_mut(&mut self, name: &str) -> Option<&mut Value> {
//                 None
//             }

//             fn fields(&self) -> &[Value] {
//                 &[]
//             }
//         }

//         let mut program = Program::new();

//         let test = program.define_symbol("Test::test");
//         let main = program.define_symbol("main");

//         Test::register_type(&mut program);

//         program.define_function(main, [CallImm(test)]).unwrap();

//         let mut vm = Vm::new(program).unwrap();

//         vm.run().unwrap();
//     }

//     #[test]
//     fn native() {
//         let mut program = Program::new();

//         String::register_type(&mut program);

//         let main = program.define_symbol("main");
//         let string_ty = program.define_symbol("String");
//         let print = program.define_symbol("String::print");

//         program
//             .define_function(
//                 main,
//                 [
//                     OpCode::LoadImm(Value::Symbol(string_ty), Register::R0),
//                     OpCode::Init(Register::R0),
//                     OpCode::CallImm(print),
//                     OpCode::Halt,
//                 ],
//             )
//             .unwrap();

//         let mut vm = Vm::new(program).unwrap();

//         vm.run().unwrap();
//     }
// }
