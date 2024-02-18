use self::{
    heap::{Heap, ObjectRef},
    memory::{CallStack, DataStack},
};
use crate::{
    object::VmObject,
    program::{NativeFn, Path, Program},
    string::Symbol,
    utils::VmResult,
    value::Value,
};
use ops::{Function, OpCode, Transition, VmError, VmErrorKind};
use std::{mem, sync::Arc};

pub mod gc;
pub mod heap;
pub mod memory;
pub mod ops;

// TODO: Reduce usage of ok_or_else and similar methods.

pub struct Vm {
    frame: CallFrame,
    call_stack: CallStack,
    memory: DataStack,
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
            frame: CallFrame::new(Arc::from("main"), main, 0),
            call_stack: CallStack::new(64),
            memory: DataStack::new(128),
            heap: Heap::new(1024),
            program,
        })
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        while let Some(opcode) = self.current_op() {
            match opcode.execute(self)? {
                Transition::Continue => self.frame.ip += 1,
                Transition::Jump => {}
                Transition::Halt => return Ok(()),
            }
        }

        Ok(())
    }

    pub fn push_frame(&mut self, frame: CallFrame) -> Result<(), VmError> {
        self.call_stack
            .push(frame)
            .map_err(|_| self.error(VmErrorKind::StackOverflow))
    }

    pub fn pop_frame(&mut self) -> Result<CallFrame, VmError> {
        self.call_stack
            .pop()
            .vm_err(VmErrorKind::StackUnderflow, self)
    }

    pub fn push_value(&mut self, value: Value) -> Result<(), VmError> {
        self.memory
            .push(value)
            .map_err(|_| self.error(VmErrorKind::StackOverflow))
    }

    pub fn pop_value(&mut self) -> Result<Value, VmError> {
        self.memory.pop().vm_err(VmErrorKind::StackUnderflow, self)
    }

    pub fn get_value(&self, index: usize) -> Result<Value, VmError> {
        let index = (self.memory.len().checked_sub(1))
            .and_then(|last| last.checked_sub(index))
            .vm_err(VmErrorKind::OutOfBounds, self)?;

        self.memory
            .get(index)
            .cloned()
            .vm_err(VmErrorKind::OutOfBounds, self)
    }

    pub fn get_uint(&self, index: usize) -> Result<u64, VmError> {
        self.get_value(index)?
            .uint()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn get_sint(&self, index: usize) -> Result<i64, VmError> {
        self.get_value(index)?
            .sint()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn get_float(&self, index: usize) -> Result<f64, VmError> {
        self.get_value(index)?
            .float()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn get_bool(&self, index: usize) -> Result<bool, VmError> {
        self.get_value(index)?
            .bool()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn get_char(&self, index: usize) -> Result<char, VmError> {
        self.get_value(index)?
            .char()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn get_address(&self, index: usize) -> Result<usize, VmError> {
        self.get_value(index)?
            .address()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn get_symbol(&self, index: usize) -> Result<Symbol, VmError> {
        self.get_value(index)?
            .symbol()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn get_object_ref(&self, index: usize) -> Result<ObjectRef, VmError> {
        self.get_value(index)?
            .object()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn get_object<T: VmObject>(&self, obj: ObjectRef) -> Result<&T, VmError> {
        self.heap
            .get(obj)
            .vm_err(VmErrorKind::InvalidObject, self)?
            .downcast_ref()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn get_object_mut<T: VmObject>(&mut self, obj: ObjectRef) -> Result<&mut T, VmError> {
        // match must be used here because the borrow checker doesn't fully understand partial 
        // borrows.
        match self.heap.get_mut(obj) {
            Some(object) => match object.downcast_mut() {
                Some(t) => Ok(t),
                None => Err(VmError::new(VmErrorKind::Type, &self.frame)),
            },
            None => Err(VmError::new(VmErrorKind::InvalidObject, &self.frame)),
        }
    }

    pub fn current_op(&self) -> Option<OpCode> {
        self.frame.func.get(self.ip()).copied()
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

    // Allocates a
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

    pub fn resolve_native_function(&self, symbol: Symbol) -> Result<Arc<NativeFn>, VmError> {
        let name = self
            .program
            .symbols
            .get(symbol)
            .vm_err(VmErrorKind::SymbolNotFound, self)?;

        let function = self
            .program
            .native_functions
            .get(name)
            .cloned()
            .vm_err(VmErrorKind::FunctionNotFound, self)?;

        Ok(function)
    }

    pub fn resolve_function(&self, symbol: Symbol) -> Result<Function, VmError> {
        let name = self
            .program
            .symbols
            .get(symbol)
            .vm_err(VmErrorKind::SymbolNotFound, self)?;

        let path = Path::new(name).vm_err(VmErrorKind::FunctionNotFound, self)?;

        let functions = match path.object {
            Some(name) => {
                let ty = self
                    .program
                    .types
                    .get(name)
                    .vm_err(VmErrorKind::TypeNotFound, self)?;
                &ty.methods
            }
            None => &self.program.functions,
        };

        let function = functions
            .get(path.member)
            .cloned()
            .vm_err(VmErrorKind::FunctionNotFound, self)?;

        Ok(function)
    }

    pub fn error(&self, kind: VmErrorKind) -> VmError {
        VmError::new(kind, &self.frame)
    }

    #[cfg(test)]
    pub fn reset(&mut self) -> Result<(), VmError> {
        let main = self
            .program
            .functions
            .get("main")
            .cloned()
            .vm_err(VmErrorKind::FunctionNotFound, self)?;

        self.frame = CallFrame::new(Arc::from("main"), main, 0);
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
    pub(crate) values: &'a mut DataStack,
    pub(crate) heap: &'a mut Heap,
}

// TODO: Rewrite tests to be up to date with current VM architecture

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
