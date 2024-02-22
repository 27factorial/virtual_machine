use crate::object::VmObject;
use crate::program::{NativeFn, Path, Program};
use crate::string::Symbol;
use crate::utils::{IntHashMap, IntoVmResult};
use crate::value::Value;
use heap::{Heap, Reference};
use memory::{CallStack, DataStack};
use ops::{Function, OpCode, Transition};
use std::sync::Arc;

pub mod gc;
pub mod heap;
pub mod memory;
pub mod ops;

pub type Result<T> = std::result::Result<T, VmError>;

pub struct Vm {
    frame: CallFrame,
    call_stack: CallStack,
    data_stack: DataStack,
    heap: Heap,
    function_cache: IntHashMap<Symbol, Function>,
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
            frame: CallFrame::new(main, 0),
            call_stack: CallStack::new(64),
            data_stack: DataStack::new(128),
            heap: Heap::new(1024),
            function_cache: IntHashMap::default(),
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
            .vm_err(VmErrorKind::StackOverflow, self)
    }

    pub fn pop_frame(&mut self) -> Result<CallFrame> {
        self.call_stack
            .pop()
            .vm_err(VmErrorKind::StackUnderflow, self)
    }

    pub fn push_value(&mut self, value: Value) -> Result<()> {
        self.data_stack
            .push(value)
            .vm_err(VmErrorKind::StackOverflow, self)
    }

    pub fn pop_value(&mut self) -> Result<Value> {
        self.data_stack
            .pop()
            .vm_err(VmErrorKind::StackUnderflow, self)
    }

    pub fn get_value(&self, index: usize) -> Result<Value> {
        let index = (self.data_stack.len().checked_sub(1))
            .and_then(|last| last.checked_sub(index))
            .vm_err(VmErrorKind::OutOfBounds, self)?;

        self.data_stack
            .get(index)
            .cloned()
            .vm_err(VmErrorKind::OutOfBounds, self)
    }

    pub fn pop_uint(&mut self) -> Result<u64> {
        self.pop_value()?.uint().vm_err(VmErrorKind::Type, self)
    }

    pub fn get_uint(&self, index: usize) -> Result<u64> {
        self.get_value(index)?
            .uint()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn pop_sint(&mut self) -> Result<i64> {
        self.pop_value()?.sint().vm_err(VmErrorKind::Type, self)
    }

    pub fn get_sint(&self, index: usize) -> Result<i64> {
        self.get_value(index)?
            .sint()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn pop_float(&mut self) -> Result<f64> {
        self.pop_value()?.float().vm_err(VmErrorKind::Type, self)
    }

    pub fn get_float(&self, index: usize) -> Result<f64> {
        self.get_value(index)?
            .float()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn pop_bool(&mut self) -> Result<bool> {
        self.pop_value()?.bool().vm_err(VmErrorKind::Type, self)
    }

    pub fn get_bool(&self, index: usize) -> Result<bool> {
        self.get_value(index)?
            .bool()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn pop_char(&mut self) -> Result<char> {
        self.pop_value()?.char().vm_err(VmErrorKind::Type, self)
    }

    pub fn get_char(&self, index: usize) -> Result<char> {
        self.get_value(index)?
            .char()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn pop_address(&mut self) -> Result<usize> {
        self.pop_value()?.address().vm_err(VmErrorKind::Type, self)
    }

    pub fn get_address(&self, index: usize) -> Result<usize> {
        self.get_value(index)?
            .address()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn pop_symbol(&mut self) -> Result<Symbol> {
        self.pop_value()?.symbol().vm_err(VmErrorKind::Type, self)
    }

    pub fn get_symbol(&self, index: usize) -> Result<Symbol> {
        self.get_value(index)?
            .symbol()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn pop_reference(&mut self) -> Result<Reference> {
        self.pop_value()?
            .reference()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn get_reference(&self, index: usize) -> Result<Reference> {
        self.get_value(index)?
            .reference()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn heap_object<T: VmObject>(&self, obj: Reference) -> Result<&T> {
        self.heap
            .get(obj)
            .vm_err(VmErrorKind::InvalidObject, self)?
            .downcast_ref()
            .vm_err(VmErrorKind::Type, self)
    }

    pub fn heap_object_mut<T: VmObject>(&mut self, obj: Reference) -> Result<&mut T> {
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

    pub fn locals(&self) -> &[Value] {
        &self.frame.locals
    }

    pub fn locals_mut(&mut self) -> &mut Vec<Value> {
        &mut self.frame.locals
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

    pub fn alloc<T: VmObject>(&mut self, value: T) -> Result<Reference> {
        match self.heap.alloc(value) {
            Ok(object) => Ok(object),
            Err(value) => {
                // Current roots:
                // all CallFrame locals
                // self.data_stack
                // I would like to use a convenience method here, but partial borrows strike again.
                let stack_values = self.data_stack.iter();
                let locals = self
                    .frame
                    .locals
                    .iter()
                    .chain(self.call_stack.iter().flat_map(|frame| frame.locals.iter()));

                let roots = stack_values
                    .chain(locals)
                    .copied()
                    .filter_map(Value::reference);

                self.heap.collect(roots);

                self.heap
                    .alloc(value)
                    .vm_err(VmErrorKind::OutOfMemory, self)
            }
        }
    }

    pub fn resolve_native_function(&self, symbol: Symbol) -> Result<Arc<NativeFn>> {
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

    pub fn resolve_function(&mut self, symbol: Symbol) -> Result<Function> {
        if let Some(function) = self.function_cache.get(&symbol) {
            Ok(function.clone())
        } else {
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

            self.function_cache.insert(symbol, function.clone());

            Ok(function)
        }
    }

    #[inline(always)]
    pub fn error(&self, kind: VmErrorKind) -> VmError {
        VmError::new(kind, &self.frame)
    }

    #[cfg(test)]
    pub fn reset(&mut self) -> Result<()> {
        let main = self
            .program
            .functions
            .get("main")
            .cloned()
            .vm_err(VmErrorKind::FunctionNotFound, self)?;

        self.frame = CallFrame::new(main, 0);
        self.call_stack.clear();
        self.data_stack.clear();
        self.heap = Heap::new(1024);

        Ok(())
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
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

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct CallFrame {
    pub(crate) func: Function,
    pub(crate) ip: usize,
    pub(crate) locals: Vec<Value>,
}

impl CallFrame {
    pub fn new(func: Function, ip: usize) -> Self {
        Self {
            func,
            ip,
            locals: Vec::with_capacity(4),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{program::Program, value::Value};

    use super::{ops::OpCode, Vm};

    #[test]
    fn basic_program() {
        let mut program = Program::new();

        let main_sym = program.define_symbol("main");
        let adder = program.define_symbol("adder");

        program
            .define_function(
                main_sym,
                [
                    // Initialize a counter to 10 million and store the counter in local variable 0
                    OpCode::Push(Value::UInt(10_000_000)),
                    OpCode::Store(0),
                    // Load the counter from local variable 0, and if it's zero, jump to the end of
                    // the program.
                    OpCode::Load(0),
                    OpCode::EqImm(Value::UInt(0)),
                    OpCode::JumpCondImm(13),
                    // else...
                    // load the value from local variable 0, subtract 1, and store the new counter
                    // back in the local variable 0.
                    OpCode::Load(0),
                    OpCode::SubImm(Value::UInt(1)),
                    OpCode::Store(0),
                    // Push two 2s onto the stack
                    OpCode::Push(Value::UInt(2)),
                    OpCode::Push(Value::UInt(2)),
                    // Call a function which pops them from the stack, adds them, then returns
                    OpCode::CallImm(adder),
                    // Remove the added value (it's not actually used)
                    OpCode::Pop,
                    // Jump back to the counter check above
                    OpCode::JumpImm(2),
                    // halt the virtual machine
                    OpCode::Halt,
                ],
            )
            .expect("failed to define `main` function");

        program
            .define_function(adder, [OpCode::Add, OpCode::Ret])
            .expect("failed to define `crunch` function");

        let mut vm = Vm::new(program).expect("failed to create VM");

        vm.run().expect("failed to run vm");
    }

    // #[test]
    // fn array_object() {
    //     let mut program = Program::new();

    //     Array::register_type(&mut program);

    //     let main_sym = program.define_symbol("main");
    //     let array_new = program.define_symbol("Array::new");
    //     let array_push = program.define_symbol("Array::push");

    //     program
    //         .define_function(
    //             main_sym,
    //             [
    //                 // Push uints 1-4 to stack
    //                 OpCode::Push(Value::UInt(4)),
    //                 OpCode::Push(Value::UInt(3)),
    //                 OpCode::Push(Value::UInt(2)),
    //                 OpCode::Push(Value::UInt(1)),
    //                 // Create a new array and save a copy of the reference to the first local variable
    //                 OpCode::CallImm(array_new),
    //                 OpCode::Copy,
    //                 OpCode::Store(0),
    //                 // Call the Array::push method four times
    //                 OpCode::CallImm(array_push),
    //                 OpCode::Load(0),
    //                 OpCode::CallImm(array_push),
    //                 OpCode::Load(0),
    //                 OpCode::CallImm(array_push),
    //                 OpCode::Load(0),
    //                 OpCode::CallImm(array_push),
    //                 OpCode::Load(0),
    //                 // print out the current state of the array
    //                 OpCode::Dbg(0),
    //                 // halt, we're done.
    //                 OpCode::Halt,
    //             ],
    //         )
    //         .expect("failed to define `main` function");

    //     let mut vm = Vm::new(std::hint::black_box(program)).expect("failed to create VM");

    //     for _ in 0..1_000_000 {
    //         vm.run().expect("failed to execute program");
    //         vm.reset().expect("failed to reset vm");
    //     }
    // }
}
