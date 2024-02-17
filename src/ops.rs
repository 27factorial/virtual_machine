use serde::{Deserialize, Serialize};

use crate::{
    string::SymbolIndex,
    value::Value,
    vm::{CallFrame, Register, Vm},
};

use std::{ops::Index, slice::SliceIndex, sync::Arc};

pub type OpResult = Result<Transition, VmError>;

/// Opcodes representing the virtual machine's instruction set.
///
/// For operations which move values into and out of memory or registers, the operands are in the
/// order (src, dst). For binary operations, the operands are ordered `opcode.0 <operation>
/// opcode.1`.
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug, Serialize, Deserialize)]
pub enum OpCode {
    /// No operation; do nothing.
    NoOp,
    /// Halt the virtual machine immediately.
    Halt,

    /// Push an immediate value to the top of the stack.
    /// Corresponds to the PushImmediate instruction.
    PushImm(Value),
    /// Push the value in a register to the top of the stack.
    Push(Register),
    /// Pop the value at the top of the stack into a register
    Pop(Register),

    /// Add the value in the first register to the value in the second register, putting the result
    /// into the accumulator register.
    Add,
    /// Add an immediate value to the value in a register, putting the result in the accumulator
    /// register.
    AddImm(Value),
    /// Subtract the value in the first register from the value in the second register, putting the
    /// result into the accumulator register.
    Sub,
    /// Subtract an immediate value from the value in a register, putting the result in the
    /// accumulator register.
    SubImm(Value),
    /// Multiply the value in the first register by the value in the second register, putting the
    /// result into the accumulator register.
    Mul,
    /// Multiply an immediate value by the value in a register, putting the result in the
    /// accumulator register.
    MulImm(Value),
    /// Divide the value in the first register by the value in the second register, putting the
    /// result into the accumulator register.
    Div,
    /// Divide the value in a register by an immediate value, putting the result in the accumulator
    /// register.
    DivImm(Value),
    /// Compute the modulus of the value in the first register and the value in the second register,
    /// putting the result into the accumulator register.
    Rem,
    /// Compute the modulus of the value in a register and an immediate value, putting the result in
    /// the accumulator register.
    RemImm(Value),

    /// Compute the bitwise AND operation of the value in the first register and the value in the
    /// second register, putting the result in the accumulator register.
    And,
    /// Compute the bitwise AND operation of the value in the register and an immediate value,
    /// putting the result in the accumulator register.
    AndImm(Value),
    /// Compute the bitwise OR operation of the value in the first register and the value in the
    /// second register, putting the result in the accumulator register.
    Or,
    /// Compute the bitwise OR operation of the value in the register and an immediate value,
    /// putting the result in the accumulator register.
    OrImm(Value),
    /// Compute the bitwise XOR operation of the value in the first register and the value in the
    /// second register, putting the result in the accumulator register.
    Xor,
    /// Compute the bitwise XOR operation of the value in the register and an immediate value,
    /// putting the result in the accumulator register.
    XorImm(Value),
    /// Compute the bitwise NOT operation on the value in the register, putting the result in the
    /// accumulator register.
    Not,

    /// Shift the value in the first register right by the value in the second register, putting the
    /// result in the accumulator register.
    Shr,
    /// Shift the value in the first register right by an immediate value, putting the result in the
    /// accumulator register.
    ShrImm(Value),
    /// Shift the value in the first register left by the value in the second register, putting the
    /// result in the accumulator register.
    Shl,
    /// Shift the value in the first register left by an immediate value, putting the result in the
    /// accumulator register.
    ShlImm(Value),

    /// Determine if the value in the first register and the value in the second register are equal,
    /// putting the result in the accumulator register.
    Eq,
    /// Determine if the value in the register and an immediate value are equal, putting the result
    /// in the accumulator register.
    EqImm(Value),
    /// Determine if the value in the first register is greater than the value in the second
    /// register, putting the result in the accumulator register
    Gt,
    /// Determine if the value in the first register is greater than an immediate value, putting the
    /// result in the accumulator register
    GtImm(Value),
    /// Determine if the value in the first register is greater than or equal to the value in the
    /// second register, putting the result in the accumulator register
    Ge,
    /// Determine if the value in the first register is greater than or equal to an immediate value,
    /// putting the result in the accumulator register
    GeImm(Value),
    /// Determine if the value in the first register is less than the value in the second register,
    /// putting the result in the accumulator register
    Lt,
    /// Determine if the value in the first register is less than an immediate value, putting the
    /// result in the accumulator register
    LtImm(Value),
    /// Determine if the value in the first register is less than or equal to the value in the
    /// second register, putting the result in the accumulator register
    Le,
    /// Determine if the value in the first register is less than or equal to an immediate value,
    /// putting the result in the accumulator register
    LeImm(Value),

    /// Jump to the index in the register within the current function.
    Jump,
    /// Jump to the immediate index within the current function.
    JumpImm(usize),
    /// If the value in the first register is a boolean value of `true`, jump to the index in the
    /// second register within the current function.
    JumpCond,
    /// If the value in the register is a boolean value of `true`, jump to the immediate index
    /// within the current function.
    JumpCondImm(usize),

    /// Jump to the first instruction of a function determined by the index in the register.
    Call,

    CallImm(SymbolIndex),
    /// Call a native function determined by the immediate index pointing to a Program's string
    /// pool.
    CallNative(SymbolIndex),
    /// Set the VM's current frame to the call frame popped from the call stack.
    Ret,

    Init,

    Index,

    /// Print out the value at a memory location to stderr.
    DbgMem(usize),
}

impl OpCode {
    pub fn execute(self, vm: &mut Vm) -> OpResult {
        use OpCode as Op;

        match self {
            Op::NoOp => vm.noop(),
            Op::Halt => vm.halt(),
            Op::PushImm(value) => vm.push_imm(value),
            Op::Push(register) => vm.push(register),
            Op::Pop(register) => vm.pop(register),
            Op::Add => vm.add(),
            Op::AddImm(value) => vm.add_imm(value),
            Op::Sub => vm.sub(),
            Op::SubImm(value) => vm.sub_imm(value),
            Op::Mul => vm.mul(),
            Op::MulImm(value) => vm.mul_imm(value),
            Op::Div => vm.div(),
            Op::DivImm(value) => vm.div_imm(value),
            Op::Rem => vm.rem(),
            Op::RemImm(value) => vm.rem_imm(value),
            Op::And => vm.and(),
            Op::AndImm(value) => vm.and_imm(value),
            Op::Or => vm.or(),
            Op::OrImm(value) => vm.or_imm(value),
            Op::Xor => vm.xor(),
            Op::XorImm(value) => vm.xor_imm(value),
            Op::Not => vm.not(),
            Op::Shr => vm.shr(),
            Op::ShrImm(value) => vm.shr_imm(value),
            Op::Shl => vm.shl(),
            Op::ShlImm(value) => vm.shl_imm(value),
            Op::Eq => vm.eq(),
            Op::EqImm(value) => vm.eq_imm(value),
            Op::Gt => vm.gt(),
            Op::GtImm(value) => vm.gt_imm(value),
            Op::Ge => vm.ge(),
            Op::GeImm(value) => vm.ge_imm(value),
            Op::Lt => vm.lt(),
            Op::LtImm(value) => vm.lt_imm(value),
            Op::Le => vm.le(),
            Op::LeImm(value) => vm.le_imm(value),
            Op::Jump => vm.jump(),
            Op::JumpImm(address) => vm.jump_imm(address),
            Op::JumpCond => vm.jump_cond(),
            Op::JumpCondImm(address) => vm.jump_cond_imm(address),
            Op::Call => vm.call(),
            Op::CallImm(symbol) => vm.call_imm(symbol),
            Op::CallNative(index) => vm.call_native(index),
            Op::Ret => vm.ret(),
            Op::Init => vm.init_object(),
            Op::Index => vm.index_object(),
            Op::DbgMem(address) => vm.dbg_mem(address),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Transition {
    Continue,
    Jump,
    Halt,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct VmError {
    kind: VmErrorKind,
    current_frame: Option<CallFrame>,
}

impl VmError {
    pub fn new<'a>(kind: VmErrorKind, current_frame: impl Into<Option<&'a CallFrame>>) -> Self {
        Self {
            kind,
            current_frame: current_frame.into().cloned(),
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
    InvalidAddress,
    FunctionNotFound,
    SymbolNotFound,
    TypeNotFound,
    OperatorNotSupported,
}

#[derive(Clone, PartialEq, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Function(pub(crate) Arc<[OpCode]>);

impl Function {
    pub fn new(ops: impl IntoIterator<Item = OpCode>) -> Self {
        let ops = ops.into_iter().collect();

        Self(ops)
    }

    pub fn get<T: SliceIndex<[OpCode]>>(&self, index: T) -> Option<&T::Output> {
        self.0.get(index)
    }
}

impl FromIterator<OpCode> for Function {
    fn from_iter<T: IntoIterator<Item = OpCode>>(ops: T) -> Self {
        Self::new(ops)
    }
}

impl<T: SliceIndex<[OpCode]>> Index<T> for Function {
    type Output = T::Output;

    fn index(&self, index: T) -> &Self::Output {
        self.0.index(index)
    }
}

impl Default for Function {
    fn default() -> Self {
        Self(Arc::from([]))
    }
}
