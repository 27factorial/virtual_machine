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

    /// Move the value in one register to another register.
    LoadReg(Register, Register),
    /// Move an immediate value into a register.
    LoadImm(Value, Register),
    /// Move the value at a memory address into a register.
    LoadMem(usize, Register),

    /// Store the value in a register in a memory location.
    StoreReg(Register, usize),
    /// Store an immediate value in a memory location.
    StoreImm(Value, usize),
    /// Store the value at a memory address in another memory address.
    StoreMem(usize, usize),

    /// Add the value in the first register to the value in the second register, putting the result
    /// into the accumulator register.
    Add(Register, Register),
    /// Add an immediate value to the value in a register, putting the result in the accumulator
    /// register.
    AddImm(Register, Value),
    /// Subtract the value in the first register from the value in the second register, putting the
    /// result into the accumulator register.
    Sub(Register, Register),
    /// Subtract an immediate value from the value in a register, putting the result in the
    /// accumulator register.
    SubImm(Register, Value),
    /// Multiply the value in the first register by the value in the second register, putting the
    /// result into the accumulator register.
    Mul(Register, Register),
    /// Multiply an immediate value by the value in a register, putting the result in the
    /// accumulator register.
    MulImm(Register, Value),
    /// Divide the value in the first register by the value in the second register, putting the
    /// result into the accumulator register.
    Div(Register, Register),
    /// Divide the value in a register by an immediate value, putting the result in the accumulator
    /// register.
    DivImm(Register, Value),
    /// Compute the modulus of the value in the first register and the value in the second register,
    /// putting the result into the accumulator register.
    Rem(Register, Register),
    /// Compute the modulus of the value in a register and an immediate value, putting the result in
    /// the accumulator register.
    RemImm(Register, Value),

    /// Compute the bitwise AND operation of the value in the first register and the value in the
    /// second register, putting the result in the accumulator register.
    And(Register, Register),
    /// Compute the bitwise AND operation of the value in the register and an immediate value,
    /// putting the result in the accumulator register.
    AndImm(Register, Value),
    /// Compute the bitwise OR operation of the value in the first register and the value in the
    /// second register, putting the result in the accumulator register.
    Or(Register, Register),
    /// Compute the bitwise OR operation of the value in the register and an immediate value,
    /// putting the result in the accumulator register.
    OrImm(Register, Value),
    /// Compute the bitwise XOR operation of the value in the first register and the value in the
    /// second register, putting the result in the accumulator register.
    Xor(Register, Register),
    /// Compute the bitwise XOR operation of the value in the register and an immediate value,
    /// putting the result in the accumulator register.
    XorImm(Register, Value),
    /// Compute the bitwise NOT operation on the value in the register, putting the result in the
    /// accumulator register.
    Not(Register),

    /// Shift the value in the first register right by the value in the second register, putting the
    /// result in the accumulator register.
    Shr(Register, Register),
    /// Shift the value in the first register right by an immediate value, putting the result in the
    /// accumulator register.
    ShrImm(Register, Value),
    /// Shift the value in the first register left by the value in the second register, putting the
    /// result in the accumulator register.
    Shl(Register, Register),
    /// Shift the value in the first register left by an immediate value, putting the result in the
    /// accumulator register.
    ShlImm(Register, Value),

    /// Determine if the value in the first register and the value in the second register are equal,
    /// putting the result in the accumulator register.
    Eq(Register, Register),
    /// Determine if the value in the register and an immediate value are equal, putting the result
    /// in the accumulator register.
    EqImm(Register, Value),
    /// Determine if the value in the first register is greater than the value in the second
    /// register, putting the result in the accumulator register
    Gt(Register, Register),
    /// Determine if the value in the first register is greater than an immediate value, putting the
    /// result in the accumulator register
    GtImm(Register, Value),
    /// Determine if the value in the first register is greater than or equal to the value in the
    /// second register, putting the result in the accumulator register
    Ge(Register, Register),
    /// Determine if the value in the first register is greater than or equal to an immediate value,
    /// putting the result in the accumulator register
    GeImm(Register, Value),
    /// Determine if the value in the first register is less than the value in the second register,
    /// putting the result in the accumulator register
    Lt(Register, Register),
    /// Determine if the value in the first register is less than an immediate value, putting the
    /// result in the accumulator register
    LtImm(Register, Value),
    /// Determine if the value in the first register is less than or equal to the value in the
    /// second register, putting the result in the accumulator register
    Le(Register, Register),
    /// Determine if the value in the first register is less than or equal to an immediate value,
    /// putting the result in the accumulator register
    LeImm(Register, Value),

    /// Jump to the index in the register within the current function.
    Jump(Register),
    /// Jump to the immediate index within the current function.
    JumpImm(usize),
    /// If the value in the first register is a boolean value of `true`, jump to the index in the
    /// second register within the current function.
    JumpCond(Register, Register),
    /// If the value in the register is a boolean value of `true`, jump to the immediate index
    /// within the current function.
    JumpCondImm(Register, usize),

    /// Jump to the first instruction of a function determined by the index in the register.
    Call(Register),

    CallImm(SymbolIndex),
    /// Call a native function determined by the immediate index pointing to a Program's string
    /// pool.
    CallNative(SymbolIndex),
    /// Set the VM's current frame to the call frame popped from the call stack.
    Ret,

    Init(Register),

    Index(Register),

    /// Print out the value in the register to stderr.
    DbgReg(Register),
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
            Op::LoadReg(register_src, register_dst) => vm.load_reg(register_src, register_dst),
            Op::LoadImm(value, register) => vm.load_imm(value, register),
            Op::LoadMem(address, register) => vm.load_mem(address, register),
            Op::StoreReg(register, address) => vm.store_reg(register, address),
            Op::StoreImm(value, address) => vm.store_imm(value, address),
            Op::StoreMem(address_src, address_dst) => vm.store_mem(address_src, address_dst),
            Op::Add(register_a, register_b) => vm.add(register_a, register_b),
            Op::AddImm(register, value) => vm.add_imm(register, value),
            Op::Sub(register_a, register_b) => vm.sub(register_a, register_b),
            Op::SubImm(register, value) => vm.sub_imm(register, value),
            Op::Mul(register_a, register_b) => vm.mul(register_a, register_b),
            Op::MulImm(register, value) => vm.mul_imm(register, value),
            Op::Div(register_a, register_b) => vm.div(register_a, register_b),
            Op::DivImm(register, value) => vm.div_imm(register, value),
            Op::Rem(register_a, register_b) => vm.rem(register_a, register_b),
            Op::RemImm(register, value) => vm.rem_imm(register, value),
            Op::And(register_a, register_b) => vm.and(register_a, register_b),
            Op::AndImm(register, value) => vm.and_imm(register, value),
            Op::Or(register_a, register_b) => vm.or(register_a, register_b),
            Op::OrImm(register, value) => vm.or_imm(register, value),
            Op::Xor(register_a, register_b) => vm.xor(register_a, register_b),
            Op::XorImm(register, value) => vm.xor_imm(register, value),
            Op::Not(register) => vm.not(register),
            Op::Shr(register_a, register_b) => vm.shr(register_a, register_b),
            Op::ShrImm(register, value) => vm.shr_imm(register, value),
            Op::Shl(register_a, register_b) => vm.shl(register_a, register_b),
            Op::ShlImm(register, value) => vm.shl_imm(register, value),
            Op::Eq(register_a, register_b) => vm.eq(register_a, register_b),
            Op::EqImm(register, value) => vm.eq_imm(register, value),
            Op::Gt(register_a, register_b) => vm.gt(register_a, register_b),
            Op::GtImm(register, value) => vm.gt_imm(register, value),
            Op::Ge(register_a, register_b) => vm.ge(register_a, register_b),
            Op::GeImm(register, value) => vm.ge_imm(register, value),
            Op::Lt(register_a, register_b) => vm.lt(register_a, register_b),
            Op::LtImm(register, value) => vm.lt_imm(register, value),
            Op::Le(register_a, register_b) => vm.le(register_a, register_b),
            Op::LeImm(register, value) => vm.le_imm(register, value),
            Op::Jump(register) => vm.jump(register),
            Op::JumpImm(address) => vm.jump_imm(address),
            Op::JumpCond(condition_register, address_register) => {
                vm.jump_cond(condition_register, address_register)
            }
            Op::JumpCondImm(register, address) => vm.jump_cond_imm(register, address),
            Op::Call(register) => vm.call(register),
            Op::CallImm(symbol) => vm.call_imm(symbol),
            Op::CallNative(index) => vm.call_native(index),
            Op::Ret => vm.ret(),
            Op::Init(register) => vm.init_object(register),
            Op::Index(register) => vm.index_object(register),
            Op::DbgReg(register) => vm.dbg_reg(register),
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
    pub fn new(kind: VmErrorKind, current_frame: Option<CallFrame>) -> Self {
        Self {
            kind,
            current_frame,
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
