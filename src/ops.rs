use crate::{
    program::Program,
    value::Value,
    vm::{Register, Vm},
};

use std::{ops::Index, slice::SliceIndex};

pub type OpResult<'a> = Result<Transition<'a>, OpError>;

/// Opcodes representing the virtual machine's instruction set.
///
/// For operations which move values into and out of memory or registers, the operands are in the
/// order (src, dst). For binary operations, the operands are ordered `opcode.0 <operation>
/// opcode.1`.
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
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
    /// Call a native function determined by the immediate index pointing to a Program's string
    /// pool.
    CallNative(usize),
    /// Set the VM's current frame to the call frame popped from the call stack.
    Ret,

    /// Print out the value in the register to stderr.
    DbgReg(Register),
    /// Print out the value at a memory location to stderr.
    DbgMem(usize),
}

impl OpCode {
    pub fn execute<'a>(self, vm: &'a mut Vm) -> OpResult<'a> {
        match self {
            OpCode::NoOp => vm.noop(),
            OpCode::Halt => vm.halt(),
            OpCode::PushImm(value) => vm.push_imm(value),
            OpCode::Push(register) => vm.push(register),
            OpCode::Pop(register) => vm.pop(register),
            OpCode::LoadReg(register_src, register_dst) => vm.load_reg(register_src, register_dst),
            OpCode::LoadImm(value, register) => vm.load_imm(value, register),
            OpCode::LoadMem(address, register) => vm.load_mem(address, register),
            OpCode::StoreReg(register, address) => vm.store_reg(register, address),
            OpCode::StoreImm(value, address) => vm.store_imm(value, address),
            OpCode::StoreMem(address_src, address_dst) => vm.store_mem(address_src, address_dst),
            OpCode::Add(register_a, register_b) => vm.add(register_a, register_b),
            OpCode::AddImm(register, value) => vm.add_imm(register, value),
            OpCode::Sub(register_a, register_b) => vm.sub(register_a, register_b),
            OpCode::SubImm(register, value) => vm.sub_imm(register, value),
            OpCode::Mul(register_a, register_b) => vm.mul(register_a, register_b),
            OpCode::MulImm(register, value) => vm.mul_imm(register, value),
            OpCode::Div(register_a, register_b) => vm.div(register_a, register_b),
            OpCode::DivImm(register, value) => vm.div_imm(register, value),
            OpCode::Rem(register_a, register_b) => vm.rem(register_a, register_b),
            OpCode::RemImm(register, value) => vm.rem_imm(register, value),
            OpCode::And(register_a, register_b) => vm.and(register_a, register_b),
            OpCode::AndImm(register, value) => vm.and_imm(register, value),
            OpCode::Or(register_a, register_b) => vm.or(register_a, register_b),
            OpCode::OrImm(register, value) => vm.or_imm(register, value),
            OpCode::Xor(register_a, register_b) => vm.xor(register_a, register_b),
            OpCode::XorImm(register, value) => vm.xor_imm(register, value),
            OpCode::Not(register) => vm.not(register),
            OpCode::Shr(register_a, register_b) => vm.shr(register_a, register_b),
            OpCode::ShrImm(register, value) => vm.shr_imm(register, value),
            OpCode::Shl(register_a, register_b) => vm.shl(register_a, register_b),
            OpCode::ShlImm(register, value) => vm.shl_imm(register, value),
            OpCode::Eq(register_a, register_b) => vm.eq(register_a, register_b),
            OpCode::EqImm(register, value) => vm.eq_imm(register, value),
            OpCode::Gt(register_a, register_b) => vm.gt(register_a, register_b),
            OpCode::GtImm(register, value) => vm.gt_imm(register, value),
            OpCode::Ge(register_a, register_b) => vm.ge(register_a, register_b),
            OpCode::GeImm(register, value) => vm.ge_imm(register, value),
            OpCode::Lt(register_a, register_b) => vm.lt(register_a, register_b),
            OpCode::LtImm(register, value) => vm.lt_imm(register, value),
            OpCode::Le(register_a, register_b) => vm.le(register_a, register_b),
            OpCode::LeImm(register, value) => vm.le_imm(register, value),
            OpCode::Jump(register) => vm.jump(register),
            OpCode::JumpImm(address) => vm.jump_imm(address),
            OpCode::JumpCond(condition_register, address_register) => {
                vm.jump_cond(condition_register, address_register)
            }
            OpCode::JumpCondImm(register, address) => vm.jump_cond_imm(register, address),
            OpCode::Call(register) => vm.call(register),
            OpCode::CallNative(index) => vm.call_native(index),
            OpCode::Ret => vm.ret(),
            OpCode::DbgReg(register) => vm.dbg_reg(register),
            OpCode::DbgMem(address) => vm.dbg_mem(address),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Transition<'a> {
    Continue,
    Jump(usize),
    Call(&'a str),
    Ret,
    Halt,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum OpError {
    Type,
    Arithmetic,
    StackUnderflow,
    OutOfMemory,
    InvalidAddress,
    FunctionNotFound,
    SymbolNotFound,
}

#[derive(Clone, PartialEq, PartialOrd, Debug, Default)]
pub struct Function(pub(crate) Box<[OpCode]>);

impl Function {
    pub fn new(ops: impl IntoIterator<Item = OpCode>) -> Self {
        let ops = ops.into_iter().collect::<Vec<_>>().into_boxed_slice();

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
