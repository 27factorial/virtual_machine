use crate::string::Symbol;
use crate::value::Value;
use crate::vm::{Result as VmResult, Vm};
use serde::{Deserialize, Serialize};

use std::{ops::Index, slice::SliceIndex, sync::Arc};

pub type OpResult = VmResult<Transition>;

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
    Push(Value),
    /// Pop the value at the top of the stack into a register
    Pop,

    /// Load a local variable onto the top of the stack.
    Load(usize),

    /// Stores the value at the top of the stack into a local variable.
    Store(usize),

    /// Copies the valuea the top of the stack and pushes it to the stack.
    Copy,

    /// Pops an integer from the stack representing the amount of local variables, and reserves the
    /// that many values at the from the top of the stack as locals for the current function.
    Reserve,
    /// Reserve the nth first elements on the stack for use as locals for the current function.
    ReserveImm(usize),

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

    CallImm(Symbol),
    /// Call a native function determined by the immediate index pointing to a Program's string
    /// pool.
    CallNative(Symbol),
    /// Set the VM's current frame to the call frame popped from the call stack.
    Ret,

    CastUint,
    CastSint,
    CastFloat,
    CastBool,

    /// Print out the value at a memory location relative to the top of the stack to stderr.
    Dbg(usize),

    /// Print out the debug representation of the VM.
    DbgVm,
}

impl OpCode {
    pub fn execute(self, vm: &mut Vm) -> OpResult {
        use OpCode as Op;

        match self {
            Op::NoOp => vm.op_nop(),
            Op::Halt => vm.op_halt(),
            Op::Push(value) => vm.op_push(value),
            Op::Pop => vm.op_pop(),
            Op::Load(index) => vm.op_load(index),
            Op::Store(index) => vm.op_store(index),
            Op::Copy => vm.op_copy(),
            Op::Reserve => vm.op_reserve(),
            Op::ReserveImm(n) => vm.op_reserve_imm(n),
            Op::Add => vm.op_add(),
            Op::AddImm(value) => vm.op_add_imm(value),
            Op::Sub => vm.op_sub(),
            Op::SubImm(value) => vm.op_sub_imm(value),
            Op::Mul => vm.op_mul(),
            Op::MulImm(value) => vm.op_mul_imm(value),
            Op::Div => vm.op_div(),
            Op::DivImm(value) => vm.op_div_imm(value),
            Op::Rem => vm.op_rem(),
            Op::RemImm(value) => vm.op_rem_imm(value),
            Op::And => vm.op_and(),
            Op::AndImm(value) => vm.op_and_imm(value),
            Op::Or => vm.op_or(),
            Op::OrImm(value) => vm.op_or_imm(value),
            Op::Xor => vm.op_xor(),
            Op::XorImm(value) => vm.op_xor_imm(value),
            Op::Not => vm.op_not(),
            Op::Shr => vm.op_shr(),
            Op::ShrImm(value) => vm.op_shr_imm(value),
            Op::Shl => vm.op_shl(),
            Op::ShlImm(value) => vm.op_shl_imm(value),
            Op::Eq => vm.op_eq(),
            Op::EqImm(value) => vm.op_eq_imm(value),
            Op::Gt => vm.op_gt(),
            Op::GtImm(value) => vm.op_gt_imm(value),
            Op::Ge => vm.op_ge(),
            Op::GeImm(value) => vm.op_ge_imm(value),
            Op::Lt => vm.op_lt(),
            Op::LtImm(value) => vm.op_lt_imm(value),
            Op::Le => vm.op_le(),
            Op::LeImm(value) => vm.op_le_imm(value),
            Op::Jump => vm.op_jump(),
            Op::JumpImm(address) => vm.op_jump_imm(address),
            Op::JumpCond => vm.op_jump_cond(),
            Op::JumpCondImm(address) => vm.op_jump_cond_imm(address),
            Op::Call => vm.op_call(),
            Op::CallImm(symbol) => vm.op_call_imm(symbol),
            Op::CallNative(index) => vm.op_call_native(index),
            Op::Ret => vm.op_ret(),
            Op::CastUint => vm.op_cast_uint(),
            Op::CastSint => vm.op_cast_sint(),
            Op::CastFloat => vm.op_cast_float(),
            Op::CastBool => vm.op_cast_bool(),
            Op::Dbg(address) => vm.op_dbg(address),
            Op::DbgVm => vm.op_dbg_vm(),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Transition {
    Continue,
    Jump,
    Halt,
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

mod imp {
    use super::{OpResult, Transition};
    use crate::utils::IntoVmResult;
    use crate::value::Value;
    use crate::vm::{Vm, VmError, VmErrorKind};
    use crate::{string::Symbol, vm::CallFrame};
    use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub};
    use std::{mem, ptr};

    macro_rules! bin_arithmetic {
        (
        vm = $self:ident,
        a = $a:expr,
        b = $b:expr,
        int_op = $int_op:ident,
        float_op = $float_op:ident $(,)?
    ) => {{
            match ($a, $b) {
                (Value::UInt(a), Value::UInt(b)) => {
                    let value =
                        Value::UInt(u64::$int_op(a, b).vm_err(VmErrorKind::Arithmetic, &$self)?);

                    $self.push_value(value)?;
                }
                (Value::SInt(a), Value::SInt(b)) => {
                    let value =
                        Value::SInt(i64::$int_op(a, b).vm_err(VmErrorKind::Arithmetic, &$self)?);

                    $self.push_value(value)?;
                }
                (Value::Float(a), Value::Float(b)) => {
                    $self.push_value(Value::Float(f64::$float_op(a, b)))?;
                }
                (Value::Address(a), Value::Address(b)) => {
                    let value = Value::Address(
                        usize::$int_op(a, b).vm_err(VmErrorKind::Arithmetic, &$self)?,
                    );

                    $self.push_value(value)?;
                }
                _ => return Err(VmError::new(VmErrorKind::Type, &$self.frame)),
            }

            Ok(Transition::Continue)
        }};
    }

    macro_rules! bin_bitwise {
        (
        vm = $self:ident,
        a = $a:expr,
        b = $b:expr,
        op = $op:path,$(,)?
    ) => {{
            match ($a, $b) {
                (Value::UInt(a), Value::UInt(b)) => {
                    $self.push_value(Value::UInt($op(a, b)))?;
                }
                (Value::SInt(a), Value::SInt(b)) => {
                    $self.push_value(Value::SInt($op(a, b)))?;
                }
                (Value::Bool(a), Value::Bool(b)) => {
                    $self.push_value(Value::Bool($op(a, b)))?;
                }
                (Value::Address(a), Value::Address(b)) => {
                    $self.push_value(Value::Address($op(a, b)))?;
                }
                _ => return Err($self.error(VmErrorKind::Type)),
            }

            Ok(Transition::Continue)
        }};
    }

    macro_rules! bin_shift {
        (
        vm = $self:ident,
        a = $a:expr,
        b = $b:expr,
        op = $op:ident $(,)?
    ) => {{
            match ($a, $b) {
                (Value::UInt(a), Value::UInt(b)) => {
                    let b = u32::try_from(b).vm_err(VmErrorKind::Arithmetic, &$self)?;

                    let value =
                        Value::UInt(u64::$op(a, b).vm_err(VmErrorKind::Arithmetic, &$self)?);

                    $self.push_value(value)?;
                }
                (Value::UInt(a), Value::SInt(b)) => {
                    let b = u32::try_from(b).vm_err(VmErrorKind::Arithmetic, &$self)?;

                    let value =
                        Value::UInt(u64::$op(a, b).vm_err(VmErrorKind::Arithmetic, &$self)?);

                    $self.push_value(value)?;
                }
                (Value::SInt(a), Value::SInt(b)) => {
                    let b = u32::try_from(b).vm_err(VmErrorKind::Arithmetic, &$self)?;

                    let value =
                        Value::SInt(i64::$op(a, b).vm_err(VmErrorKind::Arithmetic, &$self)?);

                    $self.push_value(value)?;
                }
                (Value::SInt(a), Value::UInt(b)) => {
                    let b = u32::try_from(b).vm_err(VmErrorKind::Arithmetic, &$self)?;

                    let value =
                        Value::SInt(i64::$op(a, b).vm_err(VmErrorKind::Arithmetic, &$self)?);

                    $self.push_value(value)?;
                }
                (Value::Address(a), Value::Address(b)) => {
                    let b = u32::try_from(b).vm_err(VmErrorKind::Arithmetic, &$self)?;

                    let value =
                        Value::Address(usize::$op(a, b).vm_err(VmErrorKind::Arithmetic, &$self)?);

                    $self.push_value(value)?;
                }
                (Value::Address(a), Value::UInt(b)) => {
                    let b = u32::try_from(b).vm_err(VmErrorKind::Arithmetic, &$self)?;

                    let value =
                        Value::Address(usize::$op(a, b).vm_err(VmErrorKind::Arithmetic, &$self)?);

                    $self.push_value(value)?;
                }
                (Value::Address(a), Value::SInt(b)) => {
                    let b = u32::try_from(b).vm_err(VmErrorKind::Arithmetic, &$self)?;

                    let value =
                        Value::Address(usize::$op(a, b).vm_err(VmErrorKind::Arithmetic, &$self)?);

                    $self.push_value(value)?;
                }
                _ => return Err(VmError::new(VmErrorKind::Type, &$self.frame)),
            }

            Ok(Transition::Continue)
        }};
    }

    macro_rules! bin_compare {
        (
        vm = $self:ident,
        a = $a:expr,
        b = $b:expr,
        null = $null:literal,
        op = $op:path $(,)?
    ) => {{
            match ($a, $b) {
                (Value::Null, Value::Null) => {
                    $self.push_value(Value::Bool($null))?;
                }
                (Value::UInt(a), Value::UInt(b)) => {
                    $self.push_value(Value::Bool($op(&a, &b)))?;
                }
                (Value::SInt(a), Value::SInt(b)) => {
                    $self.push_value(Value::Bool($op(&a, &b)))?;
                }
                (Value::Float(a), Value::Float(b)) => {
                    $self.push_value(Value::Bool($op(&a, &b)))?;
                }
                (Value::Bool(a), Value::Bool(b)) => {
                    $self.push_value(Value::Bool($op(&a, &b)))?;
                }
                (Value::Char(a), Value::Char(b)) => {
                    $self.push_value(Value::Bool($op(&a, &b)))?;
                }
                (Value::Address(a), Value::Address(b)) => {
                    $self.push_value(Value::Bool($op(&a, &b)))?;
                }
                (Value::Reference(a), Value::Reference(b)) => {
                    $self.push_value(Value::Bool($op(&a, &b)))?;
                }
                _ => return Err(VmError::new(VmErrorKind::Type, &$self.frame)),
            }

            Ok(Transition::Continue)
        }};
    }

    impl Vm {
        // nop
        pub(super) fn op_nop(&self) -> OpResult {
            Ok(Transition::Continue)
        }

        // halt
        pub(super) fn op_halt(&self) -> OpResult {
            Ok(Transition::Halt)
        }

        // push
        pub(super) fn op_push(&mut self, value: Value) -> OpResult {
            self.push_value(value)?;
            Ok(Transition::Continue)
        }

        // pop
        pub(super) fn op_pop(&mut self) -> OpResult {
            let _ = self.pop_value()?;

            Ok(Transition::Continue)
        }

        pub(super) fn op_load(&mut self, index: usize) -> OpResult {
            let value = self.get_local(index)?;

            self.push_value(value)?;

            Ok(Transition::Continue)
        }

        pub(super) fn op_store(&mut self, index: usize) -> OpResult {
            let value = self.pop_value()?;

            self.set_local(index, value)?;

            Ok(Transition::Continue)
        }

        pub(super) fn op_copy(&mut self) -> OpResult {
            let value = self.get_value(0)?;
            self.push_value(value)?;

            Ok(Transition::Continue)
        }

        // rsrv
        pub(super) fn op_reserve(&mut self) -> OpResult {
            let n: usize = match self.pop_value()? {
                Value::UInt(n) => n.try_into().vm_err(VmErrorKind::OutOfBounds, self)?,
                Value::SInt(n) => n.try_into().vm_err(VmErrorKind::OutOfBounds, self)?,
                _ => return Err(self.error(VmErrorKind::Type)),
            };

            self.op_reserve_imm(n)
        }

        // rsrvi
        pub(super) fn op_reserve_imm(&mut self, n: usize) -> OpResult {
            if self.data_stack.len() >= self.frame.stack_base + n {
                self.frame.locals = n;
                Ok(Transition::Continue)
            } else {
                Err(self.error(VmErrorKind::OutOfBounds))
            }
        }

        // add
        pub(super) fn op_add(&mut self) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                int_op = checked_add,
                float_op = add,
            }
        }

        // addi
        pub(super) fn op_add_imm(&mut self, value: Value) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                int_op = checked_add,
                float_op = add,
            }
        }

        // sub
        pub(super) fn op_sub(&mut self) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                int_op = checked_sub,
                float_op = sub,
            }
        }

        // subi
        pub(super) fn op_sub_imm(&mut self, value: Value) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                int_op = checked_sub,
                float_op = sub,
            }
        }

        // mul
        pub(super) fn op_mul(&mut self) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                int_op = checked_mul,
                float_op = mul,
            }
        }

        // muli
        pub(super) fn op_mul_imm(&mut self, value: Value) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                int_op = checked_mul,
                float_op = mul,
            }
        }

        // div
        pub(super) fn op_div(&mut self) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                int_op = checked_div,
                float_op = div,
            }
        }

        // divi
        pub(super) fn op_div_imm(&mut self, value: Value) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                int_op = checked_div,
                float_op = div,
            }
        }

        // rem
        pub(super) fn op_rem(&mut self) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                int_op = checked_rem,
                float_op = rem,
            }
        }

        // remi
        pub(super) fn op_rem_imm(&mut self, value: Value) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                int_op = checked_rem,
                float_op = rem,
            }
        }

        // and
        pub(super) fn op_and(&mut self) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                op = BitAnd::bitand,
            }
        }

        // andi
        pub(super) fn op_and_imm(&mut self, value: Value) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                op = BitAnd::bitand,
            }
        }

        // or
        pub(super) fn op_or(&mut self) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                op = BitOr::bitor,
            }
        }

        // ori
        pub(super) fn op_or_imm(&mut self, value: Value) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                op = BitOr::bitor,
            }
        }

        // xor
        pub(super) fn op_xor(&mut self) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                op = BitXor::bitxor,
            }
        }

        // xori
        pub(super) fn op_xor_imm(&mut self, value: Value) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                op = BitXor::bitxor,
            }
        }

        // not
        pub(super) fn op_not(&mut self) -> OpResult {
            let value = self.pop_value()?;

            match value {
                Value::UInt(val) => {
                    self.push_value(Value::UInt(!val))?;
                }
                Value::SInt(val) => {
                    self.push_value(Value::SInt(!val))?;
                }
                Value::Bool(val) => {
                    self.push_value(Value::Bool(!val))?;
                }
                Value::Address(val) => {
                    self.push_value(Value::Address(!val))?;
                }
                _ => return Err(self.error(VmErrorKind::Type)),
            }

            Ok(Transition::Continue)
        }

        // shr
        pub(super) fn op_shr(&mut self) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                op = checked_shr,
            }
        }

        // shri
        pub(super) fn op_shr_imm(&mut self, value: Value) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                op = checked_shr,
            }
        }

        // shl
        pub(super) fn op_shl(&mut self) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                op = checked_shl,
            }
        }

        // shli
        pub(super) fn op_shl_imm(&mut self, value: Value) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                op = checked_shl,
            }
        }

        // eq
        pub(super) fn op_eq(&mut self) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                null = true,
                op = PartialEq::eq,
            }
        }

        // eqi
        pub(super) fn op_eq_imm(&mut self, value: Value) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                null = true,
                op = PartialEq::eq,
            }
        }

        // gt
        pub(super) fn op_gt(&mut self) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                null = false,
                op = PartialOrd::gt,
            }
        }

        // gti
        pub(super) fn op_gt_imm(&mut self, value: Value) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                null = false,
                op = PartialOrd::gt,
            }
        }

        // ge
        pub(super) fn op_ge(&mut self) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                null = true,
                op = PartialOrd::ge,
            }
        }

        // gei
        pub(super) fn op_ge_imm(&mut self, value: Value) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                null = true,
                op = PartialOrd::ge,
            }
        }

        // lt
        pub(super) fn op_lt(&mut self) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                null = false,
                op = PartialOrd::lt,
            }
        }

        // lti
        pub(super) fn op_lt_imm(&mut self, value: Value) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                null = false,
                op = PartialOrd::lt,
            }
        }

        // le
        pub(super) fn op_le(&mut self) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                null = true,
                op = PartialOrd::le,
            }
        }

        // lei
        pub(super) fn op_le_imm(&mut self, value: Value) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                null = true,
                op = PartialOrd::le,
            }
        }

        // jmp
        pub(super) fn op_jump(&mut self) -> OpResult {
            let address = self.pop_address()?;

            self.op_jump_imm(address)
        }

        // jmpi
        pub(super) fn op_jump_imm(&mut self, address: usize) -> OpResult {
            self.frame.ip = address;

            Ok(Transition::Jump)
        }

        // jmpc
        pub(super) fn op_jump_cond(&mut self) -> OpResult {
            if self.pop_bool()? {
                self.op_jump()
            } else {
                Ok(Transition::Continue)
            }

            // match self.get_value(0)? {
            //     Value::Bool(true) => self.op_jump(),
            //     Value::Bool(false) => Ok(Transition::Continue),
            //     _ => Err(self.error(VmErrorKind::Type)),
            // }
        }

        // jmpci
        pub(super) fn op_jump_cond_imm(&mut self, address: usize) -> OpResult {
            if self.pop_bool()? {
                self.op_jump_imm(address)
            } else {
                Ok(Transition::Continue)
            }

            // match self.get_value(0)? {
            //     Value::Bool(true) => self.op_jump_imm(address),
            //     Value::Bool(false) => Ok(Transition::Continue),
            //     _ => Err(self.error(VmErrorKind::Type)),
            // }
        }

        // call
        pub(super) fn op_call(&mut self) -> OpResult {
            let symbol = self.pop_symbol()?;

            self.op_call_imm(symbol)
        }

        // calli
        pub(super) fn op_call_imm(&mut self, symbol: Symbol) -> OpResult {
            let called_func = self.resolve_function(symbol)?;
            let new_base = self.frame.stack_base + self.frame.locals;

            let caller = mem::replace(&mut self.frame, CallFrame::new(called_func, 0, new_base, 0));

            self.push_frame(caller)?;

            Ok(Transition::Jump)
        }

        // calln
        pub(super) fn op_call_native(&mut self, symbol: Symbol) -> OpResult {
            let native = self.resolve_native_function(symbol)?;

            let value = native(self)?;

            if !value.is_null() {
                self.push_value(value)?;
            }

            Ok(Transition::Continue)
        }

        // ret
        pub(super) fn op_ret(&mut self) -> OpResult {
            let new_frame = self.pop_frame()?;
            self.frame = new_frame;

            Ok(Transition::Continue)
        }

        pub(super) fn op_cast_uint(&mut self) -> OpResult {
            let cast = match self.pop_value()? {
                Value::UInt(v) => Value::UInt(v),
                Value::SInt(v) => Value::UInt(v as u64),
                Value::Float(v) => Value::UInt(v as u64),
                Value::Bool(v) => Value::UInt(v as u64),
                Value::Char(v) => Value::UInt(v as u64),
                _ => return Err(self.error(VmErrorKind::Type)),
            };

            self.push_value(cast)?;

            Ok(Transition::Continue)
        }

        pub(super) fn op_cast_sint(&mut self) -> OpResult {
            let cast = match self.pop_value()? {
                Value::UInt(v) => Value::SInt(v as i64),
                Value::SInt(v) => Value::SInt(v),
                Value::Float(v) => Value::SInt(v as i64),
                Value::Bool(v) => Value::SInt(v as i64),
                Value::Char(v) => Value::SInt(v as i64),
                _ => return Err(self.error(VmErrorKind::Type)),
            };

            self.push_value(cast)?;

            Ok(Transition::Continue)
        }

        pub(super) fn op_cast_float(&mut self) -> OpResult {
            let cast = match self.pop_value()? {
                Value::UInt(v) => Value::Float(v as f64),
                Value::SInt(v) => Value::Float(v as f64),
                Value::Float(v) => Value::Float(v),
                Value::Bool(v) => Value::Float(v as u64 as f64),
                Value::Char(v) => Value::Float(v as u64 as f64),
                _ => return Err(self.error(VmErrorKind::Type)),
            };

            self.push_value(cast)?;

            Ok(Transition::Continue)
        }

        pub(super) fn op_cast_bool(&mut self) -> OpResult {
            let cast = match self.pop_value()? {
                Value::UInt(v) => Value::Bool(v != 0),
                Value::SInt(v) => Value::Bool(v != 0),
                Value::Float(v) => Value::Bool(v != 0.0),
                Value::Bool(v) => Value::Bool(v),
                Value::Char(v) => Value::Bool(v != '\0'),
                _ => return Err(self.error(VmErrorKind::Type)),
            };

            self.push_value(cast)?;

            Ok(Transition::Continue)
        }

        // dbg
        pub(super) fn op_dbg(&self, index: usize) -> OpResult {
            let value = self.get_value(index).unwrap_or(Value::Null);

            match value {
                reference @ Value::Reference(v) => {
                    eprint!("stack[{index:#x}]: {reference:?} => ");

                    let obj_debug = self.heap.get(v).map(|obj| obj.as_debug());

                    match obj_debug {
                        Some(debug) => {
                            let address = ptr::from_ref(debug).addr();

                            eprintln!("{debug:#?} @ {address:#x}")
                        }
                        None => eprintln!("<invalid>"),
                    }
                }
                value => eprintln!("*(sp - {index:#x}): {value:?}"),
            }
            Ok(Transition::Continue)
        }

        // dbgvm
        pub(super) fn op_dbg_vm(&self) -> OpResult {
            eprintln!("{self:#?}");

            Ok(Transition::Continue)
        }
    }
}
