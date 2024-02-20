use serde::{Deserialize, Serialize};

use crate::{
    string::Symbol,
    value::Value,
    vm::{CallFrame, Vm},
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
    Push(Value),
    /// Pop the value at the top of the stack into a register
    Pop,

    /// Load a local variable onto the top of the stack.
    Load(usize),

    /// Stores the value at the top of the stack into a local variable.
    Store(usize),

    /// Copies the valuea the top of the stack and pushes it to the stack.
    Copy,

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

    /// Print out the value at a memory location relative to the top of the stack to stderr.
    Dbg(usize),
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
            Op::Dbg(address) => vm.op_dbg(address),
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
    use super::{OpResult, Transition, VmError, VmErrorKind};
    use crate::utils::VmResult;
    use crate::value::Value;
    use crate::vm::Vm;
    use crate::{string::Symbol, vm::CallFrame};
    use std::mem;
    use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub};
    

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
                (Value::Object(a), Value::Object(b)) => {
                    $self.push_value(Value::Bool($op(&a, &b)))?;
                }
                _ => return Err(VmError::new(VmErrorKind::Type, &$self.frame)),
            }

            Ok(Transition::Continue)
        }};
    }

    impl Vm {
        // nop
        #[inline]
        pub(super) fn op_nop(&self) -> OpResult {
            Ok(Transition::Continue)
        }

        // halt
        #[inline]
        pub(super) fn op_halt(&self) -> OpResult {
            Ok(Transition::Halt)
        }

        // push
        #[inline]
        pub(super) fn op_push(&mut self, value: Value) -> OpResult {
            self.push_value(value)?;
            Ok(Transition::Continue)
        }

        // pop
        #[inline]
        pub(super) fn op_pop(&mut self) -> OpResult {
            let _ = self.pop_value()?;

            Ok(Transition::Continue)
        }

        pub(super) fn op_load(&mut self, index: usize) -> OpResult {
            let value = self.locals().get(index).copied().unwrap_or(Value::Null);

            self.push_value(value)?;

            Ok(Transition::Continue)
        }

        #[inline]
        pub(super) fn op_store(&mut self, index: usize) -> OpResult {
            let value = self.pop_value()?;

            let locals = self.locals_mut();

            if index > locals.len() {
                locals.resize(index + 1, Value::Null);
            }

            locals[index] = value;

            Ok(Transition::Continue)
        }

        #[inline]
        pub(super) fn op_copy(&mut self) -> OpResult {
            let value = self.get_value(0)?;
            self.push_value(value)?;

            Ok(Transition::Continue)
        }

        // add
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
        pub(super) fn op_and(&mut self) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                op = BitAnd::bitand,
            }
        }

        // andi
        #[inline]
        pub(super) fn op_and_imm(&mut self, value: Value) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                op = BitAnd::bitand,
            }
        }

        // or
        #[inline]
        pub(super) fn op_or(&mut self) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                op = BitOr::bitor,
            }
        }

        // ori
        #[inline]
        pub(super) fn op_or_imm(&mut self, value: Value) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                op = BitOr::bitor,
            }
        }

        // xor
        #[inline]
        pub(super) fn op_xor(&mut self) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                op = BitXor::bitxor,
            }
        }

        // xori
        #[inline]
        pub(super) fn op_xor_imm(&mut self, value: Value) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                op = BitXor::bitxor,
            }
        }

        // not
        #[inline]
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
        #[inline]
        pub(super) fn op_shr(&mut self) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                op = checked_shr,
            }
        }

        // shri
        #[inline]
        pub(super) fn op_shr_imm(&mut self, value: Value) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                op = checked_shr,
            }
        }

        // shl
        #[inline]
        pub(super) fn op_shl(&mut self) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.pop_value()?,
                b = self.pop_value()?,
                op = checked_shl,
            }
        }

        // shli
        #[inline]
        pub(super) fn op_shl_imm(&mut self, value: Value) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.pop_value()?,
                b = value,
                op = checked_shl,
            }
        }

        // eq
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
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
        #[inline]
        pub(super) fn op_jump(&mut self) -> OpResult {
            let address = self.pop_address()?;

            self.op_jump_imm(address)
        }

        // jmpi
        #[inline]
        pub(super) fn op_jump_imm(&mut self, address: usize) -> OpResult {
            self.frame.ip = address;

            Ok(Transition::Jump)
        }

        // jmpc
        #[inline]
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
        #[inline]
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
        #[inline]
        pub(super) fn op_call(&mut self) -> OpResult {
            let symbol = self.pop_symbol()?;

            self.op_call_imm(symbol)
        }

        // calli
        #[inline]
        pub(super) fn op_call_imm(&mut self, symbol: Symbol) -> OpResult {
            let called_func = self.resolve_function(symbol)?;

            let caller = mem::replace(&mut self.frame, CallFrame::new(called_func, 0));

            self.push_frame(caller)?;

            Ok(Transition::Jump)
        }

        #[inline]
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
        #[inline]
        pub(super) fn op_ret(&mut self) -> OpResult {
            self.frame = self.pop_frame()?;

            Ok(Transition::Continue)
        }

        // // DebugRegister
        // #[inline]
        // pub(super) fn op_dbg_reg(&self, register: Register) -> OpResult {
        //     let value = self.registers[register];

        //     eprintln!("Register {register:?}: {value:?}");
        //     Ok(Transition::Continue)
        // }

        // dbg
        #[inline]
        pub(super) fn op_dbg(&self, index: usize) -> OpResult {
            let value = self.get_value(index).ok();

            eprintln!("*(sp - {index:#x}): {value:?}");
            Ok(Transition::Continue)
        }

        // trap
        pub(super) fn op_trap(&self) -> OpResult {
            Ok(Transition::Continue)
        }
    }
}
