use crate::symbol::Symbol;
use crate::value::Value;
use crate::vm::{Result as VmResult, Vm, VmPanic};
use serde::{Deserialize, Serialize};

use super::function::Function;
use super::CallFrame;

pub type OpResult = VmResult<Transition>;

/// Opcodes representing the virtual machine's instruction set.
///
/// For operations which move values into and out of memory or registers, the operands are in the
/// order (src, dst). For binary operations, the operands are ordered `opcode.0 <operation>
/// opcode.1`.
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug, Serialize, Deserialize)]
pub enum OpCode {
    /// No operation; do nothing.
    NoOp,
    /// Halt the virtual machine immediately.
    Halt,

    /// Push an immediate value to the top of the stack.
    /// Corresponds to the PushImmediate instruction.
    Push(Value),

    PushConst(usize),
    /// Pop the value at the top of the stack into a register
    Pop,

    /// Load a local variable onto the top of the stack.
    Load(usize),

    /// Stores the value at the top of the stack into a local variable.
    Store(usize),

    /// Copies the valuea the top of the stack and pushes it to the stack.
    Dup,

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
    JumpImm(isize),
    /// If the value in the first register is a boolean value of `true`, jump to the index in the
    /// second register within the current function.
    JumpCond,
    /// If the value in the register is a boolean value of `true`, jump to the immediate index
    /// within the current function.
    JumpCondImm(isize),

    /// Jump to the first instruction of a function determined by the index in the register.
    Call,

    CallImm(Function),

    CallBuiltin(usize),
    /// Call a native function determined by the immediate index pointing to a Module's string
    /// pool.
    CallNative(Symbol),
    /// Set the VM's current frame to the call frame popped from the call stack.
    Ret,

    CastInt,
    CastFloat,
    CastBool,

    /// Print out the value at a memory location relative to the top of the stack to stderr.
    Dbg(usize),

    /// Print out the debug representation of the VM.
    DbgVm,
}

impl OpCode {
    pub fn execute(self, vm: &mut Vm, frame: &mut CallFrame) -> OpResult {
        use OpCode as Op;

        match self {
            Op::NoOp => vm.op_nop(),
            Op::Halt => vm.op_halt(),
            Op::Push(value) => vm.op_push(value, frame),
            Op::PushConst(index) => vm.op_push_const(index, frame),
            Op::Pop => vm.op_pop(frame),
            Op::Load(index) => vm.op_load(index, frame),
            Op::Store(index) => vm.op_store(index, frame),
            Op::Dup => vm.op_dup(frame),
            Op::Reserve => vm.op_reserve(frame),
            Op::ReserveImm(n) => vm.op_reserve_imm(n, frame),
            Op::Add => vm.op_add(frame),
            Op::AddImm(value) => vm.op_add_imm(value, frame),
            Op::Sub => vm.op_sub(frame),
            Op::SubImm(value) => vm.op_sub_imm(value, frame),
            Op::Mul => vm.op_mul(frame),
            Op::MulImm(value) => vm.op_mul_imm(value, frame),
            Op::Div => vm.op_div(frame),
            Op::DivImm(value) => vm.op_div_imm(value, frame),
            Op::Rem => vm.op_rem(frame),
            Op::RemImm(value) => vm.op_rem_imm(value, frame),
            Op::And => vm.op_and(frame),
            Op::AndImm(value) => vm.op_and_imm(value, frame),
            Op::Or => vm.op_or(frame),
            Op::OrImm(value) => vm.op_or_imm(value, frame),
            Op::Xor => vm.op_xor(frame),
            Op::XorImm(value) => vm.op_xor_imm(value, frame),
            Op::Not => vm.op_not(frame),
            Op::Shr => vm.op_shr(frame),
            Op::ShrImm(value) => vm.op_shr_imm(value, frame),
            Op::Shl => vm.op_shl(frame),
            Op::ShlImm(value) => vm.op_shl_imm(value, frame),
            Op::Eq => vm.op_eq(frame),
            Op::EqImm(value) => vm.op_eq_imm(value, frame),
            Op::Gt => vm.op_gt(frame),
            Op::GtImm(value) => vm.op_gt_imm(value, frame),
            Op::Ge => vm.op_ge(frame),
            Op::GeImm(value) => vm.op_ge_imm(value, frame),
            Op::Lt => vm.op_lt(frame),
            Op::LtImm(value) => vm.op_lt_imm(value, frame),
            Op::Le => vm.op_le(frame),
            Op::LeImm(value) => vm.op_le_imm(value, frame),
            Op::Jump => vm.op_jump(frame),
            Op::JumpImm(address) => vm.op_jump_imm(address, frame),
            Op::JumpCond => vm.op_jump_cond(frame),
            Op::JumpCondImm(address) => vm.op_jump_cond_imm(address, frame),
            Op::Call => vm.op_call(frame),
            Op::CallImm(symbol) => vm.op_call_imm(symbol, frame),
            Op::CallBuiltin(idx) => vm.op_call_builtin(idx, frame),
            Op::CallNative(index) => vm.op_call_native(index, frame),
            Op::Ret => vm.op_ret(frame),
            Op::CastInt => vm.op_cast_int(frame),
            Op::CastFloat => vm.op_cast_float(frame),
            Op::CastBool => vm.op_cast_bool(frame),
            Op::Dbg(address) => vm.op_dbg(address, frame),
            Op::DbgVm => vm.op_dbg_vm(frame),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Transition {
    Continue,
    Jump,
    Return,
    Halt,
}

mod imp {
    use super::{OpResult, Transition};
    use crate::throw;
    use crate::utils::IntoVmResult;
    use crate::value::Value;
    use crate::vm::builtin::BUILTINS;
    use crate::vm::function::Function;
    use crate::vm::{Vm, VmErrorKind};
    use crate::{symbol::Symbol, vm::CallFrame};
    use std::cell::Ref;
    use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub};
    use std::ptr;

    macro_rules! bin_arithmetic {
        // Normal
        (
            vm = $self:ident,
            a = $a:expr,
            b = $b:expr,
            int_op = $int_op:ident,
            float_op = $float_op:ident, 
            frame = $frame:expr $(,)?
        ) => {{
            match ($a, $b) {
                (Value::Int(a), Value::Int(b)) => {
                    let new_value = match i64::$int_op(a, *b) {
                        Some(v) => v,
                        None => throw!(VmErrorKind::Arithmetic, $frame),
                    };
                    *b = new_value;
                }
                (Value::Float(a), Value::Float(b)) => {
                    *b = f64::$float_op(a, *b);
                }
                _ => throw!(VmErrorKind::Type, $frame),
            }

            Ok(Transition::Continue)
        }};

        // Immediate
        (
            vm = $self:ident,
            a = $a:expr,
            imm = $b:expr,
            int_op = $int_op:ident,
            float_op = $float_op:ident,
            frame = $frame:expr $(,)?
        ) => {{
            match ($a, $b) {
                (Value::Int(a), Value::Int(b)) => {
                    let new_value = match i64::$int_op(*a, b) {
                        Some(v) => v,
                        None => throw!(VmErrorKind::Arithmetic, $frame),
                    };
                    *a = new_value;
                }
                (Value::Float(a), Value::Float(b)) => {
                    *a = f64::$float_op(*a, b);
                }
                _ => throw!(VmErrorKind::Type, $frame),
            }

            Ok(Transition::Continue)
        }};
    }

    macro_rules! bin_bitwise {
        (
        vm = $self:ident,
        a = $a:expr,
        b = $b:expr,
        op = $op:path,
        frame = $frame:expr $(,)?
    ) => {{
            match ($a, $b) {
                (Value::Int(a), Value::Int(b)) => {
                    let new_value = $op(a, *b);
                    *b = new_value;
                }
                (Value::Bool(a), Value::Bool(b)) => {
                    let new_value = $op(a, *b);
                    *b = new_value;
                }
                _ => throw!(VmErrorKind::Type, $frame),
            }

            Ok(Transition::Continue)
        }};
        (
            vm = $self:ident,
            a = $a:expr,
            imm = $b:expr,
            op = $op:path,
            frame = $frame:expr $(,)?
        ) => {{
            match ($a, $b) {
                (Value::Int(a), Value::Int(b)) => {
                    let new_value = $op(*a, b);
                    *a = new_value;
                }
                (Value::Bool(a), Value::Bool(b)) => {
                    let new_value = $op(*a, b);
                    *a = new_value;
                }
                _ => throw!(VmErrorKind::Type, $frame),
            }

            Ok(Transition::Continue)
        }};
    }

    macro_rules! bin_shift {
        (
        vm = $self:ident,
        a = $a:expr,
        b = $b:expr,
        op = $op:ident,
        frame = $frame:expr $(,)?
    ) => {{
            // This one has to be different, since `top` might be a different variant than `$a`, so
            // the variant has to be changed out when reassigning to it.
            match ($a, $b) {
                (Value::Int(a), top) => {
                    let operand_res: Result<u32, _> = match top {
                        Value::Int(v) => (*v).try_into(),
                        _ => throw!(VmErrorKind::Type, $frame),
                    };

                    let Ok(b) = operand_res else {
                        throw!(VmErrorKind::Arithmetic, $frame);
                    };

                    let new_value = match i64::$op(a, b) {
                        Some(v) => v,
                        None => throw!(VmErrorKind::Arithmetic, $frame),
                    };

                    *top = Value::Int(new_value);
                }
                _ => throw!(VmErrorKind::Type, $frame),
            }

            Ok(Transition::Continue)
        }};
        (
            vm = $self:ident,
            a = $a:expr,
            imm = $b:expr,
            op = $op:ident,
            frame = $frame:expr $(,)?
        ) => {{
            match ($a, $b) {
                (Value::Int(a), Value::Int(b)) => {
                    let b = match u32::try_from(b) {
                        Ok(v) => v,
                        Err(_) => throw!(VmErrorKind::Arithmetic, $frame),
                    };

                    let new_value = match i64::$op(*a, b) {
                        Some(v) => v,
                        None => throw!(VmErrorKind::Arithmetic, $frame),
                    };

                    *a = new_value;
                }
                _ => throw!(VmErrorKind::Type, $frame),
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
        op = $op:path,
        frame = $frame:expr $(,)?
    ) => {{
            let result = $op(&$a, $b);

            *$b = Value::Bool(result);

            Ok(Transition::Continue)
        }};
        (
            vm = $self:ident,
            a = $a:expr,
            imm = $b:expr,
            null = $null:literal,
            op = $op:path,
            frame = $frame:expr $(,)?
        ) => {{
            let result = $op(&*$a, &$b);

            *$a = Value::Bool(result);

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
        pub(super) fn op_push(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            self.push_value(value, frame)?;
            Ok(Transition::Continue)
        }

        // pushc
        pub(super) fn op_push_const(&mut self, index: usize, frame: &CallFrame) -> OpResult {
            let Some(constant) = self.module.constants.get(index) else {
                throw!(VmErrorKind::OutOfBounds, frame);
            };
    
            self.data_stack
                .push_from_ref(constant)
                .vm_result(VmErrorKind::StackOverflow, frame)?;

            Ok(Transition::Continue)
        }

        // pop
        pub(super) fn op_pop(&mut self, frame: &CallFrame) -> OpResult {
            let _ = self.pop_value(frame)?;

            Ok(Transition::Continue)
        }

        pub(super) fn op_load(&mut self, index: usize, frame: &CallFrame) -> OpResult {
            let base = frame.stack_base;
            let count = frame.locals;

            let valid_range = base..base + count;

            if valid_range.contains(&index) {
                self.data_stack.copy_to_top(index);
                Ok(Transition::Continue)
            } else {
                throw!(VmErrorKind::OutOfBounds, frame);
            }
        }

        pub(super) fn op_store(&mut self, index: usize, frame: &CallFrame) -> OpResult {
            let base = frame.stack_base;
            let count = frame.locals;

            let valid_range = base..base + count;

            if valid_range.contains(&index) {
                self.data_stack.replace_from_top(index);
                Ok(Transition::Continue)
            } else {
                throw!(VmErrorKind::OutOfBounds, frame);
            }
        }

        pub(super) fn op_dup(&mut self, _: &CallFrame) -> OpResult {
            // TODO: Handle error states. This can panic.
            self.data_stack.copy_to_top(self.data_stack.len() - 1);

            Ok(Transition::Continue)
        }

        // rsrv
        pub(super) fn op_reserve(&mut self, frame: &mut CallFrame) -> OpResult {
            let n: usize = match self.pop_value(frame)? {
                Value::Int(n) => n.try_into().vm_result(VmErrorKind::OutOfBounds, &*frame)?,
                _ => throw!(VmErrorKind::Type, &*frame),
            };

            self.op_reserve_imm(n, frame)
        }

        // rsrvi
        pub(super) fn op_reserve_imm(&mut self, n: usize, frame: &mut CallFrame) -> OpResult {
            self.set_reserved(n, frame)?;

            Ok(Transition::Continue)
        }

        // add
        pub(super) fn op_add(&mut self, frame: &CallFrame) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                int_op = checked_add,
                float_op = add,
                frame = frame,
            }
        }

        // addi
        pub(super) fn op_add_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                int_op = checked_add,
                float_op = add,
                frame = frame,
            }
        }

        // sub
        pub(super) fn op_sub(&mut self, frame: &CallFrame) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                int_op = checked_sub,
                float_op = sub,
                frame = frame,
            }
        }

        // subi
        pub(super) fn op_sub_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                int_op = checked_sub,
                float_op = sub,
                frame = frame,
            }
        }

        // mul
        pub(super) fn op_mul(&mut self, frame: &CallFrame) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                int_op = checked_mul,
                float_op = mul,
                frame = frame,
            }
        }

        // muli
        pub(super) fn op_mul_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                int_op = checked_mul,
                float_op = mul,
                frame = frame,
            }
        }

        // div
        pub(super) fn op_div(&mut self, frame: &CallFrame) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                int_op = checked_div,
                float_op = div,
                frame = frame,
            }
        }

        // divi
        pub(super) fn op_div_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                int_op = checked_div,
                float_op = div,
                frame = frame,
            }
        }

        // rem
        pub(super) fn op_rem(&mut self, frame: &CallFrame) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                int_op = checked_rem,
                float_op = rem,
                frame = frame,
            }
        }

        // remi
        pub(super) fn op_rem_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_arithmetic! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                int_op = checked_rem,
                float_op = rem,
                frame = frame,
            }
        }

        // and
        pub(super) fn op_and(&mut self, frame: &CallFrame) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                op = BitAnd::bitand,
                frame = frame,
            }
        }

        // andi
        pub(super) fn op_and_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                op = BitAnd::bitand,
                frame = frame,
            }
        }

        // or
        pub(super) fn op_or(&mut self, frame: &CallFrame) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                op = BitOr::bitor,
                frame = frame,
            }
        }

        // ori
        pub(super) fn op_or_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                op = BitOr::bitor,
                frame = frame,
            }
        }

        // xor
        pub(super) fn op_xor(&mut self, frame: &CallFrame) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                op = BitXor::bitxor,
                frame = frame,
            }
        }

        // xori
        pub(super) fn op_xor_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_bitwise! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                op = BitXor::bitxor,
                frame = frame,
            }
        }

        // not
        pub(super) fn op_not(&mut self, frame: &CallFrame) -> OpResult {
            let value = self.top_value_mut(frame)?;

            match value {
                Value::Int(val) => {
                    *val = !*val;
                }
                Value::Bool(val) => {
                    *val = !*val;
                }
                _ => throw!(VmErrorKind::Type, frame),
            }

            Ok(Transition::Continue)
        }

        // shr
        pub(super) fn op_shr(&mut self, frame: &CallFrame) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                op = checked_shr,
                frame = frame,
            }
        }

        // shri
        pub(super) fn op_shr_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                op = checked_shr,
                frame = frame,
            }
        }

        // shl
        pub(super) fn op_shl(&mut self, frame: &CallFrame) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                op = checked_shl,
                frame = frame,
            }
        }

        // shli
        pub(super) fn op_shl_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_shift! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                op = checked_shl,
                frame = frame,
            }
        }

        // eq
        pub(super) fn op_eq(&mut self, frame: &CallFrame) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                null = true,
                op = PartialEq::eq,
                frame = frame,
            }
        }

        // eqi
        pub(super) fn op_eq_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                null = true,
                op = PartialEq::eq,
                frame = frame,
            }
        }

        // gt
        pub(super) fn op_gt(&mut self, frame: &CallFrame) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                null = false,
                op = PartialOrd::gt,
                frame = frame,
            }
        }

        // gti
        pub(super) fn op_gt_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                null = false,
                op = PartialOrd::gt,
                frame = frame,
            }
        }

        // ge
        pub(super) fn op_ge(&mut self, frame: &CallFrame) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                null = true,
                op = PartialOrd::ge,
                frame = frame,
            }
        }

        // gei
        pub(super) fn op_ge_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                null = true,
                op = PartialOrd::ge,
                frame = frame,
            }
        }

        // lt
        pub(super) fn op_lt(&mut self, frame: &CallFrame) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                null = false,
                op = PartialOrd::lt,
                frame = frame,
            }
        }

        // lti
        pub(super) fn op_lt_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                null = false,
                op = PartialOrd::lt,
                frame = frame,
            }
        }

        // le
        pub(super) fn op_le(&mut self, frame: &CallFrame) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.pop_value(frame)?,
                b = self.top_value_mut(frame)?,
                null = true,
                op = PartialOrd::le,
                frame = frame,
            }
        }

        // lei
        pub(super) fn op_le_imm(&mut self, value: Value, frame: &CallFrame) -> OpResult {
            bin_compare! {
                vm = self,
                a = self.top_value_mut(frame)?,
                imm = value,
                null = true,
                op = PartialOrd::le,
                frame = frame,
            }
        }

        // jmp
        pub(super) fn op_jump(&mut self, frame: &mut CallFrame) -> OpResult {
            let address = self
                .pop_int(frame)?
                .try_into()
                .vm_result(VmErrorKind::OutOfBounds, &*frame)?;

            self.op_jump_imm(address, frame)
        }

        // jmpi
        pub(super) fn op_jump_imm(&mut self, offset: isize, frame: &mut CallFrame) -> OpResult {
            let positive = offset.is_positive();
            let offset: usize = offset
                .checked_abs()
                .vm_result(VmErrorKind::OutOfBounds, &*frame)?
                .try_into()
                .vm_result(VmErrorKind::OutOfBounds, &*frame)?;

            frame.ip = if positive {
                frame
                    .ip
                    .checked_add(offset)
                    .vm_result(VmErrorKind::OutOfBounds, &*frame)?
            } else {
                frame
                    .ip
                    .checked_sub(offset)
                    .vm_result(VmErrorKind::OutOfBounds, &*frame)?
            };

            Ok(Transition::Jump)
        }

        // jmpc
        pub(super) fn op_jump_cond(&mut self, frame: &mut CallFrame) -> OpResult {
            if self.pop_bool(frame)? {
                self.op_jump(frame)
            } else {
                Ok(Transition::Continue)
            }

            // match self.get_value(0)? {
            //     Value::Bool(true) => self.op_jump(),
            //     Value::Bool(false) => Ok(Transition::Continue),
            //     _ => throw!(VmErrorKind::Type)),
            // }
        }

        // jmpci
        pub(super) fn op_jump_cond_imm(
            &mut self,
            offset: isize,
            frame: &mut CallFrame,
        ) -> OpResult {
            if self.pop_bool(frame)? {
                self.op_jump_imm(offset, frame)
            } else {
                Ok(Transition::Continue)
            }
        }

        // call
        pub(super) fn op_call(&mut self, frame: &CallFrame) -> OpResult {
            let func = self.pop_function(frame)?;

            self.op_call_imm(func, frame)
        }

        // calli
        pub(super) fn op_call_imm(&mut self, func: Function, frame: &CallFrame) -> OpResult {
            let new_base = frame.stack_base + frame.locals;

            self.run_function(func, new_base)
        }

        pub(super) fn op_call_builtin(&mut self, index: usize, frame: &CallFrame) -> OpResult {
            let func = BUILTINS
                .get(index)
                .vm_result(VmErrorKind::FunctionNotFound, frame)?;

            func(self, frame)?;
            Ok(Transition::Continue)
        }

        // calln
        pub(super) fn op_call_native(&mut self, symbol: Symbol, frame: &CallFrame) -> OpResult {
            let native = self.resolve_native_function(symbol, frame)?;

            let value = native(self, frame)?;

            self.push_value(value, frame)?;

            Ok(Transition::Continue)
        }

        // ret
        pub(super) fn op_ret(&mut self, frame: &CallFrame) -> OpResult {
            let new_stack_len = frame.stack_base + frame.locals + 1;
            self.data_stack.truncate(new_stack_len);

            Ok(Transition::Return)
        }

        // casts
        pub(super) fn op_cast_int(&mut self, frame: &CallFrame) -> OpResult {
            let cast = match self.pop_value(frame)? {
                Value::Int(v) => Value::Int(v),
                Value::Float(v) => Value::Int(v as i64),
                Value::Bool(v) => Value::Int(v as i64),
                Value::Char(v) => Value::Int(v as i64),
                _ => throw!(VmErrorKind::Type, frame),
            };

            self.push_value(cast, frame)?;

            Ok(Transition::Continue)
        }

        // castf
        pub(super) fn op_cast_float(&mut self, frame: &CallFrame) -> OpResult {
            let cast = match self.pop_value(frame)? {
                Value::Int(v) => Value::Float(v as f64),
                Value::Float(v) => Value::Float(v),
                Value::Bool(v) => Value::Float(v as u64 as f64),
                Value::Char(v) => Value::Float(v as u64 as f64),
                _ => throw!(VmErrorKind::Type, frame),
            };

            self.push_value(cast, frame)?;

            Ok(Transition::Continue)
        }

        // castb
        pub(super) fn op_cast_bool(&mut self, frame: &CallFrame) -> OpResult {
            let cast = match self.pop_value(frame)? {
                Value::Int(v) => Value::Bool(v != 0),
                Value::Float(v) => Value::Bool(v != 0.0),
                Value::Bool(v) => Value::Bool(v),
                Value::Char(v) => Value::Bool(v != '\0'),
                _ => throw!(VmErrorKind::Type, frame),
            };

            self.push_value(cast, frame)?;

            Ok(Transition::Continue)
        }

        // dbg
        pub(super) fn op_dbg(&self, index: usize, frame: &CallFrame) -> OpResult {
            let value = self.get_value(index, frame)?;

            match value {
                reference @ Value::Reference(v) => {
                    eprint!("stack[{index:#x}]: {reference:?} => ");

                    let obj_ref = self
                        .heap
                        .get(v)
                        .map(|obj| Ref::map(obj, |obj| obj.as_debug()));

                    match obj_ref {
                        Some(debug) => {
                            let address = ptr::from_ref(&*debug).addr();

                            eprintln!("{debug:#?} @ {address:#x}")
                        }
                        None => eprintln!("<invalid>"),
                    }
                }
                value => eprintln!("stack[{index:#x}]: {value:?}"),
            }
            Ok(Transition::Continue)
        }

        // dbgvm
        pub(super) fn op_dbg_vm(&self, frame: &CallFrame) -> OpResult {
            eprintln!("vm: {self:#?}, frame: {frame:#?}");

            Ok(Transition::Continue)
        }
    }
}
