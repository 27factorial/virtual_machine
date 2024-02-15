use super::*;
use crate::ops::OpResult;
use crate::string::SymbolIndex;

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
                $self.registers[Register::R0] =
                    Value::UInt(u64::$int_op(a, b).ok_or(OpError::Arithmetic)?);
            }
            (Value::SInt(a), Value::SInt(b)) => {
                $self.registers[Register::R0] =
                    Value::SInt(i64::$int_op(a, b).ok_or(OpError::Arithmetic)?);
            }
            (Value::Float(a), Value::Float(b)) => {
                $self.registers[Register::R0] = Value::Float(f64::$float_op(a, b));
            }
            (Value::Address(a), Value::Address(b)) => {
                $self.registers[Register::R0] =
                    Value::Address(usize::$int_op(a, b).ok_or(OpError::Arithmetic)?)
            }
            _ => return Err(OpError::Type),
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
                $self.registers[Register::R0] = Value::UInt($op(a, b));
            }
            (Value::SInt(a), Value::SInt(b)) => {
                $self.registers[Register::R0] = Value::SInt($op(a, b));
            }
            (Value::Bool(a), Value::Bool(b)) => {
                $self.registers[Register::R0] = Value::Bool($op(a, b));
            }
            (Value::Address(a), Value::Address(b)) => {
                $self.registers[Register::R0] = Value::Address($op(a, b));
            }
            _ => return Err(OpError::Type),
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
                let b = u32::try_from(b).or(Err(OpError::Arithmetic))?;
                $self.registers[Register::R0] =
                    Value::UInt(u64::$op(a, b).ok_or(OpError::Arithmetic)?);
            }
            (Value::UInt(a), Value::SInt(b)) => {
                let b = u32::try_from(b).or(Err(OpError::Arithmetic))?;
                $self.registers[Register::R0] =
                    Value::UInt(u64::$op(a, b).ok_or(OpError::Arithmetic)?);
            }
            (Value::SInt(a), Value::SInt(b)) => {
                let b = u32::try_from(b).or(Err(OpError::Arithmetic))?;
                $self.registers[Register::R0] =
                    Value::SInt(i64::$op(a, b).ok_or(OpError::Arithmetic)?);
            }
            (Value::SInt(a), Value::UInt(b)) => {
                let b = u32::try_from(b).or(Err(OpError::Arithmetic))?;
                $self.registers[Register::R0] =
                    Value::SInt(i64::$op(a, b).ok_or(OpError::Arithmetic)?);
            }
            (Value::Address(a), Value::Address(b)) => {
                let b = u32::try_from(b).or(Err(OpError::Arithmetic))?;
                $self.registers[Register::R0] =
                    Value::Address(usize::$op(a, b).ok_or(OpError::Arithmetic)?)
            }
            (Value::Address(a), Value::UInt(b)) => {
                let b = u32::try_from(b).or(Err(OpError::Arithmetic))?;
                $self.registers[Register::R0] =
                    Value::Address(usize::$op(a, b).ok_or(OpError::Arithmetic)?)
            }
            (Value::Address(a), Value::SInt(b)) => {
                let b = u32::try_from(b).or(Err(OpError::Arithmetic))?;
                $self.registers[Register::R0] =
                    Value::Address(usize::$op(a, b).ok_or(OpError::Arithmetic)?)
            }
            _ => return Err(OpError::Type),
        }

        Ok(Transition::Continue)
    }};
}

macro_rules! bin_compare {
    (
        vm = $self:ident,
        a = $a:expr,
        b = $b:expr,
        op = $op:path $(,)?
    ) => {{
        match ($a, $b) {
            (Value::Null, Value::Null) => {
                $self.registers[Register::R0] = Value::Bool(true);
            }
            (Value::UInt(a), Value::UInt(b)) => {
                $self.registers[Register::R0] = Value::Bool($op(&a, &b));
            }
            (Value::SInt(a), Value::SInt(b)) => {
                $self.registers[Register::R0] = Value::Bool($op(&a, &b));
            }
            (Value::Float(a), Value::Float(b)) => {
                $self.registers[Register::R0] = Value::Bool($op(&a, &b));
            }
            (Value::Bool(a), Value::Bool(b)) => {
                $self.registers[Register::R0] = Value::Bool($op(&a, &b));
            }
            (Value::Char(a), Value::Char(b)) => {
                $self.registers[Register::R0] = Value::Bool($op(&a, &b));
            }
            (Value::Address(a), Value::Address(b)) => {
                $self.registers[Register::R0] = Value::Bool($op(&a, &b));
            }
            (Value::Object(a), Value::Object(b)) => {
                $self.registers[Register::R0] = Value::Bool($op(&a, &b));
            }
            _ => return Err(OpError::Type),
        }

        Ok(Transition::Continue)
    }};
}

impl Vm {
    // NoOp
    #[inline]
    pub(crate) fn noop(&self) -> OpResult {
        Ok(Transition::Continue)
    }

    // Halt
    #[inline]
    pub(crate) fn halt(&self) -> OpResult {
        Ok(Transition::Halt)
    }

    // PushImmediate
    #[inline]
    pub(crate) fn push_imm(&mut self, value: Value) -> OpResult {
        self.push_data_stack(value)?;
        Ok(Transition::Continue)
    }

    // Push
    #[inline]
    pub(crate) fn push(&mut self, register: Register) -> OpResult {
        self.push_data_stack(self.registers[register])?;
        Ok(Transition::Continue)
    }

    // Pop
    #[inline]
    pub(crate) fn pop(&mut self, register: Register) -> OpResult {
        let value = self.pop_data_stack()?;

        self.registers[register] = value;
        Ok(Transition::Continue)
    }

    // LoadRegister
    #[inline]
    pub(crate) fn load_reg(&mut self, register_src: Register, register_dst: Register) -> OpResult {
        let value = self.registers[register_src];
        self.registers[register_dst] = value;
        Ok(Transition::Continue)
    }

    // LoadImmediate
    #[inline]
    pub(crate) fn load_imm(&mut self, value: Value, register: Register) -> OpResult {
        self.registers[register] = value;
        Ok(Transition::Continue)
    }

    // LoadMemory
    #[inline]
    pub(crate) fn load_mem(&mut self, index: usize, register: Register) -> OpResult {
        let value = self
            .memory
            .get(index)
            .copied()
            .ok_or(OpError::InvalidAddress)?;

        self.registers[register] = value;
        Ok(Transition::Continue)
    }

    // StoreRegister
    #[inline]
    pub(crate) fn store_reg(&mut self, register: Register, index: usize) -> OpResult {
        let value = self.registers[register];

        let location = self.memory.get_mut(index).ok_or(OpError::InvalidAddress)?;

        *location = value;

        Ok(Transition::Continue)
    }

    // StoreImmediate
    #[inline]
    pub(crate) fn store_imm(&mut self, value: Value, index: usize) -> OpResult {
        let location = self.memory.get_mut(index).ok_or(OpError::InvalidAddress)?;

        *location = value;

        Ok(Transition::Continue)
    }

    // StoreMemory
    #[inline]
    pub(crate) fn store_mem(&mut self, index_src: usize, index_dst: usize) -> OpResult {
        let value = self
            .memory
            .get(index_src)
            .copied()
            .ok_or(OpError::InvalidAddress)?;

        let dst = self
            .memory
            .get_mut(index_dst)
            .ok_or(OpError::InvalidAddress)?;

        *dst = value;

        Ok(Transition::Continue)
    }

    // Add
    #[inline]
    pub(crate) fn add(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_arithmetic! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            int_op = checked_add,
            float_op = add,
        }
    }

    // AddImmediate
    #[inline]
    pub(crate) fn add_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_arithmetic! {
            vm = self,
            a = self.registers[register],
            b = value,
            int_op = checked_add,
            float_op = add,
        }
    }

    // Sub
    #[inline]
    pub(crate) fn sub(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_arithmetic! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            int_op = checked_sub,
            float_op = sub,
        }
    }

    // SubImmediate
    #[inline]
    pub(crate) fn sub_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_arithmetic! {
            vm = self,
            a = self.registers[register],
            b = value,
            int_op = checked_sub,
            float_op = sub,
        }
    }

    // Mul
    #[inline]
    pub(crate) fn mul(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_arithmetic! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            int_op = checked_mul,
            float_op = mul,
        }
    }

    // MulImmediate
    #[inline]
    pub(crate) fn mul_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_arithmetic! {
            vm = self,
            a = self.registers[register],
            b = value,
            int_op = checked_mul,
            float_op = mul,
        }
    }

    // Div
    #[inline]
    pub(crate) fn div(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_arithmetic! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            int_op = checked_div,
            float_op = div,
        }
    }

    // DivImmediate
    #[inline]
    pub(crate) fn div_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_arithmetic! {
            vm = self,
            a = self.registers[register],
            b = value,
            int_op = checked_div,
            float_op = div,
        }
    }

    // Rem
    #[inline]
    pub(crate) fn rem(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_arithmetic! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            int_op = checked_rem,
            float_op = rem,
        }
    }

    // RemImmediate
    #[inline]
    pub(crate) fn rem_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_arithmetic! {
            vm = self,
            a = self.registers[register],
            b = value,
            int_op = checked_rem,
            float_op = rem,
        }
    }

    // And
    #[inline]
    pub(crate) fn and(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_bitwise! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            op = BitAnd::bitand,
        }
    }

    // AndImmediate
    #[inline]
    pub(crate) fn and_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_bitwise! {
            vm = self,
            a = self.registers[register],
            b = value,
            op = BitAnd::bitand,
        }
    }

    // Or
    #[inline]
    pub(crate) fn or(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_bitwise! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            op = BitOr::bitor,
        }
    }

    // OrImmediate
    #[inline]
    pub(crate) fn or_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_bitwise! {
            vm = self,
            a = self.registers[register],
            b = value,
            op = BitOr::bitor,
        }
    }

    // Xor
    #[inline]
    pub(crate) fn xor(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_bitwise! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            op = BitXor::bitxor,
        }
    }

    // XorImmediate
    #[inline]
    pub(crate) fn xor_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_bitwise! {
            vm = self,
            a = self.registers[register],
            b = value,
            op = BitXor::bitxor,
        }
    }

    // Not
    #[inline]
    pub(crate) fn not(&mut self, register: Register) -> OpResult {
        let value = self.registers[register];

        match value {
            Value::UInt(val) => {
                self.registers[Register::R0] = Value::UInt(!val);
            }
            Value::SInt(val) => {
                self.registers[Register::R0] = Value::SInt(!val);
            }
            Value::Bool(val) => {
                self.registers[Register::R0] = Value::Bool(!val);
            }
            Value::Address(val) => {
                self.registers[Register::R0] = Value::Address(!val);
            }
            _ => return Err(OpError::Type),
        }

        Ok(Transition::Continue)
    }

    // ShiftRight
    #[inline]
    pub(crate) fn shr(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_shift! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            op = checked_shr,
        }
    }

    // ShiftRightImmediate
    #[inline]
    pub(crate) fn shr_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_shift! {
            vm = self,
            a = self.registers[register],
            b = value,
            op = checked_shr,
        }
    }

    // ShiftLeft
    #[inline]
    pub(crate) fn shl(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_shift! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            op = checked_shl,
        }
    }

    // ShiftLeftImmediate
    #[inline]
    pub(crate) fn shl_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_shift! {
            vm = self,
            a = self.registers[register],
            b = value,
            op = checked_shl,
        }
    }

    // Equals
    #[inline]
    pub(crate) fn eq(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_compare! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            op = PartialEq::eq,
        }
    }

    // EqualsImmediate
    #[inline]
    pub(crate) fn eq_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_compare! {
            vm = self,
            a = self.registers[register],
            b = value,
            op = PartialEq::eq,
        }
    }

    // GreaterThan
    #[inline]
    pub(crate) fn gt(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_compare! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            op = PartialOrd::gt,
        }
    }

    // GreaterThanImmediate
    #[inline]
    pub(crate) fn gt_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_compare! {
            vm = self,
            a = self.registers[register],
            b = value,
            op = PartialOrd::gt,
        }
    }

    // GreaterThanOrEqual
    #[inline]
    pub(crate) fn ge(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_compare! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            op = PartialOrd::ge,
        }
    }

    // GreaterThanOrEqualImmediate
    #[inline]
    pub(crate) fn ge_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_compare! {
            vm = self,
            a = self.registers[register],
            b = value,
            op = PartialOrd::ge,
        }
    }

    // LessThan
    #[inline]
    pub(crate) fn lt(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_compare! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            op = PartialOrd::lt,
        }
    }

    // LessThanImmediate
    #[inline]
    pub(crate) fn lt_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_compare! {
            vm = self,
            a = self.registers[register],
            b = value,
            op = PartialOrd::lt,
        }
    }

    // LessThanEqual
    #[inline]
    pub(crate) fn le(&mut self, register_a: Register, register_b: Register) -> OpResult {
        bin_compare! {
            vm = self,
            a = self.registers[register_a],
            b = self.registers[register_b],
            op = PartialOrd::le,
        }
    }

    // LessThanEqualImmediate
    #[inline]
    pub(crate) fn le_imm(&mut self, register: Register, value: Value) -> OpResult {
        bin_compare! {
            vm = self,
            a = self.registers[register],
            b = value,
            op = PartialOrd::le,
        }
    }

    // Jump
    #[inline]
    pub(crate) fn jump(&mut self, register: Register) -> OpResult {
        let address = self.registers[register].address_or_err(OpError::Type)?;

        self.jump_imm(address)
    }

    // JumpImmediate
    #[inline]
    pub(crate) fn jump_imm(&mut self, address: usize) -> OpResult {
        self.current_frame.ip = address;

        Ok(Transition::Jump)
    }

    // JumpConditional
    #[inline]
    pub(crate) fn jump_cond(
        &mut self,
        condition_register: Register,
        address_register: Register,
    ) -> OpResult {
        match self.registers[condition_register] {
            Value::Bool(true) => self.jump(address_register),
            Value::Bool(false) => Ok(Transition::Continue),
            _ => Err(OpError::Type),
        }
    }

    // JumpConditionalImmediate
    #[inline]
    pub(crate) fn jump_cond_imm(&mut self, register: Register, address: usize) -> OpResult {
        match self.registers[register] {
            Value::Bool(true) => self.jump_imm(address),
            Value::Bool(false) => Ok(Transition::Continue),
            _ => Err(OpError::Type),
        }
    }

    // Call
    #[inline]
    pub(crate) fn call(&mut self, register: Register) -> OpResult {
        let symbol = self.registers[register].symbol_or_err(OpError::Type)?;

        self.call_imm(symbol)
    }

    // CallImmediate
    #[inline]
    pub(crate) fn call_imm(&mut self, symbol: SymbolIndex) -> OpResult {
        let called_func = self.resolve_function(symbol)?;

        let caller = mem::replace(&mut self.current_frame, CallFrame::new(called_func, 0));

        self.push_call_stack(caller)?;

        Ok(Transition::Jump)
    }

    #[inline]
    // CallNative
    pub(crate) fn call_native(&mut self, symbol: SymbolIndex) -> OpResult {
        let native = self.resolve_native_function(symbol)?;

        if let Some(value) = native(self) {
            self.registers[Register::R0] = value;
        };

        Ok(Transition::Continue)
    }

    // Return
    #[inline]
    pub(crate) fn ret(&mut self) -> OpResult {
        self.current_frame = self.pop_call_stack()?;

        Ok(Transition::Continue)
    }

    // InitializeObject
    pub(crate) fn init_object(&mut self, register: Register) -> OpResult {
        let symbol = self.registers[register].symbol_or_err(OpError::Type)?;
        let name = self
            .program
            .symbols
            .get(symbol)
            .ok_or(OpError::SymbolNotFound)?;
        let ty = self.program.types.get(name).ok_or(OpError::TypeNotFound)?;

        let called_func = ty.operators.init.clone();

        let caller = mem::replace(&mut self.current_frame, CallFrame::new(called_func, 0));

        self.push_call_stack(caller)?;

        Ok(Transition::Continue)
    }

    // IndexObject
    pub(crate) fn index_object(&mut self, register: Register) -> OpResult {
        let symbol = self.registers[register].symbol_or_err(OpError::Type)?;
        let name = self
            .program
            .symbols
            .get(symbol)
            .ok_or(OpError::SymbolNotFound)?;
        let ty = self.program.types.get(name).ok_or(OpError::TypeNotFound)?;

        let called_func = ty
            .operators
            .index
            .clone()
            .ok_or(OpError::OperatorNotSupported)?;

        let caller = mem::replace(&mut self.current_frame, CallFrame::new(called_func, 0));

        self.push_call_stack(caller)?;

        Ok(Transition::Continue)
    }

    // DebugRegister
    #[inline]
    pub(crate) fn dbg_reg(&self, register: Register) -> OpResult {
        let value = self.registers[register];

        eprintln!("Register {register:?}: {value:?}");
        Ok(Transition::Continue)
    }

    // DebugMemory
    #[inline]
    pub(crate) fn dbg_mem(&self, index: usize) -> OpResult {
        let value = self.memory.get(index);

        eprintln!("Address {index:#x}: {value:?}");
        Ok(Transition::Continue)
    }
}
