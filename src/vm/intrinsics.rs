use crate::{utils::IntoVmResult, value::Value};

use super::{CallFrame, Result, Vm, VmError, VmErrorKind};
use paste::paste;

macro_rules! float_intrinsics {
    ($($name:ident),* $(,)?) => {
        $(
            paste! {
                fn [<vmbi_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let Value::Float(top) = vm.top_value_mut(frame)? else {
                        return Err(VmError::new(VmErrorKind::Type, frame));
                    };

                    *top = top.$name();
                    Ok(())
                }
            }
        )*
    }
}

macro_rules! float_to_bool_intrinsics {
    ($($name:ident),* $(,)?) => {
        $(
            paste! {
                fn [<vmbi_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let top = vm.top_value_mut(frame)?;

                    let Value::Float(arg) = top else {
                        return Err(VmError::new(VmErrorKind::Type, frame));
                    };

                    *top = Value::Bool(arg.$name());
                    Ok(())
                }
            }
        )*
    }
}

macro_rules! count_intrinsics {
    ($($name:ident),* $(,)?) => {
        $(
            paste! {
                fn [<vmbi_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let top = vm.top_value_mut(frame)?;

                    let count = match top {
                        Value::UInt(v) => v.$name() as u64,
                        Value::SInt(v) => v.$name() as u64,
                        Value::Address(v) => v.$name() as u64,
                        _ => return Err(VmError::new(VmErrorKind::Type, frame))
                    };

                    *top = Value::UInt(count);
                    Ok(())
                }
            }
        )*
    }
}

macro_rules! int_arithmetic_intrinsics {
    ($($name:ident),* $(,)?) => {
        $(
            paste! {
                fn [<vmbi_wrapping_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let rhs = vm.pop_value(frame)?;
                    let top = vm.top_value_mut(frame)?;

                    let result = match (&top, rhs) {
                        (Value::UInt(a), Value::UInt(b))  => Value::UInt(a.[<wrapping_ $name>](b)),
                        (Value::SInt(a), Value::SInt(b))  => Value::SInt(a.[<wrapping_ $name>](b)),
                        (Value::Address(a), Value::Address(b))  => Value::Address(a.[<wrapping_ $name>](b)),
                        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
                    };

                    *top = result;
                    Ok(())
                }

                fn [<vmbi_saturating_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let rhs = vm.pop_value(frame)?;
                    let top = vm.top_value_mut(frame)?;

                    let result = match (&top, rhs) {
                        (Value::UInt(a), Value::UInt(b))  => Value::UInt(a.[<saturating_ $name>](b)),
                        (Value::SInt(a), Value::SInt(b))  => Value::SInt(a.[<saturating_ $name>](b)),
                        (Value::Address(a), Value::Address(b))  => Value::Address(a.[<saturating_ $name>](b)),
                        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
                    };

                    *top = result;
                    Ok(())
                }

                fn [<vmbi_overflowing_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let rhs = vm.pop_value(frame)?;
                    let top = vm.top_value_mut(frame)?;

                    let (result, flag) = match (&top, rhs) {
                        (Value::UInt(a), Value::UInt(b))  => {
                            let (val, flag) = a.[<overflowing_ $name>](b);
                            (Value::UInt(val), flag)
                        },
                        (Value::SInt(a), Value::SInt(b))  => {
                            let (val, flag) = a.[<overflowing_ $name>](b);
                            (Value::SInt(val), flag)
                        },
                        (Value::Address(a), Value::Address(b))  => {
                            let (val, flag) = a.[<overflowing_ $name>](b);
                            (Value::Address(val), flag)
                        },
                        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
                    };

                    *top = result;
                    vm.push_value(Value::Bool(flag), frame)?;
                    Ok(())
                }
            }
        )*
    }
}

type Intrinsic = fn(&mut Vm, frame: &CallFrame) -> Result<()>;

#[rustfmt::skip]
pub static INTRINSICS: &[Intrinsic] = &[
    // Panics
    vmbi_assert,
    vmbi_unreachable,
    vmbi_panic,

    // Counts
    vmbi_count_ones,
    vmbi_count_zeros,
    vmbi_leading_zeros,
    vmbi_trailing_zeros,
    vmbi_leading_ones,
    vmbi_trailing_ones,

    // Integer/float checked arithmetic
    vmbi_abs,
    vmbi_pow,
    vmbi_log,

    // Wrapping integer arithmetic
    vmbi_wrapping_add,
    vmbi_wrapping_sub,
    vmbi_wrapping_mul,
    vmbi_wrapping_div,
    vmbi_wrapping_rem,
    vmbi_wrapping_neg,
    vmbi_wrapping_abs,
    vmbi_wrapping_pow,

    // Saturating integer arithmetic
    vmbi_saturating_add,
    vmbi_saturating_sub,
    vmbi_saturating_mul,
    vmbi_saturating_div,
    // vmbi_saturating_rem doesn't exist, it has the same behavior as the Rem instruction.
    vmbi_saturating_neg,
    vmbi_saturating_abs,
    vmbi_saturating_pow,

    // Overflowing integer arithmetic (e.g., returns a bool indicating overflow)
    vmbi_overflowing_add,
    vmbi_overflowing_sub,
    vmbi_overflowing_mul,
    vmbi_overflowing_div,
    vmbi_overflowing_rem,
    vmbi_overflowing_neg,
    vmbi_overflowing_abs,
    vmbi_overflowing_pow,

    // Specific integer/float logarithms
    vmbi_log2,
    vmbi_log10,

    // Signs
    vmbi_signum,
    vmbi_is_positive,
    vmbi_is_negative,

    // Min/max and ranges
    vmbi_min,
    vmbi_max,
    vmbi_clamp,

    // Floating point functions
    vmbi_sin,
    vmbi_cos,
    vmbi_tan,
    vmbi_sinh,
    vmbi_cosh,
    vmbi_tanh,
    vmbi_asin,
    vmbi_acos,
    vmbi_atan,
    vmbi_atan2,
    vmbi_asinh,
    vmbi_acosh,
    vmbi_atanh,
    vmbi_floor,
    vmbi_ceil,
    vmbi_round,
    vmbi_trunc,
    vmbi_fract,
    vmbi_sqrt,
    vmbi_cbrt,
    vmbi_exp,
    vmbi_exp2,
    vmbi_exp_m1,
    vmbi_ln,
    vmbi_ln_1p,
    vmbi_to_degrees,
    vmbi_to_radians,
    vmbi_recip,

    // Misc. useful functions
    vmbi_to_string,
];

//////////////////////////////////////////
// ============== PANICS ============== //
//////////////////////////////////////////

fn get_panic_message(vm: &mut Vm, frame: &CallFrame) -> String {
    match vm.pop_value(frame) {
        Ok(Value::Null) => "null".into(),
        Ok(Value::UInt(v)) => v.to_string(),
        Ok(Value::SInt(v)) => v.to_string(),
        Ok(Value::Float(v)) => v.to_string(),
        Ok(Value::Bool(v)) => v.to_string(),
        Ok(Value::Char(v)) => v.to_string(),
        Ok(Value::Address(v)) => format!("{v:x}"),
        Ok(Value::Symbol(v)) => vm.program.symbols.get(v).unwrap_or("<no message>").into(),
        Ok(Value::Reference(_)) => todo!("reference to_string"),
        Err(_) => "<no message>".into(),
    }
}

fn vmbi_assert(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let message = get_panic_message(vm, frame);
    let condition = vm.pop_bool(frame)?;

    if !condition {
        panic!("assertion failure: {message}");
    }

    Ok(())
}

fn vmbi_unreachable(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let message: String = get_panic_message(vm, frame);
    unreachable!("{message}");
}

fn vmbi_panic(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let message = get_panic_message(vm, frame);
    panic!("{message}");
}

//////////////////////////////////////////
// ============== COUNTS ============== //
//////////////////////////////////////////

count_intrinsics! {
    count_ones,
    count_zeros,
    leading_zeros,
    trailing_zeros,
    leading_ones,
    trailing_ones,
}

//////////////////////////////////////////////
// ============== ARITHMETIC ============== //
//////////////////////////////////////////////

int_arithmetic_intrinsics! {
    add,
    sub,
    mul,
    div,
}

// abs
fn vmbi_abs(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let value = match top {
        Value::UInt(v) => Value::UInt(*v),
        Value::SInt(v) => Value::SInt(v.abs()),
        Value::Float(v) => Value::Float(v.abs()),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = value;
    Ok(())
}

fn vmbi_wrapping_abs(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::UInt(v) => Value::UInt(*v),
        Value::SInt(v) => Value::SInt(v.wrapping_abs()),
        Value::Address(v) => Value::Address(*v),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_saturating_abs(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::UInt(v) => Value::UInt(*v),
        Value::SInt(v) => Value::SInt(v.saturating_abs()),
        Value::Address(v) => Value::Address(*v),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_overflowing_abs(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let (result, flag) = match top {
        Value::UInt(v) => (Value::UInt(*v), false),
        Value::SInt(v) => {
            let (val, flag) = v.overflowing_abs();
            (Value::SInt(val), flag)
        }
        Value::Address(v) => (Value::Address(*v), false),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    vm.push_value(Value::Bool(flag), frame)?;
    Ok(())
}

fn vmbi_wrapping_rem(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let rhs = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let result = match (&top, rhs) {
        (Value::UInt(a), Value::UInt(b)) => Value::UInt(a.wrapping_rem(b)),
        (Value::SInt(a), Value::SInt(b)) => Value::SInt(a.wrapping_rem(b)),
        (Value::Address(a), Value::Address(b)) => Value::Address(a.wrapping_rem(b)),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_overflowing_rem(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let rhs = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let (result, flag) = match (&top, rhs) {
        (Value::UInt(a), Value::UInt(b)) => {
            let (val, flag) = a.overflowing_rem(b);
            (Value::UInt(val), flag)
        }
        (Value::SInt(a), Value::SInt(b)) => {
            let (val, flag) = a.overflowing_rem(b);
            (Value::SInt(val), flag)
        }
        (Value::Address(a), Value::Address(b)) => {
            let (val, flag) = a.overflowing_rem(b);
            (Value::Address(val), flag)
        }
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    vm.push_value(Value::Bool(flag), frame)?;
    Ok(())
}

fn vmbi_wrapping_neg(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let Value::SInt(top) = vm.top_value_mut(frame)? else {
        return Err(VmError::new(VmErrorKind::Type, frame));
    };

    let result = top.wrapping_neg();

    *top = result;
    Ok(())
}

fn vmbi_saturating_neg(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let Value::SInt(top) = vm.top_value_mut(frame)? else {
        return Err(VmError::new(VmErrorKind::Type, frame));
    };

    let result = top.saturating_neg();

    *top = result;
    Ok(())
}

fn vmbi_overflowing_neg(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let Value::SInt(top) = vm.top_value_mut(frame)? else {
        return Err(VmError::new(VmErrorKind::Type, frame));
    };

    let (result, flag) = top.overflowing_neg();

    *top = result;
    vm.push_value(Value::Bool(flag), frame)?;
    Ok(())
}

// The normal pow function is more complex than a normal instruction, since it has different
// behaviors if the exponent is an unsigned, signed, or floating point number.
fn vmbi_pow(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    #[inline(always)]
    // Integer powers (works in the range 0..=u32::MAX for integers and the range
    // i32::MIN..=i32::MAX for floats).
    fn powi(
        value: &Value,
        exponent: impl TryInto<u32> + TryInto<i32>,
        frame: &CallFrame,
    ) -> Result<Value> {
        match value {
            Value::UInt(v) => {
                let exp: u32 = exponent
                    .try_into()
                    .vm_result(VmErrorKind::Arithmetic, frame)?;
                let result = v
                    .checked_pow(exp)
                    .vm_result(VmErrorKind::Arithmetic, frame)?;
                Ok(Value::UInt(result))
            }
            Value::SInt(v) => {
                let exp: u32 = exponent
                    .try_into()
                    .vm_result(VmErrorKind::Arithmetic, frame)?;
                let result = v
                    .checked_pow(exp)
                    .vm_result(VmErrorKind::Arithmetic, frame)?;
                Ok(Value::SInt(result))
            }
            Value::Float(v) => {
                let exp: i32 = exponent
                    .try_into()
                    .vm_result(VmErrorKind::Arithmetic, frame)?;
                Ok(Value::Float(v.powi(exp)))
            }
            _ => Err(VmError::new(VmErrorKind::Type, frame)),
        }
    }

    let exponent = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let result = match exponent {
        Value::UInt(exp) => powi(top, exp, frame)?,
        Value::SInt(exp) => powi(top, exp, frame)?,
        Value::Float(exp) => {
            let Value::Float(v) = top else {
                return Err(VmError::new(VmErrorKind::Type, frame));
            };

            Value::Float(v.powf(exp))
        }
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_wrapping_pow(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let exponent = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let exponent = match exponent {
        Value::UInt(v) => u32::try_from(v).vm_result(VmErrorKind::Arithmetic, frame)?,
        Value::SInt(v) => u32::try_from(v).vm_result(VmErrorKind::Arithmetic, frame)?,
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    let result = match top {
        Value::UInt(v) => Value::UInt(v.wrapping_pow(exponent)),
        Value::SInt(v) => Value::SInt(v.wrapping_pow(exponent)),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_saturating_pow(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let exponent = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let exponent = match exponent {
        Value::UInt(v) => u32::try_from(v).vm_result(VmErrorKind::Arithmetic, frame)?,
        Value::SInt(v) => u32::try_from(v).vm_result(VmErrorKind::Arithmetic, frame)?,
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    let result = match top {
        Value::UInt(v) => Value::UInt(v.saturating_pow(exponent)),
        Value::SInt(v) => Value::SInt(v.saturating_pow(exponent)),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_overflowing_pow(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let exponent = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let exponent = match exponent {
        Value::UInt(v) => u32::try_from(v).vm_result(VmErrorKind::Arithmetic, frame)?,
        Value::SInt(v) => u32::try_from(v).vm_result(VmErrorKind::Arithmetic, frame)?,
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    let (result, flag) = match top {
        Value::UInt(v) => {
            let (result, flag) = v.overflowing_pow(exponent);
            (Value::UInt(result), flag)
        }
        Value::SInt(v) => {
            let (result, flag) = v.overflowing_pow(exponent);
            (Value::SInt(result), flag)
        }
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    vm.push_value(Value::Bool(flag), frame)?;
    Ok(())
}

fn vmbi_log(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let base = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let result = match (&top, base) {
        (Value::UInt(a), Value::UInt(b)) => {
            let result = a
                .checked_ilog(b)
                .vm_result(VmErrorKind::Arithmetic, frame)?;
            Value::UInt(result as u64)
        }
        (Value::SInt(a), Value::SInt(b)) => {
            let result = a
                .checked_ilog(b)
                .vm_result(VmErrorKind::Arithmetic, frame)?;
            Value::SInt(result as i64)
        }
        (Value::Float(a), Value::Float(b)) => Value::Float(a.log(b)),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_log2(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::UInt(a) => Value::UInt(a.ilog2() as u64),
        Value::SInt(a) => Value::SInt(a.ilog2() as i64),
        Value::Float(a) => Value::Float(a.log2()),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_log10(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::UInt(a) => Value::UInt(a.ilog10() as u64),
        Value::SInt(a) => Value::SInt(a.ilog10() as i64),
        Value::Float(a) => Value::Float(a.log10()),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_signum(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::UInt(_) => Value::UInt(1),
        Value::SInt(a) => Value::SInt(a.signum()),
        Value::Float(a) => Value::Float(a.signum()),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_is_positive(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::UInt(_) => Value::Bool(true),
        Value::SInt(a) => Value::Bool(a.is_positive()),
        Value::Float(a) => Value::Bool(a.is_sign_positive()),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_is_negative(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::UInt(_) => Value::Bool(true),
        Value::SInt(a) => Value::Bool(a.is_negative()),
        Value::Float(a) => Value::Bool(a.is_sign_negative()),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

///////////////////////////////////////////
// ============== MIN/MAX ============== //
///////////////////////////////////////////

fn vmbi_min(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let other = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let result = match (&top, other) {
        (Value::UInt(a), Value::UInt(b)) => Value::UInt((*a).min(b)),
        (Value::SInt(a), Value::SInt(b)) => Value::SInt((*a).min(b)),
        (Value::Float(a), Value::Float(b)) => Value::Float(a.min(b)),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_max(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let other = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let result = match (&top, other) {
        (Value::UInt(a), Value::UInt(b)) => Value::UInt((*a).max(b)),
        (Value::SInt(a), Value::SInt(b)) => Value::SInt((*a).max(b)),
        (Value::Float(a), Value::Float(b)) => Value::Float(a.max(b)),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

fn vmbi_clamp(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let max = vm.pop_value(frame)?;
    let min = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let result = match (&top, min, max) {
        (Value::UInt(a), Value::UInt(min), Value::UInt(max)) => Value::UInt((*a).clamp(min, max)),
        (Value::SInt(a), Value::SInt(min), Value::SInt(max)) => Value::SInt((*a).clamp(min, max)),
        (Value::Float(a), Value::Float(min), Value::Float(max)) => Value::Float(a.clamp(min, max)),
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = result;
    Ok(())
}

//////////////////////////////////////////
// ============== FLOATS ============== //
//////////////////////////////////////////

float_intrinsics! {
    // Trig
    sin,
    cos,
    tan,
    sinh,
    cosh,
    tanh,
    asin,
    acos,
    atan,
    asinh,
    acosh,
    atanh,

    // Rounding/fractional
    floor,
    ceil,
    round,
    trunc,
    fract,

    // Roots
    sqrt,
    cbrt,

    // Exp/log
    exp,
    exp2,
    exp_m1,
    ln,
    ln_1p,

    // Angles
    to_degrees,
    to_radians,

    // Misc.
    recip,
}

float_to_bool_intrinsics! {
    is_nan,
    is_infinite,
    is_finite,
    is_subnormal,
    is_normal,
}

fn vmbi_atan2(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let second = vm.pop_value(frame)?;
    let top = vm.top_value_mut(frame)?;

    let arg1 = match top {
        Value::UInt(v) => *v as f64,
        Value::SInt(v) => *v as f64,
        Value::Float(v) => *v,
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    let arg2 = match second {
        Value::UInt(v) => v as f64,
        Value::SInt(v) => v as f64,
        Value::Float(v) => v,
        _ => return Err(VmError::new(VmErrorKind::Type, frame)),
    };

    *top = Value::Float(arg1.atan2(arg2));
    Ok(())
}

////////////////////////////////////////
// ============== MISC ============== //
////////////////////////////////////////

fn vmbi_to_string(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let string = match vm.pop_value(frame)? {
        Value::Null => "null".into(),
        Value::UInt(v) => v.to_string(),
        Value::SInt(v) => v.to_string(),
        Value::Float(v) => v.to_string(),
        Value::Bool(v) => v.to_string(),
        Value::Char(v) => v.to_string(),
        Value::Address(v) => format!("{v:x}"),
        Value::Symbol(v) => vm.program.symbols.get(v).unwrap_or("").into(),
        Value::Reference(v) => todo!("reference to_string"),
    };

    todo!("TODO: Allocate string and push new object ref");
}
