use std::{hash::Hasher, sync::Arc};

use crate::{
    module::core::collections::{VmArray, VmDictionary, VmString},
    utils::IntoVmResult,
    value::Value,
};

use super::{CallFrame, Result, Vm, VmErrorKind, VmPanic};
use crate::throw;
use paste::paste;

macro_rules! float_intrinsics {
    ($($name:ident),* $(,)?) => {
        $(
            paste! {
                fn [<vmbi_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let Value::Float(top) = vm.top_value_mut(frame)? else {
                        throw!(VmErrorKind::Type, frame);
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
                        throw!(VmErrorKind::Type, frame);
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
                        Value::Int(v) => v.$name() as i64,
                        _ => throw!(VmErrorKind::Type, frame)
                    };

                    *top = Value::Int(count);
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
                    let lhs = vm.pop_value(frame)?;
                    let rhs = vm.top_value_mut(frame)?;

                    let result = match (lhs, &rhs) {
                        (Value::Int(a), Value::Int(b))  => Value::Int(a.[<wrapping_ $name>](*b)),
                        _ => throw!(VmErrorKind::Type, frame),
                    };

                    *rhs = result;
                    Ok(())
                }

                fn [<vmbi_saturating_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let lhs = vm.pop_value(frame)?;
                    let rhs = vm.top_value_mut(frame)?;

                    let result = match (lhs, &rhs) {
                        (Value::Int(a), Value::Int(b))  => Value::Int(a.[<saturating_ $name>](*b)),
                        _ => throw!(VmErrorKind::Type, frame),
                    };

                    *rhs = result;
                    Ok(())
                }

                fn [<vmbi_overflowing_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let lhs = vm.pop_value(frame)?;
                    let rhs = vm.top_value_mut(frame)?;

                    let (result, flag) = match (lhs, &rhs) {
                        (Value::Int(a), Value::Int(b))  => {
                            let (val, flag) = a.[<overflowing_ $name>](*b);
                            (Value::Int(val), flag)
                        },
                        _ => throw!(VmErrorKind::Type, frame),
                    };

                    *rhs = result;
                    vm.push_value(Value::Bool(flag), frame)?;
                    Ok(())
                }
            }
        )*
    }
}

macro_rules! define_builtins {
    ($($name:ident),+ $(,)?) => {
        const BUILTINS_LEN: usize = define_builtins!(@count $($name)+);

        paste! {
            pub static BUILTINS: [fn(&mut Vm, frame: &CallFrame) -> Result<()>; BUILTINS_LEN] = [
                $(
                    [<vmbi_ $name>]
                ),+
            ];
        }

        define_builtins! {
            @constant $($name)*
        }
    };
    (@constant $name:ident $($rest:ident)*) => {
        paste! {
            pub const [<$name:upper>]: usize = BUILTINS_LEN - define_builtins!(@count $($rest)*) - 1;
        }

        define_builtins! {
            @constant $($rest)*
        }
    };
    (@constant) => {};
    (@replace_expr $_t:tt $sub:expr) => {$sub};
    (@count $($tts:tt)*) => {
        <[()]>::len(&[$(define_builtins!(@replace_expr $tts ())),*])
    };
}

define_builtins! {
    // Panics
    assert,
    unreachable,
    panic,

    // Counts
    count_ones,
    count_zeros,
    leading_zeros,
    trailing_zeros,
    leading_ones,
    trailing_ones,

    // Integer/float checked arithmetic
    abs,
    pow,
    log,

    // Wrapping integer arithmetic
    wrapping_add,
    wrapping_sub,
    wrapping_mul,
    wrapping_div,
    wrapping_rem,
    wrapping_neg,
    wrapping_abs,
    wrapping_pow,

    // Saturating integer arithmetic
    saturating_add,
    saturating_sub,
    saturating_mul,
    saturating_div,
    // saturating_rem doesn't exist, it has the same behavior as the Rem instruction.
    saturating_neg,
    saturating_abs,
    saturating_pow,

    // Overflowing integer arithmetic (e.g., returns a bool indicating overflow)
    overflowing_add,
    overflowing_sub,
    overflowing_mul,
    overflowing_div,
    overflowing_rem,
    overflowing_neg,
    overflowing_abs,
    overflowing_pow,

    // Specific integer/float logarithms
    log2,
    log10,

    // Signs
    signum,
    is_positive,
    is_negative,

    // Min/max and ranges
    min,
    max,
    clamp,

    // Floating point functions
    sin,
    cos,
    tan,
    sinh,
    cosh,
    tanh,
    asin,
    acos,
    atan,
    atan2,
    asinh,
    acosh,
    atanh,
    floor,
    ceil,
    round,
    trunc,
    fract,
    sqrt,
    cbrt,
    exp,
    exp2,
    exp_m1,
    ln,
    ln_1p,
    to_degrees,
    to_radians,
    is_nan,
    is_infinite,
    is_finite,
    is_subnormal,
    is_normal,

    // Array functions
    array_new,
    array_with_capacity,
    array_length,
    array_index,
    array_capacity,
    array_reserve,
    array_shrink_to_fit,
    array_shrink_to,
    array_truncate,
    array_swap_remove,
    array_insert,
    array_remove,
    array_push,
    array_pop,
    array_contains,

    // String functions
    string_new,
    string_from,
    string_with_capacity,
    string_length,
    string_index_byte,
    string_index_char,
    string_capacity,
    string_reserve,
    string_shrink_to_fit,
    string_shrink_to,
    string_truncate,
    string_insert,
    string_remove,
    string_push,
    string_pop,
    string_contains,

    // Dictionary functions
    dictionary_new,
    dictionary_with_capacity,
    dictionary_length,
    dictionary_index,
    dictionary_capacity,
    dictionary_reserve,
    dictionary_shrink_to_fit,
    dictionary_shrink_to,
    dictionary_insert,
    dictionary_remove,
    dictionary_contains,

    // I/O
    print,
    println,
}

fn get_panic_message(vm: &mut Vm, frame: &CallFrame) -> String {
    match vm.pop_value(frame) {
        Ok(Value::Int(v)) => v.to_string(),
        Ok(Value::Float(v)) => v.to_string(),
        Ok(Value::Bool(v)) => v.to_string(),
        Ok(Value::Char(v)) => v.to_string(),
        Ok(Value::Symbol(v)) => vm.module.symbols.get(v).unwrap_or("<no message>").into(),
        Ok(Value::Function(v)) => format!("{:x}", v.0),
        Ok(Value::Reference(_)) => todo!("reference to_string"),
        Err(_) => "<no message>".into(),
    }
}

//////////////////////////////////////////
// ============== PANICS ============== //
//////////////////////////////////////////

fn vmbi_assert(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let message = get_panic_message(vm, frame);
    let condition = vm.pop_bool(frame)?;

    if !condition {
        VmPanic::from(format!("assertion failure: {message}")).panic();
    }

    Ok(())
}

fn vmbi_unreachable(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let message: String = get_panic_message(vm, frame);

    VmPanic::from(format!(
        "internal error: entered unreachable code: {message}"
    ))
    .panic();
}

fn vmbi_panic(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let message = get_panic_message(vm, frame);
    VmPanic::new(message).panic();
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
        Value::Int(v) => Value::Int(v.abs()),
        Value::Float(v) => Value::Float(v.abs()),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *top = value;
    Ok(())
}

fn vmbi_wrapping_abs(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::Int(v) => Value::Int(v.wrapping_abs()),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *top = result;
    Ok(())
}

fn vmbi_saturating_abs(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::Int(v) => Value::Int(v.saturating_abs()),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *top = result;
    Ok(())
}

fn vmbi_overflowing_abs(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let (result, flag) = match top {
        Value::Int(v) => {
            let (val, flag) = v.overflowing_abs();
            (Value::Int(val), flag)
        }
        _ => throw!(VmErrorKind::Type, frame),
    };

    *top = result;
    vm.push_value(Value::Bool(flag), frame)?;
    Ok(())
}

fn vmbi_wrapping_rem(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let lhs = vm.pop_value(frame)?;
    let rhs = vm.top_value_mut(frame)?;

    let result = match (lhs, &rhs) {
        (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_rem(*b)),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *rhs = result;
    Ok(())
}

fn vmbi_overflowing_rem(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let lhs = vm.pop_value(frame)?;
    let rhs = vm.top_value_mut(frame)?;

    let (result, flag) = match (lhs, &rhs) {
        (Value::Int(a), Value::Int(b)) => {
            let (val, flag) = a.overflowing_rem(*b);
            (Value::Int(val), flag)
        }
        _ => throw!(VmErrorKind::Type, frame),
    };

    *rhs = result;
    vm.push_value(Value::Bool(flag), frame)?;
    Ok(())
}

fn vmbi_wrapping_neg(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let Value::Int(top) = vm.top_value_mut(frame)? else {
        throw!(VmErrorKind::Type, frame);
    };

    let result = top.wrapping_neg();

    *top = result;
    Ok(())
}

fn vmbi_saturating_neg(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let Value::Int(top) = vm.top_value_mut(frame)? else {
        throw!(VmErrorKind::Type, frame);
    };

    let result = top.saturating_neg();

    *top = result;
    Ok(())
}

fn vmbi_overflowing_neg(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let Value::Int(top) = vm.top_value_mut(frame)? else {
        throw!(VmErrorKind::Type, frame);
    };

    let (result, flag) = top.overflowing_neg();

    *top = result;
    vm.push_value(Value::Bool(flag), frame)?;
    Ok(())
}

// The normal pow function is more complex than a normal instruction, since it has different
// behaviors if the exponent is an unsigned, signed, or floating point number.
fn vmbi_pow(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    // Integer powers (works in the range 0..=u32::MAX for integers and the range
    // i32::MIN..=i32::MAX for floats).
    let base = vm.pop_value(frame)?;
    let exponent = vm.top_value_mut(frame)?;

    let result = match base {
        Value::Int(base) => {
            let &mut Value::Int(exponent) = exponent else {
                throw!(VmErrorKind::Type, frame);
            };

            let exp: u32 = exponent
                .try_into()
                .vm_result(VmErrorKind::Arithmetic, frame)?;

            let result = base
                .checked_pow(exp)
                .vm_result(VmErrorKind::Arithmetic, frame)?;

            Value::Int(result)
        }
        Value::Float(exp) => {
            let Value::Float(v) = exponent else {
                throw!(VmErrorKind::Type, frame);
            };

            Value::Float(v.powf(exp))
        }
        _ => throw!(VmErrorKind::Type, frame),
    };

    *exponent = result;
    Ok(())
}

fn vmbi_wrapping_pow(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_int(frame)?;
    let exponent = vm.top_value_mut(frame)?;

    let exp = match exponent {
        Value::Int(v) => u32::try_from(*v).vm_result(VmErrorKind::Arithmetic, frame)?,
        _ => throw!(VmErrorKind::Type, frame),
    };

    *exponent = this.wrapping_pow(exp).into();
    Ok(())
}

fn vmbi_saturating_pow(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_int(frame)?;
    let exponent = vm.top_value_mut(frame)?;

    let exp = match exponent {
        Value::Int(v) => u32::try_from(*v).vm_result(VmErrorKind::Arithmetic, frame)?,
        _ => throw!(VmErrorKind::Type, frame),
    };

    *exponent = this.saturating_pow(exp).into();
    Ok(())
}

fn vmbi_overflowing_pow(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_int(frame)?;
    let exponent = vm.top_value_mut(frame)?;

    let exp = match exponent {
        Value::Int(v) => u32::try_from(*v).vm_result(VmErrorKind::Arithmetic, frame)?,
        _ => throw!(VmErrorKind::Type, frame),
    };

    let (result, flag) = this.overflowing_pow(exp);

    *exponent = result.into();
    vm.push_value(flag.into(), frame)?;
    Ok(())
}

fn vmbi_log(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let arg = vm.pop_value(frame)?;
    let base = vm.top_value_mut(frame)?;

    let result = match (arg, &base) {
        (Value::Int(a), Value::Int(b)) => {
            let result = a
                .checked_ilog(*b)
                .vm_result(VmErrorKind::Arithmetic, frame)?;
            Value::Int(result as i64)
        }
        (Value::Float(a), Value::Float(b)) => Value::Float(a.log(*b)),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *base = result;
    Ok(())
}

fn vmbi_log2(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::Int(a) => {
            let result = a
                .checked_ilog2()
                .vm_result(VmErrorKind::Arithmetic, frame)?;
            Value::Int(result as i64)
        }
        Value::Float(a) => Value::Float(a.log2()),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *top = result;
    Ok(())
}

fn vmbi_log10(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::Int(a) => {
            let result = a
                .checked_ilog10()
                .vm_result(VmErrorKind::Arithmetic, frame)?;
            Value::Int(result as i64)
        }
        Value::Float(a) => Value::Float(a.log10()),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *top = result;
    Ok(())
}

fn vmbi_signum(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::Int(a) => Value::Int(a.signum()),
        Value::Float(a) => Value::Float(a.signum()),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *top = result;
    Ok(())
}

fn vmbi_is_positive(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::Int(a) => Value::Bool(a.is_positive()),
        Value::Float(a) => Value::Bool(a.is_sign_positive()),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *top = result;
    Ok(())
}

fn vmbi_is_negative(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let top = vm.top_value_mut(frame)?;

    let result = match top {
        Value::Int(a) => Value::Bool(a.is_negative()),
        Value::Float(a) => Value::Bool(a.is_sign_negative()),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *top = result;
    Ok(())
}

///////////////////////////////////////////
// ============== MIN/MAX ============== //
///////////////////////////////////////////

fn vmbi_min(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_value(frame)?;
    let other = vm.top_value_mut(frame)?;

    let result = match (this, &other) {
        (Value::Int(a), Value::Int(b)) => Value::Int(a.min(*b)),
        (Value::Float(a), Value::Float(b)) => Value::Float(a.min(*b)),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *other = result;
    Ok(())
}

fn vmbi_max(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_value(frame)?;
    let other = vm.top_value_mut(frame)?;

    let result = match (this, &other) {
        (Value::Int(a), Value::Int(b)) => Value::Int(a.max(*b)),
        (Value::Float(a), Value::Float(b)) => Value::Float(a.max(*b)),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *other = result;
    Ok(())
}

fn vmbi_clamp(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_value(frame)?;
    let min = vm.pop_value(frame)?;
    let max = vm.top_value_mut(frame)?;

    let result = match (this, min, &max) {
        (Value::Int(a), Value::Int(min), Value::Int(max)) => Value::Int(a.clamp(min, *max)),
        (Value::Float(a), Value::Float(min), Value::Float(max)) => Value::Float(a.clamp(min, *max)),
        _ => throw!(VmErrorKind::Type, frame),
    };

    *max = result;
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
}

float_to_bool_intrinsics! {
    is_nan,
    is_infinite,
    is_finite,
    is_subnormal,
    is_normal,
}

fn vmbi_atan2(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_value(frame)?;
    let arg = vm.top_value_mut(frame)?;

    let Value::Float(arg1) = this else {
        throw!(VmErrorKind::Type, frame);
    };

    let Value::Float(arg2) = arg else {
        throw!(VmErrorKind::Type, frame);
    };

    let result = arg1.atan2(*arg2);
    *arg2 = result;
    Ok(())
}

/////////////////////////////////////////
// ============== ARRAY ============== //
/////////////////////////////////////////

fn vmbi_array_new(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.alloc(VmArray::new(), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_array_with_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    // Using isize::MAX as a maximum bound ensures that the capac'ity cannot exceed the bounds
    // of usize, so the `as` casts here are fine. This is also a degenerate case that will
    // likely cause an OOM error in Rust anyway.
    let capacity: usize = vm
        .pop_int(frame)?
        .min(isize::MAX as i64)
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;
    let this_ref = vm.alloc(VmArray::with_capacity(capacity), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_array_length(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let len = vm.heap_object::<VmArray>(this_ref, frame)?.len();

    vm.push_value(Value::Int(len as i64), frame)?;
    Ok(())
}

fn vmbi_array_index(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let index: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    let value = vm
        .heap_object::<VmArray>(this_ref, frame)?
        .get(index)
        .copied()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(value, frame)?;
    Ok(())
}

fn vmbi_array_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let capacity = vm.heap_object::<VmArray>(this, frame)?.capacity();

    // Casting to i64 is fine here since Rust limits Vecs to a capacity of isize::MAX bytes, which
    // can never exceed i64::MAX bytes
    vm.push_value(Value::Int(capacity as i64), frame)?;
    Ok(())
}

fn vmbi_array_reserve(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let additional: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<VmArray>(this, frame)?
        .reserve(additional);

    Ok(())
}

fn vmbi_array_shrink_to_fit(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    vm.heap_object_mut::<VmArray>(this, frame)?.shrink_to_fit();

    Ok(())
}

fn vmbi_array_shrink_to(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let min_capacity = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<VmArray>(this, frame)?
        .shrink_to(min_capacity);

    Ok(())
}

fn vmbi_array_truncate(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let len = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<VmArray>(this, frame)?.truncate(len);

    Ok(())
}

fn vmbi_array_swap_remove(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let idx: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    let mut this = vm.heap_object_mut::<VmArray>(this_ref, frame)?;

    if idx < this.len() {
        let value = this.swap_remove(idx);
        drop(this);

        vm.push_value(value, frame)?;
        Ok(())
    } else {
        throw!(VmErrorKind::OutOfBounds, frame);
    }
}

fn vmbi_array_insert(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let idx: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;
    let value = vm.pop_value(frame)?;

    let mut this = vm.heap_object_mut::<VmArray>(this_ref, frame)?;

    if idx <= this.len() {
        this.insert(idx, value);
        Ok(())
    } else {
        throw!(VmErrorKind::OutOfBounds, frame)
    }
}

fn vmbi_array_remove(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let idx: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    let mut this = vm.heap_object_mut::<VmArray>(this_ref, frame)?;

    if idx < this.len() {
        let value = this.remove(idx);
        drop(this);

        vm.push_value(value, frame)?;
        Ok(())
    } else {
        throw!(VmErrorKind::OutOfBounds, frame)
    }
}

fn vmbi_array_push(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let value = vm.pop_value(frame)?;

    vm.heap_object_mut::<VmArray>(this_ref, frame)?.push(value);

    Ok(())
}

fn vmbi_array_pop(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;

    let value = vm
        .heap_object_mut::<VmArray>(this_ref, frame)?
        .pop()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(value, frame)?;
    Ok(())
}

fn vmbi_array_contains(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let value = vm.pop_value(frame)?;

    let contains = vm
        .heap_object_mut::<VmArray>(this_ref, frame)?
        .contains(&value);

    vm.push_value(Value::Bool(contains), frame)?;
    Ok(())
}

//////////////////////////////////////////
// ============== STRING ============== //
//////////////////////////////////////////

fn vmbi_string_new(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.alloc(VmString::new(), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_string_from(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let string = match vm.pop_value(frame)? {
        Value::Int(v) => v.to_string(),
        Value::Float(v) => v.to_string(),
        Value::Bool(v) => v.to_string(),
        Value::Char(v) => v.to_string(),
        Value::Function(v) => format!("{:x}", v.0),
        Value::Symbol(v) => vm.module.symbols.get(v).unwrap_or("").into(),
        Value::Reference(_v) => todo!("reference to_string"),
    };

    let this_ref = vm.alloc(VmString::from(string), frame)?;
    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_string_with_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let capacity: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;
    let this_ref = vm.alloc(VmString::with_capacity(capacity), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_string_length(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let len = vm.heap_object::<VmString>(this_ref, frame)?.len();

    vm.push_value(Value::Int(len as i64), frame)?;
    Ok(())
}

fn vmbi_string_index_byte(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let index: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    let value = vm
        .heap_object::<VmString>(this_ref, frame)?
        .as_bytes()
        .get(index)
        .copied()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(Value::Int(value as i64), frame)?;
    Ok(())
}

fn vmbi_string_index_char(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let index: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    let value = vm
        .heap_object::<VmString>(this_ref, frame)?
        .chars()
        .nth(index)
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(Value::Char(value), frame)?;
    Ok(())
}

fn vmbi_string_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let capacity = vm.heap_object::<VmString>(this, frame)?.capacity();

    // Casting to i64 is fine here since Rust limits Vecs to a capacity of isize::MAX bytes, which
    // can never exceed i64::MAX bytes
    vm.push_value(Value::Int(capacity as i64), frame)?;
    Ok(())
}

fn vmbi_string_reserve(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let additional: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<VmString>(this, frame)?
        .reserve(additional);

    Ok(())
}

fn vmbi_string_shrink_to_fit(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    vm.heap_object_mut::<VmString>(this, frame)?.shrink_to_fit();

    Ok(())
}

fn vmbi_string_shrink_to(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let min_capacity = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<VmString>(this, frame)?
        .shrink_to(min_capacity);

    Ok(())
}

fn vmbi_string_truncate(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let len = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<VmString>(this, frame)?.truncate(len);

    Ok(())
}

fn vmbi_string_insert(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let idx: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;
    let value = vm.pop_char(frame)?;

    let mut this = vm.heap_object_mut::<VmString>(this_ref, frame)?;

    if idx <= this.len() {
        this.insert(idx, value);
        Ok(())
    } else {
        throw!(VmErrorKind::OutOfBounds, frame)
    }
}

fn vmbi_string_remove(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let idx: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    let mut this = vm.heap_object_mut::<VmString>(this_ref, frame)?;

    if idx < this.len() {
        let value = this.remove(idx);
        drop(this);

        vm.push_value(Value::Char(value), frame)?;
        Ok(())
    } else {
        throw!(VmErrorKind::OutOfBounds, frame)
    }
}

fn vmbi_string_push(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let value = vm.pop_value(frame)?;

    let mut this = vm.heap_object_mut::<VmString>(this_ref, frame)?;

    match value {
        Value::Char(v) => this.push(v),
        Value::Symbol(v) => {
            let s = vm
                .module
                .symbols
                .get(v)
                .vm_result(VmErrorKind::SymbolNotFound, frame)?;
            this.push_str(s);
        }
        Value::Reference(v) => todo!(),
        _ => throw!(VmErrorKind::Type, frame),
    }

    Ok(())
}

fn vmbi_string_pop(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;

    let value = vm
        .heap_object_mut::<VmString>(this_ref, frame)?
        .pop()
        .vm_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(Value::Char(value), frame)?;
    Ok(())
}

fn vmbi_string_contains(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let value = vm.pop_value(frame)?;

    let contains = {
        let this = vm.heap_object::<VmString>(this_ref, frame)?;

        match value {
            Value::Char(v) => this.contains(v),
            Value::Symbol(v) => {
                let s = vm
                    .module
                    .symbols
                    .get(v)
                    .vm_result(VmErrorKind::SymbolNotFound, frame)?;
                this.contains(s)
            }
            Value::Reference(v) => todo!(),
            _ => throw!(VmErrorKind::Type, frame),
        }
    };

    vm.push_value(Value::Bool(contains), frame)?;

    Ok(())
}

//////////////////////////////////////////////
// ============== DICTIONARY ============== //
//////////////////////////////////////////////

fn vmbi_dictionary_new(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.alloc(VmDictionary::new(), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_dictionary_with_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    // Using isize::MAX as a maximum bound ensures that the capac'ity cannot exceed the bounds
    // of usize, so the `as` casts here are fine. This is also a degenerate case that will
    // likely cause an OOM error in Rust anyway.
    let capacity: usize = vm
        .pop_int(frame)?
        .max(isize::MAX as i64)
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;
    let this_ref = vm.alloc(VmDictionary::with_capacity(capacity), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_dictionary_length(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let len = vm.heap_object::<VmDictionary>(this_ref, frame)?.len();

    vm.push_value(Value::Int(len as i64), frame)?;
    Ok(())
}

fn vmbi_dictionary_index(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let value = {
        let this_ref = vm.pop_reference(frame)?;
        let key_ref = vm.pop_reference(frame)?;

        let key = vm.heap_object::<VmString>(key_ref, frame)?;

        vm.heap_object::<VmDictionary>(this_ref, frame)?
            .get(key.as_str())
            .copied()
            .vm_result(VmErrorKind::OutOfBounds, frame)?
    };

    vm.push_value(value, frame)?;
    Ok(())
}

fn vmbi_dictionary_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let capacity = vm.heap_object::<VmDictionary>(this, frame)?.capacity();

    // Casting to i64 is fine here since Rust limits Vecs to a capacity of isize::MAX bytes, which
    // can never exceed i64::MAX bytes
    vm.push_value(Value::Int(capacity as i64), frame)?;
    Ok(())
}

fn vmbi_dictionary_reserve(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let additional: usize = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<VmDictionary>(this, frame)?
        .reserve(additional);

    Ok(())
}

fn vmbi_dictionary_shrink_to_fit(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    vm.heap_object_mut::<VmDictionary>(this, frame)?
        .shrink_to_fit();

    Ok(())
}

fn vmbi_dictionary_shrink_to(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let min_capacity = vm
        .pop_int(frame)?
        .try_into()
        .vm_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<VmDictionary>(this, frame)?
        .shrink_to(min_capacity);

    Ok(())
}

fn vmbi_dictionary_insert(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let key = match vm.pop_value(frame)? {
        Value::Char(v) => {
            // Arguably chars shouldn't be allowed to be converted, but chars are essentially
            // one-character strings, so it's not that far-fetched to do an implicit cast here.
            let mut buf = [0u8; 4];
            let s = &*v.encode_utf8(&mut buf);

            Arc::from(s)
        }
        Value::Symbol(symbol) => {
            let s = vm
                .module
                .symbols
                .get(symbol)
                .vm_result(VmErrorKind::SymbolNotFound, frame)?;

            Arc::from(s)
        }
        Value::Reference(v) => {
            let s = vm.heap_object::<VmString>(v, frame)?;

            Arc::from(s.as_str())
        }
        _ => throw!(VmErrorKind::Type, frame),
    };
    let value = vm.pop_value(frame)?;

    vm.heap_object_mut::<VmDictionary>(this_ref, frame)?
        .insert(key, value);

    Ok(())
}

fn vmbi_dictionary_remove(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let key_ref = vm.pop_reference(frame)?;

    let value = {
        let mut this = vm.heap_object_mut::<VmDictionary>(this_ref, frame)?;
        let key = vm.heap_object::<VmString>(key_ref, frame)?;

        this.remove(key.as_str())
            .vm_result(VmErrorKind::OutOfBounds, frame)?
    };

    vm.push_value(value, frame)?;

    Ok(())
}

fn vmbi_dictionary_contains(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let key_ref = vm.pop_reference(frame)?;

    let contains = {
        let this = vm.heap_object::<VmDictionary>(this_ref, frame)?;
        let key = vm.heap_object::<VmString>(key_ref, frame)?;

        this.contains_key(key.as_str())
    };

    vm.push_value(Value::Bool(contains), frame)?;

    Ok(())
}

///////////////////////////////////////
// ============== I/O ============== //
///////////////////////////////////////

fn vmbi_print(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let string_ref = vm.pop_reference(frame)?;

    let value = vm.heap_object::<VmString>(string_ref, frame)?;

    print!("{}", value.as_str());
    Ok(())
}

fn vmbi_println(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let string_ref = vm.pop_reference(frame)?;

    let value = vm.heap_object::<VmString>(string_ref, frame)?;

    println!("{}", value.as_str());
    Ok(())
}

///////////////////////////////////////////////////
// ============== DYNAMIC LOADING ============== //
///////////////////////////////////////////////////

fn vmbi_load_module(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    todo!("loading modules");
}
