use std::{hash::Hasher, sync::Arc};

use crate::{
    module::core::collections::{Array, Dict, Set, Str},
    utils::IntoVmResult,
    value::{EqValue, Value},
};

use super::{exception::ExceptionPayload, CallFrame, Result, Vm, VmExceptionPayload, VmPanic};
use crate::throw;
use paste::paste;

macro_rules! float_intrinsics {
    ($($name:ident),* $(,)?) => {
        $(
            paste! {
                fn [<vmbi_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let top = match vm.top_value_mut(frame)? {
                        Value::Float(v) => v,
                        value => throw!(VmExceptionPayload::Type {
                            expected: Value::FLOAT_TYPE_NAME.into(),
                            actual: value.type_name().into()
                        }
                        .into_exception()
                        .with_frame(frame.clone())),
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

                    let arg = match top {
                        Value::Float(v) => v,
                        value => throw!(VmExceptionPayload::Type {
                            expected: Value::FLOAT_TYPE_NAME.into(),
                            actual: value.type_name().into()
                        }
                        .into_exception()
                        .with_frame(frame.clone())),
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
                        value => throw!(VmExceptionPayload::Type {
                            expected: Value::INT_TYPE_NAME.into(),
                            actual: value.type_name().into()
                        }
                        .into_exception()
                        .with_frame(frame.clone())),
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
                        value => throw!(VmExceptionPayload::Type {
                            expected: Value::INT_TYPE_NAME.into(),
                            actual: value.type_name().into()
                        }
                        .into_exception()
                        .with_frame(frame.clone())),
                    };

                    *rhs = result;
                    Ok(())
                }

                fn [<vmbi_saturating_ $name>](vm: &mut Vm, frame: &CallFrame) -> Result<()> {
                    let lhs = vm.pop_value(frame)?;
                    let rhs = vm.top_value_mut(frame)?;

                    let result = match (lhs, &rhs) {
                        (Value::Int(a), Value::Int(b))  => Value::Int(a.[<saturating_ $name>](*b)),
                        _ => throw!(VmExceptionPayload::Type {
                            expected: Value::INT_TYPE_NAME.into(),
                            actual: value.type_name().into()
                        }
                        .into_exception()
                        .with_frame(frame.clone())),
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
    str_new,
    str_from,
    str_with_capacity,
    str_length,
    str_index_byte,
    str_index_char,
    str_capacity,
    str_reserve,
    str_shrink_to_fit,
    str_shrink_to,
    str_truncate,
    str_insert,
    str_remove,
    str_push,
    str_pop,
    str_contains,

    // Dictionary functions
    dict_new,
    dict_with_capacity,
    dict_length,
    dict_index,
    dict_capacity,
    dict_reserve,
    dict_shrink_to_fit,
    dict_shrink_to,
    dict_insert,
    dict_remove,
    dict_contains,

    // Set functions
    set_new,
    set_with_capacity,
    set_length,
    set_index,
    set_capacity,
    set_reserve,
    set_shrink_to_fit,
    set_shrink_to,
    set_insert,
    set_remove,
    set_contains,
    set_is_disjoint,
    set_difference,
    set_symmetric_difference,
    set_intersection,
    set_union,

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
                .exception_result(VmErrorKind::Arithmetic, frame)?;

            let result = base
                .checked_pow(exp)
                .exception_result(VmErrorKind::Arithmetic, frame)?;

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
        Value::Int(v) => u32::try_from(*v).exception_result(VmErrorKind::Arithmetic, frame)?,
        _ => throw!(VmErrorKind::Type, frame),
    };

    *exponent = this.wrapping_pow(exp).into();
    Ok(())
}

fn vmbi_saturating_pow(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_int(frame)?;
    let exponent = vm.top_value_mut(frame)?;

    let exp = match exponent {
        Value::Int(v) => u32::try_from(*v).exception_result(VmErrorKind::Arithmetic, frame)?,
        _ => throw!(VmErrorKind::Type, frame),
    };

    *exponent = this.saturating_pow(exp).into();
    Ok(())
}

fn vmbi_overflowing_pow(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_int(frame)?;
    let exponent = vm.top_value_mut(frame)?;

    let exp = match exponent {
        Value::Int(v) => u32::try_from(*v).exception_result(VmErrorKind::Arithmetic, frame)?,
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
                .exception_result(VmErrorKind::Arithmetic, frame)?;
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
                .exception_result(VmErrorKind::Arithmetic, frame)?;
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
                .exception_result(VmErrorKind::Arithmetic, frame)?;
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
    let this_ref = vm.alloc(Array::new(), frame)?;

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
        .exception_result(VmErrorKind::InvalidSize, frame)?;
    let this_ref = vm.alloc(Array::with_capacity(capacity), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_array_length(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let len = vm.heap_object::<Array>(this_ref, frame)?.len();

    vm.push_value(Value::Int(len as i64), frame)?;
    Ok(())
}

fn vmbi_array_index(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let index: usize = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    let value = vm
        .heap_object::<Array>(this_ref, frame)?
        .get(index)
        .copied()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(value, frame)?;
    Ok(())
}

fn vmbi_array_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let capacity = vm.heap_object::<Array>(this, frame)?.capacity();

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
        .exception_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<Array>(this, frame)?
        .reserve(additional);

    Ok(())
}

fn vmbi_array_shrink_to_fit(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    vm.heap_object_mut::<Array>(this, frame)?.shrink_to_fit();

    Ok(())
}

fn vmbi_array_shrink_to(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let min_capacity = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<Array>(this, frame)?
        .shrink_to(min_capacity);

    Ok(())
}

fn vmbi_array_truncate(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let len = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<Array>(this, frame)?.truncate(len);

    Ok(())
}

fn vmbi_array_swap_remove(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let idx: usize = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    let mut this = vm.heap_object_mut::<Array>(this_ref, frame)?;

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
        .exception_result(VmErrorKind::OutOfBounds, frame)?;
    let value = vm.pop_value(frame)?;

    let mut this = vm.heap_object_mut::<Array>(this_ref, frame)?;

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
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    let mut this = vm.heap_object_mut::<Array>(this_ref, frame)?;

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

    vm.heap_object_mut::<Array>(this_ref, frame)?.push(value);

    Ok(())
}

fn vmbi_array_pop(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;

    let value = vm
        .heap_object_mut::<Array>(this_ref, frame)?
        .pop()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(value, frame)?;
    Ok(())
}

fn vmbi_array_contains(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let value = vm.pop_value(frame)?;

    let contains = vm
        .heap_object_mut::<Array>(this_ref, frame)?
        .contains(&value);

    vm.push_value(Value::Bool(contains), frame)?;
    Ok(())
}

///////////////////////////////////////
// ============== STR ============== //
///////////////////////////////////////

fn vmbi_str_new(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.alloc(Str::new(), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_str_from(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let string = match vm.pop_value(frame)? {
        Value::Int(v) => v.to_string(),
        Value::Float(v) => v.to_string(),
        Value::Bool(v) => v.to_string(),
        Value::Char(v) => v.to_string(),
        Value::Function(v) => format!("{:x}", v.0),
        Value::Symbol(v) => vm.module.symbols.get(v).unwrap_or("").into(),
        Value::Reference(_v) => todo!("reference to_string"),
    };

    let this_ref = vm.alloc(Str::from(string), frame)?;
    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_str_with_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let capacity: usize = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;
    let this_ref = vm.alloc(Str::with_capacity(capacity), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_str_length(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let len = vm.heap_object::<Str>(this_ref, frame)?.len();

    vm.push_value(Value::Int(len as i64), frame)?;
    Ok(())
}

fn vmbi_str_index_byte(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let index: usize = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    let value = vm
        .heap_object::<Str>(this_ref, frame)?
        .as_bytes()
        .get(index)
        .copied()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(Value::Int(value as i64), frame)?;
    Ok(())
}

fn vmbi_str_index_char(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let index: usize = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    let value = vm
        .heap_object::<Str>(this_ref, frame)?
        .chars()
        .nth(index)
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(Value::Char(value), frame)?;
    Ok(())
}

fn vmbi_str_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let capacity = vm.heap_object::<Str>(this, frame)?.capacity();

    // Casting to i64 is fine here since Rust limits Vecs to a capacity of isize::MAX bytes, which
    // can never exceed i64::MAX bytes
    vm.push_value(Value::Int(capacity as i64), frame)?;
    Ok(())
}

fn vmbi_str_reserve(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let additional: usize = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<Str>(this, frame)?.reserve(additional);

    Ok(())
}

fn vmbi_str_shrink_to_fit(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    vm.heap_object_mut::<Str>(this, frame)?.shrink_to_fit();

    Ok(())
}

fn vmbi_str_shrink_to(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let min_capacity = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<Str>(this, frame)?
        .shrink_to(min_capacity);

    Ok(())
}

fn vmbi_str_truncate(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let len = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<Str>(this, frame)?.truncate(len);

    Ok(())
}

fn vmbi_str_insert(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let idx: usize = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;
    let value = vm.pop_char(frame)?;

    let mut this = vm.heap_object_mut::<Str>(this_ref, frame)?;

    if idx <= this.len() {
        this.insert(idx, value);
        Ok(())
    } else {
        throw!(VmErrorKind::OutOfBounds, frame)
    }
}

fn vmbi_str_remove(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let idx: usize = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    let mut this = vm.heap_object_mut::<Str>(this_ref, frame)?;

    if idx < this.len() {
        let value = this.remove(idx);
        drop(this);

        vm.push_value(Value::Char(value), frame)?;
        Ok(())
    } else {
        throw!(VmErrorKind::OutOfBounds, frame)
    }
}

fn vmbi_str_push(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let value = vm.pop_value(frame)?;

    let mut this = vm.heap_object_mut::<Str>(this_ref, frame)?;

    match value {
        Value::Char(v) => this.push(v),
        Value::Symbol(v) => {
            let s = vm
                .module
                .symbols
                .get(v)
                .exception_result(VmErrorKind::SymbolNotFound, frame)?;
            this.push_str(s);
        }
        Value::Reference(v) => todo!(),
        _ => throw!(VmErrorKind::Type, frame),
    }

    Ok(())
}

fn vmbi_str_pop(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;

    let value = vm
        .heap_object_mut::<Str>(this_ref, frame)?
        .pop()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(Value::Char(value), frame)?;
    Ok(())
}

fn vmbi_str_contains(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let value = vm.pop_value(frame)?;

    let contains = {
        let this = vm.heap_object::<Str>(this_ref, frame)?;

        match value {
            Value::Char(v) => this.contains(v),
            Value::Symbol(v) => {
                let s = vm
                    .module
                    .symbols
                    .get(v)
                    .exception_result(VmErrorKind::SymbolNotFound, frame)?;
                this.contains(s)
            }
            Value::Reference(v) => todo!(),
            _ => throw!(VmErrorKind::Type, frame),
        }
    };

    vm.push_value(Value::Bool(contains), frame)?;

    Ok(())
}

////////////////////////////////////////
// ============== DICT ============== //
////////////////////////////////////////

fn vmbi_dict_new(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.alloc(Dict::new(), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_dict_with_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    // Using isize::MAX as a maximum bound ensures that the capac'ity cannot exceed the bounds
    // of usize, so the `as` casts here are fine. This is also a degenerate case that will
    // likely cause an OOM error in Rust anyway.
    let capacity: usize = vm
        .pop_int(frame)?
        .max(isize::MAX as i64)
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;
    let this_ref = vm.alloc(Dict::with_capacity(capacity), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_dict_length(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let len = vm.heap_object::<Dict>(this_ref, frame)?.len();

    vm.push_value(Value::Int(len as i64), frame)?;
    Ok(())
}

fn vmbi_dict_index(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let value = {
        let this_ref = vm.pop_reference(frame)?;
        let key_ref = vm.pop_reference(frame)?;

        let key = vm.heap_object::<Str>(key_ref, frame)?;

        vm.heap_object::<Dict>(this_ref, frame)?
            .get(key.as_str())
            .copied()
            .exception_result(VmErrorKind::OutOfBounds, frame)?
    };

    vm.push_value(value, frame)?;
    Ok(())
}

fn vmbi_dict_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let capacity = vm.heap_object::<Dict>(this, frame)?.capacity();

    // Casting to i64 is fine here since Rust limits Vecs to a capacity of isize::MAX bytes, which
    // can never exceed i64::MAX bytes
    vm.push_value(Value::Int(capacity as i64), frame)?;
    Ok(())
}

fn vmbi_dict_reserve(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let additional: usize = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<Dict>(this, frame)?.reserve(additional);

    Ok(())
}

fn vmbi_dict_shrink_to_fit(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    vm.heap_object_mut::<Dict>(this, frame)?.shrink_to_fit();

    Ok(())
}

fn vmbi_dict_shrink_to(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let min_capacity = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<Dict>(this, frame)?
        .shrink_to(min_capacity);

    Ok(())
}

fn vmbi_dict_insert(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
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
                .exception_result(VmErrorKind::SymbolNotFound, frame)?;

            Arc::from(s)
        }
        Value::Reference(v) => {
            let s = vm.heap_object::<Str>(v, frame)?;

            Arc::from(s.as_str())
        }
        _ => throw!(VmErrorKind::Type, frame),
    };
    let value = vm.pop_value(frame)?;

    vm.heap_object_mut::<Dict>(this_ref, frame)?
        .insert(key, value);

    Ok(())
}

fn vmbi_dict_remove(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let key_ref = vm.pop_reference(frame)?;

    let value = {
        let mut this = vm.heap_object_mut::<Dict>(this_ref, frame)?;
        let key = vm.heap_object::<Str>(key_ref, frame)?;

        this.remove(key.as_str())
            .exception_result(VmErrorKind::OutOfBounds, frame)?
    };

    vm.push_value(value, frame)?;

    Ok(())
}

fn vmbi_dict_contains(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let key_ref = vm.pop_reference(frame)?;

    let contains = {
        let this = vm.heap_object::<Dict>(this_ref, frame)?;
        let key = vm.heap_object::<Str>(key_ref, frame)?;

        this.contains_key(key.as_str())
    };

    vm.push_value(Value::Bool(contains), frame)?;

    Ok(())
}

///////////////////////////////////////
// ============== SET ============== //
///////////////////////////////////////

fn vmbi_set_new(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.alloc(Set::new(), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_set_with_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    // Using isize::MAX as a maximum bound ensures that the capac'ity cannot exceed the bounds
    // of usize, so the `as` casts here are fine. This is also a degenerate case that will
    // likely cause an OOM error in Rust anyway.
    let capacity: usize = vm
        .pop_int(frame)?
        .max(isize::MAX as i64)
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;
    let this_ref = vm.alloc(Set::with_capacity(capacity), frame)?;

    vm.push_value(Value::Reference(this_ref), frame)?;
    Ok(())
}

fn vmbi_set_length(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let len = vm.heap_object::<Set>(this_ref, frame)?.len();

    vm.push_value(Value::Int(len as i64), frame)?;
    Ok(())
}

fn vmbi_set_index(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let key = vm.pop_value(frame)?;

    let value = vm
        .heap_object::<Set>(this_ref, frame)?
        .get(&EqValue::from(key))
        .copied()
        .exception_result(VmErrorKind::OutOfBounds, frame)?;

    vm.push_value(value.into(), frame)?;
    Ok(())
}

fn vmbi_set_capacity(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let capacity = vm.heap_object::<Set>(this, frame)?.capacity();

    // Casting to i64 is fine here since Rust limits Vecs to a capacity of isize::MAX bytes, which
    // can never exceed i64::MAX bytes
    vm.push_value(Value::Int(capacity as i64), frame)?;
    Ok(())
}

fn vmbi_set_reserve(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let additional: usize = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<Set>(this, frame)?.reserve(additional);

    Ok(())
}

fn vmbi_set_shrink_to_fit(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    vm.heap_object_mut::<Set>(this, frame)?.shrink_to_fit();

    Ok(())
}

fn vmbi_set_shrink_to(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this = vm.pop_reference(frame)?;
    let min_capacity = vm
        .pop_int(frame)?
        .try_into()
        .exception_result(VmErrorKind::InvalidSize, frame)?;

    vm.heap_object_mut::<Set>(this, frame)?
        .shrink_to(min_capacity);

    Ok(())
}

fn vmbi_set_insert(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let key = vm.pop_value(frame)?;

    if matches!(key, Value::Float(v) if v.is_nan()) {
        throw!(VmErrorKind::InvalidReference, frame)
    }

    let inserted = vm
        .heap_object_mut::<Set>(this_ref, frame)?
        .insert(key.into());

    vm.push_value(inserted.into(), frame)?;

    Ok(())
}

fn vmbi_set_remove(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let key = vm.pop_value(frame)?;

    let removed = vm
        .heap_object_mut::<Set>(this_ref, frame)?
        .remove(&EqValue::from(key));

    vm.push_value(removed.into(), frame)?;

    Ok(())
}

fn vmbi_set_contains(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let key = vm.pop_value(frame)?;

    let contains = vm
        .heap_object::<Set>(this_ref, frame)?
        .contains(&EqValue::from(key));

    vm.push_value(Value::Bool(contains), frame)?;

    Ok(())
}

fn vmbi_set_is_disjoint(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let other_ref = vm.pop_reference(frame)?;

    let is_disjoint = {
        let this = vm.heap_object::<Set>(this_ref, frame)?;
        let other = vm.heap_object::<Set>(other_ref, frame)?;

        this.is_disjoint(&other)
    };

    vm.push_value(is_disjoint.into(), frame)?;

    Ok(())
}

fn vmbi_set_difference(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let other_ref = vm.pop_reference(frame)?;

    let difference = {
        let this = vm.heap_object::<Set>(this_ref, frame)?;
        let other = vm.heap_object::<Set>(other_ref, frame)?;

        Set(this.difference(&other).copied().collect())
    };

    let difference_ref = vm.alloc(difference, frame)?;

    vm.push_value(difference_ref.into(), frame)?;

    Ok(())
}

fn vmbi_set_symmetric_difference(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let other_ref = vm.pop_reference(frame)?;

    let symmetric_difference = {
        let this = vm.heap_object::<Set>(this_ref, frame)?;
        let other = vm.heap_object::<Set>(other_ref, frame)?;

        Set(this.symmetric_difference(&other).copied().collect())
    };

    let symmetric_difference_ref = vm.alloc(symmetric_difference, frame)?;

    vm.push_value(symmetric_difference_ref.into(), frame)?;

    Ok(())
}

fn vmbi_set_intersection(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let other_ref = vm.pop_reference(frame)?;

    let symmetric_intersection = {
        let this = vm.heap_object::<Set>(this_ref, frame)?;
        let other = vm.heap_object::<Set>(other_ref, frame)?;

        Set(this.intersection(&other).copied().collect())
    };

    let intersection_ref = vm.alloc(symmetric_intersection, frame)?;

    vm.push_value(intersection_ref.into(), frame)?;

    Ok(())
}

fn vmbi_set_union(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let this_ref = vm.pop_reference(frame)?;
    let other_ref = vm.pop_reference(frame)?;

    let union = {
        let this = vm.heap_object::<Set>(this_ref, frame)?;
        let other = vm.heap_object::<Set>(other_ref, frame)?;

        Set(this.union(&other).copied().collect())
    };

    let union_ref = vm.alloc(union, frame)?;

    vm.push_value(union_ref.into(), frame)?;

    Ok(())
}

///////////////////////////////////////
// ============== I/O ============== //
///////////////////////////////////////

fn vmbi_print(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let string_ref = vm.pop_reference(frame)?;

    let value = vm.heap_object::<Str>(string_ref, frame)?;

    print!("{}", value.as_str());
    Ok(())
}

fn vmbi_println(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    let string_ref = vm.pop_reference(frame)?;

    let value = vm.heap_object::<Str>(string_ref, frame)?;

    println!("{}", value.as_str());
    Ok(())
}

///////////////////////////////////////////////////
// ============== DYNAMIC LOADING ============== //
///////////////////////////////////////////////////

fn vmbi_load_module(vm: &mut Vm, frame: &CallFrame) -> Result<()> {
    todo!("loading modules");
}
