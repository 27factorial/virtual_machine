use crate::program::Program;
use crate::utils::IntoVmResult;
use crate::value::Value;
use crate::vm::ops::OpCode;
use crate::vm::{CallFrame, Result as VmResult, Vm, VmErrorKind};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};

use super::{Type, TypeBuilder, VmObject};

#[derive(Clone, PartialEq, PartialOrd, Debug, Default, Serialize, Deserialize)]
pub struct Array(pub Vec<Value>);

impl Array {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    fn vm_new(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this_ref = vm.alloc(Self::new(), frame)?;

        Ok(Value::Reference(this_ref))
    }

    fn vm_with_capacity(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        // Using isize::MAX as a maximum bound ensures that the capacity cannot exceed the bounds
        // of usize, so the `as` casts here are fine. This is also a degenerate case that will
        // likely cause an OOM error in Rust anyway.
        let capacity = vm.pop_uint(frame)?.max(isize::MAX as u64) as usize;
        let this_ref = vm.alloc(Self::with_capacity(capacity), frame)?;

        Ok(Value::Reference(this_ref))
    }

    fn vm_length(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this_ref = vm.pop_reference(frame)?;
        let len = vm.heap_object::<Self>(this_ref, frame)?.len();

        Ok(Value::UInt(len as u64))
    }

    fn vm_index(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this_ref = vm.pop_reference(frame)?;
        let index: usize = vm
            .pop_uint(frame)?
            .try_into()
            .vm_result(VmErrorKind::OutOfBounds, frame)?;

        let value = vm
            .heap_object::<Self>(this_ref, frame)?
            .get(index)
            .copied()
            .vm_result(VmErrorKind::OutOfBounds, frame)?;

        Ok(value)
    }

    

    fn vm_capacity(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this = vm.pop_reference(frame)?;
        let capacity = vm.heap_object::<Self>(this, frame)?.capacity();

        Ok(Value::UInt(capacity as u64))
    }

    fn vm_reserve(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this = vm.pop_reference(frame)?;
        let additional: usize = vm
            .pop_uint(frame)?
            .try_into()
            .vm_result(VmErrorKind::OutOfBounds, frame)?;

        vm.heap_object_mut::<Self>(this, frame)?.reserve(additional);
        Ok(Value::Null)
    }

    fn vm_shrink_to_fit(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this = vm.pop_reference(frame)?;
        vm.heap_object_mut::<Self>(this, frame)?.shrink_to_fit();

        Ok(Value::Null)
    }

    fn vm_shrink_to(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this = vm.pop_reference(frame)?;
        let min_capacity = vm.pop_uint(frame)?.max(isize::MAX as u64) as usize;

        vm.heap_object_mut::<Self>(this, frame)?
            .shrink_to(min_capacity);

        Ok(Value::Null)
    }

    fn vm_truncate(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this = vm.pop_reference(frame)?;
        let len = vm.pop_uint(frame)?.max(isize::MAX as u64) as usize;

        vm.heap_object_mut::<Self>(this, frame)?.truncate(len);

        Ok(Value::Null)
    }

    fn vm_swap_remove(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this_ref = vm.pop_reference(frame)?;
        let idx: usize = vm
            .pop_uint(frame)?
            .try_into()
            .vm_result(VmErrorKind::OutOfBounds, frame)?;

        let this = vm.heap_object_mut::<Self>(this_ref, frame)?;

        if idx < this.len() {
            let value = this.swap_remove(idx);
            Ok(value)
        } else {
            Err(vm.error(VmErrorKind::OutOfBounds, frame))
        }
    }

    fn vm_insert(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this_ref = vm.pop_reference(frame)?;
        let idx: usize = vm
            .pop_uint(frame)?
            .try_into()
            .vm_result(VmErrorKind::OutOfBounds, frame)?;
        let value = vm.pop_value(frame)?;

        let this = vm.heap_object_mut::<Self>(this_ref, frame)?;

        if idx <= this.len() {
            this.insert(idx, value);
            Ok(Value::Null)
        } else {
            Err(vm.error(VmErrorKind::OutOfBounds, frame))
        }
    }

    fn vm_remove(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this_ref = vm.pop_reference(frame)?;
        let idx: usize = vm
            .pop_uint(frame)?
            .try_into()
            .vm_result(VmErrorKind::OutOfBounds, frame)?;

        let this = vm.heap_object_mut::<Self>(this_ref, frame)?;

        if idx < this.len() {
            let value = this.remove(idx);
            Ok(value)
        } else {
            Err(vm.error(VmErrorKind::OutOfBounds, frame))
        }
    }

    fn vm_push(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this_ref = vm.pop_reference(frame)?;
        let value = vm.pop_value(frame)?;

        vm.heap_object_mut::<Self>(this_ref, frame)?.push(value);

        Ok(Value::Null)
    }

    fn vm_pop(vm: &mut Vm, frame: &CallFrame) -> VmResult<Value> {
        let this_ref = vm.pop_reference(frame)?;

        let value = vm
            .heap_object_mut::<Self>(this_ref, frame)?
            .pop()
            .vm_result(VmErrorKind::OutOfBounds, frame)?;

        Ok(value)
    }
}

impl Deref for Array {
    type Target = Vec<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Array {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl VmObject for Array {
    fn register_type(program: &mut Program) -> &Type
    where
        Self: Sized,
    {
        let type_name = "Array";

        let mut builder = TypeBuilder::new(type_name);

        let native_funcs = [
            ("new", Array::vm_new as fn(&mut Vm, &CallFrame) -> _),
            ("with_capacity", Array::vm_with_capacity),
            ("length", Array::vm_length),
            ("index", Array::vm_index),
            ("capacity", Array::vm_capacity),
            ("reserve", Array::vm_reserve),
            ("shrink_to_fit", Array::vm_shrink_to_fit),
            ("shrink_to", Array::vm_shrink_to),
            ("truncate", Array::vm_truncate),
            ("swap_remove", Array::vm_swap_remove),
            ("insert", Array::vm_insert),
            ("remove", Array::vm_remove),
            ("push", Array::vm_push),
            ("pop", Array::vm_pop),
        ];

        for (name, func) in native_funcs {
            let native_name = [type_name, "::", name];
            let native_sym = program.define_symbol_iter(native_name);

            program
                .define_native_function(native_sym, func)
                .unwrap_or_else(|_| {
                    panic!(
                        "native method {} already defined on type {type_name}",
                        String::from_iter(native_name)
                    )
                });

            // methods on Array simply forward to the native implementation, so the methods are
            // pretty trivial.
            builder.with_method(name, [OpCode::CallNative(native_sym), OpCode::Ret]);
        }

        builder.register(program)
    }

    fn field(&self, _: &str) -> Option<&Value> {
        None
    }

    fn field_mut(&mut self, _: &str) -> Option<&mut Value> {
        None
    }

    fn data(&self) -> &[Value] {
        &self.0
    }
}
