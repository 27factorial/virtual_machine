use crate::program::Program;
use crate::utils::IntoVmResult;
use crate::value::Value;
use crate::vm::ops::OpCode;
use crate::vm::{Result as VmResult, Vm, VmErrorKind};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};

use super::{VmObject, VmType};

#[derive(Clone, PartialEq, PartialOrd, Debug, Default, Serialize, Deserialize)]
pub struct Array(pub Vec<Value>);

impl Array {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    fn vm_new(vm: &mut Vm) -> VmResult<Value> {
        let this_ref = vm.alloc(Self::new())?;

        Ok(Value::Reference(this_ref))
    }

    fn vm_index(vm: &mut Vm) -> VmResult<Value> {
        let this_ref = vm.pop_reference()?;
        let index: usize = vm
            .pop_uint()?
            .try_into()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        let value = vm
            .heap_object::<Self>(this_ref)?
            .get(index)
            .copied()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        Ok(value)
    }

    fn vm_length(vm: &mut Vm) -> VmResult<Value> {
        let this_ref = vm.pop_reference()?;
        let len = vm.heap_object::<Self>(this_ref)?.len();

        Ok(Value::UInt(len as u64))
    }

    fn vm_push(vm: &mut Vm) -> VmResult<Value> {
        let this_ref = vm.pop_reference()?;
        let value = vm.pop_value()?;

        vm.heap_object_mut::<Self>(this_ref)?.push(value);

        Ok(Value::Null)
    }

    fn vm_pop(vm: &mut Vm) -> VmResult<Value> {
        let this_ref = vm.pop_reference()?;

        let value = vm
            .heap_object_mut::<Self>(this_ref)?
            .pop()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        Ok(value)
    }

    fn vm_insert(vm: &mut Vm) -> VmResult<Value> {
        let this_ref = vm.pop_reference()?;
        let idx: usize = vm
            .pop_uint()?
            .try_into()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;
        let value = vm.pop_value()?;

        let this = vm.heap_object_mut::<Self>(this_ref)?;

        if idx <= this.len() {
            this.insert(idx, value);
            Ok(Value::Null)
        } else {
            Err(vm.error(VmErrorKind::OutOfBounds))
        }
    }

    fn vm_remove(vm: &mut Vm) -> VmResult<Value> {
        let this_ref = vm.pop_reference()?;
        let idx: usize = vm
            .pop_uint()?
            .try_into()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        let this = vm.heap_object_mut::<Self>(this_ref)?;

        if idx < this.len() {
            let value = this.remove(idx);
            Ok(value)
        } else {
            Err(vm.error(VmErrorKind::OutOfBounds))
        }
    }

    fn vm_swap_remove(vm: &mut Vm) -> VmResult<Value> {
        let this_ref = vm.pop_reference()?;
        let idx: usize = vm
            .pop_uint()?
            .try_into()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        let this = vm.heap_object_mut::<Self>(this_ref)?;

        if idx < this.len() {
            let value = this.swap_remove(idx);
            Ok(value)
        } else {
            Err(vm.error(VmErrorKind::OutOfBounds))
        }
    }

    fn vm_reserve(vm: &mut Vm) -> VmResult<Value> {
        let this = vm.pop_reference()?;
        let additional: usize = vm
            .pop_uint()?
            .try_into()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        vm.heap_object_mut::<Self>(this)?.reserve(additional);
        Ok(Value::Null)
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
    fn register_type(program: &mut Program) -> &VmType
    where
        Self: Sized,
    {
        let ty_name = "Array";

        let mut ty = VmType::new(ty_name);

        let native_funcs = [
            ("new", Array::vm_new as fn(&mut Vm) -> _),
            ("index", Array::vm_index),
            ("length", Array::vm_length),
            ("push", Array::vm_push),
            ("pop", Array::vm_pop),
            ("insert", Array::vm_insert),
            ("remove", Array::vm_remove),
            ("swap_remove", Array::vm_swap_remove),
            ("reserve", Array::vm_reserve),
        ];

        for (name, func) in native_funcs {
            let native_name = [ty_name, "::", name];
            let native_sym = program.define_symbol_iter(native_name);

            program
                .define_native_function(native_sym, func)
                .unwrap_or_else(|_| {
                    panic!(
                        "native method {} already defined on type {ty_name}",
                        String::from_iter(native_name)
                    )
                });

            // methods on Array simply forward to the native implementation, so the methods are
            // pretty trivial.
            ty.with_method(name, [OpCode::CallNative(native_sym), OpCode::Ret]);
        }

        program.register_type(ty)
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
