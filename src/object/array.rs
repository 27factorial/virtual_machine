use std::ops::{Deref, DerefMut};

use crate::{
    program::Program,
    utils::VmResult,
    value::Value,
    vm::{
        ops::{OpCode, VmError, VmErrorKind},
        Vm,
    },
};

use super::{VmObject, VmType};

pub struct Array(pub Vec<Value>);

impl Array {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    fn vm_new(vm: &mut Vm) -> Result<Value, VmError> {
        let this_ref = vm.alloc(Self::new())?;

        Ok(Value::Object(this_ref))
    }

    fn vm_index(vm: &mut Vm) -> Result<Value, VmError> {
        let this_ref = vm.pop_object_ref()?;
        let index: usize = vm
            .pop_uint()?
            .try_into()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        let value = vm
            .get_object::<Self>(this_ref)?
            .get(index)
            .copied()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        Ok(value)
    }

    fn vm_length(vm: &mut Vm) -> Result<Value, VmError> {
        let this_ref = vm.pop_object_ref()?;
        let len = vm.get_object::<Self>(this_ref)?.len();

        Ok(Value::UInt(len as u64))
    }

    fn vm_push(vm: &mut Vm) -> Result<Value, VmError> {
        let this_ref = vm.pop_object_ref()?;
        let value = vm.pop_value()?;

        vm.get_object_mut::<Self>(this_ref)?.push(value);

        Ok(Value::Null)
    }

    fn vm_pop(vm: &mut Vm) -> Result<Value, VmError> {
        let this_ref = vm.pop_object_ref()?;

        let value = vm
            .get_object_mut::<Self>(this_ref)?
            .pop()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        Ok(value)
    }

    fn vm_insert(vm: &mut Vm) -> Result<Value, VmError> {
        let this_ref = vm.pop_object_ref()?;
        let idx: usize = vm
            .pop_uint()?
            .try_into()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;
        let value = vm.pop_value()?;

        let this = vm.get_object_mut::<Self>(this_ref)?;

        if idx <= this.len() {
            this.insert(idx, value);
            Ok(Value::Null)
        } else {
            Err(vm.error(VmErrorKind::OutOfBounds))
        }
    }

    fn vm_remove(vm: &mut Vm) -> Result<Value, VmError> {
        let this_ref = vm.pop_object_ref()?;
        let idx: usize = vm
            .pop_uint()?
            .try_into()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        let this = vm.get_object_mut::<Self>(this_ref)?;

        if idx < this.len() {
            let value = this.remove(idx);
            Ok(value)
        } else {
            Err(vm.error(VmErrorKind::OutOfBounds))
        }
    }

    fn vm_swap_remove(vm: &mut Vm) -> Result<Value, VmError> {
        let this_ref = vm.pop_object_ref()?;
        let idx: usize = vm
            .pop_uint()?
            .try_into()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        let this = vm.get_object_mut::<Self>(this_ref)?;

        if idx < this.len() {
            let value = this.swap_remove(idx);
            Ok(value)
        } else {
            Err(vm.error(VmErrorKind::OutOfBounds))
        }
    }

    fn vm_reserve(vm: &mut Vm) -> Result<Value, VmError> {
        let this = vm.pop_object_ref()?;
        let additional: usize = vm
            .pop_uint()?
            .try_into()
            .vm_err(VmErrorKind::OutOfBounds, vm)?;

        vm.get_object_mut::<Self>(this)?.reserve(additional);
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

    fn field(&self, name: &str) -> Option<&Value> {
        todo!()
    }

    fn field_mut(&mut self, name: &str) -> Option<&mut Value> {
        todo!()
    }

    fn fields(&self) -> &[Value] {
        todo!()
    }
}
