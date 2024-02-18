use std::ops::{Deref, DerefMut};

use crate::{
    program::Program,
    string::Symbol,
    utils::VmResult,
    value::Value,
    vm::{
        ops::{OpCode, VmError, VmErrorKind},
        Vm,
    },
};

use super::{Operations, VmObject, VmType};

pub struct Array(Vec<Value>);

impl Array {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    fn vm_init(vm: &mut Vm) -> Result<Value, VmError> {
        let object = vm.alloc(Self::new());

        Ok(Value::Object(object))
    }

    fn vm_index(vm: &mut Vm) -> Result<Value, VmError> {
        let this_ref = vm.get_object_ref(0)?;
        let index: usize = vm.get_uint(1)?.try_into().vm_err(VmErrorKind::Type, vm)?;

        let this = vm.get_object::<Self>(this_ref)?;

        this.get(index).vm_err(VmErrorKind::OutOfBounds, vm)?;

        todo!()
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

impl Operations for Array {
    type InitArgs = Symbol;

    type DeinitArgs = ();

    type IndexArgs = Symbol;

    fn init(native: Symbol) -> impl IntoIterator<Item = OpCode> {
        [OpCode::CallNative(native), OpCode::Ret]
    }

    fn deinit(_: ()) -> Option<impl IntoIterator<Item = OpCode>> {
        Self::unimplemented()
    }

    fn index(native: Symbol) -> Option<impl IntoIterator<Item = OpCode>> {
        Some([OpCode::CallNative(native), OpCode::Ret])
    }
}

impl VmObject for Array {
    fn register_type(program: &mut Program) -> &VmType
    where
        Self: Sized,
    {
        todo!()
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
