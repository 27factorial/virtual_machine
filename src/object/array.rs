use std::ops::{Deref, DerefMut};

use crate::{
    program::Program,
    string::SymbolIndex,
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
        let this_ref = vm
            .get_data_stack(0)?
            .object()
            .ok_or_else(|| vm.error(VmErrorKind::Type))?;
        let index: usize = vm
            .get_data_stack(1)?
            .uint()
            .ok_or_else(|| vm.error(VmErrorKind::Type))?
            .try_into()
            .map_err(|_| vm.error(VmErrorKind::Type))?;

        let this = vm
            .heap()
            .get(this_ref)
            .ok_or_else(|| vm.error(VmErrorKind::OutOfBounds))?
            .downcast_ref::<Array>()
            .ok_or_else(|| vm.error(VmErrorKind::InvalidObject))?;

        this.get(index).ok_or_else(|| vm.error(VmErrorKind::OutOfBounds))?;

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
    type InitArgs = SymbolIndex;

    type DeinitArgs = ();

    type IndexArgs = SymbolIndex;

    fn init(native: SymbolIndex) -> impl IntoIterator<Item = OpCode> {
        [OpCode::CallNative(native), OpCode::Ret]
    }

    fn deinit(_: ()) -> Option<impl IntoIterator<Item = OpCode>> {
        Self::unimplemented()
    }

    fn index(native: SymbolIndex) -> Option<impl IntoIterator<Item = OpCode>> {
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
