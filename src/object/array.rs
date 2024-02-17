use std::ops::{Deref, DerefMut};

use crate::{
    ops::OpCode,
    program::Program,
    string::SymbolIndex,
    value::Value,
    vm::{Register, Vm},
};

use super::{Operations, VmObject, VmType};

pub struct Array(Vec<Value>);

impl Array {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    fn vm_init(vm: &mut Vm) -> Option<Value> {
        let object = vm.alloc(Self::new());

        Some(Value::Object(object))
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
