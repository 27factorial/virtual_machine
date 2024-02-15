use crate::{ops::OpCode, program::Program, string::SymbolIndex, value::Value};

use super::{Operations, VmObject, VmType};

impl Operations for String {
    /// The index of the symbol which names the String constructor in the program's native functions
    type InitArgs = SymbolIndex;
    type DeinitArgs = ();
    type IndexArgs = ();

    fn init(native: SymbolIndex) -> impl IntoIterator<Item = OpCode> {
        use OpCode::*;

        [CallNative(native)]
    }

    fn deinit(_: ()) -> Option<impl IntoIterator<Item = OpCode>> {
        Self::unimplemented()
    }

    fn index(_: ()) -> Option<impl IntoIterator<Item = OpCode>> {
        Self::unimplemented()
    }
}

impl VmObject for String {
    fn register_type(program: &mut Program) -> &VmType
    where
        Self: Sized,
    {
        let new = program.define_symbol("String::new");
        let print = program.define_symbol("String::print");

        let ty = VmType::new("String", String::init(new))
            .with_method("String::print", [OpCode::CallNative(print)]);

        program.register_type(ty)
    }

    fn field(&self, _: &str) -> Option<&Value> {
        None
    }

    fn field_mut(&mut self, _: &str) -> Option<&mut Value> {
        None
    }

    fn fields(&self) -> &[Value] {
        &[]
    }
}
