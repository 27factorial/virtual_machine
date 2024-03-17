use crate::{
    object::Type,
    program::NativeFn,
    vm::{
        function::{Function, Functions},
        ops::OpCode,
        Vm,
    },
};

pub trait Module {
    fn load(self, vm: &mut Vm);
    fn types(&self) -> impl Iterator<Item = &Type>;
    fn functions(&self) -> impl Iterator<Item = (&str, &Function)>;
    fn native_functions(&self) -> impl Iterator<Item = (&str, &NativeFn)>;
    fn code(&self) -> &[OpCode];
}
