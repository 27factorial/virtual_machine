use crate::{
    object::Type,
    program::{NativeFn, Program},
    value::Value,
    vm::{
        function::{Function},
    },
};

pub trait Module {
    fn load(self, program: &mut Program);
    fn constants(&self) -> impl Iterator<Item = &Value>;
    fn types(&self) -> impl Iterator<Item = &Type>;
    fn functions(&self) -> impl Iterator<Item = (&str, &Function)>;
    fn native_functions(&self) -> impl Iterator<Item = (&str, &NativeFn)>;
}
