use super::{Type, TypeBuilder, VmObject};
use crate::value::Value;
use crate::vm::builtin;
use crate::vm::ops::OpCode;
use crate::{module::Module, vm::heap::Collector};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};

#[derive(Clone, PartialEq, PartialOrd, Debug, Default, Serialize, Deserialize)]
pub struct VmArray(pub Vec<Value>);

impl VmArray {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }
}

impl Deref for VmArray {
    type Target = Vec<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for VmArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl VmObject for VmArray {
    fn register_type(module: &mut Module) -> &Type
    where
        Self: Sized,
    {
        let type_name = "Array";

        let mut builder = TypeBuilder::new(type_name);

        let builtin_funcs = [
            ("new", builtin::ARRAY_NEW),
            ("with_capacity", builtin::ARRAY_WITH_CAPACITY),
            ("length", builtin::ARRAY_LENGTH),
            ("index", builtin::ARRAY_INDEX),
            ("capacity", builtin::ARRAY_CAPACITY),
            ("reserve", builtin::ARRAY_RESERVE),
            ("shrink_to_fit", builtin::ARRAY_SHRINK_TO_FIT),
            ("shrink_to", builtin::ARRAY_SHRINK_TO),
            ("truncate", builtin::ARRAY_TRUNCATE),
            ("swap_remove", builtin::ARRAY_SWAP_REMOVE),
            ("insert", builtin::ARRAY_INSERT),
            ("remove", builtin::ARRAY_REMOVE),
            ("push", builtin::ARRAY_PUSH),
            ("pop", builtin::ARRAY_POP),
        ];

        for (name, builtin) in builtin_funcs {
            // methods on Array simply forward to the built-in implementation, so the methods are
            // pretty trivial.
            builder.with_method(name, [OpCode::CallBuiltin(builtin), OpCode::Ret]);
        }

        builder.register(module)
    }

    fn field(&self, _: &str) -> Option<&Value> {
        None
    }

    fn field_mut(&mut self, _: &str) -> Option<&mut Value> {
        None
    }

    fn gc(&self, mut collector: Collector<'_>) {
        collector.collect_from(self.iter().copied())
    }
}
