use std::ops::{Deref, DerefMut};

use serde::{Deserialize, Serialize};

use crate::{
    module::Module,
    value::Value,
    vm::{builtin, ops::OpCode},
};

use super::{Type, TypeBuilder, VmObject};

#[derive(Clone, PartialEq, PartialOrd, Debug, Default, Serialize, Deserialize)]
pub struct VmString(pub String);

impl VmString {
    pub const fn new() -> Self {
        Self(String::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(String::with_capacity(capacity))
    }
}

impl Deref for VmString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for VmString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<String> for VmString {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl VmObject for VmString {
    fn register_type(module: &mut Module) -> &Type
    where
        Self: Sized,
    {
        let type_name = "String";

        let mut builder = TypeBuilder::new(type_name);

        let builtin_funcs = [
            ("new", builtin::STRING_NEW),
            ("from", builtin::STRING_FROM),
            ("with_capacity", builtin::STRING_WITH_CAPACITY),
            ("length", builtin::STRING_LENGTH),
            ("index_byte", builtin::STRING_INDEX_BYTE),
            ("index_char", builtin::STRING_INDEX_CHAR),
            ("capacity", builtin::STRING_CAPACITY),
            ("reserve", builtin::STRING_RESERVE),
            ("shrink_to_fit", builtin::STRING_SHRINK_TO_FIT),
            ("shrink_to", builtin::STRING_SHRINK_TO),
            ("truncate", builtin::STRING_TRUNCATE),
            ("insert", builtin::STRING_INSERT),
            ("remove", builtin::STRING_REMOVE),
            ("push", builtin::STRING_PUSH),
            ("pop", builtin::STRING_POP),
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

    fn data(&self) -> &[Value] {
        &[]
    }
}
