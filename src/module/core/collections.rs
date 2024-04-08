use crate::object::{Type, TypeBuilder, VmObject};
use crate::value::Value;
use crate::vm::builtin;
use crate::vm::ops::OpCode;
use crate::{module::Module, vm::heap::Collector};
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

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
    fn register(module: &mut Module) -> &Type
    where
        Self: Sized,
    {
        let type_name = "core::collections::Array";

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
            ("contains", builtin::ARRAY_CONTAINS),
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

#[derive(Clone, PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct VmDictionary(HashMap<Arc<str>, Value>);

impl VmDictionary {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(HashMap::with_capacity(capacity))
    }
}

impl Deref for VmDictionary {
    type Target = HashMap<Arc<str>, Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for VmDictionary {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl VmObject for VmDictionary {
    fn register(module: &mut Module) -> &Type
    where
        Self: Sized,
    {
        let type_name = "core::collections::Dictionary";

        let mut builder = TypeBuilder::new(type_name);

        let builtin_funcs = [
            ("new", builtin::DICTIONARY_NEW),
            ("with_capacity", builtin::DICTIONARY_WITH_CAPACITY),
            ("length", builtin::DICTIONARY_LENGTH),
            ("index", builtin::DICTIONARY_INDEX),
            ("capacity", builtin::DICTIONARY_CAPACITY),
            ("reserve", builtin::DICTIONARY_RESERVE),
            ("shrink_to_fit", builtin::DICTIONARY_SHRINK_TO_FIT),
            ("shrink_to", builtin::DICTIONARY_SHRINK_TO),
            ("insert", builtin::DICTIONARY_INSERT),
            ("remove", builtin::DICTIONARY_REMOVE),
            ("contains", builtin::DICTIONARY_CONTAINS),
        ];

        for (name, builtin) in builtin_funcs {
            // methods on Array simply forward to the built-in implementation, so the methods are
            // pretty trivial.
            builder.with_method(name, [OpCode::CallBuiltin(builtin), OpCode::Ret]);
        }

        builder.register(module)
    }

    fn field(&self, name: &str) -> Option<&Value> {
        self.0.get(name)
    }

    fn field_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.0.get_mut(name)
    }

    fn gc(&self, mut collector: Collector<'_>) {
        collector.collect_from(self.0.values().copied())
    }
}

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
    fn register(module: &mut Module) -> &Type
    where
        Self: Sized,
    {
        let type_name = "core::collections::String";

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
            ("contains", builtin::STRING_CONTAINS),
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

    fn gc(&self, _: Collector<'_>) {}
}
