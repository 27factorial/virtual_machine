use crate::object::{Type, TypeBuilder, VmObject};
use crate::value::{EqValue, Value};
use crate::vm::builtin;
use crate::vm::exception::ExceptionPayload;
use crate::vm::ops::OpCode;
use crate::{module::Module, vm::heap::Collector};
use hashbrown::{HashMap, HashSet};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

#[derive(Clone, PartialEq, PartialOrd, Debug, Default, Serialize, Deserialize)]
pub struct Array(pub Vec<Value>);

impl Array {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
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
pub struct Dict(HashMap<Arc<str>, Value>);

impl Dict {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(HashMap::with_capacity(capacity))
    }
}

impl Deref for Dict {
    type Target = HashMap<Arc<str>, Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Dict {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl VmObject for Dict {
    fn register(module: &mut Module) -> &Type
    where
        Self: Sized,
    {
        let type_name = "core::collections::Dictionary";

        let mut builder = TypeBuilder::new(type_name);

        let builtin_funcs = [
            ("new", builtin::DICT_NEW),
            ("with_capacity", builtin::DICT_WITH_CAPACITY),
            ("length", builtin::DICT_LENGTH),
            ("index", builtin::DICT_INDEX),
            ("capacity", builtin::DICT_CAPACITY),
            ("reserve", builtin::DICT_RESERVE),
            ("shrink_to_fit", builtin::DICT_SHRINK_TO_FIT),
            ("shrink_to", builtin::DICT_SHRINK_TO),
            ("insert", builtin::DICT_INSERT),
            ("remove", builtin::DICT_REMOVE),
            ("contains", builtin::DICT_CONTAINS),
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
        collector.collect_from(self.0.values().copied())
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct Set(pub HashSet<EqValue>);

impl Set {
    pub fn new() -> Self {
        Self(HashSet::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(HashSet::with_capacity(capacity))
    }
}

impl Deref for Set {
    type Target = HashSet<EqValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Set {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl VmObject for Set {
    fn register(module: &mut Module) -> &Type
    where
        Self: Sized,
    {
        let type_name = "core::collections::Set";

        let mut builder = TypeBuilder::new(type_name);

        let builtin_funcs = [
            ("new", builtin::SET_NEW),
            ("with_capacity", builtin::SET_WITH_CAPACITY),
            ("length", builtin::SET_LENGTH),
            ("index", builtin::SET_INDEX),
            ("capacity", builtin::SET_CAPACITY),
            ("reserve", builtin::SET_RESERVE),
            ("shrink_to_fit", builtin::SET_SHRINK_TO_FIT),
            ("shrink_to", builtin::SET_SHRINK_TO),
            ("insert", builtin::SET_INSERT),
            ("remove", builtin::SET_REMOVE),
            ("contains", builtin::SET_CONTAINS),
            ("is_disjoint", builtin::SET_IS_DISJOINT),
            ("difference", builtin::SET_DIFFERENCE),
            ("symmetric_difference", builtin::SET_SYMMETRIC_DIFFERENCE),
            ("intersection", builtin::SET_INTERSECTION),
            ("union", builtin::SET_UNION),
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
        collector.collect_from(self.iter().map(|ev| Value::from(*ev)))
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug, Default, Serialize, Deserialize)]
pub struct Str(pub String);

impl Str {
    pub const fn new() -> Self {
        Self(String::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(String::with_capacity(capacity))
    }
}

impl Deref for Str {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Str {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<String> for Str {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl VmObject for Str {
    fn register(module: &mut Module) -> &Type
    where
        Self: Sized,
    {
        let type_name = "core::collections::Str";

        let mut builder = TypeBuilder::new(type_name);

        let builtin_funcs = [
            ("new", builtin::STR_NEW),
            ("from", builtin::STR_FROM),
            ("with_capacity", builtin::STR_WITH_CAPACITY),
            ("length", builtin::STR_LENGTH),
            ("index_byte", builtin::STR_INDEX_BYTE),
            ("index_char", builtin::STR_INDEX_CHAR),
            ("capacity", builtin::STR_CAPACITY),
            ("reserve", builtin::STR_RESERVE),
            ("shrink_to_fit", builtin::STR_SHRINK_TO_FIT),
            ("shrink_to", builtin::STR_SHRINK_TO),
            ("truncate", builtin::STR_TRUNCATE),
            ("insert", builtin::STR_INSERT),
            ("remove", builtin::STR_REMOVE),
            ("push", builtin::STR_PUSH),
            ("pop", builtin::STR_POP),
            ("contains", builtin::STR_CONTAINS),
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

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Error)]
pub enum ArrayExceptionPayload {
    #[error("array index out of bounds: the length is {len} but the index is {index}")]
    OutOfBounds {
        len: usize,
        index: i64,
    }
}

impl ExceptionPayload for ArrayExceptionPayload {
    fn is_fatal(&self) -> bool {
        false
    }
}
