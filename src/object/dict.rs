use crate::{
    module::Module,
    value::Value,
    vm::{builtin, heap::{Collector, Heap}, ops::OpCode},
};
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use std::{ops::{Deref, DerefMut}, sync::Arc};

use super::{Type, TypeBuilder, VmObject};

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
    fn register_type(module: &mut Module) -> &Type
    where
        Self: Sized,
    {
        let type_name = "Dictionary";

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
