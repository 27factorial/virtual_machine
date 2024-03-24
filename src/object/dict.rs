use crate::{
    module::Module,
    value::Value,
    vm::heap::{Collector, Heap},
};
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use std::{ops::{Deref, DerefMut}, sync::Arc};

use super::{Type, VmObject};

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
        todo!()
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
