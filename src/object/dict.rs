use crate::{
    module::Module,
    value::Value,
    vm::heap::{Collector, Heap},
};
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use super::{Type, VmObject};

#[derive(Clone, PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct VmDictionary(HashMap<Arc<str>, Value>);

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

    fn collect_data(&self, mut collector: Collector<'_>) {
        collector.collect_from(self.0.values().copied())
    }
}
