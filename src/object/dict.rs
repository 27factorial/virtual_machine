use crate::{module::Module, value::Value};
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use super::{Type, VmObject};

#[derive(Clone, PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct VmDictionary {
    indices: HashMap<Arc<str>, usize>,
    values: Vec<Value>,
}

impl VmObject for VmDictionary {
    fn register_type(module: &mut Module) -> &Type
    where
        Self: Sized {
        todo!()
    }

    fn field(&self, name: &str) -> Option<&Value> {
        let &idx = self.indices.get(name)?;
        self.values.get(idx)
    }

    fn field_mut(&mut self, name: &str) -> Option<&mut Value> {
        let &idx = self.indices.get(name)?;
        self.values.get_mut(idx)
    }

    fn data(&self) -> &[Value] {
        &self.values
    }
}