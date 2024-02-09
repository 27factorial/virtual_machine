use hashbrown::hash_map::RawEntryMut;
use serde::{Deserialize, Serialize};
use std::{rc::Rc, sync::Arc};

use crate::{
    ops::{Function, OpCode},
    utils::HashMap,
    value::Value,
    vm::Vm,
};

pub trait VmObject: 'static {
    fn type_meta() -> VmType
    where
        Self: Sized;
    fn field(&self, name: &str) -> Option<&Value>;
    fn field_mut(&mut self, name: &str) -> Option<&mut Value>;
    fn fields(&self) -> &[Value];
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct VmType {
    pub(crate) name: Arc<str>,
    pub(crate) fields: HashMap<Arc<str>, usize>,
    pub(crate) methods: HashMap<Arc<str>, Function>,
}
