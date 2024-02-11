use hashbrown::hash_map::RawEntryMut;
use serde::{Deserialize, Serialize};
use std::{rc::Rc, sync::Arc};

use crate::{
    ops::{Function, OpCode},
    program::Program,
    utils::HashMap,
    value::Value,
    vm::Vm,
};

mod std_impls;

pub trait VmObject: 'static {
    fn register_type(program: &mut Program) -> Option<&VmType>
    where
        Self: Sized;
    fn field(&self, name: &str) -> Option<&Value>;
    fn field_mut(&mut self, name: &str) -> Option<&mut Value>;
    fn fields(&self) -> &[Value];
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct VmType {
    pub(crate) name: Arc<str>,
    pub(crate) operators: Operators,
    pub(crate) fields: HashMap<Arc<str>, usize>,
    pub(crate) methods: HashMap<Arc<str>, Function>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Operators {
    pub(crate) init: Function,
    pub(crate) deinit: Option<Function>,
    pub(crate) index: Option<Function>,
}
