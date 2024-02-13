
use serde::{Deserialize, Serialize};
use std::{sync::Arc};

use crate::{
    ops::{Function, OpCode},
    program::Program,
    utils::HashMap,
    value::Value,
};

mod std_impls;

pub trait VmObject: 'static {
    fn register_type(program: &mut Program) -> &VmType
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

impl VmType {
    pub fn new(name: impl Into<Arc<str>>, init: impl IntoIterator<Item = OpCode>) -> Self {
        Self {
            name: name.into(),
            operators: Operators {
                init: init.into_iter().collect(),
                ..Default::default()
            },
            fields: Default::default(),
            methods: Default::default(),
        }
    }

    pub fn with_field(mut self, name: impl Into<Arc<str>>, index: usize) -> Self {
        self.fields.insert(name.into(), index);
        self
    }

    pub fn with_method(
        mut self,
        name: impl Into<Arc<str>>,
        func: impl IntoIterator<Item = OpCode>,
    ) -> Self {
        fn inner(methods: &mut HashMap<Arc<str>, Function>, name: Arc<str>, func: Function) {
            methods.insert(name, func);
        }

        inner(&mut self.methods, name.into(), func.into_iter().collect());
        self
    }

    pub fn with_deinit(
        mut self,
        func: impl IntoIterator<Item = OpCode>,
    ) -> Self {
        self.operators.deinit = Some(func.into_iter().collect());
        self
    }

    pub fn with_index(
        mut self,
        func: impl IntoIterator<Item = OpCode>,
    ) -> Self {
        self.operators.index = Some(func.into_iter().collect());
        self
    }


}

#[derive(Clone, PartialEq, PartialOrd, Debug, Default, Serialize, Deserialize)]
pub struct Operators {
    pub(crate) init: Function,
    pub(crate) deinit: Option<Function>,
    pub(crate) index: Option<Function>,
}
