use serde::{Deserialize, Serialize};
use std::{
    any::{Any, TypeId},
    sync::Arc,
};

use crate::{
    program::Program,
    utils::HashMap,
    value::Value,
    vm::gc::GcBox,
    vm::ops::{Function, OpCode},
};

pub mod array;
mod std_impls;

pub trait VmObject: Any + Send + Sync {
    fn register_type(program: &mut Program) -> &VmType
    where
        Self: Sized;
    fn field(&self, name: &str) -> Option<&Value>;
    fn field_mut(&mut self, name: &str) -> Option<&mut Value>;
    fn fields(&self) -> &[Value];
}

impl dyn VmObject {
    pub fn downcast_ref<T: VmObject>(&self) -> Option<&T> {
        (self as &dyn Any).downcast_ref()
    }

    pub fn downcast_mut<T: VmObject>(&mut self) -> Option<&mut T> {
        (self as &mut dyn Any).downcast_mut()
    }

    pub fn downcast<T: VmObject>(self: Box<Self>) -> Result<Box<T>, Box<Self>> {
        // Must be implemented manually to return Err(Box<Self>)
        if (*self).type_id() == TypeId::of::<T>() {
            let raw = Box::into_raw(self);
            let (data, _) = raw.to_raw_parts();
            unsafe { Ok(Box::from_raw(data.cast())) }
        } else {
            Err(self)
        }
    }
}

impl GcBox<dyn VmObject> {
    pub fn downcast_gc<T: VmObject>(self) -> Result<GcBox<T>, Self> {
        // Must be implemented manually because `Any` doesn't support downcasting
        // in a GcBox
        if (*self).type_id() == TypeId::of::<T>() {
            let raw = GcBox::into_raw(self);
            let (data, _) = raw.to_raw_parts();
            unsafe { Ok(GcBox::from_raw(data.cast())) }
        } else {
            Err(self)
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct VmType {
    pub(crate) name: Arc<str>,
    pub(crate) fields: HashMap<Arc<str>, usize>,
    pub(crate) methods: HashMap<Arc<str>, Function>,
}

impl VmType {
    pub fn new(name: impl Into<Arc<str>>) -> Self {
        Self {
            name: name.into(),
            fields: Default::default(),
            methods: Default::default(),
        }
    }

    pub fn with_field(&mut self, name: impl Into<Arc<str>>, index: usize) -> &mut Self {
        self.fields.insert(name.into(), index);
        self
    }

    pub fn with_method(
        &mut self,
        name: impl Into<Arc<str>>,
        func: impl IntoIterator<Item = OpCode>,
    ) -> &mut Self {
        fn inner(methods: &mut HashMap<Arc<str>, Function>, name: Arc<str>, func: Function) {
            methods.insert(name, func);
        }

        inner(&mut self.methods, name.into(), func.into_iter().collect());
        self
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug, Default, Serialize, Deserialize)]
pub struct Operators {
    pub(crate) init: Function,
    pub(crate) deinit: Option<Function>,
    pub(crate) index: Option<Function>,
}
