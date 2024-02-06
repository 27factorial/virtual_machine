use hashbrown::hash_map::RawEntryMut;
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
    fn call_method(&self, name: &str) -> Result<Value, ()>;
}

pub struct VmType {
    // index into the strings section of a program
    name: Arc<str>,
    operators: TypeOperators,
    methods: HashMap<Arc<str>, Function>,
}

pub struct TypeOperators {
    construct: Function,
    finalize: Option<Function>,
    index: Option<Function>,
    add: Option<Function>,
    sub: Option<Function>,
    mul: Option<Function>,
    div: Option<Function>,
}

impl TypeOperators {
    pub fn new(construct: Function) -> Self {
        Self {
            construct,
            finalize: None,
            index: None,
            add: None,
            sub: None,
            mul: None,
            div: None,
        }
    }

    pub fn register_finalize(mut self, finalize: impl IntoIterator<Item = OpCode>) -> Self {
        self.finalize = Some(Function::new(finalize));
        self
    }

    pub fn register_index(mut self, index: impl IntoIterator<Item = OpCode>) -> Self {
        self.index = Some(Function::new(index));
        self
    }

    pub fn register_add(mut self, add: impl IntoIterator<Item = OpCode>) -> Self {
        self.add = Some(Function::new(add));
        self
    }

    pub fn register_sub(mut self, sub: impl IntoIterator<Item = OpCode>) -> Self {
        self.sub = Some(Function::new(sub));
        self
    }

    pub fn register_mul(mut self, mul: impl IntoIterator<Item = OpCode>) -> Self {
        self.mul = Some(Function::new(mul));
        self
    }

    pub fn register_div(mut self, div: impl IntoIterator<Item = OpCode>) -> Self {
        self.div = Some(Function::new(div));
        self
    }
}

pub enum Operator {
    Construct,
    Finalize,
    Index,
    Add,
    Sub,
    Mul,
    Div,
}
