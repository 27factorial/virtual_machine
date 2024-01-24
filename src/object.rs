use hashbrown::{hash_map::RawEntryMut, HashMap};
use std::rc::Rc;

use crate::{ops::Function, value::Value, vm::Vm};

pub trait Object: 'static {
    fn type_meta() -> TypeMeta
    where
        Self: Sized;
    fn field(&self, name: &str) -> Option<&Value>;
    fn field_mut(&mut self, name: &str) -> Option<&mut Value>;
    fn fields(&self) -> &[Value];
    fn call_method(&self, name: &str) -> Result<Value, ()>;
}

pub struct TypeMeta {
    // index into the strings section of a program
    name: usize,
    operators: TypeOperators,
    methods: Vec<Function>,
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

    pub fn with_finalize(mut self, finalize: impl Into<Function>) -> Self {
        self.finalize = Some(finalize.into());
        self
    }

    pub fn with_index(mut self, index: impl Into<Function>) -> Self {
        self.index = Some(index.into());
        self
    }

    pub fn with_add(mut self, add: impl Into<Function>) -> Self {
        self.add = Some(add.into());
        self
    }

    pub fn with_sub(mut self, sub: impl Into<Function>) -> Self {
        self.sub = Some(sub.into());
        self
    }

    pub fn with_mul(mut self, mul: impl Into<Function>) -> Self {
        self.mul = Some(mul.into());
        self
    }

    pub fn with_div(mut self, div: impl Into<Function>) -> Self {
        self.div = Some(div.into());
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
