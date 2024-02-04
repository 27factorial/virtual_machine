use hashbrown::HashMap;

use std::{ops::Index, rc::Rc, sync::Arc};

use crate::{
    object::TypeMeta,
    ops::{Function, OpCode},
    value::Value,
    vm::CallFrame,
};

const VALID_MAGIC: &[u8; 7] = b"27FCTRL";

pub struct ProgramFile {
    pub(crate) types: HashMap<Box<str>, TypeMeta>,
    pub(crate) constants: HashMap<Box<str>, Value>,
    pub(crate) functions: HashMap<Box<str>, Function>
}
