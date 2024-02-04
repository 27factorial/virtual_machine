use hashbrown::HashMap;

use std::ops::Index;

use crate::{
    object::TypeMeta,
    ops::{Function, OpCode},
    value::Value,
    vm::CallFrame,
};

const VALID_MAGIC: &[u8; 7] = b"27FCTRL";

pub struct ProgramFile {
    pub(crate) types: HashMap<StringIndex, TypeMeta>,
    pub(crate) constants: Vec<Value>,
    pub(crate) functions: Vec<Function>,
    
}
