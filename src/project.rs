use hashbrown::HashMap;

use std::{ops::Index, rc::Rc, sync::Arc};

use crate::{
    object::TypeMeta, ops::{Function, OpCode}, string::Symbols, value::Value, vm::CallFrame
};

const VALID_MAGIC: &[u8; 7] = b"27FCTRL";

pub struct ProgramFile {
    pub(crate) types: HashMap<Arc<str>, TypeMeta>,
    pub(crate) constants: Vec<Value>,
    pub(crate) functions: HashMap<Arc<str>, Function>,
    pub(crate) symbols: Symbols,
}
