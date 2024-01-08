use hashbrown::HashMap;

use std::ops::Index;

use crate::{
    object::TypeMeta,
    ops::{Function, OpCode},
    value::Value,
    vm::CallFrame,
};

const VALID_MAGIC: &[u8; 11] = b"27FACTORIAL";

pub struct Program {
    pub(crate) header: ProgramHeader,
    pub(crate) constants: Vec<Value>,
    pub(crate) strings: Vec<String>,
    pub(crate) functions: Vec<Function>,
    pub(crate) types: Vec<TypeMeta>,
}

impl Index<CallFrame> for Program {
    type Output = OpCode;

    fn index(&self, index: CallFrame) -> &Self::Output {
        self.functions.index(index.func).0.index(index.ip)
    }
}

pub struct ProgramHeader {
    magic: [u8; VALID_MAGIC.len()],
    pub(crate) type_indices: HashMap<String, usize>,
}
