use std::{ops::Index, rc::Rc, sync::Arc};

use hashbrown::hash_map::RawEntryMut;

use crate::{
    object::TypeMeta,
    ops::{Function, OpCode},
    string::{SymbolIndex, Symbols},
    utils::HashMap,
    value::Value,
    vm::CallFrame,
};

const VALID_MAGIC: &[u8; 7] = b"27FCTRL";

#[derive(Clone, PartialEq, Debug, Default)]
pub struct Program {
    pub(crate) constants: Vec<Value>,
    pub(crate) functions: HashMap<Arc<str>, Function>,
    pub(crate) symbols: Symbols,
}

impl Program {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            functions: HashMap::with_hasher(Default::default()),
            symbols: Symbols::new(),
        }
    }
    
    pub fn define_constant(&mut self, v: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(v);
        index
    }

    pub fn define_function<F: IntoIterator<Item = OpCode>>(&mut self, name: impl AsRef<str>, func: F) -> Result<SymbolIndex, F> {
        let name = name.as_ref();

        match self.functions.raw_entry_mut().from_key(name) {
            RawEntryMut::Occupied(_) => Err(func),
            RawEntryMut::Vacant(entry) => {
                let index = self.symbols.get_or_push(name);
                entry.insert(Arc::from(name), func.into_iter().collect());
                Ok(index)
            },
        }
    }
}