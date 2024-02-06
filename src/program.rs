use serde::{Deserialize, Serialize};
use std::{ops::Index, rc::Rc, sync::Arc};

use hashbrown::hash_map::RawEntryMut;

use crate::{
    object::VmType,
    ops::{Function, OpCode},
    string::{SymbolIndex, Symbols},
    utils::HashMap,
    value::Value,
    vm::CallFrame,
};

const VALID_MAGIC: &[u8; 7] = b"27FCTRL";

#[derive(Clone, PartialEq, Debug, Default, Serialize, Deserialize)]
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

    pub fn define_symbol(&mut self, symbol: impl AsRef<str>) -> SymbolIndex {
        self.symbols.get_or_push(symbol.as_ref())
    }

    pub fn define_constant(&mut self, v: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(v);
        index
    }

    pub fn define_function<F: IntoIterator<Item = OpCode>>(
        &mut self,
        symbol: SymbolIndex,
        func: F,
    ) -> Result<(), F> {
        if let Some(name) = self.symbols.get(symbol) {
            match self.functions.raw_entry_mut().from_key(name) {
                RawEntryMut::Vacant(entry) => {
                    entry.insert(Arc::from(name), func.into_iter().collect());
                    Ok(())
                }
                RawEntryMut::Occupied(_) => Err(func),
            }
        } else {
            Err(func)
        }
    }
}
