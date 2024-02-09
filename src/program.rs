use serde::{Deserialize, Serialize};
use std::{ops::Index, rc::Rc, str::FromStr, sync::Arc};

use hashbrown::hash_map::RawEntryMut;

use crate::{
    object::{VmObject, VmType},
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
    pub(crate) types: HashMap<Arc<str>, VmType>,
    pub(crate) symbols: Symbols,
}

impl Program {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            functions: HashMap::with_hasher(Default::default()),
            types: HashMap::with_hasher(Default::default()),
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

    pub fn register_type<T: VmObject>(&mut self, symbol: SymbolIndex) -> Option<&VmType> {
        if let Some(name) = self.symbols.get(symbol) {
            let (_, vm_type) = self
                .types
                .raw_entry_mut()
                .from_key(name)
                .or_insert_with(|| (Arc::from(name), T::type_meta()));
            
            Some(vm_type)
        } else {
            None
        }
    }
}

pub struct Path<'a> {
    pub object: Option<&'a str>,
    pub member: &'a str,
}

impl<'a> Path<'a> {
    pub fn new(path: &'a str) -> Option<Self> {
        if path.is_empty() {
            return None;
        }

        let (object_path, member) = path.rsplit_once("::")?;

        let object = if !object_path.is_empty() {
            Some(object_path)
        } else {
            None
        };

        Some(Self { object, member })
    }
}
