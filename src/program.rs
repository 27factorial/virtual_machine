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

    pub fn register_type(&mut self, symbol: SymbolIndex, ty: VmType) -> Option<&VmType> {
        if let Some(name) = self.symbols.get(symbol) {
            let (_, vm_type) = self
                .types
                .raw_entry_mut()
                .from_key(name)
                .or_insert_with(|| (Arc::from(name), ty));

            Some(vm_type)
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub struct Path<'a> {
    pub object: Option<&'a str>,
    pub member: &'a str,
}

impl<'a> Path<'a> {
    pub fn new(path: &'a str) -> Option<Self> {
        if path.is_empty() {
            return None;
        }

        // Attempts to split a string of the form "object::member" or just "member". If there is no
        // "::" separator, then rsplit_once will return None, and the unwrap_or will cause the
        // result to be  (object = None, member = path). If a is separator is found, rsplit_once
        // will return Some(("object", "member")). Map turns this into
        // (object = Some("object"), member = "member"). This means that, if path isn't empty,
        // We end up with the components of a Path, where there is at least a member and possibly
        // an object.
        let (object, member) = path
            .rsplit_once("::")
            .map(|(obj, member)| (Some(obj), member))
            .unwrap_or((None, path));

        Some(Self { object, member })
    }
}
