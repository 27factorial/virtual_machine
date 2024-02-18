use serde::{Deserialize, Serialize};
use std::sync::Arc;

use hashbrown::hash_map::RawEntryMut;

use crate::{
    object::VmType,
    string::{Symbol, Symbols},
    utils::HashMap,
    value::Value,
    vm::{
        ops::{Function, OpCode, VmError},
        Vm,
    },
};

const VALID_MAGIC: &[u8; 7] = b"27FCTRL";

pub type NativeFn = dyn Fn(&mut Vm) -> Result<Value, VmError> + 'static;

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Program {
    pub(crate) constants: Vec<Value>,
    pub(crate) functions: HashMap<Arc<str>, Function>,
    #[serde(skip)]
    pub(crate) native_functions: HashMap<Arc<str>, Arc<NativeFn>>,
    pub(crate) types: HashMap<Arc<str>, VmType>,
    pub(crate) symbols: Symbols,
}

impl Program {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            functions: HashMap::default(),
            native_functions: HashMap::default(),
            types: HashMap::default(),
            symbols: Symbols::new(),
        }
    }

    pub fn define_symbol(&mut self, symbol: impl AsRef<str>) -> Symbol {
        self.symbols.get_or_push(symbol.as_ref())
    }

    pub fn define_constant(&mut self, v: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(v);
        index
    }

    pub fn define_function<F: IntoIterator<Item = OpCode>>(
        &mut self,
        symbol: Symbol,
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

    pub fn define_native_function<F>(&mut self, symbol: Symbol, func: F) -> Result<(), F>
    where
        F: Fn(&mut Vm) -> Result<Value, VmError> + 'static,
    {
        if let Some(name) = self.symbols.get(symbol) {
            match self.native_functions.raw_entry_mut().from_key(name) {
                RawEntryMut::Vacant(entry) => {
                    entry.insert(Arc::from(name), Arc::new(func));
                    Ok(())
                }
                RawEntryMut::Occupied(_) => Err(func),
            }
        } else {
            Err(func)
        }
    }

    pub fn register_type(&mut self, ty: VmType) -> &VmType {
        self.symbols.get_or_push(&ty.name);

        let (_, vm_type) = self
            .types
            .raw_entry_mut()
            .from_key(&ty.name)
            .or_insert_with(|| (Arc::clone(&ty.name), ty));

        vm_type
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
        // result to be  (object = None, member = path). If the separator is found, rsplit_once
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
