use crate::object::VmType;
use crate::symbol::{Symbol, Symbols};
use crate::utils::FxHashMap;
use crate::value::Value;
use crate::vm::function::{Function, NewFunction};
use crate::vm::ops::OpCode;
use crate::vm::Result as VmResult;
use crate::vm::Vm;
use hashbrown::hash_map::RawEntryMut;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use std::iter;
use std::sync::Arc;

pub const VALID_MAGIC: &[u8; 7] = b"27FCTRL";

pub type NativeFn = dyn Fn(&mut Vm) -> VmResult<Value> + 'static;

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Program {
    pub(crate) constants: Vec<Value>,
    pub(crate) functions: FxHashMap<Arc<str>, Function>,
    pub(crate) functions_2: FxHashMap<Arc<str>, NewFunction>,
    pub(crate) code: Vec<OpCode>,
    #[serde(skip)]
    pub(crate) native_functions: FxHashMap<Arc<str>, Arc<NativeFn>>,
    pub(crate) types: FxHashMap<Arc<str>, VmType>,
    pub(crate) symbols: Symbols,
}

impl Program {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            functions: FxHashMap::default(),
            functions_2: FxHashMap::default(),
            code: Vec::new(),
            native_functions: FxHashMap::default(),
            types: FxHashMap::default(),
            symbols: Symbols::new(),
        }
    }

    pub fn define_symbol(&mut self, symbol: impl AsRef<str>) -> Symbol {
        self.symbols.get_or_push(symbol.as_ref())
    }

    pub fn define_symbol_iter<'a, I>(&mut self, iter: I) -> Symbol
    where
        I: IntoIterator<Item = &'a str>,
        I::IntoIter: Clone,
    {
        self.symbols.get_or_push_iter(iter)
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

    pub fn define_function_2<F: IntoIterator<Item = OpCode>>(
        &mut self,
        symbol: Symbol,
        func: F,
    ) -> Result<NewFunction, F> {
        if let Some(name) = self.symbols.get(symbol) {
            match self.functions_2.raw_entry_mut().from_key(name) {
                RawEntryMut::Vacant(entry) => {
                    let start = self.code.len();
                    self.code.extend(func);
                    let func = NewFunction::new(start);

                    entry.insert(Arc::from(name), func);
                    Ok(func)
                }
                RawEntryMut::Occupied(_) => Err(func),
            }
        } else {
            Err(func)
        }
    }

    pub fn define_native_function<F>(&mut self, symbol: Symbol, func: F) -> Result<(), F>
    where
        F: Fn(&mut Vm) -> VmResult<Value> + 'static,
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

impl Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct NativeFnsDebug<'a>(&'a FxHashMap<Arc<str>, Arc<NativeFn>>);

        impl Debug for NativeFnsDebug<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_map()
                    .entries(
                        self.0
                            .iter()
                            .map(|(k, v)| (&**k, format!("<native fn @ {:p}>", &**v))),
                    )
                    .finish()
            }
        }

        f.debug_struct("Program")
            .field("constants", &self.constants)
            .field("functions", &self.functions)
            .field("functions_2", &self.functions_2)
            .field("code", &self.code)
            .field("native_functions", &NativeFnsDebug(&self.native_functions))
            .field("types", &self.types)
            .field("symbols", &self.symbols)
            .finish()
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
