use crate::object::{Type, TypeBuilder};
use crate::symbol::{Symbol, Symbols};
use crate::utils::FxHashMap;
use crate::value::Value;
use crate::vm::function::{Function, Functions};
use crate::vm::ops::OpCode;
use crate::vm::Vm;
use crate::vm::{CallFrame, Result as VmResult};
use hashbrown::hash_map::{Entry, RawEntryMut};
use serde::{Deserialize, Serialize};
use std::fmt::{self, Debug};
use std::sync::Arc;

pub const VALID_MAGIC: &[u8; 4] = b"PFVM";

pub type NativeFn = dyn Fn(&mut Vm, &CallFrame) -> VmResult<Value> + 'static;

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Program {
    pub(crate) constants: Vec<Value>,
    pub(crate) functions: Functions,
    pub(crate) function_indices: FxHashMap<Arc<str>, Function>,
    pub(crate) code: Vec<OpCode>,
    #[serde(skip)]
    pub(crate) native_functions: FxHashMap<Arc<str>, Arc<NativeFn>>,
    pub(crate) types: FxHashMap<Arc<str>, Type>,
    pub(crate) symbols: Symbols,
}

impl Program {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            functions: Functions::new(),
            function_indices: FxHashMap::default(),
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

    pub fn define_function<I: IntoIterator<Item = OpCode>>(
        &mut self,
        symbol: Symbol,
        func: I,
    ) -> Result<Function, I> {
        match self.symbols.get(symbol) {
            Some(name) => self.functions.define(name, func),
            None => Err(func),
        }
    }

    pub fn define_native_function<F>(&mut self, symbol: Symbol, func: F) -> Result<(), F>
    where
        F: Fn(&mut Vm, &CallFrame) -> VmResult<Value> + 'static,
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

    pub(crate) fn register_type(&mut self, builder: TypeBuilder) -> &Type {
        let TypeBuilder {
            name: type_name,
            fields,
            methods: method_ranges,
            code,
        } = builder;

        let Entry::Vacant(entry) = self.types.entry(Arc::clone(&type_name)) else {
            panic!("Attempt to register duplicate type {type_name}");
        };

        self.symbols.get_or_push(&type_name);

        let mut methods =
            FxHashMap::with_capacity_and_hasher(method_ranges.len(), Default::default());

        for (name, range) in method_ranges {
            self.symbols.get_or_push_iter([&type_name, "::", &name]);

            let start = self.code.len();
            self.code.extend(&code[range]);
            let func = Function::new(start);

            methods.insert(name, func);
        }

        let ty = entry.insert(Type {
            name: Arc::clone(&type_name),
            fields,
            methods,
        });

        ty
    }
}

impl Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct NativeFnsDebug<'a>(&'a FxHashMap<Arc<str>, Arc<NativeFn>>);

        impl Debug for NativeFnsDebug<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            .field("function_indices", &self.function_indices)
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
