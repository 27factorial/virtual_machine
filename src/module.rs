use std::{
    fmt::{self, Debug},
    sync::Arc,
};

use hashbrown::hash_map::{Entry, RawEntryMut};
use indexmap::map::{raw_entry_v1::RawEntryMut as RawIndexEntryMut, RawEntryApiV1};
use serde::{Deserialize, Serialize};

use crate::{
    object::{array::VmArray, dict::VmDictionary, string::VmString, Type, TypeBuilder, VmObject},
    symbol::{Symbol, Symbols},
    utils::{FxHashMap, FxIndexMap},
    value::Value,
    vm::{
        builtin,
        function::{Function, Functions},
        ops::OpCode,
        CallFrame, Result as VmResult, Vm,
    },
};

pub type NativeFn = dyn Fn(&mut Vm, &CallFrame) -> VmResult<Value> + Send + Sync + 'static;

/// A trait for converting types into a module. This can be used to implement libraries written in
/// Rust that can be loaded into the VM. It's also how the core libraries of the VM are implemented.
pub trait ToModule {
    fn to_module(self) -> Module;
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Module {
    pub(crate) symbols: Symbols,
    pub(crate) types: FxHashMap<Arc<str>, Type>,
    pub(crate) constants: Vec<Value>,
    pub(crate) functions: Functions,
    #[serde(skip)]
    pub(crate) native_functions: FxHashMap<Arc<str>, Arc<NativeFn>>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            symbols: Symbols::new(),
            types: FxHashMap::default(),
            constants: Vec::new(),
            functions: Functions::new(),
            native_functions: FxHashMap::default(),
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
        F: Fn(&mut Vm, &CallFrame) -> VmResult<Value> + Send + Sync + 'static,
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

            let start = self.functions.code.len();
            self.functions.code.extend(&code[range]);
            let func = Function::new(start);

            self.functions.indices.insert(Arc::clone(&name), func);
            methods.insert(name, func);
        }

        let ty = entry.insert(Type {
            name: type_name,
            fields,
            methods,
        });

        ty
    }
}

impl Debug for Module {
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

        f.debug_struct("Module")
            .field("symbols", &self.symbols)
            .field("types", &self.types)
            .field("constants", &self.constants)
            .field("functions", &self.functions)
            .field("native_functions", &NativeFnsDebug(&self.native_functions))
            .finish()
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Program {
    modules: FxIndexMap<Arc<str>, Module>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            modules: FxIndexMap::default(),
        }
    }

    pub fn load<M: ToModule>(&mut self, name: impl AsRef<str>, module: M) -> Result<(), M> {
        let name = name.as_ref();

        match self.modules.raw_entry_mut_v1().from_key(name) {
            RawIndexEntryMut::Vacant(entry) => {
                entry.insert(Arc::from(name), module.to_module());
                Ok(())
            }
            RawIndexEntryMut::Occupied(_) => Err(module),
        }
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&Module> {
        let name = name.as_ref();
        self.modules.get(name)
    }

    pub fn get_index(&self, module: usize) -> Option<&Module> {
        self.modules.get_index(module).map(|(_, module)| module)
    }
}

/// The core library of the PFVM.
pub struct CoreLib;

impl ToModule for CoreLib {
    fn to_module(self) -> Module {
        let mut module = Module::new();

        VmArray::register(&mut module);
        VmString::register(&mut module);
        VmDictionary::register(&mut module);

        module
    }
}

pub struct Io;

impl ToModule for Io {
    fn to_module(self) -> Module {
        let mut module = Module::new();

        let print_sym = module.define_symbol("print");
        let println_sym = module.define_symbol("eprintln");

        module
            .define_function(
                print_sym,
                [OpCode::CallBuiltin(builtin::PRINT), OpCode::Ret],
            )
            .unwrap();
        module
            .define_function(
                println_sym,
                [OpCode::CallBuiltin(builtin::PRINTLN), OpCode::Ret],
            )
            .unwrap();

        module
    }
}

#[cfg(test)]
mod test {}
