use std::{
    fmt::{self, Debug},
    sync::Arc,
};

use hashbrown::hash_map::{Entry, RawEntryMut};
use indexmap::map::{raw_entry_v1::RawEntryMut as RawIndexEntryMut, RawEntryApiV1};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{
    module::core::collections::{Array, Dict, Str},
    object::{Type, TypeBuilder, VmObject},
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

pub mod core;

fn relocate(symbols: &mut Symbols, module: &mut Module, code_offset: usize, const_offset: usize) {
    let constants = module.constants.iter_mut();
    let opcodes = module.functions.code.iter_mut();
    let methods = module
        .types
        .values_mut()
        .flat_map(|ty| ty.methods.values_mut());
    let mut symbol_map =
        FxHashMap::with_capacity_and_hasher(module.symbols.len(), Default::default());

    for (symbol, string) in module.symbols.iter() {
        let new_symbol = symbols.get_or_push(string);

        symbol_map.insert(symbol, new_symbol);
    }

    for value in constants {
        match value {
            Value::Function(Function(n)) => *n += code_offset,
            Value::Symbol(sym) => {
                let new_symbol = symbol_map
                    .get(sym)
                    .expect("every symbol should be in the map");
                *sym = *new_symbol
            }
            _ => {}
        }
    }

    for opcode in opcodes {
        match opcode {
            OpCode::Push(Value::Function(Function(n))) => *n += code_offset,
            OpCode::PushConst(n) => *n += const_offset,
            OpCode::ResolveImm(sym) => {
                let new_symbol = symbol_map
                    .get(sym)
                    .expect("every symbol should be in the map");
                *sym = *new_symbol
            }
            OpCode::CallImm(Function(n)) => *n += code_offset,
            OpCode::CallNative(sym) => {
                let new_symbol = symbol_map
                    .get(sym)
                    .expect("every symbol should be in the map");
                *sym = *new_symbol
            }
            _ => {}
        }
    }

    for Function(method) in methods {
        *method += code_offset;
    }
}

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
            self.symbols.get_or_push_iter([&type_name, ".", &name]);

            let start = self.functions.code.len();
            self.functions.code.extend(&code[range]);
            let func = Function::new(start);

            methods.insert(name, func);
        }

        let ty = entry.insert(Type {
            name: type_name,
            fields,
            methods,
        });

        ty
    }

    pub fn load_module(&mut self, mut other: Module) {
        relocate(
            &mut self.symbols,
            &mut other,
            self.functions.code.len(),
            self.constants.len(),
        );

        // Types
        other
            .types
            .into_iter()
            .for_each(|(name, ty)| match self.types.entry(name) {
                Entry::Occupied(entry) => panic!("Registered duplicate type {}", entry.key()),
                Entry::Vacant(entry) => {
                    entry.insert(ty);
                }
            });

        // Constants
        self.constants.extend(other.constants);

        // Functions
        other
            .functions
            .indices
            .into_iter()
            .for_each(|(name, func)| match self.functions.indices.entry(name) {
                Entry::Occupied(entry) => panic!("Registered duplicate function {}", entry.key()),
                Entry::Vacant(entry) => {
                    entry.insert(func);
                }
            });
        self.functions.code.extend(other.functions.code);
        self.functions.code.push(OpCode::Halt);

        // Native functions
        other.native_functions.into_iter().for_each(|(name, func)| {
            match self.native_functions.entry(name) {
                Entry::Occupied(entry) => {
                    panic!("Registered duplicate native function {}", entry.key())
                }
                Entry::Vacant(entry) => {
                    entry.insert(func);
                }
            }
        });
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

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Error)]
#[error("\"{0}\" is not a valid path")]
pub struct ModulePathError<'a>(&'a str);

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum ModulePath<'a> {
    FieldOrMethod {
        ty: &'a str,
        field_or_method: &'a str,
    },
    Function(&'a str),
}

impl<'a> ModulePath<'a> {
    pub fn new(path: &'a str) -> Result<Self, ModulePathError<'a>> {
        #[inline(always)]
        fn is_valid_identifier(s: &str) -> bool {
            s.chars().all(|c| c == '_' || c.is_alphanumeric())
        }

        let split = path.rsplit_once('.');

        match split {
            // ".", ".method", or "Type."
            Some(("", "")) | Some(("", _)) | Some((_, "")) => Err(ModulePathError(path)),
            Some((ty, field_or_method)) => {
                let valid_ty = ty
                    .split("::")
                    .all(|substr| is_valid_identifier(substr) && !substr.is_empty());
                let valid_field_or_method = is_valid_identifier(field_or_method);

                if valid_ty && valid_field_or_method {
                    Ok(ModulePath::FieldOrMethod {
                        ty,
                        field_or_method,
                    })
                } else {
                    Err(ModulePathError(path))
                }
            }
            _ if !path.is_empty() => {
                let valid_function = path
                    .split("::")
                    .all(|substr| is_valid_identifier(substr) && !substr.is_empty());

                if valid_function {
                    Ok(ModulePath::Function(path))
                } else {
                    Err(ModulePathError(path))
                }
            }
            _ => Err(ModulePathError(path)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{ModulePath, ModulePathError};

    #[test]
    fn well_formed_fn_paths() {
        let a = "a::b::c::Type.field";
        let b = "d::function";

        assert_eq!(
            ModulePath::new(a),
            Ok(ModulePath::FieldOrMethod {
                ty: "a::b::c::Type",
                field_or_method: "field",
            })
        );

        assert_eq!(ModulePath::new(b), Ok(ModulePath::Function("d::function")));
    }

    #[test]
    fn ill_formed_fn_paths() {
        let only_dot = "a::b::c::.";
        let missing_type_or_function = "a::b::";
        let missing_field = "a::b::Type.";
        let missing_type = "a::b::.field";
        let duplicate_type = "a::Type.Type.member";
        let lots_of_dots = "a::Type.........member";
        let empty_module_component = "a::b::::c::function";
        let non_alphanumeric_component = "a.b::c::d::function";

        assert_eq!(ModulePath::new(only_dot), Err(ModulePathError(only_dot)));
        assert_eq!(
            ModulePath::new(missing_type_or_function),
            Err(ModulePathError(missing_type_or_function))
        );
        assert_eq!(
            ModulePath::new(missing_field),
            Err(ModulePathError(missing_field))
        );
        assert_eq!(
            ModulePath::new(missing_type),
            Err(ModulePathError(missing_type))
        );
        assert_eq!(ModulePath::new(""), Err(ModulePathError("")));
        assert_eq!(
            ModulePath::new(duplicate_type),
            Err(ModulePathError(duplicate_type))
        );
        assert_eq!(
            ModulePath::new(lots_of_dots),
            Err(ModulePathError(lots_of_dots))
        );
        assert_eq!(
            ModulePath::new(empty_module_component),
            Err(ModulePathError(empty_module_component))
        );
        assert_eq!(
            ModulePath::new(non_alphanumeric_component),
            Err(ModulePathError(non_alphanumeric_component))
        );
    }
}
