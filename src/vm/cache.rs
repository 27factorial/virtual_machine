use std::{
    fmt::{self, Debug},
    sync::Arc,
};

use hashbrown::HashTable;

use crate::{
    program::NativeFn,
    symbol::Symbol,
    utils::{IntEntry, IntHashMap},
};

use super::{function::Function, Result as VmResult};

#[derive(Clone, Default)]
pub struct Cache {
    functions: HashTable<Function>,
    native_functions: IntHashMap<Symbol, Arc<NativeFn>>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            functions: HashTable::new(),
            native_functions: IntHashMap::default(),
        }
    }

    pub fn get_or_insert_function_2<F>(&mut self, symbol: Symbol, f: F) -> VmResult<Function>
    where
        F: FnOnce() -> VmResult<Function>,
    {
        // integers that are <= 64 bits are guaranteed to be unique, so there's no need to do any
        // work to determine if any hash collisions have different keys, since that will never be
        // any hash collisions guarantee that the keys are equivalent.
        // TODO: Do this for native functions too.
        let hash = symbol.0 as u64;
        let func_opt = self.functions.find(hash, |_| true);

        match func_opt {
            Some(func) => Ok(func.clone()),
            None => {
                let func = f()?;

                Ok(self
                    .functions
                    .insert_unique(hash, func, |_| hash)
                    .get()
                    .clone())
            }
        }
    }

    #[inline(always)]
    pub fn native_function_entry(&mut self, symbol: Symbol) -> IntEntry<'_, Symbol, Arc<NativeFn>> {
        self.native_functions.entry(symbol)
    }
}

impl Debug for Cache {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct NativeFnsDebug<'a>(&'a IntHashMap<Symbol, Arc<NativeFn>>);

        impl Debug for NativeFnsDebug<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_map()
                    .entries(
                        self.0
                            .iter()
                            .map(|(k, v)| (k, format!("<native fn @ {:p}>", &**v))),
                    )
                    .finish()
            }
        }

        f.debug_struct("Cache")
            .field("functions", &self.functions)
            .field("native_functions", &NativeFnsDebug(&self.native_functions))
            .finish()
    }
}
