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

use super::{
    function::{Function, NewFunction},
    ops::OpCode,
    Result as VmResult,
};

#[derive(Clone, Default)]
pub struct Cache {
    functions: HashTable<Function>,
    new_functions: HashTable<NewFunction>,
    native_functions: IntHashMap<Symbol, Arc<NativeFn>>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            functions: HashTable::new(),
            new_functions: HashTable::new(),
            native_functions: IntHashMap::default(),
        }
    }

    // FIXME: There is a huge oversight here that doesn't consider the fact that multiple items may
    // be in a single bucket, and it always marks them as equivalent, causing weird bugs. Fix this
    // first!
    pub fn get_or_insert_function<F>(&mut self, symbol: Symbol, f: F) -> VmResult<Function>
    where
        F: FnOnce() -> VmResult<Function>,
    {
        // Integers that are <= 64 bits are guaranteed to be unique, so there's no need to do any
        // work to determine if any hash collisions have different keys. Any hash collisions that
        // occur are guaranteed to be because the keys are equivalent.
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

    // FIXME: Read above
    pub fn get_or_insert_new_function<F>(&mut self, symbol: Symbol, f: F) -> VmResult<NewFunction>
    where
        F: FnOnce() -> VmResult<NewFunction>,
    {
        // Integers that are <= 64 bits are guaranteed to be unique, so there's no need to do any
        // work to determine if any hash collisions have different keys. Any hash collisions that
        // occur are guaranteed to be because the keys are equivalent.
        // TODO: Do this for native functions too.
        let hash = symbol.0 as u64;
        let func_opt = self.new_functions.find(hash, |_| true);

        match func_opt {
            Some(func) => Ok(*func),
            None => {
                let func = f()?;

                Ok(*self.new_functions.insert_unique(hash, func, |_| hash).get())
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
