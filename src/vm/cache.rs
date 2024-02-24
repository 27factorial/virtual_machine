use std::{fmt::Debug, sync::Arc};

use crate::{
    program::NativeFn,
    symbol::Symbol,
    utils::{IntEntry, IntHashMap},
};

use super::ops::Function;

#[derive(Clone, Default)]
pub struct Cache {
    functions: IntHashMap<Symbol, Function>,
    native_functions: IntHashMap<Symbol, Arc<NativeFn>>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            functions: IntHashMap::default(),
            native_functions: IntHashMap::default(),
        }
    }

    pub fn function_entry(&mut self, symbol: Symbol) -> IntEntry<'_, Symbol, Function> {
        self.functions.entry(symbol)
    }

    pub fn native_function_entry(&mut self, symbol: Symbol) -> IntEntry<'_, Symbol, Arc<NativeFn>> {
        self.native_functions.entry(symbol)
    }
}

impl Debug for Cache {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct NativeFnsDebug<'a>(&'a IntHashMap<Symbol, Arc<NativeFn>>);

        impl Debug for NativeFnsDebug<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
