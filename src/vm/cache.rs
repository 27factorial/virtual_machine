use std::sync::Arc;

use crate::{program::NativeFn, string::Symbol, utils::{IntEntry, IntHashMap}, value::Value};

use super::ops::Function;

#[derive(Clone, Default)]
pub struct Cache {
    functions: IntHashMap<Symbol, Function>,
    native_functions: IntHashMap<Symbol, Arc<NativeFn>>,
    locals: Vec<Vec<Value>>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            functions: IntHashMap::default(),
            native_functions: IntHashMap::default(),
            locals: Vec::new(),
        }
    }

    pub fn cache_locals(&mut self, locals: Vec<Value>) {
        self.locals.push(locals)
    }

    pub fn function_entry(&mut self, symbol: Symbol) -> IntEntry<'_, Symbol, Function> {
        self.functions.entry(symbol)
    }

    pub fn native_function_entry(&mut self, symbol: Symbol) -> IntEntry<'_, Symbol, Arc<NativeFn>> {
        self.native_functions.entry(symbol)
    }

    pub fn take_locals(&mut self) -> Option<Vec<Value>> {
        self.locals.pop()
    }
}