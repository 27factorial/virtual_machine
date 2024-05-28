use crate::object::VmObject;

use self::collections::{Array, Dict, Set, Str};

use super::{Module, ToModule};

pub mod closure;
pub mod collections;
pub mod io;

/// The core library of the PFVM.
pub struct CoreLib;

impl ToModule for CoreLib {
    fn to_module(self) -> Module {
        let mut module = Module::new();

        Array::register(&mut module);
        Str::register(&mut module);
        Dict::register(&mut module);
        Set::register(&mut module);

        module
    }
}
