use std::rc::Rc;

use hashbrown::hash_map::RawEntryMut;

use crate::{program::Program, utils::HashMap, value::Value, vm::Vm};

pub type NativeFn = dyn Fn(&mut Vm, &Program) -> Option<Value> + 'static;

pub struct NativeRegistry(HashMap<String, Rc<NativeFn>>);

impl NativeRegistry {
    pub fn new() -> Self {
        Self(HashMap::with_hasher(Default::default()))
    }

    pub fn register<F>(&mut self, name: impl ToString + AsRef<str>, f: F) -> Result<(), F>
    where
        F: Fn(&mut Vm, &Program) -> Option<Value> + 'static,
    {
        let name_ref = name.as_ref();

        let entry = self.0.raw_entry_mut().from_key(name_ref);

        match entry {
            RawEntryMut::Vacant(entry) => {
                entry.insert(name.to_string(), Rc::new(f));
                Ok(())
            }
            RawEntryMut::Occupied(_) => Err(f),
        }
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<Rc<NativeFn>> {
        self.0.get(name.as_ref()).cloned()
    }
}
