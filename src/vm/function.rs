use hashbrown::hash_map::RawEntryMut;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::utils::FxHashMap;

use super::ops::OpCode;

#[derive(
    Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default, Serialize, Deserialize,
)]
pub struct Function(pub(crate) usize);

impl Function {
    #[inline(always)]
    pub fn new(start: usize) -> Self {
        Self(start)
    }
}

#[derive(Clone, PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct Functions {
    indices: FxHashMap<Arc<str>, Function>,
    code: Vec<OpCode>,
}

impl Functions {
    pub fn new() -> Self {
        Self {
            indices: FxHashMap::default(),
            code: Vec::new(),
        }
    }

    pub fn define<I: IntoIterator<Item = OpCode>>(
        &mut self,
        name: impl AsRef<str>,
        func: I,
    ) -> Result<Function, I> {
        let name = name.as_ref();

        match self.indices.raw_entry_mut().from_key(name) {
            RawEntryMut::Vacant(entry) => {
                let start = self.code.len();
                self.code.extend(func);
                let func = Function::new(start);

                entry.insert(Arc::from(name), func);
                Ok(func)
            }
            RawEntryMut::Occupied(_) => Err(func),
        }
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<Function> {
        self.indices.get(name.as_ref()).copied()
    }
}
