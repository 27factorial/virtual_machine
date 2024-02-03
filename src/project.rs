use hashbrown::HashMap;

use std::ops::Index;

use crate::{
    object::TypeMeta,
    ops::{Function, OpCode},
    value::Value,
    vm::CallFrame,
};

const VALID_MAGIC: &[u8; 7] = b"27FCTRL";

pub struct ProgramFile {
    pub(crate) types: HashMap<StringIndex, TypeMeta>,
    pub(crate) constants: Vec<Value>,
    pub(crate) functions: Vec<Function>,
    
}

pub struct Strings {
    data: String,
    indices: Vec<StringIndex>,
}

impl Strings {
    pub fn new() -> Self {
        Self {
            data: String::new(),
            indices: Vec::new(),
        }
    }

    pub fn get_or_push(&mut self, s: impl AsRef<str>) -> StringIndex {
        fn inner(this: &mut Strings, s: &str) -> StringIndex {
            // Since substring searching is O(m * n), and self.data is expected to be somewhat
            // large, we can possibly speed this search up by first checking self.indices for any
            // strings matching the length of the given string, then filtering self.data to only
            // strings of the same length. Only those strings with the same length will be checked
            // for equality in that case. If no strings match, then we just push the string to the
            // end of self.data and push the index data to self.indices.
            let possible_idx = this
                .indices
                .iter()
                .filter(|idx| idx.len == s.len())
                .map(|idx| (idx, &this.data[idx.start..idx.start + idx.len]))
                .find_map(|(idx, string)| (s == string).then_some(*idx));

            match possible_idx {
                Some(idx) => idx,
                None => {
                    let idx = StringIndex {
                        start: this.data.len(),
                        len: s.len(),
                    };

                    this.data.push_str(s);
                    this.indices.push(idx);

                    idx
                }
            }
        }

        inner(self, s.as_ref())
    }

    pub fn get(&self, index: usize) -> Option<&str> {
        self.indices
            .get(index)
            .and_then(|idx| self.data.get(idx.start..idx.start + idx.len))
    }

    pub fn get_index(&self, index: usize) -> Option<StringIndex> {
        self.indices.get(index).copied()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub struct StringIndex {
    start: usize,
    len: usize,
}
