use hashbrown::HashMap;

use std::ops::Index;

use crate::{
    object::TypeMeta,
    ops::{Function, OpCode},
    value::Value,
    vm::CallFrame,
};

const VALID_MAGIC: &[u8; 7] = b"27FCTRL";

pub struct Program {
    pub(crate) header: ProgramHeader,
    pub(crate) constants: Vec<Value>,
    pub(crate) strings: ProgramStrings,
    pub(crate) functions: Vec<Function>,
    pub(crate) types: Vec<TypeMeta>,
}

impl Program {
    pub fn get_string(&self, index: StringIndex) -> Option<&str> {
        self.strings.get(index)
    }
}

impl Index<CallFrame> for Program {
    type Output = OpCode;

    fn index(&self, index: CallFrame) -> &Self::Output {
        self.functions.index(index.func).0.index(index.ip)
    }
}

pub struct ProgramHeader {
    magic: [u8; VALID_MAGIC.len()],
    pub(crate) type_indices: HashMap<String, usize>,
}

pub struct ProgramStrings {
    data: String,
    indices: Vec<StringIndex>,
}

impl ProgramStrings {
    pub fn new() -> Self {
        Self {
            data: String::new(),
            indices: Vec::new(),
        }
    }

    pub fn push(&mut self, s: impl AsRef<str>) -> StringIndex {
        fn inner(this: &mut ProgramStrings, s: &str) -> StringIndex {
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

    pub fn get(&self, index: StringIndex) -> Option<&str> {
        self.data.get(index.start..index.start + index.len)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub struct StringIndex {
    start: usize,
    len: usize,
}
