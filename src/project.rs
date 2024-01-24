use hashbrown::HashMap;

use std::ops::Index;

use crate::{
    object::TypeMeta,
    ops::{Function, OpCode},
    value::Value,
    vm::CallFrame,
};

const VALID_MAGIC: &[u8; 11] = b"27FACTORIAL";

pub struct Program {
    pub(crate) header: ProgramHeader,
    pub(crate) constants: Vec<Value>,
    pub(crate) strings: ProgramStrings,
    pub(crate) functions: Vec<Function>,
    pub(crate) types: Vec<TypeMeta>,
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

    pub fn insert(&mut self, s: impl AsRef<str>) -> StringIndex {
        fn inner(this: &mut ProgramStrings, s: &str) -> StringIndex {
            // Since substring searching is O(m * n), and self.data is expected to be somewhat
            // large, we can search through the indices first and see if there's a string with
            // the same length. If there's not, we know that the provided string is not already in
            // the set.
            let possible_idx = this
                .indices
                .iter()
                .filter_map(|idx| {
                    (idx.len == s.len()).then(|| (idx, &this.data[idx.start..idx.start + idx.len]))
                })
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
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
struct StringIndex {
    start: usize,
    len: usize,
}
