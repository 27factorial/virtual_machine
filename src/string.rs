pub struct Strings {
    data: String,
    indices: Vec<StringSpan>,
}

impl Strings {
    pub fn new() -> Self {
        Self {
            data: String::new(),
            indices: Vec::new(),
        }
    }

    pub fn get_or_push(&mut self, s: impl AsRef<str>) -> StringSpan {
        fn inner(this: &mut Strings, s: &str) -> StringSpan {
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
                    let idx = StringSpan {
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

    pub fn get(&self, symbol: StringRef) -> Option<&str> {
        self.indices
            .get(symbol.0)
            .and_then(|idx| self.data.get(idx.start..idx.start + idx.len))
    }

    pub fn get_span(&self, index: usize) -> Option<StringSpan> {
        self.indices.get(index).copied()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub struct StringSpan {
    start: usize,
    len: usize,
}

pub struct StringRef(pub usize);
