use serde::{Deserialize, Serialize};
use std::ops::Index;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default, Serialize, Deserialize)]
pub struct Symbols {
    data: String,
    indices: Vec<SymbolSpan>,
}

impl Symbols {
    pub fn new() -> Self {
        Self {
            data: String::new(),
            indices: Vec::new(),
        }
    }

    pub fn get_or_push(&mut self, s: impl AsRef<str>) -> Symbol {
        fn inner(this: &mut Symbols, s: &str) -> Symbol {
            // Since substring searching is O(m * n), and self.data is expected to be somewhat
            // large, we can possibly speed this search up by first checking self.indices for any
            // strings matching the length of the given string, then filtering self.data to only
            // strings of the same length. Only those strings with the same length will be checked
            // for equality in that case. If no strings match, then we just push the string to the
            // end of self.data and push the index data to self.indices.
            let possible_symbol = this
                .indices
                .iter()
                .enumerate()
                .filter(|(_, span)| span.len == s.len())
                .map(|(idx, span)| (idx, &this.data[span.start..span.start + span.len]))
                .find_map(|(idx, string)| (s == string).then_some(idx))
                .map(Symbol);

            match possible_symbol {
                Some(symbol) => symbol,
                None => {
                    let span = SymbolSpan {
                        start: this.data.len(),
                        len: s.len(),
                    };
                    let idx = this.indices.len();

                    this.data.push_str(s);
                    this.indices.push(span);

                    Symbol(idx)
                }
            }
        }

        inner(self, s.as_ref())
    }

    pub fn get_or_push_iter<'a, I>(&mut self, iter: I) -> Symbol
    where
        I: IntoIterator<Item = &'a str>,
        I::IntoIter: Clone,
    {
        let iter = iter.into_iter();

        let total_len: usize = iter.clone().map(|elem| elem.len()).sum();

        // This might be the ugliest shit I've ever written.
        let possible_symbol = self
            .indices
            .iter()
            .enumerate()
            .filter(|(_, span)| span.len == total_len)
            .map(|(idx, span)| (idx, &self.data[span.start..span.start + span.len]))
            .find_map(|(idx, string)| {
                string
                    .as_bytes()
                    .iter()
                    .eq(iter.clone().flat_map(|s| s.as_bytes()))
                    .then_some(idx)
            })
            .map(Symbol);

        match possible_symbol {
            Some(symbol) => symbol,
            None => {
                let span = SymbolSpan {
                    start: self.data.len(),
                    len: total_len,
                };

                let idx = self.indices.len();

                self.data.extend(iter);
                self.indices.push(span);

                Symbol(idx)
            }
        }
    }

    pub fn len(&self) -> usize {
        self.indices.len()
    }

    pub fn byte_len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, symbol: Symbol) -> Option<&str> {
        self.indices.get(symbol.0).map(|span| {
            // SAFETY: if this closure runs, the information in `span` corresponds to a valid
            // substring inside of `self.data`, since a `SymbolSpan` can only be created from one of
            // the methods above, which save the starting index and length of a previously pushed
            // string.
            unsafe { self.data.get_unchecked(span.start..span.start + span.len) }
        })
    }

    pub fn get_span(&self, index: usize) -> Option<SymbolSpan> {
        self.indices.get(index).copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Symbol, &str)> + '_ {
        self.indices.iter().enumerate().map(|(idx, span)| {
            let sym = Symbol(idx);
            let s = unsafe { self.data.get_unchecked(span.start..span.start + span.len) };

            (sym, s)
        })
    }

    pub fn symbols(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.indices.iter().enumerate().map(|(idx, _)| Symbol(idx))
    }

    pub fn strs(&self) -> impl Iterator<Item = &str> + '_ {
        self.indices
            .iter()
            .map(|span| unsafe { self.data.get_unchecked(span.start..span.start + span.len) })
    }
}

impl Index<Symbol> for Symbols {
    type Output = str;

    fn index(&self, index: Symbol) -> &Self::Output {
        self.get(index).expect("index out of bounds")
    }
}

impl Index<SymbolSpan> for Symbols {
    type Output = str;

    fn index(&self, index: SymbolSpan) -> &Self::Output {
        self.data
            .get(index.start..index.start + index.len)
            .expect("index out of bounds")
    }
}

#[derive(
    Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default, Serialize, Deserialize,
)]
pub struct SymbolSpan {
    start: usize,
    len: usize,
}

#[derive(
    Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default, Serialize, Deserialize,
)]
pub struct Symbol(pub(crate) usize);
