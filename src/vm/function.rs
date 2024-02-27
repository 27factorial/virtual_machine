use std::{ops::Index, slice::SliceIndex, sync::Arc};

use serde::{Deserialize, Serialize};

use super::ops::OpCode;

#[derive(Clone, PartialEq, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Function(pub(crate) Arc<[OpCode]>);

impl Function {
    pub fn new(ops: impl IntoIterator<Item = OpCode>) -> Self {
        let ops = ops.into_iter().collect();

        Self(ops)
    }

    pub fn get<T: SliceIndex<[OpCode]>>(&self, index: T) -> Option<&T::Output> {
        self.0.get(index)
    }
}

impl FromIterator<OpCode> for Function {
    fn from_iter<T: IntoIterator<Item = OpCode>>(ops: T) -> Self {
        Self::new(ops)
    }
}

impl<T: SliceIndex<[OpCode]>> Index<T> for Function {
    type Output = T::Output;

    fn index(&self, index: T) -> &Self::Output {
        self.0.index(index)
    }
}

impl Default for Function {
    fn default() -> Self {
        Self(Arc::from([]))
    }
}