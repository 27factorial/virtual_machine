use std::{
    ops::{Index, Range},
    slice::SliceIndex,
    sync::Arc,
};

use serde::{Deserialize, Serialize};

use crate::{program::Program, utils::IntoVmResult};

use super::{ops::OpCode, Result as VmResult, VmErrorKind};

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
