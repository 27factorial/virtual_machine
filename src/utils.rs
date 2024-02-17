use rustc_hash::FxHasher;
use std::hash::BuildHasherDefault;

use crate::vm::{ops::{VmError, VmErrorKind}, CallFrame};

pub type HashMap<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<FxHasher>>;

mod sealed {
    pub trait Sealed {}

    impl<T> Sealed for Option<T> {}
}
