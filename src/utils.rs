use crate::vm::{CallFrame, Result as VmResult, Vm, VmError, VmErrorKind};
use rustc_hash::FxHasher;
use serde::{Deserialize, Serialize};
use std::hash::{BuildHasherDefault, Hasher};

pub type FxHashMap<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<FxHasher>>;
pub type FxEntry<'a, K, V> = hashbrown::hash_map::Entry<'a, K, V, BuildHasherDefault<FxHasher>>;
pub type IntHashMap<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<IntHasher>>;
pub type IntEntry<'a, K, V> = hashbrown::hash_map::Entry<'a, K, V, BuildHasherDefault<IntHasher>>;

pub trait IntoVmResult: sealed::Sealed {
    type Ok;

    fn vm_result<'a>(self, kind: VmErrorKind, frame: impl Into<Option<&'a CallFrame>>) -> VmResult<Self::Ok>;
}

impl<T> IntoVmResult for Option<T> {
    type Ok = T;

    #[inline]
    fn vm_result<'a>(self, kind: VmErrorKind, frame: impl Into<Option<&'a CallFrame>>) -> VmResult<T> {
        match self {
            Some(t) => Ok(t),
            None => Err(VmError::new(kind, frame)),
        }
    }
}

impl<T, E> IntoVmResult for Result<T, E> {
    type Ok = T;

    #[inline]
    fn vm_result<'a>(self, kind: VmErrorKind, frame: impl Into<Option<&'a CallFrame>>) -> VmResult<T> {
        match self {
            Ok(t) => Ok(t),
            Err(_) => Err(VmError::new(kind, frame)),
        }
    }
}

#[derive(
    Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default, Serialize, Deserialize,
)]
pub struct IntHasher(u64, #[cfg(debug_assertions)] bool);

impl Hasher for IntHasher {
    #[inline(always)]
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, _: &[u8]) {
        unimplemented!("the `Hasher::write` trait method is not supported by `IntHasher`");
    }

    #[inline(always)]
    fn write_u8(&mut self, i: u8) {
        self.write_u64(i as u64)
    }

    #[inline(always)]
    fn write_u16(&mut self, i: u16) {
        self.write_u64(i as u64)
    }

    #[inline(always)]
    fn write_u32(&mut self, i: u32) {
        self.write_u64(i as u64)
    }

    #[inline(always)]
    fn write_u64(&mut self, i: u64) {
        #[cfg(debug_assertions)]
        if self.1 {
            panic!("attempt to write multiple values to `IntHasher`")
        }

        self.0 = i;

        #[cfg(debug_assertions)]
        {
            self.1 = true;
        }
    }

    #[inline(always)]
    fn write_u128(&mut self, i: u128) {
        self.write_u64(i as u64)
    }

    #[inline(always)]
    fn write_usize(&mut self, i: usize) {
        self.write_u64(i as u64)
    }

    #[inline(always)]
    fn write_i8(&mut self, i: i8) {
        self.write_u64(i as u64)
    }

    #[inline(always)]
    fn write_i16(&mut self, i: i16) {
        self.write_u64(i as u64)
    }

    #[inline(always)]
    fn write_i32(&mut self, i: i32) {
        self.write_u64(i as u64)
    }

    #[inline(always)]
    fn write_i64(&mut self, i: i64) {
        self.write_u64(i as u64)
    }

    #[inline(always)]
    fn write_i128(&mut self, i: i128) {
        self.write_u64(i as u64)
    }

    #[inline(always)]
    fn write_isize(&mut self, i: isize) {
        self.write_u64(i as u64)
    }
}

mod sealed {
    pub trait Sealed {}

    impl<T> Sealed for Option<T> {}
    impl<T, E> Sealed for Result<T, E> {}
}
