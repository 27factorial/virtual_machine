use rustc_hash::FxHasher;
use std::hash::BuildHasherDefault;

use crate::vm::{
    ops::{VmError, VmErrorKind},
    CallFrame, Vm,
};

pub type HashMap<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<FxHasher>>;

pub trait VmResult: sealed::Sealed {
    type Ok;

    fn vm_err(self, kind: VmErrorKind, vm: &Vm) -> Result<Self::Ok, VmError>;
}

impl<T> VmResult for Option<T> {
    type Ok = T;

    fn vm_err(self, kind: VmErrorKind, vm: &Vm) -> Result<T, VmError> {
        match self {
            Some(t) => Ok(t),
            None => Err(vm.error(kind)),
        }
    }
}

impl<T, E> VmResult for Result<T, E> {
    type Ok = T;

    fn vm_err(self, kind: VmErrorKind, vm: &Vm) -> Result<T, VmError> {
        match self {
            Ok(t) => Ok(t),
            Err(_) => Err(vm.error(kind)),
        }
    }
}

mod sealed {
    pub trait Sealed {}

    impl<T> Sealed for Option<T> {}
    impl<T, E> Sealed for Result<T, E> {}
}
