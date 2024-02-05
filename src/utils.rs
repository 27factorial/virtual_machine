use std::hash::BuildHasherDefault;
use rustc_hash::FxHasher;

pub type HashMap<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<FxHasher>>;