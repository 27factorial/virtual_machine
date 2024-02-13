use rustc_hash::FxHasher;
use std::hash::BuildHasherDefault;

pub type HashMap<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<FxHasher>>;
