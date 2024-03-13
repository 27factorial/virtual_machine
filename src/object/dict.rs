use crate::value::Value;
use hashbrown::HashMap;
use std::sync::Arc;

pub struct Dictionary {
    indices: HashMap<Arc<str>, usize>,
    values: Vec<Value>,
}
