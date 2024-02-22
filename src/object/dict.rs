use std::sync::Arc;
use crate::value::Value;
use hashbrown::HashMap;

pub struct Dictionary(HashMap<Arc<str>, Value>);
