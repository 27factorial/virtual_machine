use crate::{utils::FxHashMap, value::Value};

pub struct Dictionary(FxHashMap<Value, Value>);
