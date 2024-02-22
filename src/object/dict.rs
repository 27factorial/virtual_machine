use crate::utils::FxHashMap;
use crate::value::Value;

pub struct Dictionary(FxHashMap<Value, Value>);
