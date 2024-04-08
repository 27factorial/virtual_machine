use std::sync::Arc;

use crate::{value::Value, vm::ops::OpCode};

pub struct Closure { 
    locals: Vec<Value>,
    code: Arc<[OpCode]>,
}