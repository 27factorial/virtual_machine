use std::{
    mem,
    ops::{Index, IndexMut},
    slice::SliceIndex,
};



use crate::value::Value;

use super::{CallFrame};

#[derive(Clone, PartialEq, PartialOrd, Debug, Default)]
pub struct DataStack {
    data: Vec<Value>,
    capacity: usize,
}

impl DataStack {
    pub fn new(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            capacity,
        }
    }

    pub fn with_byte_capacity(capacity: usize) -> Self {
        let value_capacity = capacity / mem::size_of::<Value>();
        Self::new(value_capacity)
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn push(&mut self, value: Value) -> Result<(), Value> {
        if self.data.len() == self.capacity {
            Err(value)
        } else {
            self.data.push(value);
            Ok(())
        }
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.data.pop()
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn get<I: SliceIndex<[Value]>>(&self, index: I) -> Option<&I::Output> {
        self.data.get(index)
    }

    pub fn get_mut<I: SliceIndex<[Value]>>(&mut self, index: I) -> Option<&mut I::Output> {
        self.data.get_mut(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.data.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        self.data.iter_mut()
    }

    pub fn into_iter(self) -> impl Iterator<Item = Value> {
        self.data.into_iter()
    }
}

impl<I: SliceIndex<[Value]>> Index<I> for DataStack {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        self.data.index(index)
    }
}

impl<I: SliceIndex<[Value]>> IndexMut<I> for DataStack {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.data.index_mut(index)
    }
}

pub struct CallStack {
    data: Vec<CallFrame>,
    capacity: usize,
}

impl CallStack {
    pub fn new(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            capacity,
        }
    }

    pub fn with_byte_capacity(capacity: usize) -> Self {
        let value_capacity = capacity / mem::size_of::<CallFrame>();
        Self::new(value_capacity)
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn push(&mut self, frame: CallFrame) -> Result<(), CallFrame> {
        if self.data.len() == self.capacity {
            Err(frame)
        } else {
            self.data.push(frame);
            Ok(())
        }
    }

    pub fn pop(&mut self) -> Option<CallFrame> {
        self.data.pop()
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn iter(&self) -> impl Iterator<Item = &CallFrame> {
        self.data.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut CallFrame> {
        self.data.iter_mut()
    }

    pub fn into_iter(self) -> impl Iterator<Item = CallFrame> {
        self.data.into_iter()
    }
}
