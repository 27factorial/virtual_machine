use super::CallFrame;
use crate::value::Value;
use std::mem;
use std::ops::{Index, IndexMut};
use std::slice::SliceIndex;

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
        if self.data.len() != self.capacity {
            self.data.push(value);
            Ok(())
        } else {
            Err(value)
        }
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.data.pop()
    }

    pub fn truncate(&mut self, new_len: usize) {
        // Since Value doesn't have an explicit Drop impl, this should just be equivalent to 
        // Vec::set_len when new_len < self.len()
        self.data.truncate(new_len)
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

impl IntoIterator for DataStack {
    type Item = <Vec<Value> as IntoIterator>::Item;

    type IntoIter = <Vec<Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

#[derive(Clone, PartialEq, Debug, Default)]
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
}

impl IntoIterator for CallStack {
    type Item = <Vec<CallFrame> as IntoIterator>::Item;

    type IntoIter = <Vec<CallFrame> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}
