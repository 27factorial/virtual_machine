use std::{collections::VecDeque, mem};

use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;

use crate::{
    gc::{GcBox, GcObject},
    gc_box,
    object::Object,
    value::Value,
};

use super::{memory::ValueMemory, Register, RegisterIter, Registers};

pub struct Heap {
    memory: Vec<Option<GcObject>>,
    free_indices: VecDeque<usize>,
    worklist: Vec<usize>,
    current_children: Vec<ObjectRef>,
    byte_len: usize,
    byte_capacity: usize,
}

impl Heap {
    pub fn new(byte_capacity: usize) -> Self {
        Self {
            memory: Vec::new(),
            free_indices: VecDeque::new(),
            worklist: Vec::new(),
            current_children: Vec::new(),
            byte_len: 0,
            byte_capacity,
        }
    }

    pub fn alloc<T: Object>(&mut self, value: T) -> Result<ObjectRef, T> {
        match self.free_indices.front().copied() {
            Some(idx) => {
                // A previous Option was in the slot at self.memory[idx], so the size of the
                // Option<GcBox<..>> itself is already accounted for. The only other data being
                // added is the heap data of size size_of::<T>().
                let required_space = self.byte_len + mem::size_of::<T>();

                if required_space <= self.byte_capacity {
                    self.byte_len = required_space;

                    // Remove the index we used from free_indices.
                    self.free_indices.pop_front();

                    self.memory[idx] = Some(GcObject::new(value));

                    Ok(ObjectRef(idx))
                } else {
                    Err(value)
                }
            }
            None => {
                let required_space =
                    self.byte_len + mem::size_of::<Option<GcObject>>() + mem::size_of::<T>();

                if required_space <= self.byte_capacity {
                    let idx = self.memory.len();
                    self.memory.push(Some(GcObject::new(value)));
                    self.byte_len = required_space;
                    Ok(ObjectRef(idx))
                } else {
                    Err(value)
                }
            }
        }
    }

    pub fn collect(&mut self, roots: RootsIter<'_>) {
        self.mark_from_roots(roots);
        self.sweep();
    }

    pub fn mark_from_roots(&mut self, roots: RootsIter<'_>) {
        for root in roots {
            let Some(object) = self.memory.get_mut(root.0).and_then(|opt| opt.as_mut()) else {
                continue;
            };

            if !object.is_marked() {
                object.mark();
                self.worklist.push(root.0);
                self.mark_children();
            }
        }

        todo!()
    }

    fn mark_children(&mut self) {
        while let Some(idx) = self.worklist.pop() {
            let object = self
                .memory
                .get(idx)
                .and_then(|opt| opt.as_ref())
                .expect("object should exist");

            let object_children = object.fields().iter().filter_map(|&field| match field {
                Value::Object(idx) => Some(idx),
                _ => None,
            });

            self.current_children.extend(object_children);

            for ObjectRef(child_idx) in self.current_children.drain(..) {
                let object_opt = self.memory.get_mut(child_idx).and_then(|opt| opt.as_mut());

                if let Some(child_object) = object_opt {
                    if !child_object.is_marked() {
                        child_object.mark();
                        self.worklist.push(child_idx);
                    }
                }
            }
        }
    }

    pub fn sweep(&mut self) {
        let enumerated = self.memory.iter_mut().enumerate();

        for (idx, slot) in enumerated {
            match slot {
                Some(object) if object.is_marked() => object.unmark(),
                opt => {
                    *opt = None;
                    self.free_indices.push_back(idx)
                }
            }
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default, Serialize, Deserialize)]
pub struct ObjectRef(usize);

pub struct RootsIter<'a> {
    registers: &'a Registers,
    memory: &'a ValueMemory,
    registers_iter: RegisterIter,
    idx: usize,
}

impl<'a> RootsIter<'a> {
    pub fn new(registers: &'a Registers, memory: &'a ValueMemory) -> Self {
        Self {
            registers,
            memory,
            registers_iter: Register::iter(),
            idx: 0,
        }
    }
}

impl Iterator for RootsIter<'_> {
    type Item = ObjectRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx == self.memory.len() {
            return None;
        }

        let value = match self.registers_iter.next() {
            Some(register) => self.registers[register],
            None => {
                let v = self.memory[self.idx];
                self.idx += 1;
                v
            }
        };

        let Value::Object(idx) = value else {
            return None;
        };

        Some(idx)
    }
}
