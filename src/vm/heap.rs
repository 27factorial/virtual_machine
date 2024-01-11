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
    free_indices: Vec<usize>,
    worklist: Vec<usize>,
    current_children: Vec<HeapIndex>,
    byte_len: usize,
    byte_capacity: usize,
}

impl Heap {
    pub fn new(byte_capacity: usize) -> Self {
        Self {
            memory: Vec::new(),
            free_indices: Vec::new(),
            worklist: Vec::new(),
            current_children: Vec::new(),
            byte_len: 0,
            byte_capacity,
        }
    }

    pub fn alloc<T: Object + 'static>(&mut self, value: T) -> Result<HeapIndex, T> {
        let value_heap_size = std::mem::size_of::<T>() + std::mem::size_of::<GcObject>();

        if self.byte_len + value_heap_size > self.byte_capacity {
            // The first time an error is returned, the VM should attempt to collect garbage.
            Err(value)
        } else {
            let heap_index = self.memory.len();
            self.memory.push(Some(GcObject::new(value)));
            self.byte_len += value_heap_size;
            Ok(HeapIndex(heap_index))
        }
    }

    pub fn mark_from_roots(&mut self, roots: RootsIter<'_>) {
        for root in roots {
            let Some(object) = self
                .memory
                .get_mut(root.0)
                .map(|opt| opt.as_mut())
                .flatten()
            else {
                continue;
            };

            if !object.is_marked() {
                object.mark();
                self.worklist.push(root.0);
                self.mark();
            }
        }

        todo!()
    }

    pub fn mark(&mut self) {
        while let Some(idx) = self.worklist.pop() {
            let object = self
                .memory
                .get(idx)
                .map(|opt| opt.as_ref())
                .flatten()
                .expect("object should exist");

            let object_children = object.obj.fields().iter().filter_map(|&field| match field {
                Value::Object(idx) => Some(HeapIndex(idx)),
                _ => None,
            });

            self.current_children.extend(object_children);

            for &HeapIndex(child_idx) in &self.current_children {
                if let Some(child_object) = self.memory.get_mut(child_idx) {
                    if !child_object.is_marked() {
                        child_object.mark();
                        self.worklist.push(child_idx);
                    }
                }
            }
        }
    }

    pub fn sweep(&mut self) {
        for object in &mut self.memory {
            if object.is_marked() {
                object.obj.unmark();
            } else {
                todo!("delete and deallocate the object")
            }
        }
    }
}

pub struct HeapIndex(usize);

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
    type Item = HeapIndex;

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

        // TODO(value_object): Change Value::Object's contained type from usize to HeapIndex
        let Value::Object(idx) = value else {
            return None;
        };

        Some(HeapIndex(idx))
    }
}
