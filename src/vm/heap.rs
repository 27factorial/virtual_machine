use crate::gc_box;
use crate::object::VmObject;
use crate::value::Value;
use crate::vm::gc::GcBox;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::mem;

#[derive(Debug)]
pub struct Heap {
    memory: Vec<Option<GcBox<dyn VmObject>>>,
    free_indices: VecDeque<usize>,
    worklist: Vec<usize>,
    current_children: Vec<Reference>,
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

    pub fn alloc<T: VmObject>(&mut self, value: T) -> Result<Reference, T> {
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

                    self.memory[idx] = Some(gc_box!(value));

                    Ok(Reference(idx))
                } else {
                    Err(value)
                }
            }
            None => {
                let required_space = self.byte_len
                    + mem::size_of::<Option<GcBox<dyn VmObject>>>()
                    + mem::size_of::<T>();

                if required_space <= self.byte_capacity {
                    let idx = self.memory.len();
                    self.memory.push(Some(gc_box!(value)));
                    self.byte_len = required_space;
                    Ok(Reference(idx))
                } else {
                    Err(value)
                }
            }
        }
    }

    pub fn mark_and_sweep(&mut self, roots: impl IntoIterator<Item = Reference>) {
        self.mark_from_roots(roots);
        self.sweep();
    }

    pub fn mark_from_roots(&mut self, roots: impl IntoIterator<Item = Reference>) {
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

    pub fn get(&self, object: Reference) -> Option<&dyn VmObject> {
        self.memory.get(object.0).and_then(|opt| opt.as_deref())
    }

    pub fn get_mut(&mut self, object: Reference) -> Option<&mut dyn VmObject> {
        self.memory
            .get_mut(object.0)
            .and_then(|opt| opt.as_deref_mut())
    }

    fn mark_children(&mut self) {
        while let Some(idx) = self.worklist.pop() {
            let object = self
                .memory
                .get(idx)
                .and_then(|opt| opt.as_ref())
                .expect("object should exist");

            let object_children = object.data().iter().copied().filter_map(Value::reference);

            self.current_children.extend(object_children);

            for Reference(child_idx) in self.current_children.drain(..) {
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
}

#[derive(
    Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default, Serialize, Deserialize,
)]
#[repr(transparent)]
pub struct Reference(pub(crate) usize);
