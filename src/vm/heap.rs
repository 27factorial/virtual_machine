use crate::gc_box;
use crate::object::VmObject;
use crate::value::Value;
use crate::vm::gc::GcBox;
use serde::{Deserialize, Serialize};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::VecDeque;
use std::marker::PhantomData;
use std::mem;
use std::ptr::NonNull;

#[derive(Debug)]
pub struct Heap {
    memory: Vec<Option<RefCell<GcBox<dyn VmObject>>>>,
    free_indices: VecDeque<usize>,
    worklist: Vec<usize>,
    current_children: Vec<Reference>,
    size: usize,
    capacity: usize,
}

impl Heap {
    pub fn new(capacity: usize) -> Self {
        Self {
            memory: Vec::new(),
            free_indices: VecDeque::new(),
            worklist: Vec::new(),
            current_children: Vec::new(),
            size: 0,
            capacity,
        }
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn alloc<T: VmObject>(&mut self, value: T) -> Result<Reference, T> {
        match self.free_indices.front().copied() {
            Some(idx) => {
                // A previous Option was in the slot at self.memory[idx], so the size of the
                // Option<GcBox<..>> itself is already accounted for. The only other data being
                // added is the heap data of size size_of::<T>().
                let required_space = self.size + mem::size_of::<T>();

                if required_space <= self.capacity {
                    self.size = required_space;

                    // Remove the index we used from free_indices.
                    self.free_indices.pop_front();

                    self.memory[idx] = Some(RefCell::new(gc_box!(value)));

                    Ok(Reference(idx))
                } else {
                    Err(value)
                }
            }
            None => {
                let required_space =
                    self.size + mem::size_of::<Option<GcBox<dyn VmObject>>>() + mem::size_of::<T>();

                if required_space <= self.capacity {
                    let idx = self.memory.len();
                    self.memory.push(Some(RefCell::new(gc_box!(value))));
                    self.size = required_space;
                    Ok(Reference(idx))
                } else {
                    Err(value)
                }
            }
        }
    }

    pub(crate) fn mark_and_sweep(&mut self, roots: impl IntoIterator<Item = Reference>) {
        self.mark_from_roots(roots);
        self.sweep();
    }

    pub(crate) fn mark_from_roots(&mut self, roots: impl IntoIterator<Item = Reference>) {
        for root in roots {
            let Some(cell) = self.memory.get_mut(root.0).and_then(|opt| opt.as_mut()) else {
                continue;
            };

            let object = cell.get_mut();

            if !object.is_marked() {
                object.mark();
                self.worklist.push(root.0);
                self.mark_children();
            }
        }

        todo!()
    }

    pub(crate) fn sweep(&mut self) {
        let enumerated = self.memory.iter_mut().enumerate();

        for (idx, slot) in enumerated {
            match slot {
                Some(cell) => {
                    let object = cell.get_mut();

                    if object.is_marked() {
                        object.unmark();
                    }
                }
                opt => {
                    *opt = None;
                    self.free_indices.push_back(idx)
                }
            }
        }
    }

    pub fn get(&self, object: Reference) -> Option<Ref<'_, dyn VmObject>> {
        self.memory.get(object.0).and_then(|opt| {
            opt.as_ref()
                .and_then(|cell| cell.try_borrow().ok().map(|gc| Ref::map(gc, |gc| &**gc)))
        })
    }

    pub fn get_mut(&self, object: Reference) -> Option<RefMut<'_, dyn VmObject>> {
        self.memory.get(object.0).and_then(|opt| {
            opt.as_ref().and_then(|cell| {
                cell.try_borrow_mut()
                    .ok()
                    .map(|gc| RefMut::map(gc, |gc| &mut **gc))
            })
        })
    }

    fn mark_children(&mut self) {
        while let Some(idx) = self.worklist.pop() {
            let object = self
                .memory
                .get_mut(idx)
                .and_then(|opt| opt.as_mut())
                .expect("object should exist");

            let object = &*object.get_mut();

            object.gc(Collector(&mut self.current_children));

            for Reference(child_idx) in self.current_children.drain(..) {
                let cell_opt = self.memory.get_mut(child_idx).and_then(|opt| opt.as_mut());

                if let Some(child_cell) = cell_opt {
                    let child_object = child_cell.get_mut();

                    if !child_object.is_marked() {
                        child_object.mark();
                        self.worklist.push(child_idx);
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Collector<'a>(&'a mut Vec<Reference>);

impl Collector<'_> {
    pub fn collect_from(&mut self, iter: impl IntoIterator<Item = Value>) {
        self.0.extend(iter.into_iter().filter_map(Value::reference));
    }
}

#[derive(
    Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default, Serialize, Deserialize,
)]
#[repr(transparent)]
pub struct Reference(pub(crate) usize);
