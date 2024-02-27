use super::CallFrame;
use crate::value::Value;
use std::alloc::{self, Layout};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::iter::FusedIterator;
use std::mem::{self, MaybeUninit};
use std::ops::{Index, IndexMut};
use std::ptr::{self, NonNull};
use std::slice::{self, SliceIndex};

#[cold]
#[inline(never)]
fn bounds_check_failed(len: usize, index: usize) -> ! {
    panic!("index out of bounds: the len is {len} but the index is {index}")
}

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

    pub fn top(&self) -> Option<Value> {
        self.data.last().copied()
    }

    pub fn top_mut(&mut self) -> Option<&mut Value> {
        self.data.last_mut()
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

fn capacity_overflow() -> ! {
    panic!("capacity overflow");
}

pub struct VmStack<T> {
    data: Box<[MaybeUninit<T>]>,
    len: usize,
}

impl<T> VmStack<T> {
    pub fn new(capacity: usize) -> Self {
        let ptr = if capacity > 0 {
            let layout = match Layout::array::<MaybeUninit<T>>(capacity) {
                Ok(layout) => layout,
                Err(_) => capacity_overflow(),
            };

            // SAFETY: the condition above checks that more than 0 bytes are being allocated.
            unsafe { alloc::alloc(layout).cast::<MaybeUninit<T>>() }
        } else {
            NonNull::dangling().as_ptr()
        };

        // SAFETY: The pointer supplied to slice_from_raw_parts_mut comes from a pointer that was
        // allocated for `capacity` elements, and was also created from GlobalAlloc, meaning a Box
        // can be constructed from the slice pointer.
        let data = unsafe { Box::from_raw(ptr::slice_from_raw_parts_mut(ptr, capacity)) };

        Self { data, len: 0 }
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline(always)]
    pub fn capacity(&self) -> usize {
        self.data.len()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn push(&mut self, value: T) -> Result<(), T> {
        if self.len() < self.capacity() {
            unsafe {
                self.push_unchecked(value);
            }
            Ok(())
        } else {
            Err(value)
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if !self.is_empty() {
            unsafe {
                self.len -= 1;
                Some(ptr::read(self.get_unchecked(self.len)))
            }
        } else {
            None
        }
    }

    pub fn truncate(&mut self, new_len: usize) {
        if new_len < self.len() {
            // TODO: Safety docs, this is essentially the same as the docs in
            // https://doc.rust-lang.org/std/vec/struct.Vec.html#method.truncate
            unsafe {
                let remaining_len = self.len - new_len;
                let s = ptr::slice_from_raw_parts_mut(
                    self.data.as_mut_ptr().cast::<T>().add(new_len),
                    remaining_len,
                );

                self.len = new_len;
                ptr::drop_in_place(s);
            }
        }
    }

    pub fn clear(&mut self) {
        let elems = ptr::from_mut(self.as_mut_slice());

        unsafe {
            self.len = 0;
            ptr::drop_in_place(elems)
        }
    }

    pub fn top(&self) -> Option<&T> {
        if !self.is_empty() {
            let item = unsafe { self.get_unchecked(self.len() - 1) };
            Some(item)
        } else {
            None
        }
    }

    pub fn top_mut(&mut self) -> Option<&mut T> {
        if !self.is_empty() {
            let item = unsafe { self.get_unchecked_mut(self.len() - 1) };
            Some(item)
        } else {
            None
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            let item = unsafe { self.get_unchecked(index) };
            Some(item)
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len() {
            let item = unsafe { self.get_unchecked_mut(index) };
            Some(item)
        } else {
            None
        }
    }

    pub fn as_slice(&self) -> &[T] {
        let start = self.data.as_ptr().cast::<T>();

        // SAFETY: All elements in the range 0..self.len are guaranteed to be valid and initialized.
        unsafe { slice::from_raw_parts(start, self.len) }
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        let start = self.data.as_mut_ptr().cast::<T>();

        // SAFETY: All elements in the range 0..self.len are guaranteed to be valid and initialized.
        unsafe { slice::from_raw_parts_mut(start, self.len) }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.data
            .iter()
            .take(self.len)
            .map(|item| unsafe {
                // SAFETY: Only the items from 0..self.len() are taken, and all items in that range
                // are always initialized.
                item.assume_init_ref()
            })
            .rev()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.data
            .iter_mut()
            .take(self.len)
            .map(|item| unsafe {
                // SAFETY: Only the items from 0..self.len() are taken, and all items in that range
                // are always initialized.
                item.assume_init_mut()
            })
            .rev()
    }

    fn from_boxed_slice(slice: Box<[T]>) -> Self {
        let len = slice.len();

        let data = unsafe { Box::from_raw(Box::into_raw(slice) as *mut _) };

        Self { data, len }
    }

    unsafe fn get_uninit_unchecked(&self, index: usize) -> &MaybeUninit<T> {
        unsafe {
            let ptr = self.data.as_ptr().add(index);
            &*ptr
        }
    }

    unsafe fn get_uninit_unchecked_mut(&mut self, index: usize) -> &mut MaybeUninit<T> {
        unsafe {
            let ptr = self.data.as_mut_ptr().add(index);
            &mut *ptr
        }
    }

    unsafe fn get_unchecked(&self, index: usize) -> &T {
        unsafe { self.get_uninit_unchecked(index).assume_init_ref() }
    }

    unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        unsafe { self.get_uninit_unchecked_mut(index).assume_init_mut() }
    }

    unsafe fn push_unchecked(&mut self, value: T) {
        unsafe {
            self.data.get_unchecked_mut(self.len).write(value);
        }
        self.len += 1;
    }
}

impl<T> Index<usize> for VmStack<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        if index < self.len() {
            // SAFETY: values at an index in the range 0..self.len() are always initialized.
            unsafe { self.get_unchecked(index) }
        } else {
            bounds_check_failed(self.len(), index)
        }
    }
}

impl<T> IndexMut<usize> for VmStack<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index < self.len() {
            // SAFETY: values at an index in the range 0..self.len() are always initialized.
            unsafe { self.get_unchecked_mut(index) }
        } else {
            bounds_check_failed(self.len(), index)
        }
    }
}

impl<T: Clone> Clone for VmStack<T> {
    fn clone(&self) -> Self {
        let mut cloned = Self::new(self.capacity());

        self.as_slice().iter().for_each(|value| unsafe {
            // SAFETY: `cloned` has the same capacity as `self`, so there's no risk of pushing
            // past the capacity of the stack.
            cloned.push_unchecked(value.clone())
        });

        cloned
    }
}

impl<T: Eq> Eq for VmStack<T> {}

impl<T, U> PartialEq<VmStack<U>> for VmStack<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &VmStack<U>) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl<T: Ord> Ord for VmStack<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl<T: PartialOrd> PartialOrd for VmStack<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_slice().partial_cmp(other.as_slice())
    }
}

impl<T: Hash> Hash for VmStack<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(self.as_slice(), state);
    }
}

impl<T: Debug> Debug for VmStack<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.as_slice().iter()).finish()
    }
}

impl<T> Drop for VmStack<T> {
    fn drop(&mut self) {
        self.clear();
    }
}

impl<T, const N: usize> From<[T; N]> for VmStack<T> {
    fn from(value: [T; N]) -> Self {
        Self::from_boxed_slice(Box::new(value))
    }
}

impl<T: Clone, const N: usize> From<&[T; N]> for VmStack<T> {
    fn from(value: &[T; N]) -> Self {
        Self::from(value.as_slice())
    }
}

impl<T: Clone> From<&[T]> for VmStack<T> {
    fn from(value: &[T]) -> Self {
        Self::from_boxed_slice(Box::from(value))
    }
}

impl<T> From<Vec<T>> for VmStack<T> {
    fn from(value: Vec<T>) -> Self {
        Self::from_boxed_slice(value.into_boxed_slice())
    }
}

impl<T> IntoIterator for VmStack<T> {
    type Item = T;

    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self)
    }
}

pub struct IntoIter<T>(VmStack<T>);

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.pop()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.0.len();
        (len, Some(len))
    }
}

impl<T> ExactSizeIterator for IntoIter<T> {}

impl<T> FusedIterator for IntoIter<T> {}

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

#[cfg(test)]
mod test {
    use super::VmStack;

    #[test]
    fn push_and_pop() {
        let mut stack = VmStack::new(8);

        loop {
            if stack.push(0u8).is_err() {
                break;
            }
        }

        assert!(stack.push(0).is_err());

        while let Some(v) = stack.pop() {
            assert_eq!(v, 0);
        }
    }

    #[test]
    fn truncate_and_clear() {
        let array = [0u8; 8];
        let mut stack = VmStack::from(array);

        stack.truncate(69);
        assert_eq!(stack.len(), 8);

        stack.truncate(4);

        assert_eq!(stack.len(), 4);
        assert_eq!(stack.capacity(), 8);

        stack.clear();

        assert_eq!(stack.len(), 0);
    }

    #[test]
    fn as_slice() {
        let mut array = [0u8, 1, 2, 3, 4, 5, 6, 7];
        let mut stack = VmStack::from(array);

        assert_eq!(array.as_slice(), stack.as_slice());

        array[0] = 42;
        stack.as_mut_slice()[0] = 42;

        assert_eq!(array.as_slice(), stack.as_slice());

        stack.truncate(1);

        assert_ne!(array.as_slice(), stack.as_slice());
        assert_eq!(&array[..1], stack.as_slice())
    }

    #[test]
    fn get_and_get_mut() {
        let mut stack = VmStack::new(64);

        assert!(stack.get(128).is_none());

        stack.push(420usize).unwrap();
        stack.push(69).unwrap();

        assert_eq!(stack.get(0), Some(&420));
        assert!(stack.get(2).is_none());

        *stack.get_mut(1).unwrap() = 69420;

        assert_eq!(stack.get(1), Some(&69420));

        stack.pop();

        assert!(stack.get(1).is_none())
    }

    #[test]
    fn zero_capacity() {
        let mut stack: VmStack<u8> = VmStack::new(0);

        assert_eq!(stack.capacity(), 0);
        assert!(stack.push(1).is_err())
    }
}
