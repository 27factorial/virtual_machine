use std::alloc::{self, Layout};
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::iter::FusedIterator;
use std::mem::MaybeUninit;
use std::ops::{Index, IndexMut};
use std::ptr::{self, NonNull};
use std::slice;
use std::{cmp, mem};

#[cold]
#[inline(never)]
#[track_caller]
fn bounds_check_failed(len: usize, index: usize) -> ! {
    panic!("index out of bounds: the len is {len} but the index is {index}")
}

#[cold]
#[inline(never)]
#[track_caller]
fn capacity_overflow() -> ! {
    panic!("capacity overflow");
}

// Converts an index starting at the end of a slice to the index starting at the beginning of a
// slice. This means that the first item in a stack will always be the item at the end of the
// internal slice. Use this to be less error prone when calculating indices from the end of a slice.
#[inline(always)]
fn stack_index(len: usize, index: usize) -> usize {
    len - index - 1
}

#[inline(always)]
unsafe fn copy_elem<T: Copy>(ptr: *mut [MaybeUninit<T>], src_index: usize, dest_index: usize) {
    let ptr = ptr as *mut MaybeUninit<T>;

    unsafe {
        let src = ptr.add(src_index);
        let dest = ptr.add(dest_index);

        ptr::copy_nonoverlapping(src, dest, 1);
    }
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
            let ptr = unsafe { alloc::alloc(layout).cast::<MaybeUninit<T>>() };

            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }

            ptr
        } else {
            NonNull::dangling().as_ptr()
        };

        // SAFETY: The pointer supplied to slice_from_raw_parts_mut comes from a pointer that was
        // allocated for `capacity` elements, and was also created from GlobalAlloc, meaning a Box
        // can be constructed from the slice pointer. The pointer is also checked above to make sure
        // it's not null.
        let data = unsafe { Box::from_raw(ptr::slice_from_raw_parts_mut(ptr, capacity)) };

        Self { data, len: 0 }
    }

    pub fn with_byte_capacity(capacity: usize) -> Self {
        let capacity = capacity / mem::size_of::<T>();

        Self::new(capacity)
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
        if self.len() == self.capacity() {
            return Err(value);
        }

        unsafe {
            self.push_unchecked(value);
        }
        Ok(())
    }

    pub fn pop(&mut self) -> Option<T> {
        if !self.is_empty() {
            unsafe { Some(self.pop_unchecked()) }
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
            let item = unsafe { self.get_unchecked(stack_index(self.len(), 0)) };
            Some(item)
        } else {
            None
        }
    }

    pub fn top_mut(&mut self) -> Option<&mut T> {
        if !self.is_empty() {
            let item = unsafe { self.get_unchecked_mut(stack_index(self.len(), 0)) };
            Some(item)
        } else {
            None
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            let item = unsafe { self.get_unchecked(stack_index(self.len(), index)) };
            Some(item)
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len() {
            let item = unsafe { self.get_unchecked_mut(stack_index(self.len(), index)) };
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

    #[inline(always)]
    pub unsafe fn get_uninit_unchecked(&self, index: usize) -> &MaybeUninit<T> {
        unsafe {
            let ptr = self.data.as_ptr().add(index);
            &*ptr
        }
    }

    #[inline(always)]
    pub unsafe fn get_uninit_unchecked_mut(&mut self, index: usize) -> &mut MaybeUninit<T> {
        unsafe {
            let ptr = self.data.as_mut_ptr().add(index);
            &mut *ptr
        }
    }

    #[inline(always)]
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        unsafe { self.get_uninit_unchecked(index).assume_init_ref() }
    }

    #[inline(always)]
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        unsafe { self.get_uninit_unchecked_mut(index).assume_init_mut() }
    }

    #[inline(always)]
    pub unsafe fn push_unchecked(&mut self, value: T) {
        unsafe {
            self.data
                .as_mut_ptr()
                .add(self.len)
                .write(MaybeUninit::new(value));
            self.len += 1;
        }
    }

    #[inline(always)]
    pub unsafe fn pop_unchecked(&mut self) -> T {
        unsafe {
            self.len -= 1;
            ptr::read(self.get_unchecked(self.len))
        }
    }

    fn from_boxed_slice(slice: Box<[T]>) -> Self {
        let len = slice.len();

        let data = unsafe { Box::from_raw(Box::into_raw(slice) as *mut _) };

        Self { data, len }
    }
}

impl<T: Copy> VmStack<T> {
    pub fn copy_to_top(&mut self, index: usize) {
        assert_ne!(self.len(), self.capacity(), "capacity check failed");
        assert!(index < self.len(), "bounds check failed");

        unsafe {
            copy_elem(ptr::addr_of_mut!(*self.data), index, self.len());
            self.len += 1;
        }
    }

    pub fn replace_from_top(&mut self, index: usize) {
        assert!(index < self.len() - 1, "bounds check failed");

        unsafe {
            copy_elem(ptr::addr_of_mut!(*self.data), self.len() - 1, index);
            self.len -= 1;
        }
    }

    pub(crate) fn push_from_ref(&mut self, elem: &T) -> Result<(), T> {
        if self.len() == self.capacity() {
            // Returning the value on the stack here is fine, because it's an error state and is,
            // presumably, somewhat rare if this function is being called.
            return Err(*elem);
        }

        // SAFETY:
        // 1. The if check above makes sure that the ptr::add below is safe to call (for the same
        // reason vec.as_ptr.add(vec.len()) is safe)
        // 2. The borrow checker ensures that the element can't overlap with self, since it's
        // mutably borrowed.
        // 3. self.len is always less than the capacity, so it can never overflow the capacity of
        // the stack.
        // TODO: Think about whether or not this can be implemented with safe code without losing
        // copying to a temporary. This function is intended to (essentially) memcpy from the element into
        // self, without first copying to the stack.
        unsafe {
            let top_ptr = (ptr::addr_of_mut!(*self.data) as *mut MaybeUninit<T>).add(self.len());
            ptr::copy_nonoverlapping(ptr::from_ref(elem).cast(), top_ptr, 1);
            self.len += 1;
        }

        Ok(())
    }
}

impl<T> Index<usize> for VmStack<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        if index < self.len() {
            // SAFETY: values at an index in the range 0..self.len() are always initialized.
            unsafe { self.get_unchecked(stack_index(self.len(), index)) }
        } else {
            bounds_check_failed(self.len(), index)
        }
    }
}

impl<T> IndexMut<usize> for VmStack<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index < self.len() {
            // SAFETY: values at an index in the range 0..self.len() are always initialized.
            unsafe { self.get_unchecked_mut(stack_index(self.len(), index)) }
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
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl<T: PartialOrd> PartialOrd for VmStack<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.as_slice().partial_cmp(other.as_slice())
    }
}

impl<T: Hash> Hash for VmStack<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
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

#[cfg(test)]
mod test {
    use std::rc::Rc;

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

        let popped = stack.pop().unwrap();
        stack.push(popped).unwrap();

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

        assert_eq!(stack.get(0), Some(&69));
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
        assert!(stack.push(1).is_err());
        assert!(stack.pop().is_none());
    }

    #[test]
    fn cloning() {
        let stack = VmStack::from([0u8, 1, 2, 3, 4]);
        let cloned = stack.clone();

        assert_eq!(stack, cloned)
    }

    #[test]
    fn iteration() {
        let array = [0u8, 1, 2, 3, 4, 5, 6, 7];
        let forty_two = [42; 8];
        let mut stack = VmStack::from(array);

        assert!(stack.iter().eq(array.iter().rev()));

        stack.iter_mut().for_each(|val| *val = 42);

        assert!(stack.into_iter().eq(forty_two.into_iter().rev()));
    }

    #[test]
    fn drops() {
        struct DropCheck(i32);

        impl Drop for DropCheck {
            fn drop(&mut self) {
                eprintln!("dropped {}", self.0)
            }
        }

        let stack = VmStack::from([DropCheck(0), DropCheck(1), DropCheck(2)]);
        drop(stack);

        let rc = Rc::new(1i32);
        let rc_clone = Rc::clone(&rc);

        drop(VmStack::from(vec![vec![rc; 16]; 16]));

        assert_eq!(Rc::strong_count(&rc_clone), 1)
    }

    #[test]
    fn refs() {
        let mut x = 42;
        let mut y = 69;
        let mut z = 420;
        let mut w = 42069;

        let mut stack = VmStack::from([&mut x, &mut y, &mut z]);

        {
            let borrow = &mut w;
            *stack.top_mut().unwrap() = borrow;
        }

        // This line should fail to compile:
        // z = 100;
        *stack.pop().unwrap() = 1;
        *stack.pop().unwrap() = 2;
        *stack.pop().unwrap() = 3;

        drop(stack);

        assert_eq!(w, 1);
        assert_eq!(y, 2);
        assert_eq!(x, 3);
    }

    #[cfg(miri)]
    #[test]
    fn copies() {
        let mut stack = VmStack::from([0, 1, 2, 3, 4, 5, 6, 7, 8]);

        stack.replace_from_top(6); // [0, 1, 2, 3, 4, 5, 8, 7]
        assert_eq!(stack[6], 8);

        dbg!(&stack);
        stack.copy_to_top(1); // [0, 1, 2, 3, 4, 5, 8, 7, 1]
        dbg!(&stack);

        assert_eq!(stack[8], 1);
    }

    // SAFETY: it's not lol.
    // #[test]
    // fn triggers_undefined_behavior_only_use_this_for_testing_miri_please_i_beg_you() {
    //     let mut stack = VmStack::new(0);
    //     unsafe { stack.push_unchecked(1); }
    // }
}
