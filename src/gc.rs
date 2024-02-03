use std::{
    alloc::{alloc, dealloc, handle_alloc_error, Layout},
    fmt::{self, Debug, Display, Pointer},
    hash::{Hash, Hasher},
    io::{self, Read, Seek, Write},
    iter::FusedIterator,
    marker::PhantomData,
    mem,
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
};

use crate::{
    object::{Object, TypeMeta},
    value::Value,
};

/// Constructs a new `GcBox<T>`, while also performing unsized coercions as necessary.
///
/// Types like `Box<T>` can be converted to a type `Box<U>` under certain conditons. The main
/// scenario this is seen in is with so-called "unsized coercions", in which a `Box<T>` is converted
/// to a `Box<dyn Trait>` if `T: Trait` and `Trait` is object safe. For user-defined types, this
/// conversion can happen if `T` is the the user type is a struct and `T` is the last field in that
/// struct. In the case of `GcBox<T>`, that `T` is behind a pointer, and thus this conversion cannot
/// happen automatically in stable Rust.
///
/// However, the conversion can still be done manually by first converting the `GcBox<T>` into
/// `*mut T`, performing the unsized cast on the raw pointer, and then using `GcBox::from_raw` to
/// create a `GcBox<dyn Trait>`, assuming the conditions stated before are upheld. Of course, doing
/// this is prone to errors if done incorrectly, and requires writing unsafe code, so this macro
/// offers a safe way to do it. This macro will cause a compile error if the coercion is invalid,
/// thus avoiding any potential errors that can happen when casting raw pointers.
#[macro_export]
macro_rules! gc_box {
    ($e:expr) => {{
        let value = $crate::gc::GcBox::new($e);

        // SAFETY: A newly constructed GcBox<T> does not have the "marked" bit set, and
        // it's valid to turn the raw pointer from into_raw into a reference. The pointer
        // supplied to from_raw is the mutable reference that was just cast to the correct type,
        // and came from the GcBox<T> created above.
        #[allow(unsafe_code)]
        unsafe {
            // Since unsizing coercions on user-defined "box-like" types are unstable, the GcBox
            // first needs to be converted into a raw pointer and then reassembled from that raw
            // pointer. A mutable reference is used here to restrict the kinds of casts that can be
            // done, as raw pointers can be cast to any (sized) type, which could cause unexpected
            // UB in user code.
            let cast = &mut *$crate::gc::GcBox::into_raw(value);
            $crate::gc::GcBox::from_raw(cast)
        }
    }};
}

const TAG: usize = 1;
const TAG_MASK: usize = !TAG;

/// Masks out the last bit of a NonNull pointer, leaving the (fat) pointer metadata intact.
///
/// # Safety
///
/// The given pointer must have a data address such that `(address & TAG_MASK) as *mut _` is not
/// null.
#[inline]
unsafe fn mask_ptr<T: ?Sized>(ptr: NonNull<T>) -> NonNull<T> {
    let (data, meta) = ptr.as_ptr().to_raw_parts();

    let data = data.map_addr(|addr| addr & TAG_MASK);

    // SAFETY: Callers must ensure that masking out the pointer does not result in a null pointer
    // being supplied here.
    unsafe { NonNull::new_unchecked(ptr::from_raw_parts_mut(data, meta)) }
}

/// Creates a Layout for a given type, with the alignment set to the maximum of `align_of::<T>` and
/// `2`, to ensure the last bit of a pointer to an allocation with the returned layout is always
/// 0.
fn gc_layout<T: Sized>() -> Layout {
    // SAFETY: The size parameter comes directly from size_of::<T>() and the align either comes from
    // align_of::<T> or is 2 (i.e. always a power of 2).
    unsafe {
        Layout::from_size_align_unchecked(
            mem::size_of::<T>(),
            std::cmp::max(mem::align_of::<T>(), 2),
        )
    }
}

/// Creates a Layout for a given type, with the alignment set to the maximum of
/// `align_of_val(value)` and `2`, to ensure the last bit of a pointer to an allocation with the
///  returned layout is always 0.
fn gc_layout_of_val<T: ?Sized>(value: &T) -> Layout {
    unsafe {
        Layout::from_size_align_unchecked(
            mem::size_of_val(value),
            std::cmp::max(mem::align_of_val(value), 2),
        )
    }
}

/// Allocates a `T` on the heap using the default allocator. The pointer returned by this function
/// will have an alignment which is valid for use in `GcBox`.
///
/// If the size of `T` is zero, this function does not actually allocate.
fn alloc_aligned<T>(value: T) -> NonNull<T> {
    let layout = gc_layout::<T>();

    if mem::size_of::<T>() == 0 {
        // SAFETY: A pointer created from the layout's alignment will always be well-aligned for `T`
        // and non-null, as alignment cannot be 0. This is essentially equivalent to
        // `NonNull::dangling`, but also ensures that the pointer is valid for use in GcBox<T>.
        unsafe { NonNull::new_unchecked(ptr::invalid_mut(layout.align())) }
    } else {
        // SAFETY: The size of the layout provided here was checked to be non-zero.
        let ptr = unsafe { alloc(layout).cast::<T>() };

        if ptr.is_null() {
            handle_alloc_error(layout)
        } else {
            // sanity check. None of the code in GcBox is sound if this assertion fails.
            debug_assert_eq!(ptr.to_raw_parts().0.addr() % 2, 0);

            unsafe {
                // SAFETY: The pointer is non-null and now pointers to memory with the proper layout
                // for Aligned<T>.
                ptr.write(value);

                // SAFETY: p cannot be null as it was checked above.
                NonNull::new_unchecked(ptr)
            }
        }
    }
}

/// A pointer type providing ownership over a heap allocated value of type `T` and a flag to mark
/// the value for garbage collection algorithms.
pub struct GcBox<T: ?Sized> {
    data: NonNull<T>,
}

impl<T> GcBox<T> {
    pub fn new(value: T) -> Self {
        // Aligned<T> makes sure that the pointer returned by `alloc` will have its last bit set
        // to zero, allowing GcBox to use it for storing the "marked" tag.
        let data = alloc_aligned(value);

        Self { data }
    }
}

impl<T: ?Sized> GcBox<T> {
    pub fn mark(&mut self) {
        let (data, meta) = self.data.as_ptr().to_raw_parts();

        let data = data.map_addr(|addr| addr | TAG);

        // SAFETY: GcBox::new ensures that this pointer is non-null.
        self.data = unsafe { NonNull::new_unchecked(ptr::from_raw_parts_mut(data, meta)) }
    }

    pub fn unmark(&mut self) {
        let (data, meta) = self.data.as_ptr().to_raw_parts();

        let data = data.map_addr(|addr| addr & TAG_MASK);

        // SAFETY: GcBox::new ensures that this pointer is non-null.
        self.data = unsafe { NonNull::new_unchecked(ptr::from_raw_parts_mut(data, meta)) }
    }

    pub fn is_marked(&self) -> bool {
        let (data, _) = self.data.as_ptr().to_raw_parts();
        let data = data.addr();

        let marked = data & TAG;

        marked == TAG
    }

    pub fn into_raw(this: Self) -> *mut T {
        let ptr = this.data.as_ptr();
        mem::forget(this);
        ptr
    }

    pub fn into_raw_unmarked(this: Self) -> *mut T {
        let ptr = unsafe { mask_ptr(this.data).as_ptr() };

        mem::forget(this);
        ptr
    }

    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        let data = unsafe { NonNull::new_unchecked(ptr) };

        Self { data }
    }
}

impl<T: ?Sized> Deref for GcBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY:
        // 1. The NonNull<T> provided to untag_pointer comes from a GcBox<T>
        // 2. The untagged pointer is properly aligned and points to a valid T, so can be converted
        // to a reference safely.
        unsafe { mask_ptr(self.data).as_ref() }
    }
}

impl<T: ?Sized> DerefMut for GcBox<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY:
        // 1. The NonNull<T> provided to untag_pointer comes from a GcBox<T>
        // 2. The untagged pointer is properly aligned, points to a valid T, and comes from a
        // mutable reference to Self, so can be turned into a mutable reference safely.
        unsafe { mask_ptr(self.data).as_mut() }
    }
}

impl<T: Clone> Clone for GcBox<T> {
    fn clone(&self) -> Self {
        let cloned = unsafe { mask_ptr(self.data).as_ref().clone() };

        let data = alloc_aligned(cloned);

        let mut ret = Self { data };

        if self.is_marked() {
            ret.mark();
        }

        ret
    }
}

impl<T: PartialEq> PartialEq for GcBox<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        (**self) == (**other)
    }
}

impl<T: Eq> Eq for GcBox<T> {}

impl<T: PartialOrd> PartialOrd for GcBox<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: Ord> Ord for GcBox<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: Hash> Hash for GcBox<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T: Debug + ?Sized> Debug for GcBox<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GcBox")
            .field("data", &&**self)
            .field("marked", &self.is_marked())
            .finish()
    }
}

impl<T: Display + ?Sized> Display for GcBox<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <T as Display>::fmt(self, f)
    }
}

impl<T: ?Sized> Pointer for GcBox<T> {
    /// Formats the value using the given formatter.
    ///
    /// Note: Formatting a `GcBox<T>` as a pointer formats the masked pointer, not including the
    /// "marked" bit. In other words, the last bit will always be zero.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // SAFETY: The masked pointer originally came from a Box<Aligned<T>>, and thus never had
        // just the last bit set.
        let ptr = unsafe { mask_ptr(self.data) };

        Pointer::fmt(&ptr, f)
    }
}

impl<T: Default> Default for GcBox<T> {
    #[inline]
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl<T: ?Sized> AsRef<T> for GcBox<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T: ?Sized> AsMut<T> for GcBox<T> {
    #[inline]
    fn as_mut(&mut self) -> &mut T {
        self
    }
}

impl<T> From<T> for GcBox<T> {
    #[inline]
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<I: Iterator + ?Sized> Iterator for GcBox<I> {
    type Item = <I as Iterator>::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        (**self).next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (**self).size_hint()
    }

    #[inline]
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        (**self).nth(n)
    }
}

impl<I: DoubleEndedIterator + ?Sized> DoubleEndedIterator for GcBox<I> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        (**self).next_back()
    }

    #[inline]
    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        (**self).nth_back(n)
    }
}

impl<I: FusedIterator + ?Sized> FusedIterator for GcBox<I> {}

impl<R: Read + ?Sized> Read for GcBox<R> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        (**self).read(buf)
    }

    #[inline]
    fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> io::Result<usize> {
        (**self).read_vectored(bufs)
    }

    #[inline]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        (**self).read_to_end(buf)
    }

    #[inline]
    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        (**self).read_to_string(buf)
    }

    #[inline]
    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        (**self).read_exact(buf)
    }
}

impl<W: Write + ?Sized> Write for GcBox<W> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        (**self).write(buf)
    }

    #[inline]
    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        (**self).write_vectored(bufs)
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        (**self).flush()
    }

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        (**self).write_all(buf)
    }

    #[inline]
    fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> io::Result<()> {
        (**self).write_fmt(fmt)
    }
}

impl<S: Seek + ?Sized> Seek for GcBox<S> {
    #[inline]
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        (**self).seek(pos)
    }

    #[inline]
    fn stream_position(&mut self) -> io::Result<u64> {
        (**self).stream_position()
    }
}

impl<T: ?Sized> Unpin for GcBox<T> {}

// SAFETY: GcBox<T> is an owned pointer which will never alias its data.
unsafe impl<T: Send> Send for GcBox<T> {}

// SAFETY: GcBox is an owned pointer which will never alias its data.
unsafe impl<T: Sync> Sync for GcBox<T> {}

impl<T: ?Sized> Drop for GcBox<T> {
    fn drop(&mut self) {
        let layout = unsafe { gc_layout_of_val(&*mask_ptr(self.data).as_ptr()) };

        let ptr = unsafe { mask_ptr(self.data).as_ptr().cast() };

        // SAFETY: The given pointer came from an `alloc` call in GcBox::new with the same layout,
        // and is non-null.
        unsafe {
            dealloc(ptr, layout);
        }
    }
}

pub struct GcObject {
    pub(crate) meta: TypeMeta,
    pub(crate) obj: GcBox<dyn Object>,
}

impl GcObject {
    pub fn new<T: Object + 'static>(value: T) -> Self {
        let meta = T::type_meta();

        Self {
            meta,
            obj: gc_box!(value),
        }
    }

    pub fn meta(&self) -> &TypeMeta {
        &self.meta
    }

    pub fn field(&self, name: &str) -> Option<&Value> {
        self.obj.field(name)
    }

    pub fn field_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.obj.field_mut(name)
    }

    pub fn fields(&self) -> &[Value] {
        self.obj.fields()
    }

    pub fn call_method(&self, name: &str) -> Result<Value, ()> {
        self.obj.call_method(name)
    }

    pub fn mark(&mut self) {
        self.obj.mark();
    }

    pub fn unmark(&mut self) {
        self.obj.unmark();
    }

    pub fn is_marked(&self) -> bool {
        self.obj.is_marked()
    }
}
