use crate::vm::heap::HeapIndex;
use paste::paste;

macro_rules! variant_methods {
    ($($variant:ident($inner_ty:ty), $article:literal);+ $(;)?) => {
        impl Value {
            paste! {
                $(
                    #[doc = "Converts from `Value` to `Option<" $inner_ty ">`, discarding"]
                    #[doc = "the value is `self` is not"]
                    #[doc = " " $article " "]
                    #[doc = "`" $variant "`."]
                    pub fn [<$variant:lower>](self) -> Option<$inner_ty> {
                        match self {
                            Self::$variant(v) => Some(v),
                            _ => None,
                        }
                    }

                    #[doc = "Converts from `Value` to `" $inner_ty "`, returning the provided"]
                    #[doc = " default value if `self` is not"]
                    #[doc = " " $article " "]
                    #[doc = "`" $variant "`."]
                    pub fn [<$variant:lower _or>](self, default: $inner_ty) -> $inner_ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => default,
                        }
                    }

                    #[doc = "Converts from `Value` to `Result<" $inner_ty ", E>`, returning the"]
                    #[doc = " provided error if `self` is not"]
                    #[doc = " " $article " "]
                    #[doc = "`" $variant "`."]
                    pub fn [<$variant:lower _or_err>]<E>(self, err: E) -> Result<$inner_ty, E> {
                        match self {
                            Self::$variant(v) => Ok(v),
                            _ => Err(err),
                        }
                    }
                )+
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Debug, Default)]
pub enum Value {
    /// A value corresponding to "nothing". The default value when operating on memory.
    #[default]
    Null,
    /// An unsigned 64-bit integer.
    UInt(u64),
    /// A signed 64-bit integer.
    SInt(i64),
    /// A double precision IEEE 754 floating point value (a.k.a. double, f64).
    Float(f64),
    /// A boolean value which can be true or false.
    Bool(bool),
    /// A single Unicode scalar value, corresponding to Rust's `char` type.
    Char(char),
    /// The index of an instruction, stack value, or object field.
    Index(usize),
    /// An object reference, referring to some data in the object heap.
    Object(HeapIndex),
}

variant_methods! {
    UInt(u64), "a";
    SInt(i64), "an";
    Float(f64), "a";
    Bool(bool), "a";
    Char(char), "a";
    Index(usize), "an";
    Object(HeapIndex), "an";
}
