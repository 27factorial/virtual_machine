use std::fmt::{self, Display};

use crate::vm::heap::Reference;
use crate::{symbol::Symbol, vm::function::Function};
use paste::paste;
use serde::{Deserialize, Serialize};

macro_rules! variant_methods {
    ($($variant:ident($inner_ty:ty), $article:literal);+ $(;)?) => {
        impl Value {
            paste! {
                $(
                    #[doc = "Returns `true` if the `Value` is " $article " `" $variant "`."]
                    pub fn [<is_ $variant:lower>](self) -> bool {
                        matches!(self, Self::$variant(_))
                    }

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

#[repr(usize)]
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug, Serialize, Deserialize)]
pub enum Value {
    /// A signed 64-bit integer.
    Int(i64),
    /// A double precision IEEE 754 floating point value (a.k.a. double, f64).
    Float(f64),
    /// A boolean value which can be true or false.
    Bool(bool),
    /// A single Unicode scalar value, corresponding to Rust's `char` type.
    Char(char),
    /// The address of an instruction, stack value, or object field.
    /// The index of a symbol.
    Symbol(Symbol),
    Function(Function),
    /// An object reference, referring to some data in the object heap.
    Reference(Reference),
}

variant_methods! {
    Int(i64), "an";
    Float(f64), "a";
    Bool(bool), "a";
    Char(char), "a";
    Symbol(Symbol), "a";
    Function(Function), "a";
    Reference(Reference), "a";
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Int(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Value::Char(value)
    }
}

impl From<Symbol> for Value {
    fn from(value: Symbol) -> Self {
        Value::Symbol(value)
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Value::Function(value)
    }
}

impl From<Reference> for Value {
    fn from(value: Reference) -> Self {
        Value::Reference(value)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Char(v) => write!(f, "{}", v),
            Value::Symbol(v) => write!(f, "{}", v.0),
            Value::Function(v) => write!(f, "f{:x}", v.0),
            Value::Reference(v) => write!(f, "r{:x}", v.0),
        }
    }
}