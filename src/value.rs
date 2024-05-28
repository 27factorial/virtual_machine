use std::fmt::{self, Debug, Display};

use crate::vm::heap::Reference;
use crate::{symbol::Symbol, vm::function::Function};
use paste::paste;
use serde::{Deserialize, Serialize};

macro_rules! variant_methods {
    ($($variant:ident($inner_ty:ty), $article:literal);+ $(;)?) => {
        impl Value {
            paste! {
                $(
                    pub const [<$variant:upper _TYPE_NAME>]: &'static str = stringify!($variant);
                )+

                $(
                    #[doc = "Returns `true` if the `Value` is " $article " `" $variant "`."]
                    #[inline]
                    pub fn [<is_ $variant:lower>](self) -> bool {
                        matches!(self, Self::$variant(_))
                    }

                    #[doc = "Converts from `Value` to `Option<" $inner_ty ">`, discarding"]
                    #[doc = "the value is `self` is not"]
                    #[doc = " " $article " "]
                    #[doc = "`" $variant "`."]
                    #[inline]
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
                    #[inline]
                    pub fn [<unwrap_ $variant:lower _or>](self, default: $inner_ty) -> $inner_ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => default,
                        }
                    }

                    #[inline]
                    pub fn [<unwrap_ $variant:lower _or_else>]<F: FnOnce(Value) -> $inner_ty>(self, f: F) -> $inner_ty {
                        match self {
                            Self::$variant(v) => v,
                            value => f(value),
                        }
                    }

                    #[doc = "Converts from `Value` to `Result<" $inner_ty ", E>`, returning the"]
                    #[doc = " provided error if `self` is not"]
                    #[doc = " " $article " "]
                    #[doc = "`" $variant "`."]
                    #[inline]
                    pub fn [<$variant:lower _or>]<E>(self, err: E) -> Result<$inner_ty, E> {
                        match self {
                            Self::$variant(v) => Ok(v),
                            _ => Err(err),
                        }
                    }

                    #[inline]
                    pub fn [<$variant:lower _ref_or>]<E>(&self, err: E) -> Result<&$inner_ty, E> {
                        match self {
                            Self::$variant(v) => Ok(v),
                            _ => Err(err),
                        }
                    }

                    #[inline]
                    pub fn [<$variant:lower _ref_mut_or>]<E>(&mut self, err: E) -> Result<&mut $inner_ty, E> {
                        match self {
                            Self::$variant(v) => Ok(v),
                            _ => Err(err),
                        }
                    }

                    #[inline]
                    pub fn [<$variant:lower _or_else>]<F, E>(self, f: F) -> Result<$inner_ty, E>
                    where
                        F: FnOnce(Value) -> E
                    {
                        match self {
                            Self::$variant(v) => Ok(v),
                            value => Err(f(value))
                        }
                    }

                    #[inline]
                    pub fn [<$variant:lower _ref_or_else>]<F, E>(&self, f: F) -> Result<&$inner_ty, E>
                    where
                        F: FnOnce(&Value) -> E
                    {
                        match self {
                            Self::$variant(v) => Ok(v),
                            value => Err(f(value))
                        }
                    }

                    #[inline]
                    pub fn [<$variant:lower _ref_mut_or_else>]<F, E>(&mut self, f: F) -> Result<&mut $inner_ty, E>
                    where
                        F: FnOnce(&mut Value) -> E
                    {
                        match self {
                            Self::$variant(v) => Ok(v),
                            value => Err(f(value))
                        }
                    }
                )+
            }
        }
    }
}

#[repr(usize)]
#[derive(Copy, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum Value {
    /// A signed 64-bit integer.
    Int(i64),
    /// A double precision IEEE 754-2008 floating point value (a.k.a. double, f64).
    Float(f64),
    /// A boolean value which can be true or false.
    Bool(bool),
    /// A single Unicode scalar value, corresponding to Rust's `char` type.
    Char(char),
    /// The index of a symbol.
    Symbol(Symbol),
    // A reference to a function.
    Function(Function),
    /// An object reference, referring to some data in the object heap.
    Reference(Reference),
}

impl Value {
    pub const fn type_name(&self) -> &'static str {
        match self {
            Self::Int(_) => Self::INT_TYPE_NAME,
            Self::Float(_) => Self::FLOAT_TYPE_NAME,
            Self::Bool(_) => Self::BOOL_TYPE_NAME,
            Self::Char(_) => Self::CHAR_TYPE_NAME,
            Self::Symbol(_) => Self::SYMBOL_TYPE_NAME,
            Self::Function(_) => Self::FUNCTION_TYPE_NAME,
            Self::Reference(_) => Self::REFERENCE_TYPE_NAME,
        }
    }
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
        Self::Int(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

impl From<Symbol> for Value {
    fn from(value: Symbol) -> Self {
        Self::Symbol(value)
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Self::Function(value)
    }
}

impl From<Reference> for Value {
    fn from(value: Reference) -> Self {
        Self::Reference(value)
    }
}

impl From<EqValue> for Value {
    fn from(value: EqValue) -> Self {
        match value {
            EqValue::Int(v) => Self::Int(v),
            EqValue::Float(v) => Self::Float(f64::from_bits(v)),
            EqValue::Bool(v) => Self::Bool(v),
            EqValue::Char(v) => Self::Char(v),
            EqValue::Symbol(v) => Self::Symbol(v),
            EqValue::Function(v) => Self::Function(v),
            EqValue::Reference(v) => Self::Reference(v),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{}", v),
            Self::Bool(v) => write!(f, "{}", v),
            Self::Char(v) => write!(f, "{}", v),
            Self::Symbol(v) => write!(f, "{}", v.0),
            Self::Function(v) => write!(f, "func({:#x})", v.0),
            Self::Reference(v) => write!(f, "ref({:#x})", v.0),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // debug_tuple isn't used here because it will cause line breaks when using the alternate
        // Debug flag, which isn't necessary since each variant only has one field.
        match self {
            Self::Int(v) => write!(f, "Int({})", v),
            Self::Float(v) => write!(f, "Float({})", v),
            Self::Bool(v) => write!(f, "Bool({})", v),
            Self::Char(v) => write!(f, "Char({})", v),
            Self::Symbol(v) => write!(f, "Symbol({})", v.0),
            Self::Function(v) => write!(f, "Function({})", v.0),
            Self::Reference(v) => write!(f, "Reference({})", v.0),
        }
    }
}

// This is a special kind of values for cases where total comparison / equality are required.
// When using this type, Float values should always be checked for NaN, otherwise things may break.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum EqValue {
    Int(i64),
    Float(u64),
    Bool(bool),
    Char(char),
    Symbol(Symbol),
    Function(Function),
    Reference(Reference),
}

impl From<Value> for EqValue {
    fn from(value: Value) -> Self {
        match value {
            Value::Int(v) => Self::Int(v),
            Value::Float(v) => Self::Float(v.to_bits()),
            Value::Bool(v) => Self::Bool(v),
            Value::Char(v) => Self::Char(v),
            Value::Symbol(v) => Self::Symbol(v),
            Value::Function(v) => Self::Function(v),
            Value::Reference(v) => Self::Reference(v),
        }
    }
}

impl Display for EqValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{}", v),
            Self::Bool(v) => write!(f, "{}", v),
            Self::Char(v) => write!(f, "{}", v),
            Self::Symbol(v) => write!(f, "{}", v.0),
            Self::Function(v) => write!(f, "func({:#x})", v.0),
            Self::Reference(v) => write!(f, "ref({:#x})", v.0),
        }
    }
}

impl Debug for EqValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // debug_tuple isn't used here because it will cause line breaks when using the alternate
        // Debug flag, which isn't necessary since each variant only has one primitive field.
        match self {
            Self::Int(v) => write!(f, "Int({})", v),
            Self::Float(v) => write!(f, "Float({})", f64::from_bits(*v)),
            Self::Bool(v) => write!(f, "Bool({})", v),
            Self::Char(v) => write!(f, "Char({})", v),
            Self::Symbol(v) => write!(f, "Symbol({})", v.0),
            Self::Function(v) => write!(f, "Function({})", v.0),
            Self::Reference(v) => write!(f, "Reference({})", v.0),
        }
    }
}
