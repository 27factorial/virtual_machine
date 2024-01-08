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
    Object(usize),
}

impl Value {
    pub fn uint_or<E>(self, err: E) -> Result<u64, E> {
        match self {
            Self::UInt(v) => Ok(v),
            _ => Err(err),
        }
    }

    pub fn sint_or<E>(self, err: E) -> Result<i64, E> {
        match self {
            Self::SInt(v) => Ok(v),
            _ => Err(err),
        }
    }

    pub fn float_or<E>(self, err: E) -> Result<f64, E> {
        match self {
            Self::Float(v) => Ok(v),
            _ => Err(err),
        }
    }

    pub fn bool_or<E>(self, err: E) -> Result<bool, E> {
        match self {
            Self::Bool(v) => Ok(v),
            _ => Err(err),
        }
    }

    pub fn char_or<E>(self, err: E) -> Result<char, E> {
        match self {
            Self::Char(v) => Ok(v),
            _ => Err(err),
        }
    }

    pub fn index_or<E>(self, err: E) -> Result<usize, E> {
        match self {
            Self::Index(v) => Ok(v),
            _ => Err(err),
        }
    }

    pub fn object_or<E>(self, err: E) -> Result<usize, E> {
        match self {
            Self::Object(v) => Ok(v),
            _ => Err(err),
        }
    }
}
