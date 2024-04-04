use super::ops::OpCode;
use crate::utils::FxHashMap;
use hashbrown::hash_map::RawEntryMut;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use std::sync::Arc;

#[derive(
    Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default, Serialize, Deserialize,
)]
pub struct Function(pub(crate) usize);

impl Function {
    #[inline(always)]
    pub fn new(start: usize) -> Self {
        Self(start)
    }
}

#[derive(Clone, PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct Functions {
    pub(crate) indices: FxHashMap<Arc<str>, Function>,
    pub(crate) code: Vec<OpCode>,
}

impl Functions {
    pub fn new() -> Self {
        Self {
            indices: FxHashMap::default(),
            code: Vec::new(),
        }
    }

    pub fn define<I: IntoIterator<Item = OpCode>>(
        &mut self,
        name: impl AsRef<str>,
        func: I,
    ) -> Result<Function, I> {
        let name = name.as_ref();

        match self.indices.raw_entry_mut().from_key(name) {
            RawEntryMut::Vacant(entry) => {
                let start = self.code.len();
                self.code.extend(func);
                let func = Function::new(start);

                entry.insert(Arc::from(name), func);
                Ok(func)
            }
            RawEntryMut::Occupied(_) => Err(func),
        }
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&Function> {
        self.indices.get(name.as_ref())
    }

    pub fn get_opcode(&self, index: usize) -> Option<&OpCode> {
        self.code.get(index)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct Path<'a> {
    pub(crate) module: &'a str,
    pub(crate) ty: Option<&'a str>,
    pub(crate) field_or_func: &'a str,
}

impl<'a> Path<'a> {
    pub fn new(path: &'a str) -> Result<Self, PathError> {
        #[inline(always)]
        fn str_is_alphanumeric(s: &str) -> bool {
            s.chars().all(char::is_alphanumeric)
        }

        if path.is_empty() {
            return Err(PathError(path));
        }

        // Paths generally take the form of either:
        // 1. module::and::submodules::function
        // 2. module::and::submodules::Type.field_or_method
        // Modules are followed by either a function, or a Type.field_or_method pair.
        let (module, rest) = path.rsplit_once("::").ok_or(PathError(path))?;

        let (ty, field_or_func) = match rest.split_once('.') {
            Some((ty, field_or_func)) if !ty.is_empty() && !field_or_func.is_empty() => {
                // Type.field_or_method
                (Some(ty), field_or_func)
            }
            _ if !rest.is_empty() => {
                // function
                (None, rest)
            }
            _ => {
                // empty case
                return Err(PathError(path));
            }
        };

        // Requirements for a valid path:
        // 1. When the module is split by the :: separator,
        //    - All of the elements in the iterator must be entirely alphanumerical.
        //    - None of the elements can be empty.
        // 2. There is no type component, or if there is, it must be alphanumeric.
        // 3. The field or free function name must be alphanumeric.
        let valid = module
            .split("::")
            .all(|sub| str_is_alphanumeric(sub) && !sub.is_empty())
            && (ty.is_none() || ty.is_some_and(str_is_alphanumeric))
            && str_is_alphanumeric(field_or_func);

        if !valid {
            return Err(PathError(path));
        }

        Ok(Self {
            module,
            ty,
            field_or_func,
        })
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Error)]
#[error("\"{0}\" is not a valid path")]
pub struct PathError<'a>(&'a str);

#[cfg(test)]
mod test {
    use super::{Path, PathError};

    #[test]
    fn well_formed_paths() {
        let a = "a::b::c::Type.field";
        let b = "d::function";

        assert_eq!(
            Path::new(a),
            Ok(Path {
                module: "a::b::c",
                ty: Some("Type"),
                field_or_func: "field"
            })
        );

        assert_eq!(
            Path::new(b),
            Ok(Path {
                module: "d",
                ty: None,
                field_or_func: "function",
            })
        );
    }

    #[test]
    fn ill_formed_paths() {
        let a = "a::b::c::.";
        let b = "a::b::";
        let c = "a::b::Type.";
        let d = "a::b::.field";
        let e = "Type.method";
        let f = "a::Type.Type.member";
        let g = "a::Type.........member";

        assert_eq!(
            Path::new(a),
            Err(PathError(a))
        );
        assert_eq!(
            Path::new(b),
            Err(PathError(b))
        );
        assert_eq!(Path::new(c), Err(PathError(c)));
        assert_eq!(Path::new(d), Err(PathError(d)));
        assert_eq!(
            Path::new(e),
            Err(PathError(e))
        );
        assert_eq!(Path::new(""), Err(PathError("")));
        assert_eq!(
            Path::new(f),
            Err(PathError(f))
        );
        assert_eq!(
            Path::new(g),
            Err(PathError(g))
        );
    }
}
