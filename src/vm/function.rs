use super::ops::OpCode;
use crate::utils::FxHashMap;
use hashbrown::hash_map::RawEntryMut;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use thiserror::Error;

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

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Error)]
#[error("\"{0}\" is not a valid path")]
pub struct FnPathError<'a>(&'a str);

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum FnPath<'a> {
    Method { ty: &'a str, method: &'a str },
    Function(&'a str),
}

impl<'a> FnPath<'a> {
    pub fn new(path: &'a str) -> Result<Self, FnPathError<'a>> {
        #[inline(always)]
        fn is_valid_identifier(s: &str) -> bool {
            s.chars().all(|c| c == '_' || c.is_alphanumeric())
        }

        let split = path.rsplit_once('.');

        match split {
            // ".", ".method", or "Type."
            Some(("", "")) | Some(("", _)) | Some((_, "")) => Err(FnPathError(path)),
            Some((ty, method)) => {
                let valid_ty = ty
                    .split("::")
                    .all(|substr| is_valid_identifier(substr) && !substr.is_empty());
                let valid_method = is_valid_identifier(method);

                if valid_ty && valid_method {
                    Ok(FnPath::Method { ty, method })
                } else {
                    Err(FnPathError(path))
                }
            }
            _ if !path.is_empty() => {
                let valid_function = path
                    .split("::")
                    .all(|substr| is_valid_identifier(substr) && !substr.is_empty());

                if valid_function {
                    Ok(FnPath::Function(path))
                } else {
                    Err(FnPathError(path))
                }
            }
            _ => Err(FnPathError(path)),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::vm::function::{FnPath, FnPathError};

    #[test]
    fn well_formed_fn_paths() {
        let a = "a::b::c::Type.field";
        let b = "d::function";

        assert_eq!(
            FnPath::new(a),
            Ok(FnPath::Method {
                ty: "a::b::c::Type",
                method: "field",
            })
        );

        assert_eq!(FnPath::new(b), Ok(FnPath::Function("d::function")));
    }

    #[test]
    fn ill_formed_fn_paths() {
        let only_dot = "a::b::c::.";
        let missing_type_or_function = "a::b::";
        let missing_field = "a::b::Type.";
        let missing_type = "a::b::.field";
        let duplicate_type = "a::Type.Type.member";
        let lots_of_dots = "a::Type.........member";
        let empty_module_component = "a::b::::c::function";
        let non_alphanumeric_component = "a.b::c::d::function";

        assert_eq!(FnPath::new(only_dot), Err(FnPathError(only_dot)));
        assert_eq!(
            FnPath::new(missing_type_or_function),
            Err(FnPathError(missing_type_or_function))
        );
        assert_eq!(FnPath::new(missing_field), Err(FnPathError(missing_field)));
        assert_eq!(FnPath::new(missing_type), Err(FnPathError(missing_type)));
        assert_eq!(FnPath::new(""), Err(FnPathError("")));
        assert_eq!(FnPath::new(duplicate_type), Err(FnPathError(duplicate_type)));
        assert_eq!(FnPath::new(lots_of_dots), Err(FnPathError(lots_of_dots)));
        assert_eq!(
            FnPath::new(empty_module_component),
            Err(FnPathError(empty_module_component))
        );
        assert_eq!(FnPath::new(non_alphanumeric_component), Err(FnPathError(non_alphanumeric_component)));
    }
}
