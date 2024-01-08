use crate::{
    object::{Object, TypeMeta},
    value::Value,
};

impl Object for String {
    fn type_meta() -> TypeMeta
    where
        Self: Sized,
    {
        // ObjectMeta {}
        todo!()
    }

    fn field(&self, name: &str) -> Option<&Value> {
        todo!()
    }

    fn field_mut(&mut self, name: &str) -> Option<&mut Value> {
        todo!()
    }

    fn call_method(&self, name: &str) -> Result<Value, ()> {
        todo!()
    }
}
