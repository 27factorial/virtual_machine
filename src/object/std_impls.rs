use crate::{program::Program, value::Value};

use super::{VmObject, VmType};

impl VmObject for String {
    fn register_type(program: &mut Program) -> &VmType
    where
        Self: Sized,
    {
        todo!()
    }

    fn field(&self, name: &str) -> Option<&Value> {
        todo!()
    }

    fn field_mut(&mut self, name: &str) -> Option<&mut Value> {
        todo!()
    }

    fn fields(&self) -> &[Value] {
        todo!() 
    }
}
