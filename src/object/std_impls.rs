use super::VmObject;

impl VmObject for String {
    fn register_type(program: &mut crate::program::Program) -> Option<&super::VmType>
    where
        Self: Sized,
    {
        todo!()
    }

    fn field(&self, name: &str) -> Option<&crate::value::Value> {
        todo!()
    }

    fn field_mut(&mut self, name: &str) -> Option<&mut crate::value::Value> {
        todo!()
    }

    fn fields(&self) -> &[crate::value::Value] {
        todo!()
    }
}
