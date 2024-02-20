// use crate::{ops::OpCode, program::Program, string::SymbolIndex, value::Value, vm::Register};

// use super::{Operations, VmObject, VmType};

// impl Operations for String {
//     /// The index of the symbol which names the String constructor in the program's native functions
//     type InitArgs = SymbolIndex;
//     type DeinitArgs = ();
//     type IndexArgs = ();

//     fn init(native: SymbolIndex) -> impl IntoIterator<Item = OpCode> {
//         use OpCode::*;

//         [CallNative(native), Ret]
//     }

//     fn deinit(_: ()) -> Option<impl IntoIterator<Item = OpCode>> {
//         Self::unimplemented()
//     }

//     fn index(_: ()) -> Option<impl IntoIterator<Item = OpCode>> {
//         Self::unimplemented()
//     }
// }

// impl VmObject for String {
//     fn register_type(program: &mut Program) -> &VmType
//     where
//         Self: Sized,
//     {
//         let new = program.define_symbol("String::new");
//         let print = program.define_symbol("String::print");

//         program
//             .define_native_function(new, |vm| {
//                 let object = vm.alloc(String::from("init worked!"));
//                 Some(Value::Object(object))
//             })
//             .unwrap_or_else(|_| todo!());

//         program
//             .define_native_function(print, |vm| {
//                 let mem = vm.memory();

//                 let object_ref = mem.registers[Register::R0].object().unwrap();
//                 let this = mem
//                     .heap
//                     .get(object_ref)
//                     .and_then(|obj| obj.downcast_ref::<Self>())
//                     .unwrap();

//                 eprintln!("{this}");

//                 None
//             })
//             .unwrap_or_else(|_| todo!());

//         let ty = VmType::new("String", String::init(new))
//             .with_method("print", [OpCode::CallNative(print), OpCode::Ret]);

//         program.register_type(ty)
//     }

//     fn field(&self, _: &str) -> Option<&Value> {
//         None
//     }

//     fn field_mut(&mut self, _: &str) -> Option<&mut Value> {
//         None
//     }

//     fn fields(&self) -> &[Value] {
//         &[]
//     }
// }
