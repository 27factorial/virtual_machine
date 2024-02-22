mod de;
mod error;
mod ser;

// #[cfg(test)]
// mod serde_test {
//     use super::de::*;
//     use super::ser::*;
//     use serde::{Deserialize, Serialize};

//     #[test]
//     #[rustfmt::skip]
//     fn primitives() {
//         let mut buf = Vec::new();

//         // --- SERIALIZATION ---
//         let mut serializer = Serializer::new(&mut buf);

//         // bool
//         true.serialize(&mut serializer).expect("failed to serialize bool (true)");
//         false.serialize(&mut serializer).expect("failed to serialize bool (false)");

//         // unsigned ints
//         42u8.serialize(&mut serializer).expect("failed to serialize u8");
//         42u16.serialize(&mut serializer).expect("failed to serialize u16");
//         42u32.serialize(&mut serializer).expect("failed to serialize u32");
//         42u64.serialize(&mut serializer).expect("failed to serialize u64");
//         42u128.serialize(&mut serializer).expect("failed to serialize u128");
//         42usize.serialize(&mut serializer).expect("failed to serialize usize");

//         // signed ints
//         (-42i8).serialize(&mut serializer).expect("failed to serialize i8");
//         (-42i16).serialize(&mut serializer).expect("failed to serialize i16");
//         (-42i32).serialize(&mut serializer).expect("failed to serialize i32");
//         (-42i64).serialize(&mut serializer).expect("failed to serialize i64");
//         (-42i128).serialize(&mut serializer).expect("failed to serialize i128");
//         (-42isize).serialize(&mut serializer).expect("failed to serialize isize");

//         // floats
//         (-42.0f32).serialize(&mut serializer).expect("failed to serialize f32");
//         (-42.0f64).serialize(&mut serializer).expect("failed to serialize f64");

//         // char
//         'a'.serialize(&mut serializer).expect("failed to serialize char");

//         // str
//         "test string".serialize(&mut serializer).expect("failed to serialize str");

//         // ---DESERIALIZATION---
//         let mut deserializer = Deserializer::new(&buf[..]);

//         assert!(bool::deserialize(&mut deserializer).expect("failed to deserialize bool (true)"));
//         assert!(!bool::deserialize(&mut deserializer).expect("failed to deserialize bool (false)"));

//         assert_eq!(u8::deserialize(&mut deserializer).expect("failed to deserialize u8"), 42);
//         assert_eq!(u16::deserialize(&mut deserializer).expect("failed to deserialize u16"), 42);
//         assert_eq!(u32::deserialize(&mut deserializer).expect("failed to deserialize u32"), 42);
//         assert_eq!(u64::deserialize(&mut deserializer).expect("failed to deserialize u64"), 42);
//         assert_eq!(u128::deserialize(&mut deserializer).expect("failed to deserialize u128"), 42);
//         assert_eq!(usize::deserialize(&mut deserializer).expect("failed to deserialize usize"), 42);

//         assert_eq!(i8::deserialize(&mut deserializer).expect("failed to deserialize i8"), -42);
//         assert_eq!(i16::deserialize(&mut deserializer).expect("failed to deserialize i16"), -42);
//         assert_eq!(i32::deserialize(&mut deserializer).expect("failed to deserialize i32"), -42);
//         assert_eq!(i64::deserialize(&mut deserializer).expect("failed to deserialize i64"), -42);
//         assert_eq!(i128::deserialize(&mut deserializer).expect("failed to deserialize i128"), -42);
//         assert_eq!(isize::deserialize(&mut deserializer).expect("failed to deserialize isize"), -42);

//         assert_eq!(f32::deserialize(&mut deserializer).expect("failed to deserialize f32"), -42.0);
//         assert_eq!(f64::deserialize(&mut deserializer).expect("failed to deserialize f64"), -42.0);

//         assert_eq!(char::deserialize(&mut deserializer).expect("failed to deserialize char"), 'a');
//         assert_eq!(
//             // strs can't be deserialized directly, so Box<str> is used.
//             &*Box::<str>::deserialize(&mut deserializer).expect("failed to deserialize str"),
//             "test string"
//         );
//     }
// }
