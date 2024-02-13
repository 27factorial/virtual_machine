use super::error::{Error, Result};
use serde::ser;
use std::io;

pub struct Serializer<W> {
    writer: W,
}

impl<W: io::Write> Serializer<W> {
    pub fn new(writer: W) -> Self {
        Self { writer }
    }
}

impl<'a, W: io::Write> ser::Serializer for &'a mut Serializer<W> {
    type Ok = ();

    type Error = Error;

    type SerializeSeq = Self;

    type SerializeTuple = Self;

    type SerializeTupleStruct = Self;

    type SerializeTupleVariant = Self;

    type SerializeMap = Self;

    type SerializeStruct = Self;

    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok> {
        self.serialize_u8(v as u8)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok> {
        self.writer.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok> {
        self.writer.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok> {
        self.writer.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok> {
        self.writer.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    fn serialize_i128(self, v: i128) -> Result<Self::Ok> {
        self.writer.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok> {
        self.writer.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok> {
        self.writer.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok> {
        self.writer.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok> {
        self.writer.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    fn serialize_u128(self, v: u128) -> Result<Self::Ok> {
        self.writer.write_all(&v.to_le_bytes())?;
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok> {
        if v.is_finite() {
            self.writer.write_all(&v.to_le_bytes())?;
            Ok(())
        } else {
            Err(Error::InvalidFloat)
        }
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok> {
        if v.is_finite() {
            self.writer.write_all(&v.to_le_bytes())?;
            Ok(())
        } else {
            Err(Error::InvalidFloat)
        }
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok> {
        self.serialize_u32(v as u32)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok> {
        self.serialize_bytes(v.as_bytes())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok> {
        self.serialize_u64(v.len() as u64)?;
        self.writer.write_all(v)?;
        Ok(())
    }

    fn serialize_none(self) -> Result<Self::Ok> {
        self.serialize_bool(false)
    }

    fn serialize_some<T: ?Sized>(self, value: &T) -> Result<Self::Ok>
    where
        T: ser::Serialize,
    {
        self.serialize_bool(true)?;
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok> {
        Ok(())
    }

    fn serialize_unit_struct(self, _: &'static str) -> Result<Self::Ok> {
        Ok(())
    }

    fn serialize_unit_variant(
        self,
        _: &'static str,
        variant_index: u32,
        _: &'static str,
    ) -> Result<Self::Ok> {
        self.serialize_u32(variant_index)
    }

    fn serialize_newtype_struct<T: ?Sized>(self, _: &'static str, value: &T) -> Result<Self::Ok>
    where
        T: ser::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _: &'static str,
        variant_index: u32,
        _: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
    where
        T: ser::Serialize,
    {
        self.serialize_u32(variant_index)?;
        value.serialize(self)
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        if let Some(len) = len {
            self.writer.write_all(&(len as u64).to_le_bytes())?;
        }

        Ok(self)
    }

    fn serialize_tuple(self, _: usize) -> Result<Self::SerializeTuple> {
        Ok(self)
    }

    fn serialize_tuple_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        Ok(self)
    }

    fn serialize_tuple_variant(
        self,
        _: &'static str,
        variant_index: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        self.serialize_u32(variant_index)?;
        Ok(self)
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        if let Some(len) = len {
            self.serialize_u64(len as u64)?;
        }

        Ok(self)
    }

    fn serialize_struct(self, _: &'static str, _: usize) -> Result<Self::SerializeStruct> {
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        _: &'static str,
        variant_index: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStructVariant> {
        self.serialize_u32(variant_index)?;
        Ok(self)
    }

    fn is_human_readable(&self) -> bool {
        false
    }
}

impl<'a, W: io::Write> ser::SerializeSeq for &'a mut Serializer<W> {
    type Ok = ();

    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(())
    }
}

impl<'a, W: io::Write> ser::SerializeTuple for &'a mut Serializer<W> {
    type Ok = ();

    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(())
    }
}

impl<'a, W: io::Write> ser::SerializeTupleStruct for &'a mut Serializer<W> {
    type Ok = ();

    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(())
    }
}

impl<'a, W: io::Write> ser::SerializeTupleVariant for &'a mut Serializer<W> {
    type Ok = ();

    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(())
    }
}

impl<'a, W: io::Write> ser::SerializeMap for &'a mut Serializer<W> {
    type Ok = ();

    type Error = Error;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        key.serialize(&mut **self)
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(())
    }
}

impl<'a, W: io::Write> ser::SerializeStruct for &'a mut Serializer<W> {
    type Ok = ();

    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, _: &'static str, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(())
    }
}

impl<'a, W: io::Write> ser::SerializeStructVariant for &'a mut Serializer<W> {
    type Ok = ();

    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, _: &'static str, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(())
    }
}
