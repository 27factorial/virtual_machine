use std::io;

use super::error::{Result, Error};
use paste::paste;
use serde::de::{self, Deserializer as _};

/// Attempt to read some bytes (in little-endian order) from a deserializer's reader and parse them
/// as a given type. This only works for the primitive integer types.
macro_rules! read {
    ($self:expr; $t:ty) => {{
        const SIZE: usize = core::mem::size_of::<$t>();

        let mut buf = [0; SIZE];

        $self.reader.read_exact(&mut buf)?;

        Ok::<_, Error>(<$t>::from_le_bytes(buf))
    }};
}

/// Automatically read a value from the deserializer and call the proper visitor method.
macro_rules! visit {
    ($self:ident, $visitor:ident; $t:ty) => {{
        paste! {
            $visitor.[<visit_ $t>](read!($self; $t)?)
        }
    }};
}

pub struct Deserializer<R> {
    reader: R,
}

impl<R: io::Read> Deserializer<R> {
    pub fn new(reader: R) -> Self {
        Self { reader }
    }

    fn read_bytes(&mut self) -> Result<Vec<u8>> {
        let len: usize = read!(self; u64)?
            .try_into()
            .map_err(|_| Error::ExpectedUsize)?;
        let mut buf = vec![0u8; len];

        self.reader.read_exact(&mut buf)?;

        Ok(buf)
    }

    fn read_string(&mut self) -> Result<String> {
        let buf = self.read_bytes()?;
        String::from_utf8(buf).map_err(|_| Error::ExpectedString)
    }
}

impl<'de, 'a, R: io::Read> de::Deserializer<'de> for &'a mut Deserializer<R>
{
    type Error = Error;

    fn deserialize_any<V>(self, _: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(de::Error::custom("deserialize_any is not supported"))
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let byte = read!(self; u8)?;
        let value = match byte {
            0 => false,
            1 => true,
            _ => return Err(Error::ExpectedBool),
        };

        visitor.visit_bool(value)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visit!(self, visitor; i8)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visit!(self, visitor; i16)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visit!(self, visitor; i32)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visit!(self, visitor; i64)
    }

    fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visit!(self, visitor; i128)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visit!(self, visitor; u8)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visit!(self, visitor; u16)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visit!(self, visitor; u32)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visit!(self, visitor; u64)
    }

    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visit!(self, visitor; u128)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let value = read!(self; f32)?;

        if value.is_finite() {
            visitor.visit_f32(value)
        } else {
            Err(Error::InvalidFloat)
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let value = read!(self; f64)?;

        if value.is_finite() {
            visitor.visit_f64(value)
        } else {
            Err(Error::InvalidFloat)
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let int = read!(self; u32)?;
        let value = char::from_u32(int).ok_or(Error::ExpectedChar)?;
        visitor.visit_char(value)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let string = self.read_string()?;
        visitor.visit_str(&string)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let string = self.read_string()?;
        visitor.visit_string(string)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let bytes = self.read_bytes()?;
        visitor.visit_bytes(&bytes)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let bytes = self.read_bytes()?;
        visitor.visit_byte_buf(bytes)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let discriminant = read!(self; u8)?;
        match discriminant {
            0 => visitor.visit_none(),
            1 => visitor.visit_some(self),
            _ => Err(Error::ExpectedBool),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(self, _: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V>(
        self,
        _: &'static str,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let len: usize = read!(self; u64)?
            .try_into()
            .map_err(|_| Error::ExpectedUsize)?;

        visitor.visit_seq(SeqAccessor::new(self, len))
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(SeqAccessor::new(self, len))
    }

    fn deserialize_tuple_struct<V>(
        self,
        _: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let len: usize = read!(self; u64)?
            .try_into()
            .map_err(|_| Error::ExpectedUsize)?;

        visitor.visit_map(MapAccessor::new(self, len))
    }

    fn deserialize_struct<V>(
        self,
        _: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let len = fields.len();

        visitor.visit_seq(SeqAccessor::new(self, len))
    }

    fn deserialize_enum<V>(
        self,
        _: &'static str,
        _: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_enum(self)
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_u32(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }
}

impl<'de, 'a, R: io::Read> de::EnumAccess<'de> for &'a mut Deserializer<R> {
    type Error = Error;

    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de> {
        let val = seed.deserialize(&mut *self)?;

        Ok((val, self))
    }
}

impl<'de, 'a, R: io::Read> de::VariantAccess<'de> for &'a mut Deserializer<R> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de> {
        seed.deserialize(&mut *self)
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de> {
        self.deserialize_tuple(len, visitor)
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de> {
        self.deserialize_struct("", fields, visitor)
    }
}

pub struct SeqAccessor<'a, R> {
    de: &'a mut Deserializer<R>,
    remaining: usize,
}

impl<'a, R> SeqAccessor<'a, R> {
    pub fn new(de: &'a mut Deserializer<R>, len: usize) -> Self {
        Self {
            de,
            remaining: len,
        }
    }
}

impl<'de, 'a, R: io::Read> de::SeqAccess<'de> for SeqAccessor<'a, R> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.remaining == 0 {
            Ok(None)
        } else {
            self.remaining -= 1;
            seed.deserialize(&mut *self.de).map(Some)
        }
    }
}

pub struct MapAccessor<'a, R> {
    de: &'a mut Deserializer<R>,
    remaining_keys: usize,
    remaining_values: usize,
}

impl<'a, R> MapAccessor<'a, R> {
    pub fn new(de: &'a mut Deserializer<R>, len: usize) -> Self {
        Self {
            de,
            remaining_keys: len,
            remaining_values: len,
        }
    }
}

impl<'de, 'a, R: io::Read> de::MapAccess<'de> for MapAccessor<'a, R>
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.remaining_keys == 0 {
            Ok(None)
        } else {
            self.remaining_keys -= 1;
            seed.deserialize(&mut *self.de).map(Some)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        if self.remaining_values == 0 {
            Err(Error::ExpectedMapValue)
        } else {
            self.remaining_values -= 1;
            seed.deserialize(&mut *self.de)
        }
    }
}
