use std::{
    collections::{
        btree_map,
        BTreeMap,
    },
    fmt::Display,
    ops::Bound,
    pin::Pin,
    iter::Peekable,
};

use byteorder::LittleEndian;
use futures_lite::{
    AsyncRead,
    Future,
};
use int_enum::IntEnum;
use serde::{
    de,
    Deserialize,
    Serialize,
};

use super::{
    Error,
    Parse,
};
use crate::file_formats::ReadBytesAsyncExt;

pub struct Metadata(pub BTreeMap<String, MetadataValue>);

impl Metadata {
    pub fn iter<'a>(&'a self) -> btree_map::Iter<'a, String, MetadataValue> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get<T>(&self, key: &str) -> Option<&MetadataValue>
    where
        T: TryFrom<MetadataValue>,
    {
        self.0.get(key)
    }

    pub fn deserialize<'a, T: Deserialize<'a>>(&'a self) -> Result<T, DeserializeError> {
        let mut deserializer = MetadataDeserializer::from_metadata(self);
        Ok(T::deserialize(&mut deserializer)?)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, IntEnum)]
#[repr(u32)]
pub enum MetadataValueType {
    U8 = 0,
    I8 = 1,
    U16 = 2,
    I16 = 3,
    U32 = 4,
    I32 = 5,
    F32 = 6,
    Bool = 7,
    String = 8,
    Array = 9,
    U64 = 10,
    I64 = 11,
    F64 = 12,
}

impl Parse for MetadataValueType {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let value_type = reader.read_u32::<LittleEndian>().await?;
        let value_type = Self::try_from(value_type)
            .map_err(|_| Error::InvalidMetadataValueType { found: value_type })?;
        Ok(value_type)
    }
}

#[derive(Clone, Debug)]
pub enum MetadataValue {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    F32(f32),
    U64(u64),
    I64(i64),
    F64(f64),
    Bool(bool),
    String(String),
    Array(Vec<MetadataValue>),
}

impl MetadataValue {
    pub fn ty(&self) -> MetadataValueType {
        match self {
            Self::U8(_) => MetadataValueType::U8,
            Self::I8(_) => MetadataValueType::I8,
            Self::U16(_) => MetadataValueType::U16,
            Self::I16(_) => MetadataValueType::I16,
            Self::U32(_) => MetadataValueType::U32,
            Self::I32(_) => MetadataValueType::I32,
            Self::F32(_) => MetadataValueType::F32,
            Self::U64(_) => MetadataValueType::U64,
            Self::I64(_) => MetadataValueType::I64,
            Self::F64(_) => MetadataValueType::F64,
            Self::Bool(_) => MetadataValueType::Bool,
            Self::String(_) => MetadataValueType::String,
            Self::Array(_) => MetadataValueType::Array,
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("can't convert {from:?} to {to}")]
pub struct MetadataValueConversionError {
    pub from: MetadataValueType,
    pub to: &'static str,
}

macro_rules! impl_metadata_value_conversion {
    ($mdv_type:ident, $target:ident) => {
        impl TryFrom<MetadataValue> for $target {
            type Error = MetadataValueConversionError;

            fn try_from(value: MetadataValue) -> Result<Self, Self::Error> {
                match value {
                    MetadataValue::$mdv_type(value) => Ok(value),
                    _ => {
                        Err(MetadataValueConversionError {
                            from: value.ty(),
                            to: stringify!($target),
                        })
                    }
                }
            }
        }
    };
}

impl_metadata_value_conversion!(U8, u8);
impl_metadata_value_conversion!(I8, i8);
impl_metadata_value_conversion!(U16, u16);
impl_metadata_value_conversion!(I16, i16);
impl_metadata_value_conversion!(U32, u32);
impl_metadata_value_conversion!(I32, i32);
impl_metadata_value_conversion!(F32, f32);
impl_metadata_value_conversion!(U64, u64);
impl_metadata_value_conversion!(I64, i64);
impl_metadata_value_conversion!(F64, f64);
impl_metadata_value_conversion!(Bool, bool);
impl_metadata_value_conversion!(String, String);

impl TryFrom<MetadataValue> for Vec<MetadataValue> {
    type Error = MetadataValueConversionError;

    fn try_from(value: MetadataValue) -> Result<Self, Self::Error> {
        match value {
            MetadataValue::Array(value) => Ok(value),
            _ => {
                Err(MetadataValueConversionError {
                    from: value.ty(),
                    to: "Vec<_>",
                })
            }
        }
    }
}

impl Parse for MetadataValue {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let value_type = MetadataValueType::parse(&mut reader).await?;
        parse_value(Box::new(reader), value_type).await
    }
}

fn parse_value<'a>(
    mut reader: Box<dyn AsyncRead + Unpin + 'a>,
    value_type: MetadataValueType,
) -> Pin<Box<dyn Future<Output = Result<MetadataValue, Error>> + 'a>> {
    Box::pin(async move {
        let value = match value_type {
            MetadataValueType::U8 => MetadataValue::U8(reader.read_u8().await?),
            MetadataValueType::I8 => MetadataValue::I8(reader.read_i8().await?),
            MetadataValueType::U16 => MetadataValue::U16(reader.read_u16::<LittleEndian>().await?),
            MetadataValueType::I16 => MetadataValue::I16(reader.read_i16::<LittleEndian>().await?),
            MetadataValueType::U32 => MetadataValue::U32(reader.read_u32::<LittleEndian>().await?),
            MetadataValueType::I32 => MetadataValue::I32(reader.read_i32::<LittleEndian>().await?),
            MetadataValueType::F32 => MetadataValue::F32(reader.read_f32::<LittleEndian>().await?),
            MetadataValueType::Bool => MetadataValue::Bool(bool::parse(&mut reader).await?),
            MetadataValueType::String => MetadataValue::String(String::parse(&mut reader).await?),
            MetadataValueType::Array => {
                let array_type = MetadataValueType::parse(&mut reader).await?;
                let array_len = reader.read_u64::<LittleEndian>().await?;
                let mut array_values = Vec::with_capacity(array_len as usize);
                for _ in 0..array_len {
                    let value = parse_value(Box::new(&mut reader), array_type).await?;
                    array_values.push(value);
                }
                MetadataValue::Array(array_values)
            }
            MetadataValueType::U64 => MetadataValue::U64(reader.read_u64::<LittleEndian>().await?),
            MetadataValueType::I64 => MetadataValue::I64(reader.read_i64::<LittleEndian>().await?),
            MetadataValueType::F64 => MetadataValue::F64(reader.read_f64::<LittleEndian>().await?),
        };
        Ok(value)
    })
}

macro_rules! impl_deserialize_unexpected {
    ($method:ident) => {
        fn $method<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
        where
            V: de::Visitor<'de>,
        {
            Err(DeserializeError::Unexpected)
        }
    };
}

pub struct MetadataDeserializer<'de> {
    metadata: &'de Metadata,
    prefix: &'de str,
}

impl<'de> MetadataDeserializer<'de> {
    pub fn from_metadata(metadata: &'de Metadata) -> Self {
        Self {
            metadata,
            prefix: "",
        }
    }
}

impl<'a, 'de> de::Deserializer<'de> for &'a mut MetadataDeserializer<'de> {
    type Error = DeserializeError;

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_map(MetadataMap::new(self))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de> {
        
        let mut iter = self.metadata.0.range::<str, _>((
            Bound::Included(self.prefix),
            Bound::Unbounded,
        ));

        if iter.next().map(|(key, _)| key.starts_with(self.prefix)).unwrap_or_default() {
            visitor.visit_some(self)
        }
        else {
            visitor.visit_none()
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    impl_deserialize_unexpected!(deserialize_identifier);
    impl_deserialize_unexpected!(deserialize_bool);
    impl_deserialize_unexpected!(deserialize_i8);
    impl_deserialize_unexpected!(deserialize_u8);
    impl_deserialize_unexpected!(deserialize_i16);
    impl_deserialize_unexpected!(deserialize_u16);
    impl_deserialize_unexpected!(deserialize_i32);
    impl_deserialize_unexpected!(deserialize_u32);
    impl_deserialize_unexpected!(deserialize_i64);
    impl_deserialize_unexpected!(deserialize_u64);
    impl_deserialize_unexpected!(deserialize_f32);
    impl_deserialize_unexpected!(deserialize_f64);
    impl_deserialize_unexpected!(deserialize_char);
    impl_deserialize_unexpected!(deserialize_str);
    impl_deserialize_unexpected!(deserialize_string);
    impl_deserialize_unexpected!(deserialize_bytes);
    impl_deserialize_unexpected!(deserialize_byte_buf);
    impl_deserialize_unexpected!(deserialize_unit);
    impl_deserialize_unexpected!(deserialize_seq);
}

enum NextStructValue<'a> {
    Nested { prefix: &'a str },
    Value { value: &'a MetadataValue },
}

struct MetadataMap<'a, 'de> {
    de: &'a MetadataDeserializer<'de>,
    iter: Peekable<btree_map::Range<'de, String, MetadataValue>>,
    next_value: Option<NextStructValue<'de>>,
}

impl<'a, 'de> MetadataMap<'a, 'de> {
    fn new(de: &'a MetadataDeserializer<'de>) -> Self {
        let iter = de.metadata.0.range::<str, _>((
            if de.prefix.is_empty() {
                Bound::<&str>::Unbounded
            }
            else {
                Bound::Excluded(de.prefix)
            },
            Bound::<&str>::Unbounded,
        )).peekable();

        Self {
            de,
            iter,
            next_value: None,
        }
    }

    fn next(&mut self) -> Option<(&'de String, &'de MetadataValue)> {
        let next = self.iter.next()?;
        next.0.starts_with(self.de.prefix).then_some(next)
    }

    fn skip_prefix(&mut self, prefix: &str) {
        loop {
            let Some((key, _)) = self.iter.peek() else { break; };
            if key.starts_with(prefix) {
                self.iter.next();
            }
            else {
                break;
            }
        }
    }
}

impl<'a, 'de> de::MapAccess<'de> for MetadataMap<'a, 'de> {
    type Error = DeserializeError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        assert!(self.next_value.is_none());

        let Some((key, value)) = self.next()
        else {
            return Ok(None);
        };

        let key_stripped = key.strip_prefix(self.de.prefix).unwrap();

        let (key, value) = if let Some(dot_index) = key_stripped.find('.') {
            let nested_key = &key_stripped[..dot_index];

            let mut key_de = MetadataKeyDeserializer::new(nested_key);
            let nested_prefix = &key[..self.de.prefix.len() + dot_index + 1];
            let key = seed.deserialize(&mut key_de)?;

            self.skip_prefix(nested_prefix);

            (key, NextStructValue::Nested { prefix: nested_prefix })
        }
        else {
            let mut key_de = MetadataKeyDeserializer::new(key_stripped);
            let key = seed.deserialize(&mut key_de)?;

            (key, NextStructValue::Value { value })
        };

        self.next_value = Some(value);

        Ok(Some(key))
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let value = self.next_value.take().unwrap();

        let value = match value {
            NextStructValue::Nested { prefix } => {
                let mut value_de = MetadataDeserializer {
                    metadata: self.de.metadata,
                    prefix,
                };
                seed.deserialize(&mut value_de)?
            }
            NextStructValue::Value { value } => {
                let mut value_de = MetadataValueDeserializer::new(value);
                seed.deserialize(&mut value_de)?
            }
        };

        Ok(value)
    }
}

struct MetadataKeyDeserializer<'de> {
    key: &'de str,
}

impl<'de> MetadataKeyDeserializer<'de> {
    fn new(key: &'de str) -> Self {
        Self { key }
    }
}

impl<'a, 'de> de::Deserializer<'de> for &'a mut MetadataKeyDeserializer<'de> {
    type Error = DeserializeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_str(self.key)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    impl_deserialize_unexpected!(deserialize_option);
    impl_deserialize_unexpected!(deserialize_bool);
    impl_deserialize_unexpected!(deserialize_i8);
    impl_deserialize_unexpected!(deserialize_u8);
    impl_deserialize_unexpected!(deserialize_i16);
    impl_deserialize_unexpected!(deserialize_u16);
    impl_deserialize_unexpected!(deserialize_i32);
    impl_deserialize_unexpected!(deserialize_u32);
    impl_deserialize_unexpected!(deserialize_i64);
    impl_deserialize_unexpected!(deserialize_u64);
    impl_deserialize_unexpected!(deserialize_f32);
    impl_deserialize_unexpected!(deserialize_f64);
    impl_deserialize_unexpected!(deserialize_char);
    impl_deserialize_unexpected!(deserialize_bytes);
    impl_deserialize_unexpected!(deserialize_byte_buf);
    impl_deserialize_unexpected!(deserialize_unit);
    impl_deserialize_unexpected!(deserialize_seq);
    impl_deserialize_unexpected!(deserialize_map);
}

macro_rules! impl_getter {
    ($method:ident, $mdv:ident, $ty:ident) => {
        fn $method(&self) -> Result<$ty, DeserializeError> {
            match self.value {
                MetadataValue::$mdv(value) => Ok(*value),
                value => {
                    Err(DeserializeError::ExpectedType {
                        expected: MetadataValueType::$mdv,
                        got: value.ty(),
                    })
                }
            }
        }
    };
}

struct MetadataValueDeserializer<'de> {
    value: &'de MetadataValue,
}

impl<'de> MetadataValueDeserializer<'de> {
    fn new(value: &'de MetadataValue) -> Self {
        Self { value }
    }

    impl_getter!(get_bool, Bool, bool);
    impl_getter!(get_i8, I8, i8);
    impl_getter!(get_u8, U8, u8);
    impl_getter!(get_i16, I16, i16);
    impl_getter!(get_u16, U16, u16);
    impl_getter!(get_i32, I32, i32);
    impl_getter!(get_u32, U32, u32);
    impl_getter!(get_i64, I64, i64);
    impl_getter!(get_u64, U64, u64);
    impl_getter!(get_f32, F32, f32);
    impl_getter!(get_f64, F64, f64);

    fn get_str(&self) -> Result<&str, DeserializeError> {
        match self.value {
            MetadataValue::String(value) => Ok(value),
            value => {
                Err(DeserializeError::ExpectedType {
                    expected: MetadataValueType::String,
                    got: value.ty(),
                })
            }
        }
    }

    fn get_array(&self) -> Result<&'de [MetadataValue], DeserializeError> {
        match self.value {
            MetadataValue::Array(value) => Ok(value),
            value => {
                Err(DeserializeError::ExpectedType {
                    expected: MetadataValueType::String,
                    got: value.ty(),
                })
            }
        }
    }
}

impl<'a, 'de> de::Deserializer<'de> for &'a mut MetadataValueDeserializer<'de> {
    type Error = DeserializeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.value {
            MetadataValue::U8(value) => visitor.visit_u8(*value),
            MetadataValue::I8(value) => visitor.visit_i8(*value),
            MetadataValue::U16(value) => visitor.visit_u16(*value),
            MetadataValue::I16(value) => visitor.visit_i16(*value),
            MetadataValue::U32(value) => visitor.visit_u32(*value),
            MetadataValue::I32(value) => visitor.visit_i32(*value),
            MetadataValue::F32(value) => visitor.visit_f32(*value),
            MetadataValue::U64(value) => visitor.visit_u64(*value),
            MetadataValue::I64(value) => visitor.visit_i64(*value),
            MetadataValue::F64(value) => visitor.visit_f64(*value),
            MetadataValue::Bool(value) => visitor.visit_bool(*value),
            MetadataValue::String(value) => visitor.visit_str(value),
            MetadataValue::Array(value) => visitor.visit_seq(ArraySeq::new(value)),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_bool(self.get_bool()?)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i8(self.get_i8()?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i16(self.get_i16()?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i32(self.get_i32()?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i64(self.get_i64()?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u8(self.get_u8()?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u16(self.get_u16()?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u32(self.get_u32()?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u64(self.get_u64()?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f32(self.get_f32()?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f64(self.get_f64()?)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_str(self.get_str()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_string(self.get_str()?.to_owned())
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(ArraySeq::new(self.get_array()?))
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(DeserializeError::Unexpected)
    }

    impl_deserialize_unexpected!(deserialize_char);
    impl_deserialize_unexpected!(deserialize_bytes);
    impl_deserialize_unexpected!(deserialize_byte_buf);
    impl_deserialize_unexpected!(deserialize_map);
    impl_deserialize_unexpected!(deserialize_identifier);
    impl_deserialize_unexpected!(deserialize_unit);
}

struct ArraySeq<'de> {
    index: usize,
    array: &'de [MetadataValue],
}

impl<'de> ArraySeq<'de> {
    fn new(array: &'de [MetadataValue]) -> Self {
        Self { index: 0, array }
    }
}

impl<'de> de::SeqAccess<'de> for ArraySeq<'de> {
    type Error = DeserializeError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        let Some(value) = self.array.get(self.index)
        else {
            return Ok(None);
        };
        self.index += 1;
        let mut de = MetadataValueDeserializer { value };
        seed.deserialize(&mut de).map(Some)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum DeserializeError {
    #[error("{0}")]
    Custom(String),

    #[error("expected a struct")]
    ExpectedStruct,

    #[error("key not found: {key}")]
    KeyNotFound { key: String },

    #[error("expected type {expected:?}, but found {got:?}")]
    ExpectedType {
        expected: MetadataValueType,
        got: MetadataValueType,
    },

    #[error("unexpected")]
    Unexpected,

    #[error("not supported")]
    NotSupported,
}

impl de::Error for DeserializeError {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Self::Custom(msg.to_string())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct General {
    pub architecture: String,
    pub quantization_version: Option<u32>,
    #[serde(default = "default_alignment")]
    pub alignment: u32,
    pub name: Option<String>,
    pub author: Option<String>,
    pub url: Option<String>,
    pub description: Option<String>,
    pub license: Option<String>,
    pub file_type: Option<u32>,
    pub source: Option<Source>,
}

fn default_alignment() -> u32 {
    32
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Source {
    pub url: Option<String>,
    pub huggingface: Option<HuggingfaceSource>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HuggingfaceSource {
    pub repository: String,
}

#[derive(Copy, Clone, Debug, IntEnum)]
#[repr(u32)]
pub enum FileType {
    AllF32 = 0,
    MostlyF16 = 1,
    MostlyQ4_0 = 2,
    MostlyQ4_1 = 3,
    MostlyQ4_1SomeF16 = 4,
    MostlyQ4_2 = 5, // (support removed)
    MostlyQ4_3 = 6, // (support removed)
    MostlyQ8_0 = 7,
    MostlyQ5_0 = 8,
    MostlyQ5_1 = 9,
    MostlyQ2K = 10,
    MostlyQ3KS = 11,
    MostlyQ3KM = 12,
    MostlyQ3KL = 13,
    MostlyQ4KS = 14,
    MostlyQ4KM = 15,
    MostlyQ5KS = 16,
    MostlyQ5KM = 17,
    MostlyQ6K = 18,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Tokenizer {
    pub ggml: Option<GgmlTokenizer>,
    pub huggingface: Option<HuggingfaceTokenizer>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GgmlTokenizer {
    pub model: String,
    //pub tokens: Vec<String>,
    //pub scores: Option<Vec<f32>>,
    //pub token_type: Option<Vec<TokenType>>,
    //pub merges: Option<Vec<String>>,
    //pub added_tokens: Option<Vec<String>>,
    pub bos_token_id: Option<u32>,
    pub eos_token_id: Option<u32>,
    pub unknown_token_id: Option<u32>,
    pub separator_token_id: Option<u32>,
    pub padding_token_id: Option<u32>,
}

#[derive(Copy, Clone, Debug, IntEnum)]
#[repr(i32)]
pub enum TokenType {
    Normal = 1,
    Unknown = 2,
    Control = 3,
    UserDefined = 4,
    Unused = 5,
    Byte = 6,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HuggingfaceTokenizer {
    // todo: deserialize this
    pub json: String,
}
