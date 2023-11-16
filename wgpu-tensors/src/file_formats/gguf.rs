use std::{
    collections::BTreeMap,
    io::SeekFrom,
};

use byteorder::LittleEndian;
use futures_lite::{
    io::{
        AsyncRead,
        AsyncReadExt,
        AsyncSeek,
    },
    AsyncSeekExt,
    Future,
};
use half::f16;

use super::ReadBytesAsyncExt;
use crate::{
    element::Element,
    error::DimensionMismatch,
    tensor::shape::Shape,
    Gpu,
    Tensor,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("io error")]
    Io(#[from] std::io::Error),

    #[error("invalid magic: {found:?}")]
    InvalidMagic { found: [u8; 4] },

    #[error("invalid magic: {found:?}")]
    IncompatibleVersion { found: u32 },

    #[error("invalid utf8 encoding")]
    Utf8(#[from] std::string::FromUtf8Error),

    #[error("invalid metadata value type: {found:?}")]
    InvalidMetadataValueType { found: u32 },

    #[error("invalid bool: {found:?}")]
    InvalidBool { found: u8 },

    #[error("invalid ggml_type: {found:?}")]
    InvalidGgmlType { found: u32 },

    #[error("tensor '{name}' not found")]
    TensorNotFound { name: String },

    #[error("dimension mismatch")]
    DimensionMismatch(#[from] DimensionMismatch),

    #[error("error during number conversion")]
    TryFromInt(#[from] std::num::TryFromIntError),

    #[error("expected type {expected:?}, but got {got:?}")]
    TypeMismatch { expected: GgmlType, got: GgmlType },
}

pub trait Parse: Sized {
    fn parse<R: AsyncRead + Unpin>(reader: R) -> impl Future<Output = Result<Self, Error>>;
}

impl Parse for String {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let len = reader.read_u64::<LittleEndian>().await?;

        let mut buf = Vec::with_capacity(len as usize);
        reader.read_exact(&mut buf).await?;

        Ok(String::from_utf8(buf)?)
    }
}

impl Parse for bool {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let raw = reader.read_u8().await?;

        match raw {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Error::InvalidBool { found: raw }),
        }
    }
}

macro_rules! impl_trivial_parse {
    ($ty:ident, $method:ident) => {
        impl Parse for $ty {
            async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
                Ok(reader.$method().await?)
            }
        }
    };

    ($ty:ident, $method:ident, $endianess:ident) => {
        impl Parse for $ty {
            async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
                Ok(reader.$method::<$endianess>().await?)
            }
        }
    };
}

impl_trivial_parse!(f32, read_f32, LittleEndian);
impl_trivial_parse!(i8, read_i8);
impl_trivial_parse!(i16, read_i16, LittleEndian);
impl_trivial_parse!(i32, read_i32, LittleEndian);

impl Parse for f16 {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let bits = reader.read_u16::<LittleEndian>().await?;
        Ok(f16::from_bits(bits))
    }
}

pub struct Gguf<R> {
    reader: R,
    header: Header,
}

impl<R: AsyncReadExt + Unpin> Gguf<R> {
    pub async fn open(mut reader: R) -> Result<Self, Error> {
        let header = Header::parse(&mut reader).await?;

        Ok(Self { reader, header })
    }
}

impl<R> Gguf<R> {
    pub fn version(&self) -> u32 {
        self.header.version
    }

    pub fn metadata(&self) -> &BTreeMap<String, MetadataValue> {
        &self.header.metadata_kv
    }

    pub fn tensor_infos(&self) -> &BTreeMap<String, TensorInfo> {
        &self.header.tensor_infos
    }
}

impl<R: AsyncReadExt + AsyncSeek + Unpin> Gguf<R> {
    pub async fn load_tensor<const D: usize, T: Element + IsGgmlType>(
        &mut self,
        name: &str,
        gpu: &Gpu,
    ) -> Result<Tensor<D, T>, Error> {
        let tensor_info = self.header.tensor_infos.get(name).ok_or_else(|| {
            Error::TensorNotFound {
                name: name.to_owned(),
            }
        })?;

        tensor_info.check_type::<T>()?;
        let shape = tensor_info.shape::<D>()?;
        let num_elements = shape.size();

        let mut builder = Tensor::<D, T>::builder(gpu, shape);

        self.reader
            .seek(SeekFrom::Start(tensor_info.offset))
            .await?;

        for _ in 0..num_elements {
            let element = T::parse(&mut self.reader).await?;
            builder.write_element(element);
        }

        assert!(builder.is_full());

        Ok(builder.build())
    }
}

pub struct Header {
    version: u32,
    metadata_kv: BTreeMap<String, MetadataValue>,
    tensor_infos: BTreeMap<String, TensorInfo>,
}

impl Header {
    const MAGIC: &'static [u8; 4] = b"GGUF";
    const VERSION: u32 = 3;
}

impl Parse for Header {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let mut magic = [0; 4];
        reader.read_exact(&mut magic).await?;
        if &magic != Self::MAGIC {
            return Err(Error::InvalidMagic { found: magic });
        }

        let version = reader.read_u32::<LittleEndian>().await?;
        if version != Self::VERSION {
            return Err(Error::IncompatibleVersion { found: version });
        }

        let tensor_count = reader.read_u64::<LittleEndian>().await?;
        let metadata_kv_count = reader.read_u64::<LittleEndian>().await?;

        let mut metadata_kv = BTreeMap::new();

        for _ in 0..metadata_kv_count {
            let key = String::parse(&mut reader).await?;
            let value = MetadataValue::parse(&mut reader).await?;
            metadata_kv.insert(key, value);
        }

        let mut tensor_infos = BTreeMap::new();

        for _ in 0..tensor_count {
            let tensor_info = TensorInfo::parse(&mut reader).await?;
            tensor_infos.insert(tensor_info.name.clone(), tensor_info);
        }

        Ok(Self {
            version,
            metadata_kv,
            tensor_infos,
        })
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
    Float32(f32),
    U64(u64),
    I64(i64),
    Float64(f64),
    Bool(bool),
    String(String),
    Array(Vec<MetadataValue>),
}

impl Parse for MetadataValue {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let value_type = reader.read_u32::<LittleEndian>().await?;
        let value = match value_type {
            0 => MetadataValue::U8(reader.read_u8().await?),
            1 => MetadataValue::I8(reader.read_i8().await?),
            2 => MetadataValue::U16(reader.read_u16::<LittleEndian>().await?),
            3 => MetadataValue::I16(reader.read_i16::<LittleEndian>().await?),
            4 => MetadataValue::U32(reader.read_u32::<LittleEndian>().await?),
            5 => MetadataValue::I32(reader.read_i32::<LittleEndian>().await?),
            6 => MetadataValue::Float32(reader.read_f32::<LittleEndian>().await?),
            7 => MetadataValue::Bool(bool::parse(&mut reader).await?),
            8 => MetadataValue::String(String::parse(&mut reader).await?),
            9 => {
                // MetadataValue::Array(reader.read_u8().await?)
                todo!();
            }
            10 => MetadataValue::U64(reader.read_u64::<LittleEndian>().await?),
            11 => MetadataValue::I64(reader.read_i64::<LittleEndian>().await?),
            12 => MetadataValue::Float64(reader.read_f64::<LittleEndian>().await?),
            _ => return Err(Error::InvalidMetadataValueType { found: value_type }),
        };
        Ok(value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GgmlType {
    F32 = 0,
    F16 = 1,
    Q4_0 = 2,
    Q4_1 = 3,
    // GGML_TYPE_Q4_2 = 4, support has been removed
    // GGML_TYPE_Q4_3 (5) support has been removed
    Q5_0 = 6,
    Q5_1 = 7,
    Q8_0 = 8,
    Q8_1 = 9,
    // k-quantizations
    Q2K = 10,
    Q3K = 11,
    Q4K = 12,
    Q5K = 13,
    Q6K = 14,
    Q8K = 15,
    I8,
    I16,
    I32,
    Count,
}

impl Parse for GgmlType {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let ty = reader.read_u32::<LittleEndian>().await?;

        let ty = match ty {
            0 => Self::F32,
            1 => Self::F16,
            2 => Self::Q4_0,
            3 => Self::Q4_1,
            6 => Self::Q5_0,
            7 => Self::Q5_1,
            8 => Self::Q8_0,
            9 => Self::Q8_1,
            10 => Self::Q2K,
            11 => Self::Q3K,
            12 => Self::Q4K,
            13 => Self::Q5K,
            14 => Self::Q6K,
            15 => Self::Q8K,
            16 => Self::I8,
            17 => Self::I16,
            18 => Self::I32,
            19 => Self::Count,
            _ => return Err(Error::InvalidGgmlType { found: ty }),
        };

        Ok(ty)
    }
}

pub struct TensorInfo {
    pub name: String,
    pub dimensions: Vec<u64>,
    pub ty: GgmlType,
    pub offset: u64,
}

impl TensorInfo {
    pub fn shape<const D: usize>(&self) -> Result<[usize; D], Error> {
        if self.dimensions.len() != D {
            Err(DimensionMismatch {
                expected: D,
                got: self.dimensions.len(),
            }
            .into())
        }
        else {
            let mut shape = [0; D];
            for (i, &x) in self.dimensions.iter().enumerate() {
                shape[i] = x.try_into()?;
            }
            Ok(shape)
        }
    }

    fn check_type<T: IsGgmlType>(&self) -> Result<(), Error> {
        if T::GGML_TYPE == self.ty {
            Ok(())
        }
        else {
            Err(Error::TypeMismatch {
                expected: T::GGML_TYPE,
                got: self.ty,
            })
        }
    }
}

impl Parse for TensorInfo {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let name = String::parse(&mut reader).await?;
        let n_dimensions = reader.read_u32::<LittleEndian>().await?;
        let mut dimensions = Vec::with_capacity(n_dimensions as usize);
        for _ in 0..n_dimensions {
            dimensions.push(reader.read_u64::<LittleEndian>().await?);
        }
        let ty = GgmlType::parse(&mut reader).await?;
        let offset = reader.read_u64::<LittleEndian>().await?;
        Ok(Self {
            name,
            dimensions,
            ty,
            offset,
        })
    }
}

// todo: we probably want to use a separate trait from `Parse`, so that we can
// construct tensors from their encoded values, instead of parsing it into
// the element type and then encoding it again.
pub trait IsGgmlType: Parse {
    const GGML_TYPE: GgmlType;
}

macro_rules! impl_is_ggml_type {
    ($ty:ident, $ggml:ident) => {
        impl IsGgmlType for $ty {
            const GGML_TYPE: GgmlType = GgmlType::$ggml;
        }
    };
}

impl_is_ggml_type!(f32, F32);
impl_is_ggml_type!(f16, F16);
impl_is_ggml_type!(i8, I8);
impl_is_ggml_type!(i16, I16);
impl_is_ggml_type!(i32, I32);
