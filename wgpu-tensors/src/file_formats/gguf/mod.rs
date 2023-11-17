pub mod metadata;

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
use int_enum::IntEnum;

use super::ReadBytesAsyncExt;
use crate::{
    element::Element,
    error::DimensionMismatch,
    file_formats::gguf::metadata::{
        Metadata,
        MetadataValue,
    },
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

        let mut buf = vec![];
        buf.resize(len as usize, 0);
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

    pub fn metadata(&self) -> &Metadata {
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
    metadata_kv: Metadata,
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
        tracing::debug!(?magic);

        let version = reader.read_u32::<LittleEndian>().await?;
        if version != Self::VERSION {
            return Err(Error::IncompatibleVersion { found: version });
        }
        tracing::debug!(?version);

        let tensor_count = reader.read_u64::<LittleEndian>().await?;
        let metadata_kv_count = reader.read_u64::<LittleEndian>().await?;
        tracing::debug!(?tensor_count, metadata_kv_count);

        let mut metadata_kv = BTreeMap::new();

        for _ in 0..metadata_kv_count {
            let key = String::parse(&mut reader).await?;
            let value = MetadataValue::parse(&mut reader).await?;
            tracing::debug!(key, ?value, "metadata");
            metadata_kv.insert(key, value);
        }

        let metadata_kv = Metadata(metadata_kv);

        let mut tensor_infos = BTreeMap::new();

        for _ in 0..tensor_count {
            let tensor_info = TensorInfo::parse(&mut reader).await?;
            tracing::debug!(?tensor_info, "tensor info");
            tensor_infos.insert(tensor_info.name.clone(), tensor_info);
        }

        Ok(Self {
            version,
            metadata_kv,
            tensor_infos,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, IntEnum)]
#[repr(u32)]
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
    I8 = 16,    // ?
    I16 = 17,   // ?
    I32 = 18,   // ?
    Count = 19, // ?
}

impl Parse for GgmlType {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let ty = reader.read_u32::<LittleEndian>().await?;
        let ty = Self::try_from(ty).map_err(|_| Error::InvalidGgmlType { found: ty })?;
        Ok(ty)
    }
}

#[derive(Clone, Debug)]
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
