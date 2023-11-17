use bytemuck::{
    Pod,
    Zeroable,
};
use futures_lite::AsyncRead;
use half::f16;

use super::{
    Error,
    Parse,
};

#[derive(Clone, Copy, Debug, Pod, Zeroable)]
#[repr(C)]
pub struct BlockQ4_0 {
    pub d: f16,
    pub qs: [u8; 16],
}

impl Parse for BlockQ4_0 {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let d = f16::parse(&mut reader).await?;
        let qs = <[u8; 16]>::parse(&mut reader).await?;
        Ok(Self { d, qs })
    }
}

#[derive(Clone, Copy, Debug, Pod, Zeroable)]
#[repr(C)]
pub struct BlockQ4_1 {
    pub d: f16,
    pub m: f16,
    pub qs: [u8; 16],
}

impl Parse for BlockQ4_1 {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let d = f16::parse(&mut reader).await?;
        let m = f16::parse(&mut reader).await?;
        let qs = <[u8; 16]>::parse(&mut reader).await?;
        Ok(Self { d, m, qs })
    }
}

#[derive(Clone, Copy, Debug, Pod, Zeroable)]
#[repr(C)]
pub struct BlockQ5_0 {
    pub d: f16,
    pub qh: [u8; 4],
    pub qs: [u8; 16],
}

impl Parse for BlockQ5_0 {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let d = f16::parse(&mut reader).await?;
        let qh = <[u8; 4]>::parse(&mut reader).await?;
        let qs = <[u8; 16]>::parse(&mut reader).await?;
        Ok(Self { d, qh, qs })
    }
}

#[derive(Clone, Copy, Debug, Pod, Zeroable)]
#[repr(C)]
pub struct BlockQ5_1 {
    pub d: f16,
    pub m: f16,
    pub qh: [u8; 4],
    pub qs: [u8; 16],
}

impl Parse for BlockQ5_1 {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let d = f16::parse(&mut reader).await?;
        let m = f16::parse(&mut reader).await?;
        let qh = <[u8; 4]>::parse(&mut reader).await?;
        let qs = <[u8; 16]>::parse(&mut reader).await?;
        Ok(Self { d, m, qh, qs })
    }
}

#[derive(Clone, Copy, Debug, Pod, Zeroable)]
#[repr(C)]
pub struct BlockQ8_0 {
    pub d: f16,
    pub qs: [i8; 32],
}

impl Parse for BlockQ8_0 {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let d = f16::parse(&mut reader).await?;
        let qs = <[i8; 32]>::parse(&mut reader).await?;
        Ok(Self { d, qs })
    }
}

#[derive(Clone, Copy, Debug, Pod, Zeroable)]
#[repr(C)]
pub struct BlockQ8_1 {
    pub d: f32,
    pub s: f32,
    pub qs: [i8; 32],
}

impl Parse for BlockQ8_1 {
    async fn parse<R: AsyncRead + Unpin>(mut reader: R) -> Result<Self, Error> {
        let d = f32::parse(&mut reader).await?;
        let s = f32::parse(&mut reader).await?;
        let qs = <[i8; 32]>::parse(&mut reader).await?;
        Ok(Self { d, s, qs })
    }
}

/*
pub struct Q8_0(pub f16);

impl Encode for Q8_0 {
    type Buffer = QuantBuffer<Self>;
    type Encoded = BlockQ8_0;
    type Primitive = f16;

    const NUM_PACKED: usize = 32;
    const WGSL_DECODE: &'static str = "todo";
    const WGSL_ENCODE: &'static str = "todo";

    fn write_into(&self, destination: &mut Self::Encoded, i: usize) {
        todo!()
    }

    fn read_from(source: &Self::Encoded, i: usize) -> Self {
        todo!()
    }
}

impl WgslType for BlockQ8_0 {
    const TYPE_NAME: &'static str = "block_q8_0_t";

    fn wgsl_literal(&self) -> Cow<'static, str> {
        todo!()
    }
}

struct QuantBuffer<T: Encode>(ArrayVec<T, { T::NUM_PACKED }>) where [(); T::NUM_PACKED]:;

impl<T> Default for QuantBuffer<T> {
    fn default() -> Self {
        Self(ArrayVec::new())
    }
}

impl<T: Encode> EncodeBuffer<T> for QuantBuffer<T> {
    fn write(&mut self, value: T) -> Option<T::Encoded> {
        todo!()
    }

    fn flush(&mut self) -> Option<T::Encoded> {
        todo!()
    }
}
 */
