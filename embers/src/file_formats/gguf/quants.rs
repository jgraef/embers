use bytemuck::{
    Pod,
    Zeroable,
};
use futures_lite::AsyncRead;
use half::f16;

use super::{
    Error,
    GgmlElement,
    GgmlType,
    Parse,
};
use crate::element::{
    block::Block,
    wgsl::{
        WgslDecodeFromBlock,
        WgslEncodeIntoBlock,
        WgslType,
    },
    Element,
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

impl Block for BlockQ8_0 {
    const NUM_PACKED: usize = 32;
}

impl WgslType for BlockQ8_0 {
    const TYPE_NAME: &'static str = "block_q8_0_t";
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

#[derive(Copy, Clone, Debug)]
pub enum Q8_0 {}

impl GgmlElement for Q8_0 {
    const GGML_TYPE: GgmlType = GgmlType::Q8_0;
}

impl Element for Q8_0 {
    type Block = BlockQ8_0;
    type Primitive = f16;
}

impl WgslDecodeFromBlock<BlockQ8_0> for f16 {
    const DECODE: &'static str = "let value = encoded.d * f16(encoded.qs[i]);";
}

impl WgslEncodeIntoBlock<BlockQ8_0> for f16 {
    const ENCODE: &'static str = "todo";
}
