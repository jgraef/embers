use std::borrow::Cow;

use super::block::Block;

pub trait WgslType {
    const TYPE_NAME: &'static str;
}

pub trait WgslLiteral {
    fn format_literal(&self) -> Cow<'static, str>;
}

pub trait WgslEncodeIntoBlock<B: Block>: WgslType {
    const ENCODE: &'static str;
}

pub trait WgslDecodeFromBlock<B: Block>: WgslType {
    const DECODE: &'static str;
}
