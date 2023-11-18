pub mod block;
pub mod primitive;
pub mod wgsl;

use std::fmt::Debug;

use half::f16;

use self::{
    block::Block,
    primitive::{
        PackedBool,
        PackedI16,
        PackedI8,
        PackedU16,
        PackedU8,
    },
    wgsl::{
        WgslDecodeFromBlock,
        WgslEncodeIntoBlock,
        WgslType,
    },
};

pub trait Zero {
    const ZERO: Self;
}

pub trait One {
    const ONE: Self;
}

pub trait Element: Copy + Debug + 'static {
    type Block: Block;
    type Primitive: WgslType + WgslDecodeFromBlock<Self::Block> + WgslEncodeIntoBlock<Self::Block>;
}

macro_rules! impl_element {
    ($ty:ident, $block:ident, $primitive:ident) => {
        impl Element for $ty {
            type Block = $block;
            type Primitive = $primitive;
        }
    };

    ($ty:ident) => {
        impl_element!($ty, Self, Self);
    };
}

impl_element!(i8, PackedI8, i32);
impl_element!(u8, PackedU8, u32);
impl_element!(i16, PackedI16, i32);
impl_element!(u16, PackedU16, u32);
impl_element!(i32);
impl_element!(u32);
impl_element!(f16);
impl_element!(f32);
impl_element!(bool, PackedBool, bool);

pub trait Number: Zero + One {}

impl Number for u8 {}
impl Number for i8 {}
impl Number for u16 {}
impl Number for i16 {}
impl Number for u32 {}
impl Number for i32 {}
impl Number for f32 {}
impl Number for f16 {}
